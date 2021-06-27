#!/usr/bin/env python3.9
"""
espy : version2, optimization via data mining   
(c) 2021, Tim Menzies, http://unlicense.org   

    :-------:                  
    | Ba    | Bad <----.      
    |    56 |          |    
    :-------:------:   |         
            | B    |   v      explore  = min(better + bad)   
            |    5 | Better   optimize = max(better - bad)   
            :------:          monitor  = max(bad - better)   

USAGE: ./eg.py [OPTIONS] [GROUP DETAILS]    

GROUP: row, rule   

OPTIONS:
 -h              show help   
 -data    FILE   when to load data from; e.g."../data/auto93.csv"   
 -do      STR    when starting up, what action to run; e.g. "none"   
 -fmt     STR    pretty print control for numbers; e.g.["{:>7.2g}"]   
 -seed    INT    default random number seed; e.g. 10013   

DETAILS:

row   
 -l       INT    when computing distance, equation coefficient; e.g. 1   
 -enough  FLOAT  when recurring on clusters, when to stop; e.g. .5   
 -sample  INT    when seeking distant pairs, how samples of the data; e.g.32   
   
rule   
 -show    INT    when displaying final output, how many rules to show; e.g.10   
 -top     INT    when hunting for rules, max number ranges to explore; e.g.10   
"""
import re,sys,copy,math,random
from etc import o, csv, cli

options =   dict(
   all =    dict(Verbose=0,
                 data="../data/auto93.csv",
                 do="none",
                 seed=10013,
                 fmt="{:>7.2g}"),
  row=      dict(l=1),
  cluster=  dict(enough=.5,
                 sample=32),
  rule=     dict(show=10,
                 top=10))

#---------------------------------------------------------------
class Col(o):
  "`Col`s are generic columns."
  def __init__(i, txt="", at=0, inits=[]):
    i.n, i.txt, i.at, i.w = 0, txt, at, -1 if "-" in txt else 1
    [self.add(z) for z in inits]
  def add(i, z): return z
  def mid(i):
    "Estimate of central  tendency."
    return "?"
  def var(i):
    "Estimate of dispersion around central tendency."
    return "?"
  def dist(i, x, y):
    "Distance."
    return 1 if x == "?" and y == "?" else i.dist1(x, y)

#---------------------------------------------------------------
class Skip(Col):
  "`Skip`s are column of things we are going to ignore."
  pass

#---------------------------------------------------------------
class Num(Col):
  "`Num`s summarize numerics"
  def __init__(i, *l, **kw):
    i._all, i.sorted = [], True
    super().__init__(*l, **kw)
  def add(i, z):
    "Add anything that we are not skipping."
    if z != "?":
      i.n += 1
      z = float(z)
      i._all += [z]
      i.sorted = False
    return z
  def norm(i, z):
    "Normalize 0..1."
    if z == "?": return z
    lo, hi = i.all()[0], i.all()[-1]
    tmp = (z - lo) / (hi - lo + 1E-32)
    return max(0, min(tmp, 1))
  def all(i):
    "Return sorted list of  all the numbers."
    if not i.sorted: i._all.sort()
    i.sorted = True
    return i._all
  def dist1(i, x, y):
    "Numeric distances. Make guesses for missing values."
    if   x == "?": y = i.norm(y); x = 0 if y > .5 else 1 
    elif y == "?": x = i.norm(x);  y = 0 if x > .5 else 1 #
    else         : x, y = i.norm(x), i.norm(y)
    return abs(x - y)
  def per(i, p=.5):
    "Return the pth percentil value in all the numbers."
    a = i.all()
    return a[int(p * len(a))]
  def mid(i):
    "Estimate of central  tendency."
    return i.per(.5)
  def var(i):
    "Estimate of dispersion around central tendency."
    return (i.per(.9) - i.per(.1)) / 2.56

#---------------------------------------------------------------
class Sym(Col):
  "`Sym`s summarize symbols."
  def __init__(i, *l, **kw):
    i.seen, i.mode, i.most = {}, None, 0
    super().__init__(*l, **kw)
  def add(i, z, n=1):
    "Add anything that we are not skipping."
    if z != "?":
      i.n += n
      tmp = i.seen[z] = i.seen.get(z, 0) + n
      if tmp > i.most:
        i.most, i.mode = tmp, z
    return z
  def mid(i):
    "Estimate of central  tendency."
    return i.mode
  def dist1(i, x, y):
    "Estimate of dispersion around central tendency."
    return 0 if x == y else 1

#---------------------------------------------------------------
class Row(o):
  "`Row`s hold one example."
  def __init__(i, t, cells): i._tab, i.cells = t, cells

  def __lt__(i, j):
    "Continuous domination."
    cols = i._tab.y
    s1, s2, n = 0, 0, len(cols)
    for col in cols:
      a, b = i.cells[col.at], j.cells[col.at]
      if a == "?" or b == "?":
        continue
      a, b = col.norm(a), col.norm(b)
      s1 -= math.e**(col.w * (a - b) / n)
      s2 -= math.e**(col.w * (b - a) / n)
    return s1 / n < s2 / n

  def dist(i, j, the, cols=None):
    "Distance between examples."
    gap, n = 0, 1E-32
    for col in cols or i._tab.x:
      tmp = col.dist(i.cells[col.at], j.cells[col.at])
      gap += tmp**the.l
      n += 1
    return (gap / n)**(1 / the.l)

#---------------------------------------------------------------
class Tab(o):
  "`Tab`les store examples, summarized in columns."
  def __init__(i, rows=[]):
    i.xy, i.x, i.y, i.all, i.rows = [], [], [], [], []
    [i.add(lst) for lst in rows]

  def __lt__(i, j):
    "Sort tables based on their central tendency."
    return Row(i, i.mid()) < Row(j, j.mid())

  def goals(i):
    "Returns the goal values of the central tendencies."
    return [col.mid() for col in i.y]

  def mid(i):
    "Returns the central tendencies."
    return [col.mid() for col in i.all]

  def clone(i, rows=[]):
    "Return a new table with the same structure."
    t = Tab()
    t.add([c.txt for c in i.all])
    [t.add(row) for row in rows]
    return t

  def add(i, lst):
    """Add a row to a table. If this is row number1,
    then these are the names that define the column
    types."""
    lst = lst.cells if type(lst) == Row else lst
    (i.data if i.all else i.head)(lst)

  def isa(i, s, at):
    """Skip names contain '?', Num names start with upper case,
     everything else is a Sym."""
    return Skip if "?" in s else (
        Num if s[0].isupper() else
        Sym)(txt=s, at=at)

  def head(i, lst):
    """Define the column types, store them in `all`. If a column
    name contains '?' then do  not add it to any lst of 'x' or 'y'
    columns."""
    for at, s in enumerate(lst):
      one = i.isa(s, at)
      i.all += [one] # everyone gets called in `all`
      if type(one) != Skip:
        i.xy += [one] # none skipped things are in `xy` or `x` or `y`
        if "+" in s or "-" in s or "!" in s:
          i.y += [one]
        else:
          i.x += [one]

  def data(i, lst):
    "Add a new row of data, update the column headers."
    for col in i.xy: # update things we are not skipping
      z = lst[col.at]
      if z != "?":
        lst[col.at] = col.add(z)
    i.rows += [Row(i, lst)]

  def div(i, the, rows=None, cols=None):
    """Split the table in two according to the proximity
    of all examples from two distant points."""
    def gap(z1, z2):
      "Return gap between two examples>"
      return z1.dist(z2, the, cols)

    def far(r1):
      "Poke around a little looking for 2 distant points."
      a = [random.choice(rows or i.rows) for _ in range(the.sample)]
      a = sorted([(gap(r1, r2), r2) for r2 in a], key=lambda z: z[0])
      return a[-3][1]  # Ignore outliers.
    # -------------------------
    zero = random.choice(rows)
    one = far(zero)
    two = far(one)
    c = gap(one, two)
    tmp = {}
    for row in rows:
      a = gap(row, one)
      b = gap(row, two)
      tmp[id(row)] = (a**2 + c**2 - b**2) / (2 * c + 1E-31)
    rows.sort(key=lambda row: tmp[id(row)])
    mid = len(rows) // 2 # split at median point
    return one, two, rows[:mid], rows[mid:]

#---------------------------------------------------------------
def cluster(t, the, cols=None):
  "Recursively divide all data. Return one table per leaf."
  def go(rows, lvl=0):
    if type(lvl) == int:
      print('|.. ' * lvl, len(rows))
    if len(rows) < enough:
      out.append(t.clone(rows))
    else:
      _, _, lefts, rights = t.div(the, rows=rows, cols=cols)
      go(lefts, lvl + 1)
      go(rights, lvl + 1)

  enough = 2 * len(t.rows)**the.enough
  cols = cols or t.x
  out = []
  go(t.rows)
  return out

#---------------------------------------------------------------
def sway(t, the, cols=None):
  """Recursively divide data, pruning worse half at each step.
     Return one table containing the best leaf."""
  def go(rows, lvl=0):
    if type(lvl) == int:
      print('|.. ' * lvl, len(rows))
    if len(rows) < enough:
      [best.add(row) for row in rows]
    else:
      left, right, lefts, rights = t.div(the, rows=rows, cols=cols)
      if left < right: [rest.add(row) for row in rights]; go(lefts,  lvl + 1)
      else           : [rest.add(row) for row in lefts ]; go(rights, lvl + 1)

  enough = 2 * len(t.rows)**the.enough
  cols = cols or t.x
  best, rest = t.clone(), t.clone()
  go(t.rows)
  return best, rest

#---------------------------------------------------------------
class Contrast(o):
  def __init__(i,here,there,the):
    def top(n, pairs):
      return [x for _, x in sorted(pairs, reverse=True)[:n]] 
    n = len(here.rows) + len(there.rows)
    hs = {True: len(here.rows), False: len(there.rows)}
    f  = {(kl, (col1.txt, col1.at, span)): f
             for col1, col2 in zip(here.x, there.x)
             for f, kl, span in col1.discretize(col2, the)}
    tmp  = [Rule.canonical(rule) for rule in
             top(the.show,
                 [(i.value(combo, f, the), combo)
                  for combo in subsets(
                     top(the.top,sorted(i.solos(f,the),
                                            reverse=True)))])]
    i.all= list({str(rule): rule for rule in tmp}.values())

  def solos(i,f,the):
    pairs = []
    for kl, x in f:
      if kl == True:
        s = i.value([x],f,the)  # if zero, then skip x
        pairs += [(s, x)]
    return pairs

  def value(i,lst,f,the):
    def optimize(b,r): b**2/(b+r) if b>r else 0
    def monitor(b,r) : r**2/(b+r) if r>b else 0
    def safety(b,r)  : return 1/(b+r)
    f = [optimize,monitor,safety][the.goal]
    return f(i.like(lst, True,f,the), i.like(lst,False.f,the))

  def like(i, lst, kl,f,the):
    prod = math.prod
    prior = (hs[kl] + the.k) / (n + the.k * 2)
    fs = {}
    for txt, pos, span in lst:
      fs[txt] = fs.get(txt, 0) + f.get((kl, (txt, pos, span)), 0)
    like = prior
    for val in fs.values():
      like *= (val + the.m * prior) / (hs[kl] + the.m)
    return like

#--------------------------------------------------------
class Rule:
  # rule dict. one key per attribute, values per key
  def combineRanges(b4):
    if len(b4) == 1 and b4 == [(-math.inf, math.inf)]:
       return None
    j, tmp = 0, []
    while j < len(b4):
      a = b4[j]
      if j < len(b4)-1:
        b = b4[j+1]
        if a[1] == b[0]:
          a = (a[0], b[1])
          j += 1
      tmp += [a]
      j += 1
      return tmp if len(tmp) == len(b4) else combineRanges(tmp)

  def canonical(rule):
    cols = {}
    where={}
    for col, at, span in rule:
      where[col]=at
      cols[col] = cols.get(col, []) + [span]
    d = {}
    for k, v in cols.items():
      s = f"{k}"
      if v1 := Rule.combineRanges(sorted(v)):
        d[k] = v1
    return [(k,where[k],d[k])  for k in d]

  def show(rule):
    return ' and '.join([txt + ' (' + (' or '.join(map(showSpan, spans)) + ')')
                         for txt,_,spans in rule])


