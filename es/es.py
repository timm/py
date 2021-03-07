#!/usr/bin/env python3
# vim: filetype=python ts=2 sw=2 sts=2 et :
"""
es.py /əˈspī/ verb LITERARY see something that is hidden, or obscure.
Optimizer, written as a data miner.  
  
Break the data up into regions of 'bad' and 'better'. 'Interesting'
things occur at very different frequencies in 'bad' and 'better'.
Find interesting bits. Combine them. Repeat. Nearly all this
processing takes log linear time.
   
     :-------:                 explore  = better==bad
     | Ba    | Bad <----.      planning = max(better - bad)
     |    56 |          |      monitor  = max(bad - better)
     :-------:------:   |      tabu     = min(bad + better)
             | B    |   v
             |    5 | Better
             :------:
"""
from etc import csv, obj, valley
import functools, random, math, re
from types import FunctionType as fun

OPTIONS = dict(
  file  = ("auto93.csv", "csv file to load"),
  dir   = ("../data",    "path to data"),
  min   = (30,           "if less that 'min' data then use best=.5"),
  eg    = ("",           "run some demos"),
  egs   = (False,        "run all demos functions"),
  ls    = (False,        "list all demo functions"),
  lives = (128,          "rule repeats"),
  k     = (1,            "bayes low frequency k"),
  m     = (2,            "bayes low frequency m"),
  beam  = (10,           "keep 'beam' number of rules"),
  best  = (.25,          "how many best/worse samples to use"))

THE     = obj(**{k:v for k,(v,_) in OPTIONS.items()})
LO      = -1E32
HI      =  1E32
TINY    = 1/HI
NO      =  "?"
LESS    = "-"
MORE    = "+"
KLASS   = "!"

def Bin(down=LO, up=HI):
  i = obj(down=down, up=up)
  def has(i,x): return (x==i.down) if (i.down==i.up) else (i.down <= x < i.up)
  return i + locals()
 
def Num(pos=0,txt=""):
  "Stores numerics, sorted. Can report, median, sd, low, high etc"
  i = obj(n=0, pos=pos, txt=txt, _all=[], ok=False,
          w= -1 if LESS in txt else 1)
  # ---- Keep and report sorted contents --------------
  def _all(i):
    i._all = i._all if i.ok else sorted(i._all)
    i.ok = True
    return i._all
  def add(i,x):
    if x != NO:
      i.n    += 1
      i._all += [x]
      i.ok    = False
    return x
  # ---- stats reports -----------------------------
  def sd(i):     return (_per(i,.9) - _per(i,.1))/2.56
  def mid(i):    return _per(i,.5)
  def _per(i,p): a= _all(i); return a[int(p*len(a))]
  def norm(i,x): a= _all(i); return (x-a[0])/(a[-1] - a[0] + TINY)
  def discretize(i,j):
    div = valley(i.mid(), i.sd(), j.mid(), j.sd())
    if div < _per(i,.1) or div > _per(i,.9) :
       div = _per(i,.1 if div < i.mid() else .9)
    return [Bin(up=div), Bin(down=div)] 
  return i + locals()

def Sym(pos=0,txt=""):
  i = obj(n=0, pos=pos, txt=txt, seen={},most=0,mode=None)
  # ---- Keep counts of seen symbols--------------
  def add(i,x):
    if x != NO:
      i.n += 1
      tmp = i.seen[x] = 1 + i.seen.get(x,0)
      if tmp > i.most:
        i.most, i.mode = tmp, x
    return x
  # ---- stats reports -----------------------------
  def mid(i): return i.mode
  def discretize(i,j): 
    return [Bin(k,k) for k in (i.seen | j.seen)]
  return i + locals()

def Skip(pos=0, txt=""):
  i = obj(pos=pos, txt=txt, n=0)
  def add(i,x):
    if x != NO: i.n += 1
    return x
  return i + locals()

def Cols():
  i = obj(all=[], y=[], x=[], klass=None, head=[]) 
  def updateFromRow(i, row): return [col.add(row[col.pos]) for col in i.all]  
  # ---- define goal types ------------------
  def _nump(x) : return LESS  in x or MORE in x or x[0].isupper()
  def _goalp(x): return KLASS in x or LESS in x or MORE in x
  # ---- initialize columns -------------------------------
  def cols(i, lst):
    i.all = [_col(i,n,x) for n,x in enumerate(lst)]
    return i.all
  def _col(i,pos,txt):
    z = Skip if NO in txt else (Num if _nump(txt)  else Sym)
    z = z(pos, txt)
    ([] if NO in txt else (i.y if _goalp(txt) else i.x)).append(z)
    if KLASS in txt: i.klass = z
    i.head += [txt]
    return z
  return i + locals()

def Tab():
  "Rows summazied into `Col`s"
  i = obj(rows=[], cols=Cols())
  # ---- update with contents of rows ----------------
  def adds(i,rows): 
    for row in rows:
      if    i.cols.all: i.rows += [i.cols.updateFromRow(row)]
      else: i.cols.all = i.cols.cols(row)
    return i
  # --- sort rows on goal scores ---------------------
  def _better(i,r1,r2):
    s1,s2,n = 0,0,len(i.cols.y)
    for col in i.cols.y:
      pos,w = col.pos, col.w
      a,b   = r1[pos], r2[pos]
      a,b   = col.norm(a), col.norm(b)
      s1   -= math.e**(w*(a-b)/n)
      s2   -= math.e**(w*(b-a)/n)
    return s1/n < s2/n
  def ordered(i,THE): 
    gt= lambda a,b: 0 if id(a)==id(b) else (-1 if _better(i,a,b) else 1)
    return sorted(i.rows, key=functools.cmp_to_key(gt))
  # ---- factory to make a table like me ---------------
  def clone(i,inits=[]):
    j = Tab()
    j.cols.cols(i.cols.head)
    return j.adds(inits)
  # ---- report contents -------------------------------
  def ys(i):
    return [col.mid() for col in i.cols.y]
  return i + locals()

def Counts(tab):
  i = obj(f={}, h={}, n=0)
  def _inc(d, k): tmp = d[k] = d.get(k,0) + 1; return tmp
  def _bin(x,bins):
    if x != NO:
      for b in bins:
        if b.has(x): return b
  def _count(rows, col, bins, label):
    for row in rows:
      i.n += 1
      if b := _bin(row[col.pos], bins):
        _inc(i.h, label)
        _inc(i.f, (label, col.txt, (b.down, b.up)))
  #-----------------------------------
  def badBetter(i,THE):
    if len(tab.rows)*THE.best < THE.min: 
      THE.best = .5
      return i.bestRest(THE)
    rows = tab.ordered(THE)
    size = int(len(rows)*THE.best)
    better = tab.clone(rows[:size])
    bad    = tab.clone(rows[size:])
    for col1,col2, in zip(better.cols.x, bad.cols.x):
      bins = col1.discretize(col2)
      _count(better.rows, col1, bins, "better")
      _count(bad.rows,    col1, bins, "bad")
    return i
  return i + locals()

def Learn(counts, THE):
  r = random.random
  def loop(rules, here, there):
    lives = THE.lives
    while True:
      lives -= 1
      total, rules = prune(rules)
      if lives < 1 or len(rules) < 2:
        return rules
      rules += [combine(pick(rules, total), pick(rules, total), here, there)]

  def value(rule, here, there, e=2):
    b = like(rule, here, 2)
    r = like(rule, there, 2)
    return b**e / (b + r) if b > r else 0

  def like(rule, h, hs=None):
    hs   = hs if hs else len(counts.h)
    like = prior = (counts.h[h] + THE.k) / (counts.n + THE.k * hs)
    like = math.log(like)
    for col, values in rule:
      f = sum(counts.f.get((h, col, v), 0) for v in values)
      inc = (f + THE.m * prior) / (counts.h[h] + THE.m)
      like += math.log(inc)
    return math.e**like

  def combine(rule1, rule2, here, there):
    val1, rule1 = rule1
    val2, rule2 = rule2
    tmp = dict()
    for rule in [rule1, rule2]:
      for k, lst in rule:
        tmp[k] = tmp.get(k, set())
        for v in lst: tmp[k].add(v)
    rule3 = sorted([[k, sorted(list(vs))] for k, vs in tmp.items()])
    val3 = value(rule3, here, there)
    return [val3, rule3]

  def same(rule1, rule2):
    if rule1[0] != rule2[0]: return False
    for x, y in zip(rule1[1], rule2[1]):
      if x != y: return False
    return True

  def prune(old):
    ordered = [[s, r] for s, r in sorted(old, reverse=True)]
    one = ordered[0]
    unique = [one]
    for two in ordered[1:]:
      if not same(one, two): unique += [two]
      one = two
    pruned = [[s, r] for s, r in unique if s > 0][:THE.beam]
    return sum(s for s, _ in pruned), pruned

  def pick(rules, total):  # (s1, r1) (s2,r2) (s3,r3) total=s1+s2+s3
    n = r()
    for rule in rules:
      n -= rule[0] / total
      if n <= 0: return rule
    return rule

  def rule0(c, x, here, there):
    rule = [[c, [x]]]
    return [value(rule, here, there), rule]

  out, all = {}, list(set([(c, x) for (_, c, x) in counts.f]))
  for there in counts.h:
    for here in counts.h:
      if here != there:
        rules = loop([rule0(c, x, here, there) for c, x in all], here, there)
        out[here] = [[value(r, here, there, 1), r] for _, r in rules]
  return out

def selects(tab, rows, rule):
  def selects1(row, ands):
    for (txt, pos), ors in ands:
      val = cell(t.cols[txt], row)
      if val:
        if val not in ors:
          return False
    return True
  s, rule = rule
  return [row for row in rows if selects1(row, rule)]

def showRule(r):
  def showRange(l):
    a,b = l
    if a==b : return f"={a}"
    if a==LO: return f"<={b}"
    if b==HI: return f">={a}"
    return f"[{a}..{b})"
  def show1(k, v):
    return k + " " + ' or '.join(map(showRange, v)) 
  s, rule = r
  out = ""
  return f"{int(100*s)} " + f' and '.join([show1(k, v) for k, v in rule])

def es(x): return x
