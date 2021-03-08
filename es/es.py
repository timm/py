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
from etc import csv, obj, valley, sys
import functools, random, math, re
from types import FunctionType as fun

OPTIONS = dict(
  file  = ("auto93.csv", "csv file to load"),
  dir   = ("opt/data",   "path to data"),
  min   = (30,           "if less that 'min' data then use best=.5"),
  eg    = ("",           "run some demos"),
  egs   = (False,        "run all demos functions"),
  size  = (.5,            "initially, div nums into bins of size n**'size'"),
  cohen = (.3,           "subsequently, ignore differences less than 'cohen'*sd"),
  quiet = (False,        "dont print information text"),
  ls    = (False,        "list all demo functions"),
  lives = (128,          "rule repeats"),
  k     = (1,            "bayes low frequency k"),
  m     = (2,            "bayes low frequency m"),
  beam  = (10,           "keep 'beam' number of rules"),
  best  = (.25,          "how many best/worse samples to use"))

# convert 'OPTIONS' into the default options
THE     = obj(**{k:v for k,(v,_) in OPTIONS.items()})
# short cuts for maths
LO      = -1E32
HI      =  1E32
TINY    = 1/HI
# magic symbols in data files
NO      =  "?"
LESS    = "-"
MORE    = "+"
KLASS   = "!"
# standard class names
LOVE    = "better"
HATE    = "bad"

#------------------------------------------------------------
def Bin(down=LO, up=HI):
  """Models some x variable running 'down' to 'up' where, along
  the way, we see the y variables in 'also'."""
  i = obj(down=down, up=up, also=Sym())
  def has(i,x): return (x==i.down) if (i.down==i.up) else (i.down <= x < i.up)
  def show(i,short=False):  return (
    f"{i.up}"     if short and i.down == i.up  else (
    f"={i.up}"    if  i.down == i.up           else (
    f"anything"   if i.down == LO and i.up==HI else (
    f"<{i.up}"    if i.down == LO              else (
    f">={i.down}" if i.up == HI                else (
    f"{i.up}"     if short                     else (
    f"[{i.down}..{i.up})")))))))
  return i + locals()

#------------------------------------------------------------
def Skip(pos=0, txt=""):
  i = obj(pos=pos, txt=txt, n=0)
  def add(i,x):
    if x != NO: i.n += 1
    return x
  def mid(i):           assert False, "you talking to me?"
  def discretize(i,_,): assert False, "you talking to me?"
  return i + locals()

def Sym(pos=0,txt=""):
  i = obj(n=0, pos=pos, txt=txt, seen={},most=0,mode=None)
  # ---- Keep counts of seen symbols--------------
  def add(i,x, n=1):
    if x != NO:
      i.n += n
      tmp = i.seen[x] = n + i.seen.get(x,0)
      if tmp > i.most:
        i.most, i.mode = tmp, x
    return x
  # ---- stats reports -----------------------------
  def ent(i): return sum(-v/i.n * math.log(v/i.n) for v in i.seen.values())
  def mid(i): return i.mode
  def discretize(i,j,_): 
    return [Bin(k,k) for k in (i.seen | j.seen)]
  # --- merge spurious distinctions
  def removeSpuriousDistinctions(i,j):
    k     = i.merge(j)
    e1,n1 = i.ent(), i.n
    e2,n2 = j.ent(), j.n
    e,n   = k.ent(), k.n
    if e1+e2<0.01 or e*.95 < n1/n*e1 + n2/n*e2:
      return k
  def merge(i, j):
    k = Sym(pos=i.pos, txt=i.txt)
    for seen in [i.seen, j.seen]:
      for x,n in seen.items(): k.add(x,n)
    return k
  return i + locals()

def Num(pos=0,txt=""):
  "Stores numerics, sorted. Can report, median, sd, low, high etc"
  i = obj(n=0, pos=pos, txt=txt, _all=[], ok=False,
          w= -1 if LESS in txt else 1)
  # ---- Keep and report sorted contents --------------
  def _alls(i):
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
  def mid(i):    return _per(i,.5) if i._all else None 
  def _per(i,p): a= _alls(i); return a[int(p*len(a))]
  def norm(i,x): a= _alls(i); return (x-a[0])/(a[-1] - a[0] + TINY)
  def discretize(i,j,THE):
    xy  = [(better,LOVE) for better in i._all] + [(bad,HATE) for bad in j._all]
    tmp = div(xy, tooSmall=i.sd()*THE.cohen, width=len(xy)**THE.size)
    return merge(tmp)
  return i + locals()

#------------------------------------------------------------
 # ## Discretization
# Return `Bins`s generated by Splitting columns from `n` rows into 
# bins of size  `n**size`. Ignore small splits.
def div(xy, tooSmall=0.01, width=20):
  while width < 4 and width < len(xy) / 2: width *= 1.2
  xy = sorted(xy)
  now = Bin(xy[0][0], xy[0][0])
  out = [now]
  for j,(x,y) in enumerate(xy):
    if j < len(xy) - width:
      if now.also.n >= width:
        if x != xy[j+1][0]:
          if now.up - now.down > tooSmall:
            now  = Bin(now.up, x)
            out += [now]
    now.up = x
    now.also.add(y)
  out[ 0].down = LO
  out[-1].up   = HI
  return out
 
# Adjacent ranges that predict for sames goal are spurious.
# If  we can find any, merge them then look for any other merges.
def merge(b4):
  j, tmp, n = 0, [], len(b4)
  while j < n:
    a = b4[j]
    if j < n - 1:
      b  = b4[j+1]
      if c := a.also.removeSpuriousDistinctions(b.also):
        a = Bin(a.down, b.up)
        a.also = c
        j += 1
    tmp += [a]
    j   += 1
  return merge(tmp) if len(tmp) < len(b4) else b4
 
#------------------------------------------------------------
#  'Tab's store rows, summarized into 'Cols'
def Tab():
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

def Cols():
  """helper for Tab. Maintains columns grouped into 'all', 'x'', 'y'
  for all, independent, dependent attributes. Also, given column header
  names, this code creates the appropriate columns. Note one trick:
  if we are 'Skip'ing any column then it gets added to 'all' but
  not 'x' or 'y'. So to process all the columns except the 'Skip'ped ones,
  just use 'x' or 'y'"""
  i = obj(all=[], y=[], x=[], klass=None, head=[],named={}) 
  def updateFromRow(i, row): return [col.add(row[col.pos]) for col in i.all]  
  # ---- define goal types ------------------
  def _nump(x) : return LESS  in x or MORE in x or x[0].isupper()
  def _goalp(x): return KLASS in x or LESS in x or MORE in x
  # ---- initialize columns -------------------------------
  def cols(i, lst):
    i.head = lst
    i.all = [_col(i,n,x) for n,x in enumerate(lst)]
    return i.all
  def _col(i,pos,txt):
    what = Skip if NO in txt else (Num if _nump(txt)  else Sym)
    col  = what(pos, txt)
    ([] if NO in txt else (i.y if _goalp(txt) else i.x)).append(col)
    if KLASS in txt: i.klass = col
    i.named[txt] = col
    return col
  return i + locals()

#------------------------------------------------------------
def Counts(tab):
  i = obj(f={}, h={}, n=0)
  def _bin(x,bins):
    if x != NO:
      for b in bins:
        if b.has(x): return b
  def _count(rows, col, bins, label):
    for row in rows:
      if b := _bin(row[col.pos], bins):
        v= (label, col.txt, (b.down, b.up))
        i.f[v] = i.f.get(v,0) + 1
  #-----------------------------------
  def badBetter(i,THE):
    if len(tab.rows)*THE.best < THE.min: 
      THE.best = .5
      return i.bestRest(THE)
    rows = tab.ordered(THE)
    size = int(len(rows)*THE.best)
    better = tab.clone(rows[:size])
    bad    = tab.clone(rows[size:])
    i.n = len(rows)
    i.h[LOVE] = size
    i.h[HATE] = len(rows) - size
    for col1,col2, in zip(better.cols.x, bad.cols.x):
      bins = col1.discretize(col2, THE)
      THE.quiet or print(f"{col1.txt:>10} :", ', '.join([bin.show() for bin in bins]))
      _count(better.rows, col1, bins, LOVE)
      _count(bad.rows,    col1, bins, HATE)
    return i
  return i + locals()

#------------------------------------------------------------
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

  def value(rule, here, there):
    b = like(rule, here, 2)
    r = like(rule, there, 2)
    return b**2 / (b + r) if b > r else 0

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
        out[here] = loop([rule0(c, x, here, there) for c, x in all], here, there)
  return out

def selects(tab, rule):
  def selectors(val,ors):
    for (lo,hi) in ors:
      if lo <= val < hi:
        return True
    return False
  def selects1(row, ands):
    for (txt, ors) in ands:
      val = row[tab.cols.named[txt].pos]
      if val != NO:
        if not selectors(val,ors):
          return False
    return True
  s, rule = rule
  return tab.clone([row for row in tab.rows if selects1(row, rule)])

def showRule(r):
  def showRange(x): return Bin(x[0],x[1]).show()
  def show1(k, v): 
      return k + " " + ' or '.join(map(showRange, v)) 
  s, rule = r
  out = ""
  return ' and '.join([show1(k, v) for k, v in rule])

def es(x): return x
