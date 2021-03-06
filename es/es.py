#!/usr/bin/env python3
# vim: filetype=python ts=2 sw=2 sts=2 et :
"""
espy
"""
from etc import obj, valley
import functools, math, re
from types import FunctionType as fun

OPTIONS= dict(
  file  = ("auto93.csv", "csv file to load"),
  dir   = ("../data",    "path to data"),
  best  = (50,           "how many best/worse samples to use"),
  size  = (.7,           "spliit 'n' numbers into bins of size 'n**size'"))

THE   = obj(**{k:v for k,(v,_) in OPTIONS.items()})
LO    = -math.inf
HI    =  math.inf
NO    =  "?"
LESS  = "-"
MORE  = "+"
KLASS = "!"

def Span(down=LO, up=HI):
  # Keep 
  i = obj(down=down, up=up)
  def has(i,x): 
    return (x==i.down) if (i.down==i.up) else (i.down <= x < i.up)
  return i + locals()
 
def Num(pos=0,txt=""):
  "Stores numerics, sorted. Can report, median, sd, low, high etc"
  i = obj(n=0, pos=pos, txt=txt, _all=[], ok=False,
         lo=HI, hi=LO, w= -1 if LESS in txt else 1)
  # ---- Keep and report sorted contents --------------
  def all(i):
    i._all = i._all if i.ok else sorted(i._all)
    i.ok = True
    return i._all
  def add(i,x):
    if x != NO:
      i.n    += 1
      i._all += [x]
      i.ok    = False
      i.hi    = max(i.hi,x)
      i.lo    = min(i.lo,x)
    return x
    return i._all
  # ---- stats reports -----------------------------
  def sd(i):     return (i.per(.9) - i,per(.1))/2.56
  def mid(i):    return i.per(.5)
  def per(i,p):  a=i.all(); return a[int(p*len(a))]
  def norm(i,x): a=i.all(); return (x-a[0])/(a[-1] - a[0] + 10**-64)
  # ---- Discretize ------------------------------
  def spans(i,j):
    div = valley(i.mid(), i.sd(), j.mid(), j.sd())
    if div < i.per(.1) or div > i.per(.9) :
       div = i.per(.1 if div < i.mid() else .9)
    return [Span(up=div), Span(down=div)] 
  return i + locals()

def Sym(pos=0,txt=""):
  i = obj(n=0, pos=pos, txt=txt, seen={},most=0,mode=None)
  def add(i,x):
    if x != NO:
      i.n += 1
      tmp = i.seen[x] = 1 + i.seen.get(x,0)
      if tmp > i.most:
        i.most, i.mode = tmp, x
    return x
  def mid(i): return i.mode
  def spans(i,j): 
    return [Span(k,k) for k in (i.seen | j.seen)]
  return i + locals()

def Skip(pos=0, txt=""):
  i = obj(pos=pos, txt=txt, n=0)
  def add(i,x):
    if x != NO: i.n += 1
    return x
  return i + locals()

def Cols():
  i = obj(all=[], y=[], x=[], klass=None, head=[]) 
  def adds(i, lst): 
    return [col.add(lst[col.pos]) for col in i.all]  
  def cols(i, lst):
    i.all = [i.col(n,x) for n,x in enumerate(lst)]
    return i.all
  def col(i,pos,txt):
    nump  = lambda x: LESS  in x or MORE in x   or x[0].isupper()
    goalp = lambda x: KLASS in x or LESS in txt or MORE in txt
    z     = (Skip if NO in txt else (Num if nump(txt)  else Sym))(pos,txt)
    if KLASS in txt: i.klass = z
    ([] if NO in txt else (i.y if goalp(txt) else i.x)).append(z)
    i.head += [txt]
    return z
  return i + locals()

def Tab():
  i = obj(rows=[], cols=Cols())
  def adds(i,src): 
    for lst in src:
      if    i.cols.all: i.rows += [i.cols.adds(lst)]
      else: i.cols.all = i.cols.cols(lst)
    return i
  def better(i,r1,r2):
    s1,s2,n = 0,0,len(i.cols.y)
    for col in i.cols.y:
      pos,w = col.pos, col.w
      a,b   = r1[pos], r2[pos]
      a,b   = col.norm(a), col.norm(b)
      s1   -= math.e**(w*(a-b)/n)
      s2   -= math.e**(w*(b-a)/n)
    return s1/n < s2/n
  def poles(i,THE): 
    gt   = lambda a,b: 0 if id(a)==id(b) else (1 if i.better(a,b) else -1)
    i.rows = sorted(i.rows, key=functools.cmp_to_key(gt))
    best = min(THE.best,len(i.rows)//2)
    return i.rows[:best], i.rows[-best:]
  def clone(i,inits=[]):
    j = Tab()
    j.cols.cols(i.cols.head)
    j.adds(inits)
    return j
  def ys(i):
    return [col.mid() for col in i.cols.y]
  return i + locals()

def Counts(THE,tab):
  i = obj(counts={}, klasses={}, totals={})
  def count(tab1, label):
    def bin(x,bins):
      if x != NO:
        for b in bins:
          if b.has(x): return b
    def inc(d,k): d[k] = d.get(k,0) + 1
    for row in rows:
      if b := bin(row[col.pos], bins):
        inc(out.klasses, label)
        inc(out.totals,  col.pos)
        inc(out.counts,  (label, col.txt, b.down, b.up))
  ############################
  nth,sth = tab.poles(THE)
  nth,sth = tab.clone(nth), tab.clone(sth)
  for nthcol,sthcol, in zip(nth.cols.x, sth.cols.x):
    spans = nthcol.spans(sthcol)
    count(nth,"best")
    count(sth,"rest")
    return out

def es(x): return x
