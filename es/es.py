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

def Bin(down=LO, up=HI):
  i = obj(down=down, up=up)
  def has(i,x): return (x==i.down) if (i.down==i.up) else (i.down <= x < i.up)
  return i + locals()
 
def Num(pos=0,txt=""):
  "Stores numerics, sorted. Can report, median, sd, low, high etc"
  i = obj(n=0, pos=pos, txt=txt, _all=[], ok=False,
          lo=HI, hi=LO, w= -1 if LESS in txt else 1)
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
      i.hi    = max(i.hi,x)
      i.lo    = min(i.lo,x)
    return x
  # ---- stats reports -----------------------------
  def sd(i):     return (_per(i,.9) - _per(i,.1))/2.56
  def mid(i):    return _per(i,.5)
  def _per(i,p): a=_all(i); return a[int(p*len(a))]
  def norm(i,x): a=_all(i); return (x-a[0])/(a[-1] - a[0] + 10**-64)
  def discretize(i,j):
    div = valley(i.mid(), i.sd(), j.mid(), j.sd())
    if div < i.per(.1) or div > i.per(.9) :
       div = i.per(.1 if div < i.mid() else .9)
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
  def updateFromRow(i, row): return [col.add(ros[col.pos]) for col in i.all]  
  # ---- define goal types ------------------
  def _nump(x) : return LESS  in x or MORE in x   or x[0].isupper()
  def _goalp(x): return KLASS in x or LESS in txt or MORE in txt
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
  def sorted(i,THE): 
    gt= lambda a,b: 0 if id(a)==id(b) else (1 if _better(i,a,b) else -1)
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
