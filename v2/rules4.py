#!/usr/bin/env python3.9
# vim: ts=2 sw=2 sts=2 et :

import re,sys,copy
from collections import defaultdict

def think( 
  Data:      "where to read data"                  = "../data/auto2.csv",
  Beam:      "focus on this many"                  = 20,
  Fars:      "how many  times to find far things?" = 20,
  FarEnough: "where to look for far things"        = .9,
  K   :      "bayes low frequency hack"            = 2,
  M   :      "bayes low frequency hack"            = 1,
  P   :      "distance coeffecient"                = 2,
  Cols:       "columns to use for inference"       = "x"):
  
  class Col(o):
      def __init__(i,at=0,txt=""): 
        i.at,i.txt,i.w = at,txt,-1 if "-" in txt else 1
      def add(i,x,n=1):
        if x!="?": i.n += 1; x= i.add1(x,n)
        return x
    
  class Skip(o):
      def __init__(i,*l,**kw):  super().__init__(*l,**kw)
      def add1(i,x,n=1)      : return x
  
  class Sym(Col):
    def __init__(i,*l,**kw): i.has={}; super().__init__(*l,**kw)
    def add1(i,x,n=1)      : inc(i.has,x,n); return  x 
    def dist(i,x,y)        : return 0  if x==y else 1
  
  class Num(Col):
    def __init__(i,**kw): i._all,i.ok=[],False; super().__init__(**kw)
    def add1(i,x)       : x=float(x); i.ok=False; i._all +=[x]; return x
    def all(i)          : 
      if not i.ok: i.ok=True;i._all.sort(); return i._all
    def norm(i,x): 
      if x=="?": return x
      a = i.all()
      return max(0, min(1, (x - first(a))/ (last(a) - first(a) + 1E-32)))
    def dist(i,x,y):
      if   x=="?": y= i.norm(y); x= 1 if y<0.5 else 0
      elif y=="?": x= i.norm(x); y= 1 if x<0.5 else 0
      else       : x,y = i.norm(x), y.norm(y)
      return abs(x-y)

  class Row(o):
    def __init__(i,lst,tab=None): i.tab, i.cells = tab, lst
    def dist(i,j):
      d= n= 1E-32
      for col in i.tab.cols[Cols]:
        n += 1
        x,y = i.cells[at], j.cells[at]
        d += 1 if  x=="?" and y=="?" else col.dist(x,y)^P
      return (d/n)^(1/P)
    def far(i,rows):
      tmp= [(dist(i,j),j)  for _ in  range(Fars)]
      return per(sorted(tmp, key=forst), FarEnough)

  def table(src):
    t = o(rows  = [],
          counts= o(n=0, klass={},  freq={}),
          cols  = o(all=[], names=None, x=[], y=[], klass=None))
    def num(x)   : return x[0].isupper()
    def skip(x)  : return "?" in x 
    def goal(x)  : return "-" in x or "+" in x or "!" in x
    def header(a):
      t.names= a
      for at,x in enumerate(a):
        new = (Skip if skip(x) in x else (Num if num(x) else Sym))(at,x)
        t.cols.all += [new]
        if skip(x): continue
        t.cols["y" if goal(x) else "x"] += [new]
    def count(a):
      if not t.cols.klass: return
      kl= a[t.cols.klass]
      t.counts.n += 1
      inc(t.klass, kl)
      for col in t.col.x:
        v= a[col.at]
        if v != "?": inc(t.counts.freq, (klass,(col.pos,(val,val))))
    def data(a):
      a= row.cells if type(a)==Row else a
      a= [col.add(a[col.pos]) for col in t.cols.all]
      t.rows += [Row(a, tab=t)]
      count(a)
    # --- --- --- --- ---
    [data(a) if t.cols.names else header(a) for a in src]
    return t
   
  def clone(t,inits=[]):
    return table([[t.names]] + inits)

#----------------------------
# misc utils

def first(a)    : return a[0]
def last(a)     : return a[-1]
def inc(d,k,n=1): tmp= d[k]= n + d.get(k,0); return tmp
def has(d,k)    : return d.get(k,0)
def per(a,p=.5) : return a[p*len(a)//1]

def subsets(l):
  out = [[]]
  for x in l: out += [sub + [x] for sub in out]
  return out[1:] 

class o(object):
  def __init__(i, **k): i.__dict__.update(**k)
  def __repr__(i): return i.__class__.__name__ + str(
    {k:v for k,v in i.__dict__.items() if k[0] != "_"})

def csv(f=None, sep=","):
  def prep(s): return re.sub(r'([\n\t\r ]|#.*)', '', s)
  if f:
    with open(f) as fp:
      for s in fp:
        if s := prep(s): yield s.split(sep)
  else:
    for s in sys.stdin:
      if s := prep(s): yield s.split(sep)


