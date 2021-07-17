#!/usr/bin/env python3.9
# vim: ts=2 sw=2 sts=2 et :

import re,sys,copy
from collections import defaultdict

def think( 
  Beam: "focus on this many" = 20,
  K   : "bayes low frequency hack" = 2,
  M   : "bayes low frequency hack" = 1,
  P   : "distance coeffecient" = 2,
  Cols: "columns to use for inference"  = "x"):
  
  class o(object):
    def __init__(i, **k): i.__dict__.update(**k)
    def __repr__(i): return i.__class__.__name__ + str(
          {k: v for k, v in i.__dict__.items() if k[0] != "_"})
  
  class Col(o):
      def __init__(i,at=0,txt=""):
        i.at,i.txt,i.w = at,txt,-1 if "-" in txt else 1
      def add(i,x,n=1):
        if x!="?": i.n+1; i.add1(x,n)
        return x
    
  class Skip(o):
      def __init__(i,*l,**kw):  super().__init__(*l,**kw)
      def add1(i,x,n=1)      : return x
  
  class Sym(Col):
    def __init__(i,*l,**kw): i.has={}; super().__init__(*l,**kw)
    def add1(i,x,n=1)      : inc(i.has,n)
    def dist(i,x,y)        : return 0  if x==y else 1
  
  class Num(Col):
    def __init__(i,**kw): i._all,i.ok=[],False; super().__init__(**kw)
    def add1(i,x)       : i.ok=False; i._all +=[x] 
    def per(i,p=.5)     : a=i.all(); return a[p*len(i._all)//1]
    def all(i)          : 
      if not i.ok: i.ok=True;i._all.sort()
      return i._all
    def norm(i,x): 
      if x=="?": return x
      a = i.all()
      tmp = (x-a[0])/ (a[-1] - a[0] + 1E-32)
      return  max(0,min(1,x))

  class Row(o):
    def __init__(i,lst,tab=None): i.tab,i.cells=tab,lst
    def dist(i,j,t):
      d=n=1E-32
      for col in i.tab.col[Cols]:
        n += 1
        x,y = i.cells[at], j.cells[at]
        if    x=="?" and y=="?": 
              d += 1
        else: d += (col.dist(x,y))**P
      return (d/n)^(1/P)

  def table(src):
    t = o(rows   = [],
          counts = o(all=0, klass={},  freq={}),
          cols   = o(all=[],names=names,x=[],y=[], klass=None))
    def header(a):
      t.names=a
      for at,x in enumerate(a):
        kl = Skip if "?" in x else (Num if x[0].isupper() else Sym)
        what = kl(at=at,txt=x)
        t.cols.all += [what]
        if type(what) != Skip:
          lst=t.cols.y if "-" in x or "x" in x or "!" in x else t.cols.x
          lst += [what]
    def data(a):
      a = row.cells if type(a)==Row else a
      for col in t.cols.all: col.add(a[col.pos])
      t.rows += [Row(a)]
      if t.cols.klass:
        kl = a[t.cols.klass]
        t.counts.all+=1
        inc(t.klass,kl)
        for col in t.col.x:
          v= a[col.at]
          if v != "?":
            inc(t.counts.freq,(klass,(col.pos,(val,val))))
    #-----------------------
    for lst in src:
      data(a) if t.cols.names else header(a)
    return t
   
  def clone(t,inits=[]):
    return table([[t.names]] + inits)

  #----------------------------
  # misc utils
  
  def inc(d,k,n=1): tmp= d[k] = n + d.get(k,0); return tmp

  def has(d,k): return d.get(k,0)

  def subsets(l):
    out = [[]]
    for x in l: out += [sub + [x] for sub in out]
    return out[1:] 

  def csv(f=None,sep=","):
    def prep(s)  : return re.sub(r'([\n\t\r ]|#.*)', '', s)
    def ako(s):
      try : return int(s)
      except:
        try : return float(s)
        except: return s
    if f:
      with open(f) as fp:
        for s in fp:
          if s := prep(s): yield [ako(x) for x in s.split(sep)]
    else:
      for s in sys.stdin:
        if s := prep(s): yield [ako(x) for x in s.split(sep)]
  
  
