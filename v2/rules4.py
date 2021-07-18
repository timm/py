#!/usr/bin/env python3.9
# vim: ts=2 sw=2 sts=2 et :
import re, sys, copy, argparse

def think( 
  Bins     :"bins are of size n**Bins"    = .5,
  Cols     :"columns to use for inference"= "x",
  Data     :"where to read data"          = "../data/auto2.csv",
  Epsilon  :"small = sd**Epsilon"         = .3,
  FarEnough:"where to look for far things"= .9,
  Goal     :"learning  goals"             = "optimize",
  K        :"bayes low frequency hack"    = 2,
  M        :"bayes low frequency hack"    = 1,
  P        :"distance coeffecient"        = 2,
  Sample   :"#samples to find far things?"= 20,
  Verbose  :"set verbose"                 = False,
  Top      :"focus on this many"          = 20):

  """(c) Tim Menzies <timm@ieee.org>, 2021, unlicense.org
Describing the difference between things is often 
shorter than describing each thing, separately."""

  class Col(o):
    def __init__(i,at=0,txt=""): 
      i.n, i.at, i.txt, i.w = 0, at, txt, -1 if "-" in txt else 1
    def add(i,x,n=1):
      if x!="?": i.n += 1; x= i.add1(x,n)
      return x
    
  class Skip(Col):
    def __init__(i,**kw):  super().__init__(**kw)
    def add1(i,x,n=1)      : return x
  
  class Sym(Col):
    def __init__(i,**kw): i.has={}; super().__init__(**kw)
    def add1(i,x,n=1)      : inc(i.has,x,n); return  x 
    def dist(i,x,y)        : return 0  if x==y else 1
    def ent(i): 
      return sum(-v / i.n * math.log(v / i.n) for v in i.has.values())
    def merge(i, j):
      k = Sym(at=i.at, txt=i.txt)
      [k.add(x,n) for seen in [i.has, j.has] for x, n in seen.items()]
      return k
    def merged(i, j):
       k = i.merge(j)
       e1,n1, e2,n2, e,n = i.ent(),i.n, j.ent(),j.n, k.ent(),k.n
       if e1 + e2 < 0.01 or e * .95 < n1 / n * e1 + n2 / n * e2:
         return k
    def bins(i, j):
      for k in (i.has | j.has): 
        yield i.has.get(k,0), True, (i.at, (k,k))
        yield j.has.get(k,0), False,(j.at, (k,k))
    
  class Num(Col):
    def __init__(i,**kw): 
      i._all,i.ok=[],False; super().__init__(**kw)
    def all(i)          : 
      if not i.ok: i.ok=True;i._all.sort(); return i._all
    def span(i): 
      return (first(i.all()), last(i.all()))
    def wide(i,epsilon=0):
      return last(i.all()) - first(i.all()) >= epsilon
    def sd(i):
      a=i.all(); return (per(a,.9) - per(a,.1))/2.56
    def add1(i,x,n)     : 
      x, i.ok  = float(x), False  
      for _ in range(n): i._all +=[x] 
      return x
    def norm(i,x): 
      if x=="?": return x
      a = i.all()
      return max(0,min(1,(x-first(a))/(last(a)-first(a)+1E-32)))
    def dist(i,x,y):
      if   x=="?": y= i.norm(y); x= 1 if y<0.5 else 0
      elif y=="?": x= i.norm(x); y= 1 if x<0.5 else 0
      else       : x,y = i.norm(x), y.norm(y)
      return abs(x-y)
    def bins(i, j):
      xy= [(z,True) for z in i._all]+[(z,False) for z in j._all]   
      sd= (i.n*i.sd() + j.n*j.sd()) / (i.n + j.n)
      for ((lo,hi),sym) in bins(xy, epsilon = sd*Epsilon, 
                                    enough  = len(xy)**Bins):
        for klass, n in sym.has.items():
          yield n, klass, (i.at,(lo,hi))

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
      tmp= [(dist(i,j),j) for _ in  range(Sample)]
      return per(sorted(tmp, key=forst), FarEnough)

  def bins(xy,epsilon=0,enough=30):
    def merge(b4):
      j, tmp, n = 0, [], len(b4)
      while j < n:
        a = b4[j]
        if j < n - 1:
         b = b4[j + 1]
         if cy := a.y.merged(b.y):
           a = o(x=(a.x[0],b.x[1]),y=cy)
           j += 1
        tmp += [a]
        j += 1
      return merge(tmp) if len(tmp) < len(b4) else b4
    #--- --- --- --- ---
    xy   = sorted(xy, key=first)
    bins = [o(x=Num(),y=Sym())]
    for i,(x,y) in enumerate(xy):
      if x != b4:
        if last(bins).x.n >= enough:
          if i < len(xy) - enough:
            if last(bins).x.wide(epsilon):
              bins += [o(x=Num(),y=Sym())]
      last(bins).x.add(x)
      last(bins).y.add(y)
      b4 = x
    return merge([o(bin.x.span(), y=bin.y) for bin in bins])      
 
   def optimize(b,r): return b**2/(b+r)
   def monitor(b,r) : return r**2/(b+r)
   def explore(b,r) : return 1/(b+r)
   todo = dict(optimize=optimize,monitor=monitor,explore=explore)

   def contrasts(here, there, t):
    counts = {(kl,(at,(lo,hi))): f
              for col1,col2 in zip(here.cols.x, there.cols.x)
              for f, kl, (at, (lo,hi)) in col1.bins(col2)}

    n = len(here.rows, there.rows)
    hs= {True: len(here.rows), False: len(here.rows)}
    def like(d, kl):
      like = prior = (hs[kl] + K) / (n + K*2)
      for at,span in d.items():
        f     = counts.get((kl,(at,span)),0)  
        like *= (f + M*prior) / (hs[kl] + M)
      return like
    def value(d): return todo[Goal](like(d,True),like(d,False)),d
    def top(sx) : return [x for _, x in 
                          sorted(sx, key=first, reverse=True)[:Top]]
    #--- --- --- --- --- ---
    solos= [value(dict(at=x)) for at,x in set([z for _,z in counts])]
    for combo in subsets(top(solos)):
      for rule in combine(combo): pass
 
      
  #--- --- --- --- ---
  if Data:
    print(table(csv(Data),Row,Num,Sym,Skip).cols.all[1])

#--------------------------------------------------
# misc utils. things that don't use the config vars

def first(a)    : return a[0]
def last(a)     : return a[-1]
def inc(d,k,n=1): tmp= d[k]= n + d.get(k,0); return tmp
def has(d,k)    : return d.get(k,0)
def per(a,p=.5) : return a[ p*len(a)//1 ]

def subsets(l):
  out = [[]]
  for x in l: out += [sub + [x] for sub in out]
  return out[1:] 

class o(object):
  def __init__(i, **k)  : i.__dict__.update(**k)
  def __getitem__(i,k)  : return i.__dict__[k]
  def __setitem__(i,k,v): i.__dict__[k] = v
  def __repr__(i)       : return i.__class__.__name__ + str(
    {k:v for k,v in sorted(i.__dict__.items()) if k[0] != "_"})

def csv(f=None, sep=","):
  def prep(s): return re.sub(r'([\n\t\r ]|#.*)', '', s)
  if f:
    with open(f) as fp:
      for s in fp:
        if s := prep(s): yield s.split(sep)
  else:
    for s in sys.stdin:
      if s := prep(s): yield s.split(sep)

def table(src,row,num,sym,skip):
  t = o(rows= [],
        cols= o(all=[], names=[], x=[], y=[], klass=None))
  def nump(x)   : return x[0].isupper()
  def skipp(x)  : return "?" in x 
  def goalp(x)  : return "-" in x or "+" in x or klassp(x)
  def klassp(x) : return "!" in x
  def header(a):
    t.cols.names= a
    for at,x in enumerate(a):
      new = skip if skipp(x) else (num if nump(x) else sym)
      new = new(at=at,txt=x)
      t.cols.all += [new]
      if skipp(x): continue
      t.cols["y" if goalp(x) else "x"] += [new]
      if klassp(x): 
        t.cols.klass= new
  def data(a):
    a= a.cells if type(a)==row else a
    a= [col.add(a[col.at]) for col in t.cols.all]
    t.rows += [row(a, tab=t)]
  # --- --- --- --- ---
  [data(a) if t.cols.names else header(a) for a in src]
  return t
 
def clone(t,inits=[]):
  return table([[t.names]] + inits)

def cli(f):
  p= argparse.ArgumentParser(prog= "./"+f.__name__+".py",
      formatter_class=argparse.RawTextHelpFormatter, description=f.__doc__)
  for (k,h),b4 in zip(list(f.__annotations__.items()),f.__defaults__):
    if b4==False:
      p.add_argument("-"+(k[0].lower()), dest=k, help=h, 
                     default=False, action="store_true")
    else:
      p.add_argument("-"+(k[0].lower()), dest=k, 
                     help= h+" ["+str(b4)+"]",
                     default=b4, type=type(b4), metavar=k)
  f(**p.parse_args().__dict__)

if __name__ == "__main__":
  cli(think)
