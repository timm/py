# vim: filetype=python ts=2 sw=2 sts=2 et :


import functools, math, re
from types import FunctionType as fun

LO    = -math.inf
HI    =  math.inf
NO    =  "?"
LESS  = "-"
MORE  = "+"
KLASS = "!"

def es(x): return x

class it:
  def __init__(i, **d)   : i.__dict__.update(d)
  def __repr__(i): 
    return "{"+ ', '.join(
           [f":{k} {v}" for k, v in sorted(i.__dict__.items()) 
                        if type(v)!=fun and k[0] != "_"])+"}"
  def __add__(i,d):
    def method(i,f): return lambda *lst, **kw: f(i, *lst, **kw)
    for k,v in d.items():
      if k[0] != "_":
        if type(v)==fun: i.__dict__[k] = method(i,v)
    return i

THE = it(m=1, k=2, best=50, size=0.5)

def csv(file):
  def atom(x):
    try: return float(x)
    except Exception: return x
  with open(file) as fp:
    for line in fp: 
      line = re.sub(r'([\n\t\r ]|#.*)','',line)
      if line:
         yield [atom(x) for x in line.split(",")]

def Num(pos=0,txt=""):
  i = it(n=0, pos=pos, txt=txt, mu=0, m2=0, sd=0, 
         lo=HI, hi=LO, w= -1 if LESS in txt else 1)
  def add(i,x):
    if x != NO:
      i.n    += 1
      delta   = x - i.mu
      i.mu   += delta/i.n
      i.m2   += delta*(x - i.mu)
      i.sd    = (i.m2/i.n)**0.5
      i.hi    = max(i.hi,x)
      i.lo    = min(i.lo,x)
    return x
  def like(i,x, *l):
    var   = i.sd**2
    denom = (math.pi*2*var)**.5
    num   = math.e**(-(x-i.mu)^2/(2*var+0.0001))
    return num/(denom + 10^-64)
  def norm(i,x): return (x - i.lo) / (i.hi - i.lo +10**-64)
  return i + locals()

def Sym(pos=0,txt=""):
  i = it(n=0, pos=pos, txt=txt, seen={},most=0,mode=None)
  def add(i,x):
    if x != NO:
      i.n += 1
      tmp = i.seen[x] = 1 + i.seen.get(x,0)
      if tmp > i.most:
        i.most, i.mode = tmp, x
    return x
  def like(i,x, THE, prior, ns):
    return (i.seen.get(x,0) + THE.m*prior)/ (ns + THE.m)
  return i + locals()

def Skip(pos=0, txt=""):
  i = it(pos=pos, txt=txt, n=0)
  def add(i,x):
    if x != NO: i.n    += 1
    return x
  return i + locals()

def Cols():
  i = it(all=[], y=[], x=[], klass=None, head=[]) 
  def add(i,pos,txt):
    nump  = lambda x: LESS  in x or MORE in x   or x[0].isupper()
    goalp = lambda x: KLASS in x or LESS in txt or MORE in txt
    also  = ([]   if NO in txt else (i.y if goalp(txt) else i.x))
    k     = (Skip if NO in txt else (Num if nump(txt)  else Sym))
    z     = k(pos,txt) 
    if KLASS in txt: i.klass = z
    also += [z]
    i.head += [txt]
    return z
  return i + locals()

def Tab():
  i = it(rows=[], cols=Cols())
  def adds(i,src): 
    for lst in src:
      if i.cols.all: i.rows += [[col.add(lst[col.pos]) for col in i.cols.all]]
      else         : i.cols.all = [i.cols.add(n,x) for n,x in enumerate(lst)]
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
  def extremes(i,THE): 
    def gt(a,b): return 0 if id(a)==id(b) else (-1 if i.better(a,b) else 1)
    rows =  sorted(i.rows, key=functools.cmp_to_key(gt))
    best = min(THE.best,len(rows)//2)
    return rows[:best], rows[-best:]
  return i + locals()
