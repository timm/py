import math,re

LO = -math.inf
HI =  math.ing
NO =  "?"
LESS = "<"
MORE = ">"

class it:
  def __init__(i, **d)   : i.__dict__.update(d)
  def __add__(i,j): 
    for k in j.__dict__: i[k] = j[k]
    return i
  def __repr__(i): 
    return "{"+ ', '.join(
           [f":{k} {v}" for k, v in sorted(i.__dict__.items()) 
                        if type(v)!=fun and k[0] != "_"])+"}"
  def has(i,d):
    def method(f): return lambda *lst, **kw: f(i, *lst, **kw)
    for k,v in d.items():
      if k[0] != "_":
        if type(v)==fun: i.__dict__[k] = method(v)
    return i

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
  return i.has(locals())

def Sym(pos=0,txt=""):
  i = it(n=0, pos=pos, txt=txt, seen={},most=0,mode=None)
  def add(i,x):
    if x != NO:
      i.n    += 1
      tmp = i.seen[x] = 1 + i.seen(x,0)
      if tmp > i.most:
        i.most, i.mode = tmp, x
  return i.has(locals())

def Skip(pos=0, txt=""):
  i = it(pos=pos, txt=txt, n=0,seen={},most=0,mode=None)
  def add(i,x):
    if x != NO:
      i.n    += 1
      tmp = i.seen[x] = 1 + i.seen(x,0)
      if tmp > i.most:
        i.most, i.mode = tmp, x
  return i.has(locals())

def Cols():
  i = it(all=[], y=[], x=[]) 
  def add(i,pos,txt):
    if   NO in txt                                     : f = Skip
    elif LESS in txt or MORE in txt or txt[0].isupper(): f = Num
    else                                               : f = Sym
    now = f(pos=pos, txt=txt, w=-1 if LESS in txt else 1)
    if   SKIP in txt                                   : also = []
    elif LESS in txt or MORE in txt                    : also = i.y
    else                                               : also = i.x
    also  += [now]
    return now
  return i.has(locals())

