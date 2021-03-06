import re
from collections import Counter

class o(object):
  def __init__(i, **k): i.__dict__.update(**k)
  def __repr__(i):
    return i.__class__.__name__ + str(
        {k: v for k, v in i.__dict__.items() if k[0] != "_"})

class Rule:
  def __init__(i): i._ready=False
  def __repr__(i): i.ready(); return i.show()
  def ready(i):
    if not i._ready:
      i._ready=True
      i.ready1()
    return  len(i.has)> 0
  
def And(Rule):
  def __init__(i,*l,**kw):
    i.has={}
    super().__init__(*l,**kw)
  def ready1(i):
    i.has={k:v for k,v in i.features.items() if i.ready(v)}
    
  def show(i):
    return " and ".join([f"{k}= {i.show(i.has[k])" 
                        for k in sorted(i.has.keys())])
def Or(Rule):
  def __init__(i,*l,**kw):
    i.has=set() # needed for socring cause the raw scores are on raw ranges
    i.reduced=[] # someitmes, when we combine ranges, we re duce this set
    super().__init__(*l,**kw)
  def ready1(i):
    "return the size of i.has"
  def show(i):
    return " or ".join(i.reduced)

   


class Ruler:
  def __init__(i,n): i.has,i.n,i._s =  {},n,None
  def add(i,feature,range):
    s = i.has[feature] = i.has(feature,set())
    s.add(range)
    i._s = None
  @property
  def score(i):
    i._s = i._s or value(i.has,i.n)
    return i._s

def like(d,n):
  prod=math.prod

def csv(f):
  with open(f) as fp:
    for line in fp:
      if line := re.sub(r'([\n\t\r ]|#.*)', '', line):
        yield line.split(",")

def inc(d,k): d[k] = d.get(k,0) + 1

def solos(n,goal):
 for kl, x in f:
   if kl == goal:
     yield (value([x],n),x)

count=o(f={}, h={},n=0)
for n,row in enumerate(csv("../data/vote.csv")):
  if n==0:
    arity=len(row)-1
  else:
    kl=row[arity]
    inc(n.h,  kl)
    n.n+=1
    for c,cell in enumerate(row):
      if c < arity and cell !="?":
        inc(n.f,(kl, (c,cell,cell)))

print(f)
print(h)

