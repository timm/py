# vim: ts=2 sw=2 sts=2 et :
import re
# rahnges have its

class o(object):
  def __init__(i, **k): i.__dict__.update(**k)
  def __repr__(i):
    return i.__class__.__name__ + str(
        {k: v for k, v in i.__dict__.items() if k[0] != "_"})

from functools import wraps

def memo(f):
  cache = {} # initialized at load time
  @wraps(f)
  def g(*lst): # called at load time
    val = cache[lst] = cache.get(lst,None) or f(*lst)
    return val
  return g

def subsets(l):
  out = [[]]
  for x in l:
    out += [sub + [x] for sub in out]
  return out[1:]

def csv(f):
  with open(f) as fp:
    for line in fp:
      if line := re.sub(r'([\n\t\r ]|#.*)', '', line):
        yield line.split(",")

class Sym(o):
  def __init__(i,at,txt): 
    i.n,i.seen,i.at,i.txt,i._y = 0,{},at,txt,{}
  def add(i,x,y,n=1):
    if x != "?":
      i.n += n
      i.seen[x] = n + i.seen.get(x,0)
      ys = i._y[x] = i._y.get(x,set())
      ys.add(y)
    return x
   
class Row(o):
  id=0
  def __init__(i,tab, cells): 
    i.id = Row.id = 1 + Row.id
    i._tab, i.cells = tab, cells
    [col.add(x,i) for x,col in zip(cells, i._tab.cols)]
  def __hash__(i): return i.id

class Tab(o):
  def __init__(i,rows=[]): 
    i.cols,i._rows = [],[]
    [i.add(lst) for lst in rows]
  def clone(i,rows=[]):
    return Tab([[col.txt for col in i.cols] + rows])
  @property
  def klass(i): return len(i.cols) - 1
  def read(i,f):
    [i.add(lst) for lst in csv("../data/vote.csv")]
    return i
  def add(i,lst):
     lst = lst.cells if type(lst)==Row else lst
     if    i.cols: i._rows += [Row(i,lst)]
     else: i.cols= [Sym(n,x) for n,x in enumerate(lst)]

class Tabs(o):
  def __init__(i):
    i.all, i.one = Tab(), {}
  def read(i,f):
    [i.add(lst) for lst in csv(f)]
    return i
  def add(i,lst):
    i.all.add(lst)
    if len(i.all._rows) > 0:
      k = lst[-1]
      d = i.one[k] = i.one.get(k,None) or i.all.clone()
      d.add(lst)
  def contrast(i,best,rest):
    bests = i.one[best]
    rests = i.one[rest]
    for col1,col2 in zip(bests.cols, rests.cols):
      if col1.at != i.all.klass:
        for x,n1 in col1.seen.items():
          yield o(x=x, at=col1.at, _best=col1._y.get(x,set()), 
                                   _rest=col2._y.get(x,set()))

class Rule:
  def __init__(i): i.features  = {}
  def add(i,feature, attribute):
    attrs = i.features[feature] = i.features.get(feature,set())
    attrs.add(attribute)

one = Tabs().read("../data/vote.csv")
for x in one.contrast('democrat','republican'):
  print(x, len(x._best))
