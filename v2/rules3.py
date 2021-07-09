import re
from collections import defaultdict

def zeros() : return defaultdict(lambda:0)
def sets()  : return defaultdict(lambda:set())

class o(object):
  def __init__(i, **k): i.__dict__.update(**k)
  def __repr__(i):
    return i.__class__.__name__ + str(
        {k: v for k, v in i.__dict__.items() if k[0] != "_"})

def subsets(l):
  out = [[]]
  for x in l: out += [sub + [x] for sub in out]
  return out[1:]

def csv(f):
  def coerce(s):
    try : return int(s)
    except:
      try : return float(s)
      except: return s
  with open(f) as fp:
    for line in fp:
      if line := re.sub(r'([\n\t\r ]|#.*)', '', line):
        yield [coerce(x) for x in line.split(",")]

class Row(o):
  id=0
  def __init__(i,lst):
    i.cells=lst
    i.id = Row.id = 1 + Row.id
  def __repr__(i): return str(i.id)

def optimize(b,r,B,R): return (b/B)^2/(b/B + r/R)
def monitor( b,r,B,R): return (r/R)^2/(b/B + r/R)
def explore( b,r,B,R): return 1      /(b/B + r/R)

class Rule(o):
  def __init__(i,tbl,goal=optimize): 
    i.has. i._score,i.tbl,i.goal = sets(),None,tbl,goal
  def adds(i,pairs): 
    [i.add(pair) for pair in pairs]
  def add(i,pair):
    i._score=None
    i.has[pair[0]].add(pair[1])
  def __add__(i,j):
    k=Rule(i.tbl,i.goal)
    for rule in [i,k]:
      for attr in rule.has:
        for val in rule.has[attr]:
          k.add((attr,val))
    return k
  def __eq__(i,j):
    return i.has == j.has
  def selects(i):
    attrs=None
    for attr in i.has:
      vals=None
      for val in i.has[attr]:
        tmp = i.tbl.all[(attr,val)]
        vals = (vals | tmp) if vals else tmp
      attrs = (attrs & vals) if attrs else vals
    return attrs
  def score(i,goal):
    if not i._score:
      bestall = restall = 0
      bestand = restand = 0
      found = i.selects()
      for klass in t.k:
        ns = len(t.all[klass])
        n = len(t.all[klass] & found) 
        if klass == goal : best,BEST = n, N
        else             : rest += n; REST += N
      i._score = i.goal(best,rest,BEST,REST)
    return i._score
    
def tbl(src, kl = -1):
  t = o(names=next(src), _rows=[], k=zeros(), av=zeros(), all=sets())
  for lst in src:
    row = Row(lst)
    t._rows += [row]
    klass = lst[kl]
    t.k[klass] += 1
    for attr,val in enumerate(lst):
      if val != "?":
        t.av[(attr,(val,val))] += 1
        for key in [klass, (attr,(val,val))]:
          t.all[key].add(row)
  return t

def rules(t):
  for attr,val in t.av: print(attr,val)

t=tbl(csv("../data/vote.csv"))
print(t.k["democrat"])
#rules(t)
