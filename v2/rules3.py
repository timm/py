import re

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

class Row:
  id=0
  def __init__(i,lst):
    i.cells=lst
    i.id = Row.id = 1 + Row.id

class Rule:
  def __init__(i,col,val):
    i.has = {col:set([val])}
    i._score = 0
  def add(rule1, rule2):
    n=1

def read(kl = -1):
  t = o(rows=[], klasses={}, names=[])
  for n,lst in enumerate(csv("../data/vote.csv")): 
    klass = lst[kl]
    klasses[klass] = 1 + klasses.get(klass,0)
    if n==0: 
      t.names = lst
    else:    
      row = Row(lst)
      t.rows += [row]
      for n,x in enumerate(lst):
        if x != "?":
          key = (klass,(n,x))
          here = at[key] = at.get(key,set())
          here.add(row)
