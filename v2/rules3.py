#!/usr/bin/env python3.9
# vim: ts=2 sw=2 sts=2 et :

import re,sys,copy
from collections import defaultdict

BEAM=7

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

def csv(f=None):
  def prep(s)  : return re.sub(r'([\n\t\r ]|#.*)', '', s)
  def splits(s): return [coerce(x) for x in s.split(",")]
  def coerce(s):
    try : return int(s)
    except:
      try : return float(s)
      except: return s
  if f:
    with open(f) as fp:
      for s in fp:
        if s := prep(s): yield splits(s)
  else:
    for s in sys.stdin:
      if s := prep(s): yield splits(s)

#--------------------------------------------
class Row(o):
  id=0
  def __init__(i,lst):
    i.cells=lst
    i.id = Row.id = 1 + Row.id
  def __repr__(i): return str(i.id)

def optimize(b,r): return b**2  / (b + r)
def monitor( b,r): return r**2 / (b + r)
def explore( b,r): return 1   / (b + r)

class Rule(o):
  def __init__(i,tbl=None,init=None,want=None,goal=optimize): 
    i.has, i._score,i.tbl,i.goal,i.want = sets(),None,tbl,goal,want
    if init: i.add(init)
  def add(i,pair):
    """For pair=(attr,val), add val to attr. If attr now holds
    all values for attr, then delete attr."""
    i._score=None
    attr,val = pair
    i.has[attr].add(val)
    if len(i.has[attr]) == len(i.tbl.attrs[attr]): i.has.pop(attr)
  def __lt__(i,j): return i.score() < j.score()
  def __add__(i,j):
    k = Rule(tbl=i.tbl,goal=i.goal,want=i.want)
    for rule in [i,j]:
      for attr in rule.has:
        for val in rule.has[attr]: k.add((attr,val))
    if k != i and k != j:
      if dict(k.has):
        return k
  def __eq__(i,j):
    return i.has == j.has
  def show(i):
    def merge(pairs): 
      j, tmp = 0, []
      while j < len(pairs):
        a = pairs[j]
        if j < len(pairs) - 1:
          b = pairs[j+1]
          if a[1]==b[0]: j,a= j+1,(a[0],b[1])
        tmp += [a]
        j += 1
      return tmp
    def show1(x):
      return str(x[0]) if x[0]==x[1] else f"{x[0]},,{x[1]}"
    return  'and '.join([('%s = (%s) '%( 
      i.tbl.names[attr],(' or '.join([show1(x) for x in merge(sorted(vals))]))))
      for attr,vals in i.has.items()])

  def selects(i):
    attrs=None
    for attr in i.has:
      vals=None
      for val in i.has[attr]:
        tmp = i.tbl.all[(attr,val)]
        vals = (vals | tmp) if vals else tmp
      attrs = (attrs & vals) if attrs else vals
    return attrs
  def score(i):
    if not i._score:
      best = rest = bestall = restall = 0
      for klass in i.tbl.klasses:
        all      = len(i.tbl.klasses[klass])
        selected = len(i.tbl.klasses[klass] & i.selects()) 
        if   klass == i.want : best += selected; bestall += all
        else                 : rest += selected; restall += all
      if best and rest:
        best /= bestall
        rest /= restall
        i._score = i.goal(best, rest) if best>rest else 0
      else:
        i._score = 0
    return i._score
    
def tbl(src):
  t = o(names=next(src), _rows=[], klass=0,
        klasses=sets(), attrs=sets(), all=sets())
  for lst in src:
    t.klass=t.klass or len(lst) -1 
    row = Row(lst)
    t._rows += [row]
    t.klasses[ lst[t.klass] ].add(row)
    for attr,val in enumerate(lst):
      if val != "?":
        t.attrs[attr].add((val,val))
        t.all[(attr,(val,val))].add(row)
  return t

def rules(t,want,top):
  solos = []
  for attr in t.attrs:
    if attr != t.klass:
      for val in t.attrs[attr]: 
        rule =  Rule(t, (attr,val),want)
        if  rule.score() > .1 :
          solos += [rule]
  solos = sorted(solos)[-top:]
  tmp = {}
  for n,s in enumerate(subsets(solos)):
    if s:
      one = copy.deepcopy(s[0])
      for another in s[1:]: 
        if two := one + another:
          one = two
      tmp[str(dict(one.has))] = one
  rules = sorted(tmp.values())[-top:]
  for rule in rules:
    print("%.3f : %s" % (rule.score(),rule.show()))

if __name__ == "__main__":
  src = sys.argv[1] if len(sys.argv)>1 else None
  t= tbl(csv(src))
  rules(t, "democrat",BEAM)
  #rules(t)
