#!/usr/bin/env python3.9
"""
./es.py : version2,  optimization via data mining.
(c) 2021, Tim Menzies, http://unlicense.org

Options:

 -data    FILE   data for data
 -do      STR    start-up action; default=None
 -p       INT    distance co-efficient; default=2
 -seed    INT    default random number seed; default=10013
"""
import re,sys,copy,random,traceback
from etc import o,csv,cli

options = dict(
  Verbose = 0,
  data    = "../data/auto93.csv",
  do      = "none",
  l       = 2,
  sample  = 32,
  seed    = 10013)

class Col(o):
  def __init__(i,txt="",at=0, inits=[]):
    i.n, i.txt, i.at, i.w = 0,txt, at, -1 if "-" in txt else 1
    [self.add(z) for z in inits]
  def add(i,z): return z
  def dist(i,x,y): 
    return 1 if x=="?" and y=="?" else i.dist1(x,y)

class Skip(Col): pass

class Num(Col): 
  def __init__(i,*l,**kw):
    i._all, i.sorted = [],True
    super().__init__(*l, **kw)
  def add(i,z): 
    z=float(z); i._all+=[z]; i.n += 1; i.sorted=False; return z
  def norm(i,z): 
    if z=="?": return z
    lo,hi = i.all()[0], i.all()[-1]
    return (z-lo)/(hi - lo +1E-32)
  def all(i):
    if not i.sorted: i._all.sort()
    i.sorted=True
    return i._all
  def dist1(i,x,y):
    if   x=="?": y = i.norm(y); x= 0 if y>.5 else 1
    elif y=="?": x = i.norm(x); y= 0 if x>.5 else 1
    else       : x, y = i.norm(x), i.norm(y)
    return abs(x-y)

def Sym(Col): 
  def __init__(i,*l,**kw):
    i.seen = {}
    super().__init__(*l, **kw)
  def add(i,z): 
    i.seen[z] = 1 + i.seen.get(z,0); i.n += 1; return z
  def dist1(i,x,y):
    return 0 if x==y else 1
    
class Tab(o):
  def __init__(i,rows=[]):
    i.xy,i.x,i.y,i.rows = [],[],[],[]
    for lst in rows: i.row(lst)

  def clone(i,rows=[]): return Tab([[col.txt for col in i.xy] + rows])
  def fromFile(i,f)   : [i.row(lst) for lst in csv(f)]
  def row(i,lst)      : (i.data if i.xy else i.head)(i,lst)

  def head(i,lst):
    for at,s in enumerate(lst):
      ako = Skip if "?" in s else (Num if s[0].isupper() else Sym)
      one = ako(s,at)
      i.xy += [one]
      (i.y if "+" in s or "-" in s or "!" in s else i.x).append(one)
  
  def data(i,lst):
    for col in i.xy:
      z = lst[col.at]
      if z != "?": lst[c] = col.add(z)
    i.rows += [lst]
  
  def dist(i,r1,r2,the,cols=None):
    gap,n = 0, 1E-32
    for col in cols or i.x:
      tmp  = col.dist(r1[col.at], r2[col.at])
      gap += tmp**the.l
      n   += 1
    return (gap/n)**(1/the.l)  

  def div(i,the,rows=None,cols=None):
    def gap(z1,z2): return i.dist(z1, z2, the, cols)
    def far(r1):
      tmp = [random.choice(rows or i.rows) for _ in range(the.sample)]
      tmp = sorted([(gap(r1,r2),r2) for r2 in tmp], key=lambda z:z[0])
      return tmp[-3]
    #-------------------------
    zero = random.choice(rows)
    one  = far(zero)
    two  = far(one)
    c    = gap(one,two)
    tmp  = {}
    for row in rows:
      a = gap(row, one)
      b = gap(row, two)
      tmp[id(row)] = (a**2 + c**2 - b**2) / (2 * c + 1E-31)
    rows.sort(key=lambda row: tmp[id(row)])
    mid = len(rows) // 2
    return rows[:mid], rows[mid:]
