#!/usr/bin/env python3.9
"""
Keys1,py : optimization via discretization.
(c) 2021, Tim Menzies, http://unlicense.org

Options:

 -data    FILE   data for data
 -do      S      start-up action; default=None
 -p       INT    distance co-efficient; default=2
 -seed    INT    default random number seed; default=10013
"""

options = dict(
  Verbose = 0,
  data    = "../data/auto93.csv",
  do      = "none",
  p       = 2,
  sample  = 32,
  seed    = 10013)

import re,sys,copy,random,traceback
from etc import o,csv,cli

class  Ignore(o): 
  def __init__(i,at,w): i.at=at
  def add(i,z): return z

class Num(o): 
  def __init__(i,at,w):
    i.at,i.w,i._all, i.n, i.sorted = at,w,txt,[],0,True
  def add(i,z): 
    z=float(z); i._all+=[z]; i.n += 1; i.sorted=False; return z
  def norm(i,z): 
    lo,hi = i.all()[0], i.all()[-1]
    return z  if z=="?" else (z-lo)/(hi - lo +1E-32)
  def all(i):
    if not i.sorted: i._all.sort()
    i.sorted=True
    return i._all
  def dist(i,x,y):
    if   x=="?": y = i.norm(y); x= 0 if y>.5 else 1
    elif y=="?": x = i.norm(x); y= 0 if x>.5 else 1
    else        : x, y = i.norm(x), i.norm(y)
    return abs(x-y)

def Sym(o): 
  def __init__(i,at,w): i.at,i.seen, i.n = at, {},0
  def add(i,z): 
    i.seen[z] = 1 + i.seen.get(z,0); i.n += 1; return z
  def dist(i,x,y):
    return 0 if x==y else 1
    
class Tab(o):
  def __init__(i,rows=[]):
    i.syms ={}; i.nums={}; i.rows=[] 
    i.cols = o(header=[], xs=[], ys=[],all=[])
    for lst in rows: i.row(lst)

  def clone(i,rows=[]): return Tab([i.cols.header] + rows)
  def slurp(i,f)      : [i.row(lst) for lst in csv(f)]
  def row(i,lst)      : i.data(lst) if i.cols.txt else i.head(lst)

  def head(i,lst):
    i.cols.header = lst
    for c,txt in enumerate(lst):
      weight = -1  if "-" in txt else 1
      one = Ignore if "?" in s else (
            Num if txt[0].isupper() else
            Sym)
      one = one(c, weight)
      i.all += [one]
      if type(what)==  Num: i.nums += [one]
      if type(what)==  Sym: i.sym += [one]
  
  def data(i,lst):
    for col in i.cols.all:
      z = lst[col.at]
      if z !="?": 
        lst[c] = col.add(z)
    i.rows += [lst]
  
  def dist(i,r1,r2,the,cols=None):
    gap,n = 0, 1E-32
    for col in cols or i.cols.xs:
      tmp  = col.dist(r1[col.at], r2[col.at])
      gap += tmp**the.p
      n   += 1
    return (gap/n)**(1/the.p)  

  def div(i,the,rows=None,cols=None):
    def gap(z1,z2): return i.dist(z1, z2, the, cols)
    def far(r1):
      tmp = [random.choice(rows or i.rows) for _ in range(the.sample)]
      tmp = sorted([(gap(r1,r2),r2) for r2 in tmp], key=lambda z:z[0])
      return tmp[-2]
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
