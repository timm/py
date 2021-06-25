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
  seed    = 10013)

import re,sys,copy,random,traceback
from etc import o,csv,cli

class Num(o): 
  def __init__(i): i._all, i.n, i.sorted = [],0,True
  def add(i,z): i._all+=[z]; i.n += 1; i.sorted=False
  def norm(i,z): 
    lo,hi = i.all()[0], i.all()[-1]
    return z  if z=="?" else (z-lo)/(hi - lo +1E-32)
  def all(i):
    if not i.sorted: i._all.sort()
    i.sorted=True
    return i._all

def Sym(o): 
  def __init__(i): i.seen, i.n = {},0
  def add(i,z)   : i.seen[z] = 1 + i.seen.get(z,0); i.n += 1

class Tab(o):
  def __init__(i,rows=[]):
    i.syms ={}; i.nums={}; i.rows=[] 
    i.cols = o(txt=[], xs={}, ys={}, w={})
    for lst in rows: i.row(lst)

  def clone(i,rows=[]): return Tab([i.cols.txt] + rows)
  def slurp(i,f)      : [row(i,lst) for lst in csv(f)]
  def row(i,lst)      : i.data(lst) if i.cols.txt else i.head(lst)

  def head(i,lst):
    i.cols.txt = lst
    for c,txt in enumerate(lst):
      if "?" not in s:
        if txt[0].isupper():
          i.nums[c] = Num()
          i.cols.w[c] = -1 if "-" in txt else 1
        else:
          i.syms[c] = Sym()
  
  def data(i,lst):
    for c in i.syms:
      z = lst[c]
      if z !="?": i.syms[c].add(z)
    for c in i.nums:
      z = lst[c]
      if z !="?":
        z = lst[c] = float(z)
        i.nums[c].add(z)
    i.rows += [lst]
  
  def dist(i,r1,r2,the,cols=None):
    cols = cols or i.cols.xs
    def dist1(z1,z2,c):
      if z1=="?" and z2=="?": return 1
      if c in i.syms      : return 0 if z1==z2 else 1
      if   z1=="?": z2 = i.norm(z2,c); z1= 0 if z2>.5 else 1
      elif z2=="?": z1 = i.norm(z1,c); z2= 0 if z1>.5 else 1
      else        : z1, z2 = norm(t,z1,c), norm(t,z2,c)
      return abs(z1-z2)
    #--------------
    gap,n = 0, 1E-32
    for c in cols:
      tmp  = dist1(r1[c], r2[c],c)
      gap += tmp**the.p
      n   += 1
    return (gap/n)**(1/the.p)  

  def div(i,the,rows=None,cols=None):
    def gap(z1,z2): return i.dist(z1, z2, the, cols)
    def far(r1):
      tmp = [random.choice(rows or i.rows) for _ in range(32)]
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
