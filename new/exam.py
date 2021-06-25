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

def Head(): return o(txt=[],xs={},ys={},all={},w={})
def Data(): return o(lo={},hi={}, seen={}, rows=[])

def slurp(f,hdr,dat): [row(lst,hdr,dat) for lst in csv(f)]

def row(lst,hdr,dat): 
  data(lst,dat) if hdr.txt else head(lst,hdr,dat)

def head(lst,hdr,dat):
  hdr.txt = lst
  for c,s in enumerate(lst):
    if "?" not in s:
      if s[0].isupper():
        dat.lo[c] =  1E31
        dat.hi[c] = -1E32
        hdr.w[c]  = -1 if "-" in s else 1
      else:
        dat.seen[c] = {}

def data(lst,dat):
  for c in dat.lo:
    z = lst[c] 
    if z != "?":
      z = lst[c] = float(z)
      dat.lo[c] = min(z, dat.lo[c])
      dat.hi[c] = max(z, dat.hi[c])
  for c in dat.seen:
    z = lst[c]
    one = dat.seen[c]
    one[z] = one.get(z,0) + 1
  dat.rows += [lst]

def div(dat,hdr,the,rows=None,cols="x"):
  def gap(z1,z2): return dist(z1, z2, dat, hdr, the, cols)
  def far(r1):
    tmp = [random.choice(rows or dat.rows) for _ in range(32)]
    tmp = sorted([(_gap(r1,r2),r2) for r2 in tmp], key=lambda z:z[0])
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
  return rows[:mid], rows[mid], rows[mid:]

def dist(r1,r2,dat,hdr,the,cols="x"):
  def dist1(z1,z2,c):
    if z1=="?" and z2=="?": return 1
    if c in hat.seen      : return 0 if z1==z2 else 1
    if   z1=="?": z2 = norm(z2,c,dat); z1= 0 if z2>.5 else 1
    elif z2=="?": z1 = norm(z1,c,dat); z2= 0 if z1>.5 else 1
    else        : z1, z2 = norm(z1,c,dat), norm(z2,c,dat)
    return abs(z1-z2)
  #--------------
  gap,n= 0, 1E-32
  for c in dat[cols]:
    tmp  = dist1(r1[c], r2[c],c)
    gap += tmp**the.p
    n   += 1
  return (gap/n)**(1/the.p)

def norm(z,c,dat):
  return (z-dat.lo[c])/(dat.hi[c] - dat.lo[c] + 1E-31)
