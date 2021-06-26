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
import re, sys, copy, math, random, traceback
from etc import o, csv, cli

options = dict(
  Verbose = 0,
  data    = "../data/auto93.csv",
  do      = "none",
  enough  = .5,
  l       = 1,
  fmt     = "%6.3g",
  sample  = 32,
  seed    = 10013)

class Col(o):
  def __init__(i,txt="",at=0, inits=[]):
    i.n, i.txt, i.at, i.w = 0,txt, at, -1 if "-" in txt else 1
    [self.add(z) for z in inits]
  def add(i,z): return z
  def mid(i)  : return "?"
  def dist(i,x,y): 
    return 1 if x=="?" and y=="?" else i.dist1(x,y)

class Skip(Col): pass

class Num(Col): 
  def __init__(i,*l,**kw):
    i._all, i.sorted = [],True
    super().__init__(*l, **kw)
  def add(i,z): 
    if z != "?":
      i.n += 1
      z=float(z); i._all+=[z]; i.sorted=False
    return z
  def norm(i,z): 
    if z=="?": return z
    lo,hi = i.all()[0], i.all()[-1]
    tmp = (z-lo)/(hi - lo +1E-32)
    return  max(0,min(tmp,1))
  def all(i):
    if not i.sorted: i._all.sort()
    i.sorted=True
    return i._all
  def dist1(i,x,y):
    if   x=="?": y = i.norm(y); x= 0 if y>.5 else 1
    elif y=="?": x = i.norm(x); y= 0 if x>.5 else 1
    else       : x, y = i.norm(x), i.norm(y)
    return abs(x-y)
  def per(i,p=.5): 
    a=i.all()
    return a[int(p*len(a))]
  def mid(i): return i.per(.5)
  def var(i): return (i.per(.9) - i.per(.1))/2.56

class Sym(Col): 
  def __init__(i,*l,**kw):
    i.seen, i.mode, i.most = {},None,0
    super().__init__(*l, **kw)
  def add(i,z,n=1): 
    if z != "?":
      i.n += n 
      tmp = i.seen[z] = i.seen.get(z,0) + n
      if tmp > i.most:
        i.most,i.mode = tmp,z
    return z
  def mid(i): return i.mode
  def dist1(i,x,y): return 0 if x==y else 1
   
class Row(o):
  def __init__(i,t, cells): i._tab,i.cells = t,cells

  def __lt__(i, j):
    cols = i._tab.y
    s1, s2, n = 0, 0, len(cols)
    for col in cols:
      a,b = i.cells[col.at], j.cells[col.at]
      if a=="?" or b=="?": continue
      a,b = col.norm(a), col.norm(b)
      s1 -= math.e**(col.w * (a - b) / n)
      s2 -= math.e**(col.w * (b - a) / n)
    return s1 / n < s2 / n

  def dist(i,j,the,cols=None):
    gap,n = 0, 1E-32
    for col in cols or i._tab.x:
      tmp  = col.dist(i.cells[col.at], j.cells[col.at])
      gap += tmp**the.l
      n   += 1
    return (gap/n)**(1/the.l)  

class Tab(o):
  def __init__(i,rows=[]):
    i.xy,i.x,i.y,i.rows = [],[],[],[]
    for lst in rows: i.row(lst)

  def __lt__(i,j): 
    return Row(i, i.mid()) < Row(j,j.mid())

  def goals(i): 
    return [col.mid() for col in i.y]

  def mid(i): 
    return [col.mid() for col in i.xy]

  def clone(i,rows=[]): 
     t= Tab()
     t.row([c.txt for c in i.xy])
     [t.row(row) for row in rows]
     return t

  def row(i,lst)      : 
    lst = lst.cells if type(lst)==Row else lst
    (i.data if i.xy else i.head)(lst)

  def head(i,lst):
    for at,s in enumerate(lst):
      ako = Skip if "?" in s else (Num if s[0].isupper() else Sym)
      one = ako(txt=s,at=at)
      i.xy += [one]
      if type(one) != Skip:
        if "+" in s or "-" in s or "!" in s:
              i.y += [one]
        else: i.x += [one]
  
  def data(i,lst):
    for col in i.xy:
      z = lst[col.at]
      if z != "?": 
        lst[col.at] = col.add(z)
    i.rows += [Row(i,lst)]
  
  def div(i,the,rows=None,cols=None):
    def gap(z1,z2): 
      return z1.dist(z2, the, cols)
    def far(r1):
      a= [random.choice(rows or i.rows) for _ in range(the.sample)]
      a= sorted([(gap(r1,r2),r2) for r2 in a], key=lambda z:z[0])
      return a[-3][1]
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
    #print(tmp)
    rows.sort(key=lambda row: tmp[id(row)])
    mid = len(rows) // 2
    return one, two, rows[:mid], rows[mid:]

def cluster(t,the, cols=None):
  def go(rows,lvl=0):
    if type(lvl)==int:
      print('|.. ' * lvl,len(rows))
    if len(rows) < enough:
      out.append(t.clone(rows))
    else:
      _, _, lefts, rights = t.div(the,rows=rows,cols=cols)
      go(lefts,   lvl+1)
      go(rights, lvl+1)

  enough = 2*len(t.rows)**the.enough
  cols = cols or t.x
  out = []
  go(t.rows)
  return out

def sway(t,the, cols=None):
  def go(rows,lvl=0):
    if type(lvl) == int  : print('|.. ' * lvl,len(rows))
    if len(rows) < enough: return t.clone(rows)
    else:
      left, right, lefts, rights = t.div(the,rows=rows,cols=cols)
      return  go(lefts if left < right else rights, lvl+1)

  enough = 2*len(t.rows)**the.enough
  cols = cols or t.x
  return go(t.rows)
