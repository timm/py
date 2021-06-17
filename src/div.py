#!/usr/bin/env python3.9
import re,sys,random

def lines(f):
  with open(f) as fp:  
    for line in fp:
      line = re.sub(r'([\n\t\r ]|#.*)', '', line)
      if line: yield line.split(",")

class Thing(object):
  def __repr__(i): 
      return i.__class__.__name__ + str(
             {k:v for k,v in  i.__dict__.items() if k[0] != "_"})
  
class Table(Thing):
  def __init__(i): i.rows, i.cols, i.x, i.y = [],[],[],[]
  def read(i,f): 
    for line in lines(f): self.add(line)
  def add(i,lst):
    lst = lst.cells if type(lst)==Row else lst
    if i.cols: i.rows += [i.row(lst)]
    else     : i.cold  = i.columns(lst)
  def i.row(lst): 
    return Row(i, [it.add(x) for it,x in zip(i.cols,x)])
  def i.columns(lst):
    out = [Item(at,pos) for at,pos in enumerate(lst)]
    for it in out:
      if "?" not in it.skip:
        if it.goal    : i.y += [it]
        if not it.goal: i.x += [it]
    return out

class Row(Thing):
  def __init__(i,tbl,cells): i._tbl, i.cells = tbl, cells
  def dist(i,j,the,cols=None):
    d,n = 0,1E-32
    for it in cols or i._tbl.x:
      n  += 1
      a,b = i.cells[it.at], j.cells[it.at]
      inc = 1 if a=="?" and b=="?" else it.dist(a,b)
      d += inc^the.p
    return (d/n)^(1/the.p)
  
class Item(Thing):
  def __init__(i,at,txt): 
    i.at, i.txt, i.ako, i.lo = at, txt, None, float('inf'); i.hi=-i.lo
    i.w    = -1 if "-" in txt else 1
    i.goal = "-" in txt or "+" in txt or "!" in txt
  def norm(i,x):
    return x if i.ako==str else (x-i.lo)/(i.hi-i.lo+1E-32)
  def dist(i,x,y):
    if i.ako==str: return 0 if x==y else 1
    if      x=="?": y=i.norm(y); x = 1 if y<0.5 else 0
    else if y=="?": x=i.norm(x); y = 1 if x<0.5 else 0
    else          : x,y = i.norm(x), i.norm(y)
    return abs(x-y)
  def add(i,x):
    if x!="?":  
      i.ako = i.ako or i._ako(x)
      x = i.ako(x)
      if i.ako != str: 
        i.lo = min(i.lo,x)
        i.hi = max(i.hi,x)
    return x
  def _ako(i,s):
    try:  int(s); return int
    except Exception: 
      try:  float(s); return float
      except Exception: return str
  def faraway(i, row1,rows,the,cols=None):
    tmp=[(row1:dist(row2,the,cols or i.x),row2) for row2 in rows]
    return sorted(tmp)[the.faraway*len(tmp)//1]

  def div2(i,the,cols=None,rows=None):
    rows = rows or i.rows
    zero = random.choice(rows)
    one  = i.faraway(zero,rows,the,cols)
    two  = i.faraway(one, rows,the,cols)
    c    = one.dist(two,the,cols)
    for row in rows:
      a = row.dist(one,the,cols)
      b = row.dist(two,the,cols)
      row.div2x = (a**2 + b**2 - c**2)/(2*c)
    rows = sorted(rows, key=lambda (x) x.div2x)
    mid = len(x//2)
    return rows[:mid], rows[mid:]


for line,meta in lines("data/auto93.csv"):
  print(meta)




