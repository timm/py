"""
data: tables of rows, each column summarized by a Num or a Sym.
(c) 2026, Tim Menzies <timm@ieee.org>, MIT license

A Num is just a (n, mu, m2) tuple grown by Welford's algorithm;
a Sym is just a dict of counts. `add` grows either (inc=-1
shrinks); `mid` and `div` are their centers and spreads.

A csv file's first row names the columns. First character upper
case means numeric (Num), else symbolic (Sym). Last character:
  +  -   goal to maximize / minimize   (a y column)
  !      the class                     (tbl.klass)
  X      ignore this column
  else   an input                      (an x column)
Cells holding "?" are unknown.
"""
from math import exp, log2
from lib import o, isa, TINY

Sym = dict
Num = lambda n=0, mu=0, m2=0: (n, mu, m2)

def add(col, v, inc=1):
  "Grow a Num or Sym by v (inc=-1 shrinks); return the new col"
  if v == "?": return col
  if isa(col, Sym):
    if (c := col.get(v,0) + inc) > 0: col[v] = c
    else: col.pop(v, None)
    return col
  n, mu, m2 = col
  if (n := n + inc) <= 0: return Num()
  d = v - mu; mu += inc * d / n
  return (n, mu, max(0, m2 + inc * d * (v - mu)))

def mid(col):
  "Center: mode of a Sym, mean of a Num"
  return max(col, key=col.get) if isa(col, Sym) else col[1]

def div(col) -> float:
  "Spread: entropy of a Sym, standard deviation of a Num"
  if isa(col, Sym):
    N = sum(col.values()) or 1
    return -sum(v/N * log2(v/N) for v in col.values() if v)
  n, _, m2 = col
  return 0 if n < 2 else (m2/(n - 1))**.5

def norm(col, v) -> float|str:
  "Map a Num's value to 0..1 by a logistic z-score"
  if v == "?": return v
  z = (v - col[1]) / (div(col) + TINY)
  return 1 / (1 + exp(-1.7 * max(-3, min(3, z))))

def TAB(src) -> o:
  "First row names and types the columns; the rest are data"
  tbl = o(names=next(src := iter(src)), cols={}, x=[], y=[],
          goal={}, klass=None, rows=[], mids=None)
  for at,s in enumerate(tbl.names):
    tbl.cols[at] = Num() if s[0].isupper() else Sym()
    if   s[-1] == "X": continue
    elif s[-1] == "!": tbl.klass = at
    elif s[-1] in "+-": tbl.y += [at]; tbl.goal[at] = s[-1]=="+"
    else:               tbl.x += [at]
  for row in src: addRow(tbl, row)
  return tbl

def addRow(tbl: o, row: list, inc=1) -> None:
  tbl.mids = None
  (tbl.rows.append if inc > 0 else tbl.rows.remove)(row)
  for at in tbl.cols: tbl.cols[at] = add(tbl.cols[at], row[at], inc)

def clone(tbl: o, rows=[]) -> o:
  "Fresh Tab with the same header"
  return TAB([tbl.names] + list(rows))

def mids(tbl: o) -> list:
  "Cached centroid, one mid per column; addRow invalidates"
  tbl.mids = tbl.mids or [mid(col) for col in tbl.cols.values()]
  return tbl.mids
