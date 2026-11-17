"""
tree: regression or classification trees over a Tab.
(c) 2026, Tim Menzies <timm@ieee.org>, MIT license
Trimmed from ezr-py/xai.py.

`bins` yields candidate (cost, at, v) splits for one column;
cost is the size-weighted spread of the two halves. `tree`
recurses the min-cost bin; leaves keep their rows and a `mid`
prediction. `leaf` routes a row down; `leaves` yields them all;
`show` pretty-prints.
"""
from .lib import o, isa
from .data import Sym, Num, add, adds, fold, mid
from .stats import xpect

def bins(tbl: o, rows: list, at: int, Y, accum, leaf=3):
  "Yield (cost, at, v) bins; both sides hold at least leaf rows"
  xy  = [(r[at], Y(r)) for r in rows if r[at] != "?"]
  n   = len(xy)
  tot = adds((y for _,y in xy), accum())
  big = lambda lo: leaf <= lo <= n - leaf
  one = lambda here,k: (xpect(here, fold(tot, here, -1)), at, k)
  if isa(tbl.cols[at], Sym):
    for k in {x for x,_ in xy}:
      ys = [y for x,y in xy if x==k]
      if big(len(ys)): yield one(adds(ys, accum()), k)
  else:
    xy.sort(); here = accum()
    for j,(x,y) in enumerate(xy):
      here = add(here, y)
      if j+1 < n and x != xy[j+1][0] and big(j+1):
        yield one(here, x)

def has(row: list, col, at: int, v) -> bool:
  "Does row fall on the yes-side of a bin? (? = yes)"
  w = row[at]
  return w == "?" or (v == w if isa(col, Sym) else w <= v)

def tree(tbl: o, rows: list, Y, accum=Num, leaf=3, maxd=8, lvl=0) -> o:
  "Recursively split rows on the min-cost bin; accum=Num|Sym"
  all = adds((Y(r) for r in rows), accum())
  t = o(at=None, v=None, mid=mid(all), n=len(rows), rows=rows,
        yes=None, no=None)
  if len(rows) >= 2*leaf and lvl < maxd:
    if bin := min((c for at in tbl.x
                   for c in bins(tbl, rows, at, Y, accum, leaf)),
                  default=None):
      _, at, v = bin
      yes, no = [], []
      for r in rows:
        (yes if has(r, tbl.cols[at], at, v) else no).append(r)
      if yes and no:
        t.at, t.v = at, v
        t.yes = tree(tbl, yes, Y, accum, leaf, maxd, lvl+1)
        t.no  = tree(tbl, no,  Y, accum, leaf, maxd, lvl+1)
  return t

def leaf(tbl: o, t: o, row: list):
  "Walk a row down to its leaf; return the leaf's mid"
  while t.at is not None:
    t = t.yes if has(row, tbl.cols[t.at], t.at, t.v) else t.no
  return t.mid

def leaves(t: o):
  if t.at is None: yield t
  else: yield from leaves(t.yes); yield from leaves(t.no)

def show(tbl: o, t: o, pad="") -> None:
  "One line per node, indented by depth"
  m = t.mid if isa(t.mid, str) else round(t.mid, 2)
  print("%6s %5s  %s" % (m, t.n, pad))
  if t.at is not None:
    for kid in [t.yes, t.no]:
      sym = isa(tbl.cols[t.at], Sym)
      txt = "%s %s %s" % (tbl.names[t.at],
                          ("==" if sym else "<=") if kid is t.yes
                          else ("!=" if sym else ">"), t.v)
      print("%6s %5s  %s%s" % ("", "", pad, txt))
      show(tbl, kid, pad + "|  ")
