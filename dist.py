"""
dist: distances between rows, and from a row to heaven.
(c) 2026, Tim Menzies <timm@ieee.org>, MIT license

distx is the distance between two rows over the x columns.
disty is a row's distance to the best possible y values
(0 = best), so a smaller disty is a better row.
"""
from lib import isa
from data import Sym, norm

def minkowski(vals, p=2) -> float:
  tot = nn = 0
  for v in vals: tot += v**p; nn += 1
  return (tot / (nn or 1)) ** (1/p)

def gap(col, u, v) -> float:
  "Distance 0..1 between two values of one column"
  if u == v == "?": return 1
  if isa(col, Sym): return u != v
  u, v = norm(col, u), norm(col, v)
  if u == "?": u = 1 if v < .5 else 0
  if v == "?": v = 1 if u < .5 else 0
  return abs(u - v)

def distx(tbl, r1: list, r2: list) -> float:
  return minkowski(gap(tbl.cols[a], r1[a], r2[a]) for a in tbl.x)

def disty(tbl, row: list) -> float:
  return minkowski(abs(norm(tbl.cols[a], row[a]) - tbl.goal[a])
                   for a in tbl.y if row[a] != "?")
