#!/usr/bin/env python3
"""
acq: active learning by acquire, the minimum that runs.
(c) 2026, Tim Menzies <timm@ieee.org>, MIT license
Kernel (csv, Tab, Num/Sym, dists) trimmed from ezr-py/xai.py.

USAGE: python3 acq.py [--key=val ...]

OPTIONS:
  --file   data file       = $MOOT/optimize/misc/auto93.csv
  --seed   random seed     = 1
  --val    warm-up labels  = 10
  --pool   unlabelled cap  = 1000
  --budget new labels      = 30

Design: label a few warm-up rows; split labelled rows into best
(top sqrt(n) by disty) and rest. Then repeat: label the first
pool row sitting nearer best's centroid than rest's.
"""
import os, re, sys, random
from math import exp, sqrt
from types import SimpleNamespace as o
from collections.abc import Iterator
TINY = 1e-32
MOOT = os.environ.get("MOOT") or os.path.expanduser("~/gits/moot")

def thing(s) -> int|float|bool|str:
  if (s[1:] if s[:1]=="-" else s).isdigit(): return int(s)
  try: return float(s)
  except ValueError: return s=="True" or (s!="False" and s)

def settings(doc) -> o:
  pat = r"--(\w+)\s+[^=\n]*=\s*(\S+)"
  return o(**{k: thing(v) for k,v in re.findall(pat, doc)})

def csv(file) -> Iterator[list]:
  file = os.path.expanduser(file.replace("$MOOT", MOOT, 1))
  with open(file, encoding="utf-8") as f:
    for line in f:
      row = [x.strip() for x in line.split(",")]
      if "".join(row): yield [thing(x) for x in row]

Sym = dict
def Num(n=0, mu=0, m2=0) -> tuple: return (n, mu, m2)
def is_sym(i) -> bool: return isinstance(i, dict)
def sd(num: tuple) -> float: n,_,m2 = num; return 0 if n<2 else (max(0,m2)/(n-1))**.5

def count(sym: dict, v, inc=1) -> dict:
  if (c := sym.get(v,0) + inc) > 0: sym[v] = c
  else: sym.pop(v, None)
  return sym

def welford(num: tuple, v, inc=1) -> tuple:
  n, mu, m2 = num
  if (n := n + inc) <= 0: return Num()
  d = v - mu; mu += inc * d / n
  return (n, mu, m2 + inc * d * (v - mu))

def add(col: tuple|dict, v, inc=1) -> tuple|dict:
  if v == "?": return col
  return (count if is_sym(col) else welford)(col, v, inc=inc)

def Tab(src) -> o:
  def init(names):
    cols,x,y,goal={},[],[],{}
    for at, s in enumerate(names):
      cols[at] = Num() if s[0].isupper() else Sym()
      if s[-1] == "X": continue
      if s[-1] in "+-": y += [at]; goal[at] = s[-1]=="+"
      else:             x += [at]
    return o(names=names,cols=cols,x=x,y=y,goal=goal,rows=[],_mids=None)
  src = iter(src)
  tbl = init(next(src))
  for row in src: addRow(tbl, row)
  return tbl

def addRow(tbl: o, row: list, inc=1) -> None:
  tbl._mids = None
  (tbl.rows.append if inc > 0 else tbl.rows.remove)(row)
  for at in tbl.cols: tbl.cols[at] = add(tbl.cols[at], row[at], inc)

def norm(num: tuple, v) -> float|str:
  if v == "?": return v
  z = (v - num[1]) / (sd(num) + TINY)
  return 1 / (1 + exp(-1.7 * max(-3, min(3, z))))

def minkowski(vals, p=2) -> float:
  tot = nn = 0
  for v in vals: tot += v**p; nn += 1
  return (tot / (nn or 1)) ** (1/p)

def gap(col: tuple|dict, u, v) -> float|bool:
  if u == v == "?": return 1
  if is_sym(col): return u != v
  u, v = norm(col, u), norm(col, v)
  if u == "?": u = 1 if v < .5 else 0
  if v == "?": v = 1 if u < .5 else 0
  return abs(u - v)

def disty(tbl: o, row: list) -> float:
  return minkowski(abs(norm(tbl.cols[a], row[a]) - tbl.goal[a])
                   for a in tbl.y if row[a] != "?")

def distx(tbl: o, r1: list, r2: list) -> float:
  return minkowski(gap(tbl.cols[a], r1[a], r2[a]) for a in tbl.x)

def mid(col: tuple|dict) -> float|str:
  return max(col, key=col.get) if is_sym(col) else col[1]

def mids(tbl: o) -> list:
  "Cached centroid, one mid per column; addRow invalidates"
  tbl._mids = tbl._mids or [mid(col) for col in tbl.cols.values()]
  return tbl._mids

#-- acquire -----------------------------------------------------
def Cache(tbl: o) -> o:
  return o(tbl=tbl, all=[], best=Tab([tbl.names]), rest=Tab([tbl.names]))

def push(cache: o, row: list) -> None:
  "Newcomers start in best; overflow demotes best's worst to rest"
  cache.all += [row]
  addRow(cache.best, row)
  while len(cache.best.rows) > (int(sqrt(len(cache.all))) or 1):
    doomed = max(cache.best.rows, key=lambda r: disty(cache.tbl, r))
    addRow(cache.best, doomed, -1); addRow(cache.rest, doomed)

def acquire(tbl: o) -> o:
  "Label the first pool row nearer best's centroid than rest's"
  rows = random.sample(tbl.rows, len(tbl.rows))
  val, pool = rows[:the.val], rows[the.val:the.val+the.pool]
  c = Cache(tbl)
  for row in val: push(c, row)
  d = lambda r1, r2: distx(tbl, r1, r2)
  while len(c.all) - the.val < the.budget and pool:
    row = pool[0]                             # default: oldest
    for r in pool:                            # first one nearer
      if d(mids(c.best), r) < d(mids(c.rest), r):  # best's centroid
        row = r; break
    pool.remove(row); push(c, row)
  return c

def main() -> None:
  for arg in sys.argv[1:]:
    if arg[:2]=="--" and "=" in arg:
      k,v = arg[2:].split("=",1)
      if k in vars(the): setattr(the, k, thing(v))
  random.seed(the.seed)
  tbl  = Tab(csv(the.file))
  c    = acquire(tbl)
  base = sorted(disty(tbl,r) for r in tbl.rows)
  best = min(c.best.rows, key=lambda r: disty(tbl,r))
  print("rows %s  labels %s+%s  best %.3f  (corpus best %.3f, median %.3f)"
        % (len(tbl.rows), the.val, len(c.all) - the.val,
           disty(tbl,best), base[0], base[len(base)//2]))
  print("  " + ", ".join("%s=%s" % (n,v)
        for n,v in zip(tbl.names, best) if n[-1] != "X"))

the = settings(__doc__)
if __name__ == "__main__": main()
