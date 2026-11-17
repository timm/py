#!/usr/bin/env python3
"""
sa: simulated annealing over tabular data, one label at a time.
(c) 2026, Tim Menzies <timm@ieee.org>, MIT license

USAGE: python3 sa.py [--key=val ...]

OPTIONS:
  --dir    data directory  = $MOOT/optimize
  --file   file(s) | all   = all
  --seed   random seed     = 1
  --runs   repeats         = 5
  --val    random evals    = 10
  --few    pool rows tried = 32
  --temp   start temp      = 0.2
  --cool   cooling rate    = 0.9

Design: an "eval" reads one row's y values (its label). Spend
--val evals on random rows. Then walk: mutate the current row by
extrapolating between two of the top sqrt(n) evaluated rows;
realize that as the nearest of --few random unevaluated rows;
evaluate it; move there if it is better, or if lucky at this
temperature.

Reports "win" after 25, 50, ... 1000 evals, one column per data
file: 100 = found the best row in all the data, 0 = no better
than the median row; mean of --runs repeats. A file with fewer
rows than evals just evaluates every row. Files are named
relative to --dir, without ".csv".
"""
import os, sys  # make hair importable whether run from source or installed
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import random
from math import exp, sqrt
from bisect import insort
from hair import *

rand  = random.random
any   = random.choice   # shadows builtin
EVALS = [25, 50, 100, 200, 500, 1000]

#-- mutate -----------------------------------------------------
def top(done: list) -> list:
  return done[:int(sqrt(len(done)))]       # done is sorted, best first

def mutate(tbl, done: list, row: list, p=1.0) -> list:
  """Walk step: copy of row where each x cell, with probability
  p, is replaced by a value extrapolated between two top rows
  (t in -.25..1.25)."""
  b1, b2 = any(top(done)), any(top(done))
  out = row[:]
  for a in tbl.x:
    u, v = b1[a], b2[a]
    if rand() > p or u == v == "?": continue
    if "?" in (u, v): out[a] = v if u == "?" else u
    elif isa(tbl.cols[a], Sym): out[a] = u if rand() < .5 else v
    else: out[a] = v + (rand()*1.5 - .25) * (u - v)
  return out

#-- sa ---------------------------------------------------------
def near(tbl, pool: list, row: list) -> int:
  some = random.sample(range(len(pool)), min(the.few, len(pool)))
  return min(some, key=lambda i: distx(tbl, pool[i], row))

def accept(y: float, ycur: float, t: float) -> bool:
  "The whole SA kernel: better, or lucky at this temperature"
  return y < ycur or rand() < exp(-(y - ycur)/(t + TINY))

def sa(tbl) -> list:
  "Return the best disty seen after each eval"
  pool, done, best = random.sample(tbl.rows, len(tbl.rows)), [], []
  def evaluate(row):
    insort(done, row, key=lambda r: disty(tbl, r))
    best.append(disty(tbl, done[0]))
    return row
  for _ in range(the.val): evaluate(pool.pop())
  cur, t = done[0], the.temp
  while pool and len(best) < EVALS[-1]:
    new = evaluate(pool.pop(near(tbl, pool, mutate(tbl, done, cur))))
    if accept(disty(tbl,new), disty(tbl,cur), t): cur = new
    t *= the.cool
  return best

#-- test -------------------------------------------------------
def win(y: float, lo: float, mid: float) -> float:
  return 100 * (1 - (y - lo) / (mid - lo + TINY))

def wins(file: str) -> list:
  tbl  = TAB(csv(file))
  ys   = sorted(disty(tbl,r) for r in tbl.rows)
  random.seed(the.seed)
  runs = [sa(tbl) for _ in range(the.runs)]
  return [sum(win(best[min(n, len(best)) - 1], ys[0], ys[len(ys)//2])
              for best in runs) / the.runs for n in EVALS]

#-- main -------------------------------------------------------
def main() -> None:
  cli(the)
  print("win after n evals: 100 = found the best row; "
        "0 = only the median row\n")
  table([name(f) for f in files()], EVALS,
        [wins(f) for f in files()])

if __name__ == "__main__": main()
