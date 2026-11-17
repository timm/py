#!/usr/bin/env python3
"""
knn: guess a row's score from its k nearest neighbours.
(c) 2026, Tim Menzies <timm@ieee.org>, MIT license

USAGE: python3 knn.py [--key=val ...]

OPTIONS:
  --dir    data directory  = $MOOT/optimize
  --file   file(s) | all   = all
  --seed   random seed     = 1
  --k      most neighbours = 5
  --test   rows to guess   = 100
  --pool   known rows, cap = 1000

Design: a row's score is its disty: distance to the best
possible goal values (0 = best). Hide the scores of --test rows.
Guess each as the mean disty of its k nearest known rows, for
k = 1 to --k, nearness measured on the x columns only. Report
the percent error, 100 * mean |actual - predicted| / mean actual,
and, as row "mean", the same when every guess is just the mean
disty of the known rows. One column per data file. Files are
named relative to --dir, without ".csv": --file=misc/auto93 gives
one column, --file=a,b two, --file=all everything in ALL.
"""
import sys; sys.path[0] += "/.."; sys.dont_write_bytecode = True
import random
from hair import *

#-- knn --------------------------------------------------------
def nearest(tbl, rows: list, row: list, k: int) -> list:
  return sorted(rows, key=lambda r: tbl.distx(r, row))[:k]

def mean(tbl, rows: list) -> float:
  return sum(map(tbl.disty, rows)) / len(rows)

def knn(tbl, rows: list, row: list, k: int) -> float:
  return mean(tbl, nearest(tbl, rows, row, k))

#-- test -------------------------------------------------------
def split(tbl) -> tuple:
  rows = random.sample(tbl.rows, len(tbl.rows))
  n    = min(the.test, len(rows)//2)       # small data: test half
  return rows[:n], rows[n:n+the.pool]

def error(tbl, test: list, guess) -> float:
  gap = sum(abs(tbl.disty(r) - guess(r)) for r in test) / len(test)
  return 100 * gap / mean(tbl, test)

def errors(file: str) -> list:
  random.seed(the.seed)
  tbl  = TAB(csv(file))
  test, known = split(tbl)
  near = {id(r): nearest(tbl, known, r, the.k) for r in test}
  mu   = mean(tbl, known)
  return ([error(tbl, test, lambda r: knn(tbl, near[id(r)], r, k))
           for k in range(1, the.k + 1)]
          + [error(tbl, test, lambda r: mu)])

#-- main -------------------------------------------------------
def main() -> None:
  cli(the)
  print("100 * mean |actual - predicted| / mean actual\n")
  table([name(f) for f in files(the)],
        ["k=%s" % k for k in range(1, the.k + 1)] + ["mean"],
        [errors(f) for f in files(the)])

the = settings(__doc__)
if __name__ == "__main__": main()
