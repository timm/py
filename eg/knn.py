#!/usr/bin/env python3
"""
egknn: guess a row's disty from its k nearest rows, two kernels:
mean (all neighbours equal) and tri (triangular: nearer rows
weigh more, weight 1 - d/dmax). Also "mu": guess the known mean.
(c) 2026, Tim Menzies <timm@ieee.org>, MIT license
"""
import sys; sys.path[0] += "/.."   # find hair in the parent
import random
from hair import *

def knn(tbl, known: list, row: list, k: int, tri=False) -> float:
  if not tri: return neary(tbl, known, row, k)
  rows = near(tbl, known, row, k)
  ds   = [distx(tbl, r, row) for r in rows]
  ws   = [1 - d/(max(ds) + TINY) for d in ds]
  return (sum(w * disty(tbl, r) for w,r in zip(ws,rows))
          / (sum(ws) + TINY))

def errors(file: str) -> list:
  random.seed(the.seed)
  tbl = TAB(csv(file))
  test, known = split(tbl.rows, the.test, the.pool)
  wants = [disty(tbl, r) for r in test]
  mu    = mid(adds(disty(tbl, r) for r in known))
  guess = lambda k,tri: [knn(tbl, known, r, k, tri) for r in test]
  return ([relerr(wants, guess(k, False)) for k in ks()]
        + [relerr(wants, guess(k, True))  for k in ks()]
        + [relerr(wants, [mu]*len(test))])

def ks() -> range: return range(2, the.k + 1)

if __name__ == "__main__":
  cli(the)
  table([name(f) for f in files()],
        ["mean k=%s" % k for k in ks()]
      + ["tri  k=%s" % k for k in ks()] + ["mu"],
        [errors(f) for f in files()])
