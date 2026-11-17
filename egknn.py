#!/usr/bin/env python3
"""
egknn: guess a row's disty from its k nearest rows, two kernels:
mean (all neighbours equal) and tri (triangular: nearer rows
weigh more, weight 1 - d/dmax). Also "mu": guess the known mean.
(c) 2026, Tim Menzies <timm@ieee.org>, MIT license
"""
import random
from hair import *

def knn(tbl, known: list, row: list, k: int, tri=False) -> float:
  near = sorted(known, key=lambda r: distx(tbl, r, row))[:k]
  ys   = [disty(tbl, r) for r in near]
  if not tri: return sum(ys) / k
  ds = [distx(tbl, r, row) for r in near]
  ws = [1 - d/(max(ds) + TINY) for d in ds]
  return sum(w*y for w,y in zip(ws,ys)) / (sum(ws) + TINY)

def errors(file: str) -> list:
  random.seed(the.seed)
  tbl = TAB(csv(file))
  test, known = split(tbl.rows, the.test, the.pool)
  wants = [disty(tbl, r) for r in test]
  mu    = sum(disty(tbl, r) for r in known) / len(known)
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
