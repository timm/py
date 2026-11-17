#!/usr/bin/env python3
"""
egknn: guess a row's disty from its k nearest rows, two kernels:
mean (all neighbours equal) and tri (triangular: nearer rows
weigh more, weight 1 - d/dmax). Also "mu": guess the known mean.
(c) 2026, Tim Menzies <timm@ieee.org>, MIT license
"""
import os, sys  # make hair importable whether run from source or installed
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import random
from hair import *

def knn(tbl, known: list, row: list, k: int, tri=False) -> float:
  if not tri: return neary(tbl, known, row, k)
  rows = near(tbl, known, row, k)
  ds   = [distx(tbl, r, row) for r in rows]
  ws   = [1 - d/(max(ds) + TINY) for d in ds]
  return (sum(w * disty(tbl, r) for w,r in zip(ws,rows))
          / (sum(ws) + TINY))

def ks() -> range: return range(1, the.k + 1)

def cols() -> list:
  "Treatment names (no spaces): mean/tri kernels per k, plus the mu baseline."
  return (["mean:k=%d" % k for k in ks()]
        + ["tri:k=%d"  % k for k in ks()] + ["mu"])

# score metric registry: name -> (eval(pairs), bigger_is_better?)
EVALS = {"relerr": (relerr, False)}

def learner(file: str) -> dict:
  "treatment -> list of folds; each fold = [(want, got), ...] over m*n xval."
  random.seed(the.seed)                           # reproducible per data set
  tbl = TAB(csv(file))
  d   = {t: [] for t in cols()}
  for train, test in xval(tbl.rows):              # default 5x5
    wants = [disty(tbl, r) for r in test]
    mu    = mid(adds(disty(tbl, r) for r in train))
    for k in ks():
      d["mean:k=%d"%k].append([(w, knn(tbl,train,r,k))      for r,w in zip(test,wants)])
      d["tri:k=%d"%k ].append([(w, knn(tbl,train,r,k,True)) for r,w in zip(test,wants)])
    d["mu"].append([(w, mu) for w in wants])
  return d

def main() -> None:
  cli(the)                                        # so the config below reflects flags
  err, big = EVALS[the.eval]
  print(conf(the, "knn.py", xval="5x5"))
  print("# %s, %s=better; ! marks a rank-1 (best) cell; last col = data set\n"
        % (the.eval, "bigger" if big else "lower"))
  learn(cols(), learner, files(), err, name, reverse=big)

if __name__ == "__main__": main()
