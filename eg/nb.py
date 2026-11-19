#!/usr/bin/env python3
"""
egnb: naive bayes. Learn one Tab per class; classify a row by
which class's columns like it most. Train on each row after
predicting it, so accuracy is on unseen rows. Data: --file
names in $MOOT/classify (with a class! column).
(c) 2026, Tim Menzies <timm@ieee.org>, MIT license
"""
import os, sys  # make hair importable whether run from source or installed
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from math import log, pi, exp
from hair import *

DATA   = "diabetes soybean breast.w vote".split()
SMOOTH = [1, 2, 3]                                    # m and k each range here
EVALS  = {"auc": (aucs, True),                        # bigger = better
          "acc": (lambda ps: 100*sum(w==max(p,key=p.get) for w,p in ps)
                               /(len(ps)+TINY), True)}

def like(col, v, prior: float, m: float) -> float:
  "log P(v|class); gaussian for Nums, m-estimate (weight m) for Syms"
  if isa(col, Sym):
    N = sum(col.values())
    return log((col.get(v,0) + m*prior) / (N + m + TINY))
  n, mu, _ = col
  sd = div(col) + TINY
  return -(v - mu)**2/(2*sd*sd) - log(sd * (2*pi)**.5)

def logp(tab, row, n, nh, m, k) -> float:
  "log posterior score of one class. k = prior Laplace, m = feature m-estimate."
  prior = (len(tab.rows) + k) / (n + k*nh)
  return log(prior) + sum(like(tab.cols[a], row[a], prior, m)
                          for a in tab.x if row[a] != "?")

def posts(tabs: dict, row: list, n, m, k) -> dict:
  "Normalized posteriors {class: prob}; subtract max-log so exp won't underflow."
  ls = {c: logp(t, row, n, len(tabs), m, k) for c, t in tabs.items()}
  hi = max(ls.values())
  es = {c: exp(v - hi) for c, v in ls.items()}
  z  = sum(es.values()) + TINY
  return {c: e/z for c, e in es.items()}

def nb(tbl, train, test):
  "Yield (smoothing, want, posteriors) per test row, m-estimate x Laplace."
  tabs = {}
  for row in train:                               # train: one Tab per class
    c = row[tbl.klass]; tabs.setdefault(c, clone(tbl)); addRow(tabs[c], row)
  for row in test:
    for m in SMOOTH:
      for k in SMOOTH:
        yield f"m={m}:k={k}", row[tbl.klass], posts(tabs, row, len(train), m, k)

def main() -> None:
  cli(the)
  metric   = the.eval if the.eval in EVALS else "auc"
  err, big = EVALS[metric]
  fs = ["$MOOT/classify/%s.csv" % f for f in DATA]
  print(conf(the, "nb.py", eval=metric, xval="5x5"))
  print("# %s (weighted, 0..100), bigger=better; ! marks rank-1; last col = data set\n"
        % metric)
  learn(fs, nb, err, name, reverse=big)

if __name__ == "__main__": main()
