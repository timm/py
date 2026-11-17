#!/usr/bin/env python3
"""
egnb: naive bayes. Learn one Tab per class; classify a row by
which class's columns like it most. Train on each row after
predicting it, so accuracy is on unseen rows. Data: --file
names in $MOOT/classify (with a class! column).
(c) 2026, Tim Menzies <timm@ieee.org>, MIT license
"""
import sys; sys.path[0] += "/.."   # find hair in the parent
from math import log, pi
from hair import *

DATA = "diabetes soybean breast.w vote".split()

def like(col, v, prior: float) -> float:
  "log P(v|class); gaussian for Nums, m-estimate for Syms"
  if isa(col, Sym):
    N = sum(col.values())
    return log((col.get(v,0) + prior) / (N + 1 + TINY))
  n, mu, _ = col
  sd = div(col) + TINY
  return -(v - mu)**2/(2*sd*sd) - log(sd * (2*pi)**.5)

def classify(tabs: dict, row: list, n: int):
  "Most liked class; prior from class frequency"
  def score(tab):
    prior = (len(tab.rows) + 1) / (n + len(tabs))
    return log(prior) + sum(like(tab.cols[a], row[a], prior)
                            for a in tab.x if row[a] != "?")
  return max(tabs, key=lambda k: score(tabs[k]))

def nb(file: str) -> float:
  tbl, tabs, pairs = TAB(csv(file)), {}, []
  for n, row in enumerate(tbl.rows):
    want = row[tbl.klass]
    if n > 5 and want in tabs:
      pairs.append((classify(tabs, row, n), want))
    tabs.setdefault(want, clone(tbl))
    addRow(tabs[want], row)
  return [confuse(pairs).acc]

if __name__ == "__main__":
  cli(the)
  fs = ["$MOOT/classify/%s.csv" % f for f in DATA]
  table([name(f) for f in fs], ["acc"], [nb(f) for f in fs])
