#!/usr/bin/env python3
"""
knn: guess a row's disty from its k nearest rows.
Table 1: kernels flat (all neighbours count the same) and tri
(nearer counts more: weight 1 - d/dmax) at k=1..5, plus mu
(always guess the train mean). Table 2: flat kernel at k=4,
trained on 32..512 rows, or all, on BIGS --dir csvs with ROWS
rows. Both: 5 repeats of a 2-way cross-val, scored by RAE, on at
most --test test rows and --pool train rows.
(c) 2026, Tim Menzies <timm@ieee.org>, MIT license
"""
import os, sys  # make hair importable from source or installed
sys.path.insert(0, os.path.dirname(os.path.dirname(
                   os.path.abspath(__file__))))
from functools import partial
from hair import *

BEST_K = 4                        # table 1's pick, used by table 2
SIZES  = [32, 64, 128, 256, 512]  # train sizes for table 2
ROWS   = range(1000, 10000)       # table 2: sets with these #rows
BIGS   = 20                       # table 2: how many such sets

#-- kernels: guess row's disty from rows (nearest first) ------------
def flat(tbl, rows: list, row=None) -> float:
  return mid(adds(disty(tbl, r) for r in rows))

def tri(tbl, rows: list, row: list) -> float:
  ds = [distx(tbl, r, row) for r in rows]
  ws = [1 - d/(max(ds) + TINY) for d in ds]
  return (sum(w*disty(tbl, r) for w, r in zip(ws, rows))
          / (sum(ws) + TINY))

#-- learner: treatment t = o(name, f, k, n); k,n None = all ------
def knn(tbl, train, test, ts: list):
  "Learner: sort train once per test row; every t slices that sort."
  for row in test:                     # xval shuffled train, so
    want = disty(tbl, row)             # i < n = a random n rows
    ns = sorted(range(len(train)), 
                key= lambda i: distx(tbl, train[i], row))
    for t in ts:
      rows = [train[i] for i in ns if t.n is None or i < t.n]
      yield t.name, want, t.f(tbl, rows[:t.k], row)

def main() -> None:
  cli(the)                         
  print(conf(the, "knn.py", eval="rae", xval="5x2"))
  print("# rae: 100 = no better than guessing the mean; lower=better\n"
        "# ! marks a rank-1 (best) cell; last col = data set")
  for x in [o(title = "kernels x k", 
              files = files(),
              ts    = [o(name=f"{f.__name__}:k={k}", f=f, k=k, n=None)
                       for f in (flat, tri) for k in range(1, the.k+1)]
                      + [o(name="mu", f=flat, k=None, n=None)]
              ),           
            o(title = f"flat:k={BEST_K} x train size (all = pool)",
              files = csvs(the.dir, ROWS, BIGS, the.seed),
              ts    = [o(name=f"n={n or 'all'}", f=flat, k=BEST_K, n=n)
                       for n in SIZES + [None]]
              )]:
    print(f"\n# {x.title}\n")
    learn(x.files, partial(knn, ts=x.ts), rae, n=2)

if __name__ == "__main__": main()
