"""
stats: are two result sets different? which treatments win?
(c) 2026, Tim Menzies <timm@ieee.org>, MIT license

`cliffs`+`ks`+`cohen` feed `same`, a conservative equality: all
three must agree before two result sets are called equal.
`rank` is a Scott-Knott: sort treatments, recursively split
where the split explains the most variance, stop when `same`.
`confuse` scores a classifier from a list of (got, want) pairs.
"""
import random, multiprocessing as mp
from bisect import bisect_left, bisect_right
from .lib import o, isa, TINY
from .about import the
from .lib import csv
from .data import Sym, div, TAB

def xpect(a, b) -> float:
  "Size-weighted mean spread of two summaries (lower = better)"
  size = lambda c: sum(c.values()) if isa(c, Sym) else c[0]
  n, m = size(a), size(b)
  return (div(a)*n + div(b)*m) / (n + m + TINY)

def cliffs(xs: list, ys: list) -> float:
  "Cliff's delta effect size in 0..1 (0 = identical)"
  ys = sorted(ys); m = len(ys)
  gt = sum(bisect_left(ys, x)      for x in xs)
  lt = sum(m - bisect_right(ys, x) for x in xs)
  return abs(gt - lt) / (len(xs) * m + TINY)

def ks(xs: list, ys: list) -> float:
  "Kolmogorov-Smirnov: max CDF gap, in units of the 0.05 critical value"
  xs, ys = sorted(xs), sorted(ys); n, m = len(xs), len(ys)
  gap = lambda v: abs(bisect_right(xs,v)/n - bisect_right(ys,v)/m)
  return max(map(gap, xs + ys)) / (((n + m) / (n * m)) ** 0.5 + TINY)

def cohen(xs: list, ys: list) -> float:
  "Effect size: |mean gap| / pooled stdev"
  mu  = lambda l: sum(l)/len(l)
  var = lambda l: sum((x-mu(l))**2 for x in l)/(len(l)-1+TINY)
  n, m = len(xs), len(ys)
  sd = (((n-1)*var(xs) + (m-1)*var(ys))/(n+m-2+TINY))**.5
  return abs(mu(xs) - mu(ys)) / (sd + TINY)

def same(xs: list, ys: list, cliff=0.195, k=1.36) -> bool:
  "Indistinguishable: small Cohen effect (--eps) AND Cliff's delta AND KS."
  return (cohen( xs, ys) <= the.eps
      and cliffs(xs, ys) <= cliff
      and ks(    xs, ys) <= k)

def rank(d: dict, reverse=False) -> dict:
  "Treatment->rank (1=best). By median; new rank when not `same`. reverse=bigger-better."
  mid  = lambda n: sorted(d[n])[len(d[n])//2]
  out, r, pool = {}, 1, []
  for n in sorted(d, key=mid, reverse=reverse):   # best first
    if pool and not same(pool, d[n]):             # a real gap from group so far
      r, pool = r + 1, []
    pool += d[n]
    out[n] = r
  return out

def report(src, reverse=False, decs=0, show=True) -> dict:
  """Matrix of ranked results, one row per data set, streamed.

  src: iterable of (about, {treatment: [scores]}); columns are the
  first data set's treatments (names with no spaces), in order.
  about: the data set's name, or a dict of extra columns (e.g.
  {"#x": 4, "dataset": "auto93"}). Per data set: rank the
  treatments (reverse=True if bigger is better), print one row -- each cell is the median score (to
  `decs` decimals), right-justified under its column name; a "!"
  follows any rank-1 (winning) cell, else ",". The about columns
  come last. A final row gives each treatment's WIN% -- the
  integer percent of data sets it topped. Rows print as they
  complete, so a crash leaves a partial table. Returns win counts.
  """
  mid = lambda l: sorted(l)[len(l)//2]
  cols, win, n = None, {}, 0
  for ds, d in src:
    if not isa(ds, dict): ds = {"dataset": ds}
    if cols is None:                               # header from 1st set
      cols = list(d); w = [len(c) for c in cols]
      win  = {t: 0 for t in cols}
      if show: print(", ".join(cols + list(ds)))
    n += 1
    rk, row = rank(d, reverse), ""
    for i, t in enumerate(cols):
      cell = "%.*f" % (decs, mid(d[t]))
      row += "%*s%s" % (w[i], cell, "! " if rk[t] == 1 else ", ")
      if rk[t] == 1: win[t] += 1
    *pre, last = ds.items()                        # last = unpadded
    row += "".join("%*s, " % (len(k), v) for k, v in pre)
    if show: print(row + str(last[1]), flush=True)
  if show and cols:
    pct = "".join("%*d, " % (w[i], round(100*win[t]/max(n,1)))
                  for i, t in enumerate(cols))
    print(pct + "WIN%")
  return win

def split(rows: list, n1: int, n2: int) -> tuple:
  "Shuffle; up to n1 test rows (max half), up to n2 train rows"
  rows = random.sample(rows, len(rows))
  n    = min(n1, len(rows)//2)
  return rows[:n], rows[n:n+n2]

def xval(rows: list, m=5, n=5):
  "m*n cross-val: reshuffle, then n folds; yield (train, test) each."
  for _ in range(m):
    rows = random.sample(rows, len(rows))        # new order per repeat
    for k in range(n):
      test  = rows[k::n]                          # every n-th row, offset k
      train = [r for i, r in enumerate(rows) if i % n != k]
      yield train, test

def rae(pairs: list) -> float:
  """Relative absolute error: 100 * sum|want-got| / sum|want-mean|
  over (want,got) pairs. 100 = no better than always guessing the
  mean; lower is better. The shift and scale of want cancel
  out, so data sets compare directly."""
  mu = sum(w for w,_ in pairs) / (len(pairs) + TINY)
  return 100 * (sum(abs(w-g) for w,g in pairs)
                / (sum(abs(w-mu) for w,_ in pairs) + TINY))

_job = None                    # learn's per-file closure, for workers
def _run(f): return _job(f)    # top-level, so Pool can pickle it

def learn(files, learner, eval, nameof=str, reverse=False,
          decs=0, m=5, n=5, about=None) -> dict:
  """One call for every learner. Per file: seed, load, m*n xval.
  Per fold, `learner(tbl, train, test)` yields (treatment, want,
  got) guesses; `eval(pairs)` scores each treatment's (want, got)
  pairs (e.g. rae, aucs), so each treatment gets one number per
  fold, i.e. a distribution to rank. `about(tbl)` may add columns
  (a dict) before the data-set name. Files run on --jobs cpus (0:
  all; forked, so each worker sees `score`); each row prints as
  soon as its file is done, so in finish order (see `report`)."""
  global _job
  def score(f):
    random.seed(the.seed)                        # reproducible per file
    tbl, out = TAB(csv(f)), {}
    for train, test in xval(tbl.rows, m, n):
      fold = {}
      for t, want, got in learner(tbl, train, test):
        fold.setdefault(t, []).append((want, got))
      for t, pairs in fold.items():
        out.setdefault(t, []).append(eval(pairs))
    return (about(tbl) if about else {}) | {"dataset": nameof(f)}, out
  _job = score
  if the.jobs == 1: return report(map(score, files), reverse, decs)
  with mp.get_context("fork").Pool(the.jobs or None) as p:
    return report(p.imap_unordered(_run, files), reverse, decs)

def confuse(pairs: list) -> o:
  "Score (want, got) pairs; per-class counts and overall accuracy"
  lbls = sorted({w for w,_ in pairs} | {g for _,g in pairs})
  N    = len(pairs)
  per  = []
  for l in lbls:
    tp = sum(l==w==g   for w,g in pairs)
    fp = sum(l==g!=w   for w,g in pairs)
    fn = sum(l==w!=g   for w,g in pairs)
    per.append(o(label=l, tp=tp, fp=fp, fn=fn, tn=N-tp-fp-fn,
                 prec   = 100*tp/(tp+fp+TINY),
                 recall = 100*tp/(tp+fn+TINY)))
  return o(acc=100*sum(w==g for w,g in pairs)/(N+TINY), classes=per)

def auc(xs: list) -> float:
  """ROC AUC (Hanley & McNeil) for one class, 0..1, over (score,
  in_class) pairs: the chance a random in-class row scores above a
  random out-of-class one (Mann-Whitney U, ties averaged)."""
  xs   = sorted(xs)                             # by score
  npos = sum(y for _,y in xs); nneg = len(xs) - npos
  if not npos or not nneg: return None          # undefined for this class
  rsum, i = 0, 0
  while i < len(xs):                            # average ranks within ties
    j = i
    while j < len(xs) and xs[j][0] == xs[i][0]: j += 1
    rsum += (i + 1 + j) / 2 * sum(y for _,y in xs[i:j])
    i = j
  return (rsum - npos*(npos+1)/2) / (npos*nneg) # U / (npos*nneg)

def aucs(pairs: list) -> float:
  """Expected AUC over all classes, 0..100: one-vs-rest auc per
  class, times that class's share of the rows. For classifiers
  that emit scores; pairs = (want, posts), posts = {class: prob}."""
  classes = sorted({w for w,_ in pairs} | {c for _,p in pairs for c in p})
  wsum = tot = 0
  for c in classes:
    a = auc([(p.get(c, 0.0), w == c) for w, p in pairs])
    if a is None: continue
    n = sum(w == c for w,_ in pairs)
    wsum += n * a; tot += n
  return 100 * wsum / (tot + TINY)
