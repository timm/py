"""
stats: are two result sets different? which treatments win?
(c) 2026, Tim Menzies <timm@ieee.org>, MIT license

`cliffs`+`ks`+`cohen` feed `same`, a conservative equality: all
three must agree before two result sets are called equal.
`rank` is a Scott-Knott: sort treatments, recursively split
where the split explains the most variance, stop when `same`.
`confuse` scores a classifier from a list of (got, want) pairs.
"""
import random
from bisect import bisect_left, bisect_right
from .lib import o, isa, TINY
from .data import Sym, div

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

def same(xs: list, ys: list, coh=0.35, cliff=0.195, k=1.36) -> bool:
  "Indistinguishable: small Cohen effect AND Cliff's delta AND KS."
  return (cohen( xs, ys) <= coh
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

def report(cols: list, src, reverse=False, decs=0, show=True) -> dict:
  """Matrix of ranked results, one row per data set, streamed.

  cols: treatment names (no spaces, e.g. "mean:k=1").
  src : iterable of (dataset_name, {treatment: [scores]}).
  Per data set: rank the treatments (reverse=True if bigger is better),
  print one row -- each cell is the median score (to `decs` decimals,
  default 0), right-justified under its column name so columns line up
  as rows stream; a "!" follows any rank-1 (winning) cell, else ",". The
  data-set name is the last column. A final row gives each treatment's
  WIN% -- the integer percent of data sets it topped. Rows print as they
  complete, so a crash leaves a partial table. Returns the win counts.
  """
  mid = lambda l: sorted(l)[len(l)//2]
  w   = [len(c) for c in cols]                      # widths from the header
  win = {t: 0 for t in cols}
  n   = 0
  if show: print(", ".join(cols) + ", dataset")
  for ds, d in src:
    n += 1
    rk, row = rank(d, reverse), ""
    for i, t in enumerate(cols):
      cell = "%.*f" % (decs, mid(d[t]))
      row += "%*s%s" % (w[i], cell, "! " if rk[t] == 1 else ", ")
      if rk[t] == 1: win[t] += 1
    if show: print(row + str(ds), flush=True)       # dataset = last column
  if show:
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

def relerr(pairs: list) -> float:
  "100 * mean|want-got| / mean want, over (want,got) pairs. Lower is better."
  n   = len(pairs) + TINY
  gap = sum(abs(w-g) for w,g in pairs) / n
  mu  = sum(w for w,_ in pairs) / n
  return 100 * gap / (mu + TINY)

def learn(cols: list, learner, files, eval, nameof=str,
          reverse=False, decs=0) -> dict:
  """One call for every learner. `learner(file)` returns
  {treatment: [fold, ...]} where a fold is a list of (want, got)
  pairs; `eval(pairs)` scores one fold (e.g. relerr, accuracy). Each
  fold becomes one number, so treatments get a distribution to rank.
  Streams a ranked row per data set (see `report`)."""
  def score(f):
    return {t: [eval(fold) for fold in folds]
            for t, folds in learner(f).items()}
  return report(cols, ((nameof(f), score(f)) for f in files),
                reverse=reverse, decs=decs)

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

def auc(pairs: list) -> float:
  """Prevalence-weighted one-vs-rest AUC, 0..100, over (want, posts)
  pairs where posts = {class: probability}. Per class: AUC = chance a
  random in-class row scores above a random out-of-class one (Mann-
  Whitney U, ties averaged); weight each by its in-class count."""
  classes = sorted({w for w,_ in pairs} | {c for _,p in pairs for c in p})
  wsum = tot = 0
  for c in classes:
    xs = sorted((p.get(c, 0.0), w == c) for w, p in pairs)   # by score
    npos = sum(y for _,y in xs); nneg = len(xs) - npos
    if not npos or not nneg: continue          # AUC undefined for this class
    rsum, i = 0, 0
    while i < len(xs):                          # average ranks within ties
      j = i
      while j < len(xs) and xs[j][0] == xs[i][0]: j += 1
      rank = (i + 1 + j) / 2                    # 1-based mean rank of the tie
      rsum += rank * sum(y for _,y in xs[i:j])
      i = j
    a = (rsum - npos*(npos+1)/2) / (npos*nneg)  # U / (npos*nneg)
    wsum += npos * a; tot += npos
  return 100 * wsum / (tot + TINY)
