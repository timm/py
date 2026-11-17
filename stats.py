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
from lib import o, TINY

def cliffs(xs: list, ys: list) -> float:
  "Cliff's delta effect size in 0..1 (0 = identical)"
  ys = sorted(ys); m = len(ys)
  gt = sum(bisect_left(ys, x)      for x in xs)
  lt = sum(m - bisect_right(ys, x) for x in xs)
  return abs(gt - lt) / (len(xs) * m + TINY)

def ks(xs: list, ys: list) -> float:
  "Kolmogorov-Smirnov: max gap between the two CDFs"
  xs, ys = sorted(xs), sorted(ys); n, m = len(xs), len(ys)
  gap = lambda v: abs(bisect_right(xs,v)/n - bisect_right(ys,v)/m)
  return max(map(gap, xs + ys))

def cohen(xs: list, ys: list, eps=0.35) -> bool:
  "Small effect: |mean gap| < eps * pooled stdev"
  mu  = lambda l: sum(l)/len(l)
  var = lambda l: sum((x-mu(l))**2 for x in l)/(len(l)-1+TINY)
  n, m = len(xs), len(ys)
  sd = (((n-1)*var(xs) + (m-1)*var(ys))/(n+m-2+TINY))**.5
  return abs(mu(xs) - mu(ys)) <= eps * (sd + TINY)

def same(xs: list, ys: list, cliff=0.195, conf=1.36) -> bool:
  "True if xs,ys are statistically indistinguishable"
  if not cohen(xs, ys): return False
  if cliffs(xs, ys) > cliff: return False
  n, m = len(xs), len(ys)
  return ks(xs, ys) <= conf * ((n + m) / (n * m)) ** 0.5

def rank(d: dict) -> dict:
  "Scott-Knott: treatment name -> rank (1 = best = lowest)"
  out = {}
  cat = lambda ns: [x for n in ns for x in d[n]]
  mu  = lambda l: sum(l)/len(l)
  def grow(names, r):
    if len(names) < 2:
      for n in names: out[n] = r
      return r
    all = cat(names)
    cut = max(range(1, len(names)), key=lambda i:
              len(cat(names[:i])) * (mu(cat(names[:i])) - mu(all))**2 +
              len(cat(names[i:])) * (mu(cat(names[i:])) - mu(all))**2)
    l, rt = names[:cut], names[cut:]
    if same(cat(l), cat(rt)):
      for n in names: out[n] = r
      return r
    return grow(rt, grow(l, r) + 1)
  med = lambda n: sorted(d[n])[len(d[n])//2]
  grow(sorted(d, key=med), 1)
  return out

def split(rows: list, n1: int, n2: int) -> tuple:
  "Shuffle; up to n1 test rows (max half), up to n2 train rows"
  rows = random.sample(rows, len(rows))
  n    = min(n1, len(rows)//2)
  return rows[:n], rows[n:n+n2]

def relerr(wants: list, gots: list) -> float:
  "100 * mean |want - got| / mean want"
  gap = sum(abs(w-g) for w,g in zip(wants,gots)) / len(wants)
  return 100 * gap / (sum(wants)/len(wants) + TINY)

def confuse(pairs: list) -> o:
  "Score (got, want) pairs; per-class counts and overall accuracy"
  lbls = sorted({w for _,w in pairs} | {g for g,_ in pairs})
  N    = len(pairs)
  per  = []
  for l in lbls:
    tp = sum(g==w==l   for g,w in pairs)
    fp = sum(g==l!=w   for g,w in pairs)
    fn = sum(w==l!=g   for g,w in pairs)
    per.append(o(label=l, tp=tp, fp=fp, fn=fn, tn=N-tp-fp-fn,
                 prec   = 100*tp/(tp+fp+TINY),
                 recall = 100*tp/(tp+fn+TINY)))
  return o(acc=100*sum(g==w for g,w in pairs)/(N+TINY), classes=per)
