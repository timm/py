#!/usr/bin/env python3
"""
sa: simulated annealing over labelled-as-you-go tabular data.
(c) 2026, Tim Menzies <timm@ieee.org>, MIT license
Kernel (csv, Tab, Num/Sym, dists) trimmed from ezr-py/xai.py.

USAGE: python3 sa.py [--key=val ...]

OPTIONS:
  --file   data file       = $MOOT/optimize/misc/auto93.csv
  --seed   random seed     = 1
  --val    holdout labels  = 10
  --pool   unlabelled cap  = 1000
  --budget new labels      = 30
  --temp   start temp      = 0.2
  --cool   cooling rate    = 0.9
  --how    sa | near | rand = sa

Design: val labelled rows seed a best/rest cache (best = top
sqrt(n) by disty). SA walks: mutate current by extrapolating
between pairs of best rows; realize as the nearest unlabelled
pool row; accept on a cheap nearest-neighbour estimate; pay one
true label per accepted step. The cache's best row is the answer.

Layout tricks: `klass` makes one-line slotted classes, so every
class shape sits together below. `x4y` moves each global `x4Y`
function onto class Y as method x, so methods group by purpose
(create, query, dist, mutate), not by class.
"""
import os, re, sys, random
from math import exp, sqrt
from collections.abc import Iterator
TINY = 1e-32
rand = random.random
any  = random.choice   # shadows builtin; csv() avoids it
MOOT = os.environ.get("MOOT") or os.path.expanduser("~/gits/moot")

#-- tricks ------------------------------------------------------
def klass(name: str, **kw) -> type:
  "Slotted class. Callable defaults are called; passed-in values stored."
  def __init__(i, **d):
    for k,v in (kw|d).items():
      setattr(i, k, v() if callable(v) and k not in d else v)
  def __repr__(i): return name + str({k:getattr(i,k) for k in kw})
  return type(name, (), dict(__slots__=tuple(kw), __init__=__init__,
                             __repr__=__repr__))

def x4y(g: dict) -> None:
  "Move each global `x4Y` function onto class Y as method x."
  for name,f in list(g.items()):
    x,_,y = name.rpartition("4")
    if x and callable(f) and isinstance(g.get(y), type):
      setattr(g[y], x, f); del g[name]

#-- klasses -----------------------------------------------------
Num   = klass("Num",   n=0, mu=0, m2=0)
Sym   = klass("Sym",   has=dict)
Tab   = klass("Tab",   names=list, cols=dict, x=list, y=list,
                       goal=dict, rows=list, _mids=None)
Cache = klass("Cache", tbl=None, all=list, best=None, rest=None)

#-- create ------------------------------------------------------
def TAB(src) -> "Tab":
  "Constructor: first row names and types the columns."
  tbl = Tab()
  tbl.names = next(src := iter(src))
  for at,s in enumerate(tbl.names):
    tbl.cols[at] = Num() if s[0].isupper() else Sym()
    if s[-1] == "X": continue
    if s[-1] in "+-": tbl.y += [at]; tbl.goal[at] = s[-1]=="+"
    else:             tbl.x += [at]
  for row in src: tbl.add(row)
  return tbl

def CACHE(tbl) -> "Cache":
  "Constructor: best and rest are Tabs cloned from tbl's header."
  return Cache(tbl=tbl, best=TAB([tbl.names]), rest=TAB([tbl.names]))

def add4Num(i, v, inc=1) -> None:
  if (n := i.n + inc) <= 0: i.n = i.mu = i.m2 = 0; return
  i.n = n
  d = v - i.mu; i.mu += inc * d / n
  i.m2 = max(0, i.m2 + inc * d * (v - i.mu))

def add4Sym(i, v, inc=1) -> None:
  if (c := i.has.get(v,0) + inc) > 0: i.has[v] = c
  else: i.has.pop(v, None)

def add4Tab(i, row: list, inc=1) -> None:
  i._mids = None
  (i.rows.append if inc > 0 else i.rows.remove)(row)
  for at,col in i.cols.items():
    if row[at] != "?": col.add(row[at], inc)

def add4Cache(i, row: list) -> None:
  "Newcomers start in best; overflow demotes best's worst to rest"
  i.all += [row]
  i.best.add(row)
  while len(i.best.rows) > (int(sqrt(len(i.all))) or 1):
    doomed = max(i.best.rows, key=i.tbl.disty)
    i.best.add(doomed, -1); i.rest.add(doomed)

#-- query -------------------------------------------------------
def sd4Num(i) -> float:
  return 0 if i.n < 2 else (i.m2/(i.n - 1))**.5

def mid4Num(i) -> float: return i.mu
def mid4Sym(i): return max(i.has, key=i.has.get)

def mids4Tab(i) -> list:
  "Cached centroid, one mid per column; add invalidates"
  i._mids = i._mids or [col.mid() for col in i.cols.values()]
  return i._mids

#-- dist --------------------------------------------------------
def norm4Num(i, v) -> float|str:
  if v == "?": return v
  z = (v - i.mu) / (i.sd() + TINY)
  return 1 / (1 + exp(-1.7 * max(-3, min(3, z))))

def minkowski(vals, p=2) -> float:
  tot = nn = 0
  for v in vals: tot += v**p; nn += 1
  return (tot / (nn or 1)) ** (1/p)

def distx4Sym(i, u, v) -> float|bool:
  return 1 if u == v == "?" else u != v

def distx4Num(i, u, v) -> float:
  if u == v == "?": return 1
  u, v = i.norm(u), i.norm(v)
  if u == "?": u = 1 if v < .5 else 0
  if v == "?": v = 1 if u < .5 else 0
  return abs(u - v)

def disty4Tab(i, row: list) -> float:
  return minkowski(abs(i.cols[a].norm(row[a]) - i.goal[a])
                   for a in i.y if row[a] != "?")

def distx4Tab(i, r1: list, r2: list) -> float:
  return minkowski(i.cols[a].distx(r1[a], r2[a]) for a in i.x)

def neary4Cache(i, row: list) -> float:
  "disty of the labelled row nearest in x-space"
  return i.tbl.disty(min(i.all, key=lambda r: i.tbl.distx(r, row)))

#-- mutate ------------------------------------------------------
def mutate4Cache(i, row: list, p=1.0) -> list:
  """Walk step: copy of row where each x cell, with probability
  p, is replaced by a value extrapolated between two best rows
  (t in -.25..1.25); the cache hides in here and nowhere else."""
  b1, b2 = any(i.best.rows), any(i.best.rows)
  out = row[:]
  for a in i.tbl.x:
    u, v = b1[a], b2[a]
    if rand() > p or u == v == "?": continue
    if "?" in (u, v): out[a] = v if u == "?" else u
    elif isinstance(i.tbl.cols[a], Sym): out[a] = u if rand() < .5 else v
    else: out[a] = v + (rand()*1.5 - .25) * (u - v)
  return out

#-- lib ---------------------------------------------------------
def thing(s: str) -> int|float|bool|str:
  if (s[1:] if s[:1]=="-" else s).isdigit(): return int(s)
  try: return float(s)
  except ValueError: return s=="True" or (s!="False" and s)

def settings(doc: str) -> type:
  pat = r"--(\w+)\s+[^=\n]*=\s*(\S+)"
  return type("the", (), {k: thing(v) for k,v in re.findall(pat, doc)})

def csv(file: str) -> Iterator[list]:
  file = os.path.expanduser(file.replace("$MOOT", MOOT, 1))
  with open(file, encoding="utf-8") as f:
    for line in f:
      row = [x.strip() for x in line.split(",")]
      if "".join(row): yield [thing(x) for x in row]

#-- optimizers --------------------------------------------------
def rando(tbl) -> "Cache":
  "Baseline: spend the whole budget on random rows"
  c = CACHE(tbl)
  for row in random.sample(tbl.rows, the.val + the.budget):
    c.add(row)
  return c

def near(tbl) -> "Cache":
  """Baseline: classic acquire. Scan the pool; label the first
  item sitting nearer its best rows than its rest rows."""
  rows = random.sample(tbl.rows, len(tbl.rows))
  val, pool = rows[:the.val], rows[the.val:the.val+the.pool]
  c = CACHE(tbl)
  for row in val: c.add(row)
  d = lambda rs, r: min(tbl.distx(r1, r) for r1 in rs)
  while len(c.all) - the.val < the.budget and pool:
    row = pool[0]
    for r in pool:
      if d(c.best.rows, r) < d(c.rest.rows, r): row = r; break
    pool.remove(row); c.add(row)
  return c

def accept(y: float, ycur: float, t: float) -> bool:
  "The whole SA kernel: better, or lucky at this temperature"
  return y < ycur or rand() < exp(-(y - ycur)/(t + TINY))

def sa(tbl) -> "Cache":
  rows = random.sample(tbl.rows, len(tbl.rows))
  val, pool = rows[:the.val], rows[the.val:the.val+the.pool]
  c = CACHE(tbl)
  for row in val: c.add(row)                    # seed: holdout
  cur   = min(c.all, key=tbl.disty)
  ycur, t = tbl.disty(cur), the.temp
  for _ in range(32 * the.budget):               # hard iteration cap
    if len(c.all) - the.val >= the.budget or not pool: break
    synth = c.mutate(cur)
    cand  = min(pool, key=lambda r: tbl.distx(synth, r))
    if accept(c.neary(cand), ycur, t):           # cheap estimate..
      pool.remove(cand)                          # ..then pay a label
      c.add(cand)
      cur, ycur = cand, tbl.disty(cand)
    t *= the.cool
  return c

#-- main --------------------------------------------------------
def main() -> None:
  for arg in sys.argv[1:]:
    if arg[:2]=="--" and "=" in arg:
      k,v = arg[2:].split("=",1)
      if hasattr(the, k): setattr(the, k, thing(v))
  random.seed(the.seed)
  tbl  = TAB(csv(the.file))
  c    = dict(sa=sa, near=near, rand=rando)[the.how](tbl)
  base = sorted(tbl.disty(r) for r in tbl.rows)
  best = min(c.best.rows, key=tbl.disty)
  print("rows %s  labels %s+%s  best %.3f  (corpus best %.3f, median %.3f)"
        % (len(tbl.rows), the.val, len(c.all) - the.val,
           tbl.disty(best), base[0], base[len(base)//2]))
  print("  " + ", ".join("%s=%s" % (n,v)
        for n,v in zip(tbl.names, best) if n[-1] != "X"))

x4y(globals())
the = settings(__doc__)
if __name__ == "__main__": main()
