#!/usr/bin/env python3
"""
hair: tiny AI, explained. This file is the library; the files in
eg/ are short examples that use it.
(c) 2026, Tim Menzies <timm@ieee.org>, MIT license
Data model trimmed from ezr-py/xai.py.

USE: from hair import *

Part one is tools that are not about data: one-line classes,
reading csv files, command-line flags, printing a table.
Part two is data: tables of rows, summarized by one Num or Sym
per column, and the distances between rows.

A csv file's first row names the columns. First character upper
case means numeric (Num), else symbolic (Sym). Last character:
  +  -   goal to maximize / minimize   (a y column)
  !      the class                     (tbl.klass)
  X      ignore this column
  else   an input                      (an x column)
Cells holding "?" are unknown.

distx is the distance between two rows over their x columns.
disty is a row's distance to the best possible y values
(0 = best), so a smaller disty is a better row.

Layout tricks: `klass` makes one-line slotted classes, so every
class shape can sit together. `x4y` moves each global `x4Y`
function onto class Y as method x, so methods can be grouped by
purpose (create, dist, ...), not by class.
"""
import os, re, sys
from math import exp, log2
from collections.abc import Iterator

TINY = 1e-32
MOOT = os.environ.get("MOOT") or os.path.expanduser("~/gits/moot")

#-- tricks -----------------------------------------------------
def klass(name: str, **kw) -> type:
  def __init__(i, **d):
    for k,v in (kw|d).items():
      setattr(i, k, v() if callable(v) and k not in d else v)
  def __repr__(i): return name + str({k:getattr(i,k) for k in kw})
  return type(name, (), dict(__slots__=tuple(kw), __init__=__init__,
                             __repr__=__repr__))

def x4y(g: dict) -> None:
  for name,f in list(g.items()):
    x,_,y = name.rpartition("4")
    if x and callable(f) and isinstance(g.get(y), type):
      setattr(g[y], x, f); del g[name]

#-- strings to things ------------------------------------------
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

#-- command line -----------------------------------------------
def cli(the: type) -> None:
  "Update `the` from the command line's --key=val flags"
  for arg in sys.argv[1:]:
    if arg[:2]=="--" and "=" in arg:
      k,v = arg[2:].split("=",1)
      if hasattr(the, k): setattr(the, k, thing(v))

ALL = """misc/auto93 config/SS-A config/SS-M systems/HSQLDB
         systems/LLVM systems/storm systems/redis sales_data/socks
         process/coc1000 process/pom3d"""

def files(the: type) -> list:
  "Paths named by --dir and --file (--file=all means ALL)"
  names = ALL.split() if the.file == "all" else the.file.split(",")
  return ["%s/%s.csv" % (the.dir, name) for name in names]

def name(file: str) -> str:
  "a/b/auto93.csv -> auto93"
  return file.split("/")[-1].removesuffix(".csv")

#-- show -------------------------------------------------------
def table(names: list, labels: list, cols: list) -> None:
  "Print one column per name, one row per label; widths fit"
  rows = [[""] + names] + [[label] + [round(col[j]) for col in cols]
                           for j, label in enumerate(labels)]
  wide = [max(len(str(x)) for x in col) for col in zip(*rows)]
  for row in rows:
    print("%-*s" % (wide[0], row[0]) + "".join(
          "  %*s" % wx for wx in zip(wide[1:], row[1:])))

#-- klasses ----------------------------------------------------
Num   = klass("Num",   n=0, mu=0, m2=0)
Sym   = klass("Sym",   has=dict)
Tab   = klass("Tab",   names=list, cols=dict, x=list, y=list,
                       goal=dict, rows=list, mids=None, klass=None)

#-- create -----------------------------------------------------
def TAB(src) -> "Tab":
  tbl = Tab()
  tbl.names = next(src := iter(src))
  for at,s in enumerate(tbl.names):
    tbl.cols[at] = Num() if s[0].isupper() else Sym()
    if   s[-1] == "X": continue
    elif s[-1] == "!": tbl.klass = at
    elif s[-1] in "+-": tbl.y += [at]; tbl.goal[at] = s[-1]=="+"
    else:               tbl.x += [at]
  for row in src: tbl.add(row)
  return tbl

def add4Num(i, v, inc=1) -> None:
  if (n := i.n + inc) <= 0: i.n = i.mu = i.m2 = 0; return
  i.n = n
  d = v - i.mu; i.mu += inc * d / n
  i.m2 = max(0, i.m2 + inc * d * (v - i.mu))

def add4Sym(i, v, inc=1) -> None:
  if (c := i.has.get(v,0) + inc) > 0: i.has[v] = c
  else: i.has.pop(v, None)

def add4Tab(i, row: list, inc=1) -> None:
  i.mids = None
  (i.rows.append if inc > 0 else i.rows.remove)(row)
  for at,col in i.cols.items():
    if row[at] != "?": col.add(row[at], inc)

def div4Num(i) -> float: return 0 if i.n < 2 else (i.m2/(i.n - 1))**.5

def div4Sym(i) -> float:
  "entropy"
  N = sum(i.has.values()) or 1
  return -sum(v/N * log2(v/N) for v in i.has.values() if v)

def norm4Num(i, v) -> float|str:
  if v == "?": return v
  z = (v - i.mu) / (i.div() + TINY)
  return 1 / (1 + exp(-1.7 * max(-3, min(3, z))))

def mid4Num(i) -> float: return i.mu
def mid4Sym(i): return max(i.has, key=i.has.get)

def mid4Tab(i) -> list:
  "Cached centroid, one mid per column; add invalidates"
  i.mids = i.mids or [col.mid() for col in i.cols.values()]
  return i.mids

#-- dist -------------------------------------------------------
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

def distx4Tab(i, r1: list, r2: list) -> float:
  return minkowski(i.cols[a].distx(r1[a], r2[a]) for a in i.x)

def disty4Tab(i, row: list) -> float:
  return minkowski(abs(i.cols[a].norm(row[a]) - i.goal[a])
                   for a in i.y if row[a] != "?")

x4y(globals())
