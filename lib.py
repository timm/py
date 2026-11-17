"""
lib: general stuff; nothing here knows about tables or AI.
(c) 2026, Tim Menzies <timm@ieee.org>, MIT license
"""
import os, re, sys
from types import SimpleNamespace as o
from collections.abc import Iterator

isa  = isinstance
TINY = 1e-32
MOOT = os.environ.get("MOOT") or os.path.expanduser("~/gits/moot")

def thing(s: str) -> int|float|bool|str:
  "Coerce a string to int/float/bool, else leave as str"
  if (s[1:] if s[:1]=="-" else s).isdigit(): return int(s)
  try: return float(s)
  except ValueError: return s=="True" or (s!="False" and s)

def settings(doc: str) -> o:
  "Parse '--key ... = val' lines of doc into an o()"
  pat = r"--(\w+)\s+[^=\n]*=\s*(\S+)"
  return o(**{k: thing(v) for k,v in re.findall(pat, doc)})

def cli(the: o) -> None:
  "Update `the` from the command line's --key=val flags"
  for arg in sys.argv[1:]:
    if arg[:2]=="--" and "=" in arg:
      k,v = arg[2:].split("=",1)
      if hasattr(the, k): setattr(the, k, thing(v))

def csv(file: str) -> Iterator[list]:
  "Yield typed rows; a leading $MOOT expands to the moot repo"
  file = os.path.expanduser(file.replace("$MOOT", MOOT, 1))
  with open(file, encoding="utf-8") as f:
    for line in f:
      row = [x.strip() for x in line.split(",")]
      if "".join(row): yield [thing(x) for x in row]

def table(names: list, labels: list, cols: list) -> None:
  "Print one column per name, one row per label; widths fit"
  rows = [[""] + names] + [[label] + [round(col[j]) for col in cols]
                           for j, label in enumerate(labels)]
  wide = [max(len(str(x)) for x in col) for col in zip(*rows)]
  for row in rows:
    print("%-*s" % (wide[0], row[0]) + "".join(
          "  %*s" % wx for wx in zip(wide[1:], row[1:])))
