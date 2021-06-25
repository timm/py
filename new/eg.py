import sys, copy, random, traceback
from exam import __doc__ as doc
from exam import options,Head,Data,slurp 
from etc import csv,cli,red,green
from eg import Num,Sym,Tab

def all(funs,defaults,help):
  fails = 0
  the = cli(defaults,help)
  for s, fun in funs.items():
    if s[:2] != "eg": continue
    if s[:2] == "eg" and (the.do=="all" or the.do==s[2:]):
      try:
        random.seed(the.seed)
        fun(copy.deepcopy(the))
        print(green("✔"),s)
      except:
        traceback.print_exc()
        print(red("✖"),s)
        fails += 1
  sys.exit(1 if fails > 1 else 0) 

def egfail(the): assert False, "a fail"

def egshow(the): print(the)

def eglines(the):
  [print(row) for row in csv(the.data)]

def egtab(the):
  h,d=Head(), Data()
  slurp(the.data,h,d)
  print(h)
  print(d.lo)

if __name__ == "__main__":
  all(vars(), options, doc)
