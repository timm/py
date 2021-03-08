#!/usr/bin/env python3
import es
import etc

def eg1(THE):
  "basic table test"
  rows = etc.csv(THE.dir + "/" + THE.file)
  t= es.Tab().adds(rows)
  rows  = t.ordered(THE)
  a,b = t.clone(rows[:30]), t.clone(rows[-30:])
  etc.ok([4499.0, 12.0, 10.0] == b.ys(), "bad?")
  etc.ok([1975.0, 18.8, 40.0] == a.ys(), "better?")

def eg2(THE):
  "generating rules"
  rows = etc.csv(THE.dir + "/" + THE.file)
  t= es.Tab().adds(rows)
  c = es.Counts(t).badBetter(THE)
  model = es.Learn(c,THE)
  names = '  '.join([f"{col.txt:>6}" for col in t.cols.y])
  print(f"\n{' ':11} {'N':>5}"+ names)
  for k,rules in model.items(): 
    print("")
    for rule in rules:
     found = es.selects(t,rule)
     if found.rows:
       report = [f"{len(found.rows):>4}"] + [etc.show(x,w=5,d=1) for x in found.ys()]
       ', '.join(report)
       print(f"{k:>10} :", ', '.join(report), es.showRule(rule))


def main(com,funs):
  funs = {k:v for k,v in funs.items() if k[:2]=="eg" and type(v) == etc.fun}
  if com.eg:
    for k,v in funs.items():
      if com.eg in k: etc.eg(v,com)
  if com.ls: 
     [print(f"{k:>15} :", v.__doc__) for k,v in funs.items()]
  if com.egs: 
    [etc.eg(v,com) for k,v in funs.items()]

THE = etc.obj(**etc.args(what="./es.py", doc=es.__doc__,**es.OPTIONS))


main(THE, locals())

