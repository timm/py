#!/usr/bin/env python3
import es
import etc
import datetime

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
  now = datetime.datetime.now()
  print(f"{'now':>10} :",now.strftime("%Y-%m-%d %H:%M:%S"))
  file = THE.dir + "/" + THE.file
  print(f"{'file':>10} :",file)
  print(f"{'seed':>10} :",THE.seed)
  print("")
  rows = etc.csv(file)
  t= es.Tab().adds(rows)
  c = es.Counts(t,THE)
  model = es.Learn(c,THE)
  names = '  '.join([f"{col.txt:>6}" for col in t.cols.y])
  print(f"\n{' ':11} {'N':>5}"+ names)
  for k,rules in model.items(): 
    print("")
    for rule in rules:
     found = es.selects(t,rule)
     if len(found.rows) >= THE.support:
       report = [f"{len(found.rows):>4}"] + [etc.show(x,w=5,d=1) for x in found.ys()]
       ', '.join(report)
       print(f"{k:>10} :", ', '.join(report), " if ",es.showRule(rule))


def main(com,funs):
  funs = {k:v for k,v in funs.items() if k[:2]=="eg" and type(v) == etc.fun}
  if com.eg:
    for k,v in funs.items():
      if com.eg in k: etc.eg(v,com.seed,com)
  if com.ls: 
     [print(f"{k:>15} :", v.__doc__) for k,v in funs.items()]
  if com.egs: 
    [etc.eg(v,com.seed,com) for k,v in funs.items()]

THE = etc.obj(**etc.args(what="./es.py", doc=es.__doc__,**es.OPTIONS))


eg2(THE)
#main(THE, locals())

