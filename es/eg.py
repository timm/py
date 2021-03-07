#!/usr/bin/env python3
import es
import etc

def eg1(THE):
  "basic table test"
  t= es.Tab().adds(etc.csv("data/auto93.csv"))
  rows  = t.ordered(THE)
  a,b = t.clone(rows[:30]), t.clone(rows[-30:])
  print(a.ys())
  print(b.ys())

def eg2(THE):
  "generating rules"
  t= es.Tab().adds(etc.csv("data/auto93.csv"))
  c = es.Counts(t).badBetter(THE)
  model = es.Learn(c,THE)
  for k,rules in model.items(): 
    for rule in rules:
     print(k, es.showRule(rule))

eg2(es.THE)

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

