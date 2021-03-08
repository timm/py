# vim: filetype=python ts=2 sw=2 sts=2 et :
import re,sys,math,random,argparse
from types import FunctionType as fun
import inspect
import copy

class base:
  def has(i): return {}

class obj(base):
  "All you ever neeed: one tiny object."
  def __init__(i, **d): i.__dict__.update(d)
  def __repr__(i)     : return "{"+ ', '.join(
                             [f":{k} {v}" for k, v in sorted(i.__dict__.items()) 
                             if type(v)!=fun and k[0] != "_"])+"}"
  def __init__(i,*p,**kw): i.__dict__.update(i.has())
  def had(i,**d): 
      tmp= super().has(); 
      print(tmp)
      for k in d: tmp[k] =d[k]; 
      return tmp


class Row(obj):
  def has(i): return dict(a=3)
  def inc(i): i.a += 1; return i.a

class Row1(Row):
  def has(i): return i.had(j=100)

r=Row()#; print(r.inc()); print(r.inc())
s=Row1()#; print(s.inc()); print(s.inc())

print(s)
