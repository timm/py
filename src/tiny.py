# vim: ts=2 sw=2 sts=2 et:
"""
tiny.py (c) Tim Menzies 2021, https://opensource.org/licenses/MIT
For classes that are mostly data stores, with few (or no) methods.
"""
import sys
import types

def funp(z): return isinstance(z, types.FunctionType)

class o:
  latest = None
  def __init__(i, **d): i.__dict__.update(d)
  def __repr__(i): return i.__class__.__name__ + str(
      {k: v for k, v in sorted(i.__dict__.items()) if str(k)[0] != "_"})

def so(**d):
  name, *rest = list(d.keys())
  o.latest = globals()[name] = type(name, (d[name],), {})
  [setattr(o.latest, x, d[x]) for x in rest if funp(d[x])]
  data = {x: d[x] for x in rest if not funp(d[x])}
  setattr(o.latest, "__init__",
          lambda i, **kw: i.__dict__.update({**data, **kw}))

def do(c=None):
  o.latest = c or o.latest
  return lambda f: setattr(o.latest,
                           f.__name__, lambda i, *l, **d: f(i, *l, **d))


if __name__ == "__main__":
  so(Robot=o, model=1970, n=2, hi=100,
     __lt__=lambda i, j: i.model < j.model)
  # @ do(Robot)
  # def __lt__(i, j): return i.model < j.model
  @ do()
  def __gt__(i, j): return i.model > j.model
  assert str(Robot()) == "Robot{'hi': 100, 'model': 1970, 'n': 2}"
  r = Robot(model=34) # can override defaults
  assert r.n == 2     # will build with local defaults
  r.n = 49            # can set variables
  assert r.n == 49    # let me show you
  assert str(r) == "Robot{'hi': 100, 'model': 34, 'n': 49}"
  assert r < Robot()  # can meta

# expected output:
# o{'a': 1, 'b': 2}
# Robot{'hi': 100, 'model': 34, 'n': 49}
