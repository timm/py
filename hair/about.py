"""
hair: tiny AI, explained. This file is the traffic cop: it loads
the parts (lib, data, dist, tree, stats), holds the settings and
the tests. The eg*.py files are short demos that import it.
(c) 2026, Tim Menzies <timm@ieee.org>, MIT license

USAGE: python3 hair.py [--key=val ...] [test ...]
       python3 hair.py all              # run every test

OPTIONS:
  --dir    data directory  = $MOOT/optimize
  --file   file(s)|all|fast = fast
  --seed   random seed     = 1234567891
  --k      most neighbours = 5
  --eval   score metric    = rae
  --test   rows to guess   = 100
  --pool   known rows, cap = 1000
  --runs   repeats         = 5
  --val    random evals    = 10
  --few    pool rows tried = 32
  --temp   start temp      = 0.2
  --cool   cooling rate    = 0.9
  --leaf   tree min leaf   = 3
  --maxd   tree max depth  = 8
  --eps    cohen: small d  = 0.35
  --jobs   cpus, 0 for all = 0

TESTS: run any by name, e.g. "python3 hair.py tab tree".
"""
from .lib import settings
the = settings(__doc__)   # one config, shared by all parts
