<img src="docs/img/hair.png" width=160 align=right alt="hair">

# hair : hands-on AI recipes

Ask an LLM for an AI workbench and you often get a code base too
big to understand, to trust, or to change safely -- even with an
LLM helping. Each change calls for more code, which calls for more
changes. That recursive explosion has a name: AI slop.

Domain engineering suggests a way out. Study a family of systems,
build what they share once, then let each new member be a small
delta. So what is the family here?

Nobody walks in asking for classification, regression, or
optimization. They ask "which of these is risky?", "what should we
try next?", "is this one weird?" -- and leave you to turn that into
code.

The good news: those jobs share parts (distances between rows;
summaries of columns; splits; a little statistics, etc). Build those
parts once and the next task arrives half-written -- then quickly
tune tools that do what you want. And if you implement many tasks,
then task+1 takes nearly no effort.

That is the hair heuristic: **H**alf **A**lready **I**mplemented
(**R**eally).

And is this small code base useful? Yes, for many reasons. It is
quicker to change when the question changes, quicker to teach to
someone new, and short enough to read end to end before you trust
it. This code imports little, so there is not much to attack and nearly nothing to install -- which is why
it may still run long after the frameworks it never used have come
and gone.

The `hair/` package is the library, split by job: `lib` (csv,
flags, printing), `data` (tables, `Num`/`Sym` summaries), `dist`
(distances, distance-to-heaven), `tree` (splits, trees), `stats`
(effect sizes, Scott-Knott ranks, confusion, AUC). Each file in
`eg/` is a short recipe you can read in a few minutes.

| file | lines | what |
|---|---|---|
| `hair/lib.py` | 41 | `o`, `isa`, string-to-thing, settings, cli, csv, table |
| `hair/data.py` | 69 | `Num` tuples, `Sym` dicts, `add`/`adds`/`fold`, `mid`, `div`, `norm`, `TAB` |
| `hair/dist.py` | 25 | `minkowski`, `gap`, `distx`, `disty`, `near`, `neary` |
| `hair/tree.py` | 64 | `bins`, `bin`, `tree`, `leaf`, `show` |
| `hair/stats.py` | 139 | `cliffs`, `ks`, `cohen`, `same`, `rank`, `xval`, `confuse`, `auc`, `aucs` |
| `hair/__init__.py` | 68 | the traffic cop: loads the parts, holds `the` and the tests |
| `eg/knn.py` | 38 | guess a row's score from its k nearest rows (two kernels) |
| `eg/nb.py` | 57 | naive bayes, scored by AUC over a 5x5 cross-validation |
| `eg/sa.py` | 60 | simulated annealing that reads one row's goals at a time |

(`make loc` prints these counts: no blank lines, comments or top
docstring.)

## Install

    git clone https://github.com/timm/hair ~/gh/hair
    git clone https://github.com/timm/moot ~/gits/moot   # the data

There is nothing to build, and the examples run in place: no
`pip install` needed. You need Python 3.10 or later. If the data
is somewhere else, set `$MOOT`.

## Run

    cd ~/gh/hair/eg
    python3 knn.py
    python3 sa.py

Each example prints one table, with one column per data file.
Each example lists its flags at the top of its file. For example:

    python3 knn.py --file=misc/auto93 --k=3
    python3 sa.py  --file=misc/auto93,config/SS-A --runs=20

Files are named relative to `--dir` (default `$MOOT/optimize`),
without `.csv`. `--file=all` (the default) runs ten files.

### knn.py

Hide the scores of 100 rows. Guess each score as the mean score
of its k nearest known rows. Cells show the percent error; row
`mean` is the error when every guess is the mean of the known rows.

          auto93  SS-A  SS-M  HSQLDB  LLVM  storm  redis  socks  coc1000  pom3d
    k=1       18     3     2       1    28     34     46      3       19     34
    k=2       17     3     2       0    22     30     50      3       16     31
    k=3       16     3     2       0    20     26     49      3       15     29
    k=4       16     3     2       1    19     25     50      4       15     27
    k=5       16     3     2       1    18     29     51      4       15     26
    mean      37    30    59      60    50     53     49     49       16     29

### sa.py

An "eval" reads the goals of one row. Cells show the "win" after
that many evals: 100 means the search found the best row in all
the data; 0 means it found nothing better than the median row.

          auto93  SS-A  SS-M  HSQLDB  LLVM  storm  redis  socks  coc1000  pom3d
    25        94    94    82      93    95    100     91     94       57     91
    50        97    95    99      98   100    100     95     98       73     96
    100       99    98   100      99   100    100     97    100       80     99
    200      100    99   100     100   100    100    100    100       89    100
    500      100   100   100     100   100    100    100    100       97    100
    1000     100   100   100     100   100    100    100    100      100    100

## Data

Comma-separated text. Row one names the columns. In each name,
the first character gives the type and the last gives the role:

    Upper case first   numeric            (else symbolic)
    +  -   last        goal to maximize / minimize
    !      last        the class
    X      last        ignore this column
    else               an input

A cell that holds `?` is unknown. For example, `auto93.csv` starts:

    Clndrs, Volume, HpX, Model, origin, Lbs-, Acc+, Mpg+
         4,     90,  48,    78,      2, 1985, 21.5,   40

Two distances come from `hair.py`. `distx` is the distance
between two rows, measured on the inputs. `disty` is the distance
from one row to the best possible goal values: 0 is best, so a
smaller `disty` is a better row. The "score" in the examples is
`disty`.

## Write your own example

Put a new file in `eg/`. Start it like this:

    """
    mine: one line that says what this does.

    OPTIONS:
      --dir    data directory  = $MOOT/optimize
      --file   file(s) | all   = all
      --seed   random seed     = 1
    """
    import sys; sys.path[0] += "/.."; sys.dont_write_bytecode = True
    from hair import *

    def main():
      cli(the)
      for f in files(the):
        tbl = TAB(csv(f))
        print(name(f), len(tbl.rows), tbl.mid())

    the = settings(__doc__)
    if __name__ == "__main__": main()

The first `import` line lets the file find `hair.py` one
directory up, and stops Python from writing `__pycache__`.
`settings` reads the defaults from the OPTIONS lines. `cli`
updates them from `--key=val` flags. `table` prints columns.

## Make

`make` in the top directory, or in `eg/`, lists the rules:

    make loc        lines of code in hair.py and each eg/ file
    make hi it=F    render file F as colored html into docs/
    make publish    render all sources, push them to the web site
    make sh         a shell with a short prompt and some aliases
    make it=F       edit file F in nvim

Pretty-printed sources: https://timm.github.io/hair/

## Use as a package

    pip install .        # from a clone; installs one module, hair
    python3 -c "from hair import *; print(TAB(csv('my.csv')).mid())"

Or copy `hair.py` next to your own code. The examples in `eg/`
are not installed.

## Author

Tim Menzies <timm@ieee.org>, MIT license.
