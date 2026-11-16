# ACQ(1)

## NAME

**acq.py** — label-efficient multi-objective optimization by
active acquisition

## SYNOPSIS

**python3 acq.py** [--key=val ...]

## DESCRIPTION

Labels a few warm-up rows, splits them into *best* (top sqrt(n)
by distance-to-heaven) and *rest*; then repeatedly labels the
first unlabelled row nearer best's centroid than rest's.
Reports the best row found and the labels spent.

A sibling, [sa.py](https://timm.github.io/py/sa.py.html), does
the same job by simulated annealing over the same cache.

## OPTIONS

    --file    data file        ($MOOT/optimize/misc/auto93.csv)
    --seed    random seed      (1)
    --val     warm-up labels   (10)
    --pool    unlabelled cap   (1000)
    --budget  new labels       (30)

## DATA

Comma-separated; row one names the columns. First character
upper case means numeric, else symbolic. Last character:

    +  -   goal to maximize / minimize   (a y column)
    X      ignore this column
    else   an input (x) column

E.g. `Clndrs,Volume,HpX,origin,Lbs-,Acc+,Mpg+` has numeric
inputs, a symbolic input `origin`, one ignored column, and
goals minimize Lbs, maximize Acc and Mpg. Cells holding `?`
are unknown. Optimizers may read any x; each y read is a
"label" and is charged to the budget.

## INSTALL

    git clone https://github.com/timm/py ~/gh/py
    git clone https://github.com/timm/moot ~/gits/moot   # data
    cd ~/gh/py && python3 acq.py

Data lives in the [moot](https://github.com/timm/moot) repo;
set `$MOOT` if you clone it somewhere other than `~/gits/moot`.

## SEE ALSO

Pretty-printed sources:
[acq.py](https://timm.github.io/py/acq.py.html),
[sa.py](https://timm.github.io/py/sa.py.html),
[Makefile](https://timm.github.io/py/Makefile.html).

## AUTHOR

Tim Menzies <timm@ieee.org>, MIT license.
