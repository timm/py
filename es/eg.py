#!/usr/bin/env python3
from es import THE, Tab,Counts,csv

t= Tab().adds(csv("data/auto93.csv"))
a, b = t.poles(THE)
a = t.clone(a)
b = t.clone(b)
print(a.ys())
print(b.ys())

# t= Tab().adds(csv("data/auto93.csv"))
# Counts(THE,t)
