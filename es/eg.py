#!/usr/bin/env python3
import sys
from es import Tab,csv,THE

t= Tab().adds(csv("data/auto93.csv"))
a, b = t.extremes(THE)
