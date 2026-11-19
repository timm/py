# hair Makefile. One file, no support code. Temps in $(Tmp); html
# into $(Root)/docs. "make" = help. "make it=f" edits f in nvim.
# Every repo path is spelled $(Root)/..., so sub-directories can
# reuse all of this with a one-line Makefile: include ../Makefile
# Long payloads live at the end as comment blocks #<name> ...
# #</name>; make greps its own source to build them (see mark).
# Inside a block, text is verbatim (no $$ doubling): {Tmp} and
# {Here} (the repo root) are filled in by sed at extraction time.
SHELL       := bash
.SHELLFLAGS := -o pipefail -ec   # a dead gawk must kill the pipe
.DELETE_ON_ERROR:                # failed recipes leave no corpses
MAKEFLAGS += --no-print-directory
.SILENT:
# no __pycache__ litter, here or in `make sh`
export PYTHONDONTWRITEBYTECODE := 1
Tmp  ?= $(HOME)/tmp/hair
it   ?=
cols ?= 6
Root := $(shell git rev-parse --show-toplevel)

ifeq ($(it),)
.DEFAULT_GOAL := help
else
.DEFAULT_GOAL := edit
endif

Need = gawk nvim git python3 sed tail basename mkdir

deps: ## complain about any missing tools, else silence
	miss=""; for c in $(Need); do command -v $$c >/dev/null || \
	  miss="$$miss $$c"; done; \
	[ -z "$$miss" ] || { echo "missing:$$miss"; \
	  echo "try: sudo apt install -y gawk neovim  (or brew install)"; \
	  exit 1; }

help: deps $(Tmp)/help.awk ## show this help
	gawk -f $(Tmp)/help.awk $(Root)/Makefile

pull: ## update from main
	git pull

push: ## save to main
	echo -en "\033[33mWhy this push? \033[0m"; read x; \
	git commit -am "$$x"; git push; git status

clean: ## remove build litter: dist/ build/ *.egg-info __pycache__
	cd $(Root); rm -rf dist build *.egg-info; \
	find . -name __pycache__ -type d -prune -exec rm -rf {} +; \
	find . -name '*.pyc' -delete; echo "cleaned"

dist: clean ## build wheel + sdist into dist/ (and check them)
	cd $(Root); rm -rf dist build *.egg-info; \
	python3 -m pip install -q --upgrade build twine; \
	python3 -m build; python3 -m twine check dist/*

testpypi: dist ## upload to TestPyPI (practice index; needs a test.pypi token)
	python3 -m twine upload --repository testpypi $(Root)/dist/*

pypi: dist ## upload to the REAL PyPI (run testpypi first; asks y/N)
	echo -en "\033[31mReally upload to real PyPI? [y/N] \033[0m"; read x; \
	[ "$$x" = y ] && python3 -m twine upload --verbose $(Root)/dist/* || echo "skipped"

edit: $(Tmp)/init.lua ## edit $(it) in nvim (use: make it=file)
	nvim --clean -u $(Tmp)/init.lua -i $(Tmp)/shada $(it)

serve: ## serve docs/ at http://localhost:8000
	cd $(Root)/docs && python3 -m http.server

sh: $(Tmp)/bashrc $(Tmp)/init.lua ## bash with our prompt and aliases
	bash --rcfile $(Tmp)/bashrc -i

$(Tmp):
	mkdir -p $(Tmp)/undo $(Tmp)/swap $(Tmp)/backup

#-- config files ---------------------------------------------------
H    := \#
mark  = gawk -v RS= 'index($$0,"$(H)<$(1)>")==1' $(Root)/Makefile | tail -n +2 \
        | sed -e 's/^$(H) \{0,1\}//' -e 's|{Tmp}|$(Tmp)|g' \
              -e 's|{Here}|$(Root)|g' -e 's|{Name}|$(notdir $(Root))|g'

$(Tmp)/help.awk: $(Root)/Makefile | $(Tmp) ; $(call mark,help.awk) > $@
$(Tmp)/init.lua: $(Root)/Makefile | $(Tmp) ; $(call mark,init.lua) > $@
$(Tmp)/bashrc:   $(Root)/Makefile | $(Tmp) ; $(call mark,bashrc)   > $@
$(Tmp)/hi.py   : $(Root)/Makefile | $(Tmp) ; $(call mark,hi.py)    > $@
$(Tmp)/loc.py  : $(Root)/Makefile | $(Tmp) ; $(call mark,loc.py)   > $@
.gitignore:      $(Root)/Makefile  ; $(call mark,gitignore) > $(Root)/$@
LICENSE.md:      $(Root)/Makefile  ; $(call mark,license)   > $(Root)/$@

#-- examples ---------------------------------------------------
# hair.py is the library; each eg/*.py is one small example of it
loc: $(Tmp)/loc.py ## lines of code in hair/ and each eg/ file
	cd $(Root) && python3 $(Tmp)/loc.py hair/*.py eg/*.py

#-- website ---------------------------------------------------
# used in publish: a parentless commit of scratch index last staged
snap = $$(git commit-tree $$(git write-tree) -m publish)

# one <li> link per non-empty file in $(1) (href = basename.html)
links = for f in $(1); do [ -s "$$f" ] || continue; \
          b=$$(basename $$f); \
          echo "<li><a href=\"$$b.html\">$$b</a></li>"; done

# emit the index page (header, Sources, Examples, lines-of-code, footer)
# to stdout; the caller redirects it. The {} group all the echoes so a
# single external redirect captures them. Plain `=` (not `define`) so
# there is no trailing newline to break `$(INDEX) > file`.
INDEX = { $(call mark,index.top); \
  $(call mark,index.why); \
  echo "<b>Sources</b><ul>";       $(call links,$(Root)/hair/*.py Makefile); \
  echo "</ul><b>Examples</b><ul>"; $(call links,$(Root)/eg/*.py); \
  echo "</ul><b>Lines of code</b><pre style=\"font-size:9pt\">"; \
  (cd $(Root) && python3 $(Tmp)/loc.py hair/*.py eg/*.py); echo "</pre>"; \
  $(call mark,index.bot); }

publish: $(Tmp)/hi.py $(Tmp)/loc.py ## render all sources to docs/, push to gh-pages
	cd $(Root) && for f in hair/*.py eg/*.py Makefile; do \
	  [ -s "$$f" ] && $(MAKE) hi it=$$f; done        # -s: skip empty __init__
	$(INDEX) > $(Root)/docs/index.html
	cd $(Root); export GIT_INDEX_FILE=$(Tmp)/pubindex; \
	git --work-tree=docs add -A . && \
	git push -q -f origin $(snap):refs/heads/gh-pages
	echo "https://timm.github.io/hair/"

#-- highlight ---------------------------------------------------
# render source as tiny-type, n-column, colored html. hi.py picks
# the syntax faces from the file's suffix, so there is nothing to
# pass but cols and the file itself.

page = $(Root)/docs/$(notdir $(it)).html

hi: $(page) ## render $(it) as colored html into docs/
$(page): $(it) $(Tmp)/hi.py
	python3 $(Tmp)/hi.py $(cols) $< > $@

#-- payloads: comment blocks, extracted by `mark` ---------------
# a block is one blank-line-delimited paragraph starting
# "#<name>" (awk RS= reads paragraphs; one condition selects)

# ──────────────────────────────────────────────────────────────

#<index.bot>
# <b>Problems</b><br>
# &nbsp;&nbsp;<a href="https://github.com/timm/{Name}/issues">raise an issue</a>
# <p>
# <b>License</b><br>
# &nbsp;&nbsp;<a href="https://github.com/timm/{Name}/blob/main/LICENSE.md">(c) 2026</a>
# by <a href="https://timm.fyi">timm</a>.  
# Share and enjoy.
# </span></td></tr></table></center>
# <p>
# <center>
# <a href="https://github.com/timm/{Name}"><img src="img/gh.svg?v=5" alt="github"></a>
# &nbsp;<a href="https://www.csc.ncsu.edu"><img src="img/ncsu.svg?v=5" alt="NC State Computer Science"></a>
# </center>

# ──────────────────────────────────────────────────────────────

#<index.top>
# <meta charset=utf-8><title>timm / {Name}</title>
# <body style="margin:0;padding:2em 0;">
# <center><h1 style="font-weight:normal;font-family:'Times New Roman',Times,serif">hair : hands-on AI recipes</h1></center>
# <center><img src="img/hair.png" width=270 alt="hair"></center>
# <p>
# <center><table cellspacing=0 cellpadding=0 border=0>
# <tr><td align="left"><span style="font-size:12pt">

# ──────────────────────────────────────────────────────────────


# ──────────────────────────────────────────────────────────────

#<index.why>
# <center><div style="max-width:34em;text-align:left;font-size:11pt">
# <p>Nobody walks in asking for classification, regression, or
# optimization. They ask "which of these is risky?", "what should
# we try next?", "is this one weird?" -- and leave you to turn
# that into code.
# <svg width="205" height="130" style="float:right;margin:.8em 0 .6em 1.2em"
#      role="img" aria-label="new lines of code per new task, falling">
# <text x="0" y="20" font-size="11" fill="#555">new lines</text>
# <line x1="26" y1="22" x2="26" y2="96" stroke="#bbb"/>
# <line x1="26" y1="96" x2="200" y2="96" stroke="#bbb"/>
# <polyline fill="none" stroke="#0969da" stroke-width="2"
#  points="28,24 46,60 64,71 83,76 101,79 119,81 137,82 155,83 174,84 192,85"/>
# <text x="28" y="110" font-size="9" fill="#555" text-anchor="middle">1</text>
# <text x="46" y="110" font-size="9" fill="#555" text-anchor="middle">2</text>
# <text x="64" y="110" font-size="9" fill="#555" text-anchor="middle">3</text>
# <text x="83" y="110" font-size="9" fill="#555" text-anchor="middle">4</text>
# <text x="101" y="110" font-size="9" fill="#555" text-anchor="middle">5</text>
# <text x="119" y="110" font-size="9" fill="#555" text-anchor="middle">6</text>
# <text x="137" y="110" font-size="9" fill="#555" text-anchor="middle">7</text>
# <text x="155" y="110" font-size="9" fill="#555" text-anchor="middle">8</text>
# <text x="174" y="110" font-size="9" fill="#555" text-anchor="middle">9</text>
# <text x="192" y="110" font-size="9" fill="#555" text-anchor="middle">10</text>
# <text x="110" y="126" font-size="11" fill="#555" text-anchor="middle">task</text>
# </svg>
# <p>The good news: those jobs share parts (distances between rows;
# summaries of columns; splits; a little statistics, etc). Build
# those once and the next task arrives half-written -- then quickly
# tune tools that do what you want. And if you implement many
# tasks, then task+1 takes nearly no effort.
# <p>That is the hair heuristic: <b>H</b>alf <b>A</b>lready
# <b>I</b>mplemented (<b>R</b>eally).
# <p>And is this small code base useful? Yes, for many reasons.
# It is quicker to change when the question changes, quicker to teach to someone new, and short
# enough to read end to end before you trust it. This code imports
# little, so there is not much to attack and nearly nothing to
# install -- which is why it may still run long after the
# frameworks it never used have come and gone.
# </div></center>
# <p style="clear:both">


# ──────────────────────────────────────────────────────────────

#<help.awk>
# # pretty help: targets with trailing ## comments. The target
# # lines go through an output pipe (awk can print into a shell
# # command!); close() flushes it so the footer prints last.
# BEGIN          {FS=":.*##"
#                 print "\n\033[1mmake\033[0m [\033[36mWHAT\033[0m]\n"}
# /^[a-z.]+:.*##/ {printf "  make \033[36m%-8s\033[0m %s\n", $1, $2 | "sort"}
# END            { print "  make \033[36mit=F\033[0m      edit file F in nvim\n"}

# ──────────────────────────────────────────────────────────────

#<init.lua>
# -- generated by Makefile; edit there, not here
# vim.opt.expandtab   = true
# vim.opt.shiftwidth  = 2
# vim.opt.tabstop     = 2
# vim.opt.number      = true
# vim.opt.wrap        = false
# vim.opt.ignorecase  = true
# vim.opt.smartcase   = true
# vim.opt.colorcolumn = "75"
# vim.opt.clipboard   = "unnamedplus"  -- yank <-> system clipboard
# vim.opt.hlsearch    = true
# vim.opt.undofile    = true
# vim.opt.undodir     = "{Tmp}/undo"
# vim.opt.directory   = "{Tmp}/swap"
# vim.opt.backupdir   = "{Tmp}/backup"
# vim.opt.statusline  = "%f %m %r %=%l:%c"
# vim.cmd [[autocmd BufReadPost * silent! normal! g`"]]
# vim.cmd [[syntax on]]
# vim.cmd [[colorscheme lunaperche]]

# ──────────────────────────────────────────────────────────────

#<license>
# # MIT License
#
# Copyright (c) 2026 Tim Menzies <timm@ieee.org>
#
# Permission is hereby granted, free of charge, to any person obtaining a
# copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:
#
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
# BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
# ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

# 
# ──────────────────────────────────────────────────────────────

#<gitignore>
# # generated html stays out of main (published via gh-pages)
# docs/*.html
# # python
# __pycache__/
# *.py[cod]
# *.egg-info/
# .venv/
# venv/
# build/
# dist/
# .pytest_cache/
# .mypy_cache/
# .ruff_cache/
# .coverage
# .ipynb_checkpoints/
# # nvim / vim
# *.swp
# *.swo
# *~
# .netrwhist
# Session.vim
# .nvimlog
# # lua
# luac.out
# *.rock
# lua_modules/
# .luarocks/
# # os
# .DS_Store

# ──────────────────────────────────────────────────────────────

#<bashrc>
# # generated by Makefile; edit there, not here
# export HISTFILE={Tmp}/bash_history
# export EDITOR="nvim --clean -u {Tmp}/init.lua -i {Tmp}/shada"
# alias ls="ls -G"
# alias vi='$EDITOR'
# alias gs="git status -sb"
# alias gl="git log --oneline --graph -12"
# alias gd="git diff --stat"
# alias ..="cd .."
# alias ...="cd ../.."
# alias root='cd "$(git rev-parse --show-toplevel)"'
# alias serve="python3 -m http.server"
# mk() { mkdir -p "$1" && cd "$1"; }       # make dir and enter it
# h()  { history | grep -i "$1" | tail; }  # search own history
# f()  { grep -rniI --exclude-dir=.git "$1" .; }  # find text here
# # every ## make rule is a command here: argless ones as aliases
# # (parsed live from the Makefile, so new rules auto-appear);
# # file-taking ones as functions; sh excluded (shadows /bin/sh)
# for t in $(gawk -F: '/^[a-z]+:.*##/ && !/^(sh|edit|hi):/ {print $1}' {Here}/Makefile); do
#   alias $t="make -C {Here} $t"
# done
# edit() { make -C {Here} edit it="$1"; }
# hi()   { make -C {Here} hi it="$1" cols="${2:-3}"; }
# export HISTCONTROL=ignoredups
# export COL='grep --line-buffered --color=always -E [0-9.]*!|$' # |$COL
# shopt -s autocd 2>/dev/null              # bare dir name = cd
# # prompt: parent/dir (branch*) -- star = dirty working tree
# _p() {
#   local b=$(git branch --show-current 2>/dev/null) s=
#   if [ -n "$b" ]; then
#     [ -n "$(git status --porcelain 2>/dev/null)" ] && s="*"
#     b=" \033[33m($b$s)"
#   fi
#   PS1="\033[36m$(basename "${PWD%/*}")/\W$b\033[0m> "
# }
# PROMPT_COMMAND=_p

# ──────────────────────────────────────────────────────────────

#<loc.py>
# # lines of code per file: no blanks, comments or top docstring.
# # count on the LEFT, path on the right; empty files are skipped.
# import ast, sys
# for path in sys.argv[1:]:
#   src  = open(path).read()
#   tree = ast.parse(src)
#   doc  = tree.body[0] if ast.get_docstring(tree) else None
#   skip = set(range(doc.lineno, doc.end_lineno + 1)) if doc else set()
#   n = sum(1 for i, s in enumerate(src.split("\n"), 1) if s.strip()
#           and not s.strip().startswith("#") and i not in skip)
#   if n: print("%5d  %s" % (n, path))

# ──────────────────────────────────────────────────────────────

#<hi.py>
# # a2ps for the browser: tiny type, N columns, line numbers.
# # One pass: named-group alternation, first rule wins, so strings
# # beat comments beat keywords -- no placeholder or state games.
# # Section banners (lines starting #-- name ---) each take the
# # next pale background. MISC names share one blue; white before
# # the first banner and at main. Faces (keywords/comment/func) are
# # chosen HERE from the file suffix, so callers pass just cols+file.
# import re, sys, html, os
#
# FACES = {
#  "py":  ("if elif else for while return break continue def lambda "
#          "yield import from as and or not in is class try except "
#          "with pass True False None self",
#          r"#.*", 
#          r"(?<=def )\w+|(?<=class )\w+"
#          ),
#  "lua": ("function local end if then else elseif for while do "
#          "repeat until return require nil true false and or not",
#          r"--.*",
#          r"(?<=function )\w+")
#          }
# MK    = ("define endef export ifeq ifneq ifdef ifndef else endif "
#          "include call shell filter notdir wildcard foreach",
#          r"#.*", 
#          r"^[A-Za-z0-9_.]+(?=:)|[$][(][A-Za-z0-9_]+[)]"
#          )
#
# cols = sys.argv[1] if len(sys.argv) > 1 else "6"
# path = sys.argv[2] if len(sys.argv) > 2 else "-"
# src  = open(path).read() if path != "-" else sys.stdin.read()
# base = os.path.basename(path)
# kw, com, fun = FACES.get(base.rsplit(".",1)[-1] if "." in base else "", MK)
#
# RULES = [
#  ("str", r'"""[\s\S]*?"""|"[^"]*"|\'[^\']*\''),
#  ("com", com),
#  ("fun", fun),
#  ("kw",  r"\b(?:%s)\b" % "|".join(map(re.escape, kw.split()))),
#  ("num", r"\b\d+\.?\d*\b")]
# COLOR = dict(str="#a31515", com="#6a737d", fun="#795e26",
#              kw="#0033b3",  num="#098658", n="#b0b0b0")
# BG   = ["#fff4e0","#eaf8ec","#f1ecfd","#e2f6f6",  # cream green
#         "#fdfbd8","#fdecf3","#f1f1f1","#ffe9e2"]  # lilac teal ..
# BLUE, WHITE = "#eaf2ff", "#ffffff"
# MISC = r"#-- *(tricks|lib|misc)\b"
# MAIN = r"#-- *main\b"
#
# PAT = re.compile("|".join("(?P<%s>%s)" % kv for kv in RULES), re.M)
#
# def paint(m) -> str:
#   return '<span style="color:%s">%s</span>' % (COLOR[m.lastgroup], m.group())
#
# out  = PAT.sub(paint, html.escape(src, quote=False))
#
# print('<meta charset=utf-8>'
#       '<link rel=stylesheet href="https://fonts.googleapis.com/css2'
#       '?family=JetBrains+Mono:wght@400&display=swap">'
#       '<body style="background:#ffffff">')
# print('<pre style="font:6.5pt/1.4 JetBrains Mono,Monaco,Menlo,monospace;'
#       'color:#1f2328;columns:%s 80ch;column-gap:2em;'
#       'column-rule:1px solid #dddddd">' % cols)
# carry, bg, k = "", WHITE, 0
# for n, (raw, line) in enumerate(zip(src.split("\n"),
#                                     out.split("\n")), 1):
#   if raw.startswith("#--"):               # new conceptual block
#     if   re.match(MISC, raw): bg = BLUE
#     elif re.match(MAIN, raw): bg = WHITE
#     else: bg, k = BG[k % len(BG)], k + 1
#   line  = carry + line
#   opens = re.findall(r"<span[^>]*>", line)
#   carry = ""
#   if len(opens) > line.count("</span>"):
#     carry = opens[-1]; line += "</span>"
#   print('<span style="display:block;background:%s">'
#         '<span style="color:%s">%4d</span> %s</span>'
#         % (bg, COLOR["n"], n, line), end="")
# # end="": a block span already ends its line; a \n here too
# # would show in <pre> as a blank line after every source line.
# print("</pre>")
