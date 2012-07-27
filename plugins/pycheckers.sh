#!/usr/bin/env bash
# Last modified: <2012-07-27 15:03:11 Friday by richard>
# Need to install PEP8 using pip.
epylint "$1" 2>/dev/null
pyflakes "$1"
pep8 --ignore=E221,E701,E202 --repeat "$1"
true
