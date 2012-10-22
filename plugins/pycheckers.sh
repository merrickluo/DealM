#!/usr/bin/env bash
# Last modified: <2012-10-22 11:58:09 Monday by richard>
# Need to install PEP8 using pip.
epylint "$1" 2>/dev/null
pyflakes "$1"
pep8 --repeat "$1"
true
