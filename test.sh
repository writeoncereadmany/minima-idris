#!/bin/bash
find . -name '*.ibc' -delete
idris --testpkg minima.ipkg
