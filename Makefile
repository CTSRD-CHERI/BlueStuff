#-
# Copyright (c) 2018 Alexandre Joannou
# All rights reserved.
#
# This software was developed by SRI International and the University of
# Cambridge Computer Laboratory (Department of Computer Science and
# Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
# DARPA SSITH research programme.
#
# @BERI_LICENSE_HEADER_START@
#
# Licensed to BERI Open Systems C.I.C. (BERI) under one or more contributor
# license agreements.  See the NOTICE file distributed with this work for
# additional information regarding copyright ownership.  BERI licenses this
# file to you under the BERI Hardware-Software License, Version 1.0 (the
# "License"); you may not use this file except in compliance with the
# License.  You may obtain a copy of the License at:
#
#   http://www.beri-open-systems.org/legal/license-1-0.txt
#
# Unless required by applicable law or agreed to in writing, Work distributed
# under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
# CONDITIONS OF ANY KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations under the License.
#
# @BERI_LICENSE_HEADER_END@
#

BSC = bsc

BLUESTUFFDIR = .
BLUEBASICSDIR = $(BLUESTUFFDIR)/BlueBasics
BSVPATH = +:$(BLUEBASICSDIR)

BSCFLAGS = -p $(BSVPATH)

# generated files directories
BUILDDIR = build
BDIR = $(BUILDDIR)/bdir
SIMDIR = $(BUILDDIR)/simdir

OUTPUTDIR = output

BSCFLAGS += -bdir $(BDIR)
BSCFLAGS += -simdir $(SIMDIR)

BSCFLAGS += -show-schedule
BSCFLAGS += +RTS -K512M -RTS
BSCFLAGS += -sched-dot
BSCFLAGS += -show-range-conflict
#BSCFLAGS += -show-rule-rel \* \*
#BSCFLAGS += -steps-warn-interval n

# Bluespec is not compatible with gcc > 4.9
# This is actually problematic when using $test$plusargs
CC = gcc-4.8
CXX = g++-4.8

EXAMPLESDIR = examples
EXAMPLESSRC = $(notdir $(wildcard $(EXAMPLESDIR)/Example*.bsv))
EXAMPLES = $(addprefix sim, $(basename $(EXAMPLESSRC)))


all: $(EXAMPLES)

simExample%: $(EXAMPLESDIR)/Example%.bsv
	mkdir -p $(OUTPUTDIR)/$@-info $(BDIR) $(SIMDIR) $(OUTPUTDIR)
	$(BSC) -cpp -Xcpp -I. -info-dir $(OUTPUTDIR)/$@-info $(BSCFLAGS) -sim -g top -u $<
	CC=$(CC) CXX=$(CXX) $(BSC) $(BSCFLAGS) -sim -e top -o $(OUTPUTDIR)/$@

.PHONY: clean mrproper

clean:
	rm -fr $(BUILDDIR)

mrproper: clean
	rm -fr $(OUTPUTDIR)
