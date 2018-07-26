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

# BSV compiler
BSC = bsc

# Top level modules
TOPVERILOGFILE = ExampleVerilog.bsv
AXIMASTERMOD = axiMaster
AXILITEMASTERMOD = axiLiteMaster
AXISLAVEMOD = axiSlave
AXILITESLAVEMOD = axiLiteSlave
TOPSIMFILE = ExampleSim.bsv
TOPSIMMOD = exampleSim

# bluespec flags
BUILDDIR = build
BDIR = $(BUILDDIR)/bdir
SIMDIR = $(BUILDDIR)/simdir

OUTPUTDIR = output
VDIR = $(OUTPUTDIR)/vdir
INFODIR = $(OUTPUTDIR)/info

BSCFLAGS = -bdir $(BDIR)
BSCFLAGS += -info-dir $(INFODIR)
BSCFLAGS += -show-schedule -sched-dot

.PHONY: sim verilog

all: verilog sim

sim: *.bsv
	mkdir -p $(INFODIR) $(BDIR) $(SIMDIR) $(OUTPUTDIR)
	$(BSC) $(BSCFLAGS) -simdir $(SIMDIR) -sim -g $(TOPSIMMOD) -u $(TOPSIMFILE)
	$(BSC) $(BSCFLAGS) -simdir $(SIMDIR) -sim -o $(OUTPUTDIR)/$(TOPSIMMOD) -e $(TOPSIMMOD)

verilog: *.bsv
	mkdir -p $(INFODIR) $(BDIR) $(VDIR)
	$(BSC) $(BSCFLAGS) -vdir $(VDIR) -opt-undetermined-vals -unspecified-to X -verilog -g $(AXIMASTERMOD) -u $(TOPVERILOGFILE)
	$(BSC) $(BSCFLAGS) -vdir $(VDIR) -opt-undetermined-vals -unspecified-to X -verilog -g $(AXILITEMASTERMOD) -u $(TOPVERILOGFILE)
	$(BSC) $(BSCFLAGS) -vdir $(VDIR) -opt-undetermined-vals -unspecified-to X -verilog -g $(AXISLAVEMOD) -u $(TOPVERILOGFILE)
	$(BSC) $(BSCFLAGS) -vdir $(VDIR) -opt-undetermined-vals -unspecified-to X -verilog -g $(AXILITESLAVEMOD) -u $(TOPVERILOGFILE)

.PHONY: clean mrproper
clean:
	rm -rf $(BUILDDIR)
mrproper: clean
	rm -rf $(OUTPUTDIR)
