/*-
 * Copyright (c) 2018-2019 Alexandre Joannou
 * All rights reserved.
 *
 * This software was developed by SRI International and the University of
 * Cambridge Computer Laboratory (Department of Computer Science and
 * Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
 * DARPA SSITH research programme.
 *
 * @BERI_LICENSE_HEADER_START@
 *
 * Licensed to BERI Open Systems C.I.C. (BERI) under one or more contributor
 * license agreements.  See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.  BERI licenses this
 * file to you under the BERI Hardware-Software License, Version 1.0 (the
 * "License"); you may not use this file except in compliance with the
 * License.  You may obtain a copy of the License at:
 *
 *   http://www.beri-open-systems.org/legal/license-1-0.txt
 *
 * Unless required by applicable law or agreed to in writing, Work distributed
 * under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
 * CONDITIONS OF ANY KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations under the License.
 *
 * @BERI_LICENSE_HEADER_END@
 */

import AXI :: *;

import SourceSink :: *;

typedef   4 ID_sz;
typedef  32 ADDR_sz;
typedef 128 DATA_sz;
typedef   0 AWUSER_sz;
typedef   0 WUSER_sz;
typedef   0 BUSER_sz;
typedef   0 ARUSER_sz;
typedef   0 RUSER_sz;

`define PARAMS ID_sz, ADDR_sz, DATA_sz, AWUSER_sz, WUSER_sz, BUSER_sz, ARUSER_sz, RUSER_sz
`define LITEPARAMS ADDR_sz, DATA_sz, AWUSER_sz, WUSER_sz, BUSER_sz, ARUSER_sz, RUSER_sz

(* synthesize, clock_prefix="aclk", reset_prefix="aresetn" *)
module axiMaster (AXIMasterSynth#(`PARAMS));

  // AXI master shim
  AXIShim#(`PARAMS) shim <- mkAXIShim;

  // arbitrary work for each channel
  rule putAWFlit; shim.slave.aw.put(?); endrule
  rule putWFlit;  shim.slave.w.put(?);  endrule
  rule putARFlit; shim.slave.ar.put(?); endrule
  rule dropBFlit; shim.slave.b.drop; endrule
  rule dropRFlit; shim.slave.r.drop; endrule

  // return AXI interface
  let ifcSynth <- toAXIMasterSynth(shim.master);
  return ifcSynth;

endmodule

(* synthesize, clock_prefix="aclk", reset_prefix="aresetn" *)
module axiLiteMaster (AXILiteMasterSynth#(`LITEPARAMS));

  // AXI master shim
  AXILiteShim#(`LITEPARAMS) shim <- mkAXILiteShim;

  // arbitrary work for each channel
  rule putAWFlit; shim.slave.aw.put(?); endrule
  rule putWFlit;  shim.slave.w.put(?);  endrule
  rule putARFlit; shim.slave.ar.put(?); endrule
  rule dropBFlit; shim.slave.b.drop; endrule
  rule dropRFlit; shim.slave.r.drop; endrule

  // return AXI interface
  let ifcSynth <- toAXILiteMasterSynth(shim.master);
  return ifcSynth;

endmodule

(* synthesize, clock_prefix="aclk", reset_prefix="aresetn" *)
module axiSlave (AXISlaveSynth#(`PARAMS));

  // AXI slave shim
  AXIShim#(`PARAMS) shim <- mkAXIShim;

  // arbitrary work for each channel
  rule dropAWFlit; shim.master.aw.drop; endrule
  rule dropWFlit;  shim.master.w.drop;  endrule
  rule dropARFlit; shim.master.ar.drop; endrule
  rule putBFlit; shim.master.b.put(?); endrule
  rule putRFlit; shim.master.r.put(?); endrule

  // return AXI interface
  let ifcSynth <- toAXISlaveSynth(shim.slave);
  return ifcSynth;

endmodule

(* synthesize, clock_prefix="aclk", reset_prefix="aresetn" *)
module axiLiteSlave (AXILiteSlaveSynth#(`LITEPARAMS));

  // AXI slave shim
  AXILiteShim#(`LITEPARAMS) shim <- mkAXILiteShim;

  // arbitrary work for each channel
  rule dropAWFlit; shim.master.aw.drop; endrule
  rule dropWFlit;  shim.master.w.drop;  endrule
  rule dropARFlit; shim.master.ar.drop; endrule
  rule putBFlit; shim.master.b.put(?); endrule
  rule putRFlit; shim.master.r.put(?); endrule

  // return AXI interface
  let ifcSynth <- toAXILiteSlaveSynth(shim.slave);
  return ifcSynth;

endmodule

`undef LITEPARAMS
`undef PARAMS
