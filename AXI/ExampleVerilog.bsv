/*-
 * Copyright (c) 2018 Alexandre Joannou
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

typedef 4 ID_sz;
typedef 32 ADDR_sz;
typedef 128 DATA_sz;
typedef 0 USER_sz;

(* synthesize, clock_prefix="aclk", reset_prefix="aresetn" *)
module axiMaster (AXIMasterSynth#(ID_sz, ADDR_sz, DATA_sz, USER_sz));

  // AXI master shim
  AXIShim#(ID_sz, ADDR_sz, DATA_sz, USER_sz) shim <- mkAXIShim;

  // arbitrary work for each channel
  rule enqAWFlit; shim.slave.aw.put(?); endrule
  rule enqWFlit;  shim.slave.w.put(?);  endrule
  rule enqARFlit; shim.slave.ar.put(?); endrule
  rule deqBFlit; let _ <- shim.slave.b.get; endrule
  rule deqRFlit; let _ <- shim.slave.r.get; endrule

  // return AXI interface
  let ifcSynth <- toAXIMasterSynth(shim.master);
  return ifcSynth;

endmodule

(* synthesize, clock_prefix="aclk", reset_prefix="aresetn" *)
module axiLiteMaster (AXILiteMasterSynth#(ADDR_sz, DATA_sz));

  // AXI master shim
  AXILiteShim#(ADDR_sz, DATA_sz) shim <- mkAXILiteShim;

  // arbitrary work for each channel
  rule enqAWFlit; shim.slave.aw.put(?); endrule
  rule enqWFlit;  shim.slave.w.put(?);  endrule
  rule enqARFlit; shim.slave.ar.put(?); endrule
  rule deqBFlit; let _ <- shim.slave.b.get; endrule
  rule deqRFlit; let _ <- shim.slave.r.get; endrule

  // return AXI interface
  let ifcSynth <- toAXILiteMasterSynth(shim.master);
  return ifcSynth;

endmodule

(* synthesize, clock_prefix="aclk", reset_prefix="aresetn" *)
module axiSlave (AXISlaveSynth#(ID_sz, ADDR_sz, DATA_sz, USER_sz));

  // AXI slave shim
  AXIShim#(ID_sz, ADDR_sz, DATA_sz, USER_sz) shim <- mkAXIShim;

  // arbitrary work for each channel
  rule deqAWFlit; let _ <- shim.master.aw.get; endrule
  rule deqWFlit;  let _ <- shim.master.w.get;  endrule
  rule deqARFlit; let _ <- shim.master.ar.get; endrule
  rule enqBFlit; shim.master.b.put(?); endrule
  rule enqRFlit; shim.master.r.put(?); endrule

  // return AXI interface
  let ifcSynth <- toAXISlaveSynth(shim.slave);
  return ifcSynth;

endmodule

(* synthesize, clock_prefix="aclk", reset_prefix="aresetn" *)
module axiLiteSlave (AXILiteSlaveSynth#(ADDR_sz, DATA_sz));

  // AXI slave shim
  AXILiteShim#(ADDR_sz, DATA_sz) shim <- mkAXILiteShim;

  // arbitrary work for each channel
  rule deqAWFlit; let _ <- shim.master.aw.get; endrule
  rule deqWFlit;  let _ <- shim.master.w.get;  endrule
  rule deqARFlit; let _ <- shim.master.ar.get; endrule
  rule enqBFlit; shim.master.b.put(?); endrule
  rule enqRFlit; shim.master.r.put(?); endrule

  // return AXI interface
  let ifcSynth <- toAXILiteSlaveSynth(shim.slave);
  return ifcSynth;

endmodule
