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

import FIFOF :: *;

typedef 4 ID_sz;
typedef 32 ADDR_sz;
typedef 128 DATA_sz;
typedef 0 USER_sz;

(* synthesize, clock_prefix="aclk", reset_prefix="aresetn" *)
module axiMaster (AXIMaster#(ID_sz, ADDR_sz, DATA_sz, USER_sz));

  // AXI master shim
  AXIMasterShim#(ID_sz, ADDR_sz, DATA_sz, USER_sz) shim <- mkAXIMasterShim;

  // arbitrary work (enq/deq FIFOF) for each channel
  rule enqAWFlit; shim.awff.enq(?); endrule
  rule enqWFlit;  shim.wff.enq(?);  endrule
  rule deqBFlit;  shim.bff.deq;     endrule
  rule enqARFlit; shim.arff.enq(?); endrule
  rule deqRFlit;  shim.rff.deq;     endrule

  // return AXI interface
  return shim.master;

endmodule

(* synthesize, clock_prefix="aclk", reset_prefix="aresetn" *)
module axiLiteMaster (AXILiteMaster#(ADDR_sz, DATA_sz));

  // AXI master shim
  AXILiteMasterShim#(ADDR_sz, DATA_sz) shim <- mkAXILiteMasterShim;

  // arbitrary work (enq/deq FIFOF) for each channel
  rule enqAWFlit; shim.awff.enq(?); endrule
  rule enqWFlit;  shim.wff.enq(?);  endrule
  rule deqBFlit;  shim.bff.deq;     endrule
  rule enqARFlit; shim.arff.enq(?); endrule
  rule deqRFlit;  shim.rff.deq;     endrule

  // return AXI interface
  return shim.master;

endmodule

(* synthesize, clock_prefix="aclk", reset_prefix="aresetn" *)
module axiSlave (AXISlave#(ID_sz, ADDR_sz, DATA_sz, USER_sz));

  // AXI slave shim
  AXISlaveShim#(ID_sz, ADDR_sz, DATA_sz, USER_sz) shim <- mkAXISlaveShim;

  // arbitrary work (enq/deq FIFOF) for each channel
  rule deqAWFlit; shim.awff.deq;   endrule
  rule deqWFlit;  shim.wff.deq;    endrule
  rule enqBFlit;  shim.bff.enq(?); endrule
  rule deqARFlit; shim.arff.deq;   endrule
  rule enqRFlit;  shim.rff.enq(?); endrule

  // return AXI interface
  return shim.slave;

endmodule

(* synthesize, clock_prefix="aclk", reset_prefix="aresetn" *)
module axiLiteSlave (AXILiteSlave#(ADDR_sz, DATA_sz));

  // AXI slave shim
  AXILiteSlaveShim#(ADDR_sz, DATA_sz) shim <- mkAXILiteSlaveShim;

  // arbitrary work (enq/deq FIFOF) for each channel
  rule deqAWFlit; shim.awff.deq;   endrule
  rule deqWFlit;  shim.wff.deq;    endrule
  rule enqBFlit;  shim.bff.enq(?); endrule
  rule deqARFlit; shim.arff.deq;   endrule
  rule enqRFlit;  shim.rff.enq(?); endrule

  // return AXI interface
  return shim.slave;

endmodule
