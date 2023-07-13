/*-
 * Copyright (c) 2023 Peter Rugg
 * All rights reserved.
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

package AXI4_DelayShim;

import AXI4 :: *;
import BlueBasics :: *;
import SourceSink :: *;

import FIFOF :: *;
import FF :: *;

// An AXI4 shim that delays transactions by the given parameter
module mkAXI4_DelayShim #(NumProxy#(depth) _dummy, Bit#(16) delay)
  (AXI4_Shim#(id_, addr_, data_, awuser_, wuser_, buser_, aruser_, ruser_));

  let awff <- mkFIFOF;
  let wff <- mkFIFOF;
  FF#(AXI4_BFlit#(id_, buser_), depth) bff <- mkUGFFDelay(delay);
  FF#(AXI4_ARFlit#(id_, addr_, aruser_), depth) arff <- mkUGFFDelay(delay);
  let rff <- mkFIFOF;
  interface AXI4_Master master;
    interface aw = toSource(awff);
    interface w = toSource(wff);
    interface b = toSink(bff);
    interface ar = toSource(arff);
    interface r = toSink(rff);
  endinterface
  interface AXI4_Slave slave;
    interface aw = toSink(awff);
    interface w = toSink(wff);
    interface b = toSource(bff);
    interface ar = toSink(arff);
    interface r = toSource(rff);
  endinterface
  interface clear = error("clear not supported");
endmodule

endpackage
