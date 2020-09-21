/*-
 * Copyright (c) 2020 Jonas Fiala
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

package MonitorWrapper; 

import Vector :: *;

import SourceSink :: *;

import Routable :: *;
import PerformanceMonitor :: *;


// Wrapper interface for reporting events of a given interface
interface Monitored#(type original_module, type bitvec);
  interface original_module ifc;
  method bitvec events provisos (BitVectorable#(bitvec, n, m));
endinterface


// Count drop events
module monitorSource #(Source#(t) s) (Monitored#(Source#(t), Bit#(1)));
  Wire#(Bit#(1)) evt <- mkDWire(0);
  interface ifc = interface Source;
    method canPeek = s.canPeek;
    method peek = s.peek;
    method drop = action
      s.drop;
      evt <= 1;
    endaction;
  endinterface;
  method events = evt;
endmodule
// Count put events
module monitorSink #(Sink#(t) s) (Monitored#(Sink#(t), Bit#(1)));
  Wire#(Bit#(1)) evt <- mkDWire(0);
  interface ifc = interface Sink;
    method canPut = s.canPut;
    method put (x) = action
      s.put(x);
      evt <= 1;
    endaction;
  endinterface;
  method events = evt;
endmodule
// Count drop (at events[0]) and drop last (at events[1])
module monitorLastSource #(Source#(t) s) (Monitored#(Source#(t), Bit#(2)))
  provisos (DetectLast#(t));
  Wire#(Bit#(2)) evt <- mkDWire(0);
  interface ifc = interface Source;
    method canPeek = s.canPeek;
    method peek = s.peek;
    method drop = action
      s.drop;
      evt <= {pack(detectLast(s.peek)), 1};
    endaction;
  endinterface;
  method events = evt;
endmodule
// Count put (at events[0]) and put last (at events[1])
module monitorLastSink #(Sink#(t) s) (Monitored#(Sink#(t), Bit#(2)))
  provisos (DetectLast#(t));
  Wire#(Bit#(2)) evt <- mkDWire(0);
  interface ifc = interface Sink;
    method canPut = s.canPut;
    method put (x) = action
      s.put(x);
      evt <= {pack(detectLast(x)), 1};
    endaction;
  endinterface;
  method events = evt;
endmodule
endpackage
