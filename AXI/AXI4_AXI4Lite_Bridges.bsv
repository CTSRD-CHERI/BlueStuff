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

// AXI imports
import AXI4_Types :: *;
import AXI4Lite_Types :: *;
import AXI4_AXI4Lite_Types :: *;

// BlueBasics import
import SourceSink :: *;

// Standard
import FIFOF :: *;
import SpecialFIFOs :: *;

// helpers
function Action abort(Fmt m) = action $display(m); $finish(0); endaction;

/////////////////////////////////////////
// from AXI4Lite Master to AXI4 Master //
////////////////////////////////////////////////////////////////////////////////

module fromAXILiteMaster#(AXILiteMaster#(addr_, data_) mlite)
  (AXIMaster#(id_, addr_, data_, user_));

  module fromAXIAWLiteSource#(Source#(AWLiteFlit#(addr_)) src)
    (Source#(AWFlit#(id_, addr_, user_)));
    function AWFlit#(id_, addr_, user_) f(AWLiteFlit#(addr_) x) = AWFlit {
      awid: 0, awaddr: x.awaddr, awlen: 0,
      awsize: fromInteger(log2(valueOf(data_)/8)),
      awburst: FIXED, awlock: False, awcache: 0,
      awprot: x.awprot, awqos: 0, awregion: 0, awuser: ?
    };
    method canGet = src.canGet;
    method peek = f(src.peek);
    method get  =
      actionvalue let flit <- src.get; return f(flit); endactionvalue;
  endmodule

  module fromAXIWLiteSource#(Source#(WLiteFlit#(data_)) src)
    (Source#(WFlit#(data_, user_)));
    function WFlit#(data_, user_) f(WLiteFlit#(data_) x) = WFlit {
      wdata: x.wdata, wstrb: x.wstrb, wlast: True, wuser: ?
    };
    method canGet = src.canGet;
    method peek = f(src.peek);
    method get  =
      actionvalue let flit <- src.get; return f(flit); endactionvalue;
  endmodule

  module fromAXIBLiteSink#(Sink#(BLiteFlit) snk)
    (Sink#(BFlit#(id_, user_)));
    method canPut = snk.canPut;
    method put(x) = action
      if (x.bid != 0) abort($format("Unsupported bid (0x%0x)", x.bid));
      snk.put(BLiteFlit {bresp: x.bresp});
    endaction;
  endmodule

  module fromAXIARLiteSource#(Source#(ARLiteFlit#(addr_)) src)
    (Source#(ARFlit#(id_, addr_, user_)));
    function ARFlit#(id_, addr_, user_) f(ARLiteFlit#(addr_) x) = ARFlit {
      arid: 0, araddr: x.araddr, arlen: 0,
      arsize: fromInteger(log2(valueOf(data_)/8)),
      arburst: FIXED, arlock: False, arcache: 0,
      arprot: x.arprot, arqos: 0, arregion: 0, aruser: ?
    };
    method canGet = src.canGet;
    method peek = f(src.peek);
    method get  =
      actionvalue let flit <- src.get; return f(flit); endactionvalue;
  endmodule

  module fromAXIRLiteSink#(Sink#(RLiteFlit#(data_)) snk)
    (Sink#(RFlit#(id_, data_, user_)));
    method canPut = snk.canPut;
    method put(x) = action
      if (x.rid != 0) abort($format("Unsupported rid (0x%0x)", x.rid));
      if (x.rlast != True)
        abort($format("Unsupported rlast (", fshow(x.rlast), ")"));
      snk.put(RLiteFlit {rdata: x.rdata, rresp: x.rresp});
    endaction;
  endmodule

  let awsource <- fromAXIAWLiteSource(mlite.aw);
  let  wsource <- fromAXIWLiteSource(mlite.w);
  let    bsink <- fromAXIBLiteSink(mlite.b);
  let arsource <- fromAXIARLiteSource(mlite.ar);
  let    rsink <- fromAXIRLiteSink(mlite.r);
  interface aw = awsource;
  interface  w = wsource;
  interface  b = bsink;
  interface ar = arsource;
  interface  r = rsink;

endmodule

///////////////////////////////////////
// from AXI4Lite Slave to AXI4 Slave //
////////////////////////////////////////////////////////////////////////////////

module fromAXILiteSlave#(AXILiteSlave#(addr_, data_) slite)
  (AXISlave#(id_, addr_, data_, user_));

  module fromAXIAWLiteSink#(Sink#(AWLiteFlit#(addr_)) snk)
    (Sink#(AWFlit#(id_, addr_, user_)));
    method canPut = snk.canPut;
    method put(x) = action
      if (x.awid != 0) abort($format("Unsupported awid (0x%0x)", x.awid));
      if (x.awlen != 0) abort($format("Unsupported awlen (0x%0x)", x.awlen));
      if (x.awsize != fromInteger(log2(valueOf(data_)/8)))
        abort($format("Unsupported awsize (0x%0x)", x.awsize));
      if (x.awburst != FIXED)
        abort($format("Unsupported awburst (", fshow(x.awburst), ")"));
      if (x.awlock != False)
        abort($format("Unsupported awlock (", fshow(x.awlock), ")"));
      if (x.awcache != 0)
        abort($format("Unsupported awcache (0x%0x)", x.awcache));
      if (x.awqos != 0) abort($format("Unsupported awqos (0x%0x)", x.awqos));
      if (x.awregion != 0)
        abort($format("Unsupported awregion (0x%0x)", x.awregion));
      snk.put(AWLiteFlit {awaddr: x.awaddr, awprot: x.awprot});
    endaction;
  endmodule

  module fromAXIWLiteSink#(Sink#(WLiteFlit#(data_)) snk)
    (Sink#(WFlit#(data_, user_)));
    method canPut = snk.canPut;
    method put(x) = action
      if (x.wlast != True)
        abort($format("Unsupported wlast (", fshow(x.wlast), ")"));
      snk.put(WLiteFlit {wdata: x.wdata, wstrb: x.wstrb});
    endaction;
  endmodule

  module fromAXIBLiteSource#(Source#(BLiteFlit) src)
    (Source#(BFlit#(id_, user_)));
    function BFlit#(id_, user_) f(BLiteFlit x) = BFlit {
      bid: 0, bresp: x.bresp, buser: ?
    };
    method canGet = src.canGet;
    method peek = f(src.peek);
    method get  =
      actionvalue let flit <- src.get; return f(flit); endactionvalue;
  endmodule

  module fromAXIARLiteSink#(Sink#(ARLiteFlit#(addr_)) snk)
    (Sink#(ARFlit#(id_, addr_, user_)));
    method canPut = snk.canPut;
    method put(x) = action
      if (x.arid != 0) abort($format("Unsupported arid (0x%0x)", x.arid));
      if (x.arlen != 0) abort($format("Unsupported arlen (0x%0x)", x.arlen));
      if (x.arsize != fromInteger(log2(valueOf(data_)/8)))
        abort($format("Unsupported arsize (0x%0x)", x.arsize));
      if (x.arburst != FIXED)
        abort($format("Unsupported arburst (", fshow(x.arburst), ")"));
      if (x.arlock != False)
        abort($format("Unsupported arlock (", fshow(x.arlock), ")"));
      if (x.arcache != 0)
        abort($format("Unsupported arcache (0x%0x)", x.arcache));
      if (x.arqos != 0) abort($format("Unsupported arqos (0x%0x)", x.arqos));
      if (x.arregion != 0)
        abort($format("Unsupported arregion (0x%0x)", x.arregion));
      snk.put(ARLiteFlit {araddr: x.araddr, arprot: x.arprot});
    endaction;
  endmodule

  module fromAXIRLiteSource#(Source#(RLiteFlit#(data_)) src)
    (Source#(RFlit#(id_, data_, user_)));
    function RFlit#(id_, data_, user_) f(RLiteFlit#(data_) x) = RFlit {
      rid: 0, rdata: x.rdata, rresp: x.rresp, rlast: True, ruser: ?
    };
    method canGet = src.canGet;
    method peek = f(src.peek);
    method get  =
      actionvalue let flit <- src.get; return f(flit); endactionvalue;
  endmodule

  let  awsink <- fromAXIAWLiteSink(slite.aw);
  let   wsink <- fromAXIWLiteSink(slite.w);
  let bsource <- fromAXIBLiteSource(slite.b);
  let  arsink <- fromAXIARLiteSink(slite.ar);
  let rsource <- fromAXIRLiteSource(slite.r);
  interface aw = awsink;
  interface  w = wsink;
  interface  b = bsource;
  interface ar = arsink;
  interface  r = rsource;

endmodule
