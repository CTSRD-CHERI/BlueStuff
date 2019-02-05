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

module fromAXI4Lite_Master#(
  AXI4Lite_Master#(addr_, data_, awuser_, wuser_, buser_, aruser_, ruser_) mlite)
  (AXI4_Master#(id_, addr_, data_, awuser_, wuser_, buser_, aruser_, ruser_));

  module fromAXIAWLiteSource#(Source#(AXI4Lite_AWFlit#(addr_, awuser_)) src)
    (Source#(AXI4_AWFlit#(id_, addr_, awuser_)));
    function AXI4_AWFlit#(id_, addr_, awuser_) f(AXI4Lite_AWFlit#(addr_, awuser_) x) =
    AXI4_AWFlit {
      awid: 0, awaddr: x.awaddr, awlen: 0,
      awsize: fromInteger(log2(valueOf(data_)/8)),
      awburst: FIXED, awlock: NORMAL, awcache: 0,
      awprot: x.awprot, awqos: 0, awregion: 0, awuser: x.awuser
    };
    method canPeek = src.canPeek;
    method peek    = f(src.peek);
    method drop    = src.drop;
  endmodule

  module fromAXIWLiteSource#(Source#(AXI4Lite_WFlit#(data_, wuser_)) src)
    (Source#(AXI4_WFlit#(data_, wuser_)));
    function AXI4_WFlit#(data_, wuser_) f(AXI4Lite_WFlit#(data_, wuser_) x) = AXI4_WFlit {
      wdata: x.wdata, wstrb: x.wstrb, wlast: True, wuser: x.wuser
    };
    method canPeek = src.canPeek;
    method peek    = f(src.peek);
    method drop    = src.drop;
  endmodule

  module fromAXIBLiteSink#(Sink#(AXI4Lite_BFlit#(buser_)) snk)
    (Sink#(AXI4_BFlit#(id_, buser_)));
    method canPut = snk.canPut;
    method put(x) = action
      if (x.bid != 0) abort($format("Unsupported bid (0x%0x)", x.bid));
      snk.put(AXI4Lite_BFlit {bresp: x.bresp, buser: x.buser});
    endaction;
  endmodule

  module fromAXIARLiteSource#(Source#(AXI4Lite_ARFlit#(addr_, aruser_)) src)
    (Source#(AXI4_ARFlit#(id_, addr_, aruser_)));
    function AXI4_ARFlit#(id_, addr_, aruser_) f(AXI4Lite_ARFlit#(addr_, aruser_) x) =
    AXI4_ARFlit {
      arid: 0, araddr: x.araddr, arlen: 0,
      arsize: fromInteger(log2(valueOf(data_)/8)),
      arburst: FIXED, arlock: NORMAL, arcache: 0,
      arprot: x.arprot, arqos: 0, arregion: 0, aruser: x.aruser
    };
    method canPeek = src.canPeek;
    method peek    = f(src.peek);
    method drop    = src.drop;
  endmodule

  module fromAXIRLiteSink#(Sink#(AXI4Lite_RFlit#(data_, ruser_)) snk)
    (Sink#(AXI4_RFlit#(id_, data_, ruser_)));
    method canPut = snk.canPut;
    method put(x) = action
      if (x.rid != 0) abort($format("Unsupported rid (0x%0x)", x.rid));
      if (x.rlast != True)
        abort($format("Unsupported rlast (", fshow(x.rlast), ")"));
      snk.put(AXI4Lite_RFlit {rdata: x.rdata, rresp: x.rresp, ruser: x.ruser});
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

module fromAXI4Lite_Slave#(
  AXI4Lite_Slave#(addr_, data_, awuser_, wuser_, buser_, aruser_, ruser_) slite)
  (AXI4_Slave#(id_, addr_, data_, awuser_, wuser_, buser_, aruser_, ruser_));

  module fromAXIAWLiteSink#(Sink#(AXI4Lite_AWFlit#(addr_, awuser_)) snk)
    (Sink#(AXI4_AWFlit#(id_, addr_, awuser_)));
    method canPut = snk.canPut;
    method put(x) = action
      if (x.awid != 0) abort($format("Unsupported awid (0x%0x)", x.awid));
      if (x.awlen != 0) abort($format("Unsupported awlen (0x%0x)", x.awlen));
      if (x.awsize != fromInteger(log2(valueOf(data_)/8)))
        abort($format("Unsupported awsize (0x%0x)", x.awsize));
      if (x.awburst != FIXED)
        abort($format("Unsupported awburst (", fshow(x.awburst), ")"));
      if (x.awlock != NORMAL)
        abort($format("Unsupported awlock (", fshow(x.awlock), ")"));
      if (x.awcache != 0)
        abort($format("Unsupported awcache (0x%0x)", x.awcache));
      if (x.awqos != 0) abort($format("Unsupported awqos (0x%0x)", x.awqos));
      if (x.awregion != 0)
        abort($format("Unsupported awregion (0x%0x)", x.awregion));
      snk.put(AXI4Lite_AWFlit {
        awaddr: x.awaddr, awprot: x.awprot, awuser: x.awuser
      });
    endaction;
  endmodule

  module fromAXIWLiteSink#(Sink#(AXI4Lite_WFlit#(data_, wuser_)) snk)
    (Sink#(AXI4_WFlit#(data_, wuser_)));
    method canPut = snk.canPut;
    method put(x) = action
      if (x.wlast != True)
        abort($format("Unsupported wlast (", fshow(x.wlast), ")"));
      snk.put(AXI4Lite_WFlit {wdata: x.wdata, wstrb: x.wstrb, wuser: x.wuser});
    endaction;
  endmodule

  module fromAXIBLiteSource#(Source#(AXI4Lite_BFlit#(buser_)) src)
    (Source#(AXI4_BFlit#(id_, buser_)));
    function AXI4_BFlit#(id_, buser_) f(AXI4Lite_BFlit#(buser_) x) = AXI4_BFlit {
      bid: 0, bresp: x.bresp, buser: x.buser
    };
    method canPeek = src.canPeek;
    method peek    = f(src.peek);
    method drop    = src.drop;
  endmodule

  module fromAXIARLiteSink#(Sink#(AXI4Lite_ARFlit#(addr_, aruser_)) snk)
    (Sink#(AXI4_ARFlit#(id_, addr_, aruser_)));
    method canPut = snk.canPut;
    method put(x) = action
      if (x.arid != 0) abort($format("Unsupported arid (0x%0x)", x.arid));
      if (x.arlen != 0) abort($format("Unsupported arlen (0x%0x)", x.arlen));
      if (x.arsize != fromInteger(log2(valueOf(data_)/8)))
        abort($format("Unsupported arsize (0x%0x)", x.arsize));
      if (x.arburst != FIXED)
        abort($format("Unsupported arburst (", fshow(x.arburst), ")"));
      if (x.arlock != NORMAL)
        abort($format("Unsupported arlock (", fshow(x.arlock), ")"));
      if (x.arcache != 0)
        abort($format("Unsupported arcache (0x%0x)", x.arcache));
      if (x.arqos != 0) abort($format("Unsupported arqos (0x%0x)", x.arqos));
      if (x.arregion != 0)
        abort($format("Unsupported arregion (0x%0x)", x.arregion));
      snk.put(AXI4Lite_ARFlit {
        araddr: x.araddr, arprot: x.arprot, aruser: x.aruser
      });
    endaction;
  endmodule

  module fromAXIRLiteSource#(Source#(AXI4Lite_RFlit#(data_, ruser_)) src)
    (Source#(AXI4_RFlit#(id_, data_, ruser_)));
    function AXI4_RFlit#(id_, data_, ruser_) f(AXI4Lite_RFlit#(data_, ruser_) x) = AXI4_RFlit {
      rid: 0, rdata: x.rdata, rresp: x.rresp, rlast: True, ruser: x.ruser
    };
    method canPeek = src.canPeek;
    method peek = f(src.peek);
    method drop = src.drop;
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
