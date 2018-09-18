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

import SourceSink :: *;

import AXI4Lite_Types :: *;

import FIFOF :: *;
import SpecialFIFOs :: *;

///////////////////////////
// AXI Read Data Channel //
////////////////////////////////////////////////////////////////////////////////

// typeclasses to convert to/from the flit type

typeclass ToAXIRLiteFlit#(type t, numeric type data_);
  function RLiteFlit#(data_) toAXIRLiteFlit (t x);
endtypeclass

instance ToAXIRLiteFlit#(RLiteFlit#(a), a);
  function toAXIRLiteFlit = id;
endinstance

typeclass FromAXIRLiteFlit#(type t, numeric type data_);
  function t fromAXIRLiteFlit (RLiteFlit#(data_) x);
endtypeclass

instance FromAXIRLiteFlit#(RLiteFlit#(a), a);
  function fromAXIRLiteFlit = id;
endinstance

// typeclass to turn an interface to the Master interface

typeclass ToAXIRLiteMaster#(type t);
  module toAXIRLiteMaster#(t#(x) ifc) (RLiteMaster#(data_))
  provisos (FromAXIRLiteFlit#(x, data_));
endtypeclass

instance ToAXIRLiteMaster#(Sink);
  module toAXIRLiteMaster#(Sink#(t) snk)
  (RLiteMaster#(data_)) provisos (FromAXIRLiteFlit#(t, data_));

    let w_rdata <- mkDWire(?);
    let w_rresp <- mkDWire(?);
    PulseWire putWire <- mkPulseWire;
    rule doPut (putWire && snk.canPut);
      snk.put(fromAXIRLiteFlit(RLiteFlit{rdata: w_rdata, rresp: w_rresp}));
    endrule

    method rdata(data)   = action w_rdata <= data; endaction;
    method rresp(resp)   = action w_rresp <= resp; endaction;
    method rvalid(valid) = action if (valid) putWire.send; endaction;
    method rready        = snk.canPut;

  endmodule
endinstance

instance ToAXIRLiteMaster#(FIFOF);
  module toAXIRLiteMaster#(FIFOF#(t) ff)
  (RLiteMaster#(data_)) provisos (FromAXIRLiteFlit#(t, data_));

    let w_rdata <- mkDWire(?);
    let w_rresp <- mkDWire(?);
    PulseWire enqWire <- mkPulseWire;
    rule doEnq (enqWire && ff.notFull);
      ff.enq(fromAXIRLiteFlit(RLiteFlit{rdata: w_rdata, rresp: w_rresp}));
    endrule

    method rdata(data)   = action w_rdata <= data; endaction;
    method rresp(resp)   = action w_rresp <= resp; endaction;
    method rvalid(valid) = action if (valid) enqWire.send; endaction;
    method rready        = ff.notFull;

  endmodule
endinstance

// typeclass to turn an interface to the Slave interface

typeclass ToAXIRLiteSlave#(type t);
  module toAXIRLiteSlave#(t#(x) ifc) (RLiteSlave#(data_))
  provisos (ToAXIRLiteFlit#(x, data_));
endtypeclass

instance ToAXIRLiteSlave#(Source);
  module toAXIRLiteSlave#(Source#(t) src)
  (RLiteSlave#(data_)) provisos (ToAXIRLiteFlit#(t, data_));

    Wire#(RLiteFlit#(data_)) flit <- mkDWire(?);
    rule getFlit (src.canGet); flit <= toAXIRLiteFlit(src.peek); endrule
    PulseWire getWire <- mkPulseWire;
    rule doGet (getWire && src.canGet); let _ <- src.get; endrule

    method rdata  = flit.rdata;
    method rresp  = flit.rresp;
    method rvalid = src.canGet;
    method rready(rdy) = action if (rdy) getWire.send; endaction;

  endmodule
endinstance

instance ToAXIRLiteSlave#(FIFOF);
  module toAXIRLiteSlave#(FIFOF#(t) ff)
  (RLiteSlave#(data_)) provisos (ToAXIRLiteFlit#(t, data_));

    Wire#(RLiteFlit#(data_)) flit <- mkDWire(?);
    rule getFlit (ff.notEmpty); flit <= toAXIRLiteFlit(ff.first); endrule
    PulseWire deqWire <- mkPulseWire;
    rule doDeq (deqWire && ff.notEmpty); ff.deq; endrule

    method rdata  = flit.rdata;
    method rresp  = flit.rresp;
    method rvalid = ff.notEmpty;
    method rready(rdy) = action if (rdy) deqWire.send; endaction;

  endmodule
endinstance
