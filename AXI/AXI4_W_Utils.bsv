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

import SourceSink :: *;

import AXI4_Types :: *;

import FIFOF :: *;
import SpecialFIFOs :: *;

////////////////////////////
// AXI Write Data Channel //
////////////////////////////////////////////////////////////////////////////////

// typeclasses to convert to/from the flit type

typeclass ToAXIWFlit#(type t, numeric type data_, numeric type user_);
  function WFlit#(data_, user_) toAXIWFlit (t x);
endtypeclass

instance ToAXIWFlit#(WFlit#(a, b), a, b);
  function toAXIWFlit = id;
endinstance

typeclass FromAXIWFlit#(type t,
numeric type data_, numeric type user_);
  function t fromAXIWFlit (WFlit#(data_, user_) x);
endtypeclass

instance FromAXIWFlit#(WFlit#(a, b), a, b);
  function fromAXIWFlit = id;
endinstance

// typeclass to turn an interface to the Master interface

typeclass ToAXIWMaster#(type t);
  module toAXIWMaster#(t#(x) ifc) (WMaster#(data_, user_))
  provisos (ToAXIWFlit#(x, data_, user_));
endtypeclass

instance ToAXIWMaster#(Source);
  module toAXIWMaster#(Source#(t) src)
  (WMaster#(data_, user_)) provisos (ToAXIWFlit#(t, data_, user_));

    Wire#(WFlit#(data_, user_)) flit <- mkDWire(?);
    rule peekFlit (src.canPeek); flit <= toAXIWFlit(src.peek); endrule
    PulseWire dropWire <- mkPulseWire;
    rule doDrop (dropWire && src.canPeek); src.drop; endrule

    method wdata  = flit.wdata;
    method wstrb  = flit.wstrb;
    method wlast  = flit.wlast;
    method wuser  = flit.wuser;
    method wvalid = src.canPeek;
    method wready(rdy) = action if (rdy) dropWire.send; endaction;

  endmodule
endinstance

instance ToAXIWMaster#(FIFOF);
  module toAXIWMaster#(FIFOF#(t) ff)
  (WMaster#(data_, user_)) provisos (ToAXIWFlit#(t, data_, user_));

    Wire#(WFlit#(data_, user_)) flit <- mkDWire(?);
    rule peekFlit (ff.notEmpty); flit <= toAXIWFlit(ff.first); endrule
    PulseWire deqWire <- mkPulseWire;
    rule doDeq (deqWire && ff.notEmpty); ff.deq; endrule

    method wdata  = flit.wdata;
    method wstrb  = flit.wstrb;
    method wlast  = flit.wlast;
    method wuser  = flit.wuser;
    method wvalid = ff.notEmpty;
    method wready(rdy) = action if (rdy) deqWire.send; endaction;

  endmodule
endinstance

// typeclass to turn an interface to the Slave interface

typeclass ToAXIWSlave#(type t);
  module toAXIWSlave#(t#(x) ifc) (WSlave#(data_, user_))
  provisos (FromAXIWFlit#(x, data_, user_));
endtypeclass

instance ToAXIWSlave#(Sink);
  module toAXIWSlave#(Sink#(t) snk)
  (WSlave#(data_, user_)) provisos (FromAXIWFlit#(t, data_, user_));

    let w_wdata <- mkDWire(?);
    let w_wstrb <- mkDWire(?);
    let w_wlast <- mkDWire(?);
    let w_wuser <- mkDWire(?);
    PulseWire putWire <- mkPulseWire;
    rule doPut (putWire && snk.canPut);
      snk.put(fromAXIWFlit(WFlit{
        wdata: w_wdata, wstrb: w_wstrb, wlast: w_wlast, wuser: w_wuser
      }));
    endrule

    method wdata(data)   = action w_wdata <= data; endaction;
    method wstrb(strb)   = action w_wstrb <= strb; endaction;
    method wlast(last)   = action w_wlast <= last; endaction;
    method wuser(user)   = action w_wuser <= user; endaction;
    method wvalid(valid) = action if (valid) putWire.send; endaction;
    method wready        = snk.canPut;

  endmodule
endinstance

instance ToAXIWSlave#(FIFOF);
  module toAXIWSlave#(FIFOF#(t) ff)
  (WSlave#(data_, user_)) provisos (FromAXIWFlit#(t, data_, user_));

    let w_wdata <- mkDWire(?);
    let w_wstrb <- mkDWire(?);
    let w_wlast <- mkDWire(?);
    let w_wuser <- mkDWire(?);
    PulseWire enqWire <- mkPulseWire;
    rule doEnq (enqWire && ff.notFull);
      ff.enq(fromAXIWFlit(WFlit{
        wdata: w_wdata, wstrb: w_wstrb, wlast: w_wlast, wuser: w_wuser
      }));
    endrule

    method wdata(data)   = action w_wdata <= data; endaction;
    method wstrb(strb)   = action w_wstrb <= strb; endaction;
    method wlast(last)   = action w_wlast <= last; endaction;
    method wuser(user)   = action w_wuser <= user; endaction;
    method wvalid(valid) = action if (valid) enqWire.send; endaction;
    method wready        = ff.notFull;

  endmodule
endinstance
