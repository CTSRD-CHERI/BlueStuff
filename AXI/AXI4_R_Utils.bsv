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

///////////////////////////
// AXI Read Data Channel //
////////////////////////////////////////////////////////////////////////////////

// typeclasses to convert to/from the flit type

typeclass ToAXI4_RFlit#(type t,
numeric type id_, numeric type data_, numeric type user_);
  function AXI4_RFlit#(id_, data_, user_) toAXI4_RFlit (t x);
endtypeclass

instance ToAXI4_RFlit#(AXI4_RFlit#(a, b, c), a, b, c);
  function toAXI4_RFlit = id;
endinstance

typeclass FromAXI4_RFlit#(type t,
numeric type id_, numeric type data_, numeric type user_);
  function t fromAXI4_RFlit (AXI4_RFlit#(id_, data_, user_) x);
endtypeclass

instance FromAXI4_RFlit#(AXI4_RFlit#(a, b, c), a, b, c);
  function fromAXI4_RFlit = id;
endinstance

// typeclass to turn an interface to the Master interface

typeclass ToAXI4_R_Master_Synth#(type t);
  module toAXI4_R_Master_Synth#(t#(x) ifc)
  (AXI4_R_Master_Synth#(id_, data_, user_))
  provisos (FromAXI4_RFlit#(x, id_, data_, user_));
endtypeclass

instance ToAXI4_R_Master_Synth#(Sink);
  module toAXI4_R_Master_Synth#(Sink#(t) snk)
  (AXI4_R_Master_Synth#(id_, data_, user_))
  provisos (FromAXI4_RFlit#(t, id_, data_, user_));

    let w_rid   <- mkDWire(?);
    let w_rdata <- mkDWire(?);
    let w_rresp <- mkDWire(?);
    let w_rlast <- mkDWire(?);
    let w_ruser <- mkDWire(?);
    PulseWire putWire <- mkPulseWire;
    rule doPut (putWire && snk.canPut);
      snk.put(fromAXI4_RFlit(AXI4_RFlit{
        rid:   w_rid,
        rdata: w_rdata,
        rresp: w_rresp,
        rlast: w_rlast,
        ruser: w_ruser
      }));
    endrule

    method rid(id)       = action w_rid   <= id; endaction;
    method rdata(data)   = action w_rdata <= data; endaction;
    method rresp(resp)   = action w_rresp <= resp; endaction;
    method rlast(last)   = action w_rlast <= last; endaction;
    method ruser(user)   = action w_ruser <= user; endaction;
    method rvalid(valid) = action if (valid) putWire.send; endaction;
    method rready        = snk.canPut;

  endmodule
endinstance

instance ToAXI4_R_Master_Synth#(FIFOF);
  module toAXI4_R_Master_Synth#(FIFOF#(t) ff)
  (AXI4_R_Master_Synth#(id_, data_, user_))
  provisos (FromAXI4_RFlit#(t, id_, data_, user_));

    let w_rid   <- mkDWire(?);
    let w_rdata <- mkDWire(?);
    let w_rresp <- mkDWire(?);
    let w_rlast <- mkDWire(?);
    let w_ruser <- mkDWire(?);
    PulseWire enqWire <- mkPulseWire;
    rule doEnq (enqWire && ff.notFull);
      ff.enq(fromAXI4_RFlit(AXI4_RFlit{
        rid:   w_rid,
        rdata: w_rdata,
        rresp: w_rresp,
        rlast: w_rlast,
        ruser: w_ruser
      }));
    endrule

    method rid(id)       = action w_rid   <= id; endaction;
    method rdata(data)   = action w_rdata <= data; endaction;
    method rresp(resp)   = action w_rresp <= resp; endaction;
    method rlast(last)   = action w_rlast <= last; endaction;
    method ruser(user)   = action w_ruser <= user; endaction;
    method rvalid(valid) = action if (valid) enqWire.send; endaction;
    method rready        = ff.notFull;

  endmodule
endinstance

// typeclass to turn an interface to the Slave interface

typeclass ToAXI4_R_Slave_Synth#(type t);
  module toAXI4_R_Slave_Synth#(t#(x) ifc)
  (AXI4_R_Slave_Synth#(id_, data_, user_))
  provisos (ToAXI4_RFlit#(x, id_, data_, user_));
endtypeclass

instance ToAXI4_R_Slave_Synth#(Source);
  module toAXI4_R_Slave_Synth#(Source#(t) src)
  (AXI4_R_Slave_Synth#(id_, data_, user_))
  provisos (ToAXI4_RFlit#(t, id_, data_, user_));

    Wire#(AXI4_RFlit#(id_, data_, user_)) flit <- mkDWire(?);
    rule peekFlit (src.canPeek); flit <= toAXI4_RFlit(src.peek); endrule
    PulseWire dropWire <- mkPulseWire;
    rule doDrop (dropWire && src.canPeek); src.drop; endrule

    method rid    = flit.rid;
    method rdata  = flit.rdata;
    method rresp  = flit.rresp;
    method rlast  = flit.rlast;
    method ruser  = flit.ruser;
    method rvalid = src.canPeek;
    method rready(rdy) = action if (rdy) dropWire.send; endaction;

  endmodule
endinstance

instance ToAXI4_R_Slave_Synth#(FIFOF);
  module toAXI4_R_Slave_Synth#(FIFOF#(t) ff)
  (AXI4_R_Slave_Synth#(id_, data_, user_))
  provisos (ToAXI4_RFlit#(t, id_, data_, user_));

    Wire#(AXI4_RFlit#(id_, data_, user_)) flit <- mkDWire(?);
    rule peekFlit (ff.notEmpty); flit <= toAXI4_RFlit(ff.first); endrule
    PulseWire deqWire <- mkPulseWire;
    rule doDeq (deqWire && ff.notEmpty); ff.deq; endrule

    method rid    = flit.rid;
    method rdata  = flit.rdata;
    method rresp  = flit.rresp;
    method rlast  = flit.rlast;
    method ruser  = flit.ruser;
    method rvalid = ff.notEmpty;
    method rready(rdy) = action if (rdy) deqWire.send; endaction;

  endmodule
endinstance
