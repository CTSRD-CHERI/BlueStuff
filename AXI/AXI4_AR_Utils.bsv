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

//////////////////////////////
// AXI Read Address Channel //
////////////////////////////////////////////////////////////////////////////////

// typeclasses to convert to/from the flit type

typeclass ToAXIARFlit#(type t,
numeric type id_, numeric type addr_, numeric type user_);
  function ARFlit#(id_, addr_, user_) toAXIARFlit (t x);
endtypeclass

instance ToAXIARFlit#(ARFlit#(a, b, c), a, b, c);
  function toAXIARFlit = id;
endinstance

typeclass FromAXIARFlit#(type t,
numeric type id_, numeric type addr_, numeric type user_);
  function t fromAXIARFlit (ARFlit#(id_, addr_, user_) x);
endtypeclass

instance FromAXIARFlit#(ARFlit#(a, b, c), a, b, c);
  function fromAXIARFlit = id;
endinstance

// typeclass to turn an interface to the Master interface

typeclass ToAXIARMaster#(type t);
  module toAXIARMaster#(t#(x) ifc) (ARMaster#(id_, addr_, user_))
  provisos (ToAXIARFlit#(x, id_, addr_, user_));
endtypeclass

instance ToAXIARMaster#(Source);
  module toAXIARMaster#(Source#(t) src)
  (ARMaster#(id_, addr_, user_)) provisos (ToAXIARFlit#(t, id_, addr_, user_));

    Wire#(ARFlit#(id_, addr_, user_)) flit <- mkDWire(?);
    rule peekFlit (src.canPeek); flit <= toAXIARFlit(src.peek); endrule
    PulseWire dropWire <- mkPulseWire;
    rule doDrop (dropWire && src.canPeek); src.drop; endrule

    method arid     = flit.arid;
    method araddr   = flit.araddr;
    method arlen    = flit.arlen;
    method arsize   = flit.arsize;
    method arburst  = flit.arburst;
    method arlock   = flit.arlock;
    method arcache  = flit.arcache;
    method arprot   = flit.arprot;
    method arqos    = flit.arqos;
    method arregion = flit.arregion;
    method aruser   = flit.aruser;
    method arvalid  = src.canPeek;
    method arready(rdy) = action if (rdy) dropWire.send; endaction;

  endmodule
endinstance

instance ToAXIARMaster#(FIFOF);
  module toAXIARMaster#(FIFOF#(t) ff)
  (ARMaster#(id_, addr_, user_)) provisos (ToAXIARFlit#(t, id_, addr_, user_));

    Wire#(ARFlit#(id_, addr_, user_)) flit <- mkDWire(?);
    rule peekFlit (ff.notEmpty); flit <= toAXIARFlit(ff.first); endrule
    PulseWire deqWire <- mkPulseWire;
    rule doDeq (deqWire && ff.notEmpty); ff.deq; endrule

    method arid     = flit.arid;
    method araddr   = flit.araddr;
    method arlen    = flit.arlen;
    method arsize   = flit.arsize;
    method arburst  = flit.arburst;
    method arlock   = flit.arlock;
    method arcache  = flit.arcache;
    method arprot   = flit.arprot;
    method arqos    = flit.arqos;
    method arregion = flit.arregion;
    method aruser   = flit.aruser;
    method arvalid  = ff.notEmpty;
    method arready(rdy) = action if (rdy) deqWire.send; endaction;

  endmodule
endinstance

// typeclass to turn an interface to the Slave interface

typeclass ToAXIARSlave#(type t);
  module toAXIARSlave#(t#(x) ifc) (ARSlave#(id_, addr_, user_))
  provisos (FromAXIARFlit#(x, id_, addr_, user_));
endtypeclass

instance ToAXIARSlave#(Sink);
  module toAXIARSlave#(Sink#(t) snk)
  (ARSlave#(id_, addr_, user_)) provisos (FromAXIARFlit#(t, id_, addr_, user_));

    let w_arid     <- mkDWire(?);
    let w_araddr   <- mkDWire(?);
    let w_arlen    <- mkDWire(?);
    let w_arsize   <- mkDWire(?);
    let w_arburst  <- mkDWire(?);
    let w_arlock   <- mkDWire(?);
    let w_arcache  <- mkDWire(?);
    let w_arprot   <- mkDWire(?);
    let w_arqos    <- mkDWire(?);
    let w_arregion <- mkDWire(?);
    let w_aruser   <- mkDWire(?);
    PulseWire putWire <- mkPulseWire;
    rule doPut (putWire && snk.canPut);
      snk.put(fromAXIARFlit(ARFlit{
        arid:     w_arid,
        araddr:   w_araddr,
        arlen:    w_arlen,
        arsize:   w_arsize,
        arburst:  w_arburst,
        arlock:   w_arlock,
        arcache:  w_arcache,
        arprot:   w_arprot,
        arqos:    w_arqos,
        arregion: w_arregion,
        aruser:   w_aruser
      }));
    endrule

    method arid(id)         = action w_arid     <= id; endaction;
    method araddr(addr)     = action w_araddr   <= addr; endaction;
    method arlen(len)       = action w_arlen    <= len; endaction;
    method arsize(size)     = action w_arsize   <= size; endaction;
    method arburst(burst)   = action w_arburst  <= burst; endaction;
    method arlock(lock)     = action w_arlock   <= lock; endaction;
    method arcache(cache)   = action w_arcache  <= cache; endaction;
    method arprot(prot)     = action w_arprot   <= prot; endaction;
    method arqos(qos)       = action w_arqos    <= qos; endaction;
    method arregion(region) = action w_arregion <= region; endaction;
    method aruser(user)     = action w_aruser   <= user; endaction;
    method arvalid(valid)   = action if (valid) putWire.send; endaction;
    method arready          = snk.canPut;

  endmodule
endinstance

instance ToAXIARSlave#(FIFOF);
  module toAXIARSlave#(FIFOF#(t) ff)
  (ARSlave#(id_, addr_, user_)) provisos (FromAXIARFlit#(t, id_, addr_, user_));

    let w_arid     <- mkDWire(?);
    let w_araddr   <- mkDWire(?);
    let w_arlen    <- mkDWire(?);
    let w_arsize   <- mkDWire(?);
    let w_arburst  <- mkDWire(?);
    let w_arlock   <- mkDWire(?);
    let w_arcache  <- mkDWire(?);
    let w_arprot   <- mkDWire(?);
    let w_arqos    <- mkDWire(?);
    let w_arregion <- mkDWire(?);
    let w_aruser   <- mkDWire(?);
    PulseWire enqWire <- mkPulseWire;
    rule doEnq (enqWire && ff.notFull);
      ff.enq(fromAXIARFlit(ARFlit{
        arid:     w_arid,
        araddr:   w_araddr,
        arlen:    w_arlen,
        arsize:   w_arsize,
        arburst:  w_arburst,
        arlock:   w_arlock,
        arcache:  w_arcache,
        arprot:   w_arprot,
        arqos:    w_arqos,
        arregion: w_arregion,
        aruser:   w_aruser
      }));
    endrule

    method arid(id)         = action w_arid     <= id; endaction;
    method araddr(addr)     = action w_araddr   <= addr; endaction;
    method arlen(len)       = action w_arlen    <= len; endaction;
    method arsize(size)     = action w_arsize   <= size; endaction;
    method arburst(burst)   = action w_arburst  <= burst; endaction;
    method arlock(lock)     = action w_arlock   <= lock; endaction;
    method arcache(cache)   = action w_arcache  <= cache; endaction;
    method arprot(prot)     = action w_arprot   <= prot; endaction;
    method arqos(qos)       = action w_arqos    <= qos; endaction;
    method arregion(region) = action w_arregion <= region; endaction;
    method aruser(user)     = action w_aruser   <= user; endaction;
    method arvalid(valid)   = action if (valid) enqWire.send; endaction;
    method arready          = ff.notFull;

  endmodule
endinstance
