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

import Printf :: *;
import List :: *;

import AXI :: *;
import BasicTypes :: *;
import SimUtils :: *;

//////////////////////////
// Virtualize interface //
//////////////////////////

typeclass Virtualizable#(type a);
  module virtualize#(a x, Integer n)(Array#(a));
endtypeclass

instance Virtualizable#(Reg#(a));
  module virtualize#(Reg#(a) r, Integer n)(Array#(Reg#(a)));

    // static priority
    Wire#(Bool) canWrite[n] <- mkDCWire(n, True);

    // interface
    Reg#(a) ifc[n];

    for (Integer i = 0; i < n; i = i + 1) begin
      ifc[i] = interface Reg#(a);
        method _write(x) if (canWrite[i]) = action
          if (i < n-1) canWrite[i+1] <= False;
          r <= x;
        endaction;
        method _read = r._read;
      endinterface;
    end

    return ifc;
  endmodule

endinstance

// virtualizable instance for Server, with static priority
import ClientServer :: *;
import GetPut :: *;
import FIFO :: *;
import SpecialFIFOs :: *;

typeclass NeedRsp#(type req_t);
  function Bool needRsp(req_t req);
endtypeclass

instance Virtualizable#(Server#(req_t, rsp_t))
provisos (NeedRsp#(req_t), Bits#(req_t, a__), Bits#(rsp_t, b__));

  module virtualize#(Server#(req_t, rsp_t) server, Integer n)(Array#(Server#(req_t, rsp_t)));

    `define MAX_IDX_SZ 4
    if (log2(n) > `MAX_IDX_SZ)
      error(sprintf("Asked for %0d interfaces, virtualize can't support more than %0d", n, 2**`MAX_IDX_SZ));

    Server#(req_t, rsp_t) ifc[n];
    FIFO#(Bit#(`MAX_IDX_SZ)) ifcIdx <- mkFIFO;
    Rules ifcRules = emptyRules;

    for (Integer i = 0; i < n; i = i + 1) begin
      let reqFF <- mkBypassFIFO;
      let rspFF <- mkBypassFIFO;
      ifcRules = rJoinDescendingUrgency(ifcRules, rules
        rule doSendReq;
          reqFF.deq;
          let req = reqFF.first;
          server.request.put(req);
          if (needRsp(req)) ifcIdx.enq(fromInteger(i));
        endrule
        rule doGetRsp (ifcIdx.first == fromInteger(i));
          ifcIdx.deq;
          let rsp <- server.response.get;
          rspFF.enq(rsp);
        endrule
      endrules);
      ifc[i] = interface Server;
        interface request = interface Put; method put = reqFF.enq; endinterface;
        interface response = interface Get;
          method get = actionvalue rspFF.deq; return rspFF.first; endactionvalue;
        endinterface;
      endinterface;
    end

    addRules(ifcRules);

    return ifc;

  endmodule
endinstance

/////////////////////
// Interface types //
////////////////////////////////////////////////////////////////////////////////

// Type to hold an n-bit value initialized by a literal starting
// at 1 and up to 2^n rather than 0 to 2^n-1
typedef struct {
  Bit#(n) val;
} BitPO#(numeric type n) deriving (Bits);

function Bit#(TAdd#(n,1)) readBitPO (BitPO#(n) x);
  return (x.val == 0) ? fromInteger(valueOf(TExp#(n))) : zeroExtend(x.val);
endfunction

instance Literal#(BitPO#(n));
  function BitPO#(n) fromInteger (Integer x);
    if (x > 0 && x < valueOf(TExp#(n)))
      return BitPO { val: fromInteger(x) };
    else if (x == valueOf(TExp#(n)))
      return BitPO { val: 0 };
    else return error(sprintf("Trying to initialize a BitPO#(%0d) with literal %0d. The range of valid values is %0d to %0d.",valueOf(n),x,1,valueOf(TExp#(n))));
  endfunction
  function Bool inLiteralRange (BitPO#(n) _, Integer x);
    return (x > 0 && x <= valueOf(TExp#(n)));
  endfunction
endinstance

instance FShow#(BitPO#(n));
  function Fmt fshow(BitPO#(n) x);
    return $format("%0d", readBitPO(x));
  endfunction
endinstance

// How many bits per byte
typedef 8 BitsPerByte;

//////////////////////////////////
// Instruction Stream interface //
//////////////////////////////////

interface InstStream#(numeric type n);
  method Bit#(n) peekInst();
  method Action nextInst();
endinterface

//////////////////////////////////
// Instruction memory interface //
//////////////////////////////////

interface IMem#(type inst_t);
  method Action reqNext ();
  method ActionValue#(inst_t) get ();
endinterface

///////////////////////////
// Data memory interface //
///////////////////////////

// Mem request
typedef union tagged {
`define DATA_BYTES TDiv#(SizeOf#(content_t), BitsPerByte)
  struct {
    addr_t addr;
    BitPO#(TLog#(`DATA_BYTES)) numBytes;
  } ReadReq;
  struct {
    addr_t addr;
    Bit#(`DATA_BYTES) byteEnable;
    content_t data;
  } WriteReq;
} MemReq#(type addr_t, type content_t) deriving (Bits, FShow);

instance NeedRsp#(MemReq#(a,b));
  function needRsp(r);
    if (r matches tagged ReadReq .*) return True; else return False;
  endfunction
endinstance

instance ToAXIAWLiteFlit#(MemReq#(addr_t, data_t), addr_sz) provisos (Bits#(addr_t, addr_sz));
  function toAXIAWLiteFlit(x);
    let w = x.WriteReq;
    return AWLiteFlit {awaddr: pack(w.addr), awprot: 0};
  endfunction
endinstance

instance ToAXIWLiteFlit#(MemReq#(addr_t, data_t), data_sz) provisos (Bits#(data_t, data_sz));
  function toAXIWLiteFlit(x);
    let w = x.WriteReq;
    return WLiteFlit {wdata: pack(w.data), wstrb: w.byteEnable};
  endfunction
endinstance

instance ToAXIARLiteFlit#(MemReq#(addr_t, data_t), addr_sz) provisos (Bits#(addr_t, addr_sz));
  function toAXIARLiteFlit(x);
    let r = x.ReadReq;
    return ARLiteFlit {araddr: pack(r.addr), arprot: 0};
  endfunction
endinstance

// Mem response
typedef union tagged {
  content_t ReadRsp;
  void Failure;
} MemRsp#(type content_t) deriving (Bits, FShow);

instance FromAXIRLiteFlit#(MemRsp#(data_t), data_sz) provisos (Bits#(data_t, data_sz));
  function fromAXIRLiteFlit(x) = ReadRsp(unpack(x.rdata));
endinstance

instance FromAXIBLiteFlit#(MemRsp#(data_t));
  function fromAXIBLiteFlit(x) = ?;
endinstance

// Mem interface
typedef Server#(MemReq#(addr_t, content_t), MemRsp#(content_t)) Mem#(type addr_t, type content_t);
typedef Mem DMem;

interface Mem2#(type addr_t, type t0, type t1);
  interface Mem#(addr_t, t0) p0;
  interface Mem#(addr_t, t1) p1;
endinterface

///////////////////////////
// Full memory interface //
///////////////////////////

interface FullMem#(type addr_t, type inst_t, type data_t);
  interface DMem#(addr_t, data_t) data;
  interface IMem#(inst_t) inst;
endinterface
