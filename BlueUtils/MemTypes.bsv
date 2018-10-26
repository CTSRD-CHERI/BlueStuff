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

import Virtualizable :: *;
import AXI :: *;

import MasterSlave :: *;
import Printf :: *;

///////////////////////
// Bit Plus One type //
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
    else return error(sprintf(
        "Trying to initialize a BitPO#(%0d) with literal %0d. "
      + "The range of valid values is %0d to %0d.",
        valueOf(n), x, 1, valueOf(TExp#(n))));
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

//////////////////////
// memory interface //
////////////////////////////////////////////////////////////////////////////////

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
`undef DATA_BYTES
} MemReq#(type addr_t, type content_t) deriving (Bits, FShow);

function MemReq#(addr_t, content_t) offsetMemReq(
  MemReq#(addr_t, content_t) r,
  Int#(addr_sz) o)
  provisos (Bits#(addr_t, addr_sz), Bits#(content_t, content_sz)) =
  case (r) matches
    tagged ReadReq .rr: ReadReq {
      addr: unpack(pack(unpack(pack(rr.addr)) + o)),
      numBytes: rr.numBytes
    };
    tagged WriteReq .wr: WriteReq {
      addr: unpack(pack(unpack(pack(wr.addr)) + o)),
      byteEnable: wr.byteEnable,
      data: wr.data
    };
  endcase;

instance NeedRsp#(MemReq#(a,b));
  function needRsp(r);
    if (r matches tagged ReadReq .*) return True; else return False;
  endfunction
endinstance

instance ToAXIAWLiteFlit#(MemReq#(addr_t, data_t), addr_sz)
  provisos (Bits#(addr_t, addr_sz));
  function toAXIAWLiteFlit(x);
    let w = x.WriteReq;
    return AWLiteFlit {awaddr: pack(w.addr), awprot: 0};
  endfunction
endinstance

instance ToAXIWLiteFlit#(MemReq#(addr_t, data_t), data_sz)
  provisos (Bits#(data_t, data_sz));
  function toAXIWLiteFlit(x);
    let w = x.WriteReq;
    return WLiteFlit {wdata: pack(w.data), wstrb: w.byteEnable};
  endfunction
endinstance

instance ToAXIARLiteFlit#(MemReq#(addr_t, data_t), addr_sz)
  provisos (Bits#(addr_t, addr_sz));
  function toAXIARLiteFlit(x);
    let r = x.ReadReq;
    return ARLiteFlit {araddr: pack(r.addr), arprot: 0};
  endfunction
endinstance

// Mem response
typedef union tagged {
  content_t ReadRsp;
  void WriteRsp;
  void BusError;
} MemRsp#(type content_t) deriving (Bits, FShow);

instance FromAXIRLiteFlit#(MemRsp#(data_t), data_sz)
  provisos (Bits#(data_t, data_sz));
  function fromAXIRLiteFlit(x) = case (x.rresp)
    OKAY: ReadRsp(unpack(x.rdata));
    default: BusError;
  endcase;
endinstance

instance FromAXIBLiteFlit#(MemRsp#(data_t));
  function fromAXIBLiteFlit(x) = case (x.bresp)
    OKAY: WriteRsp;
    default: BusError;
  endcase;
endinstance

// Mem interface
typedef Slave#(MemReq#(addr_t, content_t), MemRsp#(content_t))
  Mem#(type addr_t, type content_t);
