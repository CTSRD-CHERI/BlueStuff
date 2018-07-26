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

import FIFO :: *;
import RegFile :: *;
import GetPut :: *;
import ClientServer :: *;
import SpecialFIFOs :: *;

import Interface :: *;
import Utils_BRAM :: *;
import Utils_UnalignedMem :: *;
import SimUtils :: *;

////////////////////////////////////////
// Shared data and instruction memory //
////////////////////////////////////////////////////////////////////////////////

// size expressed in bytes

import "BDPI" mem_create = function ActionValue#(Bit#(64)) mem_create(st s)
                             provisos (Bits#(st, sw));
import "BDPI" mem_init   = function Action mem_init(Bit#(64) m, String f, Bit#(64) o);
import "BDPI" mem_read   = function ActionValue#(dt) mem_read(Bit#(64) m, at a, st s)
                             provisos (Bits#(dt, dw), Bits#(at, aw), Bits#(st, sw));
import "BDPI" mem_write  = function Action mem_write(Bit#(64) m, at a, st s, bet be, dt d)
                             provisos (Bits#(at, aw), Bits#(st, sw), Bits#(bet, bew), Bits#(dt, dw));
module mkSimSharedMem2#(Integer size, String file) (Mem2#(addr_t, t0, t1))
provisos (Bits#(addr_t, addr_sz), Bits#(t0, t0_sz), Bits#(t1, t1_sz));

  Reg#(Bool) isInitialized <- mkReg(False);
  Reg#(Bit#(64)) mem_ptr <- mkRegU;

  rule do_init (!isInitialized);
    let tmp <- mem_create(fromInteger(size));
    mem_init(tmp, file, fromInteger(0));
    mem_ptr <= tmp;
    isInitialized <= True;
  endrule

  FIFO#(MemRsp#(t0)) rsp0 <- mkPipelineFIFO;
  FIFO#(MemRsp#(t1)) rsp1 <- mkPipelineFIFO;

  interface Mem p0;
    interface request = interface Put; method put (req) if (isInitialized) = action
      case (req) matches
        tagged ReadReq .r: begin
          let addr = r.addr;
          let nbytes = readBitPO(r.numBytes);
          let res <- mem_read(mem_ptr, addr, nbytes);
          rsp0.enq(ReadRsp(res));
        end
        tagged WriteReq .w: mem_write(mem_ptr, w.addr, fromInteger(valueOf(TDiv#(SizeOf#(t0), 8))), w.byteEnable, w.data);
      endcase
    endaction; endinterface;
    interface response = interface Get; method get if (isInitialized) = actionvalue
      rsp0.deq; return rsp0.first;
    endactionvalue; endinterface;
  endinterface

  interface Mem p1;
    interface request = interface Put; method put (req) if (isInitialized) = action
      case (req) matches
        tagged ReadReq .r: begin
          let addr = r.addr;
          let nbytes = readBitPO(r.numBytes);
          let res <- mem_read(mem_ptr, addr, nbytes);
          rsp1.enq(ReadRsp(res));
        end
        tagged WriteReq .w: mem_write(mem_ptr, w.addr, fromInteger(valueOf(TDiv#(SizeOf#(t1), 8))), w.byteEnable, w.data);
      endcase
      endaction; endinterface;
    interface response = interface Get; method get if (isInitialized) = actionvalue
      rsp1.deq; return rsp1.first;
    endactionvalue; endinterface;
  endinterface

endmodule

module mkSharedMem2#(Integer size, String file) (Mem2#(addr_t, t0, t1))
provisos(
  Bits#(addr_t, addr_sz), Bits#(t0, t0_sz), Bits#(t1, t1_sz),
  Max#(t0_sz, t1_sz, chunk_sz), Div#(chunk_sz, BitsPerByte, chunk_byte_sz),
  Add#(idx_sz, TLog#(chunk_byte_sz), addr_sz),
  // port0 size relationships
  Add#(a__, TDiv#(t0_sz, BitsPerByte), chunk_byte_sz),
  Add#(b__, t0_sz, chunk_sz),
  Add#(c__, TLog#(TDiv#(t0_sz, BitsPerByte)), TAdd#(TLog#(chunk_byte_sz), 1)),
  Add#(d__, TLog#(TDiv#(t0_sz, BitsPerByte)), TLog#(chunk_byte_sz)),
  Log#(TAdd#(1, TDiv#(t0_sz, BitsPerByte)), TAdd#(TLog#(TDiv#(t0_sz, BitsPerByte)), 1)),
  // port1 size relationships
  Add#(e__, TDiv#(t1_sz, BitsPerByte), chunk_byte_sz),
  Add#(f__, t1_sz, chunk_sz),
  Add#(g__, TLog#(TDiv#(t1_sz, BitsPerByte)), TAdd#(TLog#(chunk_byte_sz), 1)),
  Add#(h__, TLog#(TDiv#(t1_sz, BitsPerByte)), TLog#(chunk_byte_sz)),
  Log#(TAdd#(1, TDiv#(t1_sz, BitsPerByte)), TAdd#(TLog#(TDiv#(t1_sz, BitsPerByte)), 1)),
  // address size relationships
  Add#(i__, TLog#(TDiv#(t1_sz, BitsPerByte)), addr_sz),
  Add#(j__, TLog#(TDiv#(t0_sz, BitsPerByte)), addr_sz),
  // chunk size relashionships
  Mul#(TDiv#(chunk_sz, chunk_byte_sz), chunk_byte_sz, chunk_sz),
  // FShow instances
  FShow#(addr_t), FShow#(t0), FShow#(t1)
);
  if (genC) begin
    Mem2#(addr_t, t0, t1) mem <- mkSimSharedMem2(size, file);
    interface p0 = mem.p0;
    interface p1 = mem.p1;
  end else begin
    // BRAM2
    BRAM2#(idx_sz, chunk_sz, idx_sz, chunk_sz)
      m <- mkAlteraBRAM2(size, file);
    Mem#(addr_t, t0) p0Ifc <- mkPortCtrl("port0", m.p0);
    Mem#(addr_t, t1) p1Ifc <- mkPortCtrl("port1", m.p1);
    // interface
    interface p0 = p0Ifc;
    interface p1 = p1Ifc;
  end
endmodule

module mkFullMem#(Integer size, String file, addr_t pc) (FullMem#(addr_t, inst_t, data_t))
provisos(
  Bits#(addr_t, addr_sz), Bits#(inst_t, inst_sz), Bits#(data_t, data_sz),
  Max#(inst_sz, data_sz, chunk_sz), Div#(chunk_sz, BitsPerByte, chunk_byte_sz),
  Add#(idx_sz, TLog#(chunk_byte_sz), addr_sz),
  // instruction size relationships
  Add#(a__, TDiv#(inst_sz, BitsPerByte), chunk_byte_sz),
  Add#(b__, inst_sz, chunk_sz),
  Add#(c__, TLog#(TDiv#(inst_sz, BitsPerByte)), TAdd#(TLog#(chunk_byte_sz), 1)),
  Add#(d__, TLog#(TDiv#(inst_sz, BitsPerByte)), TLog#(chunk_byte_sz)),
  Log#(TAdd#(1, TDiv#(inst_sz, BitsPerByte)), TAdd#(TLog#(TDiv#(inst_sz, BitsPerByte)), 1)),
  // data size relationships
  Add#(e__, TDiv#(data_sz, BitsPerByte), chunk_byte_sz),
  Add#(f__, data_sz, chunk_sz),
  Add#(g__, TLog#(TDiv#(data_sz, BitsPerByte)), TAdd#(TLog#(chunk_byte_sz), 1)),
  Add#(h__, TLog#(TDiv#(data_sz, BitsPerByte)), TLog#(chunk_byte_sz)),
  Log#(TAdd#(1, TDiv#(data_sz, BitsPerByte)), TAdd#(TLog#(TDiv#(data_sz, BitsPerByte)), 1)),
  // address size relationships
  Add#(i__, TLog#(TDiv#(data_sz, BitsPerByte)), addr_sz),
  Add#(j__, TLog#(TDiv#(inst_sz, BitsPerByte)), addr_sz),
  // chunk size relashionships
  Mul#(TDiv#(chunk_sz, chunk_byte_sz), chunk_byte_sz, chunk_sz),
  // FShow instances
  FShow#(addr_t), FShow#(inst_t), FShow#(data_t)
);

  Mem2#(addr_t, inst_t, data_t) mem <- mkSharedMem2(size, file);
  interface IMem inst;
    method Action reqNext () =  mem.p0.request.put(tagged ReadReq {
      addr: pc,
      numBytes: fromInteger(valueOf(TDiv#(SizeOf#(inst_t), BitsPerByte)))
    });
    method ActionValue#(inst_t) get ();
      let rsp <- mem.p0.response.get();
      return case (rsp) matches
        tagged ReadRsp .val: val;
        default: ?;
      endcase;
    endmethod
  endinterface
  interface data = mem.p1;

endmodule

///////////////////////////////
// Simple instruction Stream //
////////////////////////////////////////////////////////////////////////////////

// XXX hex format example in test-program.hex

`ifdef MAX_ISTREAM_LENGTH
typedef TLog#(MAX_ISTREAM_LENGTH) IStreamIdxSz;
`else
typedef 12 IStreamIdxSz;
`endif

// size expressed in bytes
module mkInstStream#(String file, Integer size) (InstStream#(n))
provisos (
  Mul#(byte_sz, BitsPerByte, n)
);

  RegFile#(Bit#(IStreamIdxSz), Bit#(n)) mem <- mkRegFileLoad(file, 0, fromInteger(size/valueOf(byte_sz) - 1));
  Reg#(Bit#(IStreamIdxSz)) counter <- mkReg(0);

  rule checkInst;
    printTLogPlusArgs("BlueUtils", $format("instr stream -- inst %0d = 0x%0x", counter, mem.sub(counter)));
  endrule

  method Bit#(n) peekInst() = mem.sub(counter);
  method Action nextInst() = action counter <= counter + 1; endaction;

endmodule
