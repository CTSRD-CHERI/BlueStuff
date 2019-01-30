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

import MemTypes :: *;

import FIFO :: *;
import SpecialFIFOs :: *;
import Clocks :: *;

import MasterSlave :: *;
import SourceSink :: *;

////////////////////////////////////////
// Shared data and instruction memory //
////////////////////////////////////////////////////////////////////////////////

// size expressed in bytes

import "BDPI" mem_create = function ActionValue#(Bit#(64)) mem_create(st s)
                             provisos (Bits#(st, sw));
import "BDPI" mem_init   = function Action mem_init(
                             Bit#(64) m,
                             String f,
                             Bit#(64) o);
import "BDPI" mem_zero   = function Action mem_zero(Bit#(64) m);
import "BDPI" mem_read   = function ActionValue#(dt) mem_read(
                             Bit#(64) m,
                             at a,
                             st s)
                             provisos (
                               Bits#(dt, dw),
                               Bits#(at, aw),
                               Bits#(st, sw));
import "BDPI" mem_write  = function Action mem_write(
                             Bit#(64) m,
                             at a,
                             st s,
                             bet be,
                             dt d)
                             provisos (
                               Bits#(at, aw),
                               Bits#(st, sw),
                               Bits#(bet, bew),
                               Bits#(dt, dw));

module mkMemSimWithOffset#(Integer n, Integer offset, Integer size, Maybe#(String) file)
  (Array#(Mem#(addr_t, data_t)))
  provisos (
    Bits#(addr_t, addr_sz), Bits#(data_t, data_sz)
    /*,FShow#(addr_t), FShow#(data_t)*/
  );

  let clk <- exposeCurrentClock;
  let rst <- mkReset(0, False, clk);
  Reg#(Bit#(64))   mem_ptr <- mkRegU;
  Reg#(Bool)   isAllocated <- mkSyncReg(False, clk, rst.new_rst, clk);
  Reg#(Bool) isInitialized <- mkReg(False);

  rule do_alloc (!isAllocated);
    let tmp <- mem_create(fromInteger(size));
    mem_ptr <= tmp;
    isAllocated <= True;
  endrule
  rule do_init (isAllocated && !isInitialized);
    case (file) matches
      tagged Valid .f: mem_init(mem_ptr, f, fromInteger(0));
      default: mem_zero(mem_ptr);
    endcase
    isInitialized <= True;
  endrule

  Mem#(addr_t, data_t) ifcs[n];
  for (Integer i = 0; i < n; i = i + 1) begin
    FIFO#(MemRsp#(data_t)) rsp <- mkPipelineFIFO;
    ifcs[i] = interface Slave;
      interface sink = interface Sink;
        method canPut = isInitialized;
        method put (req) if (isInitialized) = action
          case (offsetMemReq(req, fromInteger(-offset))) matches
            tagged ReadReq .r: begin
              let addr = r.addr;
              let nbytes = readBitPO(r.numBytes);
              let res <- mem_read(mem_ptr, addr, nbytes);
              rsp.enq(ReadRsp(res));
            end
            tagged WriteReq .w: begin
              mem_write(mem_ptr,
                        w.addr,
                        fromInteger(valueOf(TDiv#(data_sz, 8))),
                        w.byteEnable,
                        w.data);
              rsp.enq(WriteRsp);
            end
          endcase
        endaction;
      endinterface;
      interface source = interface Source;
        method canPeek = isInitialized;
        method peek if (isInitialized) = rsp.first;
        method drop if (isInitialized) = rsp.deq;
      endinterface;
    endinterface;
  end

  return ifcs;

endmodule
module mkMemSim#(Integer n, Integer size, Maybe#(String) file)
  (Array#(Mem#(addr_t, data_t)))
  provisos (
    Bits#(addr_t, addr_sz), Bits#(data_t, data_sz)
    /*,FShow#(addr_t), FShow#(data_t)*/
  );
  let mem <- mkMemSimWithOffset(n, 0, size, file);
  return mem;
endmodule
