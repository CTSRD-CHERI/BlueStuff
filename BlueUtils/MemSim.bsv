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

import MemTypes :: *;

import FIFO :: *;
import SpecialFIFOs :: *;
import GetPut :: *;
import ClientServer :: *;

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

module mkMemSimWithOffset#(Integer n, Integer offset, Integer size, String file)
  (Array#(Mem#(addr_t, data_t)))
  provisos (Bits#(addr_t, addr_sz), Bits#(data_t, data_sz));

  Reg#(Bool) isInitialized <- mkReg(False);
  Reg#(Bit#(64)) mem_ptr <- mkRegU;

  rule do_init (!isInitialized);
    let tmp <- mem_create(fromInteger(size));
    mem_init(tmp, file, fromInteger(0));
    mem_ptr <= tmp;
    isInitialized <= True;
  endrule

  Mem#(addr_t, data_t) ifcs[n];
  for (Integer i = 0; i < n; i = i + 1) begin
    FIFO#(MemRsp#(data_t)) rsp <- mkPipelineFIFO;
    ifcs[i] = interface Server;
      interface request = interface Put;
        method put (req) if (isInitialized) = action
          case (offsetMemReq(req, fromInteger(-offset))) matches
            tagged ReadReq .r: begin
              let addr = r.addr;
              let nbytes = readBitPO(r.numBytes);
              let res <- mem_read(mem_ptr, addr, nbytes);
              rsp.enq(ReadRsp(res));
            end
            tagged WriteReq .w: mem_write(
                                  mem_ptr,
                                  w.addr,
                                  fromInteger(valueOf(TDiv#(data_sz, 8))),
                                  w.byteEnable,
                                  w.data);
          endcase
        endaction;
      endinterface;
      interface response = interface Get;
        method get  if (isInitialized) = actionvalue
          rsp.deq;
          return rsp.first;
        endactionvalue;
      endinterface;
    endinterface;
  end

  return ifcs;

endmodule
module mkMemSim#(Integer n, Integer size, String file)
  (Array#(Mem#(addr_t, data_t)))
  provisos (Bits#(addr_t, addr_sz), Bits#(data_t, data_sz));
  let mem <- mkMemSimWithOffset(n, 0, size, file);
  return mem;
endmodule
