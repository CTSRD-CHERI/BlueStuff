/*-
 * Copyright (c) 2018-2021 Alexandre Joannou
 * All rights reserved.
 *
 * This software was developed by SRI International and the University of
 * Cambridge Computer Laboratory (Department of Computer Science and
 * Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
 * DARPA SSITH research programme.
 *
 * This material is based upon work supported by the DoD Information Analysis
 * Center Program Management Office (DoD IAC PMO), sponsored by the Defense
 * Technical Information Center (DTIC) under Contract No. FA807518D0004.  Any
 * opinions, findings and conclusions or recommendations expressed in this
 * material are those of the author(s) and do not necessarily reflect the views
 * of the Air Force Installation Contracting Agency (AFICA).
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

import Printf :: *;
import FIFO :: *;
import SpecialFIFOs :: *;
import Clocks :: *;

import MasterSlave :: *;
import SourceSink :: *;

///////////////////////
// Simulation memory //
////////////////////////////////////////////////////////////////////////////////

typedef 64 MemSimMaxAddrSize;

typedef Bit #(64) MemCHandle;
typedef Bit #(MemSimMaxAddrSize) AddrT;
typedef Bit #(64) SizeT;

import "BDPI" mem_create =
  function ActionValue #(MemCHandle) mem_create (SizeT byteSize);
import "BDPI" mem_init =
  function Action mem_init (MemCHandle men, String hexFile, AddrT offset);
import "BDPI" mem_zero = function Action mem_zero (MemCHandle mem);
import "BDPI" mem_read =
  //function ActionValue #(data_t) mem_read ( MemCHandle mem
  function data_t mem_read ( MemCHandle mem
                           , AddrT addr
                           , SizeT byteSize )
  provisos (Bits #(data_t, data_sz));
import "BDPI" mem_write =
  function Action mem_write ( MemCHandle mem
                            , AddrT addr
                            , SizeT byteSize
                            , byteEn_t byteEn
                            , data_t data )
  provisos ( Bits #(byteEn_t, byteEn_sz)
           , Bits #(data_t, data_sz) );

module mkMemSimWithOffset #( Integer nIfcs
                           , Integer offset
                           , Integer size
                           , Maybe #(String) file)
  (Array #(Mem #(addr_t, data_t)))
  provisos ( Bits #(addr_t, addr_sz)
           , Bits #(data_t, data_sz)
           , Add #(_a, addr_sz, MemSimMaxAddrSize) );

  // sanitize size input
  SizeT actualSize = fromInteger (size);
  if (2**(log2 (size)) != size) begin
    SizeT powerOf2Size = fromInteger (2**(log2 (size) + 1));
    errorM (sprintf ( "Error: mkMemSimWithOffset called with non-power-of 2"
                    + " size %0d (nearest power-of-2 is %0d)"
                    , actualSize, powerOf2Size ));
  end

  let clk <- exposeCurrentClock;
  let rst <- mkReset (0, False, clk);

  Reg #(MemCHandle) memCHandle <- mkRegU;
  Reg #(Bool)     isAllocated <- mkSyncReg (False, clk, rst.new_rst, clk);
  Reg #(Bool)   isInitialized <- mkReg (False);

  rule do_alloc (!isAllocated);
    let tmp <- mem_create (actualSize);
    memCHandle <= tmp;
    isAllocated <= True;
  endrule
  rule do_init (isAllocated && !isInitialized);
    case (file) matches
      tagged Valid .f: mem_init (memCHandle, f, fromInteger (0));
      default: mem_zero (memCHandle);
    endcase
    isInitialized <= True;
  endrule

  Mem #(addr_t, data_t) ifcs[nIfcs];
  for (Integer i = 0; i < nIfcs; i = i + 1) begin
    FIFO #(MemRsp #(data_t)) rsp <- mkPipelineFIFO;
    ifcs[i] = interface Slave;
      interface sink = interface Sink;
        method canPut = isInitialized;
        method put (req) if (isInitialized) = action
          case (offsetMemReq (req, fromInteger (-offset))) matches
            tagged ReadReq .r: begin
              //let res <- mem_read ( memCHandle
              //                    , zeroExtend (pack (r.addr))
              //                    , 64'b1 << r.numBytes );
              let res = mem_read ( memCHandle
                                 , zeroExtend (pack (r.addr))
                                 , 64'b1 << r.numBytes );
              rsp.enq (ReadRsp (res));
            end
            tagged WriteReq .w: begin
              mem_write ( memCHandle
                        , zeroExtend (pack (w.addr))
                        , fromInteger (valueOf (TDiv #(data_sz, 8)))
                        , w.byteEnable
                        , w.data );
              rsp.enq (WriteRsp);
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

module mkMemSim #(Integer nIfcs, Integer size, Maybe #(String) file)
  (Array #(Mem #(addr_t, data_t)))
  provisos ( Bits #(addr_t, addr_sz)
           , Bits #(data_t, data_sz)
           , Add #(_a, addr_sz, MemSimMaxAddrSize) );
  let mem <- mkMemSimWithOffset (nIfcs, 0, size, file);
  return mem;
endmodule
