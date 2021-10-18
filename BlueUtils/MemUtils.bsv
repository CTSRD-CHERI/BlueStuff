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
import MemBRAM :: *;
import MemSim :: *;
import AXI :: *;
import SourceSink :: *;
import MasterSlave :: *;
import FIFO :: *;

////////////////
// 1 port Mem //
////////////////////////////////////////////////////////////////////////////////

module mkMem #(Integer size, Maybe #(String) file) (Mem #(addr_t, data_t))
  provisos ( Bits #(addr_t, addr_sz)
           , Bits #(data_t, data_sz)
           , Add #(idx_sz, TLog #(TDiv #(data_sz, 8)), addr_sz)
           , Log #( TAdd #(1, TDiv #(data_sz, 8))
                  , TAdd #(TLog #(TDiv #(data_sz, 8)), 1) )
           , Add #(_a, addr_sz, MemSimMaxAddrSize) );
  `ifdef ALTERA
    BRAM #(idx_sz, data_sz)
      m <- mkAlteraBRAM (size, fromMaybe ("UNUSED", file));
    Mem #(addr_t, data_t) mem <- wrapUnaligned ("port 0", m);
    return mem;
  `else
    Mem #(addr_t, data_t) mem[1] <- mkMemSim (1, size, file);
    return mem[0];
  `endif
endmodule

module mkAXI4LiteMem #(Integer size, Maybe #(String) file)
                      (AXI4Lite_Slave #( addr_sz, data_sz
                                       , awuser_sz, wuser_sz, buser_sz
                                       , aruser_sz, ruser_sz ))
  provisos ( Log #( TAdd #(1, TDiv #(data_sz, 8))
                  , TAdd #(TLog #(TDiv #(data_sz, 8)), 1) )
           , Add #(_a, TLog #(TDiv #(data_sz, 8)), addr_sz)
           , Add #(_b, TLog #(TDiv #(data_sz, 8)), data_sz)
           , Add #(_c, addr_sz, MemSimMaxAddrSize) );
  Mem #(Bit #(addr_sz), Bit #(data_sz)) mem <- mkMem (size, file);
  Mem #(Bit #(addr_sz), Bit #(data_sz)) m[2];
  m[0] = mem;
  m[1] = mem;
  let ifc <- mkMem2ToAXI4Lite_Slave (m);
  return ifc;
endmodule

module mkAXI4Mem #(Integer size, Maybe #(String) file)
                  (AXI4_Slave #( id_sz, addr_sz, data_sz
                               , awuser_sz, wuser_sz, buser_sz
                               , aruser_sz, ruser_sz ))
  provisos ( NumAlias #(axiNumBytes_fat_sz, TExp #(SizeOf #(AXI4_Size)))
           , NumAlias #( numBytes_sz
                       , TMax #( 1
                               , TLog #(TAdd #( 1
                                              , TLog #(TDiv #(data_sz, 8))))))
           , NumAlias #(data_byte_sz, TDiv #(data_sz, 8))
           , NumAlias #(byteIdx_sz, TLog #(data_byte_sz))
           , Log #( TAdd #(1, TDiv #(data_sz, 8))
                  , TAdd #(TLog #(TDiv #(data_sz, 8)), 1))
           , Log #(data_byte_sz, byteIdx_sz)
           , Add #(_a, axiNumBytes_fat_sz, addr_sz)
           , Add #(_b, byteIdx_sz, addr_sz)
           , Add #(_c, numBytes_sz, SizeOf #(AXI4_Size))
           , Add #(_d, addr_sz, MemSimMaxAddrSize) );
  Mem #(Bit #(addr_sz), Bit #(data_sz)) mem <- mkMem (size, file);
  Mem #(Bit #(addr_sz), Bit #(data_sz)) m[2];
  m[0] = mem;
  m[1] = mem;
  let ifc <- mkMem2ToAXI4_Slave (m);
  return ifc;
endmodule

///////////////////////////
// 2 ports shared memory //
////////////////////////////////////////////////////////////////////////////////

module mkSharedMem2 #(Integer size, Maybe #(String) file)
                     (Array #(Mem #(addr_t, data_t)))
  provisos ( Bits #(addr_t, addr_sz)
           , Bits #(data_t, data_sz)
           , Add #(idx_sz, TLog #(TDiv #(data_sz, 8)), addr_sz)
           , Log #( TAdd #(1, TDiv #(data_sz, 8))
                  , TAdd #(TLog #(TDiv #(data_sz, 8)), 1) )
           , Add #(_a, addr_sz, MemSimMaxAddrSize) );
  `ifdef ALTERA
    BRAM2 #(idx_sz, data_sz, idx_sz, data_sz)
      m <- mkAlteraBRAM2 (size, fromMaybe ("UNUSED", file));
    Mem #(addr_t, data_t) mem[2];
    mem[0] <- wrapUnaligned ("port0", m.p0);
    mem[1] <- wrapUnaligned ("port1", m.p1);
    return mem;
  `else
    Mem #(addr_t, data_t) mem[2] <- mkMemSim (2, size, file);
    return mem;
  `endif
endmodule

module mkAXI4LiteSharedMem2 #(Integer size, Maybe #(String) file)
                             (AXI4Lite_Slave #( addr_sz, data_sz
                                              , awuser_sz, wuser_sz, buser_sz
                                              , aruser_sz, ruser_sz ))
  provisos ( Log #( TAdd #(1, TDiv #(data_sz, 8))
                  , TAdd #(TLog #(TDiv #(data_sz, 8)), 1) )
           , Add #(_a, TLog #(TDiv #(data_sz, 8)), addr_sz)
           , Add #(_b, TLog #(TDiv #(data_sz, 8)), data_sz)
           , Add #(_d, addr_sz, MemSimMaxAddrSize) );
  Mem #(Bit #(addr_sz), Bit #(data_sz)) mem[2] <- mkSharedMem2 (size, file);
  AXI4Lite_Slave #( addr_sz, data_sz
                  , awuser_sz, wuser_sz, buser_sz
                  , aruser_sz, ruser_sz) ifc;
  ifc <- mkMem2ToAXI4Lite_Slave (mem);
  return ifc;
endmodule

module mkAXI4SharedMem2 #(Integer size, Maybe #(String) file)
                         (AXI4_Slave #( id_sz, addr_sz, data_sz
                                      , awuser_sz, wuser_sz, buser_sz
                                      , aruser_sz, ruser_sz ))
  provisos ( NumAlias #(axiNumBytes_fat_sz, TExp #(SizeOf #(AXI4_Size)))
           , NumAlias #( numBytes_sz
                       , TMax #( 1
                               , TLog #(TAdd #( 1
                                              , TLog #(TDiv #(data_sz, 8))))))
           , NumAlias #(data_byte_sz, TDiv #(data_sz, 8))
           , NumAlias #(byteIdx_sz, TLog #(data_byte_sz))
           , Log #( TAdd #(1, TDiv #(data_sz, 8))
                  , TAdd #(TLog #(TDiv #(data_sz, 8)), 1))
           , Log #(data_byte_sz, byteIdx_sz)
           , Add #(_a, axiNumBytes_fat_sz, addr_sz)
           , Add #(_b, byteIdx_sz, addr_sz)
           , Add #(_c, numBytes_sz, SizeOf #(AXI4_Size))
           , Add #(_d, addr_sz, MemSimMaxAddrSize) );
  Mem #(Bit #(addr_sz), Bit #(data_sz)) mem[2] <- mkSharedMem2 (size, file);
  AXI4_Slave #( id_sz, addr_sz, data_sz
              , awuser_sz, wuser_sz, buser_sz
              , aruser_sz, ruser_sz) ifc;
  ifc <- mkMem2ToAXI4_Slave (mem);
  return ifc;
endmodule

//////////////////////////
// AXI4Lite Mem wrapper //
////////////////////////////////////////////////////////////////////////////////

module mkMem2ToAXI4Lite_Slave #(Array #(Mem #(addr_t, data_t)) mem)
                               (AXI4Lite_Slave #( addr_sz, data_sz
                                                , awuser_sz, wuser_sz, buser_sz
                                                , aruser_sz, ruser_sz ))
  provisos ( Bits #(data_t, data_sz)
           , Bits #(addr_t, addr_sz)
           , Div #(data_sz, 8, data_byte_sz)
           , Log #(data_byte_sz, byteIdx_sz)
           , Add #(_a, byteIdx_sz, addr_sz) );

  // AXI4Lite shim to provide a slave port
  AXI4Lite_Shim #( addr_sz, data_sz
                 , awuser_sz, wuser_sz, buser_sz
                 , aruser_sz, ruser_sz ) shim <- mkAXI4LiteShim;

  // responses info tracking fifos
  FIFO #(Bit #(0)) writeFF <- mkLFIFO;
  FIFO #(Bit #(addr_sz)) readFF <- mkLFIFO;

  // Force some prioritisation to cope with the case where both mem interfaces
  // are the same (i.e. using a single port Mem as the backing memory)
  (* descending_urgency = "readReq, writeReq" *)
  (* descending_urgency = "readRsp, writeRsp" *)

  // handle AXI4Lite write requests
  /////////////////////////////////

  rule writeReq;
    AXI4Lite_AWFlit #(addr_sz, awuser_sz) awflit <- get (shim.master.aw);
    AXI4Lite_WFlit #(data_sz, wuser_sz) wflit <- get (shim.master.w);
    Bit #(byteIdx_sz) byteShift = truncate (awflit.awaddr);
    let bitShift = {byteShift, 3'b000};
    mem[0].sink.put (
      tagged WriteReq { addr: unpack (awflit.awaddr)
                      , byteEnable: unpack (wflit.wstrb) >> byteShift
                      , data: unpack (wflit.wdata >> bitShift) });
    writeFF.enq (?);
  endrule
  rule writeRsp;
    mem[0].source.drop;
    writeFF.deq;
    shim.master.b.put (AXI4Lite_BFlit { bresp: OKAY, buser: ? });
  endrule

  // handle AXI4 read requests
  ////////////////////////////

  rule readReq;
    let arflit <- get (shim.master.ar);
    mem[1].sink.put (
      tagged ReadReq { addr: unpack (arflit.araddr)
                     , numBytes: fromInteger (log2 (valueOf (data_byte_sz))) });
    readFF.enq (arflit.araddr);
  endrule
  rule readRsp;
    let rsp <- get (mem[1].source);
    readFF.deq;
    Bit #(byteIdx_sz) byteShift = truncate (readFF.first);
    let bitShift = {byteShift, 3'b000};
    shim.master.r.put (AXI4Lite_RFlit { rdata: pack (rsp.ReadRsp) << bitShift
                                      , rresp: OKAY
                                      , ruser: ? });
  endrule

  // return the slave interface
  return shim.slave;
endmodule

//////////////////////
// AXI4 Mem wrapper //
////////////////////////////////////////////////////////////////////////////////

module mkMem2ToAXI4_Slave #(Array #(Mem #(addr_t, data_t)) mem)
                           (AXI4_Slave #( id_sz, addr_sz, data_sz
                                        , awuser_sz, wuser_sz, buser_sz
                                        , aruser_sz, ruser_sz ))
  provisos ( Bits #(data_t, data_sz)
           , Bits #(addr_t, addr_sz)
           , NumAlias #(axiNumBytes_fat_sz, TExp #(SizeOf #(AXI4_Size)))
           , NumAlias #( numBytes_sz
                       , TMax #( 1
                               , TLog #(TAdd #( 1
                                              , TLog #(TDiv #(data_sz, 8))))))
           , NumAlias #(data_byte_sz, TDiv #(data_sz, 8))
           , NumAlias #(byteIdx_sz, TLog #(data_byte_sz))
           , Log #(data_byte_sz, byteIdx_sz)
           , Add #(_a, axiNumBytes_fat_sz, addr_sz)
           , Add #(_b, byteIdx_sz, addr_sz)
           , Add #(_c, numBytes_sz, SizeOf #(AXI4_Size)) );

  // AXI4 shim to provide a slave port
  let shim <- mkAXI4Shim;

  // AXI4 addr calculation helpers
  function nextAddr (burst, size, addr) = case (burst)
    FIXED: addr;
    default: (addr + zeroExtend (fromAXI4_Size (size)));
  endcase;
  function nextLen (len) = len - 1;

  // rrequests info tracking registers
  Reg #(Maybe #( AXI4_AWFlit #(id_sz, addr_sz, awuser_sz)))
    awReg[2] <- mkCReg (2, Invalid);
  Reg #(Maybe #( AXI4_ARFlit #(id_sz, addr_sz, aruser_sz)))
    arReg[2] <- mkCReg (2, Invalid);

  // responses info tracking fifos
  let writeFF <- mkLFIFO;
  FIFO #(Tuple3 #(Bit #(id_sz), Bit #(addr_sz), AXI4_Len))
    readFF <- mkLFIFO;

  // Force some prioritisation to cope with the case where both mem interfaces
  // are the same (i.e. using a single port Mem as the backing memory)
  (* descending_urgency = "readReq, writeReq" *)
  (* descending_urgency = "readRsp, writeRsp" *)

  // handle AXI4 write requests
  /////////////////////////////

  rule onAW (!isValid (awReg [0]));
    let awflit <- get (shim.master.aw);
    awReg[0] <= (Valid (awflit));
  endrule
  rule writeReq (isValid (awReg[1]));
    let awflit = validValue (awReg[1]);
    let wflit <- get (shim.master.w);
    Bit #(byteIdx_sz) byteShift = truncate (awflit.awaddr);
    let bitShift = {byteShift, 3'b000};
    mem[0].sink.put (
      tagged WriteReq { addr: unpack (awflit.awaddr)
                      , byteEnable: unpack (wflit.wstrb) >> byteShift
                      , data: unpack (wflit.wdata >> bitShift) });
    if (awflit.awlen == 0) begin
      awReg[1] <= Invalid;
      writeFF.enq (awflit.awid);
      if (!wflit.wlast) begin
        $display ("should not happen");
        $finish();
      end
    end else
      awReg[1] <= Valid ( AXI4_AWFlit { awid: awflit.awid
                                      , awaddr: nextAddr ( awflit.awburst
                                                         , awflit.awsize
                                                         , awflit.awaddr )
                                      , awlen: nextLen (awflit.awlen)
                                      , awsize: awflit.awsize
                                      , awburst: awflit.awburst
                                      , awlock: ?
                                      , awcache: ?
                                      , awprot: ?
                                      , awqos: ?
                                      , awregion: ?
                                      , awuser: ? } );
  endrule
  rule writeRsp;
    mem[0].source.drop;
    writeFF.deq;
    shim.master.b.put (AXI4_BFlit { bid: writeFF.first
                                  , bresp: OKAY
                                  , buser: ? });
  endrule

  // handle AXI4 read requests
  ////////////////////////////

  rule onAR (!isValid (arReg [0]));
    let arflit <- get (shim.master.ar);
    arReg[0] <= (Valid (arflit));
  endrule
  rule readReq (isValid (arReg[1]));
    let arflit = validValue (arReg[1]);
    mem[1].sink.put (
      tagged ReadReq { addr: unpack (arflit.araddr)
                     , numBytes: truncate (pack (arflit.arsize)) });
    if (arflit.arlen == 0) begin
      arReg[1] <= Invalid;
      readFF.enq (tuple3 (arflit.arid, arflit.araddr, arflit.arlen));
    end else
      arReg[1] <= Valid ( AXI4_ARFlit { arid: arflit.arid
                                      , araddr: nextAddr ( arflit.arburst
                                                         , arflit.arsize
                                                         , arflit.araddr )
                                      , arlen: nextLen (arflit.arlen)
                                      , arsize: arflit.arsize
                                      , arburst: arflit.arburst
                                      , arlock: ?
                                      , arcache: ?
                                      , arprot: ?
                                      , arqos: ?
                                      , arregion: ?
                                      , aruser: ? } );
  endrule
  rule readRsp;
    let rsp <- get (mem[1].source);
    match {.arid, .araddr, .arlen} = readFF.first;
    readFF.deq;
    Bit #(byteIdx_sz) byteShift = truncate (araddr);
    let bitShift = {byteShift, 3'b000};
    shim.master.r.put (AXI4_RFlit { rid: arid
                                  , rdata: pack (rsp.ReadRsp) << bitShift
                                  , rresp: OKAY
                                  , rlast: arlen == 0
                                  , ruser: ? });
  endrule

  // return the slave interface
  return shim.slave;
endmodule
