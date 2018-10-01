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

import Vector :: *;
import ConfigReg :: *;
import Printf :: *;

// Architectural state helpers
////////////////////////////////////////////////////////////////////////////////

// Architectural register with a commit method separate from the write
interface ArchReg#(type t);
  method t _read;
  method Action _write (t x);
  method Action commit;
  interface WriteOnly#(t) early;
  interface Reg#(t) late;
endinterface
module mkArchReg#(t resetVal) (ArchReg#(t)) provisos (Bits#(t, n));
  Reg#(t)         arch   <- mkConfigReg(resetVal);
  Reg#(Maybe#(t)) tmp[4] <- mkConfigCReg(4, Invalid);
  method  _read = arch._read;
  method _write(x) = tmp[1]._write(Valid(x));
  method Action commit = action if (isValid(tmp[3]._read)) begin
    arch <= tmp[3]._read.Valid;
    tmp[3] <= Invalid;
  end endaction;
  interface WriteOnly early;
    method _write(x) = tmp[0]._write(Valid(x));
  endinterface
  interface Reg late;
    method  _read = isValid(tmp[2]._read) ? tmp[2]._read.Valid : arch._read;
    method _write(x) = tmp[2]._write(Valid(x));
  endinterface
endmodule

// Read-only register
module mkROReg#(parameter a v) (Reg#(a));
  method Action _write (a _) = action endaction;
  method a _read() = v;
endmodule

// Register file with read-only 0 register (set to 0)
module mkRegFileZ (Vector#(n, Reg#(a)))
provisos (Bits#(a, a_sz), Literal#(a));
  Reg#(a) r0 <- mkROReg(0);
  Vector#(TSub#(n, 1), Reg#(a)) rf <- replicateM(mkReg(0));
  return cons(r0,rf);
endmodule

// Bypassable Register
module mkBypassReg#(parameter a v) (Reg#(a)) provisos(Bits#(a, a_sz));
  Reg#(a) r[2] <- mkCReg(2, v);
  method Action _write(a x) = action r[0] <= x; endaction;
  method a _read() = r[1];
endmodule

module mkBypassRegU (Reg#(a)) provisos(Bits#(a, a_sz));
  Reg#(a) r[2] <- mkCRegU(2);
  method Action _write(a x) = action r[0] <= x; endaction;
  method a _read() = r[1];
endmodule

// make an undefined register yeild compile time errors on reads and writes
module mkRegUndef#(String name) (Reg#(a));
  method a _read() =
    error(sprintf("%s register read but not initialised", name));
  method Action _write(a val) =
    error(sprintf("%s register written but not initialised", name));
endmodule

// make a Default Concurent Wire
module mkDCWire#(Integer n, t dflt) (Array#(Wire#(t))) provisos (Bits#(t, tw));
  Wire#(t) ifc[n];
  if (n > 0) begin
    List#(RWire#(t)) newVal <- List::replicateM(n, mkRWire);
    List#(Wire#(t)) prevVal <- List::replicateM(n, mkWire);
    rule defVal; prevVal[0] <= dflt; endrule
    for (Integer i = 0; i < n; i = i + 1) begin
      t readVal = fromMaybe(prevVal[i], newVal[i].wget);
      if (i < n-1) rule propagateVal; prevVal[i+1] <= readVal; endrule
      ifc[i] = interface Wire#(t);
        method _write = newVal[i].wset;
        method _read  = readVal;
      endinterface;
    end
  end
  return ifc;
endmodule

// CReg with a ConfigReg at its core
module mkConfigCReg#(Integer n, t dflt) (Array#(Reg#(t))) provisos (Bits#(t, tw));
  Reg#(t)     r <- mkConfigReg(dflt);
  Wire#(t) w[n] <- mkDCWire(n, r._read);
  if (n > 0) rule update_reg; r._write(w[n-1]); endrule
  Reg#(t) ifc[n];
  for (Integer i = 0; i < n; i = i + 1) begin
    ifc[i] = interface Reg#(t);
      method _write = w[i]._write;
      method  _read = (i > 0) ? w[i-1]._read : r._read;
    endinterface;
  end
  return ifc;
endmodule

// Combinational primitives
////////////////////////////////////////////////////////////////////////////////

// signed comparison functions
function Bool signedLT (Bit#(n) a, Bit#(n) b);
  Int#(n) sa = unpack(a);
  Int#(n) sb = unpack(b);
  return sa < sb;
endfunction
function Bool signedGT (Bit#(n) a, Bit#(n) b);
  Int#(n) sa = unpack(a);
  Int#(n) sb = unpack(b);
  return sa > sb;
endfunction
function Bool signedGE (Bit#(n) a, Bit#(n) b);
  Int#(n) sa = unpack(a);
  Int#(n) sb = unpack(b);
  return sa >= sb;
endfunction

// arithmetic right shift
function Bit#(n) arithRightShift (Bit#(n) a, Bit#(m) b);
  Int#(n) sa = unpack(a);
  return pack(sa >> b);
endfunction
