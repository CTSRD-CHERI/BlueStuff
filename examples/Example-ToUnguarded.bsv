/*-
 * Copyright (c) 2020 Alexandre Joannou
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

import FIFOF :: *;
import SpecialFIFOs :: *;
import SourceSink :: *;

interface SrcSnk#(type t);
  interface Source#(t) source;
  interface Sink#(t) sink;
endinterface

module mkGuardedSrcSnk_own (SrcSnk#(t)) provisos (Bits#(t, t_sz));
  Reg#(Bool) hasData <- mkReg(False);
  Reg#(t) r <- mkRegU;
  interface source = interface Source;
    method canPeek = hasData;
    method peek if (hasData) = r;
    method drop if (hasData) = action hasData <= False; endaction;
  endinterface;
  interface sink = interface Sink;
    method canPut = !hasData;
    method put (x) if (!hasData) = action r <= x; hasData <= True; endaction;
  endinterface;
endmodule

module mkGuardedSrcSnk_FIFOF (SrcSnk#(t)) provisos (Bits#(t, t_sz));
  let ff <- mkFIFOF;
  interface source = toSource (ff);
  interface sink   = toSink   (ff);
endmodule

module mkGuardedSrcSnk_UGFIFOF (SrcSnk#(t)) provisos (Bits#(t, t_sz));
  let ff <- mkUGFIFOF;
  interface source = toGuardedSource (ff);
  interface sink   = toGuardedSink   (ff);
endmodule

module top (Empty);

  Reg #(Bit #(8)) count <- mkReg (0);
  rule countUp; count <= count + 1; endrule

  //let srcsnk <- mkGuardedSrcSnk_own;
  //let srcsnk <- mkGuardedSrcSnk_FIFOF;
  let srcsnk <- mkGuardedSrcSnk_UGFIFOF;
  let snk = debugSink (srcsnk.sink, $format ("Sink G side"));
  let src = debugSource (srcsnk.source, $format ("Source G side"));

  `define NO_LOSS
  `ifdef NO_LOSS
  rule write;
    $display ("%0t - G - writing %0d", $time, count);
    snk.put (count);
  endrule
  rule read;
    $display ("%0t - G - reading %0d", $time, src.peek);
    src.drop;
  endrule
  `else
  let tmp_snk <- toUnguardedSink (snk);
  let tmp_src <- toUnguardedSource (src, 55);
  let u_snk = debugSink (tmp_snk, $format ("Sink UG side"));
  let u_src = debugSource (tmp_src, $format ("Source UG side"));
  rule u_write (count < 10);
    $display ("%0t - UG - writing %0d", $time, count);
    u_snk.put (count);
  endrule
  rule u_read (count >= 10);
    $display ("%0t - UG - reading %0d", $time, u_src.peek);
    u_src.drop;
    if (count >= 30) $finish (0);
  endrule
  `endif

  rule finishing (count > 15); $finish(); endrule

endmodule
