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

package Routable;

import List :: *;
import ListExtra :: *;

import Dict :: *;

////////////////////////
// Routable typeclass //
////////////////////////////////////////////////////////////////////////////////

typeclass Routable#(type a, type b) dependencies(a determines b);
  function b    routingField (a val);
  function Bool isLast       (a val);
endtypeclass

///////////////////////
// Memory Range type //
////////////////////////////////////////////////////////////////////////////////

typedef struct {
  Bit#(n) base;
  Bit#(n) size;
} Range#(numeric type n);
function Bit#(n) rangeBase(Range#(n) range) = range.base;
function Bit#(n) rangeSize(Range#(n) range) = range.size;
function Bit#(n) rangeTop (Range#(n) range) = range.base + range.size;
function Bool inRange(Bit#(n) addr, Range#(n) range) =
  (addr >= range.base && addr < rangeTop(range));

////////////////////////
// Mapping table type //
////////////////////////////////////////////////////////////////////////////////
typedef List#(Range#(n)) MappingTable#(numeric type n);
function List#(Bool) routeFromMappingTable (MappingTable#(n) mt, Bit#(n) addr);
  function pred(x) = inRange(addr, tpl_1(x));
  case (find(pred, zip(mt, upto(0, valueOf(n)-1)))) matches
    tagged Valid {.range, .idx}: return oneHotList(length(mt), idx);
    tagged Invalid: return replicate(valueOf(n), False);
  endcase
endfunction

endpackage
