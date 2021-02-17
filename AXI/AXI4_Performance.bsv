/*-
 * Copyright (c) 2021 Marno van der Maas
 * Copyright (c) 2021 Alexandre Joannou
 */

import AXI4 :: *;
import SourceSink :: *;

// event types
///////////////////////////////////////////////////////////////////////////////

typedef 8 Report_Width;
typedef Bit #(Report_Width) HpmRpt;
typedef struct {
   HpmRpt evt_AW_REQ;
   HpmRpt evt_W_REQ;
   HpmRpt evt_B_RSP;
   HpmRpt evt_AR_REQ;
   HpmRpt evt_R_RSP;
} AXI4_Events deriving (Bits, FShow);
//typedef TDiv#(SizeOf#(AXI4_Events),Report_Width) AXI4_Events_Elements;

// interface types
///////////////////////////////////////////////////////////////////////////////

interface AXI4_Initiator_Performance #(
    numeric type id_,
    numeric type addr_,
    numeric type data_,
    numeric type awuser_,
    numeric type wuser_,
    numeric type buser_,
    numeric type aruser_,
    numeric type ruser_);
  interface AXI4_Initiator #(id_, addr_, data_, awuser_, wuser_, buser_, aruser_, ruser_) initiator;
  method AXI4_Events events;
endinterface

// toAXI4PerformanceInitiator module definition
///////////////////////////////////////////////////////////////////////////////

module toAXI4PerformanceInitiator #(AXI4_Initiator #(a, b, c, d, e, f, g, h) i)
  (AXI4_Initiator_Performance #(a, b, c, d, e, f, g, h));

    PulseWire awWire <- mkPulseWire;
    PulseWire  wWire <- mkPulseWire;
    PulseWire  bWire <- mkPulseWire;
    PulseWire arWire <- mkPulseWire;
    PulseWire  rWire <- mkPulseWire;

    interface initiator = interface AXI4_Initiator;
                            interface aw = onDrop(i.aw, awWire.send);
                            interface  w = onDrop(i.w, wWire.send);
                            interface  b = onPut(i.b, constFn(bWire.send));
                            interface ar = onDrop(i.ar, arWire.send);
                            interface  r = onPut(i.r, constFn(rWire.send));
                          endinterface;
    method events = AXI4_Events {
      evt_AW_REQ: awWire ? 1 : 0,
      evt_W_REQ:   wWire ? 1 : 0,
      evt_B_RSP:   bWire ? 1 : 0,
      evt_AR_REQ: arWire ? 1 : 0,
      evt_R_RSP:   rWire ? 1 : 0
    };
endmodule
