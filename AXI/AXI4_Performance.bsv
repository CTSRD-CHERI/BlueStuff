/*-
 * Copyright (c) 2021 Marno van der Maas
 * Copyright (c) 2021 Alexandre Joannou
 */

import AXI4::*;

// event types
///////////////////////////////////////////////////////////////////////////////

typedef 8 Report_Width;
typedef Bit#(Report_Width) HpmRpt;
typedef struct {
   HpmRpt evt_READ_REQ;
   HpmRpt evt_WRITE_REQ;
   HpmRpt evt_WRITE_REQ_FLIT;
   HpmRpt evt_READ_RSP;
   HpmRpt evt_READ_RSP_FLIT;
   HpmRpt evt_WIRTE_RSP;
} EventsAxi4 deriving (Bits, FShow); // Memory needs more space for reporting delays
typedef TDiv#(SizeOf#(EventsAxi4),Report_Width) EventsAxi4Elements;

// interface types
///////////////////////////////////////////////////////////////////////////////

interface AXI4_Performance_IFC;
  interface AXI4_Master#(a, b, c, d, e, f, g, h) initiator;
  EventAxi4 events;
endinterface

// mkAxiPerformance module definition
///////////////////////////////////////////////////////////////////////////////

//(*synthesize*)
module mkAxiPerformance#(AXI4_Master#(a, b, c, d, e, f, g, h) i)(AXI4_Performance_IFC);

  EventsAxi4 axi4Evnt = unpack(0);

  interface initiator = interface AXI4_Master;
    interface aw = i.aw;
    interface w  = i.w;
    interface b  = i.b;
    interface ar = i.ar;
    interface r  = i.r;
  endinterface;

  rule countEvents;
    HpmRpt reqRead      = 0;
    HpmRpt reqWrite     = 0;
    HpmRpt reqWriteLast = 0;
    HpmRpt rspRead      = 0;
    HpmRpt rspReadLast  = 0;
    HpmRpt rspWrite     = 0;
    //TODO rewrite the rest of this rule...
    case (reqGet) matches
      tagged Valid .r: case (r.operation) matches
        tagged Read .rr: reqRead = True;
        tagged Write .wr: begin
          reqWrite = True;
          reqWriteLast = wr.last;
        end
      endcase
    endcase
    case (rspPut) matches
      tagged Valid .r: case (r.operation) matches
        tagged Read .rr: begin
          rspRead = True;
          rspReadLast = rr.last;
        end
        tagged Write: rspWrite = True;
      endcase
    endcase
    masterEvnt <= MasterEvents {
      id: 0,
      incReadReq:      reqRead,
      incWriteReq:     reqWrite && reqWriteLast,
      incWriteReqFlit: reqWrite,
      incReadRsp:      rspRead && rspReadLast,
      incReadRspFlit:  rspRead,
      incWriteRsp:     rspWrite
    };
  endrule

endmodule
