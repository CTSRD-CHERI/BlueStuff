// Copyright Marno van der Maas

//From AXI4_Types.bsv
import AXI4_Types :: *;

typedef AXI4_AW_Master_Synth AXI4_AW_Initiator_Synth;
typedef AXI4_AW_Slave_Synth AXI4_AW_Target_Synth;
typedef AXI4_W_Master_Synth AXI4_W_Initiator_Synth;
typedef AXI4_W_Slave_Synth AXI4_W_Target_Synth;
typedef AXI4_B_Master_Synth AXI4_B_Initiator_Synth;
typedef AXI4_B_Slave_Synth AXI4_B_Target_Synth;
typedef AXI4_AR_Master_Synth AXI4_AR_Initiator_Synth;
typedef AXI4_AR_Slave_Synth AXI4_AR_Target_Synth;
typedef AXI4_R_Master_Synth AXI4_R_Initiator_Synth;
typedef AXI4_R_Slave_Synth AXI4_R_Target_Synth;
typedef AXI4_Master AXI4_Initiator;
typedef AXI4_Master_Synth AXI4_Initiator_Synth;
typedef AXI4_Master_Xactor AXI4_Initiator_Xactor;
typedef AXI4_Slave AXI4_Target;
typedef AXI4_Slave_Synth AXI4_Target_Synth;
typedef AXI4_Slave_Xactor AXI4_Target_Xactor;
typedef AXI4_Slave_Width_Xactor AXI4_Target_Width_Xactor;

//From AXI4_Utils.bsv
import AXI4_Utils :: *;
import Monitored :: *;

interface AXI4_InitiatorTarget_Shim#(
  numeric type id_,
  numeric type addr_,
  numeric type data_,
  numeric type awuser_,
  numeric type wuser_,
  numeric type buser_,
  numeric type aruser_,
  numeric type ruser_);
  method Action clear;
  interface AXI4_Initiator#(
    id_, addr_, data_, awuser_, wuser_, buser_, aruser_, ruser_
  ) initiator;
  interface AXI4_Target#(
    id_, addr_, data_, awuser_, wuser_, buser_, aruser_, ruser_
  ) target;
endinterface

module monitorAXI4_Initiator #(AXI4_Initiator#(a, b, c, d, e, f, g, h) initiator)
                              (Monitored#(AXI4_Initiator#(a, b, c, d, e, f, g, h)
                                         , AXI4_Events));
  let ret <- monitorAXI4_Master(initiator);
  return ret;
endmodule

module monitorAXI4_Target #(AXI4_Target#(a, b, c, d, e, f, g, h) target)
                          (Monitored#( AXI4_Slave#(a, b, c, d, e, f, g, h)
                                     , AXI4_Events));
  let ret <- monitorAXI4_Slave(target);
endmodule
