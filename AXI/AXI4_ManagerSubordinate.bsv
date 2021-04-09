// Copyright Marno van der Maas

//From AXI4_Types.bsv
import AXI4_Types :: *;

typedef AXI4_AW_Master_Synth AXI4_AW_Manager_Synth;
typedef AXI4_AW_Slave_Synth AXI4_AW_Subordinate_Synth;
typedef AXI4_W_Master_Synth AXI4_W_Manager_Synth;
typedef AXI4_W_Slave_Synth AXI4_W_Subordinate_Synth;
typedef AXI4_B_Master_Synth AXI4_B_Manager_Synth;
typedef AXI4_B_Slave_Synth AXI4_B_Subordinate_Synth;
typedef AXI4_AR_Master_Synth AXI4_AR_Manager_Synth;
typedef AXI4_AR_Slave_Synth AXI4_AR_Subordinate_Synth;
typedef AXI4_R_Master_Synth AXI4_R_Manager_Synth;
typedef AXI4_R_Slave_Synth AXI4_R_Subordinate_Synth;
typedef AXI4_Master AXI4_Manager;
typedef AXI4_Master_Synth AXI4_Manager_Synth;
typedef AXI4_Master_Xactor AXI4_Manager_Xactor;
typedef AXI4_Slave AXI4_Subordinate;
typedef AXI4_Slave_Synth AXI4_Subordinate_Synth;
typedef AXI4_Slave_Xactor AXI4_Subordinate_Xactor;
typedef AXI4_Slave_Width_Xactor AXI4_Subordinate_Width_Xactor;

//From AXI4_Utils.bsv
import AXI4_Utils :: *;
import Monitored :: *;

interface AXI4_ManagerSubordinate_Shim#(
  numeric type id_,
  numeric type addr_,
  numeric type data_,
  numeric type awuser_,
  numeric type wuser_,
  numeric type buser_,
  numeric type aruser_,
  numeric type ruser_);
  method Action clear;
  interface AXI4_Manager#(
    id_, addr_, data_, awuser_, wuser_, buser_, aruser_, ruser_
  ) manager;
  interface AXI4_Subordinate#(
    id_, addr_, data_, awuser_, wuser_, buser_, aruser_, ruser_
  ) subordinate;
endinterface

module monitorAXI4_Manager #(AXI4_Manager#(a, b, c, d, e, f, g, h) manager)
                              (Monitored#(AXI4_Manager#(a, b, c, d, e, f, g, h)
                                         , AXI4_Events));
  let ret <- monitorAXI4_Master(manager);
  return ret;
endmodule

module monitorAXI4_Subordinate #(AXI4_Subordinate#(a, b, c, d, e, f, g, h) subordinate)
                          (Monitored#( AXI4_Slave#(a, b, c, d, e, f, g, h)
                                     , AXI4_Events));
  let ret <- monitorAXI4_Slave(subordinate);
  return ret;
endmodule

`define defAXI4ManagerSubordinateShimFIFOF (name)\
module mkAXI4ManagerSubordinateShim``name (AXI4_ManagerSubordinate_Shim#(a, b, c, d, e, f, g, h));\
  let internal <- mkAXI4Shim``name;\
  method clear = internal.clear;\
  interface manager = internal.master;\
  interface subordinate = internal.slave;\
endmodule

`defAXI4ManagerSubordinateShimFIFOF(BypassFIFOF)
`defAXI4ManagerSubordinateShimFIFOF(BypassFF1)
`defAXI4ManagerSubordinateShimFIFOF(FF1)
`defAXI4ManagerSubordinateShimFIFOF(FF)
`defAXI4ManagerSubordinateShimFIFOF(SizedFIFOF4)
`defAXI4ManagerSubordinateShimFIFOF(SizedFIFOF32)
`defAXI4ManagerSubordinateShimFIFOF(UGSizedFIFOF32)
`defAXI4ManagerSubordinateShimFIFOF(UGSizedFIFOF4)

function AXI4_Manager#(a,b,c,d,e,f,g,h) zeroManagerUserFields (AXI4_Manager#(a,b,c,d_,e_,f_,g_,h_) m);
  return zeroMasterUserFields(m);
endfunction

function AXI4_Subordinate#(a,b,c,d,e,f,g,h) zeroSubordinateUserFields (AXI4_Subordinate#(a,b,c,d_,e_,f_,g_,h_) s);
  return zeroSlaveUserFields(s);
endfunction

module mkBurstToNoBurst_ManagerSubordinate (AXI4_ManagerSubordinate_Shim#(a, b, c, d, e, f, g, h));
  let internal <- mkBurstToNoBurst;
  interface manager = internal.master;
  interface subordinate = internal.slave;
endmodule
