BlueStuff
=========

**BlueStuff** is a [Bluespec SystemVerilog](http://wiki.bluespec.com/bluespec-systemverilog-and-compiler) library of miscellaneous components. It currently provides a set of utility components in the [BlueUtils](BlueUtils) directory, and an implementation of the AXI interface in the [AXI](AXI) directory.

BlueStuff is currently used by [BID](https://github.com/CTSRD-CHERI/BID.git) and [RVBS](https://github.com/CTSRD-CHERI/RVBS.git).

Jobs TODO:

- There are two different conventions for modules implementing transformations, e.g. buses and transactors:
  1. The inputs and outputs are exposed as ports, likely requiring a mkConnection on both sides of the module
  2. The inputs and outputs are provided as parameters, allowing direct passing of the argument master/slaves into the module.
  Both conventions should be able to express the same behaviour, but it may be more convenient to use one or the other. It would be nice to add this option into BlueStuff, e.g. by adding a ...Connection version of each module and implementing the module which exposes the ports as a wrapper around this.
