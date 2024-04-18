`default_nettype none
module Decoder
#(parameter WIDTH = 8)
(input logic [$clog2(WIDTH)-1:0] I, 
input logic en,
output logic [WIDTH-1:0] D);
    
    always_comb begin
        if(en) 
            D = 1 << I;
        else 
            D = '0;
    end
endmodule: Decoder


module BarrelShifter
(input logic [15:0] V,
input logic [3:0] by,
output logic [15:0] S);
    
    assign S = V << by;
endmodule: BarrelShifter

module Multiplexer
#(parameter WIDTH = 8)
(input logic [WIDTH-1:0] I,
input logic [$clog2(WIDTH)-1:0] S,
output logic Y);
    
    assign Y = I[S];

endmodule: Multiplexer

module Mux2to1
#(parameter WIDTH = 8)
(input logic [WIDTH-1:0]I0,
input logic [WIDTH-1:0]I1,
input logic S,
output logic [WIDTH-1:0]Y);

    assign Y = S ? I1 : I0;

endmodule: Mux2to1

module MagComp
#(parameter WIDTH = 8)
(input logic [WIDTH-1:0] A,
input logic [WIDTH-1:0] B,
output logic AltB, AeqB, AgtB);

    assign AltB = A < B;
    assign AeqB = A == B;
    assign AgtB = A > B;

endmodule: MagComp

module Comparator
#(parameter WIDTH = 4)
(input logic [WIDTH-1:0] A,
input logic [WIDTH-1:0] B,
output logic AeqB);

    assign AeqB = A == B;
endmodule: Comparator

module Adder
#(parameter WIDTH = 8)
(input logic [WIDTH-1:0]A,
input logic [WIDTH-1:0]B,
input logic cin,
output logic [WIDTH-1:0]sum,
output logic cout);

    
    assign {cout, sum} = A + B + cin;

endmodule: Adder

module Subtracter
#(parameter WIDTH = 8)
(input logic [WIDTH-1:0]A,
input logic [WIDTH-1:0]B,
input logic bin,
output logic [WIDTH-1:0]diff,
output logic bout);

    
    assign {bout, diff} = A - B - bin;
endmodule: Subtracter

module DFlipFlop(
  output logic Q,
  input logic D, clock, reset_L, preset_L);

  always_ff @(posedge clock, negedge reset_L, negedge preset_L)
    if (reset_L == 1'b0)
      Q <= 0;
    else if (preset_L == 1'b0)
      Q <= 1;
    else
      Q <= D;
endmodule: DFlipFlop

module Register
#(parameter WIDTH = 8)
(
    input logic [WIDTH-1:0] D,
    input logic en,
    input logic clock,
    input logic clear,
    output logic [WIDTH-1:0] Q
);

    always_ff @(posedge clock) 
        if (en)
            Q <= D;
        else if (clear)
            Q <= 0;
    
        
endmodule: Register

module Counter
#(parameter WIDTH = 4)
(
    input logic clock,
    input logic clear,
    input logic en,
    input logic load, 
    input logic up,
    input logic [WIDTH-1:0] D,
    output logic [WIDTH-1:0] Q
);

    always_ff @(posedge clock) 
        if (clear)
            Q <= 0;
        else
            if (en)
                if (load)
                    Q <= D;
                else
                    if (up)
                        Q <= Q + 1;
                    else
                        Q <= Q - 1;


endmodule: Counter

module Synchronizer
(
    input logic async,
    input logic clock,
    output logic sync
);

    always_ff @(posedge clock)
        sync <= async;

endmodule: Synchronizer


module ShiftRegisterSIPO
#(parameter WIDTH = 8)
(
    input logic en,
    input logic left,
    input logic serial,
    input logic clock,
    output logic [WIDTH-1:0] Q
);

    always_ff @(posedge clock)
        if (en)
            if (left)
                Q <= {Q[WIDTH-2:0], serial};
            else
                Q <= {serial, Q[WIDTH-1:1]};

endmodule: ShiftRegisterSIPO

module ShiftRegisterPIPO
#(parameter WIDTH = 8)
(
    input logic en,
    input logic left,
    input logic load,
    input logic [WIDTH-1:0] D,
    input logic clock,
    output logic [WIDTH-1:0] Q
);

    always_ff @(posedge clock)
        if (en)
            if (left)
                Q <= {Q[WIDTH-2:0], load};
            else
                Q <= {load, Q[WIDTH-1:1]};

endmodule: ShiftRegisterPIPO

module BarrelShiftRegister
#(parameter WIDTH = 8)
(
    input logic en,
    input logic [1:0] by,
    input logic [WIDTH-1:0] D,
    input logic clock,
    output logic [WIDTH-1:0] Q
);

    always_ff @(posedge clock)
        if (en)
            Q <= D << by;

endmodule: BarrelShiftRegister

module BusDriver
#(parameter WIDTH = 8)
(input logic [WIDTH-1:0] data,
input logic [WIDTH-1:0] buff,
input logic en,
output logic [WIDTH-1:0] bus);


    assign bus = (en) ? data : buff;

endmodule: BusDriver

module Memory
#(parameter DW = 16,
  parameter W = 256,
  parameter AW = $clog2(W))
(input logic re, we, clock,
 input logic [AW-1:0] addr,
 inout tri [DW-1:0] data);

    logic [DW-1:0] M[W];

    logic [DW-1:0] rData;

    assign data = (re) ? rData : 'bz;

    always_ff @(posedge clock)
        if (we)
            M[addr] <= data;
        
    always_comb
        rData = M[addr];

    

endmodule: Memory