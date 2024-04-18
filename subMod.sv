`default_nettype none
module coinInput
    (
    input logic CLOCK_50, reset,
    input logic [1:0] CoinValue, //coin value
    input logic CoinInserted, //affirm if the coin is inserted
    output logic [4:0] CoinValueOut
    //output logic play
    );
    
    logic MSelect, Renable;
    logic [4:0] coin; //coin value
    logic [4:0] totalCoinVal;
    logic [4:0] addedCoinVal;
    logic l4, eq4, g4;
    logic cout;
    logic [4:0] max;
    //get coin value to add 
    always_comb begin
        case (CoinValue)
            2'b00: CoinValueOut = 5'd0;
            2'b01: CoinValueOut = 5'd1;
            2'b10: CoinValueOut = 5'd3;
            2'b11: CoinValueOut = 5'd5;
            default: CoinValueOut = 5'd0;
        endcase
    end


    //choose 0 or coin to add
    Mux2to1 #(5) selectCoin (.I1(5'd0), .I0(CoinValueOut), .S(MSelect), .Y(coin));

    Adder #(5) addCoin (.A(totalCoinVal), .B(coin), .cin(5'd0), .sum(addedCoinVal), .cout(cout));

    //total coin value
    Register #(5) totalCoin (.D(addedCoinVal), .en(Renable), .clock(CLOCK_50), .clear(reset), .Q(totalCoinVal));

    assign max = totalCoinVal >= 5'd28;
    //check if can play
    //MagComp #(5) checkCoin (.A(totalCoinVal), .B(5'd4), .AltB(l4), .AeqB(eq4), .AgtB(g4));

    //assign play = eq4 || g4;

endmodule: coinInput

module CoinInputfsm
    (
        input logic CLOCK_50, reset,
        input logic CoinInserted,
        input logic max,
        output logic Renable, MSelect
    );

    enum logic {NotInput = 1'b0, Inputting = 1'b1} cur_state, n_state;

    always_comb begin
        case (cur_state)
            NotInput: begin
                n_state = CoinInserted ? Inputting : NotInput;
                Renable = CoinInserted ? 1 : 0;
                MSelect = CoinInserted ? 1 : 0; //when MSelect is high, sel none
            end
            Inputting: begin
                n_state = max ? NotInput : Inputting;
                Renable = CoinInserted ? 1 : 0;
                MSelect = CoinInserted ? 1 : 0;
            end
    endcase
    end

    always_ff @(posedge CLOCK_50, posedge reset)
        if (reset)
            cur_state <= NotInput;
        else
            cur_state <= n_state;

endmodule: CoinInputfsm


module beginPlay
    (
        input logic CLOCK_50, reset,
        //input logic play,
        input logic [4:0] TotalCoinValue,
        input logic StartGame,
        output logic [3:0] NumGames,
    );

    logic Sen, Cclear, Cen, Cload, Bclear;
    logic canPlay;
    logic [3:0] RoundsPlayable;
    
    //fix play and canPlay
   
    Counter #(4) numGamesCounter (.clock(CLOCK_50), .clear(Cclear), .en(Cen), .load(Cload), .up(1'b0), .D(RoundsPlayable), .Q(NumGames));
    
    //Mux2to1 #(4) selectRound (.I1(RoundNumber), .I0(RoundsPlayable), .S(select), .Y(currentRound));
    BarrelShiftRegister #(4) shiftRound (.by(2'b10), D(TotalCoinValue), .en(Sen), .clock(CLOCK_50), .clear(BClear), .Q(RoundsPlayable));
    MagComp #(4) compareRound (.A(NumGames), .B(4'd0), .AltB(), .AeqB(), .AgtB(canPlay));

endmodule: beginPlay

module beginPlayfsm
    (
        input logic CLOCK_50, reset,
        input logic play, canPlay,
        input logic StartGame,
        output logic Sen, Cclear, Cen, Cload, Bclear
    );

    enum logic {waitingPlay = 1'b0, playing = 1'b1} cur_state, n_state;

    always_comb begin
        case (cur_state)
            waitingPlay: begin
                n_state = (StartGame & canPlay)? playing : waitingPlay;
                Cclear = 0;
                Cen = 0;
                Cload = (StartGame & canPlay) ? 1 : 0;
                Bclear = 0;
                Sen = (StartGame & canPlay) ? 1 : 0;

              
            end
            playing: begin
                n_state = canPlay ? playing : waitingPlay;
                Sen = 0;
                Cload = 0;
                Cen = (canPlay & startgame) ? 1 : 0;
                Bclear = canPlay ? 0 : 1; //reset
                Cclear = canPlay ? 0 : 1; //reset



            end
    endcase
    end

    always_ff @(posedge CLOCK_50, posedge reset)
        if (reset)
            cur_state <= waitingPlay;
        else
            cur_state <= playing;

endmodule: beginPlayfsm

module loadPattern
    (
        input logic CLOCK_50, reset,
        input logic [1:0] ShapeLocation,
        input logic [2:0] LoadShape,
        input logic LoadShapeNow, 
        output logic [11:0] Pattern,
        output logic ready
    );

    //location equal
    logic loc1, loc2, loc3, loc4;
     

    logic [2:0] count;
    logic [3:0] shapeD1, shapeD2, shapeD3, shapeD4;
    logic preset_Digit1, preset_Digit2, preset_Digit3, preset_Digit4;
    logic clear;
    logic LoadShapeD1, LoadShapeD2, LoadShapeD3, LoadShapeD4;
    logic reset_L;
    
    //store shape by Digit
    
    Register #(3) D1 (.D(LoadShape), .en(LoadShapeD1), .clock(CLOCK_50), .clear(clear), .Q(shapeD1)); 
    Register #(3) D2 (.D(LoadShape), .en(LoadShapeD2), .clock(CLOCK_50), .clear(clear), .Q(shapeD2));
    Register #(3) D3 (.D(LoadShape), .en(LoadShapeD3), .clock(CLOCK_50), .clear(clear), .Q(shapeD3));
    Register #(3) D4 (.D(LoadShape), .en(LoadShapeD4), .clock(CLOCK_50), .clear(clear), .Q(shapeD4));

    assign Pattern = {shapeD1, shapeD2, shapeD3, shapeD4};
    Register #(1) clearCounter (.D(shape1Input), .en(1'b1), .clock(CLOCK_50), .clear(reset_L), .Q(D1));
    //check if digit is already set
    //DFlipFlop (.D(preset_Digit1), .clock(CLOCK_50), .reset_L(reset_L), .preset_L(1'b1), .Q(D1));
    //DFlipFlop (.D(preset_Digit2), .clock(teCLOCK_50), .reset_L(reset_L), .preset_L(1'b1), .Q(D2));
    //DFlipFlop (.D(preset_Digit3), .clock(CLOCK_50), .reset_L(reset_L), .preset_L(1'b1), .Q(D3));
    //DFlipFlop (.D(preset_Digit4), .clock(CLOCK_50), .reset_L(reset_L), .preset_L(1'b1), .Q(D4));

    //determine if all digits are set
    assign count = D1 + D2 + D3 + D4;
    Comparator #(3) l0 (.A(count), .B(3'd4), .AeqB(ready));

    //find location
    Comparator #(3) l1 (.A(ShapeLocation), .B(2'b00), .AeqB(loc1));
    Comparator #(3) l2 (.A(ShapeLocation), .B(2'b01), .AeqB(loc2));
    Comparator #(3) l3 (.A(ShapeLocation), .B(2'b10), .AeqB(loc3));
    Comparator #(3) l4 (.A(ShapeLocation), .B(2'b11), .AeqB(loc4));

    assign Loc1Load = ~D1 & loc1;
    assign Loc2Load = ~D2 & loc2;
    assign Loc3Load = ~D3 & loc3;
    assign Loc4Load = ~D4 & loc4;

    
endmodule: loadPattern


module loadPatternfsm
    (
        input logic CLOCK_50, reset,
        input logic LoadShapeNow,
        input logic Loc1Load, Loc2Load, Loc3Load, Loc4Load,
        input logic ready,
        output logic LoadShapeD1, LoadShapeD2, LoadShapeD3, LoadShapeD4,
        output logic reset_L,
        output logic preset_Digit1, preset_Digit1, preset_Digit1, preset_Digit1
    );
    enum logic {NotLoading = 1'b0, Loading = 1'b1} cur_state, n_state;

    always_comb begin
        case (cur_state)
            NotLoading: begin
               n_state = LoadShapeNow ? Loading : NotLoading;
                LoadShapeD1 = 0;
                LoadShapeD2 = 0;
                LoadShapeD3 = 0;
                LoadShapeD4 = 0;

                preset_Digit1 = 0;
                preset_Digit2 = 0;
                preset_Digit3 = 0;
                preset_Digit4 = 0;

                reset_L = 0;
            end
            Loading: begin
                n_state = ready ? NotLoading : Loading; //finished loading 
                LoadShapeD1 = LoadShapeNow & Loc1Load ? 1 : 0;
                LoadShapeD2 = LoadShapeNow & Loc2Load ? 1 : 0;
                LoadShapeD3 = LoadShapeNow & Loc3Load ? 1 : 0;
                LoadShapeD4 = LoadShapeNow & Loc4Load ? 1 : 0;

                preset_Digit1 = LoadShapeNow & Loc1Load ? 1 : 0;
                preset_Digit2 = LoadShapeNow & Loc2Load ? 1 : 0;
                preset_Digit3 = LoadShapeNow & Loc3Load ? 1 : 0;
                preset_Digit4 = LoadShapeNow & Loc4Load ? 1 : 0;
                reset_L = 1;

            end
    endcase
    end

    always_ff @(posedge CLOCK_50, posedge reset)
        if (reset)
            cur_state <= NotLoading;
        else
            cur_state <= Loading;





endmodule: loadPatternfsm

`default_nettype none

module grader
 
  (input logic [11:0] Guess, masterPattern,
  input logic GradeIt, CLOCK_50, reset,  
  output logic [3:0] Znarly, Zood);
 
  logic [2:0] zaT,zaC,zaO,zaD,zaI,zaZ;
  logic [2:0] zoT,zoC,zoO,zoD,zoI,zoZ;
  logic [2:0] lT,lC,lO,lD,lI,lZ;
  logic [2:0] zoTm,zoCm,zoOm,zoDm,zoIm,zoZm;
  logic [3:0] Znarlys;

 
  assign Znarlys[3] = (Guess[11:9] == masterPattern[11:9]);
  assign Znarlys[2] = (Guess[8:6] == masterPattern[8:6]);
  assign Znarlys[1] = (Guess[5:3] == masterPattern[5:3]);
  assign Znarlys[0] = (Guess[2:0] == masterPattern[2:0]);

  ZnarlysL T(Guess,masterPattern,3'b001,zaT),
          C(Guess,masterPattern,3'b010,zaC),
          O(Guess,masterPattern,3'b011,zaO),
          D(Guess,masterPattern,3'b100,zaD),
          I(Guess,masterPattern,3'b101,zaI),
          Z(Guess,masterPattern,3'b110,zaZ);

  ZoodsL  Tg(Guess,masterPattern,Znarlys,3'b001,zoT),
          Cg(Guess,masterPattern,Znarlys,3'b010,zoC),
          Og(Guess,masterPattern,Znarlys,3'b011,zoO),
          Dg(Guess,masterPattern,Znarlys,3'b100,zoD),
          Ig(Guess,masterPattern,Znarlys,3'b101,zoI),
          Zg(Guess,masterPattern,Znarlys,3'b110,zoZ);

  letters Tf(masterPattern,3'b001,lT),
          Cf(masterPattern,3'b010,lC),
          Of(masterPattern,3'b011,lO),
          Df(masterPattern,3'b100,lD),
          If(masterPattern,3'b101,lI),
          Zf(masterPattern,3'b110,lZ);
 
   assign zoTm = (zoT > lT - zaT) ? lT - zaT : zoT;
   assign zoCm = (zoC > lC - zaC) ? lC - zaC : zoC;
   assign zoOm = (zoO > lO - zaO) ? lO - zaO : zoO;
   assign zoDm = (zoD > lD - zaD) ? lD - zaD : zoD;
   assign zoIm = (zoI > lI - zaI) ? lI - zaI : zoI;
   assign zoZm = (zoZ > lZ - zaZ) ? lZ - zaZ : zoZ;

  enum logic {A = 1'b0, B = 1'b1}cur,next;

  logic [3:0] RoundNumber;
  logic Cclear, Cen, Cload;

  Counter #(4) RoundGamesCounter (.clock(CLOCK_50), .clear(Cclear), .en(Cen), 
  .load(Cload), .up(1'b1), .D(4'd1), .Q(RoundNumber));

  assign RoundDone = RoundNumber == 4'd8;

  always_comb begin
  case(cur)
    A: begin
      next = (GradeIt) ? B : A;
      Znarly = 4'b0000;
      Zood = 4'b0000;
      Cclear = 0;
      Cen = 0;
      Cload = (GradeIt) ? 1 : 0;
      end
    B: begin
      next = (RoundDone) ? A : B;
      Znarly = Znarlys[3] + Znarlys[2] + Znarlys[1] + Znarlys[0];
      Zood = zoTm + zoCm + zoOm + zoDm + zoIm + zoZm;
      Cclear = (RoundDone) ? 1 : 0;
      Cen = (GradeIt) ? 1 : 0;
      Cload = 0;

      end
    endcase
  end
  

  always_ff @(posedge CLOCK_50, posedge reset)
    if(reset)
     cur <= A;
    else
     cur <= next;

endmodule: grader
//comment
module ZnarlysL
  (input logic [11:0] Guess, masterPattern,
  input logic [2:0] letter,
  output logic [2:0] numZnarlys);
  logic [3:0]D;

  assign D[3] = (Guess[11:9] == masterPattern[11:9]) && (Guess[11:9] == letter[2:0]);
  assign D[2] = (Guess[8:6] == masterPattern[8:6]) && (Guess[8:6] == letter[2:0]);
  assign D[1] = (Guess[5:3] == masterPattern[5:3]) && (Guess[5:3] == letter[2:0]);
  assign D[0] = (Guess[2:0] == masterPattern[2:0])&& (Guess[2:0] == letter[2:0]);
  assign numZnarlys = D[3] + D[2] + D[1] + D[0];

endmodule: ZnarlysL

module ZoodsL
  (input logic [11:0] Guess, masterPattern,
  input logic [3:0] Znarlys,
  input logic [2:0] letter,
  output logic [2:0] numZoods);
  logic [3:0]Z;

   assign Z[3] =  (Guess[11:9] == letter[2:0] && ~Znarlys[3]) &&
                  ((Guess[11:9] == masterPattern[8:6] && ~Znarlys[2])|
                  (Guess[11:9] == masterPattern[5:3] && ~Znarlys[1]) |
                  (Guess[11:9] == masterPattern[2:0] && ~Znarlys[0]));
    assign Z[2] = (Guess[8:6] == letter[2:0] && ~Znarlys[2]) &&
                  ((Guess[8:6] == masterPattern[11:9] && ~Znarlys[3])|
                  (Guess[8:6] == masterPattern[5:3] && ~Znarlys[1]) |
                  (Guess[8:6] == masterPattern[2:0] && ~Znarlys[0]));
    assign Z[1] = (Guess[5:3] == letter[2:0]  && ~Znarlys[1]) &&
                  ((Guess[5:3] == masterPattern[11:9] && ~Znarlys[3])|
                  (Guess[5:3] == masterPattern[8:6] && ~Znarlys[2])  |
                  (Guess[5:3] == masterPattern[2:0] && ~Znarlys[0]));
    assign Z[0] = (Guess[2:0] == letter[2:0]  && ~Znarlys[0]) &&
                  ((Guess[2:0] == masterPattern[11:9] && ~Znarlys[3])|
                  (Guess[2:0] == masterPattern[8:6] && ~Znarlys[2])  |
                  (Guess[2:0] == masterPattern[5:3] && ~Znarlys[1]));
  assign numZoods = Z[3] + Z[2] + Z[1] + Z[0];

endmodule: ZoodsL

module letters
  (input logic [11:0] masterPattern,
  input logic [2:0] letter,
  output logic [2:0] numLetter);
  logic [3:0]D;

  assign D[3] = (letter[2:0] == masterPattern[11:9]);
  assign D[2] = (letter[2:0] == masterPattern[8:6]);
  assign D[1] = (letter[2:0] == masterPattern[5:3]);
  assign D[0] = (letter[2:0] == masterPattern[2:0]);
  assign numLetter = D[3] + D[2] + D[1] + D[0];

endmodule: letters


module graderTest;

  logic [11:0] masterPattern;
  logic [11:0] Guess;
  logic [3:0] Zood;
  logic [3:0] Znarly;
  logic GradeIt,CLOCK_50,reset;
   
  grader DUT(Guess,masterPattern,GradeIt,CLOCK_50,reset,Znarly,Zood);

  initial begin


  CLOCK_50 = 0;
  reset = 1;
  reset <= 0;
  forever #5 CLOCK_50 = ~CLOCK_50;
  end

  initial begin
  $monitor ($time,, "Zood=%d, Znarly=%d, state=%s,guess = %b, pat = %b",
  Zood,Znarly,DUT.cur.name,Guess,masterPattern);
  Guess <= 12'b001001001001; masterPattern <= 12'b001001001001; GradeIt <= 1'b0;
  @(posedge CLOCK_50); GradeIt <= 1'b1;
  @(posedge CLOCK_50); reset <= 0; Guess = 12'b001001001001; masterPattern <= 12'b001001001001;
  @(posedge CLOCK_50); Guess <= 12'b101101011101; masterPattern <= 12'b001001101011;
  @(posedge CLOCK_50); Guess <= 12'b101001001101; masterPattern <= 12'b001001001001;
  @(posedge CLOCK_50); Guess <= 12'b001011001001; masterPattern <= 12'b001001001001;
  @(posedge CLOCK_50); Guess <= 12'b001011010001; masterPattern <= 12'b001001101011;
  @(posedge CLOCK_50); Guess <= 12'b001001001001; masterPattern <= 12'b001001001001;
  $finish;
  end

endmodule: graderTest


module checkWin
    (
        input logic [2:0] Znarly,
        input logic [2:0] Zood,
        output logic win
    );
    
    Comparator #(3) compareZ (.A(Znarly), .B(3'b4), .AeqB(win));
endmodule: checkWin

