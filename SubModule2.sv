`default_nettype none
module coinInput
    (
    input logic CLOCK_50, reset,
    input logic [1:0] CoinValue, //coin value
    input logic CoinInserted, //affirm if the coin is inserted
    input logic StartGame,
    output logic [4:0] NumRounds,
    output logic roundReady

    //output logic play
    );
    
    logic MSelect, Renable;
    logic [4:0] coin; //coin value
    logic [4:0] totalCoinVal;
    logic [4:0] addedCoinVal;
    logic l4, eq4, g4;
    logic cout;
    logic [4:0] max;
    logic [4:0] subCoinVal;
    logic bout;
    logic [4:0] CoinValueOut;
    logic [4:0] modifiedCoinVal;
    logic ASSelect;
    logic play;
    logic Rclear;
    
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
    CoinInputfsm CoinInputfsm1
    (
        .CLOCK_50(CLOCK_50), .reset(reset),
        .CoinInserted(CoinInserted),
        .max(max), .play(play),
        .Renable(Renable), .MSelect(MSelect), .roundReady(roundReady),
        .Rclear(Rclear)
    );

    //choose 0 or coin to add
    Mux2to1 #(5) selectCoin1 (.I1(5'd0), .I0(totalCoinVal), .S(MSelect), .Y(coin));
    Mux2to1 #(5) selectCoin2 (.I0(addedCoinVal), .I1(subCoinVal), .S(ASSelect), .Y(modifiedCoinVal));
    Adder #(5) addCoin (.A(coin), .B(CoinValueOut), .cin(5'd0), .sum(addedCoinVal), .cout(cout));

    Subtracter #(5) subCoin (.A(totalCoinVal), .B(5'd4), .bin(1'b0), .diff(subCoinVal), .bout(bout));
    //total coin value
    Register #(5) totalCoin (.D(modifiedCoinVal), .en(Renable), .clock(CLOCK_50), .clear(Rclear), .Q(totalCoinVal));

    //BarrelShiftRegister #(4) shiftRound (.by(2'b10), D(TotalCoinValue), .en(Sen), .clock(CLOCK_50), .clear(BClear), .Q(RoundsPlayable));

    assign NumRounds = totalCoinVal >> 2;

    //check if can play
    MagComp #(5) checkCoin (.A(totalCoinVal), .B(5'd4), .AltB(l4), .AeqB(eq4), .AgtB(g4));

    assign play = eq4 | g4;
    assign ASSelect = play & StartGame & ~CoinInserted;

endmodule: coinInput

module CoinInputfsm
    (
        input logic CLOCK_50, reset,
        input logic CoinInserted,
        input logic max, play,
        output logic Renable, MSelect, roundReady, Rclear
    );

    enum logic [1:0] {NotInput = 2'b00, Inputting = 2'b01, Full = 2'b10} cur_state, n_state;

    always_comb begin
        case (cur_state)
            NotInput: begin
                n_state = CoinInserted ? Inputting : NotInput;
                Renable = CoinInserted ? 1 : 0;
                MSelect = CoinInserted ? 1 : 0;
                roundReady = 0;
                Rclear = 0;
               
            end
            Inputting: begin
                n_state = max ? Full : Inputting;
                Renable = CoinInserted ? 1 : 0;
                MSelect = CoinInserted ? 0 : 1;
                roundReady = 1;
                Rclear = 1;
            
            end
            Full: begin
                n_state = max ? Full : Inputting;
                Renable = 0;
                MSelect = 0;
                roundReady = 1;
                Rclear = 1;
                
            end


    endcase
    end

    always_ff @(posedge CLOCK_50, posedge reset)
        if (reset)
            cur_state <= NotInput;
        else
            cur_state <= n_state;

endmodule: CoinInputfsm



module loadPattern
    (
        input logic CLOCK_50, reset,
        input logic [1:0] ShapeLocation,
        input logic [2:0] LoadShape,
        input logic LoadShapeNow, 
        output logic [11:0] Pattern,
        input logic StartGame,
        output logic ready
    );

    //location equal
    logic loc1, loc2, loc3, loc4;
     
    logic Loc1Load, Loc2Load, Loc3Load, Loc4Load;
    logic [2:0] count;
    logic [2:0] shapeD1, shapeD2, shapeD3, shapeD4;
    logic D1, D2, D3, D4;
    logic clear;
    logic LoadShapeD1, LoadShapeD2, LoadShapeD3, LoadShapeD4;
    
    logic [1:0] cur;
    loadPatternfsm loadPatternfsm1
    (
        .CLOCK_50(CLOCK_50), 
        .LoadShapeNow(LoadShapeNow),
        .Loc1Load(Loc1Load), .Loc2Load(Loc2Load), .Loc3Load(Loc3Load), 
        .Loc4Load(Loc4Load),
        .ready(ready), .StartGame(StartGame),
        .LoadShapeD1(LoadShapeD1), .LoadShapeD2(LoadShapeD2), 
        .LoadShapeD3(LoadShapeD3), .LoadShapeD4(LoadShapeD4),
         .reset(reset), .clear(clear), .cur(cur)
       
    );
    //store shape by Digit
    Register #(3) D1R (.D(LoadShape), .en(LoadShapeD1), .clock(CLOCK_50), .clear(clear), .Q(shapeD1)); 
    Register #(3) D2R (.D(LoadShape), .en(LoadShapeD2), .clock(CLOCK_50), .clear(clear), .Q(shapeD2));
    Register #(3) D3R (.D(LoadShape), .en(LoadShapeD3), .clock(CLOCK_50), .clear(clear), .Q(shapeD3));
    Register #(3) D4R (.D(LoadShape), .en(LoadShapeD4), .clock(CLOCK_50), .clear(clear), .Q(shapeD4));

    Register #(1) D1B (.D(1'b1), .en(LoadShapeD1), .clock(CLOCK_50), .clear(clear), .Q(D1)); 
    Register #(1) D2B (.D(1'b1), .en(LoadShapeD2), .clock(CLOCK_50), .clear(clear), .Q(D2));
    Register #(1) D3B (.D(1'b1), .en(LoadShapeD3), .clock(CLOCK_50), .clear(clear), .Q(D3));
    Register #(1) D4B (.D(1'b1), .en(LoadShapeD4), .clock(CLOCK_50), .clear(clear), .Q(D4));

    assign Pattern = {shapeD1, shapeD2, shapeD3, shapeD4};
    
    //check if digit is already set
    

    //determine if all digits are set
    assign count = D1 + D2 + D3 + D4;
    Comparator #(3) l0 (.A(count), .B(3'd4), .AeqB(ready));

    //find location
    Comparator #(2) l1 (.A(ShapeLocation), .B(2'b00), .AeqB(loc1));
    Comparator #(2) l2 (.A(ShapeLocation), .B(2'b01), .AeqB(loc2));
    Comparator #(2) l3 (.A(ShapeLocation), .B(2'b10), .AeqB(loc3));
    Comparator #(2) l4 (.A(ShapeLocation), .B(2'b11), .AeqB(loc4));

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
        input logic ready, StartGame,
        output logic LoadShapeD1, LoadShapeD2, LoadShapeD3, LoadShapeD4,
        output logic clear,
        output logic [1:0] cur
       
    );
    enum logic [1:0] {NotLoading = 2'b00, Loading = 2'b01, PatternReady = 2'b10} cur_state, n_state;
    assign cur = cur_state;
    always_comb begin
        case (cur_state)
            NotLoading: begin
               n_state = Loading;
                LoadShapeD1 = 0;
                LoadShapeD2 = 0;
                LoadShapeD3 = 0;
                LoadShapeD4 = 0;
 
                clear = 1;
            end
            Loading: begin
                n_state = ready ? PatternReady : Loading; //finished loading 
                LoadShapeD1 = LoadShapeNow & Loc1Load ? 1 : 0;
                LoadShapeD2 = LoadShapeNow & Loc2Load ? 1 : 0;
                LoadShapeD3 = LoadShapeNow & Loc3Load ? 1 : 0;
                LoadShapeD4 = LoadShapeNow & Loc4Load ? 1 : 0;

              
                
                clear = 0;
            end
            PatternReady: begin
                n_state = StartGame ? NotLoading : PatternReady;
                LoadShapeD1 = 0;
                LoadShapeD2 = 0;
                LoadShapeD3 = 0;
                LoadShapeD4 = 0;

                
               
                clear = 0;
            
            end
    endcase
    end

    always_ff @(posedge CLOCK_50, posedge reset)
        if (reset)
            cur_state <= NotLoading;
        else
            cur_state <= n_state;





endmodule: loadPatternfsm



module grader
 
  (input logic [11:0] Guess, masterPattern,
  input logic GradeIt, CLOCK_50, reset,  
  input logic ready,
  input logic StartGame,
  output logic GuessReady,
  output logic [3:0] Znarly, Zood,
  output logic [3:0] RoundNumber);
 
  logic [2:0] zaT,zaC,zaO,zaD,zaI,zaZ;
  logic [2:0] zoT,zoC,zoO,zoD,zoI,zoZ;
  logic [2:0] lT,lC,lO,lD,lI,lZ;
  logic [2:0] zoTm,zoCm,zoOm,zoDm,zoIm,zoZm;
  logic [3:0] Znarlys;
  logic RoundDone;

 
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

  enum logic [1:0] {A = 2'b00, B = 2'b01, CA = 2'b10} cur, next;

  
  logic Cclear, Cen, Cload;

  Counter #(4) RoundGamesCounter (.clock(CLOCK_50), .clear(Cclear), .en(Cen), 
  .load(Cload), .up(1'b1), .D(4'd1), .Q(RoundNumber));


  assign RoundDone = RoundNumber == 4'd7;

  always_comb begin
  case(cur)
    A: begin
      next = (ready & GradeIt) ? B : A;
      GuessReady = 0;
      Znarly = 4'b0000;
      Zood = 4'b0000;
      Cclear = (~ready | ~GradeIt) ? 1 : 0;
      Cen = 0;
      Cload = (ready & GradeIt) ? 1 : 0;
      end
    B: begin
      next = (RoundDone) ? CA : B;
      Znarly = Znarlys[3] + Znarlys[2] + Znarlys[1] + Znarlys[0];
      Zood = zoTm + zoCm + zoOm + zoDm + zoIm + zoZm;
      GuessReady = (GradeIt & ~RoundDone) ? 1 : 0;
      Cclear = 0;
      Cen = (GradeIt) ? 1 : 0;
      Cload = 0;
    end
    CA: begin
      next = (StartGame) ? A : CA;
      Znarly = Znarlys[3] + Znarlys[2] + Znarlys[1] + Znarlys[0];
      Zood = zoTm + zoCm + zoOm + zoDm + zoIm + zoZm;
      GuessReady = 0;
      Cclear = (RoundDone) ? 1 : 0;
      Cen = 0;
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




