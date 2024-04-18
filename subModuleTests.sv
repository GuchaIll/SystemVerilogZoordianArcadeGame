module CoinInput_test();

    logic clock, reset;
    logic [1:0] CoinValue;
    logic CoinInserted;
    logic StartGame;
    logic [4:0] NumRounds;
    logic roundReady;

    coinInput coinInput1
    (
        .CLOCK_50(clock), .reset(reset),
        .CoinValue(CoinValue), .CoinInserted(CoinInserted),
        .StartGame(StartGame),
        .NumRounds(NumRounds), .roundReady(roundReady)
    );
    
    initial begin
        clock = 0;
        
        reset = 1;
        reset <= 0;
        forever #5 clock = ~clock;
    end

    initial begin
        $monitor($time,, "CoinValue=%b, CoinInserted = %d, NumRounds = %d, roundReady = %b, totalCoin = %d", CoinValue, CoinInserted, 
        NumRounds, roundReady, coinInput1.totalCoinVal);

        CoinValue = 2'b00;
        CoinInserted = 1;
        StartGame = 0;
        @(posedge clock);
        CoinValue = 2'b01;
        CoinInserted = 1;
        StartGame = 0;
        @(posedge clock);
        CoinValue = 2'b10;
        CoinInserted = 1;
        StartGame = 0;
        @(posedge clock);
        CoinValue = 2'b11;
        CoinInserted = 1;
        StartGame = 0;
        @(posedge clock);
        CoinValue = 2'b01;
        CoinInserted = 0;
        StartGame = 1;
        @(posedge clock);
        StartGame = 1;
        @(posedge clock);
        @ (posedge clock);
        $finish;

    end
        

endmodule: CoinInput_test

module loadPatternTest();
    logic clock, reset;
    logic [1:0] ShapeLocation;
    logic [2:0] LoadShape;
    logic LoadShapeNow;
    logic [11:0] Pattern;
    logic StartGame;
    logic ready;

    loadPattern loadPattern1
    (
        .CLOCK_50(clock), .reset(reset),
        .ShapeLocation(ShapeLocation), .LoadShape(LoadShape),
        .LoadShapeNow(LoadShapeNow), .Pattern(Pattern),
        .StartGame(StartGame), .ready(ready)
    );

    initial begin
        clock = 0;
        reset = 1;
        reset <= 0;
        forever #5 clock = ~clock;
    end

    initial begin
        $monitor($time,, "ShapeLocation=%d, LoadShape = %b, LoadShapeNow = %b, Pattern = %b, StartGame = %b, ready = %b, state=%b, loadShape = %b, D1 = %b, D2 = %b, D3 = %b, D4 = %b", ShapeLocation, LoadShape, LoadShapeNow, Pattern, StartGame, ready,  loadPattern1.shapeD1, loadPattern1.cur, loadPattern1.D1, loadPattern1.D2, loadPattern1.D3, loadPattern1.D4);
        @(posedge clock);
        ShapeLocation = 2'b00;
        LoadShape = 3'b000;
        LoadShapeNow = 1;
        StartGame = 0;
        @(posedge clock);
        ShapeLocation = 2'b01;
        LoadShape = 3'b001;
        LoadShapeNow = 1;
        StartGame = 0;
        @(posedge clock);
        ShapeLocation = 2'b10;
        LoadShape = 3'b010;
        LoadShapeNow = 1;
        StartGame = 0;
        @(posedge clock);
        StartGame = 1;
        @(posedge clock);
        ShapeLocation = 2'b11;
        LoadShape = 3'b011;
        LoadShapeNow = 1;
        StartGame = 0;
        @(posedge clock);
        LoadShapeNow = 0;
        StartGame = 1;
        @(posedge clock);
        StartGame = 1;
        @(posedge clock);
        @ (posedge clock);
        @ (posedge clock);
        $finish;

    end


endmodule: loadPatternTest

module graderTest();
    logic clock, reset;
    logic [11:0] Guess, masterPattern;
    logic GradeIt, ready, StartGame;
    logic GuessReady;
    logic [3:0] Znarly, Zood;
    logic [3:0] RoundNumber;

    grader grader1
    (
        .Guess(Guess), .masterPattern(masterPattern),
        .GradeIt(GradeIt), .CLOCK_50(clock), .reset(reset),
        .ready(ready), .StartGame(StartGame),
        .GuessReady(GuessReady), .Znarly(Znarly), .Zood(Zood),
        .RoundNumber(RoundNumber)
    );

    initial begin
        clock = 0;
        reset = 1;
        reset <= 0;
        forever #5 clock = ~clock;
    end

    initial begin
        $monitor($time,, "Guess=%b, masterPattern = %b, GradeIt = %d, ready = %d, StartGame = %d, GuessReady = %d, Znarly = %d, Zood = %d, RoundNumber = %d, state = %s", Guess, masterPattern, GradeIt, ready, StartGame, GuessReady, Znarly, Zood, RoundNumber, grader1.cur);
        @(posedge clock);
        Guess = 12'b000000000000;
        masterPattern = 12'b000000000000;
        GradeIt = 0;
        ready = 0;
        StartGame = 0;
        @(posedge clock);
        Guess = 12'b000000000000;
        masterPattern = 12'b000000000000;
        GradeIt = 1;
        ready = 1;
        StartGame = 0;
        @(posedge clock);
        Guess = 12'b000000000000;
        masterPattern = 12'b000000000000;
        GradeIt = 1;
        ready = 1;
        StartGame = 0;
        @(posedge clock);
        Guess = 12'b000000000000;
        masterPattern = 12'b000000000000;
        GradeIt = 1;
        ready = 1;
        StartGame = 0;
        @(posedge clock);
        Guess = 12'b000000000000;
        masterPattern = 12'b000000000000;
        GradeIt = 1;
        ready = 1;
        StartGame = 0;
        @(posedge clock);
        Guess = 12'b000000000000;
        masterPattern = 12'b000000000000;
        GradeIt = 1;
        ready = 1;
        StartGame = 0;
        @(posedge clock);
        Guess = 12'b000000000000;
        masterPattern = 12'b000000000000;
        GradeIt = 1;
        ready = 1;
        StartGame = 1;
        @(posedge clock);
        Guess = 12'b000000000010;
        masterPattern = 12'b000000000000;
        GradeIt = 1;
        ready = 1;
        StartGame = 1;
        @(posedge clock);
        Guess = 12'b000000000000;
        @(posedge clock);
        Guess = 12'b000000001000;
        GradeIt = 1;
         @(posedge clock);
          @(posedge clock);
           @(posedge clock);
        $finish;
    end
endmodule: graderTest