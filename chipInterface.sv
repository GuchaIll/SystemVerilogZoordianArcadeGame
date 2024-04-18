`default_nettype none
module chipInterface(
    input logic [17:0] SW,
    input logic [3:0] KEY,
    input logic CLOCK_50,
    output logic [6:0] HEX0,
    output logic [6:0] HEX1,
    output logic [6:0] HEX2,
    output logic [6:0] HEX3,
    output logic [7:0] LEDG,
    output logic [7:0] VGA_R,
    output logic [7:0]VGA_G,
    output logic [7:0]VGA_B,
    output logic VGA_BLANK_N,
    output logic VGA_CLK,
    output logic VGA_SYNC_N,
    output logic VGA_VS,
    output logic VGA_HS
    

);
logic [1:0] CoinValue;
logic CoinInserted;
logic StartGame;
logic [11:0] Guess;
logic GradeIt;
logic [2:0] LoadShape;
logic [1:0] ShapeLocation;
logic LoadShapeNow;
logic [3:0] Znarly;
logic [3:0] Zood;
logic [3:0] RoundNumber;
logic [3:0] NumGames;
logic GameWon;
logic reset;
logic clock;
logic [4:0] CoinValueOut;
logic [11:0] Pattern;
logic ready;
logic roundReady;
logic GuessReady;



assign CoinValue = SW[17:16];
assign Guess = SW[11:0];

assign LoadShape = SW[2:0];
assign ShapeLocation = SW[4:3];


assign clock = CLOCK_50;

 
logic key3high;
assign GradeIt = key3high;
assign LoadShapeNow = key3high;

keyPressedfsm CoinI (.buttonPressed(~KEY[1]), .buttonRelease(KEY[1]), .val(CoinInserted), .clock(CLOCK_50), .reset(reset));
keyPressedfsm CoinSg (.buttonPressed(~KEY[2]), .buttonRelease(KEY[2]), .val(StartGame), .clock(CLOCK_50), .reset(reset));
keyPressedfsm CoinGI (.buttonPressed(~KEY[3]), .buttonRelease(KEY[3]), .val(key3high), .clock(CLOCK_50), .reset(reset));
keyPressedfsm CoinReset (.buttonPressed(~KEY[0]), .buttonRelease(KEY[0]), .val(reset), .clock(CLOCK_50), .reset(reset));

/*Here is the logic for connecting the chipInterface to submodules
and also the VGA*/

coinInput coinInput1
    (
   .CLOCK_50(CLOCK_50), .reset(reset),
   .CoinValue(CoinValue), //coin value
    .CoinInserted(CoinInserted),
    .StartGame(StartGame), //affirm if the coin is inserted
    .NumRounds(NumGames),
    .roundReady(roundReady) //number of rounds
   
    );

loadPattern loadPattern1
    (
        .CLOCK_50(CLOCK_50), .reset(reset),
        .ShapeLocation(ShapeLocation),
        .LoadShape(LoadShape),
        .LoadShapeNow(LoadShapeNow), 
        .Pattern(Pattern),
         .StartGame(StartGame),
        .ready(ready)
    );
grader grader1 (.Guess(Guess), .masterPattern(Pattern),
  .GradeIt(GradeIt), .CLOCK_50(CLOCK_50), .reset(reset),  
  .Znarly(Znarly), .Zood(Zood), .RoundNumber(RoundNumber),
  .GuessReady(GuessReady)
  );

assign GameWon = Znarly == 3'd4;

mastermindVGA VGA(
.CLOCK_50(CLOCK_50),
// VGA display signals -- route directly to FPGA pins
.VGA_R(VGA_R), .VGA_G(VGA_G), .VGA_B(VGA_B),
.VGA_BLANK_N(VGA_BLANK_N), .VGA_CLK(VGA_CLK), .VGA_SYNC_N(VGA_SYNC_N),
.VGA_VS(VGA_VS), .VGA_HS(VGA_HS),
// game information
.numGames(NumGames),
.loadNumGames(roundReady), //roundReady
// Items for a particular round
.roundNumber(RoundNumber),
.guess(Guess),
.loadGuess(GuessReady),//GuessReady
.znarly(Znarly), .zood(Zood),
.loadZnarlyZood(GuessReady),//GuessReady
.clearGame(StartGame),
// master patterns
.masterPattern(Pattern),
.displayMasterPattern(SW[15]),
// other
.reset(reset)
);

//HEX0
BCDtoSevenSegment BCDtoSevenSegment0(.bcd(NumGames), .segment(HEX0));
BCDtoSevenSegment BCDtoSevenSegment1(.bcd(RoundNumber), .segment(HEX1));
BCDtoSevenSegment BCDtoSevenSegment2(.bcd(Zood), .segment(HEX2));
BCDtoSevenSegment BCDtoSevenSegment3(.bcd(Znarly), .segment(HEX3));
assign LEDG = (GameWon) ? 8'b1111_1111 : 8'b0000_0000;


endmodule

module BCDtoSevenSegment
    (input logic [3:0] bcd,
    output logic [6:0] segment);

    always_comb begin
        case(bcd)
            4'b0000: segment = ~7'b0111111;
            4'b0001: segment = ~7'b0000110;
            4'b0010: segment = ~7'b1011011;
            4'b0011: segment = ~7'b1001111;
            4'b0100: segment = ~7'b1100110;
            4'b0101: segment = ~7'b1101101;
            4'b0110: segment = ~7'b1111101;
            4'b0111: segment = ~7'b0000111;
            4'b1000: segment = ~7'b1111111;
            4'b1001: segment = ~7'b1101111;
            4'b1010: segment = ~7'b1110111;
            4'b1011: segment = ~7'b1111100;
            4'b1100: segment = ~7'b1011000;
            4'b1101: segment = ~7'b1011110;
            4'b1110: segment = ~7'b1111001;
            4'b1111: segment = ~7'b1110001;
            default: segment = ~7'b1111111;
        endcase
    end
endmodule: BCDtoSevenSegment

module keyPressedfsm
(input logic buttonPressed,
 input logic buttonRelease,
 input logic clock, reset,
 output logic val);
	enum logic {noPress = 1'b0, Press = 1'b1}ns, cs;
	
	always_comb
		unique case (cs)
			noPress: begin
				ns = buttonPressed ? Press : noPress;
				val = 0;
			end
			Press: begin
				ns = buttonRelease ? noPress : Press;
				val = buttonRelease ? 1 : 0;
			end
		endcase
	always_ff @(posedge clock, posedge reset)
		if(reset) cs <= noPress;
		else cs <= ns;
		
endmodule: keyPressedfsm