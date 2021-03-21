
module exponent_MUX(input [7:0] exp_a,input [7:0] exp_b,input temp,output [7:0] exp_out);
	assign exp_out=(temp)?exp_b:exp_a;				//Assigning larger exponent to the exp_out variable
endmodule

module fraction_MUX(input [31:0] frac_a,input sign_a,input [31:0] frac_b,input sign_b,input temp,output [31:0] fracout, output signout);
	assign signout=(temp)?sign_b:sign_a;				//Assigning sign based on value of temp variable which determines which exponent is larger
	assign fracout=(temp)?frac_b:frac_a;				//Assigning fraction based on the value of temp variable which determines which exponent is larger
endmodule

module expALU(input [7:0] exp_a,input [7:0]exp_b,input temp,output [7:0]out);
	wire [7:0]d;
	assign d = exp_a-exp_b;						//subtracting the 2 exponents
	assign temp= d[7];						//Assiging temp 0 or 1 based on the calculated difference
	assign out=((d[7]==1)?(-1*d):d);				//Stores the Absolute difference of the 2 exponents
endmodule

module shiftFracRight(input [31:0] frac_a, input [7:0] exp_diff, output [31:0] shifted_frac);
	assign shifted_frac=frac_a>>exp_diff;				//Shifts the fraction to left by amount stored in exp_diff
endmodule


module addsubALU(input [31:0] frac_A, input [31:0] frac_B, input sign1, sign2, output reg signout, output reg [31:0] fraction_ans);
always @(*)
begin 
	if (sign1==sign2)						//Same sign = output sign same as both the numbers (overflow might be generated)
	begin
		fraction_ans=frac_A+frac_B;				//Adds the two fractions
		signout=sign1;						//Stores the final output sign
	end
	else 								//Different Signs 
	begin 
		if (frac_A>frac_B)					//Smaller number subtracted from larger number 
		begin 
			fraction_ans=frac_A-frac_A;
			signout=sign1;					//Sign of output = Sign of larger number
		end
		else 
		begin
			if (frac_A==frac_B)				//Sign different but fractions equal
			begin
				fraction_ans=32'b00000000100000000000000000000000;
				signout=0;
			end
			else
			begin						//Signs different, subtracting smaller number from larger number
				fraction_ans=frac_B-frac_A;
				signout=sign2;
			end
		end
	end
end
endmodule


module normalise(input [31:0] fraction, input [7:0] exponent, output reg [31:0] fraction_out, output reg [7:0] exponent_out);
wire [8:0]d;
always@(*)
begin
	fraction_out=fraction;	
	exponent_out=exponent;
	if(fraction_out == 32'b00000000100000000000000000000000)	//Checking for overflow in fractional value
	begin
		exponent_out=8'b00000000;				//Overflow = Assigning exponent boundary condition
	end
	while(fraction_out[31:23]!=1&&fraction_out!=0)			//No overflow and fraction non zero then we check for left or right shift
	begin
		if(fraction_out[31:23]>1)				//Overflow occurs = shift right
		begin
			fraction_out=fraction_out>>1;			//Increment exponent
			exponent_out=exponent_out+1;
			if(exponent_out==8'b11111111)			//Exponent out of bounds (infinite condition)
			begin
				fraction_out=32'b00000000100000000000000000000000;
			end
		end
		else
		begin
			fraction_out=fraction_out<<1;			//Shifting fraction left for subtraction cases
			exponent_out=exponent_out-1;			//decrementing exponent value to normalise output
		end
	end
end
endmodule




module floatingPointAdder( 	
	input [31:0] A, B,
	output [31:0] answer_out,output [31:0] fraction, output [7:0]  exponent, output [7:0] exponent_diff,output sign_out);
	
	wire [31:0] fraction_A;		//Stores the fractional part of A
	wire [31:0] fraction_B;		//Stores the fractional part of B
	wire [31:0] fraction_X;		//Stores the fractional part to be shifted
	wire [31:0] fraction_Y;		//Stores the fractional part after shifting left
	wire [31:0] fraction_Z;		//Stores the fractional part that need not be shifted	
	wire [7:0]  exponent_A;		//Stores the exponent part of A
	wire [7:0]  exponent_B;		//Stores the exponent part of B
	wire sign_A;			//Stores the sign of number not being shifted
	wire sign_B;			//Stores the sign of number being shifted
	wire sign_1;			//Stores the sign of A
	wire sign_2;			//Stores the sign of B
	
	wire temp;			//temporary variable
	reg [7:0]  exponent_X;		//temporary variable
	reg [7:0]  exponent_Y;		//temporary variable
	wire [31:0]frac_a;		//temporary variable
	wire [7:0] exp_a;		//temporary variable
	//Assigning the Values to the variables
        assign fraction_A = {8'b0,1'b1,A[22:0]}; 
        assign fraction_B = {8'b0,1'b1,B[22:0]};
        assign exponent_A = A[30:23];
        assign exponent_B = B[30:23];
        assign sign_1     = A[31];
        assign sign_2     = B[31]; 
	//Calling modules for addition
	expALU u1(exponent_A,exponent_B,temp,exponent_diff);						//difference of the two exponents is stored in exponent_diff
	exponent_MUX m1( exponent_A, exponent_B, temp, exp_a);						//the greater exponent is stored in exp_a
	fraction_MUX m2( fraction_B, sign_2, fraction_A, sign_1, temp, fraction_X, sign_A);		//the fraction to be shifted is stored in fraction_X
	fraction_MUX m3( fraction_A, sign_1, fraction_B, sign_2, temp, fraction_Z, sign_B);		//the other fraction is stored in fraction_Z
	shiftFracRight m4 (fraction_X, exponent_diff, fraction_Y);					//shifted fraction is stored in fraction_Y
	addsubALU m5(fraction_Z, fraction_Y, sign_B, sign_A , sign_out, frac_a); 			//the output of addition is stored in frac_a
	normalise m6(frac_a,exp_a,fraction,exponent);
	assign answer_out = {sign_out,exponent,fraction[22:0]};

endmodule



module testbenchfinal();
reg [31:0] A,B;
wire [31:0] fraction;
wire [7:0] exponent;
wire [7:0] exponent_diff;
wire [31:0] answer_out;
wire sign_out;
floatingPointAdder test(A,B,answer_out,fraction,exponent,exponent_diff,sign_out);
initial 
begin 
A<=32'b00000000110000000000000000000000;
B<=32'b10000000110000000000000000000000;
#20 A <= 32'b00000000110000000000000000000000; B <= 32'b00000000110000000000000000000000;
#20 A <= 32'b11000000111010000000000000000000; B <= 32'b01000000100001000000000000011000;
#20 A <= 32'b11000010110010000000000000000000; B <= 32'b01000010110111100000000000111000;
#20 A <= 32'b01100011011111111111111110000111; B <= 32'b01100111010000000000000001111001;		
end

endmodule
