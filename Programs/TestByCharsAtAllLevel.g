TestByCharsAtAllLevel := function( tsct )

local lengths, lengthOfBlocks, numOfBlocks, coefficients, i, j, numOfTSChars, lengthDecomp,
coefs1, coefs2, prob1, prob2, lengthT, p
;

p := tsct.char ; 

lengths := List( tsct.cts, x -> Length( Irr( x mod p ) ) ) ;
numOfBlocks := Length( lengths ) ;
lengthOfBlocks := [ Length( Irr( tsct.cts[ 1 ] mod p  ) ) ] ;
for i in [ 2 .. numOfBlocks ] do
	Add(lengthOfBlocks, lengths[ i ] + lengthOfBlocks[ i - 1 ] );
od ;



coefficients := [ ] ;
numOfTSChars := Length( tsct.table ) ;

for i in [ 1 .. numOfTSChars ] do 
	for j in [ 2 .. Length( lengthOfBlocks ) ] do
		Add( coefficients, SolutionMat( Irr( tsct.cts[ j ] mod p ), tsct.table[ i ]{ [ (lengthOfBlocks[ j -1 ] + 1) .. lengthOfBlocks[ j ] ] } ) ) ;
	od ;
od ;


coefs1 := [ ] ;
coefs2 := [ ] ;

# Attempt to decompose each row as an N-linear combination of the rows of the potential trivial source character table 

coefficients := Concatenation( coefficients ) ;

lengthDecomp := Length( coefficients ) ;

# Find the rows that have problems, i.e. the rows that decompose coefficients that are not natural numbers 

coefs1 := List( [ 1 .. lengthDecomp ], x -> IsInt( coefficients[ x ] ) ) ;

coefs2 := List( [ 1 .. lengthDecomp ], x -> SignInt( coefficients[ x ] ) ) ;

lengthT := Length( tsct.table ) ;

if Position( coefs1, false ) <> fail then
	prob1 := Int( Position( coefs1, false )/lengthT ) + 1 ;
	Print( "Some rows decompose with non-integer entries.\n" ) ;
	Print( "There is an error in row: \n" ) ;
	return prob1 ;
	return fail ;
fi ;
	
if Position(coefs2,-1) <> fail then
	prob2 := Int( Position( coefs2, -1 )/lengthT )+1 ;
	Print( "There is a problem with the coefficients. One row decomposes with negative entries.\n" ) ;
	Print( "There is an error in row: \n" ) ;
	return prob2 ;
	return fail ;
fi ;

# If everything else is ok then the tables passes the test.

if Position( coefs1, false ) = fail and Position( coefs2, -1 ) = fail then
	Print( "The table passes the test." ) ;
fi ;


end ;
