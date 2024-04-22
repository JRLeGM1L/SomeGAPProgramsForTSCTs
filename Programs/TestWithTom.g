###########################################################################################################################################
### This program checks if the trivial source characters of the permutation representations of a group G can be written as an N-linear ####
### combination of the rows of a potential trivial source character table. If the program returns an error, it indicates some entries of ##
### the table are incorrect. The inputs for the program are: t=the table of marks of G, T=the trivial source character table to be ########
### tested and f=the fusion map from t to T.###############################################################################################
###########################################################################################################################################


TestWithTom := function( tsct )

local y, list, decomp, coefs1, coefs2, prob1, prob2, lengthT, lengthY,lengthDecomp, t, f, ct, T
;

# First we check if the fusion map has the correct length 

f := tsct.fusions[ Filtered( [ 1..Length( tsct.fusions ) ], x -> tsct.fusions[ x ].name = "ToTom" )[ 1 ] ].map ;
T := tsct.table ;
lengthT := Length( T ) ;

if Length( f ) <> lengthT then
	Print( "The fusion map is incompatible.\n" ) ;
	return fail ;
fi ;

# Extract the appropriate rows from the table of marks 

ct := tsct.cts[ 1 ] ;
t := TableOfMarks( ct ) ;
y := MatTom( t ) ;
y := List( [ 1..Length( y ) ], x -> y[ x ]{ f } ) ;
lengthY := Length( y ) ;
coefs1 := [ ] ;
coefs2 := [ ] ;

# Attempt to decompose each row as an N-linear combination of the rows of the potential trivial source character table 

decomp := Concatenation( List( [ 1..lengthY ], x -> SolutionMat( T, y[ x ] ) ) ) ;
lengthDecomp := Length( decomp ) ;

# Find the rows that have problems, i.e. the rows that decompose coefficients that are not natural numbers 

coefs1 := List( [ 1..lengthDecomp ], x -> IsInt( decomp[ x ] ) ) ;

coefs2 := List( [ 1..lengthDecomp ], x -> SignInt( decomp[ x ] ) ) ;

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
