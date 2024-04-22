###############################################################################################################################################
### This function checks consistency of a "trivial source character table" by taking tensor products of all the rows, and attempting to #######
### express the results as an N-linear combiation of the entries of the input table. The program only requires one argument, ##################
###which is the table T to be tested ##########################################################################################################
###############################################################################################################################################

TestWithTensors := function( tsct )

local i, j, list, coefs, test1, test2, prob1, prob2, prob3, T, lengthT
;

T := tsct.table ;
list := [ ] ;
lengthT := Length( T ) ;

# Create a list in the tensor product of all the rows of T with themselves 

for i in [ 1..lengthT ] do
	for j in [ 1..lengthT ] do
		Append( list, Tensored( [ T[ i ] ], [ T[ j ] ] ) ) ;
	od;
od;
#list:=Concatenation(list);

# Create a list with all the entries, and look in this list for not non-negative integers or rows that could not be decomposed at all

coefs := Concatenation( List( [ 1..Length( list ) ], x -> SolutionMat( T, list[ x ] ) ) ) ;

# If a product of two rows can not be expressed as a linear combination of the rows of T then therer is a problem with the coefficients of T. 
# The program return the number row in the matrix with tensors that fail to decompose.

if Position( coefs, fail ) <> fail then
	prob1 := Int( Position( coefs, fail )/ lengthT )+1 ;
	prob1 := [ Int( prob1/lengthT )+1, prob1-Int( prob1/lengthT )*lengthT ] ;
	Print( "There is an error. The tensor product of the following two rows cannot be decomposed.\n " ) ;
	return prob1 ;
fi ;

# Now we test for those rows who decomposed but with coefficients not in N. 

test1 := List( [ 1..Length( coefs ) ], x -> IsInt( coefs[ x ] ) ) ;
test2 := List( [ 1..Length( coefs ) ], x -> SignInt( coefs[ x ] ) ) ;

if Position( test1, false ) <> fail then
	prob2 := Int( Position( test1, false )/lengthT )+1 ;
	prob2 :=[ Int( prob2/lengthT)+1, prob2-Int( prob2/lengthT )*lengthT ] ;
	Print( "There is an error. Tensoring the rows" ) ;
	Print( prob2 ) ;
	Print( "decomposes with non-integer coefficients:\n" ) ;

fi ;

if Position( test2, -1 ) <> fail then
	prob3 := Int( Position( test2, -1 )/lengthT )+1 ;
	prob3 :=[ Int( prob3/lengthT)+1, prob3-Int( prob3/lengthT )*lengthT ] ;	
	Print( "There is an error. Tensoring the rows" ) ;
	Print( prob3 ) ;
	Print( "decomposes with negative numbers:\n" ) ;
fi ;

# If at least one row decomposes as linear combination of the rows of T  but with coefficients that are not natural numbers, 
# the program returns the rows whose tensor product  has the problem. Otherwise the table passes the test 

if Position( coefs, fail ) = fail and Position( test1, fail ) = fail and Position( test2, -1) = fail then
	Print( "The table passes the test." ) ;
fi ;

end ;
