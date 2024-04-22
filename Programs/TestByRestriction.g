TestByRestriction := function(T2, T1)


local nameT2, fus, i, restrictedChars, j, coefs, test1, test2, prob1, prob2, LengthT1
;


nameT2 := T2.name ;
LengthT1 := Length( T1.table ) ;


if Filtered( [ 1 .. Length( T1.fusions ) ], x -> T1.fusions[ x ].name=nameT2 ) = [ ] then
	Print( "I need the fusion map" ) ;
	return fail ;
else
	fus := T1.fusions[ Filtered( [ 1 .. Length( T1.fusions ) ], x -> T1.fusions[ x ].name = nameT2 )[ 1 ] ].map ;
fi ;


restrictedChars := [ ] ;


for i in [ 1 .. Length( T2.table ) ] do
	Append( restrictedChars, [T2.table[ i ]{ fus }] ) ;
od ;


coefs := [ ] ;

for j in [ 1 .. Length( restrictedChars ) ] do
	Append( coefs, SolutionMat( T1.table, restrictedChars[ j ] ) ) ;
od ;

test1 := List( [ 1 .. Length( coefs ) ], x -> IsInt( coefs[ x ] ) ) ;
test2 := List( [ 1 .. Length( coefs ) ], x -> SignInt( coefs[ x ] ) ) ;


if Position( test1, false ) <> fail then
	prob1 := Int( Position( test1, false )/LengthT1 ) + 1 ;
	Print( "Some rows decompose with non-integer entries.\n" ) ;
	Print( "There is an error in row: \n" ) ;
	return prob1 ;
	return fail ;
fi ;
	
if Position(test2,-1) <> fail then
	prob2 := Int( Position( test2, -1 )/LengthT1 )+1 ;
	Print( "There is a problem with the coefficients. One row decomposes with negative entries.\n" ) ;
	Print( "There is an error in row: \n" ) ;
	return prob2 ;
	return fail ;
fi ;

# If everything else is ok then the tables passes the test.

if Position( test1, false ) = fail and Position( test2, -1 ) = fail then
	Print( "The table passes the test." ) ;
fi ;

end ;
