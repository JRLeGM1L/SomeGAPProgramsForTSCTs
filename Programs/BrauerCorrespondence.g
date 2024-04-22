BrauerCorrespondence := function( ct, ctn, p )

local induced, coefficients, constituents, blocksOfG, ordCharsInBlocks,
i, j, Bparts, BrauerCorrespondentPos, Bpart,
BlocksOfN, b, FinalList
;


BrauerCorrespondent := function( ct, ctn, char, p )

if IsPrime( p ) = false then
	Print( "The fourth entry must be a prime number.\n" ) ;
	  	return fail ;
	fi ;


if IsCharacterTable( ct ) = false then 
	Print("The first argument should be the character table of a group.\n");
	  	return fail ;
	fi ;


if IsCharacterTable( ctn ) = false then 
  	Print( "The second argument should be the character table of a group.\n" ) ;
	    return fail ;
	fi ;


if ( char in Irr( ctn ) ) <> true then 
	Print( "The third argument should be an ordinary irreducible character of ctn. \n" ) ;
	    return fail ;
	fi ;
	
blocksOfG := BlocksInfo( ct mod p ) ;

if GetFusionMap( ctn, ct ) = fail then
  	Print("Fusion Map is needed") ;
   		return fail ;
	fi ;
	
induced := InducedClassFunction( ctn, char, ct ) ;


Bpart := function( induced, B )
 	coefficients := List( B.ordchars, x->ScalarProduct(Irr(ct)[x],induced));
 	constituents := List( B.ordchars, x->Irr( ct )[ x ] ) ;
 	return ( Degree( coefficients*constituents ) ) ;
 	end ;



Bparts := [ ] ;
	for i in [ 1..Length( blocksOfG ) ] do
  		Add( Bparts, Bpart( induced, blocksOfG[ i ] ) ) ;
 	od ;


BrauerCorrespondentPos := [ ] ;
	for j in [ 1..Length( blocksOfG ) ] do
  		Add( BrauerCorrespondentPos, PValuation( Bparts[ j ], p ) = PValuation( Degree( induced ), p ) ) ;
	od ;
	

return( Position( BrauerCorrespondentPos, true ) ) ;
 	end ;


BlocksOfN := BlocksInfo( ctn mod p ) ; 
 	b := [ ] ;
	
for i in [ 1..Length( BlocksOfN ) ] do
  		Add( b,( BlocksOfN[ i ].ordchars )[ 1 ] ); 
 	od ;

FinalList := [ ] ;
for i in [ 1..Length( b ) ] do
	Add( FinalList, BrauerCorrespondent( ct, ctn, Irr( ctn )[ b[ i ] ], p ) ) ;
od ;

return( FinalList ) ; 
end ;
