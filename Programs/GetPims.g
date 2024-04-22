GetPims := function( ct, p )


if ct mod p = fail then
    Print( "The Brauer character table is not in the database" ) ; 
    return fail ;
fi ;

return Irr(ct mod p)*TransposedMat( DecompositionMatrix( ct mod p ) )*DecompositionMatrix( ct mod p ) ; 
end ;