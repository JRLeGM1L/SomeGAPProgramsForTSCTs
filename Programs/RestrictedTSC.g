RestrictedTSC := function(T2, tsc, T1)


local nameT2, LengthT1, fus
;


nameT2 := T2.name ;
LengthT1 := Length( T1.table ) ;


if Filtered( [ 1 .. Length( T1.fusions ) ], x -> T1.fusions[ x ].name=nameT2 ) = [ ] then
	Print( "I need the fusion map" ) ;
	return fail ;
else
	fus := T1.fusions[ Filtered( [ 1 .. Length( T1.fusions ) ], x -> T1.fusions[ x ].name = nameT2 )[ 1 ] ].map ;
fi ;


return tsc{ fus } ;

end ;
