#This function retrieves a fusion map 'name' stored in the record data structure of a trivial source character table T.

GetFusion := function( T, name )

local position, map
;

position := Filtered( [ 1..Length( T.fusions ) ], x-> T.fusions[ x ].name = name ) ; 
position := position[ 1 ] ;
map := T.fusions[ position ].map ;

return( map ) ;
end ;
