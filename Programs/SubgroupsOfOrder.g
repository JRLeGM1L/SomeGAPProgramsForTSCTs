#############################################################################################################################################
### This function returns the numbers in the table of marks of G and the structure description of representatives of conjugacy classes of ###
### groups with a certain order. The inputs are the table of marks of G and the order of the subgroups ######################################
#############################################################################################################################################

SubgroupsOfOrder := function( t, n )

local pos, structure
;

pos := Filtered( [ 1..Length( MatTom( t ) ) ], x -> OrdersTom( t )[ x ] = n ) ;

structure := List( pos, x -> StructureDescription( RepresentativeTom( t, x ) ) ) ;

return( [ pos, structure ] ) ;

end;