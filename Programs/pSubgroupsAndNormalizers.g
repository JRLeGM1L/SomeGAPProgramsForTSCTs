##############################################################################################################################################
### We require the followig information to start computing the trivial source character table of a group G in characteristic p: ##############
### The number of conjugacy classes of p-subgroups of G, the normalizers of those p-subgroups. This function return that information and #####
### the identifier of the corresponsing group in the table of marks of G. The function requires two inputs: ct which is the character table ##
### of G and n, which is the order of a Sylow p-subgroup of G ################################################################################
##############################################################################################################################################

pSubgroupsAndNormalizers := function( ct, p )

local t, pSubgroups, pSubgroupDescription, Normalizers, NormalizerDescription, info, n
;

if IsPrime( p ) = false then
    Print( "The second entry must be a prime number.\n " ) ;
    return fail;
fi ;

t := TableOfMarks( ct ) ;
n := p^PValuation( SizesCentralizers( ct )[ 1 ], p ) ;


pSubgroups := Filtered( [ 1..Length( MatTom( t ) ) ], x -> IsInt( n/OrdersTom( t )[ x ] ) )  ;

pSubgroupDescription := List( pSubgroups, x -> StructureDescription( RepresentativeTom( t, x ) ) );

Normalizers := List( pSubgroups, x -> NormalizerTom( t, x ) ) ;

NormalizerDescription := List( Normalizers, x -> StructureDescription( RepresentativeTom( t , x ) ) ) ;

info := [ pSubgroups, pSubgroupDescription, Normalizers, NormalizerDescription ] ;

return info ;

end ;