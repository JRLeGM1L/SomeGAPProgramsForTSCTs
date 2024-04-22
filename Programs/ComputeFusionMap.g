########################################################################################################
# This function computes the fusion map from a tsct in characteristic p of a subgroup of G to ##########
# the tsct in characteristic p of G. ###################################################################
########################################################################################################

ComputeFusionMap := function(tsct1, tsct2)

local entriesLevel1, p, ct1, ct2, fusionOfToms, pSubsT1, nametsct2, fusionOfPSubs, i, fusion, cts1, cts2,
FusionAtLeveli, t1, t2, pSubgroupsAndNormalizers
;

# Check first that the characteristic of the two tsct is the same
if tsct1.char <> tsct2.char then
    Print("The two trivial source character tables need to be in the same characteristic.\n" ) ;
    return fail;
fi;

p := tsct1.char ;
ct1 := tsct1.cts[ 1 ] ;
t1 := TableOfMarks( ct1 ) ;
ct2 :=tsct2.cts[ 1 ] ;
t2 := TableOfMarks( ct2 ) ;

#########################################################################################################

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


########################################################################################################

# This is an auxiliary function over which will we will loop

FusionAtLeveli := function( ct1, ct2 )

local fus
;

fus := GetFusionMap( ct1 mod p, ct2 mod p ) ;

return fus ;
 
 end ;

########################################################################################################

# Look at the fusion of the psubgroups from tsct1 to tsct2

nametsct2 := Identifier( ct2 ) ;

pSubsT1 := pSubgroupsAndNormalizers( ct1, p ) ;

# Recover the fusion map from ToM t1 to ToM t2

fusionOfToms := FusionsTom( t1 ) ;
fusionOfToms := Filtered( fusionOfToms, x -> x[ 1 ] = nametsct2 )[ 1 ][ 2 ] ;

# Retrive the information of the fusion of the p-subgroups
fusionOfPSubs := fusionOfToms{ pSubsT1[ 1 ] } ;
fusionOfPSubs := List( [ 1..Length( fusionOfPSubs ) ] ) ;


cts1 := tsct1.cts ;
cts2 := tsct2.cts ;

fusion := [ ] ;

for i in [ 1 .. Length( fusionOfPSubs ) ] do 
    Add(fusion, FusionAtLeveli( cts1[ i ], cts2[fusionOfPSubs[ i ] ] ) ) ;
od ;

return fusion ;

end ;
