# This function computes the fusion map from the trivial source character table of G in characteristic p
# to the table of marks of G. The columns of the trivial source character table are fixed according 
# to the character tables of the quotient groups N/P.


FusToTom := function( ct, p )

local pSubgroupsAndNormalizers, pSubsAndNorms, t, entryOfFusionMapByLevel, fusToTom, i, auxFusToTom
;

# We use the function pSubgroupsAndNormalizers 
############################################################################################################

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

############################################################################################################

pSubsAndNorms := pSubgroupsAndNormalizers( ct, p ) ;
t := TableOfMarks( ct ) ;

############################################################################################################

entryOfFusionMapByLevel:=function(t, i, p)

local positionOfP, positionOfN, G, P, N, normalSubs, length, PinN, proj, factorGroup, cc, pReg, preImages, sub,
orders, positions, prob, position, j
;

# The first and the third entries on the output of pSubgroupsAndNormalizers are lists with the positions of 
# the representatives of p-subgroups of G and their normalisers. For this reason 1 and 3 are hard-coded.
positionOfP := pSubsAndNorms[ 1 ][ i ] ;
positionOfN := pSubsAndNorms[ 3 ][ i ] ;

# We construct G, P and N from ToM
G := RepresentativeTom( t, Length( MatTom( t ) ) ) ;
P := RepresentativeTom( t, positionOfP ) ;
N := RepresentativeTom( t, positionOfN ) ;

# We look for P inside of N
normalSubs := NormalSubgroups( N ) ;
length := Length( normalSubs ) ;
PinN := Filtered( [ 1..length ], x -> IsConjugate( G, P, normalSubs[ x ]) )[ 1 ] ;

# Construct the canonical projection N->N/P
proj := NaturalHomomorphismByNormalSubgroup( N, normalSubs[ PinN ] ) ;

# The factor group fac is N/P
factorGroup := Image( proj ) ;

# We look for the p-regular classes of N/P
cc := ConjugacyClasses( factorGroup ) ;
pReg := Filtered( cc, x -> Order( Representative( x ) ) mod p <> 0 ) ;


# We look at a list of representatives of the preimages of representatives of the p-regular classes of N/P
preImages := List( pReg, x -> PreImagesRepresentative( proj, Representative( x ) ) ) ;
# We take the subgroup of G generated by P and the element obtained in the last line.
sub := List( preImages, x -> Subgroup( G, Concatenation( GeneratorsOfGroup( normalSubs[ PinN ] ), [ x ] ) ) ) ;

orders := List( sub, Size ) ;

positions := [ ] ;

for j in [ 1..Length( orders ) ] do
    position := Positions( OrdersTom( t ), orders[ j ] ) ;
    prob := List( position, x -> RepresentativeTom( t, x ) ) ;
    Append( positions, [ position[ Position( List( prob, x -> IsConjugate( G, x, sub[ j ] ) ), true ) ] ] ) ;
od ;

return positions ;


end ;

############################################################################################################

fusToTom := [ ] ;
# We want the first columns of T at level 1 to be in the same order as the Brauer character table of G.
# Therefore we extract the first entries of the fusion map to ToM independently.
auxFusToTom := FusionCharTableTom( ct, t ) ;

for i in [1 ..Length( OrdersClassRepresentatives( ct ) ) ] do
    if OrdersClassRepresentatives( ct )[ i ]  mod p <>0 then 
    Add( fusToTom, auxFusToTom[ i ] ) ;
    fi ;
od ;

# Now we add the entries for the remaining entries
for i in [ 2..Length( pSubsAndNorms[ 1 ] ) ] do
    Append( fusToTom, entryOfFusionMapByLevel( t, i, p ) ) ;
od ;

return fusToTom ;


end ; 