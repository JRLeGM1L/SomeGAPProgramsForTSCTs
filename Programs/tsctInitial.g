tsctInitial := function( name, p )

local ct, t, OrderOfSylow, pSubgroupsAndNormalizers, pims, i, diagBlocks, Tinitial, ctFactorGroups,factorGroup,
pSubsAndNorms, NrBlocksOfT, partOfFusionToTom, cc, pRegcc, preimages, G, fusToTom, subs, sizeT, j, rowBlocks,
sizesLowTblocks, cents
;

ct := CharacterTable( name ) ;
t := TableOfMarks( ct ) ;
G := UnderlyingGroup( t ) ;
OrderOfSylow := p^PValuation( SizesCentralizers( ct )[ 1 ], p ) ;

##########################################################################################################

pSubgroupsAndNormalizers := function( t, n )

local  pSubgroups, pSubgroupDescription, Normalizers, NormalizerDescription, info
;

pSubgroups := Filtered( [ 1..Length( MatTom( t ) ) ], x -> IsInt( n/OrdersTom( t )[ x ] ) );
pSubgroupDescription := List( pSubgroups, x -> StructureDescription( RepresentativeTom( t, x ) ) ) ;
Normalizers := List( pSubgroups, x -> NormalizerTom( t , x ) ) ;
NormalizerDescription := List( Normalizers, x -> StructureDescription( RepresentativeTom( t , x ) ) ) ;
info := [ pSubgroups, pSubgroupDescription, Normalizers, NormalizerDescription ] ;
return info ;

end;

#############################################################################################################

factorGroup := function( pSubsAndNorms, t, i )

local positionOfN, positionOfP, N, normalSubs, length, sizes, G, P, PinN, fG, cc, pims
;

positionOfN := pSubsAndNorms[ 3 ][ i ] ;
positionOfP := pSubsAndNorms[ 1 ] [ i ] ;
N := RepresentativeTom( t, positionOfN ) ;
normalSubs := NormalSubgroups( N ) ;
length := Length( normalSubs ) ;
sizes := List( [ 1..length ], x -> Size( normalSubs[ x ] ) ) ;
G := RepresentativeTom( t, Length( MatTom( t ) ) ) ;
P := RepresentativeTom( t, positionOfP ) ;
PinN := Filtered( [ 1..length ], x -> IsConjugate( G, P, normalSubs[ x ] ) )[ 1 ] ;
fG := [ Image( NaturalHomomorphismByNormalSubgroup( N, normalSubs[ PinN ] ) ),
NaturalHomomorphismByNormalSubgroup( N, normalSubs[ PinN ] ), normalSubs[ PinN ] ] ;

return fG;

end;

################################################################################################################

pims := function( ct, p )

return( Irr( ct mod p )*TransposedMat( DecompositionMatrix( ct mod p ) )*DecompositionMatrix( ct mod p ) ) ;

end;

###################################################################################################################


Tinitial := rec( ) ;

pSubsAndNorms := pSubgroupsAndNormalizers( t, OrderOfSylow ) ;
NrBlocksOfT := Length( pSubsAndNorms[ 1 ] ) ;
diagBlocks := [ pims( ct, p ) ] ;
Append( diagBlocks, List( [ 2..NrBlocksOfT ], x -> pims( CharacterTable( factorGroup( pSubsAndNorms, t, x )[ 1 ] ),p ) ) ) ;

Tinitial.diagonalBlocks := diagBlocks ;

#####################################################################################################################

rowBlocks := [ ] ;

sizeT := Sum( diagBlocks, x -> Length( x ) ) ;
sizesLowTblocks := [ 0 ] ;

for i in [ 2..Length( diagBlocks ) ] do 
	Append( sizesLowTblocks, [ Length( diagBlocks[ i-1 ] )+sizesLowTblocks[ i-1 ] ] ) ;
od ;

for i in [ 1..NrBlocksOfT ] do
	for j in [ 1..Length( diagBlocks[ i ] ) ] do
		Append( rowBlocks, [ Concatenation( diagBlocks[ i ][ j ], ListWithIdenticalEntries( sizeT-( sizesLowTblocks[ i ]+Length( diagBlocks[ i ] ) ), 0 ) ) ] ) ;
	od ; 
od ;

for i in [ 1..NrBlocksOfT ] do
	for j in [ 1..Length( diagBlocks[ i ] ) ] do
		rowBlocks[ sizesLowTblocks[ i ]+j ] := Concatenation( ListWithIdenticalEntries( sizesLowTblocks[ i ], 0 ), rowBlocks[ sizesLowTblocks[ i ]+j ] ) ;
	od ; 
od ;

Tinitial.table := rowBlocks ;
Tinitial.cts := Concatenation( [ ct ], List( [ 2..NrBlocksOfT ], x -> CharacterTable( factorGroup( pSubsAndNorms, t, x )[ 1 ] ) ) ) ;
Tinitial.char := p ;
cents := Concatenation( List( [ 1..Length( Tinitial.cts ) ], x -> SizesCentralizers( Tinitial.cts[ x ] mod p ) ) ) ;
Tinitial.SizesCentralizers := cents ;
Tinitial.fusions := [ rec( name := "ToTom", map := [ ] ) ] ;
Tinitial.name := "tsct" ;

return Tinitial ;

end ;
