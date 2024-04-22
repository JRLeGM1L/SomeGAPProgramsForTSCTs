##############################################################################################################################################
### This function computes the trivial source character of a trivial source module obtained by induction. The inputs for this function are ###
### T1= the tsct of a subgroup H of G, R=the row of T1 to be induced, T2=the tsct of G,fus=the fusion map from T1 to T2 ######################
##############################################################################################################################################

InductionOfSpecies := function( T1, number, T2 )

local i,  induced, p, cts, CTS, aux, weightsT1, weightsT2, weights, rowInNewT, info, Row, fus, nameT2
;

# We check that both tsct are in the same characteristic

if T1.char <> T2.char then
	Print( "The characteristics of both of the trivial source character tables must be equal.\n" ) ;
	return fail ;
fi ;

# Extract the character tables of Ni/Pi for T1 and T2

cts := T1.cts ;
CTS := T2.cts ;

p := T2.char ;

# Extract the weights that appear in the induction formula (See Chapter 3 of Dissertation)

weights := List( [ 1..Length ( T2.table[ 1 ] ) ], x -> [ ] ) ;
weightsT1 := T1.centWeights ;
weightsT2 := T2.centWeights ;

# Next we compute two lists, one with the division of the weights that appear in the induction formula and one with the entries of 
# the row that will be induced 

nameT2 := T2.name ;

if Filtered( [ 1..Length( T1.fusions ) ], x -> T1.fusions[ x ].name=nameT2) = [ ] then
	Print( "I need the fusion map" ) ;

else
	fus := T1.fusions[ Filtered( [ 1..Length ( T1.fusions ) ], x -> T1.fusions[ x ].name = nameT2 )[ 1 ] ].map ;
fi ;



for i in [ 1..Length( fus ) ] do
	Add( weights[ fus[ i ] ], 1/weightsT1[ i ] ) ;
od ;

for i in [ 1..Length( weights ) ] do
	if weights[ i ] <> [ ] then
		weights[ i ] := weightsT2[ i ]*weights[ i ] ;
	fi ;
od ;


Row := T1.table[ number ] ;

rowInNewT := List( [ 1..Length( T2.table[ 1 ] ) ], x -> [ ] ) ;
for i in [1..Length( fus ) ] do
	Add( rowInNewT[ fus[ i ] ], Row[ i ] ) ;
od ;

induced := [ ] ;

# Now do the point-wise product of the two list and sum the result according to the fusion map 

for i in [ 1..Length( rowInNewT ) ] do
	Append( induced, Tensored( [ weights[ i ] ], [ rowInNewT[ i ] ] ) ) ;
od ;

for i in [ 1..Length( induced ) ] do
	induced[ i ] := Sum( induced[ i ]) ;
od ;	


info := [ ] ;

return( induced ) ;

end;