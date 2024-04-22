###############################################################################################################################################
### This function computes the maximum number of times the character of a PIM can be contained in a trivial source character. #################
### The inputs for the function are the trivial source character table in characteristic p of a group and the trivial source character ########
### The output is a list of two lists: the first list contains the numbers of the rows that are possibly contained in the row R and the #######
### second list contains the maximum number of times the corresponding pim could be contained in R ############################################
###############################################################################################################################################

PossiblePimsContained := function( T, R )

local CandidatesPimsContained, MaximumPossibleMultiplicity, OrdChar, ct, mults, pims,p, OrdinaryChar, fusToOrdCharTable
;

# We use two auxiliary function, the first one is CandidatesPimsContained. CandidatsPimsContained returns the number of the pims that 
# be contained in R, in the order that are stored in GAP.

###############################################################################################################################################

CandidatesPimsContained := function( T, R )

local p, ct, possiblePimsContained, OrdChar, constituents, candidates, constituentsPims, pims, candidatesRows, i
;

p := T.char ;

ct := T.cts[ 1 ] ;

pims := Irr( ct )*( DecompositionMatrix( ct mod p ) ) ;

fusToOrdCharTable := T.fusions[ Filtered( [ 1..Length( T.fusions ) ], x->T.fusions[ x ].name = "OrdCharTable" )[ 1 ] ].map ;

OrdChar := R{ fusToOrdCharTable } ;
constituents := SolutionMat( Irr( ct ), OrdChar ) ;

candidates := [ ] ;

constituentsPims := TransposedMat( DecompositionMatrix( ct mod p ) ) ;

for i in [ 1..Length( pims ) ] do
	if Minimum( SolutionMat( Irr( ct ), OrdChar - pims[ i ] ) ) >= 0 then
		Add( candidates, AsList( pims[ i ] ) ) ;
	fi ;
od ;

pims := List( [ 1..Length( pims ) ], x -> AsList( pims[ x ] ) ) ;

candidatesRows := List( [ 1..Length( candidates ) ], x -> Position( pims, candidates[ x ] ) ) ;

return( candidatesRows ); 
end ;
###############################################################################################################################################
	
# The second auxiliary function is MaximumPossibleMutliplicity. It returns the maximum number of times a specific pim could be contained in an
# ordinary character OrdChar

###############################################################################################################################################
MaximumPossibleMultiplicity := function( OrdChar, pim )

local maxMultiplicity, i, numRemoval ;

maxMultiplicity := infinity ;
numRemoval :=0 ;

for i in [ 1..Length( OrdChar ) ] do
    if OrdChar[ i ] > 0 and pim[ i ] > 0 then
        numRemoval := Int( OrdChar[ i ]/ pim[ i ] ) ;
        if numRemoval < maxMultiplicity then
            maxMultiplicity := numRemoval ;
        fi ;
    fi ;
od ; 

return maxMultiplicity ;
end ;

###############################################################################################################################################

OrdinaryChar:=function(T, R)

local char, decomp, fusToOrdCharTable
;

# We extract the entries from the trivial source character table of the ordinary character of the trivial source RG-lattice with the fusion map 

fusToOrdCharTable := T.fusions[ Filtered( [ 1..Length( T.fusions ) ], x -> T.fusions[ x ].name = "OrdCharTable" ) [ 1 ] ].map ;
char := R{ fusToOrdCharTable } ;

# Then we decompose the resulting ordinary character into a sum of irreducible ordinary characters

decomp := SolutionMat( Irr( T.cts[ 1 ] ), char ) ;

# The result is a list with entry i in position j if the i-th irreducible ordinary character is a constituent with multiplicty j of 
# the character in question 

return( decomp ) ;

end;

##########################################################################################################

OrdChar := OrdinaryChar( T, R ) ;
ct := T.cts[ 1 ] ;
p := T.char ;

pims := TransposedMat( DecompositionMatrix( ct mod p ) ) ;

mults := List( [1..Length( CandidatesPimsContained( T, R ) ) ], x -> MaximumPossibleMultiplicity( OrdChar, pims[ CandidatesPimsContained( T, R )[ x ] ] ) ) ;
	
return( [ CandidatesPimsContained( T, R ), mults ] ) ;
end ;