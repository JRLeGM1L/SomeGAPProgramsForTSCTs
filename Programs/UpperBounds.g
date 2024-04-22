###############################################################################################################################################
### This function upper bounds for the number of times a row R if a tsct T can be contained in a trivial source character tsc.#################
###############################################################################################################################################

UpperBounds := function( T, R, tsc )

local MaximumPossibleMultiplicity, OrdChar, ct, mults, pims,p, OrdinaryChar, fusToOrdCharTable, upperBound
;


###############################################################################################################################################
	
#  MaximumPossibleMutliplicity returns the maximum number of times yje ordinary character of a trivial ####
# source character tsc could be contained in an ordinary character OrdChar

###############################################################################################################################################
MaximumPossibleMultiplicity := function( OrdChar, tsc )

local maxMultiplicity, i, numRemoval ;

maxMultiplicity := infinity ;
numRemoval :=0 ;

for i in [ 1..Length( OrdChar ) ] do
    if OrdChar[ i ] > 0 and tsc[ i ] > 0 then
        numRemoval := Int( tsc[ i ]/ OrdChar[ i ] ) ;
        if numRemoval < maxMultiplicity then
            maxMultiplicity := numRemoval ;
        fi ;
    fi ;
od ; 

return maxMultiplicity ;
end ;

###############################################################################################################################################

OrdinaryChar := function( T, R )

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
tsc := OrdinaryChar( T, tsc ) ;
ct := T.cts[ 1 ] ;
p := T.char ;

upperBound := MaximumPossibleMultiplicity( OrdChar, tsc ) ;

return( upperBound ) ;

end ;
