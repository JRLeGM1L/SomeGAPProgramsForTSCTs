###############################################################################################################################################
### This function returns the ordinary character of a trivial source RG-lattice with trivial source character R. The two inputs are the #######
### trivial source character table of G in characteristic pand the trivial source character ###################################################
###############################################################################################################################################

OrdinaryChar := function( T, R )

local char, decomp, fusToOrdCharTable
;

# We extract the entries from the trivial source character table of the ordinary character of the trivial source RG-lattice with the fusion map

fusToOrdCharTable := T.fusions[ Filtered( [ 1..Length( T.fusions ) ], x-> T.fusions[ x ].name = "OrdCharTable" )[ 1 ] ].map ;
char := R{ fusToOrdCharTable } ;

# Then we decompose the resulting ordinary character into a sum of irreducible ordinary characters.
decomp:=SolutionMat(Irr(T.cts[1]),char);

# The result is a list with entry i in position j if the i-th irreducible ordinary character is a constituent with multiplicty j
# of the character in question 

return( decomp ) ;

end;
