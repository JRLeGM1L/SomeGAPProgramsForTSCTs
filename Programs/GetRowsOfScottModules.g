########################################################################
### This function returns the rows of a trivial source character table #
### that correspond to Scott modules. The program requires the fusion ##
### map from the ordinary character table to the TSCT in question ######
########################################################################


GetRowsOfScottModules := function ( tsct )

local OrdinaryChar, NumOfRows, i, Scotts 
;
########################################################################
OrdinaryChar := function( T, R )

local char, decomp, fusToOrdCharTable
;

# We extract the entries from the trivial source character table of the ordinary character of the trivial source RG-lattice with the fusion map

fusToOrdCharTable := T.fusions[ Filtered( [ 1..Length( T.fusions ) ], x-> T.fusions[ x ].name = "OrdCharTable" )[ 1 ] ].map ;
char := R{ fusToOrdCharTable } ;

# Then we decompose the resulting ordinary character into a sum of irreducible ordinary characters.
decomp:=SolutionMat( Irr(T.cts[ 1 ] ), char ) ;

# The result is a list with entry i in position j if the i-th irreducible ordinary character is a constituent with multiplicty j
# of the character in question 

return( decomp ) ;

end;
########################################################################

Scotts := [ ] ;
NumOfRows := Length( tsct.table ) ;

for i in [ 1 .. NumOfRows ] do 
    if OrdinaryChar( tsct, tsct.table[ i ] )[ 1 ] = 1 then 
        Add( Scotts, i ) ;
    fi ;
od ;

return Scotts ;

end ;


