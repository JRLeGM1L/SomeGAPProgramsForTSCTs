#############################################################################################################################################
### This function removes rows of a trivial source character table that belong to a block of defect 0 and are contained in a trivial ########
### source character R ######################################################################################################################
#############################################################################################################################################

RemoveDef0 := function( T, R )

local blocks, def0, simplePims, ModChar, decomp, i, d, def0Rows, p, BrauerTable1
;

p := T.char ;
BrauerTable1 := T.cts[ 1 ] mod p ;
blocks := BlocksInfo( BrauerTable1 ) ;

# Extract the blocks of defect 0

def0 := Filtered( [ 1..Length( blocks ) ], x -> blocks[ x ].defect = 0 ) ;

# Get the Brauer characters in the blocks of defect 0 

simplePims := Concatenation( List( def0, x -> ( BlocksInfo( BrauerTable1 )[ x ].modchars ) ) ) ;

# Get the ordinary character of the row T  

ModChar := R{ [ 1..Length( Irr( BrauerTable1 ) ) ] } ;

d := DecompositionMatrix( BrauerTable1 ) ;

# Initialise a list that will contain the defect 0 rows of T contained in R 

def0Rows := [ ] ;

# Add to def0Rows the rows of defect 0 contained in T 

for i in [ 1..Length( Irr( BrauerTable1 ) ) ] do
	if Sum( TransposedMat( d )[ i ]) = 1 then
		Add( def0Rows, i ) ;
	fi ;
od ;

decomp := SolutionMat( Irr( BrauerTable1 ), ModChar ) ;

for i in [ 1..Length( def0Rows ) ] do
	R := R-Irr( BrauerTable1 )[ def0Rows[ i ] ]*decomp[ def0Rows[ i ] ] ;
od ;

return R ;
end ;