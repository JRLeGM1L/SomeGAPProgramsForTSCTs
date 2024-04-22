#######################################################################################
### This program computes the fusion from the ordinary character table of a group G ###
### to the TSCT of G in characteristic p. #############################################
#######################################################################################

FusToOrdCharTable := function ( tsct )

local ct, t, p, ToTom, FusCharTableTom, Fusion, PreFusion, i
;


ct := tsct.cts[ 1 ] ;
t := TableOfMarks( ct ) ;
p := tsct.char ;


ToTom := GetFusion( tsct, "ToTom" ) ;
FusCharTableTom := FusionCharTableTom( ct, t ) ;



Fusion := [ ] ;
PreFusion := List( FusCharTableTom, x->Positions( ToTom, x ) ) ;

for i in [ 1 .. Length( PreFusion ) ] do
    if Length( PreFusion[ i ] ) = 1 then
        Add( Fusion, PreFusion[ i ][ 1 ] ) ;
    else
        if (PreFusion[ i ] in Fusion) = false then
            Add( Fusion, PreFusion[ i ] ) ;
        fi ;
    fi ;
od ;

Fusion := Flat( Fusion ) ;


return( Fusion ) ;

end ;
