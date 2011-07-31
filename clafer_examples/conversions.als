pred show {}
run  show for 1

lone sig c1_BOOLEAN_OPTION
{}
{}
one sig c2_BOOLEAN_TO_INTEGER
{ ref : one Int }
{ (some c1_BOOLEAN_OPTION) => (all cl0 : this.@ref | cl0 = 1) else (all cl0 : this.@ref | cl0 = 0) }
