pred show {}
run  show for 1

one sig c1_A
{}
{ not (some c1_A)
  no c1_A }
one sig c2_B
{ ref : one Int }
{ not (some c2_B)
  no c2_B
  all cl0 : c2_B.@ref | cl0 > 6 }
