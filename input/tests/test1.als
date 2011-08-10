pred show {}
run  show for 1

abstract sig c1_AA_ABSTRACT
{}
{ (#(c1_AA_ABSTRACT) = 0) => (no (c4_CYGPKG_HAL).(@r_c5_AA)) }
one sig c2_true
{}
{}
lone sig c3_false
{}
{ no c3_false }
lone sig c4_CYGPKG_HAL
{ r_c5_AA : lone c5_AA
, r_c6_CYGPKG_HAL_COMMON_INTERRUPTS : one c6_CYGPKG_HAL_COMMON_INTERRUPTS }
{ some c3_false }
lone sig c5_AA
{ ref : lone Int }
{ one r_c5_AA
  all cl0 : this.@ref | cl0 = #((((c4_CYGPKG_HAL).(@r_c6_CYGPKG_HAL_COMMON_INTERRUPTS)).(@r_c7_CYGNUM_HAL_COMMON_INTERRUPTS_STACK_SIZE)).(@r_c8_AA_ABSTRACT))
  #((((c4_CYGPKG_HAL).(@r_c6_CYGPKG_HAL_COMMON_INTERRUPTS)).(@r_c7_CYGNUM_HAL_COMMON_INTERRUPTS_STACK_SIZE)).(@r_c8_AA_ABSTRACT)) > 0
  all cl0 : this.@ref | cl0 >= 5 }
lone sig c6_CYGPKG_HAL_COMMON_INTERRUPTS
{ r_c7_CYGNUM_HAL_COMMON_INTERRUPTS_STACK_SIZE : lone c7_CYGNUM_HAL_COMMON_INTERRUPTS_STACK_SIZE }
{ one r_c6_CYGPKG_HAL_COMMON_INTERRUPTS }
lone sig c7_CYGNUM_HAL_COMMON_INTERRUPTS_STACK_SIZE
{ ref : lone Int
, r_c8_AA_ABSTRACT : one c8_AA_ABSTRACT }
{ one r_c7_CYGNUM_HAL_COMMON_INTERRUPTS_STACK_SIZE
  all cl0 : this.@ref | cl0 < 5
  (all cl0 : this.@ref | cl0 >= 0) && (all cl0 : this.@ref | cl0 =< 5) }
lone sig c8_AA_ABSTRACT extends c1_AA_ABSTRACT
{}
{ one r_c8_AA_ABSTRACT }
lone sig c9_CYGPKG_LIBC_STARTUP
{}
{ some c3_false
  (some ((c4_CYGPKG_HAL).(@r_c6_CYGPKG_HAL_COMMON_INTERRUPTS)).(@r_c7_CYGNUM_HAL_COMMON_INTERRUPTS_STACK_SIZE)) => (all cl0 : ((c4_CYGPKG_HAL).(@r_c6_CYGPKG_HAL_COMMON_INTERRUPTS)).(@r_c7_CYGNUM_HAL_COMMON_INTERRUPTS_STACK_SIZE) | 1 = cl0) else (1 = 0) }
