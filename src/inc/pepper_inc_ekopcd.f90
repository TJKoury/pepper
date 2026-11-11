!> EK Operator Codes.
!>
!>     Within the EK system, operators used in EK queries are
!>     represented by integer codes.  The codes and their meanings are
!>     listed below.
!>
!>     Relational expressions in EK queries have the form
!>
!>     For columns containing numeric values, the operators
!>
!>     may be used; these operators have the same meanings as their
!>     Fortran counterparts.  For columns containing character values,
!>     the list of allowed operators includes those in the above list,
!>     and in addition includes the operators
!>
!>     which are used to compare strings to a template.  In the character
!>     case, the meanings of the parameters
!>
!>     match those of the Fortran lexical functions
!>
!>     The additional unary operators
!>
!>## Version
!>
!>        ekopcd.inc  Version 1  30-DEC-1994 (NJB)
!>
!>        <column name> <operator> <value>
!>
!>        EQ,  GE,  GT,  LE,  LT,  NE
!>
!>        LIKE,  UNLIKE
!>
!>        GE,  GT,  LE,  LT
!>
!>        LGE, LGT, LLE, LLT
!>
!>        ISNULL, NOTNUL
module pepper_inc_ekopcd

   use pepper_kinds, only: ip

   implicit none

   public

   integer(ip), parameter :: eq = 1  !! are used to test whether a value of any type is null.

   integer(ip), parameter :: ge = eq + 1

   integer(ip), parameter :: gt = ge + 1

   integer(ip), parameter :: le = gt + 1

   integer(ip), parameter :: lt = le + 1

   integer(ip), parameter :: ne = lt + 1

   integer(ip), parameter :: like = ne + 1

   integer(ip), parameter :: unlike = like + 1

   integer(ip), parameter :: isnull = unlike + 1

   integer(ip), parameter :: notnul = isnull + 1

end module pepper_inc_ekopcd
