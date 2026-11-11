!> The parameters of SPICELIB's include file ekopcd.inc (an include
!> file until PEPPER; tools/inc_to_modules.py). Its comments follow.
module pepper_inc_ekopcd

   use pepper_kinds, only: ip

   implicit none

   public

!     Include Section:  EK Operator Codes
!
!        ekopcd.inc  Version 1  30-DEC-1994 (NJB)
!
!
!     Within the EK system, operators used in EK queries are
!     represented by integer codes.  The codes and their meanings are
!     listed below.
!
!     Relational expressions in EK queries have the form
!
!        <column name> <operator> <value>
!
!     For columns containing numeric values, the operators
!
!        EQ,  GE,  GT,  LE,  LT,  NE
!
!     may be used; these operators have the same meanings as their
!     Fortran counterparts.  For columns containing character values,
!     the list of allowed operators includes those in the above list,
!     and in addition includes the operators
!
!        LIKE,  UNLIKE
!
!     which are used to compare strings to a template.  In the character
!     case, the meanings of the parameters
!
!        GE,  GT,  LE,  LT
!
!     match those of the Fortran lexical functions
!
!        LGE, LGT, LLE, LLT
!
!
!     The additional unary operators
!
!        ISNULL, NOTNUL
!
!     are used to test whether a value of any type is null.
!
!
   integer(ip), parameter :: eq = 1

   integer(ip), parameter :: ge = eq + 1

   integer(ip), parameter :: gt = ge + 1

   integer(ip), parameter :: le = gt + 1

   integer(ip), parameter :: lt = le + 1

   integer(ip), parameter :: ne = lt + 1

   integer(ip), parameter :: like = ne + 1

   integer(ip), parameter :: unlike = like + 1

   integer(ip), parameter :: isnull = unlike + 1

   integer(ip), parameter :: notnul = isnull + 1

!
!     End Include Section:  EK Operator Codes
!

end module pepper_inc_ekopcd
