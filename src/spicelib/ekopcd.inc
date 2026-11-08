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
      INTEGER               EQ
      PARAMETER           ( EQ     =         1 )

      INTEGER               GE
      PARAMETER           ( GE     =  EQ  +  1 )

      INTEGER               GT
      PARAMETER           ( GT     =  GE  +  1 )

      INTEGER               LE
      PARAMETER           ( LE     =  GT  +  1 )

      INTEGER               LT
      PARAMETER           ( LT     =  LE  +  1 )

      INTEGER               NE
      PARAMETER           ( NE     =  LT  +  1 )

      INTEGER               LIKE
      PARAMETER           ( LIKE   =  NE     +  1 )

      INTEGER               UNLIKE
      PARAMETER           ( UNLIKE =  LIKE   +  1 )

      INTEGER               ISNULL
      PARAMETER           ( ISNULL =  UNLIKE +  1 )

      INTEGER               NOTNUL
      PARAMETER           ( NOTNUL =  ISNULL +  1 )

!
!     End Include Section:  EK Operator Codes
!
