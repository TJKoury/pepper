!     Include Section:  EK Boolean Enumerated Type
!
!
!        ekbool.inc Version 1   21-DEC-1994 (NJB)
!
!
!     Within the EK system, boolean values sometimes must be
!     represented by integer or character codes.  The codes and their
!     meanings are listed below.
!
!     Integer code indicating `true':
!
      INTEGER               ITRUE
      PARAMETER           ( ITRUE  =  1 )

!
!     Integer code indicating `false':
!
      INTEGER               IFALSE
      PARAMETER           ( IFALSE = -1 )

!
!     Character code indicating `true':
!
      CHARACTER*(*)         CTRUE
      PARAMETER           ( CTRUE  = 'T' )

!
!     Character code indicating `false':
!
      CHARACTER*(*)         CFALSE
      PARAMETER           ( CFALSE = 'F' )

!
!     End Include Section:  EK Boolean Enumerated Type
!
