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
      integer               itrue
      parameter           ( itrue  =  1 )

!
!     Integer code indicating `false':
!
      integer               ifalse
      parameter           ( ifalse = -1 )

!
!     Character code indicating `true':
!
      character*(*)         ctrue
      parameter           ( ctrue  = 'T' )

!
!     Character code indicating `false':
!
      character*(*)         cfalse
      parameter           ( cfalse = 'F' )

!
!     End Include Section:  EK Boolean Enumerated Type
!
