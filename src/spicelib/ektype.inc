!     Include Section:  EK Data Types
!
!        ektype.inc Version 1  27-DEC-1994 (NJB)
!
!
!     Within the EK system, data types of EK column contents are
!     represented by integer codes.  The codes and their meanings
!     are listed below.
!
!     Integer codes are also used within the DAS system to indicate
!     data types; the EK system makes no assumptions about compatibility
!     between the codes used here and those used in the DAS system.
!
!
!     Character type:
!
      INTEGER               CHR
      PARAMETER           ( CHR  = 1 )

!
!     Double precision type:
!
      INTEGER               DP
      PARAMETER           ( DP   = 2 )

!
!     Integer type:
!
      INTEGER               INT
      PARAMETER           ( INT  = 3 )

!
!     `Time' type:
!
!     Within the EK system, time values are represented as ephemeris
!     seconds past J2000 (TDB), and double precision numbers are used
!     to store these values.  However, since time values require special
!     treatment both on input and output, and since the `TIME' column
!     has a special role in the EK specification and code, time values
!     are identified as a type distinct from double precision numbers.
!
      INTEGER               TIME
      PARAMETER           ( TIME = 4 )

!
!     End Include Section:  EK Data Types
!
