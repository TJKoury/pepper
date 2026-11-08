! $ Abstract
!
!     This file contains public, global parameter declarations
!     for the SPICELIB Direct Access Segregated (DAS) subsystem.
!
! $ Required_Reading
!
!     DAS
!
! $ Keywords
!
!     None.
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 07-APR-2020 (JDR)
!
!        Added CHARDT, DPDT and INTDT parameters.
!
!-    SPICELIB Version 1.0.0, 10-FEB-2017 (NJB)
!
!-&

!
!     Parameter declarations follow.
!

!
!     DAS file table size:
!
!        The parameter name is FTSIZE. The value of the parameter is
!        defined in the include file
!
!           zzddhman.inc
!
!        That value is duplicated here, since zzddhman.inc contains
!        other declarations that conflict with some of those in DAS
!        routines.
!
      INTEGER               FTSIZE
      PARAMETER           ( FTSIZE  = 5000 )

!
!     Capacity of DAS data records:
!
!        -- NWD double precision numbers.
!        -- NWI integers.
!        -- NWC characters.
!
!     These parameters are named to enhance ease of maintenance of
!     the code; the values should not be changed.

      INTEGER               NWD
      PARAMETER           ( NWD     =  128 )

      INTEGER               NWI
      PARAMETER           ( NWI     =  256 )

      INTEGER               NWC
      PARAMETER           ( NWC     = 1024 )

!
!     DAS data type specifiers used in all DAS routines that require
!     a data type either as input or to extract data from an output
!     array.
!
!     CHARDT,
!     DPDT,
!     INTDT    are data type specifiers which indicate CHARACTER,
!              DOUBLE PRECISION, and INTEGER respectively. These
!              parameters are used in all DAS routines that require a
!              data type specifier.
!
      INTEGER               CHARDT
      PARAMETER           ( CHARDT  =  1   )

      INTEGER               DPDT
      PARAMETER           ( DPDT    =  2   )

      INTEGER               INTDT
      PARAMETER           ( INTDT   =  3   )

!
!     End of include file das.inc
!
