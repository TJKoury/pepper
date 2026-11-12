
!$Procedure      F_BODVAR ( BODVAR family tests )

      SUBROUTINE F_BODVAR ( OK )
      use pepper_legacy
      IMPLICIT NONE

      LOGICAL               OK

! $ Abstract
!
!     This routine tests the SPICELIB routines
!
!        BODVAR
!        BODVRD
!
! $ Version
!
!-    TSPICE Version 1.0.0, 11-FEB-2004 (NJB)
!
!-&

!
!     SPICELIB functions
!

!
!     Local parameters
!
      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-14 )

      INTEGER               FILSIZ
      PARAMETER           ( FILSIZ = 255 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

!
!     Local variables
!
      CHARACTER*(LNSIZE)    CVALS   ( 3 )
      CHARACTER*(FILSIZ)    PCK

      DOUBLE PRECISION      RADII   ( 3 )
      DOUBLE PRECISION      XRADII  ( 3 )

      INTEGER               N

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_BODVAR' )

      CALL TCASE ( 'Setup:  create full text PCK file.' )


      PCK = 'test_0008.tpc'

!
!     Create the PCK file, load it, and delete it.
!
      CALL T_PCK08 ( PCK, .TRUE., .FALSE. )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Get the radii of the earth from the kernel pool using
!     BODVRD.
!
      CALL TCASE ( 'Look up earth radii using BODVRD' )

      CALL VPACK  ( 6378.14D0, 6378.14D0, 6356.75D0, XRADII )

      CALL BODVRD ( 'EARTH', 'RADII', 3,  N, RADII )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'radii count', N,  '=', 3, 0, OK )

      CALL CHCKAD ( 'Earth radii', RADII,  '~', XRADII,  3, TIGHT , OK )



      CALL TCASE ( 'Look up earth radii using BODVRD.  Name = 399' )

      CALL VPACK  ( 6378.14D0, 6378.14D0, 6356.75D0, XRADII )

      CALL BODVRD ( '399', 'RADII', 3,  N, RADII )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'radii count', N,  '=', 3, 0, OK )

      CALL CHCKAD ( 'Earth radii', RADII,  '~', XRADII,  3, TIGHT , OK )


!
!     Error cases for BODVRD:
!
      CALL TCASE ( 'Output array too small' )

      CALL BODVRD ( 'EARTH', 'RADII', 2,  N, RADII )

      CALL CHCKXC ( .TRUE., 'SPICE(ARRAYTOOSMALL)', OK )


      CALL TCASE ( 'Data type mismatch' )

      CVALS(1) = 'A'
      CVALS(2) = 'B'
      CVALS(3) = 'C'

      CALL PCPOOL ( 'BODY399_SYMBOLIC_RADII', 3, CVALS )

      CALL BODVRD ( 'EARTH', 'SYMBOLIC_RADII', 3,  N, RADII )

      CALL CHCKXC ( .TRUE., 'SPICE(TYPEMISMATCH)', OK )


      CALL TCASE ( 'Variable not present' )

      CALL BODVRD ( 'EARTH', 'radii', 2,  N, RADII )

      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )


      CALL TCASE ( 'Body name not associated with code' )

      CALL BODVRD ( 'XYZ', 'RADII', 2,  N, RADII )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTRANSLATION)', OK )


!
!     Try again with BODVAR.
!
      CALL TCASE ( 'Look up earth radii using BODVAR' )
      CALL CLEARD ( 3, RADII )

      CALL BODVAR ( 399, 'RADII', N, RADII )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'Earth radii', RADII,  '~', XRADII,  3, TIGHT , OK )




      CALL TCASE ( 'Variable not present' )

      CALL BODVAR ( 399, 'radii', N, RADII )

      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

      CALL T_SUCCESS ( OK )

      END
