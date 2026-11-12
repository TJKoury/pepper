!$Procedure F_DHFA ( DHFA, DVSEP family tests )

      SUBROUTINE F_DHFA ( OK )
      use pepper_legacy

! $ Abstract
!
!     This routine tests the SPICELIB routines
!
!        ZZGFDHFA
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TEST FAMILY
!
! $ Declarations

      IMPLICIT NONE
      LOGICAL               OK

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     OK         O   logical indicating test status.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     OK         is a logical that indicates the test status to the
!                caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     This routine does not generate any errors. Routines in its
!     call tree may generate errors that are either intentional and
!     trapped or unintentional and need reporting.  The test family
!     utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     None.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     E.D. Wright     (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.1 06-JUL-2009 (EDW)
!
!        Rename of the ZZDHA call to DHFA.
!
!-    TSPICE Version 1.0.0, 15-OCT-2008 (EDW)
!
!-&

!
!     SPICELIB functions
!

      DOUBLE PRECISION      T_RANDD

!
!     Local parameters
!

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT = 1.D-12 )

      DOUBLE PRECISION      RBODY
      PARAMETER           ( RBODY = 1000.D0 )

      DOUBLE PRECISION      ZERO6 ( 6 )
      DOUBLE PRECISION      ONE6  ( 6 )
      DOUBLE PRECISION      VAL
      DOUBLE PRECISION      ADOT
      DOUBLE PRECISION      R
      DOUBLE PRECISION      BODYR
      DOUBLE PRECISION      STATE(6)

      INTEGER               SEED

      DATA                  ZERO6 /0.D0, 0.D0, 0.D0, 0.D0, 0.D0, 0.D0/
      DATA                  ONE6  /1.D0, 1.D0, 1.D0, 1.D0, 1.D0, 1.D0/

      SAVE

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_DHFA' )

!
!     Case 1
!
      CALL TCASE ( 'Non-positive body radius.' )

      SEED = -1804030
      BODYR = T_RANDD( -199999999.D0, 0.D0, SEED )

      VAL   = DHFA( ONE6, BODYR )
      CALL CHCKXC ( .TRUE., 'SPICE(BADRADIUS)', OK )



!
!     Case 2
!
      CALL TCASE ( 'Position vector equals zero vector.' )

      VAL   = DHFA( ZERO6, RBODY )
      CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATECASE)', OK )



!
!     Case 3
!
      CALL TCASE ( 'Range less-than body radius.' )

      VAL   = DHFA( ONE6, RBODY )
      CALL CHCKXC ( .TRUE., 'SPICE(BADGEOMETRY)', OK )



!
!     Case 4
!
!     Circular orbits; constant half angle. Physically
!     realistic orbits not required, only geometry with
!     -   -
!     R . V = 0
!
      CALL TCASE ( 'DHFA Circular orbits.' )

!         .
!     x * x = 1
!         .
!     y * y = -1/2
!         .
!     z * z = -1/2
!
      STATE(1) = 1.D08
      STATE(2) = 1.1D12
      STATE(3) = -2.3D10
      STATE(4) = 1.D-08
      STATE(5) = -0.5D0 * 1.1D-12
      STATE(6) =  0.5D0 * 2.3D10

      VAL   = DHFA( STATE, RBODY )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'DHFA Circular', VAL, '~', 0.D0, TIGHT, OK )



!
!     Case 5
!
!     Linear orbits.
!     Physically realistic orbits not required, only geometry with
!     -   -
!     R . V = 1
!
      CALL TCASE ( 'DHFA Linear orbits.' )

!         .
!     x * x = 1/3
!         .
!     y * y = 1/3
!         .
!     z * z = 1/3
!
      STATE(1) = 1.D04
      STATE(2) = 1.1D08
      STATE(3) = -2.3D06
      STATE(4) =  (1.D0/3.D0) * 1.D0-4
      STATE(5) =  (1.D0/3.D0) * 1.1D0-8
      STATE(6) = -(1.D0/3.D0) * 2.3D0-6

!
!     Reduce the function for the half angle derivative given
!     -   -
!     R . V = 1
!
!     to the form
!
!     d              - body_radius                 1
!     --(ALPHA) =  --------------------- *      ------
!     dt                  2             2  1/2       2
!                   (range - body_radius )      range
!
      R    =  VNORM(STATE(1))
      ADOT = -RBODY/DSQRT( R**2 - RBODY**2)
      ADOT =  ADOT/R**2

      VAL  = DHFA( STATE, RBODY )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSD ( 'DHFA Linear', VAL, '~', ADOT, TIGHT, OK )


      CALL T_SUCCESS ( OK )

      RETURN
      END

