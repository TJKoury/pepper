!$Procedure F_NPSGPT ( NPSGPT tests )

      SUBROUTINE F_NPSGPT ( OK )
      use pepper_legacy

! $ Abstract
!
!     Exercise the SPICELIB routine NPSGPT.
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
!     This routine tests the SPICELIB routine NPSGPT.
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
!     N.J. Bachman     (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 18-MAY-2016 (NJB)
!
!-&

!
!     SPICELIB functions
!

!
!     Local Parameters
!
      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-14 )


      INTEGER               TITLEN
      PARAMETER           ( TITLEN = 160 )

!
!     Local Variables
!
      CHARACTER*(TITLEN)    TITLE

      DOUBLE PRECISION      DIR    ( 3 )
      DOUBLE PRECISION      DIST
      DOUBLE PRECISION      DSCALE
      DOUBLE PRECISION      EP1    ( 3 )
      DOUBLE PRECISION      EP2    ( 3 )
      DOUBLE PRECISION      MAXSCL
      DOUBLE PRECISION      MINSCL
      DOUBLE PRECISION      P      ( 3 )
      DOUBLE PRECISION      PERP   ( 3 )
      DOUBLE PRECISION      PNEAR  ( 3 )
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      SMALL
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      UDIR   ( 3 )
      DOUBLE PRECISION      X      ( 3 )
      DOUBLE PRECISION      XDIST
      DOUBLE PRECISION      XPNEAR ( 3 )
      DOUBLE PRECISION      Y      ( 3 )
      DOUBLE PRECISION      Z      ( 3 )

      INTEGER               I
      INTEGER               NSCALE


!
!     Saved values
!
      SAVE

!
!     Initial values
!

!
!     Open the test family.
!
      CALL TOPEN ( 'F_NPSGPT' )


!***********************************************************************
!
!     Normal cases
!
!***********************************************************************


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Segment lies on X-axis between X = 1 and X = 2. ' &
      //           'Point has negative X.'                           )


      CALL VPACK (  1.D0, 0.D0, 0.D0, EP1 )
      CALL VPACK (  2.D0, 0.D0, 0.D0, EP2 )

      CALL VPACK ( -1.D0, 0.D0, 0.D0, P   )

      CALL VEQU ( EP1, XPNEAR )

      XDIST = 2.D0

      CALL NPSGPT ( EP1, EP2, P, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )
      CALL CHCKSD ( 'DIST',  DIST,  '~',   XDIST,     TOL, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Segment lies on X-axis between X = 1 and X = 2. ' &
      //           'Point has X = 1.'                               )


      CALL VPACK (  1.D0, 0.D0, 0.D0, EP1 )
      CALL VPACK (  2.D0, 0.D0, 0.D0, EP2 )

      CALL VPACK (  1.D0, 0.D0, 0.D0, P   )

      CALL VEQU ( EP1, XPNEAR )

      XDIST = 0.D0

      CALL NPSGPT ( EP1, EP2, P, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )
      CALL CHCKSD ( 'DIST',  DIST,  '~',   XDIST,     TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Segment lies on X-axis between X = 1 and X = 2. ' &
      //           'Point has X = 2.'                               )


      CALL VPACK (  1.D0, 0.D0, 0.D0, EP1 )
      CALL VPACK (  2.D0, 0.D0, 0.D0, EP2 )

      CALL VPACK (  2.D0, 0.D0, 0.D0, P   )

      CALL VEQU ( EP2, XPNEAR )

      XDIST = 0.D0

      CALL NPSGPT ( EP1, EP2, P, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )
      CALL CHCKSD ( 'DIST',  DIST,  '~',   XDIST,     TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Segment lies on X-axis between X = 1 and X = 2. ' &
      //           'Point has X > 2.'                               )


      CALL VPACK (  1.D0, 0.D0, 0.D0, EP1 )
      CALL VPACK (  2.D0, 0.D0, 0.D0, EP2 )

      CALL VPACK (  3.D0, 0.D0, 0.D0, P   )

      CALL VEQU ( EP2, XPNEAR )

      XDIST = 1.D0

      CALL NPSGPT ( EP1, EP2, P, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )
      CALL CHCKSD ( 'DIST',  DIST,  '~',   XDIST,     TOL, OK )


!
! --- Case: ------------------------------------------------------
!


!
!     Now try some more general cases.
!
      MINSCL = -100.D0
      MAXSCL =  100.D0
      NSCALE =  30
      DSCALE =  ( MAXSCL - MINSCL ) / NSCALE


      DO I = 0, NSCALE
!
!        Generate endpoints of the segment.
!
         SCALE = 10.D0 ** ( MINSCL + (I*DSCALE) )

         CALL VPACK ( 1.D0,   3.D0, -7.D0,  EP1 )
         CALL VPACK ( 5.D0, -13.D0, -0.5D0, EP2 )

         CALL VSCLIP ( SCALE, EP1 )
         CALL VSCLIP ( SCALE, EP2 )

!
! --- Case: ------------------------------------------------------
!
         TITLE = 'Point projection is off EP1 end; I = #.'
         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL CHCKXC( .FALSE., ' ', OK )

         CALL TCASE ( TITLE )

!
!        Let DIR be the vector from EP1 to EP2.
!
         CALL VSUB ( EP2, EP1, DIR  )
         CALL VHAT ( DIR,      UDIR )
         CALL VEQU ( UDIR,     X    )

!
!        Get some vectors orthogonal to DIR; generate
!        a perpendicular component vector for P.
!
         CALL FRAME ( X, Y, Z )
         CALL CHCKXC( .FALSE., ' ', OK )

         CALL VLCOM ( 2.D0*SCALE, Y, 5.D0*SCALE, Z, PERP )

!
!        Generate an input point.
!
         SMALL = 1.D-13

         CALL VLCOM3 ( 1.D0, EP1, -SMALL, DIR, 1.D0, PERP, P )

         TOL = VTIGHT

         CALL VEQU ( EP1, XPNEAR )

         XDIST = VDIST ( P, EP1 )

         CALL NPSGPT ( EP1, EP2, P, PNEAR, DIST )
         CALL CHCKXC( .FALSE., ' ', OK )

         CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )
         CALL CHCKSD ( 'DIST',  DIST,  '~/',  XDIST,     TOL, OK )


!
! --- Case: ------------------------------------------------------
!
         TITLE = 'Point projection is EP1; I = #.'
         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL CHCKXC( .FALSE., ' ', OK )

         CALL TCASE ( TITLE )

!
!        Generate an input point.
!
         CALL VLCOM3 ( 1.D0, EP1, 0.0D0, DIR, 1.D0, PERP, P )

         TOL = VTIGHT

         CALL VEQU ( EP1, XPNEAR )

         XDIST = VDIST ( P, EP1 )

         CALL NPSGPT ( EP1, EP2, P, PNEAR, DIST )
         CALL CHCKXC( .FALSE., ' ', OK )

         CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )
         CALL CHCKSD ( 'DIST',  DIST,  '~/',  XDIST,     TOL, OK )


!
! --- Case: ------------------------------------------------------
!
         TITLE = 'Point projection is between EP1 and EP2; I = #.'
         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL CHCKXC( .FALSE., ' ', OK )

         CALL TCASE ( TITLE )

!
!        Generate an input point.
!
         CALL VLCOM3 ( 0.5D0, EP1, 0.5D0, EP2, 1.D0, PERP, P )

         TOL = VTIGHT

         CALL VLCOM ( 0.5D0, EP1, 0.5D0, EP2, XPNEAR )

         XDIST = VNORM( PERP )

         CALL NPSGPT ( EP1, EP2, P, PNEAR, DIST )
         CALL CHCKXC( .FALSE., ' ', OK )

         CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )
         CALL CHCKSD ( 'DIST',  DIST,  '~/',  XDIST,     TOL, OK )


!
! --- Case: ------------------------------------------------------
!
         TITLE = 'Point projection is EP2; I = #.'
         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL CHCKXC( .FALSE., ' ', OK )

         CALL TCASE ( TITLE )


!
!        Generate an input point.
!
         CALL VLCOM3 ( 1.D0, EP2, 0.0D0, DIR, 1.D0, PERP, P )

         TOL = VTIGHT

         CALL VEQU ( EP2, XPNEAR )

         XDIST = VDIST ( P, EP2 )

         CALL NPSGPT ( EP1, EP2, P, PNEAR, DIST )
         CALL CHCKXC( .FALSE., ' ', OK )

         CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )
         CALL CHCKSD ( 'DIST',  DIST,  '~/',  XDIST,     TOL, OK )


!
! --- Case: ------------------------------------------------------
!
         TITLE = 'Point projection is off EP2 end; I = #.'
         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL CHCKXC( .FALSE., ' ', OK )

         CALL TCASE ( TITLE )

!
!        Generate an input point.
!
         SMALL = 1.D-13

         CALL VLCOM3 ( 1.D0, EP2, SMALL, DIR, 1.D0, PERP, P )

         TOL = VTIGHT

         CALL VEQU ( EP2, XPNEAR )

         XDIST = VDIST ( P, EP2 )

         CALL NPSGPT ( EP1, EP2, P, PNEAR, DIST )
         CALL CHCKXC( .FALSE., ' ', OK )

         CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )
         CALL CHCKSD ( 'DIST',  DIST,  '~/',  XDIST,     TOL, OK )

      END DO


!*********************************************************************
!*
!*    Non-error exceptional case
!*
!*********************************************************************

      CALL TCASE ( 'EP1 == EP2' )


      CALL VLCOM ( 1.D0, EP2, 1.D0, DIR, P )

      CALL NPSGPT ( EP1, EP1, P, PNEAR, DIST )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL VEQU ( EP1, XPNEAR )

      XDIST = VDIST ( P, EP1 )

      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )
      CALL CHCKSD ( 'DIST',  DIST,  '~/',  XDIST,     TOL, OK )




!*********************************************************************
!*
!*    Error cases
!*
!*********************************************************************

!
!     None.
!


!
! ---------------------------------------------------------
!

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


