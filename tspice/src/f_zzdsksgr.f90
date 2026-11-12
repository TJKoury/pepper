!$Procedure      F_ZZDSKSGR ( Test ZZDSKSGR )

      SUBROUTINE F_ZZDSKSGR ( OK )
      use pepper_legacy
      use pepper_inc_dskdsc
      IMPLICIT NONE

! $ Abstract
!
!     Test the SPICELIB bounding ellipsoid routine ZZDSKSGR.
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
!     This routine tests the routine ZZDSKSGR.
!
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
!-    TSPICE Version 1.0.0, 13-FEB-2017 (NJB)
!
!-&


!
!     Test Utility Functions
!


!
!     SPICELIB functions
!

!
!     Local parameters
!
      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-13 )

!
!     Local Variables
!
      DOUBLE PRECISION      BDS    ( 2, 3 )
      DOUBLE PRECISION      CORNER ( 3 )
      DOUBLE PRECISION      DSKDSC ( DSKDSZ )
      DOUBLE PRECISION      F
      DOUBLE PRECISION      R
      DOUBLE PRECISION      RE
      DOUBLE PRECISION      RP
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      XR

      INTEGER               I
      INTEGER               J
      INTEGER               K

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_ZZDSKSGR' )

!
!     ZZDSKSGR error cases:
!

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Error case: unrecognized coordinate system code.' )


      CALL CLEARD ( DSKDSZ, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DSKDSC(SYSIDX) = DBLE( -1 )

      R = ZZDSKSGR ( DSKDSC )
      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Error case: bad flattening coefficient.' )


      CALL CLEARD ( DSKDSZ, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DSKDSC(SYSIDX)   = DBLE( PDTSYS )

      RE               = 3.D3
      F                = 1.D0 + 1.D-13
      DSKDSC(PARIDX  ) = RE
      DSKDSC(PARIDX+1) = F

      R = ZZDSKSGR ( DSKDSC )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Error case: negative equatorial radius.' )


      CALL CLEARD ( DSKDSZ, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DSKDSC(SYSIDX)   = DBLE( PDTSYS )

      RE               = -3.D3
      F                = 0.D0
      DSKDSC(PARIDX  ) = RE
      DSKDSC(PARIDX+1) = F

      R = ZZDSKSGR ( DSKDSC )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Error case: negative minimum radius.' )


      CALL CLEARD ( DSKDSZ, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DSKDSC(SYSIDX)   = DBLE( LATSYS )

      DSKDSC(MN3IDX)   = -1.D0
      DSKDSC(MX3IDX)   =  1.D0


      R = ZZDSKSGR ( DSKDSC )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )



!
!     ZZDSKSGR normal cases:
!

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Find radius for rectangular boundary.' )


      CALL CLEARD ( DSKDSZ, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DSKDSC(SYSIDX) = DBLE( RECSYS )

      DSKDSC(MN1IDX) = -7.D0
      DSKDSC(MX1IDX) =  3.D0
      DSKDSC(MN2IDX) =  5.D0
      DSKDSC(MX2IDX) =  6.D0
      DSKDSC(MN3IDX) = -1.D0
      DSKDSC(MX3IDX) =  3.D0

!
!     Compute expected radius. This is the magnitude of the longest
!     vector from the origin to a corner of the bounding box.
!
!     Use the dumbest possible brute force approach. This is
!     done to have a different algorithm from that in the
!     SPICELIB routine.
!
      CALL MOVED ( DSKDSC(MN1IDX), 6, BDS )

      XR = 0.D0

      DO I = 1, 2

         CORNER(1) = BDS(I,1)

         DO J = 1, 2

            CORNER(2) = BDS(J,2)

            DO K = 1, 2

               CORNER(3) = BDS(K,3)

               XR = MAX( XR, VNORM(CORNER) )

            END DO

         END DO

      END DO


      R   = ZZDSKSGR ( DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = TIGHT

      CALL CHCKSD ( 'R', R, '~/', XR, TOL, OK )


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Find radius for latitudinal boundary.' )


      CALL CLEARD ( DSKDSZ, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DSKDSC(SYSIDX) = DBLE( LATSYS )

      DSKDSC(MN1IDX) = -PI()/4
      DSKDSC(MX1IDX) =  PI()/2
      DSKDSC(MN2IDX) =  PI()/4
      DSKDSC(MX2IDX) =  PI()/3
      DSKDSC(MN3IDX) =  1.D3
      DSKDSC(MX3IDX) =  2.D3

      XR  = DSKDSC( MX3IDX )

      R   = ZZDSKSGR( DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = 0.D0

      CALL CHCKSD ( 'R', R, '~/', XR, TOL, OK )



!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Find radius for oblate planetodetic boundary.' )


      CALL CLEARD ( DSKDSZ, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DSKDSC(SYSIDX)   = DBLE( PDTSYS )

      RE               = 3.D3
      RP               = 2.D3
      F                = ( RE - RP ) / RE
      DSKDSC(PARIDX  ) = RE
      DSKDSC(PARIDX+1) = F


      DSKDSC(MN1IDX) = -PI()/4
      DSKDSC(MX1IDX) =  PI()/2
      DSKDSC(MN2IDX) =  PI()/4
      DSKDSC(MX2IDX) =  PI()/3
      DSKDSC(MN3IDX) =  -1.D1
      DSKDSC(MX3IDX) =   2.D1

!
!     The largest distance from the origin is on the equator
!     at max altitude.
!
      XR  = RE + DSKDSC(MX3IDX)

      R   = ZZDSKSGR( DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = TIGHT

      CALL CHCKSD ( 'R', R, '~/', XR, TOL, OK )





      CALL T_SUCCESS ( OK )
      RETURN
      END
