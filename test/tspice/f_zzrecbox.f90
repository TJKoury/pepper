!$Procedure F_ZZRECBOX ( ZZRECBOX tests )

      SUBROUTINE F_ZZRECBOX ( OK )

! $ Abstract
!
!     Exercise the private SPICELIB routine ZZRECBOX.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
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

      INCLUDE 'dskdsc.inc'
      INCLUDE 'dsktol.inc'

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
!     This routine tests the private SPICELIB routine ZZRECBOX.
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
!-    TSPICE Version 1.0.0, 04-JUN-2016 (NJB)
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

!
!     Local Variables
!
      DOUBLE PRECISION      BOUNDS ( 2, 3 )
      DOUBLE PRECISION      CENTER ( 3 )
      DOUBLE PRECISION      LX
      DOUBLE PRECISION      LY
      DOUBLE PRECISION      LZ
      DOUBLE PRECISION      RADIUS
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      XCTR   ( 3 )
      DOUBLE PRECISION      XR
      DOUBLE PRECISION      XLX
      DOUBLE PRECISION      XLY
      DOUBLE PRECISION      XLZ

      INTEGER               I


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
      CALL TOPEN ( 'F_ZZRECBOX' )


!***********************************************************************
!
!     Normal cases
!
!***********************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Box includes origin.' )

      BOUNDS(1,1) =  -1.D0
      BOUNDS(2,1) =  3.D0

      BOUNDS(1,2) = -3.D0
      BOUNDS(2,2) =  4.D0

      BOUNDS(1,3) = -6.D0
      BOUNDS(2,3) =  8.D0


      DO I = 1, 3
         XCTR(I) = ( BOUNDS(1,I) + BOUNDS(2,I) ) / 2
      END DO

      XLX = BOUNDS(2,1) - BOUNDS(1,1)
      XLY = BOUNDS(2,2) - BOUNDS(1,2)
      XLZ = BOUNDS(2,3) - BOUNDS(1,3)

      XR  = SQRT( XLX**2 + XLY**2 + XLZ**2 ) / 2


      CALL ZZRECBOX ( BOUNDS, CENTER, LX, LY, LZ, RADIUS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKAD ( 'CENTER', CENTER, '~~/', XCTR, 3, TOL, OK )

      CALL CHCKSD ( 'LX', LX, '~', XLX, TOL, OK )
      CALL CHCKSD ( 'LY', LY, '~', XLY, TOL, OK )
      CALL CHCKSD ( 'LZ', LZ, '~', XLZ, TOL, OK )

      CALL CHCKSD ( 'RADIUS', RADIUS, '~', XR, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Box excludes origin.' )

      BOUNDS(1,1) =  3.D0
      BOUNDS(2,1) =  5.D0

      BOUNDS(1,2) = -3.D0
      BOUNDS(2,2) = -1.D0

      BOUNDS(1,3) = -6.D0
      BOUNDS(2,3) = -5.D0


      DO I = 1, 3
         XCTR(I) = ( BOUNDS(1,I) + BOUNDS(2,I) ) / 2
      END DO

      XLX = BOUNDS(2,1) - BOUNDS(1,1)
      XLY = BOUNDS(2,2) - BOUNDS(1,2)
      XLZ = BOUNDS(2,3) - BOUNDS(1,3)

      XR  = SQRT( XLX**2 + XLY**2 + XLZ**2 ) / 2


      CALL ZZRECBOX ( BOUNDS, CENTER, LX, LY, LZ, RADIUS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKAD ( 'CENTER', CENTER, '~~/', XCTR, 3, TOL, OK )

      CALL CHCKSD ( 'LX', LX, '~', XLX, TOL, OK )
      CALL CHCKSD ( 'LY', LY, '~', XLY, TOL, OK )
      CALL CHCKSD ( 'LZ', LZ, '~', XLZ, TOL, OK )

      CALL CHCKSD ( 'RADIUS', RADIUS, '~', XR, TOL, OK )




!***********************************************************************
!
!     Error cases
!
!***********************************************************************

! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error: invalid rectangular coordinate bounds.' )


      BOUNDS(1,1) =  1.D0
      BOUNDS(2,1) =  -3.D0

      BOUNDS(1,2) = -3.D0
      BOUNDS(2,2) = -1.D0

      BOUNDS(1,3) =  6.D0
      BOUNDS(2,3) =  8.D0


      CALL ZZRECBOX ( BOUNDS, CENTER, LX, LY, LZ, RADIUS )
      CALL CHCKXC ( .TRUE., 'SPICE(BOUNDSOUTOFORDER)', OK )

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


