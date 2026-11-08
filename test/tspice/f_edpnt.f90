!$Procedure F_EDPNT ( EDPNT tests )

      SUBROUTINE F_EDPNT ( OK )

! $ Abstract
!
!     Exercise the SPICELIB routine EDPNT.
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
!     OK       is a logical that indicates the test status to the
!              caller.
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
!     This routine tests the SPICELIB routine EDPNT.
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!
! $ Version
!
!-    TSPICE Version 1.1.0, 06-APR-2020 (JDR)
!
!        Fixed error cases, which did not test SPICELIB routine EDPNT.
!
!-    TSPICE Version 1.0.0, 17-MAY-2016 (NJB)
!
!-&


! $ Index_Entries
!
!     None.
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      PI

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

      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      DLAT
      DOUBLE PRECISION      DLON
      DOUBLE PRECISION      DSCALE
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      MAXSCL
      DOUBLE PRECISION      MINSCL
      DOUBLE PRECISION      ORIGIN ( 3 )
      DOUBLE PRECISION      P      ( 3 )
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      SP     ( 3 )
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      XPT    ( 3 )

      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               NSTEP
      INTEGER               NSCALE

      LOGICAL               FOUND

!
!     Saved values
!
      SAVE

!
!     Initial values
!
      DATA                  ORIGIN / 3 * 0.D0 /

!
!     Open the test family.
!
      CALL TOPEN ( 'F_EDPNT' )


!***********************************************************************
!
!     Normal cases
!
!***********************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Triaxial ellipsoid tests' )

      A      = 100.D0
      B      =  50.D0
      C      =   3.D0

      NSTEP  = 10
      DLON   = 2*PI() / NSTEP
      DLAT   =   PI() / NSTEP

      NSCALE =  20
      MINSCL = -100.D0
      MAXSCL =  100.D0
      DSCALE =  ( MAXSCL - MINSCL ) / NSCALE

      DO I = 1, NSTEP

         LON = I * DLON

         DO J = 0, NSTEP

            LAT = ( PI()/2 ) - J * DLAT

            CALL LATREC ( 1.D0, LON, LAT, P )

            CALL SURFPT ( ORIGIN, P, A, B, C, XPT, FOUND )
            CALL CHCKXC( .FALSE., ' ', OK )
!
!           We expect that a result will always be found.
!
            CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

            DO K = 0, NSCALE

               SCALE = 10.D0 ** ( MINSCL + K*DSCALE )

               CALL VSCL ( SCALE, XPT, SP )

!
! --- Case: ------------------------------------------------------
!
               TITLE = 'Ellipsoid: point = (#,#,#)'
               CALL REPMD ( TITLE, '#', SP(1), 14, TITLE )
               CALL REPMD ( TITLE, '#', SP(2), 14, TITLE )
               CALL REPMD ( TITLE, '#', SP(3), 14, TITLE )
               CALL CHCKXC( .FALSE., ' ', OK )

               CALL TCASE ( TITLE )

               CALL EDPNT ( SP, A, B, C, P )
               CALL CHCKXC ( .FALSE., ' ', OK )
!
!              Check P.
!
               TOL = VTIGHT

               CALL CHCKAD ( 'P', P, '~~/', XPT, 3, TOL, OK )

            END DO

         END DO

      END DO





!*********************************************************************
!*
!*    Error cases
!*
!*********************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Invalid ellipsoid radii lengths.' )


      A     = 100.D0
      B     =  50.D0
      C     =   3.D0

      CALL VPACK ( 0.D0, 0.D0, 1.D0, SP )

      CALL EDPNT  ( SP, -A, B, C, P )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDRADII)', OK )

      CALL EDPNT  ( SP, A, -B, C, P )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDRADII)', OK )

      CALL EDPNT  ( SP, A, B, -C, P )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDRADII)', OK )

      CALL EDPNT  ( SP, 0.D0, B, C, P )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDRADII)', OK )

      CALL EDPNT  ( SP, A, 0.D0, C, P )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDRADII)', OK )

      CALL EDPNT  ( SP, A, B, 0.D0, P )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDRADII)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Point too close to the origin.' )


      A     = 1.D300
      B     = A
      C     = A

      CALL VPACK  ( 1.D0, 0.D0, 0.D0, SP )

      CALL EDPNT  ( SP, A, B, C, P )
      CALL CHCKXC ( .TRUE., 'SPICE(POINTTOOSMALL)', OK )

      CALL VPACK  ( 0.D0, 1.D0, 0.D0, SP )

      CALL EDPNT  ( SP, A, B, C, P )
      CALL CHCKXC ( .TRUE., 'SPICE(POINTTOOSMALL)', OK )

      CALL VPACK  ( 0.D0, 0.D0, 1.D0, SP )

      CALL EDPNT  ( SP, A, B, C, P )
      CALL CHCKXC ( .TRUE., 'SPICE(POINTTOOSMALL)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Input point was the zero vector.' )


      A = 1.D0
      B = A
      C = A

      CALL CLEARD ( 3, SP )

      CALL EDPNT  ( SP, A, B, C, P )
      CALL CHCKXC ( .TRUE., 'SPICE(ZEROVECTOR)', OK )


!
! ---------------------------------------------------------
!

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


