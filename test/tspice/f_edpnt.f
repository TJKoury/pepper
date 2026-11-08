C$Procedure F_EDPNT ( EDPNT tests )

      SUBROUTINE F_EDPNT ( OK )

C$ Abstract
C
C     Exercise the SPICELIB routine EDPNT.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     None.
C
C$ Keywords
C
C     TEST FAMILY
C
C$ Declarations

      IMPLICIT NONE

      LOGICAL               OK

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     OK         O   logical indicating test status.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     OK       is a logical that indicates the test status to the
C              caller.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     This routine does not generate any errors. Routines in its
C     call tree may generate errors that are either intentional and
C     trapped or unintentional and need reporting.  The test family
C     utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine tests the SPICELIB routine EDPNT.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman       (JPL)
C     J. Diaz del Rio    (ODC Space)
C
C$ Version
C
C-    TSPICE Version 1.1.0, 06-APR-2020 (JDR)
C
C        Fixed error cases, which did not test SPICELIB routine EDPNT.
C
C-    TSPICE Version 1.0.0, 17-MAY-2016 (NJB)
C
C-&


C$ Index_Entries
C
C     None.
C
C-&


C
C     SPICELIB functions
C
      DOUBLE PRECISION      PI

C
C     Local Parameters
C
      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-14 )


      INTEGER               TITLEN
      PARAMETER           ( TITLEN = 160 )

C
C     Local Variables
C
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

C
C     Saved values
C
      SAVE

C
C     Initial values
C
      DATA                  ORIGIN / 3 * 0.D0 /

C
C     Open the test family.
C
      CALL TOPEN ( 'F_EDPNT' )


C***********************************************************************
C
C     Normal cases
C
C***********************************************************************

C
C --- Case: ------------------------------------------------------
C
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
C
C           We expect that a result will always be found.
C
            CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

            DO K = 0, NSCALE

               SCALE = 10.D0 ** ( MINSCL + K*DSCALE )

               CALL VSCL ( SCALE, XPT, SP )

C
C --- Case: ------------------------------------------------------
C
               TITLE = 'Ellipsoid: point = (#,#,#)'
               CALL REPMD ( TITLE, '#', SP(1), 14, TITLE )
               CALL REPMD ( TITLE, '#', SP(2), 14, TITLE )
               CALL REPMD ( TITLE, '#', SP(3), 14, TITLE )
               CALL CHCKXC( .FALSE., ' ', OK )

               CALL TCASE ( TITLE )

               CALL EDPNT ( SP, A, B, C, P )
               CALL CHCKXC ( .FALSE., ' ', OK )
C
C              Check P.
C
               TOL = VTIGHT

               CALL CHCKAD ( 'P', P, '~~/', XPT, 3, TOL, OK )

            END DO

         END DO

      END DO





C*********************************************************************
C*
C*    Error cases
C*
C*********************************************************************

C
C --- Case: ------------------------------------------------------
C
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


C
C --- Case: ------------------------------------------------------
C
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


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Input point was the zero vector.' )


      A = 1.D0
      B = A
      C = A

      CALL CLEARD ( 3, SP )

      CALL EDPNT  ( SP, A, B, C, P )
      CALL CHCKXC ( .TRUE., 'SPICE(ZEROVECTOR)', OK )


C
C ---------------------------------------------------------
C

C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN
      END


