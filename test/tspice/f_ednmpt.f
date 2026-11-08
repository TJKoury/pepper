C$Procedure F_EDNMPT ( EDNMPT tests )
 
      SUBROUTINE F_EDNMPT ( OK )
 
C$ Abstract
C
C     Exercise the SPICELIB routine EDNMPT.
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
C     OK         is a logical that indicates the test status to the
C                caller.
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
C     This routine tests the SPICELIB routine EDNMPT.
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
C     N.J. Bachman     (JPL)
C
C$ Version
C
C-    TSPICE Version 1.0.0, 17-MAY-2016 (NJB)
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
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      P      ( 3 )
      DOUBLE PRECISION      R
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      XNORML ( 3 )
            
      INTEGER               I
      INTEGER               J
      INTEGER               NSTEP

C
C     Saved values
C
      SAVE

C
C     Initial values
C

C
C     Open the test family.
C
      CALL TOPEN ( 'F_EDNMPT' )


C***********************************************************************
C
C     Normal cases
C
C***********************************************************************


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Sphere tests' )

      R     = 7.D0
      A     = R
      B     = R
      C     = R

      NSTEP = 10
      DLON  = 2*PI() / NSTEP
      DLAT  =   PI() / NSTEP

      DO I = 1, NSTEP

         LON = I * DLON

         DO J = 0, NSTEP

            LAT = ( PI()/2 ) - J * DLAT

            CALL LATREC ( 1.D0, LON, LAT, XNORML )

C
C --- Case: ------------------------------------------------------
C
            TITLE = 'Sphere: normal = (#,#,#)'
            CALL REPMD ( TITLE, '#', XNORML(1), 14, TITLE )
            CALL REPMD ( TITLE, '#', XNORML(2), 14, TITLE )
            CALL REPMD ( TITLE, '#', XNORML(3), 14, TITLE )
            CALL CHCKXC( .FALSE., ' ', OK )

            CALL TCASE ( TITLE )


            CALL EDNMPT ( A, B, C, XNORML, P )
            CALL CHCKXC ( .FALSE., ' ', OK )
C
C           Check that we can recover the input normal
C           from P.
C
            CALL SURFNM ( A, B, C, P, NORMAL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            TOL = VTIGHT

            CALL CHCKAD ( 'NORMAL',  NORMAL, '~~/', 
     .                    XNORML, 3, TOL,    OK   )
 
         END DO

      END DO
      


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Triaxial ellipsoid tests' )

      A     = 100.D0
      B     =  50.D0
      C     =   3.D0

      NSTEP = 10
      DLON  = 2*PI() / NSTEP
      DLAT  =   PI() / NSTEP

      DO I = 1, NSTEP

         LON = I * DLON

         DO J = 0, NSTEP

            LAT = ( PI()/2 ) - J * DLAT

            CALL LATREC ( 1.D0, LON, LAT, XNORML )

C
C --- Case: ------------------------------------------------------
C
            TITLE = 'Ellipsoid: normal = (#,#,#)'
            CALL REPMD ( TITLE, '#', XNORML(1), 14, TITLE )
            CALL REPMD ( TITLE, '#', XNORML(2), 14, TITLE )
            CALL REPMD ( TITLE, '#', XNORML(3), 14, TITLE )
            CALL CHCKXC( .FALSE., ' ', OK )

            CALL TCASE ( TITLE )


            CALL EDNMPT ( A, B, C, XNORML, P )
            CALL CHCKXC ( .FALSE., ' ', OK )
C
C           Check that we can recover the input normal
C           from P.
C
            CALL SURFNM ( A, B, C, P, NORMAL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            TOL = VTIGHT

            CALL CHCKAD ( 'NORMAL',  NORMAL, '~~/', 
     .                    XNORML, 3, TOL,    OK   )
 
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
      CALL TCASE ( 'Invalid semi-axis lengths.' )
 

      A     = 100.D0
      B     =  50.D0
      C     =   3.D0

      CALL VPACK ( 0.D0, 0.D0, 1.D0, XNORML )

      CALL EDNMPT ( -A, B, C, XNORML, P )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )

      CALL EDNMPT (  A, -B, C, XNORML, P )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )

      CALL EDNMPT (  A, B, -C, XNORML, P )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )

      CALL EDNMPT (  0.D0, B, C, XNORML, P )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )

      CALL EDNMPT (  A, 0.D0, C, XNORML, P )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )

      CALL EDNMPT (  A, B, 0.D0, XNORML, P )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Invalid semi-axis length after scaling.' )
 

      A     = 1.D300
      B     = 1.D-300
      C     =   3.D0

      CALL VPACK ( 0.D0, 0.D0, 1.D0, XNORML )

      CALL EDNMPT (  A, B, C, XNORML, P )
      CALL CHCKXC ( .TRUE., 'SPICE(AXISUNDERFLOW)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Zero normal vector.' )
      
      A = 1.D0
      B = A
      C = A

      CALL CLEARD ( 3, XNORML )
      
      CALL EDNMPT (  A, B, C, XNORML, P )
      CALL CHCKXC ( .TRUE., 'SPICE(ZEROVECTOR)', OK )



C
C --- Case: ------------------------------------------------------
C
      
C
C     The degenerate case error resulting from a non-positive
C     reciprocal of the square of lambda should never occur.
C     A disposable version of the code should be used to
C     exercise the error handling for this case.
C


C
C ---------------------------------------------------------
C

C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END


