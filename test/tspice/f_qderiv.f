C$Procedure F_QDERIV ( QDERIV tests )

      SUBROUTINE F_QDERIV ( OK )

C$ Abstract
C
C     Test family to exercise the SPICELIB routine QDERIV.
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
C     None.
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
C-    TSPICE Version 1.0.1, 06-APR-2020 (JDR)
C
C        Fixed title in CHCKSD calls (case 2 and 4) and added comments
C        to the test family.
C
C        Updated header to comply with NAIF standard.
C
C-    TSPICE Version 1.0.0, 24-NOV-2004 (NJB)
C
C-&


C$ Index_Entries
C
C     test QDERIV
C
C-&


C
C     SPICELIB functions
C
      DOUBLE PRECISION      PI

C
C     Local Parameters
C

C
C     Tolerance levels for various tests.
C
      DOUBLE PRECISION      DM16
      PARAMETER           ( DM16   = 1.D-16 )

      DOUBLE PRECISION      DM11
      PARAMETER           ( DM11   = 1.D-11 )

      DOUBLE PRECISION      DM8
      PARAMETER           ( DM8    = 1.D-8  )


      INTEGER               MAXN
      PARAMETER           ( MAXN = 2 )

C
C     Local Variables
C
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      EPSLON
      DOUBLE PRECISION      F0     ( MAXN )
      DOUBLE PRECISION      F2     ( MAXN )
      DOUBLE PRECISION      DFDT   ( MAXN )
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      X0
      DOUBLE PRECISION      XD     ( MAXN )

      INTEGER               N

C
C     Open the test family.
C
      CALL TOPEN ( 'F_QDERIV' )

C
C     Compute machine epsilon and the horizontal scale
C     factor required for determining an appropriate value
C     of DELTA.
C
      EPSLON = 2.D0 ** (-53)

      SCALE  = EPSLON ** ( 1.D0 / 3.D0 )

C*********************************************************************
C*
C*    Normal cases
C*
C*********************************************************************

C
C --- Case 1 ------------------------------------------------------
C
      CALL TCASE ( 'Estimate the derivative of sin(x) at x = 0.'  )


      N     = 1
      DELTA = 1.D-6

      F0(1) = SIN( -DELTA )
      F2(1) = SIN(  DELTA )

      CALL QDERIV ( N, F0, F2, DELTA, DFDT )

      CALL CHCKXC ( .FALSE., ' ', OK )

      XD(1) = 1.D0

      CALL CHCKSD ( 'd(sin(x))/dt at x=0', DFDT(1), '~',
     .               XD(1),                DM11,    OK   )



C
C --- Case 2 ------------------------------------------------------
C
      CALL TCASE ( 'Estimate the derivative of sin(x) at x = pi/4.'  )


      N     = 1
      X0    = PI()/4
      DELTA = X0 * SCALE

      F0(1) = SIN( X0 - DELTA )
      F2(1) = SIN( X0 + DELTA )

      CALL QDERIV ( N, F0, F2, DELTA, DFDT )

      CALL CHCKXC ( .FALSE., ' ', OK )

      XD(1) = COS ( X0 )

      CALL CHCKSD ( 'd(sin(x))/dt at x = pi/4', DFDT(1), '~',
     .               XD(1),                      DM11,    OK   )


C
C --- Case 3 ------------------------------------------------------
C
      CALL TCASE ( 'Estimate the derivative of x2 at x = 2.'  )


      N     = 1
      DELTA = 1.D-3

      F0(1) = ( 2.D0 - DELTA ) ** 2.D0
      F2(1) = ( 2.D0 + DELTA ) ** 2.D0

      CALL QDERIV ( N, F0, F2, DELTA, DFDT )

      CALL CHCKXC ( .FALSE., ' ', OK )

      XD(1) = 4.D0

      CALL CHCKSD ( 'd(x**2)/dt at x=2', DFDT(1), '~',
     .               XD(1),              DM11,    OK   )



C
C --- Case 4 ------------------------------------------------------
C
      CALL TCASE ( 'Vector case: combine the previous cases.'  )

      N     = 2
      DELTA = 1.D-6

      F0(1) = SIN( -DELTA )
      F2(1) = SIN(  DELTA )

      F0(2) = ( 2.D0 - DELTA ) ** 2.D0
      F2(2) = ( 2.D0 + DELTA ) ** 2.D0

      CALL QDERIV ( N, F0, F2, DELTA, DFDT )

      CALL CHCKXC ( .FALSE., ' ', OK )

      XD(1) = 1.D0
      XD(2) = 4.D0

      CALL CHCKAD ( 'Vector case: combine previous cases', DFDT, '~',
     .               XD,                 2,       DM8,    OK        )


C*********************************************************************
C*
C*    Error cases
C*
C*********************************************************************

C
C --- Case 5 ------------------------------------------------------
C
      CALL TCASE ( 'DELTA == 0'  )

      CALL QDERIV ( N, F0, F2, 0.D0, DFDT )

      CALL CHCKXC ( .TRUE., 'SPICE(DIVIDEBYZERO)', OK )

C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN
      END


