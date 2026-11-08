!$Procedure F_QDERIV ( QDERIV tests )

      SUBROUTINE F_QDERIV ( OK )

! $ Abstract
!
!     Test family to exercise the SPICELIB routine QDERIV.
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!
! $ Version
!
!-    TSPICE Version 1.0.1, 06-APR-2020 (JDR)
!
!        Fixed title in CHCKSD calls (case 2 and 4) and added comments
!        to the test family.
!
!        Updated header to comply with NAIF standard.
!
!-    TSPICE Version 1.0.0, 24-NOV-2004 (NJB)
!
!-&


! $ Index_Entries
!
!     test QDERIV
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      PI

!
!     Local Parameters
!

!
!     Tolerance levels for various tests.
!
      DOUBLE PRECISION      DM16
      PARAMETER           ( DM16   = 1.D-16 )

      DOUBLE PRECISION      DM11
      PARAMETER           ( DM11   = 1.D-11 )

      DOUBLE PRECISION      DM8
      PARAMETER           ( DM8    = 1.D-8  )


      INTEGER               MAXN
      PARAMETER           ( MAXN = 2 )

!
!     Local Variables
!
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      EPSLON
      DOUBLE PRECISION      F0     ( MAXN )
      DOUBLE PRECISION      F2     ( MAXN )
      DOUBLE PRECISION      DFDT   ( MAXN )
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      X0
      DOUBLE PRECISION      XD     ( MAXN )

      INTEGER               N

!
!     Open the test family.
!
      CALL TOPEN ( 'F_QDERIV' )

!
!     Compute machine epsilon and the horizontal scale
!     factor required for determining an appropriate value
!     of DELTA.
!
      EPSLON = 2.D0 ** (-53)

      SCALE  = EPSLON ** ( 1.D0 / 3.D0 )

!*********************************************************************
!*
!*    Normal cases
!*
!*********************************************************************

!
! --- Case 1 ------------------------------------------------------
!
      CALL TCASE ( 'Estimate the derivative of sin(x) at x = 0.'  )


      N     = 1
      DELTA = 1.D-6

      F0(1) = SIN( -DELTA )
      F2(1) = SIN(  DELTA )

      CALL QDERIV ( N, F0, F2, DELTA, DFDT )

      CALL CHCKXC ( .FALSE., ' ', OK )

      XD(1) = 1.D0

      CALL CHCKSD ( 'd(sin(x))/dt at x=0', DFDT(1), '~', &
                     XD(1),                DM11,    OK   )



!
! --- Case 2 ------------------------------------------------------
!
      CALL TCASE ( 'Estimate the derivative of sin(x) at x = pi/4.'  )


      N     = 1
      X0    = PI()/4
      DELTA = X0 * SCALE

      F0(1) = SIN( X0 - DELTA )
      F2(1) = SIN( X0 + DELTA )

      CALL QDERIV ( N, F0, F2, DELTA, DFDT )

      CALL CHCKXC ( .FALSE., ' ', OK )

      XD(1) = COS ( X0 )

      CALL CHCKSD ( 'd(sin(x))/dt at x = pi/4', DFDT(1), '~', &
                     XD(1),                      DM11,    OK   )


!
! --- Case 3 ------------------------------------------------------
!
      CALL TCASE ( 'Estimate the derivative of x2 at x = 2.'  )


      N     = 1
      DELTA = 1.D-3

      F0(1) = ( 2.D0 - DELTA ) ** 2.D0
      F2(1) = ( 2.D0 + DELTA ) ** 2.D0

      CALL QDERIV ( N, F0, F2, DELTA, DFDT )

      CALL CHCKXC ( .FALSE., ' ', OK )

      XD(1) = 4.D0

      CALL CHCKSD ( 'd(x**2)/dt at x=2', DFDT(1), '~', &
                     XD(1),              DM11,    OK   )



!
! --- Case 4 ------------------------------------------------------
!
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

      CALL CHCKAD ( 'Vector case: combine previous cases', DFDT, '~', &
                     XD,                 2,       DM8,    OK        )


!*********************************************************************
!*
!*    Error cases
!*
!*********************************************************************

!
! --- Case 5 ------------------------------------------------------
!
      CALL TCASE ( 'DELTA == 0'  )

      CALL QDERIV ( N, F0, F2, 0.D0, DFDT )

      CALL CHCKXC ( .TRUE., 'SPICE(DIVIDEBYZERO)', OK )

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


