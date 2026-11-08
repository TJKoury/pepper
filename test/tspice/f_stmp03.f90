!$Procedure F_STMP03 ( STMP03 tests )

      SUBROUTINE F_STMP03 ( OK )

! $ Abstract
!
!     Exercise the SPICELIB Stumpff function routine STMP03.
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
!     This routine tests the SPICELIB routine
!
!        STMP03
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
!-    TSPICE Version 1.0.0, 23-APR-2014 (NJB)
!
!-&

!
!     SPICELIB functions
!
      DOUBLE PRECISION      DPMAX


!
!     Local Parameters
!

      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 240 )

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-12 )

!
!     Local Variables
!
      CHARACTER*(MSGLEN)    TITLE

      DOUBLE PRECISION      C0
      DOUBLE PRECISION      C1
      DOUBLE PRECISION      C2
      DOUBLE PRECISION      C3
      DOUBLE PRECISION      LBOUND
      DOUBLE PRECISION      UBOUND
      DOUBLE PRECISION      X
      DOUBLE PRECISION      XC0
      DOUBLE PRECISION      XC1
      DOUBLE PRECISION      XC2
      DOUBLE PRECISION      XC3
      DOUBLE PRECISION      Z

      INTEGER               I

!
!     Open the test family.
!
      CALL TOPEN ( 'F_STMP03' )

!**********************************************************************
!
!     Error cases
!
!**********************************************************************


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Argument too large.' )

!
!     LBOUND is an approximate lower bound on the input
!     values for which the Stumpff functions can be computed.
!
      LBOUND = - (  LOG(2.D0) + LOG( DPMAX() )  )**2

      X      = 2 * LBOUND

      CALL STMP03 ( X, C0, C1, C2, C3 )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


!**********************************************************************
!
!     Normal cases
!
!**********************************************************************


!
!     Test STMP03 for values of X less than -1.
!
      X = -1.D0 - 1.D-15

      DO WHILE ( X .GT. LBOUND )

!
! --- Case: ------------------------------------------------------
!
         TITLE = 'X = #'
         CALL REPMD  ( TITLE, '#', X, 14, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL TCASE ( TITLE )


         CALL STMP03 ( X, C0, C1, C2, C3 )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Generate expected results.
!
         Z   = SQRT ( -X )
         XC0 = COSH (  Z )
         XC1 = SINH (  Z )    / Z
         XC2 = ( 1.D0 - XC0 ) / X
         XC3 = ( 1.D0 - XC1 ) / X


         CALL CHCKSD ( 'C0', C0, '~/', XC0,  TIGHT, OK )
         CALL CHCKSD ( 'C1', C1, '~/', XC1,  TIGHT, OK )
         CALL CHCKSD ( 'C2', C2, '~/', XC2,  TIGHT, OK )
         CALL CHCKSD ( 'C3', C3, '~/', XC3,  TIGHT, OK )

         X = X * 1.5D0

      END DO


!
!     Test STMP03 for values of X between -1 and 0.
!
      X = -1.D0

      DO WHILE ( X .LT. -1.D-15 )

!
! --- Case: ------------------------------------------------------
!
         TITLE = 'X = #'
         CALL REPMD  ( TITLE, '#', X, 14, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL TCASE ( TITLE )


         CALL STMP03 ( X, C0, C1, C2, C3 )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Generate expected results.
!
         Z   = SQRT ( -X )
         XC0 = COSH (  Z )
         XC1 = SINH (  Z ) / Z

         CALL CHCKSD ( 'C0', C0, '~/', XC0,  TIGHT, OK )
         CALL CHCKSD ( 'C1', C1, '~/', XC1,  TIGHT, OK )

         IF ( X .LT. -1.D-2 ) THEN

            XC2 = ( 1.D0 - XC0 ) / X
            XC3 = ( 1.D0 - XC1 ) / X

            CALL CHCKSD ( 'C2', C2, '~/', XC2,  TIGHT, OK )
            CALL CHCKSD ( 'C3', C3, '~/', XC3,  TIGHT, OK )

         ELSE
!
!           Compute the C3 and C2 series directly. Group terms to avoid
!           loss of precision.
!
            XC3 = 1.D0 - ( X / (18 * 19) )

            DO I = 1, 7

               XC3 = 1.D0 - ( X / ((18-2*I)*(19-2*I)) ) * XC3

            END DO

            XC3 = XC3 / 6

            CALL CHCKSD ( 'C3', C3, '~/', XC3,  TIGHT, OK )


            XC2 = 1.D0 - ( X / (17 * 18) )

            DO I = 1, 7

               XC2 = 1.D0 - ( X / ((17-2*I)*(18-2*I)) ) * XC2

            END DO

            XC2 = XC2 / 2

            CALL CHCKSD ( 'C2', C2, '~/', XC2,  TIGHT, OK )

         END IF

         X = X * 0.3D0

      END DO

!
!     Test STMP03 for values of X greater than 1.
!
      UBOUND = DPMAX() / 10

      X      = 1.D0 + 1.D-15

      DO WHILE ( X .LT. UBOUND )

!
! --- Case: ------------------------------------------------------
!
         TITLE = 'X = #'
         CALL REPMD  ( TITLE, '#', X, 14, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL TCASE ( TITLE )



         CALL STMP03 ( X, C0, C1, C2, C3 )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Generate expected results.
!
         Z   = SQRT ( X )

         XC0 = COS  (  Z )
         XC1 = SIN  (  Z )    / Z
         XC2 = ( 1.D0 - XC0 ) / X
         XC3 = ( 1.D0 - XC1 ) / X

         CALL CHCKSD ( 'C0', C0, '~/', XC0,  TIGHT, OK )
         CALL CHCKSD ( 'C1', C1, '~/', XC1,  TIGHT, OK )
         CALL CHCKSD ( 'C2', C2, '~/', XC2,  TIGHT, OK )
         CALL CHCKSD ( 'C3', C3, '~/', XC3,  TIGHT, OK )

         X = X * 7

      END DO


!
!     Test STMP03 for values of X between 0 and 1.
!
      UBOUND = DPMAX() / 10

      X      =  1.D-15

      DO WHILE ( X .LE. 1.D0 )

!
! --- Case: ------------------------------------------------------
!
         TITLE = 'X = #'
         CALL REPMD  ( TITLE, '#', X, 14, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL TCASE ( TITLE )



         CALL STMP03 ( X, C0, C1, C2, C3 )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Generate expected results.
!
         Z   = SQRT ( X )
         XC0 = COS  ( Z )
         XC1 = SIN  ( Z )     / Z

         CALL CHCKSD ( 'C0', C0, '~/', XC0,  TIGHT, OK )
         CALL CHCKSD ( 'C1', C1, '~/', XC1,  TIGHT, OK )


         IF ( X .GT. 1.D-2 ) THEN

            XC2 = ( 1.D0 - XC0 ) / X
            XC3 = ( 1.D0 - XC1 ) / X

            CALL CHCKSD ( 'C2', C2, '~/', XC2,  TIGHT, OK )
            CALL CHCKSD ( 'C3', C3, '~/', XC3,  TIGHT, OK )

         ELSE
!
!           Compute the C3 and C2 series directly. Group terms to avoid
!           loss of precision.
!
            XC3 = 1.D0 - ( X / (18 * 19) )

            DO I = 1, 7

               XC3 = 1.D0 - ( X / ((18-2*I)*(19-2*I)) ) * XC3

            END DO

            XC3 = XC3 / 6

            CALL CHCKSD ( 'C3', C3, '~/', XC3,  TIGHT, OK )


            XC2 = 1.D0 - ( X / (17 * 18) )

            DO I = 1, 7

               XC2 = 1.D0 - ( X / ((17-2*I)*(18-2*I)) ) * XC2

            END DO

            XC2 = XC2 / 2

            CALL CHCKSD ( 'C2', C2, '~/', XC2,  TIGHT, OK )


         END IF


         X = X * 3

      END DO


!
!     Test STMP03 for values of X = 0.
!
      X = 0.D0

!
! --- Case: ------------------------------------------------------
!
      TITLE = 'X = #'
      CALL REPMD  ( TITLE, '#', X, 14, TITLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL TCASE ( TITLE )


      CALL STMP03 ( X, C0, C1, C2, C3 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Generate expected results.
!
      XC0 = 1.D0
      XC1 = 1.D0
      XC2 = 0.5D0
      XC3 = 1.D0/6


      CALL CHCKSD ( 'C0', C0, '~/', XC0,  TIGHT, OK )
      CALL CHCKSD ( 'C1', C1, '~/', XC1,  TIGHT, OK )
      CALL CHCKSD ( 'C2', C2, '~/', XC2,  TIGHT, OK )
      CALL CHCKSD ( 'C3', C3, '~/', XC3,  TIGHT, OK )
!
! ---------------------------------------------------------
!
!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END
