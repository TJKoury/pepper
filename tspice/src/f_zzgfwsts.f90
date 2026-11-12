!$Procedure      F_ZZGFWSTS ( Test ZZGFWSTS )

      SUBROUTINE F_ZZGFWSTS ( OK )
      use pepper_legacy
      use pepper_inc_gf

! $ Abstract
!
!     Test the GF window sift routine ZZGFWSTS.
!
! $ Required_Reading
!
!     GF
!
! $ Keywords
!
!     GF
!     TEST
!     SEARCH
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
!     This family tests the GF window sift routine ZZGFWSTS.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     Progress reporting and interrupt handling are not currently tested
!     in this version of this this test family, but test cases for these
!     features should be added.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 26-SEP-2008 (NJB)
!
!-&


!
!     SPICELIB functions
!

!
!     Local parameters
!
      INTEGER               INCLEN
      PARAMETER           ( INCLEN = 2 )

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               MAXWIN
      PARAMETER           ( MAXWIN = 1000 )

      INTEGER               NOPS
      PARAMETER           ( NOPS   = 4 )
!
!     Local Variables
!
      CHARACTER*(INCLEN)    OPS    ( NOPS )
      CHARACTER*(LNSIZE)    QNAME

      DOUBLE PRECISION      DELTA1
      DOUBLE PRECISION      DELTA2
      DOUBLE PRECISION      FINISH
      DOUBLE PRECISION      START
      DOUBLE PRECISION      STEP1
      DOUBLE PRECISION      STEP2
      DOUBLE PRECISION      WNDW1  ( LBCELL : MAXWIN )
      DOUBLE PRECISION      WNDW2  ( LBCELL : MAXWIN )
      DOUBLE PRECISION      WNDW3  ( LBCELL : MAXWIN )
      DOUBLE PRECISION      XWIN   ( LBCELL : MAXWIN )

      INTEGER               CARD
      INTEGER               I
      INTEGER               J
      INTEGER               N
      INTEGER               XCARD

!
!     Saved everything
!
      SAVE

!
!     Initial values
!
      DATA                  OPS / '()', '[]', '[)', '(]' /

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_ZZGFWSTS' )




!*********************************************************************
!*
!*    Error cases
!*
!*********************************************************************

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Invalid inclusion operator'  )

      CALL SSIZED ( MAXWIN, WNDW1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZED ( MAXWIN, WNDW2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZED ( MAXWIN, WNDW3 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZGFWSTS ( WNDW1, WNDW2, ') )', WNDW3 )

      CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNINCLUSION)', OK )


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Window overflow'  )

      CALL SSIZED ( MAXWIN, WNDW1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZED ( MAXWIN, WNDW2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZED ( 2, WNDW3 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( 1.D0, 2.D0, WNDW1 )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( 3.D0, 4.D0, WNDW1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( 0.D0, 5.D0, WNDW2 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL ZZGFWSTS ( WNDW1, WNDW2, '[]', WNDW3 )

      CALL CHCKXC ( .TRUE., 'SPICE(OUTOFROOM)', OK )



!*********************************************************************
!*
!*    Normal cases
!*
!*********************************************************************






!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'First window precedes second; one interval each.' )

      CALL SSIZED ( MAXWIN, WNDW1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZED ( MAXWIN, WNDW2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZED ( MAXWIN, WNDW3 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL WNINSD ( 1.D0, 2.D0, WNDW1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( 3.D0, 4.D0, WNDW2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, NOPS

         CALL SCARDD ( 0, WNDW3 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL ZZGFWSTS ( WNDW1, WNDW2, OPS(I), WNDW3 )

         CALL CHCKXC ( .FALSE., ' ', OK )

         CARD = WNCARD( WNDW3 )

         QNAME = 'CARD (I=#)'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CHCKSI ( QNAME, CARD, '=', 0, 0, OK )

      END DO

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Second window precedes first; one interval each.' )

      CALL SSIZED ( MAXWIN, WNDW1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZED ( MAXWIN, WNDW2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZED ( MAXWIN, WNDW3 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL WNINSD ( 1.D0, 2.D0, WNDW2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( 3.D0, 4.D0, WNDW1 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      DO I = 1, NOPS

         CALL SCARDD ( 0, WNDW3 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL ZZGFWSTS ( WNDW1, WNDW2, OPS(I), WNDW3 )

         CALL CHCKXC ( .FALSE., ' ', OK )

         CARD = WNCARD( WNDW3 )

         QNAME = 'CARD (I=#)'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CHCKSI ( QNAME, CARD, '=', 0, 0, OK )

      END DO

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'First window properly contained in second; ' &
      //           'one interval each.' )

      CALL SSIZED ( MAXWIN, WNDW1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZED ( MAXWIN, WNDW2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZED ( MAXWIN, WNDW3 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL WNINSD (  1.D0, 2.D0, WNDW1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( -2.D0, 3.D0, WNDW2 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SSIZED ( MAXWIN, XWIN )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL COPYD  ( WNDW1, XWIN )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, NOPS

         CALL ZZGFWSTS ( WNDW1, WNDW2, OPS(I), WNDW3 )

         CALL CHCKXC ( .FALSE., ' ', OK )

         CARD = WNCARD( WNDW3 )

         QNAME = 'CARD (I=#)'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CHCKSI ( QNAME, CARD, '=', 1, 0, OK )


         QNAME = 'WNDW3 (I=#)'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CHCKAD ( QNAME, WNDW3, '=', XWIN, 3, 0.D0, OK )

      END DO


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Second window properly contained in first; ' &
      //           'one interval each.' )

      CALL SSIZED ( MAXWIN, WNDW1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZED ( MAXWIN, WNDW2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZED ( MAXWIN, WNDW3 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL WNINSD (  1.D0, 2.D0, WNDW2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( -2.D0, 3.D0, WNDW1 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SSIZED ( MAXWIN, XWIN )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL COPYD  ( WNDW2, XWIN )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, NOPS

         CALL ZZGFWSTS ( WNDW1, WNDW2, OPS(I), WNDW3 )

         CALL CHCKXC ( .FALSE., ' ', OK )

         CARD = WNCARD( WNDW3 )

         QNAME = 'CARD (I=#)'
         CALL REPMI ( QNAME, '#', I, QNAME )

!
!        We expect the sift result to be empty.
!
         CALL CHCKSI ( QNAME, CARD, '=', 0, 0, OK )

      END DO


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'First window''s endpoints match those of second; ' &
      //           'one interval each.' )

      CALL SSIZED ( MAXWIN, WNDW1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZED ( MAXWIN, WNDW2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZED ( MAXWIN, WNDW3 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL WNINSD (  1.D0, 2.D0, WNDW1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL COPYD  ( WNDW1,   WNDW2   )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SSIZED ( MAXWIN, XWIN )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL COPYD  ( WNDW1, XWIN )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, NOPS

         CALL ZZGFWSTS ( WNDW1, WNDW2, OPS(I), WNDW3 )

         CALL CHCKXC ( .FALSE., ' ', OK )

         CARD = WNCARD( WNDW3 )

!
!        We have inclusion only if the second window's interval
!        is treated as being closed.
!
         IF ( OPS(I) .EQ. '[]' ) THEN
            XCARD = 1
         ELSE
            XCARD = 0
         END IF

         QNAME = 'CARD (I=#)'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CHCKSI ( QNAME, CARD, '=', XCARD, 0, OK )

         IF ( XCARD .GT. 0 ) THEN

            QNAME = 'WNDW3 (I=#)'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL CHCKAD ( QNAME, WNDW3, '=', XWIN, 3, 0.D0, OK )

         END IF

      END DO


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'First window''s left endpoint matches that ' &
      //           'of second; one interval each.'               )

      CALL SSIZED ( MAXWIN, WNDW1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZED ( MAXWIN, WNDW2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZED ( MAXWIN, WNDW3 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL WNINSD (  1.D0, 2.D0, WNDW1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD (  1.D0, 3.D0, WNDW2 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SSIZED ( MAXWIN, XWIN )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL COPYD  ( WNDW1, XWIN )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, NOPS

         CALL ZZGFWSTS ( WNDW1, WNDW2, OPS(I), WNDW3 )

         CALL CHCKXC ( .FALSE., ' ', OK )

         CARD = WNCARD( WNDW3 )

!
!        We have inclusion only if the second window's interval
!        is treated as being closed on the left.
!
         IF ( OPS(I) .EQ. '[]' ) THEN
            XCARD = 1
         ELSE IF ( OPS(I) .EQ. '[)' ) THEN
            XCARD = 1
         ELSE
            XCARD = 0
         END IF

         QNAME = 'CARD (I=#)'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CHCKSI ( QNAME, CARD, '=', XCARD, 0, OK )

         IF ( XCARD .GT. 0 ) THEN

            QNAME = 'WNDW3 (I=#)'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL CHCKAD ( QNAME, WNDW3, '=', XWIN, 3, 0.D0, OK )

         END IF

      END DO


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'First window''s right endpoint matches that ' &
      //           'of second; one interval each.'               )

      CALL SSIZED ( MAXWIN, WNDW1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZED ( MAXWIN, WNDW2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZED ( MAXWIN, WNDW3 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL WNINSD (  2.D0, 3.D0, WNDW1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD (  1.D0, 3.D0, WNDW2 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SSIZED ( MAXWIN, XWIN )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL COPYD  ( WNDW1, XWIN )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, NOPS

         CALL ZZGFWSTS ( WNDW1, WNDW2, OPS(I), WNDW3 )

         CALL CHCKXC ( .FALSE., ' ', OK )

         CARD = WNCARD( WNDW3 )

!
!        We have inclusion only if the second window's interval
!        is treated as being closed on the right.
!
         IF ( OPS(I) .EQ. '[]' ) THEN
            XCARD = 1
         ELSE IF ( OPS(I) .EQ. '(]' ) THEN
            XCARD = 1
         ELSE
            XCARD = 0
         END IF

         QNAME = 'CARD (I=#)'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CHCKSI ( QNAME, CARD, '=', XCARD, 0, OK )

         IF ( XCARD .GT. 0 ) THEN

            QNAME = 'WNDW3 (I=#)'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL CHCKAD ( QNAME, WNDW3, '=', XWIN, 3, 0.D0, OK )

         END IF

      END DO

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Multiple WNDW1 intervals in each WNDW2 interval.' )

      CALL SSIZED ( MAXWIN, WNDW1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZED ( MAXWIN, WNDW2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZED ( MAXWIN, WNDW3 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      STEP1  = 1.D0
      DELTA1 = 0.2D0

      STEP2  = 10.D0
      DELTA2 =  4.D0


!
!     Create second window.
!
      N = 20

      DO I = 1, N

         START  = I*STEP2 - DELTA2
         FINISH = I*STEP2 + DELTA2

         CALL WNINSD (  START, FINISH, WNDW2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

!
!     Create first window. There are 7 small intervals
!     in each interval of the large window.
!
      DO I = 1, N

         DO J = -3, 3

            START  = I*STEP2 + J*STEP1 - DELTA1
            FINISH = I*STEP2 + J*STEP1 + DELTA1

            CALL WNINSD (  START, FINISH, WNDW1 )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

      END DO


      CALL SSIZED ( MAXWIN, XWIN )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL COPYD  ( WNDW1, XWIN )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL ZZGFWSTS ( WNDW1, WNDW2, '[]', WNDW3 )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CARD = WNCARD( WNDW3 )

      QNAME = 'CARD'
      CALL REPMI ( QNAME, '#', I, QNAME )

      CALL CHCKSI ( QNAME, CARD, '=', N*7, 0, OK )


      QNAME = 'WNDW3'
      CALL REPMI ( QNAME, '#', I, QNAME )

      CALL CHCKAD ( QNAME, WNDW3, '=', XWIN, 3, 0.D0, OK )


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'WNDW2 intervals span multiple WNDW1 intervals.' )

!
!     Use the input windows from the previous test.
!

      CALL ZZGFWSTS ( WNDW2, WNDW1, '[]', WNDW3 )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CARD = WNCARD( WNDW3 )

      QNAME = 'CARD'
      CALL REPMI ( QNAME, '#', I, QNAME )

      CALL CHCKSI ( QNAME, CARD, '=', 0, 0, OK )



!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Multiple WNDW1 intervals in each WNDW2 interval; ' &
      //           'some intervals not contained.'                    )

      CALL SSIZED ( MAXWIN, WNDW1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZED ( MAXWIN, WNDW2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZED ( MAXWIN, WNDW3 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      STEP1  = 1.D0
      DELTA1 = 0.2D0

      STEP2  = 10.D0
      DELTA2 =  4.D0


!
!     Create second window.
!
      N = 20

      DO I = 1, N

         START  = I*STEP2 - DELTA2
         FINISH = I*STEP2 + DELTA2

         CALL WNINSD (  START, FINISH, WNDW2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

!
!     Create first window. There is a sequence of 9 small intervals
!     centered at the center of each interval of the large window;
!     the intervals at the ends of each of these sequences extend
!     beyond the corresponding interval of the second window.
!
      CALL SSIZED ( MAXWIN, XWIN )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, N

         DO J = -4, 4

            START  = I*STEP2 + J*STEP1 - DELTA1
            FINISH = I*STEP2 + J*STEP1 + DELTA1

            CALL WNINSD (  START, FINISH, WNDW1 )
            CALL CHCKXC ( .FALSE., ' ', OK )

            IF ( ( J .GT. -4 ) .OR. ( J .LT. 4 ) ) THEN

               CALL WNINSD (  START, FINISH, XWIN )
               CALL CHCKXC ( .FALSE., ' ', OK )

            END IF

         END DO

      END DO


      CALL ZZGFWSTS ( WNDW1, WNDW2, '[]', WNDW3 )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CARD = WNCARD( WNDW3 )

      QNAME = 'CARD'
      CALL REPMI ( QNAME, '#', I, QNAME )

      CALL CHCKSI ( QNAME, CARD, '=', N*7, 0, OK )


      QNAME = 'WNDW3'
      CALL REPMI ( QNAME, '#', I, QNAME )

      CALL CHCKAD ( QNAME, WNDW3, '=', XWIN, 3, 0.D0, OK )


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Multiple WNDW1 intervals between each ' &
      //           'WNDW2 interval.'                       )

      CALL SSIZED ( MAXWIN, WNDW1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZED ( MAXWIN, WNDW2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZED ( MAXWIN, WNDW3 )
      CALL CHCKXC ( .FALSE., ' ', OK )



      STEP1  = 1.D0
      DELTA1 = 0.2D0

      STEP2  = 10.D0
      DELTA2 =  1.D0


!
!     Create second window.
!
      N = 20

      DO I = 1, N

         START  = I*STEP2 - DELTA2
         FINISH = I*STEP2 + DELTA2

         CALL WNINSD (  START, FINISH, WNDW2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

!
!     Create first window. There are 7 small intervals
!     between each interval of the large window.
!
      DO I = 1, N-1

         DO J = -3, 3

            START  = (I+0.5D0)*STEP2 + J*STEP1 - DELTA1
            FINISH = (I+0.5D0)*STEP2 + J*STEP1 + DELTA1

            CALL WNINSD (  START, FINISH, WNDW1 )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

      END DO


      CALL ZZGFWSTS ( WNDW1, WNDW2, '[]', WNDW3 )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CARD = WNCARD( WNDW3 )

      QNAME = 'CARD'
      CALL REPMI ( QNAME, '#', I, QNAME )

      CALL CHCKSI ( QNAME, CARD, '=', 0, 0, OK )


      QNAME = 'WNDW3'
      CALL REPMI ( QNAME, '#', I, QNAME )

      CALL CHCKAD ( QNAME, WNDW3, '=', XWIN, 3, 0.D0, OK )


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Valid inclusion operators with blanks' )

      CALL WNINSD (  1.D0, 2.D0, WNDW1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( -2.D0, 3.D0, WNDW1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZGFWSTS ( WNDW2, WNDW1, '  (   ) ', WNDW3 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZGFWSTS ( WNDW2, WNDW1, '  [   ] ', WNDW3 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZGFWSTS ( WNDW2, WNDW1, '  [   ) ', WNDW3 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZGFWSTS ( WNDW2, WNDW1, '  (   ] ', WNDW3 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      RETURN
      END





