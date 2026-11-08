C$Procedure      F_ZZGFWSTS ( Test ZZGFWSTS )
  
      SUBROUTINE F_ZZGFWSTS ( OK )

C$ Abstract
C
C     Test the GF window sift routine ZZGFWSTS.
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
C     GF
C
C$ Keywords
C
C     GF
C     TEST
C     SEARCH
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE              'gf.inc'

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
C     This family tests the GF window sift routine ZZGFWSTS.
C      
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     Progress reporting and interrupt handling are not currently tested
C     in this version of this this test family, but test cases for these
C     features should be added.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 26-SEP-2008 (NJB)
C
C-&
 
  
C
C     SPICELIB functions
C
      INTEGER               WNCARD

C     
C     Local parameters
C
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
C
C     Local Variables
C
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
  
C
C     Saved everything
C
      SAVE

C
C     Initial values
C
      DATA                  OPS / '()', '[]', '[)', '(]' /

C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_ZZGFWSTS' )




C*********************************************************************
C*
C*    Error cases
C*
C*********************************************************************

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Invalid inclusion operator'  )

      CALL SSIZED ( MAXWIN, WNDW1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZED ( MAXWIN, WNDW2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZED ( MAXWIN, WNDW3 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL ZZGFWSTS ( WNDW1, WNDW2, ') )', WNDW3 )

      CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNINCLUSION)', OK )

      
C
C---- Case -------------------------------------------------------------
C
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



C*********************************************************************
C*
C*    Normal cases
C*
C*********************************************************************






C
C---- Case -------------------------------------------------------------
C
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
      
C
C---- Case -------------------------------------------------------------
C
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
      
C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'First window properly contained in second; '
     .//           'one interval each.' )
  
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
      

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Second window properly contained in first; '
     .//           'one interval each.' )
  
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

C
C        We expect the sift result to be empty.
C
         CALL CHCKSI ( QNAME, CARD, '=', 0, 0, OK )

      END DO
      

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'First window''s endpoints match those of second; '
     .//           'one interval each.' )
  
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

C
C        We have inclusion only if the second window's interval
C        is treated as being closed.
C
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


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'First window''s left endpoint matches that ' 
     .//           'of second; one interval each.'               )
  
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

C
C        We have inclusion only if the second window's interval
C        is treated as being closed on the left.
C
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


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'First window''s right endpoint matches that ' 
     .//           'of second; one interval each.'               )
  
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

C
C        We have inclusion only if the second window's interval
C        is treated as being closed on the right.
C
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

C
C---- Case -------------------------------------------------------------
C
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


C
C     Create second window.
C
      N = 20

      DO I = 1, N

         START  = I*STEP2 - DELTA2
         FINISH = I*STEP2 + DELTA2

         CALL WNINSD (  START, FINISH, WNDW2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

C
C     Create first window. There are 7 small intervals
C     in each interval of the large window.
C
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


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'WNDW2 intervals span multiple WNDW1 intervals.' )

C
C     Use the input windows from the previous test.
C

      CALL ZZGFWSTS ( WNDW2, WNDW1, '[]', WNDW3 )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CARD = WNCARD( WNDW3 )

      QNAME = 'CARD'
      CALL REPMI ( QNAME, '#', I, QNAME )

      CALL CHCKSI ( QNAME, CARD, '=', 0, 0, OK )

 

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Multiple WNDW1 intervals in each WNDW2 interval; '
     .//           'some intervals not contained.'                    )
  
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


C
C     Create second window.
C
      N = 20

      DO I = 1, N

         START  = I*STEP2 - DELTA2
         FINISH = I*STEP2 + DELTA2

         CALL WNINSD (  START, FINISH, WNDW2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

C
C     Create first window. There is a sequence of 9 small intervals
C     centered at the center of each interval of the large window;
C     the intervals at the ends of each of these sequences extend
C     beyond the corresponding interval of the second window.
C
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


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Multiple WNDW1 intervals between each '
     .//           'WNDW2 interval.'                       )
  
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


C
C     Create second window.
C
      N = 20

      DO I = 1, N

         START  = I*STEP2 - DELTA2
         FINISH = I*STEP2 + DELTA2

         CALL WNINSD (  START, FINISH, WNDW2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

C
C     Create first window. There are 7 small intervals
C     between each interval of the large window.
C
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


C
C---- Case -------------------------------------------------------------
C
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





