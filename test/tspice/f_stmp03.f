C$Procedure F_STMP03 ( STMP03 tests )
 
      SUBROUTINE F_STMP03 ( OK )
 
C$ Abstract
C
C     Exercise the SPICELIB Stumpff function routine STMP03.
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
C     This routine tests the SPICELIB routine
C
C        STMP03
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
C-    TSPICE Version 1.0.0, 23-APR-2014 (NJB)
C
C-&
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      DPMAX
 

C
C     Local Parameters
C

      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 240 )
 
      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-12 )

C
C     Local Variables
C      
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

C
C     Open the test family.
C
      CALL TOPEN ( 'F_STMP03' )
 
C**********************************************************************
C
C     Error cases
C
C**********************************************************************


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Argument too large.' )
 
C
C     LBOUND is an approximate lower bound on the input
C     values for which the Stumpff functions can be computed.
C
      LBOUND = - (  LOG(2.D0) + LOG( DPMAX() )  )**2 

      X      = 2 * LBOUND

      CALL STMP03 ( X, C0, C1, C2, C3 )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


C**********************************************************************
C
C     Normal cases
C
C**********************************************************************


C
C     Test STMP03 for values of X less than -1.
C
      X = -1.D0 - 1.D-15

      DO WHILE ( X .GT. LBOUND )

C
C --- Case: ------------------------------------------------------
C
         TITLE = 'X = #'
         CALL REPMD  ( TITLE, '#', X, 14, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL TCASE ( TITLE )


         CALL STMP03 ( X, C0, C1, C2, C3 )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Generate expected results.
C
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


C
C     Test STMP03 for values of X between -1 and 0.
C      
      X = -1.D0 

      DO WHILE ( X .LT. -1.D-15 )

C
C --- Case: ------------------------------------------------------
C
         TITLE = 'X = #'
         CALL REPMD  ( TITLE, '#', X, 14, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL TCASE ( TITLE )


         CALL STMP03 ( X, C0, C1, C2, C3 )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Generate expected results.
C
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
C
C           Compute the C3 and C2 series directly. Group terms to avoid
C           loss of precision.
C
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
       
C
C     Test STMP03 for values of X greater than 1.
C
      UBOUND = DPMAX() / 10

      X      = 1.D0 + 1.D-15

      DO WHILE ( X .LT. UBOUND )

C
C --- Case: ------------------------------------------------------
C
         TITLE = 'X = #'
         CALL REPMD  ( TITLE, '#', X, 14, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL TCASE ( TITLE )



         CALL STMP03 ( X, C0, C1, C2, C3 )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Generate expected results.
C
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


C
C     Test STMP03 for values of X between 0 and 1.
C
      UBOUND = DPMAX() / 10

      X      =  1.D-15

      DO WHILE ( X .LE. 1.D0 )

C
C --- Case: ------------------------------------------------------
C
         TITLE = 'X = #'
         CALL REPMD  ( TITLE, '#', X, 14, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL TCASE ( TITLE )



         CALL STMP03 ( X, C0, C1, C2, C3 )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Generate expected results.
C
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
C
C           Compute the C3 and C2 series directly. Group terms to avoid
C           loss of precision.
C
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


C
C     Test STMP03 for values of X = 0.
C
      X = 0.D0

C
C --- Case: ------------------------------------------------------
C
      TITLE = 'X = #'
      CALL REPMD  ( TITLE, '#', X, 14, TITLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL TCASE ( TITLE )


      CALL STMP03 ( X, C0, C1, C2, C3 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Generate expected results.
C
      XC0 = 1.D0
      XC1 = 1.D0
      XC2 = 0.5D0
      XC3 = 1.D0/6


      CALL CHCKSD ( 'C0', C0, '~/', XC0,  TIGHT, OK )
      CALL CHCKSD ( 'C1', C1, '~/', XC1,  TIGHT, OK )
      CALL CHCKSD ( 'C2', C2, '~/', XC2,  TIGHT, OK )
      CALL CHCKSD ( 'C3', C3, '~/', XC3,  TIGHT, OK )
C
C ---------------------------------------------------------
C
C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END
