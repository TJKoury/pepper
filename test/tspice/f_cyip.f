C$Procedure F_CYIP ( Cycle array in place tests )
 
      SUBROUTINE F_CYIP ( OK )
 
C$ Abstract
C
C     Exercise routines that cycle arrays in place.
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
C$ Files
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
C$ Particulars
C
C     This routine tests the in-place variants of the array
C     cycling routines.  The routines exercised by this family
C     are:
C
C        CYACIP
C        CYADIP
C        CYAIIP
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman     (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    TSPICE Version 1.0.0, 20-OCT-2005 (NJB)
C
C
C-&
 
C
C     SPICELIB functions
C

C
C     Local Parameters
C
      INTEGER               CSIZE
      PARAMETER           ( CSIZE   = 10 )

      INTEGER               DSIZE
      PARAMETER           ( DSIZE   = 100 )

      INTEGER               ISIZE
      PARAMETER           ( ISIZE   = 25 )

      INTEGER               STLEN
      PARAMETER           ( STLEN   = 80 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE  = 80 )

C
C     Local Variables
C
      CHARACTER*(STLEN)     CARRAY ( CSIZE )
      CHARACTER*(STLEN)     C2ARRY ( CSIZE )
      CHARACTER*(LNSIZE)    LABEL
      CHARACTER*(STLEN)     XCARRY ( CSIZE )


      DOUBLE PRECISION      DARRAY ( DSIZE )
      DOUBLE PRECISION      D2ARRY ( DSIZE )
      DOUBLE PRECISION      XDARRY ( DSIZE )

      INTEGER               I
      INTEGER               IARRAY ( ISIZE )
      INTEGER               I2ARRY ( ISIZE )
      INTEGER               J
      INTEGER               Q
      INTEGER               R
      INTEGER               XIARRY ( ISIZE )

C
C     Open the test family.
C
      CALL TOPEN ( 'F_CYIP' )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Test CYAIIP:  cycle forward.' )

      DO I = 1, ISIZE
         IARRAY(I) = I
      END DO


      DO I = 1, 2 * ISIZE
C
C        Use CYCLAI to find the expected result.
C
         CALL CYCLAI ( IARRAY, ISIZE, 'F', I, XIARRY )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Also cycle the array in place.     
C
         CALL MOVEI ( IARRAY, ISIZE, I2ARRY )

         CALL CYAIIP ( ISIZE, 'F', I, I2ARRY )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAI ( 'I2ARRY', I2ARRY, '=', XIARRY, ISIZE, OK )

C
C        Just to be careful, let's test CYCLAI itself.  Create
C        the expected result of cycling the array forward by I.
C
         CALL CLEARI ( ISIZE, I2ARRY )

         DO J = 1, ISIZE
C
C           Find the target location of the Jth element of IARRAY.
C            
            CALL RMAINI ( J + I, ISIZE, Q, R )

            IF ( R .EQ. 0 ) THEN
               R = ISIZE
            END IF

            I2ARRY(R) = IARRAY(J)

         END DO

         CALL CHCKAI ( 'I2ARRY', I2ARRY, '=', XIARRY, ISIZE, OK )

C
C        Cycle backward by -I as well.
C        
         CALL CYCLAI ( IARRAY, ISIZE, 'B', -I, XIARRY )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAI ( 'I2ARRY', I2ARRY, '=', XIARRY, ISIZE, OK )

C
C        Cycle in the opposite direction using a negative count.        
C
         CALL MOVEI ( IARRAY, ISIZE,   I2ARRY )
         CALL CYAIIP ( ISIZE, 'B', -I, I2ARRY )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAI ( 'I2ARRY', I2ARRY, '=', XIARRY, ISIZE, OK )

      END DO


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Test CYAIIP:  cycle backward.' )

      DO I = 1, 2 * ISIZE
C
C        Use CYCLAI to find the expected result.
C
         CALL CYCLAI ( IARRAY, ISIZE, 'B', I, XIARRY )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Also cycle the array in place.     
C
         CALL MOVEI ( IARRAY, ISIZE, I2ARRY )

         CALL CYAIIP ( ISIZE, 'B', I, I2ARRY )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAI ( 'I2ARRY', I2ARRY, '=', XIARRY, ISIZE, OK )

C
C        Just to be careful, let's test CYCLAI itself.  Create
C        the expected result of cycling the array backward by I.
C
         CALL CLEARI ( ISIZE, I2ARRY )

         DO J = 1, ISIZE
C
C           Find the target location of the Jth element of IARRAY.
C            
            CALL RMAINI ( J - I, ISIZE, Q, R )

            IF ( R .EQ. 0 ) THEN
               R = ISIZE
            END IF

            I2ARRY(R) = IARRAY(J)

         END DO

         CALL CHCKAI ( 'I2ARRY', I2ARRY, '=', XIARRY, ISIZE, OK )


C
C        Cycle forward by -I as well.
C        
         CALL CYCLAI ( IARRAY, ISIZE, 'F', -I, XIARRY )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAI ( 'I2ARRY', I2ARRY, '=', XIARRY, ISIZE, OK )

C
C        Cycle in the opposite direction using a negative count.        
C
         CALL MOVEI ( IARRAY, ISIZE,   I2ARRY )
         CALL CYAIIP ( ISIZE, 'F', -I, I2ARRY )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAI ( 'I2ARRY', I2ARRY, '=', XIARRY, ISIZE, OK )

      END DO

C
C --- Case: ------------------------------------------------------
C
C
C     When the array size is non-positive, the call should be
C     a no-op.
C
C
C     Use a negative size.
C
      CALL TCASE ( 'Error case:  non-positive array size' )

      CALL MOVEI ( IARRAY, ISIZE, I2ARRY )
  
      CALL CYAIIP ( -ISIZE, 'F', -5, I2ARRY )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAI ( 'I2ARRY', I2ARRY, '=', IARRAY, ISIZE, OK )


C
C     Use size zero.
C
      CALL MOVEI ( IARRAY, ISIZE, I2ARRY )

      CALL CYAIIP (  0, 'F', -5, I2ARRY )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAI ( 'I2ARRY', I2ARRY, '=', IARRAY, ISIZE, OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error case:  invalid direction.' )
      
      
      CALL CYAIIP ( ISIZE, 'Z', -5, I2ARRY )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIRECTION)', OK )





C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Test CYADIP:  cycle forward.' )

      DO I = 1, DSIZE
         DARRAY(I) = I
      END DO


      DO I = 1, 2 * DSIZE
C
C        Use CYCLAD to find the expected result.
C
         CALL CYCLAD ( DARRAY, DSIZE, 'F', I, XDARRY )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Also cycle the array in place.     
C
         CALL MOVED ( DARRAY, DSIZE, D2ARRY )

         CALL CYADIP ( DSIZE, 'F', I, D2ARRY )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( 'D2ARRY', D2ARRY, '=', XDARRY, DSIZE, 0.D0, OK )


C
C        Just to be careful, let's test CYCLAD itself.  Create
C        the expected result of cycling the array forward by I.
C
         CALL CLEARD ( DSIZE, D2ARRY )

         DO J = 1, DSIZE
C
C           Find the target location of the Jth element of DARRAY.
C            
            CALL RMAINI ( J + I, DSIZE, Q, R )

            IF ( R .EQ. 0 ) THEN
               R = DSIZE
            END IF

            D2ARRY(R) = DARRAY(J)

         END DO

         CALL CHCKAD ( 'D2ARRY', D2ARRY, '=', XDARRY, DSIZE, 0.D0, OK )

C
C        Cycle backward by -I as well.
C        
         CALL CYCLAD ( DARRAY, DSIZE, 'B', -I, XDARRY )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( 'D2ARRY', D2ARRY, '=', XDARRY, DSIZE, 0.D0, OK )

C
C        Cycle in the opposite direction using a negative count.        
C
         CALL MOVED  ( DARRAY, DSIZE,  D2ARRY )
         CALL CYADIP ( DSIZE, 'B', -I, D2ARRY )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( 'D2ARRY', D2ARRY, '=', XDARRY, DSIZE, 0.D0, OK )

      END DO


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Test CYADIP:  cycle backward.' )

      DO I = 1, 2 * DSIZE
C
C        Use CYCLAD to find the expected result.
C
         CALL CYCLAD ( DARRAY, DSIZE, 'B', I, XDARRY )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Also cycle the array in place.     
C
         CALL MOVED  ( DARRAY, DSIZE, D2ARRY )

         CALL CYADIP ( DSIZE, 'B', I, D2ARRY )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( 'D2ARRY', D2ARRY, '=', XDARRY, DSIZE, 0.D0, OK )

C
C        Just to be careful, let's test CYCLAD itself.  Create
C        the expected result of cycling the array backward by I.
C
         CALL CLEARD ( DSIZE, D2ARRY )

         DO J = 1, DSIZE
C
C           Find the target location of the Jth element of DARRAY.
C            
            CALL RMAINI ( J - I, DSIZE, Q, R )

            IF ( R .EQ. 0 ) THEN
               R = DSIZE
            END IF

            D2ARRY(R) = DARRAY(J)

         END DO

         CALL CHCKAD ( 'D2ARRY', D2ARRY, '=', XDARRY, DSIZE, 0.D0, OK )

C
C        Cycle forward by -I as well.
C        
         CALL CYCLAD ( DARRAY, DSIZE, 'F', -I, XDARRY )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( 'D2ARRY', D2ARRY, '=', XDARRY, DSIZE, 0.D0, OK )

C
C        Cycle in the opposite direction using a negative count.        
C
         CALL MOVED  ( DARRAY, DSIZE,  D2ARRY )
         CALL CYADIP ( DSIZE, 'F', -I, D2ARRY )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( 'D2ARRY', D2ARRY, '=', XDARRY, DSIZE, 0.D0, OK )

      END DO

C
C --- Case: ------------------------------------------------------
C
C
C     When the array size is non-positive, the call should be
C     a no-op.
C
C
C     Use a negative size.
C
      CALL TCASE ( 'Error case:  non-positive array size' )

      CALL MOVED ( DARRAY, DSIZE, D2ARRY )
  
      CALL CYADIP ( -DSIZE, 'F', -5, D2ARRY )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'D2ARRY', D2ARRY, '=', DARRAY, DSIZE, 0.D0, OK )


C
C     Use size zero.
C
      CALL MOVED ( DARRAY, DSIZE, D2ARRY )

      CALL CYADIP (  0, 'F', -5, D2ARRY )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'D2ARRY', D2ARRY, '=', DARRAY, DSIZE, 0.D0, OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error case:  invalid direction.' )
      
      
      CALL CYADIP ( DSIZE, 'Z', -5, D2ARRY )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIRECTION)', OK )







C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Test CYACIP:  cycle forward.' )

      DO I = 1, CSIZE
         CALL INTSTR ( I, CARRAY(I) )
      END DO


      DO I = 1, 2 * CSIZE
C
C        Use CYCLAC to find the expected result.
C
         CALL CYCLAC ( CARRAY, CSIZE, 'F', I, XCARRY )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Also cycle the array in place.     
C
         CALL MOVEC ( CARRAY, CSIZE, C2ARRY )

         CALL CYACIP ( CSIZE, 'F', I, C2ARRY )
         CALL CHCKXC ( .FALSE., ' ', OK )

         DO J = 1, CSIZE

            LABEL = 'C2ARRY(#)'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKSC ( LABEL, C2ARRY(J), '=', XCARRY(J), OK )

         END DO

C
C        Just to be careful, let's test CYCLAC itself.  Create
C        the expected result of cycling the array forward by I.
C
         CALL CLEARC ( CSIZE, C2ARRY )

         DO J = 1, CSIZE
C
C           Find the target location of the Jth element of CARRAY.
C            
            CALL RMAINI ( J + I, CSIZE, Q, R )

            IF ( R .EQ. 0 ) THEN
               R = CSIZE
            END IF

            C2ARRY(R) = CARRAY(J)

         END DO

         DO J = 1, CSIZE

            LABEL = 'C2ARRY(#)'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKSC ( LABEL, C2ARRY(J), '=', XCARRY(J), OK )

         END DO

C
C        Cycle backward by -I as well.
C        
         CALL CYCLAC ( CARRAY, CSIZE, 'B', -I, XCARRY )
         CALL CHCKXC ( .FALSE., ' ', OK )

         DO J = 1, CSIZE

            LABEL = 'C2ARRY(#)'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKSC ( LABEL, C2ARRY(J), '=', XCARRY(J), OK )

         END DO


C
C        Cycle in the opposite direction using a negative count.        
C
         CALL MOVEC ( CARRAY, CSIZE,   C2ARRY )
         CALL CYACIP ( CSIZE, 'B', -I, C2ARRY )
         CALL CHCKXC ( .FALSE., ' ', OK )

         DO J = 1, CSIZE

            LABEL = 'C2ARRY(#)'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKSC ( LABEL, C2ARRY(J), '=', XCARRY(J), OK )

         END DO

      END DO


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Test CYACIP:  cycle backward.' )

      DO I = 1, 2 * CSIZE
C
C        Use CYCLAC to find the expected result.
C
         CALL CYCLAC ( CARRAY, CSIZE, 'B', I, XCARRY )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Also cycle the array in place.     
C
         CALL MOVEC ( CARRAY, CSIZE, C2ARRY )

         CALL CYACIP ( CSIZE, 'B', I, C2ARRY )
         CALL CHCKXC ( .FALSE., ' ', OK )

         DO J = 1, CSIZE

            LABEL = 'C2ARRY(#)'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKSC ( 'C2ARRY', C2ARRY(J), '=', XCARRY(J), OK )

         END DO

C
C        Just to be careful, let's test CYCLAC itself.  Create
C        the expected result of cycling the array backward by I.
C
         CALL CLEARC ( CSIZE, C2ARRY )

         DO J = 1, CSIZE
C
C           Find the target location of the Jth element of CARRAY.
C            
            CALL RMAINI ( J - I, CSIZE, Q, R )

            IF ( R .EQ. 0 ) THEN
               R = CSIZE
            END IF

            C2ARRY(R) = CARRAY(J)

         END DO

         DO J = 1, CSIZE

            LABEL = 'C2ARRY(#)'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKSC ( LABEL, C2ARRY(J), '=', XCARRY(J), OK )

         END DO

C
C        Cycle forward by -I as well.
C        
         CALL CYCLAC ( CARRAY, CSIZE, 'F', -I, XCARRY )
         CALL CHCKXC ( .FALSE., ' ', OK )

         DO J = 1, CSIZE

            LABEL = 'C2ARRY(#)'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKSC ( LABEL, C2ARRY(J), '=', XCARRY(J), OK )

         END DO

C
C        Cycle in the opposite direction using a negative count.        
C
         CALL MOVEC ( CARRAY, CSIZE,   C2ARRY )
         CALL CYACIP ( CSIZE, 'F', -I, C2ARRY )
         CALL CHCKXC ( .FALSE., ' ', OK )

         DO J = 1, CSIZE

            LABEL = 'C2ARRY(#)'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKSC ( 'C2ARRY', C2ARRY(J), '=', XCARRY(J), OK )

         END DO

      END DO

C
C --- Case: ------------------------------------------------------
C
C
C     When the array size is non-positive, the call should be
C     a no-op.
C
C
C     Use a negative size.
C
      CALL TCASE ( 'Error case:  non-positive array size' )

      CALL MOVEC ( CARRAY, CSIZE, C2ARRY )
  
      CALL CYACIP ( -CSIZE, 'F', -5, C2ARRY )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, CSIZE

         LABEL = 'C2ARRY(#)'
         CALL REPMI ( LABEL, '#', I, LABEL )

         CALL CHCKSC ( LABEL, C2ARRY(I), '=', CARRAY(I), OK )

      END DO


C
C     Use size zero.
C
      CALL MOVEC ( CARRAY, CSIZE, C2ARRY )

      CALL CYACIP (  0, 'F', -5, C2ARRY )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, CSIZE

         LABEL = 'C2ARRY(#)'
         CALL REPMI ( LABEL, '#', I, LABEL )

         CALL CHCKSC ( LABEL, C2ARRY(I), '=', CARRAY(I), OK )

      END DO


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error case:  invalid direction.' )
      
      
      CALL CYACIP ( CSIZE, 'Z', -5, C2ARRY )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIRECTION)', OK )


C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END


