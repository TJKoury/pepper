!$Procedure F_CYIP ( Cycle array in place tests )

      SUBROUTINE F_CYIP ( OK )

! $ Abstract
!
!     Exercise routines that cycle arrays in place.
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
! $ Files
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
! $ Particulars
!
!     This routine tests the in-place variants of the array
!     cycling routines.  The routines exercised by this family
!     are:
!
!        CYACIP
!        CYADIP
!        CYAIIP
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman     (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    TSPICE Version 1.0.0, 20-OCT-2005 (NJB)
!
!
!-&

!
!     SPICELIB functions
!

!
!     Local Parameters
!
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

!
!     Local Variables
!
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

!
!     Open the test family.
!
      CALL TOPEN ( 'F_CYIP' )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Test CYAIIP:  cycle forward.' )

      DO I = 1, ISIZE
         IARRAY(I) = I
      END DO


      DO I = 1, 2 * ISIZE
!
!        Use CYCLAI to find the expected result.
!
         CALL CYCLAI ( IARRAY, ISIZE, 'F', I, XIARRY )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Also cycle the array in place.
!
         CALL MOVEI ( IARRAY, ISIZE, I2ARRY )

         CALL CYAIIP ( ISIZE, 'F', I, I2ARRY )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAI ( 'I2ARRY', I2ARRY, '=', XIARRY, ISIZE, OK )

!
!        Just to be careful, let's test CYCLAI itself.  Create
!        the expected result of cycling the array forward by I.
!
         CALL CLEARI ( ISIZE, I2ARRY )

         DO J = 1, ISIZE
!
!           Find the target location of the Jth element of IARRAY.
!
            CALL RMAINI ( J + I, ISIZE, Q, R )

            IF ( R .EQ. 0 ) THEN
               R = ISIZE
            END IF

            I2ARRY(R) = IARRAY(J)

         END DO

         CALL CHCKAI ( 'I2ARRY', I2ARRY, '=', XIARRY, ISIZE, OK )

!
!        Cycle backward by -I as well.
!
         CALL CYCLAI ( IARRAY, ISIZE, 'B', -I, XIARRY )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAI ( 'I2ARRY', I2ARRY, '=', XIARRY, ISIZE, OK )

!
!        Cycle in the opposite direction using a negative count.
!
         CALL MOVEI ( IARRAY, ISIZE,   I2ARRY )
         CALL CYAIIP ( ISIZE, 'B', -I, I2ARRY )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAI ( 'I2ARRY', I2ARRY, '=', XIARRY, ISIZE, OK )

      END DO


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Test CYAIIP:  cycle backward.' )

      DO I = 1, 2 * ISIZE
!
!        Use CYCLAI to find the expected result.
!
         CALL CYCLAI ( IARRAY, ISIZE, 'B', I, XIARRY )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Also cycle the array in place.
!
         CALL MOVEI ( IARRAY, ISIZE, I2ARRY )

         CALL CYAIIP ( ISIZE, 'B', I, I2ARRY )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAI ( 'I2ARRY', I2ARRY, '=', XIARRY, ISIZE, OK )

!
!        Just to be careful, let's test CYCLAI itself.  Create
!        the expected result of cycling the array backward by I.
!
         CALL CLEARI ( ISIZE, I2ARRY )

         DO J = 1, ISIZE
!
!           Find the target location of the Jth element of IARRAY.
!
            CALL RMAINI ( J - I, ISIZE, Q, R )

            IF ( R .EQ. 0 ) THEN
               R = ISIZE
            END IF

            I2ARRY(R) = IARRAY(J)

         END DO

         CALL CHCKAI ( 'I2ARRY', I2ARRY, '=', XIARRY, ISIZE, OK )


!
!        Cycle forward by -I as well.
!
         CALL CYCLAI ( IARRAY, ISIZE, 'F', -I, XIARRY )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAI ( 'I2ARRY', I2ARRY, '=', XIARRY, ISIZE, OK )

!
!        Cycle in the opposite direction using a negative count.
!
         CALL MOVEI ( IARRAY, ISIZE,   I2ARRY )
         CALL CYAIIP ( ISIZE, 'F', -I, I2ARRY )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAI ( 'I2ARRY', I2ARRY, '=', XIARRY, ISIZE, OK )

      END DO

!
! --- Case: ------------------------------------------------------
!
!
!     When the array size is non-positive, the call should be
!     a no-op.
!
!
!     Use a negative size.
!
      CALL TCASE ( 'Error case:  non-positive array size' )

      CALL MOVEI ( IARRAY, ISIZE, I2ARRY )

      CALL CYAIIP ( -ISIZE, 'F', -5, I2ARRY )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAI ( 'I2ARRY', I2ARRY, '=', IARRAY, ISIZE, OK )


!
!     Use size zero.
!
      CALL MOVEI ( IARRAY, ISIZE, I2ARRY )

      CALL CYAIIP (  0, 'F', -5, I2ARRY )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAI ( 'I2ARRY', I2ARRY, '=', IARRAY, ISIZE, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error case:  invalid direction.' )


      CALL CYAIIP ( ISIZE, 'Z', -5, I2ARRY )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIRECTION)', OK )





!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Test CYADIP:  cycle forward.' )

      DO I = 1, DSIZE
         DARRAY(I) = I
      END DO


      DO I = 1, 2 * DSIZE
!
!        Use CYCLAD to find the expected result.
!
         CALL CYCLAD ( DARRAY, DSIZE, 'F', I, XDARRY )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Also cycle the array in place.
!
         CALL MOVED ( DARRAY, DSIZE, D2ARRY )

         CALL CYADIP ( DSIZE, 'F', I, D2ARRY )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( 'D2ARRY', D2ARRY, '=', XDARRY, DSIZE, 0.D0, OK )


!
!        Just to be careful, let's test CYCLAD itself.  Create
!        the expected result of cycling the array forward by I.
!
         CALL CLEARD ( DSIZE, D2ARRY )

         DO J = 1, DSIZE
!
!           Find the target location of the Jth element of DARRAY.
!
            CALL RMAINI ( J + I, DSIZE, Q, R )

            IF ( R .EQ. 0 ) THEN
               R = DSIZE
            END IF

            D2ARRY(R) = DARRAY(J)

         END DO

         CALL CHCKAD ( 'D2ARRY', D2ARRY, '=', XDARRY, DSIZE, 0.D0, OK )

!
!        Cycle backward by -I as well.
!
         CALL CYCLAD ( DARRAY, DSIZE, 'B', -I, XDARRY )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( 'D2ARRY', D2ARRY, '=', XDARRY, DSIZE, 0.D0, OK )

!
!        Cycle in the opposite direction using a negative count.
!
         CALL MOVED  ( DARRAY, DSIZE,  D2ARRY )
         CALL CYADIP ( DSIZE, 'B', -I, D2ARRY )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( 'D2ARRY', D2ARRY, '=', XDARRY, DSIZE, 0.D0, OK )

      END DO


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Test CYADIP:  cycle backward.' )

      DO I = 1, 2 * DSIZE
!
!        Use CYCLAD to find the expected result.
!
         CALL CYCLAD ( DARRAY, DSIZE, 'B', I, XDARRY )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Also cycle the array in place.
!
         CALL MOVED  ( DARRAY, DSIZE, D2ARRY )

         CALL CYADIP ( DSIZE, 'B', I, D2ARRY )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( 'D2ARRY', D2ARRY, '=', XDARRY, DSIZE, 0.D0, OK )

!
!        Just to be careful, let's test CYCLAD itself.  Create
!        the expected result of cycling the array backward by I.
!
         CALL CLEARD ( DSIZE, D2ARRY )

         DO J = 1, DSIZE
!
!           Find the target location of the Jth element of DARRAY.
!
            CALL RMAINI ( J - I, DSIZE, Q, R )

            IF ( R .EQ. 0 ) THEN
               R = DSIZE
            END IF

            D2ARRY(R) = DARRAY(J)

         END DO

         CALL CHCKAD ( 'D2ARRY', D2ARRY, '=', XDARRY, DSIZE, 0.D0, OK )

!
!        Cycle forward by -I as well.
!
         CALL CYCLAD ( DARRAY, DSIZE, 'F', -I, XDARRY )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( 'D2ARRY', D2ARRY, '=', XDARRY, DSIZE, 0.D0, OK )

!
!        Cycle in the opposite direction using a negative count.
!
         CALL MOVED  ( DARRAY, DSIZE,  D2ARRY )
         CALL CYADIP ( DSIZE, 'F', -I, D2ARRY )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( 'D2ARRY', D2ARRY, '=', XDARRY, DSIZE, 0.D0, OK )

      END DO

!
! --- Case: ------------------------------------------------------
!
!
!     When the array size is non-positive, the call should be
!     a no-op.
!
!
!     Use a negative size.
!
      CALL TCASE ( 'Error case:  non-positive array size' )

      CALL MOVED ( DARRAY, DSIZE, D2ARRY )

      CALL CYADIP ( -DSIZE, 'F', -5, D2ARRY )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'D2ARRY', D2ARRY, '=', DARRAY, DSIZE, 0.D0, OK )


!
!     Use size zero.
!
      CALL MOVED ( DARRAY, DSIZE, D2ARRY )

      CALL CYADIP (  0, 'F', -5, D2ARRY )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'D2ARRY', D2ARRY, '=', DARRAY, DSIZE, 0.D0, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error case:  invalid direction.' )


      CALL CYADIP ( DSIZE, 'Z', -5, D2ARRY )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIRECTION)', OK )







!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Test CYACIP:  cycle forward.' )

      DO I = 1, CSIZE
         CALL INTSTR ( I, CARRAY(I) )
      END DO


      DO I = 1, 2 * CSIZE
!
!        Use CYCLAC to find the expected result.
!
         CALL CYCLAC ( CARRAY, CSIZE, 'F', I, XCARRY )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Also cycle the array in place.
!
         CALL MOVEC ( CARRAY, CSIZE, C2ARRY )

         CALL CYACIP ( CSIZE, 'F', I, C2ARRY )
         CALL CHCKXC ( .FALSE., ' ', OK )

         DO J = 1, CSIZE

            LABEL = 'C2ARRY(#)'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKSC ( LABEL, C2ARRY(J), '=', XCARRY(J), OK )

         END DO

!
!        Just to be careful, let's test CYCLAC itself.  Create
!        the expected result of cycling the array forward by I.
!
         CALL CLEARC ( CSIZE, C2ARRY )

         DO J = 1, CSIZE
!
!           Find the target location of the Jth element of CARRAY.
!
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

!
!        Cycle backward by -I as well.
!
         CALL CYCLAC ( CARRAY, CSIZE, 'B', -I, XCARRY )
         CALL CHCKXC ( .FALSE., ' ', OK )

         DO J = 1, CSIZE

            LABEL = 'C2ARRY(#)'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKSC ( LABEL, C2ARRY(J), '=', XCARRY(J), OK )

         END DO


!
!        Cycle in the opposite direction using a negative count.
!
         CALL MOVEC ( CARRAY, CSIZE,   C2ARRY )
         CALL CYACIP ( CSIZE, 'B', -I, C2ARRY )
         CALL CHCKXC ( .FALSE., ' ', OK )

         DO J = 1, CSIZE

            LABEL = 'C2ARRY(#)'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKSC ( LABEL, C2ARRY(J), '=', XCARRY(J), OK )

         END DO

      END DO


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Test CYACIP:  cycle backward.' )

      DO I = 1, 2 * CSIZE
!
!        Use CYCLAC to find the expected result.
!
         CALL CYCLAC ( CARRAY, CSIZE, 'B', I, XCARRY )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Also cycle the array in place.
!
         CALL MOVEC ( CARRAY, CSIZE, C2ARRY )

         CALL CYACIP ( CSIZE, 'B', I, C2ARRY )
         CALL CHCKXC ( .FALSE., ' ', OK )

         DO J = 1, CSIZE

            LABEL = 'C2ARRY(#)'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKSC ( 'C2ARRY', C2ARRY(J), '=', XCARRY(J), OK )

         END DO

!
!        Just to be careful, let's test CYCLAC itself.  Create
!        the expected result of cycling the array backward by I.
!
         CALL CLEARC ( CSIZE, C2ARRY )

         DO J = 1, CSIZE
!
!           Find the target location of the Jth element of CARRAY.
!
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

!
!        Cycle forward by -I as well.
!
         CALL CYCLAC ( CARRAY, CSIZE, 'F', -I, XCARRY )
         CALL CHCKXC ( .FALSE., ' ', OK )

         DO J = 1, CSIZE

            LABEL = 'C2ARRY(#)'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKSC ( LABEL, C2ARRY(J), '=', XCARRY(J), OK )

         END DO

!
!        Cycle in the opposite direction using a negative count.
!
         CALL MOVEC ( CARRAY, CSIZE,   C2ARRY )
         CALL CYACIP ( CSIZE, 'F', -I, C2ARRY )
         CALL CHCKXC ( .FALSE., ' ', OK )

         DO J = 1, CSIZE

            LABEL = 'C2ARRY(#)'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKSC ( 'C2ARRY', C2ARRY(J), '=', XCARRY(J), OK )

         END DO

      END DO

!
! --- Case: ------------------------------------------------------
!
!
!     When the array size is non-positive, the call should be
!     a no-op.
!
!
!     Use a negative size.
!
      CALL TCASE ( 'Error case:  non-positive array size' )

      CALL MOVEC ( CARRAY, CSIZE, C2ARRY )

      CALL CYACIP ( -CSIZE, 'F', -5, C2ARRY )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, CSIZE

         LABEL = 'C2ARRY(#)'
         CALL REPMI ( LABEL, '#', I, LABEL )

         CALL CHCKSC ( LABEL, C2ARRY(I), '=', CARRAY(I), OK )

      END DO


!
!     Use size zero.
!
      CALL MOVEC ( CARRAY, CSIZE, C2ARRY )

      CALL CYACIP (  0, 'F', -5, C2ARRY )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, CSIZE

         LABEL = 'C2ARRY(#)'
         CALL REPMI ( LABEL, '#', I, LABEL )

         CALL CHCKSC ( LABEL, C2ARRY(I), '=', CARRAY(I), OK )

      END DO


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error case:  invalid direction.' )


      CALL CYACIP ( CSIZE, 'Z', -5, C2ARRY )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIRECTION)', OK )


!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


