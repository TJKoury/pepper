!$Procedure      F_ZZMSXF ( Check the routine zzmsxf )

      SUBROUTINE F_ZZMSXF ( OK )
      IMPLICIT NONE

      LOGICAL               OK

! $ Abstract
!
!     This tests the private rouine ZZMSXF.  Note that we put
!     something that can't be the correct answer in OUTPUT in
!     each test case to make sure that the OUTPUT is completely
!     filled by ZZMSXF
!-&


!
!     Test Utility Functions
!


!
!     SPICELIB Functions
!

!
!     Local Variables
!
      DOUBLE PRECISION      SXFORM ( 6, 6, 5 )
      DOUBLE PRECISION      TEMP   ( 6, 6    )
      DOUBLE PRECISION      OUTPUT ( 6, 6    )
      DOUBLE PRECISION      EXPECT ( 6, 6    )

      DOUBLE PRECISION      ROT1   ( 3, 3 )
      DOUBLE PRECISION      ROT2   ( 3, 3 )
      DOUBLE PRECISION      DROT1  ( 3, 3 )
      DOUBLE PRECISION      DROT2  ( 3, 3 )

      INTEGER               I
      INTEGER               J
      INTEGER               N
      INTEGER               DELTA

      DELTA(I,J) = - ( ABS(I-J) - 1 ) / ( ABS(I-J) + 1 )


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_ZZMSXF' )

      CALL TCASE ( 'Test out the case when N = 0. We should get the ' &
      //           '6x6 identity matrix in this case.' )

         DO J = 1, 6
            DO I = 1,6
               EXPECT(I,J) = DBLE( DELTA(I,J) )
               OUTPUT(I,J) = -1.0D0
            END DO
         END DO
         N = 0

         CALL ZZMSXF ( SXFORM, N, OUTPUT )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKAD ( 'OUTPUT', OUTPUT, '=', EXPECT, 36, 0.0D0, OK )

      CALL TCASE ( 'Test the case when N = 1.  For this case the ' &
      //           'output should be a copy of the input.' )

         DO J = 1, 6
            DO I = 1,6
               OUTPUT(I,J) = -1.0D0
            END DO
         END DO

         DO J = 1, 3
            DO I = 1, 3

               SXFORM ( I,   J,   1 ) = DBLE( J * DELTA(I,J) )
               SXFORM ( I+3, J+3, 1 ) = DBLE( J * DELTA(I,J) )
               SXFORM ( I+3, J,   1 ) = DBLE(     DELTA(I,J) )
               SXFORM ( I,   J+3, 1 ) = 0.0D0

               EXPECT ( I,   J   )    = DBLE( J * DELTA(I,J) )
               EXPECT ( I+3, J+3 )    = DBLE( J * DELTA(I,J) )
               EXPECT ( I+3, J   )    = DBLE(     DELTA(I,J) )
               EXPECT ( I,   J+3 )    = 0.0D0

            END DO
         END DO

         N = 1

         CALL ZZMSXF ( SXFORM, N, OUTPUT )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKAD ( 'OUTPUT', OUTPUT, '=', EXPECT, 36, 0.0D0, OK )


      CALL TCASE ( 'N = 2, Actual product computed.' )


         DO J = 1, 6
            DO I = 1,6
               OUTPUT(I,J) = -1.0D0
            END DO
         END DO

         ROT1(1,1) = 1.0D0
         ROT1(2,1) = 2.0D0
         ROT1(3,1) = 3.0D0

         ROT1(1,2) = 4.0D0
         ROT1(2,2) = 5.0D0
         ROT1(3,2) = 6.0D0

         ROT1(1,3) = 7.0D0
         ROT1(2,3) = 8.0D0
         ROT1(3,3) = 9.0D0


         ROT2(1,1) = 1.0D0
         ROT2(2,1) = 2.0D0
         ROT2(3,1) = 3.0D0

         ROT2(1,2) = 2.0D0
         ROT2(2,2) = 3.0D0
         ROT2(3,2) = 1.0D0

         ROT2(1,3) = 3.0D0
         ROT2(2,3) = 1.0D0
         ROT2(3,3) = 2.0D0

         DROT1(1,1) = 1.0D0
         DROT1(2,1) = 0.0D0
         DROT1(3,1) = 0.0D0

         DROT1(1,2) = 0.0D0
         DROT1(2,2) = 1.0D0
         DROT1(3,2) = 0.0D0

         DROT1(1,3) = 0.0D0
         DROT1(2,3) = 0.0D0
         DROT1(3,3) = 1.0D0


         DROT2(1,1) = 1.0D0
         DROT2(2,1) = 2.0D0
         DROT2(3,1) = 3.0D0

         DROT2(1,2) = 2.0D0
         DROT2(2,2) = 3.0D0
         DROT2(3,2) = 1.0D0

         DROT2(1,3) = 3.0D0
         DROT2(2,3) = 1.0D0
         DROT2(3,3) = 2.0D0


         DO J = 1,3
            DO I = 1,3

               SXFORM( I,   J,   1 ) = ROT1 (I,J)
               SXFORM( I+3, J+3, 1 ) = ROT1 (I,J)
               SXFORM( I,   J+3, 1 ) = 0.0D0
               SXFORM( I+3, J,   1 ) = DROT1(I,J)

               SXFORM( I,   J,   2 ) = ROT2 (I,J)
               SXFORM( I+3, J+3, 2 ) = ROT2 (I,J)
               SXFORM( I,   J+3, 2 ) = 0.0D0
               SXFORM( I+3, J,   2 ) = DROT2(I,J)

            END DO
         END DO


         CALL MXMG  ( SXFORM(1,1,2), SXFORM(1,1,1), 6,6,6, EXPECT )
         CALL ZZMSXF( SXFORM,        N,                    OUTPUT )


      CALL TCASE ( 'Three matrices multiplied together' )

         DO J = 1, 6
            DO I = 1,6
               OUTPUT(I,J) = -1.0D0
            END DO
         END DO

         DO J = 1,3
            DO I = 1,3

               SXFORM( I,   J,   3 ) = ROT1 (I,J)
               SXFORM( I+3, J+3, 3 ) = ROT1 (I,J)
               SXFORM( I,   J+3, 3 ) = 0.0D0
               SXFORM( I+3, J,   3 ) = DROT1(I,J)

            END DO
         END DO

         N = 3

         CALL MXMG  ( SXFORM(1,1,3), SXFORM(1,1,2), 6,6,6, TEMP   )
         CALL MXMG  ( TEMP,          SXFORM(1,1,1), 6,6,6, EXPECT )
         CALL ZZMSXF( SXFORM,        N,                    OUTPUT )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKAD ( 'OUTPUT', OUTPUT, '=', EXPECT, 36, 0.0D0, OK )

      CALL TCASE ( 'Four matrices multiplied together' )

         N = 4

         DO J = 1, 6
            DO I = 1,6
               OUTPUT(I,J) = -1.0D0
            END DO
         END DO

         DO J = 1,3
            DO I = 1,3

               SXFORM( I,   J,   4 ) = ROT2 (I,J)
               SXFORM( I+3, J+3, 4 ) = ROT2 (I,J)
               SXFORM( I,   J+3, 4 ) = 0.0D0
               SXFORM( I+3, J,   4 ) = DROT2(I,J)

            END DO
         END DO

         CALL MXMG  ( SXFORM(1,1,4), SXFORM(1,1,3), 6,6,6, EXPECT )
         CALL MXMG  ( EXPECT,        SXFORM(1,1,2), 6,6,6, TEMP   )
         CALL MXMG  ( TEMP,          SXFORM(1,1,1), 6,6,6, EXPECT )

         CALL ZZMSXF( SXFORM,        N,                    OUTPUT )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKAD ( 'OUTPUT', OUTPUT, '=', EXPECT, 36, 0.0D0, OK )


      CALL TCASE ( 'Five matrices multiplied together' )

         N = 5

         DO J = 1, 6
            DO I = 1,6
               OUTPUT(I,J) = -1.0D0
            END DO
         END DO

         DO J = 1,3
            DO I = 1,3

               SXFORM( I,   J,   5 ) = ROT1 (I,J)
               SXFORM( I+3, J+3, 5 ) = ROT1 (I,J)
               SXFORM( I,   J+3, 5 ) = 0.0D0
               SXFORM( I+3, J,   5 ) = DROT1(I,J)

            END DO
         END DO

         CALL MXMG  ( SXFORM(1,1,5), SXFORM(1,1,4), 6,6,6, TEMP   )
         CALL MXMG  ( TEMP,          SXFORM(1,1,3), 6,6,6, EXPECT )
         CALL MXMG  ( EXPECT,        SXFORM(1,1,2), 6,6,6, TEMP   )
         CALL MXMG  ( TEMP,          SXFORM(1,1,1), 6,6,6, EXPECT )

         CALL ZZMSXF( SXFORM,        N,                    OUTPUT )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKAD ( 'OUTPUT', OUTPUT, '=', EXPECT, 36, 0.0D0, OK )


      CALL T_SUCCESS ( OK )
      RETURN
      END
