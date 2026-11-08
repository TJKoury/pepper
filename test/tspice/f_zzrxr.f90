!$Procedure      F_ZZRXR ( Check the routine zzRXR )

      SUBROUTINE F_ZZRXR ( OK )
      IMPLICIT NONE

      LOGICAL               OK

! $ Abstract
!
!     This tests the private rouine ZZRXR.  Note that we put
!     something that can't be the correct answer in OUTPUT in
!     each test case to make sure that the OUTPUT is completely
!     filled by ZZRXR
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
      DOUBLE PRECISION      SXFORM ( 3, 3, 5 )
      DOUBLE PRECISION      TEMP   ( 3, 3    )
      DOUBLE PRECISION      OUTPUT ( 3, 3    )
      DOUBLE PRECISION      EXPECT ( 3, 3    )

      DOUBLE PRECISION      ROT1   ( 3, 3 )
      DOUBLE PRECISION      ROT2   ( 3, 3 )

      INTEGER               I
      INTEGER               J
      INTEGER               N
      INTEGER               DELTA

      DELTA(I,J) = - ( ABS(I-J) - 1 ) / ( ABS(I-J) + 1 )


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_ZZRXR' )

      CALL TCASE ( 'Test out the case when N = 0. We should get the ' &
      //           '3x3 identity matrix in this case.' )

         DO J = 1,3
            DO I = 1,3
               EXPECT(I,J) = DBLE( DELTA(I,J) )
               OUTPUT(I,J) = -1.0D0
            END DO
         END DO
         N = 0

         CALL ZZRXR ( SXFORM, N, OUTPUT )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKAD ( 'OUTPUT', OUTPUT, '=', EXPECT, 9, 0.0D0, OK )

      CALL TCASE ( 'Test the case when N = 1.  For this case the ' &
      //           'output should be a copy of the input.' )

         DO J = 1, 3
            DO I = 1,3
               OUTPUT(I,J) = -1.0D0
            END DO
         END DO

         DO J = 1, 3
            DO I = 1, 3

               SXFORM ( I,   J,   1 ) = DBLE( J * DELTA(I,J) )

               EXPECT ( I,   J   )    = DBLE( J * DELTA(I,J) )

            END DO
         END DO

         N = 1

         CALL ZZRXR ( SXFORM, N, OUTPUT )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKAD ( 'OUTPUT', OUTPUT, '=', EXPECT, 9, 0.0D0, OK )


      CALL TCASE ( 'N = 2, Actual product computed.' )


         DO J = 1, 3
            DO I = 1,3
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



         DO J = 1,3
            DO I = 1,3

               SXFORM( I,   J,   1 ) = ROT1 (I,J)
               SXFORM( I,   J,   2 ) = ROT2 (I,J)

            END DO
         END DO

         N = 2

         CALL MXM   ( SXFORM(1,1,2), SXFORM(1,1,1),  EXPECT )
         CALL ZZRXR ( SXFORM,        N,              OUTPUT )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKAD ( 'OUTPUT', OUTPUT, '=', EXPECT, 9, 0.0D0, OK )

      CALL TCASE ( 'Three matrices multiplied together' )

         DO J = 1, 3
            DO I = 1,3
               OUTPUT(I,J) = -1.0D0
            END DO
         END DO

         DO J = 1,3
            DO I = 1,3

               SXFORM( I,   J,   3 ) = ROT1 (I,J)

            END DO
         END DO

         N = 3

         CALL MXM   ( SXFORM(1,1,3), SXFORM(1,1,2),  TEMP   )
         CALL MXM   ( TEMP,          SXFORM(1,1,1),  EXPECT )
         CALL ZZRXR ( SXFORM,        N,              OUTPUT )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKAD ( 'OUTPUT', OUTPUT, '=', EXPECT, 9, 0.0D0, OK )

      CALL TCASE ( 'Four matrices multiplied together' )

         N = 4

         DO J = 1, 3
            DO I = 1,3
               OUTPUT(I,J) = -1.0D0
            END DO
         END DO

         DO J = 1,3
            DO I = 1,3

               SXFORM( I,   J,   4 ) = ROT2 (I,J)

            END DO
         END DO

         CALL MXM   ( SXFORM(1,1,4), SXFORM(1,1,3),  EXPECT )
         CALL MXM   ( EXPECT,        SXFORM(1,1,2),  TEMP   )
         CALL MXM   ( TEMP,          SXFORM(1,1,1),  EXPECT )

         CALL ZZRXR ( SXFORM,        N,              OUTPUT )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKAD ( 'OUTPUT', OUTPUT, '=', EXPECT, 9, 0.0D0, OK )


      CALL TCASE ( 'Five matrices multiplied together' )

         N = 5

         DO J = 1, 3
            DO I = 1,3
               OUTPUT(I,J) = -1.0D0
            END DO
         END DO

         DO J = 1,3
            DO I = 1,3
               SXFORM( I,   J,   5 ) = ROT1 (I,J)
            END DO
         END DO

         CALL MXM   ( SXFORM(1,1,5), SXFORM(1,1,4),  TEMP   )
         CALL MXM   ( TEMP,          SXFORM(1,1,3),  EXPECT )
         CALL MXM   ( EXPECT,        SXFORM(1,1,2),  TEMP   )
         CALL MXM   ( TEMP,          SXFORM(1,1,1),  EXPECT )

         CALL ZZRXR ( SXFORM,        N,              OUTPUT )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKAD ( 'OUTPUT', OUTPUT, '=', EXPECT, 9, 0.0D0, OK )


      CALL T_SUCCESS ( OK )
      RETURN
      END
