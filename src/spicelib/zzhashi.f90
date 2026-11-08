!$Procedure ZZHASHI ( Private---integer hash function )

      INTEGER FUNCTION ZZHASHI ( N, M )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     This function returns the hash value corresponding to an integer.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     PRIVATE UTILITY
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               N
      INTEGER               M

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     N          I   An integer number.
!     M          I   Divisor used for the hash function
!
!     The function returns the hash value associated with N.
!
! $ Detailed_Input
!
!     N           is an integer number.
!
!     M           is the divisor of the hashing function. This value
!                 defines the spread of the hash values, that spread
!                 covering the interval [1, M]. M must be a positive
!                 number. If it is not, the function signals an error
!                 and returns 0.
!
! $ Detailed_Output
!
!     The function returns the hash value of N as computed using M
!     as the hash function divisor.
!
!     This function is sign-insensitive, i.e. it computes the same
!     hash value for A as is it does for -A.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) If the input divisor value is not in the allowed range, the
!        error 'SPICE(INVALIDDIVISOR)' will be signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine computes the hash value of an integer number using
!     the user specified hash divisor value.
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
!     W.L. Taber      (JPL)
!     E.D. Wright     (JPL)
!     B.V. Semenov    (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 02-AUG-2013 (BVS)
!
!-&

!
!     Check divisor.
!
      IF ( M .LE. 0 ) THEN

         ZZHASHI = 0

         CALL CHKIN  ( 'ZZHASHI'                                     )
         CALL SETMSG ( 'The input hash function divisor was not '    // &
                       'a positive number. It was #.'                )
         CALL ERRINT ( '#', M                                        )
         CALL SIGERR ( 'SPICE(INVALIDDIVISOR)'                       )
         CALL CHKOUT ( 'ZZHASHI'                                     )
         RETURN

      END IF

!
!     Use simple division method -- h(k) = k mod m.
!
      ZZHASHI = MOD(ABS(N),M) + 1

      RETURN
      END


