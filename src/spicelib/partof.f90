!$Procedure PARTOF ( Parabolic time of flight )

      SUBROUTINE PARTOF ( MA, D )

! $ Abstract
!
!     Solve the time of flight equation MA = D + (D**3) / 3
!     for the parabolic eccentric anomaly D, given mean anomaly.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CONIC
!
! $ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      MA
      DOUBLE PRECISION      D

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     MA         I   Mean anomaly at epoch.
!     D          O   Parabolic eccentric anomaly.
!
! $ Detailed_Input
!
!     MA       is the parabolic mean anomaly of an orbiting body at
!              some epoch t,
!
!                                   3  1/2
!                 MA = (t-T) (mu/(2q ))
!
!              where T is the time of periapsis passage, mu is
!              the gravitational parameter of the primary body,
!              and q is the perifocal distance.
!
! $ Detailed_Output
!
!     D        is the corresponding parabolic anomaly. This is the
!              solution to the time of flight equation
!
!                           3
!                 MA = D + D / 3
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Iterate to solve
!
!                         3
!        f(D,MA,p) = D + D / 3 - MA = 0
!
! $ Examples
!
!     ELLTOF, HYPTOF, and PARTOF are used by CONICS.
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     [1]  R. Bate, D. Mueller, and J. White, "Fundamentals of
!          Astrodynamics," Dover Publications Inc., 1971.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.1.0, 14-APR-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Removed
!        unnecessary $Revisions section.
!
!-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 2.0.0, 19-APR-1991 (WLT)
!
!        A write statement left over from debugging days was removed.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
!
!-&


! $ Index_Entries
!
!     parabolic time of flight
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               RETURN
      DOUBLE PRECISION      DCBRT

!
!     Local parameters
!
      DOUBLE PRECISION      TOL
      PARAMETER           ( TOL = 1.D-13 )

!
!     Local variables
!
      DOUBLE PRECISION      M
      DOUBLE PRECISION      FN
      DOUBLE PRECISION      DERIV
      DOUBLE PRECISION      DERIV2
      DOUBLE PRECISION      CHANGE




!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'PARTOF' )
      END IF



!
!     If the mean anomaly is zero, the eccentric anomaly is also zero
!     (by inspection). If the mean anomaly is negative, we can pretend
!     that it's positive (by symmetry).
!

      IF ( MA .EQ. 0.D0 ) THEN
         D = 0.D0
         CALL CHKOUT ( 'PARTOF' )
         RETURN
      ELSE
         M = ABS ( MA )
      END IF


!
!     We need an initial guess for the eccentric anomaly D. The function
!     is well behaved, so just about any guess will do.
!
      D = DCBRT ( 3.D0 * M )


!
!     Use the Newton second-order method,
!
!                                           2
!          F    = F  - (f/f')*(1 + f*f''/2f' )
!           i+1    i
!
!     where
!
!                     3
!          f   = D + D / 3 - M
!
!                     2
!          f'  = 1 + D
!
!
!          f'' = 2 D
!

      CHANGE = 1.D0

      DO WHILE ( ABS ( CHANGE ) .GT. TOL )

         FN     = D    + D**3/3.D0 - M
         DERIV  = 1.D0 + D**2
         DERIV2 = 2.D0 * D

         CHANGE = (FN/DERIV) * ( 1.D0 + (FN*DERIV2) / (2.D0*DERIV**2) )
         D      = D - CHANGE

      END DO

      IF ( MA .LT. 0.D0 ) THEN
         D = -D
      END IF


      CALL CHKOUT ( 'PARTOF' )
      RETURN
      END
