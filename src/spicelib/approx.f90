!$Procedure APPROX ( Approximate equality )

      LOGICAL FUNCTION APPROX ( X, Y, TOL )

! $ Abstract
!
!     Return .TRUE. if two double precision numbers are equal to
!     within some tolerance.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     COMPARE
!     NUMBERS
!
! $ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      X
      DOUBLE PRECISION      Y
      DOUBLE PRECISION      TOL

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     X,
!     Y          I   Double precision numbers.
!     TOL        I   Tolerance.
!
!     The function is .TRUE. whenever |X - Y| <= TOL.
!
! $ Detailed_Input
!
!     X,
!     Y        are arbitrary double precision numbers.
!
!     TOL      is a tolerance. X and Y are considered to be equal
!              if they differ by no more than this amount. If TOL
!              is negative, X and Y are never considered equal.
!
! $ Detailed_Output
!
!     The function is .TRUE. whenever |X - Y| <= TOL, and is .FALSE.
!     otherwise.
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
!     None.
!
! $ Examples
!
!     C
!     C     If the eccentricity is near one, this a parabola.
!     C
!           IF ( APPROX ( ECC, 1.D0, 10.D-12 ) ) THEN
!              TYPE = 'PARABOLA'
!
!           ELSE IF ( ECC .LT. 1 ) THEN
!              TYPE = 'ELLIPSE'
!
!           ELSE
!              TYPE = 'HYPERBOLA'
!           END IF
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
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
!
!-&


! $ Index_Entries
!
!     approximate equality
!
!-&


!
!     Just shorthand, really.
!
      APPROX = ( ABS ( X - Y ) .LE. TOL )

      RETURN
      END
