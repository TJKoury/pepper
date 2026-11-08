!$Procedure VZERO    ( Is a vector the zero vector? )

      LOGICAL FUNCTION VZERO ( V )

! $ Abstract
!
!     Indicate whether a 3-vector is the zero vector.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     MATH
!     VECTOR
!
! $ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      V ( 3 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     V          I   Vector to be tested.
!
!     The function returns the value .TRUE. if and only if V is the
!     zero vector.
!
! $ Detailed_Input
!
!     V        is a vector in 3-space.
!
! $ Detailed_Output
!
!     The function returns the value .TRUE. if and only if V is the
!     zero vector.
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
!     This function has the same truth value as the logical expression
!
!        VNORM ( V )  .EQ.  0.D0
!
!     Replacing the above expression by
!
!        VZERO ( V )
!
!     has several advantages: the latter expresses the test more
!     clearly, looks better, and doesn't go through the work of scaling,
!     squaring, taking a square root, and re-scaling (all of which
!     VNORM must do) just to find out that a vector is non-zero.
!
!     A related function is VZEROG, which accepts vectors of arbitrary
!     dimension.
!
! $ Examples
!
!     1)  When testing whether a vector is the zero vector, one
!         normally constructs tests like
!
!            IF (  VNORM ( V )  .EQ.  0.D0  ) THEN
!                        .
!                        .
!                        .
!
!
!         These can be replaced with the code
!
!            IF (  VZERO ( V )  ) THEN
!                        .
!                        .
!                        .
!
!
!      2)  Check that a normal vector is non-zero before creating
!         a plane with PNV2PL:
!
!         IF (  VZERO ( NORMAL )  )  THEN
!
!            [ handle error ]
!
!         ELSE
!
!            CALL PNV2PL ( POINT, NORMAL, PLANE )
!                          .
!                          .
!                          .
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 05-JUL-2021 (JDR)
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
!-    SPICELIB Version 1.0.0, 17-JUL-1990 (NJB) (IMU)
!
!-&


! $ Index_Entries
!
!     test whether a 3-dimensional vector is the zero vector
!
!-&


!
!     `Just do it'.
!
!
      VZERO   =        (  V(1) .EQ. 0.D0  ) &
                .AND.  (  V(2) .EQ. 0.D0  ) &
                .AND.  (  V(3) .EQ. 0.D0  )

      RETURN
      END
