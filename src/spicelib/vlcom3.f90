!$Procedure VLCOM3 ( Vector linear combination, 3 dimensions )

      subroutine vlcom3 ( a, v1, b, v2, c, v3, sum )

! $ Abstract
!
!     Compute the vector linear combination of three double precision
!     3-dimensional vectors.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     VECTOR
!
! $ Declarations

      implicit none

      double precision   a
      double precision   v1  ( 3 )
      double precision   b
      double precision   v2  ( 3 )
      double precision   c
      double precision   v3  ( 3 )
      double precision   sum ( 3 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     A          I   Coefficient of V1.
!     V1         I   Vector in 3-space.
!     B          I   Coefficient of V2.
!     V2         I   Vector in 3-space.
!     C          I   Coefficient of V3.
!     V3         I   Vector in 3-space.
!     SUM        O   Linear vector combination A*V1 + B*V2 + C*V3.
!
! $ Detailed_Input
!
!     A        is the double precision scalar variable that multiplies
!              V1.
!
!     V1       is an arbitrary, double precision 3-dimensional vector.
!
!     B        is the double precision scalar variable that multiplies
!              V2.
!
!     V2       is an arbitrary, double precision 3-dimensional vector.
!
!     C        is the double precision scalar variable that multiplies
!              V3.
!
!     V3       is a double precision 3-dimensional vector.
!
! $ Detailed_Output
!
!     SUM      is the double precision 3-dimensional vector which
!              contains the linear combination
!
!                 A * V1 + B * V2 + C * V3
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
!     The code reflects precisely the following mathematical expression
!
!        For each value of the index I, from 1 to 3:
!
!           SUM(I) = A * V1(I) + B * V2(I) + C * V3(I)
!
!     No error checking is performed to guard against numeric overflow.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Suppose you have an instrument with an elliptical field
!        of view described by its angular extent along the semi-minor
!        and semi-major axes.
!
!        The following code example demonstrates how to create
!        16 vectors aiming at visualizing the field-of-view in
!        three dimensional space.
!
!
!        Example code begins here.
!
!
!              PROGRAM VLCOM3_EX1
!              IMPLICIT NONE
!
!        C
!        C     SPICELIB functions.
!        C
!              DOUBLE PRECISION      TWOPI
!
!        C
!        C     Local parameters.
!        C
!        C     Define the two angular extends, along the semi-major
!        C     (U) and semi-minor (V) axes of the elliptical field
!        C     of view, in radians.
!        C
!              DOUBLE PRECISION      MAXANG
!              PARAMETER           ( MAXANG = 0.07D0 )
!
!              DOUBLE PRECISION      MINANG
!              PARAMETER           ( MINANG = 0.035D0 )
!
!        C
!        C     Local variables.
!        C
!              DOUBLE PRECISION      A
!              DOUBLE PRECISION      B
!              DOUBLE PRECISION      STEP
!              DOUBLE PRECISION      THETA
!              DOUBLE PRECISION      U      ( 3 )
!              DOUBLE PRECISION      V      ( 3 )
!              DOUBLE PRECISION      VECTOR ( 3 )
!              DOUBLE PRECISION      Z      ( 3 )
!
!              INTEGER               I
!
!        C
!        C     Let U and V be orthonormal 3-vectors spanning the
!        C     focal plane of the instrument, and Z its
!        C     boresight.
!        C
!              DATA                  U  /  1.D0,  0.D0,  0.D0 /
!              DATA                  V  /  0.D0,  1.D0,  0.D0 /
!              DATA                  Z  /  0.D0,  0.D0,  1.D0 /
!
!        C
!        C     Find the length of the ellipse's axes. Note that
!        C     we are dealing with unitary vectors.
!        C
!              A = TAN ( MAXANG )
!              B = TAN ( MINANG )
!
!        C
!        C     Compute the vectors of interest and display them
!        C
!              THETA = 0.D0
!              STEP  = TWOPI() / 16
!
!              DO I = 1, 16
!
!                 CALL VLCOM3 ( 1.D0,           Z, A * COS(THETA), U,
!             .                 B * SIN(THETA), V, VECTOR            )
!
!                 WRITE(*,'(I2,A,3F10.6)') I, ':', VECTOR
!
!                 THETA = THETA + STEP
!
!              END DO
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!         1:  0.070115  0.000000  1.000000
!         2:  0.064777  0.013399  1.000000
!         3:  0.049578  0.024759  1.000000
!         4:  0.026832  0.032349  1.000000
!         5:  0.000000  0.035014  1.000000
!         6: -0.026832  0.032349  1.000000
!         7: -0.049578  0.024759  1.000000
!         8: -0.064777  0.013399  1.000000
!         9: -0.070115  0.000000  1.000000
!        10: -0.064777 -0.013399  1.000000
!        11: -0.049578 -0.024759  1.000000
!        12: -0.026832 -0.032349  1.000000
!        13: -0.000000 -0.035014  1.000000
!        14:  0.026832 -0.032349  1.000000
!        15:  0.049578 -0.024759  1.000000
!        16:  0.064777 -0.013399  1.000000
!
!
! $ Restrictions
!
!     1)  No error checking is performed to guard against numeric
!         overflow or underflow. The user is responsible for insuring
!         that the input values are reasonable.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Added complete
!        code example.
!
!        Added restriction #1.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 01-NOV-1990 (WLT)
!
!-&


! $ Index_Entries
!
!     linear combination of three 3-dimensional vectors
!
!-&


      sum(1) = a*v1(1) + b*v2(1) + c*v3(1)
      sum(2) = a*v1(2) + b*v2(2) + c*v3(2)
      sum(3) = a*v1(3) + b*v2(3) + c*v3(3)

      return
      end
