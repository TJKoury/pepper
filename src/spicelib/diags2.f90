!$Procedure DIAGS2   ( Diagonalize symmetric 2x2 matrix )

      SUBROUTINE DIAGS2 ( SYMMAT, DIAG, ROTATE )

! $ Abstract
!
!     Diagonalize a symmetric 2x2 matrix.
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
!     ROTATION
!
! $ Keywords
!
!     ELLIPSE
!     MATRIX
!     ROTATION
!     TRANSFORMATION
!
! $ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      SYMMAT ( 2, 2 )
      DOUBLE PRECISION      DIAG   ( 2, 2 )
      DOUBLE PRECISION      ROTATE ( 2, 2 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     SYMMAT     I   A symmetric 2x2 matrix.
!     DIAG       O   A diagonal matrix similar to SYMMAT.
!     ROTATE     O   A rotation used as the similarity transformation.
!
! $ Detailed_Input
!
!     SYMMAT   is a symmetric 2x2 matrix. That is, SYMMAT has the
!              form
!
!                 .-        -.
!                 |  A    B  |
!                 |          |
!                 |  B    C  |
!                 `-        -'
!
!              This routine uses only the upper-triangular
!              elements of SYMMAT, that is, the elements
!
!                 SYMMAT(1,1)
!                 SYMMAT(1,2)
!                 SYMMAT(2,2)
!
!              to determine the outputs DIAG and ROTATE.
!
! $ Detailed_Output
!
!     DIAG,
!     ROTATE   are, respectively, a diagonal matrix and a 2x2
!              rotation matrix that satisfy the equation
!
!                                  T
!                 DIAG   =   ROTATE   *  SYMMAT  *  ROTATE.
!
!              In other words, DIAG is similar to SYMMAT, and ROTATE is
!              a change-of-basis matrix that diagonalizes SYMMAT. DIAGS2
!              chooses ROTATE so that its angle of rotation has the
!              smallest possible magnitude. If there are two rotations
!              that meet these criteria (they will be inverses of one
!              another), either rotation may be chosen.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  The matrix element SYMMAT(2,1) is not used in this routine's
!         computations, so the condition
!
!            SYMMAT(1,2)  .NE.  SYMMAT(2,1)
!
!         has no effect on this routine's outputs.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The capability of diagonalizing a 2x2 symmetric matrix is
!     especially useful in a number of geometric applications
!     involving quadratic curves such as ellipses. Such curves are
!     described by expressions of the form
!
!           2                    2
!        A x   +   B xy   +   C y   +   D x    +    E y   +   F   =   0.
!
!     Diagonalization of the matrix
!
!        .-         -.
!        | A     B/2 |
!        |           |
!        | B/2     C |
!        `-         -'
!
!     allows us to perform a coordinate transformation (a rotation,
!     specifically) such that the equation of the curve becomes
!
!           2         2
!        P u   +   Q v   +   R u    +    S v   +   T   =   0
!
!     in the transformed coordinates. This form is much easier to
!     handle. If the quadratic curve in question is an ellipse,
!     we can easily find its center, semi-major axis, and semi-minor
!     axis from the second equation.
!
!     Ellipses turn up frequently in navigation geometry problems;
!     for example, the limb and terminator (if we treat the Sun as a
!     point source) of a body modeled as a tri-axial ellipsoid are
!     ellipses.
!
!     A mathematical note: because SYMMAT is symmetric, we can ALWAYS
!     find an orthogonal similarity transformation that diagonalizes
!     SYMMAT, and we can choose the similarity transformation to be a
!     rotation matrix. By `orthogonal' we mean that if the ROTATE is
!     the matrix in question, then
!
!              T                         T
!        ROTATE  ROTATE  =  ROTATE ROTATE  =  I.
!
!     The reasons this routine handles only the 2x2 case are: first,
!     the 2x2 case is much simpler than the general case, in which
!     iterative diagonalization methods must be used, and second, the
!     2x2 case is adequate for solving problems involving ellipses in
!     3 dimensional space. Finally, this routine can be used to
!     support a routine that solves the general-dimension
!     diagonalization problem for symmetric matrices.
!
!     Another feature of the routine that might provoke curiosity is
!     its insistence on choosing the diagonalization matrix that
!     rotates the original basis vectors by the smallest amount. The
!     rotation angle of ROTATE is of no concern for most applications,
!     but can be important if this routine is used as part of an
!     iterative diagonalization method for higher-dimensional matrices.
!     In that case, it is most undesirable to interchange diagonal
!     matrix elements willy-nilly; the matrix to be diagonalized could
!     get ever closer to being diagonal without converging. Choosing
!     the smallest rotation angle precludes this possibility.
!
! $ Examples
!
!     1)  A case that can be verified by hand computation:
!         Suppose SYMMAT is
!
!            +-                -+
!            |  1.0D0    4.0D0  |
!            |                  |
!            |  4.0D0   -5.0D0  |
!            +-                -+
!
!         Then SYMMAT is similar to the diagonal matrix
!
!            +-                -+
!            |  3.0D0    0.0D0  |
!            |                  |
!            |  0.0D0   -7.0D0  |
!            +-                -+
!
!         so
!
!            DIAG(1,1) =  3.D0
!            DIAG(2,1) =  0.D0
!            DIAG(1,2) =  0.D0
!            DIAG(2,2) = -7.D0
!
!         and ROTATE is
!
!            +-                                   -+
!            |   0.894427191          -0.447213595 |
!            |                                     |
!            |   0.447213595           0.894427191 |
!            +-                                   -+
!
!        which is an approximation to
!
!            +-                                   -+
!            |  0.4 * 5**(1/2)     -0.2 * 5**(1/2) |
!            |                                     |
!            |  0.2 * 5**(1/2)      0.4 * 5**(1/2) |
!            +-                                   -+
!
!
!     2)  Suppose we want to find the semi-axes of the ellipse defined
!         by
!                2                 2
!            27 x  +  10 xy  +  3 y   =  1.
!
!         We can write the above equation as the matrix equation
!
!            +-     -+  +-         -+  +- -+
!            | x   y |  | 27     5  |  | x |    =   1;
!            +-     -+  |           |  |   |
!                       |  5     3  |  | y |
!                       +-         -+  +- -+
!
!         let SYMMAT be the symmetric matrix on the left. The code
!         fragment
!
!            SYMMAT(1,1)  =  27.D0
!            SYMMAT(2,1)  =   5.D0
!            SYMMAT(1,2)  =   5.D0
!            SYMMAT(2,2)  =   3.D0
!
!            CALL DIAGS2 ( SYMMAT, DIAG, ROTATE )
!
!         will return DIAG, an array containing the eigenvalues of
!         SYMMAT, and ROTATE, the coordinate transformation required
!         to diagonalize SYMMAT. In this case,
!
!            DIAG(1,1)   =  28.D0
!            DIAG(2,1)   =  0.D0
!            DIAG(1,2)   =  0.D0
!            DIAG(2,2)   =  2.D0
!
!          and
!
!            ROTATE(1,1) =  0.980580676D0
!            ROTATE(2,1) =  0.196116135D0
!            ROTATE(1,2) = -0.196116135D0
!            ROTATE(2,2) =  0.980580676D0
!
!         The columns of ROTATE give the ellipse's axes, after scaling
!         them by
!
!                   1                            1
!            ----------------     and     ---------------
!              ____________                 ____________
!            \/  DIAG(1,1)                \/  DIAG(2,2)
!
!         respectively.
!
!         If SMAJOR and SMINOR are semi-major and semi-minor axes,
!         we can find them as shown below. For brevity, we omit the
!         check for zero or negative eigenvalues. Negative or zero
!         eigenvalues will occur only as a result of round-off error;
!         mathematically, the eigenvalues of the matrix SYMMAT are
!         guaranteed to be positive, since they are the reciprocals of
!         the squares of the lengths of the ellipse's semi-axes.
!
!            DO I = 1, 2
!               SMAJOR(I) = ROTATE(I,1)  /  DSQRT( DIAG(1,1) )
!               SMINOR(I) = ROTATE(I,2)  /  DSQRT( DIAG(2,2) )
!            END DO
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     [1]  T. Apostol, "Calculus, Vol. II," chapter 5, "Eigenvalues of
!          Operators Acting on Euclidean Spaces," John Wiley & Sons,
!          1969.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.3.0, 17-JUN-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Added
!        "Error free." to $Exceptions section. Removed unnecessary
!        $Revisions section.
!
!-    SPICELIB Version 1.2.0, 06-SEP-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in VHATG and SWAPD calls.
!
!-    SPICELIB Version 1.1.0, 24-JAN-2002 (EDW)
!
!        Edited incorrect examples in the header. The example
!        outputs did not correspond to the actual function
!        of the routine.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 04-NOV-1990 (NJB)
!
!-&


! $ Index_Entries
!
!     diagonalize symmetric 2x2_matrix
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               RETURN

!
!     Local variables
!
      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      EIGVEC ( 2 )
      DOUBLE PRECISION      IDENT  ( 2, 2 )
      DOUBLE PRECISION      ROOT1  ( 2 )
      DOUBLE PRECISION      ROOT2  ( 2 )
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      TMPD
      DOUBLE PRECISION      TMPV   ( 2 )

!
!     Saved variables
!
      SAVE                  IDENT

!
!     Initial values
!
      DATA IDENT /  1.D0, 0.D0, &
                    0.D0, 1.D0  /



!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DIAGS2' )
      END IF

!
!     We check for the case of a diagonal input matrix, since
!     eigenvector determination is simplified by ruling out this
!     case.

      IF  ( SYMMAT(1,2) .EQ. 0.D0 ) THEN

         CALL MOVED  ( IDENT,  4, ROTATE )
         CALL MOVED  ( SYMMAT, 4, DIAG   )
!
!        Explicitly zero out the (2,1) entry of DIAG, since DIAG is
!        guaranteed to be diagonal.
!
         DIAG(2,1) = 0.D0

         CALL CHKOUT ( 'DIAGS2' )
         RETURN

      END IF

!
!     Getting here means there's some actual work to do.  We start out
!     by scaling our matrix, in order to reduce the chance of overflow.
!     We divide everything by the largest magnitude of any element of
!     SYMMAT.  We're guaranteed that SCALE is non-zero, since the 0
!     matrix is diagonal.
!
      SCALE  =  MAX (  DABS( SYMMAT(1,1) ), &
                       DABS( SYMMAT(1,2) ), &
                       DABS( SYMMAT(2,2) )   )

      A      =  SYMMAT(1,1) / SCALE
      B      =  SYMMAT(1,2) / SCALE
      C      =  SYMMAT(2,2) / SCALE

!
!     Compute the eigenvalues of the scaled version of SYMMAT.  The
!     eigenvalues are roots of the equation
!
!          DET (  (1 / SCALE) * SYMMAT  -  x * IDENTITY  ) = 0,
!
!     or equivalently,
!
!           2                             2
!          x   -  ( A + C ) x  +  ( AC - B )  =   0.
!
!
      CALL RQUAD (  1.D0,  -(A + C),   A*C - B**2,   ROOT1,   ROOT2   )

!
!     ROOT1 is the root corresponding to the positive discriminant term;
!     this is guaranteed by RQUAD.
!
      DIAG(1,1) = ROOT1(1)
      DIAG(2,1) = 0.D0
      DIAG(1,2) = 0.D0
      DIAG(2,2) = ROOT2(1)

!
!     Our next job is to find an eigenvector corresponding to the
!     eigenvalue of smaller magnitude.  We can unitize it and choose
!     an orthogonal unit vector so as to create the desired rotation
!     matrix.
!
!        If our original matrix is
!
!           +-        -+
!           |  A    B  |
!           |          |,
!           |  B    C  |
!           +-        -+
!
!        then the matrix
!
!           +-                                -+
!           |  A - DIAG(x,x)    B              |
!           |                                  |
!           |  B                C - DIAG(x,x)  |
!           +-                                -+
!
!        maps to zero all elements of the eigenspace corresponding to
!        DIAG(x,x), where x is either 1 or 2.
!
!        So
!
!           +-               -+           +-               -+
!           |  B              |           |  DIAG(x,x) - C  |
!           |                 |   and     |                 |
!           |  DIAG(x,x) - A  |           |  B              |
!           +-               -+           +-               -+
!
!        are candidates for eigenvectors for DIAG(x,x).  To minimize
!        loss of accuracy in our eigenvector due to subtraction of
!        nearly equal quantities, we choose the vector in which the
!        term involving the eigenvalue has the larger magnitude.  The
!        rigorous justification of this choice would literally take
!        pages of explanation, and we are not going to go through it
!        here.  In most cases, either choice is satisfactory, and in
!        the case where cancellation is a problem, our choice is
!        preferable.
!
!        Note that there is nothing to be gained as far as accuracy is
!        concerned by working with one eigenvalue as opposed to the
!        other:  the magnitudes of the quantities DIAG(x,x) - A and
!        DIAG(x,x) - C would be interchanged by taking x = '2' instead
!        of x = '1'.
!

      IF (  DABS( DIAG(1,1) - A ) .GE. DABS( DIAG(1,1) - C )  ) THEN
!
!        In this case, the second eigenvector component EIGVEC(2)
!        should be larger than |B|; we explain why in detail below.
!        We use the MAX function below to guard against reversal of the
!        inequality due to round-off error.
!
         EIGVEC(1)  =  B
         EIGVEC(2)  =  MAX (   DIAG(1,1) - A,   ABS(B)   )

!
!        Recall that DIAG(1,1) is an eigenvalue of the scaled version
!        of SYMMAT
!
!           +-      -+
!           | A    B |
!           |        |.
!           | B    C |
!           +-      -+
!
!        DIAG(1,1) is the positive-discriminant root of this matrix's
!        characteristic equation.  EIGVEC's y-component
!
!           DIAG(1,1) - A
!
!        is positive and of magnitude at least as large as that of B,
!        since it is the larger of
!                                                 ______________________
!                                                /         2
!                             C - A             / ( A - C )           2
!           DIAG(1,1) - A  =  -----   +     \  /  ----------    +    B
!                               2            \/       4
!
!        and
!                                                 ______________________
!                                                /         2
!                             A - C             / ( A - C )           2
!           DIAG(1,1) - C  =  -----   +     \  /  ----------    +    B
!                               2            \/       4
!
!        Equality between these expressions can occur only when A is
!        equal to C, in which case both expressions are equal (except
!        for round-off error) to |B|.
!

!
!        So the argument of EIGVEC is in the interval [pi/4, 3*pi/4].
!        The second eigenvector is EIGVEC, and the first
!        eigenvector is found by rotating EIGVEC by -pi/2.  Since
!        DIAG(1,1) is the eigenvalue for the SECOND eigenvector, we
!        must swap the eigenvalues.
!

!
!        Unitize the eigenvector.
!
         CALL VHATG ( EIGVEC, 2, TMPV   )
         CALL MOVED ( TMPV,   2, EIGVEC )

         ROTATE(1,1)  =  EIGVEC(2)
         ROTATE(2,1)  = -EIGVEC(1)
         ROTATE(1,2)  =  EIGVEC(1)
         ROTATE(2,2)  =  EIGVEC(2)

!
!        Swap DIAG(1,1) and DIAG(2,2).
!
         TMPD         =  DIAG(2,2)
         DIAG(2,2)    =  DIAG(1,1)
         DIAG(1,1)    =  TMPD

      ELSE

         EIGVEC(1)  =  MAX (  DIAG(1,1) - C,   ABS(B)  )
         EIGVEC(2)  =  B

!
!        The x-component of EIGVEC is positive and has magnitude
!        greater than or equal to that of the y-component of EIGVEC.
!        The argument of EIGVEC is in [-pi/4, pi/4], and the second
!        eigenvector is found by rotating EIGVEC by pi/2.
!

!
!        Unitize the eigenvector.
!
         CALL VHATG ( EIGVEC, 2, TMPV   )
         CALL MOVED ( TMPV,   2, EIGVEC )

         ROTATE(1,1)  =  EIGVEC(1)
         ROTATE(2,1)  =  EIGVEC(2)
         ROTATE(1,2)  = -EIGVEC(2)
         ROTATE(2,2)  =  EIGVEC(1)

      END IF

!
!     We must scale the eigenvalues.
!
      DIAG(1,1)  =  DIAG(1,1)  *  SCALE
      DIAG(2,2)  =  DIAG(2,2)  *  SCALE

      CALL CHKOUT ( 'DIAGS2' )
      RETURN
      END
