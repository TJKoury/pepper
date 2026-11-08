!$Procedure FRAME ( Build a right handed coordinate frame )

      SUBROUTINE FRAME ( X, Y, Z )

! $ Abstract
!
!     Build a right handed orthonormal frame (x,y,z) from a
!     3-dimensional input vector, where the X-axis of the resulting
!     frame is parallel to the original input vector.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     AXES
!     FRAME
!
! $ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION    X ( 3 )
      DOUBLE PRECISION    Y ( 3 )
      DOUBLE PRECISION    Z ( 3 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  ------------------------------------------------
!     X         I-O  Input vector. A parallel unit vector on output.
!     Y          O   Unit vector in the plane orthogonal to X.
!     Z          O   Unit vector given by the cross product <X,Y>.
!
! $ Detailed_Input
!
!     X        is a 3-dimensional vector used to form the first vector
!              of a right-handed orthonormal triple.
!
! $ Detailed_Output
!
!     X,
!     Y,
!     Z        are the 3-dimensional unit vectors that form a right
!              handed orthonormal frame, where X is now a unit vector
!              parallel to the original input vector in X. There are no
!              special geometric properties connected to Y and Z (other
!              than that they complete the right handed frame).
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If X on input is the zero vector the "standard" frame (ijk)
!         is returned.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Given an input vector X, this routine returns unit vectors X,
!     Y, and Z such that XYZ forms a right-handed orthonormal frame
!     where the output X is parallel to the input X.
!
!     This routine is intended primarily to provide a basis for
!     the plane orthogonal to X. There are no special properties
!     associated with Y and Z other than that the resulting XYZ frame
!     is right handed and orthonormal. There are an infinite
!     collection of pairs (Y,Z) that could be used to this end.
!     Even though for a given X, Y and Z are uniquely
!     determined, users
!     should regard the pair (Y,Z) as a random selection from this
!     infinite collection.
!
!     For instance, when attempting to determine the locus of points
!     that make up the limb of a triaxial body, it is a straightforward
!     matter to determine the normal to the limb plane. To find
!     the actual parametric equation of the limb one needs to have
!     a basis of the plane. This routine can be used to get a basis
!     in which one can describe the curve and from which one can
!     then determine the principal axes of the limb ellipse.
!
! $ Examples
!
!     In addition to using a vector to construct a right handed frame
!     with the x-axis aligned with the input vector, one can construct
!     right handed frames with any of the axes aligned with the input
!     vector.
!
!     For example suppose we want a right hand frame XYZ with the
!     Z-axis aligned with some vector V. Assign V to Z
!
!           Z(1) = V(1)
!           Z(2) = V(2)
!           Z(3) = V(3)
!
!     Then call FRAME with the arguments X,Y,Z cycled so that Z
!     appears first.
!
!           CALL FRAME (Z, X, Y)
!
!     The resulting XYZ frame will be orthonormal with Z parallel
!     to the vector V.
!
!     To get an XYZ frame with Y parallel to V perform the following
!
!           Y(1) = V(1)
!           Y(2) = V(2)
!           Y(3) = V(3)
!
!           CALL FRAME (Y, Z, X)
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
!-    SPICELIB Version 1.3.0, 03-JUL-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Improved
!        argument descriptions.
!
!-    SPICELIB Version 1.2.0, 02-SEP-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in VHAT call.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
!
!-&


! $ Index_Entries
!
!     build a right handed coordinate frame
!
!-&


! $ Revisions
!
!-    Beta Version 2.0.0, 29-DEC-1988 (WLT) (IMU)
!
!        The routine was modified so that it now accepts any input
!        vector in the X slot (it originally was assumed to be a unit
!        vector).  Moreover, the original algorithm has been streamlined
!        a great deal to take advantage of our knowledge of the
!        internal structure of the orthonormal triple.
!
!-&


!
!
!     Local variables
!
      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      F

      INTEGER               S1
      INTEGER               S2
      INTEGER               S3



!
!     First make X into a unit vector.
!
      CALL VHATIP ( X )

!
!     We'll need the squares of the components of X in a bit.
!
      A = X(1)*X(1)
      B = X(2)*X(2)
      C = X(3)*X(3)

!
!     If X is zero, then just return the ijk frame.
!
      IF ( A+B+C .EQ. 0.0D0 ) THEN

         X(1) = 1.0D0
         X(2) = 0.0D0
         X(3) = 0.0D0

         Y(1) = 0.0D0
         Y(2) = 1.0D0
         Y(3) = 0.0D0

         Z(1) = 0.0D0
         Z(2) = 0.0D0
         Z(3) = 1.0D0

         RETURN

      END IF


!
!     If we make it this far, determine which component of X has the
!     smallest magnitude.  This component will be zero in Y. The other
!     two components of X will put into Y swapped with the sign of
!     the first changed.  From there, Z can have only one possible
!     set of values which it gets from the smallest component
!     of X, the non-zero components of Y and the length of Y.
!
      IF      (       ( A .LE. B ) &
                .AND. ( A .LE. C ) ) THEN

         F  = DSQRT ( B + C )
         S1 = 1
         S2 = 2
         S3 = 3

      ELSE IF (       ( B .LE. A ) &
                .AND. ( B .LE. C ) ) THEN

         F  = DSQRT ( A + C )
         S1 = 2
         S2 = 3
         S3 = 1

      ELSE

         F  = DSQRT ( A + B )
         S1 = 3
         S2 = 1
         S3 = 2

      END IF

!
!     Note: by construction, F is the magnitude of the large components
!     of X.  With this in mind, one can verify by inspection that X, Y
!     and Z yield an orthonormal frame.  The right handedness follows
!     from the assignment of values to S1, S2 and S3 (they are merely
!     cycled from one case to the next).
!
      Y(S1)  =   0.0D0
      Y(S2)  = - X(S3) / F
      Y(S3)  =   X(S2) / F

      Z(S1)  =   F
      Z(S2)  = - X(S1) * Y(S3)
      Z(S3)  =   X(S1) * Y(S2)

      RETURN
      END
