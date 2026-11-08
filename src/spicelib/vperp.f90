!$Procedure VPERP ( Perpendicular component of a 3-vector )

      SUBROUTINE VPERP ( A, B, P )

! $ Abstract
!
!     Find the component of a vector that is perpendicular to a second
!     vector. All vectors are 3-dimensional.
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

      IMPLICIT NONE

      DOUBLE PRECISION   A ( 3 )
      DOUBLE PRECISION   B ( 3 )
      DOUBLE PRECISION   P ( 3 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     A          I   The vector whose orthogonal component is sought.
!     B          I   The vector used as the orthogonal reference.
!     P          O   The component of A orthogonal to B.
!
! $ Detailed_Input
!
!     A        is a double precision, 3-dimensional vector. It the
!              vector whose component orthogonal to B is sought. (There
!              is a unique decomposition of A into a sum V + P, where V
!              is parallel to B and P is orthogonal to B. We want the
!              component P.)
!
!     B        is a double precision, 3-dimensional vector. This
!              vector is the vector used as a reference for the
!              decomposition of A.
!
! $ Detailed_Output
!
!     P        is a double precision, 3-dimensional vector containing
!              the component of A that is orthogonal to B.
!              P may overwrite either A or B.
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
!     Given and non-zero vector B and a vector A, there is a unique
!     decomposition of A as a sum V + P such that P is orthogonal
!     to B and V is parallel to B. This routine finds the vector P.
!
!     If B is a zero vector, P will be identical to A.
!
! $ Examples
!
!     The following table gives sample inputs and results from calling
!     VPERP.
!
!        A                  B                 P
!        ------------------------------------------
!        (6, 6, 6)      ( 2, 0, 0)        (0, 6, 6)
!        (6, 6, 6)      (-3, 0, 0)        (0, 6, 6)
!        (6, 6, 0)      ( 0, 7, 0)        (6, 0, 0)
!        (6, 0, 0)      ( 0, 0, 9)        (6, 0, 0)
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     [1]  G. Thomas and R. Finney, "Calculus and Analytic Geometry,"
!          7th Edition, Addison Wesley, 1988.
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
!-    SPICELIB Version 1.2.0, 16-SEP-2020 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.1.1, 11-MAY-2010 (EDW)
!
!        Minor edit to code comments eliminating typo.
!
!        Reordered header sections to proper NAIF convention.
!        Removed Revision section, it listed a duplication of a
!        $Version section entry.
!
!-    SPICELIB Version 1.1.0, 09-SEP-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in VSCL call.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
!
!-&


! $ Index_Entries
!
!     perpendicular component of a 3-vector
!
!-&


!
!     Local variables
!
      DOUBLE PRECISION      BIGA
      DOUBLE PRECISION      BIGB
      DOUBLE PRECISION      R(3)
      DOUBLE PRECISION      T(3)
      DOUBLE PRECISION      V(3)


!
!     Error free routine:  no check-in.
!
      BIGA = MAX ( DABS(A(1)), DABS(A(2)), DABS(A(3)) )
      BIGB = MAX ( DABS(B(1)), DABS(B(2)), DABS(B(3)) )

!
!     If A is the zero vector, just set P to zero and return.
!
      IF ( BIGA .EQ. 0.0D0 ) THEN

         P(1) = 0.0D0
         P(2) = 0.0D0
         P(3) = 0.0D0
         RETURN

      END IF

!
!     If B is the zero vector, then set P equal to A.
!
      IF ( BIGB .EQ. 0.0D0 ) THEN

         P(1) = A(1)
         P(2) = A(2)
         P(3) = A(3)
         RETURN

      END IF

      T(1) = A(1) / BIGA
      T(2) = A(2) / BIGA
      T(3) = A(3) / BIGA

      R(1) = B(1) / BIGB
      R(2) = B(2) / BIGB
      R(3) = B(3) / BIGB

      CALL VPROJ  ( T,    R, V )
      CALL VSUB   ( T,    V, P )
      CALL VSCLIP ( BIGA,    P )

      RETURN
      END
