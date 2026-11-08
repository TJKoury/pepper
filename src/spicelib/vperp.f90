!$Procedure VPERP ( Perpendicular component of a 3-vector )

      subroutine vperp ( a, b, p )
      use pepper_kinds, only: wp

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

      implicit none

      real(wp)           a ( 3 )
      real(wp)           b ( 3 )
      real(wp)           p ( 3 )

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
      real(wp)              biga
      real(wp)              bigb
      real(wp)              r(3)
      real(wp)              t(3)
      real(wp)              v(3)


!
!     Error free routine:  no check-in.
!
      biga = max ( dabs(a(1)), dabs(a(2)), dabs(a(3)) )
      bigb = max ( dabs(b(1)), dabs(b(2)), dabs(b(3)) )

!
!     If A is the zero vector, just set P to zero and return.
!
      if ( biga .eq. 0.0_wp ) then

         p(1) = 0.0_wp
         p(2) = 0.0_wp
         p(3) = 0.0_wp
         return

      end if

!
!     If B is the zero vector, then set P equal to A.
!
      if ( bigb .eq. 0.0_wp ) then

         p(1) = a(1)
         p(2) = a(2)
         p(3) = a(3)
         return

      end if

      t(1) = a(1) / biga
      t(2) = a(2) / biga
      t(3) = a(3) / biga

      r(1) = b(1) / bigb
      r(2) = b(2) / bigb
      r(3) = b(3) / bigb

      call vproj  ( t,    r, v )
      call vsub   ( t,    v, p )
      call vsclip ( biga,    p )

      return
      end
