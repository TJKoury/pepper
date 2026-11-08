!$Procedure CGV2EL ( Center and generating vectors to ellipse )

      subroutine cgv2el ( center, vec1, vec2, ellips )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Form a SPICE ellipse from a center vector and two generating
!     vectors.
!
! $ Required_Reading
!
!     ELLIPSES
!
! $ Keywords
!
!     ELLIPSE
!     GEOMETRY
!
! $ Declarations

      implicit none

      integer(ip)           ubel
      parameter           ( ubel    =   9 )

      real(wp)              center (    3 )
      real(wp)              vec1   (    3 )
      real(wp)              vec2   (    3 )
      real(wp)              ellips ( ubel )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     CENTER,
!     VEC1,
!     VEC2       I   Center and two generating vectors for an ellipse.
!     ELLIPS     O   The SPICE ellipse defined by the input vectors.
!
! $ Detailed_Input
!
!     CENTER,
!     VEC1,
!     VEC2     are a center and two generating vectors defining
!              an ellipse in three-dimensional space. The
!              ellipse is the set of points
!
!                 CENTER  +  cos(theta) VEC1  +  sin(theta) VEC2
!
!              where theta ranges over the interval (-pi, pi].
!              VEC1 and VEC2 need not be linearly independent.
!
! $ Detailed_Output
!
!     ELLIPS   is the SPICE ellipse defined by the input
!              vectors.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If VEC1 and VEC2 are linearly dependent, ELLIPS will be
!         degenerate. SPICE ellipses are allowed to represent
!         degenerate geometric ellipses.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     SPICE ellipses serve to simplify calling sequences and reduce
!     the chance for error in declaring and describing argument lists
!     involving ellipses.
!
!     The set of ellipse conversion routines is
!
!        CGV2EL ( Center and generating vectors to ellipse )
!        EL2CGV ( Ellipse to center and generating vectors )
!
! $ Examples
!
!     The numerical results shown for these examples may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Create a SPICE ellipse given its center and two linearly
!        independent generating vectors of the ellipse.
!
!
!        Example code begins here.
!
!
!              PROGRAM CGV2EL_EX1
!              IMPLICIT NONE
!
!        C
!        C     Local constants.
!        C
!              INTEGER                 UBEL
!              PARAMETER             ( UBEL =   9 )
!
!        C
!        C     Local variables.
!        C
!              DOUBLE PRECISION        CENTER ( 3    )
!              DOUBLE PRECISION        ECENTR ( 3    )
!              DOUBLE PRECISION        ELLIPS ( UBEL )
!              DOUBLE PRECISION        SMAJOR ( 3    )
!              DOUBLE PRECISION        SMINOR ( 3    )
!              DOUBLE PRECISION        VEC1   ( 3    )
!              DOUBLE PRECISION        VEC2   ( 3    )
!
!              INTEGER                 I
!
!        C
!        C     Define the center and two linearly independent
!        C     generating vectors of an ellipse (the vectors need not
!        C     be linearly independent).
!        C
!              DATA                    CENTER / -1.D0,  1.D0, -1.D0 /
!
!              DATA                    VEC1   /  1.D0,  1.D0,  1.D0 /
!
!              DATA                    VEC2   /  1.D0, -1.D0,  1.D0 /
!
!        C
!        C     Create the ELLIPS.
!        C
!              CALL CGV2EL ( CENTER, VEC1, VEC2, ELLIPS )
!
!        C
!        C     In a real application, please use SPICELIB API EL2CGV
!        C     to retrieve the center and generating vectors from the
!        C     ellipse structure (see next block).
!        C
!              WRITE(*,'(A)') 'SPICE ellipse:'
!              WRITE(*,'(A,3F10.6)') '  Semi-minor axis:',
!             .                                    ( ELLIPS(I), I=7,9 )
!              WRITE(*,'(A,3F10.6)') '  Semi-major axis:',
!             .                                    ( ELLIPS(I), I=4,6 )
!              WRITE(*,'(A,3F10.6)') '  Center         :',
!             .                                    ( ELLIPS(I), I=1,3 )
!              WRITE(*,*) ' '
!
!        C
!        C     Obtain the center and generating vectors from the
!        C     ELLIPS.
!        C
!              CALL EL2CGV ( ELLIPS, ECENTR, SMAJOR, SMINOR )
!
!              WRITE(*,'(A)') 'SPICE ellipse (using EL2CGV):'
!              WRITE(*,'(A,3F10.6)') '  Semi-minor axis:', SMINOR
!              WRITE(*,'(A,3F10.6)') '  Semi-major axis:', SMAJOR
!              WRITE(*,'(A,3F10.6)') '  Center         :', ECENTR
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        SPICE ellipse:
!          Semi-minor axis:  0.000000  1.414214  0.000000
!          Semi-major axis:  1.414214 -0.000000  1.414214
!          Center         : -1.000000  1.000000 -1.000000
!
!        SPICE ellipse (using EL2CGV):
!          Semi-minor axis:  0.000000  1.414214  0.000000
!          Semi-major axis:  1.414214 -0.000000  1.414214
!          Center         : -1.000000  1.000000 -1.000000
!
!
!     2) Find the intersection of an ellipse with a plane.
!
!
!        Example code begins here.
!
!
!              PROGRAM CGV2EL_EX2
!              IMPLICIT NONE
!
!        C
!        C     Local constants.
!        C
!              INTEGER                 UBEL
!              PARAMETER             ( UBEL =   9 )
!
!              INTEGER                 UBPL
!              PARAMETER             ( UBPL =   4 )
!
!        C
!        C     Local variables.
!        C
!              DOUBLE PRECISION        CENTER ( 3    )
!              DOUBLE PRECISION        ELLIPS ( UBEL )
!              DOUBLE PRECISION        NORMAL ( 3    )
!              DOUBLE PRECISION        PLANE  ( UBPL )
!              DOUBLE PRECISION        VEC1   ( 3    )
!              DOUBLE PRECISION        VEC2   ( 3    )
!              DOUBLE PRECISION        XPTS   ( 3, 2 )
!
!              INTEGER                 I
!              INTEGER                 NXPTS
!
!        C
!        C     The ellipse is defined by the vectors CENTER, VEC1, and
!        C     VEC2. The plane is defined by the normal vector NORMAL
!        C     and the CENTER.
!        C
!              DATA                    CENTER /  0.D0,  0.D0,  0.D0 /
!              DATA                    VEC1   /  1.D0,  7.D0,  2.D0 /
!              DATA                    VEC2   / -1.D0,  1.D0,  3.D0 /
!
!              DATA                    NORMAL /  0.D0,  1.D0,  0.D0 /
!
!        C
!        C     Make a SPICE ellipse and a plane.
!        C
!              CALL CGV2EL ( CENTER, VEC1, VEC2, ELLIPS )
!              CALL NVP2PL ( NORMAL, CENTER,     PLANE  )
!
!        C
!        C     Find the intersection of the ellipse and plane.
!        C     NXPTS is the number of intersection points; XPTS
!        C     are the points themselves.
!        C
!              CALL INELPL ( ELLIPS,    PLANE,    NXPTS,
!             .              XPTS(1,1), XPTS(1,2)       )
!
!              WRITE(*,'(A,I2)') 'Number of intercept points: ', NXPTS
!
!              DO I = 1, NXPTS
!
!                 WRITE(*,'(A,I2,A,3F10.6)') '  Point', I, ':',
!             .                         XPTS(1,I), XPTS(2,I), XPTS(3,I)
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
!        Number of intercept points:  2
!          Point 1:  1.131371  0.000000 -2.687006
!          Point 2: -1.131371 -0.000000  2.687006
!
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
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 24-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!        Added complete code example.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 02-NOV-1990 (NJB)
!
!-&


! $ Index_Entries
!
!     center and generating vectors to ellipse
!
!-&


!
!     SPICELIB functions
!
      logical               return

!
!     Local parameters
!

!
!     SPICE ellipses contain a center vector, a semi-major
!     axis vector, and a semi-minor axis vector.  These are
!     located, respectively, in elements
!
!        CTRPOS through CTRPOS + 1
!
!        MAJPOS through MAJPOS + 1
!
!        MINPOS through MINPOS + 1
!
!
      integer(ip)           ctrpos
      parameter           ( ctrpos = 1 )

      integer(ip)           majpos
      parameter           ( majpos = 4 )

      integer(ip)           minpos
      parameter           ( minpos = 7 )


!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     The center of the ellipse is held in the first three elements.
!
      call vequ ( center, ellips(ctrpos) )

!
!     Find the semi-axes of the ellipse.  These may be degenerate.
!
      call saelgv ( vec1, vec2, ellips(majpos), ellips(minpos) )

      return
      end
