!$Procedure VPROJG ( Vector projection, general dimension )

      subroutine vprojg ( a, b, ndim, p )

! $ Abstract
!
!     Compute the projection of one vector onto another vector. All
!     vectors are of arbitrary dimension.
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

      integer            ndim
      double precision   a ( ndim )
      double precision   b ( ndim )
      double precision   p ( ndim )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     A          I   The vector to be projected.
!     B          I   The vector onto which A is to be projected.
!     NDIM       I   Dimension of A, B, and P.
!     P          O   The projection of A onto B.
!
! $ Detailed_Input
!
!     A        is a double precision vector of arbitrary dimension.
!              This vector is to be projected onto the vector B.
!
!     B        is a double precision vector of arbitrary dimension.
!              This vector is the vector which receives the
!              projection.
!
!     NDIM     is the dimension of A, B and P.
!
! $ Detailed_Output
!
!     P        is a double precision vector of arbitrary dimension
!              containing the projection of A onto B. (P is
!              necessarily parallel to B.)
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
!     The projection of a vector A onto a vector B is, by definition,
!     that component of A which is parallel to B. To find this
!     component it is enough to find the scalar ratio of the length of
!     B to the projection of A onto B, and then use this number to
!     scale the length of B. This ratio is given by
!
!         RATIO = < A, B > / < B, B >
!
!     where <,> denotes the general vector dot product. This routine
!     does not attempt to divide by zero in the event that B is the
!     zero vector.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Define two sets of vectors and compute the projection of
!        each vector of the first set on the corresponding vector of
!        the second set.
!
!        Example code begins here.
!
!
!              PROGRAM VPROJG_EX1
!              IMPLICIT NONE
!
!        C
!        C     Local parameters.
!        C
!              INTEGER               NDIM
!              PARAMETER           ( NDIM   = 4 )
!
!              INTEGER               SETSIZ
!              PARAMETER           ( SETSIZ = 4 )
!
!        C
!        C     Local variables.
!        C
!              DOUBLE PRECISION      SETA ( NDIM, SETSIZ )
!              DOUBLE PRECISION      SETB ( NDIM, SETSIZ )
!              DOUBLE PRECISION      PVEC ( NDIM )
!
!              INTEGER               I
!              INTEGER               M
!
!        C
!        C     Define the two vector sets.
!        C
!              DATA                  SETA / 6.D0,  6.D0,  6.D0,  0.D0,
!             .                             6.D0,  6.D0,  6.D0,  0.D0,
!             .                             6.D0,  6.D0,  0.D0,  0.D0,
!             .                             6.D0,  0.D0,  0.D0,  0.D0 /
!
!              DATA                  SETB / 2.D0,  0.D0,  0.D0,  0.D0,
!             .                            -3.D0,  0.D0,  0.D0,  0.D0,
!             .                             0.D0,  7.D0,  0.D0,  0.D0,
!             .                             0.D0,  0.D0,  9.D0,  0.D0 /
!
!        C
!        C     Calculate the projection
!        C
!              DO I=1, SETSIZ
!
!                 CALL VPROJG ( SETA(1,I), SETB(1,I), NDIM, PVEC )
!                 WRITE(*,'(A,4F5.1)') 'Vector A  : ',
!             .                     ( SETA(M,I), M = 1, NDIM )
!                 WRITE(*,'(A,4F5.1)') 'Vector B  : ',
!             .                     ( SETB(M,I), M = 1, NDIM )
!                 WRITE(*,'(A,4F5.1)') 'Projection: ', PVEC
!                 WRITE(*,*)
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
!        Vector A  :   6.0  6.0  6.0  0.0
!        Vector B  :   2.0  0.0  0.0  0.0
!        Projection:   6.0  0.0  0.0  0.0
!
!        Vector A  :   6.0  6.0  6.0  0.0
!        Vector B  :  -3.0  0.0  0.0  0.0
!        Projection:   6.0 -0.0 -0.0 -0.0
!
!        Vector A  :   6.0  6.0  0.0  0.0
!        Vector B  :   0.0  7.0  0.0  0.0
!        Projection:   0.0  6.0  0.0  0.0
!
!        Vector A  :   6.0  0.0  0.0  0.0
!        Vector B  :   0.0  0.0  9.0  0.0
!        Projection:   0.0  0.0  0.0  0.0
!
!
! $ Restrictions
!
!     1)  No error detection or recovery schemes are incorporated into
!         this routine except to insure that no attempt is made to
!         divide by zero. Thus, the user is required to make sure that
!         the vectors A and B are such that no floating point overflow
!         will occur when the dot products are calculated.
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
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Corrected math
!        expression in $Particulars section. Removed unnecessary
!        $Revisions section.
!
!        Added complete code example to $Examples section based on
!        existing example.
!
!-    SPICELIB Version 1.0.3, 23-APR-2010 (NJB)
!
!        Header correction: assertions that the output
!        can overwrite the input have been removed.
!
!-    SPICELIB Version 1.0.2, 22-AUG-2001 (EDW)
!
!        Corrected ENDIF to END IF.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (HAN) (NJB)
!
!-&


! $ Index_Entries
!
!     n-dimensional vector projection
!
!-&


!
!     SPICELIB functions
!
      double precision vdotg

!
!     Local variables
!
      double precision adotb
      double precision bdotb
      double precision scale


      adotb  = vdotg (a,b,ndim)
      bdotb  = vdotg (b,b,ndim)

      if ( bdotb .eq. 0.d0 ) then
         scale = 0.d0
      else
         scale = adotb/bdotb
      end if

      call vsclg (scale, b, ndim, p)

      return
      end
