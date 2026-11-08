!$Procedure UCRSS ( Unitized cross product, 3x3 )

      subroutine ucrss ( v1, v2, vout )

! $ Abstract
!
!     Compute the normalized cross product of two 3-vectors.
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

      double precision   v1   ( 3 )
      double precision   v2   ( 3 )
      double precision   vout ( 3 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     V1         I   Left vector for cross product.
!     V2         I   Right vector for cross product.
!     VOUT       O   Normalized cross product of V1 and V2.
!
! $ Detailed_Input
!
!     V1,
!     V2       are two double precision 3-dimensional vectors.
!              Typically, these might represent the (possibly unit)
!              vector to a planet, Sun, or a star which defines the
!              orientation of axes of some reference frame.
!
! $ Detailed_Output
!
!     VOUT     is the double precision 3-dimensional normalized cross
!              product of V1 and V2. VOUT is the result of the
!              computation
!
!                     V1 x V2
!                 ---------------
!                  || V1 x V2 ||
!
!              where "x" denotes the cross product and ||X||| the norm
!              of a vector X.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If the cross product of V1 and V2 yields the zero-vector,
!         then the zero-vector is returned instead of a vector of
!         unit length.
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
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Define two sets of vectors and compute the normalized cross
!        product of each vector in first set and the corresponding
!        vector in the second set.
!
!
!        Example code begins here.
!
!
!              PROGRAM UCRSS_EX1
!              IMPLICIT NONE
!
!        C
!        C     Local parameters.
!        C
!              INTEGER               NDIM
!              PARAMETER           ( NDIM   = 3 )
!
!              INTEGER               SETSIZ
!              PARAMETER           ( SETSIZ = 2 )
!
!        C
!        C     Local variables.
!        C
!              DOUBLE PRECISION      V1   ( NDIM, SETSIZ )
!              DOUBLE PRECISION      V2   ( NDIM, SETSIZ )
!              DOUBLE PRECISION      VOUT ( NDIM )
!
!              INTEGER               I
!              INTEGER               J
!
!        C
!        C     Define the two vector sets.
!        C
!              DATA                  V1   / 0.D0,  1.D0,  0.D0,
!             .                             5.D0,  5.D0,  5.D0  /
!
!              DATA                  V2   / 3.D0,  0.D0,  0.D0,
!             .                            -2.D0, -2.D0, -2.D0  /
!
!        C
!        C     Calculate the cross product of each pair of vectors
!        C
!              DO I=1, SETSIZ
!
!                 CALL UCRSS ( V1(1,I), V2(1,I), VOUT )
!
!                 WRITE(*,'(A,3F5.1)') 'Vector A                : ',
!             .                                  ( V1(J,I), J=1,3 )
!                 WRITE(*,'(A,3F5.1)') 'Vector B                : ',
!             .                                  ( V2(J,I), J=1,3 )
!                 WRITE(*,'(A,3F5.1)') 'Normalized cross product: ',
!             .                                                VOUT
!                 WRITE(*,*) ' '
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
!        Vector A                :   0.0  1.0  0.0
!        Vector B                :   3.0  0.0  0.0
!        Normalized cross product:   0.0  0.0 -1.0
!
!        Vector A                :   5.0  5.0  5.0
!        Vector B                :  -2.0 -2.0 -2.0
!        Normalized cross product:   0.0  0.0  0.0
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
!     J. Diaz del Rio    (ODC Space)
!     W.M. Owen          (JPL)
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
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) (WLT)
!
!-&


! $ Index_Entries
!
!     unitized cross product
!
!-&


! $ Revisions
!
!-    Beta Version 1.1.0, 10-JAN-1989 (WLT)
!
!        Error free specification added. In addition the algorithm was
!        made more robust in the sense that floating point overflows
!        cannot occur.
!
!-&


!
!     SPICELIB functions
!
      double precision      vnorm

!
!     Local variables
!
      double precision      vcross(3)
      double precision      vmag

      double precision      maxv1
      double precision      maxv2

      double precision      tv1 ( 3 )
      double precision      tv2 ( 3 )

!
!     Get the biggest component of each of the two vectors.
!
      maxv1 = max ( dabs(v1(1)), dabs(v1(2)), dabs(v1(3)) )
      maxv2 = max ( dabs(v2(1)), dabs(v2(2)), dabs(v2(3)) )

!
!     Scale V1 and V2 by 1/MAXV1 and 1/MAXV2 respectively
!
      if ( maxv1 .ne. 0 ) then
         tv1(1) = v1(1)/maxv1
         tv1(2) = v1(2)/maxv1
         tv1(3) = v1(3)/maxv1
      else
         tv1(1) = 0.0d0
         tv1(2) = 0.0d0
         tv1(3) = 0.0d0
      end if

      if ( maxv2 .ne. 0 ) then
         tv2(1) = v2(1)/maxv2
         tv2(2) = v2(2)/maxv2
         tv2(3) = v2(3)/maxv2
      else
         tv2(1) = 0.0d0
         tv2(2) = 0.0d0
         tv2(3) = 0.0d0
      end if

!
!  Calculate the cross product of V1 and V2
!
      vcross(1) = tv1(2)*tv2(3) - tv1(3)*tv2(2)
      vcross(2) = tv1(3)*tv2(1) - tv1(1)*tv2(3)
      vcross(3) = tv1(1)*tv2(2) - tv1(2)*tv2(1)

!
!  Get the magnitude of VCROSS and normalize it
!
      vmag = vnorm(vcross)

      if (vmag.gt.0.d0) then
         vout(1) = vcross(1) / vmag
         vout(2) = vcross(2) / vmag
         vout(3) = vcross(3) / vmag
      else
         vout(1) = 0.d0
         vout(2) = 0.d0
         vout(3) = 0.d0
      end if

      return
      end
