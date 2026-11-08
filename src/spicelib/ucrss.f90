!$Procedure UCRSS ( Unitized cross product, 3x3 )

      SUBROUTINE UCRSS ( V1, V2, VOUT )

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

      IMPLICIT NONE

      DOUBLE PRECISION   V1   ( 3 )
      DOUBLE PRECISION   V2   ( 3 )
      DOUBLE PRECISION   VOUT ( 3 )

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
      DOUBLE PRECISION      VNORM

!
!     Local variables
!
      DOUBLE PRECISION      VCROSS(3)
      DOUBLE PRECISION      VMAG

      DOUBLE PRECISION      MAXV1
      DOUBLE PRECISION      MAXV2

      DOUBLE PRECISION      TV1 ( 3 )
      DOUBLE PRECISION      TV2 ( 3 )

!
!     Get the biggest component of each of the two vectors.
!
      MAXV1 = MAX ( DABS(V1(1)), DABS(V1(2)), DABS(V1(3)) )
      MAXV2 = MAX ( DABS(V2(1)), DABS(V2(2)), DABS(V2(3)) )

!
!     Scale V1 and V2 by 1/MAXV1 and 1/MAXV2 respectively
!
      IF ( MAXV1 .NE. 0 ) THEN
         TV1(1) = V1(1)/MAXV1
         TV1(2) = V1(2)/MAXV1
         TV1(3) = V1(3)/MAXV1
      ELSE
         TV1(1) = 0.0D0
         TV1(2) = 0.0D0
         TV1(3) = 0.0D0
      END IF

      IF ( MAXV2 .NE. 0 ) THEN
         TV2(1) = V2(1)/MAXV2
         TV2(2) = V2(2)/MAXV2
         TV2(3) = V2(3)/MAXV2
      ELSE
         TV2(1) = 0.0D0
         TV2(2) = 0.0D0
         TV2(3) = 0.0D0
      END IF

!
!  Calculate the cross product of V1 and V2
!
      VCROSS(1) = TV1(2)*TV2(3) - TV1(3)*TV2(2)
      VCROSS(2) = TV1(3)*TV2(1) - TV1(1)*TV2(3)
      VCROSS(3) = TV1(1)*TV2(2) - TV1(2)*TV2(1)

!
!  Get the magnitude of VCROSS and normalize it
!
      VMAG = VNORM(VCROSS)

      IF (VMAG.GT.0.D0) THEN
         VOUT(1) = VCROSS(1) / VMAG
         VOUT(2) = VCROSS(2) / VMAG
         VOUT(3) = VCROSS(3) / VMAG
      ELSE
         VOUT(1) = 0.D0
         VOUT(2) = 0.D0
         VOUT(3) = 0.D0
      END IF

      RETURN
      END
