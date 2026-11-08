!$Procedure VLCOMG ( Vector linear combination, general dimension )

      SUBROUTINE VLCOMG ( N, A, V1, B, V2, SUM )

! $ Abstract
!
!     Compute a vector linear combination of two double precision
!     vectors of arbitrary dimension.
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
!     None.
!
! $ Keywords
!
!     VECTOR
!
! $ Declarations

      IMPLICIT NONE

      INTEGER            N
      DOUBLE PRECISION   A
      DOUBLE PRECISION   V1  ( N )
      DOUBLE PRECISION   B
      DOUBLE PRECISION   V2  ( N )
      DOUBLE PRECISION   SUM ( N )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     N          I   Dimension of vector space.
!     A          I   Coefficient of V1.
!     V1         I   Vector in N-space.
!     B          I   Coefficient of V2.
!     V2         I   Vector in N-space.
!     SUM        O   Linear vector combination A*V1 + B*V2.
!
! $ Detailed_Input
!
!     N        is the dimension of V1, V2 and SUM.
!
!     A        is the double precision scalar variable that multiplies
!              V1.
!
!     V1       is an arbitrary, double precision n-dimensional vector.
!
!     B        is the double precision scalar variable that multiplies
!              V2.
!
!     V2       is an arbitrary, double precision n-dimensional vector.
!
! $ Detailed_Output
!
!     SUM      is the double precision n-dimensional vector which
!              contains the linear combination
!
!                 A * V1 + B * V2
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
!        For each value of the index I, from 1 to N:
!
!           SUM(I) = A * V1(I) + B * V2(I)
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
!     1) Perform the projection of a 4-dimensional vector into a
!        2-dimensional plane in 4-space.
!
!
!        Example code begins here.
!
!
!              PROGRAM VLCOMG_EX1
!              IMPLICIT NONE
!
!        C
!        C     SPICELIB functions.
!        C
!              DOUBLE PRECISION      VDOTG
!
!        C
!        C     Local parameters.
!        C
!              INTEGER               NDIM
!              PARAMETER           ( NDIM = 4 )
!
!        C
!        C     Local variables.
!        C
!              DOUBLE PRECISION      PUV    ( NDIM )
!              DOUBLE PRECISION      X      ( NDIM )
!              DOUBLE PRECISION      U      ( NDIM )
!              DOUBLE PRECISION      V      ( NDIM )
!
!        C
!        C     Let X be an arbitrary NDIM-vector
!        C
!              DATA                  X  /  4.D0, 35.D0, -5.D0, 7.D0  /
!
!        C
!        C     Let U and V be orthonormal NDIM-vectors spanning the
!        C     plane of interest.
!        C
!              DATA                  U  /  0.D0,  0.D0,  1.D0, 0.D0 /
!
!              V(1) =  SQRT(3.D0)/3.D0
!              V(2) = -SQRT(3.D0)/3.D0
!              V(3) =  0.D0
!              V(4) =  SQRT(3.D0)/3.D0
!
!        C
!        C     Compute the projection of X onto this 2-dimensional
!        C     plane in NDIM-space.
!        C
!              CALL VLCOMG ( NDIM, VDOTG ( X, U, NDIM), U,
!             .                    VDOTG ( X, V, NDIM), V, PUV )
!
!        C
!        C     Display the results.
!        C
!              WRITE(*,'(A,4F6.1)') 'Input vector             : ', X
!              WRITE(*,'(A,4F6.1)') 'Projection into 2-d plane: ', PUV
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        Input vector             :    4.0  35.0  -5.0   7.0
!        Projection into 2-d plane:   -8.0   8.0  -5.0  -8.0
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
!-    SPICELIB Version 1.1.0, 13-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Removed
!        unnecessary $Revisions section.
!
!        Added complete code example based on existing example.
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
!     linear combination of two n-dimensional vectors
!
!-&


!
!     Local variables
!
      INTEGER        I

      DO I = 1,N
         SUM(I) = A*V1(I) + B*V2(I)
      END DO

      RETURN
      END
