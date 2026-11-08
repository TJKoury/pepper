!$Procedure VDISTG ( Vector distance, general dimension )

      DOUBLE PRECISION FUNCTION VDISTG ( V1, V2, NDIM )

! $ Abstract
!
!     Return the distance between two vectors of arbitrary dimension.
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

      DOUBLE PRECISION      V1   ( * )
      DOUBLE PRECISION      V2   ( * )
      INTEGER               NDIM

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     V1,
!     V2         I   Two vectors of arbitrary dimension.
!     NDIM       I   The common dimension of V1 and V2
!
!     The function returns the distance between V1 and V2.
!
! $ Detailed_Input
!
!     V1,
!     V2       are two vectors of arbitrary dimension, the
!              distance between which is desired.
!
!     NDIM     is the common dimension of V1 and V2. NDIM must be
!              non-negative and must not exceed the minimum of the
!              declared sizes of the actual arguments corresponding
!              to V1 and V2.
!
! $ Detailed_Output
!
!     The function returns the distance between V1 and V2. This is
!     defined as
!
!        ||  V1 - V2  ||,
!
!     where || x || indicates the Euclidean norm of the vector x.
!
!     If NDIM is less than 1, the function value is set to 0.D0.
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
!     The Euclidean norm of an n-dimensional vector
!
!        (x ,  x , ... , x )
!          1    2         n
!
!     is defined as
!
!                                                1/2
!              2        2                  2
!        (   x    +   x    +  . . .  +   x     ).
!             1        2                  n
!
!     This number is the distance of the point (x, y, z) from the
!     origin. If n = 3, and A and B are two vectors whose components
!     are
!
!        ( A(1), A(2), A(3) )    and    ( B(1), B(2), B(3) ),
!
!     then the distance between A and B is the norm of the difference
!     A - B, which has components
!
!        (  A(1) - B(1),  A(2) - B(2),  A(3) - B(3)  ).
!
!     A related routine is VDIST, which computes the distance between
!     two 3-vectors.
!
! $ Examples
!
!     1)  If V1 is
!
!            ( 2.0D0,  3.0D0 )
!
!         and V2 is
!
!            ( 5.0D0,  7.0D0 ),
!
!         and NDIM is 2, then
!
!            VDISTG (V1, V2, NDIM )
!
!         will be 5.D0.
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
!-    SPICELIB Version 1.1.0, 25-MAY-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 17-JUL-1990 (NJB)
!
!-&


! $ Index_Entries
!
!     distance between n-dimensional vectors
!
!-&


!
!     Local variables
!
      DOUBLE PRECISION      SCALE
      INTEGER               I

!
!     We find the norm of a scaled version of the difference vector,
!     and then rescale this norm.  This method helps prevent overflow
!     due to squaring the components of the difference vector.
!
!     The code here is almost identical to that of VNORMG.  We'd love
!     to just call VNORMG, but that would require storage for the
!     difference vector.  So we do the job ourselves.
!

!
!     Find the scale factor.
!
      SCALE = 0.D0

      DO I = 1, NDIM
         SCALE = MAX (  SCALE,  DABS ( V1(I) - V2(I) )  )
      END DO


      IF ( SCALE .EQ. 0.D0 ) THEN

         VDISTG = 0.D0
         RETURN

      ELSE

         VDISTG = 0.D0

         DO I = 1, NDIM
            VDISTG = VDISTG  +  (  ( V1(I) - V2(I) ) / SCALE  ) ** 2
         END DO

         VDISTG = SCALE  *  SQRT ( VDISTG )

      END IF

      RETURN
      END
