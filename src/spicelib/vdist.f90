!$Procedure VDIST ( Vector distance )

      DOUBLE PRECISION FUNCTION  VDIST ( V1, V2 )

! $ Abstract
!
!     Return the distance between two three-dimensional vectors.
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

      DOUBLE PRECISION      V1 ( 3 )
      DOUBLE PRECISION      V2 ( 3 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!
!     V1,
!     V2         I   Two 3-vectors.
!
!     The function returns the distance between V1 and V2.
!
! $ Detailed_Input
!
!     V1,
!     V2       are two vectors in three-dimensional space, the
!              distance between which is desired.
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
!     This function is simply shorthand for the code
!
!        CALL VSUB ( V1, V2, DIFF )
!
!        DIST = VNORM ( DIFF )
!
!     Using this function saves you the annoyance of declaring local
!     storage for the difference vector DIFF.
!
!
!     The Euclidean norm of a three-dimensional vector (x, y, z) is
!     defined as
!
!                                     1/2
!             2        2        2
!        (   x    +   y    +   z    ).
!
!
!     This number is the distance of the point (x, y, z) from the
!     origin. If A and B are two vectors whose components are
!
!        ( A(1), A(2), A(3) )    and    ( B(1), B(2), B(3) ),
!
!     then the distance between A and B is the norm of the difference
!     A - B, which has components
!
!
!        (  A(1) - B(1),  A(2) - B(2),  A(3) - B(3)  ).
!
!
!     A related routine is VDISTG, which computes the distance between
!     two vectors of general dimension.
!
! $ Examples
!
!     1)  If V1 is
!
!            ( 2.0D0,  3.0D0,  0.D0 )
!
!         and V2 is
!
!            ( 5.0D0,  7.0D0,  12.D0 ),
!
!         VDIST (V1, V2) will be 13.D0.
!
!
!     2)  If VGR2 and NEP are states of the Voyager 2 spacecraft and
!         Neptune with respect to some common center at a given time
!         ET, then
!
!            VDIST ( VGR2, NEP )
!
!         yields the distance between the spacecraft and Neptune at time
!         ET.
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
!-    SPICELIB Version 1.0.0, 08-JUL-1990 (NJB)
!
!-&


! $ Index_Entries
!
!     distance between 3-dimensional vectors
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      VNORM

!
!     Local variables
!
      DOUBLE PRECISION      DIFF ( 3 )


!
!     No surprises.
!
      CALL VSUB ( V1, V2, DIFF )

      VDIST = VNORM (DIFF)

      RETURN
      END
