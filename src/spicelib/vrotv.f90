!$Procedure VROTV ( Vector rotation about an axis )

      subroutine vrotv ( v, axis, theta, r )
      use pepper_kinds, only: wp

! $ Abstract
!
!     Rotate a vector about a specified axis vector by a specified
!     angle and return the rotated vector.
!
! $ Required_Reading
!
!     ROTATION
!
! $ Keywords
!
!     ROTATION
!     VECTOR
!
! $ Declarations

      implicit none

      real(wp)           v    ( 3 )
      real(wp)           axis ( 3 )
      real(wp)           theta
      real(wp)           r    ( 3 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     V          I   Vector to be rotated.
!     AXIS       I   Axis of the rotation.
!     THETA      I   Angle of rotation (radians).
!     R          O   Result of rotating V about AXIS by THETA.
!
! $ Detailed_Input
!
!     V        is a 3-dimensional vector to be rotated.
!
!     AXIS     is the axis about which the rotation is to be
!              performed.
!
!     THETA    is the angle through which V is to be rotated about
!              AXIS.
!
! $ Detailed_Output
!
!     R        is the result of rotating V about AXIS by THETA.
!              If AXIS is the zero vector, R = V.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If the input axis is the zero vector, R will be returned
!         as V.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine computes the result of rotating (in a right handed
!     sense) the vector V about the axis represented by AXIS through
!     an angle of THETA radians.
!
!     If W is a unit vector parallel to AXIS, then R is given by:
!
!         R = V + ( 1 - cos(THETA) ) Wx(WxV) + sin(THETA) (WxV)
!
!     where "x" above denotes the vector cross product.
!
! $ Examples
!
!     If AXIS = ( 0, 0, 1 ) and THETA = PI/2 then the following results
!     for R will be obtained
!
!             V                           R
!        -------------             ----------------
!        ( 1, 2, 3 )                ( -2, 1, 3 )
!        ( 1, 0, 0 )                (  0, 1, 0 )
!        ( 0, 1, 0 )                ( -1, 0, 0 )
!
!
!     If AXIS = ( 0, 1, 0 ) and THETA = PI/2 then the following results
!     for R will be obtained
!
!             V                           R
!        -------------             ----------------
!        ( 1, 2, 3 )                (  3, 2, -1 )
!        ( 1, 0, 0 )                (  0, 0, -1 )
!        ( 0, 1, 0 )                (  0, 1,  0 )
!
!
!     If AXIS = ( 1, 1, 1 ) and THETA = PI/2 then the following results
!     for R will be obtained
!
!             V                                     R
!        -----------------------------   -----------------------------
!        ( 1.0,     2.0,     3.0     )   ( 2.577.., 0.845.., 2.577.. )
!        ( 2.577.., 0.845.., 2.577.. )   ( 3.0      2.0,     1.0     )
!        ( 3.0      2.0,     1.0     )   ( 1.422.., 3.154.., 1.422.. )
!        ( 1.422.., 3.154.., 1.422.. )   ( 1.0      2.0,     3.0     )
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
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Removed
!        unnecessary $Revisions section.
!
!-    SPICELIB Version 1.0.2, 05-FEB-2003 (NJB)
!
!        Header examples were corrected.  $Exceptions section
!        filled in. Miscellaneous header corrections were made.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (NJB) (HAN)
!
!-&


! $ Index_Entries
!
!     vector rotation about an axis
!
!-&


!
!     SPICELIB functions
!
      real(wp) vnorm

!
!     Local Variables
!
      real(wp) c
      real(wp) s
      real(wp) rplane (3)
      real(wp) p      (3)
      real(wp) v1     (3)
      real(wp) v2     (3)
      real(wp) x      (3)


!
!     Just in case the user tries to rotate about the zero vector -
!     check, and if so return the input vector
!
      if ( vnorm( axis ) .eq. 0.0_wp ) then
         call moved (v,3,r)
         return
      end if

!
!     Compute the unit vector that lies in the direction of the
!     AXIS.  Call it X.
!
      call vhat ( axis, x )

!
!     Compute the projection of V onto AXIS.  Call it P.
!
      call vproj ( v, x, p )

!
!     Compute the component of V orthogonal to the AXIS.  Call it V1.
!
      call vsub  ( v, p,  v1 )

!
!     Rotate V1 by 90 degrees about the AXIS and call the result V2.
!
      call vcrss  ( x, v1, v2)

!
!     Compute COS(THETA)*V1 + SIN(THETA)*V2. This is V1 rotated about
!     the AXIS in the plane normal to the axis, call the result RPLANE
!
      c = dcos (theta)
      s = dsin (theta)

      call vlcom ( c, v1, s, v2, rplane )

!
!     Add the rotated component in the normal plane to AXIS to the
!     projection of V onto AXIS (P) to obtain R.
!
      call vadd ( rplane, p, r )

!
      return
      end
