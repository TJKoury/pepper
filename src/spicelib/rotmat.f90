!$Procedure ROTMAT ( Rotate a matrix )

      subroutine rotmat ( m1, angle, iaxis, mout )

! $ Abstract
!
!     Apply a rotation of ANGLE radians about axis IAXIS to a matrix.
!     This rotation is thought of as rotating the coordinate system.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     MATRIX
!     ROTATION
!
! $ Declarations

      implicit none

      double precision   m1    ( 3,3 )
      double precision   angle
      integer            iaxis
      double precision   mout  ( 3,3 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     M1         I   Matrix to be rotated.
!     ANGLE      I   Angle of rotation (radians).
!     IAXIS      I   Axis of rotation (X=1, Y=2, Z=3).
!     MOUT       O   Resulting rotated matrix [ANGLE]      * M1
!                                                   IAXIS
!
! $ Detailed_Input
!
!     M1       is a 3x3 matrix to which a rotation is to be applied. In
!              matrix algebra, the components of the matrix are relevant
!              in one particular coordinate system. Applying ROTMAT
!              changes the components of M1 so that they are relevant to
!              a rotated coordinate system.
!
!     ANGLE    is the angle in radians through which the original
!              coordinate system is to be rotated.
!
!     IAXIS    is the index for the axis of the original coordinate
!              system about which the rotation by ANGLE is to be
!              performed. IAXIS = 1,2 or 3 designates the X-, Y- or
!              Z-axis, respectively.
!
! $ Detailed_Output
!
!     MOUT     is the matrix resulting from the application of the
!              specified rotation to the input matrix M1. If
!
!                 [ANGLE]
!                        IAXIS
!
!              denotes the rotation matrix by ANGLE radians about IAXIS,
!              (refer to the routine ROTATE) then MOUT is given by the
!              following matrix equation:
!
!                 MOUT = [ANGLE]      * M1
!                               IAXIS
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If the axis index is not in the range 1 to 3, it will be
!         treated the same as that integer 1, 2, or 3 that is congruent
!         to it mod 3.
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
!     Suppose that to rotate a set of inertial axes to body fixed
!     axes, one must first roll the coordinate axes about the x-axis by
!     angle R to get x', y', z'. From this one must pitch about the y'
!     axis by angle P to get x'', y'', z''.  And finally yaw the x'',
!     y'', z'' about the z'' axis by angle Y to obtain the
!     transformation to bodyfixed coordinates. If ID is the identity
!     matrix, then the following code fragment generates the
!     transformation from inertial to body fixed.
!
!        CALL ROTMAT ( ID, R,     1,     M1   )
!        CALL ROTMAT ( M1, P,     2,     M2   )
!        CALL ROTMAT ( M2, Y,     3,     TIBF )
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
!     W.M. Owen          (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 27-MAY-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Reformatted
!        arguments' description.
!
!-    SPICELIB Version 1.0.2, 23-APR-2010 (NJB)
!
!        Header correction: assertions that the output
!        can overwrite the input have been removed.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
!
!-&


! $ Index_Entries
!
!     rotate a matrix
!
!-&


! $ Revisions
!
!-    Beta Version 1.1.0, 3-JAN-1989 (WLT)
!
!     Upgrade the routine to work with negative axis indexes. Also take
!     care of the funky way the indices (other than the input) were
!     obtained via the MOD function. It works but isn't as clear
!     (or fast) as just reading the axes from data.
!
!-&


      double precision      s
      double precision      c

      integer               temp
      integer               i1
      integer               i2
      integer               i3
      integer               i

      double precision      prodm   (3,3)

      integer               indexs  (5)
      save                  indexs

      data                  indexs  / 3, 1, 2, 3, 1 /
!
!  Get the sine and cosine of ANGLE
!
      s = dsin(angle)
      c = dcos(angle)
!
!  Get indices for axes. The first index is for the axis of rotation.
!  The next two axes follow in right hand order (XYZ).  First get the
!  non-negative value of IAXIS mod 3 .
!
      temp = mod ( mod(iaxis,3) + 3, 3 )

      i1   = indexs( temp + 1 )
      i2   = indexs( temp + 2 )
      i3   = indexs( temp + 3 )
!
!  Calculate the output matrix column by column
!
      do i=1,3
         prodm(i1,i) =  m1(i1,i)
         prodm(i2,i) =            c*m1(i2,i) + s*m1(i3,i)
         prodm(i3,i) =           -s*m1(i2,i) + c*m1(i3,i)
      end do
!
!  Move the buffered matrix into MOUT.
!
      call moved (prodm, 9, mout)
!
      return
      end
