!$Procedure ROTMAT ( Rotate a matrix )

      SUBROUTINE ROTMAT ( M1, ANGLE, IAXIS, MOUT )

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

      IMPLICIT NONE

      DOUBLE PRECISION   M1    ( 3,3 )
      DOUBLE PRECISION   ANGLE
      INTEGER            IAXIS
      DOUBLE PRECISION   MOUT  ( 3,3 )

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


      DOUBLE PRECISION      S
      DOUBLE PRECISION      C

      INTEGER               TEMP
      INTEGER               I1
      INTEGER               I2
      INTEGER               I3
      INTEGER               I

      DOUBLE PRECISION      PRODM   (3,3)

      INTEGER               INDEXS  (5)
      SAVE                  INDEXS

      DATA                  INDEXS  / 3, 1, 2, 3, 1 /
!
!  Get the sine and cosine of ANGLE
!
      S = DSIN(ANGLE)
      C = DCOS(ANGLE)
!
!  Get indices for axes. The first index is for the axis of rotation.
!  The next two axes follow in right hand order (XYZ).  First get the
!  non-negative value of IAXIS mod 3 .
!
      TEMP = MOD ( MOD(IAXIS,3) + 3, 3 )

      I1   = INDEXS( TEMP + 1 )
      I2   = INDEXS( TEMP + 2 )
      I3   = INDEXS( TEMP + 3 )
!
!  Calculate the output matrix column by column
!
      DO I=1,3
         PRODM(I1,I) =  M1(I1,I)
         PRODM(I2,I) =            C*M1(I2,I) + S*M1(I3,I)
         PRODM(I3,I) =           -S*M1(I2,I) + C*M1(I3,I)
      END DO
!
!  Move the buffered matrix into MOUT.
!
      CALL MOVED (PRODM, 9, MOUT)
!
      RETURN
      END
