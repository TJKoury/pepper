!> Rotations and Euler angles, frames defined by two vectors, the built-in inertial frames (CHGIRF), and the precession, nutation and obliquity models used by dynamic frames.
!>
!> The procedures keep SPICELIB's names and documentation; the context is
!> their first argument (interfaces and type-bound procedures in
!> [[pepper_instance]]). Moved here from src/spicelib by
!> tools/modularize_ctx.py; the external procedures with the original
!> interface are in tspice/src/legacy/pepper_legacy_rotations.f90.
submodule (pepper_instance) pepper_rotations

   implicit none

contains

      !>## Detailed Input
      !>
      !>     ANGLE    is the angle, given in radians, through which the
      !>              rotation is performed.
      !>
      !>     IAXIS    is the index of the axis of rotation. The X, Y, and Z
      !>              axes have indices 1, 2 and 3 respectively.
      !>
      !>## Detailed Output
      !>
      !>     MOUT     is the rotation matrix which describes the rotation of
      !>              a reference frame through ANGLE radians about the axis
      !>              whose index is IAXIS.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If the axis index is not in the range 1 to 3, it will be
      !>         treated the same as that integer 1, 2, or 3 that is congruent
      !>         to it mod 3.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     A rotation about the first, i.e. x-axis, is described by
      !>
      !>        .-                            -.
      !>        |  1        0           0      |
      !>        |  0   cos(theta)  sin(theta)  |
      !>        |  0  -sin(theta)  cos(theta)  |
      !>        `-                            -'
      !>
      !>     A rotation about the second, i.e. y-axis, is described by
      !>
      !>        .-                            -.
      !>        |  cos(theta)  0  -sin(theta)  |
      !>        |      0       1        0      |
      !>        |  sin(theta)  0   cos(theta)  |
      !>        `-                            -'
      !>
      !>     A rotation about the third, i.e. z-axis, is described by
      !>
      !>        .-                            -.
      !>        |  cos(theta)  sin(theta)  0   |
      !>        | -sin(theta)  cos(theta)  0   |
      !>        |       0          0       1   |
      !>        `-                            -'
      !>
      !>     ROTATE decides which form is appropriate according to the value
      !>     of IAXIS.
      !>
      !>## Examples
      !>
      !>     If ROTATE is called from a FORTRAN program as follows:
      !>
      !>           CALL ROTATE (PI/4, 3, MOUT)
      !>
      !>     then MOUT will be given by
      !>
      !>                  | SQRT(2)/2   SQRT(2)/2   0  |
      !>           MOUT = |-SQRT(2)/2   SQRT(2)/2   0  |
      !>                  |     0           0       1  |
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.M. Owen          (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 25-MAY-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) (WLT)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 1.1.0, 03-JAN-1989 (WLT)
      !>
      !>     Upgrade the routine to work with negative axis indexes. Also take
      !>     care of the funky way the indices (other than the input) were
      !>     obtained via the MOD function. It works but isn't as clear
      !>     (or fast) as just reading the axes from data.
      module procedure rotate
      use pepper_kinds, only: ip, wp
      implicit none

      real(wp)              s
      real(wp)              c

      integer(ip)           temp
      integer(ip)           i1
      integer(ip)           i2
      integer(ip)           i3
      integer(ip), parameter :: indexs ( 5 ) = &
         [ 3, 1, 2, 3, 1 ]

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

      i1 = indexs( temp + 1 )
      i2 = indexs( temp + 2 )
      i3 = indexs( temp + 3 )

!
!  Construct the rotation matrix
!
      mout(i1,i1) = 1.0_wp
      mout(i2,i1) = 0.0_wp
      mout(i3,i1) = 0.0_wp
      mout(i1,i2) = 0.0_wp
      mout(i2,i2) = c
      mout(i3,i2) = -s
      mout(i1,i3) = 0.0_wp
      mout(i2,i3) = s
      mout(i3,i3) = c
      return
      end procedure rotate

      !>## Detailed Input
      !>
      !>     M1       is a 3x3 matrix to which a rotation is to be applied. In
      !>              matrix algebra, the components of the matrix are relevant
      !>              in one particular coordinate system. Applying ROTMAT
      !>              changes the components of M1 so that they are relevant to
      !>              a rotated coordinate system.
      !>
      !>     ANGLE    is the angle in radians through which the original
      !>              coordinate system is to be rotated.
      !>
      !>     IAXIS    is the index for the axis of the original coordinate
      !>              system about which the rotation by ANGLE is to be
      !>              performed. IAXIS = 1,2 or 3 designates the X-, Y- or
      !>              Z-axis, respectively.
      !>
      !>## Detailed Output
      !>
      !>     MOUT     is the matrix resulting from the application of the
      !>              specified rotation to the input matrix M1. If
      !>
      !>                 [ANGLE]
      !>                        IAXIS
      !>
      !>              denotes the rotation matrix by ANGLE radians about IAXIS,
      !>              (refer to the routine ROTATE) then MOUT is given by the
      !>              following matrix equation:
      !>
      !>                 MOUT = [ANGLE]      * M1
      !>                               IAXIS
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If the axis index is not in the range 1 to 3, it will be
      !>         treated the same as that integer 1, 2, or 3 that is congruent
      !>         to it mod 3.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     None.
      !>
      !>## Examples
      !>
      !>     Suppose that to rotate a set of inertial axes to body fixed
      !>     axes, one must first roll the coordinate axes about the x-axis by
      !>     angle R to get x', y', z'. From this one must pitch about the y'
      !>     axis by angle P to get x'', y'', z''.  And finally yaw the x'',
      !>     y'', z'' about the z'' axis by angle Y to obtain the
      !>     transformation to bodyfixed coordinates. If ID is the identity
      !>     matrix, then the following code fragment generates the
      !>     transformation from inertial to body fixed.
      !>
      !>        CALL ROTMAT ( ID, R,     1,     M1   )
      !>        CALL ROTMAT ( M1, P,     2,     M2   )
      !>        CALL ROTMAT ( M2, Y,     3,     TIBF )
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.M. Owen          (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 27-MAY-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Reformatted
      !>        arguments' description.
      !>
      !>-    SPICELIB Version 1.0.2, 23-APR-2010 (NJB)
      !>
      !>        Header correction: assertions that the output
      !>        can overwrite the input have been removed.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 1.1.0, 3-JAN-1989 (WLT)
      !>
      !>     Upgrade the routine to work with negative axis indexes. Also take
      !>     care of the funky way the indices (other than the input) were
      !>     obtained via the MOD function. It works but isn't as clear
      !>     (or fast) as just reading the axes from data.
      module procedure rotmat
      use pepper_kinds, only: ip, wp
      implicit none

      real(wp)              s
      real(wp)              c

      integer(ip)           temp
      integer(ip)           i1
      integer(ip)           i2
      integer(ip)           i3
      integer(ip)           i

      real(wp)              prodm   (3,3)

      integer(ip), parameter :: indexs  (5) = &
         [ 3, 1, 2, 3, 1 ]

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
      mout = prodm
      return
      end procedure rotmat

      !>## Detailed Input
      !>
      !>     R        is a 3x3 rotation matrix that is to be factored as
      !>              a product of three rotations about a specified
      !>              coordinate axes. The angles of these rotations are
      !>              called "Euler angles."
      !>
      !>     AXIS3,
      !>     AXIS2,
      !>     AXIS1    are the indices of the rotation axes of the
      !>              "factor" rotations, whose product is R. R is
      !>              factored as
      !>
      !>                 R = [ ANGLE3 ]      [ ANGLE2 ]      [ ANGLE1 ]
      !>                               AXIS3           AXIS2           AXIS1
      !>
      !>              The axis numbers must belong to the set {1, 2, 3}.
      !>              The second axis number MUST differ from the first
      !>              and third axis numbers.
      !>
      !>              See the $Particulars section below for details
      !>              concerning this notation.
      !>
      !>## Detailed Output
      !>
      !>     ANGLE3,
      !>     ANGLE2,
      !>     ANGLE1   are the Euler angles corresponding to the matrix
      !>              R and the axes specified by AXIS3, AXIS2, and
      !>              AXIS1. These angles satisfy the equality
      !>
      !>                 R = [ ANGLE3 ]      [ ANGLE2 ]      [ ANGLE1 ]
      !>                               AXIS3           AXIS2           AXIS1
      !>
      !>              See the $Particulars section below for details
      !>              concerning this notation.
      !>
      !>              The range of ANGLE3 and ANGLE1 is (-pi, pi].
      !>
      !>              The range of ANGLE2 depends on the exact set of
      !>              axes used for the factorization. For
      !>              factorizations in which the first and third axes
      !>              are the same,
      !>
      !>                 R = [r]  [s]  [t] ,
      !>                        a    b    a
      !>
      !>              the range of ANGLE2 is [0, pi].
      !>
      !>              For factorizations in which the first and third
      !>              axes are different,
      !>
      !>                 R = [r]  [s]  [t] ,
      !>                        a    b    c
      !>
      !>              the range of ANGLE2 is [-pi/2, pi/2].
      !>
      !>              For rotations such that ANGLE3 and ANGLE1 are not
      !>              uniquely determined, ANGLE3 will always be set to
      !>              zero; ANGLE1 is then uniquely determined.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If any of AXIS3, AXIS2, or AXIS1 do not have values in
      !>
      !>            { 1, 2, 3 }
      !>
      !>         the error SPICE(BADAXISNUMBERS) is signaled.
      !>
      !>     2)  If AXIS2 is equal to AXIS3 or AXIS1, the error
      !>         SPICE(BADAXISNUMBERS) is signaled. An arbitrary rotation
      !>         matrix cannot be expressed using a sequence of Euler angles
      !>         unless the second rotation axis differs from the other two.
      !>
      !>     3)  If the input matrix R is not a rotation matrix, the error
      !>         SPICE(NOTAROTATION) is signaled.
      !>
      !>     4)  If ANGLE3 and ANGLE1 are not uniquely determined, ANGLE3
      !>         is set to zero, and ANGLE1 is determined.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     A word about notation: the symbol
      !>
      !>        [ x ]
      !>             i
      !>
      !>     indicates a coordinate system rotation of x radians about the
      !>     ith coordinate axis. To be specific, the symbol
      !>
      !>        [ x ]
      !>             1
      !>
      !>     indicates a coordinate system rotation of x radians about the
      !>     first, or x-, axis; the corresponding matrix is
      !>
      !>        .-                    -.
      !>        |  1      0       0    |
      !>        |                      |
      !>        |  0    cos(x)  sin(x) |
      !>        |                      |
      !>        |  0   -sin(x)  cos(x) |
      !>        `-                    -'
      !>
      !>     Remember, this is a COORDINATE SYSTEM rotation by x radians; this
      !>     matrix, when applied to a vector, rotates the vector by -x
      !>     radians, not x radians. Applying the matrix to a vector yields
      !>     the vector's representation relative to the rotated coordinate
      !>     system.
      !>
      !>     The analogous rotation about the second, or y-, axis is
      !>     represented by
      !>
      !>        [ x ]
      !>             2
      !>
      !>     which symbolizes the matrix
      !>
      !>        .-                    -.
      !>        | cos(x)   0   -sin(x) |
      !>        |                      |
      !>        |  0       1      0    |
      !>        |                      |
      !>        | sin(x)   0    cos(x) |
      !>        `-                    -'
      !>
      !>     and the analogous rotation about the third, or z-, axis is
      !>     represented by
      !>
      !>        [ x ]
      !>             3
      !>
      !>     which symbolizes the matrix
      !>
      !>        .-                    -.
      !>        |  cos(x)  sin(x)   0  |
      !>        |                      |
      !>        | -sin(x)  cos(x)   0  |
      !>        |                      |
      !>        |  0        0       1  |
      !>        `-                    -'
      !>
      !>     The input matrix is assumed to be the product of three
      !>     rotation matrices, each one of the form
      !>
      !>        .-                    -.
      !>        |  1      0       0    |
      !>        |                      |
      !>        |  0    cos(r)  sin(r) |     (rotation of r radians about the
      !>        |                      |      x-axis),
      !>        |  0   -sin(r)  cos(r) |
      !>        `-                    -'
      !>
      !>        .-                    -.
      !>        | cos(s)   0   -sin(s) |
      !>        |                      |
      !>        |  0       1      0    |     (rotation of s radians about the
      !>        |                      |      y-axis),
      !>        | sin(s)   0    cos(s) |
      !>        `-                    -'
      !>
      !>     or
      !>
      !>        .-                    -.
      !>        |  cos(t)  sin(t)   0  |
      !>        |                      |
      !>        | -sin(t)  cos(t)   0  |     (rotation of t radians about the
      !>        |                      |      z-axis),
      !>        |  0        0       1  |
      !>        `-                    -'
      !>
      !>     where the second rotation axis is not equal to the first or
      !>     third. Any rotation matrix can be factored as a sequence of
      !>     three such rotations, provided that this last criterion is met.
      !>
      !>     This routine is related to the SPICELIB routine EUL2M, which
      !>     produces a rotation matrix, given a sequence of Euler angles.
      !>     This routine is a `right inverse' of EUL2M:  the sequence of
      !>     calls
      !>
      !>        CALL M2EUL ( R,  AXIS3,   AXIS2,   AXIS1,
      !>       .                 ANGLE3,  ANGLE2,  ANGLE1     )
      !>
      !>        CALL EUL2M (     ANGLE3,  ANGLE2,  ANGLE1,
      !>       .                 AXIS3,   AXIS2,   AXIS1,   R )
      !>
      !>     preserves R, except for round-off error.
      !>
      !>     On the other hand, the sequence of calls
      !>
      !>        CALL EUL2M (     ANGLE3,  ANGLE2,  ANGLE1,
      !>       .                 AXIS3,   AXIS2,   AXIS1,   R )
      !>
      !>        CALL M2EUL ( R,  AXIS3,   AXIS2,   AXIS1,
      !>       .                 ANGLE3,  ANGLE2,  ANGLE1     )
      !>
      !>     preserve ANGLE3, ANGLE2, and ANGLE1 only if these angles start
      !>     out in the ranges that M2EUL's outputs are restricted to.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for these examples may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Conversion of instrument pointing from a matrix representation
      !>        to Euler angles:
      !>
      !>        Suppose we want to find camera pointing in alpha, delta, and
      !>        kappa, given the inertial-to-camera coordinate transformation
      !>
      !>        .-                                                         -.
      !>        |  0.491273796781358  0.508726203218642  0.706999085398824  |
      !>        |                                                           |
      !>        | -0.508726203218642 -0.491273796781358  0.706999085398824  |
      !>        |                                                           |
      !>        |  0.706999085398824 -0.706999085398824  0.017452406437284  |
      !>        `-                                                         -'
      !>
      !>        Find angles alpha, delta, kappa such that
      !>
      !>           TICAM  =  [ kappa ]  [ pi/2 - delta ]  [ pi/2 + alpha ] .
      !>                              3                 1                 3
      !>
      !>        Example code begins here.
      !>
      !>              PROGRAM M2EUL_EX1
      !>              IMPLICIT NONE
      !>
      !>        C
      !>        C     SPICELIB functions
      !>        C
      !>              DOUBLE PRECISION      DPR
      !>              DOUBLE PRECISION      HALFPI
      !>              DOUBLE PRECISION      TWOPI
      !>
      !>        C
      !>        C     Local variables
      !>        C
      !>              DOUBLE PRECISION      ALPHA
      !>              DOUBLE PRECISION      ANG1
      !>              DOUBLE PRECISION      ANG2
      !>              DOUBLE PRECISION      DELTA
      !>              DOUBLE PRECISION      KAPPA
      !>              DOUBLE PRECISION      TICAM  ( 3, 3 )
      !>
      !>              DATA TICAM /  0.491273796781358D0,
      !>             .             -0.508726203218642D0,
      !>             .              0.706999085398824D0,
      !>             .              0.508726203218642D0,
      !>             .             -0.491273796781358D0,
      !>             .             -0.706999085398824D0,
      !>             .              0.706999085398824D0,
      !>             .              0.706999085398824D0,
      !>             .              0.017452406437284D0  /
      !>
      !>              CALL M2EUL ( TICAM, 3, 1, 3, KAPPA, ANG2, ANG1 )
      !>
      !>              DELTA = HALFPI() - ANG2
      !>              ALPHA = ANG1     - HALFPI()
      !>
      !>              IF ( KAPPA .LT. 0.D0 ) THEN
      !>                 KAPPA = KAPPA + TWOPI()
      !>              END IF
      !>
      !>              IF ( ALPHA .LT. 0.D0 ) THEN
      !>                 ALPHA = ALPHA + TWOPI()
      !>              END IF
      !>
      !>              WRITE (*,'(A,F24.14)') 'Alpha (deg): ', DPR() * ALPHA
      !>              WRITE (*,'(A,F24.14)') 'Delta (deg): ', DPR() * DELTA
      !>              WRITE (*,'(A,F24.14)') 'Kappa (deg): ', DPR() * KAPPA
      !>
      !>              END
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Alpha (deg):       315.00000000000000
      !>        Delta (deg):         1.00000000000003
      !>        Kappa (deg):        45.00000000000000
      !>
      !>     2) Conversion of instrument pointing angles from a non-J2000,
      !>        not necessarily inertial frame to J2000-relative RA, Dec,
      !>        and Twist.
      !>
      !>        Suppose that we have orientation for the CASSINI Narrow Angle
      !>        Camera (NAC) frame expressed as
      !>
      !>           [ gamma ]  [ beta ]  [ alpha ]
      !>                    1         2          3
      !>
      !>        with respect to the CASSINI spacecraft frame.
      !>
      !>        We want to express that orientation with respect to the J2000
      !>        frame as the sequence of rotations
      !>
      !>           [ twist ]  [ dec ]  [ ra ] .
      !>                    3        1       3
      !>
      !>        Use the meta-kernel shown below to load the required SPICE
      !>        kernels.
      !>
      !>           KPL/MK
      !>
      !>           File name: m2eul_ex2.tm
      !>
      !>           This meta-kernel is intended to support operation of SPICE
      !>           example programs. The kernels shown here should not be
      !>           assumed to contain adequate or correct versions of data
      !>           required by SPICE-based user applications.
      !>
      !>           In order for an application to use this meta-kernel, the
      !>           kernels referenced here must be present in the user's
      !>           current working directory.
      !>
      !>           The names and contents of the kernels referenced
      !>           by this meta-kernel are as follows:
      !>
      !>              File name                      Contents
      !>              ---------                      --------
      !>              naif0010.tls                   Leapseconds
      !>              cas00145.tsc                   Cassini SCLK
      !>              cas_v40.tf                     Cassini frames
      !>              08052_08057ra.bc               Orientation for Cassini
      !>
      !>           \begindata
      !>
      !>              KERNELS_TO_LOAD = ( 'naif0010.tls'
      !>                                  'cas00145.tsc'
      !>                                  'cas_v40.tf'
      !>                                  '08052_08057ra.bc')
      !>
      !>           \begintext
      !>
      !>           End of meta-kernel
      !>
      !>        Example code begins here.
      !>
      !>              PROGRAM M2EUL_EX2
      !>              IMPLICIT NONE
      !>
      !>        C
      !>        C     SPICELIB functions
      !>        C
      !>              DOUBLE PRECISION      DPR
      !>              DOUBLE PRECISION      HALFPI
      !>              DOUBLE PRECISION      RPD
      !>              DOUBLE PRECISION      TWOPI
      !>
      !>        C
      !>        C     Local parameters
      !>        C
      !>              CHARACTER*(*)         META
      !>              PARAMETER           ( META   =  'm2eul_ex2.tm' )
      !>
      !>              DOUBLE PRECISION      ALPHA
      !>              PARAMETER           ( ALPHA =  89.9148D0   )
      !>
      !>              DOUBLE PRECISION      BETA
      !>              PARAMETER           ( BETA  = -0.03300D0   )
      !>
      !>              DOUBLE PRECISION      GAMMA
      !>              PARAMETER           ( GAMMA = -90.009796D0 )
      !>
      !>        C
      !>        C     Local variables
      !>        C
      !>              DOUBLE PRECISION      RA
      !>              DOUBLE PRECISION      ANG1
      !>              DOUBLE PRECISION      ANG2
      !>              DOUBLE PRECISION      DEC
      !>              DOUBLE PRECISION      ET
      !>              DOUBLE PRECISION      INST2J ( 3, 3 )
      !>              DOUBLE PRECISION      INST2S ( 3, 3 )
      !>              DOUBLE PRECISION      S2J    ( 3, 3 )
      !>              DOUBLE PRECISION      TWIST
      !>
      !>        C
      !>        C     Load the kernels.
      !>        C
      !>              CALL FURNSH ( META )
      !>
      !>        C
      !>        C     First, we use subroutine EUL2M to form the
      !>        C     transformation from instrument coordinates to
      !>        C     CASSINI spacecraft frame.
      !>        C
      !>              CALL EUL2M ( GAMMA * RPD(), BETA * RPD(), ALPHA * RPD(),
      !>             .             1,             2,            3,    INST2S )
      !>
      !>        C
      !>        C     Now we compute the transformation from CASSINI
      !>        C     spacecraft frame to J2000, at a given time.
      !>        C
      !>              CALL STR2ET ( '2008 Feb 25', ET )
      !>              CALL PXFORM ( 'CASSINI_SC_COORD', 'J2000', ET, S2J )
      !>
      !>        C
      !>        C     Next, we form the transformation from instrument
      !>        C     coordinates to J2000 frame.
      !>        C
      !>              CALL MXM ( S2J, INST2S, INST2J )
      !>
      !>        C
      !>        C     Finally, we express INST2J using the desired Euler
      !>        C     angles.
      !>        C
      !>              CALL M2EUL ( INST2J, 3, 1, 3, TWIST, ANG1, ANG2 )
      !>
      !>              RA   =  ANG2 - HALFPI()
      !>              DEC  =  HALFPI() - ANG1
      !>
      !>        C
      !>        C     If we wish to make sure that RA, DEC, and TWIST are in
      !>        C     the ranges [0, 2pi), [-pi/2, pi/2], and [0, 2pi)
      !>        C     respectively, we may add the code
      !>        C
      !>              IF ( RA .LT. 0.D0 ) THEN
      !>                 RA = RA + TWOPI()
      !>              END IF
      !>
      !>              IF ( TWIST .LT. 0.D0 ) THEN
      !>                 TWIST = TWIST + TWOPI()
      !>              END IF
      !>
      !>        C
      !>        C     Now RA, DEC, and TWIST express the instrument pointing
      !>        C     as RA, Dec, and Twist, relative to the J2000 system.
      !>        C
      !>              WRITE (*,'(A,F24.14)') 'RA    (deg): ', DPR() * RA
      !>              WRITE (*,'(A,F24.14)') 'Dec   (deg): ', DPR() * DEC
      !>              WRITE (*,'(A,F24.14)') 'Twist (deg): ', DPR() * TWIST
      !>
      !>              END
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        RA    (deg):        83.77802387778848
      !>        Dec   (deg):       -14.92925498590898
      !>        Twist (deg):       294.55732942050986
      !>
      !>        Note:  more than one definition of RA, Dec, and Twist is
      !>        extant. Before using this example in an application, check
      !>        that the definition given here is consistent with that used
      !>        in your application.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.3.0, 06-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary entries from $Revisions section.
      !>
      !>        Added complete code examples from existing fragments.
      !>
      !>        Removed erroneous comments about behavior of ATAN2.
      !>
      !>-    SPICELIB Version 1.2.1, 21-DEC-2006 (NJB)
      !>
      !>        Error corrected in header example: input matrix
      !>        previously did not match shown outputs. Offending
      !>        example now includes complete program.
      !>
      !>-    SPICELIB Version 1.2.0, 15-OCT-2005 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in MXM and MTXM calls. A short error message cited in
      !>        the $Exceptions section of the header failed to match
      !>        the actual short message used; this has been corrected.
      !>
      !>-    SPICELIB Version 1.1.2, 13-OCT-2004 (NJB)
      !>
      !>        Fixed header typo.
      !>
      !>-    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.1.0, 02-NOV-1990 (NJB)
      !>
      !>        Header upgraded to describe notation in more detail. Argument
      !>        names were changed to describe the use of the arguments more
      !>        accurately. No change in functionality was made; the operation
      !>        of the routine is identical to that of the previous version.
      !>
      !>-    SPICELIB Version 1.0.0, 03-SEP-1990 (NJB)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 1.1.0, 02-NOV-1990 (NJB)
      !>
      !>        Argument names were changed to describe the use of the
      !>        arguments more accurately. The axis and angle numbers
      !>        now decrease, rather than increase, from left to right.
      !>        The current names reflect the order of operator application
      !>        when the Euler angle rotations are applied to a vector: the
      !>        rightmost matrix
      !>
      !>           [ ANGLE1 ]
      !>                     AXIS1
      !>
      !>        is applied to the vector first, followed by
      !>
      !>           [ ANGLE2 ]
      !>                     AXIS2
      !>
      !>        and then
      !>
      !>           [ ANGLE3 ]
      !>                     AXIS3
      !>
      !>        Previously, the names reflected the order in which the Euler
      !>        angle matrices appear on the page, from left to right. This
      !>        naming convention was found to be a bit obtuse by a various
      !>        users.
      !>
      !>        No change in functionality was made; the operation of the
      !>        routine is identical to that of the previous version.
      !>
      !>        Also, the header was upgraded to describe the notation in more
      !>        detail. The symbol
      !>
      !>           [ x ]
      !>                i
      !>
      !>        is explained at mind-numbing length. An example was added
      !>        that shows a specific set of inputs and the resulting output
      !>        matrix.
      !>
      !>        The angle sequence notation was changed to be consistent with
      !>        Rotations required reading.
      !>
      !>          1-2-3  and  a-b-c
      !>
      !>        have been changed to
      !>
      !>          3-2-1  and  c-b-a.
      !>
      !>       Also, one `)' was changed to a `}'.
      !>
      !>       The phrase `first and third' was changed to `first or third'
      !>       in the $Particulars section, where the criterion for the
      !>       existence of an Euler angle factorization is stated.
      module procedure m2eul
      use pepper_kinds, only: ip, wp
      use pepper_math, only: mtxm, mxm, vhat
      implicit none

!
!     NTOL and DETOL are used to determine whether R is a rotation
!     matrix.
!
!     NTOL is the tolerance for the norms of the columns of R.
!
!     DTOL is the tolerance for the determinant of a matrix whose
!     columns are the unitized columns of R.
!
      real(wp), parameter :: ntol = 0.1_wp

      real(wp), parameter :: dtol = 0.1_wp

      real(wp)              change ( 3, 3 )
      real(wp)              sign
      real(wp)              tmpmat ( 3, 3 )
      real(wp)              tmprot ( 3, 3 )

      integer(ip)           c
      integer(ip)           i
      integer(ip), parameter :: next   ( 3 ) = &
         [ 2, 3, 1 ]

      logical               degen

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     The first order of business is to screen out the goofy cases.
!
!     Make sure the axis numbers are all right:  They must belong to
!     the set {1, 2, 3}...
!
      if (      (  ( axis3 < 1 ) .or. ( axis3 > 3 )  ) &
           .or. (  ( axis2 < 1 ) .or. ( axis2 > 3 )  ) &
           .or. (  ( axis1 < 1 ) .or. ( axis1 > 3 )  )   ) then

         call ctx%setmsg ( 'Axis numbers are #,  #,  #. ' )
         call ctx%errint ( '#',  axis3                    )
         call ctx%errint ( '#',  axis2                    )
         call ctx%errint ( '#',  axis1                    )
         call ctx%sigerr ( 'SPICE(BADAXISNUMBERS)'        )
         return

!
!     ...and the second axis number must differ from its neighbors.
!
      else if ( ( axis3 == axis2 ) .or. ( axis1 == axis2 ) ) then

         call ctx%setmsg ( 'Middle axis matches neighbor: # # #.' )
         call ctx%errint ( '#',  axis3                            )
         call ctx%errint ( '#',  axis2                            )
         call ctx%errint ( '#',  axis1                            )
         call ctx%sigerr ( 'SPICE(BADAXISNUMBERS)'                )
         return

!
!     R must be a rotation matrix, or we may as well forget it.
!
      else if (  .not. isrot ( ctx, r, ntol, dtol )  ) then

         call ctx%setmsg ( 'Input matrix is not a rotation.' )
         call ctx%sigerr ( 'SPICE(NOTAROTATION)'             )
         return

      end if

!
!     AXIS3, AXIS2, AXIS1 and R have passed their tests at this
!     point.  We take the liberty of working with TMPROT, a version
!     of R that has unitized columns.
!
      do i = 1, 3
         call vhat ( r(1,i), tmprot(1,i) )
      end do

!
!     We now proceed to recover the promised Euler angles from
!     TMPROT.
!
!        The ideas behind our method are explained in excruciating
!        detail in the ROTATION required reading, so we'll be terse.
!        Nonetheless, a word of explanation is in order.
!
!        The sequence of rotation axes used for the factorization
!        belongs to one of two categories:  a-b-a or c-b-a.  We
!        wish to handle each of these cases in one shot, rather than
!        using different formulas for each sequence to recover the
!        Euler angles.
!
!        What we're going to do is use the Euler angle formula for the
!        3-1-3 factorization for all of the a-b-a sequences, and the
!        formula for the 3-2-1 factorization for all of the c-b-a
!        sequences.
!
!        How can we get away with this?  The Euler angle formulas for
!        each factorization are different!
!
!        Our trick is to apply a change-of-basis transformation to the
!        input matrix R.  For the a-b-a factorizations, we choose a new
!        basis such that a rotation of ANGLE3 radians about the basis
!        vector indexed by AXIS3 becomes a rotation of ANGLE3 radians
!        about the third coordinate axis, and such that a rotation of
!        ANGLE2 radians about the basis vector indexed by AXIS2 becomes
!        a rotation of ANGLE2 radians about the first coordinate axis.
!        So R can be factored as a 3-1-3 rotation relative to the new
!        basis, and the Euler angles we obtain are the exact ones we
!        require.
!
!        The c-b-a factorizations can be handled in an analogous
!        fashion.  We transform R to a basis where the original axis
!        sequence becomes a 3-2-1 sequence.  In some cases, the angles
!        we obtain will be the negatives of the angles we require.  This
!        will happen if and only if the ordered basis (here the e's are
!        the standard basis vectors)
!
!            { e        e        e      }
!               AXIS3    AXIS2    AXIS1
!
!        is not right-handed.  An easy test for this condition is that
!        AXIS2 is not the successor of AXIS3, where the ordering is
!        cyclic.
!

      if ( axis3 == axis1 ) then

!
!        The axis order is a-b-a.  We're going to find a matrix CHANGE
!        such that
!
!                 T
!           CHANGE  R  CHANGE
!
!        gives us R in the a useful basis, that is, a basis in which
!        our original a-b-a rotation is a 3-1-3 rotation, but where the
!        rotation angles are unchanged. To achieve this pleasant
!        simplification, we set column 3 of CHANGE to to e(AXIS3),
!        column 1 of CHANGE to e(AXIS2), and column 2 of CHANGE to
!
!          (+/-) e(C),
!
!        (C is the remaining index) depending on whether
!        AXIS3-AXIS2-C is a right-handed sequence of axes:  if it
!        is, the sign is positive.  (Here e(1), e(2), e(3) are the
!        standard basis vectors.)
!
!        Determine the sign of our third basis vector, so that we can
!        ensure that our new basis is right-handed.  The variable NEXT
!        is just a little mapping that takes 1 to 2, 2 to 3, and 3 to
!        1.
!
         if ( axis2 == next(axis3) ) then
            sign =  1.0_wp
         else
            sign = -1.0_wp
         end if

!
!        Since the axis indices sum to 6,
!
         c = 6 - axis3 - axis2

!
!        Set up the entries of CHANGE:
!
         change = 0.0_wp

         change ( axis3, 3 ) =         1.0_wp
         change ( axis2, 1 ) =         1.0_wp
         change ( c,     2 ) =  sign * 1.0_wp

!
!        Transform TMPROT.
!
         call mxm  ( tmprot,  change,  tmpmat )
         call mtxm ( change,  tmpmat,  tmprot )

!
!        Now we're ready to find the Euler angles, using a
!        3-1-3 factorization.  In general, the matrix product
!
!           [ a1 ]   [ a2 ]   [ a3 ]
!                 3        1        3
!
!        has the form
!
!     +-                                                              -+
!     |         cos(a1)cos(a3)          cos(a1)sin(a3)  sin(a1)sin(a2) |
!     | -sin(a1)cos(a2)sin(a3)  +sin(a1)cos(a2)cos(a3)                 |
!     |                                                                |
!     |        -sin(a1)cos(a3)         -sin(a1)sin(a3)  cos(a1)sin(a2) |
!     | -cos(a1)cos(a2)sin(a3)  +cos(a1)cos(a2)cos(a3)                 |
!     |                                                                |
!     |         sin(a2)sin(a3)         -sin(a2)cos(a3)         cos(a2) |
!     +-                                                              -+
!
!        but if a2 is 0 or pi, the product matrix reduces to
!
!     +-                                                              -+
!     |         cos(a1)cos(a3)          cos(a1)sin(a3)               0 |
!     | -sin(a1)cos(a2)sin(a3)  +sin(a1)cos(a2)cos(a3)                 |
!     |                                                                |
!     |        -sin(a1)cos(a3)         -sin(a1)sin(a3)               0 |
!     | -cos(a1)cos(a2)sin(a3)  +cos(a1)cos(a2)cos(a3)                 |
!     |                                                                |
!     |                      0                       0         cos(a2) |
!     +-                                                              -+
!
!        In this case, a1 and a3 are not uniquely determined.  If we
!        arbitrarily set a1 to zero, we arrive at the matrix
!
!           +-                                         -+
!           |         cos(a3)         sin(a3)      0    |
!           | -cos(a2)sin(a3)  cos(a2)cos(a3)      0    |
!           |               0            0      cos(a2) |
!           +-                                         -+
!
!        We take care of this case first.  We test three conditions
!        that are mathematically equivalent, but may not be satisfied
!        simultaneously because of round-off:
!
         degen =      (        (       tmprot(1,3)    == 0.0_wp  ) &
                         .and. (       tmprot(2,3)    == 0.0_wp  )  ) &
                 .or. (        (       tmprot(3,1)    == 0.0_wp  ) &
                         .and. (       tmprot(3,2)    == 0.0_wp  )  ) &
                 .or. (           abs( tmprot(3,3) )  == 1.0_wp     )

!
!        In the following block of code, we make use of the fact that
!
!           SIN ( ANGLE2 )   >  0
!                            -
!        in choosing the signs of the ATAN2 arguments correctly.
!
         if ( degen ) then

            angle3 = 0.0_wp
            angle2 = acos  (  tmprot(3,3)                 )
            angle1 = atan2 (  tmprot(1,2),   tmprot(1,1)  )

         else
!
!           The normal case.
!
            angle3 = atan2 (  tmprot(1,3),   tmprot(2,3)  )
            angle2 = acos  (  tmprot(3,3)                 )
            angle1 = atan2 (  tmprot(3,1),  -tmprot(3,2)  )

         end if

      else

!
!        The axis order is c-b-a.  We're going to find a matrix CHANGE
!        such that
!
!                 T
!           CHANGE  R  CHANGE
!
!        gives us R in the a useful basis, that is, a basis in which
!        our original c-b-a rotation is a 3-2-1 rotation, but where the
!        rotation angles are unchanged, or at worst negated.  To
!        achieve this pleasant simplification, we set column 1 of
!        CHANGE to to e(AXIS3), column 2 of CHANGE to e(AXIS2), and
!        column 3 of CHANGE to
!
!          (+/-) e(AXIS1),
!
!        depending on whether AXIS3-AXIS2-AXIS1 is a right-handed
!        sequence of axes:  if it is, the sign is positive.  (Here
!        e(1), e(2), e(3) are the standard basis vectors.)
!
!        We must be cautious here, because if AXIS3-AXIS2-AXIS1 is a
!        right-handed sequence of axes, all of the rotation angles will
!        be the same in our new basis, but if it's a left-handed
!        sequence, the third angle will be negated.  Let's get this
!        straightened out right now.  The variable NEXT is just a
!        little mapping that takes 1 to 2, 2 to 3, and 3 to 1.
!
         if ( axis2 == next(axis3) ) then
            sign =  1.0_wp
         else
            sign = -1.0_wp
         end if

!
!        Set up the entries of CHANGE:
!
         change = 0.0_wp

         change ( axis3, 1 ) =         1.0_wp
         change ( axis2, 2 ) =         1.0_wp
         change ( axis1, 3 ) =  sign * 1.0_wp

!
!        Transform TMPROT.
!
         call mxm  ( tmprot,  change,  tmpmat )
         call mtxm ( change,  tmpmat,  tmprot )

!
!        Now we're ready to find the Euler angles, using a
!        3-2-1 factorization.  In general, the matrix product
!
!           [ a1 ]   [ a2 ]   [ a3 ]
!                 1        2        3
!
!        has the form
!
!     +-                                                              -+
!     |         cos(a2)cos(a3)          cos(a2)sin(a3)        -sin(a2) |
!     |                                                                |
!     |        -cos(a1)sin(a3)          cos(a1)cos(a3)  sin(a1)cos(a2) |
!     | +sin(a1)sin(a2)cos(a3)  +sin(a1)sin(a2)sin(a3)                 |
!     |                                                                |
!     |         sin(a1)sin(a3)         -sin(a1)cos(a3)  cos(a1)cos(a2) |
!     | +cos(a1)sin(a2)cos(a3)  +cos(a1)sin(a2)sin(a3)                 |
!     +-                                                              -+
!
!        but if a2 is -pi/2 or pi/2, the product matrix reduces to
!
!     +-                                                              -+
!     |                      0                       0        -sin(a2) |
!     |                                                                |
!     |        -cos(a1)sin(a3)          cos(a1)cos(a3)               0 |
!     | +sin(a1)sin(a2)cos(a3)  +sin(a1)sin(a2)sin(a3)                 |
!     |                                                                |
!     |         sin(a1)sin(a3)         -sin(a1)cos(a3)               0 |
!     | +cos(a1)sin(a2)cos(a3)  +cos(a1)sin(a2)sin(a3)                 |
!     +-                                                              -+
!
!        In this case, a1 and a3 are not uniquely determined.  If we
!        arbitrarily set a1 to zero, we arrive at the matrix
!
!           +-                                              -+
!           |               0                 0    -sin(a2)  |
!           |        -sin(a3)           cos(a3)          0   |,
!           |  sin(a2)cos(a3)    sin(a2)sin(a3)          0   |
!           +-                                              -+
!
!        We take care of this case first.  We test three conditions
!        that are mathematically equivalent, but may not be satisfied
!        simultaneously because of round-off:
!
         degen =      (        (       tmprot(1,1)    == 0.0_wp  ) &
                         .and. (       tmprot(1,2)    == 0.0_wp  )  ) &
                 .or. (        (       tmprot(2,3)    == 0.0_wp  ) &
                         .and. (       tmprot(3,3)    == 0.0_wp  )  ) &
                 .or. (           abs( tmprot(1,3) )  == 1.0_wp     )

!
!        In the following block of code, we make use of the fact that
!
!           COS ( ANGLE2 )   >  0
!                            -
!        in choosing the signs of the ATAN2 arguments correctly.
!
         if ( degen ) then

            angle3 =          0.0_wp
            angle2 =          asin  ( -tmprot(1,3)                 )
            angle1 = sign  *  atan2 ( -tmprot(2,1),   tmprot(2,2)  )

         else
!
!           The normal case.
!
            angle3 =          atan2 (  tmprot(2,3),   tmprot(3,3)  )
            angle2 =          asin  ( -tmprot(1,3)                 )
            angle1 = sign  *  atan2 (  tmprot(1,2),   tmprot(1,1)  )

         end if

      end if

      return
      end procedure m2eul

      !>## Detailed Input
      !>
      !>     ANGLE3,
      !>     ANGLE2,
      !>     ANGLE1,
      !>     AXIS3,
      !>     AXIS2,
      !>     AXIS1    are, respectively, a set of three angles and three
      !>              coordinate axis numbers; each pair ANGLEx and AXISx
      !>              specifies a coordinate transformation consisting of a
      !>              rotation by ANGLEx radians about the coordinate axis
      !>              indexed by AXISx. These coordinate transformations are
      !>              typically symbolized by
      !>
      !>                 [ ANGLEx ]     .
      !>                           AXISx
      !>
      !>              See the $Particulars section below for details concerning
      !>              this notation.
      !>
      !>              Note that these coordinate transformations rotate vectors
      !>              by
      !>
      !>                 -ANGLEx
      !>
      !>              radians about the axis indexed by AXISx.
      !>
      !>              The values of AXISx may be 1, 2, or 3, indicating the X,
      !>              Y, and Z axes respectively.
      !>
      !>## Detailed Output
      !>
      !>     R        is a rotation matrix representing the composition of the
      !>              rotations defined by the input angle-axis pairs.
      !>              Together, the three pairs specify a composite
      !>              transformation that is the result of performing the
      !>              rotations about the axes indexed by AXIS1, AXIS2, and
      !>              AXIS3, in that order. So,
      !>
      !>                 R = [ ANGLE3 ]      [ ANGLE2 ]      [ ANGLE1 ]
      !>                               AXIS3           AXIS2           AXIS1
      !>
      !>              See the $Particulars section below for details concerning
      !>              this notation.
      !>
      !>              The resulting matrix R may be thought of as a coordinate
      !>              transformation; applying it to a vector yields the
      !>              vector's coordinates in the rotated system.
      !>
      !>              Viewing R as a coordinate transformation matrix, the
      !>              basis that R transforms vectors to is created by rotating
      !>              the original coordinate axes first by ANGLE1 radians
      !>              about the coordinate axis indexed by AXIS1, next by
      !>              ANGLE2 radians about the coordinate axis indexed by
      !>              AXIS2, and finally by ANGLE3 radians about coordinate
      !>              axis indexed by AXIS3. At the second and third steps of
      !>              this process, the coordinate axes about which rotations
      !>              are performed belong to the bases resulting from the
      !>              previous rotations.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If any of AXIS3, AXIS2, or AXIS1 do not have values in
      !>
      !>            { 1, 2, 3 }
      !>
      !>         the error SPICE(BADAXISNUMBERS) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     A word about notation: the symbol
      !>
      !>        [ x ]
      !>             i
      !>
      !>     indicates a rotation of x radians about the ith coordinate axis.
      !>     To be specific, the symbol
      !>
      !>        [ x ]
      !>             1
      !>
      !>     indicates a coordinate system rotation of x radians about the
      !>     first, or x-, axis; the corresponding matrix is
      !>
      !>        .-                    -.
      !>        |  1      0       0    |
      !>        |                      |
      !>        |  0    cos(x)  sin(x) |
      !>        |                      |
      !>        |  0   -sin(x)  cos(x) |
      !>        `-                    -'
      !>
      !>     Remember, this is a COORDINATE SYSTEM rotation by x radians; this
      !>     matrix, when applied to a vector, rotates the vector by -x
      !>     radians, not x radians. Applying the matrix to a vector yields
      !>     the vector's representation relative to the rotated coordinate
      !>     system.
      !>
      !>     The analogous rotation about the second, or y-, axis is
      !>     represented by
      !>
      !>        [ x ]
      !>             2
      !>
      !>     which symbolizes the matrix
      !>
      !>        .-                    -.
      !>        | cos(x)   0   -sin(x) |
      !>        |                      |
      !>        |  0       1      0    |
      !>        |                      |
      !>        | sin(x)   0    cos(x) |
      !>        `-                    -'
      !>
      !>     and the analogous rotation about the third, or z-, axis is
      !>     represented by
      !>
      !>        [ x ]
      !>             3
      !>
      !>     which symbolizes the matrix
      !>
      !>        .-                    -.
      !>        |  cos(x)  sin(x)   0  |
      !>        |                      |
      !>        | -sin(x)  cos(x)   0  |
      !>        |                      |
      !>        |  0        0       1  |
      !>        `-                    -'
      !>
      !>     From time to time, (depending on one's line of work, perhaps) one
      !>     may happen upon a pair of coordinate systems related by a
      !>     sequence of rotations. For example, the coordinate system
      !>     defined by an instrument such as a camera is sometime specified
      !>     by RA, DEC, and twist; if alpha, delta, and phi are the rotation
      !>     angles, then the series of rotations
      !>
      !>        [ phi ]     [ pi/2  -  delta ]     [ alpha ]
      !>               3                      2             3
      !>
      !>     produces a transformation from inertial to camera coordinates.
      !>
      !>     This routine is related to the SPICELIB routine M2EUL, which
      !>     produces a sequence of Euler angles, given a rotation matrix.
      !>     This routine is a "left inverse" of M2EUL: the sequence of
      !>     calls
      !>
      !>        CALL M2EUL ( R,  AXIS3,   AXIS2,   AXIS1,
      !>       .                 ANGLE3,  ANGLE2,  ANGLE1     )
      !>
      !>        CALL EUL2M (     ANGLE3,  ANGLE2,  ANGLE1,
      !>       .                 AXIS3,   AXIS2,   AXIS1,   R )
      !>
      !>     preserves R, except for round-off error.
      !>
      !>     On the other hand, the sequence of calls
      !>
      !>        CALL EUL2M (     ANGLE3,  ANGLE2,  ANGLE1,
      !>       .                 AXIS3,   AXIS2,   AXIS1,   R )
      !>
      !>        CALL M2EUL ( R,  AXIS3,   AXIS2,   AXIS1,
      !>       .                 ANGLE3,  ANGLE2,  ANGLE1     )
      !>
      !>     preserve ANGLE3, ANGLE2, and ANGLE1 only if these angles start
      !>     out in the ranges that M2EUL's outputs are restricted to.
      !>
      !>## Examples
      !>
      !>     1)  Create a coordinate transformation matrix by rotating
      !>         the original coordinate axes first by 30 degrees about
      !>         the z axis, next by 60 degrees about the y axis resulting
      !>         from the first rotation, and finally by -50 degrees about
      !>         the z axis resulting from the first two rotations.
      !>
      !>            C
      !>            C     Create the coordinate transformation matrix
      !>            C
      !>            C                   o          o          o
      !>            C        R  =  [ -50  ]   [  60  ]   [  30  ]
      !>            C                      3          2          3
      !>            C
      !>            C     All angles in radians, please. The SPICELIB
      !>            C     function RPD (radians per degree) gives the
      !>            C     conversion factor.
      !>            C
      !>            C     The z axis is `axis 3'; the y axis is `axis 2'.
      !>            C
      !>                  ANGLE1 = RPD() *  30.D0
      !>                  ANGLE2 = RPD() *  60.D0
      !>                  ANGLE3 = RPD() * -50.D0
      !>
      !>                  AXIS1  = 3
      !>                  AXIS2  = 2
      !>                  AXIS3  = 3
      !>
      !>                  CALL EUL2M (  ANGLE3, ANGLE2, ANGLE1,
      !>                 .              AXIS3,  AXIS2,  AXIS1,   R  )
      !>
      !>     2)  A trivial example using actual numbers.
      !>
      !>         The code fragment
      !>
      !>            CALL EUL2M (  0.D0,     0.D0,     HALFPI(),
      !>           .                 1,        1,            3,      R  )
      !>
      !>         sets R equal to the matrix
      !>
      !>            +-                  -+
      !>            |  0      1       0  |
      !>            |                    |
      !>            | -1      0       0  |.
      !>            |                    |
      !>            |  0      0       1  |
      !>            +-                  -+
      !>
      !>     3)  Finding the rotation matrix specified by a set of `clock,
      !>         cone, and twist' angles, as defined on the Voyager 2 project:
      !>
      !>            Voyager 2 narrow angle camera pointing, relative to the
      !>            Sun-Canopus coordinate system, was frequently specified
      !>            by a set of Euler angles called `clock, cone, and twist'.
      !>            These defined a 3-2-3 coordinate transformation matrix
      !>            TSCTV as the product
      !>
      !>               [ twist ]  [ cone ]   [ clock ] .
      !>                        3         2           3
      !>
      !>            Given the angles CLOCK, CONE, and TWIST (in units of
      !>            radians), we can compute TSCTV with the code fragment
      !>
      !>               CALL EUL2M (  TWIST,  CONE,  CLOCK,
      !>              .              3,      2,     3,      TSCTV  )
      !>
      !>     4)  Finding the rotation matrix specified by a set of `right
      !>         ascension, declination, and twist' angles, as defined on the
      !>         Galileo project:
      !>
      !>            Galileo scan platform pointing, relative to an inertial
      !>            reference frame, (EME50 variety) is frequently specified
      !>            by a set of Euler angles called `right ascension (RA),
      !>            declination (Dec), and twist'. These define a 3-2-3
      !>            coordinate transformation matrix TISP as the product
      !>
      !>               [ Twist ]  [ pi/2 - Dec ]   [ RA ] .
      !>                        3               2        3
      !>
      !>            Given the angles RA, DEC, and TWIST (in units of radians),
      !>            we can compute TISP with the code fragment
      !>
      !>               CALL EUL2M (  TWIST,   HALFPI()-DEC,   RA,
      !>              .              3,       2,              3,   TISP  )
      !>
      !>## Restrictions
      !>
      !>     1)  Beware: more than one definition of "RA, DEC and twist"
      !>         exists.
      !>
      !>## Literature References
      !>
      !>     [1]  W. Owen, "Galileo Attitude and Camera Models," JPL
      !>          Interoffice Memorandum 314-323, Nov. 11, 1983. NAIF document
      !>          number 204.0.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     L.S. Elson         (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.3.0, 06-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary entries from $Revisions section.
      !>
      !>-    SPICELIB Version 1.2.1, 26-DEC-2006 (NJB)
      !>
      !>        Corrected header typo.
      !>
      !>-    SPICELIB Version 1.2.0, 25-AUG-2005 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in ROTMAT calls.
      !>
      !>-    SPICELIB Version 1.1.2, 14-OCT-2004 (LSE)
      !>
      !>        Corrected a typo in the header.
      !>
      !>-    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.1.0, 02-NOV-1990 (NJB)
      !>
      !>        Names of input arguments changed to reflect the order in
      !>        which the rotations are applied when their product is
      !>        computed. The header was upgraded to describe notation in
      !>        more detail. Examples were added.
      !>
      !>-    SPICELIB Version 1.0.0, 30-AUG-1990 (NJB)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 1.1.0, 02-NOV-1990 (NJB)
      !>
      !>        Argument names were changed to describe the use of the
      !>        arguments more accurately. The axis and angle numbers
      !>        now decrease, rather than increase, from left to right.
      !>        The current names reflect the order of operator
      !>        application when the Euler angle rotations are applied to
      !>        a vector: the rightmost matrix
      !>
      !>           [ ANGLE1 ]
      !>                     AXIS1
      !>
      !>        is applied to the vector first, followed by
      !>
      !>           [ ANGLE2 ]
      !>                     AXIS2
      !>
      !>        and then
      !>
      !>           [ ANGLE3 ]
      !>                     AXIS3
      !>
      !>        Previously, the names reflected the order in which the
      !>        Euler angle matrices appear on the page, from left to
      !>        right. This naming convention was found to be a bit
      !>        obtuse by a various users.
      !>
      !>        No change in functionality was made; the operation of the
      !>        routine is identical to that of the previous version.
      !>
      !>        Two new examples were added to assist users in verifying
      !>        their understanding of the routine.
      !>
      !>        Also, the header was upgraded to describe the notation in
      !>        more detail. The symbol
      !>
      !>           [ x ]
      !>                i
      !>
      !>        is explained at mind-numbing length. An example was
      !>        added that shows a specific set of inputs and the
      !>        resulting output matrix.
      module procedure eul2m
      use pepper_kinds, only: ip, wp
      implicit none

      real(wp)              r1    ( 3, 3 )
      logical               badax

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Make sure the axis numbers are all right:  They must belong to
!     the set {1, 2, 3}.
!
      badax =       (  ( axis3 < 1 ) .or. ( axis3 > 3 )  ) &
               .or. (  ( axis2 < 1 ) .or. ( axis2 > 3 )  ) &
               .or. (  ( axis1 < 1 ) .or. ( axis1 > 3 )  )

      if ( badax ) then

         call ctx%setmsg ( 'Axis numbers are #,  #,  #. ' )
         call ctx%errint ( '#',  axis3                    )
         call ctx%errint ( '#',  axis2                    )
         call ctx%errint ( '#',  axis1                    )
         call ctx%sigerr ( 'SPICE(BADAXISNUMBERS)'        )
         return

      end if

!
!     Just do it.
!
      call rotate ( ctx,      angle1,  axis1,  r  )
      call rotmat ( ctx, r,   angle2,  axis2,  r1 )
      call rotmat ( ctx, r1,  angle3,  axis3,  r  )

      return
      end procedure eul2m

      !>## Detailed Input
      !>
      !>     XFORM    is a state transformation matrix from some frame FRAME1
      !>              to another frame FRAME2. Pictorially, XFORM has the
      !>              structure shown here.
      !>
      !>                 .-             -.
      !>                 |       |       |
      !>                 |   R   |   0   |
      !>                 |       |       |
      !>                 |-------+-------|
      !>                 |       |       |
      !>                 | dR/dt |   R   |
      !>                 |       |       |
      !>                 `-             -'
      !>
      !>              where R is a rotation matrix that varies with respect to
      !>              time and dR/dt is its time derivative.
      !>
      !>              More specifically, if S1 is the state of some object
      !>              in FRAME1, then S2, the state of the same object
      !>              relative to FRAME2 is given by
      !>
      !>                 S2 = XFORM * S1
      !>
      !>              where "*" denotes the matrix vector product.
      !>
      !>     AXISA,
      !>     AXISB,
      !>     AXISC    are the axes desired for the factorization of R.
      !>
      !>              All must be in the range from 1 to 3. Moreover
      !>              it must be the case that AXISA and AXISB are distinct
      !>              and that AXISB and AXISC are distinct.
      !>
      !>              Every rotation matrix can be represented as a product
      !>              of three rotation matrices about the principal axes
      !>              of a reference frame.
      !>
      !>                 R =  [ ALPHA ]      [ BETA ]      [ GAMMA ]
      !>                               AXISA         AXISB          AXISC
      !>
      !>              The value 1 corresponds to the X axis.
      !>              The value 2 corresponds to the Y axis.
      !>              The value 3 corresponds to the Z axis.
      !>
      !>## Detailed Output
      !>
      !>     EULANG   is the set of Euler angles corresponding to the
      !>              specified factorization.
      !>
      !>              If we represent R as shown here:
      !>
      !>                 R =  [ ALPHA ]      [ BETA ]      [ GAMMA ]
      !>                               AXISA         AXISB          AXISC
      !>
      !>              then
      !>
      !>                 EULANG(1) = ALPHA
      !>                 EULANG(2) = BETA
      !>                 EULANG(3) = GAMMA
      !>                 EULANG(4) = dALPHA/dt
      !>                 EULANG(5) = dBETA/dt
      !>                 EULANG(6) = dGAMMA/dt
      !>
      !>              The range of ALPHA and GAMMA is (-pi, pi].
      !>
      !>              The range of BETA depends on the exact set of
      !>              axes used for the factorization. For
      !>              factorizations in which the first and third axes
      !>              are the same, the range of BETA is [0, pi].
      !>
      !>              For factorizations in which the first and third
      !>              axes are different, the range of BETA is
      !>              [-pi/2, pi/2].
      !>
      !>              For rotations such that ALPHA and GAMMA are not
      !>              uniquely determined, ALPHA and dALPHA/dt will
      !>              always be set to zero; GAMMA and dGAMMA/dt are
      !>              then uniquely determined.
      !>
      !>     UNIQUE   is a logical that indicates whether or not the
      !>              values in EULANG are uniquely determined. If
      !>              the values are unique then UNIQUE will be set to
      !>              .TRUE. If the values are not unique and some
      !>              components ( EULANG(1) and EULANG(4) ) have been set
      !>              to zero, then UNIQUE will have the value .FALSE.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If any of AXISA, AXISB, or AXISC do not have values in
      !>
      !>            { 1, 2, 3 }
      !>
      !>         an error is signaled by a routine in the call tree of this
      !>         routine.
      !>
      !>     2)  If AXISB is equal to AXISC or AXISA, an error is signaled by a
      !>         routine in the call tree of this routine. An arbitrary
      !>         rotation matrix cannot be expressed using a sequence of Euler
      !>         angles unless the second rotation axis differs from the other
      !>         two.
      !>
      !>     3)  If the input matrix XFORM is not a rotation matrix, an error
      !>         is signaled by a routine in the call tree of this routine.
      !>
      !>     4)  If EULANG(1) and EULANG(3) are not uniquely determined,
      !>         EULANG(1) is set to zero, and EULANG(3) is determined.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     A word about notation: the symbol
      !>
      !>        [ x ]
      !>             i
      !>
      !>     indicates a coordinate system rotation of x radians about the
      !>     ith coordinate axis. To be specific, the symbol
      !>
      !>        [ x ]
      !>             1
      !>
      !>     indicates a coordinate system rotation of x radians about the
      !>     first, or x-, axis; the corresponding matrix is
      !>
      !>        .-                    -.
      !>        |  1      0       0    |
      !>        |                      |
      !>        |  0    cos(x)  sin(x) |
      !>        |                      |
      !>        |  0   -sin(x)  cos(x) |
      !>        `-                    -'
      !>
      !>     Remember, this is a COORDINATE SYSTEM rotation by x radians; this
      !>     matrix, when applied to a vector, rotates the vector by -x
      !>     radians, not x radians. Applying the matrix to a vector yields
      !>     the vector's representation relative to the rotated coordinate
      !>     system.
      !>
      !>     The analogous rotation about the second, or y-, axis is
      !>     represented by
      !>
      !>        [ x ]
      !>             2
      !>
      !>     which symbolizes the matrix
      !>
      !>        .-                    -.
      !>        | cos(x)   0   -sin(x) |
      !>        |                      |
      !>        |  0       1      0    |
      !>        |                      |
      !>        | sin(x)   0    cos(x) |
      !>        `-                    -'
      !>
      !>     and the analogous rotation about the third, or z-, axis is
      !>     represented by
      !>
      !>        [ x ]
      !>             3
      !>
      !>     which symbolizes the matrix
      !>
      !>        .-                    -.
      !>        |  cos(x)  sin(x)   0  |
      !>        |                      |
      !>        | -sin(x)  cos(x)   0  |
      !>        |                      |
      !>        |  0        0       1  |
      !>        `-                    -'
      !>
      !>     The input matrix is assumed to be the product of three
      !>     rotation matrices, each one of the form
      !>
      !>        .-                    -.
      !>        |  1      0       0    |
      !>        |                      |
      !>        |  0    cos(r)  sin(r) |     (rotation of r radians about the
      !>        |                      |      x-axis),
      !>        |  0   -sin(r)  cos(r) |
      !>        `-                    -'
      !>
      !>        .-                    -.
      !>        | cos(s)   0   -sin(s) |
      !>        |                      |
      !>        |  0       1      0    |     (rotation of s radians about the
      !>        |                      |      y-axis),
      !>        | sin(s)   0    cos(s) |
      !>        `-                    -'
      !>
      !>     or
      !>
      !>        .-                    -.
      !>        |  cos(t)  sin(t)   0  |
      !>        |                      |
      !>        | -sin(t)  cos(t)   0  |     (rotation of t radians about the
      !>        |                      |      z-axis),
      !>        |  0        0       1  |
      !>        `-                    -'
      !>
      !>     where the second rotation axis is not equal to the first or
      !>     third. Any rotation matrix can be factored as a sequence of
      !>     three such rotations, provided that this last criterion is met.
      !>
      !>     This routine is related to the routine EUL2XF which produces
      !>     a state transformation from an input set of axes, Euler angles
      !>     and derivatives.
      !>
      !>     The two subroutine calls shown here will not change
      !>     XFORM except for round off errors.
      !>
      !>        CALL XF2EUL ( XFORM,  AXISA, AXISB, AXISC, EULANG, UNIQUE )
      !>        CALL EUL2XF ( EULANG, AXISA, AXISB, AXISC, XFORM          )
      !>
      !>     On the other hand the two calls
      !>
      !>        CALL EUL2XF ( EULANG, AXISA, AXISB, AXISC, XFORM          )
      !>        CALL XF2EUL ( XFORM,  AXISA, AXISB, AXISC, EULANG, UNIQUE )
      !>
      !>     will leave EULANG unchanged only if the components of EULANG
      !>     are in the range produced by XF2EUL and the Euler representation
      !>     of the rotation component of XFORM is unique within that range.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Determine the rate of change of the right ascension and
      !>        declination of the pole of the moon, from the state
      !>        transformation matrix that transforms J2000 states to object
      !>        fixed states.
      !>
      !>        Recall that the rotation component of the state transformation
      !>        matrix is given by
      !>
      !>           [W]  [HALFPI-DEC]  [RA+HALFPI]
      !>              3             1            3
      !>
      !>        Use the meta-kernel shown below to load the required SPICE
      !>        kernels.
      !>
      !>           KPL/MK
      !>
      !>           File name: xf2eul_ex1.tm
      !>
      !>           This meta-kernel is intended to support operation of SPICE
      !>           example programs. The kernels shown here should not be
      !>           assumed to contain adequate or correct versions of data
      !>           required by SPICE-based user applications.
      !>
      !>           In order for an application to use this meta-kernel, the
      !>           kernels referenced here must be present in the user's
      !>           current working directory.
      !>
      !>           The names and contents of the kernels referenced
      !>           by this meta-kernel are as follows:
      !>
      !>              File name                     Contents
      !>              ---------                     --------
      !>              pck00010.tpc                  Planet orientation and
      !>                                            radii
      !>              naif0012.tls                  Leapseconds
      !>
      !>           \begindata
      !>
      !>              KERNELS_TO_LOAD = ( 'pck00010.tpc',
      !>                                  'naif0012.tls'  )
      !>
      !>           \begintext
      !>
      !>           End of meta-kernel
      !>
      !>        Example code begins here.
      !>
      !>              PROGRAM XF2EUL_EX1
      !>              IMPLICIT NONE
      !>
      !>        C
      !>        C     SPICELIB functions.
      !>        C
      !>              DOUBLE PRECISION      HALFPI
      !>
      !>        C
      !>        C     Local parameters.
      !>        C
      !>              CHARACTER*(*)         META
      !>              PARAMETER           ( META   = 'xf2eul_ex1.tm' )
      !>
      !>              CHARACTER*(*)         UTCSTR
      !>              PARAMETER           ( UTCSTR = 'May 15, 2007' )
      !>
      !>        C
      !>        C     Local variables.
      !>        C
      !>              DOUBLE PRECISION      EULANG ( 6    )
      !>              DOUBLE PRECISION      ET
      !>              DOUBLE PRECISION      FTMTRX ( 6, 6 )
      !>
      !>              INTEGER               I
      !>              INTEGER               J
      !>
      !>              LOGICAL               UNIQUE
      !>
      !>        C
      !>        C     Load SPICE kernels.
      !>        C
      !>              CALL FURNSH ( META )
      !>
      !>        C
      !>        C     Convert the input time to seconds past J2000 TDB.
      !>        C
      !>              CALL STR2ET ( UTCSTR, ET )
      !>
      !>        C
      !>        C     Get the transformation matrix from J2000 frame to
      !>        C     IAU_MOON.
      !>        C
      !>              CALL SXFORM ( 'J2000', 'IAU_MOON', ET, FTMTRX )
      !>
      !>        C
      !>        C     Convert the transformation matrix to
      !>        C     Euler angles (3-1-3).
      !>        C
      !>              CALL XF2EUL ( FTMTRX, 3, 1, 3, EULANG, UNIQUE )
      !>
      !>        C
      !>        C     Display the results.
      !>        C
      !>              IF ( UNIQUE ) THEN
      !>
      !>                 WRITE(*,'(2A)') 'UTC: ', UTCSTR
      !>                 WRITE(*,'(A,F20.16)') 'W       = ', EULANG(1)
      !>                 WRITE(*,'(A,F20.16)') 'DEC     = ',
      !>             .                  HALFPI() - EULANG(2)
      !>                 WRITE(*,'(A,F20.16)') 'RA      = ',
      !>             .                  EULANG(3) - HALFPI()
      !>                 WRITE(*,'(A,F20.16)') 'dW/dt   = ', EULANG(4)
      !>                 WRITE(*,'(A,F20.16)') 'dDEC/dt = ', EULANG(5)
      !>                 WRITE(*,'(A,F20.16)') 'dRA/dt  = ', EULANG(6)
      !>
      !>              ELSE
      !>
      !>                 WRITE(*,*) 'The values in EULANG are not uniquely '
      !>             .          //  'determined.'
      !>
      !>              END IF
      !>
      !>              END
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        UTC: May 15, 2007
      !>        W       =  -2.6490877296701645
      !>        DEC     =   1.1869108599473206
      !>        RA      =  -1.5496443908099826
      !>        dW/dt   =   0.0000026578085601
      !>        dDEC/dt =   0.0000000004021737
      !>        dRA/dt  =   0.0000000039334471
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.1.0, 26-OCT-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section.
      !>
      !>        Added complete code example based on existing fragment.
      !>        Corrected input argument name in $Exceptions section.
      !>
      !>-    SPICELIB Version 2.0.1, 25-APR-2007 (EDW)
      !>
      !>        Corrected code in EUL2EF entry point $Examples section, example
      !>        showed a XF2EUL call:
      !>
      !>           CALL XF2EUL ( XFORM,  1, 2, 3, RPYANG )
      !>
      !>        The proper form of the call:
      !>
      !>           CALL XF2EUL ( XFORM,  1, 2, 3, RPYANG, UNIQUE )
      !>
      !>-    SPICELIB Version 2.0.0, 31-OCT-2005 (NJB)
      !>
      !>        Entry point EUL2XF was updated to allow axis sequences
      !>        in which the second angle is not distinct from the first
      !>        or third.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JUL-1995 (WLT)
      module procedure xf2eul
      use pepper_kinds, only: ip, wp
      use pepper_math, only: mxmt, mxv
      implicit none
      integer(ip), parameter :: alpha = 1
      integer(ip), parameter :: beta = 2
      integer(ip), parameter :: gamma = 3
      integer(ip), parameter :: dalpha = 4
      integer(ip), parameter :: dbeta = 5
      integer(ip), parameter :: dgamma = 6
      real(wp)              ca
      real(wp)              d
      real(wp), parameter :: delta  ( 3, 3 ) = reshape ( &
         [ 0.0_wp, -1.0_wp, 1.0_wp, 1.0_wp, 0.0_wp, -1.0_wp, -1.0_wp, &
           1.0_wp, 0.0_wp ], [ 3, 3 ] )
      real(wp)              domega ( 3 )
      real(wp)              drdt   ( 3, 3 )
      real(wp)              drdtrt ( 3, 3 )
      real(wp)              locang ( 6 )
      real(wp)              omega  ( 3 )
      real(wp)              r      ( 3, 3 )
      real(wp)              sa
      real(wp)              solutn ( 3, 3 )
      real(wp)              u
      real(wp)              v
      integer(ip)           a
      integer(ip)           b
      integer(ip)           i
      integer(ip)           j
      integer(ip)           k
      integer(ip)           l
      integer(ip)           locaxa
      integer(ip)           locaxb
      integer(ip)           locaxc
      integer(ip), parameter :: next   ( 3 ) = &
         [ 2, 3, 1 ]
      if ( ctx%failed() ) return

!
!     Get the rotation and derivative of the rotation separately.
!
      do i = 1,3

         k = i + 3

         do j = 1,3
            r    (i,j) = xform(i,j)
            drdt (i,j) = xform(k,j)
         end do

      end do

!
!     We have to do it sooner or later so we take care of getting
!     the various Euler angles now.  This will take care of all the
!     bad axis cases too so we don't have to check here.
!
      call m2eul ( ctx, r, axisa,         axisb,        axisc, &
                      eulang(alpha), eulang(beta), eulang(gamma) )

      if ( ctx%failed() ) return

!
!     Construct local copies of the axes, determine L and D from the
!     derivation above.
!
      a = axisa
      b = axisb
      l = 6 - a - b
      d = delta(a,b)

!
!                      t
!     Compute DR/DT * R   and extract OMEGA
!
      call mxmt ( drdt, r, drdtrt )

!
!     The vector corresponding to DRDTRT is computed as shown below.
!
!        w(1) = drdtrt (3,2)
!        w(2) = drdtrt (1,3)
!        w(3) = drdtrt (2,1)
!
!     However, we need the 3-vector
!
!        w(A)
!        w(B)
!        w(L)
!
!     We'll call this vector omega. It's computed as shown here.
!
!        omega(1) = w(A) = d*drdtrt(L,B)
!        omega(2) = w(B) = d*drdtrt(A,L)
!        omega(3) = w(L) = d*drdtrt(B,A)
!

      omega(1) = d*drdtrt(l,b)
      omega(2) = d*drdtrt(a,l)
      omega(3) = d*drdtrt(b,a)

!
!     Compute the various sines and cosines that we need.
!
      ca       = dcos( eulang(alpha) )
      sa       = dsin( eulang(alpha) )

      if ( axisa == axisc ) then
         u  =       dcos ( eulang(beta) )
         v  =   d * dsin ( eulang(beta) )
      else
         u  =  -d * dsin( eulang(beta) )
         v  =       dcos( eulang(beta) )
      end if

!
!     To avoid floating point overflows we make sure that we
!     can perform a division by V.  We do this by looking at U.
!     If it has absolute value 1, then we set V equal to zero.
!     After all U*U + V*V = 1 if SIN and COS and various arithmetic
!     operations work perfectly.
!
      if ( dabs(u) == 1.0_wp ) then
         v = 0.0_wp
      end if

!
!     We have to look at the singular case first. Recall from above that
!
!        [ W(A) ]   [ -1     0     -U   ][dALPHA/dt]
!        | W(B) | = |  0   -CA  -D*SA*V ||dBETA /dt|
!        [ W(C) ]   [  0  D*SA    -CA*V ][dGAMMA/dt]
!
!     The singularity arises if V = 0.  In this case the equation
!     becomes:  ( Note that U  is plus or minus 1 so that division
!     by U is the same as multiplication by U. )
!
!        [ OMEGA(1) ]   [ -1     0  -U  ][dALPHA/dt]
!        | OMEGA(2) | = |  0   -CA   0  ||dBETA /dt|
!        [ OMEGA(3) ]   [  0  D*SA   0  ][dGAMMA/dt]
!
      if ( v == 0 ) then

         unique         = .false.
         eulang(dalpha) =  0.0_wp
         eulang(dgamma) = -u*omega(1)

!
!        We solve for EULANG(DBETA) by selecting the more stable of
!        the two available equations.
!
         if ( dabs(ca) > dabs(sa) ) then
            eulang(dbeta) =   -omega(2)/ca
         else
            eulang(dbeta) =  d*omega(3)/sa
         end if

         return

      end if

!
!     The matrix needed to compute the derivatives uniquely
!     exists.  Construct it and carry out the multiplication.
!
!     [dALPHA/dt]   [ -1  U*D*SA/V  U*CA/V ] [ OMEGA(1) ]
!     |dBETA /dt| = |  0   -CA      D*SA   | [ OMEGA(2) |
!     [dGAMMA/dt]   [  0   -D*SA/V   -CA/V ] [ OMEGA(3) ]
!

      unique      = .true.

      solutn(1,1) =  -1.0_wp
      solutn(2,1) =   0.0_wp
      solutn(3,1) =   0.0_wp

      solutn(1,2) =   u*d*sa/v
      solutn(2,2) =      -ca
      solutn(3,2) =    -d*sa/v

      solutn(1,3) =     u*ca/v
      solutn(2,3) =     d*sa
      solutn(3,3) =      -ca/v

      call mxv ( solutn, omega, eulang(4) )

      return
      end procedure xf2eul

      !>## Detailed Input
      !>
      !>     EULANG   is the set of Euler angles corresponding to the
      !>              specified factorization.
      !>
      !>              If we represent R as shown here:
      !>
      !>                 R =  [ ALPHA ]      [ BETA ]      [ GAMMA ]
      !>                               AXISA         AXISB          AXISC
      !>
      !>              then
      !>
      !>                 EULANG(1) = ALPHA
      !>                 EULANG(2) = BETA
      !>                 EULANG(3) = GAMMA
      !>                 EULANG(4) = dALPHA/dt
      !>                 EULANG(5) = dBETA/dt
      !>                 EULANG(6) = dGAMMA/dt
      !>
      !>     AXISA,
      !>     AXISB,
      !>     AXISC    are the axes desired for the factorization of R.
      !>
      !>              All must be in the range from 1 to 3. Moreover
      !>              it must be the case that AXISA and AXISB are distinct
      !>              and that AXISB and AXISC are distinct.
      !>
      !>              Every rotation matrix can be represented as a product
      !>              of three rotation matrices about the principal axes
      !>              of a reference frame.
      !>
      !>                 R =  [ ALPHA ]      [ BETA ]      [ GAMMA ]
      !>                               AXISA         AXISB          AXISC
      !>
      !>              The value 1 corresponds to the X axis.
      !>              The value 2 corresponds to the Y axis.
      !>              The value 3 corresponds to the Z axis.
      !>
      !>## Detailed Output
      !>
      !>     XFORM    is the state transformation matrix corresponding to R
      !>              and dR/dt as described above. Pictorially,
      !>
      !>                 .-             -.
      !>                 |       |       |
      !>                 |   R   |   0   |
      !>                 |       |       |
      !>                 |-------+-------|
      !>                 |       |       |
      !>                 | dR/dt |   R   |
      !>                 |       |       |
      !>                 `-             -'
      !>
      !>              where R is a rotation matrix that varies with respect to
      !>              time and dR/dt is its time derivative.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If any of AXISA, AXISB, or AXISC do not have values in
      !>
      !>            { 1, 2, 3 }
      !>
      !>         an error is signaled by a routine in the call tree of this
      !>         routine.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     A word about notation: the symbol
      !>
      !>        [ x ]
      !>             i
      !>
      !>     indicates a coordinate system rotation of x radians about the
      !>     ith coordinate axis. To be specific, the symbol
      !>
      !>        [ x ]
      !>             1
      !>
      !>     indicates a coordinate system rotation of x radians about the
      !>     first, or x-, axis; the corresponding matrix is
      !>
      !>        .-                    -.
      !>        |  1      0       0    |
      !>        |                      |
      !>        |  0    cos(x)  sin(x) |
      !>        |                      |
      !>        |  0   -sin(x)  cos(x) |
      !>        `-                    -'
      !>
      !>     Remember, this is a COORDINATE SYSTEM rotation by x radians; this
      !>     matrix, when applied to a vector, rotates the vector by -x
      !>     radians, not x radians. Applying the matrix to a vector yields
      !>     the vector's representation relative to the rotated coordinate
      !>     system.
      !>
      !>     The analogous rotation about the second, or y-, axis is
      !>     represented by
      !>
      !>        [ x ]
      !>             2
      !>
      !>     which symbolizes the matrix
      !>
      !>        .-                    -.
      !>        | cos(x)   0   -sin(x) |
      !>        |                      |
      !>        |  0       1      0    |
      !>        |                      |
      !>        | sin(x)   0    cos(x) |
      !>        `-                    -'
      !>
      !>     and the analogous rotation about the third, or z-, axis is
      !>     represented by
      !>
      !>        [ x ]
      !>             3
      !>
      !>     which symbolizes the matrix
      !>
      !>        .-                    -.
      !>        |  cos(x)  sin(x)   0  |
      !>        |                      |
      !>        | -sin(x)  cos(x)   0  |
      !>        |                      |
      !>        |  0        0       1  |
      !>        `-                    -'
      !>
      !>     The input matrix is assumed to be the product of three
      !>     rotation matrices, each one of the form
      !>
      !>        .-                    -.
      !>        |  1      0       0    |
      !>        |                      |
      !>        |  0    cos(r)  sin(r) |     (rotation of r radians about the
      !>        |                      |      x-axis),
      !>        |  0   -sin(r)  cos(r) |
      !>        `-                    -'
      !>
      !>        .-                    -.
      !>        | cos(s)   0   -sin(s) |
      !>        |                      |
      !>        |  0       1      0    |     (rotation of s radians about the
      !>        |                      |      y-axis),
      !>        | sin(s)   0    cos(s) |
      !>        `-                    -'
      !>
      !>     or
      !>
      !>        .-                    -.
      !>        |  cos(t)  sin(t)   0  |
      !>        |                      |
      !>        | -sin(t)  cos(t)   0  |     (rotation of t radians about the
      !>        |                      |      z-axis),
      !>        |  0        0       1  |
      !>        `-                    -'
      !>
      !>     where the second rotation axis is not equal to the first or
      !>     third. Any rotation matrix can be factored as a sequence of
      !>     three such rotations, provided that this last criterion is met.
      !>
      !>     This routine is intended to provide an inverse for XF2EUL.
      !>
      !>     The two subroutine calls shown here will not change
      !>     XFORM except for round off errors.
      !>
      !>        CALL XF2EUL ( XFORM,  AXISA, AXISB, AXISC, EULANG, UNIQUE )
      !>        CALL EUL2XF ( EULANG, AXISA, AXISB, AXISC, XFORM          )
      !>
      !>     On the other hand the two calls
      !>
      !>        CALL EUL2XF ( EULANG, AXISA, AXISB, AXISC, XFORM          )
      !>        CALL XF2EUL ( XFORM,  AXISA, AXISB, AXISC, EULANG, UNIQUE )
      !>
      !>     will leave EULANG unchanged only if the components of EULANG
      !>     are in the range produced by XF2EUL and the Euler representation
      !>     of the rotation component of XFORM is unique within that range.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Suppose you have a set of Euler angles and their derivatives
      !>        for a 3 1 3 rotation, and that you would like to determine
      !>        the equivalent angles and derivatives for a 1 2 3 rotation.
      !>
      !>           R = [ALPHA]  [BETA]  [GAMMA]
      !>                      3       1        3
      !>
      !>           R = [ROLL]  [PITCH]  [YAW]
      !>                     1        2      3
      !>
      !>        The following code example will perform the desired
      !>        computation.
      !>
      !>        Example code begins here.
      !>
      !>              PROGRAM EUL2XF_EX1
      !>              IMPLICIT NONE
      !>
      !>        C
      !>        C     Local variables.
      !>        C
      !>              DOUBLE PRECISION      ABGANG ( 6    )
      !>              DOUBLE PRECISION      RPYANG ( 6    )
      !>              DOUBLE PRECISION      XFORM  ( 6, 6 )
      !>
      !>              LOGICAL               UNIQUE
      !>
      !>        C
      !>        C     Define the initial set of Euler angles.
      !>        C
      !>              ABGANG(1) =  0.01D0
      !>              ABGANG(2) =  0.03D0
      !>              ABGANG(3) =  0.09D0
      !>              ABGANG(4) = -0.001D0
      !>              ABGANG(5) = -0.003D0
      !>              ABGANG(6) = -0.009D0
      !>
      !>        C
      !>        C     Compute the equivalent angles and derivatives for a
      !>        C     1-2-3 rotation.
      !>        C
      !>              CALL EUL2XF ( ABGANG, 3, 1, 3, XFORM  )
      !>              CALL XF2EUL ( XFORM,  1, 2, 3, RPYANG, UNIQUE )
      !>
      !>              IF ( UNIQUE )  THEN
      !>
      !>                 WRITE(*,'(A)') '1-2-3 equivalent rotation to input '
      !>             .               // '(radians):'
      !>                 WRITE(*,'(2(A,F13.9))') 'Roll  ', RPYANG(1),
      !>             .                           ', dRoll/dt  ', RPYANG(4)
      !>                 WRITE(*,'(2(A,F13.9))') 'Pitch ', RPYANG(2),
      !>             .                           ', dPitch/dt ', RPYANG(5)
      !>                 WRITE(*,'(2(A,F13.9))') 'Yaw   ', RPYANG(3),
      !>             .                           ', dYaw/dt   ', RPYANG(6)
      !>
      !>              ELSE
      !>
      !>                 WRITE(*,*) 'The values in RPYANG are not uniquely '
      !>             .          //  'determined.'
      !>
      !>              END IF
      !>
      !>              END
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        1-2-3 equivalent rotation to input (radians):
      !>        Roll    0.029998501, dRoll/dt   -0.002999550
      !>        Pitch  -0.000299950, dPitch/dt   0.000059980
      !>        Yaw     0.099995501, dYaw/dt    -0.009998650
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.1.0, 26-OCT-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example based on existing fragment.
      !>
      !>-    SPICELIB Version 2.0.1, 25-APR-2007 (EDW)
      !>
      !>        Corrected code in $Examples section, example showed
      !>        a XF2EUL call:
      !>
      !>           CALL XF2EUL ( XFORM,  1, 2, 3, RPYANG )
      !>
      !>        The proper form of the call:
      !>
      !>           CALL XF2EUL ( XFORM,  1, 2, 3, RPYANG, UNIQUE )
      !>
      !>-    SPICELIB Version 2.0.0, 31-OCT-2005 (NJB)
      !>
      !>        Restriction that second axis must differ from both the first
      !>        and third axes was removed.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JUL-1995 (WLT)
      module procedure eul2xf
      use pepper_kinds, only: ip, wp
      use pepper_math, only: mxm, mxv
      implicit none
      integer(ip), parameter :: alpha = 1
      integer(ip), parameter :: beta = 2
      integer(ip), parameter :: gamma = 3
      integer(ip), parameter :: dalpha = 4
      integer(ip), parameter :: dbeta = 5
      integer(ip), parameter :: dgamma = 6
      real(wp)              ca
      real(wp)              d
      real(wp), parameter :: delta  ( 3, 3 ) = reshape ( &
         [ 0.0_wp, -1.0_wp, 1.0_wp, 1.0_wp, 0.0_wp, -1.0_wp, -1.0_wp, &
           1.0_wp, 0.0_wp ], [ 3, 3 ] )
      real(wp)              domega ( 3 )
      real(wp)              drdt   ( 3, 3 )
      real(wp)              drdtrt ( 3, 3 )
      real(wp)              locang ( 6 )
      real(wp)              omega  ( 3 )
      real(wp)              r      ( 3, 3 )
      real(wp)              sa
      real(wp)              solutn ( 3, 3 )
      real(wp)              u
      real(wp)              v
      integer(ip)           a
      integer(ip)           b
      integer(ip)           i
      integer(ip)           j
      integer(ip)           k
      integer(ip)           l
      integer(ip)           locaxa
      integer(ip)           locaxb
      integer(ip)           locaxc
      integer(ip), parameter :: next   ( 3 ) = &
         [ 2, 3, 1 ]
      if ( ctx%failed() ) return

!
!     We're going to work with a local copy LOCANG of the euler angle
!     state vector EULANG.  We'll also use a local set of axis
!     numbers.
!
      locang = eulang

      locaxa = axisa
      locaxb = axisb
      locaxc = axisc

!
!     Parts of the following algorithm depend on the central axis
!     being different from the first and third axes.  We'll adjust
!     the axes and angles to make this so, if necessary.
!
      if (  ( axisb == axisa ) .or. ( axisb == axisc )  ) then

         if ( axisb == axisa ) then
!
!           The first angle will "absorb" the second, and the
!           second will be set to zero.  All we do here is select
!           the first angle.
!
            i = 1
         else
            i = 3
         end if
!
!        Absorb the second angle into the selected angle and set the
!        second angle to zero.  The same goes for the angular rates.
!
         locang(i)   = locang(i)   + locang(2)
         locang(2)   = 0.0_wp

         locang(i+3) = locang(i+3) + locang(5)
         locang(5)   = 0.0_wp

!
!        Pick a second axis that doesn't match the others.  Since
!        the rotation angle about the second axis is zero, all that
!        matters here is picking a distinct axis.
!
         if ( axisc == next(axisa) ) then
!
!           The first axis is the predecessor of the third, so we pick
!           the successor of the third.
!
            locaxb = next( axisc )

         else
!
!           Either the third axis is the predecessor of the first or
!           matches the first, so the successor of the first is our
!           choice.
!
            locaxb = next( axisa )

         end if

      end if

!
!     The following local variables are set:
!
!        LOCANG(*), LOCAXA, LOCAXB, LOCAXC
!
!     These variables describe the input rotation, but the second
!     axis is now guaranteed to differ from the first and third.
!
!     The derivation for everything that is about to happen here
!     is included in the previous entry point.
!
      call eul2m ( ctx, locang(alpha), locang(beta), locang(gamma), &
                   locaxa,        locaxb,       locaxc,         r )

      if ( ctx%failed() ) return

!
!     Construct local copies of the axes, determine L and D from the
!     derivation above.
!
      a = locaxa
      b = locaxb
      l = 6 - a - b
      d = delta(a,b)

!
!     Compute the various sines and cosines that we need.
!
      ca       = dcos( locang(alpha) )
      sa       = dsin( locang(alpha) )

      if ( locaxa == locaxc ) then
         u  =       dcos ( locang(beta) )
         v  =   d * dsin ( locang(beta) )
      else
         u  =  -d * dsin( locang(beta) )
         v  =       dcos( locang(beta) )
      end if

!
!                            t
!     Next we compute dR/dt R.  Recall from the derivation above
!     that
!
!        [ W(A) ]   [ -1     0     -U   ][dALPHA/dt]
!        | W(B) | = |  0   -CA  -D*SA*V ||dBETA /dt|
!        [ W(L) ]   [  0  D*SA    -CA*V ][dGAMMA/dt]
!
!     In the previous entry point we used OMEGA for the vector
!     of rearranged components of W.
!
!        OMEGA(1) = W(A) = D*DRDTRT(L,B)
!        OMEGA(2) = W(B) = D*DRDTRT(A,L)
!        OMEGA(3) = W(L) = D*DRDTRT(B,A)
!
!        DRDTRT(L,B) = D*OMEGA(1)
!        DRDTRT(A,L) = D*OMEGA(2)
!        DRDTRT(B,A) = D*OMEGA(3)
!
!        [ DRDTRT(L,B) ]   [ -D     0     -D*U ][dALPHA/dt]
!        | DRDTRT(A,L) | = |  0 -D*CA    -SA*V ||dBETA /dt|
!        [ DRDTRT(B,A) ]   [  0    SA  -D*CA*V ][dGAMMA/dt]
!
!     We set up the matrix of this equation in SOLUTN below
!     and compute D*OMEGA which we denote by the variable DOMEGA.
!
      solutn(1,1) =  -d
      solutn(2,1) =   0.0_wp
      solutn(3,1) =   0.0_wp

      solutn(1,2) =   0.0_wp
      solutn(2,2) =  -d*ca
      solutn(3,2) =     sa

      solutn(1,3) =     -d*u
      solutn(2,3) =    -sa*v
      solutn(3,3) =  -d*ca*v

      call mxv ( solutn, locang(4), domega )

      drdtrt(l,b) =  domega(1)
      drdtrt(b,l) = -domega(1)

      drdtrt(a,l) =  domega(2)
      drdtrt(l,a) = -domega(2)

      drdtrt(b,a) =  domega(3)
      drdtrt(a,b) = -domega(3)

      drdtrt(1,1) =  0.0_wp
      drdtrt(2,2) =  0.0_wp
      drdtrt(3,3) =  0.0_wp

      call mxm ( drdtrt, r, drdt )

      do j = 1,3
         do i = 1,3
            xform(i,  j  ) = r   (i,j)
            xform(i+3,j+3) = r   (i,j)
            xform(i+3,j  ) = drdt(i,j)
            xform(i,  j+3) = 0.0_wp
         end do
      end do

      return
      end procedure eul2xf

      !>## Detailed Input
      !>
      !>     AXDEF    is a vector defining one of the principle axes of a
      !>              coordinate frame.
      !>
      !>     INDEXA   is a number that determines which of the three
      !>              coordinate axes contains AXDEF.
      !>
      !>              If INDEXA is 1 then AXDEF defines the X axis of the
      !>              coordinate frame.
      !>
      !>              If INDEXA is 2 then AXDEF defines the Y axis of the
      !>              coordinate frame.
      !>
      !>              If INDEXA is 3 then AXDEF defines the Z axis of the
      !>              coordinate frame.
      !>
      !>     PLNDEF   is a vector defining (with AXDEF) a principal plane of
      !>              the coordinate frame. AXDEF and PLNDEF must be
      !>              linearly independent.
      !>
      !>     INDEXP   is the second axis of the principal frame determined
      !>              by AXDEF and PLNDEF.  INDEXA, INDEXP must be different
      !>              and be integers from 1 to 3.
      !>
      !>              If INDEXP is 1, the second axis of the principal
      !>              plane is the X-axis.
      !>
      !>              If INDEXP is 2, the second axis of the principal
      !>              plane is the Y-axis.
      !>
      !>              If INDEXP is 3, the second axis of the principal plane
      !>              is the Z-axis.
      !>
      !>## Detailed Output
      !>
      !>     MOUT     is a rotation matrix that transforms coordinates given
      !>              in the input frame to the frame determined by AXDEF,
      !>              PLNDEF, INDEXA and INDEXP.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If INDEXA or INDEXP is not in the set {1,2,3}, the error
      !>         SPICE(BADINDEX) is signaled.
      !>
      !>     2)  If INDEXA and INDEXP are the same, the error
      !>         SPICE(UNDEFINEDFRAME) is signaled.
      !>
      !>     3)  If the cross product of the vectors AXDEF and PLNDEF is zero,
      !>         the error SPICE(DEPENDENTVECTORS) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Given two linearly independent vectors there is a unique
      !>     right-handed coordinate frame having:
      !>
      !>        AXDEF lying along the INDEXA axis.
      !>
      !>        PLNDEF lying in the INDEXA-INDEXP coordinate plane.
      !>
      !>     This routine determines the transformation matrix that transforms
      !>     from coordinates used to represent the input vectors to the
      !>     the system determined by AXDEF and PLNDEF. Thus a vector
      !>     (x,y,z) in the input coordinate system will have coordinates
      !>
      !>                     t
      !>        MOUT* (x,y,z)
      !>
      !>     in the frame determined by AXDEF and PLNDEF.
      !>
      !>## Examples
      !>
      !>     The rotation matrix TICC from inertial to Sun-Canopus
      !>     (celestial) coordinates is found by the call
      !>
      !>        CALL TWOVEC (Sun vector, 3, Canopus vector, 1, TICC)
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.M. Owen          (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 06-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section.
      !>
      !>-    SPICELIB Version 1.1.0, 31-AUG-2005 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in VSCL call.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) (WLT)
      module procedure twovec
      use pepper_kinds, only: ip, wp
      use pepper_math, only: ucrss, vhat
      implicit none

      real(wp)              mtemp  ( 3, 3 )

      integer(ip)           i1
      integer(ip)           i2
      integer(ip)           i3
      integer(ip), parameter :: seqnce ( 5 ) = &
         [ 1, 2, 3, 1, 2 ]

!
!     Standard SPICE error handling
!
      if ( ctx%failed() ) return

!
!     Check for obvious bad inputs.
!
      if (      ( max(indexp,indexa) > 3 ) &
           .or. ( min(indexp,indexa) < 1 ) ) then

         call ctx%setmsg ( 'The definition indexes must lie in the range '// &
                       'from 1 to 3.  The value of INDEXA was #. '    // &
                       'The value of INDEXP was #. '                  )

         call ctx%errint ( '#', indexa  )
         call ctx%errint ( '#', indexp  )
         call ctx%sigerr ( 'SPICE(BADINDEX)' )
         return

      else if   ( indexa == indexp )  then

         call ctx%setmsg ( 'The values of INDEXA and INDEXP were the '    // &
                       'same, namely #.  They are required to be '    // &
                       'different.'                                   )

         call ctx%errint ( '#', indexa  )
         call ctx%sigerr ( 'SPICE(UNDEFINEDFRAME)' )
         return

      end if

!
!     Get indices for right-handed axes
!
!     First AXDEF ...
!
      i1 = indexa
!
!     ... then the other two.
!
      i2 = seqnce ( indexa + 1 )
      i3 = seqnce ( indexa + 2 )

!
!     Row I1 contains normalized AXDEF (store in columns for now)
!
      call vhat ( axdef, mout(1,i1) )

!
!     Obtain rows I2 and I3 using cross products.  Which order to use
!     depends on whether INDEXP = I2 (next axis in right-handed order)
!     or INDEXP = I3 (previous axis in right-handed order).
!
      if ( indexp == i2 ) then

         call ucrss ( axdef,      plndef,        mout(1,i3) )
         call ucrss ( mout(1,i3), axdef,         mout(1,i2) )

      else

         call ucrss ( plndef,     axdef,         mout(1,i2) )
         call ucrss ( axdef,      mout(1,i2),    mout(1,i3) )

      end if

!
!     Finally, check to see that we actually got something non-zero
!     in one of the one columns of MOUT(1,I2) and MOUT(1,I3) (we need
!     only check one of them since they are related by a cross product).
!
      if (       ( mout(1,i2) == 0.0_wp ) &
           .and. ( mout(2,i2) == 0.0_wp ) &
           .and. ( mout(3,i2) == 0.0_wp ) ) then

         call ctx%setmsg ( 'The input vectors AXDEF and PLNDEF are '      // &
                       'linearly dependent.'                          )
         call ctx%sigerr ( 'SPICE(DEPENDENTVECTORS)' )

      end if
!
!     Transpose MOUT.
!
      mtemp = transpose(mout)
      mout = mtemp

      return
      end procedure twovec

      !>## Abstract
      !>
      !>     SPICE Private routine intended solely for the support of SPICE
      !>     routines.  Users should not call this routine directly due
      !>     to the volatile nature of this routine.
      !>
      !>     Find the state transformation to a base frame from the
      !>     right-handed frame defined by two state vectors:  one state
      !>     vector defining a specified axis and a second state vector
      !>     defining a specified coordinate plane.
      !>
      !>## Detailed Input
      !>
      !>     AXDEF      is a "generalized" state vector defining one of the
      !>                principal axes of a reference frame. This vector
      !>                consists of three components of a vector-valued
      !>                function of one independent variable t followed by
      !>                the derivatives of the components with respect to that
      !>                variable:
      !>
      !>                   ( a, b, c, da/dt, db/dt, dc/dt )
      !>
      !>                This routine treats the input states as unitless, but
      !>                in most applications the input states represent
      !>                quantities that have associated units. The first three
      !>                components must have the same units, and the units of
      !>                the last three components must be compatible with
      !>                those of the first three:  if the first three
      !>                components of AXDEF
      !>
      !>                   ( a, b, c )
      !>
      !>                have units U and t has units T, then the units of
      !>                AXDEF normally would be
      !>
      !>                   ( U, U, U, U/T, U/T, U/T )
      !>
      !>                Note that the direction and angular velocity defined
      !>                by AXDEF are actually independent of U, so scaling
      !>                AXDEF doesn't affect the output of this routine.
      !>
      !>                AXDEF could represent position and velocity; it could
      !>                also represent velocity and acceleration.  AXDEF could
      !>                for example represent the velocity and acceleration of
      !>                a time-dependent position vector ( x(t), y(t), z(t) ),
      !>                in which case AXDEF would be defined by
      !>
      !>                   a     = dx/dt
      !>                   b     = dy/dt
      !>                   c     = dz/dt
      !>
      !>                            2      2
      !>                   da/dt = d x / dt
      !>
      !>                            2      2
      !>                   db/dt = d y / dt
      !>
      !>                            2      2
      !>                   dc/dt = d z / dt
      !>
      !>                Below, we'll call the normalized (unit length) version
      !>                of
      !>
      !>                   ( a, b, c )
      !>
      !>                the "direction" of AXDEF.
      !>
      !>                We call the frame relative to which AXDEF is specified
      !>                the "base frame."  The input state PLNDEF must be
      !>                specified relative to the same base frame.
      !>
      !>     INDEXA     is the index of the reference frame axis that is
      !>                parallel to the direction of AXDEF.
      !>
      !>                   Value of INDEXA             Axis
      !>
      !>                         1                      X
      !>                         2                      Y
      !>                         3                      Z
      !>
      !>     PLNDEF     is a state vector defining (with AXDEF) a principal
      !>                plane of the reference frame.  This vector consists
      !>                of three components followed by their derivatives with
      !>                respect to the independent variable t associated with
      !>                AXDEF, so PLNDEF is
      !>
      !>                   ( e, f, g, de/dt, df/dt, dg/dt )
      !>
      !>                Below, we'll call the unitized version of
      !>
      !>                   ( e, f, g )
      !>
      !>                the "direction" of PLNDEF.
      !>
      !>                The second axis of the principal plane containing the
      !>                direction vectors of AXDEF and PLNDEF is perpendicular
      !>                to the first axis and has positive dot product with
      !>                the direction vector of PLNDEF.
      !>
      !>                The first three components of PLNDEF must have the
      !>                same units, and the units of the last three components
      !>                must be compatible with those of the first three:  if
      !>                the first three components of PLNDEF
      !>
      !>                   ( e, f, g )
      !>
      !>                have units U2 and t has units T, then the units of
      !>                PLNDEF normally would be
      !>
      !>                   ( U2, U2, U2, U2/T, U2/T, U2/T )
      !>
      !>                ***For meaningful results, the angular velocities
      !>                   defined by AXDEF and PLNDEF must both have units of
      !>                   1/T.***
      !>
      !>                As with AXDEF, scaling PLNDEF doesn't affect the
      !>                output of this routine.
      !>
      !>                AXDEF and PLNDEF must be specified relative to a
      !>                common reference frame, which we call the "base
      !>                frame."
      !>
      !>     INDEXP     is the index of  second axis of the principal frame
      !>                determined by AXDEF and PLNDEF.  The association of
      !>                integer values and axes is the same as for INDEXA.
      !>
      !>## Detailed Output
      !>
      !>     XFORM      is the 6x6 matrix that transforms states to the frame
      !>                relative to which AXDEF and PLNDEF are specified (the
      !>                "base frame") from the frame whose axes and derivative
      !>                are determined by AXDEF, PLNDEF, INDEXA and INDEXP.
      !>
      !>                The matrix XFORM has the structure shown below:
      !>
      !>                    -            -
      !>                   |       :      |
      !>                   |   R   :  0   |
      !>                   | ......:......|
      !>                   |       :      |
      !>                   | dR_dt :  R   |
      !>                   |       :      |
      !>                    -            -
      !>
      !>                where R is a rotation matrix that is a function of
      !>                the independent variable associated with AXDEF and
      !>                PLNDEF, and where dR_dt is the derivative of R
      !>                with respect to that independent variable.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1) If INDEXA or INDEXP is not in the set {1,2,3} the error
      !>        SPICE(BADINDEX) will be signaled.
      !>
      !>     2) If INDEXA and INDEXP are the same the error
      !>        SPICE(UNDEFINEDFRAME) will be signaled.
      !>
      !>     3) If the cross product of the vectors AXDEF and PLNDEF is zero,
      !>        the error SPICE(DEPENDENTVECTORS) will be signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine exists to support the public routine TWOVXF:
      !>     TWOVXF does its job by calling this routine, inverting the
      !>     matrix returned by this routine, and returning the result.
      !>
      !>     The SPICELIB frame subsystem typically requires this routine
      !>     rather than TWOVXF, since the frame subsystem produces
      !>     transformations from frames defined in frame kernels to their
      !>     base frames.  Calling this routine rather than TWOVXF allows
      !>     the frame subsystem to eliminate two unnecessary calls to
      !>     INVSTM.
      !>
      !>     Given two linearly independent state vectors AXDEF and PLNDEF,
      !>     define vectors DIR1 and DIR2 by
      !>
      !>        DIR1 = ( AXDEF(1),   AXDEF(2),   AXDEF(3)  )
      !>        DIR2 = ( PLNDEF(1),  PLNDEF(2),  PLNDEF(3) )
      !>
      !>     Then there is a unique right-handed reference frame F having:
      !>
      !>        DIR1 lying along the INDEXA axis.
      !>
      !>        DIR2 lying in the INDEXA-INDEXP coordinate plane, such that
      !>        the dot product of DIR2 with the positive INDEXP axis is
      !>        positive.
      !>
      !>     This routine determines the 6x6 matrix that transforms states
      !>     to the base frame used to represent the input vectors from the
      !>     the frame F determined by AXDEF and PLNDEF.  Thus a state vector
      !>
      !>        S       = ( x, y, z, dx/dt, dy/dt, dz/dt )
      !>         F
      !>
      !>     in the reference frame F will be transformed to
      !>
      !>        S      = XFORM * S
      !>         base             F
      !>
      !>     in the base frame relative to which AXDEF and PLNDEF are
      !>     specified.
      !>
      !>## Examples
      !>
      !>     See TWOVXF.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman    (JPL)
      !>     W.M. Owen       (JPL)
      !>     W.L. Taber      (JPL)
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 06-SEP-2005 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in DUCRSS and MOVED calls.
      !>
      !>-    SPICELIB Version 1.0.0, 18-DEC-2004 (NJB) (WMO) (WLT)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 1.1.0, 06-SEP-2005 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in DUCRSS and MOVED calls.
      !>
      !>     SPICELIB functions
      !>
      !>     Local Variables
      module procedure zztwovxf
      use pepper_kinds, only: ip, wp
      use pepper_math, only: ducrss, dvhat
      real(wp)              tmpsta ( 6 )

      integer(ip)           i
      integer(ip)           i1
      integer(ip)           i2
      integer(ip)           i3
      integer(ip)           j
      integer(ip), parameter :: seqnce ( 5 ) = &
         [ 1, 2, 3, 1, 2 ]

!
!     Standard SPICE error handling
!
      if ( ctx%failed() ) return

!
!     Check for obvious bad inputs.
!
      if (      ( max(indexp,indexa) > 3 ) &
           .or. ( min(indexp,indexa) < 1 ) ) then

         call ctx%setmsg ( 'The definition indices must lie in the range '// &
                       'from 1 to 3.  The value of INDEXA was #. '    // &
                       'The value of INDEXP was #. '                  )
         call ctx%errint ( '#', indexa       )
         call ctx%errint ( '#', indexp       )
         call ctx%sigerr ( 'SPICE(BADINDEX)' )
         return

      else if ( indexa == indexp ) then

         call ctx%setmsg ( 'The values of INDEXA and INDEXP were the '    // &
                       'same, namely #.  They are required to be '    // &
                       'different.'                                   )
         call ctx%errint ( '#', indexa             )
         call ctx%sigerr ( 'SPICE(UNDEFINEDFRAME)' )
         return

      end if

!
!     Get indices for right-handed axes:
!
!     First AXDEF ...
!
      i1 = indexa
!
!     ... then the other two.
!
      i2 = seqnce ( indexa + 1 )
      i3 = seqnce ( indexa + 2 )

!
!     Column I1 of XFORM contains a unit vector parallel to AXDEF and
!     the derivative of the unit vector.
!
      call dvhat ( axdef, xform(1,i1) )

!
!     Obtain columns I2 and I3 of XFORM using cross products.
!     Which order to use depends on whether INDEXP = I2 (next axis in
!     right-handed order) or INDEXP = I3 (previous axis in right-handed
!     order).
!
!     Select column indices...
!
      if ( indexp == i2 ) then
!
!        We compute the third axis in the sequence, then the second.
!
         call ducrss ( axdef,        plndef,        xform(1,i3) )
         call ducrss ( xform(1,i3),  axdef,         tmpsta      )
         xform(:,i2) = tmpsta
      else
         call ducrss ( plndef,       axdef,         xform(1,i2) )
         call ducrss ( axdef,        xform(1,i2),   tmpsta      )
         xform(:,i3) = tmpsta
      end if

!
!     ...and compute the output frame's non-principal unit basis
!     vectors and the derivatives of these vectors.
!

!
!     At this point, we've filled in the left half of XFORM.
!
!     The upper right block is the 3x3 zero matrix.
!     The lower right block matches the upper left block.
!
      xform(1:3,4) = 0.0_wp
      xform(1:3,5) = 0.0_wp
      xform(1:3,6) = 0.0_wp

      do j = 1, 3

         do i = 1, 3

            xform(3+i,3+j) = xform(i,j)

         end do

      end do

!
!     Finally, check to see that we actually got something non-zero in
!     the first three components of at least one of the columns
!     XFORM(1,I2) and XFORM(1,I3) (we need only check one of them since
!     they are related by a cross product).
!
      if (  all(xform(1:3,i2) == 0.0_wp)  ) then

         call ctx%setmsg ( 'The direction vectors associated with ' // &
                       'states AXDEF and PLNDEF are '           // &
                       'linearly dependent.'                    )
         call ctx%sigerr ( 'SPICE(DEPENDENTVECTORS)'                )
         return

      end if

      return
      end procedure zztwovxf

!$Procedure CHGIRF ( Change inertial reference frames )
!
!     PEPPER: SPICELIB's umbrella routine CHGIRF is not a procedure any more:
!     its entry points are the module procedures below, and its saved
!     variables are in pepper_state_chgirf. Its documentation follows.
!

! $ Abstract
!
!     Support changes among a standard set of inertial coordinate
!     reference frames.
!
! $ Detailed_Input
!
!     See entry points IRFROT, IRFNUM, IRFNAM, and IRFDEF.
!
! $ Detailed_Output
!
!     See entry points IRFROT, IRFNUM, and IRFNAM.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If CHGIRF is called directly, the signal, the error
!         SPICE(BOGUSENTRY) is signaled.
!
!     2)  See entry points IRFROT, IRFNUM, IRFNAM, and IRFDEF for
!         exceptions specific to those routines.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     CHGIRF exists only as an umbrella for data to be shared
!     by its entry points (IRFROT, IRFNUM, IRFNAM, and IRFDEF).
!     It should never be called directly.
!
! $ Examples
!
!     See entry points IRFROT, IRFNUM, IRFNAM, and IRFDEF.
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     [1]  J. Lieske, "Precession Matrix Based on IAU (1976) System of
!          Astronomical Constants," Astron. Astrophys. 73, 282-284,
!          1979.
!
!     [2]  E. M. Standish, Jr., "Orientation of the JPL Ephemerides,
!          DE 200/LE 200, to the Dynamical Equinox of J2000," Astron.
!          Astrophys. 114, 297-302, 1982.
!
!     [3]  E. M. Standish, Jr., "Conversion of Ephemeris Coordinates
!          from the B1950 System to the J2000 System," JPL IOM
!          314.6-581, 24 June 1985.
!
!     [4]  E. M. Standish, Jr., "The Equinox Offsets of the JPL
!          Ephemeris," JPL IOM 314.6-929, 26 February 1988.
!
!     [5]  J. Lieske, "Expressions for the Precession Quantities Based
!          upon the IAU (1976) System of Astronomical Constants,"
!          Astron. Astrophys. 58, 1-16, 1977.
!
!     [6]  L. Bass and R. Cesarone, "Mars Observer Planetary Constants
!          and Models," JPL D-3444, November 1990.
!
!     [7]  P. Kenneth Seidelmann (Ed.), "Explanatory Supplement to the
!          Astronomical Almanac," University Science Books, 1992.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 4.5.0, 06-NOV-2021 (JDR) (NJB)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!        Corrected sign error in comments describing the angles used to
!        convert between the J2000 and B1950 reference frames. Also
!        corrected a typo in the comments.
!
!-    SPICELIB Version 4.4.0, 24-SEP-2013 (BVS)
!
!        Updated entry point IRFNUM to treat J2000 as a special case
!        and to not participate of CHKIN/CHOUT to increase efficiency.
!
!-    SPICELIB Version 4.3.0, 25-AUG-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in CONVRT, ROTMAT and MXM calls.
!
!-    SPICELIB Version 4.2.1, 04-JAN-2002 (EDW)
!
!        Added DE-143 to header description for IRFROT.
!
!-    SPICELIB Version 4.2.0, 10-APR-1997 (WLT)
!
!        A descriptive diagnostic was added to the entry points
!        IRFROT and IRFDEF. Before they simply signaled the error
!        with no diagnostic.
!
!-    SPICELIB Version 4.1.0, 14-OCT-1996 (WLT)
!
!        The number of inertial frames recognized is now stored
!        in the include file ninert.inc.
!
!-    SPICELIB Version 4.0.0, 20-MAY-1996 (WLT)
!
!        The inertial frame DE-143 was added to the list of recognized
!        inertial frames.
!
!-    SPICELIB Version 3.0.0, 20-MAR-1995 (WLT)
!
!        The inertial frames DE-140 and DE-142  were added to the
!        list of recognized inertial frames.
!
!-    SPICELIB Version 2.0.0, 30-JUL-1993 (WLT)
!
!        The transformation from J2000 to B1950 was upgraded
!        so that the transformation matrix produced matches
!        the matrix given in [1].
!
!        The frame MARSIAU was added to the list
!        of recognized frames. This is the standard mars
!        referenced inertial frame used by the Mars Observer
!        project.
!
!        Values for the obliquity of the ecliptic were taken
!        from the Explanatory Supplement [7] to the Astronomical
!        Almanac (1992) at both the epochs J2000 and B1950 and
!        used to define the mean ecliptic and equinox frames
!        ECLIPJ2000 and ECLIPB1950.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
!

! $ Revisions
!
!-    SPICELIB Version 4.3.0, 25-AUG-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in CONVRT, ROTMAT  and MXM calls.
!

!     Each frame is defined in terms of another frame, except for
!     the root frame, which is defined in terms of itself. For now,
!     the root frame is the standard IAU reference frame, J2000,
!     defined by the Earth mean equator and dynamical equinox of
!     Julian year 2000.
!
!     Each definition consists of a series of rotations, each
!     through some angle (in arc seconds) and about some axis.
!     The rotations are listed in the opposite order in which
!     they are to be performed, so as to correspond more closely
!     to conventional notation. For example, the definition
!
!        FRAMES(i) = 'F2'
!        BASES(i)  = 'F1'
!        DEFS(i)   = '22.34  3   31.21  2   0.449  1'
!
!     means that a vector in frame F1 is converted to the equivalent
!     vector in frame F2 by applying the following rotation:
!
!        -                                            -
!        v    = ( [ 22.34 ]  [ 31.21 ]  [ 0.449 ]  )  v
!         F2               3          2          1     F1
!
!     where the notation
!
!        [ theta ]
!                 a
!
!     means ``rotate through angle theta about axis a.''
!
!     New frames may be added by:
!
!        1) Increasing the value of MAXF.
!
!        2) Adding new values for FRAMES, BASES, and DEFS.
!
!     The actual transformations (TRANS) will be computed during
!     initialization.
!
!     Note that BASES must be the name of a previously defined
!     reference frame, and that no frame should appear more than
!     once in FRAMES.
!
!     Note also that the list of valid reference frames maintained
!     by CHGIRF must be updated whenever new frames are added.
!

!
!     The root frame is mostly for show. Rotate by 0 arc seconds
!     about the x-axis to obtain the identity matrix.
!
!     PEPPER: the tables FRAMES, BASES, DEFS are parameters (declared above);
!     the comments on their entries follow.

!
!     The B1950 reference frame is obtained by precessing the J2000
!     frame backwards from Julian year 2000 to Besselian year 1950,
!     using the 1976 IAU precession model.
!
!     The rotation from B1950 to J2000 is
!
!        [ -z ]  [ theta ]  [ -zeta ]
!              3          2          3
!
!     So the rotation from J2000 to B1950 is the transpose,
!
!        [ zeta ]  [ -theta ]  [ z ]
!                3           2      3
!
!     The values for z, theta, and zeta are computed from the formulas
!     given in table 5 of [5].
!
!        z     =  1153.04066200330"
!        theta =  1002.26108439117"
!        zeta  =  1152.84248596724"
!

!
!     The FK4 reference frame is derived from the B1950 frame by
!     applying the equinox offset determined by Fricke. This is just
!     the rotation
!
!        [ 0.525" ]
!                  3
!

!
!     The DE-118 reference frame is nearly identical to the FK4
!     reference frame. It is also derived from the B1950 frame.
!     Only the offset is different:
!
!        [ 0.53155" ]
!                    3
!
!     In [2], Standish uses two separate rotations,
!
!        [ 0.00073" ]  P [ 0.5316" ]
!                    3              3
!
!     (where P is the precession matrix used above to define the
!     B1950 frame). The major effect of the second rotation is to
!     correct for truncating the magnitude of the first rotation.
!     At his suggestion, we will use the untruncated value, and
!     stick to a single rotation.
!

!
!     Most of the other DE reference frames may be defined relative
!     to either the DE-118 or B1950 frames. The values below are taken
!     from [4].
!
!        DE number   Offset from DE-118   Offset from B1950
!        ---------   ------------------   -----------------
!               96             +0.1209"            +0.4107"
!              102             +0.3956"            +0.1359"
!              108             +0.0541"            +0.4775"
!              111             -0.0564"            +0.5880"
!              114             -0.0213"            +0.5529"
!              122             +0.0000"            +0.5316"
!              125             -0.0438"            +0.5754"
!              130             +0.0069"            +0.5247"
!
!     We will use B1950 for now, since the offsets generally have
!     more significant digits.
!

!
!     The Galactic System II reference frame is defined by the
!     following rotations:
!
!             o          o            o
!        [ 327  ]  [ 62.6  ]  [ 282.25  ]
!                3          1            3
!
!      In the absence of better information, we will assume that
!      it is derived from the FK4 frame. Converting the angles from
!      degrees to arc seconds,
!
!           o
!        327      = 1177200"
!            o
!        62.6     =  225360"
!              o
!        282.25   = 1016100"
!

!
!     According to Standish, the various DE-200 frames are identical
!     with J2000, because he rotates the ephemerides before releasing
!     them (in order to avoid problems like the one that this routine
!     is designed to solve). Because we have to have something, we
!     will use
!
!             o
!        [ 0.0 ]
!               3
!

!
!     The values for the transformation from J2000 to MARSIAU_MO
!     are derived from the constants given for the pole of Mars
!     on page 8-2 of reference [6].
!

!
!     The value for the obliquity of the ecliptic at J2000  is
!     taken from page  114 of [7] equation 3.222-1.  This agrees
!     with the expression given in [5]
!

!
!     The value for the obliquity of the ecliptic at B1950  is
!     taken from page  171 of [7].
!

!
!     The frame for DE-140 is simply DE-400 rotated by the rotation:
!
!      0.9999256765384668  0.0111817701197967  0.0048589521583895
!     -0.0111817701797229  0.9999374816848701 -0.0000271545195858
!     -0.0048589520204830 -0.0000271791849815  0.9999881948535965
!
!     Note that the DE-400 frame is J2000.
!
!     The transpose of this is the frame from DE140 to DE400. To get
!     the euler angles below, the matrix given above was copied into
!     a matrix XFORM.
!
!     This matrix was transposed to give the transformation from
!     DE-140 to J2000.
!
!        CALL XPOSE ( XFORM, XFORM )
!
!     Using the SPICE routine M2EUL, the euler representation of the
!     transformation from DE140 to J2000 was constructed.
!
!        CALL M2EUL ( XFORM, 3, 2, 3, A1, A2, A3 )
!
!     Angles were converted to the range from -180 to 180 degrees
!     and converted to arcseconds.  At this point we have the
!     euler representation from DE-140 to J2000.
!
!        [ A1 ]  [ A2 ]  [ A3 ]
!              3       2       3
!
!     To get the Euler representation of the transformation from
!     J2000 to DE-140  we use.
!
!        [ -A3 ]  [ -A2 ] [ -A1 ]
!               3        2       3
!
!     This method was used because it yields a nicer form of
!     representation than the straight forward transformation.
!     Note that these numbers are quite close to the values used
!     for the transformation from J2000 to B1950
!

!
!     The frame for DE-142 is simply DE-402 rotated by the rotation:
!
!      0.9999256765402605  0.0111817697320531  0.0048589526815484
!     -0.0111817697907755  0.9999374816892126 -0.0000271547693170
!     -0.0048589525464121 -0.0000271789392288  0.9999881948510477
!
!     Note that the DE-402 frame is J2000.
!
!     The Euler angles giving the transformation for J2000 to
!     DE-142 were constructed in the same way as the transformation
!     from J2000 to DE140.  Only the input matrix changed to use the
!     one given above.
!

!
!     The frame for DE-143 is simply DE-403 rotated by the rotation:
!
!      0.9999256765435852  0.0111817743077255  0.0048589414674762
!     -0.0111817743300355  0.9999374816382505 -0.0000271622115251
!     -0.0048589414161348 -0.0000271713942366  0.9999881949053349
!
!     Note that the DE-403 frame is J2000.
!
!     The Euler angles giving the transformation for J2000 to
!     DE-143 were constructed in the same way as the transformation
!     from J2000 to DE140.  Only the input matrix changed to use the
!     one given above.
!

!
!     Until defined (by a call to IRFDEF), the default frame is
!     undefined.
!

!
!     Standard SPICE error handling.

      !>## Detailed Input
      !>
      !>     REFA,
      !>     REFB     are the indices of two standard inertial reference
      !>              frames. The complete set of supported frames is shown
      !>              below.
      !>
      !>                 Index  Name      Description
      !>                 -----  --------  --------------------------------
      !>                  1    J2000      Earth mean equator, dynamical
      !>                                  equinox of J2000
      !>
      !>                  2    B1950      Earth mean equator, dynamical
      !>                                  equinox of B1950
      !>
      !>                  3    FK4        Fundamental Catalog (4)
      !>
      !>                  4    DE-118     JPL Developmental Ephemeris (118)
      !>
      !>                  5    DE-96      JPL Developmental Ephemeris ( 96)
      !>
      !>                  6    DE-102     JPL Developmental Ephemeris (102)
      !>
      !>                  7    DE-108     JPL Developmental Ephemeris (108)
      !>
      !>                  8    DE-111     JPL Developmental Ephemeris (111)
      !>
      !>                  9    DE-114     JPL Developmental Ephemeris (114)
      !>
      !>                 10    DE-122     JPL Developmental Ephemeris (122)
      !>
      !>                 11    DE-125     JPL Developmental Ephemeris (125)
      !>
      !>                 12    DE-130     JPL Developmental Ephemeris (130)
      !>
      !>                 13    GALACTIC   Galactic System II
      !>
      !>                 14    DE-200     JPL Developmental Ephemeris (200)
      !>
      !>                 15    DE-202     JPL Developmental Ephemeris (202)
      !>
      !>                 16    MARSIAU    Mars Observer inertial frame
      !>                                  defined relative to MARS.
      !>
      !>                 17    ECLIPJ2000 Earth mean ecliptic and equinox
      !>                                  of the epoch J2000
      !>
      !>                 18    ECLIPB1950 Earth mean ecliptic and equinox
      !>                                  of the Besselian date 1950.
      !>
      !>                 19    DE-140    JPL Developmental Ephemeris (140)
      !>
      !>                 20    DE-142    JPL Developmental Ephemeris (142)
      !>
      !>                 21    DE-143    JPL Developmental Ephemeris (143)
      !>
      !>## Detailed Output
      !>
      !>     ROTAB    is the rotation which, when applied to a vector v
      !>              in reference frame A,
      !>                 _            _
      !>                 v  = (ROTAB) v
      !>                  B            A
      !>
      !>              yields the same vector in reference frame B. The
      !>              inverse rotation is performed by applying the
      !>              transpose,
      !>                 _           T _
      !>                 v  = (ROTAB)  v
      !>                  A             B
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If either REFA or REFB is outside the range [1,MAXF],
      !>         where MAXF is the number of supported frames, the error
      !>         SPICE(IRFNOTREC) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     IRFROT exists primarily for use by the ephemeris and star
      !>     catalog readers in the SPICELIB toolkit library.
      !>
      !>## Examples
      !>
      !>     In the following code fragment, IRFROT is used to rotate
      !>     vectors originally referenced to the DE-118 coordinate frame
      !>     to equivalent vectors referenced to the IAU standard J2000
      !>     reference frame.
      !>
      !>        CALL IRFROT ( 4, 1, R )
      !>
      !>        CALL MXV ( R, SC1950, SC2000 )
      !>        CALL MXV ( R, MP1950, MP2000 )
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     [1]  J. Lieske, "Precession Matrix Based on IAU (1976) System of
      !>          Astronomical Constants," Astron. Astrophys. 73, 282-284,
      !>          1979.
      !>
      !>     [2]  E. M. Standish, Jr., "Orientation of the JPL Ephemerides,
      !>          DE 200/LE 200, to the Dynamical Equinox of J2000," Astron.
      !>          Astrophys. 114, 297-302, 1982.
      !>
      !>     [3]  E. M. Standish, Jr., "Conversion of Ephemeris Coordinates
      !>          from the B1950 System to the J2000 System," JPL IOM
      !>          314.6-581, 24 June 1985.
      !>
      !>     [4]  E. M. Standish, Jr., "The Equinox Offsets of the JPL
      !>          Ephemeris," JPL IOM 314.6-929, 26 February 1988.
      !>
      !>     [5]  J. Lieske, "Expressions for the Precession Quantities Based
      !>          upon the IAU (1976) System of Astronomical Constants,"
      !>          Astron. Astrophys. 58, 1-16, 1977.
      !>
      !>     [6]  L. Bass and R. Cesarone, "Mars Observer Planetary Constants
      !>          and Models," JPL D-3444, November 1990.
      !>
      !>     [7]  P. Kenneth Seidelmann (Ed.), "Explanatory Supplement to the
      !>          Astronomical Almanac," University Science Books, 1992.
      !>
      !>## Author and Institution
      !>
      !>     J. Diaz del Rio    (ODC Space)
      !>     B.V. Semenov       (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 4.4.0, 17-JUN-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Corrected
      !>        argument name in $Brief_I/O section: ROTAB was MATRIX. Added
      !>        $Literature_References.
      !>
      !>-    SPICELIB Version 4.3.0, 24-SEP-2013 (BVS)
      !>
      !>        Updated to do discovery check-in/check-out.
      !>
      !>-    SPICELIB Version 4.2.1, 04-JAN-2002 (EDW)
      !>
      !>        Added DE-143 to header description for IRFROT.
      !>
      !>-    SPICELIB Version 4.2.0, 10-APR-1997 (WLT)
      !>
      !>        A descriptive diagnostic was added to the entry points
      !>        IRFROT and IRFDEF. Before they simply signaled the error
      !>        with no diagnostic.
      !>
      !>-    SPICELIB Version 4.1.0, 14-OCT-1996 (WLT)
      !>
      !>        The number of inertial frames recognized is now stored
      !>        in the include file ninert.inc.
      !>
      !>-    SPICELIB Version 4.0.0, 20-MAY-1996 (WLT)
      !>
      !>        The inertial frame DE-143 was added to the list of recognized
      !>        inertial frames.
      !>
      !>-    SPICELIB Version 3.0.0, 20-MAR-1995 (WLT)
      !>
      !>        The inertial frames DE-140 and DE-142  were added to the
      !>        list of recognized inertial frames.
      !>
      !>-    SPICELIB Version 2.0.0, 30-JUL-1993 (WLT)
      !>
      !>        The transformation from J2000 to B1950 was upgraded
      !>        so that the transformation matrix produced matches
      !>        the matrix given in [1].
      !>
      !>        The frame MARSIAU was added to the list
      !>        of recognized frames. This is the standard mars
      !>        referenced inertial frame used by the Mars Observer
      !>        project.
      !>
      !>        Values for the obliquity of the ecliptic were taken
      !>        from the Explanatory Supplement [7] to the Astronomical
      !>        Almanac (1992) at both the epochs J2000 and B1950 and
      !>        used to define the mean ecliptic and equinox frames
      !>        ECLIPJ2000 and ECLIPB1950.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
      module procedure irfrot
      use pepper_kinds, only: ip, wp
      use pepper_math, only: mxm, mxmt
      use pepper_strings, only: nparsd, nparsi, nthwd, wdcnt
      use pepper_inc_ninert
      implicit none
      integer(ip), parameter :: maxf = ninert
      character(16), parameter :: bases ( maxf ) = [ character(16) :: &
         'J2000',&
         'J2000',&
         'B1950',&
         'B1950',&
         'B1950',&
         'B1950',&
         'B1950',&
         'B1950',&
         'B1950',&
         'B1950',&
         'B1950',&
         'B1950',&
         'FK4',&
         'J2000',&
         'J2000',&
         'J2000',&
         'J2000',&
         'B1950',&
         'J2000',&
         'J2000',&
         'J2000' ]
      character(80), parameter :: defs ( maxf ) = [ character(80) :: &
         '0.0  1',&
         '1152.84248596724 3  -1002.26108439117  2  1153.04066200330  3',&
         '0.525  3',&
         '0.53155  3',&
         '0.4107  3',&
         '0.1359  3',&
         '0.4775  3',&
         '0.5880  3',&
         '0.5529  3',&
         '0.5316  3',&
         '0.5754  3',&
         '0.5247  3',&
         '1177200.0  3  225360.0  1  1016100.0  3',&
         '0.0  3',&
         '0.0  3',&
         '324000.0D0 3 133610.4D0 2 -152348.4D0 3',&
         '84381.448 1',&
         '84404.836 1',&
         '1152.71013777252 3  -1002.25042010533  2  1153.75719544491  3',&
         '1152.72061453864 3  -1002.25052830351  2  1153.74663857521  3',&
         '1153.03919093833, 3, -1002.24822382286, 2, 1153.42900222357, 3' ]
      character(25)         error
      character(16), parameter :: frames ( maxf ) = [ character(16) :: &
         'J2000',&
         'B1950',&
         'FK4',&
         'DE-118',&
         'DE-96',&
         'DE-102',&
         'DE-108',&
         'DE-111',&
         'DE-114',&
         'DE-122',&
         'DE-125',&
         'DE-130',&
         'GALACTIC',&
         'DE-200',&
         'DE-202',&
         'MARSIAU',&
         'ECLIPJ2000',&
         'ECLIPB1950',&
         'DE-140',&
         'DE-142',&
         'DE-143' ]
      character(25)         word
      real(wp)              angle
      real(wp)              radang
      real(wp)              tmpmat ( 3, 3    )
      integer(ip)           axis
      integer(ip)           b
      integer(ip)           i
      integer(ip)           j
      integer(ip)           loc
      integer(ip)           p
      if ( .not. allocated ( ctx%chgirf_cache ) ) allocate ( ctx%chgirf_cache )
      associate ( ready => ctx%chgirf_cache%ready, &
                  trans => ctx%chgirf_cache%trans )
      if ( ctx%failed() ) return

!
!     If it has not been done already, construct the transformation
!     from the root frame to each supported reference frame.
!
!     Begin by constructing the identity matrix (rotating by zero
!     radians about the x-axis). Apply the rotations indicated in
!     the frame definition (from right to left) to get the incremental
!     rotation from the base frame. The final rotation is
!
!        R             = (R           ) (R          )
!         root->frame      base->frame    root->base
!
      if ( .not. ready ) then

         do i = 1, maxf

            call rotate ( ctx, 0.0_wp, 1, trans(1,i) )

            do j = wdcnt ( defs(i) ), 2, -2

               call nthwd  ( defs(i),     j,  word, loc )
               call nparsi ( word,     axis, error,   p )

               call nthwd  ( defs(i),   j-1,  word, loc )
               call nparsd ( word,    angle, error,   p )

               call convrt ( ctx, angle, 'ARCSECONDS', 'RADIANS', radang )

               call rotmat ( ctx, trans(1,i),  radang,  axis,  tmpmat     )
               trans(:,i) = reshape(tmpmat, [9])

            end do

            b = findloc(frames(1:i), bases(i), dim=1)

            call mxm   ( trans(1,i), trans(1,b), tmpmat     )
            trans(:,i) = reshape(tmpmat, [9])

         end do

         ready = .true.

      end if

!
!     If the transformations have been defined, we can proceed with
!     the business at hand: determining the rotation from one frame
!     to another. To get from frame A to frame B, the rotation is
!
!                                     T
!        R     = (R       ) (R       )
!         A->B     root->B    root->A
!
!     If A and B are the same frame, the rotation is just the identity.
!     In theory, computing
!
!                                     T
!        R     = (R       ) (R       )
!         A->A     root->A    root->A
!
!     should work, but why risk roundoff problems?
!
      if ( refa < 1  .or.  refa > maxf ) then

         call ctx%setmsg ( 'A request has been made to obtain the ' &
         //            'transformation from inertial reference ' &
         //            'frame # to inertial reference frame #. ' &
         //            'Unfortunately # is not the id-code of a ' &
         //            'known inertial frame. ' )
         call ctx%errint ( '#', refa )
         call ctx%errint ( '#', refb )
         call ctx%errint ( '#', refa )
         call ctx%sigerr ( 'SPICE(IRFNOTREC)' )

      else if ( refb < 1  .or.  refb > maxf ) then

         call ctx%setmsg ( 'A request has been made to obtain the ' &
         //            'transformation from inertial reference ' &
         //            'frame # to inertial reference frame #. ' &
         //            'Unfortunately # is not the id-code of a ' &
         //            'known inertial frame. ' )
         call ctx%errint ( '#', refa )
         call ctx%errint ( '#', refb )
         call ctx%errint ( '#', refb )
         call ctx%sigerr ( 'SPICE(IRFNOTREC)' )

      else if ( refa == refb ) then
         call rotate ( ctx, 0.0_wp, 1, rotab )

      else
         call mxmt ( trans(1,refb), trans(1,refa), rotab )
      end if

      return
      end associate
      end procedure irfrot

      !>## Detailed Input
      !>
      !>     NAME     is the name of one of the standard inertial
      !>              reference frames supported by IRFROT, or
      !>              'DEFAULT'.
      !>
      !>## Detailed Output
      !>
      !>     INDEX    is the index of the frame specified by NAME.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If NAME is not recognized, INDEX is zero.
      !>
      !>     2)  If no default frame has been specified, INDEX is zero.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     IRFNUM is supplied as a convenience, to allow users to refer to
      !>     the various standard inertial reference frames by name.
      !>
      !>## Examples
      !>
      !>     In the following example, the rotation from DE-118 to FK4 is
      !>     computed without knowing the indices of these frames.
      !>
      !>        CALL IRFNUM ( 'DE-118', A )
      !>        CALL IRFNUM ( 'FK4',    B )
      !>
      !>        CALL IRFROT ( A, B, ROTAB )
      !>
      !>     IRFNUM can be used to rotate vectors into the default frame,
      !>     as illustrated by the following code fragment.
      !>
      !>        CALL IRFNUM ( 'FK4',     A )
      !>        CALL IRFNUM ( 'DEFAULT', B )
      !>
      !>        CALL IRFROT ( A, B, ROTAB                 )
      !>        CALL MXV    (       ROTAB, OLDVEC, NEWVEC )
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     [1]  J. Lieske, "Precession Matrix Based on IAU (1976) System of
      !>          Astronomical Constants," Astron. Astrophys. 73, 282-284,
      !>          1979.
      !>
      !>     [2]  E. M. Standish, Jr., "Orientation of the JPL Ephemerides,
      !>          DE 200/LE 200, to the Dynamical Equinox of J2000," Astron.
      !>          Astrophys. 114, 297-302, 1982.
      !>
      !>     [3]  E. M. Standish, Jr., "Conversion of Ephemeris Coordinates
      !>          from the B1950 System to the J2000 System," JPL IOM
      !>          314.6-581, 24 June 1985.
      !>
      !>     [4]  E. M. Standish, Jr., "The Equinox Offsets of the JPL
      !>          Ephemeris," JPL IOM 314.6-929, 26 February 1988.
      !>
      !>     [5]  J. Lieske, "Expressions for the Precession Quantities Based
      !>          upon the IAU (1976) System of Astronomical Constants,"
      !>          Astron. Astrophys. 58, 1-16, 1977.
      !>
      !>     [6]  L. Bass and R. Cesarone, "Mars Observer Planetary Constants
      !>          and Models," JPL D-3444, November 1990.
      !>
      !>     [7]  P. Kenneth Seidelmann (Ed.), "Explanatory Supplement to the
      !>          Astronomical Almanac," University Science Books, 1992.
      !>
      !>## Author and Institution
      !>
      !>     J. Diaz del Rio    (ODC Space)
      !>     B.V. Semenov       (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 4.4.0, 17-JUN-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added
      !>        $Literature_References.
      !>
      !>-    SPICELIB Version 4.3.0, 24-SEP-2013 (BVS)
      !>
      !>        Updated to treat J2000 as a special case and to not
      !>        CHKIN/CHOUT to increase efficiency.
      !>
      !>-    SPICELIB Version 4.2.1, 04-JAN-2002 (EDW)
      !>
      !>        Added DE-143 to header description for IRFROT.
      !>
      !>-    SPICELIB Version 4.2.0, 10-APR-1997 (WLT)
      !>
      !>        A descriptive diagnostic was added to the entry points
      !>        IRFROT and IRFDEF. Before they simply signaled the error
      !>        with no diagnostic.
      !>
      !>-    SPICELIB Version 4.1.0, 14-OCT-1996 (WLT)
      !>
      !>        The number of inertial frames recognized is now stored
      !>        in the include file ninert.inc.
      !>
      !>-    SPICELIB Version 4.0.0, 20-MAY-1996 (WLT)
      !>
      !>        The inertial frame DE-143 was added to the list of recognized
      !>        inertial frames.
      !>
      !>-    SPICELIB Version 3.0.0, 20-MAR-1995 (WLT)
      !>
      !>        The inertial frames DE-140 and DE-142  were added to the
      !>        list of recognized inertial frames.
      !>
      !>-    SPICELIB Version 2.0.0, 30-JUL-1993 (WLT)
      !>
      !>        The transformation from J2000 to B1950 was upgraded
      !>        so that the transformation matrix produced matches
      !>        the matrix given in [1].
      !>
      !>        The frame MARSIAU was added to the list
      !>        of recognized frames. This is the standard mars
      !>        referenced inertial frame used by the Mars Observer
      !>        project.
      !>
      !>        Values for the obliquity of the ecliptic were taken
      !>        from the Explanatory Supplement [7] to the Astronomical
      !>        Almanac (1992) at both the epochs J2000 and B1950 and
      !>        used to define the mean ecliptic and equinox frames
      !>        ECLIPJ2000 and ECLIPB1950.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
      module procedure irfnum
      use pepper_kinds, only: ip, wp
      use pepper_strings, only: eqstr, esrchc
      use pepper_inc_ninert
      implicit none
      integer(ip), parameter :: maxf = ninert
      character(16), parameter :: bases ( maxf ) = [ character(16) :: &
         'J2000',&
         'J2000',&
         'B1950',&
         'B1950',&
         'B1950',&
         'B1950',&
         'B1950',&
         'B1950',&
         'B1950',&
         'B1950',&
         'B1950',&
         'B1950',&
         'FK4',&
         'J2000',&
         'J2000',&
         'J2000',&
         'J2000',&
         'B1950',&
         'J2000',&
         'J2000',&
         'J2000' ]
      character(80), parameter :: defs ( maxf ) = [ character(80) :: &
         '0.0  1',&
         '1152.84248596724 3  -1002.26108439117  2  1153.04066200330  3',&
         '0.525  3',&
         '0.53155  3',&
         '0.4107  3',&
         '0.1359  3',&
         '0.4775  3',&
         '0.5880  3',&
         '0.5529  3',&
         '0.5316  3',&
         '0.5754  3',&
         '0.5247  3',&
         '1177200.0  3  225360.0  1  1016100.0  3',&
         '0.0  3',&
         '0.0  3',&
         '324000.0D0 3 133610.4D0 2 -152348.4D0 3',&
         '84381.448 1',&
         '84404.836 1',&
         '1152.71013777252 3  -1002.25042010533  2  1153.75719544491  3',&
         '1152.72061453864 3  -1002.25052830351  2  1153.74663857521  3',&
         '1153.03919093833, 3, -1002.24822382286, 2, 1153.42900222357, 3' ]
      character(25)         error
      character(16), parameter :: frames ( maxf ) = [ character(16) :: &
         'J2000',&
         'B1950',&
         'FK4',&
         'DE-118',&
         'DE-96',&
         'DE-102',&
         'DE-108',&
         'DE-111',&
         'DE-114',&
         'DE-122',&
         'DE-125',&
         'DE-130',&
         'GALACTIC',&
         'DE-200',&
         'DE-202',&
         'MARSIAU',&
         'ECLIPJ2000',&
         'ECLIPB1950',&
         'DE-140',&
         'DE-142',&
         'DE-143' ]
      character(25)         word
      real(wp)              angle
      real(wp)              radang
      real(wp)              tmpmat ( 3, 3    )
      integer(ip)           axis
      integer(ip)           b
      integer(ip)           i
      integer(ip)           j
      integer(ip)           loc
      integer(ip)           p
      if ( .not. allocated ( ctx%spice%chgirf_state ) ) allocate ( ctx%spice%chgirf_state )
      associate ( dframe => ctx%spice%chgirf_state%dframe )
      if ( ctx%failed() ) return

      if ( name == 'J2000' .or. name == 'j2000' ) then
         index = 1
         return
      end if

      if ( eqstr ( name, 'DEFAULT' ) ) then
         index = dframe
      else
         index = esrchc ( name, maxf, frames )
      end if

      return
      end associate
      end procedure irfnum

      !>## Detailed Input
      !>
      !>     INDEX    is the index of one of the standard inertial
      !>              reference frames supported by IRFROT.
      !>
      !>## Detailed Output
      !>
      !>     NAME     is the name of the frame specified by INDEX.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If INDEX is not the index of a supported frame, NAME is blank.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     IRFNAM is supplied as a convenience, to allow users to determine
      !>     the names of standard inertial reference frames referred to only
      !>     by index (as in the segment descriptors of a GEF ephemeris file).
      !>
      !>## Examples
      !>
      !>     In the following example, the identity of a rotation from DE-118
      !>     to FK4 is deduced from the indices used to create the rotation.
      !>
      !>        CALL IRFROT ( A, B, ROTAB )
      !>
      !>        CALL IRFNAM ( A, NAME(1) )
      !>        CALL IRFNAM ( B, NAME(2) )
      !>
      !>        WRITE (6,*) 'Rotation from ' // NAME(1) // ' to ' // NAME(2)
      !>
      !>     Note that the name of the default reference frame can only be
      !>     recovered from the number:
      !>
      !>        CALL IRFNUM ( 'DEFAULT', DINDEX        )
      !>        CALL IRFNAM (            DINDEX, DNAME )
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     [1]  J. Lieske, "Precession Matrix Based on IAU (1976) System of
      !>          Astronomical Constants," Astron. Astrophys. 73, 282-284,
      !>          1979.
      !>
      !>     [2]  E. M. Standish, Jr., "Orientation of the JPL Ephemerides,
      !>          DE 200/LE 200, to the Dynamical Equinox of J2000," Astron.
      !>          Astrophys. 114, 297-302, 1982.
      !>
      !>     [3]  E. M. Standish, Jr., "Conversion of Ephemeris Coordinates
      !>          from the B1950 System to the J2000 System," JPL IOM
      !>          314.6-581, 24 June 1985.
      !>
      !>     [4]  E. M. Standish, Jr., "The Equinox Offsets of the JPL
      !>          Ephemeris," JPL IOM 314.6-929, 26 February 1988.
      !>
      !>     [5]  J. Lieske, "Expressions for the Precession Quantities Based
      !>          upon the IAU (1976) System of Astronomical Constants,"
      !>          Astron. Astrophys. 58, 1-16, 1977.
      !>
      !>     [6]  L. Bass and R. Cesarone, "Mars Observer Planetary Constants
      !>          and Models," JPL D-3444, November 1990.
      !>
      !>     [7]  P. Kenneth Seidelmann (Ed.), "Explanatory Supplement to the
      !>          Astronomical Almanac," University Science Books, 1992.
      !>
      !>## Author and Institution
      !>
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 4.3.0, 17-JUN-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added
      !>        $Literature_References.
      !>
      !>-    SPICELIB Version 4.2.1, 04-JAN-2002 (EDW)
      !>
      !>        Added DE-143 to header description for IRFROT.
      !>
      !>-    SPICELIB Version 4.2.0, 10-APR-1997 (WLT)
      !>
      !>        A descriptive diagnostic was added to the entry points
      !>        IRFROT and IRFDEF. Before they simply signaled the error
      !>        with no diagnostic.
      !>
      !>-    SPICELIB Version 4.1.0, 14-OCT-1996 (WLT)
      !>
      !>        The number of inertial frames recognized is now stored
      !>        in the include file ninert.inc.
      !>
      !>-    SPICELIB Version 4.0.0, 20-MAY-1996 (WLT)
      !>
      !>        The inertial frame DE-143 was added to the list of recognized
      !>        inertial frames.
      !>
      !>-    SPICELIB Version 3.0.0, 20-MAR-1995 (WLT)
      !>
      !>        The inertial frames DE-140 and DE-142  were added to the
      !>        list of recognized inertial frames.
      !>
      !>-    SPICELIB Version 2.0.0, 30-JUL-1993 (WLT)
      !>
      !>        The transformation from J2000 to B1950 was upgraded
      !>        so that the transformation matrix produced matches
      !>        the matrix given in [1].
      !>
      !>        The frame MARSIAU was added to the list
      !>        of recognized frames. This is the standard mars
      !>        referenced inertial frame used by the Mars Observer
      !>        project.
      !>
      !>        Values for the obliquity of the ecliptic were taken
      !>        from the Explanatory Supplement [7] to the Astronomical
      !>        Almanac (1992) at both the epochs J2000 and B1950 and
      !>        used to define the mean ecliptic and equinox frames
      !>        ECLIPJ2000 and ECLIPB1950.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
      module procedure irfnam
      use pepper_kinds, only: ip, wp
      use pepper_inc_ninert
      implicit none
      integer(ip), parameter :: maxf = ninert
      character(16), parameter :: bases ( maxf ) = [ character(16) :: &
         'J2000',&
         'J2000',&
         'B1950',&
         'B1950',&
         'B1950',&
         'B1950',&
         'B1950',&
         'B1950',&
         'B1950',&
         'B1950',&
         'B1950',&
         'B1950',&
         'FK4',&
         'J2000',&
         'J2000',&
         'J2000',&
         'J2000',&
         'B1950',&
         'J2000',&
         'J2000',&
         'J2000' ]
      character(80), parameter :: defs ( maxf ) = [ character(80) :: &
         '0.0  1',&
         '1152.84248596724 3  -1002.26108439117  2  1153.04066200330  3',&
         '0.525  3',&
         '0.53155  3',&
         '0.4107  3',&
         '0.1359  3',&
         '0.4775  3',&
         '0.5880  3',&
         '0.5529  3',&
         '0.5316  3',&
         '0.5754  3',&
         '0.5247  3',&
         '1177200.0  3  225360.0  1  1016100.0  3',&
         '0.0  3',&
         '0.0  3',&
         '324000.0D0 3 133610.4D0 2 -152348.4D0 3',&
         '84381.448 1',&
         '84404.836 1',&
         '1152.71013777252 3  -1002.25042010533  2  1153.75719544491  3',&
         '1152.72061453864 3  -1002.25052830351  2  1153.74663857521  3',&
         '1153.03919093833, 3, -1002.24822382286, 2, 1153.42900222357, 3' ]
      character(25)         error
      character(16), parameter :: frames ( maxf ) = [ character(16) :: &
         'J2000',&
         'B1950',&
         'FK4',&
         'DE-118',&
         'DE-96',&
         'DE-102',&
         'DE-108',&
         'DE-111',&
         'DE-114',&
         'DE-122',&
         'DE-125',&
         'DE-130',&
         'GALACTIC',&
         'DE-200',&
         'DE-202',&
         'MARSIAU',&
         'ECLIPJ2000',&
         'ECLIPB1950',&
         'DE-140',&
         'DE-142',&
         'DE-143' ]
      character(25)         word
      real(wp)              angle
      real(wp)              radang
      real(wp)              tmpmat ( 3, 3    )
      integer(ip)           axis
      integer(ip)           b
      integer(ip)           i
      integer(ip)           j
      integer(ip)           loc
      integer(ip)           p
      if ( ctx%failed() ) return

      if ( index < 1  .or.  index > maxf ) then
         name = ' '

      else
         name = frames(index)
      end if

      return
      end procedure irfnam

      !>## Detailed Input
      !>
      !>     INDEX    is the index of one of the standard inertial
      !>              reference frames supported by IRFROT.
      !>
      !>## Detailed Output
      !>
      !>     None.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If INDEX is outside the range [1,MAXF], where MAXF is the
      !>         number of supported frames, the error SPICE(IRFNOTREC) is
      !>         signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     IRFDEF allows tools to be written at a relatively high level
      !>     without requiring the reference frame to be tramp coupled or
      !>     placed in global memory.
      !>
      !>## Examples
      !>
      !>     Typically, the calling program will select a default frame
      !>     during initialization,
      !>
      !>        C
      !>        C     Use J2000 for all ephemeris, star data.
      !>        C
      !>              CALL IRFDEF ( 1 )
      !>
      !>     and recover the default frame at lower levels,
      !>
      !>        C
      !>        C     Rotate all vectors into the default frame.
      !>        C
      !>              CALL IRFNUM ( 'DEFAULT', REFD   )
      !>
      !>              DO I = 1, NVEC
      !>                 CALL IRFROT ( REFIN, REFD, ROT           )
      !>                 CALL MXV                   ROT, VEC, VEC )
      !>              END DO
      !>
      !>     Note that many utilities accept 'DEFAULT' as the name of
      !>     an inertial reference frame,
      !>
      !>        CALL SPKEZ ( TARGET, ..., 'DEFAULT', ... )
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     [1]  J. Lieske, "Precession Matrix Based on IAU (1976) System of
      !>          Astronomical Constants," Astron. Astrophys. 73, 282-284,
      !>          1979.
      !>
      !>     [2]  E. M. Standish, Jr., "Orientation of the JPL Ephemerides,
      !>          DE 200/LE 200, to the Dynamical Equinox of J2000," Astron.
      !>          Astrophys. 114, 297-302, 1982.
      !>
      !>     [3]  E. M. Standish, Jr., "Conversion of Ephemeris Coordinates
      !>          from the B1950 System to the J2000 System," JPL IOM
      !>          314.6-581, 24 June 1985.
      !>
      !>     [4]  E. M. Standish, Jr., "The Equinox Offsets of the JPL
      !>          Ephemeris," JPL IOM 314.6-929, 26 February 1988.
      !>
      !>     [5]  J. Lieske, "Expressions for the Precession Quantities Based
      !>          upon the IAU (1976) System of Astronomical Constants,"
      !>          Astron. Astrophys. 58, 1-16, 1977.
      !>
      !>     [6]  L. Bass and R. Cesarone, "Mars Observer Planetary Constants
      !>          and Models," JPL D-3444, November 1990.
      !>
      !>     [7]  P. Kenneth Seidelmann (Ed.), "Explanatory Supplement to the
      !>          Astronomical Almanac," University Science Books, 1992.
      !>
      !>## Author and Institution
      !>
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 4.3.0, 17-JUN-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added
      !>        $Literature_References.
      !>
      !>-    SPICELIB Version 4.2.1, 04-JAN-2002 (EDW)
      !>
      !>        Added DE-143 to header description for IRFROT.
      !>
      !>-    SPICELIB Version 4.2.0, 10-APR-1997 (WLT)
      !>
      !>        A descriptive diagnostic was added to the entry points
      !>        IRFROT and IRFDEF. Before they simply signaled the error
      !>        with no diagnostic.
      !>
      !>-    SPICELIB Version 4.1.0, 14-OCT-1996 (WLT)
      !>
      !>        The number of inertial frames recognized is now stored
      !>        in the include file ninert.inc.
      !>
      !>-    SPICELIB Version 4.0.0, 20-MAY-1996 (WLT)
      !>
      !>        The inertial frame DE-143 was added to the list of recognized
      !>        inertial frames.
      !>
      !>-    SPICELIB Version 3.0.0, 20-MAR-1995 (WLT)
      !>
      !>        The inertial frames DE-140 and DE-142  were added to the
      !>        list of recognized inertial frames.
      !>
      !>-    SPICELIB Version 2.0.0, 30-JUL-1993 (WLT)
      !>
      !>        The transformation from J2000 to B1950 was upgraded
      !>        so that the transformation matrix produced matches
      !>        the matrix given in [1].
      !>
      !>        The frame MARSIAU was added to the list
      !>        of recognized frames. This is the standard mars
      !>        referenced inertial frame used by the Mars Observer
      !>        project.
      !>
      !>        Values for the obliquity of the ecliptic were taken
      !>        from the Explanatory Supplement [7] to the Astronomical
      !>        Almanac (1992) at both the epochs J2000 and B1950 and
      !>        used to define the mean ecliptic and equinox frames
      !>        ECLIPJ2000 and ECLIPB1950.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
      module procedure irfdef
      use pepper_kinds, only: ip, wp
      use pepper_inc_ninert
      implicit none
      integer(ip), parameter :: maxf = ninert
      character(16), parameter :: bases ( maxf ) = [ character(16) :: &
         'J2000',&
         'J2000',&
         'B1950',&
         'B1950',&
         'B1950',&
         'B1950',&
         'B1950',&
         'B1950',&
         'B1950',&
         'B1950',&
         'B1950',&
         'B1950',&
         'FK4',&
         'J2000',&
         'J2000',&
         'J2000',&
         'J2000',&
         'B1950',&
         'J2000',&
         'J2000',&
         'J2000' ]
      character(80), parameter :: defs ( maxf ) = [ character(80) :: &
         '0.0  1',&
         '1152.84248596724 3  -1002.26108439117  2  1153.04066200330  3',&
         '0.525  3',&
         '0.53155  3',&
         '0.4107  3',&
         '0.1359  3',&
         '0.4775  3',&
         '0.5880  3',&
         '0.5529  3',&
         '0.5316  3',&
         '0.5754  3',&
         '0.5247  3',&
         '1177200.0  3  225360.0  1  1016100.0  3',&
         '0.0  3',&
         '0.0  3',&
         '324000.0D0 3 133610.4D0 2 -152348.4D0 3',&
         '84381.448 1',&
         '84404.836 1',&
         '1152.71013777252 3  -1002.25042010533  2  1153.75719544491  3',&
         '1152.72061453864 3  -1002.25052830351  2  1153.74663857521  3',&
         '1153.03919093833, 3, -1002.24822382286, 2, 1153.42900222357, 3' ]
      character(25)         error
      character(16), parameter :: frames ( maxf ) = [ character(16) :: &
         'J2000',&
         'B1950',&
         'FK4',&
         'DE-118',&
         'DE-96',&
         'DE-102',&
         'DE-108',&
         'DE-111',&
         'DE-114',&
         'DE-122',&
         'DE-125',&
         'DE-130',&
         'GALACTIC',&
         'DE-200',&
         'DE-202',&
         'MARSIAU',&
         'ECLIPJ2000',&
         'ECLIPB1950',&
         'DE-140',&
         'DE-142',&
         'DE-143' ]
      character(25)         word
      real(wp)              angle
      real(wp)              radang
      real(wp)              tmpmat ( 3, 3    )
      integer(ip)           axis
      integer(ip)           b
      integer(ip)           i
      integer(ip)           j
      integer(ip)           loc
      integer(ip)           p
      if ( .not. allocated ( ctx%spice%chgirf_state ) ) allocate ( ctx%spice%chgirf_state )
      associate ( dframe => ctx%spice%chgirf_state%dframe )
      if ( ctx%failed() ) return

!
!     There's not much to do, except save the value for later use.
!
      if ( index < 1  .or.  index > maxf ) then

         call ctx%setmsg ( 'The reference frame with id-code # is ' &
         //            'not a recognized inertial reference ' &
         //            'frame. ' )
         call ctx%errint ( '#', index )

         call ctx%sigerr ( 'SPICE(IRFNOTREC)' )

      else
         dframe = index
      end if

      return
      end associate
      end procedure irfdef

      !>## Abstract
      !>
      !>     Return the mean obliquity of the ecliptic, and its time
      !>     derivative, at a specified epoch.
      !>
      !>     SPICE Private routine intended solely for the support of SPICE
      !>     routines.  Users should not call this routine directly due
      !>     to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     ET             is the epoch at which the obliquity of the ecliptic
      !>                    is to be computed.  ET is barycentric dynamical
      !>                    time, expressed as seconds past J2000.
      !>
      !>## Detailed Output
      !>
      !>     MOB            is the mean obliquity of the ecliptic at epoch ET.
      !>                    The mean obliquity of the ecliptic is the
      !>                    inclination of the ecliptic of date to the mean
      !>                    Earth equator of date.  Output units are radians.
      !>
      !>     DMOB           is the time derivative of MOB at ET, expressed in
      !>                    radians per second.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The expression for mean is obliquity is
      !>
      !>                          ''        ''            ''         2
      !>        MOBLIQ   =   84381 .448 - 46 .8150 * T - 0 .00059 * T
      !>
      !>                      ''          3
      !>                   + 0 .001813 * T
      !>
      !>     where T indicates Julian centuries past J2000.  This is from
      !>     equation 5-153 of reference [2].
      !>
      !>## Examples
      !>
      !>     See the routine ENUTAT for an example of usage.
      !>
      !>## Restrictions
      !>
      !>     1)  This is a preliminary version of the routine.
      !>
      !>## Literature References
      !>
      !>     [1] "Explanatory Supplement to the Astronomical Almanac"
      !>          edited by P. Kenneth Seidelmann. University Science
      !>          Books, 20 Edgehill Road, Mill Valley, CA 94941 (1992)
      !>
      !>     [2] "Section 5, Geocentric Space-Fixed Position, Velocity, and
      !>         Acceleration Vectors of Tracking Station" by T. D. Moyer.
      !>         Draft of JPL Publication documenting the JPL navigation
      !>         program "Regres."
      !>
      !>## Author and Institution
      !>
      !>     W.L. Taber         (JPL)
      !>     N.J. Bachman       (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1 02-OCT-2021 (NJB)
      !>
      !>        Corrected typo in comments.
      !>
      !>-    SPICELIB Version 1.0.0 18-JUL-1997 (WLT)
      !>
      !>        Adapted Nat's routine to private version making output
      !>        rate be radians/sec.
      !>
      !>-    Beta Version 1.0.0, 29-SEP-1996 (NJB)
      module procedure zzmobliq
      use pepper_kinds, only: wp
      use pepper_constants, only: jyear, rpd
      implicit none
      real(wp), parameter :: c0 = 84381.448_wp
      real(wp), parameter :: c1 = -46.8150_wp
      real(wp), parameter :: c2 = -0.00059_wp
      real(wp), parameter :: c3 = 0.001813_wp
      real(wp) :: t
      if ( .not. allocated ( ctx%zzmobliq_state ) ) allocate ( ctx%zzmobliq_state )
      associate ( rad => ctx%zzmobliq_state%rad, &
                  year => ctx%zzmobliq_state%year, &
                  persec => ctx%zzmobliq_state%persec, &
                  first => ctx%zzmobliq_state%first )
      if ( first ) then

         first  = .false.
         year   = jyear
         rad    = rpd
         persec = 1.0_wp / ( year * 100.0_wp )

      end if

!
!     Convert the input epoch to Julian centuries past J2000:
!
      t  =  ( et / year )  / 100.0_wp

!
!     Compute the obliquity at epoch.  The polynomial yields arcseconds;
!     convert the units to radians.
!
      mob  = ( rad / 3.6e3_wp ) * (  c0 + t*( c1   + t*( c2 + t*c3 ) )  )

      dmob = ( rad / 3.6e3_wp ) * (  c1 + t*( 2*c2 + t*3*c3 )  ) * persec
      end associate
      end procedure zzmobliq

      !>## Detailed Input
      !>
      !>     ET         is the epoch for which nutation angles are being
      !>                requested expressed in TDB seconds past the epoch
      !>                of J2000.
      !>
      !>## Detailed Output
      !>
      !>     DVNUT      are the nutation angles and their derivatives.
      !>                Following the notation on page 112 of the
      !>                Explanatory Supplement to the Astronomical
      !>                Almanac we have
      !>
      !>                DVNUT(1) = Psi------nutation in longitude (radians)
      !>                DVNUT(2) = Epsilon--nutation in obliquity (radians)
      !>                DVNUT(3) = dPsi/dt     (radians/second)
      !>                DVNUT(4) = dEpsilon/dt (radians/second)
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine computes the angles required for computing the
      !>     transformation from the mean of date frame for the earth
      !>     to the true of date frame of the earth.
      !>
      !>## Examples
      !>
      !>     None.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     Explanatory Supplement to the Astronomical Almanac edited
      !>     by P. Kenneth Siedelmann. (1992) (University Science
      !>     Books, Mill Valley CA) pp. 111-116
      !>
      !>## Author and Institution
      !>
      !>     W.L. Taber      (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 02-OCT-2021 (NJB)
      !>
      !>        Corrected typos in comments. Reordered header sections.
      !>
      !>-    SPICELIB Version 1.0.0, 15-JUL-1997 (WLT)
      !>
      !>        This routine was adapted from a routine provided by
      !>        Skip Newhall.  Skip's notes indicate that he obtained this
      !>        from Jay Lieske and Mylse Standish.  The actual notes
      !>        from the original routine WAHR are given here.
      !>
      !>           Lieske 3/91.  NUTATION in the IAU J2000 system.  Univac
      !>           version obtained from Myles Standish, (subroutine WAHR)
      !>           who had obtained it from USNO.  Re-ordered terms to match
      !>           Astronomical Almanac 1984 table S23-S25 and corrected
      !>           the rate for dPsi in the 0 0 2 -2 2 term.  Eliminated
      !>           the equivalences, common block and added necessary SAVEs.
      !>           Corrected the fundamental angles (L, L', F, D, Node) to
      !>           match Almanac.
      !>
      !>        In the current routine the various angles L, L', F, D, and
      !>        Node (MG) are computed using the actual values given
      !>        in the Explanatory Supplement.
      !>
      !>        Note that there is an error in the Explanatory supplement
      !>        for the Node term.  The Explanatory Supplement (page 114) has
      !>
      !>          OMEGA = 135 degrees 2 minutes 40.280 seconds
      !>                +  etc.
      !>
      !>        The correct formulation should be:
      !>
      !>          OMEGA = 125 degrees 2 minutes 40.280 seconds
      !>                +  etc.
      !>
      !>        This is the value used in this routine.  The verification of
      !>        this error is courtesy of Myles Standish.
      !>
      !>     SPICELIB Functions
      !>
      !>     Parameters
      !>
      !>     NTERM is the number of SIN and COSINE terms used in the
      !>     computation of Delta Psi and Delta epsilon
      !>
      !>     The parameters below stand for
      !>
      !>        revolutions
      !>        degrees
      !>        minutes
      !>        seconds
      !>        julian century
      !>        julian century ** 2
      !>        julian century ** 3
      !>
      !>     These parameters are needed for converting the quantities
      !>     on page 114 of the Explanatory supplement from revolutions,
      !>     degrees, minutes and seconds / century, century**2 and century**3
      !>     to degrees, degrees/day, degrees/(0.0001 days)**2 and
      !>     degrees/(0.0001 days)**3.
      !>
      !>     The next set of parameters is an enumeration of the various
      !>     angles needed in the computation of nutations.
      !>
      !>     Local Variables.
      !>
      !>     PEPPER: the table of the series (one term, i.e. one column, per
      !>     line; SPICELIB set it with DATA statements).
      !>
      !>     Below are the coefficients for the various periods of the
      !>     nutation model.  There does not appear to be any particular reason
      !>     for the ordering selected.  The n'th row corresponds to the n'th
      !>     period listed above each data statement.
      !>
      !>>> Periods: 6798.4, 3399.2, 1305.5, 1095.2, 1615.7, 3232.9, 6786.3,
      !>             943.2,  182.6,  365.3,  121.7,  365.2,  177.8,  205.9,
      !>             173.3,  182.6,  386.0,   91.3,  346.6
      !>
      !>     Periods: 199.8, 346.6, 212.3, 119.6, 411.8, 131.7, 169.0, 329.8,
      !>              409.2, 388.3, 117.5,  13.7,  27.6,  13.6,   9.1,  31.8,
      !>               27.1,  14.8,  27.7
      !>
      !>     Periods: 27.4, 9.6,  9.1,  7.1, 13.8, 23.9, 6.9, 13.6, 27.0, 32.0,
      !>              31.7, 9.5, 34.8, 13.2, 14.2,  5.6, 9.6, 12.8, 14.8
      !>
      !>     Periods: 7.1, 23.9, 14.7, 29.8, 6.9, 15.4, 26.9, 29.5, 25.6, 9.1,
      !>              9.4,  9.8, 13.7,  5.5, 7.2,  8.9, 32.6, 13.8, 27.8
      !>
      !>      Periods: 9.2,  9.3, 27.3, 10.1, 14.6,  5.8, 15.9, 22.5,  5.6,
      !>               7.3,  9.1, 29.3, 12.8,  4.7,  9.6, 12.7,  8.7, 23.8,
      !>              13.1
      !>
      !>     Periods: 35.0, 13.6, 25.4, 14.2, 9.5, 14.2, 34.7, 32.8, 7.1, 4.8,
      !>              27.3
      module procedure zzwahr
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: pi, spd, twopi
      implicit none
      integer(ip), parameter :: nterm = 106
      real(wp), parameter :: zero = 0.0_wp
      real(wp), parameter :: rev = 360.0_wp
      real(wp), parameter :: deg = 1.0_wp
      real(wp), parameter :: min = 1.0_wp / 60.0_wp
      real(wp), parameter :: sec = 1.0_wp / 3600.0_wp
      real(wp), parameter :: cent1 = 36525.0_wp
      real(wp), parameter :: cent2 = cent1*cent1*1.0e-8_wp
      real(wp), parameter :: cent3 = cent1*cent2*1.0e-4_wp
      integer(ip), parameter :: l = 1
      integer(ip), parameter :: lp = l      + 1
      integer(ip), parameter :: f = lp     + 1
      integer(ip), parameter :: d = f      + 1
      integer(ip), parameter :: mg = d      + 1
      integer(ip), parameter :: matrix(9,nterm) = reshape ( [ &
              0,      0,      0,      0,      1, -171996,  -1742,  92025,     89,&
              0,      0,      0,      0,      2,   2062,      2,   -895,      5,&
             -2,      0,      2,      0,      1,     46,      0,    -24,      0,&
              2,      0,     -2,      0,      0,     11,      0,      0,      0,&
             -2,      0,      2,      0,      2,     -3,      0,      1,      0,&
              1,     -1,      0,     -1,      0,     -3,      0,      0,      0,&
              0,     -2,      2,     -2,      1,     -2,      0,      1,      0,&
              2,      0,     -2,      0,      1,      1,      0,      0,      0,&
              0,      0,      2,     -2,      2, -13187,    -16,   5736,    -31,&
              0,      1,      0,      0,      0,   1426,    -34,     54,     -1,&
              0,      1,      2,     -2,      2,   -517,     12,    224,     -6,&
              0,     -1,      2,     -2,      2,    217,     -5,    -95,      3,&
              0,      0,      2,     -2,      1,    129,      1,    -70,      0,&
              2,      0,      0,     -2,      0,     48,      0,      1,      0,&
              0,      0,      2,     -2,      0,    -22,      0,      0,      0,&
              0,      2,      0,      0,      0,     17,     -1,      0,      0,&
              0,      1,      0,      0,      1,    -15,      0,      9,      0,&
              0,      2,      2,     -2,      2,    -16,      1,      7,      0,&
              0,     -1,      0,      0,      1,    -12,      0,      6,      0,&
             -2,      0,      0,      2,      1,     -6,      0,      3,      0,&
              0,     -1,      2,     -2,      1,     -5,      0,      3,      0,&
              2,      0,      0,     -2,      1,      4,      0,     -2,      0,&
              0,      1,      2,     -2,      1,      4,      0,     -2,      0,&
              1,      0,      0,     -1,      0,     -4,      0,      0,      0,&
              2,      1,      0,     -2,      0,      1,      0,      0,      0,&
              0,      0,     -2,      2,      1,      1,      0,      0,      0,&
              0,      1,     -2,      2,      0,     -1,      0,      0,      0,&
              0,      1,      0,      0,      2,      1,      0,      0,      0,&
             -1,      0,      0,      1,      1,      1,      0,      0,      0,&
              0,      1,      2,     -2,      0,     -1,      0,      0,      0,&
              0,      0,      2,      0,      2,  -2274,     -2,    977,     -5,&
              1,      0,      0,      0,      0,    712,      1,     -7,      0,&
              0,      0,      2,      0,      1,   -386,     -4,    200,      0,&
              1,      0,      2,      0,      2,   -301,      0,    129,     -1,&
              1,      0,      0,     -2,      0,   -158,      0,     -1,      0,&
             -1,      0,      2,      0,      2,    123,      0,    -53,      0,&
              0,      0,      0,      2,      0,     63,      0,     -2,      0,&
              1,      0,      0,      0,      1,     63,      1,    -33,      0,&
             -1,      0,      0,      0,      1,    -58,     -1,     32,      0,&
             -1,      0,      2,      2,      2,    -59,      0,     26,      0,&
              1,      0,      2,      0,      1,    -51,      0,     27,      0,&
              0,      0,      2,      2,      2,    -38,      0,     16,      0,&
              2,      0,      0,      0,      0,     29,      0,     -1,      0,&
              1,      0,      2,     -2,      2,     29,      0,    -12,      0,&
              2,      0,      2,      0,      2,    -31,      0,     13,      0,&
              0,      0,      2,      0,      0,     26,      0,     -1,      0,&
             -1,      0,      2,      0,      1,     21,      0,    -10,      0,&
             -1,      0,      0,      2,      1,     16,      0,     -8,      0,&
              1,      0,      0,     -2,      1,    -13,      0,      7,      0,&
             -1,      0,      2,      2,      1,    -10,      0,      5,      0,&
              1,      1,      0,     -2,      0,     -7,      0,      0,      0,&
              0,      1,      2,      0,      2,      7,      0,     -3,      0,&
              0,     -1,      2,      0,      2,     -7,      0,      3,      0,&
              1,      0,      2,      2,      2,     -8,      0,      3,      0,&
              1,      0,      0,      2,      0,      6,      0,      0,      0,&
              2,      0,      2,     -2,      2,      6,      0,     -3,      0,&
              0,      0,      0,      2,      1,     -6,      0,      3,      0,&
              0,      0,      2,      2,      1,     -7,      0,      3,      0,&
              1,      0,      2,     -2,      1,      6,      0,     -3,      0,&
              0,      0,      0,     -2,      1,     -5,      0,      3,      0,&
              1,     -1,      0,      0,      0,      5,      0,      0,      0,&
              2,      0,      2,      0,      1,     -5,      0,      3,      0,&
              0,      1,      0,     -2,      0,     -4,      0,      0,      0,&
              1,      0,     -2,      0,      0,      4,      0,      0,      0,&
              0,      0,      0,      1,      0,     -4,      0,      0,      0,&
              1,      1,      0,      0,      0,     -3,      0,      0,      0,&
              1,      0,      2,      0,      0,      3,      0,      0,      0,&
              1,     -1,      2,      0,      2,     -3,      0,      1,      0,&
             -1,     -1,      2,      2,      2,     -3,      0,      1,      0,&
             -2,      0,      0,      0,      1,     -2,      0,      1,      0,&
              3,      0,      2,      0,      2,     -3,      0,      1,      0,&
              0,     -1,      2,      2,      2,     -3,      0,      1,      0,&
              1,      1,      2,      0,      2,      2,      0,     -1,      0,&
             -1,      0,      2,     -2,      1,     -2,      0,      1,      0,&
              2,      0,      0,      0,      1,      2,      0,     -1,      0,&
              1,      0,      0,      0,      2,     -2,      0,      1,      0,&
              3,      0,      0,      0,      0,      2,      0,      0,      0,&
              0,      0,      2,      1,      2,      2,      0,     -1,      0,&
             -1,      0,      0,      0,      2,      1,      0,     -1,      0,&
              1,      0,      0,     -4,      0,     -1,      0,      0,      0,&
             -2,      0,      2,      2,      2,      1,      0,     -1,      0,&
             -1,      0,      2,      4,      2,     -2,      0,      1,      0,&
              2,      0,      0,     -4,      0,     -1,      0,      0,      0,&
              1,      1,      2,     -2,      2,      1,      0,     -1,      0,&
              1,      0,      2,      2,      1,     -1,      0,      1,      0,&
             -2,      0,      2,      4,      2,     -1,      0,      1,      0,&
             -1,      0,      4,      0,      2,      1,      0,      0,      0,&
              1,     -1,      0,     -2,      0,      1,      0,      0,      0,&
              2,      0,      2,     -2,      1,      1,      0,     -1,      0,&
              2,      0,      2,      2,      2,     -1,      0,      0,      0,&
              1,      0,      0,      2,      1,     -1,      0,      0,      0,&
              0,      0,      4,     -2,      2,      1,      0,      0,      0,&
              3,      0,      2,     -2,      2,      1,      0,      0,      0,&
              1,      0,      2,     -2,      0,     -1,      0,      0,      0,&
              0,      1,      2,      0,      1,      1,      0,      0,      0,&
             -1,     -1,      0,      2,      1,      1,      0,      0,      0,&
              0,      0,     -2,      0,      1,     -1,      0,      0,      0,&
              0,      0,      2,     -1,      2,     -1,      0,      0,      0,&
              0,      1,      0,      2,      0,     -1,      0,      0,      0,&
              1,      0,     -2,     -2,      0,     -1,      0,      0,      0,&
              0,     -1,      2,      0,      1,     -1,      0,      0,      0,&
              1,      1,      0,     -2,      1,     -1,      0,      0,      0,&
              1,      0,     -2,      2,      0,     -1,      0,      0,      0,&
              2,      0,      0,      2,      0,      1,      0,      0,      0,&
              0,      0,      2,      4,      2,     -1,      0,      0,      0,&
              0,      1,      0,      1,      0,      1,      0,      0,      0 ], &
         [ 9, nterm ] )
      real(wp) :: rasec
      real(wp) :: t
      real(wp) :: dddj
      real(wp) :: cl
      integer(ip) :: j
      real(wp) :: dd
      real(wp) :: cosang
      real(wp) :: ce
      integer(ip) :: i
      real(wp) :: sinang
      real(wp) :: dpi
      real(wp) :: dj
      real(wp) :: arg
      real(wp) :: argrt
      real(wp) :: dd2
      if ( .not. allocated ( ctx%zzwahr_state ) ) allocate ( ctx%zzwahr_state )
      associate ( angle => ctx%zzwahr_state%angle, &
                  angrt => ctx%zzwahr_state%angrt, &
                  dtwopi => ctx%zzwahr_state%dtwopi, &
                  factr => ctx%zzwahr_state%factr, &
                  oneday => ctx%zzwahr_state%oneday, &
                  radian => ctx%zzwahr_state%radian, &
                  d0 => ctx%zzwahr_state%d0, &
                  d1 => ctx%zzwahr_state%d1, &
                  d2 => ctx%zzwahr_state%d2, &
                  d3 => ctx%zzwahr_state%d3, &
                  f0 => ctx%zzwahr_state%f0, &
                  f1 => ctx%zzwahr_state%f1, &
                  f2 => ctx%zzwahr_state%f2, &
                  f3 => ctx%zzwahr_state%f3, &
                  l0 => ctx%zzwahr_state%l0, &
                  l1 => ctx%zzwahr_state%l1, &
                  l2 => ctx%zzwahr_state%l2, &
                  l3 => ctx%zzwahr_state%l3, &
                  lp0 => ctx%zzwahr_state%lp0, &
                  lp1 => ctx%zzwahr_state%lp1, &
                  lp2 => ctx%zzwahr_state%lp2, &
                  lp3 => ctx%zzwahr_state%lp3, &
                  mg0 => ctx%zzwahr_state%mg0, &
                  mg1 => ctx%zzwahr_state%mg1, &
                  mg2 => ctx%zzwahr_state%mg2, &
                  mg3 => ctx%zzwahr_state%mg3, &
                  first => ctx%zzwahr_state%first )
      if (first) then

         first  = .false.
         dpi    = pi
         dtwopi = twopi
         radian =  180.0_wp / dpi
         rasec  = 3600.0_wp * radian
         factr  =    1.0e4_wp * rasec
         oneday = spd

!
!        The following values are direct conversions to degrees from
!        page 114 of the Explanatory Supplement to the Astronomical
!        Almanac.
!
!        L0 through L3 are the coefficients for l---the mean longitude
!        of the Moon minus the mean longitude of the Moon's perigee.
!        Units for the various terms:
!
!           L0      degrees
!           L1      degrees/day
!           L2      degrees/(0.0001 days)**2
!           L3      degrees/(0.0001 days)**3
!
         l0 =                134.0_wp*deg + 57.0_wp*min + 46.733_wp*sec
         l1 =(1325.0_wp*rev + 198.0_wp*deg + 52.0_wp*min +  2.633_wp*sec) &
                             / cent1
         l2 =   31.310_wp*sec / cent2
         l3 =    0.064_wp*sec / cent3

!
!        LP0 through LP3 are the coefficients for l'---the mean
!        longitude of the Sun minus the mean longitude of the Sun's
!        perigee. Units for the various terms:
!
!           LP0      degrees
!           LP1      degrees/day
!           LP2      degrees/(0.0001 days)**2
!           LP3      degrees/(0.0001 days)**3
!
         lp0 =               357.0_wp*deg + 31.0_wp*min + 39.804_wp*sec
         lp1 = (99.0_wp*rev + 359.0_wp*deg +  3.0_wp*min +  1.224_wp*sec) &
                              / cent1
         lp2 =   -0.577_wp*sec / cent2
         lp3 =   -0.012_wp*sec / cent3

!
!        F0 through F3 are the coefficients for F---the mean longitude
!        of the Moon minus the mean longitude of the Moon's node. Units
!        for the various terms:
!
!           F0      degrees
!           F1      degrees/day
!           F2      degrees/(0.0001 days)**2
!           F3      degrees/(0.0001 days)**3
!
         f0 =                 93.0_wp*deg + 16.0_wp*min + 18.877_wp*sec
         f1 = (1342.0_wp*rev + 82.0_wp*deg +  1.0_wp*min +  3.137_wp*sec) &
                              / cent1
         f2 =   -13.257_wp*sec / cent2
         f3 =     0.011_wp*sec / cent3

!
!        D0 through D3 are the coefficients for D---the mean longitude
!        of the Moon minus the mean longitude of the Sun. Units
!        for the various terms:
!
!           D0      degrees
!           D1      degrees/day
!           D2      degrees/(0.0001 days)**2
!           D3      degrees/(0.0001 days)**3
!
         d0 =                297.0_wp*deg + 51.0_wp*min +  1.307_wp*sec
         d1 =(1236.0_wp*rev + 307.0_wp*deg +  6.0_wp*min + 41.328_wp*sec) &
                             / cent1
         d2 =   -6.891_wp*sec / cent2
         d3 =    0.019_wp*sec / cent3

!
!        MG0 through MG3 are the coefficients for Omega---the longitude
!        of the mean ascending node of the lunar orbit on the ecliptic
!        measured from the mean equinox of date.  NOTE: The constant
!        term MG0 is correct.  The value
!               o
!            135 02' 40".280
!
!        given in the Explanatory Supplement page 114 has a typo.  The
!        correct value is the one used here:
!
!               o
!            125 02' 40".280
!
!           MG0      degrees
!           MG1      degrees/day
!           MG2      degrees/(0.0001 days)**2
!           MG3      degrees/(0.0001 days)**3
!
         mg0 =               125.0_wp*deg + 2.0_wp*min + 40.280_wp*sec
         mg1 =-( 5.0_wp*rev + 134.0_wp*deg + 8.0_wp*min + 10.539_wp*sec) &
                              / cent1
         mg2 =    7.455_wp*sec / cent2
         mg3 =    0.008_wp*sec / cent3

      end if

!
!     Compute all of the various time components.  DJ is the delta
!     in the Julian date from the J2000 epoch.
!
      dj   = et/oneday
      dd   = dj/1.0e4_wp
      dddj = dd/1.0e4_wp
      dd2  = dd*dd
      t    = dj/365250.0_wp

!
!     Now compute all of the various angles and their rates
!     at the current epoch
!
      angle(l)  = l0   +  dj*l1   +  (l2  + dd*l3 )*dd2
      angle(lp) = lp0  +  dj*lp1  +  (lp2 + dd*lp3)*dd2
      angle(f)  = f0   +  dj*f1   +  (f2  + dd*f3 )*dd2
      angle(d)  = d0   +  dj*d1   +  (d2  + dd*d3 )*dd2
      angle(mg) = mg0  +  dj*mg1  +  (mg2 + dd*mg3)*dd2

      angrt(l ) = l1   +  dddj * (2.0_wp*l2  + 3.0_wp*dd*l3 )
      angrt(lp) = lp1  +  dddj * (2.0_wp*lp2 + 3.0_wp*dd*lp3)
      angrt(f ) = f1   +  dddj * (2.0_wp*f2  + 3.0_wp*dd*f3 )
      angrt(d ) = d1   +  dddj * (2.0_wp*d2  + 3.0_wp*dd*d3 )
      angrt(mg) = mg1  +  dddj * (2.0_wp*mg2 + 3.0_wp*dd*mg3)

!
!     Wrap all of the angles and rates to range from 0 to 360, then
!     convert to radians.
!
      do j = 1,5
         angle(j) = dmod(angle(j), 360.0_wp)
         angrt(j) = dmod(angrt(j), 360.0_wp)

         angle(j) = angle(j)/radian
         angrt(j) = angrt(j)/radian
      end do
!
!     Zero out the components of the nutation array
!
      do j = 1,4
         dvnut(j) = zero
      end do
!
!     Now we accumulate the various terms of Delta Psi and Delta
!     epsilon as expressed on page 115 of the Green Book
!     (Explanatory Supplement to the Astronomical Almanac).
!
      do i = 1,nterm

         arg   = zero
         argrt = zero

         do j = 1,5
            if ( matrix(j,i) /= 0 ) then
               arg   = arg   + matrix(j,i)*angle(j)
               argrt = argrt + matrix(j,i)*angrt(j)
               arg   = mod(arg, dtwopi)
            end if
         end do

         cl = matrix(6,i)

         if ( matrix(7,i) /= 0 ) then
            cl = cl + matrix(7,i)*t
         end if

         ce = matrix(8,i)

         if ( matrix(9,i) /= 0 ) then
            ce = ce + matrix(9,i)*t
         end if

         cosang   = dcos(arg)
         sinang   = dsin(arg)

         dvnut(1) = dvnut(1) + cl*sinang/factr
         dvnut(2) = dvnut(2) + ce*cosang/factr
         dvnut(3) = dvnut(3) + cl*cosang*argrt/factr
         dvnut(4) = dvnut(4) - ce*sinang*argrt/factr

      end do

!
!     Finally convert DVNUT(3) and DVNUT(4) to radians/second
!
      dvnut(3) = dvnut(3)/oneday
      dvnut(4) = dvnut(4)/oneday

      return
      end associate
      end procedure zzwahr

      !>## Abstract
      !>
      !>     SPICE Private routine intended solely for the support of SPICE
      !>     routines.  Users should not call this routine directly due
      !>     to the volatile nature of this routine.
      !>
      !>     Compute the state transformation matrix implementing the IAU 1980
      !>     nutation model.
      !>
      !>## Detailed Input
      !>
      !>     ET             is an epoch, expressed as seconds past J2000 TDB.
      !>
      !>## Detailed Output
      !>
      !>     NUTXF          is a state transformation matrix that maps states
      !>                    from the earth mean equator and equinox of date
      !>                    frame (based on the 1976 IAU precession model) to
      !>                    the earth true equator and equinox frame of date
      !>                    (based on the 1980 IAU nutation model).
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     See the private SPICELIB routine ZZWAHR for a discussion
      !>     of the implementation of the 1980 IAU nutation model.
      !>
      !>     See the private SPICELIB routine ZZMOBLIQ for a discussion
      !>     of the implementation of the 1980 IAU earth mean obliquity
      !>     of date model.
      !>
      !>## Examples
      !>
      !>     See ZZDYNFRM.
      !>
      !>## Restrictions
      !>
      !>     1) This is a SPICE private routine; the routine is subject
      !>        to change without notice.  User applications should not
      !>        call this routine.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman    (JPL)
      !>
      !>## Literature References
      !>
      !>     [1] "Explanatory Supplement to the Astronomical Almanac"
      !>          edited by P. Kenneth Seidelmann. University Science
      !>          Books, 20 Edgehill Road, Mill Valley, CA 94941 (1992)
      !>
      !>     [2] "Section 5, Geocentric Space-Fixed Position, Velocity, and
      !>         Acceleration Vectors of Tracking Station" by T. D. Moyer.
      !>         Draft of JPL Publication documenting the JPL navigation
      !>         program "Regres."
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.0, 18-DEC-2004 (NJB)
      !>
      !>     SPICELIB functions
      !>
      !>     Local variables
      module procedure zzenut80
      use pepper_kinds, only: wp
      implicit none
      real(wp)              dmob
      real(wp)              dvnut  ( 4 )
      real(wp)              eulang ( 6 )
      real(wp)              mob

      if ( ctx%failed() ) return

!
!      Get nutation angles and their rates.  We're expecting
!
!         DVNUT(1) = Psi------nutation in longitude (radians)
!         DVNUT(2) = Epsilon--nutation in obliquity (radians)
!         DVNUT(3) = dPsi/dt     (radians/second)
!         DVNUT(4) = dEpsilon/dt (radians/second)
!
      call zzwahr ( ctx, et, dvnut )

!
!     Get the mean obliquity of date.
!
!     We're expecting the outputs to be as follows:
!
!         MOB      is the mean obliquity of the ecliptic at epoch
!                  ET. The mean obliquity of the ecliptic is the
!                  inclination of the ecliptic of date to the
!                  mean Earth equator of date.  Output units are
!                  radians.
!
!         DMOB     is the time derivative of MOB at ET, expressed
!                        in radians per second.

      call zzmobliq ( ctx, et, mob, dmob )

!
!     The nutation rotation N is defined by
!
!         N = [ -MOB - NUOBL ]  [ -NULON ]   [ MOB ]
!                             1           3         1
!
!     where MOBLIQ is the mean obliquity of the earth's ecliptic
!     at epoch, NUOB is nutation in obliquity at epoch, and
!     NULONG is nutation in longitude at epoch.  Using our
!     variable names, the Euler angle sequence is
!
!        [ -MOB - DVNUT(2) ]  [ -DVNUT(1) ]  [ MOB ]
!                           1              3        1
!
!     The rates corresponding to these angles are:
!
!        -DMOB - DVNUT(4),  -DVNUT(3),  DMOB
!
!     We can use EUL2XF to form the state transformation from
!     the nutation base frame to the nutation frame.
!

      eulang(1) =  -mob  - dvnut(2)
      eulang(2) =        - dvnut(1)
      eulang(3) =   mob
      eulang(4) =  -dmob - dvnut(4)
      eulang(5) =        - dvnut(3)
      eulang(6) =   dmob

      call eul2xf ( ctx, eulang, 1, 3, 1, nutxf )

      return
      end procedure zzenut80

      !>## Abstract
      !>
      !>     SPICE Private routine intended solely for the support of SPICE
      !>     routines.  Users should not call this routine directly due
      !>     to the volatile nature of this routine.
      !>
      !>     Compute the state transformation matrix implementing the IAU 1976
      !>     precession model.
      !>
      !>## Detailed Input
      !>
      !>     ET             is the epoch at which the precession matrix is
      !>                    to be computed.  ET is barycentric dynamical time,
      !>                    expressed as seconds past J2000.
      !>
      !>## Detailed Output
      !>
      !>     PRECXF         is a 6x6 matrix that transforms states from the
      !>                    J2000 frame to the mean equator and equinox frame
      !>                    of the earth at the epoch ET.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     According to reference [2], the precession model used in this
      !>     routine is that used in the JPL navigation program "Regres."
      !>
      !>     The precession matrix is defined using the Euler angles
      !>
      !>        zeta ,   z ,  and theta
      !>            A     A            A
      !>
      !>     Equation (5-147) of [2] gives the matrix determined by these
      !>     angles as
      !>
      !>        A  =  [ -z   ]   [ theta  ]   [ -zeta  ]
      !>                  A   3         A  2         A  3
      !>
      !>     Formulas for the Euler angles are from [2], equation
      !>     (5-143):
      !>                                              2                3
      !>         zeta   =  2306".2181*T  +  0".30188*T   +  0".017998*T
      !>             A
      !>
      !>                                              2                3
      !>         z      =  2306".2181*T  +  1".09468*T   +  0".018203*T
      !>          A
      !>
      !>                                              2                3
      !>         theta  =  2004".3109*T  -  0".42665*T   -  0".041833*T
      !>              A
      !>
      !>## Examples
      !>
      !>     1) Convert a state vector S from J2000 to Earth Mean equator and
      !>        equinox of date coordinates at epoch ET.  Call the resulting
      !>        vector SMOD.
      !>
      !>           CALL ZZEPRC76 ( ET,     PRECXF        )
      !>           CALL MXVG     ( PRECXF, S, 6, 6, SMOD )
      !>
      !>## Restrictions
      !>
      !>     1) This is a SPICE private routine; the routine is subject to
      !>        change without notice.  User applications should not call this
      !>        routine.
      !>
      !>     2) Though reference [1] does not specify limitations on the range
      !>        of valid time inputs for this precession model, the fact that
      !>        the rotation angles used in the model are defined by
      !>        polynomials implies that the model is not valid for all time.
      !>
      !>## Literature References
      !>
      !>     [1] "Explanatory Supplement to the Astronomical Almanac"
      !>          edited by P. Kenneth Seidelmann. University Science
      !>          Books, 20 Edgehill Road, Mill Valley, CA 94941 (1992)
      !>
      !>     [2] "Section 5, Geocentric Space-Fixed Position, Velocity, and
      !>         Acceleration Vectors of Tracking Station" by T. D. Moyer.
      !>         Draft of JPL Publication documenting the JPL navigation
      !>         program "Regres."
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.0.1, 18-APR-2016 (NJB)
      !>
      !>        Corrected typo in header abstract: changed "1876"
      !>        to "1976."
      !>
      !>-    SPICELIB Version 2.0.0, 18-DEC-2004 (NJB)
      module procedure zzeprc76
      use pepper_kinds, only: wp
      use pepper_constants, only: jyear, rpd
      implicit none
      real(wp), parameter :: zeta1 = 2306.2181_wp

      real(wp), parameter :: zeta2 = 0.30188_wp

      real(wp), parameter :: zeta3 = 0.017998_wp

      real(wp), parameter :: z1 = 2306.2181_wp

      real(wp), parameter :: z2 = 1.09468_wp

      real(wp), parameter :: z3 = 0.018203_wp

      real(wp), parameter :: theta1 = 2004.3109_wp

      real(wp), parameter :: theta2 = -0.42665_wp

      real(wp), parameter :: theta3 = -0.041833_wp

      real(wp)              cent
      real(wp)              dtheta
      real(wp)              dz
      real(wp)              dzeta
      real(wp)              eulang ( 6 )
      real(wp)              scale
      real(wp)              t
      real(wp)              theta
      real(wp)              ts
      real(wp)              z
      real(wp)              zeta

!
!     No check-in required; this routine does not participate in
!     SPICELIB error handling.
!
!     Compute the precession angles first.  The time argument has
!     units of Julian centuries.  The polynomial expressions yield
!     angles in units of arcseconds prior to scaling.  After scaling,
!     the angles are in units of radians.
!
      cent    =  jyear * 100.0_wp
      t       =  et    / cent
      scale   =  rpd / 3600.0_wp

      zeta    =  t  * (  zeta1  + t*(   zeta2  +   t*zeta3  )  ) * scale
      z       =  t  * (  z1     + t*(   z2     +   t*z3     )  ) * scale
      theta   =  t  * (  theta1 + t*(   theta2 +   t*theta3 )  ) * scale

      ts      =  1.0_wp / cent
      dzeta   =  ts * (  zeta1  + t*( 2*zeta2  + 3*t*zeta3  )  ) * scale
      dz      =  ts * (  z1     + t*( 2*z2     + 3*t*z3     )  ) * scale
      dtheta  =  ts * (  theta1 + t*( 2*theta2 + 3*t*theta3 )  ) * scale

!
!     Now compute the precession matrix.
!
      eulang(1)  =  -z
      eulang(2)  =   theta
      eulang(3)  =  -zeta
      eulang(4)  =  -dz
      eulang(5)  =   dtheta
      eulang(6)  =  -dzeta

      call eul2xf  ( ctx, eulang, 3, 2, 3, precxf )

      end procedure zzeprc76

      !>## Detailed Input
      !>
      !>     STATE    is a 6-vector giving the position and velocity of some
      !>              object in the body-fixed coordinates of the ellipsoid.
      !>
      !>     NEARP    the calculated/derived coordinates of the point on the
      !>              ellipsoid closest to STATE.
      !>
      !>              In body-fixed coordinates, the semi-axes of the ellipsoid
      !>              are aligned with the X, Y, and Z-axes of the coordinate
      !>              system.
      !>
      !>     A        is the length of the semi-axis of the ellipsoid that is
      !>              parallel to the X-axis of the body-fixed coordinate
      !>              system.
      !>
      !>     B        is the length of the semi-axis of the ellipsoid that is
      !>              parallel to the Y-axis of the body-fixed coordinate
      !>              system.
      !>
      !>     C        is the length of the semi-axis of the ellipsoid that is
      !>              parallel to the Z-axis of the body-fixed coordinate
      !>              system.
      !>
      !>## Detailed Output
      !>
      !>     DNEAR    is the 3-vector giving the velocity in
      !>              body-fixed coordinates of the point on the ellipsoid,
      !>              closest to the object whose position and velocity are
      !>              represented by STATE.
      !>
      !>              While the position component of DNEAR is always
      !>              meaningful, the velocity component of DNEAR will be
      !>              meaningless if FOUND if .FALSE. (See the discussion of
      !>              the meaning of FOUND below.)
      !>
      !>     DALT     is rate of change of the altitude, i.e. the range rate.
      !>
      !>              Note that the rate of change of altitude is meaningful if
      !>              and only if FOUND is .TRUE. (See the discussion of the
      !>              meaning of FOUND below.)
      !>
      !>     FOUND    is a logical flag indicating whether or not the velocity
      !>              portion of DNEAR is meaningful. If the velocity portion
      !>              of DNEAR is meaningful FOUND will be returned with a
      !>              value of .TRUE. Under very rare circumstance the velocity
      !>              of the near point is undefined. Under these circumstances
      !>              FOUND will be returned with the value .FALSE.
      !>
      !>              FOUND can be .FALSE. only for states whose position
      !>              components are inside the ellipsoid and then only at
      !>              points on a special surface contained inside the
      !>              ellipsoid called the focal set of the ellipsoid.
      !>
      !>              A point in the interior is on this special surface only
      !>              if there are two or more points on the ellipsoid that are
      !>              closest to it. The origin is such a point and the only
      !>              such point if the ellipsoid is a sphere. For
      !>              non-spheroidal ellipsoids the focal set contains small
      !>              portions of the planes of symmetry of the ellipsoid.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the axes are non-positive, an error is signaled by a
      !>         routine in the call tree of this routine.
      !>
      !>     2)  If an object is passing through the interior of an ellipsoid
      !>         there are points at which there is more than 1 point on
      !>         the ellipsoid that is closest to the object.  At these
      !>         points the velocity of the near point is undefined. (See
      !>         the description of the output variable FOUND).
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine computes DNEAR from STATE. In addition it returns
      !>     the range rate of altitude.
      !>
      !>     Note that this routine can compute DNEAR for STATES outside,
      !>     on, or inside the ellipsoid.  However, DNEAR and derivative
      !>     of altitude do not exist for a "small" set of STATES in the
      !>     interior of the ellipsoid. See the discussion of FOUND above
      !>     for a description of this set of points.
      !>
      !>## Examples
      !>
      !>     None.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman    (JPL)
      !>     W.L. Taber      (JPL)
      !>     E.D. Wright     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.0, 30-SEP-2021 (EDW)
      module procedure zzdnpt
      use pepper_kinds, only: ip, wp
      use pepper_math, only: mxv, unorm, vdot, vtmv
      implicit none
      real(wp)              denom
      real(wp)              dterm  ( 3 )
      real(wp)              grad   ( 3 )
      real(wp)              l
      real(wp)              length
      real(wp)              lprime
      real(wp)              norml  ( 3 )
      real(wp)              temp   ( 3 )
      real(wp)              zenith ( 3 )
      integer(ip)           i
      if ( .not. allocated ( ctx%zzdnpt_state ) ) allocate ( ctx%zzdnpt_state )
      associate ( gradm => ctx%zzdnpt_state%gradm, &
                  m => ctx%zzdnpt_state%m )
      if ( ctx%failed() ) return

!
!     Until we have reason to believe otherwise, we set FOUND to TRUE.
!
      found = .true.

!
!     Now for the work of this routine.  We need to compute the
!     velocity component of NEARP.
!
!     In all of the discussions below we let <,> stand for the
!     dot product (inner product).
!
!     Let P be the position (first three components) of STATE
!     and let N be the position (first three components) of NEARP.
!
!     The surface of the ellipsoid is described as the level set
!     f(x,y,z) = 1 for the function f defined by
!
!                    x**2 + y**2 + z**2
!         f(x,y,z) = ----   ----   ----
!                    A**2   B**2   C**2
!
!     Let GRAD be the "half" gradient of f,
!
!         (NABLA * f)/2
!
!     with NABLA the operator
!
!         (Dx, Dy, Dz)
!
!     ("D" indicating partial derivative). Then for some L
!
!           N + L * GRAD = P                         ( 1 )
!
!     Solve for L
!
!           L * GRAD = P - N
!
!      Apply <,GRAD> to LHS and RHS of expression
!
!           <L * GRAD, GRAD> = < P - N, GRAD >
!
!           L * < GRAD, GRAD > = < P - N, GRAD >
!
!     So that
!                < P - N, GRAD >
!           L =  --------------
!                < GRAD , GRAD >
!
!      Recall
!
!            < X, X > = |X|**2 , X in Rn, R3 in this case
!
!                          GRAD
!             =  < P - N, ------ >  /  | GRAD |
!                         |GRAD|
!
!     Since GRAD is computed at a point on the level set f(x,y,z) = 1
!     we don't have to worry about the magnitude of |GRAD| being
!     so small that underflow can occur (mostly).
!
!     Note that the half gradient of f can be computed by simple
!     vector multiplication
!
!                       [ 1/A**2    0       0    ] [ x ]
!        GRAD(x,y,z)  = |   0     1/B**2    0    | | y |
!                       [   0       0     1/C**2 ] [ z ]
!
!     We call the matrix above GRADM.  The correct off
!     diagonal values have been established in the data statement
!     following the declaration section of this routine.
!

      gradm(1,1) = 1.0_wp/(a*a)
      gradm(2,2) = 1.0_wp/(b*b)
      gradm(3,3) = 1.0_wp/(c*c)

      zenith = state(1:3) - nearp

      call mxv   ( gradm,  nearp,    grad   )
      call unorm ( grad,   norml,    length )

      l  = vdot  ( zenith, norml ) / length

!
!     We can rewrite equation (1) as
!
!        P = N + L * GRADM * N
!
!     from this it follows that
!
!        P' =  N' + L' * GRADM * N
!                 + L  * GRADM * N'
!
!           = ( IDENT + L*GRADM ) * N'   + L' * GRADM * N
!
!           = ( IDENT + L*GRADM ) * N'   + L' * GRAD
!
!     where IDENT is the 3x3 identity matrix.
!
!     Let M be the inverse of the matrix IDENT + L*GRADM. (Provided
!     of course that all of the diagonal entries are non-zero).
!
!     If we multiply both sides of the equation above by M
!     we have
!
!        M*P'  = N'  + L'* M * GRAD                      ( 2 )
!
!     Recall now that N' is orthogonal to GRAD (N' lies in the
!     tangent plane to the ellipsoid at N and GRAD is normal
!     to this tangent plane).  Thus
!
!        < GRAD, M*P' > = L' < GRAD, M * GRAD >
!
!     and
!
!                 < GRAD, M*P'   >
!        L'   =   -----------------
!                 < GRAD, M*GRAD >
!
!             =   VTMV ( GRAD, M, P' ) / VTMV ( GRAD, M, GRAD )
!
!     Let's pause now to compute M and L'.
!
!        This is where things could go bad.  M might not exist (which
!        indicates STATE is on the focal set of the ellipsoid).  In
!        addition it is conceivable that VTMV ( GRAD, M, GRAD ) is
!        zero.  This turns out not to be possible.  However, the
!        demonstration of this fact requires delving into the details
!        of how N was computed by NEARPT.  Rather than spending a
!        lot of time explaining the details we will make an
!        unnecessary but inexpensive check that we don't divide by
!        zero when computing L'.
!

      do i = 1, 3
         dterm(i) = 1.0_wp + l*gradm(i,i)
      end do

      do i = 1, 3

         if ( dterm(i) /= 0.0_wp ) then
            m(i,i) = 1.0_wp / dterm(i)
         else
            found = .false.
            return
         end if

      end do

      denom = vtmv ( grad, m, grad )

      if ( denom == 0.0_wp ) then
         found = .false.
         return
      end if

      lprime = vtmv( grad, m, state(4) ) / denom

!
!     Now that we have L' we can easily compute N'. Rewriting
!     equation (2) from above we have.
!
!        N'  = M * ( P' - L'*GRAD )
!
      temp = 1.0_wp*state(4:6) + (-lprime)*grad
      call mxv   ( m,     temp,                    dnear )

!
!     Only one thing left to do. Compute the derivative
!     of the altitude ALT. This quantity equals the range rate of the
!     vector from the near point, N, to the observer object, P.
!
!                               ^
!     Range rate in R3 equals < r, v >. In this case, NORML defines
!     the unit vector from N to P. The velocity of P with respect
!     to N,
!
!        V = d(P - N) = P' - N'
!            --
!            dt
!
!     But as we discussed earlier, N' is orthogonal to NORML (GRAD).
!     Thus
!
!          ^
!        < r, v > = < NORML, P' - N' >
!                 = < NORML, P'> - < NORML, N'>
!                 = < NORML, P'>
!
!        dALT/dt = < NORML, P'>
!
!     Given P' = STATE(4,5,6)
!
      dalt = vdot ( norml, state(4) )

      return
      end associate
      end procedure zzdnpt

      !>## Detailed Input
      !>
      !>     STATE    is a 6-vector giving the position and velocity of some
      !>              object in the body-fixed coordinates of the ellipsoid.
      !>
      !>              In body-fixed coordinates, the semi-axes of the ellipsoid
      !>              are aligned with the X, Y, and Z-axes of the coordinate
      !>              system.
      !>
      !>     A        is the length of the semi-axis of the ellipsoid that is
      !>              parallel to the X-axis of the body-fixed coordinate
      !>              system.
      !>
      !>     B        is the length of the semi-axis of the ellipsoid that is
      !>              parallel to the Y-axis of the body-fixed coordinate
      !>              system.
      !>
      !>     C        is the length of the semi-axis of the ellipsoid that is
      !>              parallel to the Z-axis of the body-fixed coordinate
      !>              system.
      !>
      !>## Detailed Output
      !>
      !>     DNEAR    is the 6-vector giving the position and velocity in
      !>              body-fixed coordinates of the point on the ellipsoid,
      !>              closest to the object whose position and velocity are
      !>              represented by STATE.
      !>
      !>              While the position component of DNEAR is always
      !>              meaningful, the velocity component of DNEAR will be
      !>              meaningless if FOUND if .FALSE. (See the discussion of
      !>              the meaning of FOUND below.)
      !>
      !>     DALT     is an array of two double precision numbers. The first
      !>              gives the altitude of STATE with respect to the
      !>              ellipsoid. The second gives the rate of change of the
      !>              altitude.
      !>
      !>              Note that the rate of change of altitude is meaningful if
      !>              and only if FOUND is .TRUE. (See the discussion of the
      !>              meaning of FOUND below.)
      !>
      !>     FOUND    is a logical flag indicating whether or not the velocity
      !>              portion of DNEAR is meaningful. If the velocity portion
      !>              of DNEAR is meaningful FOUND will be returned with a
      !>              value of .TRUE. Under very rare circumstance the velocity
      !>              of the near point is undefined. Under these circumstances
      !>              FOUND will be returned with the value .FALSE.
      !>
      !>              FOUND can be .FALSE. only for states whose position
      !>              components are inside the ellipsoid and then only at
      !>              points on a special surface contained inside the
      !>              ellipsoid called the focal set of the ellipsoid.
      !>
      !>              A point in the interior is on this special surface only
      !>              if there are two or more points on the ellipsoid that are
      !>              closest to it. The origin is such a point and the only
      !>              such point if the ellipsoid is a sphere. For
      !>              non-spheroidal ellipsoids the focal set contains small
      !>              portions of the planes of symmetry of the ellipsoid.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the axes are non-positive, an error is signaled by a
      !>         routine in the call tree of this routine.
      !>
      !>     2)  If an object is passing through the interior of an ellipsoid
      !>         there are points at which there is more than 1 point on the
      !>         ellipsoid that is closest to the object. At these points the
      !>         velocity of the near point is undefined. (See the description
      !>         of the output variable FOUND).
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     If an object is moving relative to some triaxial body along a
      !>     trajectory C(t) then there is a companion trajectory N(t) that
      !>     gives the point on the ellipsoid that is closest to C(t) as a
      !>     function of `t'. The instantaneous position and velocity of C(t),
      !>     STATE, are sufficient to compute the instantaneous position and
      !>     velocity of N(t), DNEAR.
      !>
      !>     This routine computes DNEAR from STATE. In addition it returns the
      !>     altitude and rate of change of altitude.
      !>
      !>     Note that this routine can compute DNEAR for STATE outside, on,
      !>     or inside the ellipsoid. However, the velocity of DNEAR and
      !>     derivative of altitude do not exist for a "small" set of STATE
      !>     in the interior of the ellipsoid. See the discussion of FOUND
      !>     above for a description of this set of points.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for these examples may differ across
      !>     platforms. The results depend on the SPICE kernels used as input,
      !>     the compiler and supporting libraries, and the machine specific
      !>     arithmetic implementation.
      !>
      !>     1) Suppose you wish to compute the velocity of the ground track
      !>        of a satellite as it passes over a location on Mars and that
      !>        the moment of passage has been previously determined. (We
      !>        assume that the spacecraft is close enough to the surface that
      !>        light time corrections do not matter.)
      !>
      !>        Use the meta-kernel shown below to load the required SPICE
      !>        kernels.
      !>
      !>           KPL/MK
      !>
      !>           File: dnearp_ex1.tm
      !>
      !>           This meta-kernel is intended to support operation of SPICE
      !>           example programs. The kernels shown here should not be
      !>           assumed to contain adequate or correct versions of data
      !>           required by SPICE-based user applications.
      !>
      !>           In order for an application to use this meta-kernel, the
      !>           kernels referenced here must be present in the user's
      !>           current working directory.
      !>
      !>           The names and contents of the kernels referenced
      !>           by this meta-kernel are as follows:
      !>
      !>              File name                        Contents
      !>              ---------                        --------
      !>              pck00010.tpc                     Planet orientation and
      !>                                               radii
      !>              naif0012.tls                     Leapseconds
      !>              de430.bsp                        Planetary ephemeris
      !>              mar097.bsp                       Mars satellite ephemeris
      !>              mro_psp4_ssd_mro95a.bsp          MRO ephemeris
      !>
      !>           \begindata
      !>
      !>              KERNELS_TO_LOAD = ( 'pck00010.tpc',
      !>                                  'naif0012.tls',
      !>                                  'de430.bsp',
      !>                                  'mar097.bsp',
      !>                                  'mro_psp4_ssd_mro95a.bsp' )
      !>
      !>           \begintext
      !>
      !>           End of meta-kernel
      !>
      !>        Example code begins here.
      !>
      !>              PROGRAM DNEARP_EX1
      !>              IMPLICIT NONE
      !>
      !>        C
      !>        C     SPICELIB functions
      !>        C
      !>              DOUBLE PRECISION      VNORM
      !>
      !>        C
      !>        C     Local parameters
      !>        C
      !>              CHARACTER*(*)         BODYNM
      !>              PARAMETER           ( BODYNM = 'MARS' )
      !>
      !>              CHARACTER*(*)         META
      !>              PARAMETER           ( META   = 'dnearp_ex1.tm' )
      !>
      !>        C
      !>        C     Local variables
      !>        C
      !>              DOUBLE PRECISION      A
      !>              DOUBLE PRECISION      B
      !>              DOUBLE PRECISION      C
      !>              DOUBLE PRECISION      DALT   ( 2 )
      !>              DOUBLE PRECISION      DNEAR  ( 6 )
      !>              DOUBLE PRECISION      ET
      !>              DOUBLE PRECISION      LT
      !>              DOUBLE PRECISION      RADII  ( 3 )
      !>              DOUBLE PRECISION      STATE  ( 6 )
      !>              DOUBLE PRECISION      GTVEL  ( 3 )
      !>
      !>              INTEGER               DIM
      !>
      !>              LOGICAL               FOUND
      !>
      !>        C
      !>        C     Load kernel files via the meta-kernel.
      !>        C
      !>              CALL FURNSH ( META )
      !>
      !>        C
      !>        C     Convert the TDB input time string to seconds past
      !>        C     J2000, TDB.
      !>        C
      !>              CALL STR2ET ( '2007 SEP 30 00:00:00 TDB', ET )
      !>
      !>        C
      !>        C     First get the axes of the body.
      !>        C
      !>              CALL BODVRD ( BODYNM, 'RADII', 3, DIM, RADII )
      !>              CALL VUPACK ( RADII, A, B, C )
      !>
      !>        C
      !>        C     Get the geometric state of the spacecraft with
      !>        C     respect to BODYNM in the body-fixed reference frame
      !>        C     at ET and compute the state of the sub-spacecraft point.
      !>        C
      !>              CALL SPKEZR ( 'MRO',  ET,    'IAU_MARS', 'NONE',
      !>             .              BODYNM, STATE, LT                  )
      !>              CALL DNEARP ( STATE, A, B, C, DNEAR, DALT, FOUND )
      !>
      !>              IF ( FOUND ) THEN
      !>
      !>        C
      !>        C        DNEAR contains the state of the subspacecraft point.
      !>        C
      !>                 CALL VEQU ( DNEAR(4), GTVEL )
      !>
      !>                 WRITE(*,'(A,3F10.6)')
      !>             .        'Ground-track velocity (km/s):', GTVEL
      !>                 WRITE(*,'(A,F10.6)')
      !>             .        'Ground-track speed    (km/s):', VNORM( GTVEL )
      !>
      !>              ELSE
      !>
      !>                 WRITE(*,*) 'DNEAR is degenerate.'
      !>
      !>              END IF
      !>
      !>              END
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Ground-track velocity (km/s):  0.505252  1.986553 -2.475506
      !>        Ground-track speed    (km/s):  3.214001
      !>
      !>     2) Suppose you wish to compute the one-way doppler shift of a
      !>        radar mounted on board a spacecraft as it passes over some
      !>        region. Moreover, assume that for your purposes it is
      !>        sufficient to neglect effects of atmosphere, topography and
      !>        antenna pattern for the sake of this computation.
      !>
      !>        Use the meta-kernel from Example 1 above.
      !>
      !>        Example code begins here.
      !>
      !>              PROGRAM DNEARP_EX2
      !>              IMPLICIT NONE
      !>
      !>        C
      !>        C     SPICELIB functions
      !>        C
      !>              DOUBLE PRECISION      CLIGHT
      !>
      !>        C
      !>        C     Local parameters
      !>        C
      !>              CHARACTER*(*)         BODYNM
      !>              PARAMETER           ( BODYNM = 'MARS' )
      !>
      !>              CHARACTER*(*)         META
      !>              PARAMETER           ( META   = 'dnearp_ex1.tm' )
      !>
      !>        C
      !>        C     Define the central frequency of the radar,
      !>        C     in megahertz.
      !>        C
      !>              DOUBLE PRECISION      RCFRQ
      !>              PARAMETER           ( RCFRQ  = 20.D0 )
      !>
      !>        C
      !>        C     Local variables
      !>        C
      !>              DOUBLE PRECISION      A
      !>              DOUBLE PRECISION      B
      !>              DOUBLE PRECISION      C
      !>              DOUBLE PRECISION      DALT   ( 2 )
      !>              DOUBLE PRECISION      DNEAR  ( 6 )
      !>              DOUBLE PRECISION      ET
      !>              DOUBLE PRECISION      LT
      !>              DOUBLE PRECISION      RADII  ( 3 )
      !>              DOUBLE PRECISION      SHIFT
      !>              DOUBLE PRECISION      STATE  ( 6 )
      !>
      !>              INTEGER               DIM
      !>
      !>              LOGICAL               FOUND
      !>
      !>        C
      !>        C     Load kernel files via the meta-kernel.
      !>        C
      !>              CALL FURNSH ( META )
      !>
      !>        C
      !>        C     Convert the TDB input time string to seconds past
      !>        C     J2000, TDB.
      !>        C
      !>              CALL STR2ET ( '2007 SEP 30 00:00:00 TDB', ET )
      !>
      !>        C
      !>        C     First get the axes of the body.
      !>        C
      !>              CALL BODVRD ( BODYNM, 'RADII', 3, DIM, RADII )
      !>              CALL VUPACK ( RADII, A, B, C )
      !>
      !>        C
      !>        C     Get the geometric state of the spacecraft with
      !>        C     respect to BODYNM in the body-fixed reference frame
      !>        C     at ET and compute the state of the sub-spacecraft point.
      !>        C
      !>              CALL SPKEZR ( 'MRO',  ET,    'IAU_MARS', 'NONE',
      !>             .              BODYNM, STATE, LT                  )
      !>              CALL DNEARP ( STATE, A, B, C, DNEAR, DALT, FOUND )
      !>
      !>              IF ( FOUND ) THEN
      !>
      !>        C
      !>        C        The change in frequency is given by multiplying SHIFT
      !>        C        times the carrier frequency
      !>        C
      !>                 SHIFT = ( DALT(2) / CLIGHT() )
      !>                 WRITE(*,'(A,F20.16)') 'Central frequency (MHz):',
      !>             .                          RCFRQ
      !>                 WRITE(*,'(A,F20.16)') 'Doppler shift     (MHz):',
      !>             .                          RCFRQ * SHIFT
      !>
      !>              ELSE
      !>
      !>                 WRITE(*,*) 'DNEAR is degenerate.'
      !>
      !>              END IF
      !>
      !>              END
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Central frequency (MHz): 20.0000000000000000
      !>        Doppler shift     (MHz): -0.0000005500991159
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.0.0, 26-OCT-2021 (JDR) (EDW)
      !>
      !>        Reimplemented routine using ZZDNPT.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code examples, based on the existing code fragments.
      !>
      !>-    SPICELIB Version 1.1.2, 26-JUN-2008 (NJB)
      !>
      !>        Corrected spelling error in abstract; re-wrote
      !>        abstract text.
      !>
      !>-    SPICELIB Version 1.1.1, 24-OCT-2005 (NJB)
      !>
      !>        Header update: changed references to BODVAR to references
      !>        to BODVCD.
      !>
      !>-    SPICELIB Version 1.1.0, 05-MAR-1998 (WLT)
      !>
      !>        In the previous version of the routine FOUND could be
      !>        returned without being set to .TRUE. when the velocity
      !>        of the near point and rate of change of altitude
      !>        could be determined. This error has been corrected.
      !>
      !>-    SPICELIB Version 1.0.0, 15-JUN-1995 (WLT)
      module procedure dnearp
      use pepper_kinds, only: wp
      implicit none

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Until we have reason to believe otherwise, we set FOUND to TRUE.
!
      found = .true.

!
!     First we need to compute the near point.
!
!     (PEPPER: NEARPT sets the altitude, DALT(1); DALT(2) is set below.)
      call nearpt ( ctx, state, a, b, c, dnear, dalt(1) )

!
!     Make sure nothing went bump in the dark innards of NEARPT.
!
      if ( ctx%failed() ) then
         found = .false.
         return
      end if

!
!     Calculate the derivative of the near point and altitude.
!

      call zzdnpt ( ctx, state, dnear, a, b, c, dnear(4), dalt(2), found )

!
!     Check error status. Bail out if failure in call.
!
      if ( ctx%failed() ) then
         found = .false.
         return
      end if

      return
      end procedure dnearp

      !>## Abstract
      !>
      !>     SPICE Private routine intended solely for the support of SPICE
      !>     routines.  Users should not call this routine directly due
      !>     to the volatile nature of this routine.
      !>
      !>     Return the state (position and velocity) of a target body
      !>     relative to an observing body, optionally corrected for light
      !>     time (planetary aberration) and stellar aberration.
      !>
      !>## Detailed Input
      !>
      !>     XMIT        is a logical flag which is set to .TRUE. for the
      !>                 "transmission" case in which photons *depart* from
      !>                 the observer's location at an observation epoch ET
      !>                 and arrive at the target's location at the light-time
      !>                 corrected epoch ET+LT, where LT is the one-way light
      !>                 time between observer and target; XMIT is set to
      !>                 .FALSE. for "reception" case in which photons depart
      !>                 from the target's location at the light-time
      !>                 corrected epoch ET-LT and *arrive* at the observer's
      !>                 location at ET.
      !>
      !>                 Note that the observation epoch is not used in this
      !>                 routine.
      !>
      !>                 XMIT must be consistent with any light time
      !>                 corrections used for the input state STARG: if that
      !>                 state has been corrected for "reception" light time;
      !>                 XMIT must be .FALSE.; otherwise XMIT must be .TRUE.
      !>
      !>     ACCOBS      is the geometric acceleration of the observer
      !>                 relative to the solar system barycenter. Units are
      !>                 km/sec**2. ACCOBS must be expressed relative to
      !>                 an inertial reference frame.
      !>
      !>     VOBS        is the geometric velocity of the observer relative to
      !>                 the solar system barycenter. VOBS must be expressed
      !>                 relative to the same inertial reference frame as
      !>                 ACCOBS. Units are km/sec.
      !>
      !>     STARG       is the Cartesian state of the target relative to the
      !>                 observer. Normally STARG has been corrected for
      !>                 one-way light time, but this is not required. STARG
      !>                 must be expressed relative to the same inertial
      !>                 reference frame as ACCOBS. Components are
      !>                 (x, y, z, dx, dy, dz). Units are km and km/sec.
      !>
      !>## Detailed Output
      !>
      !>     SCORR       is the stellar aberration correction for the position
      !>                 component of STARG. Adding SCORR to this position
      !>                 vector produces the input observer-target position,
      !>                 corrected for stellar aberration.
      !>
      !>                 The reference frame of SCORR is the common frame
      !>                 relative to which the inputs ACCOBS, VOBS, and STARG
      !>                 are expressed. Units are km.
      !>
      !>     DSCORR      is the stellar aberration correction for the velocity
      !>                 component of STARG. Adding DSCORR to this velocity
      !>                 vector produces the input observer-target velocity,
      !>                 corrected for stellar aberration.
      !>
      !>                 The reference frame of DSCORR is the common frame
      !>                 relative to which the inputs ACCOBS, VOBS, and STARG
      !>                 are expressed. Units are km/s.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1) If attempt to divide by zero occurs, the error
      !>        SPICE(DIVIDEBYZERO) will be signaled. This case may occur
      !>        due to uninitialized inputs.
      !>
      !>     2) Loss of precision will occur for geometric cases in which
      !>        VOBS is nearly parallel to the position component of STARG.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine computes a Newtonian estimate of the stellar
      !>     aberration correction of an input state. Normally the input state
      !>     has already been corrected for one-way light time.
      !>
      !>     Since stellar aberration corrections are typically "small"
      !>     relative to the magnitude of the input observer-target position
      !>     and velocity, this routine avoids loss of precision by returning
      !>     the corrections themselves rather than the corrected state
      !>     vector. This allows the caller to manipulate (for example,
      !>     interpolate) the corrections with greater accuracy.
      !>
      !>## Examples
      !>
      !>     See SPICELIB routine SPKACS.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     SPK Required Reading.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman    (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.0.0, 15-APR-2014 (NJB)
      !>
      !>        Added RETURN test and discovery check-in.
      !>        Check for division by zero was added. This
      !>        case might occur due to uninitialized inputs.
      !>
      !>-    SPICELIB Version 1.0.1, 12-FEB-2009 (NJB)
      !>
      !>        Minor updates were made to the inline documentation.
      !>
      !>-    SPICELIB Version 1.0.0, 17-JAN-2008 (NJB)
      module procedure zzstelab
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: clight
      use pepper_math, only: dvhat, vdot, vhat, vnorm, vperp
      implicit none

!
!     Note for the maintenance programmer
!     ===================================
!
!     The source code of the test utility T_ZZSTLABN must be
!     kept in sync with the source code of this routine. That
!     routine uses a value of SEPLIM that forces the numeric
!     branch of the velocity computation to be taken in all
!     cases. See the documentation of that routine for details.
!

!     Let PHI be the (non-negative) rotation angle of the stellar
!     aberration correction; then SEPLIM is a limit on how close PHI
!     may be to zero radians while stellar aberration velocity is
!     computed analytically. When sin(PHI) is less than SEPLIM, the
!     velocity must be computed numerically.
!
      real(wp), parameter :: seplim = 1.0e-6_wp

!
!     Let TDELTA be the time interval, measured in seconds,
!     used for numerical differentiation of the stellar
!     aberration correction, when this is necessary.
!
      real(wp), parameter :: tdelta = 1.0_wp

      real(wp)              c
      real(wp)              dphi
      real(wp)              dptmag
      real(wp)              drhat  ( 3 )
      real(wp)              dvp    ( 3 )
      real(wp)              dvphat ( 3 )
      real(wp)              eptarg ( 3 )
      real(wp)              evobs  ( 3 )
      real(wp)              lcacc  ( 3 )
      real(wp)              lcvobs ( 3 )
      real(wp)              ptarg  ( 3 )
      real(wp)              ptgmag
      real(wp)              rhat   ( 3 )
      real(wp)              s
      real(wp)              saoff  ( 3, 2 )
      real(wp)              sgn
      real(wp)              srhat  ( 6 )
      real(wp)              svp    ( 6 )
      real(wp)              svphat ( 6 )
      real(wp)              term1  ( 3 )
      real(wp)              term2  ( 3 )
      real(wp)              term3  ( 3 )
      real(wp)              vp     ( 3 )
      real(wp)              vphat  ( 3 )
      real(wp)              vtarg  ( 3 )

      integer(ip)           i

      if ( ctx%failed() ) return

!
!     In the discussion below, the dot product of vectors X and Y
!     is denoted by
!
!        <X,Y>
!
!     The speed of light is denoted by the lower case letter "c." BTW,
!     variable names used here are case-sensitive: upper case "C"
!     represents a different quantity which is unrelated to the speed
!     of light.
!
!     Variable names ending in "HAT" denote unit vectors. Variable
!     names starting with "D" denote derivatives with respect to time.
!
!     We'll compute the correction SCORR and its derivative with
!     respect to time DSCORR for the reception case. In the
!     transmission case, we perform the same computation with the
!     negatives of the observer velocity and acceleration.
!
!     In the code below, we'll store the position and velocity portions
!     of the input observer-target state STARG in the variables PTARG
!     and VTARG, respectively.
!
!     Let VP be the component of VOBS orthogonal to PTARG. VP
!     is defined as
!
!         VOBS - < VOBS, RHAT > RHAT                                 (1)
!
!     where RHAT is the unit vector
!
!         PTARG/||PTARG||
!
!     Then
!
!        ||VP||/c                                                    (2)
!
!     is the magnitude of
!
!        s = sin( phi )                                              (3)
!
!     where phi is the stellar aberration correction angle. We'll
!     need the derivative with respect to time of (2).
!
!     Differentiating (1) with respect to time yields the
!     velocity DVP, where, letting
!
!        DRHAT  =  d(RHAT) / dt
!        VPHAT  =  VP      / ||VP||
!        DVPMAG =  d( ||VP|| ) / dt
!
!     we have
!
!        DVP = d(VP)/dt
!
!            = ACCOBS - (  ( <VOBS,DRHAT> + <ACCOBS, RHAT> )*RHAT
!                        +   <VOBS,RHAT>  * DRHAT                 )  (4)
!
!     and
!
!        DVPMAG = < DVP, VPHAT >                                     (5)
!
!     Now we can find the derivative with respect to time of
!     the stellar aberration angle phi:
!
!        ds/dt = d(sin(phi))/dt = d(phi)/dt * cos(phi)               (6)
!
!     Using (2) and (5), we have for positive phi,
!
!        ds/dt = (1/c)*DVPMAG = (1/c)*<DVP, VPHAT>                   (7)
!
!     Then for positive phi
!
!        d(phi)/dt = (1/cos(phi)) * (1/c) * <DVP, VPHAT>             (8)
!
!     Equation (8) is well-defined as along as VP is non-zero:
!     if VP is the zero vector, VPHAT is undefined. We'll treat
!     the singular and near-singular cases separately.
!
!     The aberration correction itself is a rotation by angle phi
!     from RHAT towards VP, so the corrected vector is
!
!        ( sin(phi)*VPHAT + cos(phi)*RHAT ) * ||PTARG||
!
!     and  we can express the offset of the corrected vector from
!     PTARG, which is the output SCORR, as
!
!        SCORR =
!
!        ( sin(phi)*VPHAT + (cos(phi)-1)*RHAT ) * ||PTARG||          (9)
!
!     Let DPTMAG be defined as
!
!        DPTMAG  =  d ( ||PTARG|| ) / dt                            (10)
!
!     Then the derivative with respect to time of SCORR is
!
!        DSCORR =
!
!             (      sin(phi)*DVPHAT
!
!                +   cos(phi)*d(phi)/dt * VPHAT
!
!                +  (cos(phi) - 1) * DRHAT
!
!                +  ( -sin(phi)*d(phi)/dt ) * RHAT   ) * ||PTARG||
!
!           + ( sin(phi)*VPHAT + (cos(phi)-1)*RHAT ) * DPTMAG       (11)
!
!     Computations begin here:
!
!     Split STARG into position and velocity components. Compute
!
!        RHAT
!        DRHAT
!        VP
!        DPTMAG
!
      if ( xmit ) then
         lcvobs = -vobs
         lcacc = -accobs
      else
         lcvobs = vobs
         lcacc = accobs
      end if

      ptarg = starg(1:3)
      vtarg = starg(4:6)

      call dvhat ( starg,      srhat )
      rhat = srhat(1:3)
      drhat = srhat(4:6)

      call vperp ( lcvobs, rhat, vp    )

      dptmag = vdot ( vtarg, rhat )

!
!     Compute sin(phi) and cos(phi), which we'll call S and C
!     respectively. Note that phi is always close to zero for
!     realistic inputs (for which ||VOBS|| << CLIGHT), so the
!     cosine term is positive.
!
      s = vnorm(vp) / clight

      c = sqrt (  max( 0.0_wp,  1 - s*s )  )

      if ( c == 0.0_wp ) then
!
!        C will be used as a divisor later (in the computation
!        of DPHI), so we'll put a stop to the problem here.
!
         call ctx%setmsg ( 'Cosine of the aberration angle is 0; ' &
         //            'this cannot occur for realistic ' &
         //            'observer velocities. This case can ' &
         //            'arise due to uninitialized inputs. ' &
         //            'This cosine value is used as a divisor ' &
         //            'in a later computation, so it must not ' &
         //            'be equal to zero.'                      )
        call ctx%sigerr ( 'SPICE(DIVIDEBYZERO)'                     )
        return

      end if

!
!     Compute the unit vector VPHAT and the stellar
!     aberration correction. We avoid relying on
!     VHAT's exception handling for the zero vector.
!
      if ( all(vp == 0.0_wp) ) then
         vphat = 0.0_wp
      else
         call vhat   ( vp, vphat )
      end if

!
!     Now we can use equation (9) to obtain the stellar
!     aberration correction SCORR:
!
!        SCORR =
!
!           ( sin(phi)*VPHAT + (cos(phi)-1)*RHAT ) * ||PTARG||
!
      ptgmag = vnorm( ptarg )

      scorr = (ptgmag * s)*vphat + (ptgmag * (c - 1.0_wp))*rhat

!
!     Now we use S as an estimate of PHI to decide if we're
!     going to differentiate the stellar aberration correction
!     analytically or numerically.
!
!     Note that S is non-negative by construction, so we don't
!     need to use the absolute value of S here.
!
      if ( s >= seplim ) then
!
!        This is the analytic case.
!
!        Compute DVP---the derivative of VP with respect to time.
!        Recall equation (4):
!
!        DVP = d(VP)/dt
!
!            = ACCOBS - (  ( <VOBS,DRHAT> + <ACCOBS, RHAT> )*RHAT
!                        +   <VOBS,RHAT>  * DRHAT                 )
!
         dvp = 1.0_wp*lcacc + (-vdot(lcvobs, drhat) - vdot(lcacc, rhat))*rhat + (-vdot(lcvobs, rhat ))*drhat

         call vhat ( vp, vphat )

!
!        Now we can compute DVPHAT, the derivative of VPHAT:
!
         svp(1:3) = vp
         svp(4:6) = dvp

         call dvhat ( svp,       svphat )
         dvphat = svphat(4:6)

!
!        Compute the DPHI, the time derivative of PHI, using equation 8:
!
!           d(phi)/dt = (1/cos(phi)) * (1/c) * <DVP, VPHAT>
!
         dphi = ( 1.0_wp / ( c * clight ) ) * vdot( dvp, vphat )

!
!        At long last we've assembled all of the "ingredients" required
!        to compute DSCORR:
!
!           DSCORR =
!
!             (     sin(phi)*DVPHAT
!
!                +  cos(phi)*d(phi)/dt * VPHAT
!
!                +  (cos(phi) - 1) * DRHAT
!
!                +  ( -sin(phi)*d(phi)/dt ) * RHAT   ) * ||PTARG||
!
!                +  ( sin(phi)*VPHAT + (cos(phi)-1)*RHAT ) * DPTMAG
!
         term1 = s*dvphat + (c * dphi)*vphat

         term2 = (c - 1.0_wp)*drhat + ((-s) * dphi)*rhat

         term3 = term1 + term2

         dscorr = ptgmag*term3 + (dptmag * s)*vphat + (dptmag * (c - 1.0_wp))*rhat

      else
!
!        This is the numeric case. We're going to differentiate
!        the stellar aberration correction offset vector using
!        a quadratic estimate.
!
         do i = 1, 2
!
!           Set the sign of the time offset.
!
            if ( i == 1 ) then
               sgn = -1.0_wp
            else
               sgn =  1.0_wp
            end if

!
!           Estimate the observer's velocity relative to the
!           solar system barycenter at the current epoch. We use
!           the local copies of the input velocity and acceleration
!           to make a linear estimate.
!
            evobs = 1.0_wp*lcvobs + (sgn*tdelta)*lcacc

!
!           Estimate the observer-target vector. We use the
!           observer-target state velocity to make a linear estimate.
!
            eptarg = 1.0_wp*starg(1:3) + (sgn*tdelta)*starg(4:6)

!
!           Let RHAT be the unit observer-target position.
!           Compute the component of the observer's velocity
!           that is perpendicular to the target position; call
!           this vector VP. Also compute the unit vector in
!           the direction of VP.
!
            call vhat  ( eptarg,  rhat     )
            call vperp ( evobs,   rhat, vp )

            if ( all(vp == 0.0_wp) ) then
               vphat = 0.0_wp
            else
               call vhat   ( vp, vphat )
            end if

!
!           Compute the sine and cosine of the correction
!           angle.
!
            s = vnorm(vp) / clight

            c = sqrt (  max( 0.0_wp,  1 - s*s )  )

!
!           Compute the vector offset of the correction.
!
            ptgmag = vnorm( eptarg )

            saoff(:,i) = (ptgmag * s)*vphat + (ptgmag * (c - 1.0_wp))*rhat
         end do

!
!        Now compute the derivative.
!
         call qderiv ( ctx, 3, saoff(1,1), saoff(1,2), tdelta, dscorr )

      end if

!
!     At this point the correction offset SCORR and its derivative
!     with respect to time DSCORR are both set.
!
      end procedure zzstelab

end submodule pepper_rotations
