!$Procedure M2EUL ( Matrix to Euler angles )

      subroutine m2eul (  r,  axis3,   axis2,   axis1, &
                              angle3,  angle2,  angle1  )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Factor a rotation matrix as a product of three rotations about
!     specified coordinate axes.
!
! $ Required_Reading
!
!     ROTATION
!
! $ Keywords
!
!     ANGLE
!     MATRIX
!     ROTATION
!     TRANSFORMATION
!
! $ Declarations

      implicit none

      real(wp)              r      ( 3, 3 )

      integer(ip)           axis3
      integer(ip)           axis2
      integer(ip)           axis1

      real(wp)              angle3
      real(wp)              angle2
      real(wp)              angle1

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     R          I   A rotation matrix to be factored.
!     AXIS3,
!     AXIS2,
!     AXIS1      I   Numbers of third, second, and first rotation axes.
!     ANGLE3,
!     ANGLE2,
!     ANGLE1     O   Third, second, and first Euler angles, in radians.
!
! $ Detailed_Input
!
!     R        is a 3x3 rotation matrix that is to be factored as
!              a product of three rotations about a specified
!              coordinate axes. The angles of these rotations are
!              called "Euler angles."
!
!     AXIS3,
!     AXIS2,
!     AXIS1    are the indices of the rotation axes of the
!              "factor" rotations, whose product is R. R is
!              factored as
!
!                 R = [ ANGLE3 ]      [ ANGLE2 ]      [ ANGLE1 ]
!                               AXIS3           AXIS2           AXIS1
!
!              The axis numbers must belong to the set {1, 2, 3}.
!              The second axis number MUST differ from the first
!              and third axis numbers.
!
!              See the $Particulars section below for details
!              concerning this notation.
!
! $ Detailed_Output
!
!     ANGLE3,
!     ANGLE2,
!     ANGLE1   are the Euler angles corresponding to the matrix
!              R and the axes specified by AXIS3, AXIS2, and
!              AXIS1. These angles satisfy the equality
!
!                 R = [ ANGLE3 ]      [ ANGLE2 ]      [ ANGLE1 ]
!                               AXIS3           AXIS2           AXIS1
!
!
!              See the $Particulars section below for details
!              concerning this notation.
!
!              The range of ANGLE3 and ANGLE1 is (-pi, pi].
!
!              The range of ANGLE2 depends on the exact set of
!              axes used for the factorization. For
!              factorizations in which the first and third axes
!              are the same,
!
!                 R = [r]  [s]  [t] ,
!                        a    b    a
!
!              the range of ANGLE2 is [0, pi].
!
!              For factorizations in which the first and third
!              axes are different,
!
!                 R = [r]  [s]  [t] ,
!                        a    b    c
!
!              the range of ANGLE2 is [-pi/2, pi/2].
!
!              For rotations such that ANGLE3 and ANGLE1 are not
!              uniquely determined, ANGLE3 will always be set to
!              zero; ANGLE1 is then uniquely determined.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If any of AXIS3, AXIS2, or AXIS1 do not have values in
!
!            { 1, 2, 3 }
!
!         the error SPICE(BADAXISNUMBERS) is signaled.
!
!     2)  If AXIS2 is equal to AXIS3 or AXIS1, the error
!         SPICE(BADAXISNUMBERS) is signaled. An arbitrary rotation
!         matrix cannot be expressed using a sequence of Euler angles
!         unless the second rotation axis differs from the other two.
!
!     3)  If the input matrix R is not a rotation matrix, the error
!         SPICE(NOTAROTATION) is signaled.
!
!     4)  If ANGLE3 and ANGLE1 are not uniquely determined, ANGLE3
!         is set to zero, and ANGLE1 is determined.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     A word about notation: the symbol
!
!        [ x ]
!             i
!
!     indicates a coordinate system rotation of x radians about the
!     ith coordinate axis. To be specific, the symbol
!
!        [ x ]
!             1
!
!     indicates a coordinate system rotation of x radians about the
!     first, or x-, axis; the corresponding matrix is
!
!        .-                    -.
!        |  1      0       0    |
!        |                      |
!        |  0    cos(x)  sin(x) |
!        |                      |
!        |  0   -sin(x)  cos(x) |
!        `-                    -'
!
!     Remember, this is a COORDINATE SYSTEM rotation by x radians; this
!     matrix, when applied to a vector, rotates the vector by -x
!     radians, not x radians. Applying the matrix to a vector yields
!     the vector's representation relative to the rotated coordinate
!     system.
!
!     The analogous rotation about the second, or y-, axis is
!     represented by
!
!        [ x ]
!             2
!
!     which symbolizes the matrix
!
!        .-                    -.
!        | cos(x)   0   -sin(x) |
!        |                      |
!        |  0       1      0    |
!        |                      |
!        | sin(x)   0    cos(x) |
!        `-                    -'
!
!     and the analogous rotation about the third, or z-, axis is
!     represented by
!
!        [ x ]
!             3
!
!     which symbolizes the matrix
!
!        .-                    -.
!        |  cos(x)  sin(x)   0  |
!        |                      |
!        | -sin(x)  cos(x)   0  |
!        |                      |
!        |  0        0       1  |
!        `-                    -'
!
!
!     The input matrix is assumed to be the product of three
!     rotation matrices, each one of the form
!
!        .-                    -.
!        |  1      0       0    |
!        |                      |
!        |  0    cos(r)  sin(r) |     (rotation of r radians about the
!        |                      |      x-axis),
!        |  0   -sin(r)  cos(r) |
!        `-                    -'
!
!
!        .-                    -.
!        | cos(s)   0   -sin(s) |
!        |                      |
!        |  0       1      0    |     (rotation of s radians about the
!        |                      |      y-axis),
!        | sin(s)   0    cos(s) |
!        `-                    -'
!
!     or
!
!        .-                    -.
!        |  cos(t)  sin(t)   0  |
!        |                      |
!        | -sin(t)  cos(t)   0  |     (rotation of t radians about the
!        |                      |      z-axis),
!        |  0        0       1  |
!        `-                    -'
!
!     where the second rotation axis is not equal to the first or
!     third. Any rotation matrix can be factored as a sequence of
!     three such rotations, provided that this last criterion is met.
!
!     This routine is related to the SPICELIB routine EUL2M, which
!     produces a rotation matrix, given a sequence of Euler angles.
!     This routine is a `right inverse' of EUL2M:  the sequence of
!     calls
!
!        CALL M2EUL ( R,  AXIS3,   AXIS2,   AXIS1,
!       .                 ANGLE3,  ANGLE2,  ANGLE1     )
!
!        CALL EUL2M (     ANGLE3,  ANGLE2,  ANGLE1,
!       .                 AXIS3,   AXIS2,   AXIS1,   R )
!
!     preserves R, except for round-off error.
!
!
!     On the other hand, the sequence of calls
!
!        CALL EUL2M (     ANGLE3,  ANGLE2,  ANGLE1,
!       .                 AXIS3,   AXIS2,   AXIS1,   R )
!
!        CALL M2EUL ( R,  AXIS3,   AXIS2,   AXIS1,
!       .                 ANGLE3,  ANGLE2,  ANGLE1     )
!
!
!     preserve ANGLE3, ANGLE2, and ANGLE1 only if these angles start
!     out in the ranges that M2EUL's outputs are restricted to.
!
! $ Examples
!
!     The numerical results shown for these examples may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Conversion of instrument pointing from a matrix representation
!        to Euler angles:
!
!        Suppose we want to find camera pointing in alpha, delta, and
!        kappa, given the inertial-to-camera coordinate transformation
!
!
!        .-                                                         -.
!        |  0.491273796781358  0.508726203218642  0.706999085398824  |
!        |                                                           |
!        | -0.508726203218642 -0.491273796781358  0.706999085398824  |
!        |                                                           |
!        |  0.706999085398824 -0.706999085398824  0.017452406437284  |
!        `-                                                         -'
!
!
!        Find angles alpha, delta, kappa such that
!
!           TICAM  =  [ kappa ]  [ pi/2 - delta ]  [ pi/2 + alpha ] .
!                              3                 1                 3
!
!        Example code begins here.
!
!
!              PROGRAM M2EUL_EX1
!              IMPLICIT NONE
!
!        C
!        C     SPICELIB functions
!        C
!              DOUBLE PRECISION      DPR
!              DOUBLE PRECISION      HALFPI
!              DOUBLE PRECISION      TWOPI
!
!        C
!        C     Local variables
!        C
!              DOUBLE PRECISION      ALPHA
!              DOUBLE PRECISION      ANG1
!              DOUBLE PRECISION      ANG2
!              DOUBLE PRECISION      DELTA
!              DOUBLE PRECISION      KAPPA
!              DOUBLE PRECISION      TICAM  ( 3, 3 )
!
!
!              DATA TICAM /  0.491273796781358D0,
!             .             -0.508726203218642D0,
!             .              0.706999085398824D0,
!             .              0.508726203218642D0,
!             .             -0.491273796781358D0,
!             .             -0.706999085398824D0,
!             .              0.706999085398824D0,
!             .              0.706999085398824D0,
!             .              0.017452406437284D0  /
!
!
!              CALL M2EUL ( TICAM, 3, 1, 3, KAPPA, ANG2, ANG1 )
!
!              DELTA = HALFPI() - ANG2
!              ALPHA = ANG1     - HALFPI()
!
!              IF ( KAPPA .LT. 0.D0 ) THEN
!                 KAPPA = KAPPA + TWOPI()
!              END IF
!
!              IF ( ALPHA .LT. 0.D0 ) THEN
!                 ALPHA = ALPHA + TWOPI()
!              END IF
!
!              WRITE (*,'(A,F24.14)') 'Alpha (deg): ', DPR() * ALPHA
!              WRITE (*,'(A,F24.14)') 'Delta (deg): ', DPR() * DELTA
!              WRITE (*,'(A,F24.14)') 'Kappa (deg): ', DPR() * KAPPA
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        Alpha (deg):       315.00000000000000
!        Delta (deg):         1.00000000000003
!        Kappa (deg):        45.00000000000000
!
!
!     2) Conversion of instrument pointing angles from a non-J2000,
!        not necessarily inertial frame to J2000-relative RA, Dec,
!        and Twist.
!
!        Suppose that we have orientation for the CASSINI Narrow Angle
!        Camera (NAC) frame expressed as
!
!           [ gamma ]  [ beta ]  [ alpha ]
!                    1         2          3
!
!        with respect to the CASSINI spacecraft frame.
!
!        We want to express that orientation with respect to the J2000
!        frame as the sequence of rotations
!
!           [ twist ]  [ dec ]  [ ra ] .
!                    3        1       3
!
!        Use the meta-kernel shown below to load the required SPICE
!        kernels.
!
!
!           KPL/MK
!
!           File name: m2eul_ex2.tm
!
!           This meta-kernel is intended to support operation of SPICE
!           example programs. The kernels shown here should not be
!           assumed to contain adequate or correct versions of data
!           required by SPICE-based user applications.
!
!           In order for an application to use this meta-kernel, the
!           kernels referenced here must be present in the user's
!           current working directory.
!
!           The names and contents of the kernels referenced
!           by this meta-kernel are as follows:
!
!              File name                      Contents
!              ---------                      --------
!              naif0010.tls                   Leapseconds
!              cas00145.tsc                   Cassini SCLK
!              cas_v40.tf                     Cassini frames
!              08052_08057ra.bc               Orientation for Cassini
!
!           \begindata
!
!              KERNELS_TO_LOAD = ( 'naif0010.tls'
!                                  'cas00145.tsc'
!                                  'cas_v40.tf'
!                                  '08052_08057ra.bc')
!
!           \begintext
!
!           End of meta-kernel
!
!
!        Example code begins here.
!
!
!              PROGRAM M2EUL_EX2
!              IMPLICIT NONE
!
!        C
!        C     SPICELIB functions
!        C
!              DOUBLE PRECISION      DPR
!              DOUBLE PRECISION      HALFPI
!              DOUBLE PRECISION      RPD
!              DOUBLE PRECISION      TWOPI
!
!        C
!        C     Local parameters
!        C
!              CHARACTER*(*)         META
!              PARAMETER           ( META   =  'm2eul_ex2.tm' )
!
!              DOUBLE PRECISION      ALPHA
!              PARAMETER           ( ALPHA =  89.9148D0   )
!
!              DOUBLE PRECISION      BETA
!              PARAMETER           ( BETA  = -0.03300D0   )
!
!              DOUBLE PRECISION      GAMMA
!              PARAMETER           ( GAMMA = -90.009796D0 )
!
!        C
!        C     Local variables
!        C
!              DOUBLE PRECISION      RA
!              DOUBLE PRECISION      ANG1
!              DOUBLE PRECISION      ANG2
!              DOUBLE PRECISION      DEC
!              DOUBLE PRECISION      ET
!              DOUBLE PRECISION      INST2J ( 3, 3 )
!              DOUBLE PRECISION      INST2S ( 3, 3 )
!              DOUBLE PRECISION      S2J    ( 3, 3 )
!              DOUBLE PRECISION      TWIST
!
!        C
!        C     Load the kernels.
!        C
!              CALL FURNSH ( META )
!
!        C
!        C     First, we use subroutine EUL2M to form the
!        C     transformation from instrument coordinates to
!        C     CASSINI spacecraft frame.
!        C
!              CALL EUL2M ( GAMMA * RPD(), BETA * RPD(), ALPHA * RPD(),
!             .             1,             2,            3,    INST2S )
!
!        C
!        C     Now we compute the transformation from CASSINI
!        C     spacecraft frame to J2000, at a given time.
!        C
!              CALL STR2ET ( '2008 Feb 25', ET )
!              CALL PXFORM ( 'CASSINI_SC_COORD', 'J2000', ET, S2J )
!
!        C
!        C     Next, we form the transformation from instrument
!        C     coordinates to J2000 frame.
!        C
!              CALL MXM ( S2J, INST2S, INST2J )
!
!        C
!        C     Finally, we express INST2J using the desired Euler
!        C     angles.
!        C
!              CALL M2EUL ( INST2J, 3, 1, 3, TWIST, ANG1, ANG2 )
!
!              RA   =  ANG2 - HALFPI()
!              DEC  =  HALFPI() - ANG1
!
!        C
!        C     If we wish to make sure that RA, DEC, and TWIST are in
!        C     the ranges [0, 2pi), [-pi/2, pi/2], and [0, 2pi)
!        C     respectively, we may add the code
!        C
!              IF ( RA .LT. 0.D0 ) THEN
!                 RA = RA + TWOPI()
!              END IF
!
!              IF ( TWIST .LT. 0.D0 ) THEN
!                 TWIST = TWIST + TWOPI()
!              END IF
!
!        C
!        C     Now RA, DEC, and TWIST express the instrument pointing
!        C     as RA, Dec, and Twist, relative to the J2000 system.
!        C
!              WRITE (*,'(A,F24.14)') 'RA    (deg): ', DPR() * RA
!              WRITE (*,'(A,F24.14)') 'Dec   (deg): ', DPR() * DEC
!              WRITE (*,'(A,F24.14)') 'Twist (deg): ', DPR() * TWIST
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        RA    (deg):        83.77802387778848
!        Dec   (deg):       -14.92925498590898
!        Twist (deg):       294.55732942050986
!
!
!        Note:  more than one definition of RA, Dec, and Twist is
!        extant. Before using this example in an application, check
!        that the definition given here is consistent with that used
!        in your application.
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
!-    SPICELIB Version 1.3.0, 06-JUL-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Removed
!        unnecessary entries from $Revisions section.
!
!        Added complete code examples from existing fragments.
!
!        Removed erroneous comments about behavior of ATAN2.
!
!-    SPICELIB Version 1.2.1, 21-DEC-2006 (NJB)
!
!        Error corrected in header example: input matrix
!        previously did not match shown outputs. Offending
!        example now includes complete program.
!
!-    SPICELIB Version 1.2.0, 15-OCT-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in MXM and MTXM calls. A short error message cited in
!        the $Exceptions section of the header failed to match
!        the actual short message used; this has been corrected.
!
!-    SPICELIB Version 1.1.2, 13-OCT-2004 (NJB)
!
!        Fixed header typo.
!
!-    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.1.0, 02-NOV-1990 (NJB)
!
!        Header upgraded to describe notation in more detail. Argument
!        names were changed to describe the use of the arguments more
!        accurately. No change in functionality was made; the operation
!        of the routine is identical to that of the previous version.
!
!-    SPICELIB Version 1.0.0, 03-SEP-1990 (NJB)
!
!-&


! $ Index_Entries
!
!     matrix to euler angles
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.1.0, 02-NOV-1990 (NJB)
!
!        Argument names were changed to describe the use of the
!        arguments more accurately. The axis and angle numbers
!        now decrease, rather than increase, from left to right.
!        The current names reflect the order of operator application
!        when the Euler angle rotations are applied to a vector: the
!        rightmost matrix
!
!           [ ANGLE1 ]
!                     AXIS1
!
!        is applied to the vector first, followed by
!
!           [ ANGLE2 ]
!                     AXIS2
!
!        and then
!
!           [ ANGLE3 ]
!                     AXIS3
!
!        Previously, the names reflected the order in which the Euler
!        angle matrices appear on the page, from left to right. This
!        naming convention was found to be a bit obtuse by a various
!        users.
!
!        No change in functionality was made; the operation of the
!        routine is identical to that of the previous version.
!
!        Also, the header was upgraded to describe the notation in more
!        detail. The symbol
!
!           [ x ]
!                i
!
!        is explained at mind-numbing length. An example was added
!        that shows a specific set of inputs and the resulting output
!        matrix.
!
!        The angle sequence notation was changed to be consistent with
!        Rotations required reading.
!
!          1-2-3  and  a-b-c
!
!        have been changed to
!
!          3-2-1  and  c-b-a.
!
!       Also, one `)' was changed to a `}'.
!
!       The phrase `first and third' was changed to `first or third'
!       in the $Particulars section, where the criterion for the
!       existence of an Euler angle factorization is stated.
!
!-&


!
!     SPICELIB functions
!
      logical               isrot
      logical               return

!
!     Local parameters
!

!
!     NTOL and DETOL are used to determine whether R is a rotation
!     matrix.
!
!     NTOL is the tolerance for the norms of the columns of R.
!
!     DTOL is the tolerance for the determinant of a matrix whose
!     columns are the unitized columns of R.
!
!
      real(wp), parameter :: ntol = 0.1_wp

      real(wp), parameter :: dtol = 0.1_wp


!
!     Local variables
!
      real(wp)              change ( 3, 3 )
      real(wp)              sign
      real(wp)              tmpmat ( 3, 3 )
      real(wp)              tmprot ( 3, 3 )

      integer(ip)           c
      integer(ip)           i
      integer(ip)           next   ( 3 )

      logical               degen

!
!     Saved variables
!
      save                  next

!
!     Initial values
!
      data  next / 2, 3, 1 /


!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     The first order of business is to screen out the goofy cases.
!
!     Make sure the axis numbers are all right:  They must belong to
!     the set {1, 2, 3}...
!
      if (      (  ( axis3 < 1 ) .or. ( axis3 > 3 )  ) &
           .or. (  ( axis2 < 1 ) .or. ( axis2 > 3 )  ) &
           .or. (  ( axis1 < 1 ) .or. ( axis1 > 3 )  )   ) then

         call setmsg ( 'Axis numbers are #,  #,  #. ' )
         call errint ( '#',  axis3                    )
         call errint ( '#',  axis2                    )
         call errint ( '#',  axis1                    )
         call sigerr ( 'SPICE(BADAXISNUMBERS)'        )
         return

!
!     ...and the second axis number must differ from its neighbors.
!
      else if ( ( axis3 == axis2 ) .or. ( axis1 == axis2 ) ) then

         call setmsg ( 'Middle axis matches neighbor: # # #.' )
         call errint ( '#',  axis3                            )
         call errint ( '#',  axis2                            )
         call errint ( '#',  axis1                            )
         call sigerr ( 'SPICE(BADAXISNUMBERS)'                )
         return

!
!     R must be a rotation matrix, or we may as well forget it.
!
      else if (  .not. isrot ( r, ntol, dtol )  ) then

         call setmsg ( 'Input matrix is not a rotation.' )
         call sigerr ( 'SPICE(NOTAROTATION)'             )
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
         call cleard ( 9, change )

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
!
!        but if a2 is 0 or pi, the product matrix reduces to
!
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
         call cleard ( 9, change )

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
!
!        but if a2 is -pi/2 or pi/2, the product matrix reduces to
!
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
!
!        We take care of this case first.  We test three conditions
!        that are mathematically equivalent, but may not be satisfied
!        simultaneously because of round-off:
!
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
      end
