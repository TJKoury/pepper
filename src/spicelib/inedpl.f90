!$Procedure INEDPL ( Intersection of ellipsoid and plane )

      subroutine inedpl ( a, b, c, plane, ellips, found )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Find the intersection of a triaxial ellipsoid and a plane.
!
! $ Required_Reading
!
!     ELLIPSES
!     PLANES
!
! $ Keywords
!
!     ELLIPSE
!     ELLIPSOID
!     GEOMETRY
!     MATH
!
! $ Declarations

      implicit none

      integer(ip), parameter :: ubel = 9

      integer(ip), parameter :: ubpl = 4

      real(wp)              a
      real(wp)              b
      real(wp)              c
      real(wp)              plane  ( ubpl )
      real(wp)              ellips ( ubel )
      logical               found

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     A          I   Length of ellipsoid semi-axis lying on the x-axis.
!     B          I   Length of ellipsoid semi-axis lying on the y-axis.
!     C          I   Length of ellipsoid semi-axis lying on the z-axis.
!     PLANE      I   Plane that intersects ellipsoid.
!     ELLIPS     O   Intersection ellipse, when FOUND is .TRUE.
!     FOUND      O   Flag indicating whether ellipse was found.
!
! $ Detailed_Input
!
!     A,
!     B,
!     C        are the lengths of the semi-axes of a triaxial
!              ellipsoid. The ellipsoid is centered at the
!              origin and oriented so that its axes lie on the
!              x, y and z axes.  A, B, and C are the lengths of
!              the semi-axes that point in the x, y, and z
!              directions respectively.
!
!     PLANE    is a SPICE plane.
!
! $ Detailed_Output
!
!     ELLIPS   is the SPICE ellipse formed by the intersection
!              of the input plane and ellipsoid. ELLIPS will
!              represent a single point if the ellipsoid and
!              plane are tangent.
!
!              If the intersection of the ellipsoid and plane is
!              empty, ELLIPS is not modified.
!
!
!     FOUND    is .TRUE. if and only if the intersection of the
!              ellipsoid and plane is non-empty.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If any of the lengths of the semi-axes of the input ellipsoid
!         are non-positive, the error SPICE(DEGENERATECASE) is
!         signaled. ELLIPS is not modified. FOUND is set to .FALSE.
!
!     2)  If the input plane in invalid, in other words, if the input
!         plane as the zero vector as its normal vector, the error
!         SPICE(INVALIDPLANE) is signaled. ELLIPS is not modified.
!         FOUND is set to .FALSE.
!
!     3)  If the input plane and ellipsoid are very nearly tangent,
!         roundoff error may cause this routine to give unreliable
!         results.
!
!     4)  If the input plane and ellipsoid are precisely tangent, the
!         intersection is a single point. In this case, the output
!         ellipse is degenerate, but FOUND will still have the value
!         .TRUE. You must decide whether this output makes sense for
!         your application.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     An ellipsoid and a plane can intersect in an ellipse, a single
!     point, or the empty set.
!
! $ Examples
!
!     The numerical results shown for these examples may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Suppose we wish to find the limb of a body, as observed from
!        location LOC in body-fixed coordinates. The SPICELIB routine
!        EDLIMB solves this problem. Here's how INEDPL is used in
!        that solution.
!
!        We assume LOC is outside of the body. The body is modeled as
!        a triaxial ellipsoid with semi-axes of length A, B, and C.
!
!        The notation
!
!           < X, Y >
!
!        indicates the inner product of the vectors X and Y.
!
!        The limb lies on the plane defined by
!
!           < X,  N >  =  1,
!
!        where the vector N is defined as
!
!                       2              2              2
!           ( LOC(1) / A ,   LOC(2) / B ,   LOC(3) / C  ).
!
!        The assignments
!
!           N(1) = LOC(1) / ( A*A )
!           N(2) = LOC(2) / ( B*B )
!           N(3) = LOC(3) / ( C*C )
!
!        and the calls
!
!           CALL NVC2PL ( N,  1.0D0,  PLANE )
!
!           CALL INEDPL ( A,  B,  C,  PLANE,  LIMB,  FOUND )
!
!           CALL EL2CGV ( LIMB, CENTER, SMAJOR, SMINOR )
!
!        will return the center and semi-axes of the limb.
!
!
!        How do we know that  < X, N > = 1  for all X on the limb?
!        This is because all limb points X satisfy
!
!           < LOC - X, SURFNM(X) >  =  0,
!
!        where SURFNM(X) is a surface normal at X.  SURFNM(X) is
!        parallel to the vector
!
!                          2            2            2
!           V = (  X(1) / A ,   X(2) / B ,   X(3) / C   )
!
!        so we have
!
!           < LOC - X, V >  =  0,
!
!           < LOC, V >      =  < X, V >  =  1  (from the original
!                                               ellipsoid
!                                               equation);
!        and finally
!
!           < X,   N >      =  1,
!
!        where N is as defined above.
!
!
!     2) We'd like to find the apparent limb of Jupiter, corrected for
!        light time and stellar aberration, as seen from JUNO
!        spacecraft's position at a given UTC time.
!
!        This example is equivalent to the one in EDLIMB, but it uses
!        INEDPL to compute the limb.
!
!
!        Use the meta-kernel shown below to load the required SPICE
!        kernels.
!
!
!           KPL/MK
!
!           File name: inedpl_ex2.tm
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
!              File name                           Contents
!              ---------                           --------
!              juno_rec_160522_160729_160909.bsp   JUNO s/c ephemeris
!              pck00010.tpc                        Planet orientation
!                                                  and radii
!              naif0012.tls                        Leapseconds
!
!           \begindata
!
!              KERNELS_TO_LOAD = ( 'juno_rec_160522_160729_160909.bsp',
!                                  'pck00010.tpc',
!                                  'naif0012.tls'  )
!
!           \begintext
!
!           End of meta-kernel
!
!
!        Example code begins here.
!
!
!              PROGRAM INEDPL_EX2
!              IMPLICIT NONE
!
!        C
!        C     Local parameters.
!        C
!              CHARACTER*(*)           UTCSTR
!              PARAMETER             ( UTCSTR = '2016 Jul 14 19:45:00' )
!
!              INTEGER                 UBEL
!              PARAMETER             ( UBEL =   9 )
!
!              INTEGER                 UBPL
!              PARAMETER             ( UBPL =   4 )
!
!        C
!        C     Local variables.
!        C
!              DOUBLE PRECISION        CENTER ( 3    )
!              DOUBLE PRECISION        ET
!              DOUBLE PRECISION        JPOS   ( 3    )
!              DOUBLE PRECISION        LIMB   ( UBEL )
!              DOUBLE PRECISION        LT
!              DOUBLE PRECISION        PLANE  ( UBPL )
!              DOUBLE PRECISION        RAD    ( 3    )
!              DOUBLE PRECISION        SMAJOR ( 3    )
!              DOUBLE PRECISION        SMINOR ( 3    )
!              DOUBLE PRECISION        SCPOS  ( 3    )
!              DOUBLE PRECISION        TIPM   ( 3, 3 )
!
!              INTEGER                 N
!
!              LOGICAL                 FOUND
!
!        C
!        C     Load the required kernels.
!        C
!              CALL FURNSH ( 'inedpl_ex2.tm' )
!
!        C
!        C     Find the viewing point in Jupiter-fixed coordinates. To
!        C     do this, find the apparent position of Jupiter as seen
!        C     from the spacecraft in Jupiter-fixed coordinates and
!        C     negate this vector. In this case we'll use light time
!        C     and stellar aberration corrections to arrive at the
!        C     apparent limb. JPOS is the Jupiter's position as seen
!        C     from the spacecraft.  SCPOS is the spacecraft's position
!        C     relative to Jupiter.
!        C
!              CALL STR2ET ( UTCSTR,    ET )
!              CALL SPKPOS ( 'JUPITER', ET, 'J2000', 'LT+S', 'JUNO',
!             .               JPOS,     LT                         )
!
!              CALL VMINUS ( JPOS, SCPOS )
!
!        C
!        C     Get Jupiter's semi-axis lengths...
!        C
!              CALL BODVRD ( 'JUPITER', 'RADII', 3, N, RAD )
!
!        C
!        C     ...and the transformation from J2000 to Jupiter
!        C     equator and prime meridian coordinates. Note that we
!        C     use the orientation of Jupiter at the time of
!        C     emission of the light that arrived at the
!        C     spacecraft at time ET.
!        C
!              CALL PXFORM ( 'J2000', 'IAU_JUPITER', ET-LT, TIPM )
!
!        C
!        C     Transform the spacecraft's position into Jupiter-
!        C     fixed coordinates.
!        C
!              CALL MXV ( TIPM, SCPOS, SCPOS )
!
!        C
!        C     Normalize the position to factors of the radii.
!        C
!              SCPOS(1) = SCPOS(1) / RAD(1)**2
!              SCPOS(2) = SCPOS(2) / RAD(2)**2
!              SCPOS(3) = SCPOS(3) / RAD(3)**2
!
!        C
!        C     Find the apparent limb.  LIMB is a SPICE ellipse
!        C     representing the limb.
!        C
!              CALL NVC2PL ( SCPOS,  1.0D0,  PLANE  )
!              CALL INEDPL ( RAD(1), RAD(2), RAD(3),
!             .              PLANE,  LIMB,   FOUND  )
!
!        C
!        C     CENTER, SMAJOR, and SMINOR are the limb's center,
!        C     semi-major axis of the limb, and a semi-minor axis
!        C     of the limb.  We obtain these from LIMB using the
!        C     SPICELIB routine EL2CGV ( Ellipse to center and
!        C     generating vectors ).
!        C
!              CALL EL2CGV ( LIMB, CENTER, SMAJOR, SMINOR )
!
!        C
!        C     Output the structure components.
!        C
!              WRITE(*,'(A)') 'Apparent limb of Jupiter as seen '
!             .            // 'from JUNO:'
!              WRITE(*,'(2A)')       '   UTC time       : ', UTCSTR
!              WRITE(*,'(A,3F14.6)') '   Semi-minor axis:', SMINOR
!              WRITE(*,'(A,3F14.6)') '   Semi-major axis:', SMAJOR
!              WRITE(*,'(A,3F14.6)') '   Center         :', CENTER
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        Apparent limb of Jupiter as seen from JUNO:
!           UTC time       : 2016 Jul 14 19:45:00
!           Semi-minor axis:  12425.547643  -5135.572410  65656.053303
!           Semi-major axis:  27305.667297  66066.222576      0.000000
!           Center         :    791.732472   -327.228993   -153.408849
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     K.R. Gehringer     (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.3.0, 24-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!        Added complete code example.
!
!-    SPICELIB Version 1.2.0, 16-NOV-2005 (NJB)
!
!        Bug fix: error detection for case of invalid input plane was
!        added.
!
!        Updated to remove non-standard use of duplicate arguments
!        in VSCL calls.
!
!-    SPICELIB Version 1.1.0, 11-JUL-1995 (KRG)
!
!        Removed potential numerical precision problems that could be
!        caused by using a REAL constant in a double precision
!        computation. The value 1.0 was replaced with the value 1.0D0
!        in the following three lines:
!
!           DSTORT(1) = 1.0 / A
!           DSTORT(2) = 1.0 / B
!           DSTORT(3) = 1.0 / C
!
!        Also changed was a numeric constant from 1.D0 to the
!        equivalent, but more aesthetically pleasing 1.0D0.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 02-NOV-1990 (NJB)
!
!-&


! $ Index_Entries
!
!     intersection of ellipsoid and plane
!
!-&


!
!     SPICELIB functions
!
      real(wp)              brcktd
      real(wp)              vnorm

      logical               return
      logical               vzero

!
!     Local variables
!
      real(wp)              center ( 3 )
      real(wp)              const
      real(wp)              dplane ( ubpl )
      real(wp)              dist
      real(wp)              dstort ( 3 )
      real(wp)              invdst ( 3 )
      real(wp)              maxrad
      real(wp)              normal ( 3 )
      real(wp)              point  ( 3 )
      real(wp)              rcircl
      real(wp)              span1  ( 3 )
      real(wp)              span2  ( 3 )
      real(wp)              vec1   ( 3 )
      real(wp)              vec2   ( 3 )

      integer(ip)           i


!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     We don't want to worry about flat ellipsoids:
!
      if (         ( a <= 0.0_wp ) &
            .or.   ( b <= 0.0_wp ) &
            .or.   ( c <= 0.0_wp )   )   then

         found = .false.

         call setmsg ( 'Semi-axes: A = #,  B = #,  C = #.'  )
         call errdp  ( '#', a                               )
         call errdp  ( '#', b                               )
         call errdp  ( '#', c                               )
         call sigerr ( 'SPICE(DEGENERATECASE)'              )
         return

      end if

!
!     Check input plane for zero normal vector.
!
      call pl2nvc ( plane, normal, const )

      if ( vzero(normal) ) then

         call setmsg ( 'Normal vector of the input ' // &
                       'PLANE is the zero vector.'   )
         call sigerr ( 'SPICE(INVALIDPLANE)'         )
         return

      end if

!
!     This algorithm is partitioned into a series of steps:
!
!
!     1)  Identify a linear transformation that maps the input
!         ellipsoid to the unit sphere.  We'll call this mapping the
!         `distortion' mapping.  Apply the distortion mapping to both
!         the input plane and ellipsoid.  The image of the plane under
!         this transformation will be a plane.
!
!     2)  Find the intersection of the transformed plane and the unit
!         sphere.
!
!     3)  Apply the inverse of the distortion mapping to the
!         intersection ellipse to find the undistorted intersection
!         ellipse.
!


!
!     Step 1:
!
!     Find the image of the ellipsoid and plane under the distortion
!     matrix.  Since the image of the ellipsoid is the unit sphere,
!     only the plane transformation requires any work.
!
!     If the input plane is too far from the origin to possibly
!     intersect the ellipsoid, return now.  This can save us
!     some numerical problems when we scale the plane and ellipsoid.
!
!     The point returned by PL2PSV is the closest point in PLANE
!     to the origin, so its norm gives the distance of the plane
!     from the origin.
!
      call pl2psv  ( plane, point, span1, span2 )

      maxrad = max ( dabs(a), dabs(b), dabs(c) )

      if ( vnorm (point) > maxrad ) then

         found = .false.
         return

      end if

!
!     The distortion matrix and its inverse are
!
!        +-               -+        +-               -+
!        |  1/A   0    0   |        |   A    0    0   |
!        |   0   1/B   0   |,       |   0    B    0   |.
!        |   0    0   1/C  |        |   0    0    C   |
!        +-               -+        +-               -+
!
!     We declare them with length three, since we are going to make
!     use of the diagonal elements only.
!
      dstort(1) = 1.0_wp / a
      dstort(2) = 1.0_wp / b
      dstort(3) = 1.0_wp / c

      invdst(1) =  a
      invdst(2) =  b
      invdst(3) =  c

!
!     Apply the distortion mapping to the input plane.  Applying
!     the distortion mapping to a point and two spanning vectors that
!     define the input plane yields a point and two spanning vectors
!     that define the distorted plane.
!
      do i = 1, 3

         point(i)  =  dstort(i)  *  point(i)
         span1(i)  =  dstort(i)  *  span1(i)
         span2(i)  =  dstort(i)  *  span2(i)

      end do

      call psv2pl ( point,   span1,  span2,  dplane )

!
!     Step 2:
!
!     Find the intersection of the distorted plane and unit sphere.
!

!
!     The intersection of the distorted plane and the unit sphere
!     may be a circle, a point, or the empty set.  The distance of the
!     plane from the origin determines which type of intersection we
!     have.  If we represent the distorted plane by a unit normal
!     vector and constant, the size of the constant gives us the
!     distance of the plane from the origin.  If the distance is greater
!     than 1, the intersection of plane and unit sphere is empty. If
!     the distance is equal to 1, we have the tangency case.
!
!     The routine PL2PSV always gives us an output point that is the
!     closest point to the origin in the input plane.  This point is
!     the center of the intersection circle.  The spanning vectors
!     returned by PL2PSV, after we scale them by the radius of the
!     intersection circle, become an orthogonal pair of vectors that
!     extend from the center of the circle to the circle itself.  So,
!     the center and these scaled vectors define the intersection
!     circle.
!
      call pl2psv  ( dplane, center, vec1, vec2 )

      dist = vnorm ( center )

      if ( dist > 1.0_wp ) then

         found = .false.
         return

      end if

!
!     Scale the generating vectors by the radius of the intersection
!     circle.
!
      rcircl =  dsqrt (  brcktd ( 1.0_wp - dist**2,  0.0_wp,  1.0_wp  )  )

      call vsclip ( rcircl, vec1 )
      call vsclip ( rcircl, vec2 )

!
!     Step 3:
!
!     Apply the inverse distortion to the intersection circle to find
!     the actual intersection ellipse.
!
      do i = 1, 3

         center(i)  =  invdst(i)  *  center(i)
         vec1(i)    =  invdst(i)  *  vec1(i)
         vec2(i)    =  invdst(i)  *  vec2(i)

      end do
!
!     Make an ellipse from the center and generating vectors.
!
      call cgv2el ( center, vec1, vec2, ellips )

      found  = .true.

      return
      end
