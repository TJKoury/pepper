!$Procedure EDLIMB   ( Ellipsoid Limb )

      SUBROUTINE EDLIMB ( A, B, C, VIEWPT, LIMB )

! $ Abstract
!
!     Find the limb of a triaxial ellipsoid, viewed from a specified
!     point.
!
! $ Required_Reading
!
!     ELLIPSES
!
! $ Keywords
!
!     ELLIPSE
!     ELLIPSOID
!     GEOMETRY
!     MATH
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               UBEL
      PARAMETER           ( UBEL   =    9 )

      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      VIEWPT (    3 )
      DOUBLE PRECISION      LIMB   ( UBEL )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     A          I   Length of ellipsoid semi-axis lying on the x-axis.
!     B          I   Length of ellipsoid semi-axis lying on the y-axis.
!     C          I   Length of ellipsoid semi-axis lying on the z-axis.
!     VIEWPT     I   Location of viewing point.
!     LIMB       O   Limb of ellipsoid as seen from viewing point.
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
!     VIEWPT   is a point from which the ellipsoid is viewed.
!              VIEWPT must be outside of the ellipsoid.
!
! $ Detailed_Output
!
!     LIMB     is a SPICE ellipse that represents the limb of
!              the ellipsoid.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the length of any semi-axis of the ellipsoid is
!         non-positive, the error SPICE(INVALIDAXISLENGTH) is signaled.
!         LIMB is not modified.
!
!     2)  If the length of any semi-axis of the ellipsoid is zero after
!         the semi-axis lengths are scaled by the reciprocal of the
!         magnitude of the longest semi-axis and then squared, the error
!         SPICE(DEGENERATECASE) is signaled. LIMB is not modified.
!
!     3)  If the viewing point VIEWPT is inside the ellipse, the error
!         SPICE(INVALIDPOINT) is signaled. LIMB is not modified.
!
!     4)  If the geometry defined by the input ellipsoid and viewing
!         point is so extreme that the limb cannot be found, the error
!         SPICE(DEGENERATECASE) is signaled.
!
!     5)  If the shape of the ellipsoid and the viewing geometry are
!         such that the limb is an excessively flat ellipsoid, the
!         limb may be a degenerate ellipse. You must determine whether
!         this possibility poses a problem for your application.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The limb of a body, as seen from a viewing point, is the boundary
!     of the portion of the body's surface that is visible from that
!     viewing point. In this definition, we consider a surface point
!     to be `visible' if it can be connected to the viewing point by a
!     line segment that doesn't pass through the body. This is a purely
!     geometrical definition that ignores the matter of which portions
!     of the surface are illuminated, or whether the view is obscured by
!     any additional objects.
!
!     If a body is modeled as a triaxial ellipsoid, the limb is always
!     an ellipse. The limb is determined by its center, a semi-major
!     axis vector, and a semi-minor axis vector.
!
!     We note that the problem of finding the limb of a triaxial
!     ellipsoid is mathematically identical to that of finding its
!     terminator, if one makes the simplifying assumption that the
!     terminator is the limb of the body as seen from the vertex of the
!     umbra. So, this routine can be used to solve this simplified
!     version of the problem of finding the terminator.
!
! $ Examples
!
!     The numerical results shown for these examples may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Given an ellipsoid and a viewpoint exterior to it, calculate
!        the limb ellipse as seen from that viewpoint.
!
!
!        Example code begins here.
!
!
!              PROGRAM EDLIMB_EX1
!              IMPLICIT NONE
!
!        C
!        C     Local constants.
!        C
!              INTEGER                 UBEL
!              PARAMETER             ( UBEL =   9 )
!
!        C
!        C     Local variables.
!        C
!              DOUBLE PRECISION        A
!              DOUBLE PRECISION        B
!              DOUBLE PRECISION        C
!              DOUBLE PRECISION        ECENTR ( 3    )
!              DOUBLE PRECISION        LIMB   ( UBEL )
!              DOUBLE PRECISION        SMAJOR ( 3    )
!              DOUBLE PRECISION        SMINOR ( 3    )
!              DOUBLE PRECISION        VIEWPT ( 3    )
!
!        C
!        C     Define a viewpoint exterior to the ellipsoid.
!        C
!              DATA                    VIEWPT /  2.D0,  0.D0,  0.D0 /
!
!        C
!        C     Define an ellipsoid.
!        C
!              A = SQRT( 2.D0 )
!              B = 2.D0 * SQRT( 2.D0 )
!              C = SQRT( 2.D0 )
!
!        C
!        C     Calculate the limb ellipse as seen by from the
!        C     viewpoint.
!        C
!              CALL EDLIMB ( A, B, C, VIEWPT, LIMB )
!
!        C
!        C     Output the structure components.
!        C
!              CALL EL2CGV ( LIMB, ECENTR, SMAJOR, SMINOR )
!
!              WRITE(*,'(A)') 'Limb ellipse as seen from viewpoint:'
!              WRITE(*,'(A,3F11.6)') '   Semi-minor axis:', SMINOR
!              WRITE(*,'(A,3F11.6)') '   Semi-major axis:', SMAJOR
!              WRITE(*,'(A,3F11.6)') '   Center         :', ECENTR
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        Limb ellipse as seen from viewpoint:
!           Semi-minor axis:   0.000000   0.000000  -1.000000
!           Semi-major axis:   0.000000   2.000000  -0.000000
!           Center         :   1.000000   0.000000   0.000000
!
!
!     2) We'd like to find the apparent limb of Jupiter, corrected for
!        light time and stellar aberration, as seen from JUNO
!        spacecraft's position at a given UTC time.
!
!        Use the meta-kernel shown below to load the required SPICE
!        kernels.
!
!
!           KPL/MK
!
!           File name: edlimb_ex2.tm
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
!              PROGRAM EDLIMB_EX2
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
!        C
!        C     Local variables.
!        C
!              DOUBLE PRECISION        CENTER ( 3    )
!              DOUBLE PRECISION        ET
!              DOUBLE PRECISION        JPOS   ( 3    )
!              DOUBLE PRECISION        LIMB   ( UBEL )
!              DOUBLE PRECISION        LT
!              DOUBLE PRECISION        RAD    ( 3    )
!              DOUBLE PRECISION        SMAJOR ( 3    )
!              DOUBLE PRECISION        SMINOR ( 3    )
!              DOUBLE PRECISION        SCPJFC ( 3    )
!              DOUBLE PRECISION        SCPOS  ( 3    )
!              DOUBLE PRECISION        TIPM   ( 3, 3 )
!
!              INTEGER                 N
!
!        C
!        C     Load the required kernels.
!        C
!              CALL FURNSH ( 'edlimb_ex2.tm' )
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
!              CALL MXV ( TIPM, SCPOS, SCPJFC )
!
!        C
!        C     Find the apparent limb.  LIMB is a SPICE ellipse
!        C     representing the limb.
!        C
!              CALL EDLIMB ( RAD(1), RAD(2), RAD(3), SCPJFC, LIMB )
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
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.4.0, 24-AUG-2021 (NJB) (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!        Added complete code example.
!
!        Corrected several header comment typos.
!
!-    SPICELIB Version 1.3.0, 23-OCT-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in VSCLG call. Updated header to refer to BODVCD instead
!        of BODVAR.
!
!-    SPICELIB Version 1.2.0, 06-OCT-1993 (NJB)
!
!        Declaration of unused local variable NEAR was removed.
!
!-    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.1.0, 04-DEC-1990 (NJB)
!
!        Error message and description changed for non-positive
!        axis length error.
!
!-    SPICELIB Version 1.0.0, 02-NOV-1990 (NJB)
!
!-&


! $ Index_Entries
!
!     ellipsoid limb
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.1.0, 04-DEC-1990 (NJB)
!
!        Error message and description changed for non-positive
!        axis length error. The former message and description did
!        not match, and the description was incorrect: it described
!        `zero-length', rather than `non-positive' axes as invalid.
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               RETURN

!
!     Local parameters
!
      INTEGER               UBPL
      PARAMETER           ( UBPL   =    4 )


!
!     Local variables
!
      DOUBLE PRECISION      LEVEL
      DOUBLE PRECISION      LPLANE ( UBPL )
      DOUBLE PRECISION      NORMAL (    3 )
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      SCLA
      DOUBLE PRECISION      SCLA2
      DOUBLE PRECISION      SCLB
      DOUBLE PRECISION      SCLB2
      DOUBLE PRECISION      SCLC
      DOUBLE PRECISION      SCLC2
      DOUBLE PRECISION      TMPEL  ( UBEL )
      DOUBLE PRECISION      V      (    3 )

      LOGICAL               FOUND


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'EDLIMB' )
      END IF

!
!     The semi-axes must have positive length.
!
      IF (       ( A .LE. 0.D0 ) &
           .OR.  ( B .LE. 0.D0 ) &
           .OR.  ( C .LE. 0.D0 )   )   THEN

         CALL SETMSG ( 'Semi-axis lengths:  A = #, B = #, C = #. ' )
         CALL ERRDP  ( '#', A                                      )
         CALL ERRDP  ( '#', B                                      )
         CALL ERRDP  ( '#', C                                      )
         CALL SIGERR ( 'SPICE(INVALIDAXISLENGTH)'                  )
         CALL CHKOUT ( 'EDLIMB'                                    )
         RETURN

      END IF

!
!     Scale the semi-axes lengths for better numerical behavior.
!     If squaring any one of the scaled lengths causes it to
!     underflow to zero, we cannot continue the computation. Otherwise,
!     scale the viewing point too.
!
      SCALE  =  MAX ( DABS(A), DABS(B), DABS(C) )

      SCLA   =  A / SCALE
      SCLB   =  B / SCALE
      SCLC   =  C / SCALE

      SCLA2  =  SCLA**2
      SCLB2  =  SCLB**2
      SCLC2  =  SCLC**2

      IF (       ( SCLA2  .EQ.  0.D0 ) &
           .OR.  ( SCLB2  .EQ.  0.D0 ) &
           .OR.  ( SCLC2  .EQ.  0.D0 )   )   THEN

         CALL SETMSG ( 'Semi-axis too small:  A = #, B = #, C = #. ' )
         CALL ERRDP  ( '#', A                                        )
         CALL ERRDP  ( '#', B                                        )
         CALL ERRDP  ( '#', C                                        )
         CALL SIGERR ( 'SPICE(DEGENERATECASE)'                       )
         CALL CHKOUT ( 'EDLIMB'                                      )
         RETURN

      END IF

      CALL VSCL ( 1.0D0 / SCALE, VIEWPT, V )

!
!     The viewing point must be outside of the ellipsoid.  LEVEL is the
!     constant of the level surface that V lies on.  The ellipsoid
!     itself is the level surface corresponding to LEVEL = 1.
!
      LEVEL   =     ( V(1)**2 / SCLA2 ) &
                 +  ( V(2)**2 / SCLB2 ) &
                 +  ( V(3)**2 / SCLC2 )

      IF ( LEVEL .LT. 1.D0 ) THEN

         CALL SETMSG ( 'Viewing point is inside the ellipsoid.' )
         CALL SIGERR ( 'SPICE(DEGENERATECASE)'                  )
         CALL CHKOUT ( 'EDLIMB'                                 )
         RETURN

      END IF

!
!     Find a normal vector for the limb plane.
!
!     To compute this vector, we use the fact that the surface normal at
!     each limb point is orthogonal to the line segment connecting the
!     viewing point and the limb point.   Let the notation
!
!        < a, b >
!
!     indicate the dot product of the vectors a and b.  If we call the
!     viewing point V and the limb point X, then
!
!
!
!                            X(1)         X(2)         X(3)
!        0  =   < V - X,  ( -------- ,   -------- ,   --------  )  >
!                                2           2             2
!                            SCLA        SCLB          SCLC
!
!
!                            X(1)         X(2)         X(3)
!           =   <   V,    ( -------- ,   -------- ,   --------  )  >
!                                2           2             2
!                            SCLA        SCLB          SCLC
!
!
!                            X(1)         X(2)         X(3)
!            - <   X,    ( -------- ,   -------- ,   --------  )  >
!                                2           2             2
!                            SCLA        SCLB          SCLC
!
!                                2           2             2
!                            X(1)        X(2)          X(3)
!           =             --------  +   --------  +  --------
!                               2            2             2
!                           SCLA         SCLB          SCLC
!
!
!           =   1
!
!
!     This last equation is just the equation of the scaled ellipsoid.
!     We can combine the last two equalities and interchange the
!     positions of X and V to obtain
!
!
!                      V(1)         V(2)         V(3)
!        <   X,    ( -------- ,   -------- ,   --------  )  >   =   1
!                          2           2             2
!                      SCLA        SCLB          SCLC
!
!
!     This is the equation of the limb plane.
!

!
!     Put together a SPICE plane, LPLANE, that represents the limb
!     plane.
!
      NORMAL(1)  =  V(1) / SCLA2
      NORMAL(2)  =  V(2) / SCLB2
      NORMAL(3)  =  V(3) / SCLC2

      CALL NVC2PL ( NORMAL, 1.0D0, LPLANE )

!
!     Find the limb by intersecting the limb plane with the ellipsoid.
!
      CALL INEDPL ( SCLA,  SCLB,  SCLC,  LPLANE,  LIMB,  FOUND )

!
!     FOUND should be true unless we've encountered numerical problems.
!
      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'Ellipsoid shape and viewing geometry are too '// &
                       'extreme; the limb was not found. '             )
         CALL SIGERR ( 'SPICE(DEGENERATECASE)'                         )
         CALL CHKOUT ( 'EDLIMB'                                        )
         RETURN

      END IF

!
!     Undo the scaling before returning the limb.
!
      CALL VSCLG ( SCALE, LIMB, UBEL, TMPEL )
      CALL MOVED ( TMPEL, UBEL,       LIMB  )

      CALL CHKOUT ( 'EDLIMB' )
      RETURN
      END
