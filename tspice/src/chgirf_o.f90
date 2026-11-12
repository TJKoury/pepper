!$Procedure CHGIRF_O ( Change inertial reference frames )

      SUBROUTINE CHGIRF_O ( REFA, REFB, ROTAB, NAME, INDEX )
      use pepper_legacy
      use pepper_inc_ninert

! $ Abstract
!
!     Support changes among a standard set of inertial coordinate
!     reference frames.
!
! $ Required_Reading
!
!     FRAMES
!
! $ Keywords
!
!     CONVERSION
!     COORDINATES
!     EPHEMERIS
!     FRAME
!     MATRIX
!     ROTATION
!     TRANSFORMATION
!     VECTOR
!
! $ Declarations

      INTEGER               REFA
      INTEGER               REFB
      DOUBLE PRECISION      ROTAB    ( 3,3 )
      CHARACTER*(*)         NAME
      INTEGER               INDEX

! $ Brief_I/O
!
!     Variable  I/O  Entry
!     --------  ---  --------------------------------------------------
!     REFA       I   IRFROT_O
!     REFB       I   IRFROT_O
!     ROTAB      O   IRFROT_O
!     NAME      I/O  IRFNUM_O, IRFNAM_O, IRFDEF_O
!     INDEX     I/O  IRFNUM_O, IRFNAM_O
!
! $ Detailed_Input
!
!     See entry points IRFROT_O, IRFNUM_O, IRFNAM_O, and IRFDEF_O.
!
! $ Detailed_Output
!
!     See entry points IRFROT_O, IRFNUM_O, and IRFNAM_O.
!
! $ Parameters
!
!      None.
!
! $ Files
!
!     None.
!
! $ Exceptions
!
!     1) If CHGIRF_O is called directly, the signal SPICE(BOGUSENTRY)
!        is signalled.
!
!     2) See entry points IRFROT_O, IRFNUM_O, IRFNAM_O, and IRFDEF_O for
!        exceptions specific to those routines.
!
! $ Particulars
!
!     CHGIRF_O exists only as an umbrella for data to be shared
!     by its entry points (IRFROT_O, IRFNUM_O, IRFNAM_O, and IRFDEF_O).
!     It should never be called directly.
!
! $ Examples
!
!     See entry points IRFROT_O, IRFNUM_O, IRFNAM_O, and IRFDEF_O.
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     [1] Jay Lieske, ``Precession Matrix Based on IAU (1976)
!         System of Astronomical Constants,'' Astron. Astrophys.
!         73, 282-284 (1979).
!
!     [2] E.M. Standish, Jr., ``Orientation of the JPL Ephemerides,
!         DE 200/LE 200, to the Dynamical Equinox of J2000,''
!         Astron. Astrophys. 114, 297-302 (1982).
!
!     [3] E.M. Standish, Jr., ``Conversion of Ephemeris Coordinates
!         from the B1950 System to the J2000 System,'' JPL IOM
!         314.6-581, 24 June 1985.
!
!     [4] E.M. Standish, Jr., ``The Equinox Offsets of the JPL
!         Ephemeris,'' JPL IOM 314.6-929, 26 February 1988.
!
!     [5] Jay Lieske, ``Expressions for the Precession  Quantities
!         Based upon the IAU (1976) System of Astronomical
!         Constants'' Astron. Astrophys. 58, 1-16 (1977).
!
!     [6] Laura Bass and Robert Cesarone "Mars Observer Planetary
!         Constants and Models" JPL D-3444 November 1990.
!
!     [7] "Explanatory Supplement to the Astronomical Almanac"
!          edited by P. Kenneth Seidelmann. University Science
!          Books, 20 Edgehill Road, Mill Valley, CA 94941 (1992)
!
! $ Author_and_Institution
!
!     W.L. Taber      (JPL)
!     I.M. Underwood  (JPL)
!
! $ Version
!
!-    SPICELIB Version 4.3.0, 25-AUG-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in CONVRT, ROTMAT and MXM calls.
!
!-    SPICELIB Version 4.2.1, 04-JAN-2002 (EDW)
!
!        Added DE-143 to header description for IRFROT_O.
!
!-    SPICELIB Version 4.2.0, 10-APR-1997 (WLT)
!
!        A descriptive diagnostic was added to the entry points
!        IRFROT_O and IRFDEF_O.  Before they simply signalled the error
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
!        of recognized frames.  This is the standard mars
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
!-&

! $ Index_Entries
!
!     change inertial reference frames
!
!-&

! $ Revisions
!
!-    SPICELIB Version 4.3.0, 25-AUG-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in CONVRT, ROTMAT  and MXM calls.
!
!-&

!
!     SPICELIB functions
!


!
!     Local parameters
!
      INTEGER               MAXF
      PARAMETER           ( MAXF = NINERT )

!
!     Local variables
!
      CHARACTER*16          BASES  (   MAXF )
      CHARACTER*80          DEFS   (   MAXF )
      CHARACTER*25          ERROR
      CHARACTER*16          FRAMES (   MAXF )
      CHARACTER*25          WORD

      DOUBLE PRECISION      ANGLE
      DOUBLE PRECISION      RADANG
      DOUBLE PRECISION      TMPMAT ( 3, 3    )
      DOUBLE PRECISION      TRANS  ( 9, MAXF )

      INTEGER               AXIS
      INTEGER               B
      INTEGER               DFRAME
      INTEGER               I
      INTEGER               J
      INTEGER               LOC
      INTEGER               P

      LOGICAL               READY

!
!     Saved variables
!
      SAVE

!
!     Initial values
!
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
!     by CHGIRF_O must be updated whenever new frames are added.
!
      DATA READY       / .FALSE. /

!
!     The root frame is mostly for show. Rotate by 0 arc seconds
!     about the x-axis to obtain the identity matrix.
!
      DATA FRAMES( 1)  / 'J2000'  /
      DATA BASES ( 1)  / 'J2000'  /
      DATA DEFS  ( 1)  / '0.0  1' /

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
!     The values for z, theta, and zeta are taken directly from
!     are computed from the formulas given in table 5 of [5].
!
!        z     =  1153.04066200330"
!        theta = -1002.26108439117"
!        zeta  =  1152.84248596724"
!
      DATA FRAMES( 2)  / 'B1950' /
      DATA BASES ( 2)  / 'J2000' /
      DATA DEFS  ( 2)  / &
      '1152.84248596724 3  -1002.26108439117  2  1153.04066200330  3' /

!
!     The FK4 reference frame is derived from the B1950 frame by
!     applying the equinox offset determined by Fricke. This is just
!     the rotation
!
!        [ 0.525" ]
!                  3
!
      DATA FRAMES( 3)  / 'FK4'   /
      DATA BASES ( 3)  / 'B1950' /
      DATA DEFS  ( 3)  / '0.525  3' /


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
      DATA FRAMES( 4)  / 'DE-118' /
      DATA BASES ( 4)  / 'B1950'  /
      DATA DEFS  ( 4)  / '0.53155  3' /

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
      DATA FRAMES( 5)  / 'DE-96'  /
      DATA BASES ( 5)  / 'B1950'  /
      DATA DEFS  ( 5)  / '0.4107  3' /

      DATA FRAMES( 6)  / 'DE-102' /
      DATA BASES ( 6)  / 'B1950'  /
      DATA DEFS  ( 6)  / '0.1359  3' /

      DATA FRAMES( 7)  / 'DE-108' /
      DATA BASES ( 7)  / 'B1950'  /
      DATA DEFS  ( 7)  / '0.4775  3' /

      DATA FRAMES( 8)  / 'DE-111' /
      DATA BASES ( 8)  / 'B1950'  /
      DATA DEFS  ( 8)  / '0.5880  3' /

      DATA FRAMES( 9)  / 'DE-114' /
      DATA BASES ( 9)  / 'B1950'  /
      DATA DEFS  ( 9)  / '0.5529  3' /

      DATA FRAMES(10)  / 'DE-122' /
      DATA BASES (10)  / 'B1950'  /
      DATA DEFS  (10)  / '0.5316  3' /

      DATA FRAMES(11)  / 'DE-125' /
      DATA BASES (11)  / 'B1950'  /
      DATA DEFS  (11)  / '0.5754  3' /

      DATA FRAMES(12)  / 'DE-130' /
      DATA BASES (12)  / 'B1950'  /
      DATA DEFS  (12)  / '0.5247  3' /

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
      DATA FRAMES(13)  / 'GALACTIC' /
      DATA BASES (13)  / 'FK4'      /
      DATA DEFS  (13)  / '1177200.0  3  225360.0  1  1016100.0  3' /

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
      DATA FRAMES(14)  / 'DE-200' /
      DATA BASES (14)  / 'J2000'  /
      DATA DEFS  (14)  / '0.0  3' /

      DATA FRAMES(15)  / 'DE-202' /
      DATA BASES (15)  / 'J2000'  /
      DATA DEFS  (15)  / '0.0  3' /
!
!     The values for the transformation from J2000 to MARSIAU_MO
!     are derived from the constants given for the pole of Mars
!     on page 8-2 of reference [6].
!
      DATA FRAMES(16)  / 'MARSIAU' /
      DATA BASES (16)  / 'J2000'   /
      DATA DEFS  (16)  / '324000.0D0 3 133610.4D0 2 -152348.4D0 3' /

!
!     The value for the obliquity of the ecliptic at J2000  is
!     taken from page  114 of [7] equation 3.222-1.  This agrees
!     with the expression given in [5]
!
      DATA FRAMES(17)  / 'ECLIPJ2000'   /
      DATA BASES (17)  / 'J2000'        /
      DATA DEFS  (17)  / '84381.448 1'  /

!
!     The value for the obliquity of the ecliptic at B1950  is
!     taken from page  171 of [7].
!
      DATA FRAMES(18)  / 'ECLIPB1950'   /
      DATA BASES (18)  / 'B1950'       /
      DATA DEFS  (18)  / '84404.836 1' /

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

      DATA FRAMES(19)  / 'DE-140'       /
      DATA BASES (19)  / 'J2000'        /
      DATA DEFS  (19)  / &
      '1152.71013777252 3  -1002.25042010533  2  1153.75719544491  3'/
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
      DATA FRAMES(20)  / 'DE-142'       /
      DATA BASES (20)  / 'J2000'        /
      DATA DEFS  (20)  / &
      '1152.72061453864 3  -1002.25052830351  2  1153.74663857521  3' /

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
      DATA FRAMES(21)  / 'DE-143'       /
      DATA BASES (21)  / 'J2000'        /
      DATA DEFS  (21) &
      /'1153.03919093833, 3, -1002.24822382286, 2, 1153.42900222357, 3'/

!
!     Until defined (by a call to IRFDEF_O), the default frame is
!     undefined.
!
      DATA DFRAME      / 0 /

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CHGIRF_O' )
      END IF

      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )

      CALL CHKOUT ( 'CHGIRF_O' )
      RETURN



!$Procedure IRFROT_O ( Inertial reference frame rotation )

      ENTRY IRFROT_O ( REFA, REFB, ROTAB )

! $ Abstract
!
!     Compute the matrix needed to rotate vectors between two
!     standard inertial reference frames.
!
! $ Required_Reading
!
!     FRAMES
!
! $ Keywords
!
!     CONVERSION
!     COORDINATES
!     EPHEMERIS
!     FRAME
!     MATRIX
!     ROTATION
!     TRANSFORMATION
!     VECTOR
!
! $ Declarations
!
!     INTEGER               REFA
!     INTEGER               REFB
!     DOUBLE PRECISION      ROTAB    ( 3,3 )
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     REFA,
!     REFB       I   Indices of target reference frames (A,B).
!     MATRIX     O   Rotation from frame A to frame B.
!
! $ Detailed_Input
!
!     REFA,
!     REFB        are the indices of two standard inertial reference
!                 frames. The complete set of supported frames is shown
!                 below.
!
!                    Index  Name      Description
!                    -----  --------  --------------------------------
!                     1    J2000      Earth mean equator, dynamical
!                                     equinox of J2000
!
!                     2    B1950      Earth mean equator, dynamical
!                                     equinox of B1950
!
!                     3    FK4        Fundamental Catalog (4)
!
!                     4    DE-118     JPL Developmental Ephemeris (118)
!
!                     5    DE-96      JPL Developmental Ephemeris ( 96)
!
!                     6    DE-102     JPL Developmental Ephemeris (102)
!
!                     7    DE-108     JPL Developmental Ephemeris (108)
!
!                     8    DE-111     JPL Developmental Ephemeris (111)
!
!                     9    DE-114     JPL Developmental Ephemeris (114)
!
!                    10    DE-122     JPL Developmental Ephemeris (122)
!
!                    11    DE-125     JPL Developmental Ephemeris (125)
!
!                    12    DE-130     JPL Developmental Ephemeris (130)
!
!                    13    GALACTIC   Galactic System II
!
!                    14    DE-200     JPL Developmental Ephemeris (200)
!
!                    15    DE-202     JPL Developmental Ephemeris (202)
!
!                    16    MARSIAU    Mars Observer inertial frame
!                                     defined relative to MARS.
!
!                    17    ECLIPJ2000 Earth mean ecliptic and equinox
!                                     of the epoch J2000
!
!                    18    ECLIPB1950 Earth mean ecliptic and equinox
!                                     of the Besselian date 1950.
!
!                    19    DE-140    JPL Developmental Ephemeris (140)
!
!                    20    DE-142    JPL Developmental Ephemeris (142)
!
!                    21    DE-143    JPL Developmental Ephemeris (143)
!
! $ Detailed_Output
!
!     ROTAB       is the rotation which, when applied to a vector v
!                 in reference frame A,
!                    _            _
!                    v  = (ROTAB) v
!                     B            A
!
!                 yields the same vector in reference frame B. The
!                 inverse rotation is performed by applying the
!                 transpose,
!                    _           T _
!                    v  = (ROTAB)  v
!                     A             B
!
! $ Parameters
!
!      None.
!
! $ Files
!
!     None.
!
! $ Exceptions
!
!     1) If either REFA or REFB is outside the range [1,MAXF],
!        where MAXF is the number of supported frames, the error
!        SPICE(IRFNOTREC) is signalled.
!
! $ Particulars
!
!     IRFROT_O exists primarily for use by the ephemeris and star
!     catalog readers in the SPICELIB toolkit library.
!
! $ Examples
!
!     In the following code fragment, IRFROT_O is used to rotate
!     vectors originally referenced to the DE-118 coordinate frame
!     to equivalent vectors referenced to the IAU standard J2000
!     reference frame.
!
!        CALL IRFROT_O ( 4, 1, R )
!
!        CALL MXV ( R, SC1950, SC2000 )
!        CALL MXV ( R, MP1950, MP2000 )
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     See subroutine CHGIRF_O.
!
! $ Author_and_Institution
!
!     W.L. Taber      (JPL)
!     I.M. Underwood  (JPL)
!
! $ Version
!
!-    SPICELIB Version 4.2.1, 04-JAN-2002 (EDW)
!
!        Added DE-143 to header description for IRFROT_O.
!
!-    SPICELIB Version 4.2.0, 10-APR-1997 (WLT)
!
!        A descriptive diagnostic was added to the entry points
!        IRFROT_O and IRFDEF_O.  Before they simply signalled the error
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
!        of recognized frames.  This is the standard mars
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
!-&

! $ Index_Entries
!
!     inertial reference frame rotation
!
!-&


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'IRFROT_O' )
      END IF

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
      IF ( .NOT. READY ) THEN

         DO I = 1, MAXF

            CALL ROTATE ( 0.D0, 1, TRANS(1,I) )

            DO J = WDCNT ( DEFS(I) ), 2, -2

               CALL NTHWD  ( DEFS(I),     J,  WORD, LOC )
               CALL NPARSI ( WORD,     AXIS, ERROR,   P )

               CALL NTHWD  ( DEFS(I),   J-1,  WORD, LOC )
               CALL NPARSD ( WORD,    ANGLE, ERROR,   P )

               CALL CONVRT ( ANGLE, 'ARCSECONDS', 'RADIANS', RADANG )

               CALL ROTMAT ( TRANS(1,I),  RADANG,  AXIS,  TMPMAT     )
               CALL MOVED  ( TMPMAT,      9,              TRANS(1,I) )

            END DO

            B = ISRCHC ( BASES(I), I, FRAMES )

            CALL MXM   ( TRANS(1,I), TRANS(1,B), TMPMAT     )
            CALL MOVED ( TMPMAT,     9,          TRANS(1,I) )

         END DO

         READY = .TRUE.

      END IF

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
      IF ( REFA .LT. 1  .OR.  REFA .GT. MAXF ) THEN

         CALL SETMSG ( 'A request has been made to obtain the ' &
         //            'transformation from inertial reference ' &
         //            'frame # to inertial reference frame #. ' &
         //            'Unfortunately # is not the id-code of a ' &
         //            'known inertial frame. ' )
         CALL ERRINT ( '#', REFA )
         CALL ERRINT ( '#', REFB )
         CALL ERRINT ( '#', REFA )
         CALL SIGERR ( 'SPICE(IRFNOTREC)' )

      ELSE IF ( REFB .LT. 1  .OR.  REFB .GT. MAXF ) THEN

         CALL SETMSG ( 'A request has been made to obtain the ' &
         //            'transformation from inertial reference ' &
         //            'frame # to inertial reference frame #. ' &
         //            'Unfortunately # is not the id-code of a ' &
         //            'known inertial frame. ' )
         CALL ERRINT ( '#', REFA )
         CALL ERRINT ( '#', REFB )
         CALL ERRINT ( '#', REFB )
         CALL SIGERR ( 'SPICE(IRFNOTREC)' )


      ELSE IF ( REFA .EQ. REFB ) THEN
         CALL ROTATE ( 0.D0, 1, ROTAB )

      ELSE
         CALL MXMT ( TRANS(1,REFB), TRANS(1,REFA), ROTAB )
      END IF

      CALL CHKOUT ( 'IRFROT_O' )
      RETURN




!$Procedure IRFNUM_O ( Inertial reference frame number )

      ENTRY IRFNUM_O ( NAME, INDEX )

! $ Abstract
!
!     Return the index of one of the standard inertial reference
!     frames supported by IRFROT_O.
!
! $ Required_Reading
!
!     FRAMES
!
! $ Keywords
!
!     CONVERSION
!     COORDINATES
!     EPHEMERIS
!     FRAME
!     MATRIX
!     ROTATION
!     TRANSFORMATION
!     VECTOR
!
! $ Declarations
!
!     CHARACTER*(*)         NAME
!     INTEGER               INDEX
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     NAME       I   Name of standard inertial reference frame.
!     INDEX      O   Index of frame.
!
! $ Detailed_Input
!
!     NAME        is the name of one of the standard inertial
!                 reference frames supported by IRFROT_O, or
!                 'DEFAULT'.
!
! $ Detailed_Output
!
!     INDEX       is the index of the frame specified by NAME.
!
! $ Parameters
!
!      None.
!
! $ Files
!
!     None.
!
! $ Exceptions
!
!     1) If NAME is not recognized, INDEX is zero.
!
!     2) If no default frame has been specified, INDEX is zero.
!
! $ Particulars
!
!     IRFNUM_O is supplied as a convenience, to allow users to refer to
!     the various standard inertial reference frames by name.
!
! $ Examples
!
!     In the following example, the rotation from DE-118 to FK4 is
!     computed without knowing the indices of these frames.
!
!        CALL IRFNUM_O ( 'DE-118', A )
!        CALL IRFNUM_O ( 'FK4',    B )
!
!        CALL IRFROT_O ( A, B, ROTAB )
!
!     IRFNUM_O can be used to rotate vectors into the default frame,
!     as illustrated by the following code fragment.
!
!        CALL IRFNUM_O ( 'FK4',     A )
!        CALL IRFNUM_O ( 'DEFAULT', B )
!
!        CALL IRFROT_O ( A, B, ROTAB                 )
!        CALL MXV    (       ROTAB, OLDVEC, NEWVEC )
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     See subroutine CHGIRF_O.
!
! $ Author_and_Institution
!
!     W.L. Taber      (JPL)
!     I.M. Underwood  (JPL)
!
! $ Version
!
!-    SPICELIB Version 4.2.1, 04-JAN-2002 (EDW)
!
!        Added DE-143 to header description for IRFROT_O.
!
!-    SPICELIB Version 4.2.0, 10-APR-1997 (WLT)
!
!        A descriptive diagnostic was added to the entry points
!        IRFROT_O and IRFDEF_O.  Before they simply signalled the error
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
!        of recognized frames.  This is the standard mars
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
!-&

! $ Index_Entries
!
!     inertial reference frame number
!
!-&


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'IRFNUM_O' )
      END IF

      IF ( EQSTR ( NAME, 'DEFAULT' ) ) THEN
         INDEX = DFRAME

      ELSE
         INDEX = ESRCHC ( NAME, MAXF, FRAMES )
      END IF

      CALL CHKOUT ( 'IRFNUM_O' )
      RETURN




!$Procedure IRFNAM_O ( Inertial reference frame name )

      ENTRY IRFNAM_O ( INDEX, NAME )

! $ Abstract
!
!     Return the name of one of the standard inertial reference
!     frames supported by IRFROT_O.
!
! $ Required_Reading
!
!     FRAMES
!
! $ Keywords
!
!     CONVERSION
!     COORDINATES
!     EPHEMERIS
!     FRAME
!     MATRIX
!     ROTATION
!     TRANSFORMATION
!     VECTOR
!
! $ Declarations
!
!     INTEGER               INDEX
!     CHARACTER*(*)         NAME
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     INDEX      I   Index of standard inertial reference frame.
!     NAME       O   Name of frame.
!
! $ Detailed_Input
!
!     INDEX       is the index of one of the standard inertial
!                 reference frames supported by IRFROT_O.
!
! $ Detailed_Output
!
!     NAME        is the name of the frame specified by INDEX.
!
! $ Parameters
!
!      None.
!
! $ Files
!
!     None.
!
! $ Exceptions
!
!     1) If INDEX is not the index of a supported frame, NAME is blank.
!
! $ Particulars
!
!     IRFNAM_O is supplied as a convenience, to allow users to determine
!     the names of standard inertial reference frames referred to only
!     by index (as in the segment descriptors of a GEF ephemeris file).
!
! $ Examples
!
!     In the following example, the identity of a rotation from DE-118
!     to FK4 is deduced from the indices used to create the rotation.
!
!        CALL IRFROT_O ( A, B, ROTAB )
!
!        CALL IRFNAM_O ( A, NAME(1) )
!        CALL IRFNAM_O ( B, NAME(2) )
!
!        WRITE (6,*) 'Rotation from ' // NAME(1) // ' to ' // NAME(2)
!
!     Note that the name of the default reference frame can only be
!     recovered from the number:
!
!        CALL IRFNUM_O ( 'DEFAULT', DINDEX        )
!        CALL IRFNAM_O (            DINDEX, DNAME )
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     See subroutine CHGIRF_O.
!
! $ Author_and_Institution
!
!     W.L. Taber      (JPL)
!     I.M. Underwood  (JPL)
!
! $ Version
!
!-    SPICELIB Version 4.2.1, 04-JAN-2002 (EDW)
!
!        Added DE-143 to header description for IRFROT_O.
!
!-    SPICELIB Version 4.2.0, 10-APR-1997 (WLT)
!
!        A descriptive diagnostic was added to the entry points
!        IRFROT_O and IRFDEF_O.  Before they simply signalled the error
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
!        of recognized frames.  This is the standard mars
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
!-&

! $ Index_Entries
!
!     inertial reference frame name
!
!-&


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'IRFNAM_O' )
      END IF

      IF ( INDEX .LT. 1  .OR.  INDEX .GT. MAXF ) THEN
         NAME = ' '

      ELSE
         NAME = FRAMES(INDEX)
      END IF

      CALL CHKOUT ( 'IRFNAM_O' )
      RETURN



!$Procedure IRFDEF_O ( Inertial reference frame, default )

      ENTRY IRFDEF_O ( INDEX )

! $ Abstract
!
!     Specify a standard inertial reference frame as the default
!     frame for a program.
!
! $ Required_Reading
!
!     FRAMES
!
! $ Keywords
!
!     CONVERSION
!     COORDINATES
!     EPHEMERIS
!     FRAME
!     MATRIX
!     ROTATION
!     TRANSFORMATION
!     VECTOR
!
! $ Declarations
!
!     INTEGER               INDEX
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     INDEX      I   Index of default frame.
!
! $ Detailed_Input
!
!     INDEX       is the index of one of the standard inertial
!                 reference frames supported by IRFROT_O.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) If INDEX is outside the range [1,MAXF], where MAXF is the
!        number of supported frames, the error SPICE(IRFNOTREC) is
!        signalled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     IRFDEF_O allows tools to be written at a relatively high level
!     without requiring the reference frame to be tramp coupled or
!     placed in global memory.
!
! $ Examples
!
!     Typically, the calling program will select a default frame
!     during initialization,
!
!        C
!        C     Use J2000 for all ephemeris, star data.
!        C
!              CALL IRFDEF_O ( 1 )
!
!     and recover the default frame at lower levels,
!
!        C
!        C     Rotate all vectors into the default frame.
!        C
!              CALL IRFNUM_O ( 'DEFAULT', REFD   )
!
!              DO I = 1, NVEC
!                 CALL IRFROT_O ( REFIN, REFD, ROT           )
!                 CALL MXV                   ROT, VEC, VEC )
!              END DO
!
!     Note that many utilities accept 'DEFAULT' as the name of
!     an inertial reference frame,
!
!        CALL SPKEZ ( TARGET, ..., 'DEFAULT', ... )
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     See subroutine CHGIRF_O.
!
! $ Author_and_Institution
!
!     W.L. Taber      (JPL)
!     I.M. Underwood  (JPL)
!
! $ Version
!
!-    SPICELIB Version 4.2.1, 04-JAN-2002 (EDW)
!
!        Added DE-143 to header description for IRFROT_O.
!
!-    SPICELIB Version 4.2.0, 10-APR-1997 (WLT)
!
!        A descriptive diagnostic was added to the entry points
!        IRFROT_O and IRFDEF_O.  Before they simply signalled the error
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
!        of recognized frames.  This is the standard mars
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
!-&

! $ Index_Entries
!
!     inertial reference frame default
!
!-&


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'IRFDEF_O' )
      END IF

!
!     There's not much to do, except save the value for later use.
!
      IF ( INDEX .LT. 1  .OR.  INDEX .GT. MAXF ) THEN

         CALL SETMSG ( 'The reference frame with id-code # is ' &
         //            'not a recognized inertial reference ' &
         //            'frame. ' )
         CALL ERRINT ( '#', INDEX )


         CALL SIGERR ( 'SPICE(IRFNOTREC)' )

      ELSE
         DFRAME = INDEX
      END IF

      CALL CHKOUT ( 'IRFDEF_O' )
      RETURN

      END
