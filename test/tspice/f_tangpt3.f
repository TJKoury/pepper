C$Procedure F_TANGPT3 ( Test tangent point routine TANGPT, part 3 )

      SUBROUTINE F_TANGPT3 ( OK )

C$ Abstract
C
C     Perform tests on the tangent point routine TANGPT.
C
C     This test family covers all non-error logic paths of TANGPT.
C     but does not cover a wide variety of geometric cases.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     None.
C
C$ Keywords
C
C     TEST FAMILY
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE 'zzdsk.inc'
      INCLUDE 'zzabcorr.inc'

      LOGICAL               OK

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     OK         O   logical indicating test status.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     OK         is a logical that indicates the test status to the
C                caller.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     This routine does not generate any errors. Routines in its
C     call tree may generate errors that are either intentional and
C     trapped or unintentional and need reporting. The test family
C     utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     For ease of maintenance, the structure of this code follows
C     that of the test family F_TANGPT2. In particular, some arrays
C     of F_TANGPT2 have been changed to arrays of size 1, but remain
C     declared as arrays rather than scalars.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman     (JPL)
C
C$ Version
C
C-    TSPICE Version 1.0.0, 21-OCT-2021 (NJB)
C
C-&

C
C     SPICELIB functions
C
      DOUBLE PRECISION      CLIGHT
      DOUBLE PRECISION      DPR
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      JYEAR
      DOUBLE PRECISION      RPD
      DOUBLE PRECISION      TWOPI
      DOUBLE PRECISION      VDIST
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      VSEP

      LOGICAL               EXISTS

C
C     Local Parameters
C
      CHARACTER*(*)         PCK0
      PARAMETER           ( PCK0 = 'tangpt_test0.tpc' )

      CHARACTER*(*)         PCK1
      PARAMETER           ( PCK1 = 'tangpt_test0.tpc' )

      CHARACTER*(*)         SCNAME
      PARAMETER           ( SCNAME = 'MARS_LOW_ORBITER' )

      CHARACTER*(*)         SPK0
      PARAMETER           ( SPK0 = 'tangpt_test0.bsp' )

      CHARACTER*(*)         SPK1
      PARAMETER           ( SPK1 = 'tangpt_test1.bsp' )


      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               IXPAIR
      PARAMETER           ( IXPAIR = 1 )

      INTEGER               IXRFRM
      PARAMETER           ( IXRFRM  = IXPAIR + 1 )

      INTEGER               IXCORR
      PARAMETER           ( IXCORR = IXRFRM + 1 )

      INTEGER               IXLOC
      PARAMETER           ( IXLOC  = IXCORR + 1 )

      INTEGER               IXSHAP
      PARAMETER           ( IXSHAP = IXLOC  + 1 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 255 )

      INTEGER               LOCLEN
      PARAMETER           ( LOCLEN = 20 )

      INTEGER               NAZ
      PARAMETER           ( NAZ    = 1 )

      INTEGER               NCORR
      PARAMETER           ( NCORR  = 9 )

      INTEGER               NDIMS
      PARAMETER           ( NDIMS  = 6 )

      INTEGER               NEL
      PARAMETER           ( NEL    = 6 )

      INTEGER               NELTS
      PARAMETER           ( NELTS  = 8 )

      INTEGER               NFRAME
      PARAMETER           ( NFRAME = 4 )

      INTEGER               NLOC
      PARAMETER           ( NLOC   = 2 )

      INTEGER               NPAIRS
      PARAMETER           ( NPAIRS = 3 )

      INTEGER               NSHAPE
      PARAMETER           ( NSHAPE = 1 )

      INTEGER               NTIMES
      PARAMETER           ( NTIMES = 1 )

      INTEGER               SCID
      PARAMETER           ( SCID   = -499001 )

      INTEGER               SHPLEN
      PARAMETER           ( SHPLEN = 25 )

      INTEGER               TIMLEN
      PARAMETER           ( TIMLEN = 40 )


      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-12 )

      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-14 )

C
C     Local Variables
C
      CHARACTER*(CORLEN)    ABCORR
      CHARACTER*(CORLEN)    CORARR ( NCORR )
      CHARACTER*(LOCLEN)    CORLOC
      CHARACTER*(FRNMLN)    FIXARR ( NPAIRS )
      CHARACTER*(FRNMLN)    FIXREF
      CHARACTER*(LOCLEN)    LOCARR ( NLOC )
      CHARACTER*(BDNMLN)    OBSARR ( NPAIRS )
      CHARACTER*(BDNMLN)    OBSRVR
      CHARACTER*(FRNMLN)    RAYARR ( NFRAME )
      CHARACTER*(FRNMLN)    RAYFRM
      CHARACTER*(BDNMLN)    RFCNAM
      CHARACTER*(SHPLEN)    SHAPNM ( NSHAPE )
      CHARACTER*(SHPLEN)    SHPNAM
      CHARACTER*(BDNMLN)    TARGET
      CHARACTER*(LNSIZE)    TITLE
      CHARACTER*(BDNMLN)    TRGARR ( NPAIRS )
      CHARACTER*(TIMLEN)    UTC

      DOUBLE PRECISION      ALT
      DOUBLE PRECISION      AZ
      DOUBLE PRECISION      AZDELT
      DOUBLE PRECISION      CP     ( 3 )
      DOUBLE PRECISION      EL
      DOUBLE PRECISION      ELEVS  ( NEL )
      DOUBLE PRECISION      ELTS   ( NELTS )
      DOUBLE PRECISION      EPOCHS ( NAZ )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ET0
      DOUBLE PRECISION      FIXDIR ( 3 )
      DOUBLE PRECISION      LNORML ( 3 )
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      PNEAR  ( 3 )
      DOUBLE PRECISION      POINTS ( 3, NAZ )
      DOUBLE PRECISION      POS    ( 3 )
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      RANGE
      DOUBLE PRECISION      RAYDIR ( 3 )
      DOUBLE PRECISION      RAYEPC
      DOUBLE PRECISION      RAYMAT ( 3, 3 )
      DOUBLE PRECISION      REFVEC ( 3 )
      DOUBLE PRECISION      RFMAT  ( 3, 3 )
      DOUBLE PRECISION      SCHSTP
      DOUBLE PRECISION      SEP
      DOUBLE PRECISION      SOLTOL
      DOUBLE PRECISION      SRFPT  ( 3 )
      DOUBLE PRECISION      SRFSTA ( 6 )
      DOUBLE PRECISION      SRFTAN ( 3 )
      DOUBLE PRECISION      SRFVEC ( 3 )
      DOUBLE PRECISION      STATE0 ( 6 )
      DOUBLE PRECISION      STLOBS ( 3 )
      DOUBLE PRECISION      SUBALT
      DOUBLE PRECISION      SUBP   ( 3 )
      DOUBLE PRECISION      SUBZEN ( 3 )
      DOUBLE PRECISION      TANGTS ( 3, NAZ )
      DOUBLE PRECISION      TANPT  ( 3 )
      DOUBLE PRECISION      TANSTA ( 6 )
      DOUBLE PRECISION      THETA
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      TRGEPC
      DOUBLE PRECISION      XALT
      DOUBLE PRECISION      XEPOCH
      DOUBLE PRECISION      XFORM  ( 3, 3 )
      DOUBLE PRECISION      XLT
      DOUBLE PRECISION      XPT    ( 3 )
      DOUBLE PRECISION      XPTEPC
      DOUBLE PRECISION      XPTVEC ( 3 )
      DOUBLE PRECISION      XRANGE
      DOUBLE PRECISION      XRAYDR ( 3 )
      DOUBLE PRECISION      XSRFPT ( 3 )
      DOUBLE PRECISION      XSRFVC ( 3 )
      DOUBLE PRECISION      XTANPT ( 3 )

      INTEGER               AZIX
      INTEGER               CASE
      INTEGER               CIX
      INTEGER               COORDS ( NDIMS )
      INTEGER               DIMS   ( NDIMS )
      INTEGER               ELIX
      INTEGER               FIX
      INTEGER               HAN0
      INTEGER               HAN1
      INTEGER               LIX
      INTEGER               MAXN
      INTEGER               NCASES
      INTEGER               NPTS   ( NAZ )
      INTEGER               NRAD
      INTEGER               PIX
      INTEGER               RAYFID
      INTEGER               RFCENT
      INTEGER               RFCLAS
      INTEGER               RFCLID

      LOGICAL               ATTBLK ( ABATSZ )
      LOGICAL               FOUND
      LOGICAL               GEOM
      LOGICAL               USECN
      LOGICAL               USELT
      LOGICAL               USESTL
      LOGICAL               XPTFND

C
C     Saved variables
C
      SAVE

C
C     Initial values
C
      DATA                  CORARR / 'NONE',
     .                               'CN', 'XCN', 'CN+S', 'XCN+S',
     .                               'LT', 'XLT', 'LT+S', 'XLT+S' /

      DATA                  DIMS   / NPAIRS, NFRAME, NCORR, NLOC,
     .                               NSHAPE, NTIMES /

C
C     Elevation units are radians.
C
      DATA                  ELEVS  / 1.D-6,  1.D-2,  1.D-1,
     .                               1.D0,  -5.D-8,  1.6D0 /

      DATA                  FIXARR / 'IAU_MARS',
     .                               'IAU_MARS', 'IAU_EARTH' /

      DATA                  LOCARR / 'TANGENT POINT', 'SURFACE POINT' /

      DATA                  OBSARR / SCNAME, 'PHOBOS', 'MARS' /

      DATA                  RAYARR / 'J2000',      'IAU_MARS',
     .                               'ECLIPJ2000', 'IAU_PHOBOS' /

      DATA                  SHAPNM / 'Normal' /

      DATA                  TRGARR / 'MARS', 'MARS', 'EARTH' /

C
C     Open the test family.
C
      CALL TOPEN ( 'F_TANGPT3' )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Setup' )

C
C     Create and load LSK, then delete LSK.
C
      CALL TSTLSK

C
C     Create default test PCK.
C
      IF ( EXISTS(PCK0) ) THEN
         CALL DELFIL( PCK0 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL T_PCK10 ( PCK0, .TRUE., .TRUE. )
      CALL CHCKXC  ( .FALSE., ' ', OK )

C
C     Create test SPK.
C
      IF ( EXISTS(SPK0) ) THEN
         CALL DELFIL( SPK0 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL TSTSPK ( SPK0, .TRUE., HAN0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Create a Mars orbiter SPK file.
C
      IF ( EXISTS(SPK1) ) THEN
         CALL DELFIL ( SPK1 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL SPKOPN ( SPK1, SPK1, 0, HAN1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Set initial time.
C
      UTC = '2020 JAN 1'
      CALL STR2ET ( UTC, ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Set up elements defining a state.  The elements expected
C     by CONICS are:
C
C        RP      Perifocal distance.
C        ECC     Eccentricity.
C        INC     Inclination.
C        LNODE   Longitude of the ascending node.
C        ARGP    Argument of periapse.
C        M0      Mean anomaly at epoch.
C        T0      Epoch.
C        MU      Gravitational parameter.
C
      ELTS(1) =  3500.D0
      ELTS(2) =     1.D-1
      ELTS(3) =    80.D0 * RPD()
      ELTS(4) =     0.D0
      ELTS(5) =    90.D0 * RPD()
      ELTS(6) =     0.D0
      ELTS(7) =    ET0
      ELTS(8) = 42828.314D0

      CALL CONICS ( ELTS, ET0, STATE0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKW05 ( HAN1,         SCID,         499,      'MARSIAU',
     .              -20*JYEAR(),  20*JYEAR(),  'Mars orbiter',
     .              ELTS(8),      1,            STATE0,    ET0       )
     .
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKCLS ( HAN1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Load the new spacecraft SPK file.
C
      CALL SPKLEF ( SPK1, HAN1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Add the orbiter's name/ID mapping to the kernel pool.
C
      CALL PCPOOL ( 'NAIF_BODY_NAME', 1,  SCNAME )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL PIPOOL ( 'NAIF_BODY_CODE', 1,  SCID      )
      CALL CHCKXC ( .FALSE., ' ', OK )


C*****************************************************************
C
C     Normal cases
C
C*****************************************************************

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Setup for Mars-Phobos tests' )

C
C     The following sets of tests cover a range of combinations of
C
C        - Observer-target pairs  3
C        - Ray frames  4
C        - Aberration corrections 9
C        - Correction loci  2
C        - Ray azimuths  1
C        - Ray elevations  6
C        - Shapes  1
C        - Times  1
C

C
C     Set reference epoch. We use a single epoch in this test family.
C
      ET0 = 5*JYEAR()

C
C     We use the utility MULTIX to convert a 1-dimensional index to
C     a 6-dimensional index. For efficiency, we handle AZ and EL
C     values separately; this allows us to do one limb computation
C     for all ray directions.
C
      NCASES = NPAIRS * NFRAME * NCORR * NLOC * NSHAPE * NTIMES

      DO CASE = 1, NCASES
C
C --- Case: ------------------------------------------------------
C

C
C        Compute the indices of each input from the case number.
C
         CALL MULTIX ( 1, NDIMS, DIMS, CASE, COORDS )
         CALL CHCKXC ( .FALSE., ' ', OK )

         PIX  = COORDS(IXPAIR)
         FIX  = COORDS(IXRFRM)
         CIX  = COORDS(IXCORR)
         LIX  = COORDS(IXLOC)

C
C        Set the inputs to TANGPT based on the test case number.
C
         TARGET = TRGARR(PIX)
         OBSRVR = OBSARR(PIX)
         FIXREF = FIXARR(PIX)
         RAYFRM = RAYARR(FIX)
         CORLOC = LOCARR(LIX)
         SHPNAM = SHAPNM(1)

C
C        Set the aberration correction and get the corresponding
C        attribute block.
C
         ABCORR = CORARR(CIX)
         CALL ZZVALCOR ( ABCORR, ATTBLK )

         USELT  = ATTBLK(LTIDX)
         GEOM   = .NOT. USELT
         USECN  = ATTBLK(CNVIDX)
         USESTL = ATTBLK(STLIDX)

C
C        We announce a case here so we can locate errors in the
C        setup, if any.
C
         TITLE = 'Cartesian product setup case number #'
         CALL REPMI ( TITLE, '#', CASE, TITLE )

         CALL TCASE ( TITLE )

C
C        Get the radii of the target body.
C
         CALL BODVRD ( TARGET, 'RADII', 3, NRAD, RADII )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Set time.
C
         ET = ET0

C
C        Units of AZ and EL are radians.
C
         AZDELT = TWOPI() / NAZ

C
C        Generate limb points for the current target, observer,
C        aberration correction, set of azimuth values, and time.
C
         SCHSTP = 0.D0
         SOLTOL = 0.D0
         MAXN   = NAZ

         CALL VPACK ( 0.D0, 0.D0, 1.D0, REFVEC )

         CALL LIMBPT ( 'TANGENT/ELLIPSOID',
     .                 TARGET,           ET,     FIXREF, ABCORR,
     .                 'ELLIPSOID LIMB', OBSRVR, REFVEC, AZDELT,
     .                 NAZ,              SCHSTP, SOLTOL, MAXN,
     .                 NPTS,             POINTS, EPOCHS, TANGTS )

         CALL CHCKXC ( .FALSE., ' ', OK )

         DO AZIX = 1, NAZ

            AZ = AZDELT * AZIX
C
C           Compute the outward normal at the current limb point.
C
            CALL SURFNM ( RADII(1),       RADII(2), RADII(3),
     .                    POINTS(1,AZIX), LNORML              )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Create a rotation axis about which to rotate the
C           tangent vector to create a pointing direction.
C
            CALL UCRSS ( TANGTS(1,AZIX), LNORML, CP )

C
C           Loop over elevation values.
C
            DO ELIX = 1, NEL

               EL = ELEVS( ELIX )

               TITLE = 'Target: #; Observer: #; Body-fixed frame: #; '
     .         //      'Correction #; Locus: #; Ray frame: #; '
     .         //      'Az (deg): #; El (deg): #; '
     .         //      'Shape type: #; Time (TDB): #'

               CALL REPMC ( TITLE, '#', TARGET, TITLE )
               CALL REPMC ( TITLE, '#', OBSRVR, TITLE )
               CALL REPMC ( TITLE, '#', FIXREF, TITLE )
               CALL REPMC ( TITLE, '#', ABCORR, TITLE )
               CALL REPMC ( TITLE, '#', CORLOC, TITLE )
               CALL REPMC ( TITLE, '#', RAYFRM, TITLE )
               CALL REPMD ( TITLE, '#', AZ * DPR(),  9, TITLE )
               CALL REPMD ( TITLE, '#', EL * DPR(),  9, TITLE )
               CALL REPMC ( TITLE, '#', SHPNAM, TITLE )
               CALL REPMD ( TITLE, '#', ET,          9, TITLE )

C
C --- Case: ------------------------------------------------------
C
               CALL TCASE ( TITLE )

C
C              Use the limb point corresponding to the current AZ,
C              and the current EL, to generate a ray in the target
C              body-fixed frame, evaluated at the epoch of the
C              limb point for this AZ value.
C
C              We rotate the vector from the observer to the current
C              limb point upward (away from the target) by the
C              elevation angle.
C
               CALL VROTV ( TANGTS(1,AZIX), CP, EL, FIXDIR )

C
C              Transform the ray's direction from the target
C              body-fixed frame to the specified ray frame. To
C              obtain a meaningful evaluation epoch for the ray
C              frame, compute the light time to the frame's center if
C              the frame is non-inertial.
C
               IF ( RAYFRM .EQ. 'J2000' ) THEN
                  RAYEPC = ET
               ELSE

                  CALL NAMFRM ( RAYFRM, RAYFID )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL FRINFO ( RAYFID, RFCENT, RFCLAS, RFCLID, FOUND )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

                  CALL BODC2S ( RFCENT, RFCNAM )
                  CALL SPKPOS ( RFCNAM, ET,     'J2000',
     .                          ABCORR, OBSRVR, POS,     LT )

                  CALL ZZCOREPC ( ABCORR, ET, LT, RAYEPC )
                  CALL CHCKXC ( .FALSE., ' ', OK )

               END IF

C
C              Compute the transformation from the target frame at
C              the limb point epoch EPOCHS(AZIX) to the fray frame at
C              RAYEPC.
C
               CALL PXFRM2 ( FIXREF, RAYFRM, EPOCHS(AZIX),
     .                       RAYEPC, RFMAT )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL MXV ( RFMAT,  FIXDIR, RAYDIR )

C
C              Get the tangent point and related outputs.
C
               CALL TANGPT ( 'ELLIPSOID',
     .                       TARGET, ET,     FIXREF, ABCORR, CORLOC,
     .                       OBSRVR, RAYFRM, RAYDIR, TANPT,  ALT,
     .                       RANGE,  SRFPT,  TRGEPC, SRFVEC         )

               CALL CHCKXC ( .FALSE., ' ', OK )
C
C              Let STLOBS be the position of the observer relative to
C              the target center in the target body-fixed frame
C              evaluated at TRGEPC. STLOBS reflects stellar aberration
C              as well if those corrections are used.
C
C              STLOBS will be used in several tests below.
C
               CALL VSUB ( SRFPT, SRFVEC, STLOBS )

C
C              Perform consistency check using NPEDLN for cases where
C              the tangent point doesn't coincide with the observer or
C              surface point.
C
               IF ( ( ALT .GT. 0.D0 ) .AND. ( RANGE .NE. 0.D0 ) ) THEN
C
C                 This is a normal geometric case.
C
C                 Check consistency of outputs. Using the target epoch,
C                 surface point, and observer-to-surface vector,
C                 compute the ray direction in the body-fixed frame and
C                 re-compute the surface point and tangent point.
C
C                 Transform the input ray to the target body-fixed
C                 frame at trgepc.
C
                  CALL PXFRM2 ( RAYFRM, FIXREF, RAYEPC, TRGEPC, XFORM )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL MXV ( XFORM, RAYDIR, FIXDIR )

                  CALL NPEDLN ( RADII(1), RADII(2), RADII(3),
     .                          STLOBS,   FIXDIR,   XSRFPT,  XALT )
                  CALL CHCKXC ( .FALSE., ' ', OK )

C
C                 Check SRFPT.
C
                  IF ( RANGE .LT. 1.D7 ) THEN

                     IF ( GEOM ) THEN

                        TOL = 1.D-11

                     ELSE IF ( USECN ) THEN
C
C                       We're using converged light time corrections.
C
                        IF ( .NOT. USESTL ) THEN
                           TOL = TIGHT
                        ELSE
                           TOL = 1.D-11
                        END IF

                     ELSE
C
C                       Light time correction is not converged.
C
                        TOL = 5.D-11

                     END IF

                  ELSE
C
C                    Range to the tangent point is over 1e7 km.
C
                     IF ( GEOM ) THEN

                        TOL = 5.D-11

                     ELSE IF ( USECN ) THEN
C
C                       We're using converged light time corrections.
C
                        IF ( .NOT. USESTL ) THEN
                           TOL = 5.D-11
                        ELSE
                           TOL = 5.D-11
                        END IF

                     ELSE
C
C                       Light time correction is not converged.
C
                        TOL = 1.D-8

                     END IF

                  END IF

                  CALL CHCKAD ( 'SRFPT (npedln)',
     .                          SRFPT, '~~/', XSRFPT, 3, TOL, OK )

C
C                 Check TANPT. Compute the expected tangent point.
C
                  CALL NPLNPT ( STLOBS, FIXDIR, SRFPT, XTANPT, XALT )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  IF ( GEOM ) THEN

                     IF ( RANGE .LT. 1.D7 ) THEN

                        TOL = VTIGHT
                     ELSE
                        TOL = 5.D-11
                     END IF

                  ELSE IF ( USECN ) THEN

                     IF ( RANGE .LT. 1.D7 ) THEN

                        TOL = VTIGHT
                     ELSE
                        TOL = 5.D-10
                     END IF

                  ELSE
C
C                    Light time correction is not converged.
C
                     IF ( RANGE .LT. 1.D7 ) THEN
                        TOL = 1.D-10
                     ELSE
                        TOL = 1.D-9
                     END IF

                  END IF

                  CALL CHCKAD ( 'TANPT (npedln)',
     .                          TANPT, '~~/', XTANPT, 3, TOL, OK )
C
C                 Check altitude.
C
                  IF ( XALT .GT. 100.D0 ) THEN
C
C                    Check relative error.
C
                     IF ( GEOM ) THEN

                        IF ( RANGE .LT. 1.D7 ) THEN

                           TOL = TIGHT

                        ELSE

                           TOL = 1.D-9

                        END IF

                     ELSE IF ( USECN ) THEN
C
C                       We're using converged light time corrections.
C
                        IF ( RANGE .LT. 1.D7 ) THEN
C
C                          Ray direction errors induce altitude errors
C                          that are roughly proportional to range.
C                          At closer range, we can use a tighter
C                          tolerance for our altitude checks.
C
                           TOL = TIGHT
                        ELSE
                           TOL = 5.D-10
                        END IF

                     ELSE
C
C                       Light time correction is not converged.
C
                        TOL = 1.D-8

                     END IF

                     CALL CHCKSD ( 'ALT (npedln)',
     .                              ALT, '~/', XALT, TOL, OK )
                  ELSE
C
C                    This is the low-altitude case.
C
C                    Check absolute error.
C
                     IF ( GEOM ) THEN

                        TOL = 1.D-10

                     ELSE IF ( USECN ) THEN
C
C                       We're using converged light time corrections.
C
                        TOL = 1.D-9

                     ELSE
C
C                       Light time correction is not converged.
C
                        TOL = 1.D-9
                     END IF

                     CALL CHCKSD ( 'ALT (npedln)',
     .                              ALT, '~', XALT, TOL, OK )
                  END IF

C
C                 Check range.
C
                  XRANGE = VDIST ( STLOBS, TANPT )

                  IF ( XRANGE .GT. 100.D0 ) THEN

                     TOL = VTIGHT

                     CALL CHCKSD ( 'RANGE (npedln)',
     .                             RANGE, '~/', XRANGE, TOL, OK )
                  ELSE
C
C                    The expected range is under 100 km. Use
C                    a 0.1 mm tolerance for absolute range error.

                     TOL = 1.D-7
                     CALL CHCKSD ( 'RANGE (npedln)',
     .                             RANGE, '~', XRANGE, TOL, OK )
                  END IF

               END IF

C
C              This is the end of the NPEDLN checks, which are
C              performed only when range and altitude are both
C              non-zero.
C
C              At this point, we need to decide whether the ray is
C              "looking away" from the target, in which case the
C              tangent point is supposed to be set equal to the
C              observer's location.
C
C              At this point, we have the inputs needed to find the
C              sub-observer point on the apparent target. We'll use
C              this point later to decide whether or not we have a
C              "look away" ray direction.
C
C              Note that, due to the highly non-spherical shapes of
C              targets, we can't rely on the angular separation between
C              the ray and the observer-target direction for this
C              determination. Instead we look at the angular separation
C              between the ray direction and the zenith direction at
C              the sub-observer point.
C
C              The observer position STLOBS reflects the aberration
C              corrections we're using and target epoch used to
C              evaluate the orientation of the target body-fixed frame.
C
               CALL NEARPT ( STLOBS, RADII(1), RADII(2), RADII(3),
     .                       SUBP,   SUBALT                       )
               CALL CHCKXC ( .FALSE., ' ', OK )
C
C              Get the zenith direction at the sub-observer point.
C              We'll use this later.
C
               CALL SURFNM ( RADII(1), RADII(2), RADII(3),
     .                       SUBP,     SUBZEN  )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              Let THETA be the angular offset of the ray direction
C              from the sub-observer point's zenith direction.
C
               THETA = VSEP ( FIXDIR, SUBZEN )

               IF ( THETA .LT. HALFPI() ) THEN
C
C                 The ray is pointing upward relative to the local
C                 level plane at the sub-observer point. This is a
C                 "look away" case.
C
C                 We expect:
C
C                    - TRGEPC is exactly ET if the locus is the
C                      tangent point; otherwise TRGEPC is derived from
C                      the observer altitude.
C
C                    - RANGE is exactly zero
C
C                    - TANPT coincides with the observer's position
C                      relative to the target center
C
C                    - SRFPT is the near point on the target
C
C                    - ALT is the altitude of the observer above the
C                      target
C
C
C                 Find the nearest point on the target to TANPT.
C
                  CALL NEARPT ( TANPT,    RADII(1), RADII(2),
     .                          RADII(3), PNEAR,    XALT     )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  IF (  LIX .EQ. 1 ) THEN
C
C                    The locus is the tangent point, which in this
C                    case coincides with the observer.
C
                     CALL CHCKSD ( 'TRGEPC A', TRGEPC,
     .                              '=',     ET, 0.D0, OK )

                  ELSE
C
C                    The locus is the surface point, which in this
C                    case is the nearest point on the target to the
C                    observer.
C
                     CALL ZZCOREPC ( ABCORR,        ET,
     .                               XALT/CLIGHT(), XEPOCH )

                     TOL = TIGHT

                     CALL CHCKSD ( 'TRGEPC B', TRGEPC,
     .                              '~/',    XEPOCH, TOL, OK )
                  END IF

C
C                 Check the range, which should be exactly 0.
C
                  CALL CHCKSD ( 'RANGE',  RANGE,  '=', 0.D0, 0.D0, OK )

C
C                 Check the tangent point, which should be
C                 equal to the observer position.
C
                  TOL = VTIGHT

                  CALL CHCKAD ( 'TANPT', TANPT, '~~/',
     .                                   STLOBS, 3, TOL, OK)

C
C                 Check the surface point, which should be
C                 equal to the near point.
C
                  TOL = VTIGHT

                  CALL CHCKAD ( 'SRFPT', SRFPT, '~~/',
     .                                   PNEAR, 3, TOL, OK)

C
C                 Check the tangent point altitude, which should be
C                 equal to the altitude found by NEARPT.
C
                  TOL = VTIGHT

                  CALL CHCKSD ( 'ALT', ALT, '~/', XALT, TOL, OK )


               ELSE IF ( EL .LE. 0.D0 ) THEN
C
C                 This is an intercept case.
C
C                 We expect:
C
C                    - ALT is exactly zero
C
C                    - TANPT is exactly SRFPT
C
C                    - RANGE is exactly ||SRFVEC||
C
C                    - TRGEPC is ET +/- RANGE / c
C
                  CALL CHCKSD ( 'ALT', ALT, '=', 0.D0, 0.D0, OK )

                  CALL CHCKAD ( 'TANPT', TANPT, '=',
     .                                   SRFPT, 3, TOL, OK)

                  XRANGE = VNORM( SRFVEC )

                  TOL = 0.D0

                  CALL CHCKSD ( 'RANGE', RANGE, '=', XRANGE, TOL, OK )


                  CALL ZZCOREPC ( ABCORR, ET, RANGE/CLIGHT(), XEPOCH )

                  TOL = TIGHT

                  CALL CHCKSD ( 'TRGEPC', TRGEPC, '~/',
     .                                    XEPOCH, TOL, OK )

                  IF ( CORLOC .EQ. 'SURFACE POINT' ) THEN
C
C                    Compare the surface point against that produced
C                    by SINCPT. We can expect good agreement if
C                    we're not using aberration corrections. The
C                    instability of near-limb ray intercepts requires
C                    large tolerances when converged Newtonian light
C                    time corrections are used. We don't attempt
C                    comparisons when stellar aberration corrections
C                    or non-converged light time corrections are used.
C
                     CALL SINCPT ( 'ELLIPSOID',
     .                             TARGET, ET,     FIXREF, ABCORR,
     .                             OBSRVR, RAYFRM, RAYDIR, XPT,
     .                             XPTEPC, XPTVEC, XPTFND         )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     IF ( ( .NOT. USELT ) .OR. USECN ) THEN
C
C                       Verify that an intercept was found.
C
                        CALL CHCKSL ( 'XPTFND', XPTFND, .TRUE., OK )

                        IF ( XPTFND ) THEN
C
C                          Check the target epoch. Use 0.1 microsecond
C                          time tolerance.
C
                           TOL = 1.D-7

                           CALL CHCKSD ( 'TRGEPC', TRGEPC, '~',
     .                                             XPTEPC, TOL, OK )

C
C                          Check the intercept.
C
                           IF ( .NOT. USELT ) THEN
C
C                             Look for agreement at the 1 mm level.
C
                              TOL = 1.D-6

                           ELSE
C
C                             Allow errors of up to 5 m.
C
                              TOL = 5.D0

                           END IF

                           CALL CHCKAD ( 'SRFPT', SRFPT, '~~',
     .                                    XPT, 3, TOL, OK )
                        END IF

                     END IF

                  END IF

               ELSE
C
C                 Checks for all normal cases follow.
C
C                 Check target epoch.
C
                  IF ( CORLOC .EQ. 'TANGENT POINT' ) THEN
                     XLT = RANGE / CLIGHT()
                  ELSE
                     XLT = VNORM( SRFVEC ) / CLIGHT()
                  END IF

                  CALL ZZCOREPC ( ABCORR, ET, XLT, XEPOCH )
                  CALL CHCKXC ( .FALSE., ' ', OK )

C
C                 Check the target epoch. Use 0.1 microsecond
C                 time tolerance.
C
                  TOL = 1.D-7
                  CALL CHCKSD ( 'TRGEPC', TRGEPC, '~', XEPOCH, TOL, OK )

C
C                 Find the nearest point on the target to TANPT.
C
                  CALL NEARPT ( TANPT,    RADII(1), RADII(2),
     .                          RADII(3), PNEAR,    XALT     )
                  CALL CHCKXC ( .FALSE., ' ', OK )

C
C                 Check altitude of the tangent point. The altitude will
C                 be zero for intercept cases, but no special test is
C                 required for those cases.
C
                  IF ( XALT .GT. 100.D0 ) THEN
C
C                    Check relative error.
C
                     TOL = 1.D-9
                     CALL CHCKSD ( 'ALT (A)', ALT, '~/',
     .                                        XALT, TOL, OK )
                  ELSE
C
C                    Check absolute error.
C
                     TOL = 1.D-9
                     CALL CHCKSD ( 'ALT (B)', ALT, '~',
     .                                        XALT, TOL, OK )
                  END IF

C
C                 Check range to the tangent point.
C
                  XRANGE = VDIST ( STLOBS, TANPT )

                  IF ( XRANGE .GT. 100.D0 ) THEN
C
C                    This is the higher altitude case. Check relative
C                    error.
C
                     TOL = TIGHT
                     CALL CHCKSD ( 'RANGE', RANGE, '~/',
     .                                      XRANGE, TOL, OK )
                  ELSE
C
C                    This is the lower altitude case. Check absolute
C                    error. We can expect agreement at the 1 micron
C                    level.
C
                     TOL = 1.D-9
                     CALL CHCKSD ( 'RANGE', RANGE, '~',
     .                                      XRANGE, TOL, OK )
                  END IF

C
C                 Compare SRFPT to the surface point found by NEARPT.
C
C                 Check SRFPT absolute error.
C
                  IF ( RANGE .LT. 1.D7 ) THEN
C
C                    Observer and target are less than 10 M km apart.
C
                     TOL = 1.D-10
                  ELSE
                     TOL = 5.D-7
                  END IF

                  CALL CHCKAD ( 'SRFPT (abs)', SRFPT, '~~',
     .                                         PNEAR, 3, TOL, OK )

C
C                 Check SRFPT relative error.
C
                  TOL = 1.D-10

                  CALL CHCKAD ( 'SRFPT (rel)', SRFPT, '~~/',
     .                                         PNEAR, 3, TOL, OK )

C
C                 Check the position of the tangent point above the
C                 surface point.
C
                  CALL SURFNM ( RADII(1), RADII(2), RADII(3),
     .                          SRFPT, NORMAL )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL VSUB ( TANPT, SRFPT, SRFTAN )

                  SEP = VSEP ( SRFTAN, NORMAL )

                  IF (  ( EL > 0.D0 )  .AND.  ( EL < HALFPI() )  ) THEN
C
C                    Check angular separation of zenith direction at
C                    SRFPT vs the SRFPT-to-TANPT vector.
C
C                    Use looser tolerance here since the relative
C                    errors in TANPT and SRFPT are larger compared to
C                    the lengths of those vectors than they are
C                    relative to the observer-target distance.
C
                     TOL = MAX( 1.D-10, 1.D-6/ALT )

                     CALL CHCKSD ( 'SRFPT-TANPT ZENITH SEP',
     .                             SEP, '~', 0.D0, TOL, OK )
C
C                    Check ALT.
C
                     XALT = VDIST( SRFPT, TANPT )

                     IF ( XALT .GT. 100.D0 ) THEN

                        TOL  = VTIGHT * MAX( RANGE, 1.D0 )

                        CALL CHCKSD ( 'ALT (C)', ALT, '~/',
     .                                           XALT, TOL, OK )
                     ELSE

                        TOL  = 1.D-9

                        CALL CHCKSD ( 'ALT (C)', ALT, '~',
     .                                           XALT, TOL, OK )
                     END IF

                  END IF

                  IF (       ( CORLOC .EQ. 'TANGENT POINT' )
     .                 .AND. ( RANGE  .NE.  0              )  ) THEN
C
C                    We'll treat the tangent point as an ephemeris
C                    object. We'll find the angular separation between
C                    the input ray and the ray from the observer to the
C                    tangent point.
C
C                    Find the aberration-corrected position of the
C                    tangent point relative to the observer, expressed
C                    in the J2000 frame. Convert this vector to the ray
C                    frame.
C
                     CALL SPKCPT ( TANPT,    TARGET,  FIXREF,
     .                             ET,       'J2000', 'TARGET',
     .                             ABCORR,   OBSRVR,  TANSTA,  LT  )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     CALL PXFORM ( 'J2000', RAYFRM, RAYEPC, RAYMAT )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     CALL MXV ( RAYMAT, TANSTA, XRAYDR )
C
C                    Check angular separation of ray direction and
C                    direction to tangent point.
C
                     SEP = VSEP( RAYDIR, XRAYDR )

                     IF ( EL .LT. 2.D-1 ) THEN
C
C                       These are the low angular separation cases.
C
                        IF (       ( RANGE .GT. 0.D0 )
     .                       .AND. ( ALT   .GT. 0.D0 ) ) THEN

                           IF ( RANGE .LT. 1.D7 ) THEN

                              IF ( USECN ) THEN

                                 IF ( USESTL ) THEN
                                    TOL = 5.D-10
                                 ELSE
                                    TOL = 1.D-10
                                 END IF

                              ELSE IF ( USELT ) THEN
C
C                                We expect very loose agreement with
C                                non-converged light time.
C
                                 TOL = 1.D-7

                              ELSE
C
C                                This is the geometric case.
C
                                 TOL = VTIGHT

                              END IF

                           ELSE
C
C                             These are the long-range cases.
C
                              IF ( USECN ) THEN

                                 TOL = 1.D-10

                              ELSE IF ( USELT ) THEN
C
C                                We expect very loose agreement with
C                                non-converged light time.
C
                                 TOL = 1.D-4

                              ELSE
C
C                                This is the geometric case.
C
                                 TOL = VTIGHT

                              END IF

                           END IF

                        ELSE
C
C                          These are special cases: the tangent point
C                          coincides with the observer or the surface
C                          intercept.
C
                           IF ( USELT ) THEN
                              TOL = 1.D-10
                           ELSE
                              TOL = 1.D-10
                           END IF

                        END IF

                     ELSE
C
C                       These are the high angular separation cases.
C
                        IF ( RANGE .GT. 1.D7 ) THEN
C
C                          Large range implies high altitude, in this
C                          case.
C
C                          Note that a high-altitude tangent point,
C                          where Mars is the central body, has very
C                          high velocity.
C
                           IF ( USECN ) THEN
                              TOL = 1.D-7
                           ELSE IF ( USELT ) THEN
                              TOL = 1.D-2
                           ELSE
                              TOL = 1.D-7
                           END IF

                        ELSE

                           IF ( USECN ) THEN
                              TOL = 5.D-7
                           ELSE IF ( USELT ) THEN
                              TOL = 1.D-3
                           ELSE
C
C                             Geometric case for high angular
C                             separation.
C
                              TOL = 1.D-8

                           END IF

                        END IF

                     END IF

                     CALL CHCKSD ( 'RAY-OBS-TO-TANPT SEP',
     .                             SEP, '~', 0.D0, TOL, OK )


C
C                    Check range against the magnitude of the
C                    position of the tangent point relative to
C                    the observer.
C
                     XRANGE = VNORM( TANSTA )

                     CALL CHCKSD ( 'RANGE (vs TANSTA)',
     .                              RANGE, '~/', XRANGE, TOL, OK )

C
C                    End of TANGENT POINT locus case.
C
                  ELSE IF (      ( CORLOC .EQ. 'SURFACE POINT' )
     .                     .AND. ( ALT    .NE.  0              ) ) THEN
C
C                    The locus is the surface point.
C
C                    We'll treat the surface point as an ephemeris
C                    object. We'll find the angular separation between
C                    the ray found by TANGPT from the observer to the
C                    surface point and the position found by SPKCPT of
C                    the surface point relative to the observer.
C
C                    Find the aberration-corrected position of the
C                    surface point relative to the observer, expressed
C                    in the target body-fixed frame.
C
                     CALL SPKCPT ( SRFPT,    TARGET,  FIXREF,
     .                             ET,       FIXREF, 'TARGET',
     .                             ABCORR,   OBSRVR,  SRFSTA,  LT  )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     CALL VEQU ( SRFSTA, XSRFVC )

                     SEP = VSEP ( SRFVEC, XSRFVC )

                     IF ( EL .LT. 2.D-1 ) THEN
C
C                       These are the low angular separation cases.
C
                        IF ( .NOT. USELT ) THEN

                           TOL = 1.D-10

                        ELSE IF ( USECN ) THEN

                           IF ( USESTL ) THEN
                              TOL = 5.D-10
                           ELSE
                              TOL = 5.D-10
                           END IF

                        ELSE
                           TOL = 1.D-7
                        END IF

                     ELSE
                        TOL = 1.D-7
                     END IF

                     CALL CHCKSD ( 'SRFPT SEP', SEP, '~',
     .                                          0.D0, TOL, OK )

C
C                    Check the surface vector as well. We can use
C                    the same tolerances.
C
                     CALL CHCKAD ( 'SRFVEC', SRFVEC, '~~/',
     .                                       XSRFVC, 3, TOL, OK )

                  END IF
C
C                 This is the end of the normal case check block.
C
               END IF

            END DO
C
C           This is the end of the elevation loop.
C
         END DO
C
C        This is the end of the azimuth loop.
C
      END DO
C
C     This is the end of the case loop.
C

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Clean up.' )

      CALL KCLEAR
      CALL CHCKXC ( .FALSE., ' ', OK )

      IF ( EXISTS(PCK0) ) THEN
         CALL DELFIL( PCK0 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      IF ( EXISTS(PCK1) ) THEN
         CALL DELFIL( PCK1 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      IF ( EXISTS(SPK0) ) THEN
         CALL SPKUEF ( HAN0 )
         CALL DELFIL ( SPK0 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      IF ( EXISTS(SPK1) ) THEN
         CALL SPKUEF ( HAN1 )
         CALL DELFIL ( SPK1 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN
      END
