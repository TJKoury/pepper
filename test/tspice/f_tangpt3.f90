!$Procedure F_TANGPT3 ( Test tangent point routine TANGPT, part 3 )

      SUBROUTINE F_TANGPT3 ( OK )

! $ Abstract
!
!     Perform tests on the tangent point routine TANGPT.
!
!     This test family covers all non-error logic paths of TANGPT.
!     but does not cover a wide variety of geometric cases.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TEST FAMILY
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'zzdsk.inc'
      INCLUDE 'zzabcorr.inc'

      LOGICAL               OK

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     OK         O   logical indicating test status.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     OK         is a logical that indicates the test status to the
!                caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     This routine does not generate any errors. Routines in its
!     call tree may generate errors that are either intentional and
!     trapped or unintentional and need reporting. The test family
!     utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     For ease of maintenance, the structure of this code follows
!     that of the test family F_TANGPT2. In particular, some arrays
!     of F_TANGPT2 have been changed to arrays of size 1, but remain
!     declared as arrays rather than scalars.
!
! $ Examples
!
!     None.
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
!     N.J. Bachman     (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 21-OCT-2021 (NJB)
!
!-&

!
!     SPICELIB functions
!
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

!
!     Local Parameters
!
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

!
!     Local Variables
!
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

!
!     Saved variables
!
      SAVE

!
!     Initial values
!
      DATA                  CORARR / 'NONE', &
                                     'CN', 'XCN', 'CN+S', 'XCN+S', &
                                     'LT', 'XLT', 'LT+S', 'XLT+S' /

      DATA                  DIMS   / NPAIRS, NFRAME, NCORR, NLOC, &
                                     NSHAPE, NTIMES /

!
!     Elevation units are radians.
!
      DATA                  ELEVS  / 1.D-6,  1.D-2,  1.D-1, &
                                     1.D0,  -5.D-8,  1.6D0 /

      DATA                  FIXARR / 'IAU_MARS', &
                                     'IAU_MARS', 'IAU_EARTH' /

      DATA                  LOCARR / 'TANGENT POINT', 'SURFACE POINT' /

      DATA                  OBSARR / SCNAME, 'PHOBOS', 'MARS' /

      DATA                  RAYARR / 'J2000',      'IAU_MARS', &
                                     'ECLIPJ2000', 'IAU_PHOBOS' /

      DATA                  SHAPNM / 'Normal' /

      DATA                  TRGARR / 'MARS', 'MARS', 'EARTH' /

!
!     Open the test family.
!
      CALL TOPEN ( 'F_TANGPT3' )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Setup' )

!
!     Create and load LSK, then delete LSK.
!
      CALL TSTLSK

!
!     Create default test PCK.
!
      IF ( EXISTS(PCK0) ) THEN
         CALL DELFIL( PCK0 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL T_PCK10 ( PCK0, .TRUE., .TRUE. )
      CALL CHCKXC  ( .FALSE., ' ', OK )

!
!     Create test SPK.
!
      IF ( EXISTS(SPK0) ) THEN
         CALL DELFIL( SPK0 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL TSTSPK ( SPK0, .TRUE., HAN0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create a Mars orbiter SPK file.
!
      IF ( EXISTS(SPK1) ) THEN
         CALL DELFIL ( SPK1 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL SPKOPN ( SPK1, SPK1, 0, HAN1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Set initial time.
!
      UTC = '2020 JAN 1'
      CALL STR2ET ( UTC, ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Set up elements defining a state.  The elements expected
!     by CONICS are:
!
!        RP      Perifocal distance.
!        ECC     Eccentricity.
!        INC     Inclination.
!        LNODE   Longitude of the ascending node.
!        ARGP    Argument of periapse.
!        M0      Mean anomaly at epoch.
!        T0      Epoch.
!        MU      Gravitational parameter.
!
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

      CALL SPKW05 ( HAN1,         SCID,         499,      'MARSIAU', &
                    -20*JYEAR(),  20*JYEAR(),  'Mars orbiter', &
                    ELTS(8),      1,            STATE0,    ET0       )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKCLS ( HAN1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Load the new spacecraft SPK file.
!
      CALL SPKLEF ( SPK1, HAN1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Add the orbiter's name/ID mapping to the kernel pool.
!
      CALL PCPOOL ( 'NAIF_BODY_NAME', 1,  SCNAME )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL PIPOOL ( 'NAIF_BODY_CODE', 1,  SCID      )
      CALL CHCKXC ( .FALSE., ' ', OK )


!*****************************************************************
!
!     Normal cases
!
!*****************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Setup for Mars-Phobos tests' )

!
!     The following sets of tests cover a range of combinations of
!
!        - Observer-target pairs  3
!        - Ray frames  4
!        - Aberration corrections 9
!        - Correction loci  2
!        - Ray azimuths  1
!        - Ray elevations  6
!        - Shapes  1
!        - Times  1
!

!
!     Set reference epoch. We use a single epoch in this test family.
!
      ET0 = 5*JYEAR()

!
!     We use the utility MULTIX to convert a 1-dimensional index to
!     a 6-dimensional index. For efficiency, we handle AZ and EL
!     values separately; this allows us to do one limb computation
!     for all ray directions.
!
      NCASES = NPAIRS * NFRAME * NCORR * NLOC * NSHAPE * NTIMES

      DO CASE = 1, NCASES
!
! --- Case: ------------------------------------------------------
!

!
!        Compute the indices of each input from the case number.
!
         CALL MULTIX ( 1, NDIMS, DIMS, CASE, COORDS )
         CALL CHCKXC ( .FALSE., ' ', OK )

         PIX  = COORDS(IXPAIR)
         FIX  = COORDS(IXRFRM)
         CIX  = COORDS(IXCORR)
         LIX  = COORDS(IXLOC)

!
!        Set the inputs to TANGPT based on the test case number.
!
         TARGET = TRGARR(PIX)
         OBSRVR = OBSARR(PIX)
         FIXREF = FIXARR(PIX)
         RAYFRM = RAYARR(FIX)
         CORLOC = LOCARR(LIX)
         SHPNAM = SHAPNM(1)

!
!        Set the aberration correction and get the corresponding
!        attribute block.
!
         ABCORR = CORARR(CIX)
         CALL ZZVALCOR ( ABCORR, ATTBLK )

         USELT  = ATTBLK(LTIDX)
         GEOM   = .NOT. USELT
         USECN  = ATTBLK(CNVIDX)
         USESTL = ATTBLK(STLIDX)

!
!        We announce a case here so we can locate errors in the
!        setup, if any.
!
         TITLE = 'Cartesian product setup case number #'
         CALL REPMI ( TITLE, '#', CASE, TITLE )

         CALL TCASE ( TITLE )

!
!        Get the radii of the target body.
!
         CALL BODVRD ( TARGET, 'RADII', 3, NRAD, RADII )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Set time.
!
         ET = ET0

!
!        Units of AZ and EL are radians.
!
         AZDELT = TWOPI() / NAZ

!
!        Generate limb points for the current target, observer,
!        aberration correction, set of azimuth values, and time.
!
         SCHSTP = 0.D0
         SOLTOL = 0.D0
         MAXN   = NAZ

         CALL VPACK ( 0.D0, 0.D0, 1.D0, REFVEC )

         CALL LIMBPT ( 'TANGENT/ELLIPSOID', &
                       TARGET,           ET,     FIXREF, ABCORR, &
                       'ELLIPSOID LIMB', OBSRVR, REFVEC, AZDELT, &
                       NAZ,              SCHSTP, SOLTOL, MAXN, &
                       NPTS,             POINTS, EPOCHS, TANGTS )

         CALL CHCKXC ( .FALSE., ' ', OK )

         DO AZIX = 1, NAZ

            AZ = AZDELT * AZIX
!
!           Compute the outward normal at the current limb point.
!
            CALL SURFNM ( RADII(1),       RADII(2), RADII(3), &
                          POINTS(1,AZIX), LNORML              )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Create a rotation axis about which to rotate the
!           tangent vector to create a pointing direction.
!
            CALL UCRSS ( TANGTS(1,AZIX), LNORML, CP )

!
!           Loop over elevation values.
!
            DO ELIX = 1, NEL

               EL = ELEVS( ELIX )

               TITLE = 'Target: #; Observer: #; Body-fixed frame: #; ' &
               //      'Correction #; Locus: #; Ray frame: #; ' &
               //      'Az (deg): #; El (deg): #; ' &
               //      'Shape type: #; Time (TDB): #'

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

!
! --- Case: ------------------------------------------------------
!
               CALL TCASE ( TITLE )

!
!              Use the limb point corresponding to the current AZ,
!              and the current EL, to generate a ray in the target
!              body-fixed frame, evaluated at the epoch of the
!              limb point for this AZ value.
!
!              We rotate the vector from the observer to the current
!              limb point upward (away from the target) by the
!              elevation angle.
!
               CALL VROTV ( TANGTS(1,AZIX), CP, EL, FIXDIR )

!
!              Transform the ray's direction from the target
!              body-fixed frame to the specified ray frame. To
!              obtain a meaningful evaluation epoch for the ray
!              frame, compute the light time to the frame's center if
!              the frame is non-inertial.
!
               IF ( RAYFRM .EQ. 'J2000' ) THEN
                  RAYEPC = ET
               ELSE

                  CALL NAMFRM ( RAYFRM, RAYFID )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL FRINFO ( RAYFID, RFCENT, RFCLAS, RFCLID, FOUND )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

                  CALL BODC2S ( RFCENT, RFCNAM )
                  CALL SPKPOS ( RFCNAM, ET,     'J2000', &
                                ABCORR, OBSRVR, POS,     LT )

                  CALL ZZCOREPC ( ABCORR, ET, LT, RAYEPC )
                  CALL CHCKXC ( .FALSE., ' ', OK )

               END IF

!
!              Compute the transformation from the target frame at
!              the limb point epoch EPOCHS(AZIX) to the fray frame at
!              RAYEPC.
!
               CALL PXFRM2 ( FIXREF, RAYFRM, EPOCHS(AZIX), &
                             RAYEPC, RFMAT )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL MXV ( RFMAT,  FIXDIR, RAYDIR )

!
!              Get the tangent point and related outputs.
!
               CALL TANGPT ( 'ELLIPSOID', &
                             TARGET, ET,     FIXREF, ABCORR, CORLOC, &
                             OBSRVR, RAYFRM, RAYDIR, TANPT,  ALT, &
                             RANGE,  SRFPT,  TRGEPC, SRFVEC         )

               CALL CHCKXC ( .FALSE., ' ', OK )
!
!              Let STLOBS be the position of the observer relative to
!              the target center in the target body-fixed frame
!              evaluated at TRGEPC. STLOBS reflects stellar aberration
!              as well if those corrections are used.
!
!              STLOBS will be used in several tests below.
!
               CALL VSUB ( SRFPT, SRFVEC, STLOBS )

!
!              Perform consistency check using NPEDLN for cases where
!              the tangent point doesn't coincide with the observer or
!              surface point.
!
               IF ( ( ALT .GT. 0.D0 ) .AND. ( RANGE .NE. 0.D0 ) ) THEN
!
!                 This is a normal geometric case.
!
!                 Check consistency of outputs. Using the target epoch,
!                 surface point, and observer-to-surface vector,
!                 compute the ray direction in the body-fixed frame and
!                 re-compute the surface point and tangent point.
!
!                 Transform the input ray to the target body-fixed
!                 frame at trgepc.
!
                  CALL PXFRM2 ( RAYFRM, FIXREF, RAYEPC, TRGEPC, XFORM )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL MXV ( XFORM, RAYDIR, FIXDIR )

                  CALL NPEDLN ( RADII(1), RADII(2), RADII(3), &
                                STLOBS,   FIXDIR,   XSRFPT,  XALT )
                  CALL CHCKXC ( .FALSE., ' ', OK )

!
!                 Check SRFPT.
!
                  IF ( RANGE .LT. 1.D7 ) THEN

                     IF ( GEOM ) THEN

                        TOL = 1.D-11

                     ELSE IF ( USECN ) THEN
!
!                       We're using converged light time corrections.
!
                        IF ( .NOT. USESTL ) THEN
                           TOL = TIGHT
                        ELSE
                           TOL = 1.D-11
                        END IF

                     ELSE
!
!                       Light time correction is not converged.
!
                        TOL = 5.D-11

                     END IF

                  ELSE
!
!                    Range to the tangent point is over 1e7 km.
!
                     IF ( GEOM ) THEN

                        TOL = 5.D-11

                     ELSE IF ( USECN ) THEN
!
!                       We're using converged light time corrections.
!
                        IF ( .NOT. USESTL ) THEN
                           TOL = 5.D-11
                        ELSE
                           TOL = 5.D-11
                        END IF

                     ELSE
!
!                       Light time correction is not converged.
!
                        TOL = 1.D-8

                     END IF

                  END IF

                  CALL CHCKAD ( 'SRFPT (npedln)', &
                                SRFPT, '~~/', XSRFPT, 3, TOL, OK )

!
!                 Check TANPT. Compute the expected tangent point.
!
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
!
!                    Light time correction is not converged.
!
                     IF ( RANGE .LT. 1.D7 ) THEN
                        TOL = 1.D-10
                     ELSE
                        TOL = 1.D-9
                     END IF

                  END IF

                  CALL CHCKAD ( 'TANPT (npedln)', &
                                TANPT, '~~/', XTANPT, 3, TOL, OK )
!
!                 Check altitude.
!
                  IF ( XALT .GT. 100.D0 ) THEN
!
!                    Check relative error.
!
                     IF ( GEOM ) THEN

                        IF ( RANGE .LT. 1.D7 ) THEN

                           TOL = TIGHT

                        ELSE

                           TOL = 1.D-9

                        END IF

                     ELSE IF ( USECN ) THEN
!
!                       We're using converged light time corrections.
!
                        IF ( RANGE .LT. 1.D7 ) THEN
!
!                          Ray direction errors induce altitude errors
!                          that are roughly proportional to range.
!                          At closer range, we can use a tighter
!                          tolerance for our altitude checks.
!
                           TOL = TIGHT
                        ELSE
                           TOL = 5.D-10
                        END IF

                     ELSE
!
!                       Light time correction is not converged.
!
                        TOL = 1.D-8

                     END IF

                     CALL CHCKSD ( 'ALT (npedln)', &
                                    ALT, '~/', XALT, TOL, OK )
                  ELSE
!
!                    This is the low-altitude case.
!
!                    Check absolute error.
!
                     IF ( GEOM ) THEN

                        TOL = 1.D-10

                     ELSE IF ( USECN ) THEN
!
!                       We're using converged light time corrections.
!
                        TOL = 1.D-9

                     ELSE
!
!                       Light time correction is not converged.
!
                        TOL = 1.D-9
                     END IF

                     CALL CHCKSD ( 'ALT (npedln)', &
                                    ALT, '~', XALT, TOL, OK )
                  END IF

!
!                 Check range.
!
                  XRANGE = VDIST ( STLOBS, TANPT )

                  IF ( XRANGE .GT. 100.D0 ) THEN

                     TOL = VTIGHT

                     CALL CHCKSD ( 'RANGE (npedln)', &
                                   RANGE, '~/', XRANGE, TOL, OK )
                  ELSE
!
!                    The expected range is under 100 km. Use
!                    a 0.1 mm tolerance for absolute range error.

                     TOL = 1.D-7
                     CALL CHCKSD ( 'RANGE (npedln)', &
                                   RANGE, '~', XRANGE, TOL, OK )
                  END IF

               END IF

!
!              This is the end of the NPEDLN checks, which are
!              performed only when range and altitude are both
!              non-zero.
!
!              At this point, we need to decide whether the ray is
!              "looking away" from the target, in which case the
!              tangent point is supposed to be set equal to the
!              observer's location.
!
!              At this point, we have the inputs needed to find the
!              sub-observer point on the apparent target. We'll use
!              this point later to decide whether or not we have a
!              "look away" ray direction.
!
!              Note that, due to the highly non-spherical shapes of
!              targets, we can't rely on the angular separation between
!              the ray and the observer-target direction for this
!              determination. Instead we look at the angular separation
!              between the ray direction and the zenith direction at
!              the sub-observer point.
!
!              The observer position STLOBS reflects the aberration
!              corrections we're using and target epoch used to
!              evaluate the orientation of the target body-fixed frame.
!
               CALL NEARPT ( STLOBS, RADII(1), RADII(2), RADII(3), &
                             SUBP,   SUBALT                       )
               CALL CHCKXC ( .FALSE., ' ', OK )
!
!              Get the zenith direction at the sub-observer point.
!              We'll use this later.
!
               CALL SURFNM ( RADII(1), RADII(2), RADII(3), &
                             SUBP,     SUBZEN  )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Let THETA be the angular offset of the ray direction
!              from the sub-observer point's zenith direction.
!
               THETA = VSEP ( FIXDIR, SUBZEN )

               IF ( THETA .LT. HALFPI() ) THEN
!
!                 The ray is pointing upward relative to the local
!                 level plane at the sub-observer point. This is a
!                 "look away" case.
!
!                 We expect:
!
!                    - TRGEPC is exactly ET if the locus is the
!                      tangent point; otherwise TRGEPC is derived from
!                      the observer altitude.
!
!                    - RANGE is exactly zero
!
!                    - TANPT coincides with the observer's position
!                      relative to the target center
!
!                    - SRFPT is the near point on the target
!
!                    - ALT is the altitude of the observer above the
!                      target
!
!
!                 Find the nearest point on the target to TANPT.
!
                  CALL NEARPT ( TANPT,    RADII(1), RADII(2), &
                                RADII(3), PNEAR,    XALT     )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  IF (  LIX .EQ. 1 ) THEN
!
!                    The locus is the tangent point, which in this
!                    case coincides with the observer.
!
                     CALL CHCKSD ( 'TRGEPC A', TRGEPC, &
                                    '=',     ET, 0.D0, OK )

                  ELSE
!
!                    The locus is the surface point, which in this
!                    case is the nearest point on the target to the
!                    observer.
!
                     CALL ZZCOREPC ( ABCORR,        ET, &
                                     XALT/CLIGHT(), XEPOCH )

                     TOL = TIGHT

                     CALL CHCKSD ( 'TRGEPC B', TRGEPC, &
                                    '~/',    XEPOCH, TOL, OK )
                  END IF

!
!                 Check the range, which should be exactly 0.
!
                  CALL CHCKSD ( 'RANGE',  RANGE,  '=', 0.D0, 0.D0, OK )

!
!                 Check the tangent point, which should be
!                 equal to the observer position.
!
                  TOL = VTIGHT

                  CALL CHCKAD ( 'TANPT', TANPT, '~~/', &
                                         STLOBS, 3, TOL, OK)

!
!                 Check the surface point, which should be
!                 equal to the near point.
!
                  TOL = VTIGHT

                  CALL CHCKAD ( 'SRFPT', SRFPT, '~~/', &
                                         PNEAR, 3, TOL, OK)

!
!                 Check the tangent point altitude, which should be
!                 equal to the altitude found by NEARPT.
!
                  TOL = VTIGHT

                  CALL CHCKSD ( 'ALT', ALT, '~/', XALT, TOL, OK )


               ELSE IF ( EL .LE. 0.D0 ) THEN
!
!                 This is an intercept case.
!
!                 We expect:
!
!                    - ALT is exactly zero
!
!                    - TANPT is exactly SRFPT
!
!                    - RANGE is exactly ||SRFVEC||
!
!                    - TRGEPC is ET +/- RANGE / c
!
                  CALL CHCKSD ( 'ALT', ALT, '=', 0.D0, 0.D0, OK )

                  CALL CHCKAD ( 'TANPT', TANPT, '=', &
                                         SRFPT, 3, TOL, OK)

                  XRANGE = VNORM( SRFVEC )

                  TOL = 0.D0

                  CALL CHCKSD ( 'RANGE', RANGE, '=', XRANGE, TOL, OK )


                  CALL ZZCOREPC ( ABCORR, ET, RANGE/CLIGHT(), XEPOCH )

                  TOL = TIGHT

                  CALL CHCKSD ( 'TRGEPC', TRGEPC, '~/', &
                                          XEPOCH, TOL, OK )

                  IF ( CORLOC .EQ. 'SURFACE POINT' ) THEN
!
!                    Compare the surface point against that produced
!                    by SINCPT. We can expect good agreement if
!                    we're not using aberration corrections. The
!                    instability of near-limb ray intercepts requires
!                    large tolerances when converged Newtonian light
!                    time corrections are used. We don't attempt
!                    comparisons when stellar aberration corrections
!                    or non-converged light time corrections are used.
!
                     CALL SINCPT ( 'ELLIPSOID', &
                                   TARGET, ET,     FIXREF, ABCORR, &
                                   OBSRVR, RAYFRM, RAYDIR, XPT, &
                                   XPTEPC, XPTVEC, XPTFND         )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     IF ( ( .NOT. USELT ) .OR. USECN ) THEN
!
!                       Verify that an intercept was found.
!
                        CALL CHCKSL ( 'XPTFND', XPTFND, .TRUE., OK )

                        IF ( XPTFND ) THEN
!
!                          Check the target epoch. Use 0.1 microsecond
!                          time tolerance.
!
                           TOL = 1.D-7

                           CALL CHCKSD ( 'TRGEPC', TRGEPC, '~', &
                                                   XPTEPC, TOL, OK )

!
!                          Check the intercept.
!
                           IF ( .NOT. USELT ) THEN
!
!                             Look for agreement at the 1 mm level.
!
                              TOL = 1.D-6

                           ELSE
!
!                             Allow errors of up to 5 m.
!
                              TOL = 5.D0

                           END IF

                           CALL CHCKAD ( 'SRFPT', SRFPT, '~~', &
                                          XPT, 3, TOL, OK )
                        END IF

                     END IF

                  END IF

               ELSE
!
!                 Checks for all normal cases follow.
!
!                 Check target epoch.
!
                  IF ( CORLOC .EQ. 'TANGENT POINT' ) THEN
                     XLT = RANGE / CLIGHT()
                  ELSE
                     XLT = VNORM( SRFVEC ) / CLIGHT()
                  END IF

                  CALL ZZCOREPC ( ABCORR, ET, XLT, XEPOCH )
                  CALL CHCKXC ( .FALSE., ' ', OK )

!
!                 Check the target epoch. Use 0.1 microsecond
!                 time tolerance.
!
                  TOL = 1.D-7
                  CALL CHCKSD ( 'TRGEPC', TRGEPC, '~', XEPOCH, TOL, OK )

!
!                 Find the nearest point on the target to TANPT.
!
                  CALL NEARPT ( TANPT,    RADII(1), RADII(2), &
                                RADII(3), PNEAR,    XALT     )
                  CALL CHCKXC ( .FALSE., ' ', OK )

!
!                 Check altitude of the tangent point. The altitude will
!                 be zero for intercept cases, but no special test is
!                 required for those cases.
!
                  IF ( XALT .GT. 100.D0 ) THEN
!
!                    Check relative error.
!
                     TOL = 1.D-9
                     CALL CHCKSD ( 'ALT (A)', ALT, '~/', &
                                              XALT, TOL, OK )
                  ELSE
!
!                    Check absolute error.
!
                     TOL = 1.D-9
                     CALL CHCKSD ( 'ALT (B)', ALT, '~', &
                                              XALT, TOL, OK )
                  END IF

!
!                 Check range to the tangent point.
!
                  XRANGE = VDIST ( STLOBS, TANPT )

                  IF ( XRANGE .GT. 100.D0 ) THEN
!
!                    This is the higher altitude case. Check relative
!                    error.
!
                     TOL = TIGHT
                     CALL CHCKSD ( 'RANGE', RANGE, '~/', &
                                            XRANGE, TOL, OK )
                  ELSE
!
!                    This is the lower altitude case. Check absolute
!                    error. We can expect agreement at the 1 micron
!                    level.
!
                     TOL = 1.D-9
                     CALL CHCKSD ( 'RANGE', RANGE, '~', &
                                            XRANGE, TOL, OK )
                  END IF

!
!                 Compare SRFPT to the surface point found by NEARPT.
!
!                 Check SRFPT absolute error.
!
                  IF ( RANGE .LT. 1.D7 ) THEN
!
!                    Observer and target are less than 10 M km apart.
!
                     TOL = 1.D-10
                  ELSE
                     TOL = 5.D-7
                  END IF

                  CALL CHCKAD ( 'SRFPT (abs)', SRFPT, '~~', &
                                               PNEAR, 3, TOL, OK )

!
!                 Check SRFPT relative error.
!
                  TOL = 1.D-10

                  CALL CHCKAD ( 'SRFPT (rel)', SRFPT, '~~/', &
                                               PNEAR, 3, TOL, OK )

!
!                 Check the position of the tangent point above the
!                 surface point.
!
                  CALL SURFNM ( RADII(1), RADII(2), RADII(3), &
                                SRFPT, NORMAL )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL VSUB ( TANPT, SRFPT, SRFTAN )

                  SEP = VSEP ( SRFTAN, NORMAL )

                  IF (  ( EL > 0.D0 )  .AND.  ( EL < HALFPI() )  ) THEN
!
!                    Check angular separation of zenith direction at
!                    SRFPT vs the SRFPT-to-TANPT vector.
!
!                    Use looser tolerance here since the relative
!                    errors in TANPT and SRFPT are larger compared to
!                    the lengths of those vectors than they are
!                    relative to the observer-target distance.
!
                     TOL = MAX( 1.D-10, 1.D-6/ALT )

                     CALL CHCKSD ( 'SRFPT-TANPT ZENITH SEP', &
                                   SEP, '~', 0.D0, TOL, OK )
!
!                    Check ALT.
!
                     XALT = VDIST( SRFPT, TANPT )

                     IF ( XALT .GT. 100.D0 ) THEN

                        TOL  = VTIGHT * MAX( RANGE, 1.D0 )

                        CALL CHCKSD ( 'ALT (C)', ALT, '~/', &
                                                 XALT, TOL, OK )
                     ELSE

                        TOL  = 1.D-9

                        CALL CHCKSD ( 'ALT (C)', ALT, '~', &
                                                 XALT, TOL, OK )
                     END IF

                  END IF

                  IF (       ( CORLOC .EQ. 'TANGENT POINT' ) &
                       .AND. ( RANGE  .NE.  0              )  ) THEN
!
!                    We'll treat the tangent point as an ephemeris
!                    object. We'll find the angular separation between
!                    the input ray and the ray from the observer to the
!                    tangent point.
!
!                    Find the aberration-corrected position of the
!                    tangent point relative to the observer, expressed
!                    in the J2000 frame. Convert this vector to the ray
!                    frame.
!
                     CALL SPKCPT ( TANPT,    TARGET,  FIXREF, &
                                   ET,       'J2000', 'TARGET', &
                                   ABCORR,   OBSRVR,  TANSTA,  LT  )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     CALL PXFORM ( 'J2000', RAYFRM, RAYEPC, RAYMAT )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     CALL MXV ( RAYMAT, TANSTA, XRAYDR )
!
!                    Check angular separation of ray direction and
!                    direction to tangent point.
!
                     SEP = VSEP( RAYDIR, XRAYDR )

                     IF ( EL .LT. 2.D-1 ) THEN
!
!                       These are the low angular separation cases.
!
                        IF (       ( RANGE .GT. 0.D0 ) &
                             .AND. ( ALT   .GT. 0.D0 ) ) THEN

                           IF ( RANGE .LT. 1.D7 ) THEN

                              IF ( USECN ) THEN

                                 IF ( USESTL ) THEN
                                    TOL = 5.D-10
                                 ELSE
                                    TOL = 1.D-10
                                 END IF

                              ELSE IF ( USELT ) THEN
!
!                                We expect very loose agreement with
!                                non-converged light time.
!
                                 TOL = 1.D-7

                              ELSE
!
!                                This is the geometric case.
!
                                 TOL = VTIGHT

                              END IF

                           ELSE
!
!                             These are the long-range cases.
!
                              IF ( USECN ) THEN

                                 TOL = 1.D-10

                              ELSE IF ( USELT ) THEN
!
!                                We expect very loose agreement with
!                                non-converged light time.
!
                                 TOL = 1.D-4

                              ELSE
!
!                                This is the geometric case.
!
                                 TOL = VTIGHT

                              END IF

                           END IF

                        ELSE
!
!                          These are special cases: the tangent point
!                          coincides with the observer or the surface
!                          intercept.
!
                           IF ( USELT ) THEN
                              TOL = 1.D-10
                           ELSE
                              TOL = 1.D-10
                           END IF

                        END IF

                     ELSE
!
!                       These are the high angular separation cases.
!
                        IF ( RANGE .GT. 1.D7 ) THEN
!
!                          Large range implies high altitude, in this
!                          case.
!
!                          Note that a high-altitude tangent point,
!                          where Mars is the central body, has very
!                          high velocity.
!
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
!
!                             Geometric case for high angular
!                             separation.
!
                              TOL = 1.D-8

                           END IF

                        END IF

                     END IF

                     CALL CHCKSD ( 'RAY-OBS-TO-TANPT SEP', &
                                   SEP, '~', 0.D0, TOL, OK )


!
!                    Check range against the magnitude of the
!                    position of the tangent point relative to
!                    the observer.
!
                     XRANGE = VNORM( TANSTA )

                     CALL CHCKSD ( 'RANGE (vs TANSTA)', &
                                    RANGE, '~/', XRANGE, TOL, OK )

!
!                    End of TANGENT POINT locus case.
!
                  ELSE IF (      ( CORLOC .EQ. 'SURFACE POINT' ) &
                           .AND. ( ALT    .NE.  0              ) ) THEN
!
!                    The locus is the surface point.
!
!                    We'll treat the surface point as an ephemeris
!                    object. We'll find the angular separation between
!                    the ray found by TANGPT from the observer to the
!                    surface point and the position found by SPKCPT of
!                    the surface point relative to the observer.
!
!                    Find the aberration-corrected position of the
!                    surface point relative to the observer, expressed
!                    in the target body-fixed frame.
!
                     CALL SPKCPT ( SRFPT,    TARGET,  FIXREF, &
                                   ET,       FIXREF, 'TARGET', &
                                   ABCORR,   OBSRVR,  SRFSTA,  LT  )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     CALL VEQU ( SRFSTA, XSRFVC )

                     SEP = VSEP ( SRFVEC, XSRFVC )

                     IF ( EL .LT. 2.D-1 ) THEN
!
!                       These are the low angular separation cases.
!
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

                     CALL CHCKSD ( 'SRFPT SEP', SEP, '~', &
                                                0.D0, TOL, OK )

!
!                    Check the surface vector as well. We can use
!                    the same tolerances.
!
                     CALL CHCKAD ( 'SRFVEC', SRFVEC, '~~/', &
                                             XSRFVC, 3, TOL, OK )

                  END IF
!
!                 This is the end of the normal case check block.
!
               END IF

            END DO
!
!           This is the end of the elevation loop.
!
         END DO
!
!        This is the end of the azimuth loop.
!
      END DO
!
!     This is the end of the case loop.
!

!
! --- Case: ------------------------------------------------------
!
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

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END
