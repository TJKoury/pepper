C$Procedure      F_LIMBPT ( LIMBPT family tests )

      SUBROUTINE F_LIMBPT ( OK )

C$ Abstract
C
C     Exercise the higher-level SPICELIB geometry routine LIMBPT.
C     Use DSK-based and ellipsoidal target shape models.
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

      INCLUDE 'dsk.inc'
      INCLUDE 'dskdsc.inc'
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
C     trapped or unintentional and need reporting.  The test family
C     utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine tests the higher-level SPICELIB
C     geometry routine LIMBPT.
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
C-    TSPICE Version 2.0.0 12-JUN-2021 (NJB)
C
C        Added tests for computations using transmission aberration
C        corrections. Deleted error test case for transmission
C        corrections.
C
C-    TSPICE Version 1.0.0 29-AUG-2016 (NJB)
C
C-&


C
C     SPICELIB functions
C
      DOUBLE PRECISION      JYEAR
      DOUBLE PRECISION      RPD
      DOUBLE PRECISION      TWOPI
      DOUBLE PRECISION      VDIST
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VNORM

      LOGICAL               EQSTR
      LOGICAL               EXISTS
      LOGICAL               MATCHI
      LOGICAL               ODD
C
C     Local parameters
C
      CHARACTER*(*)         DSK0
      PARAMETER           ( DSK0  = 'limbpt_dsk0.bds' )

      CHARACTER*(*)         DSK1
      PARAMETER           ( DSK1  = 'limbpt_dsk1.bds' )

      CHARACTER*(*)         DSK2
      PARAMETER           ( DSK2  = 'limbpt_dsk2.bds' )

      CHARACTER*(*)         DSK3
      PARAMETER           ( DSK3  = 'limbpt_dsk3.bds' )

      CHARACTER*(*)         DSK4
      PARAMETER           ( DSK4  = 'limbpt_dsk4.bds' )


      CHARACTER*(*)         PCK0
      PARAMETER           ( PCK0  = 'test_0008.tpc' )

      CHARACTER*(*)         PCK1
      PARAMETER           ( PCK1  = 'nat.tpc' )


      CHARACTER*(*)         SPK1
      PARAMETER           ( SPK1  = 'limbpt_spk.bsp' )

      CHARACTER*(*)         SPK2
      PARAMETER           ( SPK2  = 'orbiter.bsp' )

      CHARACTER*(*)         SPK3
      PARAMETER           ( SPK3  = 'nat.bsp' )


      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-14 )

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-12 )

      DOUBLE PRECISION      MTIGHT
      PARAMETER           ( MTIGHT = 1.D-10 )

      DOUBLE PRECISION      LOOSE
      PARAMETER           ( LOOSE  = 5.D-6 )


      INTEGER               ALPHA
      PARAMETER           ( ALPHA  = 1000 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 320 )

      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 32 )

      INTEGER               NREF
      PARAMETER           ( NREF   = 1 )

      INTEGER               NABC
      PARAMETER           ( NABC   = 9 )

      INTEGER               ABCLEN
      PARAMETER           ( ABCLEN = 10 )

      INTEGER               TIMLEN
      PARAMETER           ( TIMLEN = 50 )

      INTEGER               NOBS
      PARAMETER           ( NOBS   = 2 )

      INTEGER               NTARG
      PARAMETER           ( NTARG  = 2 )

      INTEGER               NTIMES
      PARAMETER           ( NTIMES = 5 )

      INTEGER               SCID
      PARAMETER           ( SCID   = -499 )

      INTEGER               NCROSS
      PARAMETER           ( NCROSS = 400 )

      INTEGER               NMAP
      PARAMETER           ( NMAP   = 4 )

      INTEGER               NMETH
      PARAMETER           ( NMETH  = 7 )

      INTEGER               NPOLYV
      PARAMETER           ( NPOLYV = 100 )

      INTEGER               NTANGT
      PARAMETER           ( NTANGT = 4 )

      INTEGER               NLOC
      PARAMETER           ( NLOC   = 2 )

      INTEGER               MAXCUT
      PARAMETER           ( MAXCUT = 1000 )

      INTEGER               MAXPNT
      PARAMETER           ( MAXPNT = MAXCUT * 20 )

      INTEGER               UBEL
      PARAMETER           ( UBEL   = 9 )

      INTEGER               UBPL
      PARAMETER           ( UBPL   = 4 )

C
C     Local variables
C
      CHARACTER*(ABCLEN)    ABCS   ( NABC )
      CHARACTER*(ABCLEN)    ABCORR
      CHARACTER*(NAMLEN)    CORLCS ( NLOC )
      CHARACTER*(NAMLEN)    CORLOC
      CHARACTER*(NAMLEN)    FIXREF
      CHARACTER*(NAMLEN)    LABEL
      CHARACTER*(MTHLEN)    METHD2
      CHARACTER*(MTHLEN)    METHOD
      CHARACTER*(MTHLEN)    METHDS ( NMETH )
      CHARACTER*(NAMLEN)    OBSRVR
      CHARACTER*(NAMLEN)    OBSNMS ( NOBS )
      CHARACTER*(NAMLEN)    REFS   ( NREF, NTARG )
      CHARACTER*(NAMLEN)    SPKLOC
      CHARACTER*(NAMLEN)    SRFNMS ( NMAP )
      CHARACTER*(NAMLEN)    TARGET
      CHARACTER*(NAMLEN)    TRGFRM
      CHARACTER*(NAMLEN)    TRGNMS ( NTARG )
      CHARACTER*(LNSIZE)    TITLE
      CHARACTER*(TIMLEN)    UTC

      DOUBLE PRECISION      ALT
      DOUBLE PRECISION      AXIS   ( 3 )
      DOUBLE PRECISION      BADRAD ( 3 )
      DOUBLE PRECISION      CENTER ( 3 )
      DOUBLE PRECISION      CUTNML ( 3 )
      DOUBLE PRECISION      D
      DOUBLE PRECISION      DIST
      DOUBLE PRECISION      ELTS   ( 8 )
      DOUBLE PRECISION      EPOCHS ( MAXCUT )
      DOUBLE PRECISION      EPNTS  ( 3, MAXPNT )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ET0
      DOUBLE PRECISION      ETNGTS ( 3, MAXPNT )
      DOUBLE PRECISION      HTOL
      DOUBLE PRECISION      ISTATE ( 6 )
      DOUBLE PRECISION      ITANVC ( 3 )
      DOUBLE PRECISION      LIMB   ( UBEL )
      DOUBLE PRECISION      LPLANE ( UBPL )
      DOUBLE PRECISION      LPOINT ( 3 )
      DOUBLE PRECISION      LPROJ  ( 3 )
      DOUBLE PRECISION      LPTEPC
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      PLNVEC ( 3 )
      DOUBLE PRECISION      PMCOEF ( 3 )
      DOUBLE PRECISION      PNEAR  ( 3 )
      DOUBLE PRECISION      POINTS ( 3, MAXPNT )
      DOUBLE PRECISION      R
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      RAYDIR ( 3 )
      DOUBLE PRECISION      RCROSS
      DOUBLE PRECISION      REFVEC ( 3 )
      DOUBLE PRECISION      ROLSTP
      DOUBLE PRECISION      SCHSTP
      DOUBLE PRECISION      SMAJOR ( 3 )
      DOUBLE PRECISION      SMINOR ( 3 )
      DOUBLE PRECISION      SOLTOL
      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      STATE0 ( 6 )
      DOUBLE PRECISION      TANGTS ( 3, MAXPNT )
      DOUBLE PRECISION      TANVEC ( 3 )
      DOUBLE PRECISION      TDELTA
      DOUBLE PRECISION      THETA
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      TRGLT
      DOUBLE PRECISION      TRGPOS ( 3 )
      DOUBLE PRECISION      UTAN   ( 3 )
      DOUBLE PRECISION      VERTEX ( 3 )
      DOUBLE PRECISION      XEPOCH ( MAXPNT )
      DOUBLE PRECISION      XFORM  ( 3, 3 )
      DOUBLE PRECISION      XISRFV ( 3 )
      DOUBLE PRECISION      XPOINT ( 3, MAXPNT )
      DOUBLE PRECISION      XPT    ( 3 )
      DOUBLE PRECISION      XSRFVC ( 3 )
      DOUBLE PRECISION      XTE

      INTEGER               ABCIDX
      INTEGER               BODYID
      INTEGER               HANDLE ( 3 )
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               LOCIDX
      INTEGER               MAXN
      INTEGER               MIX
      INTEGER               N
      INTEGER               NCUTS
      INTEGER               NLAT
      INTEGER               NLON
      INTEGER               NPTS   ( MAXCUT )
      INTEGER               NSURF
      INTEGER               OBSCDE
      INTEGER               OBSIDX
      INTEGER               REFIDX
      INTEGER               SRFBOD ( NMAP )
      INTEGER               SRFIDS ( NMAP )
      INTEGER               SURFID
      INTEGER               TIMIDX
      INTEGER               TRGCDE
      INTEGER               TRGIDX
      INTEGER               UB

      LOGICAL               ATTBLK ( ABATSZ )
      LOGICAL               FOUND
      LOGICAL               ISDSK
      LOGICAL               ISELL
      LOGICAL               ISHI
      LOGICAL               ISTAN
      LOGICAL               USECN
      LOGICAL               USELT

C
C     Saved variables
C
      SAVE

C
C     Initial values
C
      DATA                  ABCS   /  'None',
     .                                'Cn',
     .                                'Cn+s',
     .                                'Lt',
     .                                'Lt+s',
     .                                'XCN',
     .                                'xcn+S',
     .                                'lT',
     .                                'LT+S'   /


C
C     This array is two-dimensional. The second dimension
C     is indexed by the input METHOD. This makes it simpler
C     to control the combinations of methods and locus
C     values.
C
      DATA                  CORLCS /  'ELLIPSOID LIMB',
     .                                'CENTER'          /


      DATA                  REFS   /  'IAU_MARS',   'IAU_PHOBOS' /


      DATA                  OBSNMS /  'Earth',
     .                                'MARS_ORBITER'    /


      DATA                  TRGNMS /  'Mars',
     .                                'PHOBOS'    /

C
C     Note that the last two method strings are identical. This
C     is done to test the logic that uses saved values obtained
C     by parsing method string.
C
      DATA                  METHDS /
     .      'ELLIPSOID / tangent',
     .      'tangent/ellipsoid',
     .      'tangent/dsk/unprioritized/surfaces="high-res"',
     .      'tangent/dsk/unprioritized/surfaces="LOW-RES"',
     .      'guided/ellipsoid',
     .      'dsk/ guided /unprioritized/surfaces="high-res"',
     .      'dsk/ guided /unprioritized/surfaces="LOW-RES"'   /



C
C     Begin every test family with an open call.q
C
      CALL TOPEN ( 'F_LIMBPT' )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Setup:  create SPK, PCK file.' )


      IF ( EXISTS(SPK1) ) THEN
         CALL DELFIL ( SPK1 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL TSTSPK ( SPK1, .TRUE., HANDLE(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Create the PCK file, and load it. Do not delete it.
C
      IF ( EXISTS(PCK0) ) THEN
         CALL DELFIL ( PCK0 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL T_PCK08 ( PCK0, .TRUE., .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Optionally scale Mars rotation.
C
      CALL BODVRD ( 'MARS', 'PM', 3, N, PMCOEF )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Leave as is for delivery.
C
      PMCOEF(2) = PMCOEF(2)*1

      CALL PDPOOL ( 'BODY499_PM', 3, PMCOEF )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Create LSK, load it, and delete it.
C
      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Set initial time.
C
      UTC = '2004 FEB 17'
      CALL STR2ET ( UTC, ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET     = ET0

C
C     Create a Mars orbiter SPK file.
C
      IF ( EXISTS(SPK2) ) THEN
         CALL DELFIL ( SPK2 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL SPKOPN ( SPK2, SPK2, 0, HANDLE(2) )
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
      ELTS(1) =  3800.D0
      ELTS(2) =     1.D-1
      ELTS(3) =    80.D0 * RPD()
      ELTS(4) =     0.D0
      ELTS(5) =    90.D0 * RPD()
      ELTS(6) =     0.D0
      ELTS(7) =    ET
      ELTS(8) = 42828.314D0

      CALL CONICS ( ELTS, ET, STATE0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKW05 ( HANDLE(2),    SCID,         499,      'MARSIAU',
     .              -10*JYEAR(),  10*JYEAR(),  'Mars orbiter',
     .              ELTS(8),      1,            STATE0,    ET        )
     .
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKCLS ( HANDLE(2) )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Load the new SPK file.
C
      CALL SPKLEF ( SPK2, HANDLE(2) )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Add the orbiter's name/ID mapping to the kernel pool.
C
      CALL PCPOOL ( 'NAIF_BODY_NAME', 1,  OBSNMS(2) )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL PIPOOL ( 'NAIF_BODY_CODE', 1,  SCID      )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Add an incomplete frame definition to the kernel pool;
C     we'll need this later.
C
      CALL PIPOOL ( 'FRAME_BAD_NAME', 1, -666 )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Setup: create DSK files.' )

C
C     For Mars, surface 1 is the "main" surface.
C
      TARGET = 'MARS'
      TRGCDE = 499
      FIXREF = 'IAU_MARS'

      BODYID = TRGCDE
      SURFID = 1
      NLON   =  220
      NLAT   =  110

      IF ( EXISTS(DSK0) ) THEN
         CALL DELFIL( DSK0 )
      END IF

      CALL T_ELDS2Z ( BODYID, SURFID, FIXREF, NLON, NLAT, DSK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Load main Mars DSK.
C
      CALL FURNSH ( DSK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Surface 2 for Mars has slightly lower resolution.
C
      BODYID = TRGCDE
      SURFID = 2
      NLON   = 190
      NLAT   =  95

      IF ( EXISTS(DSK1) ) THEN
         CALL DELFIL( DSK1 )
      END IF

C
C     Create and load the second DSK.
C
      CALL T_ELDS2Z ( BODYID, SURFID, FIXREF, NLON, NLAT, DSK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH ( DSK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Surface 1 for Phobos is high-res.
C
      BODYID = 401
      SURFID = 1
      FIXREF = 'IAU_PHOBOS'

      NLON   = 200
      NLAT   = 100

      IF ( EXISTS(DSK2) ) THEN
         CALL DELFIL( DSK2 )
      END IF

C
C     Create and load the first Phobos DSK.
C
      CALL T_ELDS2Z ( BODYID, SURFID, FIXREF, NLON, NLAT, DSK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH ( DSK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Surface 2 for Phobos is lower-res.
C
      BODYID = 401
      SURFID = 2
      FIXREF = 'IAU_PHOBOS'

      NLON   =  80
      NLAT   =  40

      IF ( EXISTS(DSK3) ) THEN
         CALL DELFIL( DSK3 )
      END IF

C
C     Create and load the second Phobos DSK.
C
      CALL T_ELDS2Z ( BODYID, SURFID, FIXREF, NLON, NLAT, DSK3 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH ( DSK3 )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Set up a surface name-ID map.
C
      SRFBOD(1) = 499
      SRFIDS(1) = 1
      SRFNMS(1) = 'high-res'

      SRFBOD(2) = 499
      SRFIDS(2) = 2
      SRFNMS(2) = 'low-res'

      SRFBOD(3) = 401
      SRFIDS(3) = 1
      SRFNMS(3) = 'high-res'

      SRFBOD(4) = 401
      SRFIDS(4) = 2
      SRFNMS(4) = 'low-res'



      CALL PCPOOL ( 'NAIF_SURFACE_NAME', NMAP, SRFNMS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PIPOOL ( 'NAIF_SURFACE_CODE', NMAP, SRFIDS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PIPOOL ( 'NAIF_SURFACE_BODY', NMAP, SRFBOD )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
C
C     Main test loop follows.
C
C
C     This loop uses a single epoch. The set of tests using
C     nested tori, located further down in this file, does
C     vary the input times.
C
C     Loop over every choice of observer.
C
      DO OBSIDX = 1, NOBS

         OBSRVR = OBSNMS ( OBSIDX )
C
C        Set the observer ID code.
C
         CALL BODN2C ( OBSRVR, OBSCDE, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Loop over every choice of target.
C
         DO TRGIDX = 1, NTARG

            TARGET = TRGNMS( TRGIDX )
C
C           Set the target ID code.
C
            CALL BODN2C ( TARGET, TRGCDE, FOUND )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Get target radii.
C
            CALL BODVAR ( TRGCDE, 'RADII', N, RADII )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Loop over every aberration correction locus.
C
            DO LOCIDX = 1, NLOC

               CORLOC = CORLCS ( LOCIDX )
C
C              Loop over every aberration correction choice.
C
               DO ABCIDX = 1, NABC

                  ABCORR = ABCS ( ABCIDX )

                  CALL ZZPRSCOR ( ABCORR, ATTBLK )
                  CALL CHCKXC ( .FALSE., ' ', OK )

C
C                 Set up some logical variables describing the
C                 attributes of the selected correction.
C
                  USELT  = ATTBLK( LTIDX  )
                  USECN  = ATTBLK( CNVIDX )

C
C                 Loop over every target body-fixed frame choice.
C
                  DO REFIDX = 1, NREF

                     TRGFRM = REFS ( REFIDX, TRGIDX )
C
C                    Loop over all method choices that are
C                    compatible with the choice of CORLOC.
C
C                    The "center" locus is required  in order
C                    to use the "guided" methods.
C
                     IF (  EQSTR( CORLOC, 'CENTER' )  ) THEN

                        UB = NMETH

                     ELSE
C
C                       The locus is 'ELLIPSOID LIMB'. Only
C                       the "tangent" methods can be used.
C
                        UB = NTANGT

                     END IF


                     DO MIX = 1, UB

                        METHOD = METHDS( MIX )

                        ISELL  =  MATCHI( METHOD, '*ELLIPSOID*',
     .                                    '*',    '?'           )

                        ISDSK  = .NOT. ISELL

                        ISTAN  =  MATCHI( METHOD, '*TANGENT*',
     .                                    '*',    '?'           )

                        ISHI   =  MATCHI( METHOD, '*HIGH*',
     .                                    '*',    '?'           )


C
C --- Case: ------------------------------------------------------
C

                        TITLE = 'Observer = #; Target = #; '
     .                  //      'ABCORR = #; TRGFRM = #; '
     .                  //      'METHOD = #; CORLOC = #.'
                        CALL REPMC ( TITLE, '#', OBSRVR,     TITLE )
                        CALL REPMC ( TITLE, '#', TARGET,     TITLE )
                        CALL REPMC ( TITLE, '#', ABCORR,     TITLE )
                        CALL REPMC ( TITLE, '#', TRGFRM,     TITLE )
                        CALL REPMC ( TITLE, '#', METHOD,     TITLE )
                        CALL REPMC ( TITLE, '#', CORLOC,     TITLE )

                        CALL TCASE ( TITLE )

C
C                       Start off by computing the set of limb points
C                       We'll then check the results.
C
                        NCUTS  = 4

                        ROLSTP = TWOPI() / NCUTS
                        CALL VPACK ( 0.D0, 0.D0, 1.D0, REFVEC )

C
C                       We expect a single limb point in each half
C                       plane, so we can use a large step.
C
                        SCHSTP = 4.D0

C
C                       Derive the solution tolerance from the height
C                       error tolerance and the observer-target center
C                       distance.
C
                        CALL SPKPOS ( TARGET, ET,     TRGFRM, ABCORR,
     .                                OBSRVR, TRGPOS, TRGLT          )
                        CALL CHCKXC ( .FALSE., ' ', OK )

                        DIST   = VNORM( TRGPOS )

                        HTOL   = 1.D-6
                        SOLTOL = HTOL / DIST

                        MAXN   = MAXPNT

C
C                       If the shape is DSK and the locus is ELLIPSOID
C                       limb, make a preliminary call to obtain the
C                       expected epochs.
C
                        IF ( ISDSK .AND. EQSTR( CORLOC,
     .                                          'ELLIPSOID LIMB') ) THEN
C
C                          Create a version of the method string with
C                          an ellipsoid shape specification.
C
                           METHD2 = 'ELLIPSOID/TANGENT'
C
C                          Get the expected epochs for the call below
C                          that uses a DSK target shape.
C
                           CALL LIMBPT ( METHD2, TARGET, ET,     TRGFRM,
     .                                   ABCORR, CORLOC, OBSRVR, REFVEC,
     .                                   ROLSTP, NCUTS,  SCHSTP, SOLTOL,
     .                                   MAXN,   NPTS,   EPNTS,  XEPOCH,
     .                                   ETNGTS                        )
                        END IF


                        CALL LIMBPT ( METHOD, TARGET, ET,     TRGFRM,
     .                                ABCORR, CORLOC, OBSRVR, REFVEC,
     .                                ROLSTP, NCUTS,  SCHSTP, SOLTOL,
     .                                MAXN,   NPTS,   POINTS, EPOCHS,
     .                                TANGTS                          )
                        CALL CHCKXC ( .FALSE., ' ', OK )

C
C                       Check the outputs.
C
                        K = 1

                        DO I = 1, NCUTS
C
C                          For convex shapes that include the origin,
C                          there should be just one limb point per
C                          cutting half-plane.
C
                           LABEL = 'NPTS(#)'
                           CALL REPMI ( LABEL, '#', I, LABEL )
                           CALL CHCKXC ( .FALSE., ' ', OK )

                           CALL CHCKSI ( LABEL, NPTS(I), '=', 1, 0, OK )


                           DO J = 1, NPTS(I)
C
C                             We'll treat the Jth limb point as an
C                             ephemeris object and find its position
C                             relative to the observer.
C
                              CALL VEQU ( POINTS(1,K), LPOINT )
                              CALL VEQU ( TANGTS(1,K), TANVEC )
                              LPTEPC = EPOCHS(I)

C
C                             Get an inertially referenced version
C                             of the tangent vector.
C
                              CALL PXFORM ( 'J2000', TRGFRM,
     .                                      LPTEPC,  XFORM  )
                              CALL CHCKXC ( .FALSE., ' ', OK )

                              CALL MTXV ( XFORM, TANVEC, ITANVC )

C
C                             Set the SPK lookup locus to be compatible
C                             with the aberration correction locus used
C                             with the limb point.
C
                              IF ( CORLOC .EQ. 'CENTER' ) THEN
C
C                                In this case we're using aberration
C                                corrections for the target body
C                                center. We must compute the expected
C                                state of the limb point "manually."
C
                                 CALL SPKPOS ( TARGET,  ET,     TRGFRM,
     .                                         ABCORR,  OBSRVR, TRGPOS,
     .                                         TRGLT                   )
                                 CALL CHCKXC ( .FALSE., ' ', OK )

                                 CALL VADD ( TRGPOS, LPOINT, XSRFVC )
C
C                                For the 'CENTER' locus, epochs
C                                associated with limb points be the
C                                epoch associated with the target
C                                body's center.
C
                                 LT = TRGLT
C
C                                Get an inertially referenced version
C                                of the expected tangent vector.
C
                                 CALL PXFORM ( 'J2000', TRGFRM,
     .                                         LPTEPC,   XFORM  )
                                 CALL CHCKXC ( .FALSE., ' ', OK )

                                 CALL MTXV ( XFORM, XSRFVC, XISRFV )


                              ELSE
C
C                                This is the 'ellipsoid limb' case.
C
                                 SPKLOC  = 'TARGET'

                                 CALL SPKCPT ( LPOINT, TARGET, TRGFRM,
     .                                         ET,     TRGFRM, SPKLOC,
     .                                         ABCORR, OBSRVR, STATE,
     .                                         LT                     )
                                 CALL CHCKXC ( .FALSE., ' ', OK )


                                 CALL VEQU ( STATE, XSRFVC )


                                 CALL SPKCPT ( LPOINT, TARGET,  TRGFRM,
     .                                         ET,     'J2000', SPKLOC,
     .                                         ABCORR, OBSRVR,  ISTATE,
     .                                         LT                     )
                                 CALL CHCKXC ( .FALSE., ' ', OK )

                                 CALL VEQU ( ISTATE, XISRFV )

                              END IF

C
C                             If LPOINT is correct, then the position
C                             of LPOINT relative to the observer should
C                             be equal to TANVEC. The light time
C                             obtained from SPKCPT or computed manually
C                             should match that implied by LPTEPC.
C
                              TOL = VTIGHT

C
C                             If the light time correction is
C                             non-converged, we can't expect tight
C                             agreement.
C
                              IF ( USELT .AND. (.NOT. USECN) ) THEN

                                 TOL = TOL * 1.D2

                              END IF

                              IF ( ISDSK   .AND.
     .                             (CORLOC .EQ. 'ELLIPSOID LIMB') ) THEN
C
C                                Set the expected epoch using that
C                                obtained using the ellipsoid target
C                                shape.
C
                                 XTE = XEPOCH(I)

                              ELSE
C
C                                Compute the expected epoch from the
C                                SPKCPT or SPKPOS call.
C
                                 CALL ZZCOREPC ( ABCORR, ET, LT, XTE )
                                 CALL CHCKXC   ( .FALSE., ' ', OK )

                              END IF

                              LABEL = 'LPTEPC PLANE #, PT #'
                              CALL REPMI ( LABEL, '#', I, LABEL )
                              CALL REPMI ( LABEL, '#', J, LABEL )

                              CALL CHCKSD ( LABEL, LPTEPC, '~/',
     .                                      XTE,   TOL,    OK    )


                              IF ( USELT ) THEN

                                 IF ( USECN ) THEN
                                    TOL = MTIGHT
                                 ELSE
                                    TOL = LOOSE
                                 END IF

                              ELSE
                                 TOL = VTIGHT
                              END IF

                              IF ( ISDSK ) THEN
C
C                                Use looser tolerances, since the
C                                aberration correction performed by
C                                LIMBPT is for the corresponding
C                                ellipsoid limb point.
C
                                 TOL = TOL * 1.D3

                              END IF


                              LABEL = 'TANVEC PLANE #, PT #'
                              CALL REPMI ( LABEL, '#', I, LABEL )
                              CALL REPMI ( LABEL, '#', J, LABEL )


                              IF ( EQSTR( OBSRVR, 'MARS_ORBITER' ) )
     .                        THEN
C
C                                Check the absolute distance error for
C                                this case. Use a 5m tolerance.
C
                                 CALL CHCKAD ( LABEL,  TANVEC, '~~',
     .                                         XSRFVC, 3,      5.D-3,
     .                                         OK )
                              ELSE
C
C                                Compare the inertially referenced
C                                tangent vectors.
C
                                 LABEL = 'ITANVC PLANE #, PT #'
                                 CALL REPMI ( LABEL, '#', I, LABEL )
                                 CALL REPMI ( LABEL, '#', J, LABEL )

C
C                                Use a tighter tolerance for the
C                                inertial vector test.
C
                                 CALL CHCKAD ( LABEL,  ITANVC, '~~/',
     .                                         XISRFV, 3,      TOL/10,
     .                                         OK)

C
C                                Check the relative error for the
C                                body-fixed vectors.
C
                                 CALL CHCKAD ( LABEL,  TANVEC, '~~/',
     .                                         XSRFVC, 3,      TOL,
     .                                         OK)

                              END IF


                              IF ( .NOT. OK ) THEN
C
C                                Perform the following test just to
C                                display the absolute magnitude of
C                                the error.
C
                                 LABEL = 'TANVEC PLANE #, PT #'
                                 CALL REPMI ( LABEL, '#', I, LABEL )
                                 CALL REPMI ( LABEL, '#', J, LABEL )

                                 CALL CHCKAD ( LABEL,  TANVEC, '~~',
     .                                         XSRFVC, 3,      1.D-6,
     .                                         OK )

                                 LABEL = 'ITANVC PLANE #, PT #'
                                 CALL REPMI ( LABEL, '#', I, LABEL )
                                 CALL REPMI ( LABEL, '#', J, LABEL )

                                 CALL CHCKAD ( LABEL,  ITANVC, '~~',
     .                                         XISRFV, 3,      1.D-6,
     .                                         OK )
                              END IF

                              IF ( ( ABCIDX .LE. 3 ) .AND. ISELL ) THEN
C
C                                The correction is 'NONE', 'CN', or
C                                'CN+S'. The shape is 'ELLIPSOID'. Look
C                                for millimeter-level agreement.

                                 CALL CHCKAD ( LABEL,  TANVEC, '~~',
     .                                         XSRFVC, 3,      1.D-6,
     .                                         OK )
                              END IF

C
C                             We've checked the consistency of LPOINT,
C                             TANVEC, and LPTEPC, but we haven't done
C                             anything to show that LPOINT is a limb
C                             point. Do this now.
C
C                             We need to verify that
C
C                                1) The limb point is on the target
C                                   body's surface.
C
C                                2) For the "tangent" methods, the
C                                   limb point is a point of tangency
C                                   on the target of a ray emanating
C                                   from the observer.
C
C                                   For the "guided" methods, the limb
C                                   point lies on a ray emanating from
C                                   the center of the limb of the
C                                   reference ellipsoid.
C
C                                   When the target shape is an
C                                   ellipsoid, there should be no
C                                   difference between results obtained
C                                   using the tangent and guided
C                                   methods.
C
C                                3) The limb point lies in the
C                                   correct half-plane.
C
C
C                             Check the limb point's distance from the
C                             target surface.
C
                              IF ( ISELL ) THEN
C
C                                Find the altitude of the limb point
C                                with respect to the reference
C                                ellipsoid.
C
                                 CALL NEARPT ( LPOINT,   RADII(1),
     .                                         RADII(2), RADII(3),
     .                                         PNEAR,    ALT      )
                                 CALL CHCKXC ( .FALSE., ' ', OK   )

C
C                                For ellipsoidal targets, one micron
C                                is a generous tolerance.
C
                                 TOL = 1.D-9


                                 LABEL = 'LIMBPT ALT, PLANE #, PT #'
                                 CALL REPMI ( LABEL, '#', I, LABEL )
                                 CALL REPMI ( LABEL, '#', J, LABEL )

                                 CALL CHCKSD ( LABEL, ALT, '~',
     .                                         0.D0,  TOL, OK  )

                              ELSE
C
C                                This is the DSK case.
C
C                                We'll create an inward-pointing ray
C                                that ideally passes through the limb
C                                point, and we'll find the ray-surface
C                                intercept. This intercept should be
C                                close to the original limb point.

                                 CALL VSCL   ( 10.D0,   LPOINT, VERTEX )
                                 CALL VMINUS ( VERTEX, RAYDIR )


                                 NSURF = 1

                                 IF ( ISHI ) THEN

                                    SURFID = 1
                                 ELSE
                                    SURFID = 2
                                 END IF


                                 CALL DSKXV  ( .FALSE., TARGET, NSURF,
     .                                         SURFID,  ET,     TRGFRM,
     .                                         1,
     .                                         VERTEX,  RAYDIR, XPT,
     .                                         FOUND          )
                                 CALL CHCKXC ( .FALSE., ' ', OK   )

                                 CALL CHCKSL ( 'DSKXV FOUND', FOUND,
     .                                         .TRUE.,         OK    )

C
C                                Our ray should hit the DSK surface very
C                                close to the limb point. One micron is
C                                enough margin.
C
                                 TOL   = 1.D-9

                                 DIST  = VDIST( XPT, LPOINT )


                                 LABEL = 'LIMBPT DIST, PLANE #, PT #'
                                 CALL REPMI ( LABEL, '#', I, LABEL )
                                 CALL REPMI ( LABEL, '#', J, LABEL )

                                 CALL CHCKSD ( LABEL, DIST, '~',
     .                                         0.D0,  TOL, OK  )

                              END IF

C
C                             Verify that the putative limb point is
C                             really on the limb.
C
C                             Create the central axis and normal to the
C                             current cutting half-plane. We'll use
C                             these for the DSK case below, and for the
C                             next set of tests, in which the limb
C                             point is tested for inclusion in the
C                             cutting half- plane.

                              CALL VSUB ( LPOINT, TANVEC, AXIS )

                              CALL VROTV ( REFVEC,       AXIS,
     .                                     (I-1)*ROLSTP, PLNVEC )

                              CALL CHCKXC ( .FALSE., ' ', OK   )

                              CALL UCRSS ( AXIS, PLNVEC, CUTNML )

C
                              IF ( ISELL ) THEN
C
C                                This is the ellipsoid case.
C
C                                If LPOINT is all it's claimed to be,
C                                the outward normal at LPOINT should be
C                                orthogonal to the observer-limb point
C                                vector.
C
                                 CALL SURFNM ( RADII(1), RADII(2),
     .                                         RADII(3), LPOINT,
     .                                         NORMAL             )
                                 CALL CHCKXC ( .FALSE., ' ', OK   )

                                 CALL VHAT ( TANVEC, UTAN )

                                 D   = VDOT( NORMAL, UTAN )

                                 TOL = VTIGHT


                                 LABEL = '<LIMBPT, TANGNT>, PLANE #, '
     .                           //      'PT #'
                                 CALL REPMI ( LABEL, '#', I, LABEL )
                                 CALL REPMI ( LABEL, '#', J, LABEL )

                                 CALL CHCKSD ( LABEL, D,   '~',
     .                                         0.D0,  TOL, OK  )

                              ELSE
C
C                                This is the DSK case.
C
C                                We could check the relative orientation
C                                of the outward normal and the
C                                associated tangent vector, but we
C                                can't expect these vectors to be
C                                orthogonal.
C
C                                We can perform a more accurate check
C                                by determining whether a small
C                                rotation of the ray defined by the
C                                observer and the tangent vector,
C                                performed within the cutting
C                                half-plane, will move the ray off of
C                                the target.

                                 IF ( ISTAN ) THEN
C
C                                   This is the DSK tangent case.
C
C                                   Rotate the tangent ray away from
C                                   the axis by twice the angular
C                                   search tolerance. Presuming the ray
C                                   was nearly tangential, it should
C                                   now point off the target.
C
                                    THETA = -2 * SOLTOL

                                    CALL VROTV ( TANVEC, CUTNML,
     .                                           THETA,  RAYDIR )

                                    NSURF = 1

                                    IF ( ISHI ) THEN

                                       SURFID = 1
                                    ELSE
                                       SURFID = 2
                                    END IF

                                    CALL DSKXV ( .FALSE., TARGET, NSURF,
     .                                           SURFID,  ET,
     .                                           TRGFRM,  1,
     .                                           AXIS,    RAYDIR,
     .                                           XPT,     FOUND       )
                                    CALL CHCKXC ( .FALSE., ' ', OK   )

                                    LABEL = 'Ray hit PLANE #, '
     .                              //      'PT #'
                                    CALL REPMI ( LABEL, '#', I, LABEL )
                                    CALL REPMI ( LABEL, '#', J, LABEL )

                                    CALL CHCKSL ( LABEL,   FOUND,
     .                                            .FALSE., OK    )

                                 ELSE
C
C                                   This is the DSK "guided" case.
C
C                                   We've already checked that the limb
C                                   point is on the DSK surface; we
C                                   need to make sure it's in the
C                                   reference ellipsoid's limb plane.
C                                   The last check we'll do will ensure
C                                   the point is in the correct cutting
C                                   half-plane.
C
C                                   Create the ellipsoid limb and
C                                   the limb plane.
C
                                    CALL EDLIMB ( RADII(1), RADII(2),
     .                                            RADII(3), AXIS, LIMB )
                                    CALL CHCKXC ( .FALSE., ' ', OK   )


                                    CALL EL2CGV ( LIMB,   CENTER,
     .                                            SMAJOR, SMINOR )
                                    CALL CHCKXC ( .FALSE., ' ', OK   )

                                    CALL PSV2PL ( CENTER, SMAJOR,
     .                                            SMINOR, LPLANE )
C
C                                   Project the guided DSK limb point
C                                   onto the limb plane.
C
                                    CALL VPRJP ( LPOINT, LPLANE, LPROJ )

                                    DIST = VDIST ( LPOINT, LPROJ )
C
C                                   One micron is a generous tolerance
C                                   for the distance of the limb point
C                                   from the ellipsoid limb plane.
C
                                    TOL = 1.D-9


                                    LABEL = 'LIMBPT ERROR, PLANE #, '
     .                              //      'PT #'
                                    CALL REPMI ( LABEL, '#', I, LABEL )
                                    CALL REPMI ( LABEL, '#', J, LABEL )

                                    CALL CHCKSD ( LABEL, ALT, '~',
     .                                            0.D0,  TOL, OK  )
                                 END IF

                              END IF

C
C                             Verify that the limb point is in the
C                             cutting half-plane. This test applies to
C                             both ellipsoids and DSKs.
C
C                             The plane containing the cutting
C                             half-plane contains the origin, so the
C                             distance from the plane of a point is
C                             given by the dot product of the point
C                             with the plane's unit normal vector.
C
                              D   = VDOT( CUTNML, LPOINT )

                              TOL = 1.D-6


                              LABEL = '<LIMBPT, CUTNML>, PLANE #, '
     .                        //      'PT #'
                              CALL REPMI ( LABEL, '#', I, LABEL )
                              CALL REPMI ( LABEL, '#', J, LABEL )

                              CALL CHCKSD ( LABEL, D,   '~',
     .                                      0.D0,  TOL, OK  )


C
C                             Update K to point to the next limb point.
C
                              K = K + 1

                           END DO

                        END DO
C
C                       We're finished with the consistency checks.
C

                     END DO
C
C                    End of the method loop.
C
                  END DO
C
C                 End of the reference frame loop.
C
               END DO
C
C              End of the aberration correction loop.
C
            END DO
C
C           End of the correction locus loop.
C
         END DO
C
C        End of the target loop.
C
      END DO
C
C     End of the observer loop.
C


C***********************************************************************
C
C     Normal case: nested tori as target shape
C
C***********************************************************************

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Torus test setup' )

C
C     Create and load Nat's SPK and PCK.
C
      CALL NATSPK ( SPK3, .TRUE., HANDLE(3) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL NATPCK ( PCK1, .TRUE., .TRUE. )


C
C     Create DSK containing two nested tori.
C
      IF ( EXISTS(DSK4) ) THEN
         CALL DELFIL( DSK4 )
      END IF

      SURFID = 3
      TRGFRM = 'ALPHA_VIEW_XY'

      R      = 7.0D4
      RCROSS = 1.0D4

      CALL VPACK ( 0.D0, 0.D0, 0.D0, CENTER )
      CALL VPACK ( 1.D0, 0.D0, 0.D0, NORMAL )

      CALL T_TORUS( ALPHA, SURFID, TRGFRM, NPOLYV, NCROSS,
     .              R,     RCROSS, CENTER, NORMAL, DSK4   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      R      = 4.0D4

      CALL VPACK ( 0.D0, 0.D0, 0.D0, CENTER )
      CALL VPACK ( 1.D0, 0.D0, 0.D0, NORMAL )

      CALL T_TORUS( ALPHA, SURFID, TRGFRM, NPOLYV, NCROSS,
     .              R,     RCROSS, CENTER, NORMAL, DSK4   )


      CALL FURNSH ( DSK4 )
      CALL CHCKXC ( .FALSE., ' ', OK )




C
C     Prepare LIMBPT call.
C
      OBSRVR = 'SUN'
      TARGET = 'ALPHA'
      CORLOC = 'CENTER'
      METHOD = 'DSK/TANGENT/UNPRIORITIZED'
      ET0    = 0
      TDELTA = 1.0D5
C
C     Loop over epochs.
C
      DO TIMIDX = 1, NTIMES

         ET = ET0 + (TIMIDX-1)*TDELTA

C
C        Loop over the aberration correction choices. For this
C        test, we use only 'NONE' and 'CN'.
C
         DO ABCIDX = 1, 2
C
C--- Case: ------------------------------------------------------
C
            ABCORR = ABCS( ABCIDX )

            TITLE = 'Observer = #; Target = #; '
     .      //      'ABCORR = #; TRGFRM = #; '
     .      //      'METHOD = #; CORLOC = #; ET = #.'
            CALL REPMC ( TITLE, '#', OBSRVR,     TITLE )
            CALL REPMC ( TITLE, '#', TARGET,     TITLE )
            CALL REPMC ( TITLE, '#', ABCORR,     TITLE )
            CALL REPMC ( TITLE, '#', TRGFRM,     TITLE )
            CALL REPMC ( TITLE, '#', METHOD,     TITLE )
            CALL REPMC ( TITLE, '#', CORLOC,     TITLE )
            CALL REPMD ( TITLE, '#', ET,     14, TITLE )
            CALL TCASE ( TITLE )

C
C           Start off by computing the set of limb points
C           We'll then check the results.
C
            NCUTS  = 1
            ROLSTP = TWOPI() / NCUTS
            CALL VPACK ( 0.D0, 0.D0, 1.D0, REFVEC )

C
C           We expect a multiple limb points in each half
C           plane, so we must use a small step.
C
            SCHSTP = 1.D-4

C
C           Derive the solution tolerance from the height
C           error tolerance and the observer-target center
C           distance.
C
            CALL SPKPOS ( TARGET, ET,     TRGFRM, ABCORR,
     .                    OBSRVR, TRGPOS, TRGLT          )
            CALL CHCKXC ( .FALSE., ' ', OK )

            DIST   = VNORM( TRGPOS )

            HTOL   = 1.D-6
            SOLTOL = HTOL / DIST

            MAXN   = MAXPNT

            CALL LIMBPT ( METHOD, TARGET, ET,     TRGFRM,
     .                    ABCORR, CORLOC, OBSRVR, REFVEC,
     .                    ROLSTP, NCUTS,  SCHSTP, SOLTOL,
     .                    MAXN,   NPTS,   POINTS, EPOCHS,
     .                    TANGTS                          )
            CALL CHCKXC ( .FALSE., ' ', OK )


C
C           Check the outputs.
C
            K = 1

            DO I = 1, NCUTS

C
C              There should be 4 limb points in each cutting
C              half-plane.
C
               LABEL = 'NPTS HALF-PLANE #,'
               CALL REPMI ( LABEL, '#', I, LABEL )

               CALL CHCKSI ( LABEL, NPTS(I), '=', 4, 0, OK )


               DO J = 1, NPTS(I)
C
C                 We'll treat the Jth limb point as an
C                 ephemeris object and find its position
C                 relative to the observer.
C
                  CALL VEQU ( POINTS(1,K), LPOINT )
                  CALL VEQU ( TANGTS(1,K), TANVEC )
                  LPTEPC = EPOCHS(I)

                  SPKLOC  = 'TARGET'

                  CALL SPKCPT ( LPOINT, TARGET, TRGFRM,
     .                          ET,     TRGFRM, SPKLOC,
     .                          ABCORR, OBSRVR, STATE,
     .                          LT                     )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL VEQU ( STATE, XSRFVC )

C
C                 Since we're using the 'CENTER' locus, the light time
C                 obtained from SPKPOS should match that implied by
C                 LPTEPC.
C
                  TOL = VTIGHT

                  CALL ZZCOREPC ( ABCORR, ET, TRGLT, XTE )
                  CALL CHCKXC   ( .FALSE., ' ', OK )

                  LABEL = 'LPTEPC PLANE #, PT #'
                  CALL REPMI ( LABEL, '#', I, LABEL )
                  CALL REPMI ( LABEL, '#', J, LABEL )

                  CALL CHCKSD ( LABEL, LPTEPC, '~/',
     .                          XTE,   TOL,    OK    )

C
C                 If LPOINT is correct, then the position
C                 of LPOINT relative to the observer should
C                 be equal to TANVEC.
C
                  IF ( ABCORR .EQ. 'NONE' ) THEN

                     TOL = VTIGHT

                  ELSE
C
C                    Use looser tolerances, since the aberration
C                    correction performed by LIMBPT is for the
C                    target center.

                     TOL = TIGHT

                  END IF


                  LABEL = 'TANVEC PLANE #, PT #'
                  CALL REPMI ( LABEL, '#', I, LABEL )
                  CALL REPMI ( LABEL, '#', J, LABEL )

C
C                 Check the absolute distance error for
C                 this case. Use a 5mm tolerance.
C
                  CALL CHCKAD ( LABEL,  TANVEC, '~~',
     .                          XSRFVC, 3,      5.D-6,
     .                          OK )

C
C                 Check the relative error for this case.
C
                  CALL CHCKAD ( LABEL,  TANVEC, '~~/',
     .                          XSRFVC, 3,      TOL,  OK)


                  IF ( .NOT. OK ) THEN

C
C                    Perform the following test just to
C                    display the absolute magnitude of
C                    the error.
C
                     CALL CHCKAD ( LABEL,  TANVEC, '~~',
     .                             XSRFVC, 3,      1.D-6,
     .                             OK )
                  END IF

C
C                 We've checked the consistency of LPOINT, TANVEC, and
C                 LPTEPC, but we haven't done anything to show that
C                 LPOINT is a limb point. Do this now.
C
C                 Check the limb point's distance from the target
C                 surface.
C
C                 We'll create an inward-pointing ray that ideally
C                 passes through the limb point, and we'll find the
C                 ray-surface intercept. This intercept should be close
C                 to the original limb point.
C
C                 Create the ray's vertex using the outward normal at
C                 the surface point. The ray's direction will be the
C                 negative of the normal.

                  CALL SRFNRM ( 'DSK/UNPRIORITIZED',
     .                          TARGET, ET, TRGFRM, 1, LPOINT, NORMAL )

                  CALL VLCOM  ( 1.0D0,  LPOINT, 0.1D0, NORMAL, VERTEX )
                  CALL VMINUS ( NORMAL, RAYDIR )


                  NSURF = 1

                  CALL DSKXV  ( .FALSE., TARGET, NSURF,
     .                          SURFID,  ET,     TRGFRM,
     .                          1,
     .                          VERTEX,  RAYDIR, XPT,
     .                          FOUND          )
                  CALL CHCKXC ( .FALSE., ' ', OK   )

                  CALL CHCKSL ( 'DSKXV FOUND', FOUND,
     .                          .TRUE.,         OK    )

C
C                 Our ray should hit the DSK surface very
C                 close to the limb point. One micron is
C                 enough margin.
C
                  TOL   = 1.D-9

                  DIST  = VDIST( XPT, LPOINT )


                  LABEL = 'LIMBPT DIST, PLANE #, PT #'
                  CALL REPMI ( LABEL, '#', I, LABEL )
                  CALL REPMI ( LABEL, '#', J, LABEL )

                  CALL CHCKSD ( LABEL, DIST, '~',
     .                          0.D0,  TOL, OK  )

C
C                 Verify that the putative limb point is
C                 really on the limb.
C
C                 Create the central axis and normal to the
C                 current cutting half-plane. We'll use these
C                 for the DSK case below, and for the next
C                 set of tests, in which the limb point is
C                 tested for inclusion in the cutting half-
C                 plane.
C
                  CALL VSUB ( LPOINT, TANVEC, AXIS )

                  CALL VROTV ( REFVEC,       AXIS,
     .                         (I-1)*ROLSTP, PLNVEC )

                  CALL CHCKXC ( .FALSE., ' ', OK   )

                  CALL UCRSS ( AXIS, PLNVEC, CUTNML )

C
C                 This is the DSK tangent case.
C
C                 Rotate the tangent ray to move it off of the target
C                 surface. The rotation is within the cutting
C                 half-plane. The direction of rotation depends on the
C                 placement of the surface point: for points having
C                 outward normals pointing away from the axis, the
C                 rotation is away from the axis; for the other points,
C                 the direction is inward. For the nested tori, the
C                 points having and odd index are the ones for which
C                 the rotation is outward.
C
C                 Outward rotation about CUTNML corresponds to a
C                 negative rotation angle.
C
C                 The rotation angle is set to twice the angular search
C                 tolerance. Presuming the ray was nearly tangential,
C                 it should now point off the target.
C
                  IF ( ODD(J) ) THEN
                     THETA = - 2 * SOLTOL
                  ELSE
                     THETA =   2 * SOLTOL
                  END IF

                  CALL VROTV ( TANVEC, CUTNML, THETA, RAYDIR )


                  NSURF = 1

                  CALL DSKXV ( .FALSE., TARGET, NSURF,
     .                         SURFID,  ET,
     .                         TRGFRM,  1,
     .                         AXIS,    RAYDIR,
     .                         XPT,     FOUND       )
                  CALL CHCKXC ( .FALSE., ' ', OK   )

                  LABEL = 'Ray hit PLANE #, '
     .            //      'PT #'
                  CALL REPMI ( LABEL, '#', I, LABEL )
                  CALL REPMI ( LABEL, '#', J, LABEL )

                  CALL CHCKSL ( LABEL, FOUND, .FALSE., OK )
     .
C
C                 Verify that the limb point is in the cutting
C                 half-plane. This test applies to both ellipsoids and
C                 DSKs.
C
C                 The plane containing the cutting half-plane contains
C                 the origin, so the distance from the plane of a point
C                 is given by the dot product of the point with the
C                 plane's unit normal vector.
C
                  D   = VDOT( CUTNML, LPOINT )

                  TOL = 1.D-6


                  LABEL = '<LIMBPT, CUTNML>, PLANE #, '
     .            //      'PT #'
                  CALL REPMI ( LABEL, '#', I, LABEL )
                  CALL REPMI ( LABEL, '#', J, LABEL )

                  CALL CHCKSD ( LABEL, D,   '~',
     .                          0.D0,  TOL, OK  )

C
C                 Update K to point to the next limb point.
C
                  K = K + 1

               END DO

            END DO

C
C           We're finished with the consistency checks.
C
         END DO
C
C        End of the aberration correction loop.
C
      END DO
C
C     End of the time loop.
C

C
C     Unload the torus DSK. Leaving it loaded would interfere
C     with some of the following tests.
C
      CALL UNLOAD ( DSK4 )
      CALL CHCKXC ( .FALSE., ' ', OK )


C***********************************************************************
C
C     Normal case: input handling
C
C***********************************************************************


C
C --- Case: ------------------------------------------------------
C

C
C     Input handling tests:  make sure target and observer
C     can be identified using integer "names."
C


C***********************************************************************
C
C     Normal case: state change detection
C
C***********************************************************************

C
C     Certain subsystem state changes must be detected and responded to
C     by SINCPT. The subsystems (or structures) having states that must
C     be monitored are:
C
C        - Target name-ID mapping
C
C        - Observer name-ID mapping
C
C        - Surface name-ID mapping
C
C        - Target body-fixed frame definition
C
C        - ZZDSKBSR state
C

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Target name changed to JUPITER for ID code 499.' )

C
C     First, get expected intercept.
C
      ET     = 8 * JYEAR()

      TARGET = 'MARS'
      FIXREF = 'IAU_MARS'
      OBSRVR = 'EARTH'
      ABCORR = 'NONE'
      NCUTS  = 1
      METHOD = 'DSK/UNPRIORITIZED/GUIDED'
      CORLOC = 'CENTER'

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF,
     .              ABCORR, CORLOC, OBSRVR, REFVEC,
     .              ROLSTP, NCUTS,  SCHSTP, SOLTOL,
     .              MAXN,   NPTS,   XPOINT, EPOCHS,
     .              TANGTS                          )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL BODDEF ( 'JUPITER', 499 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF,
     .              ABCORR, CORLOC, OBSRVR, REFVEC,
     .              ROLSTP, NCUTS,  SCHSTP, SOLTOL,
     .              MAXN,   NPTS,   POINTS, EPOCHS,
     .              TANGTS                          )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect an exact match here.
C
      CALL CHCKAD ( 'POINTS', POINTS, '=', XPOINT, 3, 0.D0, OK )


C     Restore original mapping.
C
      CALL BODDEF ( 'JUPITER', 599 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Observer name changed to SUN for ID code 399.' )

      CALL BODDEF ( 'SUN', 399 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF,
     .              ABCORR, CORLOC, OBSRVR, REFVEC,
     .              ROLSTP, NCUTS,  SCHSTP, SOLTOL,
     .              MAXN,   NPTS,   POINTS, EPOCHS,
     .              TANGTS                          )
      CALL CHCKXC ( .FALSE., ' ', OK )
C
C     We expect an exact match here.
C
      CALL CHCKAD ( 'POINTS', POINTS, '=', XPOINT, 3, 0.D0, OK )

C
C     Restore original mapping.
C
      CALL BODDEF ( 'SUN', 10 )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Mars high-res surface name changed to '
     .//           'AAAbbb.'                               )

C
C     Get expected results first.
C
      ET     = 8 * JYEAR()

      TARGET = 'MARS'
      FIXREF = 'IAU_MARS'
      OBSRVR = 'EARTH'
      ABCORR = 'NONE'

      METHOD = 'DSK/UNPRIORITIZED/GUIDED/surfaces = 1'



      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF,
     .              ABCORR, CORLOC, OBSRVR, REFVEC,
     .              ROLSTP, NCUTS,  SCHSTP, SOLTOL,
     .              MAXN,   NPTS,   XPOINT, EPOCHS,
     .              TANGTS                          )
      CALL CHCKXC ( .FALSE., ' ', OK )


      SRFNMS(1) = 'AAAbbb'

      CALL PCPOOL ( 'NAIF_SURFACE_NAME', NMAP, SRFNMS )
      CALL CHCKXC ( .FALSE., ' ', OK )



      METHOD = 'guided/dsk/unprioritized/surfaces = AAAbbb'


      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF,
     .              ABCORR, CORLOC, OBSRVR, REFVEC,
     .              ROLSTP, NCUTS,  SCHSTP, SOLTOL,
     .              MAXN,   NPTS,   POINTS, EPOCHS,
     .              TANGTS                          )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     We expect an exact match here.
C
      CALL CHCKAD ( 'POINTS', POINTS, '=', XPOINT, 3, 0.D0, OK )

C
C     Restore original mapping.
C
      SRFNMS(1) = 'high-res'

      CALL PCPOOL ( 'NAIF_SURFACE_NAME', NMAP, SRFNMS )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Unload Mars high-res DSK.'  )

C
C     Get reference result using low-res Mars DSK.
C
      ET     = 8 * JYEAR()

      TARGET = 'MARS'
      FIXREF = 'IAU_MARS'
      OBSRVR = 'EARTH'
      ABCORR = 'NONE'



      METHOD = 'guided/dsk/unprioritized/surfaces = low-res'

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF,
     .              ABCORR, CORLOC, OBSRVR, REFVEC,
     .              ROLSTP, NCUTS,  SCHSTP, SOLTOL,
     .              MAXN,   NPTS,   XPOINT, EPOCHS,
     .              TANGTS                          )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Unload the high-res DSK; set METHOD to remove
C     surface specification.
C
      CALL UNLOAD ( DSK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      METHOD = 'guided/dsk/unprioritized'

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF,
     .              ABCORR, CORLOC, OBSRVR, REFVEC,
     .              ROLSTP, NCUTS,  SCHSTP, SOLTOL,
     .              MAXN,   NPTS,   POINTS, EPOCHS,
     .              TANGTS                          )
      CALL CHCKXC ( .FALSE., ' ', OK )
C
C     We expect an exact match here.
C
      CALL CHCKAD ( 'POINTS', POINTS, '=', XPOINT, 3, 0.D0, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Unload Mars low-res DSK; '
     .//           'reload Mars high-res DSK.'  )

C
C     Restore DSK, unload low-res DSK, and repeat computation.
C
      CALL FURNSH ( DSK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL UNLOAD ( DSK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      METHOD = 'guided/dsk/unprioritized'

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF,
     .              ABCORR, CORLOC, OBSRVR, REFVEC,
     .              ROLSTP, NCUTS,  SCHSTP, SOLTOL,
     .              MAXN,   NPTS,   POINTS, EPOCHS,
     .              TANGTS                          )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Make sure the result matches that obtained with the
C     high-res DSK specified.
C
      METHOD = 'guided/dsk/unprioritized/ SURFACES = "HIGH-RES" '

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF,
     .              ABCORR, CORLOC, OBSRVR, REFVEC,
     .              ROLSTP, NCUTS,  SCHSTP, SOLTOL,
     .              MAXN,   NPTS,   XPOINT, EPOCHS,
     .              TANGTS                          )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     We expect an exact match here.
C
      CALL CHCKAD ( 'POINTS', POINTS, '=', XPOINT, 3, 0.D0, OK )






C***********************************************************************
C
C     Error handling tests follow.
C
C***********************************************************************


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Invalid method.' )

      ET     = 8 * JYEAR()

      TARGET = 'MARS'
      FIXREF = 'IAU_MARS'
      OBSRVR = 'EARTH'
      ABCORR = 'NONE'



      METHOD = 'ELLIPSOID TANGENT'

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF,
     .              ABCORR, CORLOC, OBSRVR, REFVEC,
     .              ROLSTP, NCUTS,  SCHSTP, SOLTOL,
     .              MAXN,   NPTS,   POINTS, EPOCHS,
     .              TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(BADMETHODSYNTAX)', OK )

      METHOD = 'DSK/GUIDED'

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF,
     .              ABCORR, CORLOC, OBSRVR, REFVEC,
     .              ROLSTP, NCUTS,  SCHSTP, SOLTOL,
     .              MAXN,   NPTS,   POINTS, EPOCHS,
     .              TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(BADPRIORITYSPEC)', OK )

      METHOD = 'DSK/UNPRIORITIZED'

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF,
     .              ABCORR, CORLOC, OBSRVR, REFVEC,
     .              ROLSTP, NCUTS,  SCHSTP, SOLTOL,
     .              MAXN,   NPTS,   POINTS, EPOCHS,
     .              TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDLIMBTYPE)', OK )

      METHOD = 'UNPRIORITIZED /DSK/guided/INTERCEPT'

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF,
     .              ABCORR, CORLOC, OBSRVR, REFVEC,
     .              ROLSTP, NCUTS,  SCHSTP, SOLTOL,
     .              MAXN,   NPTS,   POINTS, EPOCHS,
     .              TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDMETHOD)', OK )


      METHOD = 'UNPRIORITIZED/umbral /DSK/guided/'

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF,
     .              ABCORR, CORLOC, OBSRVR, REFVEC,
     .              ROLSTP, NCUTS,  SCHSTP, SOLTOL,
     .              MAXN,   NPTS,   POINTS, EPOCHS,
     .              TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDMETHOD)', OK )

C
C     Restore a valid method. We'll use the tangent limb
C     type because it requires the search step and
C     tolerance.
C
      METHOD = 'TANGENT/DSK/UNPRIORITIZED'

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Invalid target name.' )

      CALL LIMBPT ( METHOD, 'marr', ET,     FIXREF,
     .              ABCORR, CORLOC, OBSRVR, REFVEC,
     .              ROLSTP, NCUTS,  SCHSTP, SOLTOL,
     .              MAXN,   NPTS,   POINTS, EPOCHS,
     .              TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Invalid observer name.' )

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF,
     .              ABCORR, CORLOC, 'sn',   REFVEC,
     .              ROLSTP, NCUTS,  SCHSTP, SOLTOL,
     .              MAXN,   NPTS,   POINTS, EPOCHS,
     .              TANGTS                          )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Observer is target.' )

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF,
     .              ABCORR, CORLOC, TARGET, REFVEC,
     .              ROLSTP, NCUTS,  SCHSTP, SOLTOL,
     .              MAXN,   NPTS,   POINTS, EPOCHS,
     .              TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Invalid reference frame center' )

      CALL LIMBPT ( METHOD, TARGET, ET,     'IAU_MOON',
     .              ABCORR, CORLOC, OBSRVR, REFVEC,
     .              ROLSTP, NCUTS,  SCHSTP, SOLTOL,
     .              MAXN,   NPTS,   POINTS, EPOCHS,
     .              TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Reference frame not found' )

      CALL LIMBPT ( METHOD, TARGET, ET,     'IAU_M',
     .              ABCORR, CORLOC, OBSRVR, REFVEC,
     .              ROLSTP, NCUTS,  SCHSTP, SOLTOL,
     .              MAXN,   NPTS,   POINTS, EPOCHS,
     .              TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(NOFRAME)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Invalid aberration correction' )

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF,
     .              'L',    CORLOC, OBSRVR, REFVEC,
     .              ROLSTP, NCUTS,  SCHSTP, SOLTOL,
     .              MAXN,   NPTS,   POINTS, EPOCHS,
     .              TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )

C
C     Test SAVE logic by repeating the call.
C
      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF,
     .              'L',    CORLOC, OBSRVR, REFVEC,
     .              ROLSTP, NCUTS,  SCHSTP, SOLTOL,
     .              MAXN,   NPTS,   POINTS, EPOCHS,
     .              TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Relativistic aberration correction' )
      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF,
     .              'RL',   CORLOC, OBSRVR, REFVEC,
     .              ROLSTP, NCUTS,  SCHSTP, SOLTOL,
     .              MAXN,   NPTS,   POINTS, EPOCHS,
     .              TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Stellar aberration correction w/o light time' )

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF,
     .              'S',    CORLOC, OBSRVR, REFVEC,
     .              ROLSTP, NCUTS,  SCHSTP, SOLTOL,
     .              MAXN,   NPTS,   POINTS, EPOCHS,
     .              TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Invalid aberration correction locus' )

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF,
     .              ABCORR, 'dsk',  OBSRVR, REFVEC,
     .              ROLSTP, NCUTS,  SCHSTP, SOLTOL,
     .              MAXN,   NPTS,   POINTS, EPOCHS,
     .              TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDLOCUS)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Invalid reference vector.' )

      CALL CLEARD ( 6, STATE )

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF,
     .              ABCORR, CORLOC, OBSRVR, STATE,
     .              ROLSTP, NCUTS,  SCHSTP, SOLTOL,
     .              MAXN,   NPTS,   POINTS, EPOCHS,
     .              TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(ZEROVECTOR)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Invalid cut count.' )

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF,
     .              ABCORR, CORLOC, OBSRVR, REFVEC,
     .              ROLSTP, 0,      SCHSTP, SOLTOL,
     .              MAXN,   NPTS,   POINTS, EPOCHS,
     .              TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)', OK )

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF,
     .              ABCORR, CORLOC, OBSRVR, REFVEC,
     .              ROLSTP, -1,     SCHSTP, SOLTOL,
     .              MAXN,   NPTS,   POINTS, EPOCHS,
     .              TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)', OK )

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF,
     .              ABCORR, CORLOC, OBSRVR, REFVEC,
     .              ROLSTP, MAXN+1, SCHSTP, SOLTOL,
     .              MAXN,   NPTS,   POINTS, EPOCHS,
     .              TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Invalid maximum point count.' )

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF,
     .              ABCORR, CORLOC, OBSRVR, REFVEC,
     .              ROLSTP, NCUTS,  SCHSTP, SOLTOL,
     .              0,      NPTS,   POINTS, EPOCHS,
     .              TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSIZE)', OK )

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF,
     .              ABCORR, CORLOC, OBSRVR, REFVEC,
     .              ROLSTP, NCUTS,  SCHSTP, SOLTOL,
     .              -1,   NPTS,   POINTS, EPOCHS,
     .              TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSIZE)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Invalid search tolerance.' )

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF,
     .              ABCORR, CORLOC, OBSRVR, REFVEC,
     .              ROLSTP, NCUTS,  SCHSTP, -1.D0,
     .              MAXN,   NPTS,   POINTS, EPOCHS,
     .              TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDTOLERANCE)', OK )


      CALL LIMBPT ( 'ellipsoid/tangent',
     .              TARGET, ET,     FIXREF,
     .              ABCORR, CORLOC, OBSRVR, REFVEC,
     .              ROLSTP, NCUTS,  -1.D0,  SOLTOL,
     .              MAXN,   NPTS,   POINTS, EPOCHS,
     .              TANGTS                          )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Invalid search step.' )

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF,
     .              ABCORR, CORLOC, OBSRVR, REFVEC,
     .              ROLSTP, NCUTS,  -1.D0,  SOLTOL,
     .              MAXN,   NPTS,   POINTS, EPOCHS,
     .              TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSEARCHSTEP)', OK )

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF,
     .              ABCORR, CORLOC, OBSRVR, REFVEC,
     .              ROLSTP, NCUTS,  0.D0,   SOLTOL,
     .              MAXN,   NPTS,   POINTS, EPOCHS,
     .              TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSEARCHSTEP)', OK )


      CALL LIMBPT ( 'ellipsoid/tangent',
     .              TARGET, ET,     FIXREF,
     .              ABCORR, CORLOC, OBSRVR, REFVEC,
     .              ROLSTP, NCUTS,  -1.D0,  SOLTOL,
     .              MAXN,   NPTS,   POINTS, EPOCHS,
     .              TANGTS                          )
      CALL CHCKXC ( .FALSE., ' ', OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Invalid roll step.' )

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF,
     .              ABCORR, CORLOC, OBSRVR, REFVEC,
     .              0.D0,   2,      SCHSTP, SOLTOL,
     .              MAXN,   NPTS,   POINTS, EPOCHS,
     .              TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDROLLSTEP)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'No loaded SPK files' )

      CALL SPKUEF ( HANDLE(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKUEF ( HANDLE(2) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKUEF ( HANDLE(3) )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF,
     .              ABCORR, CORLOC, OBSRVR, REFVEC,
     .              ROLSTP, NCUTS,  SCHSTP, SOLTOL,
     .              MAXN,   NPTS,   POINTS, EPOCHS,
     .              TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(NOLOADEDFILES)', OK )


      CALL SPKLEF ( SPK1, HANDLE(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKLEF ( SPK2, HANDLE(2) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKLEF ( SPK3, HANDLE(3) )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'No ephemeris data for observer' )



      CALL LIMBPT ( METHOD, TARGET, ET,       FIXREF,
     .              ABCORR, CORLOC, 'GASPRA', REFVEC,
     .              ROLSTP, NCUTS,  SCHSTP,   SOLTOL,
     .              MAXN,   NPTS,   POINTS,   EPOCHS,
     .              TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'No ephemeris data for target' )

      CALL FURNSH ( DSK4 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKUEF ( HANDLE(3) )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL LIMBPT ( METHOD, 'ALPHA',  ET,     'ALPHAFIXED',
     .              ABCORR, CORLOC,   OBSRVR, REFVEC,
     .              ROLSTP, NCUTS,    SCHSTP, SOLTOL,
     .              MAXN,   NPTS,     POINTS, EPOCHS,
     .              TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

      CALL UNLOAD ( DSK4 )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'No orientation data for target' )


C
C     Fetch target radii.
C
      CALL BODVCD (  499, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )
C
C     Clear the kernel pool.
C
      CALL CLPOOL
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Restore target radii so we can get to the error
C     condition we're looking for.
C
      CALL PDPOOL ( 'BODY499_RADII', 3, BADRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF,
     .              ABCORR, CORLOC, OBSRVR, REFVEC,
     .              ROLSTP, NCUTS,  SCHSTP, SOLTOL,
     .              MAXN,   NPTS,   POINTS, EPOCHS,
     .              TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)', OK )

      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH ( PCK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'No radius data for target' )

C
C     We need a method that uses an ellipsoid for this one.
C
      METHOD = 'DSK / UNPRIORITIZED / GUIDED'

      CALL DVPOOL ( 'BODY499_RADII' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF,
     .              ABCORR, CORLOC, OBSRVR, REFVEC,
     .              ROLSTP, NCUTS,  SCHSTP, SOLTOL,
     .              MAXN,   NPTS,   POINTS, EPOCHS,
     .              TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )


      CALL FURNSH ( PCK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Bad radius data for target' )


C
C     Fetch original radii.
C
      CALL BODVCD (  499, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Overwrite good radii with bad in the kernel pool.
C
      CALL VPACK  ( -1.D0, 0.D0,  3.D0, BADRAD )

      CALL PDPOOL ( 'BODY499_RADII', 3, BADRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF,
     .              ABCORR, CORLOC, OBSRVR, REFVEC,
     .              ROLSTP, NCUTS,  SCHSTP, SOLTOL,
     .              MAXN,   NPTS,   POINTS, EPOCHS,
     .              TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDAXISLENGTH)', OK )

C
C     Replace original radii.
C
      CALL PDPOOL ( 'BODY499_RADII', 3, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Bad radius count for target' )


C
C     Fetch original radii.
C
      CALL BODVCD (  499, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Overwrite good radii with short array
C     in the kernel pool.
C
      CALL PDPOOL ( 'BODY499_RADII', 2, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF,
     .              ABCORR, CORLOC, OBSRVR, REFVEC,
     .              ROLSTP, NCUTS,  SCHSTP, SOLTOL,
     .              MAXN,   NPTS,   POINTS, EPOCHS,
     .              TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(BADRADIUSCOUNT)', OK )

C
C     Replace original radii.
C
      CALL PDPOOL ( 'BODY499_RADII', 3, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Not enough room in output arrays.' )

C
C     The checks on NCUTS and MAXN prevent this situation from
C     occurring in the usual case, in which there is one limb
C     point per cutting half-plane. The torus case can present
C     problems.
C
C     Prepare LIMBPT call.
C
      OBSRVR = 'SUN'
      TARGET = 'ALPHA'
      FIXREF = 'ALPHAFIXED'
      CORLOC = 'CENTER'
      METHOD = 'DSK/TANGENT/UNPRIORITIZED'
      ET     = 0.D0
      NCUTS  = 2
      MAXN   = 2

      CALL FURNSH ( DSK4 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH ( PCK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKLEF ( SPK3, HANDLE(3) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF,
     .              ABCORR, CORLOC, OBSRVR, REFVEC,
     .              ROLSTP, NCUTS,  SCHSTP, SOLTOL,
     .              MAXN,   NPTS,   POINTS, EPOCHS,
     .              TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(OUTOFROOM)', OK )

      CALL UNLOAD ( DSK4 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKUEF ( HANDLE(3) )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'GUIDED limb definition w/ ELLIPSOID LIMB '
     .//           'correction locus.'                        )

      TARGET = 'MARS'
      FIXREF = 'IAU_MARS'
      OBSRVR = 'EARTH'
      ABCORR = 'NONE'
      CORLOC = 'ELLIPSOID LIMB'

      METHOD = 'GUIDED/DSK/UNPRIORITIZED'

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF,
     .              ABCORR, CORLOC, OBSRVR, REFVEC,
     .              ROLSTP, NCUTS,  SCHSTP, SOLTOL,
     .              MAXN,   NPTS,   POINTS, EPOCHS,
     .              TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(BADLIMBLOCUSMIX)', OK )

C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'No loaded DSKs.' )


      CALL UNLOAD ( DSK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL UNLOAD ( DSK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL UNLOAD ( DSK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL UNLOAD ( DSK3 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET     = 8 * JYEAR()

      TARGET = 'MARS'
      FIXREF = 'IAU_MARS'
      OBSRVR = 'EARTH'
      ABCORR = 'NONE'


      METHOD = 'guided/dsk/unprioritized'

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF,
     .              ABCORR, CORLOC, OBSRVR, REFVEC,
     .              ROLSTP, NCUTS,  SCHSTP, SOLTOL,
     .              MAXN,   NPTS,   POINTS, EPOCHS,
     .              TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(NOLOADEDDSKFILES)', OK )


C
C --- Case: ------------------------------------------------------
C

C
C     Clean up.
C
      CALL KCLEAR


      CALL DELFIL ( PCK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( PCK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKUEF ( HANDLE(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL DELFIL ( SPK1      )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKUEF ( HANDLE(2) )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL DELFIL ( SPK2      )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKUEF ( HANDLE(3) )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL DELFIL ( SPK3      )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL UNLOAD ( DSK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL DELFIL ( DSK0      )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL UNLOAD ( DSK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL DELFIL ( DSK1      )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL UNLOAD ( DSK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL DELFIL ( DSK2      )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL UNLOAD ( DSK3 )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL DELFIL ( DSK3      )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL UNLOAD ( DSK4 )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL DELFIL ( DSK4      )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL T_SUCCESS ( OK )

      RETURN
      END



C***********************************************************************
C
C     S U P P O R T   U T I L I T I E S
C
C***********************************************************************

C
C     None so far....
C
