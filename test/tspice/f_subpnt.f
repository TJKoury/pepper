C$Procedure      F_SUBPNT ( SUBPNT family tests )

      SUBROUTINE F_SUBPNT ( OK )

C$ Abstract
C
C     Exercise the higher-level SPICELIB geometry routine SUBPNT.
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
C     geometry routine SUBPNT.
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
C-    TSPICE Version 4.0.0 04-AUG-2020 (NJB)
C
C        Updated to include a test using a DSK with time range not
C        including the J2000 epoch.
C
C-    TSPICE Version 3.0.0 06-APR-2016 (NJB)
C
C        Updated to test DSK usage. Also more thoroughly
C        tests ellipsoid target cases.
C
C-    TSPICE Version 2.0.0, 02-APR-2012 (NJB)
C
C           Added cases for bad aberration correction values.
C
C-    TSPICE Version 1.0.0, 22-FEB-2008 (NJB)
C
C
C-&


C
C     SPICELIB functions
C
      DOUBLE PRECISION      JYEAR
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      RPD

      LOGICAL               EXISTS
      LOGICAL               MATCHI
C
C     Local parameters
C
      CHARACTER*(*)         DSK0
      PARAMETER           ( DSK0  = 'subpnt_dsk0.bds' )

      CHARACTER*(*)         DSK1
      PARAMETER           ( DSK1  = 'subpnt_dsk1.bds' )

      CHARACTER*(*)         DSK2
      PARAMETER           ( DSK2  = 'subpnt_dsk2.bds' )

      CHARACTER*(*)         DSK3
      PARAMETER           ( DSK3  = 'subpnt_dsk3.bds' )

      CHARACTER*(*)         DSK4
      PARAMETER           ( DSK4  = 'subpnt_dsk4.bds' )

      CHARACTER*(*)         PCK
      PARAMETER           ( PCK   = 'test_0008.tpc' )

      CHARACTER*(*)         SPK1
      PARAMETER           ( SPK1  = 'subpnt_spk.bsp' )

      CHARACTER*(*)         SPK2
      PARAMETER           ( SPK2  = 'orbiter.bsp' )


      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT  = 1.D-14 )

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT   = 1.D-12 )

      DOUBLE PRECISION      MTIGHT
      PARAMETER           ( MTIGHT  = 1.D-10 )

      DOUBLE PRECISION      LOOSE
      PARAMETER           ( LOOSE  = 5.D-6 )


      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 240 )

      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 32 )

      INTEGER               NREF
      PARAMETER           ( NREF = 1 )

      INTEGER               NABC
      PARAMETER           ( NABC = 9 )

      INTEGER               ABCLEN
      PARAMETER           ( ABCLEN = 10 )

      INTEGER               TIMLEN
      PARAMETER           ( TIMLEN = 50 )

      INTEGER               NOBS
      PARAMETER           ( NOBS   = 2 )

      INTEGER               NTARG
      PARAMETER           ( NTARG  = 2 )

      INTEGER               NTIMES
      PARAMETER           ( NTIMES = 3 )

      INTEGER               SCID
      PARAMETER           ( SCID   = -499 )

      INTEGER               NMAP
      PARAMETER           ( NMAP   = 4 )

      INTEGER               NMETH
      PARAMETER           ( NMETH  = 8 )

C
C     Local variables
C
      CHARACTER*(ABCLEN)    ABCS   ( NABC )
      CHARACTER*(ABCLEN)    ABCORR
      CHARACTER*(NAMLEN)    FIXREF
      CHARACTER*(MTHLEN)    METHOD
      CHARACTER*(MTHLEN)    METHDS ( NMETH )
      CHARACTER*(NAMLEN)    OBSRVR
      CHARACTER*(NAMLEN)    OBSNMS ( NOBS )
      CHARACTER*(NAMLEN)    REFS   ( NREF, NTARG )
      CHARACTER*(NAMLEN)    SRFNMS ( NMAP )
      CHARACTER*(NAMLEN)    TARGET
      CHARACTER*(NAMLEN)    TRGFRM
      CHARACTER*(NAMLEN)    TRGNMS ( NTARG )
      CHARACTER*(LNSIZE)    TITLE
      CHARACTER*(TIMLEN)    UTC

      DOUBLE PRECISION      ALT
      DOUBLE PRECISION      BADRAD ( 3 )
      DOUBLE PRECISION      BOUNDS ( 2, 3 )
      DOUBLE PRECISION      CORPAR ( NSYPAR )
      DOUBLE PRECISION      ELTS   ( 8 )
      DOUBLE PRECISION      ESUBPT ( 3 )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ET0
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      LAST
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      OBSPOS ( 3 )
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      SPOINT ( 3 )
      DOUBLE PRECISION      SRFVEC ( 3 )
      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      STATE0 ( 6 )
      DOUBLE PRECISION      SUBDIR ( 3 )
      DOUBLE PRECISION      TDELTA
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      TRGEPC
      DOUBLE PRECISION      XEPOCH
      DOUBLE PRECISION      XSUBPT ( 3 )
      DOUBLE PRECISION      XSRFVC ( 3 )
      DOUBLE PRECISION      XTE

      INTEGER               ABCIDX
      INTEGER               BODYID
      INTEGER               HANDLE ( 2 )
      INTEGER               MIX
      INTEGER               N
      INTEGER               NLAT
      INTEGER               NLON
      INTEGER               OBSCDE
      INTEGER               OBSIDX
      INTEGER               REFIDX
      INTEGER               SRFBOD ( NMAP )
      INTEGER               SRFIDS ( NMAP )
      INTEGER               SURFID
      INTEGER               SRFLST ( MAXSRF )
      INTEGER               TIMIDX
      INTEGER               TRGCDE
      INTEGER               TRGIDX

      LOGICAL               FOUND
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
     .                                'Lt',
     .                                'Lt+s',
     .                                'Cn',
     .                                'Cn+s',
     .                                'Xlt',
     .                                'Xlt+s',
     .                                'Xcn',
     .                                'Xcn+s'  /

C
C     REFS is a two-dimensional array. There's a set of
C     ray reference  frames for each target. Currently
C     there are only two targets: Mars and Phobos.
C
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
     .      'ELLIPSOID / intercept',
     .      'intercept: ellipsoid',
     .      'near point: ellipsoid',
     .      'near point /ellipsoid',
     .      'nadir/dsk/unprioritized/surfaces="high-res"',
     .      'dsk/ nadir /unprioritized/surfaces="high-res"',
     .      'intercept/ UNPRIORITIZED/ dsk /SURFACES ="LOW-RES"',
     .      'intercept/UNPRIORITIZED/ dsk /SURFACES ="LOW-RES"'
     .                             /

C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_SUBPNT' )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Setup:  create SPK, PCK file.' )

      CALL TSTSPK ( SPK1, .TRUE., HANDLE(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Create the PCK file, and load it. Do not delete it.
C
      CALL T_PCK08 ( PCK, .TRUE., .TRUE. )
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
      TDELTA = JYEAR()

C
C     Create a Mars orbiter SPK file.
C
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
      NLON   =  200
      NLAT   =  100

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
C     Surface 2 for Mars is very low-res.
C
      BODYID = TRGCDE
      SURFID = 2
      NLON   = 40
      NLAT   = 20

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
C     Surface 1 for Phobos is low-res.
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
C     Create DSK for Mars with coverage shifted from the normal
C     range.
C
C     Use surface 3.
C
      TARGET = 'MARS'
      TRGCDE = 499
      FIXREF = 'IAU_MARS'

      BODYID = TRGCDE
      SURFID = 3
      NLON   = 40
      NLAT   = 20

      FIRST  = JYEAR()
      LAST   = JYEAR() * 2

      IF ( EXISTS(DSK4) ) THEN
         CALL DELFIL( DSK4 )
      END IF

      CALL CLEARD( NSYPAR, CORPAR )

      CALL BODVCD( TRGCDE, 'RADII', 3, N, RADII )

      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = 2 * PI()
      BOUNDS(1,2) = -PI()/2
      BOUNDS(2,2) =  PI()/2
      BOUNDS(1,3) = 0.D0
      BOUNDS(2,3) = RADII(1)

      CALL T_ELDSK2 ( BODYID,   SURFID, FIXREF,  FIRST,    LAST,
     .                LATSYS,   CORPAR, BOUNDS,  RADII(1), RADII(2),
     .                RADII(3), NLON,   NLAT,   .FALSE.,   DSK4     )

      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
C
C     Main test loop follows.
C

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
C           Loop over the time sequence.
C
            DO TIMIDX = 1, NTIMES

               ET = ET0 + (TIMIDX-1)*TDELTA

C
C              Loop over every aberration correction choice.
C

               DO ABCIDX = 1, NABC

                  ABCORR = ABCS ( ABCIDX )
C
C                 Set up some logical variables describing the
C                 attributes of the selected correction.
C
                  USELT  = ABCORR .NE. 'None'

                  USECN  =       ( ABCORR(1:2) .EQ. 'Cn'  )
     .                      .OR. ( ABCORR(1:3) .EQ. 'Xcn' )

C
C                 Loop over every target body-fixed frame choice.
C
                  DO REFIDX = 1, NREF

                     TRGFRM = REFS ( REFIDX, TRGIDX )

C
C                    Loop over all method choices.
C

                     DO MIX = 1, NMETH

                        METHOD = METHDS( MIX )

C
C --- Case: ------------------------------------------------------
C

                        TITLE = 'Observer = #; Target = #; '
     .                  //      'ABCORR = #; TRGFRM = #; '
     .                  //      'METHOD = #; ET = #.'
                        CALL REPMC ( TITLE, '#', OBSRVR,     TITLE )
                        CALL REPMC ( TITLE, '#', TARGET,     TITLE )
                        CALL REPMC ( TITLE, '#', ABCORR,     TITLE )
                        CALL REPMC ( TITLE, '#', TRGFRM,     TITLE )
                        CALL REPMC ( TITLE, '#', METHOD,     TITLE )
                        CALL REPMD ( TITLE, '#', ET,     14, TITLE )

                        CALL TCASE ( TITLE )

C
C                       Start off by computing the sub-observer point.
C                       We'll then check the results.
C
                        CALL SUBPNT ( METHOD, TARGET, ET,
     .                                TRGFRM, ABCORR, OBSRVR,
     .                                SPOINT, TRGEPC, SRFVEC )
                        CALL CHCKXC ( .FALSE., ' ', OK )

C
C                       We'll treat the computed sub-observer point as
C                       an ephemeris object and find its position
C                       relative to the observer.
C
                        CALL SPKCPT ( SPOINT, TARGET,   TRGFRM, ET,
     .                                TRGFRM, 'TARGET', ABCORR, OBSRVR,
     .                                STATE,  LT                     )
                        CALL CHCKXC ( .FALSE., ' ', OK )

C
C                       If SPOINT is correct, then the position of
C                       SPOINT relative to the observer should be equal
C                       to SRFVEC. The light time obtained from SPKCPT
C                       should match that implied by TRGEPC.
C
                        TOL = TIGHT

                        CALL ZZCOREPC ( ABCORR, ET, LT, XTE )
                        CALL CHCKXC   ( .FALSE., ' ', OK )

                        CALL CHCKSD ( 'TRGEPC', TRGEPC, '~/',
     .                                XTE,      TOL,    OK    )

                        IF ( USELT ) THEN

                           IF ( USECN ) THEN
                              TOL = MTIGHT
                           ELSE
                              TOL = LOOSE
                           END IF

                        ELSE
                           TOL = VTIGHT
                        END IF

                        CALL CHCKAD ( 'SRFVEC', SRFVEC, '~~/',
     .                                STATE,    3,      TOL,  OK )

C
C                       We've checked the consistency of SPOINT,
C                       SRFVEC, and TRGEPC, but we haven't done
C                       anything to show that SPOINT is a sub-observer
C                       point. Do that now.
C

C                       Compute the position of the observer relative to
C                       the target center.
C
                        CALL VSUB ( SPOINT, STATE, OBSPOS )


                        FOUND = .FALSE.

                        IF (  MATCHI( METHOD, '*INTERCEPT*',
     .                                '*',    '?'            ) ) THEN

                           IF (  MATCHI( METHOD, '*ELLIPSOID*',
     .                                   '*',    '?'            ) ) THEN

                              CALL SURFPT ( OBSPOS,   STATE,
     .                                      RADII(1), RADII(2),
     .                                      RADII(3), XSUBPT,  FOUND )
                              CALL CHCKXC   ( .FALSE., ' ', OK )


                           ELSE
C
C                             This is the DSK case. Note that we're
C                             working with the target epoch (although
C                             in this case, it's only used for DSK
C                             segment selection).
C
C                             Set the surface list according to the
C                             value of the surface list in METHOD.
C
                              IF ( MATCHI( METHOD, '*HIGH-RES*',
     .                                     '*',    '?'          ) ) THEN

                                 SRFLST(1) = 1
                              ELSE
                                 SRFLST(1) = 2
                              END IF


                              CALL DSKXV ( .FALSE., TARGET, 1, SRFLST,
     .                                     TRGEPC,  TRGFRM, 1, OBSPOS,
     .                                     STATE,   XSUBPT,    FOUND  )
                              CALL CHCKXC   ( .FALSE., ' ', OK )

                           END IF

                           CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

C
C                          Use the tolerance we set for the
C                          SRFVEC test.
C
C                          To simplify the comparison, we'll convert
C                          the expected sub-observer point to an
C                          expected observer-surface vector. This will
C                          give us a vector having the scale on which
C                          we based our tolerance magnitude.
C
                           CALL VSUB ( XSUBPT, OBSPOS, XSRFVC )

                           CALL CHCKAD( 'SRFVEC (2)', SRFVEC, '~~/',
     .                                  XSRFVC,    3, TOL,    OK   )

                        ELSE
C
C                          This is the "near point" or "nadir" case.
C
C                          We need the ellipsoid near point for both
C                          the ellipsoid and DSK target shape cases.
C
                           CALL NEARPT ( OBSPOS,   RADII(1),
     .                                   RADII(2), RADII(3),
     .                                   ESUBPT,   ALT       )
                           CALL CHCKXC   ( .FALSE., ' ', OK )


                           IF (  MATCHI( METHOD, '*ELLIPSOID*',
     .                                   '*',    '?'            ) ) THEN
C
C                             This is the ellipsoid case.
C
                              CALL VEQU ( ESUBPT, XSUBPT )

                           ELSE
C
C                             This is the DSK case.
C
C                             Compute the direction vector of a ray
C                             pointing from the observer to the
C                             ellipsoid near point.
C
                              CALL VSUB ( ESUBPT, OBSPOS, SUBDIR )
C
C                             Set the surface list according to the
C                             value of the surface list in METHOD.
C
                              IF ( MATCHI( METHOD, '*HIGH-RES*',
     .                                     '*',    '?'          ) ) THEN

                                 SRFLST(1) = 1
                              ELSE
                                 SRFLST(1) = 2
                              END IF

C
C                             Update XSUBPT to be the DSK surface
C                             intercept of the ray from the observer to
C                             the ellipsoid near point.
C
                              CALL DSKXV ( .FALSE., TARGET, 1, SRFLST,
     .                                     TRGEPC,  TRGFRM, 1, OBSPOS,
     .                                     SUBDIR,  XSUBPT,    FOUND  )
                              CALL CHCKXC   ( .FALSE., ' ', OK )

                              CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

                           END IF
C
C                          Update XSRFVC based on the expected near
C                          point or nadir point.
C
                           CALL VSUB ( XSUBPT, OBSPOS, XSRFVC )

                           CALL CHCKAD( 'SRFVEC (2)', SRFVEC, '~~/',
     .                                  XSRFVC,    3, TOL,    OK   )


                        END IF
C
C                       We're finished with the consistency checks for
C                       the intercept and near point/nadir cases.
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
C           End of the time loop.
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
      CALL TCASE ( 'Use integer observer and target names.' )

C
C     Set target and target-fixed frame.
C
      OBSRVR = 'EARTH'
      TARGET = 'MARS'
      FIXREF = 'IAU_MARS'
      METHOD = 'ellipsoid/intercept'

      CALL SUBPNT ( METHOD, TARGET, ET,     FIXREF,
     .              ABCORR, OBSRVR, XSUBPT, XEPOCH, XSRFVC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SUBPNT ( METHOD, '499',  ET,     FIXREF,
     .              ABCORR, '399',  SPOINT, TRGEPC, SRFVEC )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKAD ( 'SPOINT', SPOINT, '=', XSUBPT, 3, 0.D0, OK )
      CALL CHCKAD ( 'SRFVEC', SRFVEC, '=', XSRFVC, 3, 0.D0, OK )
      CALL CHCKSD ( 'TRGEPC', TRGEPC, '=', XEPOCH,    0.D0, OK )


C***********************************************************************
C
C     Normal case: DSK time bounds don't include J2000.
C
C***********************************************************************


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE( 'Use DSK that doesn''t include J2000 epoch in '
     .//          'its coverage.' )


      CALL UNLOAD ( DSK0 )
      CALL UNLOAD ( DSK1 )
      CALL UNLOAD ( DSK2 )
      CALL UNLOAD ( DSK3 )
      CALL UNLOAD ( DSK4 )

C
C     Verify that no DSK data are available.
C
      ET     = 1.5D0 * JYEAR()

      METHOD = 'DSK/UNPRIORITIZED/INTERCEPT'
      FIXREF = 'IAU_MARS'
      ABCORR = 'NONE'

      CALL SUBPNT ( METHOD, '499',  ET,     FIXREF,
     .              ABCORR, '399',  SPOINT, TRGEPC, SRFVEC )

      CALL CHCKXC ( .TRUE., 'SPICE(NOLOADEDDSKFILES)', OK )

C
C     Make a call that requires data available at a TDB time not equal
C     to J2000.
C
      CALL FURNSH ( DSK4 )

      CALL SUBPNT ( METHOD, '499',  ET,     FIXREF,
     .              ABCORR, '399',  SPOINT, TRGEPC, SRFVEC )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Unload DSK4 so as to not interfere with later tests.
C
      CALL UNLOAD ( DSK4 )

C
C     Restore previously loaded DSKs.
C
      CALL FURNSH( DSK0 )
      CALL FURNSH( DSK1 )
      CALL FURNSH( DSK2 )
      CALL FURNSH( DSK3 )

C***********************************************************************
C
C     Normal case: state change detection
C
C***********************************************************************

C
C     Certain subsystem state changes must be detected and responded to
C     by SUBPNT. The subsystems (or structures) having states that must
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



      CALL SUBPNT ( METHOD, TARGET, ET,     FIXREF,
     .              ABCORR, OBSRVR, XSUBPT, XEPOCH, XSRFVC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL BODDEF ( 'JUPITER', 499 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SUBPNT ( METHOD, TARGET, ET,     FIXREF,
     .              ABCORR, OBSRVR, SPOINT, TRGEPC, SRFVEC )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect an exact match here.
C
      CALL CHCKAD ( 'SPOINT', SPOINT, '=', XSUBPT, 3, 0.D0, OK )


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


      CALL SUBPNT ( METHOD, TARGET, ET,     FIXREF,
     .              ABCORR, OBSRVR, SPOINT, TRGEPC, SRFVEC )
      CALL CHCKXC ( .FALSE., ' ', OK )
C
C     We expect an exact match here.
C
      CALL CHCKAD ( 'SPOINT', SPOINT, '=', XSUBPT, 3, 0.D0, OK )

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



      METHOD = 'intercept/dsk/unprioritized/surfaces = 1'

      CALL SUBPNT ( METHOD, TARGET, ET,     FIXREF,
     .              ABCORR, OBSRVR, XSUBPT, XEPOCH, XSRFVC )
      CALL CHCKXC ( .FALSE., ' ', OK )


      SRFNMS(1) = 'AAAbbb'

      CALL PCPOOL ( 'NAIF_SURFACE_NAME', NMAP, SRFNMS )
      CALL CHCKXC ( .FALSE., ' ', OK )



      METHOD = 'intercept/dsk/unprioritized/surfaces = AAAbbb'

      CALL SUBPNT ( METHOD, TARGET, ET,     FIXREF,
     .              ABCORR, OBSRVR, SPOINT, TRGEPC, SRFVEC )
      CALL CHCKXC ( .FALSE., ' ', OK )



C
C     We expect an exact match here.
C
      CALL CHCKAD ( 'SPOINT', SPOINT, '=', XSUBPT, 3, 0.D0, OK )

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



      METHOD = 'nadir/dsk/unprioritized/surfaces = low-res'

      CALL SUBPNT ( METHOD, TARGET, ET,     FIXREF,
     .              ABCORR, OBSRVR, XSUBPT, XEPOCH, XSRFVC )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Unload the high-res DSK; set METHOD to remove
C     surface specification.
C
      CALL UNLOAD ( DSK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      METHOD = 'nadir/dsk/unprioritized'

      CALL SUBPNT ( METHOD, TARGET, ET,     FIXREF,
     .              ABCORR, OBSRVR, SPOINT, TRGEPC, SRFVEC )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect an exact match here.
C
      CALL CHCKAD ( 'SPOINT', SPOINT, '=', XSUBPT, 3, 0.D0, OK )


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

      METHOD = 'nadir/dsk/unprioritized'

      CALL SUBPNT ( METHOD, TARGET, ET,     FIXREF,
     .              ABCORR, OBSRVR, SPOINT, TRGEPC, SRFVEC )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Make sure the result matches that obtained with the
C     high-res DSK specified.
C
      METHOD = 'nadir/dsk/unprioritized/ SURFACES = "HIGH-RES" '

      CALL SUBPNT ( METHOD, TARGET, ET,     FIXREF,
     .              ABCORR, OBSRVR, XSUBPT, XEPOCH, XSRFVC )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     We expect an exact match here.
C
      CALL CHCKAD ( 'SPOINT', SPOINT, '=', XSUBPT, 3, 0.D0, OK )



C***********************************************************************
C
C     Error handling tests follow.
C
C***********************************************************************


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Invalid method.' )

      CALL SUBPNT ( 'INTERCPT/ ELLIPSOID',
     .              'EARTH', ET,  'IAU_EARTH', 'NONE', 'SUN',
     .              SPOINT,  TRGEPC, SRFVEC             )

      CALL CHCKXC ( .TRUE., 'SPICE(BADMETHODSYNTAX)', OK )


      CALL SUBPNT ( 'INTERCEPT: ELLIPSOIDD',
     .              'EARTH', ET,  'IAU_EARTH', 'NONE', 'SUN',
     .              SPOINT,  TRGEPC, SRFVEC             )

      CALL CHCKXC ( .TRUE., 'SPICE(BADMETHODSYNTAX)', OK )

      CALL SUBPNT ( 'ellipsoid / INTERCEPT /',
     .              'EARTH', ET,  'IAU_EARTH', 'NONE', 'SUN',
     .              SPOINT,  TRGEPC, SRFVEC             )

      CALL CHCKXC ( .TRUE., 'SPICE(BADMETHODSYNTAX)', OK )


      CALL SUBPNT ( 'ELLIPSOID',
     .              'EARTH', ET,  'IAU_EARTH', 'NONE', 'SUN',
     .              SPOINT,  TRGEPC, SRFVEC             )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSUBTYPE)', OK )


      CALL SUBPNT ( 'INTERCEPT ELLIPSOID',
     .              'EARTH', ET,  'IAU_EARTH', 'NONE', 'SUN',
     .              SPOINT,  TRGEPC, SRFVEC             )

      CALL CHCKXC ( .TRUE., 'SPICE(BADMETHODSYNTAX)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Invalid target name.' )

      CALL SUBPNT ( 'INTERCEPT: ELLIPSOID',
     .              'ERTH', ET,  'IAU_EARTH', 'NONE', 'SUN',
     .              SPOINT,  TRGEPC, SRFVEC             )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Invalid observer name.' )

      CALL SUBPNT ( 'INTERCEPT: ELLIPSOID',
     .              'EARTH', ET,  'IAU_EARTH', 'NONE', 'SN',
     .              SPOINT,  TRGEPC, SRFVEC             )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Observer is target.' )

      CALL SUBPNT ( 'INTERCEPT: ELLIPSOID',
     .              'EARTH', ET,  'IAU_EARTH', 'NONE', 'earth',
     .              SPOINT,  TRGEPC, SRFVEC             )
      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Invalid reference frame center' )

      CALL SUBPNT ( 'INTERCEPT: ELLIPSOID',
     .              'EARTH', ET,  'IAU_MOON', 'NONE', 'SUN',
     .              SPOINT,  TRGEPC, SRFVEC             )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Reference frame not found' )

      CALL SUBPNT ( 'INTERCEPT: ELLIPSOID',
     .              'EARTH', ET,  'IAU_EART', 'NONE', 'sun',
     .              SPOINT,  TRGEPC, SRFVEC             )
      CALL CHCKXC ( .TRUE., 'SPICE(NOFRAME)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Invalid aberration correction' )

      CALL SUBPNT ( 'INTERCEPT: ELLIPSOID',
     .              'EARTH', ET,  'IAU_EARTH', 'LTT', 'sun',
     .              SPOINT,  TRGEPC, SRFVEC             )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )

C
C     Test SAVE logic by repeating the call.
C
      CALL SUBPNT ( 'INTERCEPT: ELLIPSOID',
     .              'EARTH', ET,  'IAU_EARTH', 'LTT', 'sun',
     .              SPOINT,  TRGEPC, SRFVEC             )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Relativistic aberration correction' )

      CALL SUBPNT ( 'INTERCEPT: ELLIPSOID',
     .              'EARTH', ET,  'IAU_EARTH', 'RL', 'sun',
     .              SPOINT,  TRGEPC, SRFVEC             )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Stellar aberration correction w/o light time' )

      CALL SUBPNT ( 'INTERCEPT: ELLIPSOID',
     .              'EARTH', ET,  'IAU_EARTH', 'S', 'sun',
     .              SPOINT,  TRGEPC, SRFVEC             )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'No loaded SPK files' )

      CALL SPKUEF ( HANDLE(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKUEF ( HANDLE(2) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SUBPNT ( 'INTERCEPT: ELLIPSOID',
     .              'EARTH', ET,  'IAU_EARTH', 'LT+S', 'sun',
     .              SPOINT,  TRGEPC, SRFVEC             )
      CALL CHCKXC ( .TRUE., 'SPICE(NOLOADEDFILES)', OK )


      CALL SPKLEF ( SPK1, HANDLE(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKLEF ( SPK2, HANDLE(2) )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'No ephemeris data for observer' )


      CALL SUBPNT ( 'INTERCEPT: ELLIPSOID',
     .              'EARTH', ET,  'IAU_EARTH', 'LT+S', '1000',
     .              SPOINT,  TRGEPC, SRFVEC             )
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'No ephemeris data for target' )


      CALL SUBPNT ( 'INTERCEPT: ELLIPSOID',
     .              'gaspra', ET,  'IAU_GASPRA', 'LT+S', '10',
     .              SPOINT,  TRGEPC, SRFVEC             )
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'No orientation data for target' )

      CALL CLPOOL
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SUBPNT ( 'INTERCEPT: ELLIPSOID',
     .              'earth', ET,  'IAU_EARTH', 'LT+S', '10',
     .              SPOINT,  TRGEPC, SRFVEC             )
      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)', OK )

      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL LDPOOL ( PCK )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'No radius data for target' )

      CALL DVPOOL ( 'BODY399_RADII' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SUBPNT ( 'INTERCEPT: ELLIPSOID',
     .              'earth', ET,  'IAU_EARTH', 'LT+S', '10',
     .              SPOINT,  TRGEPC, SRFVEC             )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )


      CALL LDPOOL ( PCK )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Bad radius data for target' )


C
C     Fetch original radii.
C
      CALL BODVCD (  399, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Overwrite good radii with bad in the kernel pool.
C
      CALL VPACK  ( -1.D0, 0.D0,  3.D0, BADRAD )

      CALL PDPOOL ( 'BODY399_RADII', 3, BADRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SUBPNT ( 'INTERCEPT: ELLIPSOID',
     .              'earth', ET,  'IAU_EARTH', 'LT+S', '10',
     .              SPOINT,  TRGEPC, SRFVEC             )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )

C
C     Replace original radii.
C
      CALL PDPOOL ( 'BODY399_RADII', 3, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )



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


      METHOD = 'nadir/dsk/unprioritized'

      CALL SUBPNT ( METHOD, '499',  ET,     FIXREF,
     .              ABCORR, '399',  SPOINT, TRGEPC, SRFVEC )
      CALL CHCKXC ( .TRUE., 'SPICE(NOLOADEDDSKFILES)', OK )




C
C --- Case: ------------------------------------------------------
C

C
C     Clean up.
C
      CALL DELFIL ( PCK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKUEF ( HANDLE(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL DELFIL ( SPK1      )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKUEF ( HANDLE(2) )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL DELFIL ( SPK2      )
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
