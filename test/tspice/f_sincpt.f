C$Procedure      F_SINCPT ( SINCPT family tests )

      SUBROUTINE F_SINCPT ( OK )

C$ Abstract
C
C     Exercise the higher-level SPICELIB geometry routine SINCPT.
C     Use both DSK-based and ellipsoidal target shape models.
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
C     geometry routine SINCPT.
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
C-    TSPICE Version 3.0.0 02-APR-2016 (NJB)
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
      DOUBLE PRECISION      CLIGHT
      DOUBLE PRECISION      JYEAR
      DOUBLE PRECISION      RPD
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      VSEP

      LOGICAL               EQSTR
      LOGICAL               EXISTS
      LOGICAL               MATCHI
C     
C     Local parameters
C
      CHARACTER*(*)         DSK0
      PARAMETER           ( DSK0  = 'sincpt_dsk0.bds' )

      CHARACTER*(*)         DSK1
      PARAMETER           ( DSK1  = 'sincpt_dsk1.bds' )

      CHARACTER*(*)         DSK2
      PARAMETER           ( DSK2  = 'sincpt_dsk2.bds' )

      CHARACTER*(*)         DSK3
      PARAMETER           ( DSK3  = 'sincpt_dsk3.bds' )

      CHARACTER*(*)         PCK 
      PARAMETER           ( PCK   = 'test_0008.tpc' )

      CHARACTER*(*)         SPK1
      PARAMETER           ( SPK1  = 'sincpt_spk.bsp' )

      CHARACTER*(*)         SPK2
      PARAMETER           ( SPK2  = 'orbiter.bsp' )


C      DOUBLE PRECISION      VTIGHT 
C      PARAMETER           ( VTIGHT  = 1.D-14 )

      DOUBLE PRECISION      TIGHT 
      PARAMETER           ( TIGHT   = 1.D-12 )

      DOUBLE PRECISION      MTIGHT 
      PARAMETER           ( MTIGHT  = 1.D-10 )

      DOUBLE PRECISION      MEDIUM
      PARAMETER           ( MEDIUM = 1.D-8 )

      DOUBLE PRECISION      LOOSE
      PARAMETER           ( LOOSE  = 5.D-6 )

      DOUBLE PRECISION      SLOPPY
      PARAMETER           ( SLOPPY = 1.D-3 )


      INTEGER               UBEL
      PARAMETER           ( UBEL    = 9 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 160 )

      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 32 )

      INTEGER               NREF
      PARAMETER           ( NREF = 4 )

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

      INTEGER               NGEOM
      PARAMETER           ( NGEOM  = 4 )

      INTEGER               SCID
      PARAMETER           ( SCID   = -499 )

      INTEGER               NMAP
      PARAMETER           ( NMAP   = 4 )

      INTEGER               NMETH
      PARAMETER           ( NMETH  = 4 )

C
C     Local variables
C
      CHARACTER*(ABCLEN)    ABCS   ( NABC )
      CHARACTER*(ABCLEN)    ABCORR
      CHARACTER*(NAMLEN)    DREF 
      CHARACTER*(NAMLEN)    FIXREF
      CHARACTER*(LNSIZE)    GEOM
      CHARACTER*(LNSIZE)    GEOMS  ( NGEOM )
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

      DOUBLE PRECISION      AXISTA ( 6 )
      DOUBLE PRECISION      AXLT
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      DEPOCH
      DOUBLE PRECISION      DIST
      DOUBLE PRECISION      DJ2    ( 3 )
      DOUBLE PRECISION      DJ2M   ( 3, 3 )
      DOUBLE PRECISION      DLT
      DOUBLE PRECISION      DVEC   ( 3 )
      DOUBLE PRECISION      DVECFX ( 3 )
      DOUBLE PRECISION      DVECJ2 ( 3 )
      DOUBLE PRECISION      ELTS   ( 8 )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ETOL
      DOUBLE PRECISION      FRAC
      DOUBLE PRECISION      J2OBS  ( 3 )
      DOUBLE PRECISION      LCENTR ( 3 )
      DOUBLE PRECISION      LIMB   ( UBEL )
      DOUBLE PRECISION      LMBOFF ( 3 )
      DOUBLE PRECISION      NEGVEC ( 3 )
      DOUBLE PRECISION      OBSPOS ( 3 )
      DOUBLE PRECISION      OBSVEC ( 3 )
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      RAYDIR ( 3 )
      DOUBLE PRECISION      RAYSTA ( 6 )
      DOUBLE PRECISION      REFPOS ( 3 )
      DOUBLE PRECISION      RLT
      DOUBLE PRECISION      SEP
      DOUBLE PRECISION      SMAJOR ( 3 )
      DOUBLE PRECISION      SMINOR ( 3 )
      DOUBLE PRECISION      SPOINT ( 3 )
      DOUBLE PRECISION      SPNTLT
      DOUBLE PRECISION      SRFVEC ( 3 )
      DOUBLE PRECISION      SRFVJ2 ( 3 )
      DOUBLE PRECISION      SSBOBS ( 6 )
      DOUBLE PRECISION      SSBTRG ( 6 )
      DOUBLE PRECISION      STATE0 ( 6 )
      DOUBLE PRECISION      TE
      DOUBLE PRECISION      TGT    ( 3 )
      DOUBLE PRECISION      TIPFX  ( 3 )
      DOUBLE PRECISION      TIPM   ( 3, 3 )
      DOUBLE PRECISION      TLT
      DOUBLE PRECISION      TMPVEC ( 3 )
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      TRGEPC
      DOUBLE PRECISION      TRGJ2M ( 3, 3 )
      DOUBLE PRECISION      XEPOCH
      DOUBLE PRECISION      XFORM  ( 3, 3 )
      DOUBLE PRECISION      XLT
      DOUBLE PRECISION      XOBSPS ( 3 )
      DOUBLE PRECISION      XPT    ( 3 )
      DOUBLE PRECISION      XRAY   ( 3 )
      DOUBLE PRECISION      XSPNT  ( 3 )
      DOUBLE PRECISION      XSRFVC ( 3 )
      DOUBLE PRECISION      XTE

      INTEGER               ABCIDX
      INTEGER               BODYID
      INTEGER               CLS
      INTEGER               CLSSID
      INTEGER               DREFID
      INTEGER               FRCODE
      INTEGER               GEOMIX
      INTEGER               HANDLE ( 2 )
      INTEGER               MIX
      INTEGER               N
      INTEGER               NLAT
      INTEGER               NLON
      INTEGER               OBSCDE
      INTEGER               OBSIDX
      INTEGER               REFCTR
      INTEGER               REFIDX
      INTEGER               SRFBOD ( NMAP )
      INTEGER               SRFIDS ( NMAP )
      INTEGER               SURFID
      INTEGER               SRFLST ( MAXSRF )
      INTEGER               TRGCDE
      INTEGER               TRGIDX

      LOGICAL               FND
      LOGICAL               FOUND
      LOGICAL               USECN
      LOGICAL               USELT
      LOGICAL               USESTL

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
      DATA                  REFS   /  'J2000',      'J2000',
     .                                'ECLIPJ2000', 'ECLIPJ2000',
     .                                'IAU_MARS',   'IAU_PHOBOS',
     .                                'IAU_EARTH',  'IAU_EARTH'  /

      DATA                  GEOMS  /  'POINT_AT_CENTER',
     .                                'MISS_BACKWARD',
     .                                'LIMB_INSIDE_NEAR',
     .                                'MISS_LIMB_NEAR'    /


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
     .                     'ELLIPSOID',
     .                     'dsk/unprioritized/surfaces="high-res"',
     .                     'UNPRIORITIZED/ dsk /SURFACES ="LOW-RES"',
     .                     'UNPRIORITIZED/ dsk /SURFACES ="LOW-RES"'
     .                             /
 
C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_SINCPT' )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Setup:  create SPK, PCK file.' )

      CALL TSTSPK ( SPK1, .TRUE., HANDLE(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Create the PCK file, load it, and delete it.
C      
      CALL T_PCK08 ( PCK, .TRUE., .FALSE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Create LSK, load it, and delete it.
C      
      CALL TSTLSK 
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Set time.
C
      UTC = '2004 FEB 17'
      CALL STR2ET ( UTC, ET )
      CALL CHCKXC ( .FALSE., ' ', OK )

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
      NLON   = 1200
      NLAT   =  600

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
C           Get target body-fixed frame name.
C
            CALL CNMFRM ( TARGET, FRCODE, TRGFRM, FOUND )
            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

C
C           Get target radii.
C
            CALL BODVAR ( TRGCDE, 'RADII', N, RADII )
            CALL CHCKXC ( .FALSE., ' ', OK )


C
C           Loop over every viewing geometry case.
C
            DO GEOMIX = 1, NGEOM

               GEOM = GEOMS ( GEOMIX )

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

                  USESTL = INDEX ( ABCORR, '+s' )  .NE.  0

C
C                 Loop over every direction vector frame choice.
C
                  DO REFIDX = 1, NREF

                     DREF = REFS ( REFIDX, TRGIDX )
C
C                    Set light time RLT from observer to center of
C                    frame for the direction vector.
C                 
                     CALL NAMFRM ( DREF,   DREFID )
                     CALL FRINFO ( DREFID, REFCTR, CLS, CLSSID, FND ) 
  
                     CALL SPKEZP ( REFCTR, ET, 'J2000', ABCORR, OBSCDE,
     .                             REFPOS, RLT                         )

C
C                    We'll need the epoch DEPOCH associated
C                    with the center of DREF.  RLT is the 
C                    light time from DREF's center to the observer.
C
                     CALL ZZCOREPC ( ABCORR, ET, RLT, DEPOCH )
                     CALL CHCKXC ( .FALSE., ' ', OK )

C
C                    Look up the transformation from frame DREF to
C                    J2000. We don't need this right away, but we'll
C                    have occasion to use it later.
C
                     CALL PXFORM ( DREF, 'J2000', DEPOCH, DJ2M )
                     CALL CHCKXC ( .FALSE., ' ', OK )

C
C                    Loop over all method choices.
C
                     DO MIX = 1, NMETH

                        METHOD = METHDS( MIX )

C
C --- Case: ------------------------------------------------------
C

                        TITLE = 'Observer = #; Target = #; '
     .                  //      'Geometry = #; ABCORR = #; ' 
     .                  //      'DREF = #; METHOD = #.'
                        CALL REPMC ( TITLE, '#', OBSRVR, TITLE )
                        CALL REPMC ( TITLE, '#', TARGET, TITLE )
                        CALL REPMC ( TITLE, '#', GEOM,   TITLE )
                        CALL REPMC ( TITLE, '#', ABCORR, TITLE )
                        CALL REPMC ( TITLE, '#', DREF,   TITLE )
                        CALL REPMC ( TITLE, '#', METHOD, TITLE )

                        CALL TCASE ( TITLE )

                        IF ( GEOM .EQ. 'POINT_AT_CENTER' ) THEN
C
C                          Look up direction vector using current frame
C                          and aberration correction.  The direction
C                          vector is going to point to the target's
C                          center, so we should hit the target.
C
                           CALL SPKPOS ( TARGET, ET,   DREF, ABCORR, 
     .                                   OBSRVR, DVEC, DLT          )
                           CALL CHCKXC ( .FALSE., ' ', OK )


                        ELSE IF ( GEOM .EQ. 'MISS_BACKWARD' ) THEN
C
C                          Set the pointing direction to the inverse of
C                          that obtained in the 'POINT_AT_CENTER' case.
C
                           CALL SPKPOS ( TARGET, ET,     DREF, ABCORR, 
     .                                   OBSRVR, NEGVEC, DLT          )
                           CALL CHCKXC ( .FALSE., ' ', OK )

                           CALL VMINUS ( NEGVEC, DVEC )


                        ELSE IF (     ( GEOM .EQ. 'LIMB_INSIDE_NEAR' ) 
     .                           .OR. ( GEOM .EQ. 'MISS_LIMB_NEAR'   ) )
     .                  THEN
C
C                          Find the limb of the target based on the
C                          aberration-corrected target center position.
C                          Select ray to hit limb plane along major
C                          axis, slightly inside or slightly outside
C                          the ellipse, depending on the geometry case.
C
C                          Note we're looking up the target state in
C                          the target's body-fixed frame, not in the
C                          DREF frame.
C
                           CALL SPKPOS ( TARGET, ET,     TRGFRM, ABCORR,
     .                                   OBSRVR, NEGVEC, TLT           )
                           CALL CHCKXC ( .FALSE., ' ', OK )

                           CALL VMINUS ( NEGVEC, OBSVEC )

C
C                          Get the limb's center and semi-axis vectors.
C
                           CALL EDLIMB ( RADII(1), RADII(2), RADII(3),
     .                                   OBSVEC,   LIMB               )

                           CALL EL2CGV ( LIMB, LCENTR, SMAJOR, SMINOR )

C
C                          Improve the limb if we're using aberration
C                          corrections.
C
C                          We'll treat one endpoint of the limb's major
C                          axis as an ephemeris object, and we'll
C                          compute the aberration-corrected position of
C                          this object.
C
C                          To get an accurate limb, we'll find the
C                          light time from observer to tip of
C                          semi-major axis and get an improved light
C                          time estimate.

                           IF ( USELT ) THEN

                              CALL VADD ( LCENTR, SMAJOR, TIPFX )
C
C                             Find the aberration-corrected position of
C                             the tip of the major axis. Compute the
C                             offset from the original position.
C                       
                              CALL SPKCPT ( TIPFX,  TARGET, TRGFRM, 
     .                                      ET,     TRGFRM, 'TARGET',
     .                                      ABCORR, OBSRVR, AXISTA, 
     .                                      AXLT                     )
                              CALL CHCKXC ( .FALSE., ' ', OK )

                              CALL VSUB ( AXISTA, TIPFX, LMBOFF )

C
C                             Shift the limb center by the offset we 
C                             just computed.
C
                              CALL VADD ( LCENTR, LMBOFF, TMPVEC )
                              CALL VEQU ( TMPVEC,         LCENTR )

C
C                             We've compute our improved limb estimate.
C                      
                           END IF

C
C                          Pick our target point near the limb. The
C                          point is 1+/- DELTA of the semi-major axis
C                          length out from the center, along one of the
C                          semi- major axes.
C
C                          A challenge that occurs here, since our
C                          target is a DSK-based shape, is that the
C                          actual limb is a polygon inscribed in the
C                          ellipsoid limb. Depending on the orientation
C                          of the polygon, the semi-major axis of the
C                          ellipsoid limb may extend outside of the DSK
C                          shape, and our target point may define a ray
C                          that will miss the ellipsoid even for cases
C                          where an intersection should occur.
C
C                          We'll mitigate this problem for the
C                          geometric case by modifying the target
C                          point. We'll try to find a surface point on
C                          the DSK shape "below" the target point, and
C                          if we find it, we'll use it instead of the
C                          original target.
C                    
                           IF ( USECN ) THEN

                              DELTA = 1.D-5

                           ELSE IF ( USELT ) THEN

                              DELTA = 1.D-3

                           ELSE 
C
C                             Value used for ellipsoidal targets is
C
C                                DELTA = 1.D-9
C
C                             We can still use this value if we're
C                             using the high-resolution DSK for Mars.
C                             Otherwise, we need a point deeper inside
C                             the ellipsoid, which corresponds to a
C                             larger value of DELTA.
C                        
                              IF (  MATCHI(METHOD, '*HIGH*', '*', '?') )
     .                        THEN

                                 IF ( TARGET .EQ. 'MARS' ) THEN

                                    DELTA = 1.D-9
                                 ELSE
                                    DELTA = 1.D-3
                                 END IF

                              ELSE                            
                                 DELTA = 1.D-2
                              END IF

                           END IF

                           IF ( GEOM(1:4) .EQ. 'MISS' ) THEN
                              FRAC = 1.D0 + DELTA
                           ELSE
                              FRAC = 1.D0 - DELTA
                           END IF

                           CALL VLCOM( 1.D0, LCENTR, FRAC, SMAJOR, TGT )

                           IF ( .NOT. USELT ) THEN
C
C                             Find the surface point "under" TGT. This
C                             can be done simply only for the geometric
C                             case, because the limb has not been
C                             translated to account for aberration
C                             corrections.
C
                              CALL VMINUS ( TGT, RAYDIR )

                              CALL DSKXV ( .FALSE., TARGET, 0,  SRFLST, 
     .                                     ET,      TRGFRM, 1,  TGT, 
     .                                     RAYDIR,  XPT,    FND        )
                              CALL CHCKXC ( .FALSE., ' ', OK )

                              IF ( FND ) THEN
C
C                                If an intercept was found, use it
C                                instead of TGT. If an intercept was
C                                not found, TGT is already inside the
C                                tessellated ellipsoid.
C
                                 CALL VEQU ( XPT, TGT )

                              END IF

                           END IF

C
C                          Our ray extends from the observer through
C                          the target point.
C
                           CALL VSUB ( TGT, OBSVEC, DVECFX )

                           SEP = VSEP ( NEGVEC, DVECFX )

C
C                          Convert the ray from the target body fixed
C                          frame to J2000, then from J2000 to the DREF
C                          frame. We need the target frame epoch TE to
C                          find the first transformation matrix TIPM.
C
                           CALL ZZCOREPC ( ABCORR, ET, TLT, TE )

                           CALL PXFORM ( 'J2000', TRGFRM, TE, TIPM )

                           CALL MTXV ( TIPM, DVECFX, DVECJ2 )

C
C                          The matrix DJ2M maps from DREF to J2000, so
C                          apply the transpose of this matrix to obtain
C                          DVEC.
C                    
                           CALL MTXV ( DJ2M, DVECJ2, DVEC )

                        ELSE
C
C                          Oops!  Name mismatch.
C
                           CALL SIGERR ( 'SPICE(BUG)' )
                           CALL CHCKXC ( .FALSE., ' ', OK )
                     
                        END IF

C
C                       Find the surface intercept point.
C
                        CALL SINCPT ( METHOD, TARGET,  ET,     TRGFRM,
     .                                ABCORR, OBSRVR,  DREF,   DVEC,
     .                                SPOINT, TRGEPC,  SRFVEC, FOUND )

                        CALL CHCKXC ( .FALSE., ' ', OK )

C
C                       Check the results.
C
                        IF ( .NOT. FOUND ) THEN

                           IF ( GEOM(1:4) .EQ. 'MISS' ) THEN
C 
C                             FOUND should be .FALSE.; the other outputs
C                             are undefined.
C
                              CALL CHCKSL( 'FOUND', FOUND, .FALSE., OK )

                           ELSE
C
C                             We're supposed to have an intercept.
C                             Force an error signal.
C
                              CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

                           END IF

                        ELSE
C
C                          FOUND is true.  
C
C                          Compute the observer position relative to
C                          the target center, consistent with the
C                          aberration corrections applicable to the
C                          apparent intercept point. Also compute the
C                          distance of between the observer and the
C                          apparent intercept point.

                           CALL VSUB ( SPOINT, SRFVEC, OBSPOS )

C
C                          Let DIST be the length of SRFVEC.
C
                           DIST = VNORM ( SRFVEC )

C
C                          The target epoch had better be consistent
C                          with DIST and ABCORR.
C            
                           XLT  = DIST/CLIGHT()

                           CALL ZZCOREPC ( ABCORR, ET, XLT, XEPOCH )
               
C
C                          This is a relative error check.
C
                           IF ( USECN ) THEN
                              ETOL = TIGHT
                           ELSE
                              ETOL = MEDIUM
                           END IF

                           CALL CHCKSD ( 'TRGEPC', TRGEPC, '~/', 
     .                                   XEPOCH,   ETOL,    OK  )

C
C                          Get the transformation from the target frame
C                          to J2000.

                           CALL PXFORM ( TRGFRM, 'J2000', 
     .                                   TRGEPC, TRGJ2M )
                           CALL CHCKXC ( .FALSE., ' ', OK )

C
C                          Now transform DVEC to the J2000 frame.
C
                           CALL MXV ( DJ2M, DVEC, DJ2 )

C
C                          The following check applies only to the case
C                          in which the pointing direction is toward
C                          the target's center.
C
                           IF ( GEOM .EQ. 'POINT_AT_CENTER' ) THEN
C
C                             The angular separation of SRFVEC and DVEC
C                             should be pretty small when these vectors
C                             are compared in compatible reference
C                             frames. We don't expect these vectors to
C                             be identical (even theoretically) because
C                             they've been computed with different
C                             target epochs.
C
C                             First step: get SRFVEC in the J2000
C                             frame.
C
                              CALL MXV ( TRGJ2M, SRFVEC, SRFVJ2 )

C
C                             Find the angular separation and make sure
C                             it's not too large.
C
                              SEP = VSEP ( SRFVJ2, DJ2 )

                              CALL CHCKSD( 'DJ2 vs NEG2 SEP', SEP,  
     .                                     '~', 0.D0, SLOPPY, OK )
                           END IF

C
C                          End of sanity check test for the
C                          POINT_AT_CENTER case.
C
C                          Having made it this far, we're ready for
C                          some more rigorous tests. In particular,
C                          we're going treat SPOINT as an ephemeris
C                          object and find its aberration-corrected
C                          position relative to the observer in J2000
C                          coordinates. This computation will allow us
C                          to derive expected values of TRGEPC, OBSPOS,
C                          the transformation from the J2000 frame to
C                          the target body-fixed frame at TRGEPC. We
C                          will verify that the aberration-corrected
C                          location of SPOINT, lies on the ray DVEC:
C                          this is the criterion we used to define
C                          SPOINT.
C
C                          These tests are primarily of interest when
C                          aberration corrections are used, but they
C                          still serve as a consistency check for the
C                          geometric case.
C
C                          We're expecting to get good agreement
C                          between all of these items and their
C                          counterparts obtained from SINCPT,
C                          especially when we use converged Newtonian
C                          corrections.

                           IF (  EQSTR(OBSRVR, 'EARTH') ) THEN

                              IF ( USECN ) THEN
                                 TOL = TIGHT
                              ELSE IF ( USELT ) THEN
                                 TOL = MEDIUM
                              ELSE
                                 TOL = TIGHT
                              END IF

                           ELSE
C
C                             Use looser tolerances for the Mars
C                             orbiter. For the orbiter, small errors in
C                             SPOINT lead to larger relative errors in
C                             DIST and SEP.

                              IF ( USECN ) THEN
                                 TOL = MTIGHT
                              ELSE IF ( USELT ) THEN
                                 TOL = LOOSE
                              ELSE
                                 TOL = MTIGHT
                              END IF

                              IF ( GEOM .EQ. 'LIMB_INSIDE_NEAR' ) THEN
C
C                                Loosen the tolerance so the
C                                MAC/OSX/Native C platform can handle
C                                these tests.
C
                                 TOL = TOL * 1.D2

                              END IF

                           END IF

C
C                          Find the aberration-corrected location of
C                          SPOINT.
C
C                          Prior to N0065, we had to do this manually.
C                          Now we can call a SPICE routine to do the
C                          job.
C
                           CALL SPKCPT ( SPOINT,  TARGET,   TRGFRM, ET, 
     .                                   'J2000', 'TARGET', ABCORR, 
     .                                   OBSRVR,  RAYSTA,   SPNTLT    )
                           CALL CHCKXC ( .FALSE., ' ', OK )

C
C                          XRAY is our expected result.
C 
                           CALL VEQU ( RAYSTA, XRAY )

C
C                          Compute the expected target epoch.
C
                           CALL ZZCOREPC ( ABCORR, ET, SPNTLT, XTE )
                           CALL CHCKXC ( .FALSE., ' ', OK )

C
C                          Moment of truth: XRAY is the J2000 vector
C                          from the observer to the
C                          aberration-corrected position of our
C                          "ephemeris object" located on the target
C                          surface at location SPOINT. If SPOINT were
C                          correct in the first place, then XRAY should
C                          be lined up with our boresight direction
C                          DVEC, when DVEC is rotated to the J2000
C                          frame.
C
C                          Actually, we computed DVEC in the J2000
C                          frame long ago: this vector is called DJ2.
C
                           CALL CHCKSD ( 'TRGEPC vs XTE', 
     .                                   TRGEPC, '~/', XTE, TOL, OK )

                           SEP = VSEP ( DJ2, XRAY )
                       
                           CALL CHCKSD ( 'XRAY vs DJ2 sep', 
     .                                   SEP, '~', 0.D0, TOL, OK )

C
C                          Check DIST against its predicted value.
C
                           CALL CHCKSD ( 'DIST',      DIST, '~/', 
     .                                   VNORM(XRAY), TOL,   OK   )

C
C                          Create a predicted value for SRFVEC: convert
C                          XRAY to the target frame using XFORM.
C
                           CALL PXFORM ( 'J2000', TRGFRM, XTE, XFORM )
                           CALL CHCKXC ( .FALSE., ' ', OK )

                           CALL MXV   ( XFORM, XRAY, XSRFVC )
                     
                           CALL CHCKAD ( 'SRFVEC', SRFVEC, '~~/', 
     .                                   XSRFVC,   3,      TOL,  OK )
               
C
C                          The following test only makes sense when
C                          stellar aberration is not used. (When
C                          stellar aberration IS used, SPOINT does not
C                          lie on the ray; the image of SPOINT under
C                          the stellar aberration correction lies on
C                          the ray.
C                       
                           IF ( .NOT. USESTL ) THEN
C
C                             Create a predicted value for SPOINT:
C                             convert the (optionally light-time
C                             corrected) target-observer vector to the
C                             target body-fixed frame at epoch XTE and
C                             add to SRFVEC to form XSPNT.
C             
                              CALL SPKSSB( OBSCDE, ET,  'J2000', SSBOBS)
                              CALL SPKSSB( TRGCDE, XTE, 'J2000', SSBTRG)
                              CALL CHCKXC( .FALSE., ' ', OK )

                              CALL VSUB ( SSBOBS, SSBTRG, J2OBS  )
                              CALL MXV  ( XFORM,  J2OBS,  XOBSPS )
                              CALL VADD ( XOBSPS, SRFVEC, XSPNT )
C
C                             Use absolute tolerances for this check:
C                             1cm for converged light time; 1km for
C                             non-converged.
C
                              IF ( USECN ) THEN
                                 TOL = 1.D-5
                              ELSE
                                 TOL = 1.D0
                              END IF

                              CALL CHCKAD ( 'SPOINT', SPOINT, '~~', 
     .                                      XSPNT,   3,      TOL,  OK )

                           END IF

                        END IF
C
C                       We're finished with the consistency checks for 
C                       the intercept cases.
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
C           End of the geometry case loop.
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
C     Set up the ray first.
C      
      CALL SPKPOS ( '499', ET,   DREF, ABCORR, 
     .              '399', DVEC, DLT          )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( '(0) FOUND', FOUND, .TRUE., OK )

C
C     Set target and target-fixed frame.
C
      TARGET = 'MARS'
      FIXREF = 'IAU_MARS'

      CALL SINCPT ( 'Ellipsoid', 'MARS',  ET,      FIXREF, 
     .              ABCORR,      'Earth', DREF,    DVEC,   
     .              XSPNT,       XEPOCH,  XSRFVC,  FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      CALL SINCPT ( 'Ellipsoid', '499',    ET,      FIXREF, 
     .              ABCORR,      '399',    DREF,    DVEC,   
     .              SPOINT,      TRGEPC,   SRFVEC,  FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      CALL CHCKAD ( 'SPOINT', SPOINT, '=', XSPNT,  3, 0.D0, OK )
      CALL CHCKAD ( 'SRFVEC', SRFVEC, '=', XSRFVC, 3, 0.D0, OK )
      CALL CHCKSD ( 'TRGEPC', TRGEPC, '=', XEPOCH,    0.D0, OK )
      CALL CHCKSD ( 'DIST',   DIST,   '=', DIST,      0.D0, OK )


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
C        - Ray direction frame definition
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
      DREF   = 'J2000'
C
C     Set up the ray first.
C      
      CALL SPKPOS ( '499', ET,   DREF, ABCORR, 
     .              '399', DVEC, DLT          )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SINCPT ( 'Ellipsoid', TARGET, ET,      FIXREF, 
     .              ABCORR,      OBSRVR, DREF,    DVEC,   
     .              XSPNT,       XEPOCH, XSRFVC,  FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( '(0) FOUND', FOUND, .TRUE., OK )


      CALL BODDEF ( 'JUPITER', 499 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SINCPT ( 'Ellipsoid', 'JUPITER', ET,     FIXREF, 
     .              ABCORR,      OBSRVR,    DREF,   DVEC,   
     .              SPOINT,      TRGEPC,    SRFVEC, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( '(1) FOUND', FOUND, .TRUE., OK )

C
C     We expect an exact match here.
C
      CALL CHCKAD ( 'SPOINT', SPOINT, '=', XSPNT, 3, 0.D0, OK )


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

      CALL SINCPT ( 'Ellipsoid', TARGET, ET,     FIXREF, 
     .              ABCORR,      'SUN',  DREF,   DVEC,   
     .              SPOINT,      TRGEPC, SRFVEC, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( '(0) FOUND', FOUND, .TRUE., OK )


C
C     We expect an exact match here.
C
      CALL CHCKAD ( 'SPOINT', SPOINT, '=', XSPNT, 3, 0.D0, OK )

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
      DREF   = 'J2000'

      CALL SPKPOS ( '499', ET,   DREF, ABCORR, 
     .              '399', DVEC, DLT          )
      CALL CHCKXC ( .FALSE., ' ', OK )


      METHOD = 'dsk/unprioritized/surfaces = 1'

      CALL SINCPT ( METHOD, TARGET, ET,      FIXREF, 
     .              ABCORR, OBSRVR, DREF,    DVEC,   
     .              XSPNT,  XEPOCH, XSRFVC,  FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( '(0) FOUND', FOUND, .TRUE., OK )


      SRFNMS(1) = 'AAAbbb'
 
      CALL PCPOOL ( 'NAIF_SURFACE_NAME', NMAP, SRFNMS )
      CALL CHCKXC ( .FALSE., ' ', OK )



      METHOD = 'dsk/unprioritized/surfaces = AAAbbb'

      CALL SINCPT ( METHOD, TARGET, ET,     FIXREF, 
     .              ABCORR, OBSRVR, DREF,   DVEC,   
     .              SPOINT, TRGEPC, SRFVEC, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( '(1) FOUND', FOUND, .TRUE., OK )


C
C     We expect an exact match here.
C
      CALL CHCKAD ( 'SPOINT', SPOINT, '=', XSPNT, 3, 0.D0, OK )


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
      DREF   = 'J2000'

      CALL SPKPOS ( '499', ET,   DREF, ABCORR, 
     .              '399', DVEC, DLT          )
      CALL CHCKXC ( .FALSE., ' ', OK )


      METHOD = 'dsk/unprioritized/surfaces = low-res'

      CALL SINCPT ( METHOD, TARGET, ET,      FIXREF, 
     .              ABCORR, OBSRVR, DREF,    DVEC,   
     .              XSPNT,  XEPOCH, XSRFVC,  FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( '(0) FOUND', FOUND, .TRUE., OK )

C
C     Unload the high-res DSK; set METHOD to remove
C     surface specification.
C
      CALL UNLOAD ( DSK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      METHOD = 'dsk/unprioritized'

      CALL SINCPT ( METHOD, TARGET, ET,     FIXREF, 
     .              ABCORR, OBSRVR, DREF,   DVEC,   
     .              SPOINT, TRGEPC, SRFVEC, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( '(1) FOUND', FOUND, .TRUE., OK )

C
C     We expect an exact match here.
C
      CALL CHCKAD ( 'SPOINT', SPOINT, '=', XSPNT, 3, 0.D0, OK )


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

      METHOD = 'dsk/unprioritized'

      CALL SINCPT ( METHOD, TARGET, ET,     FIXREF, 
     .              ABCORR, OBSRVR, DREF,   DVEC,   
     .              SPOINT, TRGEPC, SRFVEC, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( '(1) FOUND', FOUND, .TRUE., OK )

C
C     Make sure the result matches that obtained with the 
C     high-res DSK specified.
C
      METHOD = 'dsk/unprioritized/ SURFACES = "HIGH-RES" '

      CALL SINCPT ( METHOD, TARGET, ET,     FIXREF, 
     .              ABCORR, OBSRVR, DREF,   DVEC,   
     .              XSPNT,  XEPOCH, XSRFVC, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( '(0) FOUND', FOUND, .TRUE., OK )

C
C     We expect an exact match here.
C
      CALL CHCKAD ( 'SPOINT', SPOINT, '=', XSPNT, 3, 0.D0, OK )



C***********************************************************************
C
C     Error handling tests follow.
C
C***********************************************************************
C
C --- Case: ------------------------------------------------------
C
       CALL TCASE ( 'Target name not translated' )
C
C     Find the surface intercept point.
C            
      CALL SINCPT ( 'Ellipsoid', 'xyz',   ET,      FIXREF, 
     .              ABCORR,      OBSRVR,  DREF,    DVEC,  
     .              SPOINT,      TRGEPC,  SRFVEC,  FOUND   )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )



C
C --- Case: ------------------------------------------------------
C
 
      CALL TCASE ( 'Observer name not translated' )
C
C     Find the surface intercept point.
C            
      CALL SINCPT ( 'Ellipsoid', TARGET,  ET,      FIXREF,
     .              ABCORR,      'xyz',   DREF,    DVEC,     
     .              SPOINT,      TRGEPC,  SRFVEC,  FOUND )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )
      


C
C --- Case: ------------------------------------------------------
C
 
      CALL TCASE ( 'Observer coincides with target' )
C
C     Find the surface intercept point.
C            
      CALL SINCPT ( 'Ellipsoid', TARGET,  ET,      FIXREF,
     .              ABCORR,      TARGET,  DREF,    DVEC,    
     .              SPOINT,      TRGEPC,  SRFVEC,  FOUND )

      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)', OK )



C
C --- Case: ------------------------------------------------------
C
 
      CALL TCASE ( 'Unsupported computation method' )
C
C     Find the surface intercept point.
C            
      CALL SINCPT ( 'xyz',       TARGET,  ET,      FIXREF,
     .              ABCORR,      OBSRVR,  DREF,    DVEC,    
     .              SPOINT,      TRGEPC,  SRFVEC,  FOUND )

      CALL CHCKXC ( .TRUE., 'SPICE(BADMETHODSYNTAX)', OK )



C
C --- Case: ------------------------------------------------------
C
 
      CALL TCASE ( 'Body-fixed frame is not centered on target. ' )
C
C     Find the surface intercept point.  Use reference frame 'BAD'
C     for direction vector.
C            
      CALL SINCPT ( 'Ellipsoid', 'Mars_orbiter',  ET,      FIXREF,
     .              ABCORR,      'EARTH',         'BAD',   DVEC,    
     .              SPOINT,      TRGEPC,          SRFVEC,  FOUND  )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)', OK )


C
C --- Case: ------------------------------------------------------
C
 
      CALL TCASE ( 'Bad aberration correction. ' )
C
C     Find the surface intercept point. Use unrecognized aberration
C     correction.
C            
      CALL SINCPT ( 'Ellipsoid', 'Mars_orbiter',  ET,          FIXREF,
     .              'ZZZ',       'EARTH',         'IAU_EARTH', DVEC,    
     .              SPOINT,      TRGEPC,          SRFVEC,  FOUND  )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )


C
C --- Case: ------------------------------------------------------
C
 
      CALL TCASE ( 'Bad aberration correction: relativistic ' )
C
C     Find the surface intercept point. Use relativistic aberration
C     correction.
C            
      CALL SINCPT ( 'Ellipsoid', 'Mars',  ET,    FIXREF,
     .              'RL',       'EARTH',         'IAU_EARTH', DVEC,    
     .              SPOINT,      TRGEPC,          SRFVEC,  FOUND  )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )


      CALL TCASE ( 'Bad aberration correction: stellar '
     .//           'aberration only'                    )
C
C     Find the surface intercept point. Use stellar aberration
C     correction w/o light time correction.
C            
      CALL SINCPT ( 'Ellipsoid', 'MARS',  ET,          FIXREF,
     .              'XS',       'EARTH',  'IAU_EARTH', DVEC,    
     .              SPOINT,      TRGEPC,   SRFVEC,     FOUND  )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )



C
C --- Case: ------------------------------------------------------
C
 
      CALL TCASE ( 'Zero direction vector' )

      CALL CLEARD ( 3, DVEC )
      CALL SINCPT ( 'Ellipsoid', TARGET,  ET,      FIXREF,
     .              ABCORR,      OBSRVR,  DREF,    DVEC,    
     .              SPOINT,      TRGEPC,  SRFVEC,  FOUND )

      CALL CHCKXC ( .TRUE., 'SPICE(ZEROVECTOR)', OK )


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

C
C     Get reference result using low-res Mars DSK.
C
      ET     = 8 * JYEAR()

      TARGET = 'MARS'
      FIXREF = 'IAU_MARS'
      OBSRVR = 'EARTH'
      ABCORR = 'NONE'
      DREF   = 'J2000'

      CALL SPKPOS ( '499', ET,   DREF, ABCORR, 
     .              '399', DVEC, DLT          )
      CALL CHCKXC ( .FALSE., ' ', OK )


      METHOD = 'dsk/unprioritized/surfaces = low-res'

      CALL SINCPT ( METHOD, TARGET, ET,      FIXREF, 
     .              ABCORR, OBSRVR, DREF,    DVEC,   
     .              XSPNT,  XEPOCH, XSRFVC,  FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(NOLOADEDDSKFILES)', OK )




C
C --- Case: ------------------------------------------------------
C
 
C
C     Clean up.
C
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


      CALL T_SUCCESS ( OK )

      RETURN
      END
