
C$Procedure      F_SRFXPT ( SRFXPT family tests )

      SUBROUTINE F_SRFXPT ( OK )
      IMPLICIT NONE

      LOGICAL               OK

C$ Abstract
C
C     This routine tests the SPICELIB routine
C     
C        SRFXPT
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
C$ Version
C
C-    TSPICE Version 1.0.0, 23-FEB-2004 (NJB)
C
C-&
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      CLIGHT
      DOUBLE PRECISION      JYEAR
      DOUBLE PRECISION      RPD
      DOUBLE PRECISION      VDIST
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      VSEP

      LOGICAL               EQSTR

C     
C     Local parameters
C
      CHARACTER*(*)         PCK 
      PARAMETER           ( PCK   = 'test_0008.tpc' )

      CHARACTER*(*)         SPK1
      PARAMETER           ( SPK1  = 'srfxpt_spk.bsp' )

      CHARACTER*(*)         SPK2
      PARAMETER           ( SPK2  = 'orbiter.bsp' )


      DOUBLE PRECISION      VTIGHT 
      PARAMETER           ( VTIGHT  = 1.D-14 )

      DOUBLE PRECISION      TIGHT 
      PARAMETER           ( TIGHT   = 1.D-12 )

      DOUBLE PRECISION      MTIGHT 
      PARAMETER           ( MTIGHT  = 1.D-10 )

      DOUBLE PRECISION      MEDIUM
      PARAMETER           ( MEDIUM = 1.D-8 )

      DOUBLE PRECISION      LOOSE
      PARAMETER           ( LOOSE  = 1.D-5 )

      DOUBLE PRECISION      SLOPPY
      PARAMETER           ( SLOPPY = 1.D-3 )


      INTEGER               UBEL
      PARAMETER           ( UBEL    = 9 )

      INTEGER               FILSIZ
      PARAMETER           ( FILSIZ = 255 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 32 )

      INTEGER               NREF
      PARAMETER           ( NREF = 3 )

      INTEGER               NABC
      PARAMETER           ( NABC = 9 )

      INTEGER               ABCLEN
      PARAMETER           ( ABCLEN = 10 )

      INTEGER               TIMLEN
      PARAMETER           ( TIMLEN = 50 )

      INTEGER               NOBS
      PARAMETER           ( NOBS   = 2 )

      INTEGER               NGEOM
      PARAMETER           ( NGEOM  = 4 )

      INTEGER               SCID
      PARAMETER           ( SCID   = -499 )

C
C     Local variables
C
      CHARACTER*(ABCLEN)    ABCS    ( NABC )
      CHARACTER*(ABCLEN)    ABCORR
      CHARACTER*(NAMLEN)    DREF 
      CHARACTER*(LNSIZE)    GEOM
      CHARACTER*(LNSIZE)    GEOMS   ( NGEOM )
      CHARACTER*(NAMLEN)    OBSRVR
      CHARACTER*(NAMLEN)    OBSNMS  ( NOBS )
      CHARACTER*(NAMLEN)    REFS    ( NREF )
      CHARACTER*(NAMLEN)    TARGET
      CHARACTER*(NAMLEN)    TRGFRM
      CHARACTER*(LNSIZE)    TITLE
      CHARACTER*(TIMLEN)    UTC

      DOUBLE PRECISION      DEPOCH
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      DIST
      DOUBLE PRECISION      DJ2     ( 3 )
      DOUBLE PRECISION      DJ2M    ( 3, 3 )
      DOUBLE PRECISION      DLT
      DOUBLE PRECISION      DVEC    ( 3 )
      DOUBLE PRECISION      DVECFX  ( 3 )
      DOUBLE PRECISION      DVECJ2  ( 3 )
      DOUBLE PRECISION      DVECST  ( 3 )
      DOUBLE PRECISION      ELTS    ( 8 )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ETOL
      DOUBLE PRECISION      FRAC
      DOUBLE PRECISION      J2OBS   ( 3 )
      DOUBLE PRECISION      LCENTR  ( 3 )
      DOUBLE PRECISION      LIMB    ( UBEL )
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      NEGJ2   ( 3 )
      DOUBLE PRECISION      NEGVEC  ( 3 )
      DOUBLE PRECISION      OBSJ2   ( 3 )
      DOUBLE PRECISION      OBSPOS  ( 3 )
      DOUBLE PRECISION      OBSVEC  ( 3 )
      DOUBLE PRECISION      RADII   ( 3 )
      DOUBLE PRECISION      REFPOS  ( 3 )
      DOUBLE PRECISION      RLT
      DOUBLE PRECISION      SEP
      DOUBLE PRECISION      SMAJOR  ( 3 )
      DOUBLE PRECISION      SMINOR  ( 3 )
      DOUBLE PRECISION      SPJ2    ( 3 )
      DOUBLE PRECISION      SPLT    ( 3 )
      DOUBLE PRECISION      SPOINT  ( 3 )
      DOUBLE PRECISION      SPPOS   ( 3 )
      DOUBLE PRECISION      SPVEC   ( 3 )
      DOUBLE PRECISION      SSBOBS  ( 6 )
      DOUBLE PRECISION      SSBTRG  ( 6 )
      DOUBLE PRECISION      STATE0  ( 6 )
      DOUBLE PRECISION      TE
      DOUBLE PRECISION      TGT     ( 3 )
      DOUBLE PRECISION      TIPFX   ( 3 )
      DOUBLE PRECISION      TIPJ2   ( 3 )
      DOUBLE PRECISION      TIPM    ( 3, 3 )
      DOUBLE PRECISION      TLT
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      TRGEPC
      DOUBLE PRECISION      TRGJ2M  ( 3, 3 )
      DOUBLE PRECISION      XDIST
      DOUBLE PRECISION      XEPOCH
      DOUBLE PRECISION      XFORM   ( 3, 3 )
      DOUBLE PRECISION      XLT
      DOUBLE PRECISION      XOBSPS  ( 3 )
      DOUBLE PRECISION      XRAY    ( 3 )
      DOUBLE PRECISION      XRTRG   ( 3 )
      DOUBLE PRECISION      XSPNT   ( 3 )
      DOUBLE PRECISION      XTE

      INTEGER               ABCIDX
      INTEGER               CLS
      INTEGER               CLSSID
      INTEGER               DREFID
      INTEGER               FRCODE
      INTEGER               GEOIDX
      INTEGER               HANDLE ( 2 )
      INTEGER               I
      INTEGER               N
      INTEGER               NITR
      INTEGER               OBSCDE
      INTEGER               OBSIDX
      INTEGER               REFCTR
      INTEGER               REFIDX
      INTEGER               TRGCDE

      LOGICAL               FND
      LOGICAL               FOUND
      LOGICAL               USECN
      LOGICAL               USELT
      LOGICAL               USESTL
      LOGICAL               XMIT

C
C     Saved variables
C
      SAVE                  ABCS
      SAVE                  REFS

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

      DATA                  REFS   /  'J2000',
     .                                'IAU_MARS',
     .                                'IAU_EARTH'  /

      DATA                  GEOMS  /  'POINT_AT_CENTER',
     .                                'MISS_BACKWARD',
     .                                'LIMB_INSIDE_NEAR',
     .                                'MISS_LIMB_NEAR'    /

      DATA                  OBSNMS /  'Earth',
     .                                'MARS_ORBITER'    /
 
C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_SRFXPT' )

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
C     Start out with consistency checks:  having found SPOINT,
C     find the aberration corrected location of SPOINT by 
C     direct computation, and compare results to those from
C     SRFXPT.
C
C     Test cases for a distant viewer:  ray emanates from earth's
C     center, points towards Mars' center.


C
C     Set target.  Get target code, target body-fixed frame
C     name.
C
      TARGET = 'Mars'

      CALL BODN2C ( TARGET, TRGCDE, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CNMFRM ( TARGET, FRCODE, TRGFRM, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

C
C     Get target radii.
C
      CALL BODVAR ( TRGCDE, 'RADII', N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

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
C        Loop over every viewing geometry case.
C
         DO GEOIDX = 1, NGEOM

            GEOM = GEOMS ( GEOIDX )

C
C           Loop over every aberration correction choice.
C
            DO ABCIDX = 1, NABC

               ABCORR = ABCS ( ABCIDX )

C
C              Set up some logical variables describing the
C              attributes of the selected correction.
C
               USELT  = ABCORR .NE. 'None'
               XMIT   = ABCORR(1:1) .EQ. 'X'

               USECN  =       ( ABCORR(1:2) .EQ. 'Cn'  ) 
     .                   .OR. ( ABCORR(1:3) .EQ. 'Xcn' )

               USESTL = INDEX ( ABCORR, '+s' )  .NE.  0

C
C              Loop over every direction vector frame choice.
C
               DO REFIDX = 1, NREF

                  DREF = REFS ( REFIDX )
C
C                 Set light time RLT from observer to center of frame
C                 for the direction vector.
C                 
                  CALL NAMFRM ( DREF,   DREFID )
                  CALL FRINFO ( DREFID, REFCTR, CLS, CLSSID, FND ) 

                  CALL SPKEZP ( REFCTR, ET, 'J2000', ABCORR, OBSCDE,
     .                          REFPOS, RLT                         )

C
C                 We'll need the epoch DEPOCH associated
C                 with the center of DREF.  RLT is the 
C                 light time from DREF's center to the observer.
C
                  IF ( USELT ) THEN

                     IF ( XMIT ) THEN               
                        DEPOCH = ET + RLT
                     ELSE
                        DEPOCH = ET - RLT
                     END IF

                  ELSE
                     DEPOCH = ET
                  END IF

C
C                 Look up the transformation from frame DREF to J2000.
C                 We don't need this right away, but we'll have 
C                 occasion to use it later.
C
                  CALL PXFORM ( DREF, 'J2000', DEPOCH, DJ2M )


                  TITLE = 'Observer = #.  Geometry = #. ' //
     .                    'ABCORR = #; DREF = #.'
                  CALL REPMC ( TITLE, '#', OBSRVR, TITLE )
                  CALL REPMC ( TITLE, '#', GEOM,   TITLE )
                  CALL REPMC ( TITLE, '#', ABCORR, TITLE )
                  CALL REPMC ( TITLE, '#', DREF,   TITLE )

                  CALL TCASE ( TITLE )

                  IF ( GEOM .EQ. 'POINT_AT_CENTER' ) THEN
C
C                    Look up direction vector using current frame and
C                    aberration correction.  The direction vector is
C                    going to point to the target's center, so we 
C                    should hit the target.
C
                     CALL SPKPOS ( TARGET, ET,   DREF, ABCORR, 
     .                             OBSRVR, DVEC, DLT          )
                     CALL CHCKXC ( .FALSE., ' ', OK )


                  ELSE IF ( GEOM .EQ. 'MISS_BACKWARD' ) THEN
C
C                    Set the pointing direction to the inverse
C                    of that obtained in the 'POINT_AT_CENTER'
C                    case.            
C
                     CALL SPKPOS ( TARGET, ET,     DREF, ABCORR, 
     .                             OBSRVR, NEGVEC, DLT          )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     CALL VMINUS ( NEGVEC, DVEC )


                  ELSE IF (     ( GEOM .EQ. 'LIMB_INSIDE_NEAR' ) 
     .                     .OR. ( GEOM .EQ. 'MISS_LIMB_NEAR'   ) ) THEN
C
C                    Find the limb of the target based on
C                    the aberration-corrected target center position.
C                    Select ray to hit limb along major axis, slightly
C                    inside or slightly outside the ellipse, depending
C                    on the geometry case.
C
C                    Note we're looking up the target state in
C                    the target's body-fixed frame, not in the
C                    DREF frame.
C
                     CALL SPKPOS ( TARGET, ET,     TRGFRM, ABCORR, 
     .                             OBSRVR, NEGVEC, TLT            )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     CALL VMINUS ( NEGVEC, OBSVEC )

C
C                    Get the limb's center and semi-axis vectors.
C
                     CALL EDLIMB ( RADII(1), RADII(2), RADII(3),
     .                             OBSVEC,   LIMB               )

                     CALL EL2CGV ( LIMB, LCENTR, SMAJOR, SMINOR )
C
C                    To get an accurate limb, we'll find the light
C                    time from observer to tip of semi-major axis
C                    and get an improved light time estimate.
C
                     CALL VADD ( LCENTR, SMAJOR, TIPFX )

                     IF ( USELT ) THEN

                        IF ( XMIT )  THEN 
                           TE  =  ET + TLT
                        ELSE
                           TE  =  ET - TLT
                        END IF

                     ELSE
                        TE = ET
                     END IF

C
C                    Get the "tip" in the J2000 frame at epoch TE.
C
                     CALL PXFORM ( 'J2000', TRGFRM, TE, TIPM )

                     CALL MTXV ( TIPM, TIPFX, TIPJ2 )

C
C                    Get state of observer relative to SSB at
C                    ET and state of target relative to SSB at 
C                    TE.
C                     
                     CALL SPKSSB ( OBSCDE, ET, 'J2000', SSBOBS )
                     CALL SPKSSB ( TRGCDE, TE, 'J2000', SSBTRG )

C
C                    Get the position of the tip.  Compute a
C                    new light time value and target epoch.
C
                     CALL VADD ( SSBTRG, TIPJ2,  TGT    )

                     TLT = VDIST ( SSBOBS, TGT ) / CLIGHT()

                     IF ( USELT ) THEN

                        IF ( XMIT )  THEN 
                           TE  =  ET + TLT
                        ELSE
                           TE  =  ET - TLT
                        END IF

                     ELSE
                        TE = ET
                     END IF

C
C                    Re-compute TIPM.
C
                     CALL PXFORM ( 'J2000', TRGFRM, TE, TIPM )

C
C                    Get the observer position in the target body-fixed
C                    frame at TE.
C
                     CALL VSUB ( SSBOBS, SSBTRG, OBSJ2  )
                     CALL MXV  ( TIPM,   OBSJ2,  OBSVEC )
                     
C
C                    Get the limb's center and semi-axis vectors.
C
                     CALL EDLIMB ( RADII(1), RADII(2), RADII(3),
     .                             OBSVEC,   LIMB               )

                     CALL EL2CGV ( LIMB, LCENTR, SMAJOR, SMINOR )
                     
C
C                    Pick our target point near the limb.  The
C                    point is 1+/- DELTA of the semi-major axis length
C                    out from the center, along one of the semi-
C                    major axes.
C
                     DELTA = 0.001D0

                     IF ( GEOM(1:4) .EQ. 'MISS' ) THEN
                        FRAC = 1.D0 + DELTA
                     ELSE
                        FRAC = 1.D0 - DELTA
                     END IF

                     CALL VLCOM( 1.D0, LCENTR, FRAC, SMAJOR, TGT )

C
C                    Our ray extends from the observer to the target
C                    point.
C
                     CALL VSUB ( TGT, OBSVEC, DVECFX )

                     SEP = VSEP ( NEGVEC, DVECFX )

C
C                    Convert the ray from the target body fixed 
C                    frame to J2000, then from J2000 to the DREF
C                    frame.  We need the target frame epoch TE
C                    to find the first transformation matrix TIPM.
C
                     IF ( USELT ) THEN

                        IF ( XMIT )  THEN 
                           TE  =  ET + TLT
                        ELSE
                           TE  =  ET - TLT
                        END IF

                     ELSE
                        TE = ET
                     END IF

                     CALL PXFORM ( 'J2000', TRGFRM, TE, TIPM )

                     CALL MTXV ( TIPM, DVECFX, DVECJ2 )

C
C                    If we're using stellar aberration correction,
C                    apply the correction to our ray direction.
C
                     IF ( USESTL ) THEN

                        IF ( XMIT ) THEN
                           CALL STLABX ( DVECJ2, SSBOBS(4), DVECST )
                        ELSE
                           CALL STELAB ( DVECJ2, SSBOBS(4), DVECST )
                        END IF

                        CALL VEQU ( DVECST, DVECJ2 )

                     END IF
C
C                    The matrix DJ2M maps from DREF to J2000, so
C                    apply the transpose of this matrix to obtain
C                    DVEC.
C                    
                     CALL MTXV ( DJ2M, DVECJ2, DVEC )

                  ELSE
C
C                    Oops!  Name mismatch.
C
                     CALL SIGERR ( 'SPICE(BUG)' )
                     CALL CHCKXC ( .FALSE., ' ', OK )
                     
                  END IF

C
C                 Find the surface intercept point.
C            
                  CALL SRFXPT ( 'Ellipsoid', TARGET,  ET,      ABCORR,
     .                          OBSRVR,      DREF,    DVEC,    SPOINT,
     .                          DIST,        TRGEPC,  OBSPOS,  FOUND  )

                  CALL CHCKXC ( .FALSE., ' ', OK )


C
C                 Check the results.
C
                  IF ( .NOT. FOUND ) THEN

                     IF ( GEOM(1:4) .EQ. 'MISS' ) THEN
C
C                       FOUND should be .FALSE.; the other outputs
C                       are undefined.
C
                        CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

                     ELSE
C
C                       We're supposed to have an intercept.
C
                        CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

                     END IF

                  ELSE
C
C                    FOUND is true.  
C
C                    Form the vector from the observer to the target
C                    intercept.
C
                     CALL VSUB ( SPOINT, OBSPOS, SPVEC )

C
C                    The length of SPVEC had better be DIST.
C
                     CALL CHCKSD ( 'DIST',       DIST,   '~', 
     .                             VNORM(SPVEC), VTIGHT, OK  )

C
C                    The target epoch had better be consistent with
C                    DIST and ABCORR.
C            
                     XLT  = DIST/CLIGHT()

                     IF ( USELT ) THEN

                        IF ( XMIT ) THEN               
                           XEPOCH = ET + XLT
                        ELSE
                           XEPOCH = ET - XLT
                        END IF

                     ELSE

                        XEPOCH = ET

                     END IF
               
C
C                    This is a relative error check.
C
                     IF ( USECN ) THEN
                        ETOL = TIGHT
                     ELSE
                        ETOL = MEDIUM
                     END IF

                     CALL CHCKSD ( 'TRGEPC', TRGEPC, '~/', 
     .                              XEPOCH,  ETOL,    OK  )

C
C                    Get the transformation from the target frame
C                    to J2000.
C
                     CALL PXFORM ( TRGFRM, 'J2000', TRGEPC, TRGJ2M )

C
C                    Now transform DVEC to the J2000 frame.
C
                     CALL MXV ( DJ2M, DVEC, DJ2 )

C
C                    The following check applies only to the case in
C                    which the pointing direction is toward the
C                    target's center.
C
                     IF ( GEOM .EQ. 'POINT_AT_CENTER' ) THEN
C
C                       The angular separation of -OBSPOS and DVEC
C                       should be pretty small when these vectors are
C                       compared in compatible reference frames.  We
C                       don't expect these vectors to be identical
C                       (even theoretically) because they've been
C                       computed with different target epochs.
C                       Furthermore, if stellar aberration correction
C                       is used, DVEC will be corrected for stellar
C                       aberration but OBSPOS will not, leading to even
C                       greater disparity.
C
C                       First step:  get -OBSPOS in the J2000 frame.
C

                        CALL VMINUS ( OBSPOS, NEGVEC )

                        CALL MXV    ( TRGJ2M, NEGVEC, NEGJ2 )

C
C                       Find the angular separation and make sure it's
C                       not too large.
C
                        SEP = VSEP ( NEGJ2, DJ2 )

                        CALL CHCKSD( 'DJ2 vs NEG2 SEP', SEP,   '~', 
     .                                0.D0,             SLOPPY, OK )

                     END IF
C
C                    End of sanity check test for the POINT_AT_CENTER
C                    case.
C
C                    Having made it this far, we're ready for some more
C                    rigorous tests.  In particular, we're going treat
C                    SPOINT as an ephemeris object and find its
C                    aberration-corrected position relative to the
C                    observer in J2000 coordinates. This computation
C                    will allow us to derive expected values of TRGEPC,
C                    OBSPOS, the transformation from the J2000 frame to
C                    the target body-fixed frame at TRGEPC.  We will
C                    verify that the aberration-corrected location
C                    of SPOINT, lies on the ray DVEC:  this is the
C                    the criterion we used to define SPOINT.
C
C                    These tests make sense only when aberration
C                    corrections are used.
C
                     IF ( USELT ) THEN
C
C                       We're expecting to get good agreement between
C                       all of these items and their counterparts
C                       obtained from SRFXPT, especially when use use
C                       converged Newtonian corrections.
C
                        IF (  EQSTR(OBSRVR, 'EARTH') ) THEN

                           IF ( USECN ) THEN
                              TOL = TIGHT
                           ELSE
                              TOL = MEDIUM
                           END IF

                        ELSE
C
C                          Use looser tolerances for the Mars
C                          orbiter.  For the orbiter, small errors
C                          in SPOINT lead to larger relative errors
C                          in DIST and SEP.
C
                           IF ( USECN ) THEN
                              TOL = MTIGHT
                           ELSE
                              TOL = LOOSE
                           END IF

                        END IF

C
C                       Find the aberration-corrected location of
C                       SPOINT.
C
C                       We need the J2000 state of the observer relative
C                       to the solar system barycenter at ET.
C
                        CALL SPKSSB ( OBSCDE, ET, 'J2000', SSBOBS )
                        CALL CHCKXC ( .FALSE., ' ', OK )

C
C                       The SPOINT re-computation is done iteratively.
C                       Since we're starting with a geometric target
C                       position, the light time value obtained from
C                       the normal light time correction corresponds to
C                       the light time found on the *second* iteration.
C                       We increment our iteration counts by 1 for both
C                       the normal and CN cases.
C
                        IF ( USECN ) THEN
                           NITR = 5
                        ELSE
                           NITR = 2
                        END IF

C
C                       The initial target position relative to the
C                       solar system barycenter is found by summing the
C                       target center position relative to the solar
C                       system barycenter at ET with SPOINT, after
C                       SPOINT has been converted to the J2000 frame at
C                       ET.
C
                        CALL SPKSSB ( TRGCDE, ET, 'J2000', SSBTRG )
                        CALL CHCKXC ( .FALSE., ' ', OK )

                        CALL PXFORM (  'J2000', TRGFRM, ET,  TIPM )
                        CALL CHCKXC ( .FALSE., ' ', OK )

                        CALL MTXV   ( TIPM,   SPOINT,  SPJ2  )
                        CALL VADD   ( SSBTRG, SPJ2,    SPPOS )

                        DO I = 1, NITR
C
C                          Make a new estimate of the target epoch XTE.
C
                           LT = VDIST ( SSBOBS, SPPOS ) / CLIGHT()

                           IF ( XMIT ) THEN
                              XTE  =  ET + LT
                           ELSE
                              XTE  =  ET - LT
                           END IF

C
C                          Compute the J2000 state of SPOINT relative
C                          to the solar system barycenter at XTE.
C
                           CALL SPKSSB ( TRGCDE, XTE, 'J2000', SSBTRG )
                           CALL CHCKXC ( .FALSE., ' ', OK )

                           CALL PXFORM ( 'J2000', TRGFRM, XTE,  TIPM )
                           CALL CHCKXC ( .FALSE., ' ', OK )
     
                           CALL CHCKXC ( .FALSE., ' ', OK )   

                           CALL MTXV   ( TIPM,   SPOINT,  SPJ2  )
                           CALL VADD   ( SSBTRG, SPJ2,    SPPOS )

                        END DO
C
C                       Compute the light-time corrected position of
C                       SPOINT as seen by the observer.
C
                        CALL VSUB ( SPPOS, SSBOBS, SPLT )
C
C                       Correct SPLT for stellar aberration, if ABCORR 
C                       so indicates.
C
                        IF ( USESTL ) THEN

                           IF ( XMIT ) THEN
                              CALL STLABX ( SPLT, SSBOBS(4), XRAY )
                           ELSE
                              CALL STELAB ( SPLT, SSBOBS(4), XRAY )
                           END IF
                           
                        ELSE
C
C                          XRAY is our expected result.
C 
                           CALL VEQU ( SPLT, XRAY )

                        END IF

C
C                       Moment of truth:  XRAY is the J2000 vector from
C                       the observer to the aberration-corrected
C                       position of our "ephemeris object" located on
C                       the target surface at location SPOINT.  If
C                       SPOINT were correct in the first place, then
C                       XRAY should be lined up with our boresight
C                       direction DVEC, when DVEC is rotated to the
C                       J2000 frame.
C
C                       Actually, we computed DVEC in the J2000 frame
C                       long ago:  this vector is called DJ2.
C
                        CALL CHCKSD ( 'TRGEPC vs XTE', 
     .                                 TRGEPC, '~/', XTE, TOL, OK )

                        SEP = VSEP ( DJ2, XRAY )
                       
                        CALL CHCKSD ( 'XRAY vs DJ2 sep', 
     .                                 SEP, '~', 0.D0, TOL, OK )

C
C                       Check DIST against its predicted value.
C
                        CALL CHCKSD ( 'DIST',      DIST, '~/', 
     .                                VNORM(XRAY), TOL,   OK   )

C
C                       Create a predicted value for OBSPOS:  the 
C                       difference of SSBOBS and SSBTRG from our last
C                       loop iteration gives us OBSPOS in the J2000
C                       frame.  Use XTE to get the transformation to
C                       target body-fixed coordinates at epoch XTE.
C
                        CALL VSUB   ( SSBOBS,  SSBTRG, J2OBS )
                        CALL PXFORM ( 'J2000', TRGFRM, XTE,   XFORM )
                        CALL MXV    ( XFORM,   J2OBS,  XOBSPS )

                        CALL CHCKAD ( 'OBSPOS', OBSPOS, '~/', 
     .                                XOBSPS,   3,      TOL,  OK )
               
C
C                       The following test only makes sense
C                       when stellar aberration is not used.
C                       (When stellar aberration IS used, SPOINT
C                       does not lie on the ray; the image of
C                       SPOINT under the stellar aberration correction
C                       lies on the ray.
C                       
                        IF ( .NOT. USESTL ) THEN
C
C                          Create a predicted value for SPOINT:  convert
C                          XRAY to target body-fixed coordinates at
C                          epoch XTE and add to XOBSPS to form XSPNT.
C             
                           CALL MXV  ( XFORM,  XRAY,   XRTRG )
                           CALL VADD ( XRTRG,  XOBSPS, XSPNT )

                           CALL CHCKAD ( 'SPOINT', SPOINT, '~/', 
     .                                 XSPNT,   3,      MEDIUM,  OK )

                        END IF

                     END IF
C
C                    We're finished with the consistency checks for
C                    the aberration correction cases.
C
                  END IF
C
C                 End of the checks for the intercept cases.
C
               END DO
C
C              End of the aberration correction loop.
C
            END DO
C
C           End of the reference frame loop.
C
         END DO
C
C        End of the geometry case loop.
C
      END DO
C
C     End of the observer loop.
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


      CALL SRFXPT ( 'Ellipsoid', 'MARS',   ET,      ABCORR,
     .              'Earth',      DREF,    DVEC,    XSPNT,
     .               XDIST,       XEPOCH,  XOBSPS,  FOUND  )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      CALL SRFXPT ( 'Ellipsoid', '499',    ET,      ABCORR,
     .              '399',        DREF,    DVEC,    SPOINT,
     .               DIST,        TRGEPC,  OBSPOS,  FOUND  )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      CALL CHCKAD ( 'SPOINT', SPOINT, '=', XSPNT,  3, 0.D0, OK )
      CALL CHCKAD ( 'OBSPOS', OBSPOS, '=', XOBSPS, 3, 0.D0, OK )
      CALL CHCKSD ( 'TRGEPC', TRGEPC, '=', XEPOCH,    0.D0, OK )
      CALL CHCKSD ( 'DIST',   DIST,   '=', DIST,      0.D0, OK )


C
C     Error handling tests follow.
C
      CALL TCASE ( 'Target name not translated' )
C
C     Find the surface intercept point.
C            
      CALL SRFXPT ( 'Ellipsoid', 'xyz',   ET,      ABCORR,
     .              OBSRVR,      DREF,    DVEC,    SPOINT,
     .              DIST,        TRGEPC,  OBSPOS,  FOUND  )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )



      CALL TCASE ( 'Observer name not translated' )
C
C     Find the surface intercept point.
C            
      CALL SRFXPT ( 'Ellipsoid', TARGET,  ET,      ABCORR,
     .              'xyz',       DREF,    DVEC,    SPOINT,
     .              DIST,        TRGEPC,  OBSPOS,  FOUND  )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )
      


      CALL TCASE ( 'Observer coincides with target' )
C
C     Find the surface intercept point.
C            
      CALL SRFXPT ( 'Ellipsoid', TARGET,  ET,      ABCORR,
     .              TARGET,      DREF,    DVEC,    SPOINT,
     .              DIST,        TRGEPC,  OBSPOS,  FOUND  )

      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)', OK )



      CALL TCASE ( 'Unsupported computation method' )
C
C     Find the surface intercept point.
C            
      CALL SRFXPT ( 'xyz',       TARGET,  ET,      ABCORR,
     .              OBSRVR,      DREF,    DVEC,    SPOINT,
     .              DIST,        TRGEPC,  OBSPOS,  FOUND  )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDMETHOD)', OK )



      CALL TCASE ( 'No body-fixed frame associated with target' )
C
C     Find the surface intercept point.
C            
      CALL SRFXPT ( 'Ellipsoid', 'Mars_orbiter',  ET,      ABCORR,
     .              'EARTH',     DREF,            DVEC,    SPOINT,
     .              DIST,        TRGEPC,          OBSPOS,  FOUND  )

      CALL CHCKXC ( .TRUE., 'SPICE(NOFRAME)', OK )




      CALL TCASE ( 'Frame name maps to code but FRINFO can''t ' //
     .             'find frame info.'                           )
C
C     Find the surface intercept point.  Use reference frame 'BAD'
C     for direction vector.
C            
      CALL SRFXPT ( 'Ellipsoid', 'Mars_orbiter',  ET,      ABCORR,
     .              'EARTH',     'BAD',           DVEC,    SPOINT,
     .              DIST,        TRGEPC,          OBSPOS,  FOUND  )

      CALL CHCKXC ( .TRUE., 'SPICE(NOFRAME)', OK )


C
C     Clean up.
C
      CALL SPKUEF ( HANDLE(1) )
      CALL DELFIL ( SPK1      )
  
      CALL SPKUEF ( HANDLE(2) )
      CALL DELFIL ( SPK2      )

      CALL T_SUCCESS ( OK )

      END
