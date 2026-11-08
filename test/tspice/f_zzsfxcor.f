C$Procedure      F_ZZSFXCOR ( ZZSFXCOR family tests )

      SUBROUTINE F_ZZSFXCOR ( OK )

C$ Abstract
C
C     Exercise the private SPICELIB geometry routine ZZSFXCOR.
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
C     This routine tests the SPICELIB private
C     geometry routine ZZSFXCOR.
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
C-    TSPICE Version 1.0.0, 06-AUG-2016 (NJB)
C
C       Based on f_sincpt.for version 2.0.0, 02-APR-2012 
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
      LOGICAL               EXISTS

C
C     External routines
C
      EXTERNAL              ZZMAXRAD
      EXTERNAL              ZZRAYNP
      EXTERNAL              ZZRAYSFX

C     
C     Local parameters
C
      CHARACTER*(*)         PCK 
      PARAMETER           ( PCK   = 'test_0008.tpc' )

      CHARACTER*(*)         SPK1
      PARAMETER           ( SPK1  = 'srfxpt_spk.bsp' )

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
      PARAMETER           ( LNSIZE = 80 )

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
      CHARACTER*(NAMLEN)    FIXREF
      CHARACTER*(LNSIZE)    GEOM
      CHARACTER*(LNSIZE)    GEOMS   ( NGEOM )
      CHARACTER*(NAMLEN)    OBSRVR
      CHARACTER*(NAMLEN)    OBSNMS  ( NOBS )
      CHARACTER*(NAMLEN)    REFS    ( NREF )
      CHARACTER*(NAMLEN)    TARGET
      CHARACTER*(NAMLEN)    TRGFRM
      CHARACTER*(LNSIZE)    TITLE
      CHARACTER*(TIMLEN)    UTC

      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      DEPOCH
      DOUBLE PRECISION      DIST
      DOUBLE PRECISION      DJ2     ( 3 )
      DOUBLE PRECISION      DJ2M    ( 3, 3 )
      DOUBLE PRECISION      DLT
      DOUBLE PRECISION      DVEC    ( 3 )
      DOUBLE PRECISION      DVECFX  ( 3 )
      DOUBLE PRECISION      DVECJ2  ( 3 )
      DOUBLE PRECISION      ELTS    ( 8 )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ETOL
      DOUBLE PRECISION      FIXCOR  ( 3 )
      DOUBLE PRECISION      FRAC
      DOUBLE PRECISION      J2OBS   ( 3 )
      DOUBLE PRECISION      LCCOR   ( 3 )
      DOUBLE PRECISION      LCENTR  ( 3 )
      DOUBLE PRECISION      LIMB    ( UBEL )
      DOUBLE PRECISION      LT
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
      DOUBLE PRECISION      SRFVEC  ( 3 )
      DOUBLE PRECISION      SRFVJ2  ( 3 )
      DOUBLE PRECISION      SSBOBS  ( 6 )
      DOUBLE PRECISION      SSBTRG  ( 6 )
      DOUBLE PRECISION      STATE0  ( 6 )
      DOUBLE PRECISION      STLCOR  ( 3 )
      DOUBLE PRECISION      TE
      DOUBLE PRECISION      TGT     ( 3 )
      DOUBLE PRECISION      TIPCOR  ( 3 )
      DOUBLE PRECISION      TIPFX   ( 3 )
      DOUBLE PRECISION      TIPJ2   ( 3 )
      DOUBLE PRECISION      TIPM    ( 3, 3 )
      DOUBLE PRECISION      TIPVEC  ( 3 )
      DOUBLE PRECISION      TLT
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      TRGEPC
      DOUBLE PRECISION      TRGJ2M  ( 3, 3 )
      DOUBLE PRECISION      XEPOCH
      DOUBLE PRECISION      XFORM   ( 3, 3 )
      DOUBLE PRECISION      XLT
      DOUBLE PRECISION      XOBSPS  ( 3 )
      DOUBLE PRECISION      XRAY    ( 3 )
      DOUBLE PRECISION      XSPNT   ( 3 )
      DOUBLE PRECISION      XSRFVC  ( 3 )
      DOUBLE PRECISION      XTE

      INTEGER               ABCIDX
      INTEGER               DCENTR
      INTEGER               DCLASS
      INTEGER               DCLSID
      INTEGER               DREFID
      INTEGER               FRCODE
      INTEGER               GEOMIX
      INTEGER               HANDLE ( 2 )
      INTEGER               I
      INTEGER               N
      INTEGER               NITR
      INTEGER               OBSCDE
      INTEGER               OBSIDX
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
     .                                'ECLIPJ2000',
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
      CALL TOPEN ( 'F_ZZSFXCOR' )

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Setup:  create SPK, PCK file.' )

      CALL TSTSPK ( SPK1, .TRUE., HANDLE(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Create the PCK file, load it, and delete it.
C      
      IF ( EXISTS(PCK) ) THEN
         CALL DELFIL ( PCK )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

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
C     Start out with consistency checks:  having found SPOINT,
C     find the aberration corrected location of SPOINT by 
C     direct computation, and compare results to those from
C     ZZSFXCOR.
C
C     Test cases for a distant viewer:  ray emanates from earth's
C     center, points towards Mars' center.


C
C     Set target.  Get target code, target body-fixed frame
C     name.
C
      TARGET = 'Mars'
      FIXREF = 'IAU_MARS'

      CALL BODN2C ( TARGET, TRGCDE, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CNMFRM ( TARGET, FRCODE, TRGFRM, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

C
C     Initialize the surface intercept utilities. For 
C     ellipsoidal target models, just the target body ID
C     is needed.
C
      CALL ZZSUELIN ( TRGCDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Get target radii.
C
      CALL BODVAR ( TRGCDE, 'RADII', N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Top of main loop.' )

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
         DO GEOMIX = 1, NGEOM

            GEOM = GEOMS ( GEOMIX )

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
                  CALL FRINFO ( DREFID, DCENTR, DCLASS, DCLSID, FND ) 

                  CALL SPKEZP ( DCENTR, ET, 'J2000', ABCORR, OBSCDE,
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

C
C --- Case: ------------------------------------------------------
C 

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
C                    Find the limb of the target based on the
C                    aberration-corrected target center position.
C                    Select ray to hit limb plane along major axis,
C                    slightly inside or slightly outside the ellipse,
C                    depending on the geometry case.
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
C                    Improve the limb if we're using aberration
C                    corrections.
C
C                    To get an accurate limb, we'll find the light
C                    time from observer to tip of semi-major axis
C                    and get an improved light time estimate.
C
                     IF ( USELT ) THEN

                        IF ( USECN ) THEN
                           NITR = 4
                        ELSE
                           NITR = 1
                        END IF

                        IF ( XMIT )  THEN 
                           TE  =  ET + TLT
                        ELSE
                           TE  =  ET - TLT
                        END IF

C
C                       Get state of observer relative to SSB at ET.
C                     
                        CALL SPKSSB ( OBSCDE, ET, 'J2000', SSBOBS )

                        DO I = 1, NITR
C
C                          Get the "tip" in the J2000 frame at epoch TE.
C
                           CALL VADD ( LCENTR, SMAJOR, TIPFX )

                           CALL PXFORM ( 'J2000', TRGFRM, TE, TIPM )

                           CALL MTXV ( TIPM, TIPFX, TIPJ2 )

C
C                          Get state of target relative to SSB at TE.
C                     
                           CALL SPKSSB ( TRGCDE, TE, 'J2000', SSBTRG )

C
C                          Get the position of the tip.  
C
                           CALL VADD ( SSBTRG, TIPJ2, TGT )

C
C                          Get the position of the tip.  Compute a
C                          new light time value and target epoch.
C

                           TLT = VDIST ( SSBOBS, TGT ) / CLIGHT()

                           IF ( XMIT )  THEN 
                              TE  =  ET + TLT
                           ELSE
                              TE  =  ET - TLT
                           END IF
C
C                          Re-compute TIPM.
C
                           CALL PXFORM ( 'J2000', TRGFRM, TE, TIPM )

C
C                          Get the observer position in the target
C                          body-fixed frame at TE.

                           CALL VSUB ( SSBOBS, SSBTRG, OBSJ2  )
                           CALL MXV  ( TIPM,   OBSJ2,  OBSVEC )
                     
C
C                          Get the limb's center and semi-axis vectors.
C
                           CALL EDLIMB ( RADII(1), RADII(2), RADII(3),
     .                                   OBSVEC,   LIMB               )

                           CALL EL2CGV ( LIMB, LCENTR, SMAJOR, SMINOR )
                       
                        END DO
C
C                       Use stellar aberration at the axis tip to
C                       shift the center, if necessary.  
C
                        IF ( USESTL ) THEN

                           CALL VADD ( LCENTR, SMAJOR, TIPFX )

                           CALL MTXV ( TIPM, TIPFX, TIPJ2 )

                           CALL VSUB ( TIPJ2, OBSJ2, TIPVEC )

                           IF ( XMIT ) THEN
                              CALL STLABX ( TIPVEC, SSBOBS(4), TIPCOR )
                           ELSE
                              CALL STELAB ( TIPVEC, SSBOBS(4), TIPCOR )
                           END IF
C
C                          Let STLCOR be the J2000 stellar aberration
C                          correction offset; let FIXCOR be the offset
C                          in the target frame.
C
                           CALL VSUB ( TIPCOR, TIPVEC, STLCOR )
                           CALL MXV  ( TIPM,   STLCOR, FIXCOR )

C
C                          Shift the limb center.
C
                           CALL VADD ( FIXCOR, LCENTR, LCCOR  )
                           CALL VEQU ( LCCOR,          LCENTR )

                        END IF
C
C                       We've compute our improved limb estimate.
C                     
                     END IF

C
C                    Pick our target point near the limb.  The
C                    point is 1+/- DELTA of the semi-major axis length
C                    out from the center, along one of the semi-
C                    major axes.
C
                     IF ( USECN ) THEN

                        DELTA = 1.D-5

                     ELSE IF ( USELT ) THEN

                        DELTA = 1.D-3

                     ELSE 
                        DELTA = 1.D-9
                     END IF

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
                  CALL ZZSFXCOR ( ZZRAYNP, ZZMAXRAD, ZZRAYSFX, TRGCDE,
     .                            ET,      ABCORR,   USELT,    USECN,
     .                            USESTL,  XMIT,     FIXREF,   OBSCDE,
     .                            DREFID,  DCLASS,   DCENTR,   DVEC,
     .                            SPOINT,  TRGEPC,   SRFVEC,   FOUND  )

                  CALL CHCKXC ( .FALSE., ' ', OK )
         
C
C                 Check the results.
C
                  IF ( GEOM(1:4) .EQ. 'MISS' ) THEN

                     CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

                  END IF

                  IF ( .NOT. FOUND ) THEN

                     IF ( GEOM(1:4) .EQ. 'MISS' ) THEN
C
C                       FOUND should be .FALSE.; the other outputs
C                       are undefined.
C
                        CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

                     ELSE
C
C                       We're supposed to have an intercept. Force
C                       an error signal.
C
                        CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

                     END IF

                  ELSE
C
C                    FOUND is true.  
C
C                    Compute the observer position relative to the
C                    target center, consistent with the aberration
C                    corrections applicable to the apparent intercept
C                    point. Also compute the distance of between the
C                    observer and the apparent intercept point.
C
                     CALL VSUB ( SPOINT, SRFVEC, OBSPOS )

C
C                    The let DIST be the length of SRFVEC.
C
                     DIST = VNORM ( SRFVEC )

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
                     CALL CHCKXC ( .FALSE., ' ', OK )

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
C                       The angular separation of SRFVEC and DVEC
C                       should be pretty small when these vectors are
C                       compared in compatible reference frames.  We
C                       don't expect these vectors to be identical
C                       (even theoretically) because they've been
C                       computed with different target epochs.
C
C                       First step:  get SRFVEC in the J2000 frame.
C
                        CALL MXV ( TRGJ2M, SRFVEC, SRFVJ2 )

C
C                       Find the angular separation and make sure it's
C                       not too large.
C
                        SEP = VSEP ( SRFVJ2, DJ2 )

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
C                    These tests are primarily of interest when
C                    aberration corrections are used, but they still
C                    serve as a consistency check for the geometric
C                    case.
C
C                    We're expecting to get good agreement between all
C                    of these items and their counterparts obtained
C                    from ZZSFXCOR, especially when we use converged
C                    Newtonian corrections.
C
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
C                       Use looser tolerances for the Mars orbiter. For
C                       the orbiter, small errors in SPOINT lead to
C                       larger relative errors in DIST and SEP.
C
                        IF ( USECN ) THEN
                           TOL = MTIGHT
                        ELSE IF ( USELT ) THEN
                           TOL = LOOSE
                        ELSE
                           TOL = MTIGHT
                        END IF

                        IF ( GEOM .EQ. 'LIMB_INSIDE_NEAR' ) THEN
C
C                          Loosen the tolerance so the MAC/OSX/Native C
C                          platform can handle these tests.
C
                           TOL = TOL * 1.D2

                        END IF

                     END IF

C
C                    Find the aberration-corrected location of SPOINT.
C
C                    We need the J2000 state of the observer relative
C                    to the solar system barycenter at ET.
C
                     CALL SPKSSB ( OBSCDE, ET, 'J2000', SSBOBS )
                     CALL CHCKXC ( .FALSE., ' ', OK )

C
C                    The SPOINT re-computation is done iteratively.
C                    Since we're starting with a geometric target
C                    position, the light time value obtained from the
C                    normal light time correction corresponds to the
C                    light time found on the *second* iteration. We
C                    increment our iteration counts by 1 for both the
C                    normal and CN cases.
C
                     IF ( USECN ) THEN

                        NITR = 5

                     ELSE IF ( USELT ) THEN

                        NITR = 2

                     ELSE 

                        NITR = 0
C
C                       The expected target epoch is computed inside a
C                       loop below; the loop won't execute for the
C                       geometric case, so set the epoch here.
C
                        XTE  = ET

                     END IF

C
C                    The initial target position relative to the solar
C                    system barycenter is found by summing the target
C                    center position relative to the solar system
C                    barycenter at ET with SPOINT, after SPOINT has
C                    been converted to the J2000 frame at ET.
C
                     CALL SPKSSB ( TRGCDE, ET, 'J2000', SSBTRG )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     CALL PXFORM (  'J2000', TRGFRM, ET,  TIPM )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     CALL MTXV   ( TIPM,   SPOINT,  SPJ2  )
                     CALL VADD   ( SSBTRG, SPJ2,    SPPOS )

                     DO I = 1, NITR
C
C                        Make a new estimate of the target epoch XTE.
C
                        LT = VDIST ( SSBOBS, SPPOS ) / CLIGHT()

                        IF ( XMIT ) THEN
                           XTE  =  ET + LT
                        ELSE
                           XTE  =  ET - LT
                        END IF

C
C                       Compute the J2000 state of SPOINT relative
C                       to the solar system barycenter at XTE.
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
C                    Compute the light-time corrected position of
C                    SPOINT as seen by the observer.
C
                     CALL VSUB ( SPPOS, SSBOBS, SPLT )
C
C                    Correct SPLT for stellar aberration, if ABCORR so
C                    indicates.
C
                     IF ( USESTL ) THEN

                        IF ( XMIT ) THEN
                           CALL STLABX ( SPLT, SSBOBS(4), XRAY )
                        ELSE
                           CALL STELAB ( SPLT, SSBOBS(4), XRAY )
                        END IF

                     ELSE
C
C                       XRAY is our expected result.
C 
                        CALL VEQU ( SPLT, XRAY )

                     END IF

C
C                    Moment of truth: XRAY is the J2000 vector from the
C                    observer to the aberration-corrected position of
C                    our "ephemeris object" located on the target
C                    surface at location SPOINT. If SPOINT were
C                    correct in the first place, then XRAY should be
C                    lined up with our boresight direction DVEC, when
C                    DVEC is rotated to the J2000 frame.
C
C                    Actually, we computed DVEC in the J2000 frame long
C                    ago:  this vector is called DJ2.
C
                     CALL CHCKSD ( 'TRGEPC vs XTE', 
     .                              TRGEPC, '~/', XTE, TOL, OK )

                     SEP = VSEP ( DJ2, XRAY )
                       
                     CALL CHCKSD ( 'XRAY vs DJ2 sep', 
     .                             SEP, '~', 0.D0, TOL, OK )

C
C                    Check DIST against its predicted value.
C
                     CALL CHCKSD ( 'DIST',      DIST, '~/', 
     .                             VNORM(XRAY), TOL,   OK   )

C
C                    Create a predicted value for SRFVEC: convert XRAY
C                    to the target frame using XFORM.
C
                     CALL PXFORM ( 'J2000', TRGFRM, XTE, XFORM )

                     CALL MXV   ( XFORM, XRAY, XSRFVC )
                     
                     CALL CHCKAD ( 'SRFVEC', SRFVEC, '~~/', 
     .                             XSRFVC,   3,      TOL,  OK )

               
C
C                    The following test only makes sense when stellar
C                    aberration is not used. (When stellar aberration
C                    IS used, SPOINT does not lie on the ray; the image
C                    of SPOINT under the stellar aberration correction
C                    lies on the ray.
C                       
                     IF ( .NOT. USESTL ) THEN
C
C                       Create a predicted value for SPOINT: convert
C                       the (optionally light-time corrected)
C                       target-observer vector to the target body-fixed
C                       frame at epoch XTE and add to SRFVEC to form
C                       XSPNT.
C             
                        CALL VSUB ( SSBOBS, SSBTRG, J2OBS  )
                        CALL MXV  ( XFORM,  J2OBS,  XOBSPS )
                        CALL VADD ( XOBSPS, SRFVEC, XSPNT )

C
C                       Use absolute tolerances for this check: 1cm for
C                       converged light time; 1km for non-converged.
C
                        IF ( USECN ) THEN
                           TOL = 1.D-5
                        ELSE
                           TOL = 1.D0
                        END IF

                        CALL CHCKAD ( 'SPOINT', SPOINT, '~~', 
     .                                 XSPNT,   3,      TOL,  OK )

                     END IF

                  END IF
C
C                 We're finished with the consistency checks for 
C                 the intercept cases.
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

      
C***********************************************************************
C
C     Error cases
C
C***********************************************************************
 

C
C     ZZSFXCOR does very little in-line error checking; it expects its
C     caller to handle this. To quote from the routine's header:
C
C     3)  This routine assumes robust input checking has been
C         performed by the caller. This routine may fail in
C         an unspecified manner if such checking has not 
C         been performed.
C

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Observer coincides with target' )

      OBSCDE = 399
      TRGCDE = 499
      FIXREF = 'IAU_MARS'
      ABCORR = 'XCN+S'
      XMIT   = .TRUE.
      USELT  = .TRUE.
      USECN  = .TRUE.
      USESTL = .TRUE.

      DREF   = FIXREF

      CALL NAMFRM ( DREF,   DREFID )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL FRINFO ( DREFID, DCENTR, DCLASS, DCLSID, FND ) 
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Initialize the surface intercept utilities. For 
C     ellipsoidal target models, just the target body ID
C     is needed.
C
      CALL ZZSUELIN ( TRGCDE )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL ZZSFXCOR ( ZZRAYNP, ZZMAXRAD, ZZRAYSFX, OBSCDE,
     .                ET,      ABCORR,   USELT,    USECN,
     .                USESTL,  XMIT,     FIXREF,   OBSCDE,
     .                DREFID,  DCLASS,   DCENTR,   DVEC,
     .                SPOINT,  TRGEPC,   SRFVEC,   FOUND  )

      CALL CHCKXC ( .TRUE., 'SPICE(NOSEPARATION)', OK )


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Bad aberration correction. ' )

C
C     To force this check to occur, we need to cause the 
C     aberration correction string to be passed to SPKEZP.
C     This occurs when the direction vector frame is non-inertial
C     and is not centered on the observer.
C
      CALL NAMFRM ( FIXREF, DREFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZSFXCOR ( ZZRAYNP, ZZMAXRAD, ZZRAYSFX, TRGCDE,
     .                ET,      'XYZ',    USELT,    USECN,
     .                USESTL,  XMIT,     FIXREF,   OBSCDE,
     .                DREFID,  DCLASS,   DCENTR,   DVEC,
     .                SPOINT,  TRGEPC,   SRFVEC,   FOUND  )


      CALL CHCKXC ( .TRUE., 'SPICE(SPKINVALIDOPTION)', OK )


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Bad aberration correction: relativistic ' )
C
C     Find the surface intercept point. Use relativistic aberration
C     correction.
C            
      CALL ZZSFXCOR ( ZZRAYNP, ZZMAXRAD, ZZRAYSFX, TRGCDE,
     .                ET,      'RLT',    USELT,    USECN,
     .                USESTL,  XMIT,     FIXREF,   OBSCDE,
     .                DREFID,  DCLASS,   DCENTR,   DVEC,
     .                SPOINT,  TRGEPC,   SRFVEC,   FOUND  )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINVALIDOPTION)', OK )


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Bad aberration correction: stellar '
     .//           'aberration only'                    )

      CALL ZZSFXCOR ( ZZRAYNP, ZZMAXRAD, ZZRAYSFX, TRGCDE,
     .                ET,      'S',      USELT,    USECN,
     .                USESTL,  XMIT,     FIXREF,   OBSCDE,
     .                DREFID,  DCLASS,   DCENTR,   DVEC,
     .                SPOINT,  TRGEPC,   SRFVEC,   FOUND  )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINVALIDOPTION)', OK )




C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Zero direction vector' )

      CALL CLEARD ( 3, DVEC )

      CALL ZZSFXCOR ( ZZRAYNP, ZZMAXRAD, ZZRAYSFX, TRGCDE,
     .                ET,      ABCORR,   USELT,    USECN,
     .                USESTL,  XMIT,     FIXREF,   OBSCDE,
     .                DREFID,  DCLASS,   DCENTR,   DVEC,
     .                SPOINT,  TRGEPC,   SRFVEC,   FOUND  )
      CALL CHCKXC ( .TRUE., 'SPICE(ZEROVECTOR)', OK )


C
C     Clean up.
C
C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Clean up.' )

      CALL SPKUEF ( HANDLE(1) )
      CALL DELFIL ( SPK1      )
  
      CALL SPKUEF ( HANDLE(2) )
      CALL DELFIL ( SPK2      )

      CALL T_SUCCESS ( OK )

      END
