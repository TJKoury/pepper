!$Procedure      F_LATSRF ( LATSRF family tests )

      SUBROUTINE F_LATSRF ( OK )
      use pepper_legacy
      use pepper_inc_dla
      use pepper_inc_dsk02
      use pepper_inc_dskdsc
      use pepper_inc_zzdsk
      use pepper_inc_zzabcorr

! $ Abstract
!
!     Exercise the higher-level SPICELIB geometry routine LATSRF.
!     Use DSK-based and ellipsoidal target shape models.
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
!
!     PEPPER: the size of this test's surface lists (SPICELIB's MAXSRF of
!     dsk.inc; the library's surface lists have no limit).
!
      INTEGER               MAXSRF
      PARAMETER           ( MAXSRF = 100 )


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
!     trapped or unintentional and need reporting.  The test family
!     utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine tests the higher-level SPICELIB
!     geometry routine LATSRF.
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
!-    TSPICE Version 1.0.0 11-MAY-2016 (NJB)
!
!-&


!
!     SPICELIB functions
!


!
!     Local parameters
!
      CHARACTER*(*)         DSK0
      PARAMETER           ( DSK0  = 'latsrf_dsk0.bds' )

      CHARACTER*(*)         DSK1
      PARAMETER           ( DSK1  = 'latsrf_dsk1.bds' )

      CHARACTER*(*)         DSK2
      PARAMETER           ( DSK2  = 'latsrf_dsk2.bds' )

      CHARACTER*(*)         DSK3
      PARAMETER           ( DSK3  = 'latsrf_dsk3.bds' )

      CHARACTER*(*)         DSK4
      PARAMETER           ( DSK4  = 'section.bds' )

      CHARACTER*(*)         DSK5
      PARAMETER           ( DSK5  = 'torus.bds' )

      CHARACTER*(*)         PCK
      PARAMETER           ( PCK   = 'test_0008.tpc' )



      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT  = 1.D-14 )

      INTEGER               LBLSIZ
      PARAMETER           ( LBLSIZ = 50 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 320 )

      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 32 )

      INTEGER               NREF
      PARAMETER           ( NREF   = 2 )

      INTEGER               TIMLEN
      PARAMETER           ( TIMLEN = 50 )

      INTEGER               NTARG
      PARAMETER           ( NTARG  = 2 )

      INTEGER               MAXPTS
      PARAMETER           ( MAXPTS = 100 )

      INTEGER               NMAP
      PARAMETER           ( NMAP   = 5 )

      INTEGER               NMETH
      PARAMETER           ( NMETH  = 4 )

      INTEGER               NTEXT
      PARAMETER           ( NTEXT  = 10 )


!
!     Local variables
!
      CHARACTER*(NAMLEN)    FIXREF
      CHARACTER*(LNSIZE)    FRMTXT ( NTEXT )
      CHARACTER*(NAMLEN)    KVNAME
      CHARACTER*(LBLSIZ)    LABEL
      CHARACTER*(MTHLEN)    METHOD
      CHARACTER*(MTHLEN)    METHDS ( NMETH )
      CHARACTER*(NAMLEN)    PNTDEF
      CHARACTER*(NAMLEN)    REFS   ( NREF, NTARG )
      CHARACTER*(NAMLEN)    SHAPE
      CHARACTER*(NAMLEN)    SRFNMS ( NMAP )
      CHARACTER*(NAMLEN)    SUBTYP
      CHARACTER*(NAMLEN)    TARGET
      CHARACTER*(NAMLEN)    TRMTYP
      CHARACTER*(NAMLEN)    TRGFRM
      CHARACTER*(NAMLEN)    TRGNMS ( NTARG )
      CHARACTER*(LNSIZE)    TITLE
      CHARACTER*(TIMLEN)    UTC

      DOUBLE PRECISION      ALTRAD ( 3 )
      DOUBLE PRECISION      BADRAD ( 3 )
      DOUBLE PRECISION      BOUNDS ( 2, 2 )
      DOUBLE PRECISION      CENTER ( 3 )
      DOUBLE PRECISION      CORPAR ( NSYPAR )
      DOUBLE PRECISION      DIRARR ( 3, MAXPTS )
      DOUBLE PRECISION      DLAT
      DOUBLE PRECISION      DLON
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ET0
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LAST
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      LONLAT ( 2, MAXPTS )
      DOUBLE PRECISION      MAXRAD
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      PNTBUF ( 3, MAXPTS )
      DOUBLE PRECISION      R
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      RAYDIR ( 3 )
      DOUBLE PRECISION      RCROSS
      DOUBLE PRECISION      SPOINT ( 3 )
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      VERTEX ( 3 )
      DOUBLE PRECISION      VRTARR ( 3, MAXPTS )
      DOUBLE PRECISION      XPT    ( 3 )
      DOUBLE PRECISION      XPTARR ( 3, MAXPTS )

      INTEGER               BODYID
      INTEGER               CORSYS
      INTEGER               DLADSC ( DLADSZ )
      INTEGER               FIXFID
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               MIX
      INTEGER               N
      INTEGER               NCROSS
      INTEGER               NLAT
      INTEGER               NLON
      INTEGER               NP
      INTEGER               NPOLYV
      INTEGER               NPTS
      INTEGER               NREFS  ( NTARG )
      INTEGER               NSFLAT
      INTEGER               NSFLON
      INTEGER               NSURF
      INTEGER               NV
      INTEGER               PLADDR
      INTEGER               PLATE  ( 3 )
      INTEGER               PTIDX
      INTEGER               REFIDX
      INTEGER               SRFBOD ( NMAP )
      INTEGER               SRFIDS ( NMAP )
      INTEGER               SRFLST ( MAXSRF )
      INTEGER               SURFID
      INTEGER               TRGCDE
      INTEGER               TRGIDX

      LOGICAL               FNDARR ( MAXPTS )
      LOGICAL               FOUND
      LOGICAL FOUND_A1 ( 1 )
      EQUIVALENCE ( FOUND, FOUND_A1 )
      LOGICAL               ISDSK
      LOGICAL               PRI


!
!     Saved variables
!
      SAVE

!
!     Initial values
!

!
!     REFS is a two-dimensional array. There's a set of
!     ray reference  frames for each target. Currently
!     there are only two targets: Mars and Phobos.
!
      DATA                  REFS   / 'IAU_MARS',   'MARS_FIXED', &
                                     'IAU_PHOBOS', ' '          /

      DATA                  NREFS  / 2, 1 /


      DATA                  TRGNMS /  'Mars', &
                                      'PHOBOS'    /

!
!     Note that the last two method strings are identical. This
!     is done to test the logic that uses saved values obtained
!     by parsing method string.
!
      DATA                  METHDS / &
            'ELLIPSOID', &
            'dsk/unprioritized/surfaces="high-res"', &
            'UNPRIORITIZED/ dsk /SURFACES ="LOW-RES"', &
            'UNPRIORITIZED/ dsk /SURFACES ="LOW-RES"' &
                                   /

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_LATSRF' )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Setup:  create PCK file.' )

!
!     Create the PCK file, and load it. Do not delete it.
!
      CALL T_PCK08 ( PCK, .TRUE., .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create LSK, load it, and delete it.
!
      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Set initial time.
!
      UTC = '2004 FEB 17'
      CALL STR2ET ( UTC, ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET  = ET0

!
!     Add an incomplete frame definition to the kernel pool;
!     we'll need this later.
!
      CALL PIPOOL ( 'FRAME_BAD_NAME', 1, [-666] )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Setup: create alternate Mars-fixed frame.' )


      FRMTXT(1)  = 'FRAME_MARS_FIXED         = 1499000'
      FRMTXT(2)  = 'FRAME_1499000_NAME       = ''MARS_FIXED'' '
      FRMTXT(3)  = 'FRAME_1499000_CLASS      = 4'
      FRMTXT(4)  = 'FRAME_1499000_CLASS_ID   = 1499000'
      FRMTXT(5)  = 'FRAME_1499000_CENTER     = 499'
      FRMTXT(6)  = 'TKFRAME_1499000_RELATIVE = ''IAU_MARS'' '
      FRMTXT(7)  = 'TKFRAME_1499000_SPEC     = ''MATRIX'' '
      FRMTXT(8)  = 'TKFRAME_1499000_MATRIX   = ( 0, 1, 0,'
      FRMTXT(9)  = '                           0, 0, 1,'
      FRMTXT(10) = '                           1, 0, 0, )'


      CALL LMPOOL ( FRMTXT, NTEXT )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Setup: create DSK files.' )

!
!     For Mars, surface 1 is the "main" surface.
!
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

!
!     Load main Mars DSK.
!
      CALL FURNSH ( DSK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Surface 2 for Mars is very low-res. We also use a
!     different scale for the Mars radii used to create
!     the tessellated shape model.
!
      BODYID = TRGCDE
      SURFID = 2
      NLON   = 40
      NLAT   = 20

      IF ( EXISTS(DSK1) ) THEN
         CALL DELFIL( DSK1 )
      END IF

      KVNAME = 'BODY499_RADII'
      CALL GDPOOL ( KVNAME, 1, 3, N, RADII, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( '499 radii FOUND', FOUND, .TRUE., OK )

      CALL VSCL ( 2.D0, RADII, ALTRAD )

      CALL PDPOOL ( KVNAME, 3, ALTRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create and load the second DSK.
!
      CALL T_ELDS2Z ( BODYID, SURFID, FIXREF, NLON, NLAT, DSK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH ( DSK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Restore normal Mars radii.
!
      CALL PDPOOL ( KVNAME, 3, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Surface 1 for Phobos is low-res.
!
      BODYID = 401
      SURFID = 1
      FIXREF = 'IAU_PHOBOS'

      NLON   = 200
      NLAT   = 100

      IF ( EXISTS(DSK2) ) THEN
         CALL DELFIL( DSK2 )
      END IF

!
!     Create and load the first Phobos DSK.
!
      CALL T_ELDS2Z ( BODYID, SURFID, FIXREF, NLON, NLAT, DSK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH ( DSK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Surface 2 for Phobos is lower-res. We also use a
!     different scale for the Mars radii used to create
!     the tessellated shape model.
!
      BODYID = 401
      SURFID = 2
      FIXREF = 'IAU_PHOBOS'

      NLON   =  80
      NLAT   =  40

      IF ( EXISTS(DSK3) ) THEN
         CALL DELFIL( DSK3 )
      END IF

      KVNAME = 'BODY401_RADII'
      CALL GDPOOL ( KVNAME, 1, 3, N, RADII, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( '401 radii FOUND', FOUND, .TRUE., OK )

      CALL VSCL ( 3.D0, RADII, ALTRAD )

      CALL PDPOOL ( KVNAME, 3, ALTRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create and load the second Phobos DSK.
!
      CALL T_ELDS2Z ( BODYID, SURFID, FIXREF, NLON, NLAT, DSK3 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Restore normal Phobos radii.
!
      CALL PDPOOL ( KVNAME, 3, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Load the DSK.
!
      CALL FURNSH ( DSK3 )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Setup: create surface map.' )



!
!     Set up a surface name-ID map.
!
      SRFBOD(1) = 499
      SRFIDS(1) = 1
      SRFNMS(1) = 'high-res'

      SRFBOD(2) = 499
      SRFIDS(2) = 2
      SRFNMS(2) = 'low-res'

      SRFBOD(3) = 499
      SRFIDS(3) = 3
      SRFNMS(3) = 'c-g'

      SRFBOD(4) = 401
      SRFIDS(4) = 1
      SRFNMS(4) = 'high-res'

      SRFBOD(5) = 401
      SRFIDS(5) = 2
      SRFNMS(5) = 'low-res'



      CALL PCPOOL ( 'NAIF_SURFACE_NAME', NMAP, SRFNMS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PIPOOL ( 'NAIF_SURFACE_CODE', NMAP, SRFIDS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PIPOOL ( 'NAIF_SURFACE_BODY', NMAP, SRFBOD )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Generate a grid of planetocentric longitude/latitude coordinate
!     pairs. These, combined with surface models, will yield a grid of
!     coordinates at which to compute surface points.
!
      NSFLON = 4
      NSFLAT = 5
      NPTS   = NSFLON * NSFLAT

      DLON   = 2*PI() /   NSFLON
      DLAT   =   PI() / ( NSFLAT - 1 )

      K = 0

      DO I = 1, NSFLON
!
!        We shift the coordinates away from possible plate
!        edges because we can't expect the normal vectors
!        to match our computed values at those locations.
!
         LON = (I-1)*DLON + 1.D-3

         DO J = 1, NSFLAT

            LAT = BRCKTD(  HALFPI() - (J-1)*DLAT,  -HALFPI(), HALFPI() )

            IF ( J .LE. NSFLAT/2 ) THEN

               LAT = LAT - 1.D-3

            ELSE

               LAT = LAT + 1.D-3

            END IF


            K   = K + 1

            LONLAT(1,K) = LON
            LONLAT(2,K) = LAT

         END DO

      END DO



!
! --- Case: ------------------------------------------------------
!
!
!     The first test loop follows. In this loop, we call LATSRF
!     once for each coordinate pair.
!

!
!     Loop over every choice of target.
!
      DO TRGIDX = 1, NTARG

         TARGET = TRGNMS( TRGIDX )

!
!        Set the target ID code.
!
         CALL BODN2C ( TARGET, TRGCDE, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Get target radii.
!
         CALL BODVAR ( TRGCDE, 'RADII', N, RADII )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Loop over the surface point sequence.
!
         DO PTIDX = 1, NPTS

            LON = LONLAT(1,PTIDX)
            LAT = LONLAT(2,PTIDX)

!
!           Loop over every target body-fixed frame choice.
!
            DO REFIDX = 1, NREFS(TRGIDX)

               TRGFRM = REFS ( REFIDX, TRGIDX )

               CALL NAMFRM ( TRGFRM, FIXFID )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Loop over all method choices.
!

               DO MIX = 1, NMETH

                  METHOD = METHDS( MIX )


                  ISDSK = MATCHI( METHOD, '*DSK*', '*', '?' )

!
!- Case: ------------------------------------------------------
!
                  TITLE = 'Target = #; TRGFRM = #; ' &
                  //      'METHOD = #; Longitude (deg) = #; ' &
                  //      'Latitude (deg) = #; ET = #.'
                  CALL REPMC ( TITLE, '#', TARGET,        TITLE )
                  CALL REPMC ( TITLE, '#', TRGFRM,        TITLE )
                  CALL REPMC ( TITLE, '#', METHOD,        TITLE )
                  CALL REPMD ( TITLE, '#', LON*DPR(), 14, TITLE )
                  CALL REPMD ( TITLE, '#', LAT*DPR(), 14, TITLE )
                  CALL REPMD ( TITLE, '#', ET,        14, TITLE )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL TCASE ( TITLE )

!
!                 Call LATSRF to produce a surface point for the
!                 input coordinates.
!
                  CALL LATSRF ( METHOD, TARGET, ET, &
                                TRGFRM, 1,      LONLAT(1,PTIDX), &
                                SPOINT                           )
                  CALL CHCKXC ( .FALSE., ' ', OK )

!
!                 Check the output point. In order to do this, we'll
!                 extract the surface list from the method string.
!
                  CALL ZZPRSMET ( TRGCDE, METHOD, MAXSRF, SHAPE, &
                                  SUBTYP, PRI,    NSURF,  SRFLST, &
                                  PNTDEF, TRMTYP                  )
                  CALL CHCKXC ( .FALSE., ' ', OK )

!
!                 Get an outer bounding radius for the target.
!
                  IF ( ISDSK ) THEN
!
!                    Perform initialization to enable generation
!                    of a bounding radius for the current surface
!                    list.
!
                     CALL ZZSUDSKI( TRGCDE, NSURF, SRFLST, FIXFID )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     CALL ZZMAXRAD ( MAXRAD )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                  ELSE
!
!                    The target shape is an ellipsoid. Just
!                    get the maximum radius.
!
                     MAXRAD = MAX( RADII(1), RADII(2), RADII(3) )

                  END IF
!
!                 Create an inward-pointing ray. The surface
!                 intercept of this ray is our expected result
!                 from LATSRF.
!
                  R = 2.D0 * MAXRAD

                  CALL LATREC ( R, LON, LAT, VERTEX )
                  CALL VMINUS ( VERTEX,      RAYDIR )


                  IF ( ISDSK ) THEN

                     CALL DSKXV ( .FALSE., TARGET, NSURF,  SRFLST, &
                                  ET,      TRGFRM, 1,      VERTEX, &
                                  RAYDIR,  XPT,    FOUND_A1          )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                  ELSE

                     CALL SURFPT ( VERTEX,   RAYDIR,   RADII(1), &
                                   RADII(2), RADII(3), XPT,     FOUND )
                  END IF

                  CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!                 We should get extremely good agreement.
!
                  TOL = VTIGHT

                  CALL CHCKAD ( 'SPOINT', SPOINT, '~~/', &
                                XPT,      3,      TOL,  OK )

               END DO
!
!              End of the method loop.
!
            END DO
!
!           End of the reference frame loop.
!
         END DO
!
!        End of the surface point loop.
!
      END DO
!
!     End of the target loop.
!




!
! --- Case: ------------------------------------------------------
!
!
!     The second test loop follows. In this loop, we call LATSRF
!     once for the full set of coordinate pairs.
!

!
!     Loop over every choice of target.
!
      DO TRGIDX = 1, NTARG

         TARGET = TRGNMS( TRGIDX )

!
!        Set the target ID code.
!
         CALL BODN2C ( TARGET, TRGCDE, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )
!
!        Get target radii.
!
         CALL BODVAR ( TRGCDE, 'RADII', N, RADII )
         CALL CHCKXC ( .FALSE., ' ', OK )
!
!        Loop over every target body-fixed frame choice.
!
         DO REFIDX = 1, NREFS(TRGIDX)

            TRGFRM = REFS ( REFIDX, TRGIDX )

            CALL NAMFRM ( TRGFRM, FIXFID )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Loop over all method choices.
!
            DO MIX = 1, NMETH

               METHOD = METHDS( MIX )


               ISDSK = MATCHI( METHOD, '*DSK*', '*', '?' )

!
!- Case: ------------------------------------------------------
!
               TITLE = 'Target = #; TRGFRM = #; ' &
               //      'METHOD = #; ET = #.'
               CALL REPMC ( TITLE, '#', TARGET,        TITLE )
               CALL REPMC ( TITLE, '#', TRGFRM,        TITLE )
               CALL REPMC ( TITLE, '#', METHOD,        TITLE )
               CALL REPMD ( TITLE, '#', ET,        14, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL TCASE ( TITLE )

!
!              Generate the array of surface points.
!
               CALL LATSRF ( METHOD, TARGET, ET, &
                             TRGFRM, NPTS,   LONLAT, &
                             PNTBUF                 )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Check the output point array. In order to do this, we'll
!              extract the surface list from the method string.

               CALL ZZPRSMET ( TRGCDE, METHOD, MAXSRF, SHAPE, &
                               SUBTYP, PRI,    NSURF,  SRFLST, &
                               PNTDEF, TRMTYP                  )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Get an outer bounding radius for the target.
!
               IF ( ISDSK ) THEN
!
!                 Perform initialization to enable generation
!                 of a bounding radius for the current surface
!                 list.
!
                  CALL ZZSUDSKI( TRGCDE, NSURF, SRFLST, FIXFID )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL ZZMAXRAD ( MAXRAD )
                  CALL CHCKXC ( .FALSE., ' ', OK )

               ELSE
!
!                 The target shape is an ellipsoid. Just
!                 get the maximum radius.
!
                  MAXRAD = MAX( RADII(1), RADII(2), RADII(3) )

               END IF
!
!              Create an array of inward-pointing rays. The respective
!              surface intercepts of these rays are our expected
!              results from LATSRF.
!
               R = 2.D0 * MAXRAD

               DO PTIDX = 1, NPTS

                  LON = LONLAT(1,PTIDX)
                  LAT = LONLAT(2,PTIDX)

                  CALL LATREC ( R, LON, LAT,     VRTARR(1,PTIDX) )
                  CALL VMINUS ( VRTARR(1,PTIDX), DIRARR(1,PTIDX) )

               END DO

               IF ( ISDSK ) THEN

                  CALL DSKXV ( .FALSE., TARGET, NSURF,  SRFLST, &
                               ET,      TRGFRM, NPTS,   VRTARR, &
                               DIRARR,  XPTARR, FNDARR        )
                  CALL CHCKXC ( .FALSE., ' ', OK )

               ELSE

                  DO PTIDX = 1, NPTS

                     CALL SURFPT ( VRTARR(1,PTIDX),  DIRARR(1,PTIDX), &
                                   RADII(1),         RADII(2), &
                                   RADII(3),         XPTARR(1,PTIDX), &
                                   FNDARR(PTIDX)                     )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                  END DO

               END IF

!
!              Make sure the expected points were computed.
!
               DO PTIDX = 1, NPTS

                  LABEL = 'FOUND #'
                  CALL REPMI ( LABEL, '#', PTIDX, LABEL )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL CHCKSL ( LABEL, FOUND, .TRUE., OK )

                  IF ( .NOT. OK ) STOP

               END DO

!
!              Check the output points.
!
!              We should get extremely good agreement.
!
               TOL = VTIGHT

               CALL CHCKAD ( 'PNTBUF', PNTBUF, '~~/', &
                             XPTARR,   3*NPTS, TOL,  OK )
            END DO
!
!           End of the method loop.
!
         END DO
!
!        End of the reference frame loop.
!
      END DO
!
!     End of the target loop.
!




!***********************************************************************
!
!     Normal case: input handling
!
!***********************************************************************


!
! --- Case: ------------------------------------------------------
!

!
!     Input handling tests:  make sure target
!     can be identified using integer "name."
!
      CALL TCASE ( 'Use integer target name.' )

!
!     Set target and target-fixed frame.
!
      TARGET = 'MARS'
      FIXREF = 'IAU_MARS'
      METHOD = 'ellipsoid'

      CALL LATSRF ( METHOD, TARGET, ET, &
                    FIXREF, 1,      LONLAT(1,NPTS/2), XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL LATSRF ( METHOD, '499', ET, &
                    FIXREF, 1,     LONLAT(1,NPTS/2), SPOINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'SPOINT', SPOINT, '=', XPT, 3, 0.D0, OK )




!***********************************************************************
!
!     Normal case: state change detection
!
!***********************************************************************

!
!     Certain subsystem state changes must be detected and responded to
!     by SINCPT. The subsystems (or structures) having states that must
!     be monitored are:
!
!        - Target name-ID mapping
!
!        - Observer name-ID mapping
!
!        - Surface name-ID mapping
!
!        - Target body-fixed frame definition
!
!        - ZZDSKBSR state
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Target name changed to JUPITER for ID code 499.' )

!
!     First, get expected intercept.
!
      ET     = 8 * JYEAR()

      TARGET = 'MARS'
      FIXREF = 'IAU_MARS'

      CALL LATSRF ( METHOD, TARGET, ET, &
                    FIXREF, 1,      LONLAT, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL BODDEF ( 'JUPITER', 499 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL LATSRF ( METHOD, TARGET, ET, &
                    FIXREF, 1,      LONLAT, SPOINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We expect exact matches here.
!
      CALL CHCKAD ( 'SPOINT', SPOINT, '=', XPT, 3, 0.D0, OK )
!
!     Restore original mapping.
!
      CALL BODDEF ( 'JUPITER', 599 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Observer name changed to SUN for ID code 399.' )

      CALL BODDEF ( 'SUN', 399 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL LATSRF ( METHOD, TARGET, ET, &
                    FIXREF, 1,      LONLAT, SPOINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We expect exact matches here.
!
      CALL CHCKAD ( 'SPOINT', SPOINT, '=', XPT, 3, 0.D0, OK )

!
!     Restore original mapping.
!
      CALL BODDEF ( 'SUN', 10 )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Mars high-res surface name changed to ' &
      //           'AAAbbb.'                               )

!
!     Get expected results first.
!
      ET     = 8 * JYEAR()

      TARGET = 'MARS'
      FIXREF = 'IAU_MARS'


      METHOD = 'dsk/unprioritized/surfaces = 1'

      CALL LATSRF ( METHOD, TARGET, ET, &
                    FIXREF, 1,      LONLAT, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )



      CALL CHCKXC ( .FALSE., ' ', OK )


      SRFNMS(1) = 'AAAbbb'

      CALL PCPOOL ( 'NAIF_SURFACE_NAME', NMAP, SRFNMS )
      CALL CHCKXC ( .FALSE., ' ', OK )



      METHOD = 'dsk/unprioritized/surfaces = AAAbbb'

      CALL LATSRF ( METHOD, TARGET, ET, &
                    FIXREF, 1,      LONLAT, SPOINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We expect exact matches here.
!
      CALL CHCKAD ( 'SPOINT', SPOINT, '=', XPT, 3, 0.D0, OK )

!
!     Restore original mapping.
!
      SRFNMS(1) = 'high-res'

      CALL PCPOOL ( 'NAIF_SURFACE_NAME', NMAP, SRFNMS )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Unload Mars high-res DSK.'  )

!
!     Get reference result using low-res Mars DSK.
!
      ET     = 8 * JYEAR()

      TARGET = 'MARS'
      FIXREF = 'IAU_MARS'

      METHOD = 'dsk/unprioritized/surfaces = low-res'

      CALL LATSRF ( METHOD, TARGET, ET, &
                    FIXREF, 1,      LONLAT, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )



      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Unload the high-res DSK; set METHOD to remove
!     surface specification.
!
      CALL UNLOAD ( DSK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      METHOD = 'dsk/unprioritized'

      CALL LATSRF ( METHOD, TARGET, ET, &
                    FIXREF, 1,      LONLAT, SPOINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We expect exact matches here.
!
      CALL CHCKAD ( 'SPOINT', SPOINT, '=', XPT, 3, 0.D0, OK )


!
!     Restore original mapping.
!
      CALL BODDEF ( 'SUN', 10 )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Unload Mars low-res DSK; ' &
      //           'reload Mars high-res DSK.'  )

!
!     Restore DSK, unload low-res DSK, and repeat computation.
!
      CALL FURNSH ( DSK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL UNLOAD ( DSK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      METHOD = 'dsk/unprioritized'

      CALL LATSRF ( METHOD, TARGET, ET, &
                    FIXREF, 1,      LONLAT, SPOINT )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Make sure the result matches that obtained with the
!     high-res DSK specified.
!
      METHOD = 'dsk/unprioritized/ SURFACES = "HIGH-RES" '


      CALL LATSRF ( METHOD, TARGET, ET, &
                    FIXREF, 1,      LONLAT, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We expect exact matches here.
!
      CALL CHCKAD ( 'SPOINT', SPOINT, '=', XPT, 3, 0.D0, OK )


!***********************************************************************
!
!     Error handling tests follow.
!
!***********************************************************************


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Invalid method.' )

      ET     = 0.D0
      TARGET = 'MARS'
      FIXREF = 'IAU_MARS'

      METHOD = 'ELLIPSOD'

      CALL LATSRF ( METHOD, TARGET, ET, &
                    FIXREF, 1,      LONLAT(1,NPTS/2), SPOINT )
      CALL CHCKXC ( .TRUE., 'SPICE(BADMETHODSYNTAX)', OK )


      METHOD = 'ELLIPSOID/UNPRIORITIZED'

      CALL LATSRF ( METHOD, TARGET, ET, &
                    FIXREF, 1,      LONLAT(1,NPTS/2), SPOINT )
      CALL CHCKXC ( .TRUE., 'SPICE(BADMETHODSYNTAX)', OK )



      METHOD = 'DSK/unprioritized/intercept'

      CALL LATSRF ( METHOD, TARGET, ET, &
                    FIXREF, 1,      LONLAT(1,NPTS/2), SPOINT )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDMETHOD)', OK )


      METHOD = 'DSK/nadir/unprioritized'

      CALL LATSRF ( METHOD, TARGET, ET, &
                    FIXREF, 1,      LONLAT(1,NPTS/2), SPOINT )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDMETHOD)', OK )


      METHOD = 'DSK'

      CALL LATSRF ( METHOD, TARGET, ET, &
                    FIXREF, 1,      LONLAT(1,NPTS/2), SPOINT )
      CALL CHCKXC ( .TRUE., 'SPICE(BADPRIORITYSPEC)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Invalid target name.' )

      METHOD = 'DSK/UNPRIORITIZED'

      CALL LATSRF ( METHOD, 'XXX',   ET, &
                    FIXREF, 1,       LONLAT(1,NPTS/2), SPOINT )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Invalid reference frame center' )


      CALL LATSRF ( METHOD,      TARGET, ET, &
                    'IAU_EARTH', 1,      LONLAT(1,NPTS/2), SPOINT )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Unrecognized reference frame' )


      CALL LATSRF ( METHOD,      TARGET, ET, &
                    'ZZZ', 1,      LONLAT(1,NPTS/2), SPOINT )
      CALL CHCKXC ( .TRUE., 'SPICE(NOFRAME)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'No orientation data for target' )

!
!     This error applies only to the DSK case.
!
      METHOD = 'UNPRIORITIZED  /  DSK'

      CALL CLPOOL
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create a class 2 (PCK) frame with no orientation data.
!

      FRMTXT(1)  = 'FRAME_MARS_FIXED_PCK     = 1499001'
      FRMTXT(2)  = 'FRAME_1499001_NAME       = ''MARS_FIXED_PCK'' '
      FRMTXT(3)  = 'FRAME_1499001_CLASS      = 2'
      FRMTXT(4)  = 'FRAME_1499001_CLASS_ID   = 1499001'
      FRMTXT(5)  = 'FRAME_1499001_CENTER     = 499'

      CALL LMPOOL ( FRMTXT, 5 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL LMPOOL ( FRMTXT, NTEXT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL LATSRF ( METHOD, TARGET,  ET, &
                    'MARS_FIXED_PCK', 1, LONLAT(1,NPTS/2), SPOINT )
      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)', OK )

      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL LDPOOL ( PCK )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'No radius data for target' )

      METHOD = 'ELLIPSOID'

      CALL DVPOOL ( 'BODY499_RADII' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL LATSRF ( METHOD, TARGET, ET, &
                    FIXREF, 1,      LONLAT(1,NPTS/2), SPOINT )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

      CALL LDPOOL ( PCK )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad radius data for target' )

      METHOD = 'ELLIPSOID'

!
!     Fetch original radii.
!
      CALL BODVCD (  499, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Overwrite good radii with bad in the kernel pool.
!
      CALL VPACK  ( -1.D0, 0.D0,  3.D0, BADRAD )

      CALL PDPOOL ( 'BODY499_RADII', 3, BADRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL LATSRF ( METHOD, TARGET, ET, &
                    FIXREF, 1,      LONLAT(1,NPTS/2), SPOINT )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )

!
!     Replace original radii.
!
      CALL PDPOOL ( 'BODY499_RADII', 3, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'No loaded DSKs.' )


      METHOD = 'UNPRIORITIZED  /  DSK'

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



      METHOD = 'dsk/unprioritized'

      CALL LATSRF ( METHOD, TARGET, ET, &
                    FIXREF, 1,      LONLAT(1,NPTS/2), SPOINT )
      CALL CHCKXC ( .TRUE., 'SPICE(NOLOADEDDSKFILES)', OK )




!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'The surface point corresponding to the ' &
      //           'input coordinates is on the wrong side ' &
      //           'of the object.'                         )

!
!     Create a DSK containing only a section of a tessellated
!     ellipsoid.
!
      TARGET = 'MARS'
      TRGCDE = 499
      SURFID = 3
      TRGFRM = 'IAU_MARS'
      FIRST  = -50 * JYEAR()
      LAST   =  50 * JYEAR()
      CORSYS = LATSYS
      CALL CLEARD ( NSYPAR, CORPAR )

      BOUNDS(1,1) =  150.D0 * RPD()
      BOUNDS(2,1) = -150.D0 * RPD()
      BOUNDS(1,2) =  -30.D0 * RPD()
      BOUNDS(2,2) =   30.D0 * RPD()

      NLON = 50
      NLAT = 25

      IF ( EXISTS(DSK4) ) THEN
         CALL DELFIL(DSK4)
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL T_SECDS2 ( TRGCDE,   SURFID, TRGFRM, FIRST,    LAST, &
                      CORSYS,   CORPAR, BOUNDS, RADII(1), RADII(2), &
                      RADII(3), NLON,   NLAT,   .FALSE.,  .FALSE., &
                      DSK4                                          )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Reader: avert your eyes.
!
!     At this point, we're going to do something rather perverse:
!     we're going to open the DSK we just created and flip the
!     parity of its plates so that their "inside" surfaces are on
!     the far side relative to the origin.
!
!     Start by getting the DAS integer address of the plate section
!     of the first DLA segment in the file.
!
      CALL DASOPR ( DSK4, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DLABFS ( HANDLE, DLADSC, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      PLADDR = DLADSC(IBSIDX) + IXPLAT

      CALL DSKZ02 ( HANDLE, DLADSC, NV, NP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASOPW ( DSK4, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, NP

         CALL DASRDI ( HANDLE, PLADDR, PLADDR+2, PLATE )
         CALL CHCKXC ( .FALSE., ' ', OK )
!
!        Do the deed. Write the updated plate back to the file.
!
         CALL SWAPI  ( PLATE(2), PLATE(3) )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL DASUDI ( HANDLE, PLADDR, PLADDR+2, PLATE )

         PLADDR = PLADDR + 3

      END DO
!
!     Close the updated file. Load for read access.
!
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH ( DSK4 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      METHOD = 'UNPRIORITIZED/DSK/SURFACES="3"'

!
!     Pick a coordinate pair across the origin from the section.
!
      LONLAT(1,1) = 0.D0
      LONLAT(2,1) = 0.D0
      ET          = 0.D0

      CALL LATSRF ( METHOD, TARGET, ET, &
                    TRGFRM, 1,      LONLAT, SPOINT )
      CALL CHCKXC ( .TRUE., 'SPICE(SHAPENOTSUPPORTED)', OK )


      CALL UNLOAD ( DSK4 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'No surface point exists for the given ' &
      //           'input coordinates.'                   )

!
!     Create a torus DSK.
!
      TARGET = 'MARS'
      TRGCDE = 499
      SURFID = 4
      TRGFRM = 'IAU_MARS'
      NPOLYV = 50
      NCROSS = 100
      R      = 10
      RCROSS =  3

      CALL VPACK ( 0.D0, 0.D0, 0.D0, CENTER )
      CALL VPACK ( 1.D0, 0.D0, 0.D0, NORMAL )

!
!     The X axis of the IAU_MARS frame is the axis of circular
!     symmetry of the torus.
!
      IF ( EXISTS(DSK5) ) THEN
         CALL DELFIL( DSK5 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL T_TORUS ( TRGCDE, SURFID, TRGFRM, NPOLYV, NCROSS, &
                     R,      RCROSS, CENTER, NORMAL, DSK5   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH ( DSK5 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      METHOD = 'UNPRIORITIZED/DSK/SURFACES="4"'

      LONLAT(1,1) = 1.D-3 * RPD()
      LONLAT(2,1) = 1.D-2 * RPD()
      ET          = 0.D0

      CALL LATSRF ( METHOD, TARGET, ET, &
                    TRGFRM, 1,      LONLAT, SPOINT )
      CALL CHCKXC ( .TRUE., 'SPICE(POINTNOTFOUND)', OK )


      CALL UNLOAD ( DSK5 )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!

!
!     Clean up.
!
      CALL DELFIL ( PCK )
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

      CALL UNLOAD ( DSK5 )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL DELFIL ( DSK5      )
      CALL CHCKXC ( .FALSE., ' ', OK )



      CALL T_SUCCESS ( OK )

      RETURN
      END
