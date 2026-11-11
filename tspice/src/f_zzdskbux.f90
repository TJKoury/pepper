!$Procedure F_ZZDSKBUX ( ZZDSKBUX tests )

      SUBROUTINE F_ZZDSKBUX ( OK )
      use pepper_inc_dla
      use pepper_inc_dsk02
      use pepper_inc_dskdsc

! $ Abstract
!
!     Exercise the private SPICELIB routine ZZDSKBUX.
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
!     OK       is a logical that indicates the test status to the
!              caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  This routine does not generate any errors. Routines in its
!         call tree may generate errors that are either intentional and
!         trapped or unintentional and need reporting. The test family
!         utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine tests the private SPICELIB routines
!
!        ZZDSKBUX
!        ZZSBFXR
!        ZZSBFXRI
!
!     as well as
!
!        ZZDSKBUN
!        ZZSBFNRM
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     B.V. Semenov       (JPL)
!
! $ Version
!
!-    TSPICE Version 1.1.0, 29-MAY-2021 (BVS) (JDR)
!
!        Added missing 'D' to literal DP constants.
!
!        Edited the header to comply with NAIF standard.
!
!-    TSPICE Version 1.0.0, 22-JUL-2016 (NJB)
!
!-&


! $ Index_Entries
!
!     None.
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      JYEAR
      DOUBLE PRECISION      PI

      LOGICAL               EXISTS

!
!     Local Parameters
!
      CHARACTER*(*)         PCK0
      PARAMETER           ( PCK0   = 'zzdskbux_test0.tpc' )

      CHARACTER*(*)         TOPFK0
      PARAMETER           ( TOPFK0 = 'zzdskbux_mars_topo.tf' )

      CHARACTER*(*)         TOPFK1
      PARAMETER           ( TOPFK1 = 'zzdskbux_saturn_topo.tf' )

      CHARACTER*(*)         TOPSP0
      PARAMETER           ( TOPSP0 = 'zzdskbux_mars_topo.bsp' )

      CHARACTER*(*)         TOPSP1
      PARAMETER           ( TOPSP1 = 'zzdskbux_saturn_topo.bsp' )



!      DOUBLE PRECISION      VTIGHT
!      PARAMETER           ( VTIGHT = 1.D-14 )

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-12 )




      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 32 )

      INTEGER               FILSIZ
      PARAMETER           ( FILSIZ = 255 )

      INTEGER               LABSIZ
      PARAMETER           ( LABSIZ = 40 )

      INTEGER               MAXFH
      PARAMETER           ( MAXFH  = 2 )

      INTEGER               MAXFW
      PARAMETER           ( MAXFW  = 4 )

      INTEGER               MAXH
      PARAMETER           ( MAXH   = 8 )

      INTEGER               MAXW
      PARAMETER           ( MAXW   = 4 )

      INTEGER               NTILEH
      PARAMETER           ( NTILEH = 3 )

      INTEGER               NTILEW
      PARAMETER           ( NTILEW = 6 )

      INTEGER               NLAYER
      PARAMETER           ( NLAYER = 3 )

      INTEGER               NTARG
      PARAMETER           ( NTARG  = 2 )

      INTEGER               MAXP
      PARAMETER           ( MAXP  = 20000 )

      INTEGER               MAXV
      PARAMETER           ( MAXV  = MAXP/2 )


!
!     Local Variables
!
      CHARACTER*(NAMLEN)    FIXREF
      CHARACTER*(NAMLEN)    FIXRFS ( NTARG )
      CHARACTER*(NAMLEN)    FRAME
      CHARACTER*(LABSIZ)    LABEL
      CHARACTER*(NAMLEN)    SITFNM ( MAXFH, MAXFW, NTARG )
      CHARACTER*(NAMLEN)    SITNMS ( MAXFH, MAXFW, NTARG )
      CHARACTER*(FILSIZ)    TOPFK
      CHARACTER*(FILSIZ)    TOPSPK
      CHARACTER*(NAMLEN)    TARGET
      CHARACTER*(NAMLEN)    TARGS  ( NTARG )
      CHARACTER*(FILSIZ)    TILDSK ( NTILEH, NTILEW, NLAYER, NTARG )

      DOUBLE PRECISION      A
      DOUBLE PRECISION      ANGLES ( 3, MAXFH, MAXFW )
      DOUBLE PRECISION      B
      DOUBLE PRECISION      BOUNDS ( 2, 2 )
      DOUBLE PRECISION      C
      DOUBLE PRECISION      CORPAR ( NSYPAR )
      DOUBLE PRECISION      CTRLAT
      DOUBLE PRECISION      CTRLON
      DOUBLE PRECISION      DC     ( 1 )
      DOUBLE PRECISION      DFRLAT
      DOUBLE PRECISION      DFRLON
      DOUBLE PRECISION      DLAT
      DOUBLE PRECISION      DLON
      DOUBLE PRECISION      DPLAT
      DOUBLE PRECISION      DPLON
      DOUBLE PRECISION      DSKDSC ( DSKDSZ )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LAST
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      MAXLAT
      DOUBLE PRECISION      MAXLON
      DOUBLE PRECISION      MINLAT
      DOUBLE PRECISION      MINLON
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      OFFSET ( 3 )
      DOUBLE PRECISION      R
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      RAYDIR ( 3 )
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      SEGDIR ( 3 )
      DOUBLE PRECISION      SEGVTX ( 3 )
      DOUBLE PRECISION      SITPOS ( 3, MAXFH, MAXFW, NTARG )
      DOUBLE PRECISION      TILEH
      DOUBLE PRECISION      TILEW
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      VERTEX ( 3 )
      DOUBLE PRECISION      VERTS  ( 3, MAXV )
      DOUBLE PRECISION      VTEMP  ( 3 )
      DOUBLE PRECISION      XDSKDS ( DSKDSZ )
      DOUBLE PRECISION      XFORM  ( 3, 3 )
      DOUBLE PRECISION      XNORML ( 3 )
      DOUBLE PRECISION      XPT    ( 3 )
      DOUBLE PRECISION      XXPT   ( 3 )

      INTEGER               AXES   ( 3, MAXFH, MAXFW )
      INTEGER               BIDS   ( NTARG )
      INTEGER               BIX
      INTEGER               BODYID
      INTEGER               CLASS
      INTEGER               CLSSID
      INTEGER               CORSYS
      INTEGER               DLADSC ( DLADSZ )
      INTEGER               FIXFID
      INTEGER               FIXH
      INTEGER               FIXW
      INTEGER               FRMCTR
      INTEGER               H
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               IC     ( 1 )
      INTEGER               J
!      INTEGER               K
!      INTEGER               L
      INTEGER               LAYRIX
      INTEGER               LAYSRF ( MAXSRF, NLAYER )
      INTEGER               N
!      INTEGER               NFLAT
!      INTEGER               NFLON
      INTEGER               NLAT
      INTEGER               NLON
      INTEGER               NP
      INTEGER               NPLAT
      INTEGER               NPLON
      INTEGER               NV
      INTEGER               NSITES
      INTEGER               NSURF
      INTEGER               PLATES ( MAXP )
      INTEGER               PLID
      INTEGER               SEGFID
      INTEGER               SITFID ( MAXFH, MAXFW, NTARG )
      INTEGER               SITIDS ( MAXFH, MAXFW, NTARG )
      INTEGER               SURFID
!      INTEGER               SRFLST ( MAXSRF )
      INTEGER               TARGIX
      INTEGER               VIX
      INTEGER               W

      LOGICAL               FOUND
      LOGICAL               MAKVTL
      LOGICAL               USEPAD



!
!     Saved values
!
      SAVE

!
!     Initial values
!
      DATA                  BIDS   / 499,        699          /
      DATA                  TARGS  / 'MARS',     'SATURN'     /
      DATA                  FIXRFS / 'IAU_MARS', 'IAU_SATURN' /

!
!     Open the test family.
!
      CALL TOPEN ( 'F_ZZDSKBUX' )




!***********************************************************************
!
!     Set-up
!
!***********************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Create topocentric kernels.' )

!
!     Our setup will be rather elaborate, since we're creating a
!     set of kernels that can be used to exercise all logic branches
!     of a moderately complex algorithm.
!
!     We'll need a generic text PCK. Keep the file after loading it.
!
      IF ( EXISTS(PCK0) ) THEN
         CALL DELFIL( PCK0 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL TSTPCK ( PCK0, .TRUE., .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create a set of topocentric frames for Mars and Saturn.
!
      FIRST  = -100 * JYEAR()
      LAST   =  100 * JYEAR()

      NSITES = MAXFW * MAXFH

      DFRLON = 2*PI() / MAXFW
      DFRLAT =   PI() / MAXFH

      DO I = 1, NTARG

         IF ( I .EQ. 1 ) THEN
            TOPFK  = TOPFK0
            TOPSPK = TOPSP0
         ELSE
            TOPFK  = TOPFK1
            TOPSPK = TOPSP1
         END IF

         TARGET = TARGS(I)
         BODYID = BIDS(I)
         FIXREF = FIXRFS(I)

         DO W = 1, MAXFW

            LON = (W-0.5D0) * DFRLON

            DO H = 1, MAXFH

               LAT = HALFPI()  -  (H - 0.5D0)*DFRLAT

               SITNMS(H,W,I) = '#_H#_W#'
               CALL REPMC ( SITNMS(H,W,I), '#', TARGET, SITNMS(H,W,I) )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( SITNMS(H,W,I), '#', H, SITNMS(H,W,I) )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( SITNMS(H,W,I), '#', W, SITNMS(H,W,I) )
               CALL CHCKXC ( .FALSE., ' ', OK )

               SITIDS(H,W,I) = 1000*BODYID  +  H + (W-1)*MAXFH
               SITFNM(H,W,I) = SITNMS(H,W,I)
               CALL SUFFIX ( '_TOPO', 0, SITFNM(H,W,I) )

               SITFID(H,W,I) = SITIDS(H,W,I)

               AXES(1,H,W)   = 3
               AXES(2,H,W)   = 2
               AXES(3,H,W)   = 3

               ANGLES(1,H,W) = -LON
               ANGLES(2,H,W) =  LAT - HALFPI()
               ANGLES(3,H,W) =  PI()

               CALL SRFREC ( BODYID, LON, LAT, SITPOS(1,H,W,I) )
               CALL CHCKXC ( .FALSE., ' ', OK )

            END DO

         END DO


         IF ( EXISTS(TOPFK) ) THEN
            CALL DELFIL( TOPFK )
            CALL CHCKXC ( .FALSE., ' ', OK )
         END IF

         IF ( EXISTS(TOPSPK) ) THEN
            CALL DELFIL( TOPSPK )
            CALL CHCKXC ( .FALSE., ' ', OK )
         END IF

         CALL T_TOPKER ( TOPFK,          TOPSPK,          TARGET, &
                         FIXREF,         NSITES,          SITIDS(1,1,I), &
                         SITNMS(1,1,I),  SITPOS(1,1,1,I), SITFNM(1,1,I), &
                         SITFID(1,1,I),  FIRST,           LAST, &
                         AXES,           ANGLES                        )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO


!
!     Load the topocentric kernels.
!
      CALL FURNSH ( TOPFK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH ( TOPSP0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH ( TOPFK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH ( TOPSP1 )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Create DSKs.' )


!
!     Start out by creating a three sets of tiled ellipsoid DSKs for
!     Mars and Saturn respectively. The DSKs will represent different
!     "layers": they'll be scaled versions of each other.
!
!     The first layer will be a tessellation using the default
!     target body radii.
!
!
!     Set the coordinate system.
!
      CORSYS = LATSYS

      CALL CLEARD ( 3, CORPAR )

!
!     Use padding.
!
      USEPAD = .TRUE.

!
!     Don't bother with the vertex-plate mapping.
!
      MAKVTL = .FALSE.

!
!     NLON and NLAT indicate the number of longitude and latitude
!     bands of plates per tile, respectively.
!
      NLON   = MAXW
      NLAT   = MAXH

!
!     Determine the spatial coverage of each tile.
!
      TILEW = 2*PI() / NTILEW
      TILEH =   PI() / NTILEH

      DO TARGIX = 1, NTARG

         TARGET = TARGS ( TARGIX )
         BODYID = BIDS  ( TARGIX )
         FIXREF = FIXRFS( TARGIX )

!
!        Get the radii used for the tessellation.
!
         CALL BODVCD ( BODYID, 'RADII', 3, N, RADII )
         CALL CHCKXC ( .FALSE., ' ', OK )


         A = RADII(1)
         B = RADII(2)
         C = RADII(3)


         DO LAYRIX = 1, NLAYER

            DO W = 1, NTILEW

               DO H = 1, NTILEH

                  TILDSK(H,W,LAYRIX,TARGIX) = &
                    'zzdskbux_#_h#_w#_layer#.bds'

                  CALL REPMC ( TILDSK(H,W,LAYRIX,TARGIX), &
                               '#', TARGS(TARGIX), &
                               TILDSK(H,W,LAYRIX,TARGIX) )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL LCASE ( TILDSK(H,W,LAYRIX,TARGIX), &
                               TILDSK(H,W,LAYRIX,TARGIX) )

                  CALL REPMI ( TILDSK(H,W,LAYRIX,TARGIX), '#', H, &
                               TILDSK(H,W,LAYRIX,TARGIX)          )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL REPMI ( TILDSK(H,W,LAYRIX,TARGIX), '#', W, &
                               TILDSK(H,W,LAYRIX,TARGIX)          )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL REPMI ( TILDSK(H,W,LAYRIX,TARGIX), '#', LAYRIX, &
                               TILDSK(H,W,LAYRIX,TARGIX)              )
                  CALL CHCKXC ( .FALSE., ' ', OK )


!                  CALL TOSTDO ( '0 '//TILDSK(H,W,LAYRIX,TARGIX) )


                  IF ( EXISTS( TILDSK(H,W,LAYRIX,TARGIX) ) ) THEN
                     CALL DELFIL( TILDSK(H,W,LAYRIX,TARGIX) )
                  END IF

!
!                 Each tile gets its own surface ID.
!
                  SURFID = H + (W-1)*NTILEH + (LAYRIX-1)*NTILEH*NTILEW

!
!                 Set coverage for the current tile.
!
                  LON         = (W-1)*TILEW

                  BOUNDS(1,1) = LON
                  BOUNDS(2,1) = LON  + TILEW

                  LAT         = HALFPI() - (H-1)*TILEH

                  BOUNDS(2,2) = LAT
                  BOUNDS(1,2) = LAT  - TILEH

                  IF ( LAYRIX .EQ. 1 ) THEN
!
!                    Use one of the topocentric frames.
!
!                    Decide which frame to use. The frame will be that
!                    associated with the lat/lon rectangle centered at
!                    the closest topocentric frame center.
!
                     CTRLAT = LAT - TILEH/2
                     CTRLON = LON + TILEW/2

                     FIXH = MIN( INT( (HALFPI() - CTRLAT)/ DFRLAT ) + 1, &
                                 MAXFH                              )

                     FIXW = MIN( INT(  CTRLON            / DFRLON ) + 1, &
                                 MAXFW )


                     FRAME = SITFNM( FIXH, FIXW, TARGIX )

                  ELSE

                     FRAME = FIXRFS( TARGIX )

                     IF ( LAYRIX .EQ. 3 ) THEN
!
!                       Use greatly contracted radii.
!
                        SCALE = 1.D-1

                        A = RADII(1) * SCALE
                        B = RADII(2) * SCALE
                        C = RADII(3) * SCALE

                     END IF

                  END IF

!
!                  TILDSK(H,W,LAYRIX,TARGIX) = 'bux0.bds'
!
!
!                 Note the time bounds.
!
                  FIRST  =  10 * ( LAYRIX - 4 ) * JYEAR()
                  LAST   =  10 *   LAYRIX       * JYEAR()


                  IF ( LAYRIX .GT. 1 ) THEN

                     CALL T_SECDS2( BODYID, SURFID, FRAME,  FIRST, &
                                    LAST,   CORSYS, CORPAR, BOUNDS, &
                                    A,      B,      C,      NLON, &
                                    NLAT,   MAKVTL, USEPAD, &
                                    TILDSK(H,W,LAYRIX,TARGIX)      )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                  ELSE

!
!                    Create the same section we would have done for the
!                    body-centered frame, but use a topocentric frame.
!                    We can't call T_SECDS2 to do this for us; we'll
!                    need to get our hands dirty.
!
!                    We can, however, create the plate set easily
!                    enough.
!
                     MINLON = BOUNDS(1,1)
                     MAXLON = BOUNDS(2,1)
                     MINLAT = BOUNDS(1,2)
                     MAXLAT = BOUNDS(2,2)

!
!                    Add longitude and latitude padding.
!
                     DPLON = ( BOUNDS(2,1) - BOUNDS(1,1) ) / NLON
                     DPLAT = ( BOUNDS(2,2) - BOUNDS(1,2) ) / NLAT

                     NPLON = NLON
                     NPLAT = NLAT

                     IF ( MINLAT .GT. 0.D0 ) THEN
                        MINLAT = MINLAT - DPLAT
                        NPLAT  = NPLAT  + 1
                     END IF

                     IF ( MAXLAT .LT. 0.D0 ) THEN
                        MAXLAT = MAXLAT + DPLAT
                        NPLAT  = NPLAT  + 1
                     END IF

                     MINLON = MINLON - DPLON
                     MAXLON = MAXLON + DPLON
                     NPLON  = NPLON  + 2

                     CALL ZZELLSEC( A,      B,      C,      MINLON, &
                                    MAXLON, MINLAT, MAXLAT, NPLON, &
                                    NPLAT,  MAXV,   MAXP,   NV, &
                                    VERTS,  NP,     PLATES         )
!
!                    We're not done with this DSK. We're going to shift
!                    the vertices to make them relative to the
!                    topocentric frame center, and we'll transform the
!                    vertices to the reference frame FRAME.
!
!                    Get the frame center offset.
!
                     CALL SPKEZP ( SITIDS(FIXH,FIXW,TARGIX), 0.D0, &
                                   FIXREF, 'NONE', &
                                   BODYID, OFFSET, &
                                   LT                             )
                     CALL CHCKXC ( .FALSE., ' ', OK )


!
!                    Get the transformation from the body-centered,
!                    body-fixed frame to the topocentric one.
!
                     CALL PXFORM ( FIXREF, SITFNM(FIXH,FIXW,TARGIX), &
                                   0.D0,   XFORM             )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     DO VIX = 1, NV

                        CALL VSUB ( VERTS(1,VIX), OFFSET, VTEMP        )

                        CALL MXV  ( XFORM,        VTEMP,  VERTS(1,VIX) )

                     END DO

!
!                    Update the segment bounds to reflect that we're
!                    using a topocentric frame. The longitude extent is
!                    360 degrees; the latitude extent is 180 degrees.
!
                     BOUNDS(1,1) =  0.D0
                     BOUNDS(2,1) =  2*PI()
                     BOUNDS(1,2) = -HALFPI()
                     BOUNDS(2,2) =  HALFPI()

!
!                    Create a DSK from our plate set.
!
                     CALL T_WRTPLT ( BODYID, SURFID, FRAME,  FIRST, &
                                     LAST,   CORSYS, CORPAR, BOUNDS, &
                                     NV,     NP,     VERTS,  PLATES, &
                                     MAKVTL, &
                                     TILDSK(H,W,LAYRIX,TARGIX)       )

                     CALL CHCKXC ( .FALSE., ' ', OK )

                  END IF

               END DO

            END DO

         END DO

      END DO


!***********************************************************************
!
!     ZZDSKBUX, ZZSBFXR, ZZSBFXRI Normal cases
!
!***********************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Simple Mars intercept case. Use layers 1 and 2.' )

!
!     Load all DSKs.
!

      DO TARGIX = 1, NTARG

         DO LAYRIX = 1, NLAYER

            DO W = 1, NTILEW

               DO H = 1, NTILEH

                  CALL FURNSH(  TILDSK( H, W, LAYRIX, TARGIX ) )
                  CALL CHCKXC ( .FALSE., ' ', OK )

               END DO

            END DO

         END DO

      END DO


!
!     Specify the surfaces from the first Mars layer. This layer
!     uses topocentric frames.
!
      NSURF = NTILEH * NTILEW

      DO I = 1, NSURF

         LAYSRF(I,1) = I
         LAYSRF(I,2) = I + NSURF
         LAYSRF(I,3) = I + NSURF*2

      END DO


!
!     Create ray. R is a scale factor used to
!     control the magnitude of the vertex.
!
      R = 1.D6

!
!     Pick a vertex that doesn't lie on a meridian
!     containing any plate vertices.
!
      CALL VPACK ( 1.1D0*R, -R, 2*R, VERTEX )

      CALL VMINUS ( VERTEX, RAYDIR )

      BODYID = 499

      ET     = 0.D0

!
!     Compute our intercept in the IAU_MARS frame.
!
      BIX    = 1
      FIXREF = FIXRFS(BIX)
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We'll call ZZSBFXR rather than ZZDSKBUX since the former
!     calls the latter, and the former has all of the flexibility
!     we need, but has a less complex calling sequence.
!
      CALL ZZSBFXR ( BODYID, NSURF,  LAYSRF(1,1), ET, &
                     FIXFID, VERTEX, RAYDIR,      XPT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND (layer 1)', FOUND, .TRUE., OK )

      IF ( FOUND ) THEN
!
!        Get the intercept on the second layer. This layer uses
!        the body-centered frame.
!
         CALL ZZSBFXR ( BODYID, NSURF,  LAYSRF(1,2), ET, &
                        FIXFID, VERTEX, RAYDIR,      XXPT, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND (layer 2)', FOUND, .TRUE., OK )

         TOL = TIGHT

!
!        Compare to the intercept on layer 1 found by ZZDSKXR.
!
         CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TOL, OK )

!
!        Now use ZZSBFXRI to do the computation. Use the second layer.
!
         CALL ZZSBFXRI ( BODYID, NSURF,  LAYSRF(1,2), ET, &
                         FIXFID, VERTEX, RAYDIR,      XPT, HANDLE, &
                         DLADSC, DSKDSC, DC,          IC,  FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'ZZSBFXRI FOUND (layer 2)', FOUND, .TRUE., OK )

         CALL CHCKAD ( 'ZZSBFXRI XPT', XPT, '~~/', XXPT, 3, TOL, OK )

!
!        Use ZZSBFXRI to find the intercept on the first layer.
!
         CALL ZZSBFXRI ( BODYID, NSURF,  LAYSRF(1,1), ET, &
                         FIXFID, VERTEX, RAYDIR,      XPT, HANDLE, &
                         DLADSC, DSKDSC, DC,          IC,  FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'ZZSBFXRI FOUND (layer 2)', FOUND, .TRUE., OK )

         CALL CHCKAD ( 'ZZSBFXRI XPT', XPT, '~~/', XXPT, 3, TOL, OK )

!
!        Check the handle, DSK and DLA descriptors, and plate ID
!        from ZZSBFXRI.
!
         CALL DSKGD ( HANDLE, DLADSC, XDSKDS )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( 'DSKDSC', DSKDSC, '=', XDSKDS, DSKDSZ, 0.D0, OK )

!
!        Confirm that we have the correct DLA descriptor by comparing
!        the ZZDSKXRI intercept to the one we find using DSKX02.
!
!        In order to use DSKX02, we need to have a vertex and ray
!        direction expressed in the segment frame. The vertex must
!        be an offset relative to the frame center.
!
         SEGFID = NINT( DSKDSC(FRMIDX) )

         IF ( SEGFID .EQ. FIXFID ) THEN

            CALL VEQU( VERTEX, SEGVTX )
            CALL VEQU( RAYDIR, SEGDIR )

         ELSE
!
!           Get the frame center offset in the body-centered frame.
!
            CALL FRINFO ( SEGFID, FRMCTR, CLASS, CLSSID, FOUND )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL SPKEZP ( FRMCTR, ET,     FIXREF, 'NONE', &
                          BODYID, OFFSET, LT             )
!
!           Translate the ray's vertex to make it relative to the
!           segment frame's center.
!
            CALL VSUB ( VERTEX, OFFSET, VTEMP )

!
!           Transform the offset vertex and the direction vector to
!           the segment frame.
!
            CALL REFCHG ( FIXFID, SEGFID, ET, XFORM )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL MXV ( XFORM, RAYDIR, SEGDIR )
            CALL MXV ( XFORM, VTEMP,  SEGVTX )

         END IF


         CALL DSKX02 ( HANDLE, DLADSC, SEGVTX, &
                       SEGDIR, PLID,   XXPT,   FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )


         IF ( SEGFID .NE. FIXFID ) THEN
!
!           We must map the intercept back to the body-centered
!           frame.
!
            CALL MTXV ( XFORM, XXPT,   VTEMP )
            CALL VADD ( VTEMP, OFFSET, XXPT  )

         END IF


         CALL CHCKSL ( 'DSKX02 FOUND', FOUND, .TRUE., OK )

         TOL = TIGHT

         CALL CHCKAD ( 'ZZDSKXRI XPT (2)', XPT, '~~/', &
                                           XXPT, 3, TOL, OK )

!
!        Check the plate ID from the integer parameter array returned
!        by ZZDSKXRI.
!
         CALL CHCKSI ( 'IC(1)', IC(1), '=', PLID, 0, OK )

      END IF



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Simple Saturn intercept case. Use layers 1 and 2.' )

!
!     Create ray.
!
      R = 1.D6

      CALL VPACK ( R, -R, 2*R, VERTEX )

      CALL VMINUS ( VERTEX, RAYDIR )

!
!     Specify the surfaces from the first Saturn layer. This layer
!     uses topocentric frames.
!
      NSURF = NTILEH * NTILEW

      DO I = 1, NSURF

         LAYSRF(I,1) = I
         LAYSRF(I,2) = I + NSURF
         LAYSRF(I,3) = I + NSURF*2

      END DO


      BODYID = 699

      ET     = 0.D0

!
!     Compute our intercept in the IAU_SATURN frame.
!
      BIX    = 2
      FIXREF = FIXRFS(BIX)
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We'll call ZZSBFXR rather than ZZDSKBUX since the former
!     calls the latter, and the former has all of the flexibility
!     we need, but has a less complex calling sequence.
!
      CALL ZZSBFXR ( BODYID, NSURF,  LAYSRF(1,1), ET, &
                     FIXFID, VERTEX, RAYDIR,      XPT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND (layer 1)', FOUND, .TRUE., OK )

      IF ( FOUND ) THEN
!
!        Get the intercept on the second layer. This layer uses
!        the body-centered frame.
!
         CALL ZZSBFXR ( BODYID, NSURF,  LAYSRF(1,2), ET, &
                        FIXFID, VERTEX, RAYDIR,      XXPT, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND (layer 2)', FOUND, .TRUE., OK )

         TOL = TIGHT

         CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TOL, OK )

      END IF


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Simple Mars intercept case. Ray''s vertex ' &
      //           'is inside layers 1 and 2 but outside layer 3.' )


      BODYID = 499

!
!     Compute the expected intercept.
!
      CALL ZZSBFXR ( BODYID, NSURF,  LAYSRF(1,2), ET, &
                     FIXFID, VERTEX, RAYDIR,      XXPT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND (layer 2)', FOUND, .TRUE., OK )

!
!     Scale down the point on layer 2 to the scale of layer 3.
!
      CALL VSCLIP ( SCALE, XXPT )

!
!     Create the scaled vertex.
!
      R = 1.D3

      CALL VPACK ( R, -R, 2*R, VERTEX )

      CALL ZZSBFXR ( BODYID, 0,      LAYSRF, ET, &
                     FIXFID, VERTEX, RAYDIR, XPT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND (small vertex)', FOUND, .TRUE., OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TOL, OK )

!
!     Restore the default vertex.
!
      R = 1.D6

      CALL VPACK ( R, -R, 2*R, VERTEX )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Simple Mars non-intercept case. Ray points ' &
      //           'away from the target. Use all layers.' )


      BODYID = 499

!
!     Point the ray away from the target.
!
      CALL ZZSBFXR ( BODYID, 0,      LAYSRF, ET, &
                     FIXFID, VERTEX, VERTEX, XPT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND (all)', FOUND, .FALSE., OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Simple Mars non-intercept case. Ray emanates ' &
      //           'from the target center. Use all layers.' )


      BODYID = 499

      CALL CLEARD ( 3, VERTEX )

!
!     Point the ray away from the target.
!
      CALL ZZSBFXR ( BODYID, 0,      LAYSRF, ET, &
                     FIXFID, VERTEX, RAYDIR, XPT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND (all)', FOUND, .FALSE., OK )

!
!     Restore the default vertex and ray direction.
!
      R = 1.D6

      CALL VPACK ( R, -R, 2*R, VERTEX )

      CALL VMINUS ( VERTEX, RAYDIR )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Simple Mars non-intercept case. Time is ' &
      //           'too early for all segments. Use all layers.' )


      BODYID = 499

!
!     The first segment starts at 30 Julian years before J2000. The
!     other segments start later.
!
      ET = -35 * JYEAR()

      CALL ZZSBFXR ( BODYID, 0,      LAYSRF, ET, &
                     FIXFID, VERTEX, RAYDIR, XPT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND (all)', FOUND, .FALSE., OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Simple Mars non-intercept case. Time is ' &
      //           'too late for all segments. Use all layers.' )


      BODYID = 499

!
!     The last segment ends at 30 Julian years after J2000. The
!     other segments end later.
!
      ET = 35 * JYEAR()

      CALL ZZSBFXR ( BODYID, 0,      LAYSRF, ET, &
                     FIXFID, VERTEX, RAYDIR, XPT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND (all)', FOUND, .FALSE., OK )

!
!     Restore ET.
!
      ET = 0.D0


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Simple Saturn non-intercept case. The selected ' &
      //           'surfaces don''t cover the potential intercept ' &
      //           'coordinates.'                                 )


      BODYID = 699

      CALL VPACK  ( 0.D0, 0.D0, -R, VERTEX )
      CALL VMINUS ( VERTEX,         RAYDIR )

      CALL ZZSBFXR ( BODYID, 1,      1,      ET, &
                     FIXFID, VERTEX, RAYDIR, XPT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND (surface ID 1)', FOUND, .FALSE., OK )


!
!     Restore the default vertex and ray direction.
!
      R = 1.D6

      CALL VPACK ( R, -R, 2*R, VERTEX )

      CALL VMINUS ( VERTEX, RAYDIR )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Mars spear case. Compare results using ' &
      //           'layers 1 and 2.'                        )


      BODYID = 499

      BIX    = 1
      FIXREF = FIXRFS(BIX)
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      NLON = 80
      NLAT = 40

      DLON = 2*PI() / NLON
      DLAT =   PI() / NLAT

      DO I = 1, NLON

         LON = (I-1)*DLON

         DO J = 0, NLAT

            LAT = HALFPI() - J*DLAT

            CALL LATREC ( R, LON, LAT, VERTEX )
            CALL VMINUS ( VERTEX,      RAYDIR )

            CALL ZZSBFXR ( BODYID, NSURF,  LAYSRF(1,1), ET, &
                           FIXFID, VERTEX, RAYDIR,      XPT, FOUND )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSL ( 'FOUND (layer 1)', FOUND, .TRUE., OK )

            CALL ZZSBFXR ( BODYID, NSURF,  LAYSRF(1,2), ET, &
                           FIXFID, VERTEX, RAYDIR,      XXPT, FOUND )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSL ( 'FOUND (layer 2)', FOUND, .TRUE., OK )

            TOL   = TIGHT

            LABEL = 'XPT I=@ J=@'
            CALL REPMI ( LABEL, '@', I, LABEL )
            CALL REPMI ( LABEL, '@', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( LABEL, XPT, '~~/', XXPT, 3, TOL, OK )

         END DO

      END DO


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Saturn spear case. Compare results using ' &
      //           'layers 1 and 2.'                        )

      BODYID = 699

      BIX    = 2
      FIXREF = FIXRFS(BIX)
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )


      NLON = 80
      NLAT = 40

      DLON = 2*PI() / NLON
      DLAT =   PI() / NLAT

      DO I = 1, NLON

         LON = (I-1)*DLON

         DO J = 0, NLAT

            LAT = HALFPI() - J*DLAT

            CALL LATREC ( R, LON, LAT, VERTEX )
            CALL VMINUS ( VERTEX,      RAYDIR )

            CALL ZZSBFXR ( BODYID, NSURF,  LAYSRF(1,1), ET, &
                           FIXFID, VERTEX, RAYDIR,      XPT, FOUND )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSL ( 'FOUND (layer 1)', FOUND, .TRUE., OK )

            CALL ZZSBFXR ( BODYID, NSURF,  LAYSRF(1,2), ET, &
                           FIXFID, VERTEX, RAYDIR,      XXPT, FOUND )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSL ( 'FOUND (layer 2)', FOUND, .TRUE., OK )

            TOL   = TIGHT

            LABEL = 'XPT I=@ J=@'
            CALL REPMI ( LABEL, '@', I, LABEL )
            CALL REPMI ( LABEL, '@', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( LABEL, XPT, '~~/', XXPT, 3, TOL, OK )

         END DO

      END DO


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Mars spear case. Compare results using ' &
      //           'layers 1 and 2. This time, load DSK kernels ' &
      //           'for the different layers in reverse order.'  )


      BODYID = 499
      BIX    = 1
      FIXREF = FIXRFS(BIX)
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )


      DO LAYRIX = 3, 1, -1

         DO W = 1, NTILEW

            DO H = 1, NTILEH

               CALL FURNSH ( TILDSK(H,W,LAYRIX,BIX) )
               CALL CHCKXC ( .FALSE., ' ', OK )

            END DO

         END DO

      END DO

      NLON = 80
      NLAT = 40

      DLON = 2*PI() / NLON
      DLAT =   PI() / NLAT

      DO I = 1, NLON

         LON = (I-1)*DLON

         DO J = 0, NLAT

            LAT = HALFPI() - J*DLAT

            CALL LATREC ( R, LON, LAT, VERTEX )
            CALL VMINUS ( VERTEX,      RAYDIR )

            CALL ZZSBFXR ( BODYID, NSURF,  LAYSRF(1,1), ET, &
                           FIXFID, VERTEX, RAYDIR,      XPT, FOUND )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSL ( 'FOUND (layer 1)', FOUND, .TRUE., OK )

            CALL ZZSBFXR ( BODYID, NSURF,  LAYSRF(1,2), ET, &
                           FIXFID, VERTEX, RAYDIR,      XXPT, FOUND )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSL ( 'FOUND (layer 2)', FOUND, .TRUE., OK )

            TOL   = TIGHT

            LABEL = 'XPT I=@ J=@'
            CALL REPMI ( LABEL, '@', I, LABEL )
            CALL REPMI ( LABEL, '@', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( LABEL, XPT, '~~/', XXPT, 3, TOL, OK )

         END DO

      END DO



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Saturn spear case. Compare results using ' &
      //           'layers 1 and 2. This time, load DSK kernels ' &
      //           'for the different layers in reverse order.'  )


      BODYID = 699
      BIX    = 2
      FIXREF = FIXRFS(BIX)
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO LAYRIX = 3, 1, -1

         DO W = 1, NTILEW

            DO H = 1, NTILEH

               CALL FURNSH ( TILDSK(H,W,LAYRIX,BIX) )
               CALL CHCKXC ( .FALSE., ' ', OK )

            END DO

         END DO

      END DO

      NLON = 80
      NLAT = 40

      DLON = 2*PI() / NLON
      DLAT =   PI() / NLAT

      DO I = 1, NLON

         LON = (I-1)*DLON

         DO J = 0, NLAT

            LAT = HALFPI() - J*DLAT

            CALL LATREC ( R, LON, LAT, VERTEX )
            CALL VMINUS ( VERTEX,      RAYDIR )

            CALL ZZSBFXR ( BODYID, NSURF,  LAYSRF(1,1), ET, &
                           FIXFID, VERTEX, RAYDIR,      XPT, FOUND )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSL ( 'FOUND (layer 1)', FOUND, .TRUE., OK )

            CALL ZZSBFXR ( BODYID, NSURF,  LAYSRF(1,2), ET, &
                           FIXFID, VERTEX, RAYDIR,      XXPT, FOUND )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSL ( 'FOUND (layer 2)', FOUND, .TRUE., OK )

             TOL   = TIGHT

             LABEL = 'XPT I=@ J=@'
             CALL REPMI ( LABEL, '@', I, LABEL )
             CALL REPMI ( LABEL, '@', J, LABEL )
             CALL CHCKXC ( .FALSE., ' ', OK )

             CALL CHCKAD ( LABEL, XPT, '~~/', XXPT, 3, TOL, OK )

         END DO

      END DO

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Mars spear case. Compare results using ' &
      //           'layer 1 against results obtained using ' &
      //           'all layers.'                            )



      BODYID = 499
      BIX    = 1
      FIXREF = FIXRFS(BIX)
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      NSURF = NTILEW * NTILEH

!
!     Reduce the number of intercepts for this one.
!
      NLON = 40
      NLAT = 20

      DLON = 2*PI() / NLON
      DLAT =   PI() / NLAT

      DO I = 1, NLON

         LON = (I-1)*DLON

         DO J = 0, NLAT

            LAT = HALFPI() - J*DLAT

            CALL LATREC ( R, LON, LAT, VERTEX )
            CALL VMINUS ( VERTEX,      RAYDIR )

            CALL ZZSBFXR ( BODYID, NSURF,  LAYSRF(1,1), ET, &
                           FIXFID, VERTEX, RAYDIR,      XPT, FOUND )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSL ( 'FOUND (layer 1)', FOUND, .TRUE., OK )

            CALL ZZSBFXR ( BODYID, 0,      LAYSRF,      ET, &
                           FIXFID, VERTEX, RAYDIR,      XXPT, FOUND )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSL ( 'FOUND (all layers)', FOUND, .TRUE., OK )

             TOL   = TIGHT

             LABEL = 'XPT I=@ J=@'
             CALL REPMI ( LABEL, '@', I, LABEL )
             CALL REPMI ( LABEL, '@', J, LABEL )
             CALL CHCKXC ( .FALSE., ' ', OK )

             CALL CHCKAD ( LABEL, XPT, '~~/', XXPT, 3, TOL, OK )

         END DO

      END DO


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Saturn spear case. Compare results using ' &
      //           'layer 1 against results obtained using ' &
      //           'all layers.'                            )



      BODYID = 699
      BIX    = 1
      FIXREF = FIXRFS(BIX)
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      NSURF = NTILEW * NTILEH

!
!     Reduce the number of intercepts for this one.
!
      NLON = 40
      NLAT = 20

      DLON = 2*PI() / NLON
      DLAT =   PI() / NLAT

      DO I = 1, NLON

         LON = (I-1)*DLON

         DO J = 0, NLAT

            LAT = HALFPI() - J*DLAT

            CALL LATREC ( R, LON, LAT, VERTEX )
            CALL VMINUS ( VERTEX,      RAYDIR )

            CALL ZZSBFXR ( BODYID, NSURF,  LAYSRF(1,1), ET, &
                           FIXFID, VERTEX, RAYDIR,      XPT, FOUND )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSL ( 'FOUND (layer 1)', FOUND, .TRUE., OK )

            CALL ZZSBFXR ( BODYID, 0,      LAYSRF,      ET, &
                           FIXFID, VERTEX, RAYDIR,      XXPT, FOUND )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSL ( 'FOUND (all layers)', FOUND, .TRUE., OK )

             TOL   = TIGHT

             LABEL = 'XPT I=@ J=@'
             CALL REPMI ( LABEL, '@', I, LABEL )
             CALL REPMI ( LABEL, '@', J, LABEL )
             CALL CHCKXC ( .FALSE., ' ', OK )

             CALL CHCKAD ( LABEL, XPT, '~~/', XXPT, 3, TOL, OK )

         END DO

      END DO


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Mars greedy spear case. Deliberately hit ' &
      //           'segment boundaries on meridians and ' &
      //           'parallels.'                               )


      BODYID = 499

      BIX    = 1
      FIXREF = FIXRFS(BIX)
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      NLON = NTILEW * 2
      NLAT = NTILEH * 2

      DLON = 2*PI() / NLON
      DLAT =   PI() / NLAT

      DO I = 1, NLON

         LON = (I-1)*DLON

         DO J = 0, NLAT

            LAT = HALFPI() - J*DLAT

            CALL LATREC ( R, LON, LAT, VERTEX )
            CALL VMINUS ( VERTEX,      RAYDIR )

            CALL ZZSBFXR ( BODYID, NSURF,  LAYSRF(1,1), ET, &
                           FIXFID, VERTEX, RAYDIR,      XPT, FOUND )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSL ( 'FOUND (layer 1)', FOUND, .TRUE., OK )

            CALL ZZSBFXR ( BODYID, NSURF,  LAYSRF(1,2), ET, &
                           FIXFID, VERTEX, RAYDIR,      XXPT, FOUND )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSL ( 'FOUND (layer 2)', FOUND, .TRUE., OK )

            TOL   = TIGHT

            LABEL = 'XPT I=@ J=@'
            CALL REPMI ( LABEL, '@', I, LABEL )
            CALL REPMI ( LABEL, '@', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( LABEL, XPT, '~~/', XXPT, 3, TOL, OK )

         END DO

      END DO

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Saturn greedy spear case. Deliberately hit ' &
      //           'segment boundaries on meridians and ' &
      //           'parallels.'                               )


      BODYID = 699

      BIX    = 2
      FIXREF = FIXRFS(BIX)
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      NLON = NTILEW * 2
      NLAT = NTILEH * 2

      DLON = 2*PI() / NLON
      DLAT =   PI() / NLAT

      DO I = 1, NLON

         LON = (I-1)*DLON

         DO J = 0, NLAT

            LAT = HALFPI() - J*DLAT

            CALL LATREC ( R, LON, LAT, VERTEX )
            CALL VMINUS ( VERTEX,      RAYDIR )

            CALL ZZSBFXR ( BODYID, NSURF,  LAYSRF(1,1), ET, &
                           FIXFID, VERTEX, RAYDIR,      XPT, FOUND )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSL ( 'FOUND (layer 1)', FOUND, .TRUE., OK )

            CALL ZZSBFXR ( BODYID, NSURF,  LAYSRF(1,2), ET, &
                           FIXFID, VERTEX, RAYDIR,      XXPT, FOUND )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSL ( 'FOUND (layer 2)', FOUND, .TRUE., OK )

            TOL   = TIGHT

            LABEL = 'XPT I=@ J=@'
            CALL REPMI ( LABEL, '@', I, LABEL )
            CALL REPMI ( LABEL, '@', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( LABEL, XPT, '~~/', XXPT, 3, TOL, OK )

         END DO

      END DO



!***********************************************************************
!
!     ZZDSKBUX Error cases
!
!***********************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Simple Saturn non-intercept case. No DSK ' &
      //           'data are available for the specified surfaces. ' &
      //           'Use all layers ' &
      //           'for Mars and Saturn. This is an error case.' )


      BODYID = 699

      CALL ZZSBFXR ( BODYID, 1,      -1,     ET, &
                     FIXFID, VERTEX, RAYDIR, XPT, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(DSKDATANOTFOUND)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Simple Venus non-intercept case. No DSK ' &
      //           'data are available for Venus. Use all layers ' &
      //           'for Mars and Saturn. This is an error case.' )


      BODYID = 299

      CALL ZZSBFXR ( BODYID, 0,      LAYSRF, ET, &
                     FIXFID, VERTEX, RAYDIR, XPT, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(NODSKSEGMENTS)', OK )



!***********************************************************************
!
!
!
!     ZZDSKBUN, ZZSBFNRM tests
!
!
!
!***********************************************************************

!***********************************************************************
!
!     ZZDSKBUN, ZZSBFNRM Normal cases
!
!***********************************************************************

!
!     We'll call ZZSBFNRM rather than ZZDSKBUN since the former
!     calls the latter, and the former has all of the flexibility
!     we need, but has a less complex calling sequence.

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Mars case: find outward normals on every plate ' &
      //           'of layer 1. Compare to normals on layer 2. ' &
      //           'Choose points so as to avoid plate edges.'     )


!
!     Re-load DSKS in their original order.
!

      DO TARGIX = 1, NTARG

         DO LAYRIX = 1, NLAYER

            DO W = 1, NTILEW

               DO H = 1, NTILEH

                  CALL FURNSH(  TILDSK( H, W, LAYRIX, TARGIX ) )
                  CALL CHCKXC ( .FALSE., ' ', OK )

               END DO

            END DO

         END DO

      END DO


      BODYID = 499

      BIX    = 1
      FIXREF = FIXRFS(BIX)
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      NLON = NTILEW * 2
      NLAT = NTILEH * 2

      DLON = 2*PI() / NLON
      DLAT =   PI() / NLAT

      R    = 1.D6

      ET   = 0.D0

      DO I = 1, NLON

         LON = ( I - 1 + (1.D0/3) )*DLON

         DO J = 1, NLAT

            LAT = HALFPI() - ((J-0.5D0)*DLAT)

            CALL LATREC ( R, LON, LAT, VERTEX )
            CALL VMINUS ( VERTEX,      RAYDIR )
!
!           Generate a surface point on the second layer. This
!           point should be very close to the first layer as well.
!
            CALL ZZSBFXRI ( BODYID, NSURF,  LAYSRF(1,2), ET, &
                            FIXFID, VERTEX, RAYDIR,      XPT, HANDLE, &
                            DLADSC, DSKDSC, DC,          IC,  FOUND  )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSL ( 'FOUND (layer 2)', FOUND, .TRUE., OK )

!
!           Find the outward normal at XPT, using the first layer.
!
            CALL ZZSBFNRM ( BODYID, NSURF, LAYSRF(1,1), ET, &
                            FIXFID, XPT,   NORMAL          )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Find the outward normal at XPT using the second layer.
!
            CALL ZZSBFNRM ( BODYID, NSURF, LAYSRF(1,2), ET, &
                            FIXFID, XPT,   XNORML           )
            CALL CHCKXC ( .FALSE., ' ', OK )


            TOL   = TIGHT

            LABEL = 'layer 1 NORMAL I=@ J=@'
            CALL REPMI ( LABEL, '@', I, LABEL )
            CALL REPMI ( LABEL, '@', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( LABEL, NORMAL, '~~/', XNORML, 3, TOL, OK )

!
!           Fetch the plate normal directly from the segment found
!           by ZZDSKXRI.
!
            CALL DSKN02 ( HANDLE, DLADSC, IC(1), NORMAL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            TOL   = TIGHT

            LABEL = 'DSKN02 NORMAL I=@ J=@'
            CALL REPMI ( LABEL, '@', I, LABEL )
            CALL REPMI ( LABEL, '@', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( LABEL, NORMAL, '~~/', XNORML, 3, TOL, OK )

         END DO

      END DO

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Saturn case: find outward normals on every plate ' &
      //           'of layer 1. Compare to normals on layer 2. ' &
      //           'Choose points so as to avoid plate edges.'     )


      BODYID = 699

      BIX    = 2
      FIXREF = FIXRFS(BIX)
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      NLON = NTILEW * 2
      NLAT = NTILEH * 2

      DLON = 2*PI() / NLON
      DLAT =   PI() / NLAT

      R    = 1.D6

      ET   = 0.D0

      DO I = 1, NLON

         LON = ( I - 1 + (1.D0/3) )*DLON

         DO J = 1, NLAT

            LAT = HALFPI() - ((J-0.5D0)*DLAT)

            CALL LATREC ( R, LON, LAT, VERTEX )
            CALL VMINUS ( VERTEX,      RAYDIR )
!
!           Generate a surface point on the second layer. This
!           point should be very close to the first layer as well.
!
            CALL ZZSBFXRI ( BODYID, NSURF,  LAYSRF(1,2), ET, &
                            FIXFID, VERTEX, RAYDIR,      XPT, HANDLE, &
                            DLADSC, DSKDSC, DC,          IC,  FOUND  )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSL ( 'FOUND (layer 2)', FOUND, .TRUE., OK )

!
!           Find the outward normal at XPT, using the first layer.
!
            CALL ZZSBFNRM ( BODYID, NSURF, LAYSRF(1,1), ET, &
                            FIXFID, XPT,   NORMAL          )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Find the outward normal at XPT using the second layer.
!
            CALL ZZSBFNRM ( BODYID, NSURF, LAYSRF(1,2), ET, &
                            FIXFID, XPT,   XNORML           )
            CALL CHCKXC ( .FALSE., ' ', OK )


            TOL   = TIGHT

            LABEL = 'layer 1 NORMAL I=@ J=@'
            CALL REPMI ( LABEL, '@', I, LABEL )
            CALL REPMI ( LABEL, '@', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( LABEL, NORMAL, '~~/', XNORML, 3, TOL, OK )

!
!           Fetch the plate normal directly from the segment found
!           by ZZDSKXRI.
!
            CALL DSKN02 ( HANDLE, DLADSC, IC(1), NORMAL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            TOL   = TIGHT

            LABEL = 'DSKN02 NORMAL I=@ J=@'
            CALL REPMI ( LABEL, '@', I, LABEL )
            CALL REPMI ( LABEL, '@', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( LABEL, NORMAL, '~~/', XNORML, 3, TOL, OK )

         END DO

      END DO



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Mars case: find outward normals on every plate, ' &
      //           'using all layers. Compare to normals on layer 2. ' &
      //           'Choose points so as to avoid plate edges.'     )


      BODYID = 499

      BIX    = 1
      FIXREF = FIXRFS(BIX)
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      NLON = NTILEW * 2
      NLAT = NTILEH * 2

      DLON = 2*PI() / NLON
      DLAT =   PI() / NLAT

      R    = 1.D6

      ET   = 0.D0

      DO I = 1, NLON

         LON = ( I - 1 + (1.D0/3) )*DLON

         DO J = 1, NLAT

            LAT = HALFPI() - ((J-0.5D0)*DLAT)

            CALL LATREC ( R, LON, LAT, VERTEX )
            CALL VMINUS ( VERTEX,      RAYDIR )
!
!           Generate a surface point on the second layer. This
!           point should be very close to the first layer as well.
!
            CALL ZZSBFXRI ( BODYID, NSURF,  LAYSRF(1,2), ET, &
                            FIXFID, VERTEX, RAYDIR,      XPT, HANDLE, &
                            DLADSC, DSKDSC, DC,          IC,  FOUND  )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSL ( 'FOUND (layer 2)', FOUND, .TRUE., OK )

!
!           Find the outward normal at XPT, using all layers.
!
            CALL ZZSBFNRM ( BODYID, 0,     LAYSRF, ET, &
                            FIXFID, XPT,   NORMAL          )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Find the outward normal at XPT using the second layer.
!
            CALL ZZSBFNRM ( BODYID, NSURF, LAYSRF(1,2), ET, &
                            FIXFID, XPT,   XNORML           )
            CALL CHCKXC ( .FALSE., ' ', OK )


            TOL   = TIGHT

            LABEL = 'layer 1 NORMAL I=@ J=@'
            CALL REPMI ( LABEL, '@', I, LABEL )
            CALL REPMI ( LABEL, '@', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( LABEL, NORMAL, '~~/', XNORML, 3, TOL, OK )

!
!           Fetch the plate normal directly from the segment found
!           by ZZDSKXRI.
!
            CALL DSKN02 ( HANDLE, DLADSC, IC(1), NORMAL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            TOL   = TIGHT

            LABEL = 'DSKN02 NORMAL I=@ J=@'
            CALL REPMI ( LABEL, '@', I, LABEL )
            CALL REPMI ( LABEL, '@', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( LABEL, NORMAL, '~~/', XNORML, 3, TOL, OK )

         END DO

      END DO

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Saturn case: find outward normals on every plate, ' &
      //           'using all layers. Compare to normals on layer 2. ' &
      //           'Choose points so as to avoid plate edges.'     )


      BODYID = 699

      BIX    = 2
      FIXREF = FIXRFS(BIX)
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      NLON = NTILEW * 2
      NLAT = NTILEH * 2

      DLON = 2*PI() / NLON
      DLAT =   PI() / NLAT

      R    = 1.D6

      ET   = 0.D0

      DO I = 1, NLON

         LON = ( I - 1 + (1.D0/3) )*DLON

         DO J = 1, NLAT

            LAT = HALFPI() - ((J-0.5D0)*DLAT)

            CALL LATREC ( R, LON, LAT, VERTEX )
            CALL VMINUS ( VERTEX,      RAYDIR )
!
!           Generate a surface point on the second layer. This
!           point should be very close to the first layer as well.
!
            CALL ZZSBFXRI ( BODYID, NSURF,  LAYSRF(1,2), ET, &
                            FIXFID, VERTEX, RAYDIR,      XPT, HANDLE, &
                            DLADSC, DSKDSC, DC,          IC,  FOUND  )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSL ( 'FOUND (layer 2)', FOUND, .TRUE., OK )

!
!           Find the outward normal at XPT, using all layers.
!
            CALL ZZSBFNRM ( BODYID, 0,     LAYSRF, ET, &
                            FIXFID, XPT,   NORMAL          )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Find the outward normal at XPT using the second layer.
!
            CALL ZZSBFNRM ( BODYID, NSURF, LAYSRF(1,2), ET, &
                            FIXFID, XPT,   XNORML           )
            CALL CHCKXC ( .FALSE., ' ', OK )


            TOL   = TIGHT

            LABEL = 'layer 1 NORMAL I=@ J=@'
            CALL REPMI ( LABEL, '@', I, LABEL )
            CALL REPMI ( LABEL, '@', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( LABEL, NORMAL, '~~/', XNORML, 3, TOL, OK )

!
!           Fetch the plate normal directly from the segment found
!           by ZZDSKXRI.
!
            CALL DSKN02 ( HANDLE, DLADSC, IC(1), NORMAL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            TOL   = TIGHT

            LABEL = 'DSKN02 NORMAL I=@ J=@'
            CALL REPMI ( LABEL, '@', I, LABEL )
            CALL REPMI ( LABEL, '@', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( LABEL, NORMAL, '~~/', XNORML, 3, TOL, OK )

         END DO

      END DO


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Mars case: find outward normals on every plate, ' &
      //           'using all layers. Compare to normals on layer 2. ' &
      //           'Choose points so as to avoid plate edges. Load ' &
      //           'DSKs so as to reverse the order of the layers.'  )

!
!     Re-load DSKs, reversing the order of the layers.
!
      DO TARGIX = 1, NTARG

         DO LAYRIX = NLAYER, 1, -1

            DO W = 1, NTILEW

               DO H = 1, NTILEH

                  CALL FURNSH(  TILDSK( H, W, LAYRIX, TARGIX ) )
                  CALL CHCKXC ( .FALSE., ' ', OK )

               END DO

            END DO

         END DO

      END DO


      BODYID = 499

      BIX    = 1
      FIXREF = FIXRFS(BIX)
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      NLON = NTILEW * 2
      NLAT = NTILEH * 2

      DLON = 2*PI() / NLON
      DLAT =   PI() / NLAT

      R    = 1.D6

      ET   = 0.D0

      DO I = 1, NLON

         LON = ( I - 1 + (1.D0/3) )*DLON

         DO J = 1, NLAT

            LAT = HALFPI() - ((J-0.5D0)*DLAT)

            CALL LATREC ( R, LON, LAT, VERTEX )
            CALL VMINUS ( VERTEX,      RAYDIR )
!
!           Generate a surface point on the second layer. This
!           point should be very close to the first layer as well.
!
            CALL ZZSBFXRI ( BODYID, NSURF,  LAYSRF(1,2), ET, &
                            FIXFID, VERTEX, RAYDIR,      XPT, HANDLE, &
                            DLADSC, DSKDSC, DC,          IC,  FOUND  )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSL ( 'FOUND (layer 2)', FOUND, .TRUE., OK )

!
!           Reload the last DSK loaded. This looks strange, but we
!           need to do this here to exercise the BSR update logic
!           in ZZDSKNRM. Otherwise, the buffer updates will be
!           handled in ZZDSKXRI before ZZDSKSNRM gets a chance to
!           respond.
!
            CALL FURNSH(  TILDSK( NTILEH, NTILEW, 1, NTARG ) )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Find the outward normal at XPT, using all layers.
!
            CALL ZZSBFNRM ( BODYID, 0,     LAYSRF, ET, &
                            FIXFID, XPT,   NORMAL          )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Find the outward normal at XPT using the second layer.
!
            CALL ZZSBFNRM ( BODYID, NSURF, LAYSRF(1,2), ET, &
                            FIXFID, XPT,   XNORML           )
            CALL CHCKXC ( .FALSE., ' ', OK )


            TOL   = TIGHT

            LABEL = 'layer 1 NORMAL I=@ J=@'
            CALL REPMI ( LABEL, '@', I, LABEL )
            CALL REPMI ( LABEL, '@', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( LABEL, NORMAL, '~~/', XNORML, 3, TOL, OK )

!
!           Fetch the plate normal directly from the segment found
!           by ZZDSKXRI.
!
            CALL DSKN02 ( HANDLE, DLADSC, IC(1), NORMAL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            TOL   = TIGHT

            LABEL = 'DSKN02 NORMAL I=@ J=@'
            CALL REPMI ( LABEL, '@', I, LABEL )
            CALL REPMI ( LABEL, '@', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( LABEL, NORMAL, '~~/', XNORML, 3, TOL, OK )

         END DO

      END DO




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Saturn case: find outward normals on every plate, ' &
      //           'using all layers. Compare to normals on layer 2. ' &
      //           'Choose points so as to avoid plate edges. DSKs ' &
      //           'have been reloaded so as to reverse the order ' &
      //           'of the layers.'  )


!
!     The reload was performed in the previous case.
!
      BODYID = 699

      BIX    = 2
      FIXREF = FIXRFS(BIX)
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      NLON = NTILEW * 2
      NLAT = NTILEH * 2

      DLON = 2*PI() / NLON
      DLAT =   PI() / NLAT

      R    = 1.D6

      ET   = 0.D0

      DO I = 1, NLON

         LON = ( I - 1 + (1.D0/3) )*DLON

         DO J = 1, NLAT

            LAT = HALFPI() - ((J-0.5D0)*DLAT)

            CALL LATREC ( R, LON, LAT, VERTEX )
            CALL VMINUS ( VERTEX,      RAYDIR )
!
!           Generate a surface point on the second layer. This
!           point should be very close to the first layer as well.
!
            CALL ZZSBFXRI ( BODYID, NSURF,  LAYSRF(1,2), ET, &
                            FIXFID, VERTEX, RAYDIR,      XPT, HANDLE, &
                            DLADSC, DSKDSC, DC,          IC,  FOUND  )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSL ( 'FOUND (layer 2)', FOUND, .TRUE., OK )

!
!           Reload the last DSK loaded. This looks strange, but we
!           need to do this here to exercise the BSR update logic
!           in ZZDSKNRM. Otherwise, the buffer updates will be
!           handled in ZZDSKXRI before ZZDSKSNRM gets a chance to
!           respond.
!
            CALL FURNSH(  TILDSK( NTILEH, NTILEW, 1, NTARG ) )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Find the outward normal at XPT, using all layers.
!
            CALL ZZSBFNRM ( BODYID, 0,     LAYSRF, ET, &
                            FIXFID, XPT,   NORMAL          )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Find the outward normal at XPT using the second layer.
!
            CALL ZZSBFNRM ( BODYID, NSURF, LAYSRF(1,2), ET, &
                            FIXFID, XPT,   XNORML           )
            CALL CHCKXC ( .FALSE., ' ', OK )


            TOL   = TIGHT

            LABEL = 'layer 1 NORMAL I=@ J=@'
            CALL REPMI ( LABEL, '@', I, LABEL )
            CALL REPMI ( LABEL, '@', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( LABEL, NORMAL, '~~/', XNORML, 3, TOL, OK )

!
!           Fetch the plate normal directly from the segment found
!           by ZZDSKXRI.
!
            CALL DSKN02 ( HANDLE, DLADSC, IC(1), NORMAL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            TOL   = TIGHT

            LABEL = 'DSKN02 NORMAL I=@ J=@'
            CALL REPMI ( LABEL, '@', I, LABEL )
            CALL REPMI ( LABEL, '@', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( LABEL, NORMAL, '~~/', XNORML, 3, TOL, OK )

         END DO

      END DO


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Mars case: find outward normals on every plate ' &
      //           'of layer 1. Compare to normals on layer 3. ' &
      //           'Choose points so as to avoid plate edges.'     )



      BODYID = 499

      BIX    = 1
      FIXREF = FIXRFS(BIX)
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      NLON = NTILEW * 2
      NLAT = NTILEH * 2

      DLON = 2*PI() / NLON
      DLAT =   PI() / NLAT

      R    = 1.D6

      ET   = 0.D0

      DO I = 1, NLON

         LON = ( I - 1 + (1.D0/3) )*DLON

         DO J = 1, NLAT

            LAT = HALFPI() - ((J-0.5D0)*DLAT)

            CALL LATREC ( R, LON, LAT, VERTEX )
            CALL VMINUS ( VERTEX,      RAYDIR )
!
!           Generate a surface point on the second layer. This
!           point should be very close to the first layer as well.
!
            CALL ZZSBFXRI ( BODYID, NSURF,  LAYSRF(1,2), ET, &
                            FIXFID, VERTEX, RAYDIR,      XPT, HANDLE, &
                            DLADSC, DSKDSC, DC,          IC,  FOUND  )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSL ( 'FOUND (layer 2)', FOUND, .TRUE., OK )

!
!           Find the outward normal at XPT, using the first layer.
!
            CALL ZZSBFNRM ( BODYID, NSURF, LAYSRF(1,1), ET, &
                            FIXFID, XPT,   NORMAL          )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Find the outward normal at the lon/lat of XPT
!           using the third layer.
!
!           We need to use a scaled version of XPT for this layer.
!
            CALL VSCL ( SCALE, XPT, VTEMP )

            CALL ZZSBFNRM ( BODYID, NSURF, LAYSRF(1,3), ET, &
                            FIXFID, VTEMP, XNORML           )
            CALL CHCKXC ( .FALSE., ' ', OK )


            TOL   = TIGHT

            LABEL = 'layer 3 NORMAL I=@ J=@'
            CALL REPMI ( LABEL, '@', I, LABEL )
            CALL REPMI ( LABEL, '@', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( LABEL, NORMAL, '~~/', XNORML, 3, TOL, OK )

!
!           Fetch the plate normal directly from the segment found
!           by ZZDSKXRI.
!
            CALL DSKN02 ( HANDLE, DLADSC, IC(1), NORMAL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            TOL   = TIGHT

            LABEL = 'DSKN02 NORMAL I=@ J=@'
            CALL REPMI ( LABEL, '@', I, LABEL )
            CALL REPMI ( LABEL, '@', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( LABEL, NORMAL, '~~/', XNORML, 3, TOL, OK )

         END DO

      END DO


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Saturn case: find outward normals on every plate ' &
      //           'of layer 1. Compare to normals on layer 3. ' &
      //           'Choose points so as to avoid plate edges.'     )



      BODYID = 699

      BIX    = 2
      FIXREF = FIXRFS(BIX)
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      NLON = NTILEW * 2
      NLAT = NTILEH * 2

      DLON = 2*PI() / NLON
      DLAT =   PI() / NLAT

      R    = 1.D6

      ET   = 0.D0

      DO I = 1, NLON

         LON = ( I - 1 + (1.D0/3) )*DLON

         DO J = 1, NLAT

            LAT = HALFPI() - ((J-0.5D0)*DLAT)

            CALL LATREC ( R, LON, LAT, VERTEX )
            CALL VMINUS ( VERTEX,      RAYDIR )
!
!           Generate a surface point on the second layer. This
!           point should be very close to the first layer as well.
!
            CALL ZZSBFXRI ( BODYID, NSURF,  LAYSRF(1,2), ET, &
                            FIXFID, VERTEX, RAYDIR,      XPT, HANDLE, &
                            DLADSC, DSKDSC, DC,          IC,  FOUND  )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSL ( 'FOUND (layer 2)', FOUND, .TRUE., OK )

!
!           Find the outward normal at XPT, using the first layer.
!
            CALL ZZSBFNRM ( BODYID, NSURF, LAYSRF(1,1), ET, &
                            FIXFID, XPT,   NORMAL          )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Find the outward normal at the lon/lat of XPT
!           using the third layer.
!
!           We need to use a scaled version of XPT for this layer.
!
            CALL VSCL ( SCALE, XPT, VTEMP )

            CALL ZZSBFNRM ( BODYID, NSURF, LAYSRF(1,3), ET, &
                            FIXFID, VTEMP, XNORML           )
            CALL CHCKXC ( .FALSE., ' ', OK )


            TOL   = TIGHT

            LABEL = 'layer 3 NORMAL I=@ J=@'
            CALL REPMI ( LABEL, '@', I, LABEL )
            CALL REPMI ( LABEL, '@', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( LABEL, NORMAL, '~~/', XNORML, 3, TOL, OK )

!
!           Fetch the plate normal directly from the segment found
!           by ZZDSKXRI.
!
            CALL DSKN02 ( HANDLE, DLADSC, IC(1), NORMAL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            TOL   = TIGHT

            LABEL = 'DSKN02 NORMAL I=@ J=@'
            CALL REPMI ( LABEL, '@', I, LABEL )
            CALL REPMI ( LABEL, '@', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( LABEL, NORMAL, '~~/', XNORML, 3, TOL, OK )

         END DO

      END DO


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Mars case: find outward normals on every plate ' &
      //           'of layer 1. Compare to normals on layer 3. ' &
      //           'Choose points so as to avoid plate edges. ' &
      //           'Select layers by time rather than surface ID.'  )



      BODYID = 499

      BIX    = 1
      FIXREF = FIXRFS(BIX)
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      NLON = NTILEW * 2
      NLAT = NTILEH * 2

      DLON = 2*PI() / NLON
      DLAT =   PI() / NLAT

      R    = 1.D6

      ET   = 0.D0

      DO I = 1, NLON

         LON = ( I - 1 + (1.D0/3) )*DLON

         DO J = 1, NLAT

            LAT = HALFPI() - ((J-0.5D0)*DLAT)

            CALL LATREC ( R, LON, LAT, VERTEX )
            CALL VMINUS ( VERTEX,      RAYDIR )
!
!           Generate a surface point on the second layer. This
!           point should be very close to the first layer as well.
!
!           Set ET for this layer.
!
            ET = 0.D0

            CALL ZZSBFXRI ( BODYID, NSURF,  LAYSRF(1,2), ET, &
                            FIXFID, VERTEX, RAYDIR,      XPT, HANDLE, &
                            DLADSC, DSKDSC, DC,          IC,  FOUND  )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSL ( 'FOUND (layer 2)', FOUND, .TRUE., OK )

!
!           Find the outward normal at XPT, using the first layer. Set
!           ET to a value covered only by the first layer.
!
            ET = -25 * JYEAR()

            CALL ZZSBFNRM ( BODYID, 0,   LAYSRF, ET, &
                            FIXFID, XPT, NORMAL    )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Find the outward normal at the lon/lat of XPT
!           using the third layer.
!
!           We need to use a scaled version of XPT for this layer.
!
            CALL VSCL ( SCALE, XPT, VTEMP )

            ET =  25 * JYEAR()

            CALL ZZSBFNRM ( BODYID, 0,     LAYSRF, ET, &
                            FIXFID, VTEMP, XNORML       )
            CALL CHCKXC ( .FALSE., ' ', OK )


            TOL   = TIGHT

            LABEL = 'layer 3 NORMAL I=@ J=@'
            CALL REPMI ( LABEL, '@', I, LABEL )
            CALL REPMI ( LABEL, '@', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( LABEL, NORMAL, '~~/', XNORML, 3, TOL, OK )

!
!           Fetch the plate normal directly from the segment found
!           by ZZDSKXRI.
!
            CALL DSKN02 ( HANDLE, DLADSC, IC(1), NORMAL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            TOL   = TIGHT

            LABEL = 'DSKN02 NORMAL I=@ J=@'
            CALL REPMI ( LABEL, '@', I, LABEL )
            CALL REPMI ( LABEL, '@', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( LABEL, NORMAL, '~~/', XNORML, 3, TOL, OK )

         END DO

      END DO




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Saturn case: find outward normals on every plate ' &
      //           'of layer 1. Compare to normals on layer 3. ' &
      //           'Choose points so as to avoid plate edges. ' &
      //           'Select layers by time rather than surface ID.'  )



      BODYID = 699

      BIX    = 2
      FIXREF = FIXRFS(BIX)
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      NLON = NTILEW * 2
      NLAT = NTILEH * 2

      DLON = 2*PI() / NLON
      DLAT =   PI() / NLAT

      R    = 1.D6

      ET   = 0.D0

      DO I = 1, NLON

         LON = ( I - 1 + (1.D0/3) )*DLON

         DO J = 1, NLAT

            LAT = HALFPI() - ((J-0.5D0)*DLAT)

            CALL LATREC ( R, LON, LAT, VERTEX )
            CALL VMINUS ( VERTEX,      RAYDIR )
!
!           Generate a surface point on the second layer. This
!           point should be very close to the first layer as well.
!
!           Set ET for this layer.
!
            ET = 0.D0

            CALL ZZSBFXRI ( BODYID, NSURF,  LAYSRF(1,2), ET, &
                            FIXFID, VERTEX, RAYDIR,      XPT, HANDLE, &
                            DLADSC, DSKDSC, DC,          IC,  FOUND  )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSL ( 'FOUND (layer 2)', FOUND, .TRUE., OK )

!
!           Find the outward normal at XPT, using the first layer. Set
!           ET to a value covered only by the first layer.
!
            ET = -25 * JYEAR()

            CALL ZZSBFNRM ( BODYID, 0,   LAYSRF, ET, &
                            FIXFID, XPT, NORMAL    )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Find the outward normal at the lon/lat of XPT
!           using the third layer.
!
!           We need to use a scaled version of XPT for this layer.
!
            CALL VSCL ( SCALE, XPT, VTEMP )

            ET =  25 * JYEAR()

            CALL ZZSBFNRM ( BODYID, 0,     LAYSRF, ET, &
                            FIXFID, VTEMP, XNORML       )
            CALL CHCKXC ( .FALSE., ' ', OK )


            TOL   = TIGHT

            LABEL = 'layer 3 NORMAL I=@ J=@'
            CALL REPMI ( LABEL, '@', I, LABEL )
            CALL REPMI ( LABEL, '@', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( LABEL, NORMAL, '~~/', XNORML, 3, TOL, OK )

!
!           Fetch the plate normal directly from the segment found
!           by ZZDSKXRI.
!
            CALL DSKN02 ( HANDLE, DLADSC, IC(1), NORMAL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            TOL   = TIGHT

            LABEL = 'DSKN02 NORMAL I=@ J=@'
            CALL REPMI ( LABEL, '@', I, LABEL )
            CALL REPMI ( LABEL, '@', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( LABEL, NORMAL, '~~/', XNORML, 3, TOL, OK )

         END DO

      END DO



!***********************************************************************
!
!     ZZDSKBUN Error cases
!
!***********************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Mars error case: non-existent surface. ' &
      //           'Use all layers for Mars and Saturn.'    )


      ET = 0.D0

!
!     Generate a legitimate surface point.
!
      BODYID = 499

      BIX    = 1
      FIXREF = FIXRFS(BIX)
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )


      R      = 1.D6
      CALL VPACK ( R, -R, 2*R, VERTEX )
      CALL VMINUS( VERTEX,     RAYDIR )

      CALL ZZSBFXRI ( BODYID, 0,  LAYSRF,     ET, &
                      FIXFID, VERTEX, RAYDIR, XPT, HANDLE, &
                      DLADSC, DSKDSC, DC,     IC,  FOUND  )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL ZZSBFNRM ( BODYID, 1,   -1,     ET, &
                      FIXFID, XPT, XNORML     )
      CALL CHCKXC ( .TRUE., 'SPICE(POINTNOTINSEGMENT)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Mars error case: point outside all segments.' )

      R = 1.D6

      CALL VPACK ( R, R, R, VTEMP )

      CALL ZZSBFNRM ( BODYID, 0,     LAYSRF,  ET, &
                      FIXFID, VTEMP, XNORML       )
      CALL CHCKXC ( .TRUE., 'SPICE(POINTNOTINSEGMENT)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Mars error case: point inside segment, but ' &
      //           'too far from any plate.'                    )


!
!     Start by generating a legitimate point.
!
      BODYID = 499
      BIX    = 1
      FIXREF = FIXRFS(BIX)
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      R      = 1.D6

      ET     = 0.D0

      CALL VPACK ( R, -R, 2*R, VERTEX )
      CALL VMINUS( VERTEX,     RAYDIR )

      CALL ZZSBFXRI ( BODYID, 0,  LAYSRF,     ET, &
                      FIXFID, VERTEX, RAYDIR, XPT, HANDLE, &
                      DLADSC, DSKDSC, DC,     IC,  FOUND  )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!     Get the normal at this point.
!
      CALL ZZSBFNRM ( BODYID, 0,   LAYSRF,  ET, &
                      FIXFID, XPT, NORMAL      )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Add to the point a small multiple of the normal.
!
      CALL VLCOM ( 1.D0, XPT, 1.D-3, NORMAL, VTEMP )

!
!     Now try to find the normal at the offset point.
!
      CALL ZZSBFNRM ( BODYID, 0,     LAYSRF,  ET, &
                      FIXFID, VTEMP, NORMAL       )
      CALL CHCKXC ( .TRUE., 'SPICE(POINTOFFSURFACE)', OK )







!***********************************************************************
!
!     Clean up.
!
!***********************************************************************

!
! ---------------------------------------------------------
!
      CALL TCASE ( 'Clean up' )


      CALL KCLEAR
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL(PCK0)
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL(TOPFK0)
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL(TOPFK1)
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL(TOPSP0)
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL(TOPSP1)
      CALL CHCKXC ( .FALSE., ' ', OK )


      DO BIX = 1, NTARG

         DO LAYRIX = 1, NLAYER

            DO W = 1, NTILEW

               DO H = 1, NTILEH

                  CALL DELFIL(  TILDSK( H, W, LAYRIX, BIX )  )
                  CALL CHCKXC ( .FALSE., ' ', OK )

               END DO

            END DO

         END DO

      END DO



!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END
