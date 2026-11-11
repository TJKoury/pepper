!$Procedure F_ZZDSKSPH ( ZZDSKSPH tests )

      SUBROUTINE F_ZZDSKSPH ( OK )
      use pepper_inc_dla
      use pepper_inc_dskdsc

! $ Abstract
!
!     Exercise the routine ZZDSKSPH.
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
!     This routine tests the SPICELIB routine ZZDSKSPH.
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
!-    TSPICE Version 1.0.0, 26-JUL-2016 (NJB)
!
!-&

!
!     SPICELIB functions
!

!
!     Other functions
!
      DOUBLE PRECISION      DPMAX
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      JYEAR
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      RPD
      DOUBLE PRECISION      TWOPI
      DOUBLE PRECISION      VNORM

      LOGICAL               EXISTS

!
!     Local Parameters
!
      CHARACTER*(*)         DSK0
      PARAMETER           ( DSK0   = 'zzdsksph_test0.bds' )

      CHARACTER*(*)         DSK1
      PARAMETER           ( DSK1   = 'zzdsksph_test1.bds' )

      CHARACTER*(*)         PCK0
      PARAMETER           ( PCK0   = 'zzdsksph_test0.tpc' )

      CHARACTER*(*)         TOPFK
      PARAMETER           ( TOPFK  = 'zzdsksph_topo.tf' )

      CHARACTER*(*)         TOPSPK
      PARAMETER           ( TOPSPK  = 'zzdsksph_topo.bsp' )


      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-11 )

      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-13 )


      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 255 )

      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 32 )

      INTEGER               NSITES
      PARAMETER           ( NSITES = 3 )

      INTEGER               MAXN
      PARAMETER           ( MAXN   = 6 )

      INTEGER               MAXV
      PARAMETER           ( MAXV   = 5000 )

      INTEGER               MAXP
      PARAMETER           ( MAXP   = 2 * MAXV )

!
!     Local Variables
!
      CHARACTER*(NAMLEN)    FIXREF
      CHARACTER*(NAMLEN)    FRAMES ( MAXN )
      CHARACTER*(NAMLEN)    SITFNM ( NSITES )
      CHARACTER*(NAMLEN)    SITNMS ( NSITES )
      CHARACTER*(NAMLEN)    TARGET
      CHARACTER*(NAMLEN)    TOPREF

      DOUBLE PRECISION      A
      DOUBLE PRECISION      ANGLES ( 3, NSITES )
      DOUBLE PRECISION      B
      DOUBLE PRECISION      BOUNDS ( 2, 3 )
      DOUBLE PRECISION      C
      DOUBLE PRECISION      CORPAR ( NSYPAR )
      DOUBLE PRECISION      DIST
      DOUBLE PRECISION      DSKDSC ( DSKDSZ )
      DOUBLE PRECISION      F
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      LAST
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      MAXLAT
      DOUBLE PRECISION      MAXLON
      DOUBLE PRECISION      MAXRAD
      DOUBLE PRECISION      MINLAT
      DOUBLE PRECISION      MINLON
      DOUBLE PRECISION      MINRAD
      DOUBLE PRECISION      OFFMAG
      DOUBLE PRECISION      OFFSET ( 3 )
      DOUBLE PRECISION      ORIGIN ( 3 )
      DOUBLE PRECISION      PNEAR  ( 3 )
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      RE
      DOUBLE PRECISION      RP
      DOUBLE PRECISION      SGMAXR
      DOUBLE PRECISION      SGMINR
      DOUBLE PRECISION      SITPOS ( 3, NSITES )
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      VERTS  ( 3, 3 )
      DOUBLE PRECISION      VRTCES ( 3, MAXV )
      DOUBLE PRECISION      VTEMP  ( 3 )
      DOUBLE PRECISION      XFORM  ( 3, 3 )
      DOUBLE PRECISION      XMAXR
      DOUBLE PRECISION      XMINR

      INTEGER               AXES   ( 3, NSITES )
      INTEGER               BODYID
      INTEGER               CORSYS
      INTEGER               DLADSC ( DLADSZ )
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               N
      INTEGER               NLAT
      INTEGER               NLON
      INTEGER               NP
      INTEGER               NSURF
      INTEGER               NV
      INTEGER               NXTDSC ( DLADSZ )
      INTEGER               PLATE  ( 3 )
      INTEGER               PLATES ( 3, MAXP )
      INTEGER               SITFID ( NSITES )
      INTEGER               SITIDS ( NSITES )
      INTEGER               SURFID
      INTEGER               SRFLST ( MAXSRF )

      LOGICAL               FOUND
      LOGICAL               MAKVTL

!
!     Saved values
!
!     Save variables in order to avoid stack room problems.
!
      SAVE

!
!     Initial values
!
      DATA                  ORIGIN / 3 * 0.D0 /


!
!     Open the test family.
!
      CALL TOPEN ( 'F_ZZDSKSPH' )



!**********************************************************************
!
!     Set-up
!
!**********************************************************************


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Setup: create topocentric frame kernels and ' &
      //           'supporting kernels.'                        )

!
!     Create and load the PCK; keep the file.
!
      IF ( EXISTS(PCK0) ) THEN
         CALL DELFIL( PCK0 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

!
!     We'll use FURNSH to perform the load; this'll save grief later.
!
      CALL TSTPCK ( PCK0, .FALSE., .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH ( PCK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Set the target.
!
      TARGET = 'MARS'
      BODYID = 499

      CALL BODVCD ( BODYID, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      A = RADII(1)
      B = RADII(2)
      C = RADII(3)


!     Create the FKs and SPKs needed to support topocentric frames.
!
      DO I = 1, NSITES

         SITIDS(I) = 499000 + I

         SITNMS(I) = 'MARS_SURFACE_SITE_#'
         CALL REPMI ( SITNMS(I), '#', I, SITNMS(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

         SITFNM(I) = 'MARS_TOPO_#'
         CALL REPMI ( SITFNM(I), '#', I, SITFNM(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

         SITFID(I) = SITIDS(I)

         FRAMES(I) = 'IAU_MARS'

         LON = ( 30.D0 + 3*(I-1) )* RPD()
         LAT = ( 45.D0 + 2*(I-1) )* RPD()

         IF ( I .EQ. 1 ) THEN
!
!           Create a site on Mars' surface.
!
            CALL SRFREC ( BODYID, LON, LAT, SITPOS(1,I) )
            CALL CHCKXC ( .FALSE., ' ', OK )

         ELSE IF ( I .EQ. 2 ) THEN
!
!           Create a site near Mars' center.
!
            CALL VPACK ( 1.D2, 2.D2, 3.D2, SITPOS(1,I) )

         ELSE IF ( I .EQ. 3 ) THEN
!
!           Create a site near Mars' surface, at a radius
!           between C and A.
!
            CALL VPACK ( (A+C)/2, 0.D0, 0.D0, SITPOS(1,I) )

         ELSE
!
!           This is the backstop case.
!
            CALL SIGERR ( 'TEST BUG' )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF

         AXES(1,I)   = 3
         AXES(2,I)   = 2
         AXES(3,I)   = 3

         ANGLES(1,I) = -LON
         ANGLES(2,I) =  LAT - PI()/2
         ANGLES(3,I) =  PI()

      END DO


      FIRST = -100 * JYEAR()
      LAST  =  100 * JYEAR()


!
!     Create topocentric kernels. We'll need to create
!     separate kernels for the different target bodies.
!

      IF ( EXISTS(TOPFK) ) THEN
         CALL DELFIL( TOPFK )
         CALL CHCKXC   ( .FALSE., ' ', OK )
      END IF

      IF ( EXISTS(TOPSPK) ) THEN
         CALL DELFIL( TOPSPK )
         CALL CHCKXC   ( .FALSE., ' ', OK )
      END IF


      CALL T_TOPKER ( TOPFK,  TOPSPK, TARGET, &
                      FRAMES, NSITES, SITIDS, &
                      SITNMS, SITPOS, SITFNM, &
                      SITFID, FIRST,  LAST, &
                      AXES,   ANGLES          )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!      Load the kernels.
!
       CALL FURNSH ( TOPFK )
       CALL CHCKXC   ( .FALSE., ' ', OK )

       CALL FURNSH ( TOPSPK )
       CALL CHCKXC   ( .FALSE., ' ', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Create Mars DSK containing segments that use ' &
      //           'each coordinate system.'                 )



      IF ( EXISTS(DSK0) ) THEN
         CALL DELFIL( DSK0 )
         CALL CHCKXC   ( .FALSE., ' ', OK )
      END IF

!
!     Start out with a tessellated ellipsoid DSK for Mars. Use
!     latitudinal coordinates.
!
      BODYID = 499
      SURFID = 1

      CORSYS = LATSYS
      CALL CLEARD ( 3, CORPAR )

      FIXREF = 'IAU_MARS'

      FIRST  = -100 * JYEAR()
      LAST   =  100 * JYEAR()

      CALL BODVCD ( BODYID, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      A = RADII(1)
      B = RADII(2)
      C = RADII(3)

      BOUNDS(1,1) =  0.D0
      BOUNDS(2,1) =  TWOPI()
      BOUNDS(1,2) = -HALFPI()
      BOUNDS(2,2) =  HALFPI()

      NLON   = 20
      NLAT   = 10

      MAKVTL = .FALSE.


      CALL T_ELDSK2 ( BODYID, SURFID, FIXREF, FIRST,  LAST, &
                      CORSYS, CORPAR, BOUNDS, A,      B, &
                      C,      NLON,   NLAT,   MAKVTL, DSK0  )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     Second segment: use planetodetic coordinates:
!
      CORSYS = PDTSYS

      RE     = A
      RP     = C
      F      = ( RE - RP ) / RE

      CORPAR(1) = RE
      CORPAR(2) = F

      SURFID = 2

      CALL T_ELDSK2 ( BODYID, SURFID, FIXREF, FIRST,  LAST, &
                      CORSYS, CORPAR, BOUNDS, A,      B, &
                      C,      NLON,   NLAT,   MAKVTL, DSK0  )
      CALL CHCKXC   ( .FALSE., ' ', OK )



!
!     Third segment: use rectangular coordinates:
!
      CORSYS = RECSYS

      BOUNDS(1,1) =  -A
      BOUNDS(2,1) =   A
      BOUNDS(1,2) =  -A
      BOUNDS(2,2) =   A

      SURFID = 3

      CALL T_ELDSK2 ( BODYID, SURFID, FIXREF, FIRST,  LAST, &
                      CORSYS, CORPAR, BOUNDS, A,      B, &
                      C,      NLON,   NLAT,   MAKVTL, DSK0  )
      CALL CHCKXC   ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Add to Mars DSK three segments having frames ' &
      //           'with offset centers.'                       )

!
!     The fourth segment of the file, which contains three segments
!     already, will be centered at a surface point. The segment
!     will have rather small extents, so the central body will be
!     well outside of the segment's outer bounding surface.
!
      CORSYS = LATSYS
      CALL CLEARD ( 3, CORPAR )

      BODYID = 499
      SURFID = 4

      FIXREF = 'IAU_MARS'
      TOPREF = SITFNM(1)

      FIRST  = -100 * JYEAR()
      LAST   =  100 * JYEAR()

      CALL BODVCD ( BODYID, 'RADII', 3, N, RADII )

      A = RADII(1)
      B = RADII(2)
      C = RADII(3)

      MINLON = 25 * RPD()
      MAXLON = 35 * RPD()
      MINLAT = 40 * RPD()
      MAXLAT = 50 * RPD()

      NLON   = 20
      NLAT   = 10
!
!     Create plate set.
!
      CALL ZZELLSEC( A,      B,      C,      MINLON, &
                     MAXLON, MINLAT, MAXLAT, NLON, &
                     NLAT,   MAXV,   MAXP,   NV, &
                     VRTCES, NP,     PLATES         )

!
!     Translate the vertices to make them relative to the origin
!     of the topocentric frame. Rotate the translated vertices
!     to that frame.
!
      CALL SPKEZP ( SITIDS(1), 0.D0, FIXREF, 'NONE', BODYID, &
                    OFFSET,    LT                            )

      CALL PXFORM ( FIXREF, TOPREF, 0.D0, XFORM )

      DO I = 1, NV

         CALL VSUB ( VRTCES(1,I), OFFSET, VTEMP       )
         CALL MXV  ( XFORM,       VTEMP,  VRTCES(1,I) )

      END DO

!
!     Add this segment to the existing Mars DSK.
!
      BOUNDS(1,1) =  0.D0
      BOUNDS(2,1) =  TWOPI()
      BOUNDS(1,2) = -HALFPI()
      BOUNDS(2,2) =  HALFPI()

      MAKVTL = .FALSE.

      CALL T_WRTPLT ( BODYID, SURFID, TOPREF, FIRST, LAST, &
                      CORSYS, CORPAR, BOUNDS, NV,    NP, &
                      VRTCES, PLATES, MAKVTL, DSK0        )




!
!     The fifth segment of the file will be centered at a point
!     close to Mars' center. The segment will surround the
!     frame center and be bounded away from it.
!
      CORSYS = LATSYS
      CALL CLEARD ( 3, CORPAR )

      BODYID = 499
      SURFID = 5

      FIXREF = 'IAU_MARS'
      TOPREF = SITFNM(2)

      FIRST  = -100 * JYEAR()
      LAST   =  100 * JYEAR()

      CALL BODVCD ( BODYID, 'RADII', 3, N, RADII )

      A = RADII(1)
      B = RADII(2)
      C = RADII(3)

      NLON   = 20
      NLAT   = 10
!
!     Create plate set.
!
      CALL ZZELLPLT ( A,      B,      C,      NLON, &
                      NLAT,   MAXV,   MAXP,   NV, &
                      VRTCES, NP,     PLATES       )

!
!     Translate the vertices to make them relative to the origin
!     of the topocentric frame. Rotate the translated vertices
!     to that frame.
!
      CALL SPKEZP ( SITIDS(2), 0.D0, FIXREF, 'NONE', BODYID, &
                    OFFSET,    LT                            )

      CALL PXFORM ( FIXREF, TOPREF, 0.D0, XFORM )

      DO I = 1, NV

         CALL VSUB ( VRTCES(1,I), OFFSET, VTEMP       )
         CALL MXV  ( XFORM,       VTEMP,  VRTCES(1,I) )

      END DO

!
!     Add this segment to the existing Mars DSK.
!
      BOUNDS(1,1) =  0.D0
      BOUNDS(2,1) =  TWOPI()
      BOUNDS(1,2) = -HALFPI()
      BOUNDS(2,2) =  HALFPI()

      MAKVTL = .FALSE.

      CALL T_WRTPLT ( BODYID, SURFID, TOPREF, FIRST, LAST, &
                      CORSYS, CORPAR, BOUNDS, NV,    NP, &
                      VRTCES, PLATES, MAKVTL, DSK0        )




!
!     The sixth segment of the file will be centered at a point
!     close to Mars' surface, but below the sphere of maximum radius.
!
      CORSYS = LATSYS
      CALL CLEARD ( 3, CORPAR )

      BODYID = 499
      SURFID = 6

      FIXREF = 'IAU_MARS'
      TOPREF = SITFNM(3)

      FIRST  = -100 * JYEAR()
      LAST   =  100 * JYEAR()

      CALL BODVCD ( BODYID, 'RADII', 3, N, RADII )

      A = RADII(1)
      B = RADII(2)
      C = RADII(3)


      NLON   = 20
      NLAT   = 10
!
!     Create plate set.
!
      CALL ZZELLPLT ( A,      B,      C,      NLON, &
                      NLAT,   MAXV,   MAXP,   NV, &
                      VRTCES, NP,     PLATES       )

!
!     Translate the vertices to make them relative to the origin
!     of the topocentric frame. Rotate the translated vertices
!     to that frame.
!
      CALL SPKEZP ( SITIDS(3), 0.D0, FIXREF, 'NONE', BODYID, &
                    OFFSET,    LT                            )

      CALL PXFORM ( FIXREF, TOPREF, 0.D0, XFORM )

      DO I = 1, NV

         CALL VSUB ( VRTCES(1,I), OFFSET, VTEMP       )
         CALL MXV  ( XFORM,       VTEMP,  VRTCES(1,I) )

      END DO

!
!     Add this segment to the existing Mars DSK.
!
      BOUNDS(1,1) =  0.D0
      BOUNDS(2,1) =  TWOPI()
      BOUNDS(1,2) = -HALFPI()
      BOUNDS(2,2) =  HALFPI()

      MAKVTL = .FALSE.

      CALL T_WRTPLT ( BODYID, SURFID, TOPREF, FIRST, LAST, &
                      CORSYS, CORPAR, BOUNDS, NV,    NP, &
                      VRTCES, PLATES, MAKVTL, DSK0        )






!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Create Saturn DSK containing segments that use ' &
      //           'each coordinate system.'                 )

      IF ( EXISTS(DSK1) ) THEN
         CALL DELFIL( DSK1 )
         CALL CHCKXC   ( .FALSE., ' ', OK )
      END IF

!
!     Start out with a tessellated ellipsoid DSK for Mars. Use
!     latitudinal coordinates.
!
      BODYID = 699
      SURFID = 1

      CORSYS = LATSYS
      CALL CLEARD ( 3, CORPAR )

      FIXREF = 'IAU_SATURN'

      FIRST  = -100 * JYEAR()
      LAST   =  100 * JYEAR()

      CALL BODVCD ( BODYID, 'RADII', 3, N, RADII )

      A = RADII(1)
      B = RADII(2)
      C = RADII(3)

      BOUNDS(1,1) =  0.D0
      BOUNDS(2,1) =  TWOPI()
      BOUNDS(1,2) = -HALFPI()
      BOUNDS(2,2) =  HALFPI()

      NLON   = 20
      NLAT   = 10

      MAKVTL = .FALSE.


      CALL T_ELDSK2 ( BODYID, SURFID, FIXREF, FIRST,  LAST, &
                      CORSYS, CORPAR, BOUNDS, A,      B, &
                      C,      NLON,   NLAT,   MAKVTL, DSK1  )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     Second segment: use planetodetic coordinates. Make
!     Saturn into a *prolate* shape to excercise logic for
!     such shapes.
!
      CORSYS = PDTSYS

      RE     = C
      RP     = A
      F      = ( RE - RP ) / RE

      CORPAR(1) = RE
      CORPAR(2) = F

      SURFID = 2

      CALL T_ELDSK2 ( BODYID, SURFID, FIXREF, FIRST,  LAST, &
                      CORSYS, CORPAR, BOUNDS, C,      C, &
                      A,      NLON,   NLAT,   MAKVTL, DSK1  )
      CALL CHCKXC   ( .FALSE., ' ', OK )


!
!     Third segment: use rectangular coordinates:
!
      CORSYS = RECSYS

      BOUNDS(1,1) =  -A
      BOUNDS(2,1) =   A
      BOUNDS(1,2) =  -A
      BOUNDS(2,2) =   A

      SURFID = 3

      CALL T_ELDSK2 ( BODYID, SURFID, FIXREF, FIRST,  LAST, &
                      CORSYS, CORPAR, BOUNDS, A,      B, &
                      C,      NLON,   NLAT,   MAKVTL, DSK1  )
      CALL CHCKXC   ( .FALSE., ' ', OK )



!**********************************************************************
!
!     Normal cases
!
!**********************************************************************


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Find bounding radii for Mars latitudinal segment.' )


      CALL FURNSH ( DSK0 )
      CALL CHCKXC   ( .FALSE., ' ', OK )


      CALL CLEARI ( MAXSRF, SRFLST )

      BODYID    = 499
      NSURF     = 1
      SRFLST(1) = 1


      CALL ZZDSKSPH ( BODYID, NSURF, SRFLST, MINRAD, MAXRAD )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     Since the number of latitude bands is even, the maximum
!     radius should be attained by vertices on the equator.
!
      CALL BODVCD ( BODYID, 'RADII', 3, N, RADII )

      A = RADII(1)
      B = RADII(2)
      C = RADII(3)

      XMAXR = A

      TOL   = VTIGHT

      CALL CHCKSD ( 'MAXRAD', MAXRAD, '~/', XMAXR, TOL, OK )


!
!     The minimum radius should match the bound in the dscriptor.
!

      CALL DASOPR ( DSK0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DLABFS ( HANDLE, DLADSC, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'DLA FOUND', FOUND, .TRUE., OK )

      CALL DSKGD ( HANDLE, DLADSC, DSKDSC )
      CALL CHCKXC   ( .FALSE., ' ', OK )


      XMINR = DSKDSC(MN3IDX)

      TOL   = 0

      CALL CHCKSD ( '(1) MINRAD', MINRAD, '=', XMINR, TOL, OK )

!
!     Find the actual minimum radius of the surface and use this
!     as a secondary check.
!
!     The minimum radius is not so simple to find. We'll brute-force
!     it by finding the distance of each plate from the origin.
!
      CALL DSKZ02 ( HANDLE, DLADSC, NV, NP )
      CALL CHCKXC ( .FALSE., ' ', OK )
      XMINR = A

      DO I = 1, NP

         CALL DSKP02 ( HANDLE, DLADSC, I, 1, N, PLATE )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Fetch vertices of the Ith plate.
!
         DO J = 1, 3

            K = PLATE(J)

            CALL DSKV02 ( HANDLE, DLADSC, K, 1, N, VERTS(1,J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

!
!        Compute the distance of the Ith plate from the origin.
!
         CALL PLTNP ( ORIGIN,     VERTS(1,1), VERTS(1,2), &
                      VERTS(1,3), PNEAR,      DIST        )

         XMINR = MIN ( XMINR, DIST )

      END DO



      TOL   = VTIGHT

      CALL CHCKSD ( '(2) MINRAD', MINRAD, '~/', XMINR, TOL, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Repeat Mars latitudinal case; re-use segment list.')


      CALL ZZDSKSPH ( BODYID, NSURF, SRFLST, MINRAD, MAXRAD )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'MAXRAD', MAXRAD, '~/', XMAXR, TOL, OK )
      CALL CHCKSD ( 'MINRAD', MINRAD, '~/', XMINR, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Find bounding radii for Mars planetodetic ' &
      //           'segment.' )


!
!     Get the DLA descriptor for this segment.
!
      CALL DLAFNS ( HANDLE, DLADSC, NXTDSC, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'DLA FOUND', FOUND, .TRUE., OK )

      CALL MOVEI ( NXTDSC, DLADSZ, DLADSC )


      BODYID    = 499
      NSURF     = 1
      SRFLST(1) = 2


      CALL ZZDSKSPH ( BODYID, NSURF, SRFLST, MINRAD, MAXRAD )
      CALL CHCKXC   ( .FALSE., ' ', OK )


!
!     The minimum radius will be computed from the reference
!     ellipsoid's radii and the segment's altitude bounds.
!
      CALL DSKGD ( HANDLE, DLADSC, DSKDSC )
      CALL CHCKXC   ( .FALSE., ' ', OK )


      XMINR = C + DSKDSC(MN3IDX)

      TOL   = VTIGHT

      CALL CHCKSD ( 'MINRAD', MINRAD, '~/', XMINR, TOL, OK )


!
!     Since the number of latitude bands is even, the maximum
!     radius should be attained by vertices on the equator.
!
      XMAXR = A

      TOL   = VTIGHT

      CALL CHCKSD ( 'MAXRAD', MAXRAD, '~/', XMAXR, TOL, OK )

!
!     Restore the expected minimum radius from the DSK descriptor.
!
      XMINR = DSKDSC(MN3IDX) + C



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE( 'Repeat Mars planetodetic case; re-use segment list.')


      CALL ZZDSKSPH ( BODYID, NSURF, SRFLST, MINRAD, MAXRAD )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'MAXRAD', MAXRAD, '~/', XMAXR, TOL, OK )
      CALL CHCKSD ( 'MINRAD', MINRAD, '~/', XMINR, TOL, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Find bounding radii for Mars rectangular ' &
      //           'segment.' )



!
!     Get the DLA descriptor for this segment.
!
      CALL DLAFNS ( HANDLE, DLADSC, NXTDSC, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'DLA FOUND', FOUND, .TRUE., OK )

      CALL MOVEI ( NXTDSC, DLADSZ, DLADSC )


      BODYID    = 499
      NSURF     = 1
      SRFLST(1) = 3


      CALL ZZDSKSPH ( BODYID, NSURF, SRFLST, MINRAD, MAXRAD )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     In this case, the outer bounding radius is that of a box
!     that circumscribes the ellipsoid.
!
      XMAXR = SQRT( 2*A**2 + C**2 )

      TOL   = VTIGHT

      CALL CHCKSD ( 'MAXRAD', MAXRAD, '~/', XMAXR, TOL, OK )


!
!     The segment's central body is inside the segment's bounding box;
!     the expected minimum radius is zero.
!
      XMINR = 0.D0

      TOL   = VTIGHT

      CALL CHCKSD ( 'MINRAD', MINRAD, '~/', XMINR, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE( 'Repeat Mars rectangular case; re-use segment list.')


      CALL ZZDSKSPH ( BODYID, NSURF, SRFLST, MINRAD, MAXRAD )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'MAXRAD', MAXRAD, '~/', XMAXR, TOL, OK )
      CALL CHCKSD ( 'MINRAD', MINRAD, '~/', XMINR, TOL, OK )

!
!     Close the DSK so we can unload it later.
!
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!
!     Now load Saturn kernels; repeat tests for Saturn.
!
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Find bounding radii for Saturn latitudinal ' &
      //           'segment.'                                   )


!
!     Load Saturn DSK; leave Mars DSK loaded.
!
      CALL FURNSH ( DSK1 )
      CALL CHCKXC   ( .FALSE., ' ', OK )


      CALL CLEARI ( MAXSRF, SRFLST )

      BODYID    = 699
      NSURF     = 1
      SRFLST(1) = 1


      CALL ZZDSKSPH ( BODYID, NSURF, SRFLST, MINRAD, MAXRAD )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     Since the number of latitude bands is even, the maximum
!     radius should be attained by vertices on the equator.
!
      CALL BODVCD ( BODYID, 'RADII', 3, N, RADII )

      A = RADII(1)
      B = RADII(2)
      C = RADII(3)

      XMAXR = A

      TOL   = VTIGHT

      CALL CHCKSD ( 'MAXRAD', MAXRAD, '~/', XMAXR, TOL, OK )

!
!     The minimum radius should match the bound in the dscriptor.
!

      CALL DASOPR ( DSK1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DLABFS ( HANDLE, DLADSC, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'DLA FOUND', FOUND, .TRUE., OK )

      CALL DSKGD ( HANDLE, DLADSC, DSKDSC )
      CALL CHCKXC   ( .FALSE., ' ', OK )


      XMINR = DSKDSC(MN3IDX)

      TOL   = 0

      CALL CHCKSD ( '(1) MINRAD', MINRAD, '=', XMINR, TOL, OK )

!
!     Find the actual minimum radius of the surface and use this
!     as a secondary check.
!
!     The minimum radius is not so simple to find. We'll brute-force
!     it by finding the distance of each plate from the origin.
!
      CALL DSKZ02 ( HANDLE, DLADSC, NV, NP )
      CALL CHCKXC ( .FALSE., ' ', OK )
      XMINR = A


      DO I = 1, NP

         CALL DSKP02 ( HANDLE, DLADSC, I, 1, N, PLATE )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Fetch vertices of the Ith plate.
!
         DO J = 1, 3

            K = PLATE(J)

            CALL DSKV02 ( HANDLE, DLADSC, K, 1, N, VERTS(1,J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

!
!        Compute the distance of the Ith plate from the origin.
!
         CALL PLTNP ( ORIGIN,     VERTS(1,1), VERTS(1,2), &
                      VERTS(1,3), PNEAR,      DIST        )

         XMINR = MIN ( XMINR, DIST )

      END DO



      TOL   = VTIGHT

      CALL CHCKSD ( '(2) MINRAD', MINRAD, '~/', XMINR, TOL, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Repeat Saturn latitudinal case; re-use ' &
      //           'segment list.')


      CALL ZZDSKSPH ( BODYID, NSURF, SRFLST, MINRAD, MAXRAD )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'MAXRAD', MAXRAD, '~/', XMAXR, TOL, OK )
      CALL CHCKSD ( 'MINRAD', MINRAD, '~/', XMINR, TOL, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Find bounding radii for Saturn planetodetic ' &
      //           'segment.' )

!
!     Get the DLA descriptor for this segment.
!
      CALL DLAFNS ( HANDLE, DLADSC, NXTDSC, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'DLA FOUND', FOUND, .TRUE., OK )

      CALL MOVEI ( NXTDSC, DLADSZ, DLADSC )

!
!     We'll need the DSK descriptor for this segment, since
!     the segment bounds are unique to this segment.
!
      CALL DSKGD ( HANDLE, DLADSC, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      BODYID    = 699
      NSURF     = 1
      SRFLST(1) = 2


      CALL ZZDSKSPH ( BODYID, NSURF, SRFLST, MINRAD, MAXRAD )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     Since the number of latitude bands is even, the maximum
!     radius should be attained by vertices on the pole (recall
!     we've made Saturn prolate in this segment).
!
      XMAXR = A

      TOL   = VTIGHT

      CALL CHCKSD ( 'MAXRAD', MAXRAD, '~/', XMAXR, TOL, OK )


!
!     The minimum radius will be computed from the reference
!     ellipsoid's radii and the segment's altitude bounds.
!
      CALL DSKGD ( HANDLE, DLADSC, DSKDSC )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     Recall this model is prolate. However, A, B, C are from
!     the kernel pool.
!
      XMINR = C + DSKDSC(MN3IDX)

      TOL   = VTIGHT

      CALL CHCKSD ( 'MINRAD', MINRAD, '~/', XMINR, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE( 'Repeat Saturn planetodetic case; re-use ' &
      //          'segment list.'                          )


      CALL ZZDSKSPH ( BODYID, NSURF, SRFLST, MINRAD, MAXRAD )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'MAXRAD', MAXRAD, '~/', XMAXR, TOL, OK )
      CALL CHCKSD ( 'MINRAD', MINRAD, '~/', XMINR, TOL, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Find bounding radii for Saturn rectangular ' &
      //           'segment.' )


!
!     Get the DLA descriptor for this segment.
!
      CALL DLAFNS ( HANDLE, DLADSC, NXTDSC, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'DLA FOUND', FOUND, .TRUE., OK )

      CALL MOVEI ( NXTDSC, DLADSZ, DLADSC )


      BODYID    = 699
      NSURF     = 1
      SRFLST(1) = 3


      CALL ZZDSKSPH ( BODYID, NSURF, SRFLST, MINRAD, MAXRAD )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     In this case, the outer bounding radius is that of a box
!     that circumscribes the ellipsoid.
!
      XMAXR = SQRT( 2*A**2 + C**2 )

      TOL   = VTIGHT

      CALL CHCKSD ( 'MAXRAD', MAXRAD, '~/', XMAXR, TOL, OK )


!
!     The segment's central body is inside the segment's bounding box;
!     the expected minimum radius is zero.
!
      XMINR = 0.D0

      TOL   = VTIGHT

      CALL CHCKSD ( 'MINRAD', MINRAD, '~/', XMINR, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE( 'Repeat Saturn rectangular case; ' &
      //          're-use segment list.')


      CALL ZZDSKSPH ( BODYID, NSURF, SRFLST, MINRAD, MAXRAD )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'MAXRAD', MAXRAD, '~/', XMAXR, TOL, OK )
      CALL CHCKSD ( 'MINRAD', MINRAD, '~/', XMINR, TOL, OK )


!
!     Check handling of BSR state change.
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE( 'Unload Mars DSK and repeat Saturn rectangular ' &
      //          'case again; re-use segment list.')


      CALL UNLOAD ( DSK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZDSKSPH ( BODYID, NSURF, SRFLST, MINRAD, MAXRAD )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'MAXRAD', MAXRAD, '~/', XMAXR, TOL, OK )
      CALL CHCKSD ( 'MINRAD', MINRAD, '~/', XMINR, TOL, OK )


!
!     Close the Saturn DSK so we can unload it later.
!
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     The next set of tests uses frames having centers offset
!     from the central body.
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Find bounding radii for Mars segment having ' &
      //           'its frame center near Mars'' surface. For this ' &
      //           'case, the minimum radius is the maximum segment ' &
      //           'radius minus the frame offset.'                  )


      CALL FURNSH ( DSK0 )
      CALL CHCKXC   ( .FALSE., ' ', OK )


      CALL CLEARI ( MAXSRF, SRFLST )

      BODYID    = 499
      NSURF     = 1
      SRFLST(1) = 4


      CALL ZZDSKSPH ( BODYID, NSURF, SRFLST, MINRAD, MAXRAD )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     For this case, the upper bound is the sum of the
!     DSK descriptor's upper radius bound and the magnitude of
!     the offset between the frame's center and Mars.
!
      CALL SPKEZP ( SITIDS(1), 0.D0,   SITFNM(1), 'NONE', &
                    BODYID,    OFFSET, LT                )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      OFFMAG = VNORM( OFFSET )

      CALL DASOPR ( DSK0, HANDLE )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL DLABFS ( HANDLE, NXTDSC, FOUND )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'DLABFS found', FOUND, .TRUE., OK )

      DO I = 1, 3

         CALL DLAFNS ( HANDLE, NXTDSC, DLADSC, FOUND )
         CALL CHCKXC   ( .FALSE., ' ', OK )
         CALL CHCKSL ( 'DLAFNS found', FOUND, .TRUE., OK )

         CALL MOVEI ( DLADSC, DLADSZ, NXTDSC )

      END DO

      CALL DSKGD ( HANDLE, DLADSC, DSKDSC )
      CALL CHCKXC   ( .FALSE., ' ', OK )


      XMAXR = DSKDSC(MX3IDX) + OFFMAG

      TOL   = VTIGHT

      CALL CHCKSD ( 'MAXRAD', MAXRAD, '~/', XMAXR, TOL, OK )


!
!     The minimum radius should be the lower bound in the descriptor,
!     minus the offset magnitude.
!
!     The minimum radius should be the offset magnitude, minus
!     the upper bound in the descriptor.
!
      XMINR = OFFMAG - DSKDSC(MX3IDX)

      TOL   = 0

      CALL CHCKSD ( 'MINRAD', MINRAD, '=', XMINR, TOL, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Find bounding radii for Mars segment having ' &
      //           'its frame center near Mars'' center. For this ' &
      //           'case, the minimum radius is the minimum segment ' &
      //           'radius minus the frame offset.'                  )



      CALL CLEARI ( MAXSRF, SRFLST )

      BODYID    = 499
      NSURF     = 1
      SRFLST(1) = 5


      CALL ZZDSKSPH ( BODYID, NSURF, SRFLST, MINRAD, MAXRAD )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     For this case, the upper bound is the sum of the
!     DSK descriptor's upper radius bound and the magnitude of
!     the offset between the frame's center and Mars.
!
      CALL SPKEZP ( SITIDS(2), 0.D0,   SITFNM(3), 'NONE', &
                    BODYID,    OFFSET, LT                )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      OFFMAG = VNORM( OFFSET )

      CALL DLAFNS ( HANDLE, DLADSC, NXTDSC, FOUND )
      CALL CHCKXC   ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'DLAFNS found', FOUND, .TRUE., OK )

      CALL MOVEI ( NXTDSC, DLADSZ, DLADSC )

      CALL DSKGD ( HANDLE, DLADSC, DSKDSC )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      XMAXR = DSKDSC(MX3IDX) + OFFMAG

      TOL   = VTIGHT

      CALL CHCKSD ( 'MAXRAD', MAXRAD, '~/', XMAXR, TOL, OK )


!
!     The minimum radius should be the lower bound in the descriptor,
!     minus the offset magnitude.
!
!     The minimum radius should be the offset magnitude, minus
!     the upper bound in the descriptor.
!
      XMINR = DSKDSC(MN3IDX) - OFFMAG

      TOL   = 0

      CALL CHCKSD ( 'MINRAD', MINRAD, '=', XMINR, TOL, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Find bounding radii for Mars segment having ' &
      //           'its frame center slightly below Mars'' surface. ' &
      //           'For this case, the minimum radius is zero.'       )



      CALL CLEARI ( MAXSRF, SRFLST )

      BODYID    = 499
      NSURF     = 1
      SRFLST(1) = 6


      CALL ZZDSKSPH ( BODYID, NSURF, SRFLST, MINRAD, MAXRAD )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     For this case, the upper bound is the sum of the
!     DSK descriptor's upper radius bound and the magnitude of
!     the offset between the frame's center and Mars.
!
      CALL SPKEZP ( SITIDS(3), 0.D0,   SITFNM(3), 'NONE', &
                    BODYID,    OFFSET, LT                )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      OFFMAG = VNORM( OFFSET )

      CALL DLAFNS ( HANDLE, DLADSC, NXTDSC, FOUND )
      CALL CHCKXC   ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'DLAFNS found', FOUND, .TRUE., OK )

      CALL MOVEI ( NXTDSC, DLADSZ, DLADSC )

      CALL DSKGD ( HANDLE, DLADSC, DSKDSC )
      CALL CHCKXC   ( .FALSE., ' ', OK )


      XMAXR = DSKDSC(MX3IDX) + OFFMAG

      TOL   = VTIGHT

      CALL CHCKSD ( 'MAXRAD', MAXRAD, '~/', XMAXR, TOL, OK )


!
!     The frame center offset is between the segment's minimum
!     and maximum radius, so we can't guarantee that the frame
!     center is a positive distance from all plates.
!
      XMINR = 0.D0

      TOL   = 0

      CALL CHCKSD ( 'MINRAD', MINRAD, '=', XMINR, TOL, OK )

!
!     Close the Mars DSK so we can unload it later.
!
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Find Mars radius bounds using empty surface list.' )

      BODYID    = 499
      NSURF     = 0

      CALL CLEARI ( MAXSRF, SRFLST )


      CALL ZZDSKSPH ( BODYID, NSURF, SRFLST, MINRAD, MAXRAD )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     Generate expected bounds by combining bounds for each surface.
!
      XMINR = DPMAX()
      XMAXR = 0.D0

      DO I = 1, 6

         CALL ZZDSKSPH ( BODYID, NSURF, SRFLST(I), SGMINR, SGMAXR )
         CALL CHCKXC   ( .FALSE., ' ', OK )

         XMINR = MIN( XMINR, SGMINR )
         XMAXR = MAX( XMAXR, SGMAXR )

      END DO

      TOL   = VTIGHT

      CALL CHCKSD ( 'MINRAD', MINRAD, '=', XMINR, TOL, OK )
      CALL CHCKSD ( 'MAXRAD', MAXRAD, '=', XMAXR, TOL, OK )





!**********************************************************************
!
!     Error cases
!
!**********************************************************************


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error: try to find bounds for body having ' &
      //           'no loaded DSK segments.'                   )


      BODYID = 299

      NSURF  = 0
      CALL CLEARI ( MAXSRF, SRFLST )

      CALL ZZDSKSPH ( BODYID, NSURF, SRFLST, MINRAD, MAXRAD )
      CALL CHCKXC   ( .TRUE., 'SPICE(DSKDATANOTFOUND)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error: find bounds for Mars. Unload Mars DSK. ' &
      //           'Try again to find bounds for Mars.'            )


      BODYID = 499
      NSURF  = 0

      CALL ZZDSKSPH ( BODYID, NSURF, SRFLST, MINRAD, MAXRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL UNLOAD ( DSK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZDSKSPH ( BODYID, NSURF, SRFLST, MINRAD, MAXRAD )
      CALL CHCKXC   ( .TRUE., 'SPICE(DSKDATANOTFOUND)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Reload Mars DSK. ' &
      //           'Unload Mars SPK. Try again to find bounds for ' &
      //           'Mars.'                                         )

      CALL FURNSH ( DSK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL UNLOAD ( TOPSPK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZDSKSPH ( BODYID, NSURF, SRFLST, MINRAD, MAXRAD )
      CALL CHCKXC   ( .TRUE., 'SPICE(NOLOADEDFILES)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Reload Mars SPK. ' &
      //           'Unload Mars FK. Try again to find bounds for ' &
      //           'Mars.'                                         )

      CALL FURNSH ( TOPSPK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL UNLOAD ( TOPFK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZDSKSPH ( BODYID, NSURF, SRFLST, MINRAD, MAXRAD )
      CALL CHCKXC   ( .TRUE., 'SPICE(NOFRAMEDATA)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Reload FK.  Try again to find bounds ' &
      //           'for Mars. This is a non-error case.'              )


      CALL FURNSH ( TOPFK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZDSKSPH ( BODYID, NSURF, SRFLST, MINRAD, MAXRAD )
      CALL CHCKXC   ( .FALSE., ' ', OK )


!
!     Generate expected bounds by combining bounds for each surface.
!
      XMINR = DPMAX()
      XMAXR = 0.D0

      DO I = 1, 6

         CALL ZZDSKSPH ( BODYID, NSURF, SRFLST(I), SGMINR, SGMAXR )
         CALL CHCKXC   ( .FALSE., ' ', OK )

         XMINR = MIN( XMINR, SGMINR )
         XMAXR = MAX( XMAXR, SGMAXR )

      END DO

      TOL   = VTIGHT

      CALL CHCKSD ( 'MINRAD', MINRAD, '=', XMINR, TOL, OK )
      CALL CHCKSD ( 'MAXRAD', MAXRAD, '=', XMAXR, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Repeat previous case, minus the kernel loads.' )



      CALL ZZDSKSPH ( BODYID, NSURF, SRFLST, MINRAD, MAXRAD )
      CALL CHCKXC   ( .FALSE., ' ', OK )


!
!     Generate expected bounds by combining bounds for each surface.
!
      XMINR = DPMAX()
      XMAXR = 0.D0

      DO I = 1, 6

         CALL ZZDSKSPH ( BODYID, NSURF, SRFLST(I), SGMINR, SGMAXR )
         CALL CHCKXC   ( .FALSE., ' ', OK )

         XMINR = MIN( XMINR, SGMINR )
         XMAXR = MAX( XMAXR, SGMAXR )

      END DO

      TOL   = VTIGHT

      CALL CHCKSD ( 'MINRAD', MINRAD, '=', XMINR, TOL, OK )
      CALL CHCKSD ( 'MAXRAD', MAXRAD, '=', XMAXR, TOL, OK )



!**********************************************************************
!
!     Clean up
!
!**********************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Clean up.' )


      CALL KCLEAR
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL DELFIL ( PCK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( TOPFK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( TOPSPK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( DSK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( DSK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


