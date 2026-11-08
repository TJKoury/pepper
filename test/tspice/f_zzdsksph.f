C$Procedure F_ZZDSKSPH ( ZZDSKSPH tests )
 
      SUBROUTINE F_ZZDSKSPH ( OK )
 
C$ Abstract
C
C     Exercise the routine ZZDSKSPH.
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

      INCLUDE 'dla.inc'
      INCLUDE 'dskdsc.inc'
      INCLUDE 'dsk.inc'

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
C     This routine tests the SPICELIB routine ZZDSKSPH.
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
C-    TSPICE Version 1.0.0, 26-JUL-2016 (NJB)
C
C-&
 
C
C     SPICELIB functions
C

C
C     Other functions
C
      DOUBLE PRECISION      DPMAX
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      JYEAR
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      RPD
      DOUBLE PRECISION      TWOPI
      DOUBLE PRECISION      VNORM

      LOGICAL               EXISTS

C
C     Local Parameters 
C
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

C 
C     Local Variables 
C  
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
 
C
C     Saved values
C
C     Save variables in order to avoid stack room problems.
C
      SAVE

C
C     Initial values
C
      DATA                  ORIGIN / 3 * 0.D0 /


C
C     Open the test family.
C
      CALL TOPEN ( 'F_ZZDSKSPH' )



C**********************************************************************
C
C     Set-up
C
C**********************************************************************


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Setup: create topocentric frame kernels and '
     .//           'supporting kernels.'                        )

C
C     Create and load the PCK; keep the file.
C
      IF ( EXISTS(PCK0) ) THEN
         CALL DELFIL( PCK0 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF
      
C
C     We'll use FURNSH to perform the load; this'll save grief later.
C
      CALL TSTPCK ( PCK0, .FALSE., .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH ( PCK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Set the target.
C
      TARGET = 'MARS'
      BODYID = 499

      CALL BODVCD ( BODYID, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
      A = RADII(1)
      B = RADII(2)
      C = RADII(3)


C     Create the FKs and SPKs needed to support topocentric frames.
C
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
C
C           Create a site on Mars' surface.
C
            CALL SRFREC ( BODYID, LON, LAT, SITPOS(1,I) )
            CALL CHCKXC ( .FALSE., ' ', OK ) 

         ELSE IF ( I .EQ. 2 ) THEN
C
C           Create a site near Mars' center.
C
            CALL VPACK ( 1.D2, 2.D2, 3.D2, SITPOS(1,I) )

         ELSE IF ( I .EQ. 3 ) THEN
C
C           Create a site near Mars' surface, at a radius
C           between C and A.
C
            CALL VPACK ( (A+C)/2, 0.D0, 0.D0, SITPOS(1,I) )

         ELSE
C
C           This is the backstop case.
C
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
 

C
C     Create topocentric kernels. We'll need to create
C     separate kernels for the different target bodies.
C

      IF ( EXISTS(TOPFK) ) THEN
         CALL DELFIL( TOPFK )
         CALL CHCKXC   ( .FALSE., ' ', OK )      
      END IF

      IF ( EXISTS(TOPSPK) ) THEN
         CALL DELFIL( TOPSPK )
         CALL CHCKXC   ( .FALSE., ' ', OK )      
      END IF


      CALL T_TOPKER ( TOPFK,  TOPSPK, TARGET, 
     .                FRAMES, NSITES, SITIDS, 
     .                SITNMS, SITPOS, SITFNM,
     .                SITFID, FIRST,  LAST,
     .                AXES,   ANGLES          )
      CALL CHCKXC   ( .FALSE., ' ', OK )      

C
C      Load the kernels.
C
       CALL FURNSH ( TOPFK )
       CALL CHCKXC   ( .FALSE., ' ', OK )      

       CALL FURNSH ( TOPSPK )
       CALL CHCKXC   ( .FALSE., ' ', OK )      



C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Create Mars DSK containing segments that use '
     .//           'each coordinate system.'                 )



      IF ( EXISTS(DSK0) ) THEN
         CALL DELFIL( DSK0 )
         CALL CHCKXC   ( .FALSE., ' ', OK )      
      END IF
    
C
C     Start out with a tessellated ellipsoid DSK for Mars. Use
C     latitudinal coordinates.
C
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
 

      CALL T_ELDSK2 ( BODYID, SURFID, FIXREF, FIRST,  LAST, 
     .                CORSYS, CORPAR, BOUNDS, A,      B, 
     .                C,      NLON,   NLAT,   MAKVTL, DSK0  )
      CALL CHCKXC   ( .FALSE., ' ', OK )      

C
C     Second segment: use planetodetic coordinates:
C
      CORSYS = PDTSYS

      RE     = A
      RP     = C
      F      = ( RE - RP ) / RE
      
      CORPAR(1) = RE
      CORPAR(2) = F

      SURFID = 2

      CALL T_ELDSK2 ( BODYID, SURFID, FIXREF, FIRST,  LAST, 
     .                CORSYS, CORPAR, BOUNDS, A,      B, 
     .                C,      NLON,   NLAT,   MAKVTL, DSK0  )
      CALL CHCKXC   ( .FALSE., ' ', OK )      

      

C
C     Third segment: use rectangular coordinates:
C
      CORSYS = RECSYS
      
      BOUNDS(1,1) =  -A
      BOUNDS(2,1) =   A
      BOUNDS(1,2) =  -A
      BOUNDS(2,2) =   A

      SURFID = 3

      CALL T_ELDSK2 ( BODYID, SURFID, FIXREF, FIRST,  LAST, 
     .                CORSYS, CORPAR, BOUNDS, A,      B, 
     .                C,      NLON,   NLAT,   MAKVTL, DSK0  )
      CALL CHCKXC   ( .FALSE., ' ', OK )      


C
C --- Case: ------------------------------------------------------
C
      
      CALL TCASE ( 'Add to Mars DSK three segments having frames '
     .//           'with offset centers.'                       )

C
C     The fourth segment of the file, which contains three segments
C     already, will be centered at a surface point. The segment
C     will have rather small extents, so the central body will be
C     well outside of the segment's outer bounding surface.
C
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
C
C     Create plate set.
C
      CALL ZZELLSEC( A,      B,      C,      MINLON, 
     .               MAXLON, MINLAT, MAXLAT, NLON,
     .               NLAT,   MAXV,   MAXP,   NV, 
     .               VRTCES, NP,     PLATES         )

C
C     Translate the vertices to make them relative to the origin
C     of the topocentric frame. Rotate the translated vertices
C     to that frame.
C
      CALL SPKEZP ( SITIDS(1), 0.D0, FIXREF, 'NONE', BODYID,
     .              OFFSET,    LT                            )

      CALL PXFORM ( FIXREF, TOPREF, 0.D0, XFORM )

      DO I = 1, NV

         CALL VSUB ( VRTCES(1,I), OFFSET, VTEMP       )
         CALL MXV  ( XFORM,       VTEMP,  VRTCES(1,I) )

      END DO

C
C     Add this segment to the existing Mars DSK.
C
      BOUNDS(1,1) =  0.D0
      BOUNDS(2,1) =  TWOPI()
      BOUNDS(1,2) = -HALFPI()
      BOUNDS(2,2) =  HALFPI()

      MAKVTL = .FALSE.

      CALL T_WRTPLT ( BODYID, SURFID, TOPREF, FIRST, LAST,
     .                CORSYS, CORPAR, BOUNDS, NV,    NP,    
     .                VRTCES, PLATES, MAKVTL, DSK0        )



      
C
C     The fifth segment of the file will be centered at a point 
C     close to Mars' center. The segment will surround the 
C     frame center and be bounded away from it.
C
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
C
C     Create plate set.
C
      CALL ZZELLPLT ( A,      B,      C,      NLON,
     .                NLAT,   MAXV,   MAXP,   NV, 
     .                VRTCES, NP,     PLATES       )

C
C     Translate the vertices to make them relative to the origin
C     of the topocentric frame. Rotate the translated vertices
C     to that frame.
C
      CALL SPKEZP ( SITIDS(2), 0.D0, FIXREF, 'NONE', BODYID,
     .              OFFSET,    LT                            )

      CALL PXFORM ( FIXREF, TOPREF, 0.D0, XFORM )

      DO I = 1, NV

         CALL VSUB ( VRTCES(1,I), OFFSET, VTEMP       )
         CALL MXV  ( XFORM,       VTEMP,  VRTCES(1,I) )

      END DO

C
C     Add this segment to the existing Mars DSK.
C
      BOUNDS(1,1) =  0.D0
      BOUNDS(2,1) =  TWOPI()
      BOUNDS(1,2) = -HALFPI()
      BOUNDS(2,2) =  HALFPI()

      MAKVTL = .FALSE.

      CALL T_WRTPLT ( BODYID, SURFID, TOPREF, FIRST, LAST,
     .                CORSYS, CORPAR, BOUNDS, NV,    NP,    
     .                VRTCES, PLATES, MAKVTL, DSK0        )




C
C     The sixth segment of the file will be centered at a point 
C     close to Mars' surface, but below the sphere of maximum radius.
C
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
C
C     Create plate set.
C
      CALL ZZELLPLT ( A,      B,      C,      NLON,
     .                NLAT,   MAXV,   MAXP,   NV, 
     .                VRTCES, NP,     PLATES       )

C
C     Translate the vertices to make them relative to the origin
C     of the topocentric frame. Rotate the translated vertices
C     to that frame.
C
      CALL SPKEZP ( SITIDS(3), 0.D0, FIXREF, 'NONE', BODYID,
     .              OFFSET,    LT                            )

      CALL PXFORM ( FIXREF, TOPREF, 0.D0, XFORM )

      DO I = 1, NV

         CALL VSUB ( VRTCES(1,I), OFFSET, VTEMP       )
         CALL MXV  ( XFORM,       VTEMP,  VRTCES(1,I) )

      END DO

C
C     Add this segment to the existing Mars DSK.
C
      BOUNDS(1,1) =  0.D0
      BOUNDS(2,1) =  TWOPI()
      BOUNDS(1,2) = -HALFPI()
      BOUNDS(2,2) =  HALFPI()

      MAKVTL = .FALSE.

      CALL T_WRTPLT ( BODYID, SURFID, TOPREF, FIRST, LAST,
     .                CORSYS, CORPAR, BOUNDS, NV,    NP,    
     .                VRTCES, PLATES, MAKVTL, DSK0        )


       



C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Create Saturn DSK containing segments that use '
     .//           'each coordinate system.'                 )

      IF ( EXISTS(DSK1) ) THEN
         CALL DELFIL( DSK1 )
         CALL CHCKXC   ( .FALSE., ' ', OK )      
      END IF

C
C     Start out with a tessellated ellipsoid DSK for Mars. Use
C     latitudinal coordinates.
C
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
 

      CALL T_ELDSK2 ( BODYID, SURFID, FIXREF, FIRST,  LAST, 
     .                CORSYS, CORPAR, BOUNDS, A,      B, 
     .                C,      NLON,   NLAT,   MAKVTL, DSK1  )
      CALL CHCKXC   ( .FALSE., ' ', OK )      

C
C     Second segment: use planetodetic coordinates. Make
C     Saturn into a *prolate* shape to excercise logic for
C     such shapes.
C
      CORSYS = PDTSYS

      RE     = C
      RP     = A
      F      = ( RE - RP ) / RE
      
      CORPAR(1) = RE
      CORPAR(2) = F

      SURFID = 2

      CALL T_ELDSK2 ( BODYID, SURFID, FIXREF, FIRST,  LAST, 
     .                CORSYS, CORPAR, BOUNDS, C,      C, 
     .                A,      NLON,   NLAT,   MAKVTL, DSK1  )
      CALL CHCKXC   ( .FALSE., ' ', OK )      


C
C     Third segment: use rectangular coordinates:
C
      CORSYS = RECSYS
      
      BOUNDS(1,1) =  -A
      BOUNDS(2,1) =   A
      BOUNDS(1,2) =  -A
      BOUNDS(2,2) =   A

      SURFID = 3

      CALL T_ELDSK2 ( BODYID, SURFID, FIXREF, FIRST,  LAST, 
     .                CORSYS, CORPAR, BOUNDS, A,      B, 
     .                C,      NLON,   NLAT,   MAKVTL, DSK1  )
      CALL CHCKXC   ( .FALSE., ' ', OK )      

      

C**********************************************************************
C
C     Normal cases
C
C**********************************************************************


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Find bounding radii for Mars latitudinal segment.' )


      CALL FURNSH ( DSK0 )
      CALL CHCKXC   ( .FALSE., ' ', OK )      


      CALL CLEARI ( MAXSRF, SRFLST )

      BODYID    = 499
      NSURF     = 1
      SRFLST(1) = 1


      CALL ZZDSKSPH ( BODYID, NSURF, SRFLST, MINRAD, MAXRAD )
      CALL CHCKXC   ( .FALSE., ' ', OK )      

C
C     Since the number of latitude bands is even, the maximum
C     radius should be attained by vertices on the equator.
C
      CALL BODVCD ( BODYID, 'RADII', 3, N, RADII )
      
      A = RADII(1)
      B = RADII(2)
      C = RADII(3)

      XMAXR = A

      TOL   = VTIGHT

      CALL CHCKSD ( 'MAXRAD', MAXRAD, '~/', XMAXR, TOL, OK )


C
C     The minimum radius should match the bound in the dscriptor.
C

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

C
C     Find the actual minimum radius of the surface and use this
C     as a secondary check.
C
C     The minimum radius is not so simple to find. We'll brute-force
C     it by finding the distance of each plate from the origin.
C     
      CALL DSKZ02 ( HANDLE, DLADSC, NV, NP )
      CALL CHCKXC ( .FALSE., ' ', OK )            
      XMINR = A

      DO I = 1, NP

         CALL DSKP02 ( HANDLE, DLADSC, I, 1, N, PLATE )
         CALL CHCKXC ( .FALSE., ' ', OK )      

C
C        Fetch vertices of the Ith plate.
C        
         DO J = 1, 3
            
            K = PLATE(J)

            CALL DSKV02 ( HANDLE, DLADSC, K, 1, N, VERTS(1,J) )
            CALL CHCKXC ( .FALSE., ' ', OK )      

         END DO

C
C        Compute the distance of the Ith plate from the origin.
C        
         CALL PLTNP ( ORIGIN,     VERTS(1,1), VERTS(1,2),
     .                VERTS(1,3), PNEAR,      DIST        )
            
         XMINR = MIN ( XMINR, DIST )

      END DO
      


      TOL   = VTIGHT

      CALL CHCKSD ( '(2) MINRAD', MINRAD, '~/', XMINR, TOL, OK )



C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Repeat Mars latitudinal case; re-use segment list.')


      CALL ZZDSKSPH ( BODYID, NSURF, SRFLST, MINRAD, MAXRAD )
      CALL CHCKXC   ( .FALSE., ' ', OK )      

      CALL CHCKSD ( 'MAXRAD', MAXRAD, '~/', XMAXR, TOL, OK )
      CALL CHCKSD ( 'MINRAD', MINRAD, '~/', XMINR, TOL, OK )


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Find bounding radii for Mars planetodetic '
     .//           'segment.' )


C
C     Get the DLA descriptor for this segment.
C
      CALL DLAFNS ( HANDLE, DLADSC, NXTDSC, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )      

      CALL CHCKSL ( 'DLA FOUND', FOUND, .TRUE., OK )

      CALL MOVEI ( NXTDSC, DLADSZ, DLADSC )


      BODYID    = 499
      NSURF     = 1
      SRFLST(1) = 2


      CALL ZZDSKSPH ( BODYID, NSURF, SRFLST, MINRAD, MAXRAD )
      CALL CHCKXC   ( .FALSE., ' ', OK )      


C
C     The minimum radius will be computed from the reference
C     ellipsoid's radii and the segment's altitude bounds.
C
      CALL DSKGD ( HANDLE, DLADSC, DSKDSC ) 
      CALL CHCKXC   ( .FALSE., ' ', OK )      


      XMINR = C + DSKDSC(MN3IDX)

      TOL   = VTIGHT

      CALL CHCKSD ( 'MINRAD', MINRAD, '~/', XMINR, TOL, OK )


C
C     Since the number of latitude bands is even, the maximum
C     radius should be attained by vertices on the equator.
C
      XMAXR = A

      TOL   = VTIGHT

      CALL CHCKSD ( 'MAXRAD', MAXRAD, '~/', XMAXR, TOL, OK )

C
C     Restore the expected minimum radius from the DSK descriptor.
C
      XMINR = DSKDSC(MN3IDX) + C

       

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE( 'Repeat Mars planetodetic case; re-use segment list.')


      CALL ZZDSKSPH ( BODYID, NSURF, SRFLST, MINRAD, MAXRAD )
      CALL CHCKXC   ( .FALSE., ' ', OK )      

      CALL CHCKSD ( 'MAXRAD', MAXRAD, '~/', XMAXR, TOL, OK )
      CALL CHCKSD ( 'MINRAD', MINRAD, '~/', XMINR, TOL, OK )
 

     
C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Find bounding radii for Mars rectangular '
     .//           'segment.' )



C
C     Get the DLA descriptor for this segment.
C
      CALL DLAFNS ( HANDLE, DLADSC, NXTDSC, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )      

      CALL CHCKSL ( 'DLA FOUND', FOUND, .TRUE., OK )

      CALL MOVEI ( NXTDSC, DLADSZ, DLADSC )


      BODYID    = 499
      NSURF     = 1
      SRFLST(1) = 3


      CALL ZZDSKSPH ( BODYID, NSURF, SRFLST, MINRAD, MAXRAD )
      CALL CHCKXC   ( .FALSE., ' ', OK )      

C
C     In this case, the outer bounding radius is that of a box
C     that circumscribes the ellipsoid.
C
      XMAXR = SQRT( 2*A**2 + C**2 )

      TOL   = VTIGHT

      CALL CHCKSD ( 'MAXRAD', MAXRAD, '~/', XMAXR, TOL, OK )


C
C     The segment's central body is inside the segment's bounding box;
C     the expected minimum radius is zero.
C
      XMINR = 0.D0

      TOL   = VTIGHT

      CALL CHCKSD ( 'MINRAD', MINRAD, '~/', XMINR, TOL, OK )
      

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE( 'Repeat Mars rectangular case; re-use segment list.')


      CALL ZZDSKSPH ( BODYID, NSURF, SRFLST, MINRAD, MAXRAD )
      CALL CHCKXC   ( .FALSE., ' ', OK )      

      CALL CHCKSD ( 'MAXRAD', MAXRAD, '~/', XMAXR, TOL, OK )
      CALL CHCKSD ( 'MINRAD', MINRAD, '~/', XMINR, TOL, OK )

C
C     Close the DSK so we can unload it later.
C
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )      
 

C
C
C     Now load Saturn kernels; repeat tests for Saturn.
C
C

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Find bounding radii for Saturn latitudinal '
     .//           'segment.'                                   )
 

C
C     Load Saturn DSK; leave Mars DSK loaded.
C
      CALL FURNSH ( DSK1 )
      CALL CHCKXC   ( .FALSE., ' ', OK )      


      CALL CLEARI ( MAXSRF, SRFLST )

      BODYID    = 699
      NSURF     = 1
      SRFLST(1) = 1


      CALL ZZDSKSPH ( BODYID, NSURF, SRFLST, MINRAD, MAXRAD )
      CALL CHCKXC   ( .FALSE., ' ', OK )      

C
C     Since the number of latitude bands is even, the maximum
C     radius should be attained by vertices on the equator.
C
      CALL BODVCD ( BODYID, 'RADII', 3, N, RADII )
      
      A = RADII(1)
      B = RADII(2)
      C = RADII(3)

      XMAXR = A

      TOL   = VTIGHT

      CALL CHCKSD ( 'MAXRAD', MAXRAD, '~/', XMAXR, TOL, OK )

C
C     The minimum radius should match the bound in the dscriptor.
C

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

C
C     Find the actual minimum radius of the surface and use this
C     as a secondary check.
C
C     The minimum radius is not so simple to find. We'll brute-force
C     it by finding the distance of each plate from the origin.
C     
      CALL DSKZ02 ( HANDLE, DLADSC, NV, NP )
      CALL CHCKXC ( .FALSE., ' ', OK )            
      XMINR = A


      DO I = 1, NP

         CALL DSKP02 ( HANDLE, DLADSC, I, 1, N, PLATE )
         CALL CHCKXC ( .FALSE., ' ', OK )      

C
C        Fetch vertices of the Ith plate.
C        
         DO J = 1, 3
            
            K = PLATE(J)

            CALL DSKV02 ( HANDLE, DLADSC, K, 1, N, VERTS(1,J) )
            CALL CHCKXC ( .FALSE., ' ', OK )      

         END DO

C
C        Compute the distance of the Ith plate from the origin.
C        
         CALL PLTNP ( ORIGIN,     VERTS(1,1), VERTS(1,2),
     .                VERTS(1,3), PNEAR,      DIST        )
                     
         XMINR = MIN ( XMINR, DIST )

      END DO
      


      TOL   = VTIGHT

      CALL CHCKSD ( '(2) MINRAD', MINRAD, '~/', XMINR, TOL, OK )



C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Repeat Saturn latitudinal case; re-use '
     .//           'segment list.')


      CALL ZZDSKSPH ( BODYID, NSURF, SRFLST, MINRAD, MAXRAD )
      CALL CHCKXC   ( .FALSE., ' ', OK )      

      CALL CHCKSD ( 'MAXRAD', MAXRAD, '~/', XMAXR, TOL, OK )
      CALL CHCKSD ( 'MINRAD', MINRAD, '~/', XMINR, TOL, OK )



C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Find bounding radii for Saturn planetodetic '
     .//           'segment.' )

C
C     Get the DLA descriptor for this segment.
C
      CALL DLAFNS ( HANDLE, DLADSC, NXTDSC, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )      

      CALL CHCKSL ( 'DLA FOUND', FOUND, .TRUE., OK )

      CALL MOVEI ( NXTDSC, DLADSZ, DLADSC )

C
C     We'll need the DSK descriptor for this segment, since
C     the segment bounds are unique to this segment.
C
      CALL DSKGD ( HANDLE, DLADSC, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )      

      BODYID    = 699
      NSURF     = 1
      SRFLST(1) = 2


      CALL ZZDSKSPH ( BODYID, NSURF, SRFLST, MINRAD, MAXRAD )
      CALL CHCKXC   ( .FALSE., ' ', OK )      

C
C     Since the number of latitude bands is even, the maximum
C     radius should be attained by vertices on the pole (recall
C     we've made Saturn prolate in this segment).
C
      XMAXR = A

      TOL   = VTIGHT

      CALL CHCKSD ( 'MAXRAD', MAXRAD, '~/', XMAXR, TOL, OK )


C
C     The minimum radius will be computed from the reference
C     ellipsoid's radii and the segment's altitude bounds.
C
      CALL DSKGD ( HANDLE, DLADSC, DSKDSC ) 
      CALL CHCKXC   ( .FALSE., ' ', OK )      
      
C
C     Recall this model is prolate. However, A, B, C are from
C     the kernel pool.
C
      XMINR = C + DSKDSC(MN3IDX)

      TOL   = VTIGHT

      CALL CHCKSD ( 'MINRAD', MINRAD, '~/', XMINR, TOL, OK )


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE( 'Repeat Saturn planetodetic case; re-use '
     .//          'segment list.'                          )


      CALL ZZDSKSPH ( BODYID, NSURF, SRFLST, MINRAD, MAXRAD )
      CALL CHCKXC   ( .FALSE., ' ', OK )      

      CALL CHCKSD ( 'MAXRAD', MAXRAD, '~/', XMAXR, TOL, OK )
      CALL CHCKSD ( 'MINRAD', MINRAD, '~/', XMINR, TOL, OK )
 

     
C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Find bounding radii for Saturn rectangular '
     .//           'segment.' )


C
C     Get the DLA descriptor for this segment.
C
      CALL DLAFNS ( HANDLE, DLADSC, NXTDSC, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )      

      CALL CHCKSL ( 'DLA FOUND', FOUND, .TRUE., OK )

      CALL MOVEI ( NXTDSC, DLADSZ, DLADSC )


      BODYID    = 699
      NSURF     = 1
      SRFLST(1) = 3


      CALL ZZDSKSPH ( BODYID, NSURF, SRFLST, MINRAD, MAXRAD )
      CALL CHCKXC   ( .FALSE., ' ', OK )      

C
C     In this case, the outer bounding radius is that of a box
C     that circumscribes the ellipsoid.
C
      XMAXR = SQRT( 2*A**2 + C**2 )

      TOL   = VTIGHT

      CALL CHCKSD ( 'MAXRAD', MAXRAD, '~/', XMAXR, TOL, OK )


C
C     The segment's central body is inside the segment's bounding box;
C     the expected minimum radius is zero.
C
      XMINR = 0.D0

      TOL   = VTIGHT

      CALL CHCKSD ( 'MINRAD', MINRAD, '~/', XMINR, TOL, OK )
      

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE( 'Repeat Saturn rectangular case; '
     .//          're-use segment list.')


      CALL ZZDSKSPH ( BODYID, NSURF, SRFLST, MINRAD, MAXRAD )
      CALL CHCKXC   ( .FALSE., ' ', OK )      

      CALL CHCKSD ( 'MAXRAD', MAXRAD, '~/', XMAXR, TOL, OK )
      CALL CHCKSD ( 'MINRAD', MINRAD, '~/', XMINR, TOL, OK )


C
C     Check handling of BSR state change.
C
      
C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE( 'Unload Mars DSK and repeat Saturn rectangular '
     .//          'case again; re-use segment list.')


      CALL UNLOAD ( DSK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )      

      CALL ZZDSKSPH ( BODYID, NSURF, SRFLST, MINRAD, MAXRAD )
      CALL CHCKXC   ( .FALSE., ' ', OK )      

      CALL CHCKSD ( 'MAXRAD', MAXRAD, '~/', XMAXR, TOL, OK )
      CALL CHCKSD ( 'MINRAD', MINRAD, '~/', XMINR, TOL, OK )


C
C     Close the Saturn DSK so we can unload it later.
C
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )      
 

C
C     The next set of tests uses frames having centers offset
C     from the central body.
C

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Find bounding radii for Mars segment having '
     .//           'its frame center near Mars'' surface. For this '
     .//           'case, the minimum radius is the maximum segment '
     .//           'radius minus the frame offset.'                  )


      CALL FURNSH ( DSK0 )
      CALL CHCKXC   ( .FALSE., ' ', OK )      


      CALL CLEARI ( MAXSRF, SRFLST )

      BODYID    = 499
      NSURF     = 1
      SRFLST(1) = 4


      CALL ZZDSKSPH ( BODYID, NSURF, SRFLST, MINRAD, MAXRAD )
      CALL CHCKXC   ( .FALSE., ' ', OK )      

C
C     For this case, the upper bound is the sum of the 
C     DSK descriptor's upper radius bound and the magnitude of
C     the offset between the frame's center and Mars.
C
      CALL SPKEZP ( SITIDS(1), 0.D0,   SITFNM(1), 'NONE',
     .              BODYID,    OFFSET, LT                )
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
      

C
C     The minimum radius should be the lower bound in the descriptor,
C     minus the offset magnitude. 
C
C     The minimum radius should be the offset magnitude, minus 
C     the upper bound in the descriptor.
C
      XMINR = OFFMAG - DSKDSC(MX3IDX) 

      TOL   = 0

      CALL CHCKSD ( 'MINRAD', MINRAD, '=', XMINR, TOL, OK )



C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Find bounding radii for Mars segment having '
     .//           'its frame center near Mars'' center. For this '
     .//           'case, the minimum radius is the minimum segment '
     .//           'radius minus the frame offset.'                  )

 

      CALL CLEARI ( MAXSRF, SRFLST )

      BODYID    = 499
      NSURF     = 1
      SRFLST(1) = 5


      CALL ZZDSKSPH ( BODYID, NSURF, SRFLST, MINRAD, MAXRAD )
      CALL CHCKXC   ( .FALSE., ' ', OK )      

C
C     For this case, the upper bound is the sum of the 
C     DSK descriptor's upper radius bound and the magnitude of
C     the offset between the frame's center and Mars.
C
      CALL SPKEZP ( SITIDS(2), 0.D0,   SITFNM(3), 'NONE',
     .              BODYID,    OFFSET, LT                )
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
      

C
C     The minimum radius should be the lower bound in the descriptor,
C     minus the offset magnitude. 
C
C     The minimum radius should be the offset magnitude, minus 
C     the upper bound in the descriptor.
C
      XMINR = DSKDSC(MN3IDX) - OFFMAG

      TOL   = 0

      CALL CHCKSD ( 'MINRAD', MINRAD, '=', XMINR, TOL, OK )



C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Find bounding radii for Mars segment having '
     .//           'its frame center slightly below Mars'' surface. '
     .//           'For this case, the minimum radius is zero.'       )

 

      CALL CLEARI ( MAXSRF, SRFLST )

      BODYID    = 499
      NSURF     = 1
      SRFLST(1) = 6


      CALL ZZDSKSPH ( BODYID, NSURF, SRFLST, MINRAD, MAXRAD )
      CALL CHCKXC   ( .FALSE., ' ', OK )      

C
C     For this case, the upper bound is the sum of the 
C     DSK descriptor's upper radius bound and the magnitude of
C     the offset between the frame's center and Mars.
C
      CALL SPKEZP ( SITIDS(3), 0.D0,   SITFNM(3), 'NONE',
     .              BODYID,    OFFSET, LT                )
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
      

C
C     The frame center offset is between the segment's minimum
C     and maximum radius, so we can't guarantee that the frame
C     center is a positive distance from all plates.
C
      XMINR = 0.D0

      TOL   = 0

      CALL CHCKSD ( 'MINRAD', MINRAD, '=', XMINR, TOL, OK )
       
C
C     Close the Mars DSK so we can unload it later.
C
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )      


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Find Mars radius bounds using empty surface list.' )

      BODYID    = 499
      NSURF     = 0
      
      CALL CLEARI ( MAXSRF, SRFLST )


      CALL ZZDSKSPH ( BODYID, NSURF, SRFLST, MINRAD, MAXRAD )
      CALL CHCKXC   ( .FALSE., ' ', OK )      

C
C     Generate expected bounds by combining bounds for each surface.
C
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
       




C**********************************************************************
C
C     Error cases
C
C**********************************************************************


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Error: try to find bounds for body having '
     .//           'no loaded DSK segments.'                   )


      BODYID = 299

      NSURF  = 0
      CALL CLEARI ( MAXSRF, SRFLST )

      CALL ZZDSKSPH ( BODYID, NSURF, SRFLST, MINRAD, MAXRAD )
      CALL CHCKXC   ( .TRUE., 'SPICE(DSKDATANOTFOUND)', OK )      



C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Error: find bounds for Mars. Unload Mars DSK. '
     .//           'Try again to find bounds for Mars.'            )


      BODYID = 499
      NSURF  = 0

      CALL ZZDSKSPH ( BODYID, NSURF, SRFLST, MINRAD, MAXRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL UNLOAD ( DSK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
      CALL ZZDSKSPH ( BODYID, NSURF, SRFLST, MINRAD, MAXRAD )
      CALL CHCKXC   ( .TRUE., 'SPICE(DSKDATANOTFOUND)', OK )    


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Reload Mars DSK. '
     .//           'Unload Mars SPK. Try again to find bounds for '
     .//           'Mars.'                                         )

      CALL FURNSH ( DSK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL UNLOAD ( TOPSPK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZDSKSPH ( BODYID, NSURF, SRFLST, MINRAD, MAXRAD )
      CALL CHCKXC   ( .TRUE., 'SPICE(NOLOADEDFILES)', OK )    


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Reload Mars SPK. '
     .//           'Unload Mars FK. Try again to find bounds for '
     .//           'Mars.'                                         )

      CALL FURNSH ( TOPSPK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL UNLOAD ( TOPFK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZDSKSPH ( BODYID, NSURF, SRFLST, MINRAD, MAXRAD )
      CALL CHCKXC   ( .TRUE., 'SPICE(NOFRAMEDATA)', OK )    

 

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Reload FK.  Try again to find bounds '
     .//           'for Mars. This is a non-error case.'              )


      CALL FURNSH ( TOPFK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZDSKSPH ( BODYID, NSURF, SRFLST, MINRAD, MAXRAD )
      CALL CHCKXC   ( .FALSE., ' ', OK )    


C
C     Generate expected bounds by combining bounds for each surface.
C
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


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Repeat previous case, minus the kernel loads.' )



      CALL ZZDSKSPH ( BODYID, NSURF, SRFLST, MINRAD, MAXRAD )
      CALL CHCKXC   ( .FALSE., ' ', OK )    


C
C     Generate expected bounds by combining bounds for each surface.
C
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
       


C**********************************************************************
C
C     Clean up
C
C**********************************************************************

C
C --- Case: ------------------------------------------------------
C 
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


C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END


