C$Procedure      F_DSKRB2 ( Test DSKRB2 )
 
      SUBROUTINE F_DSKRB2 ( OK )

C$ Abstract
C
C     Test the SPICELIB routine DSKRB2.
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
C     DSK
C
C$ Keywords
C
C     TEST 
C     UTILITY
C
C$ Declarations
 
      IMPLICIT NONE 
      LOGICAL               OK

      INCLUDE 'dla.inc'
      INCLUDE 'dsk02.inc'
      INCLUDE 'dskdsc.inc'
 
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
C     trapped or unintentional and need reporting. The test family
C     utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine tests the SPICELIB routine DSKRB2.
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
C-    TSPICE Version 1.0.0, 09-AUG-2016  (NJB)
C
C-&
 
 
C
C     Test Utility Functions
C
 

C
C     SPICELIB functions
C
      DOUBLE PRECISION      DPMAX
      DOUBLE PRECISION      DPMIN
      DOUBLE PRECISION      VDIST
      DOUBLE PRECISION      VNORM

      LOGICAL               EXISTS

C
C     Local parameters
C
      CHARACTER*(*)         DSK0
      PARAMETER           ( DSK0   = 'dskrb2_test.bds' )

      
      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-12 )


      INTEGER               MAXV
      PARAMETER           ( MAXV   = 10000 )

      INTEGER               MAXP
      PARAMETER           ( MAXP   = 2 * MAXV )

      INTEGER               NSHAPE
      PARAMETER           ( NSHAPE = 2 )

      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 32 )

C
C     Local Variables
C  
      CHARACTER*(NAMLEN)    FIXREF

      DOUBLE PRECISION      A
      DOUBLE PRECISION      ALT
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      CENTER ( 3 )
      DOUBLE PRECISION      CORPAR ( NSYPAR )
      DOUBLE PRECISION      DIST
      DOUBLE PRECISION      F
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      MXCOR3
      DOUBLE PRECISION      MNCOR3
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      ORIGIN ( 3 )
      DOUBLE PRECISION      PLTCTR ( 3 )
      DOUBLE PRECISION      PNEAR  ( 3 )
      DOUBLE PRECISION      R
      DOUBLE PRECISION      RCROSS
      DOUBLE PRECISION      S
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      VERTS  ( 3, MAXV  )
      DOUBLE PRECISION      XMAX
      DOUBLE PRECISION      XMIN

      INTEGER               BODYID
      INTEGER               CORSYS
      INTEGER               DLADSC ( DLADSZ )
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               PLATES ( 3, MAXP )
      INTEGER               N
      INTEGER               NCROSS
      INTEGER               NLAT
      INTEGER               NLON
      INTEGER               NP
      INTEGER               NPOLYV
      INTEGER               NV
      INTEGER               SHAPIX
      INTEGER               SURFID

      LOGICAL               FOUND

C
C     Saved variables
C
      SAVE

C
C     Initial values
C
      DATA                  ORIGIN / 3 * 0.D0 /



C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_DSKRB2' )


C***********************************************************************
C
C     Normal cases
C
C***********************************************************************

C
C     Loop over the set of shapes.
C     
      DO SHAPIX = 1, NSHAPE

C
C --- Case --------------------------------------------------------
C
         IF ( SHAPIX .EQ. 1 ) THEN
         
            CALL TCASE ( 'Set-up: create plates for a tessellated '
     .      //           'ellipsoid. NLON = 100; NLAT = 50; NV = '
     .      //           '4902; NP = 9800.'                       )
  
            A = 3000.D0
            B = A
            C = 1000.D0

            NLON = 100
            NLAT = 50

            CALL ZZELLPLT ( A,    B,  C,     NLON, NLAT,  MAXV,
     .                      MAXP, NV, VERTS, NP,   PLATES      )
            CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case --------------------------------------------------------
C

 
         ELSE IF ( SHAPIX .EQ. 2 ) THEN
C
C           Create a plate set consisting of a tessellated torus.
C           We'll create a DSK file containing the model, then
C           we'll extract the vertices and plates. We don't need
C           the file; it's just simpler this way.
C

            CALL TCASE ( 'Set-up: create plates for a tessellated '
     .      //           'torus.'                                  )
  

            BODYID =  401
            SURFID =    1
            FIXREF = 'IAU_PHOBOS'
            NPOLYV =  100
            NCROSS =  100
            R      = 3000.D0
            RCROSS =  300.D0
            
            CALL VPACK ( 3.D4, 4.D4, 5.D4, CENTER )
            CALL VPACK ( 0.D0, 0.D0, 1.D0, NORMAL )


            IF ( EXISTS(DSK0) ) THEN
               CALL DELFIL( DSK0 )
               CALL CHCKXC ( .FALSE., ' ', OK )
            END IF

            CALL T_TORUS ( BODYID, SURFID, FIXREF, NPOLYV, NCROSS,
     .                     R,      RCROSS, CENTER, NORMAL, DSK0   )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Extract the plate set from the DSK we just created.
C           
            CALL DASOPR ( DSK0, HANDLE )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL DLABFS ( HANDLE, DLADSC, FOUND )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSL ( 'DLABFS found', FOUND, .TRUE., OK )

            CALL DSKZ02 ( HANDLE, DLADSC, NV, NP )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL DSKV02 ( HANDLE, DLADSC, 1, NV, N, VERTS  )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL DSKP02 ( HANDLE, DLADSC, 1, NP, N, PLATES )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL DASCLS ( HANDLE )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF

          

C
C --- Case --------------------------------------------------------
C

         CALL TCASE ( 'Check the radius extents of the plate set.' )
  

         CORSYS = LATSYS
         CALL CLEARD ( NSYPAR, CORPAR )

         CALL DSKRB2 ( NV,     VERTS,  NP,     PLATES, 
     .                 CORSYS, CORPAR, MNCOR3, MXCOR3  )
         CALL CHCKXC ( .FALSE., ' ', OK )


C
C        Find the expected radius extents.
C
         XMAX = DPMIN()
         XMIN = DPMAX()
         
C
C        The maximum radius occurs at a vertex.
C
         DO I = 1, NV
            
            XMAX = MAX ( XMAX, VNORM(VERTS(1,I)) )

         END DO
C
C        The minimum radius could occur in the interior of
C        a plate. We must find the minimum distance of each
C        plate from the origin.
C
         DO I = 1, NP

            CALL PLTNP ( ORIGIN, VERTS(1,PLATES(1,I)),
     .                           VERTS(1,PLATES(2,I)),
     .                           VERTS(1,PLATES(3,I)), PNEAR, DIST )
            CALL CHCKXC ( .FALSE., ' ', OK )


            XMIN  = MIN ( XMIN, DIST )

         END DO


         CALL CHCKSD ( 'MNCOR3', MNCOR3, '~/', XMIN, TOL, OK )
         CALL CHCKSD ( 'MXCOR3', MXCOR3, '~/', XMAX, TOL, OK )




C
C --- Case --------------------------------------------------------
C
         CALL TCASE ( 'Check the Z extents of the plate set.' )

C
C        The coordinate system is rectangular.
C        

         CORSYS = RECSYS
         CALL CLEARD ( NSYPAR, CORPAR )

         CALL DSKRB2 ( NV,     VERTS,  NP,     PLATES, 
     .                 CORSYS, CORPAR, MNCOR3, MXCOR3  )
         CALL CHCKXC ( .FALSE., ' ', OK )


C
C        Find the expected Z extents. The extreme values occur at
C        vertices.
C
         XMAX = DPMIN()
         XMIN = DPMAX()
         
         DO I = 1, NV

            XMIN = MIN ( XMIN, VERTS(3,I) )
            XMAX = MAX ( XMAX, VERTS(3,I) )

         END DO

        CALL CHCKSD ( 'MNCOR3', MNCOR3, '~/', XMIN, TOL, OK )
        CALL CHCKSD ( 'MXCOR3', MXCOR3, '~/', XMAX, TOL, OK )



C
C --- Case --------------------------------------------------------
C
         CALL TCASE ( 'Check the altitude extents of the plate set. '
     .   //           'Use the spheroid parameters that were used '
     .   //           'to create the tessellated spheroid.'         )

C
C        The coordinate system is rectangular.
C        
         CORSYS = PDTSYS
         CALL CLEARD ( NSYPAR, CORPAR )

         CORPAR(1) =   A
         F         = ( A - C ) / A
         CORPAR(2) =   F


         CALL DSKRB2 ( NV,     VERTS,  NP,     PLATES, 
     .                 CORSYS, CORPAR, MNCOR3, MXCOR3  )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Find the expected altitude extents. The maximum 
C        values occur at vertices The minimum values can
C        occur with plates' interiors.
C
         XMAX = DPMIN()
         XMIN = DPMAX()
         
         DO I = 1, NV

            CALL RECGEO ( VERTS(1,I), A, F, LON, LAT, ALT )
            CALL CHCKXC ( .FALSE., ' ', OK )

            XMAX = MAX ( XMAX, ALT )

         END DO
        
C
C        Minimum altitudes are a little more complicated. We don't
C        compute exact minima; we simply compute a reasonable lower
C        bound for the altitude of each plate. This is done by finding
C        the altitude of the plate's center and subtracting the maximum
C        distance from the center of the plate's vertices.
C
         DO I = 1, NP

            S = 1.D0 / 3.D0

            CALL VLCOM3 ( S, VERTS(1,PLATES(1,I)),
     .                    S, VERTS(1,PLATES(2,I)),
     .                    S, VERTS(1,PLATES(3,I)),  PLTCTR )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL RECGEO ( PLTCTR, A, F, LON, LAT, ALT ) 
            CALL CHCKXC ( .FALSE., ' ', OK )

            DIST = MAX ( VDIST( PLTCTR, VERTS(1,PLATES(1,I)) ),
     .                   VDIST( PLTCTR, VERTS(1,PLATES(2,I)) ),
     .                   VDIST( PLTCTR, VERTS(1,PLATES(3,I)) ) )

            
            XMIN = MIN ( XMIN, ALT-DIST )

         END DO


C
C        Note: use absolute comparisons for the ellipsoid case, 
C        since the expected value may be zero.
C        
         TOL = TIGHT

         IF ( SHAPIX .EQ. 1 ) THEN

            CALL CHCKSD ( 'MNCOR3', MNCOR3, '~',  XMIN, TOL, OK )
            CALL CHCKSD ( 'MXCOR3', MXCOR3, '~',  XMAX, TOL, OK )

         ELSE

            CALL CHCKSD ( 'MNCOR3', MNCOR3, '~/', XMIN, TOL, OK )
            CALL CHCKSD ( 'MXCOR3', MXCOR3, '~/', XMAX, TOL, OK )

         END IF

          


      END DO


  

         

C***********************************************************************
C
C     Error cases
C
C***********************************************************************


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Bad coordinate system.'  )


      CALL DSKRB2 ( NV,  VERTS,  NP,     PLATES, 
     .              0,   CORPAR, MNCOR3, MXCOR3  )
      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Bad coordinate parameters.'  )

      CORSYS    = PDTSYS
      CORPAR(1) = 0.D0

      CALL DSKRB2 ( NV,     VERTS,  NP,     PLATES, 
     .              CORSYS, CORPAR, MNCOR3, MXCOR3  )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


      CORSYS    = PDTSYS
      CORPAR(1) = A
      CORPAR(2) = 2.D0

      CALL DSKRB2 ( NV,     VERTS,  NP,     PLATES, 
     .              CORSYS, CORPAR, MNCOR3, MXCOR3  )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )
 


C***********************************************************************
C
C     Clean up.
C
C***********************************************************************


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Clean up.' )

      CALL KCLEAR 
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( DSK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL T_SUCCESS ( OK )
      RETURN
      END
