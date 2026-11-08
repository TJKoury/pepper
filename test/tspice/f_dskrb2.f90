!$Procedure      F_DSKRB2 ( Test DSKRB2 )

      SUBROUTINE F_DSKRB2 ( OK )

! $ Abstract
!
!     Test the SPICELIB routine DSKRB2.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
!
! $ Required_Reading
!
!     DSK
!
! $ Keywords
!
!     TEST
!     UTILITY
!
! $ Declarations

      IMPLICIT NONE
      LOGICAL               OK

      INCLUDE 'dla.inc'
      INCLUDE 'dsk02.inc'
      INCLUDE 'dskdsc.inc'

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
!     trapped or unintentional and need reporting. The test family
!     utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine tests the SPICELIB routine DSKRB2.
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
!-    TSPICE Version 1.0.0, 09-AUG-2016  (NJB)
!
!-&


!
!     Test Utility Functions
!


!
!     SPICELIB functions
!
      DOUBLE PRECISION      DPMAX
      DOUBLE PRECISION      DPMIN
      DOUBLE PRECISION      VDIST
      DOUBLE PRECISION      VNORM

      LOGICAL               EXISTS

!
!     Local parameters
!
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

!
!     Local Variables
!
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

!
!     Saved variables
!
      SAVE

!
!     Initial values
!
      DATA                  ORIGIN / 3 * 0.D0 /



!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_DSKRB2' )


!***********************************************************************
!
!     Normal cases
!
!***********************************************************************

!
!     Loop over the set of shapes.
!
      DO SHAPIX = 1, NSHAPE

!
! --- Case --------------------------------------------------------
!
         IF ( SHAPIX .EQ. 1 ) THEN

            CALL TCASE ( 'Set-up: create plates for a tessellated ' &
            //           'ellipsoid. NLON = 100; NLAT = 50; NV = ' &
            //           '4902; NP = 9800.'                       )

            A = 3000.D0
            B = A
            C = 1000.D0

            NLON = 100
            NLAT = 50

            CALL ZZELLPLT ( A,    B,  C,     NLON, NLAT,  MAXV, &
                            MAXP, NV, VERTS, NP,   PLATES      )
            CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case --------------------------------------------------------
!


         ELSE IF ( SHAPIX .EQ. 2 ) THEN
!
!           Create a plate set consisting of a tessellated torus.
!           We'll create a DSK file containing the model, then
!           we'll extract the vertices and plates. We don't need
!           the file; it's just simpler this way.
!

            CALL TCASE ( 'Set-up: create plates for a tessellated ' &
            //           'torus.'                                  )


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

            CALL T_TORUS ( BODYID, SURFID, FIXREF, NPOLYV, NCROSS, &
                           R,      RCROSS, CENTER, NORMAL, DSK0   )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Extract the plate set from the DSK we just created.
!
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



!
! --- Case --------------------------------------------------------
!

         CALL TCASE ( 'Check the radius extents of the plate set.' )


         CORSYS = LATSYS
         CALL CLEARD ( NSYPAR, CORPAR )

         CALL DSKRB2 ( NV,     VERTS,  NP,     PLATES, &
                       CORSYS, CORPAR, MNCOR3, MXCOR3  )
         CALL CHCKXC ( .FALSE., ' ', OK )


!
!        Find the expected radius extents.
!
         XMAX = DPMIN()
         XMIN = DPMAX()

!
!        The maximum radius occurs at a vertex.
!
         DO I = 1, NV

            XMAX = MAX ( XMAX, VNORM(VERTS(1,I)) )

         END DO
!
!        The minimum radius could occur in the interior of
!        a plate. We must find the minimum distance of each
!        plate from the origin.
!
         DO I = 1, NP

            CALL PLTNP ( ORIGIN, VERTS(1,PLATES(1,I)), &
                                 VERTS(1,PLATES(2,I)), &
                                 VERTS(1,PLATES(3,I)), PNEAR, DIST )
            CALL CHCKXC ( .FALSE., ' ', OK )


            XMIN  = MIN ( XMIN, DIST )

         END DO


         CALL CHCKSD ( 'MNCOR3', MNCOR3, '~/', XMIN, TOL, OK )
         CALL CHCKSD ( 'MXCOR3', MXCOR3, '~/', XMAX, TOL, OK )




!
! --- Case --------------------------------------------------------
!
         CALL TCASE ( 'Check the Z extents of the plate set.' )

!
!        The coordinate system is rectangular.
!

         CORSYS = RECSYS
         CALL CLEARD ( NSYPAR, CORPAR )

         CALL DSKRB2 ( NV,     VERTS,  NP,     PLATES, &
                       CORSYS, CORPAR, MNCOR3, MXCOR3  )
         CALL CHCKXC ( .FALSE., ' ', OK )


!
!        Find the expected Z extents. The extreme values occur at
!        vertices.
!
         XMAX = DPMIN()
         XMIN = DPMAX()

         DO I = 1, NV

            XMIN = MIN ( XMIN, VERTS(3,I) )
            XMAX = MAX ( XMAX, VERTS(3,I) )

         END DO

        CALL CHCKSD ( 'MNCOR3', MNCOR3, '~/', XMIN, TOL, OK )
        CALL CHCKSD ( 'MXCOR3', MXCOR3, '~/', XMAX, TOL, OK )



!
! --- Case --------------------------------------------------------
!
         CALL TCASE ( 'Check the altitude extents of the plate set. ' &
         //           'Use the spheroid parameters that were used ' &
         //           'to create the tessellated spheroid.'         )

!
!        The coordinate system is rectangular.
!
         CORSYS = PDTSYS
         CALL CLEARD ( NSYPAR, CORPAR )

         CORPAR(1) =   A
         F         = ( A - C ) / A
         CORPAR(2) =   F


         CALL DSKRB2 ( NV,     VERTS,  NP,     PLATES, &
                       CORSYS, CORPAR, MNCOR3, MXCOR3  )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Find the expected altitude extents. The maximum
!        values occur at vertices The minimum values can
!        occur with plates' interiors.
!
         XMAX = DPMIN()
         XMIN = DPMAX()

         DO I = 1, NV

            CALL RECGEO ( VERTS(1,I), A, F, LON, LAT, ALT )
            CALL CHCKXC ( .FALSE., ' ', OK )

            XMAX = MAX ( XMAX, ALT )

         END DO

!
!        Minimum altitudes are a little more complicated. We don't
!        compute exact minima; we simply compute a reasonable lower
!        bound for the altitude of each plate. This is done by finding
!        the altitude of the plate's center and subtracting the maximum
!        distance from the center of the plate's vertices.
!
         DO I = 1, NP

            S = 1.D0 / 3.D0

            CALL VLCOM3 ( S, VERTS(1,PLATES(1,I)), &
                          S, VERTS(1,PLATES(2,I)), &
                          S, VERTS(1,PLATES(3,I)),  PLTCTR )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL RECGEO ( PLTCTR, A, F, LON, LAT, ALT )
            CALL CHCKXC ( .FALSE., ' ', OK )

            DIST = MAX ( VDIST( PLTCTR, VERTS(1,PLATES(1,I)) ), &
                         VDIST( PLTCTR, VERTS(1,PLATES(2,I)) ), &
                         VDIST( PLTCTR, VERTS(1,PLATES(3,I)) ) )


            XMIN = MIN ( XMIN, ALT-DIST )

         END DO


!
!        Note: use absolute comparisons for the ellipsoid case,
!        since the expected value may be zero.
!
         TOL = TIGHT

         IF ( SHAPIX .EQ. 1 ) THEN

            CALL CHCKSD ( 'MNCOR3', MNCOR3, '~',  XMIN, TOL, OK )
            CALL CHCKSD ( 'MXCOR3', MXCOR3, '~',  XMAX, TOL, OK )

         ELSE

            CALL CHCKSD ( 'MNCOR3', MNCOR3, '~/', XMIN, TOL, OK )
            CALL CHCKSD ( 'MXCOR3', MXCOR3, '~/', XMAX, TOL, OK )

         END IF




      END DO






!***********************************************************************
!
!     Error cases
!
!***********************************************************************


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Bad coordinate system.'  )


      CALL DSKRB2 ( NV,  VERTS,  NP,     PLATES, &
                    0,   CORPAR, MNCOR3, MXCOR3  )
      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Bad coordinate parameters.'  )

      CORSYS    = PDTSYS
      CORPAR(1) = 0.D0

      CALL DSKRB2 ( NV,     VERTS,  NP,     PLATES, &
                    CORSYS, CORPAR, MNCOR3, MXCOR3  )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


      CORSYS    = PDTSYS
      CORPAR(1) = A
      CORPAR(2) = 2.D0

      CALL DSKRB2 ( NV,     VERTS,  NP,     PLATES, &
                    CORSYS, CORPAR, MNCOR3, MXCOR3  )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )



!***********************************************************************
!
!     Clean up.
!
!***********************************************************************


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Clean up.' )

      CALL KCLEAR
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( DSK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL T_SUCCESS ( OK )
      RETURN
      END
