!$Procedure ZZELLSEC ( Tessellate an ellipsoid section with plates )

      SUBROUTINE ZZELLSEC ( A,      B,      C,     MINLON, MAXLON, &
                            MINLAT, MAXLAT, NLON,  NLAT,   MAXV, &
                            MAXP,   NV,     VERTS, NP,     PLATES )
      use pepper_legacy

! $ Abstract
!
!     Create a set of triangular plates covering a specified section
!     of the surface of a triaxial ellipsoid. The boundaries of the
!     section are curves of constant planetocentric longitude and
!     latitude.
!
! $ Required_Reading
!
!     DSK
!
! $ Keywords
!
!     ELLIPSOID
!     PLATE
!     TILE
!     TESSELLATE
!
! $ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      MINLON
      DOUBLE PRECISION      MAXLON
      DOUBLE PRECISION      MINLAT
      DOUBLE PRECISION      MAXLAT
      INTEGER               NLON
      INTEGER               NLAT
      INTEGER               MAXV
      INTEGER               MAXP
      INTEGER               NV
      DOUBLE PRECISION      VERTS  ( 3, * )
      INTEGER               NP
      INTEGER               PLATES ( 3, * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     A          I   Length of ellipsoid semi-axis lying on the x-axis.
!     B          I   Length of ellipsoid semi-axis lying on the y-axis.
!     C          I   Length of ellipsoid semi-axis lying on the z-axis.
!     MINLON     I   Minimum longitude of section.
!     MAXLON     I   Minimum longitude of section.
!     MINLAT     I   Minimum latitude of section.
!     MAXLAT     I   Minimum latitude of section.
!     NLON       I   Number of longitude bands in plate set.
!     NLAT       I   Number of latitude bands in plate set.
!     MAXV       I   Maximum number of vertices to return.
!     MAXP       I   Maximum number of plates to return.
!     NV         O   Number of vertices in output array.
!     VERTS      O   Vertices.
!     NP         O   Number of plates in output array.
!     PLATES     O   Plates.
!
! $ Detailed_Input
!
!     A,
!     B,
!     C          are the lengths of the semi-axes of a triaxial
!                ellipsoid. The ellipsoid is centered at the origin and
!                oriented so that its axes lie on the x, y and z axes.
!                A, B, and C are the lengths of the semi-axes that
!                point in the x, y, and z directions respectively.
!
!
!     MINLON,
!     MAXLON,
!     MINLAT,
!     MAXLAT     are, respectively, the longitude and latitude bounds
!                of a section of the surface the triaxial ellipsoid.
!                The coordinate system is latitudinal. Units are
!                radians.
!
!
!     NLON       is the number of longitude bands in the output plate
!                set. Each longitude band is bounded by two meridians.
!                All longitude bands have equal angular extent in
!                longitude. The vertices of any plate lie on adjacent
!                longitude band boundaries.
!
!
!     NLAT       is the number of latitude bands in the output plate
!                set. The vertices of each band are bounded by two
!                cones of constant planetocentric latitude. All
!                latitude bands have equal angular extent in
!                planetocentric latitude. The vertices of any plate lie
!                on adjacent latitude band boundaries.
!
!                Each polar "cap" consists of one longitude band.
!
!
!     MAXV       is the maximum number of vertices to return. The
!                number of vertices created depends on which polar
!                caps are created. In all cases the number does not
!                exceed
!
!                   ( NLON + 1 ) * ( NLAT + 1 )
!
!                The array VERTS must have size at least 3*MAXV.
!
!
!     MAXP       is the maximum number of plates to return. The
!                number of plates created depends on which polar
!                caps are created and whether longitude wrapping
!                is selected. In all cases the number does not
!                exceed
!
!                   2 * NLON * NLAT
!
!                The array PLATES must have size at least 3*MAXP.
!
! $ Detailed_Output
!
!     NV         is the number of vertices in the output array VERTS.
!
!     VERTS      is an array containing the vertices of the output
!                plate set. There is a vertex at each intersection of a
!                latitude band boundary and a longitude band boundary.
!                The vertices at the north and south poles are at
!                indices NV and NV-1, respectively, if both polar caps
!                are created. The non-polar vertex indices start at 1.
!                Non-polar vertices are indexed in top-down,
!                left-to-right order, with vertices of each latitude
!                band stored contiguously.
!
!     NP         is the number of plates in the output array PLATES.
!
!
!     PLATES     is an array containing the tessellating plate set.
!                Each plate is an array of three vertex indices, where
!                the indices range from 1 to NV.
!
!                The vertices of any plate are ordered in the
!                right-handed sense about the outward normal direction
!                for that plate.
!
!                The non-polar plates---those not having a vertex at
!                the north or south pole---are indexed in top-down,
!                left-to-right order, with plates belonging to each
!                latitude band stored contiguously. Plates constituting
!                the north polar cap follow the non-polar plate set,
!                and plates constituting the south polar cap follow
!                those.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the length of any semi-axis of the ellipsoid is
!         non-positive, the error SPICE(INVALIDAXISLENGTH) is signaled.
!
!     2)  If NLAT is less than 2, the error SPICE(INVALIDCOUNT) is
!         signaled.
!
!     3)  If NLON is less than 3, the error SPICE(INVALIDCOUNT) is
!         signaled.
!
!     4)  If the number of vertices implied by the input values NLON
!         and NLAT exceeds MAXV, the error SPICE(ARRAYTOOSMALL) is
!         signaled.
!
!     5)  If the number of plates implied by the input values NLON
!         and NLAT exceeds MAXP, the error SPICE(ARRAYTOOSMALL) is
!         signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The vertex and plate sets created by this routine are suitable
!     for use in a type 2 DSK segment.
!
!     While the primary purpose of this routine is to support testing,
!     there may be some user applications for which a tessellated plate
!     model is valuable. For example, computing an estimate of the area
!     of a specified surface region may be simplified by using a plate
!     model.
!
!     Note that, for ellipsoids having three distinct radii, the Z
!     components of the vertices on any latitude band boundary (except
!     the poles themselves) will vary with longitude.
!
!     Also note that the horizontal edge of a plate may extend beyond
!     the boundaries of the latitude band containing the plate.
!
! $ Examples
!
!     See use of C language version in tspice_c test families.
!
! $ Restrictions
!
!     1) For use only by TSPICE.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman    (JPL)
!
! $ Version
!
!-    TESTUTIL Version 1.0.0, 29-SEP-2014 (NJB)
!
!        Based on ZZELLPLT Version 1.0.0, 30-APR-2014 (NJB)
!
!-&

! $ Index_Entries
!
!     tessellate ellipsoid
!
!-&

!
!     SPICELIB functions
!


!
!     Local parameters
!
      DOUBLE PRECISION      TOL
      PARAMETER           ( TOL  = 1.D-12 )

!
!     Local variables
!
      DOUBLE PRECISION      DIR    ( 3 )
      DOUBLE PRECISION      DLAT
      DOUBLE PRECISION      DLON
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LEVEL
      DOUBLE PRECISION      LMXLON
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      S

      INTEGER               BIX
      INTEGER               I
      INTEGER               J
      INTEGER               LATLB
      INTEGER               LATUB
      INTEGER               N
      INTEGER               NCOLS
      INTEGER               NNP
      INTEGER               NROWS
      INTEGER               NSP
      INTEGER               PIX
      INTEGER               POLIDX
      INTEGER               VIX

      LOGICAL               NCAP
      LOGICAL               SCAP
      LOGICAL               WRAP

      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZELLSEC' )

!
!     The semi-axes must have positive length.
!
      IF (       ( A .LE. 0.D0 ) &
           .OR.  ( B .LE. 0.D0 ) &
           .OR.  ( C .LE. 0.D0 )   )   THEN

         CALL SETMSG ( 'Semi-axis lengths:  A = #, B = #, C = #. ' )
         CALL ERRDP  ( '#', A                                      )
         CALL ERRDP  ( '#', B                                      )
         CALL ERRDP  ( '#', C                                      )
         CALL SIGERR ( 'SPICE(INVALIDAXISLENGTH)'                  )
         CALL CHKOUT ( 'ZZELLSEC'                                  )
         RETURN

      END IF

!
!     The longitude and latitude band counts must be realizable.
!
      IF ( NLAT .LT. 2 ) THEN

         CALL SETMSG ( 'The latitude band count must be at least ' &
         //            '2 but was #.'                              )
         CALL ERRINT ( '#', NLAT                                   )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                       )
         CALL CHKOUT ( 'ZZELLSEC'                                  )
         RETURN

      END IF

      IF ( NLON .LT. 3 ) THEN

         CALL SETMSG ( 'The longitude band count must be at least ' &
         //            '3 but was #.'                              )
         CALL ERRINT ( '#', NLON                                   )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                       )
         CALL CHKOUT ( 'ZZELLSEC'                                  )
         RETURN

      END IF

!
!     Decide whether we have two distinct longitude boundaries. First
!     create a local maximum longitude that's greater than the minimum
!     longitude. The logical variable WRAP is .TRUE. if and only if we
!     have 2*pi - TOL radians of longitude coverage, where TOL is a
!     small value.
!
      IF ( MAXLON .GT. MINLON ) THEN

         LMXLON = MAXLON
      ELSE
         LMXLON = MAXLON + (2*PI())
      END IF

      WRAP = ( LMXLON - MINLON ) .GT. ( 2*PI() - TOL )

!
!     Decide whether we have north or south polar caps.
!
      NCAP = MAXLAT .GT. ( ( PI()/2) - TOL )
      SCAP = MINLAT .LT. ( (-PI()/2) + TOL )

!
!     Compute the vertex counts.
!
      IF ( WRAP ) THEN
!
!        Vertex count:  When both caps are present, there are NLAT-2
!                       latitude bands, excluding the polar caps. These
!                       are bounded by NLAT-1 rows of vertices. Each
!                       row of vertices has NLON members. The caps add
!                       two vertices.
!
         IF ( NCAP .AND. SCAP ) THEN
!
!           There are two polar caps.
!
            NV = ( NLON * ( NLAT - 1 ) )  +  2

         ELSE IF ( NCAP .OR. SCAP ) THEN
!
!           There's just one polar cap. Excluding the polar
!           vertex, there are NLAT rows of vertices.
!
            NV = ( NLON * NLAT )  +  1

         ELSE
!
!           No polar caps. There are NLAT+1 rows of vertices.
!
            NV = NLON * ( NLAT + 1 )

         END IF

      ELSE

         IF ( NCAP .AND. SCAP ) THEN
!
!           There are two polar caps.
!
            NV = ( (NLON+1) * ( NLAT - 1 ) )  +  2

         ELSE IF ( NCAP .OR. SCAP ) THEN
!
!           There's just one polar cap. Excluding the polar
!           vertex, there are NLAT rows of vertices.
!
            NV = ( (NLON+1) * NLAT )  +  1

         ELSE
!
!           No polar caps. There are NLAT+1 rows of vertices.
!
            NV = ( NLON + 1 ) * ( NLAT + 1 )

         END IF

      END IF


!
!     Compute the plate counts. These depend on the set of
!     polar caps.
!
!
!        Plate count:   each latitude band, excluding the polar caps,
!                       contains 2*NLON plates. Each cap contains NLON
!                       plates.
!
      IF ( NCAP .AND. SCAP ) THEN
!
!        There are two polar caps.
!
         NP = 2 * NLON * ( NLAT - 1 )

      ELSE IF ( NCAP .OR. SCAP ) THEN
!
!        There's just one polar cap. Excluding the polar
!        vertex, there are NLAT rows of vertices.
!
         NP = NLON * ( ( 2 * NLAT ) - 1 )

      ELSE
!
!        No polar caps. There are NLAT+1 rows of vertices.
!
         NP = 2 * NLON * NLAT

      END IF



      IF ( NV .GT. MAXV ) THEN

         CALL SETMSG ( 'The requested plate model requires # ' &
         //            'vertices but the maximum vertex count is #.' )
         CALL ERRINT ( '#', NV                                       )
         CALL ERRINT ( '#', MAXV                                     )
         CALL SIGERR ( 'SPICE(ARRAYTOOSMALL)'                        )
         CALL CHKOUT ( 'ZZELLSEC'                                      )
         RETURN

      END IF

      IF ( NP .GT. MAXP ) THEN

         CALL SETMSG ( 'The requested plate model requires # ' &
         //            'plates but the maximum plate count is #.' )
         CALL ERRINT ( '#', NP                                    )
         CALL ERRINT ( '#', MAXP                                  )
         CALL SIGERR ( 'SPICE(ARRAYTOOSMALL)'                     )
         CALL CHKOUT ( 'ZZELLSEC'                                   )
         RETURN

      END IF

!
!     Create the vertex set, excluding the polar caps.
!
!     LATLB will be the index of the first vertex row, excluding
!     the caps. LATUB will be the index of the last vertex row,
!     excluding the caps.
!
      IF ( NCAP .AND. SCAP ) THEN

         LATLB = 2
         LATUB = NLAT

      ELSE IF ( NCAP ) THEN

         LATLB = 2
         LATUB = NLAT + 1

      ELSE IF ( SCAP ) THEN

         LATLB = 1
         LATUB = NLAT

      ELSE

         LATLB = 1
         LATUB = NLAT + 1

      END IF


!
!     NCOLS is the number of columns of vertices.
!
      IF ( WRAP ) THEN
         NCOLS = NLON
      ELSE
         NCOLS = NLON + 1
      END IF

!
!     The latitude bands are equally spaced in planetocentric latitude.
!
      DLAT = ( MAXLAT - MINLAT ) / NLAT
      DLON = ( LMXLON - MINLON ) / NLON

      VIX  = 1

      DO I = LATLB, LATUB

         LAT = MAXLAT - ( (I-1) * DLAT )

         DO J = 1, NCOLS

            LON = MINLON  +  ( (J-1) * DLON )
!
!           Create a unit direction vector for the current
!           vertex. Scale this vector to make it lie on the
!           ellipsoid's surface; the scaled vector is the
!           current vertex.
!
            CALL LATREC ( 1.D0, LON, LAT, DIR )

            LEVEL = (DIR(1)/A)**2 + (DIR(2)/B)**2 + (DIR(3)/C)**2

            S     = 1.D0 / SQRT( LEVEL )

            CALL VSCL ( S, DIR, VERTS(1,VIX) )
!
!           Next vertex.
!
            VIX   = VIX + 1

         END DO

      END DO

!
!     Create the polar vertices if necessary.
!
      IF ( NCAP .AND. SCAP ) THEN

         CALL VPACK ( 0.D0, 0.D0,  C, VERTS(1, NV-1) )
         CALL VPACK ( 0.D0, 0.D0, -C, VERTS(1, NV  ) )

      ELSE IF ( NCAP ) THEN

         CALL VPACK ( 0.D0, 0.D0,  C, VERTS(1, NV) )

      ELSE IF ( SCAP ) THEN

         CALL VPACK ( 0.D0, 0.D0, -C, VERTS(1, NV) )

      END IF



!
!     Create the plates for the latitude bounds other than
!     those belonging to the caps.
!
!     The first two inputs are the vertex row and column counts.
!     Next is a logical flag indicating whether longitude wrapping
!     should be used.
!
      IF ( LATUB .GT. LATLB ) THEN

         NROWS = LATUB - LATLB + 1

         CALL ZZGRDPLT ( NROWS, NCOLS, WRAP, N, PLATES )

         IF ( FAILED() ) THEN
            CALL CHKOUT( 'ZZELLSEC' )
            RETURN
         END IF

      ELSE

         N = 0

      END IF


      IF ( NCAP ) THEN
!
!        Add the north cap. This is a set of plates; the vertices
!        already have been computed.
!
!        PIX is the index of the first cap plate. BIX is the
!        base (predecessor) index of the first vertex in the
!        first vertex row.
!
         PIX = N + 1
         BIX = 0

!
!        POLIDX is the vertex index of the north polar vertex.
!
         IF ( SCAP ) THEN
            POLIDX = NV - 1
         ELSE
            POLIDX = NV
         END IF

         CALL ZZCAPPLT ( NCOLS, .TRUE., WRAP, &
                         BIX,   POLIDX, NNP,  PLATES(1,PIX) )

         IF ( FAILED() ) THEN
            CALL CHKOUT( 'ZZELLSEC' )
            RETURN
         END IF

      END IF


      IF ( SCAP ) THEN
!
!        Add the south cap.
!
         POLIDX = NV

         IF ( NCAP ) THEN

            PIX = PIX +  NNP
            BIX = NV  - ( NCOLS + 2 )

         ELSE

            PIX = N  + 1
            BIX = NV - ( NCOLS + 1 )

         END IF

         CALL ZZCAPPLT ( NCOLS, .FALSE., WRAP, &
                         BIX,   POLIDX,  NSP,  PLATES(1,PIX) )
      END IF

      CALL CHKOUT ( 'ZZELLSEC' )
      RETURN
      END


