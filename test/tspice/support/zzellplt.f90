!$Procedure ZZELLPLT ( Tessellate an ellipsoid with triangular plates )

      SUBROUTINE ZZELLPLT ( A,    B,  C,     NLON, NLAT,  MAXV, &
                            MAXP, NV, VERTS, NP,   PLATES      )

! $ Abstract
!
!     Create a set of triangular plates covering a specified triaxial
!     ellipsoid.
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
!     MAXV       is the maximum number of vertices to return. MAXV must
!                be at least
!
!                   ( NLON * ( NLAT - 1 ) )  +  2
!
!                The array VERTS must have size at least 3*MAXV.
!
!
!     MAXP       is the maximum number of plates to return. MAXP must
!                be at least
!
!                   2 * NLON * ( NLAT - 1 )
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
!                indices NV and NV-1, respectively. The non-polar
!                vertex indices start at 1. Non-polar vertices are
!                indexed in top-down, left-to-right order, with
!                vertices of each latitude band stored contiguously.
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
!-    TESTUTIL Version 1.0.0, 30-APR-2014 (NJB)
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
      DOUBLE PRECISION      PI

      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local variables
!
      DOUBLE PRECISION      DIR    ( 3 )
      DOUBLE PRECISION      DLAT
      DOUBLE PRECISION      DLON
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LEVEL
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      S

      INTEGER               BIX
      INTEGER               I
      INTEGER               J
      INTEGER               N
      INTEGER               NNP
      INTEGER               NSP
      INTEGER               PIX
      INTEGER               VIX


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZELLPLT' )

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
         CALL CHKOUT ( 'ZZELLPLT'                                    )
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
         CALL CHKOUT ( 'ZZELLPLT'                                    )
         RETURN

      END IF

      IF ( NLON .LT. 3 ) THEN

         CALL SETMSG ( 'The longitude band count must be at least ' &
         //            '3 but was #.'                              )
         CALL ERRINT ( '#', NLON                                   )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                       )
         CALL CHKOUT ( 'ZZELLPLT'                                    )
         RETURN

      END IF

!
!     Compute the vertex and plate counts. Check against available
!     room.
!
!        Vertex count: there are NLAT-2 latitude bands, excluding
!                      the polar caps. These are bounded by NLAT-1 rows
!                      of vertices. Each row of vertices has NLON
!                      members. The caps add two vertices.
!
!        Plate count:  each latitude band, excluding the polar caps,
!                      contains 2*NLON plates. Each cap contains NLON
!                      plates.
!
!
      NV = ( NLON * ( NLAT - 1 ) )  +  2

      NP = 2 * NLON * ( NLAT - 1 )


      IF ( NV .GT. MAXV ) THEN

         CALL SETMSG ( 'The requested plate model requires # ' &
         //            'vertices but the maximum vertex count is #.' )
         CALL ERRINT ( '#', NV                                       )
         CALL ERRINT ( '#', MAXV                                     )
         CALL SIGERR ( 'SPICE(ARRAYTOOSMALL)'                        )
         CALL CHKOUT ( 'ZZELLPLT'                                      )
         RETURN

      END IF

      IF ( NP .GT. MAXP ) THEN

         CALL SETMSG ( 'The requested plate model requires # ' &
         //            'plates but the maximum plate count is #.' )
         CALL ERRINT ( '#', NP                                    )
         CALL ERRINT ( '#', MAXP                                  )
         CALL SIGERR ( 'SPICE(ARRAYTOOSMALL)'                     )
         CALL CHKOUT ( 'ZZELLPLT'                                   )
         RETURN

      END IF

!
!     Create the vertex set. The north polar vertex is
!     at index 1; the south vertex is at index NV. It will
!     be convenient to make these the last two vertices.
!
      CALL VPACK ( 0.D0, 0.D0,  C, VERTS(1, NV-1) )
      CALL VPACK ( 0.D0, 0.D0, -C, VERTS(1, NV  ) )

!
!     The latitude bands are equally spaced in planetocentric
!     latitude.
!
      DLAT =     PI() / NLAT
      DLON = 2 * PI() / NLON

      VIX  = 1

      DO I = 1, NLAT-1

         LAT = ( PI()/2 ) - ( I * DLAT )

         DO J = 1, NLON

            LON = (J-1) * DLON
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
!     Create the plates for the latitude bounds other than
!     those belonging to the caps.
!
!     The first two inputs are the vertex row and column counts.
!     Next is a logical flag indicating whether longitude wrapping
!     should be used.
!
      IF ( NLAT .GT. 2 ) THEN

         CALL ZZGRDPLT ( NLAT-1, NLON, .TRUE., N, PLATES )

         IF ( FAILED() ) THEN
            CALL CHKOUT( 'ZZELLPLT' )
            RETURN
         END IF

      END IF

!
!     Add the north cap. This is a set of plates; the vertices
!     already have been computed.
!
      PIX = ( NP - 2*NLON ) + 1
      BIX = 0

      CALL ZZCAPPLT ( NLON, .TRUE., .TRUE., &
                      BIX,  NV-1,   NNP,   PLATES(1,PIX) )

      IF ( FAILED() ) THEN
         CALL CHKOUT( 'ZZELLPLT' )
         RETURN
      END IF

!
!     Add the south cap.
!
      PIX = PIX +   NLON
      BIX = NV  - ( NLON + 2 )

      CALL ZZCAPPLT ( NLON, .FALSE., .TRUE., &
                      BIX,  NV,      NSP,    PLATES(1,PIX) )


      CALL CHKOUT ( 'ZZELLPLT' )
      RETURN
      END


