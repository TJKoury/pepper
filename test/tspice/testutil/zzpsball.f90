!$Procedure ZZPSBALL ( Plate set, create ball )

      SUBROUTINE ZZPSBALL ( R, NLON, NLAT, VOUT, POUT )

! $ Abstract
!
!     Create a plate set representing a sphere centered at the origin.
!     The radius of the sphere is specified by the caller.
!
! $ Required_Reading
!
!     CELLS
!     DSK
!
! $ Keywords
!
!     DSK
!     PLATE
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      DOUBLE PRECISION      R
      INTEGER               NLON
      INTEGER               NLAT
      DOUBLE PRECISION      VOUT ( LBCELL : * )
      INTEGER               POUT ( LBCELL : * )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     R          I   Radius of sphere.
!     NLON       I   Number of longitude bands of plates.
!     NLAT       I   Number of latitude bands of plates.
!     VOUT       O   Vertex cell of the plate set.
!     POUT       O   Plate cell of the plate set.
!
! $ Detailed_Input
!
!     R              is the radius of the sphere to be represented
!                    by a collection of plates.
!
!     NLON,
!     NLAT           are, respectively, the number of longitude and
!                    latitude bands of plates representing the
!                    sphere's surface.
!
!                    The vertices of the plate set are located on
!                    NLON equally spaced (in angular measure) meridians
!                    and NLAT parallels, with an additional vertex at
!                    each pole.
!
! $ Detailed_Output
!
!     VOUT           is a SPICELIB double precision cell containing the
!                    vertices of a plate set representing the surface
!                    of a sphere having radius R. The sphere is
!                    centered at the origin.
!
!                    The vertex cell VOUT contains
!
!                       ( NLON * (NLAT-1) )  +  2
!
!                    vertices.
!
!
!     POUT           is a SPICELIB integer cell containing the plates
!                    of the plate set. The vertices of each plate are
!                    ordered so that they define an outward normal
!                    vector having positive dot product with the
!                    plate's vertices. (In other words, the normal
!                    vectors point outward.)
!
!                    The plate cell POUT contains
!
!                       2 * NLON * (NLAT-1)
!
!                    plates.
!
!
! $ Parameters
!
!    LBCELL is the lower bound of the SPICELIB cell data structure.
!
! $ Exceptions
!
!     1) If the size of VOUT is too small to contain the output
!        plate set's vertices, the error will be diagnosed by
!        routines in the call tree of this routine.
!
!     2) If the size of POUT is too small to contain the output plate
!        set's plates, the error will be diagnosed by routines in the
!        call tree of this routine.
!
!     3) Any other errors that occur while accessing SPICE cells
!        will be diagnosed by routines in the call tree of this
!        routine.
!
!     4) If the radius is non-positive, the error will be diagnosed by
!        routines in the call tree of this routine.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     None.
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
!     N.J. Bachman    (JPL)
!
! $ Version
!
!-    DSKLIB Version 1.0.0, 03-SEP-2014 (NJB)
!
!-&



      INTEGER               SIZED
      INTEGER               SIZEI

      LOGICAL               FAILED
      LOGICAL               RETURN


      INTEGER               MAXP
      INTEGER               MAXV
      INTEGER               NP
      INTEGER               NV


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN( 'ZZPSBALL' )


      MAXV = SIZED( VOUT ) / 3
      MAXP = SIZEI( POUT ) / 3

!
!     From the header of ZZELLPLT:
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

      CALL ZZELLPLT ( R,    R,  R,       NLON, NLAT,   MAXV, &
                      MAXP, NV, VOUT(1), NP,   POUT(1)      )


      IF ( FAILED() ) THEN
         CALL CHKOUT( 'ZZPSBALL' )
         RETURN
      END IF

      CALL SCARDD ( 3*NV,  VOUT )
      CALL SCARDI ( 3*NP,  POUT )

      CALL CHKOUT ( 'ZZPSBALL' )
      RETURN
      END
