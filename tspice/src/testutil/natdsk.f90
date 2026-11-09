!$Procedure   NATDSK (Create a DSK for Nat's solar system)

      SUBROUTINE NATDSK ( DSK,    AFRAME, ANLON, ANLAT, &
                          BFRAME, BNLON,  BNLAT        )
      use pepper_inc_dskdsc
      use pepper_inc_dsk02

! $ Abstract
!
!     Create and if appropriate load a DSK containing shape data
!     for the objects in Nat's solar system.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TESTING
!
! $ Declarations

      IMPLICIT NONE


      CHARACTER*(*)         DSK
      CHARACTER*(*)         AFRAME
      INTEGER               ANLON
      INTEGER               ANLAT
      CHARACTER*(*)         BFRAME
      INTEGER               BNLON
      INTEGER               BNLAT

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     DSK        I   The name of the DSK file to create.
!     AFRAME     I   Body-fixed frame for body Alpha.
!     ANLON      I   Number of longitude bands in model for Alpha.
!     ANLAT      I   Number of latitude bands in model for Alpha.
!     BFRAME     I   Body-fixed frame for body Beta.
!     BNLON      I   Number of longitude bands in model for Beta.
!     BNLAT      I   Number of latitude bands in model for Beta.
!
! $ Detailed_Input
!
!     DSK        is the name of a DSK file to create. If a DSK
!                of the same name already exists, it is deleted.
!
!     AFRAME     is the name of the body-fixed frame to be used in
!                the segment for body Alpha.
!
!     ANLON      is the number of longitude bands in the plate set for
!                body Alpha. Each longitude band is bounded by two
!                meridians. All longitude bands have equal angular
!                extent in longitude. The vertices of any plate lie on
!                adjacent longitude band boundaries.
!
!     ANLAT      is the number of latitude bands in the plate set for
!                body Alpha. The vertices of each band are bounded by
!                two cones of constant planetocentric latitude. All
!                latitude bands have equal angular extent in
!                planetocentric latitude. The vertices of any plate lie
!                on adjacent latitude band boundaries.
!
!                Each polar "cap" consists of one longitude band.
!
!     BFRAME     is the name of the body-fixed frame to be used in
!                the segment for body Beta.
!
!     BNLON      is the number of longitude bands in the plate set for
!                body Beta. See description of ANLON for details.
!
!     BNLAT      is the number of latitude bands in the plate set for
!                body Beta. See description of ANLAT for details.
!
!
! $ Detailed_Output
!
!     None. This routine creates an output file.
!
! $ Parameters
!
!     The maximum plate and column counts for each segment are
!     defined by the parameters in T_WRDSK2. These values
!     are considerably smaller than those used in dsk02.inc.
!
! $ Exceptions
!
!     1) Any error occurring during the creation of spatial indexes
!        for the segments in the output DSK will be signaled by
!        routines in the call tree of this routine.
!
!     2) Any I/O errors occurring in the creation of the DSK file
!        will be signaled by routines in the call tree of this
!        routine.
!
!     Since this routine is normally used within the TSPICE system,
!     it's up the caller to call CHCKXC to catch errors signaled by
!     this routine.
!
! $ Files
!
!     Input
!     =====
!
!     This routine requires the kernel pool variables defined by
!     the PCK file created by NATPCK. That kernel should be loaded
!     by the caller.
!
!     Output
!     ======
!
!     This routine creates a DSK file meant for use with the SPK file
!     and PCK file created by the respective test utilities NATSPK and
!     NATPCK.
!
!     The DSK file contains tessellated ellipsoid shape models for
!     bodies ALPHA and BETA. Radii for the ellipsoids are obtained from
!     the PCK created by NATPCK.
!
!     Each model is contained in a single segment. The surface IDs
!     for both segments are set to 1. The coordinate systems of both
!     segments are latitudinal.
!
!     The body-fixed frames associated with ALPHA and BETA are specified
!     by the caller.
!
! $ Particulars
!
!     This routine creates a DSK file for use in testing. See the Files
!     section above.
!
!     The DSK created by this routine in particular supports testing
!     the routines
!
!        GFOCLT
!        OCCULT
!
! $ Examples
!
!     The normal way to use this routine is shown below.
!
!        CALL NATPCK ( 'nat.tpc' )
!
!           ...
!
!        CALL NATSPK ( 'nat.bds',
!       .              'ALPHAFIXED', ANLON, ANLAT,
!       .              'BETAFIXED',  BNLON, BNLAT )
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
!-    Test Utilities 1.0.0, 23-MAY-2016 (NJB)
!

!-&

! $ Index_Entries
!
!     Create a "Nat's solar system" DSK file
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      JYEAR
      DOUBLE PRECISION      TWOPI

      LOGICAL               EXISTS
      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local Parameters
!
      INTEGER               ALPHA
      PARAMETER           ( ALPHA  = 1000 )

      INTEGER               BETA
      PARAMETER           ( BETA   = 2000 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

!
!     Local Variables
!
      DOUBLE PRECISION      ARAD   ( 3 )
      DOUBLE PRECISION      BOUNDS ( 2, 2 )
      DOUBLE PRECISION      BRAD   ( 3 )
      DOUBLE PRECISION      CORPAR ( NSYPAR )
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      LAST

      INTEGER               BODYID
      INTEGER               CORSYS
      INTEGER               N
      INTEGER               SURFID

      LOGICAL               MAKVTL

      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'NATDSK' )

!
!     Delete any existing file of the same name.
!
      IF ( EXISTS(DSK) ) THEN

         CALL DELFIL( DSK )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'NATDSK' )
            RETURN
         END IF

      END IF

!
!     Fetch body radii.
!
      CALL BODVCD ( ALPHA, 'RADII', 3, N, ARAD )
      CALL BODVCD ( BETA,  'RADII', 3, N, BRAD )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'NATDSK' )
         RETURN
      END IF

!
!     Create a tessellated plate model segment for body
!     Alpha.
!
      BODYID = ALPHA
      SURFID = 1
      FIRST  = -100 * JYEAR()
      LAST   =  100 * JYEAR()
      CORSYS = LATSYS

      CALL CLEARD ( NSYPAR, CORPAR )

!
!     Set longitude and latitude bounds for the segment.
!
      BOUNDS(1,1) =  0.D0
      BOUNDS(2,1) =  TWOPI()

      BOUNDS(1,2) = -HALFPI()
      BOUNDS(2,2) =  HALFPI()

!
!     Don't make a vertex-plate list.
!
      MAKVTL = .FALSE.

      CALL T_ELDSK2 ( BODYID,  SURFID, AFRAME, FIRST,   LAST, &
                      CORSYS,  CORPAR, BOUNDS, ARAD(1), ARAD(2), &
                      ARAD(3), ANLON,  ANLAT,  MAKVTL,  DSK     )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'NATDSK' )
         RETURN
      END IF

!
!     Append to the DSK we just created a segment for body Beta.
!
      BODYID = BETA
      SURFID = 1
      FIRST  = -100 * JYEAR()
      LAST   =  100 * JYEAR()
      CORSYS = LATSYS

      CALL CLEARD ( NSYPAR, CORPAR )

!
!     Set longitude and latitude bounds for the segment.
!
      BOUNDS(1,1) =  0.D0
      BOUNDS(2,1) =  TWOPI()

      BOUNDS(1,2) = -HALFPI()
      BOUNDS(2,2) =  HALFPI()

!
!     Don't make a vertex-plate list.
!
      MAKVTL = .FALSE.

      CALL T_ELDSK2 ( BODYID,  SURFID, BFRAME, FIRST,   LAST, &
                      CORSYS,  CORPAR, BOUNDS, BRAD(1), BRAD(2), &
                      BRAD(3), BNLON,  BNLAT,  MAKVTL,  DSK     )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'NATDSK' )
         RETURN
      END IF


      CALL CHKOUT ( 'NATDSK' )
      RETURN
      END
