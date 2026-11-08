C$Procedure   NATDSK (Create a DSK for Nat's solar system)
 
      SUBROUTINE NATDSK ( DSK,    AFRAME, ANLON, ANLAT,
     .                    BFRAME, BNLON,  BNLAT        )
 
C$ Abstract
C
C     Create and if appropriate load a DSK containing shape data
C     for the objects in Nat's solar system.
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
C     TESTING
C
C$ Declarations
 
      IMPLICIT NONE

      INCLUDE 'dskdsc.inc'
      INCLUDE 'dsk02.inc'

      CHARACTER*(*)         DSK
      CHARACTER*(*)         AFRAME
      INTEGER               ANLON
      INTEGER               ANLAT
      CHARACTER*(*)         BFRAME
      INTEGER               BNLON
      INTEGER               BNLAT

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     DSK        I   The name of the DSK file to create.
C     AFRAME     I   Body-fixed frame for body Alpha.
C     ANLON      I   Number of longitude bands in model for Alpha.
C     ANLAT      I   Number of latitude bands in model for Alpha.
C     BFRAME     I   Body-fixed frame for body Beta.
C     BNLON      I   Number of longitude bands in model for Beta.
C     BNLAT      I   Number of latitude bands in model for Beta.
C     
C$ Detailed_Input
C
C     DSK        is the name of a DSK file to create. If a DSK 
C                of the same name already exists, it is deleted.
C
C     AFRAME     is the name of the body-fixed frame to be used in
C                the segment for body Alpha.     
C
C     ANLON      is the number of longitude bands in the plate set for
C                body Alpha. Each longitude band is bounded by two
C                meridians. All longitude bands have equal angular
C                extent in longitude. The vertices of any plate lie on
C                adjacent longitude band boundaries.
C
C     ANLAT      is the number of latitude bands in the plate set for
C                body Alpha. The vertices of each band are bounded by
C                two cones of constant planetocentric latitude. All
C                latitude bands have equal angular extent in
C                planetocentric latitude. The vertices of any plate lie
C                on adjacent latitude band boundaries.
C
C                Each polar "cap" consists of one longitude band.
C
C     BFRAME     is the name of the body-fixed frame to be used in
C                the segment for body Beta.     
C
C     BNLON      is the number of longitude bands in the plate set for
C                body Beta. See description of ANLON for details.  
C
C     BNLAT      is the number of latitude bands in the plate set for
C                body Beta. See description of ANLAT for details.
C
C
C$ Detailed_Output
C
C     None. This routine creates an output file.
C
C$ Parameters
C
C     The maximum plate and column counts for each segment are 
C     defined by the parameters in T_WRDSK2. These values
C     are considerably smaller than those used in dsk02.inc.
C
C$ Exceptions
C
C     1) Any error occurring during the creation of spatial indexes
C        for the segments in the output DSK will be signaled by 
C        routines in the call tree of this routine.
C
C     2) Any I/O errors occurring in the creation of the DSK file
C        will be signaled by routines in the call tree of this
C        routine.  
C
C     Since this routine is normally used within the TSPICE system,
C     it's up the caller to call CHCKXC to catch errors signaled by
C     this routine.
C 
C$ Files
C
C     Input
C     =====
C
C     This routine requires the kernel pool variables defined by 
C     the PCK file created by NATPCK. That kernel should be loaded
C     by the caller.
C
C     Output
C     ======
C
C     This routine creates a DSK file meant for use with the SPK file
C     and PCK file created by the respective test utilities NATSPK and
C     NATPCK.
C
C     The DSK file contains tessellated ellipsoid shape models for 
C     bodies ALPHA and BETA. Radii for the ellipsoids are obtained from
C     the PCK created by NATPCK. 
C
C     Each model is contained in a single segment. The surface IDs
C     for both segments are set to 1. The coordinate systems of both
C     segments are latitudinal.
C
C     The body-fixed frames associated with ALPHA and BETA are specified
C     by the caller.
C
C$ Particulars
C
C     This routine creates a DSK file for use in testing. See the Files
C     section above.
C
C     The DSK created by this routine in particular supports testing
C     the routines
C
C        GFOCLT
C        OCCULT
C
C$ Examples
C
C     The normal way to use this routine is shown below.
C
C        CALL NATPCK ( 'nat.tpc' )
C
C           ...
C
C        CALL NATSPK ( 'nat.bds', 
C       .              'ALPHAFIXED', ANLON, ANLAT,
C       .              'BETAFIXED',  BNLON, BNLAT )
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
C     N.J. Bachman    (JPL)
C
C$ Version
C
C-    Test Utilities 1.0.0, 23-MAY-2016 (NJB)
C     
 
C-&
 
C$ Index_Entries
C
C     Create a "Nat's solar system" DSK file
C
C-&
 
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      JYEAR
      DOUBLE PRECISION      TWOPI

      LOGICAL               EXISTS
      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Local Parameters
C
      INTEGER               ALPHA
      PARAMETER           ( ALPHA  = 1000 )

      INTEGER               BETA
      PARAMETER           ( BETA   = 2000 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )
  
C
C     Local Variables
C
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

C
C     Delete any existing file of the same name.
C
      IF ( EXISTS(DSK) ) THEN

         CALL DELFIL( DSK )
         
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'NATDSK' )
            RETURN
         END IF

      END IF

C
C     Fetch body radii.
C
      CALL BODVCD ( ALPHA, 'RADII', 3, N, ARAD )
      CALL BODVCD ( BETA,  'RADII', 3, N, BRAD )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'NATDSK' )
         RETURN
      END IF

C
C     Create a tessellated plate model segment for body
C     Alpha.
C
      BODYID = ALPHA
      SURFID = 1
      FIRST  = -100 * JYEAR()
      LAST   =  100 * JYEAR()
      CORSYS = LATSYS

      CALL CLEARD ( NSYPAR, CORPAR )

C
C     Set longitude and latitude bounds for the segment.
C
      BOUNDS(1,1) =  0.D0
      BOUNDS(2,1) =  TWOPI()

      BOUNDS(1,2) = -HALFPI()
      BOUNDS(2,2) =  HALFPI()

C
C     Don't make a vertex-plate list.
C
      MAKVTL = .FALSE.

      CALL T_ELDSK2 ( BODYID,  SURFID, AFRAME, FIRST,   LAST,
     .                CORSYS,  CORPAR, BOUNDS, ARAD(1), ARAD(2), 
     .                ARAD(3), ANLON,  ANLAT,  MAKVTL,  DSK     )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'NATDSK' )
         RETURN
      END IF

C
C     Append to the DSK we just created a segment for body Beta.
C
      BODYID = BETA
      SURFID = 1
      FIRST  = -100 * JYEAR()
      LAST   =  100 * JYEAR()
      CORSYS = LATSYS

      CALL CLEARD ( NSYPAR, CORPAR )

C
C     Set longitude and latitude bounds for the segment.
C
      BOUNDS(1,1) =  0.D0
      BOUNDS(2,1) =  TWOPI()

      BOUNDS(1,2) = -HALFPI()
      BOUNDS(2,2) =  HALFPI()

C
C     Don't make a vertex-plate list.
C
      MAKVTL = .FALSE.

      CALL T_ELDSK2 ( BODYID,  SURFID, BFRAME, FIRST,   LAST,
     .                CORSYS,  CORPAR, BOUNDS, BRAD(1), BRAD(2), 
     .                BRAD(3), BNLON,  BNLAT,  MAKVTL,  DSK     )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'NATDSK' )
         RETURN
      END IF


      CALL CHKOUT ( 'NATDSK' )
      RETURN
      END
