C$Procedure ZZPSBALL ( Plate set, create ball )
 
      SUBROUTINE ZZPSBALL ( R, NLON, NLAT, VOUT, POUT )
 
C$ Abstract
C
C     Create a plate set representing a sphere centered at the origin.
C     The radius of the sphere is specified by the caller.
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
C     CELLS
C     DSK
C
C$ Keywords
C
C     DSK
C     PLATE
C
C$ Declarations

      IMPLICIT NONE

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      DOUBLE PRECISION      R
      INTEGER               NLON
      INTEGER               NLAT
      DOUBLE PRECISION      VOUT ( LBCELL : * )
      INTEGER               POUT ( LBCELL : * )

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     R          I   Radius of sphere.
C     NLON       I   Number of longitude bands of plates.
C     NLAT       I   Number of latitude bands of plates.
C     VOUT       O   Vertex cell of the plate set.
C     POUT       O   Plate cell of the plate set.
C
C$ Detailed_Input
C
C     R              is the radius of the sphere to be represented
C                    by a collection of plates.
C
C     NLON,
C     NLAT           are, respectively, the number of longitude and
C                    latitude bands of plates representing the 
C                    sphere's surface.
C
C                    The vertices of the plate set are located on
C                    NLON equally spaced (in angular measure) meridians 
C                    and NLAT parallels, with an additional vertex at
C                    each pole.
C                         
C$ Detailed_Output
C
C     VOUT           is a SPICELIB double precision cell containing the
C                    vertices of a plate set representing the surface
C                    of a sphere having radius R. The sphere is
C                    centered at the origin.
C
C                    The vertex cell VOUT contains 
C
C                       ( NLON * (NLAT-1) )  +  2
C
C                    vertices.
C
C
C     POUT           is a SPICELIB integer cell containing the plates
C                    of the plate set. The vertices of each plate are
C                    ordered so that they define an outward normal
C                    vector having positive dot product with the
C                    plate's vertices. (In other words, the normal
C                    vectors point outward.)
C                  
C                    The plate cell POUT contains 
C
C                       2 * NLON * (NLAT-1)
C
C                    plates.
C
C
C$ Parameters
C
C    LBCELL is the lower bound of the SPICELIB cell data structure.
C
C$ Exceptions
C
C     1) If the size of VOUT is too small to contain the output
C        plate set's vertices, the error will be diagnosed by
C        routines in the call tree of this routine.
C
C     2) If the size of POUT is too small to contain the output plate
C        set's plates, the error will be diagnosed by routines in the
C        call tree of this routine.
C
C     3) Any other errors that occur while accessing SPICE cells
C        will be diagnosed by routines in the call tree of this
C        routine.
C
C     4) If the radius is non-positive, the error will be diagnosed by
C        routines in the call tree of this routine.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     None.
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
C     N.J. Bachman    (JPL)
C
C$ Version
C
C-    DSKLIB Version 1.0.0, 03-SEP-2014 (NJB)
C
C-&



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

C
C     From the header of ZZELLPLT:
C
C
C     MAXV       is the maximum number of vertices to return. MAXV must
C                be at least
C
C                   ( NLON * ( NLAT - 1 ) )  +  2
C
C                The array VERTS must have size at least 3*MAXV.
C
C 
C     MAXP       is the maximum number of plates to return. MAXP must
C                be at least
C
C                   2 * NLON * ( NLAT - 1 )
C
C                The array PLATES must have size at least 3*MAXP.
C

      CALL ZZELLPLT ( R,    R,  R,       NLON, NLAT,   MAXV, 
     .                MAXP, NV, VOUT(1), NP,   POUT(1)      )

      
      IF ( FAILED() ) THEN
         CALL CHKOUT( 'ZZPSBALL' )
         RETURN
      END IF

      CALL SCARDD ( 3*NV,  VOUT )
      CALL SCARDI ( 3*NP,  POUT )

      CALL CHKOUT ( 'ZZPSBALL' )
      RETURN
      END
