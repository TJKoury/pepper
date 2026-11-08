C$Procedure ZZPSPYR ( Plate set, create pyramid )
 
      SUBROUTINE ZZPSPYR ( N, VRTCES, H, VOUT, POUT )
 
C$ Abstract
C
C     Create a plate set representing a pyramid with 2-D polygonal
C     base, centered at the origin, in the X-Y plane. The apex of the
C     pyramid is on the -Z axis at Z = -ABS(H).
C
C     Vertices are required to be ordered in the positive sense about
C     the +Z axis.
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

      INTEGER               N
      DOUBLE PRECISION      VRTCES ( 2, * )
      DOUBLE PRECISION      H
      DOUBLE PRECISION      VOUT ( LBCELL : * )
      INTEGER               POUT ( LBCELL : * )

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     N          I   Number of vertices in the base polygon.
C     VRTCES     I   Vertices of the base polygon.
C     H          I   Absolute value of the pyramid's height.
C     VOUT       O   Vertex cell of the plate set.
C     POUT       O   Plate cell of the plate set.
C
C$ Detailed_Input
C
C     N              is the number of vertices in the base polygon.
C
C     VRTCES         is an array of 2-D vertices representing the
C                    boundary of the base of a pyramid. The vertices
C                    must be ordered in the positive (counterclockwise)
C                    sense about the +Z axis.
C
C     H              is the absolute value of the pyramid's height.
C                    The apex of the pyramid has coordinates
C
C                       ( 0, 0, -ABS(H) )
C                         
C$ Detailed_Output
C
C     VOUT           is a SPICELIB double precision cell containing the
C                    vertices of a plate set representing the surface
C                    of a pyramid. The pyramid's base is on the 
C                    X-Y plane, and the pyramid's apex is on the 
C                    -Z axis at Z = -ABS(H).
C
C     POUT           is a SPICELIB integer cell containing the plates
C                    of the plate set. The vertices of each plate are
C                    ordered so that they define an outward normal
C                    vector having positive dot product with the
C                    plate's vertices. (In other words, the normal
C                    vectors point outward.)
C                  
C$ Parameters
C
C    LBCELL is the lower bound of the SPICELIB cell data structure.
C
C$ Exceptions
C
C     1) If the size of VOUT is too small to contain the output
C        plate set's vertices, the error SPICE(VERTARRAYTOOSMALL) is
C        signaled.
C
C     2) If the size of POUT is too small to contain the output plate
C        set's plates, the error SPICE(PLTARRAYTOOSMALL) is signaled.
C
C     3) Any other errors that occur while accessing SPICE cells
C        will be diagnosed by routines in the call tree of this
C        routine.
C
C     4) If the vertices are not ordered in the positive sense 
C        about the +Z axis, the error SPICE(BADVERTEXORDER) will be
C        signaled.
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
C-    DSKLIB Version 1.0.0, 04-SEP-2014 (NJB)
C
C-&
 

C
C     SPICELIB functions
C
      DOUBLE PRECISION      VDOTG

      INTEGER               SIZED
      INTEGER               SIZEI

      LOGICAL               RETURN

C
C     Local variables
C
      DOUBLE PRECISION      PERP  ( 2 )

      INTEGER               CPOUT
      INTEGER               CVOUT
      INTEGER               I
      INTEGER               J
      INTEGER               NP
      INTEGER               NV

      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZPSPYR' )

C
C     Check the space in the output cells.
C     
      NV    = N + 2
      CVOUT = 3 * NV

      NP    = 2 * N
      CPOUT = 3 * NP

      IF ( SIZED(VOUT) .LT. CVOUT ) THEN

         CALL SETMSG ( 'Output vertex array size is #; required '
     .   //            'space is # elements.'                    )
         CALL ERRINT ( '#',  SIZED(VOUT)                         )
         CALL ERRINT ( '#',  CVOUT                               )
         CALL SIGERR ( 'SPICE(VERTARRAYTOOSMALL)'                )
         CALL CHKOUT ( 'ZZPSPYR'                                   )
         RETURN

      END IF

      IF ( SIZEI(POUT) .LT. CPOUT ) THEN

         CALL SETMSG ( 'Output plate array size is #; required '
     .   //            'space is # elements.'                    )
         CALL ERRINT ( '#',  SIZEI(POUT)                         )
         CALL ERRINT ( '#',  CPOUT                               )
         CALL SIGERR ( 'SPICE(PLTARRAYTOOSMALL)'                 )
         CALL CHKOUT ( 'ZZPSPYR'                                 )
         RETURN

      END IF 
 
C
C     Check the order of the input vertices.
C
      DO I = 1, N-1

         PERP(1) = - VRTCES(2,I)
         PERP(2) =   VRTCES(1,I)
         
         IF ( VDOTG( VRTCES(1,I+1), PERP, 2 ) .LE. 0.D0 ) THEN

            CALL SETMSG ( 'Input vertices are not in strictly '
     .      //            'increasing order: rotation angle from '
     .      //            'vertex # to vertex # about the +Z '
     .      //            'axis is not positive.'                 )
            CALL ERRINT ( '#', I                                  )
            CALL ERRINT ( '#', I+1                                )
            CALL SIGERR ( 'SPICE(BADVERTEXORDER)'                 )
            CALL CHKOUT ( 'ZZPSPYR'                               )
            RETURN

         END IF

      END DO

C
C     Create vertices.
C
      J = 1

      DO I = 1, N

         VOUT(J  ) = VRTCES(1,I)
         VOUT(J+1) = VRTCES(2,I)
         VOUT(J+2) = 0.D0
         J         = J + 3

      END DO

C
C     The penultimate vertex is the origin. The last
C     vertex is the apex.
C
      CALL CLEARD ( 6, VOUT(CVOUT-5) )

      VOUT( CVOUT ) = - ABS(H)

      CALL SCARDD ( CVOUT, VOUT )
         
C
C     Create the base plates.
C
      J = 1

      DO I = 1, N-1

         POUT(J  ) = NV-1
         POUT(J+1) = I
         POUT(J+2) = I + 1
         J         = J + 3
  
      END DO

C
C     Fill in the last base plate.
C
      POUT(J  ) = NV-1
      POUT(J+1) = N
      POUT(J+2) = 1

C
C     Create the side plates.
C
      J = J + 3

      DO I = 1, N-1

         POUT(J  ) = I
         POUT(J+1) = NV
         POUT(J+2) = I+1
         J         = J + 3
  
      END DO

C
C     Fill in the last side plate.
C
      POUT(J  ) = N
      POUT(J+1) = NV
      POUT(J+2) = 1


      CALL SCARDI ( CPOUT, POUT )

      CALL CHKOUT ( 'ZZPSPYR' )
      RETURN
      END


