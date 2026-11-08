C$Procedure ZZPSBOX ( Plate set, create box )
 
      SUBROUTINE ZZPSBOX ( A, B, C, VOUT, POUT )
 
C$ Abstract
C
C     Create a plate set representing a box centered at the origin.
C     The dimensions of the box are specified by the caller.
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

      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      VOUT ( LBCELL : * )
      INTEGER               POUT ( LBCELL : * )

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     A          I   Box edge length in the X direction.
C     B          I   Box edge length in the Y direction.
C     C          I   Box edge length in the Z direction.
C     VOUT       O   Vertex cell of the plate set.
C     POUT       O   Plate cell of the plate set.
C
C$ Detailed_Input
C
C     A,
C     B,
C     C              are the edge lengths of the box in the
C                    X, Y, and Z directions, respectively.
C                         
C$ Detailed_Output
C
C     VOUT           is a SPICELIB double precision cell containing the
C                    vertices of a plate set representing the surface
C                    of a box having dimensions A x B x C. The edges of
C                    the box are parallel to the coordinate axes. The
C                    box is centered at the origin.
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
C     4) If any of the box dimensions are non-positive, the error
C        SPICE(INVALIDEDGELENGTH) will be signaled.
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
C-    DSKLIB Version 1.0.0, 05-SEP-2014 (NJB)
C
C-&
 

C
C     SPICELIB functions
C
      INTEGER               SIZED
      INTEGER               SIZEI

C
C     Local variables
C
      LOGICAL               RETURN

      INTEGER               CPOUT
      INTEGER               CVOUT
      INTEGER               IX
      INTEGER               IY
      INTEGER               IZ
      INTEGER               J
      INTEGER               NP
      INTEGER               NV
      INTEGER               PLATE ( 3, 12 )

      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZPSBOX' )

C
C     Check the space in the output cells.
C     
      NV    = 8
      CVOUT = 3 * NV

      NP    = 12
      CPOUT = 3 * NP

      IF ( SIZED(VOUT) .LT. CVOUT ) THEN

         CALL SETMSG ( 'Output vertex array is too small to '
     .   //            'contain box vertex array. '
     .   //            'Output vertex array size is #; required '
     .   //            'space is # elements.'                    )
         CALL ERRINT ( '#',  SIZED(VOUT)                         )
         CALL ERRINT ( '#',  CVOUT                               )
         CALL SIGERR ( 'SPICE(VERTARRAYTOOSMALL)'                )
         CALL CHKOUT ( 'ZZPSBOX'                                 )
         RETURN

      END IF

      IF ( SIZEI(POUT) .LT. CPOUT ) THEN

         CALL SETMSG ( 'Output plate array is too small to '
     .   //            'contain box plate array. '
     .   //            'Output plate array size is #; required '
     .   //            'space is # elements.'                    )
         CALL ERRINT ( '#',  SIZEI(POUT)                         )
         CALL ERRINT ( '#',  CPOUT                               )
         CALL SIGERR ( 'SPICE(PLTARRAYTOOSMALL)'                 )
         CALL CHKOUT ( 'ZZPSBOX'                                 )
         RETURN

      END IF

C
C     Check the box dimensions.
C     
      IF (      ( A .LE. 0.D0 ) 
     .     .OR. ( B .LE. 0.D0 )
     .     .OR. ( C .LE. 0.D0 ) ) THEN

         CALL SETMSG ( 'Box dimensions must be positive. Actual '  
     .   //            'dimensions were A = #; B = #; C = #.'    )
         CALL ERRDP  ( '#',  A                                   )
         CALL ERRDP  ( '#',  B                                   )
         CALL ERRDP  ( '#',  C                                   )
         CALL SIGERR ( 'SPICE(INVALIDDGELENGTH)'                 )
         CALL CHKOUT ( 'ZZPSBOX'                                 )
         RETURN

      END IF

C
C     Create the vertices.
C
      J = 1

      DO IZ = 0, 1

         DO IY = 0, 1

            DO IX = 0, 1

               VOUT(J+2) = (IZ - 0.5D0) * C
               VOUT(J+1) = (IY - 0.5D0) * B
               VOUT(J  ) = (IX - 0.5D0) * A
               J         = J  + 3

            END DO

         END DO
         
      END DO

      CALL SCARDD ( CVOUT, VOUT )

C
C     Create the plates.
C
      
C
C     +Z face: 
C
      PLATE(1,1) = 5
      PLATE(2,1) = 6
      PLATE(3,1) = 7

      PLATE(1,2) = 6
      PLATE(2,2) = 8
      PLATE(3,2) = 7

C
C     -Z face: 
C
      PLATE(1,3) = 1
      PLATE(2,3) = 3
      PLATE(3,3) = 2

      PLATE(1,4) = 2
      PLATE(2,4) = 3
      PLATE(3,4) = 4

C
C     +Y face: 
C
      PLATE(1,5) = 4
      PLATE(2,5) = 3
      PLATE(3,5) = 8

      PLATE(1,6) = 3
      PLATE(2,6) = 7
      PLATE(3,6) = 8

C
C     -Y face: 
C
      PLATE(1,7) = 1
      PLATE(2,7) = 2
      PLATE(3,7) = 5

      PLATE(1,8) = 2
      PLATE(2,8) = 6
      PLATE(3,8) = 5

C
C     +X face: 
C
      PLATE(1,9)  = 2
      PLATE(2,9)  = 4
      PLATE(3,9)  = 6

      PLATE(1,10) = 4
      PLATE(2,10) = 8
      PLATE(3,10) = 6

C
C     -X face: 
C
      PLATE(1,11) = 3
      PLATE(2,11) = 1
      PLATE(3,11) = 7

      PLATE(1,12) = 1
      PLATE(2,12) = 5
      PLATE(3,12) = 7

      CALL MOVEI ( PLATE,  CPOUT, POUT(1) )

      CALL SCARDI ( CPOUT, POUT )

      CALL CHKOUT ( 'ZZPSBOX' )
      RETURN
      END


