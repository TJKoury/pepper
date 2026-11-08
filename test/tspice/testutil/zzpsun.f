C$Procedure ZZPSUN ( Plate set union )
 
      SUBROUTINE ZZPSUN ( V1, P1, V2, P2, VOUT, POUT )

C$ Abstract
C
C     Compute the union of two plate sets.
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

      DOUBLE PRECISION      V1   ( LBCELL : * )
      INTEGER               P1   ( LBCELL : * )
      DOUBLE PRECISION      V2   ( LBCELL : * )
      INTEGER               P2   ( LBCELL : * )
      DOUBLE PRECISION      VOUT ( LBCELL : * )
      INTEGER               POUT ( LBCELL : * )

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     V1         I   Vertex cell of first plate set.
C     P1         I   Plate cell of first plate set.
C     V2         I   Vertex cell of second plate set.
C     P2         I   Plate cell of second plate set.
C     VOUT       O   Vertex cell of plate set union.
C     POUT       O   Plate cell of plate set union.
C
C$ Detailed_Input
C
C     V1             is a SPICELIB double precision cell containing the
C                    vertices of the first input plate set. The
C                    vertices are stored in increasing order of the
C                    vertex IDs, and the components of each vertex are
C                    stored consecutively in the order X, Y, Z.
C
C     P1             is a SPICELIB integer cell containing the plates
C                    of the first input plate set. Each plate is a
C                    sequence of three vertex IDs. The vertex IDs
C                    are 1-based: they range from 1 to NV, where
C                    NV is the number of vertices stored in V1.
C                    The vertices of each plate are ordered in the
C                    positive (counterclockwise) sense about the
C                    plate's outward normal vector. The plates are
C                    store in increasing order of the plate IDs.
C
C     V2             is a SPICELIB double precision cell containing the
C                    vertices of the second input plate set.
C
C     P2             is a SPICELIB integer cell containing the plates
C                    of the second input plate set.
C
C$ Detailed_Output
C
C     VOUT           is a SPICELIB double precision cell containing the
C                    "union" of the sets of vertices of the input plate
C                    sets. The union is constructed by appending the
C                    data of V2 to the data of V1; no attempt is made
C                    to identify and remove repeated vertices. The
C                    cardinality of VOUT is the sum of the cardinalities
C                    of V1 and V2.
C                    
C     POUT           is a SPICELIB integer cell containing the plates
C                    of the union of the input plate sets. In POUT,
C                    each vertex ID from P2 is augmented by the number
C                    of vertices in V1. Each vertex ID from P1 is
C                    simply copied. The union is constructed by
C                    appending to the data from P1 the augmented
C                    versions of the plates from P2. The cardinality of
C                    POUT is the sum of the cardinalities of P1 and P2.
C
C$ Parameters
C
C    LBCELL is the lower bound of the SPICELIB cell data structure.
C
C$ Exceptions
C
C     1) If the size of VOUT is too small to contain the union
C        of the vertex sets, the error SPICE(VERTARRAYTOOSMALL)
C        is signaled.
C
C     2) If the size of POUT is too small to contain the union
C        of the plate sets, the error SPICE(PLTARRAYTOOSMALL)
C        is signaled.
C
C     3) Any other errors that occur while accessing SPICE cells
C        will be diagnosed by routines in the call tree of this
C        routine.
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
C-    DSKLIB Version 1.0.0, 02-SEP-2014 (NJB)
C
C-&
 

C
C     SPICELIB functions
C 
      INTEGER               CARDD
      INTEGER               CARDI
      INTEGER               SIZED
      INTEGER               SIZEI
      LOGICAL               RETURN

C
C     Local variables
C
      INTEGER               CP1
      INTEGER               CP2
      INTEGER               CPOUT
      INTEGER               CV1
      INTEGER               CV2
      INTEGER               CVOUT
      INTEGER               I


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN( 'ZZPSUN' )

C
C     Check room in output vertex array.
C
      CV1   = CARDD ( V1 )
      CV2   = CARDD ( V2 )
      CVOUT = CV1 + CV2

      IF ( SIZED(VOUT) .LT. CVOUT ) THEN

         CALL SETMSG ( 'Output vertex array is too small to '
     .   //            'contain union of input vertex arrays. '
     .   //            'Output vertex array size is #; required '
     .   //            'space is # elements.'                    )
         CALL ERRINT ( '#',  SIZED(VOUT)                         )
         CALL ERRINT ( '#',  CVOUT                               )
         CALL SIGERR ( 'SPICE(VERTARRAYTOOSMALL)'                )
         CALL CHKOUT ( 'ZZPSUN'                                  )
         RETURN

      END IF

C
C     Check room in output plate array.
C
      CP1   = CARDI ( P1 )
      CP2   = CARDI ( P2 )
      CPOUT = CP1 + CP2

      IF ( SIZEI(POUT) .LT. CPOUT ) THEN

         CALL SETMSG ( 'Output plate array is too small to '
     .   //            'contain union of input plate arrays. '
     .   //            'Output plate array size is #; required '
     .   //            'space is # elements.'                    )
         CALL ERRINT ( '#',  SIZEI(POUT)                         )
         CALL ERRINT ( '#',  CPOUT                               )
         CALL SIGERR ( 'SPICE(PLTARRAYTOOSMALL)'                 )
         CALL CHKOUT ( 'ZZPSUN'                                  )
         RETURN

      END IF
      
C
C     Copy the vertices and plates of the first input plate set
C     to the output plate set.
C
      CALL COPYD ( V1, VOUT )
      CALL COPYI ( P1, POUT )
      
C
C     Append the vertices of the second input plate set to the 
C     output plate set. We've already verified there's enough
C     room in the output set.
C
      CALL MOVED ( V2(1), CV2, VOUT(CV1+1) )
         
      CALL SCARDD ( CV1+CV2, VOUT )
      
C
C     In the output plate set, the vertex indices of the 
C     second input plate set must be shifted by the number
C     of vertices in the first input vertex set. 
C
C     Shift and append the plates of the second input plate set to the
C     output plate set. We've already verified there's enough room in
C     the output set.
C
      DO I = 1, CP2

         POUT( CP1 + I )  =  P2(I) + (CV1/3)

      END DO

      CALL SCARDI ( CP1+CP2, POUT )


      CALL CHKOUT ( 'ZZPSUN' )
      RETURN
      END 
      
