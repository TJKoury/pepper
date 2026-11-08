C$Procedure ZZPSXFRM ( Plate set, transform )
 
      SUBROUTINE ZZPSXFRM ( V1, XFORM, VOUT )
 
C$ Abstract
C
C     Apply a linear transformation to a plate set: left-multiply
C     the vertices of a plate set by a specified 3x3 matrix.
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

      DOUBLE PRECISION      V1     ( LBCELL : * )
      DOUBLE PRECISION      XFORM  ( 3, 3 )
      DOUBLE PRECISION      VOUT   ( LBCELL : * )

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     V1         I   Vertex cell of a plate set.
C     XFORM      I   Matrix by which to multiply each vertex of V1.
C     VOUT       O   Vertex cell of the transformed plate set.
C
C$ Detailed_Input
C
C     V1             is a SPICELIB double precision cell containing the
C                    vertices of a plate set.
C
C     XFORM          is a 3x3 matrix used to transform each vertex of
C                    V1 by left multiplication.
C
C$ Detailed_Output
C
C     VOUT           is a SPICELIB double precision cell containing the
C                    vertices of a plate set resulting from multiplying
C                    each vertex of V1 by XFORM.
C                  
C$ Parameters
C
C     LBCELL is the lower bound of the SPICELIB cell data structure.
C
C$ Exceptions
C
C     1) If the cardinality of V1 is not a multiple of 3, the error 
C        SPICE(BADVERTEXARRAY) is signaled.
C
C     2) If the size of VOUT is too small to contain the vertices
C        of the output plate set, the error SPICE(VERTARRAYTOOSMALL)
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
C-    DSKLIB Version 1.0.0, 04-SEP-2014 (NJB)
C
C-&
 

C
C     SPICELIB functions
C
      INTEGER               CARDD
      INTEGER               SIZED
      LOGICAL               RETURN


      INTEGER               CV1
      INTEGER               I
      INTEGER               J
      INTEGER               NV


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN( 'ZZPSXFRM' )

C
C     Make sure the cardinality of the input vertex set is a 
C     multiple of 3.
C
      CV1 = CARDD( V1 )

      NV  = CV1 / 3

      IF ( ( NV * 3 ) .NE. CV1 ) THEN

         CALL SETMSG ( 'Input vertex set cardinality # is not '
     .   //            'a multiple of 3.'                      )
         CALL ERRINT ( '#', CV1                                )
         CALL SIGERR ( 'SPICE(BADVERTEXARRAY)'                 )
         CALL CHKOUT ( 'ZZPSXFRM'                              )
         RETURN

      END IF

C
C     Check room in output vertex array.
C
      IF ( SIZED(VOUT) .LT. CV1 ) THEN

         CALL SETMSG ( 'Output vertex array size is #; required '
     .   //            'space is # elements.'                    )
         CALL ERRINT ( '#',  SIZED(VOUT)                         )
         CALL ERRINT ( '#',  CV1                                 )
         CALL SIGERR ( 'SPICE(VERTARRAYTOOSMALL)'                )
         CALL CHKOUT ( 'ZZPSXFRM'                                )
         RETURN

      END IF
       
C
C     Transform each vertex of the input plate set and append the
C     result to the output plate set. We've already verified there's
C     enough room in the output set.
C
      DO I = 1, NV

         J = 1 + ((I-1)*3)

         CALL MXV ( XFORM, V1(J), VOUT(J) )
         
      END DO

      CALL SCARDD ( CV1, VOUT )

      CALL CHKOUT ( 'ZZPSXFRM' )
      RETURN
      END 
      
