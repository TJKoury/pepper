C$Procedure ZZT_CG ( Make plate set for shape like comet C-G )

      SUBROUTINE ZZT_CG ( VOUT, POUT )
  
C$ Abstract
C
C     Create a plate set representing, in an extremely crude fashion,
C     the self-occulting property of the shape of comet C-G.
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
C     DSK
C     PCK
C
C$ Keywords
C
C     BODY
C     PLATE
C     TILE
C     TESSELLATE
C
C$ Declarations

      IMPLICIT NONE

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      DOUBLE PRECISION      VOUT   ( LBCELL : * )
      INTEGER               POUT   ( LBCELL : * )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     VOUT       O   Vertex cell of the plate set.
C     POUT       O   Plate cell of the plate set.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     VOUT           is a SPICELIB double precision cell containing the
C                    vertices of a plate set that simulates the
C                    self-occulting property of the surface of comet
C                    C-G. The surface is composed of three disjoint
C                    boxes.
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
C     See the implementation for buffer size parameters.
C
C$ Exceptions
C
C     1) Any other errors that occur while accessing SPICE cells
C        will be diagnosed by routines in the call tree of this
C        routine.
C
C$ Files
C
C     1)  This routine creates a plate set consisting of a 
C         union of box shapes constituting an extremely crude
C         representation of an object such as comet C-G.
C
C$ Particulars
C 
C     See Files above.
C
C$ Examples
C
C     See use of C language version in tspice_c test families.
C
C$ Restrictions
C
C     1) For use only by TSPICE.
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
C-    TESTUTIL Version 1.0.0, 09-SEP-2014 (NJB)
C
C-&

C$ Index_Entries
C
C     make crude plate set for object like comet c-g
C
C-&

C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local parameters
C
      DOUBLE PRECISION      SCALE
      PARAMETER           ( SCALE  = 5.D0 )

      INTEGER               MAXV
      PARAMETER           ( MAXV   = 3 * 500 )
      
      INTEGER               MAXP
      PARAMETER           ( MAXP   = 2 * MAXV )

C
C     Local variables
C
      DOUBLE PRECISION      A
      DOUBLE PRECISION      OFFSET ( 3 )
      DOUBLE PRECISION      VTEMP0 ( LBCELL : MAXV )
      DOUBLE PRECISION      VTEMP1 ( LBCELL : MAXV )

      INTEGER               PTEMP0 ( LBCELL : MAXP )
      INTEGER               PTEMP1 ( LBCELL : MAXP )


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZT_CG' )
      
      CALL SSIZED ( MAXV, VTEMP0 )
      CALL SSIZED ( MAXV, VTEMP1 )
      CALL SSIZEI ( MAXP, PTEMP0 )
      CALL SSIZEI ( MAXP, PTEMP1 )


      A = SCALE

      CALL ZZPSBOX  ( A, 3*A, 4*A, VOUT,   POUT   )
      CALL ZZPSBOX  ( A,   A,   A, VTEMP0, PTEMP0 )

      OFFSET(1) = 1.5D0 * A
      OFFSET(2) = A
      OFFSET(3) = 0.D0
      

      CALL ZZPSXLAT ( VTEMP0, OFFSET, VTEMP1 )
      CALL COPYD    ( VTEMP1,         VTEMP0 )

      CALL ZZPSUN   ( VTEMP0, PTEMP0, VOUT, POUT, VTEMP1, PTEMP1 )

      CALL COPYD    ( VTEMP1, VOUT )
      CALL COPYI    ( PTEMP1, POUT )


      CALL ZZPSBOX  ( A, 5*A, 3*A, VTEMP0, PTEMP0 )

      OFFSET(1) = 3*A
      OFFSET(2) = A
      OFFSET(3) = A

      CALL ZZPSXLAT ( VTEMP0, OFFSET, VTEMP1 )
      CALL COPYD    ( VTEMP1,         VTEMP0 )

      CALL ZZPSUN ( VTEMP0, PTEMP0, VOUT, POUT, VTEMP1, PTEMP1 )

      CALL COPYD  ( VTEMP1, VOUT )
      CALL COPYI  ( PTEMP1, POUT )

      CALL CHKOUT ( 'ZZT_CG' )
      RETURN
      END
