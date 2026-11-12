!$Procedure ZZPSUN ( Plate set union )

      SUBROUTINE ZZPSUN ( V1, P1, V2, P2, VOUT, POUT )
      use pepper_legacy

! $ Abstract
!
!     Compute the union of two plate sets.
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

      DOUBLE PRECISION      V1   ( LBCELL : * )
      INTEGER               P1   ( LBCELL : * )
      DOUBLE PRECISION      V2   ( LBCELL : * )
      INTEGER               P2   ( LBCELL : * )
      DOUBLE PRECISION      VOUT ( LBCELL : * )
      INTEGER               POUT ( LBCELL : * )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     V1         I   Vertex cell of first plate set.
!     P1         I   Plate cell of first plate set.
!     V2         I   Vertex cell of second plate set.
!     P2         I   Plate cell of second plate set.
!     VOUT       O   Vertex cell of plate set union.
!     POUT       O   Plate cell of plate set union.
!
! $ Detailed_Input
!
!     V1             is a SPICELIB double precision cell containing the
!                    vertices of the first input plate set. The
!                    vertices are stored in increasing order of the
!                    vertex IDs, and the components of each vertex are
!                    stored consecutively in the order X, Y, Z.
!
!     P1             is a SPICELIB integer cell containing the plates
!                    of the first input plate set. Each plate is a
!                    sequence of three vertex IDs. The vertex IDs
!                    are 1-based: they range from 1 to NV, where
!                    NV is the number of vertices stored in V1.
!                    The vertices of each plate are ordered in the
!                    positive (counterclockwise) sense about the
!                    plate's outward normal vector. The plates are
!                    store in increasing order of the plate IDs.
!
!     V2             is a SPICELIB double precision cell containing the
!                    vertices of the second input plate set.
!
!     P2             is a SPICELIB integer cell containing the plates
!                    of the second input plate set.
!
! $ Detailed_Output
!
!     VOUT           is a SPICELIB double precision cell containing the
!                    "union" of the sets of vertices of the input plate
!                    sets. The union is constructed by appending the
!                    data of V2 to the data of V1; no attempt is made
!                    to identify and remove repeated vertices. The
!                    cardinality of VOUT is the sum of the cardinalities
!                    of V1 and V2.
!
!     POUT           is a SPICELIB integer cell containing the plates
!                    of the union of the input plate sets. In POUT,
!                    each vertex ID from P2 is augmented by the number
!                    of vertices in V1. Each vertex ID from P1 is
!                    simply copied. The union is constructed by
!                    appending to the data from P1 the augmented
!                    versions of the plates from P2. The cardinality of
!                    POUT is the sum of the cardinalities of P1 and P2.
!
! $ Parameters
!
!    LBCELL is the lower bound of the SPICELIB cell data structure.
!
! $ Exceptions
!
!     1) If the size of VOUT is too small to contain the union
!        of the vertex sets, the error SPICE(VERTARRAYTOOSMALL)
!        is signaled.
!
!     2) If the size of POUT is too small to contain the union
!        of the plate sets, the error SPICE(PLTARRAYTOOSMALL)
!        is signaled.
!
!     3) Any other errors that occur while accessing SPICE cells
!        will be diagnosed by routines in the call tree of this
!        routine.
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
!-    DSKLIB Version 1.0.0, 02-SEP-2014 (NJB)
!
!-&


!
!     SPICELIB functions
!

!
!     Local variables
!
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

!
!     Check room in output vertex array.
!
      CV1   = CARDD ( V1 )
      CV2   = CARDD ( V2 )
      CVOUT = CV1 + CV2

      IF ( SIZED(VOUT) .LT. CVOUT ) THEN

         CALL SETMSG ( 'Output vertex array is too small to ' &
         //            'contain union of input vertex arrays. ' &
         //            'Output vertex array size is #; required ' &
         //            'space is # elements.'                    )
         CALL ERRINT ( '#',  SIZED(VOUT)                         )
         CALL ERRINT ( '#',  CVOUT                               )
         CALL SIGERR ( 'SPICE(VERTARRAYTOOSMALL)'                )
         CALL CHKOUT ( 'ZZPSUN'                                  )
         RETURN

      END IF

!
!     Check room in output plate array.
!
      CP1   = CARDI ( P1 )
      CP2   = CARDI ( P2 )
      CPOUT = CP1 + CP2

      IF ( SIZEI(POUT) .LT. CPOUT ) THEN

         CALL SETMSG ( 'Output plate array is too small to ' &
         //            'contain union of input plate arrays. ' &
         //            'Output plate array size is #; required ' &
         //            'space is # elements.'                    )
         CALL ERRINT ( '#',  SIZEI(POUT)                         )
         CALL ERRINT ( '#',  CPOUT                               )
         CALL SIGERR ( 'SPICE(PLTARRAYTOOSMALL)'                 )
         CALL CHKOUT ( 'ZZPSUN'                                  )
         RETURN

      END IF

!
!     Copy the vertices and plates of the first input plate set
!     to the output plate set.
!
      CALL COPYD ( V1, VOUT )
      CALL COPYI ( P1, POUT )

!
!     Append the vertices of the second input plate set to the
!     output plate set. We've already verified there's enough
!     room in the output set.
!
      CALL MOVED ( V2(1), CV2, VOUT(CV1+1) )

      CALL SCARDD ( CV1+CV2, VOUT )

!
!     In the output plate set, the vertex indices of the
!     second input plate set must be shifted by the number
!     of vertices in the first input vertex set.
!
!     Shift and append the plates of the second input plate set to the
!     output plate set. We've already verified there's enough room in
!     the output set.
!
      DO I = 1, CP2

         POUT( CP1 + I )  =  P2(I) + (CV1/3)

      END DO

      CALL SCARDI ( CP1+CP2, POUT )


      CALL CHKOUT ( 'ZZPSUN' )
      RETURN
      END

