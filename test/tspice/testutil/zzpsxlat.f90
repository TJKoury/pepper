!$Procedure ZZPSXLAT ( Plate set, translate )

      SUBROUTINE ZZPSXLAT ( V1, OFFSET, VOUT )

! $ Abstract
!
!     Apply a translation to a plate set: offset the vertices of a
!     plate set by a specified vector.
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

      DOUBLE PRECISION      V1     ( LBCELL : * )
      DOUBLE PRECISION      OFFSET ( 3 )
      DOUBLE PRECISION      VOUT   ( LBCELL : * )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     V1         I   Vertex cell of a plate set.
!     OFFSET     I   Vector to add to each vertex of V1.
!     VOUT       O   Vertex cell of the translated plate set.
!
! $ Detailed_Input
!
!     V1             is a SPICELIB double precision cell containing the
!                    vertices of a plate set.
!
!     OFFSET         is a 3-vector to be added to each vertex of V1.
!
! $ Detailed_Output
!
!     VOUT           is a SPICELIB double precision cell containing the
!                    vertices of a plate set resulting from adding
!                    OFFSET to each vertex of V1.
!
! $ Parameters
!
!    LBCELL is the lower bound of the SPICELIB cell data structure.
!
! $ Exceptions
!
!     1) If the cardinality of V1 is not a multiple of 3, the error
!        SPICE(BADVERTEXARRAY) is signaled.
!
!     2) If the size of VOUT is too small to contain the vertices
!        of the output plate set, the error SPICE(VERTARRAYTOOSMALL)
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
!-    DSKLIB Version 1.0.0, 04-SEP-2014 (NJB)
!
!-&


!
!     SPICELIB functions
!
      INTEGER               CARDD
      INTEGER               SIZED
      LOGICAL               RETURN

!
!     Local variables
!
      INTEGER               CV1
      INTEGER               I
      INTEGER               J
      INTEGER               NV


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN( 'ZZPSXLAT' )

!
!     Make sure the cardinality of the input vertex set is a
!     multiple of 3.
!
      CV1 = CARDD( V1 )

      NV  = CV1 / 3

      IF ( ( NV * 3 ) .NE. CV1 ) THEN

         CALL SETMSG ( 'Input vertex set cardinality # is not ' &
         //            'a multiple of 3.'                      )
         CALL ERRINT ( '#', CV1                                )
         CALL SIGERR ( 'SPICE(BADVERTEXARRAY)'                 )
         CALL CHKOUT ( 'ZZPSXLAT'                              )
         RETURN

      END IF

!
!     Check room in output vertex array.
!
      CV1   = CARDD ( V1 )

      IF ( SIZED(VOUT) .LT. CV1 ) THEN

         CALL SETMSG ( 'Output vertex array size is #; required ' &
         //            'space is # elements.'                    )
         CALL ERRINT ( '#',  SIZED(VOUT)                         )
         CALL ERRINT ( '#',  CV1                                 )
         CALL SIGERR ( 'SPICE(VERTARRAYTOOSMALL)'                )
         CALL CHKOUT ( 'ZZPSXLAT'                                )
         RETURN

      END IF

!
!     Add the offset vector to each vertex of the input plate set and
!     append the result to the output plate set. We've already
!     verified there's enough room in the output set.
!
      DO I = 1, NV

         J = 1  +  ((I-1)*3)

         CALL VADD ( OFFSET, V1(J), VOUT(J) )

      END DO

      CALL SCARDD ( CV1, VOUT )


      CALL CHKOUT ( 'ZZPSXLAT' )
      RETURN
      END

