!$Procedure ZZPSSCAL ( Plate set, scale )

      SUBROUTINE ZZPSSCAL ( SCALE, V1, VOUT )
      use pepper_legacy

! $ Abstract
!
!     Scale a plate set: multiply the vertices of a plate set by a
!     specified factor.
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

      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      V1     ( LBCELL : * )
      DOUBLE PRECISION      VOUT   ( LBCELL : * )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     V1         I   Vertex cell of a plate set.
!     SCALE      I   Scale factor.
!     VOUT       O   Vertex cell of the translated plate set.
!
! $ Detailed_Input
!
!     V1             is a SPICELIB double precision cell containing the
!                    vertices of a plate set.
!
!     SCALE          is a double precision scale factor. All vertices
!                    in V1 are to be multiplied by SCALE.
!
! $ Detailed_Output
!
!     VOUT           is a SPICELIB double precision cell containing the
!                    vertices of a plate set resulting from multiplying
!                    each vertex of V1 by SCALE.
!
! $ Parameters
!
!     LBCELL is the lower bound of the SPICELIB cell data structure.
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

!
!     Local variables
!
      INTEGER               CV1
      INTEGER               I
      INTEGER               NV


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN( 'ZZPSSCAL' )

!
!     Make sure the cardinality of the input vertex set is a
!     multiple of 3.
!
      CV1 = CARDD ( V1 )

      NV  = CV1 / 3

      IF ( ( NV * 3 ) .NE. CV1 ) THEN

         CALL SETMSG ( 'Input vertex set cardinality # is not ' &
         //            'a multiple of 3.'                      )
         CALL ERRINT ( '#', CV1                                )
         CALL SIGERR ( 'SPICE(BADVERTEXARRAY)'                 )
         CALL CHKOUT ( 'ZZPSSCAL'                              )
         RETURN

      END IF

!
!     Check room in output vertex array.
!
      IF ( SIZED(VOUT) .LT. CV1 ) THEN

         CALL SETMSG ( 'Output vertex array size is #; required ' &
         //            'space is # elements.'                    )
         CALL ERRINT ( '#',  SIZED(VOUT)                         )
         CALL ERRINT ( '#',  CV1                                 )
         CALL SIGERR ( 'SPICE(VERTARRAYTOOSMALL)'                )
         CALL CHKOUT ( 'ZZPSSCAL'                                )
         RETURN

      END IF

!
!     Scale each vertex of the input plate set and append the result to
!     the output plate set. We've already verified there's enough room
!     in the output set.
!
      DO I = 1, CV1

         VOUT(I) = SCALE * V1(I)

      END DO

      CALL SCARDD ( CV1, VOUT )

      CALL CHKOUT ( 'ZZPSSCAL' )
      RETURN
      END

