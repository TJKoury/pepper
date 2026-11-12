!$Procedure ZZPSPOLY ( Plate set, create polygon )

      SUBROUTINE ZZPSPOLY ( N, VRTCES, VOUT, POUT )
      use pepper_legacy

! $ Abstract
!
!     Create a plate set with a polygonal boundary in the X-Y plane.
!     The normal vectors of the plates point in the +Z direction.
!
!     The input vertices are required to be ordered in the positive
!     sense about the +Z axis.
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

      INTEGER               N
      DOUBLE PRECISION      VRTCES ( 2, * )
      DOUBLE PRECISION      VOUT ( LBCELL : * )
      INTEGER               POUT ( LBCELL : * )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     N          I   Number of vertices in the polygon.
!     VRTCES     I   2-D vertices of the polygon.
!     VOUT       O   Vertex cell of the plate set.
!     POUT       O   Plate cell of the plate set.
!
! $ Detailed_Input
!
!     N              is the number of vertices in the polygon.
!
!     VRTCES         is an array of 2-D vertices. The vertices must be
!                    ordered in the positive (counterclockwise) sense
!                    about the +Z axis.
!
! $ Detailed_Output
!
!     VOUT           is a SPICELIB double precision cell containing the
!                    (3-D) vertices of a plate set representing planar
!                    region bounded by the polygon.
!
!     POUT           is a SPICELIB integer cell containing the plates
!                    of the plate set. The vertices of each plate are
!                    ordered so that they define a normal vector
!                    pointing in the +Z direction.
!
! $ Parameters
!
!     LBCELL is the lower bound of the SPICELIB cell data structure.
!
! $ Exceptions
!
!     1) If the size of VOUT is too small to contain the output
!        plate set's vertices, the error SPICE(VERTARRAYTOOSMALL) is
!        signaled.
!
!     2) If the size of POUT is too small to contain the output plate
!        set's plates, the error SPICE(PLTARRAYTOOSMALL) is signaled.
!
!     3) Any other errors that occur while accessing SPICE cells
!        will be diagnosed by routines in the call tree of this
!        routine.
!
!     4) If the vertices are not ordered in the positive sense
!        about the +Z axis, the error SPICE(BADVERTEXORDER) will be
!        signaled.
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

      CALL CHKIN ( 'ZZPSPOLY' )

!
!     Check the space in the output cells.
!
      NV    = N + 1
      CVOUT = 3 * NV

      NP    = N
      CPOUT = 3 * NP

      IF ( SIZED(VOUT) .LT. CVOUT ) THEN

         CALL SETMSG ( 'Output vertex array size is #; required ' &
         //            'space is # elements.'                    )
         CALL ERRINT ( '#',  SIZED(VOUT)                         )
         CALL ERRINT ( '#',  CVOUT                               )
         CALL SIGERR ( 'SPICE(VERTARRAYTOOSMALL)'                )
         CALL CHKOUT ( 'ZZPSPOLY'                                )
         RETURN

      END IF

      IF ( SIZEI(POUT) .LT. CPOUT ) THEN

         CALL SETMSG ( 'Output plate array size is #; required ' &
         //            'space is # elements.'                    )
         CALL ERRINT ( '#',  SIZEI(POUT)                         )
         CALL ERRINT ( '#',  CPOUT                               )
         CALL SIGERR ( 'SPICE(PLTARRAYTOOSMALL)'                 )
         CALL CHKOUT ( 'ZZPSPOLY'                                )
         RETURN

      END IF

!
!     Check the order of the input vertices.
!
      DO I = 1, N-1

         PERP(1) = - VRTCES(2,I)
         PERP(2) =   VRTCES(1,I)

         IF ( VDOTG( VRTCES(1,I+1), PERP, 2 ) .LE. 0.D0 ) THEN

            CALL SETMSG ( 'Input vertices are not in strictly ' &
            //            'increasing order: rotation angle from ' &
            //            'vertex # to vertex # about the +Z ' &
            //            'axis is not positive.'                 )
            CALL ERRINT ( '#', I                                  )
            CALL ERRINT ( '#', I+1                                )
            CALL SIGERR ( 'SPICE(BADVERTEXORDER)'                 )
            CALL CHKOUT ( 'ZZPSPOLY'                              )
            RETURN

         END IF

      END DO

!
!     Create 3-D vertices.
!
      J = 1

      DO I = 1, N

         VOUT(J  ) = VRTCES(1,I)
         VOUT(J+1) = VRTCES(2,I)
         VOUT(J+2) = 0.D0
         J         = J + 3

      END DO

!
!     The last vertex is the origin.
!
      CALL CLEARD ( 3, VOUT(CVOUT-2) )

      CALL SCARDD ( CVOUT, VOUT )

!
!     Create plates.
!
      J = 1

      DO I = 1, N-1

         POUT(J  ) = NV
         POUT(J+1) = I
         POUT(J+2) = I + 1
         J         = J + 3

      END DO

!
!     Fill in the last plate.
!
      POUT(J  ) = NV
      POUT(J+1) = N
      POUT(J+2) = 1

      CALL SCARDI ( CPOUT, POUT )

      CALL CHKOUT ( 'ZZPSPOLY' )
      RETURN
      END


