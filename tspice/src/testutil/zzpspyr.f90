!$Procedure ZZPSPYR ( Plate set, create pyramid )

      SUBROUTINE ZZPSPYR ( N, VRTCES, H, VOUT, POUT )

! $ Abstract
!
!     Create a plate set representing a pyramid with 2-D polygonal
!     base, centered at the origin, in the X-Y plane. The apex of the
!     pyramid is on the -Z axis at Z = -ABS(H).
!
!     Vertices are required to be ordered in the positive sense about
!     the +Z axis.
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
      DOUBLE PRECISION      H
      DOUBLE PRECISION      VOUT ( LBCELL : * )
      INTEGER               POUT ( LBCELL : * )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     N          I   Number of vertices in the base polygon.
!     VRTCES     I   Vertices of the base polygon.
!     H          I   Absolute value of the pyramid's height.
!     VOUT       O   Vertex cell of the plate set.
!     POUT       O   Plate cell of the plate set.
!
! $ Detailed_Input
!
!     N              is the number of vertices in the base polygon.
!
!     VRTCES         is an array of 2-D vertices representing the
!                    boundary of the base of a pyramid. The vertices
!                    must be ordered in the positive (counterclockwise)
!                    sense about the +Z axis.
!
!     H              is the absolute value of the pyramid's height.
!                    The apex of the pyramid has coordinates
!
!                       ( 0, 0, -ABS(H) )
!
! $ Detailed_Output
!
!     VOUT           is a SPICELIB double precision cell containing the
!                    vertices of a plate set representing the surface
!                    of a pyramid. The pyramid's base is on the
!                    X-Y plane, and the pyramid's apex is on the
!                    -Z axis at Z = -ABS(H).
!
!     POUT           is a SPICELIB integer cell containing the plates
!                    of the plate set. The vertices of each plate are
!                    ordered so that they define an outward normal
!                    vector having positive dot product with the
!                    plate's vertices. (In other words, the normal
!                    vectors point outward.)
!
! $ Parameters
!
!    LBCELL is the lower bound of the SPICELIB cell data structure.
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
      DOUBLE PRECISION      VDOTG

      INTEGER               SIZED
      INTEGER               SIZEI

      LOGICAL               RETURN

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

      CALL CHKIN ( 'ZZPSPYR' )

!
!     Check the space in the output cells.
!
      NV    = N + 2
      CVOUT = 3 * NV

      NP    = 2 * N
      CPOUT = 3 * NP

      IF ( SIZED(VOUT) .LT. CVOUT ) THEN

         CALL SETMSG ( 'Output vertex array size is #; required ' &
         //            'space is # elements.'                    )
         CALL ERRINT ( '#',  SIZED(VOUT)                         )
         CALL ERRINT ( '#',  CVOUT                               )
         CALL SIGERR ( 'SPICE(VERTARRAYTOOSMALL)'                )
         CALL CHKOUT ( 'ZZPSPYR'                                   )
         RETURN

      END IF

      IF ( SIZEI(POUT) .LT. CPOUT ) THEN

         CALL SETMSG ( 'Output plate array size is #; required ' &
         //            'space is # elements.'                    )
         CALL ERRINT ( '#',  SIZEI(POUT)                         )
         CALL ERRINT ( '#',  CPOUT                               )
         CALL SIGERR ( 'SPICE(PLTARRAYTOOSMALL)'                 )
         CALL CHKOUT ( 'ZZPSPYR'                                 )
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
            CALL CHKOUT ( 'ZZPSPYR'                               )
            RETURN

         END IF

      END DO

!
!     Create vertices.
!
      J = 1

      DO I = 1, N

         VOUT(J  ) = VRTCES(1,I)
         VOUT(J+1) = VRTCES(2,I)
         VOUT(J+2) = 0.D0
         J         = J + 3

      END DO

!
!     The penultimate vertex is the origin. The last
!     vertex is the apex.
!
      CALL CLEARD ( 6, VOUT(CVOUT-5) )

      VOUT( CVOUT ) = - ABS(H)

      CALL SCARDD ( CVOUT, VOUT )

!
!     Create the base plates.
!
      J = 1

      DO I = 1, N-1

         POUT(J  ) = NV-1
         POUT(J+1) = I
         POUT(J+2) = I + 1
         J         = J + 3

      END DO

!
!     Fill in the last base plate.
!
      POUT(J  ) = NV-1
      POUT(J+1) = N
      POUT(J+2) = 1

!
!     Create the side plates.
!
      J = J + 3

      DO I = 1, N-1

         POUT(J  ) = I
         POUT(J+1) = NV
         POUT(J+2) = I+1
         J         = J + 3

      END DO

!
!     Fill in the last side plate.
!
      POUT(J  ) = N
      POUT(J+1) = NV
      POUT(J+2) = 1


      CALL SCARDI ( CPOUT, POUT )

      CALL CHKOUT ( 'ZZPSPYR' )
      RETURN
      END


