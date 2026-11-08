!$Procedure ZZPSBOX ( Plate set, create box )

      SUBROUTINE ZZPSBOX ( A, B, C, VOUT, POUT )

! $ Abstract
!
!     Create a plate set representing a box centered at the origin.
!     The dimensions of the box are specified by the caller.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
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

      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      VOUT ( LBCELL : * )
      INTEGER               POUT ( LBCELL : * )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     A          I   Box edge length in the X direction.
!     B          I   Box edge length in the Y direction.
!     C          I   Box edge length in the Z direction.
!     VOUT       O   Vertex cell of the plate set.
!     POUT       O   Plate cell of the plate set.
!
! $ Detailed_Input
!
!     A,
!     B,
!     C              are the edge lengths of the box in the
!                    X, Y, and Z directions, respectively.
!
! $ Detailed_Output
!
!     VOUT           is a SPICELIB double precision cell containing the
!                    vertices of a plate set representing the surface
!                    of a box having dimensions A x B x C. The edges of
!                    the box are parallel to the coordinate axes. The
!                    box is centered at the origin.
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
!     4) If any of the box dimensions are non-positive, the error
!        SPICE(INVALIDEDGELENGTH) will be signaled.
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
!-    DSKLIB Version 1.0.0, 05-SEP-2014 (NJB)
!
!-&


!
!     SPICELIB functions
!
      INTEGER               SIZED
      INTEGER               SIZEI

!
!     Local variables
!
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

!
!     Check the space in the output cells.
!
      NV    = 8
      CVOUT = 3 * NV

      NP    = 12
      CPOUT = 3 * NP

      IF ( SIZED(VOUT) .LT. CVOUT ) THEN

         CALL SETMSG ( 'Output vertex array is too small to ' &
         //            'contain box vertex array. ' &
         //            'Output vertex array size is #; required ' &
         //            'space is # elements.'                    )
         CALL ERRINT ( '#',  SIZED(VOUT)                         )
         CALL ERRINT ( '#',  CVOUT                               )
         CALL SIGERR ( 'SPICE(VERTARRAYTOOSMALL)'                )
         CALL CHKOUT ( 'ZZPSBOX'                                 )
         RETURN

      END IF

      IF ( SIZEI(POUT) .LT. CPOUT ) THEN

         CALL SETMSG ( 'Output plate array is too small to ' &
         //            'contain box plate array. ' &
         //            'Output plate array size is #; required ' &
         //            'space is # elements.'                    )
         CALL ERRINT ( '#',  SIZEI(POUT)                         )
         CALL ERRINT ( '#',  CPOUT                               )
         CALL SIGERR ( 'SPICE(PLTARRAYTOOSMALL)'                 )
         CALL CHKOUT ( 'ZZPSBOX'                                 )
         RETURN

      END IF

!
!     Check the box dimensions.
!
      IF (      ( A .LE. 0.D0 ) &
           .OR. ( B .LE. 0.D0 ) &
           .OR. ( C .LE. 0.D0 ) ) THEN

         CALL SETMSG ( 'Box dimensions must be positive. Actual ' &
         //            'dimensions were A = #; B = #; C = #.'    )
         CALL ERRDP  ( '#',  A                                   )
         CALL ERRDP  ( '#',  B                                   )
         CALL ERRDP  ( '#',  C                                   )
         CALL SIGERR ( 'SPICE(INVALIDDGELENGTH)'                 )
         CALL CHKOUT ( 'ZZPSBOX'                                 )
         RETURN

      END IF

!
!     Create the vertices.
!
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

!
!     Create the plates.
!

!
!     +Z face:
!
      PLATE(1,1) = 5
      PLATE(2,1) = 6
      PLATE(3,1) = 7

      PLATE(1,2) = 6
      PLATE(2,2) = 8
      PLATE(3,2) = 7

!
!     -Z face:
!
      PLATE(1,3) = 1
      PLATE(2,3) = 3
      PLATE(3,3) = 2

      PLATE(1,4) = 2
      PLATE(2,4) = 3
      PLATE(3,4) = 4

!
!     +Y face:
!
      PLATE(1,5) = 4
      PLATE(2,5) = 3
      PLATE(3,5) = 8

      PLATE(1,6) = 3
      PLATE(2,6) = 7
      PLATE(3,6) = 8

!
!     -Y face:
!
      PLATE(1,7) = 1
      PLATE(2,7) = 2
      PLATE(3,7) = 5

      PLATE(1,8) = 2
      PLATE(2,8) = 6
      PLATE(3,8) = 5

!
!     +X face:
!
      PLATE(1,9)  = 2
      PLATE(2,9)  = 4
      PLATE(3,9)  = 6

      PLATE(1,10) = 4
      PLATE(2,10) = 8
      PLATE(3,10) = 6

!
!     -X face:
!
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


