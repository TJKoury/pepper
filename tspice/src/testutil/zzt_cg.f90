!$Procedure ZZT_CG ( Make plate set for shape like comet C-G )

      SUBROUTINE ZZT_CG ( VOUT, POUT )

! $ Abstract
!
!     Create a plate set representing, in an extremely crude fashion,
!     the self-occulting property of the shape of comet C-G.
!
! $ Required_Reading
!
!     DSK
!     PCK
!
! $ Keywords
!
!     BODY
!     PLATE
!     TILE
!     TESSELLATE
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      DOUBLE PRECISION      VOUT   ( LBCELL : * )
      INTEGER               POUT   ( LBCELL : * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     VOUT       O   Vertex cell of the plate set.
!     POUT       O   Plate cell of the plate set.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     VOUT           is a SPICELIB double precision cell containing the
!                    vertices of a plate set that simulates the
!                    self-occulting property of the surface of comet
!                    C-G. The surface is composed of three disjoint
!                    boxes.
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
!     See the implementation for buffer size parameters.
!
! $ Exceptions
!
!     1) Any other errors that occur while accessing SPICE cells
!        will be diagnosed by routines in the call tree of this
!        routine.
!
! $ Files
!
!     1)  This routine creates a plate set consisting of a
!         union of box shapes constituting an extremely crude
!         representation of an object such as comet C-G.
!
! $ Particulars
!
!     See Files above.
!
! $ Examples
!
!     See use of C language version in tspice_c test families.
!
! $ Restrictions
!
!     1) For use only by TSPICE.
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
!-    TESTUTIL Version 1.0.0, 09-SEP-2014 (NJB)
!
!-&

! $ Index_Entries
!
!     make crude plate set for object like comet c-g
!
!-&

!
!     SPICELIB functions
!
      LOGICAL               RETURN

!
!     Local parameters
!
      DOUBLE PRECISION      SCALE
      PARAMETER           ( SCALE  = 5.D0 )

      INTEGER               MAXV
      PARAMETER           ( MAXV   = 3 * 500 )

      INTEGER               MAXP
      PARAMETER           ( MAXP   = 2 * MAXV )

!
!     Local variables
!
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
