!$Procedure HALFPI ( Half the value of pi )

      DOUBLE PRECISION FUNCTION HALFPI ( )

! $ Abstract
!
!     Return half the value of pi (the ratio of the circumference of
!     a circle to its diameter).
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CONSTANTS
!
! $ Declarations
!
!     None.
!
! $ Brief_I/O
!
!     The function returns half the value of pi.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     The function returns half the value of pi (the ratio of
!     a circle's circumference to its diameter), determined by
!     the ACOS function. That is,
!
!           HALFPI = ACOS ( -1.D0 ) * 0.5D0
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The first time the function is referenced, the value is computed
!     as shown above. The value is saved, and returned directly upon
!     subsequent reference.
!
! $ Examples
!
!     The subroutine shown below illustrates the use of HALFPI.
!
!                 SUBROUTINE BFTRAN ( RA, DEC, W, TIPM )
!
!           C
!           C     Compute the transformation from inertial to body
!           C     fixed coordinates, given the directions of the north
!           C     pole and prime meridian of the body.
!           C
!                 DOUBLE PRECISION    RA
!                 DOUBLE PRECISION    DEC
!                 DOUBLE PRECISION    W
!                 DOUBLE PRECISION    TIPM ( 3,3 )
!
!           C
!           C     SPICELIB functions
!           C
!                 DOUBLE PRECISION    HALFPI
!
!           C
!           C     The transformation is defined by the compound
!           C     rotation
!           C
!           C        [W] [pi/2 - Dec] [RA + pi/2]
!           C           3            1           3
!           C
!                 CALL ROTATE (       RA + HALFPI(),  3, TIPM)
!                 CALL ROTMAT (TIPM,  HALFPI() - DEC, 1, TIPM)
!                 CALL ROTMAT (TIPM,  W,              3, TIPM)
!
!                 RETURN
!                 END
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
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.3, 09-JUL-2020 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.2, 08-APR-2015 (JDR)
!
!        Minor edit to example comments eliminating typos.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
!
!-&


! $ Index_Entries
!
!     half the value of pi
!
!-&


!
!     Local variables
!
      DOUBLE PRECISION      VALUE
      SAVE                  VALUE

!
!     Initial values
!
      DATA                  VALUE      / 0.D0 /


!
!     What is there to say?
!
      IF ( VALUE .EQ. 0.D0 ) THEN
         VALUE = ACOS ( -1.D0 ) * 0.5D0
      END IF

      HALFPI = VALUE

      RETURN
      END
