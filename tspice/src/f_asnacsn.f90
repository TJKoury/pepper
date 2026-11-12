!$Procedure      F_ASNACSN (Family of tests for DASINE and DACOSN)

      SUBROUTINE F_ASNACSN ( OK )
      use pepper_legacy

      IMPLICIT              NONE

      LOGICAL               OK

! $ Abstract
!
!     This is a test routine for the arc sine and arc cosine
!     functions with argument bound checking.
!
! $ Version
!
!     TSPICE Version 1.0.0 7-MAR-2006 (LSE)
!-&


!
!     Test Utility Functions
!


!
!     SPICELIB functions
!


!
!     Local variables.
!

      DOUBLE PRECISION      TSTARG
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      TSTANG

!
!     Begin every test family with an open call.
!

      CALL TOPEN ( 'F_ASNACSN' )

!
!     Test for tolerance negative error.
!

      TOL       =  -1.D-12

      CALL TCASE  ( 'Tolerance negative, error in DASINE' )

      TSTANG    =   DASINE ( 0.D0,  TOL                   )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK  )

      CALL TCASE  ( 'Tolerance negative, error in DACOSN' )

      TSTANG    =   DACOSN ( 1.D0,  TOL                   )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK  )

!
!     Test for error when argument is too large
!

      TOL       =  1.D-12

      TSTARG    =  1.D0 + 1.D-11

      CALL TCASE  ( 'Argument too large in DASINE.'               )

      TSTANG    =  DASINE ( TSTARG,  TOL                          )

      CALL CHCKXC ( .TRUE., 'SPICE(INPUTOUTOFBOUNDS)', OK         )

      CALL TCASE  ( 'Is value = ASIN (1.0) in DASINE?)'           )

      CALL CHCKSD ( 'TSTANG', TSTANG,   '=', HALFPI(),  0.D0,  OK )

      CALL TCASE  ( 'Argument too large in DACOSN.'               )

      TSTANG    =  DACOSN ( TSTARG,  TOL                          )

      CALL CHCKXC ( .TRUE., 'SPICE(INPUTOUTOFBOUNDS)', OK         )

      CALL TCASE  ( 'Is value = ACOS (1.0) in DACOSN?)'           )

      CALL CHCKSD ( 'TSTANG', TSTANG,   '=', 0.D0,  0.D0,  OK     )

!
!     Test for error when argument is too small
!

      TOL       =  1.D-12

      TSTARG    = -1.D0 - 1.D-11

      CALL TCASE  ( 'Argument too small in DASINE.'               )

      TSTANG    =  DASINE ( TSTARG,  TOL                          )

      CALL CHCKXC ( .TRUE., 'SPICE(INPUTOUTOFBOUNDS)', OK         )

      CALL TCASE  ( 'Is value = ASIN (-1.0) in DASINE?)'          )

      CALL CHCKSD ( 'TSTANG', TSTANG,   '=',-HALFPI(),  0.D0,  OK )

      CALL TCASE  ( 'Argument too small in DACOSN.'               )

      TSTANG    =  DACOSN ( TSTARG,  TOL                          )

      CALL CHCKXC ( .TRUE., 'SPICE(INPUTOUTOFBOUNDS)', OK         )

      CALL TCASE  ( 'Is value = ACOS (-1.0) in DACOSN?)'          )

      CALL CHCKSD ( 'TSTANG', TSTANG,   '=', PI(),  0.D0,  OK     )

!
!     All done.
!

      CALL T_SUCCESS ( OK )
      RETURN
      END

