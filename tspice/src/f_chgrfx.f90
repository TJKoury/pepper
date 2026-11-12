!$Procedure      F_CHGRFX ( Test the Change IRF exceptions )

      SUBROUTINE F_CHGRFX ( OK )
      use pepper_legacy
      IMPLICIT NONE

      LOGICAL               OK

! $ Abstract
!
!     This routine is for examining the error message returned
!     by IRFROT and IRFDEF
!
!-&

!
!     Local Variables
!
      DOUBLE PRECISION      ROT ( 3, 3 )


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_CHGRFX' )

      CALL TCASE ( 'Make sure an unrecognized FROM frame is ' &
      //           'properly diagnosed. ' )

         CALL IRFROT ( 1, 10013, ROT )
         CALL CHCKXC ( .TRUE., 'SPICE(IRFNOTREC)',  OK )


      CALL TCASE ( 'Make sure an unrecognized TO ' &
      //           'frame is properly diagnosed. ' )

         CALL IRFROT ( 10013, 1, ROT )
         CALL CHCKXC ( .TRUE., 'SPICE(IRFNOTREC)',  OK )


      CALL TCASE ( 'Make sure an unrecognized ' &
      //           'DEFAULT frame is properly diagnosed. ' )


         CALL IRFDEF ( 10013 )
         CALL CHCKXC ( .TRUE., 'SPICE(IRFNOTREC)',  OK )



      CALL T_SUCCESS ( OK )
      RETURN
      END
