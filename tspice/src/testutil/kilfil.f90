!$Procedure KILFIL ( Kill a file  )

      SUBROUTINE KILFIL ( FILNAM )
      use pepper_legacy

! $ Abstract
!
!     Delete a file.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     UTILITY
!
! $ Declarations

      IMPLICIT NONE

      CHARACTER*(*)         FILNAM

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     FILNAM     I   The name of a file to be deleted.
!
! $ Detailed_Input
!
!     FILNAM   is the name of a file that is to be deleted. Upon
!              successful completion of this routine this file will
!              no longer exist.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     None
!
! $ Exceptions
!
!     1)  If the file name is blank, the error SPICE(BLANKFILENAME)
!         is signaled.
!
!     2)  If the inquire on the filename specified by FILNAM fails for
!         some reason, the error SPICE(INQUIREERROR) is signaled.
!
!     3)  If the file specified by FILNAM is already open, the file
!         is closed with an attempt to delete it.
!
!     4)  If the file doesn't exist, this routine simply returns.
!
!     5)  If the attempt to open the file specified by FILNAM fails,
!         the error SPICE(FILEOPENFAILED) is signaled.
!
!     6)  If the attempt to delete the file fails the error
!         SPICE(FILEDELETEFAILED) is signaled.
!
! $ Files
!
!     The  file specified by FILNAM is opened, if not already opened,
!     and then closed by this routine to delete it. The file must be
!     closed for this routine to delete it.
!
! $ Particulars
!
!     This subroutine is a support utility that deletes a file.
!
! $ Examples
!
!     Suppose you wish to delete a file named 'delete.me' in the
!     current directory. The code fragment below would accomplish this.
!
!        FILE = 'delete.me'
!        CALL KILFIL ( FILE )
!
! $ Restrictions
!
!     The file to be deleted must be closed when this routine is
!     invoked.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     K.R. Gehringer     (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-     Test Utility Version 1.1.0, 05-MAR-2020 (JDR)
!
!         Added IMPLICIT NONE and removed else block from the standard
!         SPICE error handling.
!
!         Updated header to comply with NAIF standard.
!
!-     Test Utility Version 1.0.0, 30-NOV-1994 (WLT) (KRG)
!
!         This is a rip off of Kevin's routine DELFIL.
!
!-&


! $ Index_Entries
!
!     delete a file
!
!-&


!
!     Spicelib Routines
!

!
!     Local Variables
!
      INTEGER               IOSTAT
      INTEGER               LUNIT

      LOGICAL               OPENED

!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF
      CALL CHKIN  ( 'KILFIL' )

!
!     Check to see if the filename we got is blank. If it is, signal an
!     error and return.
!
      IF ( FILNAM .EQ. ' ' ) THEN

         CALL SETMSG ( 'The file name is blank.' )
         CALL SIGERR ( 'SPICE(BLANKFILENAME)'    )
         CALL CHKOUT ( 'KILFIL'                  )
         RETURN

      END IF

!
!     If the file doesn't exist, there's nothing to do.
!
      IF ( .NOT. EXISTS(FILNAM) ) THEN
         CALL CHKOUT ( 'KILFIL' )
         RETURN
      END IF

!
!     We inquire before we try opening anything to see if the file
!     exists or is currently open.
!
      INQUIRE ( FILE   = FILNAM, &
                IOSTAT = IOSTAT, &
                OPENED = OPENED  )

!
!     Not too likely, but if the INQUIRE statement fails signal an error
!     and return.
!
      IF ( IOSTAT .NE. 0 ) THEN

         CALL SETMSG ( 'INQUIRE statement failed for file ''#''.' // &
                       ' IOSTAT = #.'                              )
         CALL ERRCH  ( '#', FILNAM                                 )
         CALL ERRINT ( '#', IOSTAT                                 )
         CALL SIGERR ( 'SPICE(INQUIREFAILED)'                      )
         CALL CHKOUT ( 'KILFIL'                                    )
         RETURN

      END IF

!
!     The file is already open, we don't have to bother opening it
!     We can just delete it.
!
      IF ( OPENED ) THEN

         INQUIRE ( FILE   = FILNAM, &
                   NUMBER = LUNIT  )


      ELSE

!
!        Get an available logical unit and attempt to open the file.
!
         CALL GETLUN ( LUNIT )

         OPEN ( UNIT=LUNIT, FILE=FILNAM, STATUS='OLD', IOSTAT=IOSTAT )

!
!        If we had trouble opening the file, signal an appropriate error
!        and return.
!
         IF ( IOSTAT .NE. 0 ) THEN

            CALL SETMSG ( 'Attempt to open the file ''#'' failed.' )
            CALL ERRCH  ( '#', FILNAM                              )
            CALL SIGERR ( 'SPICE(FILEOPENFAILED)'                  )
            CALL CHKOUT ( 'KILFIL'                                 )
            RETURN

         END IF

      END IF



!
!     We opened the file successfully, so let's try to close it with
!     STATUS = 'DELETE'. If this fails, attempt to just close the file,
!     signal an error and return.
!
      CLOSE ( LUNIT, STATUS='DELETE', IOSTAT=IOSTAT )

      IF ( IOSTAT .NE. 0 ) THEN

         CLOSE       ( LUNIT                                      )
         CALL SETMSG ( 'Attempt to delete the file ''#'' failed.' )
         CALL ERRCH  ( '#', FILNAM                                )
         CALL SIGERR ( 'SPICE(FILEDELETEFAILED)'                  )
         CALL CHKOUT ( 'KILFIL'                                   )
         RETURN

      END IF

      CALL CHKOUT ( 'KILFIL' )
      RETURN

      END
