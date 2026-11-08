!$Procedure LUN2FN ( Map logical unit of open file to its name. )

      SUBROUTINE LUN2FN ( LUNIT, FILNAM )

! $ Abstract
!
!     Map the logical unit of an open file to its associated filename.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     FILES
!
! $ Declarations

      IMPLICIT NONE

      INTEGER          LUNIT
      CHARACTER*(*)    FILNAM

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     LUNIT      I   A logical unit to be mapped to a filename.
!     FILNAM     O   Name of the file associated with LUNIT.
!
! $ Detailed_Input
!
!     LUNIT    is the Fortran logical unit that is to be mapped to the
!              filename with which it is associated. The file must be
!              open for this routine to work properly.
!
! $ Detailed_Output
!
!     FILNAM   is the filename that is associated with the Fortran
!              logical unit LUNIT.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the logical unit is not positive, the error
!         SPICE(INVALIDARGUMENT) is signaled.
!
!     2)  If an error occurs during the execution of the Fortran INQUIRE
!         statement, the error SPICE(INQUIREFAILED) is signaled.
!
!     3)  If the logical unit is not attached to an open file, the
!         error SPICE(FILENOTOPEN) is signaled.
!
!     4)  In the event of an error the contents of the variable FILNAM
!         are not defined and should not be used.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Uses the Fortran INQUIRE statement to determine the filename that
!     is associated with the Fortran logical unit LUNIT.
!
! $ Examples
!
!     The following code fragment illustrates the use of LUN2FN.
!
!     C
!     C      Convert the logical unit to its filename and display it.
!     C
!            CALL LUN2FN ( UNIT1, FNAME1 )
!            WRITE (*,*) 'The filename is: ', FNAME1
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
!     K.R. Gehringer     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 16-AUG-1994 (KRG)
!
!-&


! $ Index_Entries
!
!     map logical unit to filename
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               RETURN

!
!     Local variables
!
      INTEGER               IOSTAT

      LOGICAL               OPENED

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'LUN2FN' )
      END IF

!
!     First we test to see if the filename is blank.
!
      IF ( LUNIT .LE. 0 ) THEN

         CALL SETMSG ( 'The Fortran logical unit was not' // &
                       ' positive: #.'                     )
         CALL ERRINT ( '#', LUNIT                          )
         CALL SIGERR ( 'SPICE(INVALIDARGUMENT)'            )
         CALL CHKOUT ( 'LUN2FN'                            )
         RETURN

      END IF

!
!     So simple, it defies explanation: just INQUIRE.
!
      INQUIRE ( UNIT   = LUNIT, &
                NAME   = FILNAM, &
                OPENED = OPENED, &
                IOSTAT = IOSTAT  )

      IF ( IOSTAT .NE. 0 ) THEN

         CALL SETMSG ( 'An error occurred while INQUIRing on'  // &
                       ' unit #. The IOSTAT value is #.'        )
         CALL ERRINT ( '#', LUNIT                               )
         CALL ERRINT ( '#', IOSTAT                              )
         CALL SIGERR ( 'SPICE(INQUIREFAILED)'                   )
         CALL CHKOUT ( 'LUN2FN'                                 )
         RETURN

      END IF

!
!     If there is no open file associated with the logical unit LUNIT
!     we cannot get a filename. So signal an error.
!
      IF ( .NOT. OPENED ) THEN

         CALL SETMSG ( 'There was no open file associated with the' // &
                       ' logical unit #.'                            )
         CALL ERRINT ( '#', LUNIT                                    )
         CALL SIGERR ( 'SPICE(FILENOTOPEN)'                          )
         CALL CHKOUT ( 'LUN2FN'                                      )
         RETURN

      END IF

!
!     If we made it to here, we are done. Just check out and return.
!
      CALL CHKOUT ( 'LUN2FN' )
      RETURN
      END
