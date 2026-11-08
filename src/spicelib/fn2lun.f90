!$Procedure FN2LUN ( Map name of open file to its logical unit. )

      subroutine fn2lun ( filnam, lunit )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Map the name of an open file to its associated logical unit.
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

      implicit none

      character*(*)    filnam
      integer(ip)      lunit

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     FILNAM     I   Name of the file to be mapped to its logical unit.
!     LUNIT      O   The logical unit associated with the filename.
!
! $ Detailed_Input
!
!     FILNAM   is the filename that is to be mapped to its associated
!              Fortran logical unit.
!
! $ Detailed_Output
!
!     LUNIT    is the Fortran logical unit that is associated with the
!              filename FILNAM. The file must be open for this routine
!              to work properly.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the filename is blank, the error SPICE(BLANKFILENAME) is
!         signaled.
!
!     2)  If an error occurs during the execution of the Fortran INQUIRE
!         statement, the error SPICE(INQUIREFAILED) is signaled.
!
!     3)  If the filename is not associated with an open file, the
!         error SPICE(FILENOTOPEN) is signaled.
!
!     4)  If the filename is not associated with an existing file, the
!         error SPICE(FILEDOESNOTEXIST) is signaled.
!
!     5)  In the event of an error the contents of the variable LUNIT
!         are not defined, and should not be used.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Use the Fortran INQUIRE statement to determine the filename
!     that is associated with the Fortran logical unit LUNIT.
!
! $ Examples
!
!     The following code fragment illustrates the use of FN2LUN.
!
!     C
!     C      Convert the logical unit to its filename and display it.
!     C
!            CALL FN2LUN ( FNAME, LUNIT )
!            WRITE (*,*) 'The logical unit is: ', LUNIT
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
!     map filename to logical unit
!
!-&


!
!     SPICELIB functions
!
      logical               return

!
!     Local variables
!
      integer(ip)           iostat

      logical               exists
      logical               opened

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'FN2LUN' )
      end if

!
!     First we test to see if the filename is blank.
!
      if ( filnam .eq. ' ' ) then

         call setmsg ( 'The filename is blank.' )
         call sigerr ( 'SPICE(BLANKFILENAME)'   )
         call chkout ( 'FN2LUN'                 )
         return

      end if

!
!     So simple, it defies explanation: just INQUIRE.
!
      inquire ( file   = filnam, &
                number = lunit, &
                exist  = exists, &
                opened = opened, &
                iostat = iostat  )

      if ( iostat .ne. 0 ) then

         call setmsg ( 'INQUIRE error on file ''#''.'  // &
                       ' The value of IOSTAT is: #.'    )
         call errch  ( '#', filnam                      )
         call errint ( '#', iostat                      )
         call sigerr ( 'SPICE(INQUIREFAILED)'           )
         call chkout ( 'FN2LUN'                         )
         return

      end if

!
!     A file cannot be open if it does not exist. We need to check this
!     because for some environments files are considered to be open if
!     they do not exist.
!
      if ( .not. exists ) then

         call setmsg ( 'No file with the name ''#'' was found.' )
         call errch  ( '#', filnam                              )
         call sigerr ( 'SPICE(FILEDOESNOTEXIST)'                )
         call chkout ( 'FN2LUN'                                 )
         return

      end if

!
!     Now check to see if the file is opened. If not, then it is an
!     error, there cannot be a logical unit associated with it..
!
      if ( .not. opened ) then

         call setmsg ( 'There was not an open file associated with' // &
                       ' the filename ''#''.'                        )
         call errch  ( '#', filnam                                   )
         call sigerr ( 'SPICE(FILENOTOPEN)'                          )
         call chkout ( 'FN2LUN'                                      )
         return

      end if

      call chkout ( 'FN2LUN' )
      return
      end
