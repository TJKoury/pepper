!$Procedure LUN2FN ( Map logical unit of open file to its name. )

      subroutine lun2fn ( lunit, filnam )
      use pepper_kinds, only: ip

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

      implicit none

      integer(ip)      lunit
      character*(*)    filnam

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
      logical               return

!
!     Local variables
!
      integer(ip)           iostat

      logical               opened

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'LUN2FN' )
      end if

!
!     First we test to see if the filename is blank.
!
      if ( lunit .le. 0 ) then

         call setmsg ( 'The Fortran logical unit was not' // &
                       ' positive: #.'                     )
         call errint ( '#', lunit                          )
         call sigerr ( 'SPICE(INVALIDARGUMENT)'            )
         call chkout ( 'LUN2FN'                            )
         return

      end if

!
!     So simple, it defies explanation: just INQUIRE.
!
      inquire ( unit   = lunit, &
                name   = filnam, &
                opened = opened, &
                iostat = iostat  )

      if ( iostat .ne. 0 ) then

         call setmsg ( 'An error occurred while INQUIRing on'  // &
                       ' unit #. The IOSTAT value is #.'        )
         call errint ( '#', lunit                               )
         call errint ( '#', iostat                              )
         call sigerr ( 'SPICE(INQUIREFAILED)'                   )
         call chkout ( 'LUN2FN'                                 )
         return

      end if

!
!     If there is no open file associated with the logical unit LUNIT
!     we cannot get a filename. So signal an error.
!
      if ( .not. opened ) then

         call setmsg ( 'There was no open file associated with the' // &
                       ' logical unit #.'                            )
         call errint ( '#', lunit                                    )
         call sigerr ( 'SPICE(FILENOTOPEN)'                          )
         call chkout ( 'LUN2FN'                                      )
         return

      end if

!
!     If we made it to here, we are done. Just check out and return.
!
      call chkout ( 'LUN2FN' )
      return
      end
