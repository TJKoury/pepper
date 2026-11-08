!$Procedure DELFIL ( Delete a file  )

      subroutine delfil ( filnam )
      use pepper_kinds, only: ip

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
!
      character(*)          filnam
!
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
!              no longer exist. The file to be deleted must be closed
!              when this routine is called.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the file name is blank, the error SPICE(BLANKFILENAME)
!         is signaled.
!
!     2)  If the inquire on the filename specified by FILNAM fails for
!         some reason, the error SPICE(INQUIREERROR) is signaled.
!
!     3)  If the file specified by FILNAM is already open, the error
!         SPICE(FILECURRENTLYOPEN) is signaled.
!
!     4)  If the file specified by FILNAM does not exist, the error
!         SPICE(NOSUCHFILE) is signaled.
!
!     5)  If the attempt to open the file specified by FILNAM fails,
!         the error SPICE(FILEOPENFAILED) is signaled.
!
!     6)  If the attempt to close the file with STATUS='DELETE' fails,
!         the error SPICE(FILEDELETEFAILED) is signaled.
!
! $ Files
!
!     The  file specified by FILNAM is opened and then closed by this
!     routine with STATUS = 'DELETE' to delete it. The file must be
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
!        CALL DELFIL ( FILE )
!
! $ Restrictions
!
!     1)  The file to be deleted must be closed when this routine is
!         invoked.
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
!-    SPICELIB Version 1.0.1, 02-JUN-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 20-DEC-1995 (KRG)
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
      logical               return
!
!     Local Variables
!
      integer(ip)           iostat
      integer(ip)           lunit

      logical               exists
      logical               opened

!
!     Standard SPICE error handling.
!
      if ( return() ) then
         return
      else
         call chkin  ( 'DELFIL' )
      end if
!
!     Check to see if the filename we got is blank. If it is, signal an
!     error and return.
!
      if ( filnam == ' ' ) then

         call setmsg ( 'The file name is blank.' )
         call sigerr ( 'SPICE(BLANKFILENAME)'    )
         call chkout ( 'DELFIL'                  )
         return

      end if
!
!     We inquire before we try opening anything to see if the file
!     exists or is currently open.
!
      inquire ( file   = filnam, &
                exist  = exists, &
                iostat = iostat, &
                opened = opened  )
!
!     Not too likely, but if the INQUIRE statement fails signal an error
!     and return.
!
      if ( iostat /= 0 ) then

         call setmsg ( 'INQUIRE statement failed for file ''#''.' // &
                       ' IOSTAT = #.'                              )
         call errch  ( '#', filnam                                 )
         call errint ( '#', iostat                                 )
         call sigerr ( 'SPICE(INQUIREFAILED)'                      )
         call chkout ( 'DELFIL'                                    )
         return

      end if
!
!     The file ought to exist if you're trying to delete it. If not,
!     signal an error and return.
!
      if ( .not. exists ) then

         call setmsg ( 'The file ''#'' does not exist.' )
         call errch  ( '#', filnam                      )
         call sigerr ( 'SPICE(NOSUCHFILE)'              )
         call chkout ( 'DELFIL'                         )
         return

      end if
!
!     The file that is to be deleted should not be in use, indicated by
!     it being open, by anything when we try to delete it. If it is
!     open, signal an error and return.
!
      if ( opened ) then

         call setmsg ( 'The file ''#'' is currently open and'  // &
                       ' cannot be deleted.'                    )
         call errch  ( '#', filnam                              )
         call sigerr ( 'SPICE(FILECURRENTLYOPEN)'               )
         call chkout ( 'DELFIL'                                 )
         return

      end if
!
!     Get an available logical unit and attempt to open the file.
!
      call getlun ( lunit )

      open ( unit=lunit, file=filnam, status='OLD', iostat=iostat )
!
!     If we had trouble opening the file, signal an appropriate error
!     and return.
!
      if ( iostat /= 0 ) then

         call setmsg ( 'Attempt to open the file ''#'' failed.' )
         call errch  ( '#', filnam                              )
         call sigerr ( 'SPICE(FILEOPENFAILED)'                  )
         call chkout ( 'DELFIL'                                 )
         return

      end if
!
!     We opened the file successfully, so let's try to close it with
!     STATUS = 'DELETE'. If this fails, attempt to just close the file,
!     signal an error and return.
!
      close ( lunit, status='DELETE', iostat=iostat )

      if ( iostat /= 0 ) then

         close       ( lunit                                      )
         call setmsg ( 'Attempt to delete the file ''#'' failed.' )
         call errch  ( '#', filnam                                )
         call sigerr ( 'SPICE(FILEDELETEFAILED)'                  )
         call chkout ( 'DELFIL'                                   )
         return

      end if

      call chkout ( 'DELFIL' )
      return

      end
