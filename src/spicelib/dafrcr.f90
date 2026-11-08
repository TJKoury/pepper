!$Procedure DAFRCR ( DAF, read character record )

      subroutine dafrcr ( handle, recno, crec )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Read the contents of a character record from a DAF.
!
! $ Required_Reading
!
!     DAF
!
! $ Keywords
!
!     FILES
!
! $ Declarations

      implicit none

      integer(ip)           handle
      integer(ip)           recno
      character(*)          crec

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of DAF.
!     RECNO      I   Record number of character record.
!     CREC       O   Character record.
!
! $ Detailed_Input
!
!     HANDLE   is the handle associated with a DAF.
!
!     RECNO    is the record number of a character record within
!              the file.
!
! $ Detailed_Output
!
!     CREC     contains the first 1000 characters of the specified
!              record from the specified file.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the declared length of CREC is not 1000 characters,
!         the error SPICE(DAFBADRECLEN) is signaled.
!
!     2)  If the specified record cannot (for some reason) be read,
!         the error SPICE(DAFCRNOTFOUND) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Unlike double precision records, character records are
!     not buffered. Also, while failing to find a specific double
!     precision record is indicated through the calling sequence,
!     failing to find a character record results in an error.
!
! $ Examples
!
!     In the following example, matching summary and name records are
!     read from a DAF:
!
!        CALL DAFGDR ( HANDLE, NEXT,   DREC, FOUND )
!        CALL DAFRCR ( HANDLE, NEXT+1, CREC        )
!
!     Note that a character record always immediately follows a summary
!     record.
!
! $ Restrictions
!
!     1)  This routine is only used to read records on environments
!         whose characters are a single byte in size. Updates
!         to this routine and routines in its call tree may be
!         required to properly handle other cases.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!     F.S. Turner        (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.1.0, 17-JUN-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Moved DAF
!        required reading from $Literature_References to
!        $Required_Reading section.
!
!-    SPICELIB Version 2.0.0, 16-NOV-2001 (FST)
!
!        Updated this routine to make proper use of the new
!        handle manager functionality installed underneath
!        DAF.
!
!-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
!
!        Literature references added to the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
!
!-&


! $ Index_Entries
!
!     read DAF character record
!
!-&


! $ Revisions
!
!-    SPICELIB Version 2.0.0, 16-NOV-2001 (FST)
!
!        This routine now makes use of the handle manager
!        code. A call to DAFSIH was inserted just after
!        the standard SPICE error handling code at the
!        head of the module. This was done to insure that
!        the caller is referring to a legitimately loaded
!        DAF. The penalty for performing this check is
!        a binary search on the number of loaded files,
!        which should be small compared to the actual READ
!        performed below.
!
!        The call to DAFHLU has been replaced with ZZDDHHLU,
!        since calls to DAFHLU locks handles to their logical
!        units.
!
!-&


!
!     SPICELIB functions
!
      logical               failed
      logical               return

!
!     Local variables
!
      integer(ip)           unit
      integer(ip)           iostat

!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     Check to be sure that HANDLE is attached to a file that is open
!     with read access.  If the call fails, check out and return.
!
      call dafsih ( handle, 'READ' )

      if ( failed() ) return

!
!     Now make certain that the string to receive the contents of
!     the character record is the appropriate length.
!
      if ( len ( crec ) /= 1000 ) then
         call setmsg ( 'Expected length of character record is 1000. '// &
                       'Passed string has length #' )
         call errint ( '#', len( crec ) )
         call sigerr ( 'SPICE(DAFBADCRECLEN)' )

      else

!
!        Retrieve a logical unit for this handle.  This has the
!        side-effect of locking this UNIT to HANDLE.
!
         call zzddhhlu ( handle, 'DAF', .false., unit )

         if ( failed() ) return

         read (unit, &
               rec=recno, &
               iostat=iostat) crec

         if ( iostat /= 0 ) then
            call setmsg ( 'Could not read record #. IOSTAT was #.' )
            call errint ( '#', recno                               )
            call errint ( '#', iostat                              )
            call sigerr ( 'SPICE(DAFCRNOTFOUND)'                   )
         end if

      end if

      return
      end
