!$Procedure DAFWCR ( DAF, write character record )

      subroutine dafwcr ( handle, recno, crec )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Write or rewrite the contents of a character record to
!     a DAF.
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
!     CREC       I   Character record.
!
! $ Detailed_Input
!
!     HANDLE   is the handle associated with a DAF.
!
!     RECNO    is the record number of a character record within
!              the file. If the record does not already exist, it
!              is created. Otherwise its contents are overwritten.
!
!     CREC     contains the first 1000 characters of the specified
!              record.
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
!     1)  If the specified file is not open for write access, an error
!         is signaled by a routine in the call tree of this routine.
!
!     2)  If the declared length of CREC is not 1000 characters,
!         the error SPICE(DAFBADRECLEN) is signaled.
!
!     3)  If the specified record cannot (for some reason) be written,
!         the error SPICE(DAFWRITEFAIL) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Unlike double precision records, character records are
!     not buffered.
!
! $ Examples
!
!     In the following example, matching summary and name records are
!     written to a DAF:
!
!        CALL DAFWDR ( HANDLE, NEXT,   DREC )
!        CALL DAFWCR ( HANDLE, NEXT+1, CREC )
!
!     Note that a character record always immediately follows a summary
!     record.
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
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!     F.S. Turner        (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.1.0, 14-APR-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Moved DAF
!        required reading from $Literature_References to
!        $Required_Reading section.
!
!-    SPICELIB Version 2.0.0, 27-NOV-2001 (FST)
!
!        Updated this routine to utilize new handle manager
!        interfaces. Replaced the check of the input handle's
!        sign with the appropriate call to DAFSIH.
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
!     write DAF character record
!
!-&


! $ Revisions
!
!-    SPICELIB Version 2.0.0, 27-NOV-2001 (FST)
!
!        The call to DAFHLU has been replaced with a call to
!        ZZDDHHLU, the handle manager interface for retrieving
!        a logical unit. DAFHLU is no longer used, since it
!        locks the unit returned to its HANDLE, tying up resources
!        in the handle manager. A call to DAFSIH was inserted to
!        make certain that HANDLE is present in DAFAH's file table,
!        rather than simply checking the sign of HANDLE.
!
!-&


!
!     SPICELIB functions
!
      logical               return

!
!     Local variables
!
      integer(ip)           unit
      integer(ip)           iostat

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'DAFWCR' )
      end if

      call zzddhhlu ( handle, 'DAF', .false., unit )

!
!     Look out for
!
!       -- Writing to a file that is open for read-only.
!
!       -- Trying to write a record that doesn't have length 1000.
!
!       -- Failed write.
!
      call dafsih ( handle, 'WRITE' )

      if ( len ( crec ) /= 1000 ) then

         call setmsg ( 'Expected length of character record is 1000.' // &
                       ' Length of passed record is #' )
         call errint ( '#', len ( crec ) )
         call sigerr ( 'SPICE(DAFBADCRECLEN)' )

      else

         write (unit, &
                rec=recno, &
                iostat=iostat) crec

         if ( iostat /= 0 ) then
            call setmsg ( 'Character record write failed. Value of '  // &
                          'IOSTAT was #' )
            call errint ( '#', iostat )
            call sigerr ( 'SPICE(DAFWRITEFAIL)' )
         end if
      end if

      call chkout ( 'DAFWCR' )
      return
      end
