!$Procedure ZZDAFGDR ( Private --- DAF Get Data Record )

      subroutine zzdafgdr ( handle, recno, dprec, found )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Read a data record from a DAF.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     PRIVATE
!
! $ Declarations

      implicit none

      include              'zzddhman.inc'

      integer(ip)           handle
      integer(ip)           recno
      real(wp)              dprec  ( * )
      logical               found

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of the DAF.
!     RECNO      I   Record number.
!     DPREC      O   Contents of the record.
!     FOUND      O   Logical indicating whether the record was found.
!
! $ Detailed_Input
!
!     HANDLE     is the handle associated with the DAF.
!
!     RECNO      is the record number of a particular double precision
!                record within the DAF, whose contents are to be read.
!
! $ Detailed_Output
!
!     DPREC      contains the contents of the specified record from
!                the DAF associated with HANDLE.
!
!     FOUND      is TRUE when the specified record is found, and is
!                FALSE otherwise.
!
! $ Parameters
!
!     None.
!
! $ Files
!
!     This routine reads data from the DAF associated with HANDLE.
!     This action may result in connecting a logical unit to the
!     file, if the handle manager has rotated the file out of the
!     unit table.
!
! $ Exceptions
!
!     1) SPICE(HANDLENOTFOUND) is signaled if HANDLE can not be
!        found in the set of loaded handles.
!
!     2) Routines in the call tree of this routine may trap and
!        signal errors.
!
! $ Particulars
!
!     This routine reads records of double precision numbers
!     from native and supported non-native DAFs.
!
!     The size of the character buffer and the number of records
!     read may have to change to support new environments.  As of
!     the original release of this routine, all systems currently
!     supported have a 1 kilobyte record length.
!
! $ Examples
!
!     See DAFGDR for sample usage.
!
! $ Restrictions
!
!     1) Numeric data when read as characters from a file preserves
!        the bit patterns present in the file in memory.
!
!     2) A record of double precision data is at most 1024 characters
!        in length.
!
!     3) DPREC has enough space to store 128 double precision numbers.
!
!     4) RECNO points to a record that contains double precision data
!        in the file to which HANDLE refers.
!
!     5) ZZXLATED will translate all possible inputs (garbage or not)
!        coming from the file associated with HANDLE.
!
! $ Author_and_Institution
!
!     F.S. Turner     (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 12-NOV-2001 (FST)
!
!
!-&

!
!     SPICELIB Functions
!
      integer(ip)           isrchc

      logical               failed
      logical               return

!
!     Local Variables
!
      character(cbfsiz)     chrbuf
      character(filen)      fname
      character(strsiz)     strbff ( numbff )
      character(strsiz)     tmpstr

      real(wp)              dpbuf  ( 128 )

      integer(ip)           i
      integer(ip)           iamh
      integer(ip)           iarch
      integer(ip)           ibff
      integer(ip)           iostat
      integer(ip)           lun
      integer(ip)           natbff

      logical               first
      logical               locfnd

!
!     Saved Variables
!
      save                  first
      save                  natbff
      save                  strbff

!
!     Data Statements
!
      data                  first  / .true. /
      data                  natbff / 0      /

!
!     Standard SPICE error handling.
!
      if ( return() ) then
         return
      else
         call chkin ( 'ZZDAFGDR' )
      end if

!
!     Perform some initialization tasks.
!
      if ( first ) then

!
!        Populate STRBFF, the buffer that contains the labels
!        for each binary file format.
!
         do i = 1, numbff
            call zzddhgsd ( 'BFF', i, strbff(i) )
         end do

!
!        Fetch the native binary file format and determine its
!        integer code.
!
         call zzplatfm ( 'FILE_FORMAT', tmpstr )
         call ucase    ( tmpstr,        tmpstr )

         natbff = isrchc ( tmpstr, numbff, strbff )

         if ( natbff == 0 ) then

            call setmsg ( 'The binary file format, ''#'', is not ' &
            //            'supported by this version of the toolkit. ' &
            //            'This is a serious problem, contact NAIF.'   )
            call errch  ( '#', tmpstr                                  )
            call sigerr ( 'SPICE(BUG)'                                 )
            call chkout ( 'ZZDAFGDR'                                   )
            return

         end if

!
!        Do not perform initialization tasks again.
!
         first = .false.

      end if

!
!     Assume the data record will not be found, until it has been read
!     from the file, and if necessary, successfully translated.
!
      found = .false.

!
!     Retrieve information regarding the file from the handle manager.
!     The value of IARCH is not a concern, since this is a DAF routine
!     all values passed into handle manager entry points will have
!     'DAF' as their architecture arguments.
!
      call zzddhnfo ( handle, fname, iarch, ibff, iamh, locfnd )

      if ( .not. locfnd ) then
         call setmsg ( 'Unable to locate file associated with ' &
         //            'HANDLE, #.  The most likely cause of this ' &
         //            'is the file that you are trying to read ' &
         //            'has been closed.'                           )
         call errint ( '#', handle                                  )
         call sigerr ( 'SPICE(HANDLENOTFOUND)'                      )
         call chkout ( 'ZZDAFGDR'                                   )
         return
      end if

!
!     Now get a logical unit for the handle.  Check FAILED() in
!     case an error occurs.
!
      call zzddhhlu ( handle, 'DAF', .false., lun )

      if ( failed() ) then
         call chkout ( 'ZZDAFGDR' )
         return
      end if

!
!     Branch based on whether the binary file format is native
!     or not.  Only supported formats can be opened by ZZDDHOPN,
!     so no check of IBFF is required.
!
      if ( ibff == natbff ) then

!
!        In the native case, just read the double precision
!        numbers from the file.
!
         read ( unit   = lun, &
                rec    = recno, &
                iostat = iostat ) ( dpbuf(i), i = 1, 128 )

!
!        Since this routine does not signal any IOSTAT based
!        errors, return if a non-zero value is assigned to IOSTAT.
!
         if ( iostat /= 0 ) then
            call chkout ( 'ZZDAFGDR' )
            return
         end if

!
!     Process the non-native binary file format case.
!
      else

!
!        Read the data record as characters.
!
         read ( unit   = lun, &
                rec    = recno, &
                iostat = iostat ) chrbuf

!
!        Again, since this routine does not signal any IOSTAT
!        based errors, return if one occurs.
!
         if ( iostat /= 0 ) then
            call chkout ( 'ZZDAFGDR' )
            return
         end if

!
!        Translate the data record.  Assume (improperly in the
!        general case) that ZZXLATED will translate the contents
!        of the entire record without signaling an error.  This
!        is appropriate at this stage since ZZXLATED simply swaps
!        bytes between BIG-IEEE and LTL-IEEE environments.  In
!        the future, updates may be necessary to prevent
!        processing of garbage data.
!
         call zzxlated ( ibff, chrbuf, 128, dpbuf )

         if ( failed() ) then
            call chkout ( 'ZZDAFGDR' )
            return
         end if

      end if

!
!     Transfer the DPs to the output argument and return
!     to the caller.
!
      found = .true.
      call moved ( dpbuf, 128, dprec )

      call chkout( 'ZZDAFGDR' )
      return

      end
