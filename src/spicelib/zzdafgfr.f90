!$Procedure ZZDAFGFR ( Private --- DAF Get File Record )

      subroutine zzdafgfr ( handle, idword, nd,   ni,   ifname, &
                            fward,  bward,  free, found         )
      use pepper_kinds, only: ip

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Read the contents of the file record of a DAF.
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
      character(*)          idword
      integer(ip)           nd
      integer(ip)           ni
      character(*)          ifname
      integer(ip)           fward
      integer(ip)           bward
      integer(ip)           free
      logical               found

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of the DAF.
!     IDWORD     O   DAF ID Word that indicates file type.
!     ND         O   Number of double precision components in summaries.
!     NI         O   Number of integer components in summaries.
!     IFNAME     O   Internal file name.
!     FWARD      O   Forward list pointer.
!     BWARD      O   Backward list pointer.
!     FREE       O   Free address pointer.
!     FOUND      O   Logical indicating whether the record was found.
!
! $ Detailed_Input
!
!     HANDLE     is the handle associated with the DAF.
!
! $ Detailed_Output
!
!     IDWORD     is a character string identifying the architecture
!                and type of a SPICE binary kernel.  In this case
!                it will be a string identifying the type of DAF.
!
!     ND,
!     NI         are the number of double precision and integer
!                components, respectively, in each array summary in
!                the specified file.
!
!     IFNAME     is the internal file name stored in the first
!                (or file) record of the specified file.
!
!     FWARD      is the forward list pointer. This points to the
!                first summary record in the file. (Records between
!                the first record and the first summary record are
!                reserved when the file is created, and are invisible
!                to DAF routines.)
!
!     BWARD      is the backward list pointer. This points
!                to the final summary record in the file.
!
!     FREE       is the free address pointer. This contains the
!                first free address in the file. (That is, the
!                initial address of the next array to be added
!                to the file.)
!
!     FOUND      is TRUE when the file record is found, and is
!                FALSE otherwise.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) SPICE(HANDLENOTFOUND) is signaled if HANDLE can not be
!        found in the set of loaded handles.  The output arguments
!        are unmodified when this error occurs.
!
!     2) Routines in the call tree of this routine may trap and
!        signal errors.  The output arguments are unmodified in
!        these cases.
!
! $ Files
!
!     This routine reads data from the DAF associated with HANDLE.
!     This action may result in connecting a logical unit to the
!     file, if the handle manager has rotated the file out of the
!     unit table.
!
! $ Particulars
!
!     This routine reads the publicly available components of
!     file records from native and supported non-native DAFs.
!
!     The size of the character buffer and the number of records
!     read may have to change to support new environments.  As of
!     the original release of this routine, all systems currently
!     supported have a 1 kilobyte record length.
!
! $ Examples
!
!     See DAFRFR for sample usage.
!
! $ Restrictions
!
!     1) Numeric data when read as characters from a file preserves
!        the bit patterns present in the file in memory.
!
!     2) A record of double precision data is at most 1024 characters
!        in length.
!
!     3) Future updates to this module must preserve the fact that
!        FOUND is returned as FALSE whenever an error occurs.  An
!        incompletely translated or extracted file record is NOT
!        FOUND.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     F.S. Turner     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.2, 03-OCT-2021 (NJB)
!
!        Corrected typo in comments.
!
!-    SPICELIB Version 1.0.1, 24-JUL-2014 (NJB)
!
!        Corrected typo in Procedure line of header.
!        Re-ordered header sections.
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
!     Local Parameters
!
!     Record Number of the file record in a DAF.
!
      integer(ip)           frlocr
      parameter           ( frlocr =  1 )

!
!     Length of the IDWORD string.
!
      integer(ip)           idwlen
      parameter           ( idwlen =  8 )

!
!     Length of the internal filename string.
!
      integer(ip)           ifnlen
      parameter           ( ifnlen = 60 )

!
!     Starting location in bytes of the internal filename in the
!     file record.
!
      integer(ip)           ifnstr
      parameter           ( ifnstr = 17 )

!
!     Size of an integer in bytes.
!
      integer(ip)           inlen
      parameter           ( inlen  =  4 )

!
!     Local Variables
!
      character(cbfsiz)     chrbuf
      character(filen)      fname
      character(idwlen)     locidw
      character(ifnlen)     locifn
      character(strsiz)     strbff ( numbff )
      character(strsiz)     tmpstr

      integer(ip)           cindex
      integer(ip)           i
      integer(ip)           iamh
      integer(ip)           iarch
      integer(ip)           ibff
      integer(ip)           iostat
      integer(ip)           locbwd
      integer(ip)           locfre
      integer(ip)           locfwd
      integer(ip)           locnd
      integer(ip)           locni
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
      if ( return() ) return

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
         return
      end if

!
!     Now get a logical unit for the handle.  Check FAILED() in
!     case an error occurs.
!
      call zzddhhlu ( handle, 'DAF', .false., lun )

      if ( failed() ) return

!
!     Branch based on whether the binary file format is native
!     or not.  Only supported formats can be opened by ZZDDHOPN,
!     so no check of IBFF is required.
!
      if ( ibff == natbff ) then

!
!        In the native case, just read the components of the file
!        record from the file.
!
         read ( unit   = lun, &
                rec    = frlocr, &
                iostat = iostat  ) locidw, locnd,  locni, locifn, &
                                   locfwd, locbwd, locfre

!
!        Since this routine does not signal any IOSTAT based
!        errors, return if a non-zero value is assigned to IOSTAT.
!
         if ( iostat /= 0 ) return

!
!     Process the non-native binary file format case.
!
      else

!
!        Read the data record as characters.
!
         read ( unit   = lun, &
                rec    = frlocr, &
                iostat = iostat  ) chrbuf

!
!        Again, since this routine does not signal any IOSTAT
!        based errors, return if one occurs.
!
         if ( iostat /= 0 ) return

!
!        Assign the character components of the file record.
!
         locidw = chrbuf( 1      : idwlen            )
         locifn = chrbuf( ifnstr : (ifnstr+ifnlen-1) )

!
!        Convert the integer components.
!
         cindex = idwlen + 1
         call zzxlatei ( ibff, chrbuf(cindex:cindex+inlen-1), &
                         1,    locnd                          )

         cindex = cindex + inlen
         call zzxlatei ( ibff, chrbuf(cindex:cindex+inlen-1), &
                         1,    locni                          )

         cindex = ifnstr + ifnlen
         call zzxlatei ( ibff, chrbuf(cindex:cindex+inlen-1), &
                         1,    locfwd                         )

         cindex = cindex + inlen
         call zzxlatei ( ibff, chrbuf(cindex:cindex+inlen-1), &
                         1,    locbwd                         )

         cindex = cindex + inlen
         call zzxlatei ( ibff, chrbuf(cindex:cindex+inlen-1), &
                         1,    locfre                         )

         if ( failed() ) return

      end if

!
!     Transfer the contents of the record to the output arguments
!     and return to the caller.
!
      found = .true.

      idword = locidw
      nd     = locnd
      ni     = locni
      ifname = locifn
      fward  = locfwd
      bward  = locbwd
      free   = locfre

      return

      end
