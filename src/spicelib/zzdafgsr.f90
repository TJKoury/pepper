!$Procedure ZZDAFGSR ( Private --- DAF Get Summary/Descriptor Record )

      subroutine zzdafgsr ( handle, recno, nd, ni, dprec, found )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Read a summary/descriptor record from a DAF.
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

      integer               handle
      integer               recno
      integer               nd
      integer               ni
      double precision      dprec  ( * )
      logical               found

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of the DAF.
!     RECNO      I   Record number.
!     ND         I   Number of double precision components in a summary.
!     NI         I   Number of integer components in a summary.
!     DPREC      O   Contents of the record.
!     FOUND      O   Logical indicating whether the record was found.
!
! $ Detailed_Input
!
!     HANDLE     is the handle associated with the DAF.
!
!     RECNO      is the record number of a particular summary record
!                within the DAF, whose contents are to be read.
!     ND,
!     NI         are the number of double precision and integer
!                components, respectively, in each array summary
!                in the specified file.
!
! $ Detailed_Output
!
!     DPREC      contains the contents of the specified record from
!                the DAF associated with HANDLE, properly translated
!                for use on the native environment.
!
!     FOUND      is TRUE when the specified record is found, and is
!                FALSE otherwise.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) SPICE(HANDLENOTFOUND) is signaled if HANDLE can not be
!        found in the set of loaded handles.
!
!     2) Routines in the call tree of this routine may trap and
!        signal errors.
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
!     This routine reads summary records of double precision
!     numbers which contain integers packed through an EQUIVALENCE
!     statement from native and supported non-native DAFs.
!
!     The size of the character buffer and the number of records
!     read may have to change to support new environments.  As of
!     the original release of this routine, all systems currently
!     supported have a 1 kilobyte record length.
!
! $ Examples
!
!     See DAFGSR for sample usage.
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
!     4) Characters a byte-sized, 8 characters constitute a double
!        precision number, and 4 characters an integer.
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
!-    SPICELIB Version 1.0.1, 02-OCT-2021 (NJB)
!
!        Corrected typo in comments. Reordered header sections.
!
!-    SPICELIB Version 1.0.0, 12-NOV-2001 (FST)
!
!
!-&

!
!     SPICELIB Functions
!
      integer               isrchc

      logical               failed
      logical               return

!
!     Local Parameters
!
!     Length in bytes of double precision numbers and integers.
!
      integer               dplen
      parameter           ( dplen = 8       )

      integer               inlen
      parameter           ( inlen = dplen/2 )

!
!     Local Variables
!
      character*(cbfsiz)    chrbuf
      character*(filen)     fname
      character*(strsiz)    strbff ( numbff )
      character*(strsiz)    tmpstr

      double precision      dpbuf  ( 128 )

      integer               cindex
      integer               dindex
      integer               i
      integer               iamh
      integer               iarch
      integer               ibff
      integer               inbuf  ( 256 )
      integer               iostat
      integer               left
      integer               lun
      integer               natbff
      integer               nsum
      integer               sumsiz

      logical               first
      logical               locfnd

!
!     Equivalence DPBUF to INBUF.
!
      equivalence         ( dpbuf, inbuf )

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
         call chkin ( 'ZZDAFGSR' )
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

         if ( natbff .eq. 0 ) then

            call setmsg ( 'The binary file format, ''#'', is not ' &
            //            'supported by this version of the toolkit. ' &
            //            'This is a serious problem, contact NAIF.'   )
            call errch  ( '#', tmpstr                                  )
            call sigerr ( 'SPICE(BUG)'                                 )
            call chkout ( 'ZZDAFGSR'                                   )
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
         call chkout ( 'ZZDAFGSR'                                   )
         return
      end if

!
!     Now get a logical unit for the handle.  Check FAILED()
!     in case an error occurs.
!
      call zzddhhlu ( handle, 'DAF', .false., lun )

      if ( failed() ) then
         found = .false.
         call chkout ( 'ZZDAFGSR' )
         return
      end if

!
!     Branch based on whether the binary file format is native
!     or not.  Only supported formats can be opened by ZZDDHOPN,
!     so no check of IBFF is required.
!
      if ( ibff .eq. natbff ) then

!
!        In the native case, just read the array of double precision
!        numbers from the file.  The packed integers will be
!        processed properly by the READ.
!
         read ( unit   = lun, &
                rec    = recno, &
                iostat = iostat ) ( dpbuf(i), i = 1, 128 )

!
!        Since this routine does not signal any IOSTAT based
!        errors, return if a non-zero value is assigned to IOSTAT.
!
         if ( iostat .ne. 0 ) then
            call chkout ( 'ZZDAFGSR' )
            return
         end if

!
!     Process the non-native binary file format case.
!
      else

!
!        Read the record as characters.
!
         read ( unit   = lun, &
                rec    = recno, &
                iostat = iostat ) chrbuf

!
!        Again, since this routine does not signal any IOSTAT
!        based errors, return if one occurs.
!
         if ( iostat .ne. 0 ) then
            call chkout ( 'ZZDAFGSR' )
            return
         end if

!
!        Translate the summary record.  First extract the leading
!        3 double precision numbers from the summary record as these
!        respectively are NEXT, PREV, and NSUM.
!
         call zzxlated ( ibff, chrbuf(1:3*dplen), 128, dpbuf )

!
!        Check FAILED() in case the translation process fails for
!        any reason.
!
         if ( failed() ) then
            call chkout ( 'ZZDAFGSR' )
            return
         end if

!
!        Convert NSUM to an integer, and compute the number of
!        double precision numbers required to store each individual
!        summary in the record.
!
         nsum   = int ( dpbuf(3) )
         sumsiz = ( nd + (ni+1)/2 )

!
!        Convert each of the summaries one at a time.
!
         do i = 1, nsum

!
!           Set the start index into the double precision array
!           to receive the components.  Also set the character
!           substring index to the start location for this summary.
!           In the diagram below, each box represents a double
!           precision number.  The figure assumes SUMSIZ is 5
!           double precision numbers:
!
!                 |--- 1 ---|--- 2 ---|--- 3 ---|   |- (I-1) -|
!           -------------------------------------   -------------
!           | | | | | | | | | | | | | | | | | | |...| | | | | | |...
!           -------------------------------------   -------------
!           |-----|                                            ^
!              ^                                               |
!              |                                            Summary
!           NEXT, PREV, NSUM                                 Start
!
            dindex = (i-1)*sumsiz + 4
            cindex = dplen*(dindex-1) + 1

!
!           First, check to see if there are any double precision
!           numbers to translate.  If so, translate, and then
!           increment DINDEX and CINDEX accordingly.
!
            if ( nd .gt. 0 ) then

!
!              DPBUF has room for 128 double precision numbers
!              total.  Compute the amount of space left in the
!              buffer.
!
               left = 128 - (i-1)*sumsiz - 3

               call zzxlated ( ibff, &
                               chrbuf(cindex:cindex+nd*dplen-1), &
                               left, &
                               dpbuf(dindex)                     )

!
!              If the translation routine fails for any reason,
!              check out and return.
!
               if ( failed() ) then
                  call chkout ( 'ZZDAFGSR' )
                  return
               end if

               dindex = dindex + nd
               cindex = cindex + dplen*nd

            end if

!
!           At this point DINDEX and CINDEX are pointing at the
!           locations for the packed integers in the record.
!           Use DINDEX to compute the index into INBUF, the
!           equivalenced integer buffer and translate.
!
            if ( ni .gt. 0 ) then

!
!              INBUF has room for 256 integers total.  Compute
!              the amount of space left in the buffer.  Since
!              it is equivalenced to DPBUF, account for the
!              double precision numbers that were just added.
!
               left = 256 - 2*(i-1)*sumsiz - nd*2 - 6

               call zzxlatei ( ibff, &
                               chrbuf(cindex:cindex+ni*inlen-1), &
                               left, &
                               inbuf(2*dindex-1)                 )

!
!              If the translation routine fails for any reason,
!              check out and return.
!
               if ( failed() ) then
                  call chkout ( 'ZZDAFGSR' )
                  return
               end if

!
!              Now check to see if NI is odd.  If so, then zero
!              the last integer occupied by the newly translated
!              summary.  This is necessary to purge any garbage
!              present in memory.
!
               if ( mod(ni,2) .eq. 1 ) then
                  inbuf(2*dindex-1+ni) = 0
               end if

            end if

         end do

!
!        Translating garbage is a bad idea in general, so set
!        the any remaining double precision numbers in the summary
!        record to 0.
!
         dindex = nsum*sumsiz + 4

         do i = dindex, 128
            dpbuf(i) = 0
         end do

      end if

!
!     Transfer the DPs to the output argument and return to the
!     caller.
!
      found = .true.
      call moved ( dpbuf, 128, dprec )

      call chkout( 'ZZDAFGSR' )
      return

      end
