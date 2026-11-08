!$Procedure   ZZEKRD03 ( EK, read class 3 column entry elements )

      subroutine zzekrd03 (  handle,  segdsc,  coldsc, &
                             recptr,  cvlen,   cval,    isnull  )
      use pepper_kinds, only: ip

      implicit none

! $ Abstract
!
!     Read a column entry from a specified record in a class 3 column.
!     Class 3 columns contain scalar character values.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
!     PRIVATE
!
! $ Declarations

      include 'ekbool.inc'
      include 'ekcoldsc.inc'
      include 'ekcnamsz.inc'
      include 'ekdatpag.inc'
      include 'ekrecptr.inc'
      include 'eksegdsc.inc'
      include 'ektype.inc'

      integer(ip)           handle
      integer(ip)           segdsc ( sdscsz )
      integer(ip)           coldsc ( cdscsz )
      integer(ip)           recptr
      integer(ip)           cvlen
      character(*)          cval
      logical               isnull

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle attached to EK file.
!     SEGDSC     I   Segment descriptor.
!     COLDSC     I   Column descriptor.
!     RECPTR     I   Record pointer.
!     CVLEN      O   Length of returned character value.
!     CVAL       O   Character value in column entry.
!     ISNULL     O   Flag indicating whether column entry is null.
!
! $ Detailed_Input
!
!     HANDLE         is an EK file handle.
!
!     SEGDSC         is the descriptor of the segment from which data is
!                    to be read.
!
!     COLDSC         is the descriptor of the column from which data is
!                    to be read.
!
!     RECPTR         is a pointer to the record containing the column
!                    entry to be written.
!
! $ Detailed_Output
!
!     CVLEN          is the length of the returned string value.  This
!                    is the index of the last non-blank character of
!                    the string.  This definition applies to both fixed-
!                    and variable-length strings.
!
!                    CVLEN is set to 1 if the column entry is null.
!
!     CVAL           is the value read from the specified column entry.
!                    If CVAL has insufficient length to hold the
!                    returned string value, the output value is
!                    truncated on the right.  Entries that are shorter
!                    than the string length of CVAL are padded with
!                    trailing blanks.
!
!     ISNULL         is a logical flag indicating whether the entry is
!                    null.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If HANDLE is invalid, the error will be diagnosed by routines
!         called by this routine.
!
!     2)  If the specified column entry has not been initialized, the
!         error SPICE(UNINITIALIZED) is signaled.
!
!     3)  If the ordinal position of the column specified by COLDSC
!         is out of range, the error SPICE(INVALIDINDEX) is signaled.
!
!     4)  If the output string CVAL is too short to accommodate the
!         returned string value, the output value is truncated on the
!         right.  No error is signaled.
!
!     5)  If an I/O error occurs while reading the indicated file,
!         the error will be diagnosed by routines called by this
!         routine.
!
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This routine is a utility for reading data from class 3 columns.
!
! $ Examples
!
!     See EKRCEC.
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
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.4.0, 07-FEB-2015 (NJB)
!
!        Now uses ERRHAN to insert DAS file name into
!        long error messages.
!
!        Bug fix: changed max column index in long error
!        message from NREC to NCOLS.
!
!-    SPICELIB Version 1.3.0, 31-MAY-2010 (NJB)
!
!        Bug fix: call to DASRDI was overwriting local memory. This
!        problem did not affect operation of the routine except on
!        the Mac/Intel/OSX/ifort/32-bit platform, on which it caused
!        a segmentation fault when this routine was compiled with
!        default optimization.
!
!-    SPICELIB Version 1.2.0, 23-JUL-1999 (NJB)
!
!        Error check for string truncation on output was removed.
!        This error check interfered with the use of this routine
!        (via a call to ZZEKRSC) within ZZEKJSRT, which relies on
!        being able to read into a buffer initial substrings of scalar
!        data.
!
!-    SPICELIB Version 1.1.0, 28-JUL-1997 (NJB)
!
!        Error check for string truncation on output was added.
!        SHORT error message SPICE(UNINITIALIZEDVALUE) was shortened
!        to SPICE(UNINITIALIZED).  Error messages were enhanced so
!        as to use column names rather than indices.  Miscellaneous
!        header fixes were made.
!
!-    SPICELIB Version 1.0.0, 18-OCT-1995 (NJB)
!
!-&

! $ Revisions
!
!-    SPICELIB Version 1.2.0, 23-JUL-1999 (NJB)
!
!        Error check for string truncation on output was removed.
!        This error check interfered with the use of this routine
!        (via a call to ZZEKRSC) within ZZEKJSRT, which relies on
!        being able to read into a buffer initial substrings of scalar
!        data.
!
!-    SPICELIB Version 1.1.0, 25-JUL-1997 (NJB)
!
!        Error check for string truncation on output was added.
!        SHORT error message SPICE(UNINITIALIZEDVALUE) was shortened
!        to SPICE(UNINITIALIZED), since the previous string exceeded
!        the maximum allowed length for the short error message.
!
!        Error messages were enhanced so as to use column names rather
!        than indices.
!
!-&

!
!     Non-SPICELIB functions
!
      integer(ip)           zzekrp2n

!
!     Local variables
!
      character(cnamsz)     column

      integer(ip)           avail
      integer(ip)           b
      integer(ip)           bpos
      integer(ip)           colidx
      integer(ip)           datptr
      integer(ip)           e
      integer(ip)           epos
      integer(ip)           l
      integer(ip)           n
      integer(ip)           ncols
      integer(ip)           p
      integer(ip)           pbase
      integer(ip)           relptr
      integer(ip)           ptrloc
      integer(ip)           recno

!
!     Make sure the column exists.
!
      ncols   =  segdsc ( ncidx  )
      colidx  =  coldsc ( ordidx )

      if (  ( colidx < 1 ) .or. ( colidx > ncols )  ) then

         recno  =  zzekrp2n ( handle, segdsc(snoidx), recptr )

         call setmsg ( 'Column index = #; valid range is 1:#.' // &
                       'SEGNO = #; RECNO = #; EK = #'          )
         call errint ( '#',  colidx                            )
         call errint ( '#',  ncols                             )
         call errint ( '#',  segdsc(snoidx)                    )
         call errint ( '#',  recno                             )
         call errhan ( '#',  handle                            )
         call sigerr ( 'SPICE(INVALIDINDEX)'                   )
         return

      end if

!
!     Compute the data pointer location, and read both the pointer
!     and the stored string size.
!
      ptrloc  =  recptr + dptbas + colidx

      call dasrdi  ( handle, ptrloc, ptrloc, datptr )

      if ( datptr > 0 ) then
!
!        Read the value.  This is slightly more complicated than
!        the numeric cases, because the value may be spread across
!        multiple pages.  Also, we must not write past the end of the
!        output string.
!
!        We'll need the number of the page at which the first character
!        of the string is stored.  This page contains at least one
!        character of the data value.
!
         call zzekgei ( handle, datptr, cvlen )

!
!        Set the data pointer to the start of the string data, skipping
!        over the encoded string length.
!
         datptr  =  datptr + encsiz

         n       =  min ( cvlen, len(cval) )

!
!        Read the available data from the page under consideration.
!
         call zzekpgpg ( chr, datptr, p, pbase )

         relptr =  datptr - pbase
         avail  =  min  ( n,  cpsize - relptr + 1 )
         b      =  datptr
         e      =  datptr + avail - 1
         bpos   =  1
         epos   =  avail
         l      =  epos - bpos + 1

         call dasrdc ( handle, b, e, bpos, epos, cval )

         n      =  n - l

         do while ( n > 0 )
!
!           Read the forward page pointer from the current page; find
!           the base address of the referenced page.
!
            call zzekgei  ( handle,  pbase+cfpidx,  p     )
            call zzekpgbs ( chr,     p,             pbase )

            avail  =  min  ( n, cpsize )
            b      =  pbase + 1
            e      =  pbase + avail
            bpos   =  epos  + 1
            epos   =  epos  + avail

            call dasrdc ( handle, b, e, bpos, epos, cval )

            n      =  n    - avail
            bpos   =  epos + 1

         end do

!
!        Blank-pad CVAL if required.
!
         if ( len(cval) > epos ) then
            cval( epos+1 : )  =  ' '
         end if

         isnull  =  .false.

      else if ( datptr == null ) then
!
!        The value is null.
!
         isnull =  .true.
         cvlen  =   1

      else if (       ( datptr == uninit ) &
                 .or. ( datptr == noback )  )  then
!
!        The data value is absent.  This is an error.
!
         recno  =  zzekrp2n ( handle, segdsc(snoidx), recptr )

         call zzekcnam ( handle, coldsc, column )

         call setmsg ( 'Attempted to read uninitialized column '    // &
                       'entry.  SEGNO = #; COLUMN = #; RECNO = #; ' // &
                       'EK = #'                                      )
         call errint ( '#',  segdsc(snoidx)                          )
         call errch  ( '#',  column                                  )
         call errint ( '#',  recno                                   )
         call errhan ( '#',  handle                                  )
         call sigerr ( 'SPICE(UNINITIALIZED)'                        )
         return

      else
!
!        The data pointer is corrupted.
!
         recno  =  zzekrp2n ( handle, segdsc(snoidx), recptr )

         call zzekcnam ( handle,   coldsc, column )

         call setmsg ( 'Data pointer is corrupted. SEGNO = #; '  // &
                       'COLUMN =  #; RECNO = #; EK = #'          )
         call errint ( '#',  segdsc(snoidx)                      )
         call errch  ( '#',  column                              )
         call errint ( '#',  recno                               )
         call errhan ( '#',  handle                              )
         call sigerr ( 'SPICE(BUG)'                              )
         return

      end if

      return
      end
