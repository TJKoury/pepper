!$Procedure   ZZEKRD09 ( EK, read class 9 column entry elements )

      subroutine zzekrd09 (  handle,  segdsc,  coldsc, &
                             recno,   cvlen,   cval,    isnull  )

      implicit none

! $ Abstract
!
!     Read a column entry from a specified record in a class 9 column.
!     Class 9 columns contain fixed record count, fixed-length,
!     scalar character values.
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
      include 'ekclas09.inc'
      include 'ekcnamsz.inc'
      include 'ekcoldsc.inc'
      include 'ekdatpag.inc'
      include 'ekpage.inc'
      include 'eksegdsc.inc'
      include 'ektype.inc'

      integer               handle
      integer               segdsc ( sdscsz )
      integer               coldsc ( cdscsz )
      integer               recno
      integer               cvlen
      character*(*)         cval
      logical               isnull

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle attached to EK file.
!     SEGDSC     I   Segment descriptor.
!     COLDSC     I   Column descriptor.
!     RECNO      I   Record number.
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
!     RECNO          is the number of the record containing the column
!                    entry to be written.
!
! $ Detailed_Output
!
!     CVLEN          is the length of the returned string value.  This
!                    is the declared string length of the column being
!                    read.  Note this definition differs from that used
!                    for class 3 columns.  In the class 9 case, no
!                    string length is stored in the file, so extra work
!                    at run time would be required to determine whether
!                    truncation would occur.
!
!     CVAL           is the value read from the specified column entry.
!                    CVAL must have sufficient length to hold the
!                    returned string value.  Entries that are shorter
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
!     2)  If the ordinal position of the column specified by COLDSC
!         is out of range, the error SPICE(INVALIDINDEX) is signaled.
!
!     3)  If the output string CVAL is too short to accommodate the
!         returned string value, the error SPICE(STRINGTRUNCATED)
!         is signaled.  CVAL must be at least as long as the declared
!         length of the column being read.
!
!     4)  If an I/O error occurs while reading the indicated file,
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
!     This routine is a utility for reading data from class 9 columns.
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
!-    SPICELIB Version 1.2.0, 07-FEB-2015 (NJB)
!
!        Now uses ERRHAN to insert DAS file name into
!        long error messages.
!
!        Bug fix: changed max column index in long error
!        message from NREC to NCOLS.
!
!-    SPICELIB Version 1.1.0, 28-JUL-1997 (NJB)
!
!        Error check for string truncation on output was added.
!        SHORT error message SPICE(UNINITIALIZEDVALUE) was shortened
!        to SPICE(UNINITIALIZED).
!
!        The argument RECPTR was renamed to RECNO.  The reference to
!        ZZEKRP2N was removed.
!
!        Miscellaneous header corrections were made.
!
!-    SPICELIB Version 1.0.0, 09-NOV-1995 (NJB)
!
!-&

! $ Revisions
!
!-    SPICELIB Version 1.1.0, 28-JUL-1997 (NJB)
!
!        Error check for string truncation on output was added.
!        SHORT error message SPICE(UNINITIALIZEDVALUE) was shortened
!        to SPICE(UNINITIALIZED).
!
!        The argument RECPTR was renamed to RECNO.  The reference to
!        ZZEKRP2N was removed.
!
!
!-&


!
!     Local variables
!
      character*(1)         cflag
      character*(cnamsz)    column

      integer               addrss
      integer               colidx
      integer               datbas
      integer               l
      integer               mdat   ( mdsz09 )
      integer               metloc
      integer               ncols
      integer               nflbas
      integer               offset
      integer               q
      integer               r
      integer               spp

      logical               nullok

!
!     Use discovery check-in.
!
!     Make sure the column exists.
!
      ncols   =  segdsc ( ncidx  )
      colidx  =  coldsc ( ordidx )
      metloc  =  coldsc ( metidx )
      nullok  =  coldsc ( nflidx ) .eq. itrue
      l       =  coldsc ( lenidx )

      if (  ( colidx .lt. 1 ) .or. ( colidx .gt. ncols )  ) then

         call chkin  ( 'ZZEKRD09'                              )
         call setmsg ( 'Column index = #; valid range is 1:#.' // &
                       'SEGNO = #; RECNO = #; EK = #'          )
         call errint ( '#',  colidx                            )
         call errint ( '#',  ncols                             )
         call errint ( '#',  segdsc(snoidx)                    )
         call errint ( '#',  recno                             )
         call errhan ( '#',  handle                            )
         call sigerr ( 'SPICE(INVALIDINDEX)'                   )
         call chkout ( 'ZZEKRD09'                              )
         return

      end if

!
!     Since class 9 columns have fixed-length strings, we already
!     know the string length.
!
      cvlen  =  l

      if ( cvlen .gt. len(cval) ) then
!
!        We have a string truncation error.  Look up the column
!        name, record number, and file name before signaling an
!        error.
!
         call zzekcnam ( handle, coldsc, column )

         call chkin  ( 'ZZEKRD09'                                  )
         call setmsg ( 'String value has length #; output string '// &
                       'can hold only # characters.  COLUMN = #; '// &
                       'SEGNO = #; RECNO = #; EK = #'              )
         call errint ( '#',  cvlen                                 )
         call errint ( '#',  len(cval)                             )
         call errch  ( '#',  column                                )
         call errint ( '#',  segdsc(snoidx)                        )
         call errint ( '#',  recno                                 )
         call errhan ( '#',  handle                                )
         call sigerr ( 'SPICE(STRINGTRUNCATED)'                    )
         call chkout ( 'ZZEKRD09'                                  )
         return

      end if

!
!     Read the metadata block.  There are two items in the block:
!
!        1) The base address of the first page of the data
!        2) The base address of the null flag array, if nulls are
!           permitted.
!
      call dasrdi ( handle, metloc+1, metloc+mdsz09, mdat )

      datbas = mdat ( dbix09 )
      nflbas = mdat ( nfix09 )

!
!     If null values are permitted, the first step is to get
!     the null flag for the value of interest.  Compute the
!     address of this flag.
!
!     There are CPSIZE null flags per page, and each page has size
!     PGSIZC.  The null flags start at the beginning of the page.
!
      if ( nullok ) then

         q       =  (recno-1)  / cpsize
         r       =  recno  - q * cpsize
         offset  =  r      + q * pgsizc
         addrss  =  nflbas + offset

         call dasrdc ( handle, addrss, addrss, 1, 1, cflag )

         isnull  =  cflag .eq. ctrue

         if ( isnull ) then
            return
         end if

      end if

!
!     If we're still here, we'll read the data value.
!
      isnull  =  .false.

!
!     The address calculation for the value is similar to that
!     for the null flag.  However, the string length must be
!     taken into account.
!
      spp     =  cpsize     /   l
      q       =  (recno-1)  /   spp
      r       =  recno      -   q*spp
      addrss  =  datbas     +   q*pgsizc  +  (r-1)*l  +  1

      call dasrdc ( handle, addrss, addrss+l-1, 1, l, cval )

!
!     Blank-pad CVAL if required.
!
      if ( len(cval) .gt. l ) then
         cval( l+1 : )  =  ' '
      end if


      return
      end
