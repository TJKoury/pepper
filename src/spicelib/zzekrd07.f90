!$Procedure   ZZEKRD07 ( EK, read class 7 column entry )

      subroutine zzekrd07 (  handle,  segdsc,  coldsc, &
                             recptr,  ival,    isnull  )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Read a column entry from a specified record in a class 7 column.
!     Class 7 columns contain fixed-count, scalar integer values.
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
      include 'ekclas07.inc'
      include 'ekcoldsc.inc'
      include 'ekdatpag.inc'
      include 'ekpage.inc'
      include 'eksegdsc.inc'
      include 'ektype.inc'

      integer(ip)           handle
      integer(ip)           segdsc ( sdscsz )
      integer(ip)           coldsc ( cdscsz )
      integer(ip)           recptr
      integer(ip)           ival
      logical               isnull

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle attached to EK file.
!     SEGDSC     I   Segment descriptor.
!     COLDSC     I   Column descriptor.
!     RECPTR     I   Record pointer.
!     IVAL       O   Integer value in column entry.
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
!                    entry to be written.  For class 7 columns, record
!                    pointers are identical to record numbers.
!
! $ Detailed_Output
!
!     IVAL           is the value read from the specified column entry.
!
!     ISNULL         is a logical flag indicating whether the column
!                    entry is null.
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
!     3)  If an I/O error occurs while reading the indicated file,
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
!     This routine is a utility for reading data from class 7 columns.
!
! $ Examples
!
!     See EKRCEI.
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
!-    SPICELIB Version 1.1.0, 07-FEB-2015 (NJB)
!
!        Now uses ERRHAN to insert DAS file name into
!        long error messages.
!
!        Bug fix: changed max column index in long error
!        message from NREC to NCOLS.
!
!-    Beta Version 1.0.0, 09-NOV-1995 (NJB)
!
!-&


!
!     Non-SPICELIB functions
!
      integer(ip)           zzekrp2n

!
!     Local variables
!
      character(1)          cflag

      integer(ip)           addrss
      integer(ip)           colidx
      integer(ip)           datbas
      integer(ip)           mdat   ( mdsz07 )
      integer(ip)           metloc
      integer(ip)           ncols
      integer(ip)           nflbas
      integer(ip)           offset
      integer(ip)           q
      integer(ip)           r
      integer(ip)           recno

      logical               nullok

!
!     Use discovery check-in.
!
!
!     Make sure the column exists.
!
      ncols   =  segdsc ( ncidx  )
      colidx  =  coldsc ( ordidx )
      metloc  =  coldsc ( metidx )
      nullok  =  coldsc ( nflidx ) == itrue


      if (  ( colidx < 1 ) .or. ( colidx > ncols )  ) then

         recno  =  zzekrp2n ( handle, segdsc(snoidx), recptr )

         call chkin  ( 'ZZEKRD07'                              )
         call setmsg ( 'Column index = #; valid range is 1:#.' // &
                       'SEGNO = #; RECNO = #; EK = #'          )
         call errint ( '#',  colidx                            )
         call errint ( '#',  ncols                             )
         call errint ( '#',  segdsc(snoidx)                    )
         call errint ( '#',  recno                             )
         call errhan ( '#',  handle                            )
         call sigerr ( 'SPICE(INVALIDINDEX)'                   )
         call chkout ( 'ZZEKRD07'                              )
         return

      end if

!
!     Read the metadata block.  There are two items in the block:
!
!        1) The base address of the first page of the data
!        2) The base address of the null flag array, if nulls are
!           permitted.
!
      call dasrdi ( handle, metloc+1, metloc+mdsz07, mdat )

      datbas = mdat ( dbix07 )
      nflbas = mdat ( nfix07 )

!
!     If null values are permitted, the first step is to get
!     the null flag for the value of interest.  Compute the
!     address of this flag.
!
!     There are CPSIZE null flags per page, and each page has size
!     PGSIZC.  The null flags start at the beginning of the page.
!
      if ( nullok ) then

         q       =  (recptr-1) / cpsize
         r       =  recptr - q * cpsize
         offset  =  r      + q * pgsizc
         addrss  =  nflbas + offset

         call dasrdc ( handle, addrss, addrss, 1, 1, cflag )

         isnull  =  cflag == ctrue

         if ( isnull ) then
            return
         end if

      end if

!
!     If we're still here, we'll read the data value.
!
      isnull  =  .false.

!
!     The address calculation for the value is analogous to that
!     for the null flag.
!
      q       =  (recptr-1)  / ipsize
      r       =  recptr - q * ipsize
      offset  =  r      + q * pgsizi
      addrss  =  datbas + offset

      call dasrdi ( handle, addrss, addrss, ival )

      return
      end
