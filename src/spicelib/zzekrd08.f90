!$Procedure   ZZEKRD08 ( EK, read class 8 column entry )

      subroutine zzekrd08 (  handle,  segdsc,  coldsc, &
                             recptr,  dval,    isnull  )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Read a column entry from a specified record in a class 8 column.
!     Class 8 columns contain fixed-count, scalar, double precision
!     values.
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
      include 'ekclas08.inc'
      include 'ekcoldsc.inc'
      include 'ekdatpag.inc'
      include 'ekpage.inc'
      include 'eksegdsc.inc'
      include 'ektype.inc'

      integer(ip)           handle
      integer(ip)           segdsc ( sdscsz )
      integer(ip)           coldsc ( cdscsz )
      integer(ip)           recptr
      real(wp)              dval
      logical               isnull

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle attached to EK file.
!     SEGDSC     I   Segment descriptor.
!     COLDSC     I   Column descriptor.
!     RECPTR     I   Record pointer.
!     DVAL       O   Double precision value in column entry.
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
!                    entry to be written.  For class 8 columns, record
!                    pointers are identical to record numbers.
!
! $ Detailed_Output
!
!     DVAL           is the value read from the specified column entry.
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
!     This routine is a utility for reading data from class 8 columns.
!
! $ Examples
!
!     See EKRCED.
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
      integer(ip)           mdat   ( mdsz08 )
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

         call setmsg ( 'Column index = #; valid range is 1:#.' &
         //            'SEGNO = #; RECNO = #; EK = #'          )
         call errint ( '#',  colidx                            )
         call errint ( '#',  ncols                             )
         call errint ( '#',  segdsc(snoidx)                    )
         call errint ( '#',  recno                             )
         call errhan ( '#',  handle                            )
         call sigerr ( 'SPICE(INVALIDINDEX)'                   )
         return

      end if

!
!     Read the metadata block.  There are two items in the block:
!
!        1) The base address of the first page of the data
!        2) The base address of the null flag array, if nulls are
!           permitted.
!
      call dasrdi ( handle, metloc+1, metloc+mdsz08, mdat )

      datbas = mdat ( dbix08 )
      nflbas = mdat ( nfix08 )

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

         if ( isnull ) return

      end if

!
!     If we're still here, we'll read the data value.
!
      isnull  =  .false.

!
!     The address calculation for the value is analogous to that
!     for the null flag.
!
      q       =  (recptr-1)  / dpsize
      r       =  recptr - q * dpsize
      offset  =  r      + q * pgsizd
      addrss  =  datbas + offset

      call dasrdd ( handle, addrss, addrss, dval )


      return
      end
