!$Procedure     ZZEKWPAL ( EK, write paged array, logical )

      subroutine zzekwpal ( handle, segdsc, nvals, lvals, p, base )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Write a logical array out to a contiguous set of EK pages.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
!
! $ Declarations

      include  'ekbool.inc'
      include  'ekdatpag.inc'
      include  'ekpage.inc'
      include  'eksegdsc.inc'
      include  'ektype.inc'

      integer(ip)           handle
      integer(ip)           segdsc ( * )
      integer(ip)           nvals
      logical               lvals  ( * )
      integer(ip)           p
      integer(ip)           base

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle attached to EK file.
!     SEGDSC     I   Descriptor of segment that owns the array.
!     NVALS      I   Number of values to write.
!     LVALS      I   Logical values.
!     P          O   Number of first page containing array.
!     BASE       O   Base address of first page.
!
! $ Detailed_Input
!
!     HANDLE         the handle of an EK file that is open for writing.
!
!     SEGDSC         is a descriptor for the segment to which data is
!                    to be added.  The segment descriptor is not
!                    updated by this routine, but some fields in the
!                    descriptor will become invalid after this routine
!                    returns.
!
!     NVALS          is the number of logical values to write.
!
!     LVALS          is an array of logical values.  The values will
!                    be stored as characters, with one character used
!                    per element of LVALS.
!
! $ Detailed_Output
!
!     P              is the number of the first page to which the
!                    input values are written.  The character
!                    representing LVALS(1) is written to the first DAS
!                    character of page P.  The values are written
!                    to a contiguous set of pages in increasing order.
!
!     BASE           is the base address of P.  BASE is the predecessor
!                    of the first DAS address belonging to page P.
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
!     2)  If an I/O error occurs while reading or writing the indicated
!         file, the error will be diagnosed by routines called by this
!         routine.
!
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This routine writes an array of logical values to a contiguous
!     set of new character pages.  The first element of the input array
!     is written to a range of values starting at the first DAS address
!     of the first page of the set.
!
!     This routine supports creation of null flag arrays for fixed-count
!     column classes.
!
!     Note that the values do not occupy a contiguous range of DAS
!     character words, since each page contains several addresses
!     reserved for bookkeeping information, and since there may be
!     unused space at the end of a data page.
!
! $ Examples
!
!     See ZZEKAC07.
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
!-    Beta Version 1.0.0, 08-NOV-1995 (NJB)
!
!-&

!
!     SPICELIB functions
!
      logical               return

!
!     Local variables
!
      character(pgsizc)     page

      integer(ip)           from
      integer(ip)           npage
      integer(ip)           to

!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     Decide how many pages are required to hold the array, and
!     allocate that many new, contiguous pages.
!
      npage  =  ( nvals + cpsize - 1 ) / cpsize

      call zzekacps ( handle, segdsc, chr, npage, p, base )

!
!     Write the input data out to the target file a page at a time.
!
!     We'll use FROM to indicate the element of LVALS we're
!     considering and TO to indicate the element of PAGE to write
!     to.
!
      to      =  1
      page    =  ' '

      do from = 1, nvals

!
!        The Assignment.
!
         if ( lvals(from) ) then
            page( to : to ) =  ctrue
         else
            page( to : to ) =  cfalse
         end if

         to  =  to + 1

         if (  ( to > cpsize ) .or. ( from == nvals )  ) then
!
!           Either the current data page is full, or we've buffered
!           the last of the available data.  It's time to write out the
!           current page.
!
!           Write out the data page.
!
            call zzekpgwc ( handle, p, page  )

!
!           Set the link count.
!
            call zzekslnk ( handle, chr, p, to-1 )

!
!           Next page.
!
            p   =  p + 1
            to  =  1

         end if

      end do

      return
      end
