!$Procedure     ZZEKWPAC ( EK, write paged array, character )

      subroutine zzekwpac ( handle, segdsc, nvals, l, cvals, p, base )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Write a character array out to a contiguous set of EK pages.
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


      include  'ekdatpag.inc'
      include  'ekpage.inc'
      include  'eksegdsc.inc'
      include  'ektype.inc'

      integer(ip)           handle
      integer(ip)           segdsc ( * )
      integer(ip)           nvals
      integer(ip)           l
      character(*)          cvals  ( * )
      integer(ip)           p
      integer(ip)           base

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle attached to EK file.
!     SEGDSC     I   Descriptor of segment that owns the array.
!     NVALS      I   Number of values to write.
!     L          I   String length.
!     CVALS      I   Character values.
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
!     NVALS          is the number of character values to write.
!
!     L              is the length of the input values.  The input
!                    strings are expected to be short compared to the
!                    size of a character page.
!
!     CVALS          is an array of character values.  The first L
!                    characters of each element of CVALS will be
!                    written.  The strings are not split across pages;
!                    instead, unused space is left at the end of each
!                    page if the string length does not divide the
!                    page size evenly.
!
! $ Detailed_Output
!
!     P              is the number of the first page to which the
!                    input values are written.  CVALS(1) is written to
!                    a range of DAS character words starting with the
!                    the first word of page P.  The values are written
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
!     3)  If L is negative or greater than the input string length,
!         the error SPICE(INVALIDSIZE) is signaled.
!
!     4)  If L is greater than the size of the data area of a
!         character page, the error SPICE(INVALIDSIZE) is signaled.
!
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This routine writes an array of character values to a contiguous
!     set of new character pages.  The first element of the input array
!     is written to a range of values starting at the first DAS address
!     of the first page of the set.
!
!     Note that the values do not occupy a contiguous range of DAS
!     character words, since each page contains several addresses
!     reserved for bookkeeping information, and since there may be
!     unused space at the end of a data page.  However, since each page
!     contains exactly CPSIZE characters and has size PGSIZC, it's easy
!     to compute the DAS address of the Ith element in the array:
!
!        N          =   IPSIZE  / L
!        Q          =   I       / N
!        R          =   I  -  Q * N
!
!        ADDRSS(I)  =  BASE  +  Q * PGSIZC  +  (R-1) * L  +  1
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
!-    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB)
!
!        Corrected typos in comments.
!
!-    Beta Version 1.0.0, 07-NOV-1995 (NJB)
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
      integer(ip)           spp
      integer(ip)           to

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'ZZEKWPAC' )
      end if

!
!     Check the input string length.
!
      if (      ( l < 0             ) &
           .or. ( l > len(cvals(1)) ) &
           .or. ( l > cpsize        )  ) then

         call setmsg ( 'String length # is just plain wrong.' )
         call errint ( '#',  l                                )
         call sigerr ( 'SPICE(INVALIDSIZE)'                   )
         call chkout ( 'ZZEKWPAC'                             )
         return

      end if

!
!     Compute the number of strings we can hold in one page.
!
      spp  =  cpsize / l

!
!     Decide how many pages are required to hold the array, and
!     allocate that many new, contiguous pages.
!
      npage  =  ( nvals + spp - 1 ) / spp

      call zzekacps ( handle, segdsc, chr, npage, p, base )

!
!     We'll use FROM to indicate the element of CVALS we're
!     considering and TO to indicate the element of PAGE to write
!     to.
!
      to   =  1
      page =  ' '


      do from = 1, nvals
!
!        The Assignment.
!
         page( to : to+l-1 ) =  cvals(from)
         to                  =  to + l


         if (      ( to   > cpsize-l+1 ) &
              .or. ( from == nvals      )  ) then
!
!           Either the current data page is full, or we've buffered
!           the last of the available data.  It's time to write out the
!           current page.
!
            call zzekpgwc ( handle, p, page  )

!
!           Set the link count.
!
            call zzekslnk ( handle, chr, p,  (to-l)/l  )

!
!           Next page.
!
            p   =  p + 1
            to  =  1

         end if

      end do


      call chkout ( 'ZZEKWPAC' )
      return
      end
