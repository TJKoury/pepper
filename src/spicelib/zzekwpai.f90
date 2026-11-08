!$Procedure     ZZEKWPAI ( EK, write paged array, integer )

      subroutine zzekwpai ( handle, segdsc, nvals, ivals, p, base )

! $ Abstract
!
!     Write an integer array out to a contiguous set of EK pages.
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

      integer               handle
      integer               segdsc ( * )
      integer               nvals
      integer               ivals  ( * )
      integer               p
      integer               base

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle attached to EK file.
!     SEGDSC     I   Descriptor of segment that owns the array.
!     NVALS      I   Number of values to write.
!     IVALS      I   Integer values.
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
!     NVALS          is the number of integer values to write.
!
!     IVALS          is an array of integer values.
!
! $ Detailed_Output
!
!     P              is the number of the first page to which the
!                    input values are written.  IVALS(1) is written to
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
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This routine writes an array of integer values to a contiguous
!     set of new integer pages.  The first element of the input array
!     is written to the first DAS address of the first page of the set.
!
!     Note that the values do not occupy a contiguous range of DAS
!     integer words, since each page contains several addresses
!     reserved for bookkeeping information.  However, since each page
!     contains exactly IPSIZE integers and has size PGSIZI, it's easy
!     to compute the DAS address of the Ith element in the array:
!
!        Q          =   (I-1)  /  IPSIZE
!        R          =   I  -  Q * IPSIZE
!
!        ADDRSS(I)  =  BASE +  Q * PGSIZI + R
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
      integer               from
      integer               npage
      integer               page   ( pgsizi )
      integer               to

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'ZZEKWPAI' )
      end if

!
!     Decide how many pages are required to hold the array, and
!     allocate that many new, contiguous pages.
!
      npage  =  ( nvals + ipsize - 1 ) / ipsize

      call zzekacps ( handle, segdsc, int, npage, p, base )

!
!     We'll use FROM to indicate the element of IVALS we're
!     considering and TO to indicate the element of PAGE to write
!     to.
!
      to  =  1
      call cleari ( pgsizi, page )


      do from = 1, nvals
!
!        The Assignment.
!
         page(to) =  ivals(from)
         to       =  to + 1


         if (  ( to .gt. ipsize ) .or. ( from .eq. nvals )  ) then
!
!           Either the current data page is full, or we've buffered
!           the last of the available data.  It's time to write out the
!           current page.  First set the link count.
!
            page ( ilcidx ) =  to - 1

!
!           Write out the data page.
!
            call zzekpgwi ( handle, p, page  )

!
!           Next page.
!
            p   =  p + 1
            to  =  1

         end if

      end do


      call chkout ( 'ZZEKWPAI' )
      return
      end
