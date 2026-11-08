!$Procedure ZZEKIF01 ( EK, initialize type 1 segment for fast load )

      subroutine zzekif01 ( handle, segno, rcptrs )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Initialize a new type 1 EK segment to allow fast loading.
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


      include 'ekdatpag.inc'
      include 'ekrecptr.inc'
      include 'eksegdsc.inc'
      include 'ektype.inc'

      integer(ip)           handle
      integer(ip)           segno
      integer(ip)           rcptrs ( * )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     SEGNO      I   Segment number.
!     RCPTRS     O   Array of record pointers.
!
! $ Detailed_Input
!
!     HANDLE         is the handle of an EK file open for write access.
!                    A new type 1 segment is to be created in this file
!                    via a fast load.  The segment's metadata has
!                    already been set up by EKBSEG.
!
!     SEGNO          is the number of the segment to prepare for a
!                    fast load.
!
! $ Detailed_Output
!
!     RCPTRS         is an array of record pointers for the input
!                    segment.  This array must not be modified by the
!                    caller.
!
!                    The array RCPTRS must be passed as an input to
!                    each column addition routine called while
!                    writing the specified segment.
!
!                    RCPTRS must be declared with dimension equal to
!                    the number of rows in the segment.
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
!     This routine carries out the type-1-specific preparation for
!     populating a type 1 EK segment with data via the fast column
!     loader routines.  This routine expects the segment's metadata to
!     already have been written by EKBSEG.
!
! $ Examples
!
!     See EKIFLD.
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
!-    SPICELIB Version 1.0.1, 06-SEP-2006 (NJB)
!
!        Added Restrictions section to header.  Changed
!        previous version line's product from "Beta" to "SPICELIB."
!
!-    SPICELIB Version 1.0.0, 06-NOV-1995 (NJB)
!
!-&


!
!     SPICELIB functions
!
      logical               return

!
!     Local variables
!
      integer(ip)           base
      integer(ip)           i
      integer(ip)           j
      integer(ip)           ncols
      integer(ip)           npage
      integer(ip)           nr
      integer(ip)           nrows
      integer(ip)           nrp
      integer(ip)           p
      integer(ip)           pbase
      integer(ip)           recno
      integer(ip)           remain
      integer(ip)           rpsize
      integer(ip)           segdsc ( sdscsz )
      integer(ip)           top

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'ZZEKIF01' )
      end if

!
!     Read in the segment descriptor.
!
      call zzeksdsc (  handle,  segno,  segdsc )

      ncols  =  segdsc(ncidx)
      nrows  =  segdsc(nridx)

!
!     Empty the EK scratch area stack.
!
      call zzekstop ( top )
      call zzeksdec ( top )

!
!     Push the handle and segment number onto the stack.
!
      call zzekspsh ( 1, handle )
      call zzekspsh ( 1, segno  )

!
!     The segment will require a record pointer structure for each row
!     in the segment.  Right now, all we're going to do is allocate
!     integer pages to hold these structures and save the base
!     addresses of each structure.
!
!     We compute the number of record pointers that can fit on one page.
!     We also compute the number of pages we'll need to hold the
!     pointers.
!
      rpsize  =    dptbas + ncols
      nrp     =    ipsize / rpsize
      npage   =  ( nrows + nrp - 1 ) / nrp

!
!     We'll compute addresses of record pointers a pageful at a time.
!
      remain  =  nrows
      recno   =  0

      do i = 1, npage
!
!        Allocate a page to hold the record pointers.  A page from
!        the free list is acceptable, hence the argument .FALSE.
!        passed to ZZEKAPS.
!
         call zzekaps  ( handle, segdsc, int, .false., p, pbase )

!
!        NR is the number of record pointers we'll eventually write to
!        this page.
!
         nr  =  min ( nrp, remain )

         do j = 1, nr
!
!           Record the base address of the current record pointer
!           in the record pointer array.
!
            base                =  (j-1) * rpsize
            rcptrs ( recno+j )  =  pbase + base

         end do

         recno   =  recno   +  nr
         remain  =  remain  -  nr

      end do


      call chkout ( 'ZZEKIF01' )
      return
      end
