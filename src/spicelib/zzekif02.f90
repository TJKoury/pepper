!$Procedure ZZEKIF02 ( EK, initialize type 2 segment for fast load )

      subroutine zzekif02 ( handle, segno )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Initialize a new type 2 EK segment to allow fast loading.
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


      include 'ekclas07.inc'
      include 'ekclas08.inc'
      include 'ekclas09.inc'
      include 'ekcoldsc.inc'
      include 'ekpage.inc'
      include 'eksegdsc.inc'
      include 'ektype.inc'

      integer(ip)           handle
      integer(ip)           segno

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     SEGNO      I   Segment number.
!
! $ Detailed_Input
!
!     HANDLE         is the handle of an EK file open for write access.
!                    A new type 2 segment is to be created in this file
!                    via a fast load.  The segment's metadata has
!                    already been set up by EKBSEG.
!
!     SEGNO          is the number of the segment to prepare for a
!                    fast load.
!
! $ Detailed_Output
!
!     None.  See $Particulars for a description of the effect of this
!     routine.
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
!     This routine carries out the type-2-specific preparation for
!     populating a type 2 EK segment with data via the fast column
!     loader routines.  This routine expects the segment's metadata to
!     already have been written by EKBSEG.
!
!     This routine expects the segment to contain columns having class
!     7, 8, or 9.
!
! $ Examples
!
!     See EKIFLD.
!
! $ Restrictions
!
!     1)  Assumes total number of words required for column metadata
!         is no greater than IPSIZE.  Currently, with a maximum of 100
!         columns and a maximum metadata size of 2 words per column,
!         this condition is met.
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
!-    Beta Version 1.0.0, 09-NOV-1995 (NJB)
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
      integer(ip)           class
      integer(ip)           dscbas
      integer(ip)           i
      integer(ip)           mbase
      integer(ip)           ncols
      integer(ip)           nrows
      integer(ip)           offset
      integer(ip)           p
      integer(ip)           page   ( pgsizi )
      integer(ip)           segdsc ( sdscsz )

!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     Read in the segment descriptor.
!
      call zzekmloc ( handle, segno,   page,         mbase  )
      call dasrdi   ( handle, mbase+1, mbase+sdscsz, segdsc )

      ncols  =  segdsc(ncidx)
      nrows  =  segdsc(nridx)

!
!     Allocate space for column metadata.  We assume that one page
!     of IPSIZE integers is enough room.
!
      call zzekaps ( handle, segdsc, int, .false., p, base  )


      offset  =  base

      do i = 1, ncols
!
!        Read the class from the descriptor of the Ith column directly
!        from the file.  We'll need the descriptor's address in order to
!        update the descriptor in the file.
!
         dscbas  =  mbase + cdoff + (i-1)*cdscsz

         call dasrdi ( handle, dscbas+clsidx, dscbas+clsidx, class )

!
!        Update the file.  Set the column descriptor's metadata pointer
!        to the base address of the metadata area.
!
         call dasudi ( handle, dscbas+metidx, dscbas+metidx, offset )

!
!        Increment the metadata offset by the size of the metadata
!        for the current column.  The classes of interest range from
!        7 to 9.
!
         if ( class == 7 ) then

            offset  =  offset + mdsz07


         else if ( class == 8 ) then

            offset  =  offset + mdsz08


         else if ( class == 9 ) then

            offset  =  offset + mdsz09

         else

            call setmsg ( 'Class # is not supported.' )
            call errint ( '#',  class                 )
            call sigerr ( 'SPICE(NOCLASS)'            )
            return

         end if

      end do


      return
      end
