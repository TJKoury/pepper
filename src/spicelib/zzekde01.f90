!$Procedure      ZZEKDE01 ( EK, delete column entry, class 1 )

      subroutine zzekde01 ( handle, segdsc, coldsc, recptr )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Delete a specified class 1 column entry from an EK record.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     PRIVATE
!     UTILITY
!
! $ Declarations

      implicit none

      include 'ekbool.inc'
      include 'ekcoldsc.inc'
      include 'ekdatpag.inc'
      include 'ekrecptr.inc'
      include 'eksegdsc.inc'
      include 'ektype.inc'

      integer(ip)           handle
      integer(ip)           segdsc ( sdscsz )
      integer(ip)           coldsc ( cdscsz )
      integer(ip)           recptr

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     SEGDSC     I   Segment descriptor.
!     COLDSC     I   Column descriptor.
!     RECPTR     I   Record pointer.
!
! $ Detailed_Input
!
!     HANDLE         is a file handle of an EK open for write access.
!
!     SEGDSC         is the descriptor of the segment from which to
!                    delete the specified column entry.
!
!     COLDSC         is the descriptor of the column from which to
!                    delete the specified column entry.
!
!     RECPTR         is a pointer to the record containing the column
!                    entry to delete.
!
! $ Detailed_Output
!
!     None.  See the $Particulars section for a description of the
!     effect of this routine.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If HANDLE is invalid, the error will be diagnosed by routines
!         called by this routine.  The file will not be modified.
!
!     2)  If an I/O error occurs while reading or writing the indicated
!         file, the error will be diagnosed by routines called by this
!         routine.  The file may be corrupted.
!
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This routine operates by side effects:  it deletes a column entry
!     from an EK segment.  The deleted entry is marked as
!     `uninitialized'.  If the column containing the entry is indexed,
!     the corresponding entry in the index is removed.  The entry must
!     be replaced with a new entry in order to be readable.
!
!     The link count for the page containing the deleted column entry
!     is decremented.  If the count becomes zero, the page is freed.
!     If the entry to be deleted is already uninitialized upon entry
!     to this routine, no link counts are modified.  The record
!     containing the entry is still marked `updated' in this
!     case.
!
!     The changes made by this routine to the target EK file become
!     permanent when the file is closed.  Failure to close the file
!     properly will leave it in an indeterminate state.
!
! $ Examples
!
!     See EKDELR.
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
!-    SPICELIB Version 1.1.0, 05-FEB-2015 (NJB)
!
!        Now uses ERRHAN.
!
!        Deleted unneeded declarations and code.
!
!-    Beta Version 1.0.0, 28-SEP-1995 (NJB)
!
!-&


!
!     SPICELIB functions
!
      logical               failed
      logical               return

!
!     Non-SPICELIB functions
!
      integer(ip)           zzekrp2n

!
!     Local variables
!
      integer(ip)           base
      integer(ip)           datptr
      integer(ip)           idxtyp
      integer(ip)           nlinks
      integer(ip)           p
      integer(ip)           ptrloc
      integer(ip)           recno

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

      call chkin ( 'ZZEKDE01' )

!
!     Before trying to actually modify the file, do every error
!     check we can.
!
!     Is this file handle valid--is the file open for paged write
!     access?  Signal an error if not.
!
      call zzekpgch ( handle, 'WRITE' )

      if ( failed() ) then
         call chkout ( 'ZZEKDE01' )
         return
      end if

!
!     Compute the data pointer location.  If the data pointer is
!     already set to `uninitialized', there's nothing to do.  If
!     the element is null, just set it to `uninitialized'.  The
!     presence of actual data obligates us to clean up, however.
!
      ptrloc  =  recptr + dptbas + coldsc(ordidx)

      call dasrdi ( handle, ptrloc, ptrloc, datptr )


      if ( datptr .gt. 0 ) then
!
!        Determine whether the column is indexed.
!
         idxtyp  =  coldsc(ixtidx)


         if ( idxtyp .ne. ifalse ) then
!
!           This column is indexed.  Delete the index entry
!           for this column.
!
            call zzekixdl ( handle, segdsc, coldsc, recptr )

         end  if

!
!        Find the number of the page containing the column entry.
!
         call zzekpgpg ( int, datptr, p, base )

!
!        Get the link count for the page.  If we have more
!        than one link to the page, decrement the link count.  If
!        we're down to one link, this deletion will finish off the
!        page:  we'll deallocate it.
!
         call zzekglnk ( handle, int, p, nlinks )

         if ( nlinks .gt. 1 ) then

            call zzekslnk ( handle, int, p, nlinks-1 )

         else
!
!           If we removed the last item from the page, we can delete
!           the page.  ZZEKDPS adjusts the segment's metadata
!           to reflect the deallocation.
!
            call zzekdps ( handle, segdsc, int, p )

         end if

!
!        Set the data pointer to indicate the item is uninitialized.
!
         call dasudi ( handle, ptrloc, ptrloc, uninit )


      else if ( datptr .eq. null ) then
!
!        Determine whether the column is indexed.
!
         idxtyp  =  coldsc(ixtidx)

         if ( idxtyp .ne. ifalse ) then
!
!           This column is indexed.  Delete the index entry
!           for this column.
!
            call zzekixdl ( handle, segdsc, coldsc, recptr )

         end  if

!
!        Mark the entry as `uninitialized'.
!
         call dasudi ( handle, ptrloc, ptrloc, uninit )


      else if ( datptr .ne. uninit ) then
!
!        UNINIT was the last valid possibility.  The data pointer is
!        corrupted.
!
         recno  =  zzekrp2n ( handle, segdsc(snoidx), recptr )

         call setmsg ( 'Data pointer is corrupted. SEGNO = #; ' &
         //            'COLIDX =  #; RECNO = #; EK = #'        )
         call errint ( '#',  segdsc(snoidx)                    )
         call errint ( '#',  coldsc(ordidx)                    )
         call errint ( '#',  recno                             )
         call errhan ( '#',  handle                            )
         call sigerr ( 'SPICE(BUG)'                            )
         call chkout ( 'ZZEKDE01'                              )
         return

      end if

      call chkout ( 'ZZEKDE01' )
      return
      end
