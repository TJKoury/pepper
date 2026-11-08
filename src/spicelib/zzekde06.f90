!$Procedure      ZZEKDE06 ( EK, delete column entry, class 6 )

      subroutine zzekde06 ( handle, segdsc, coldsc, recptr )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Delete a specified class 6 column entry from an EK record.
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
!     from an EK segment.  The status of the record containing the entry
!     is set to `updated'.  The deleted entry is marked as
!     `uninitialized'.
!
!     The link counts for the pages containing the deleted column entry
!     are decremented.  If the count for a page becomes zero, that page
!     is freed.  If the entry to be deleted is already uninitialized
!     upon entry to this routine, no link counts are modified.  The
!     record containing the entry is still marked `updated' in this
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
!-    SPICELIB Version 1.1.0, 07-FEB-2015 (NJB)
!
!        Now uses ERRHAN to insert DAS file name into
!        long error message.
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
      integer(ip)           nchars
      integer(ip)           nelts
      integer(ip)           next
      integer(ip)           nlinks
      integer(ip)           nseen
      integer(ip)           p
      integer(ip)           ptrloc
      integer(ip)           recno

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

      call chkin ( 'ZZEKDE06' )

!
!     Before trying to actually modify the file, do every error
!     check we can.
!
!     Is this file handle valid--is the file open for paged write
!     access?  Signal an error if not.
!
      call zzekpgch ( handle, 'WRITE' )

      if ( failed() ) then
         call chkout ( 'ZZEKDE06' )
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


      if ( datptr > 0 ) then
!
!        Get the element count for the entry.  Compute the character
!        count for the entry.
!
         call zzekgei ( handle, datptr, nelts )

         nchars  =  coldsc(lenidx) * nelts

!
!        Set the data pointer to indicate the item is uninitialized.
!
         call dasudi ( handle, ptrloc, ptrloc, uninit )

!
!        Find the number of the page containing the column entry.
!
         call zzekpgpg ( chr, datptr, p, base )

!
!        Look up the forward pointer.  This pointer will be valid
!        if the column entry is continued on another page.
!
         call zzekgfwd ( handle, chr, p, next )

!
!        Get the link count for the current page.  If we have more
!        than one link to the page, decrement the link count.  If
!        we're down to one link, this deletion will finish off the
!        page:  we'll deallocate it.
!
         call zzekglnk ( handle, chr, p, nlinks )

         if ( nlinks > 1 ) then

            call zzekslnk ( handle, chr, p, nlinks-1 )

         else
!
!           If we removed the last item from the page, we can delete
!           the page.  ZZEKDPS adjusts the segment's metadata
!           to reflect the deallocation.
!
            call zzekdps  ( handle, segdsc, chr, p )

         end if


         nseen = min (  nchars,  (base+cpsize-datptr)  )


         do while (  ( nseen < nchars ) .and. ( .not. failed() )  )
!
!           The column entry is continued on the page indicated by
!           NEXT.
!
!           Get the link count for the current page.  If we have more
!           than one link to the page, decrement the link count.  If
!           we're down to one link, this deletion will finish off the
!           page:  we'll deallocate it.
!
            p  =  next

            call zzekgfwd ( handle, chr, p, next   )
            call zzekglnk ( handle, chr, p, nlinks )


            if ( nlinks > 1 ) then

               call zzekslnk ( handle, chr, p, nlinks-1 )

            else
!
!              If we removed the last item from the page, we can delete
!              the page.  ZZEKDPS adjusts the segment's metadata
!              to reflect the deallocation.
!
               call zzekdps  ( handle, segdsc, chr, p )

            end if

            nseen  =  min (  nchars,  ( cpsize + nseen )  )

         end do


      else if ( datptr == null ) then
!
!        Mark the entry as `uninitialized'.
!
         call dasudi ( handle, ptrloc, ptrloc, uninit )


      else if ( datptr /= uninit ) then
!
!        UNINIT was the last valid possibility.  The data pointer is
!        corrupted.
!
         recno  =  zzekrp2n ( handle, segdsc(snoidx), recptr )

         call setmsg ( 'Data pointer is corrupted. SEGNO = #; '  // &
                       'COLIDX =  #; RECNO = #; EK = #'          )
         call errint ( '#',  segdsc(snoidx)                      )
         call errint ( '#',  coldsc(ordidx)                      )
         call errint ( '#',  recno                               )
         call errhan ( '#',  handle                              )
         call sigerr ( 'SPICE(BUG)'                              )
         call chkout ( 'ZZEKDE06'                                )
         return

      end if

!
!     Set the record's status to indicate that this record is updated.
!
      call dasudi ( handle, recptr+staidx, recptr+staidx, update )


      call chkout ( 'ZZEKDE06' )
      return
      end
