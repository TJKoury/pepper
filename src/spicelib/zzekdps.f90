!$Procedure      ZZEKDPS ( EK, delete page from segment )

      subroutine zzekdps ( handle, segdsc, type, p )

! $ Abstract
!
!     Delete a specified data page for a specified EK segment.
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

      include 'ekdatpag.inc'
      include 'eksegdsc.inc'
      include 'ektype.inc'

      integer               handle
      integer               segdsc ( sdscsz )
      integer               type
      integer               p

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     SEGDSC    I-O  Segment descriptor.
!     TYPE       I   Data type of page.
!     P          I   Page number.
!
! $ Detailed_Input
!
!     HANDLE         is a file handle of an EK open for write access.
!
!     SEGDSC         is the descriptor of the segment from which to
!                    delete a data page.
!
!     TYPE           is the data type of the page.
!
!     P              is number of the page to delete.  This
!                    number is recognized by the EK paged access
!                    routines.
!
! $ Detailed_Output
!
!     SEGDSC         is the descriptor of the segment from which the
!                    specified page was deleted.  If P is the current
!                    data page of TYPE, the descriptor element
!                    specifying the last word in use of this data type
!                    will be updated on exit from this routine.
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
!     This routine operates by side effects:  it deletes an EK data
!     page from a specified segment.  The segment's metadata is updated
!     to reflect deletion of the page.  If the deleted page is the last
!     one of its type in use in the specified segment, the last word in
!     use of that type is set to the maximum value.  This prevents
!     further attempts to write to the page.
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
!-    Beta Version 2.0.0, 02-APR-1996 (NJB)
!
!        Updated to make SEGDSC an in-out argument.  The last word
!        in use of the data type of P is set to the maximum value
!        on output.  Also, an error in the deletion of the page
!        from the parent data page tree was corrected.
!
!-    Beta Version 1.0.0, 10-OCT-1995 (NJB)
!
!-&

!
!     SPICELIB functions
!
      logical               failed

!
!     Non-SPICELIB functions
!
      integer               zzektrls

!
!     Local variables
!
      integer               mbase
      integer               loc
      integer               tree


!
!     Use discovery check-in.
!
!
!     Ashes to ashes, dust to dust.  This page goes to the free list.
!
      call zzekpgfr ( handle, type, p )

      if ( failed() ) then
         return
      end if

!
!     Update the segment's metadata.  For type 1 segments,
!     the new page into the page tree of the appropriate data type.
!
!     If this page is the last one in use in the segment, set the last
!     word in use of the appropriate type to the maximum number.  This
!     prevents further writing to the page we're deleting.
!
      mbase  =  segdsc ( imdidx )


      if ( type .eq. chr ) then

         tree  =  segdsc ( cptidx )

         if ( segdsc(lcpidx) .eq. p ) then
            call dasudi ( handle, mbase+lcwidx, mbase+lcwidx, cpsize )
         end if

         if ( p .eq. segdsc(lcpidx) ) then
            segdsc(lcwidx) = cpsize
         end if


      else if ( type .eq. dp ) then

         tree  =  segdsc ( dptidx )

         if ( segdsc(ldpidx) .eq. p ) then
            call dasudi ( handle, mbase+ldwidx, mbase+ldwidx, dpsize )
         end if

         if ( p .eq. segdsc(ldpidx) ) then
            segdsc(ldwidx) = dpsize
         end if

      else if ( type .eq. int ) then
!
!        The remaining possibility is that TYPE is INT.  If we had had
!        an unrecognized type, one of the allocation routines would have
!        complained.
!
         tree  =  segdsc ( iptidx )

         if ( segdsc(lipidx) .eq. p ) then
            call dasudi ( handle, mbase+liwidx, mbase+liwidx, ipsize )
         end if

         if ( p .eq. segdsc(lipidx) ) then
            segdsc(liwidx) = ipsize
         end if

      end if

!
!     Remove the page's number from the data page tree of the
!     appropriate type.  This removal requires finding the key that
!     points to the page to be removed.
!
      loc  =  zzektrls ( handle, tree, p )

      call zzektrdl (  handle, tree, loc )

      return
      end
