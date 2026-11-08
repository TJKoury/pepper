!$Procedure EKDELR ( EK, delete record from segment )

      subroutine ekdelr ( handle, segno, recno )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Delete a specified record from a specified E-kernel segment.
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
      include 'ekcnamsz.inc'
      include 'eksegdsc.inc'
      include 'ektype.inc'

      integer(ip)           handle
      integer(ip)           segno
      integer(ip)           recno

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     SEGNO      I   Segment number.
!     RECNO      I   Record number.
!
! $ Detailed_Input
!
!     HANDLE   is a file handle of an EK open for write access.
!
!     SEGNO    is the number of the segment from which to
!              delete the specified record.
!
!     RECNO    is the index of the record to delete. RECNO must
!              be in the range 1 : NREC, where NREC is the
!              number of records in the segment prior to the
!              insertion.
!
! $ Detailed_Output
!
!     None. See the $Particulars section for a description of the
!     effect of this routine.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If HANDLE is invalid, an error is signaled by a routine in the
!         call tree of this routine. The file will not be modified.
!
!     2)  If SEGNO is out of range, the error SPICE(INVALIDINDEX)
!         is signaled. The file will not be modified.
!
!     3)  If RECNO is out of range, the error SPICE(INVALIDINDEX)
!         is signaled. The file will not be modified.
!
!     4)  If an I/O error occurs while reading or writing the indicated
!         file, the error is signaled by a routine in the call tree of
!         this routine. The file may be corrupted.
!
! $ Files
!
!     See the EK Required Reading ek.req for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This routine operates by side effects: it deletes a record
!     from an EK segment. Deleting a record implies:
!
!        1) All column entries in the record are deleted.
!
!        2) Link counts are decremented for data pages containing
!           column entries in the record to be deleted. Pages whose
!           link counts drop to zero are freed.
!
!        3) All column indexes are updated for the parent segment.
!
!        4) The link count is decremented for the page containing the
!           record pointer structure of the record to be deleted. If
!           the link count drops to zero, the page is freed.
!
!        5) The pointer to the deleted record is deleted from the
!           record tree for the parent segment.
!
!        6) The segment's metadata is updated to reflect the new
!           record count.
!
! $ Examples
!
!     1)  Suppose the second segment of an EK file designated by
!         HANDLE contains 5 records:
!
!            +-----------------+
!            |     Record 1    |
!            +-----------------+
!            |     Record 2    |
!            +-----------------+
!            |     Record 3    |
!            +-----------------+
!            |     Record 4    |
!            +-----------------+
!            |     Record 5    |
!            +-----------------+
!
!         Then the call
!
!            CALL EKDELR ( HANDLE, 2, 3 )
!
!         deletes the third record from the segment, leaving the
!         segment's contents as follows:
!
!            +-----------------+
!            |     Record 1    |
!            +-----------------+
!            |     Record 2    |
!            +-----------------+
!            |     Record 4    |
!            +-----------------+
!            |     Record 5    |
!            +-----------------+
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 26-MAY-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 19-DEC-1995 (NJB)
!
!-&


! $ Index_Entries
!
!     delete record from an EK segment
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
      character(cnamsz)     column

      integer(ip)           base
      integer(ip)           class
      integer(ip)           coldsc ( cdscsz )
      integer(ip)           dscbas
      integer(ip)           i
      integer(ip)           mbase
      integer(ip)           mp
      integer(ip)           ncols
      integer(ip)           nlinks
      integer(ip)           nrec
      integer(ip)           p
      integer(ip)           recptr
      integer(ip)           segdsc ( sdscsz )
      integer(ip)           unit

!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     Before trying to actually modify the file, do every error
!     check we can.
!
!     Is this file handle valid--is the file open for paged write
!     access?  Signal an error if not.
!
      call zzekpgch ( handle, 'WRITE' )

      if ( failed() ) return

!
!     Look up the integer metadata page and page base for the segment.
!     Given the base address, we can read the pertinent metadata in
!     one shot.
!
      call zzekmloc ( handle,  segno,  mp,  mbase  )

      if ( failed() ) return

      call dasrdi ( handle,  mbase+1,  mbase+sdscsz,  segdsc )

!
!     In case the target EK is shadowed, let the shadow system know
!     about the deletion.  This must be done before the data is
!     deleted.  The argument COLDSC is unused on this call.
!
      call zzekrbck ( 'DELETE', handle, segdsc, coldsc, recno )

!
!     We'll need to know how many columns the segment has in order to
!     compute the size of the record pointer.  The record pointer
!     contains DPTBAS items plus two elements for each column.
!
      ncols  =  segdsc ( ncidx )

!
!     Check the number of records already present.  RECNO must not
!     exceed this count.
!
      nrec  =  segdsc ( nridx )

      if (  ( recno < 1 ) .or. ( recno > nrec )  ) then

         call setmsg ( 'Record number = #; valid range is 1:#.' )
         call errint ( '#',  recno                              )
         call errint ( '#',  nrec                               )
         call sigerr ( 'SPICE(INVALIDINDEX)'                    )
         return

      end if

!
!     Delete all of the column entries in the record.  The deletion
!     routines handle updating column indexes and freeing unlinked
!     pages.
!
      call zzektrdp ( handle, segdsc(rtidx), recno, recptr )

      do i =  1, ncols
!
!        Get the descriptor of the Ith column.
!
         dscbas  =  mbase  +  cdoff  +  (i-1)*cdscsz

         call dasrdi ( handle, dscbas+1, dscbas+cdscsz, coldsc )

         class   =  coldsc(clsidx)

!
!        Delete the entry in the current column.
!
         if ( class == 1 ) then

            call zzekde01 ( handle, segdsc, coldsc, recptr )


         else if ( class == 2 ) then

            call zzekde02 ( handle, segdsc, coldsc, recptr )


         else if ( class == 3 ) then

            call zzekde03 ( handle, segdsc, coldsc, recptr )


         else if ( class == 4 ) then

            call zzekde04 ( handle, segdsc, coldsc, recptr )


         else if ( class == 5 ) then

            call zzekde05 ( handle, segdsc, coldsc, recptr )


         else if ( class == 6 ) then

            call zzekde06 ( handle, segdsc, coldsc, recptr )


         else
!
!           This is an unsupported class.
!
            recno  =  zzekrp2n ( handle, segdsc(snoidx), recptr )

            call dashlu    ( handle, unit )
            call zzekcnam  ( handle, coldsc, column )


            call setmsg ( 'Class # from input column descriptor '  // &
                          'is not supported.  COLUMN = #; '        // &
                          'RECNO = #; SEGNO = #; EK = #.'           )
            call errint ( '#',  class                               )
            call errch  ( '#',  column                              )
            call errint ( '#',  recno                               )
            call errint ( '#',  segdsc(snoidx)                      )
            call errfnm ( '#',  unit                                )
            call sigerr ( 'SPICE(NOCLASS)'                          )
            return

         end if

      end do

!
!     Find the page containing the record pointer.
!
      call zzekpgpg ( int, recptr+1, p, base )

!
!     Get the link count for the page.  If we have more
!     than one link to the page, decrement the link count.  If
!     we're down to one link, this deletion will finish off the
!     page:  we'll deallocate it.
!
      call zzekglnk ( handle, int, p, nlinks )

      if ( nlinks > 1 ) then

         call zzekslnk ( handle, int, p, nlinks-1 )

      else
!
!        If we removed the last item from the page, we can delete
!        the page.  ZZEKDPS adjusts the segment's metadata
!        to reflect the deallocation.
!
         call zzekdps ( handle, segdsc, int, p )

      end if

!
!     The entry corresponding to the record is deleted from
!     the data record tree at index RECNO.  The record count gets
!     decremented.
!
      call zzektrdl ( handle,  segdsc(rtidx),  recno )

      segdsc ( nridx )  =  segdsc ( nridx ) - 1

!
!     Write out the updated segment descriptor.
!
      call dasudi ( handle,  mbase+1,  mbase+sdscsz,  segdsc )


      return
      end
