!$Procedure      ZZEKDE01 ( EK, delete column entry, class 1 )

      SUBROUTINE ZZEKDE01 ( HANDLE, SEGDSC, COLDSC, RECPTR )

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

      IMPLICIT NONE

      INCLUDE 'ekbool.inc'
      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'ekdatpag.inc'
      INCLUDE 'ekrecptr.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'

      INTEGER               HANDLE
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               COLDSC ( CDSCSZ )
      INTEGER               RECPTR

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
      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Non-SPICELIB functions
!
      INTEGER               ZZEKRP2N

!
!     Local variables
!
      INTEGER               BASE
      INTEGER               DATPTR
      INTEGER               IDXTYP
      INTEGER               NLINKS
      INTEGER               P
      INTEGER               PTRLOC
      INTEGER               RECNO

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZEKDE01' )

!
!     Before trying to actually modify the file, do every error
!     check we can.
!
!     Is this file handle valid--is the file open for paged write
!     access?  Signal an error if not.
!
      CALL ZZEKPGCH ( HANDLE, 'WRITE' )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZEKDE01' )
         RETURN
      END IF

!
!     Compute the data pointer location.  If the data pointer is
!     already set to `uninitialized', there's nothing to do.  If
!     the element is null, just set it to `uninitialized'.  The
!     presence of actual data obligates us to clean up, however.
!
      PTRLOC  =  RECPTR + DPTBAS + COLDSC(ORDIDX)

      CALL DASRDI ( HANDLE, PTRLOC, PTRLOC, DATPTR )


      IF ( DATPTR .GT. 0 ) THEN
!
!        Determine whether the column is indexed.
!
         IDXTYP  =  COLDSC(IXTIDX)


         IF ( IDXTYP .NE. IFALSE ) THEN
!
!           This column is indexed.  Delete the index entry
!           for this column.
!
            CALL ZZEKIXDL ( HANDLE, SEGDSC, COLDSC, RECPTR )

         END  IF

!
!        Find the number of the page containing the column entry.
!
         CALL ZZEKPGPG ( INT, DATPTR, P, BASE )

!
!        Get the link count for the page.  If we have more
!        than one link to the page, decrement the link count.  If
!        we're down to one link, this deletion will finish off the
!        page:  we'll deallocate it.
!
         CALL ZZEKGLNK ( HANDLE, INT, P, NLINKS )

         IF ( NLINKS .GT. 1 ) THEN

            CALL ZZEKSLNK ( HANDLE, INT, P, NLINKS-1 )

         ELSE
!
!           If we removed the last item from the page, we can delete
!           the page.  ZZEKDPS adjusts the segment's metadata
!           to reflect the deallocation.
!
            CALL ZZEKDPS ( HANDLE, SEGDSC, INT, P )

         END IF

!
!        Set the data pointer to indicate the item is uninitialized.
!
         CALL DASUDI ( HANDLE, PTRLOC, PTRLOC, UNINIT )


      ELSE IF ( DATPTR .EQ. NULL ) THEN
!
!        Determine whether the column is indexed.
!
         IDXTYP  =  COLDSC(IXTIDX)

         IF ( IDXTYP .NE. IFALSE ) THEN
!
!           This column is indexed.  Delete the index entry
!           for this column.
!
            CALL ZZEKIXDL ( HANDLE, SEGDSC, COLDSC, RECPTR )

         END  IF

!
!        Mark the entry as `uninitialized'.
!
         CALL DASUDI ( HANDLE, PTRLOC, PTRLOC, UNINIT )


      ELSE IF ( DATPTR .NE. UNINIT ) THEN
!
!        UNINIT was the last valid possibility.  The data pointer is
!        corrupted.
!
         RECNO  =  ZZEKRP2N ( HANDLE, SEGDSC(SNOIDX), RECPTR )

         CALL SETMSG ( 'Data pointer is corrupted. SEGNO = #; ' &
         //            'COLIDX =  #; RECNO = #; EK = #'        )
         CALL ERRINT ( '#',  SEGDSC(SNOIDX)                    )
         CALL ERRINT ( '#',  COLDSC(ORDIDX)                    )
         CALL ERRINT ( '#',  RECNO                             )
         CALL ERRHAN ( '#',  HANDLE                            )
         CALL SIGERR ( 'SPICE(BUG)'                            )
         CALL CHKOUT ( 'ZZEKDE01'                              )
         RETURN

      END IF

      CALL CHKOUT ( 'ZZEKDE01' )
      RETURN
      END
