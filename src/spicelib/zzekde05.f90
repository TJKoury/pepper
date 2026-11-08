!$Procedure      ZZEKDE05 ( EK, delete column entry, class 5 )

      SUBROUTINE ZZEKDE05 ( HANDLE, SEGDSC, COLDSC, RECPTR )

! $ Abstract
!
!     Delete a specified class 5 column entry from an EK record.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
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
      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Non-SPICELIB functions
!
      INTEGER               ZZEKRP2N

!
!     Local variables
!
      DOUBLE PRECISION      DPNELT

      INTEGER               BASE
      INTEGER               DATPTR
      INTEGER               NELTS
      INTEGER               NEXT
      INTEGER               NLINKS
      INTEGER               NSEEN
      INTEGER               P
      INTEGER               PTRLOC
      INTEGER               RECNO

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZEKDE05' )

!
!     Before trying to actually modify the file, do every error
!     check we can.
!
!     Is this file handle valid--is the file open for paged write
!     access?  Signal an error if not.
!
      CALL ZZEKPGCH ( HANDLE, 'WRITE' )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZEKDE05' )
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
!        Get the element count for the entry.
!
         CALL DASRDD ( HANDLE, DATPTR, DATPTR, DPNELT )

         NELTS  =  NINT ( DPNELT )

!
!        Set the data pointer to indicate the item is uninitialized.
!
         CALL DASUDI ( HANDLE, PTRLOC, PTRLOC, UNINIT )

!
!        Find the number of the page containing the column entry.
!
         CALL ZZEKPGPG ( DP, DATPTR, P, BASE )

!
!        Look up the forward pointer.  This pointer will be valid
!        if the column entry is continued on another page.
!
         CALL ZZEKGFWD ( HANDLE, DP, P, NEXT )

!
!        Get the link count for the current page.  If we have more
!        than one link to the page, decrement the link count.  If
!        we're down to one link, this deletion will finish off the
!        page:  we'll deallocate it.
!
         CALL ZZEKGLNK ( HANDLE, DP, P, NLINKS )

         IF ( NLINKS .GT. 1 ) THEN

            CALL ZZEKSLNK ( HANDLE, DP, P, NLINKS-1 )

         ELSE
!
!           If we removed the last item from the page, we can delete
!           the page.  ZZEKDPS adjusts the segment's metadata
!           to reflect the deallocation.
!
            CALL ZZEKDPS  ( HANDLE, SEGDSC, DP, P )

         END IF


         NSEEN = MIN (  NELTS,  (BASE+DPSIZE-DATPTR)  )


         DO WHILE (  ( NSEEN .LT. NELTS ) .AND. ( .NOT. FAILED() )  )
!
!           The column entry is continued on the page indicated by
!           NEXT.
!
!           Get the link count for the current page.  If we have more
!           than one link to the page, decrement the link count.  If
!           we're down to one link, this deletion will finish off the
!           page:  we'll deallocate it.
!
            P  =  NEXT

            CALL ZZEKGFWD ( HANDLE, DP, P, NEXT   )
            CALL ZZEKGLNK ( HANDLE, DP, P, NLINKS )


            IF ( NLINKS .GT. 1 ) THEN

               CALL ZZEKSLNK ( HANDLE, DP, P, NLINKS-1 )

            ELSE
!
!              If we removed the last item from the page, we can delete
!              the page.  ZZEKDPS adjusts the segment's metadata
!              to reflect the deallocation.
!
               CALL ZZEKDPS ( HANDLE, SEGDSC, DP, P )

            END IF

            NSEEN  =  MIN (  NELTS,  ( DPSIZE + NSEEN )  )

         END DO


      ELSE IF ( DATPTR .EQ. NULL ) THEN
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

         CALL SETMSG ( 'Data pointer is corrupted. SEGNO = #; '  // &
                       'COLIDX =  #; RECNO = #; EK = #'          )
         CALL ERRINT ( '#',  SEGDSC(SNOIDX)                      )
         CALL ERRINT ( '#',  COLDSC(ORDIDX)                      )
         CALL ERRINT ( '#',  RECNO                               )
         CALL ERRHAN ( '#',  HANDLE                              )
         CALL SIGERR ( 'SPICE(BUG)'                              )
         CALL CHKOUT ( 'ZZEKDE05'                                )
         RETURN

      END IF

!
!     Set the record's status to indicate that this record is updated.
!
      CALL DASUDI ( HANDLE, RECPTR+STAIDX, RECPTR+STAIDX, UPDATE )


      CALL CHKOUT ( 'ZZEKDE05' )
      RETURN
      END
