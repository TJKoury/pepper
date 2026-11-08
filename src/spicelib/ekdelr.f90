!$Procedure EKDELR ( EK, delete record from segment )

      SUBROUTINE EKDELR ( HANDLE, SEGNO, RECNO )

! $ Abstract
!
!     Delete a specified record from a specified E-kernel segment.
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

      IMPLICIT NONE

      INCLUDE 'ekbool.inc'
      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'ekcnamsz.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'

      INTEGER               HANDLE
      INTEGER               SEGNO
      INTEGER               RECNO

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
      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Non-SPICELIB functions
!
      INTEGER               ZZEKRP2N

!
!     Local variables
!
      CHARACTER*(CNAMSZ)    COLUMN

      INTEGER               BASE
      INTEGER               CLASS
      INTEGER               COLDSC ( CDSCSZ )
      INTEGER               DSCBAS
      INTEGER               I
      INTEGER               MBASE
      INTEGER               MP
      INTEGER               NCOLS
      INTEGER               NLINKS
      INTEGER               NREC
      INTEGER               P
      INTEGER               RECPTR
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               UNIT

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'EKDELR' )
      END IF

!
!     Before trying to actually modify the file, do every error
!     check we can.
!
!     Is this file handle valid--is the file open for paged write
!     access?  Signal an error if not.
!
      CALL ZZEKPGCH ( HANDLE, 'WRITE' )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'EKDELR' )
         RETURN
      END IF

!
!     Look up the integer metadata page and page base for the segment.
!     Given the base address, we can read the pertinent metadata in
!     one shot.
!
      CALL ZZEKMLOC ( HANDLE,  SEGNO,  MP,  MBASE  )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'EKDELR' )
         RETURN
      END IF

      CALL DASRDI ( HANDLE,  MBASE+1,  MBASE+SDSCSZ,  SEGDSC )

!
!     In case the target EK is shadowed, let the shadow system know
!     about the deletion.  This must be done before the data is
!     deleted.  The argument COLDSC is unused on this call.
!
      CALL ZZEKRBCK ( 'DELETE', HANDLE, SEGDSC, COLDSC, RECNO )

!
!     We'll need to know how many columns the segment has in order to
!     compute the size of the record pointer.  The record pointer
!     contains DPTBAS items plus two elements for each column.
!
      NCOLS  =  SEGDSC ( NCIDX )

!
!     Check the number of records already present.  RECNO must not
!     exceed this count.
!
      NREC  =  SEGDSC ( NRIDX )

      IF (  ( RECNO .LT. 1 ) .OR. ( RECNO .GT. NREC )  ) THEN

         CALL SETMSG ( 'Record number = #; valid range is 1:#.' )
         CALL ERRINT ( '#',  RECNO                              )
         CALL ERRINT ( '#',  NREC                               )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                    )
         CALL CHKOUT ( 'EKDELR'                                 )
         RETURN

      END IF

!
!     Delete all of the column entries in the record.  The deletion
!     routines handle updating column indexes and freeing unlinked
!     pages.
!
      CALL ZZEKTRDP ( HANDLE, SEGDSC(RTIDX), RECNO, RECPTR )

      DO I =  1, NCOLS
!
!        Get the descriptor of the Ith column.
!
         DSCBAS  =  MBASE  +  CDOFF  +  (I-1)*CDSCSZ

         CALL DASRDI ( HANDLE, DSCBAS+1, DSCBAS+CDSCSZ, COLDSC )

         CLASS   =  COLDSC(CLSIDX)

!
!        Delete the entry in the current column.
!
         IF ( CLASS .EQ. 1 ) THEN

            CALL ZZEKDE01 ( HANDLE, SEGDSC, COLDSC, RECPTR )


         ELSE IF ( CLASS .EQ. 2 ) THEN

            CALL ZZEKDE02 ( HANDLE, SEGDSC, COLDSC, RECPTR )


         ELSE IF ( CLASS .EQ. 3 ) THEN

            CALL ZZEKDE03 ( HANDLE, SEGDSC, COLDSC, RECPTR )


         ELSE IF ( CLASS .EQ. 4 ) THEN

            CALL ZZEKDE04 ( HANDLE, SEGDSC, COLDSC, RECPTR )


         ELSE IF ( CLASS .EQ. 5 ) THEN

            CALL ZZEKDE05 ( HANDLE, SEGDSC, COLDSC, RECPTR )


         ELSE IF ( CLASS .EQ. 6 ) THEN

            CALL ZZEKDE06 ( HANDLE, SEGDSC, COLDSC, RECPTR )


         ELSE
!
!           This is an unsupported class.
!
            RECNO  =  ZZEKRP2N ( HANDLE, SEGDSC(SNOIDX), RECPTR )

            CALL DASHLU    ( HANDLE, UNIT )
            CALL ZZEKCNAM  ( HANDLE, COLDSC, COLUMN )


            CALL SETMSG ( 'Class # from input column descriptor '  // &
                          'is not supported.  COLUMN = #; '        // &
                          'RECNO = #; SEGNO = #; EK = #.'           )
            CALL ERRINT ( '#',  CLASS                               )
            CALL ERRCH  ( '#',  COLUMN                              )
            CALL ERRINT ( '#',  RECNO                               )
            CALL ERRINT ( '#',  SEGDSC(SNOIDX)                      )
            CALL ERRFNM ( '#',  UNIT                                )
            CALL SIGERR ( 'SPICE(NOCLASS)'                          )
            CALL CHKOUT ( 'EKDELR'                                  )
            RETURN

         END IF

      END DO

!
!     Find the page containing the record pointer.
!
      CALL ZZEKPGPG ( INT, RECPTR+1, P, BASE )

!
!     Get the link count for the page.  If we have more
!     than one link to the page, decrement the link count.  If
!     we're down to one link, this deletion will finish off the
!     page:  we'll deallocate it.
!
      CALL ZZEKGLNK ( HANDLE, INT, P, NLINKS )

      IF ( NLINKS .GT. 1 ) THEN

         CALL ZZEKSLNK ( HANDLE, INT, P, NLINKS-1 )

      ELSE
!
!        If we removed the last item from the page, we can delete
!        the page.  ZZEKDPS adjusts the segment's metadata
!        to reflect the deallocation.
!
         CALL ZZEKDPS ( HANDLE, SEGDSC, INT, P )

      END IF

!
!     The entry corresponding to the record is deleted from
!     the data record tree at index RECNO.  The record count gets
!     decremented.
!
      CALL ZZEKTRDL ( HANDLE,  SEGDSC(RTIDX),  RECNO )

      SEGDSC ( NRIDX )  =  SEGDSC ( NRIDX ) - 1

!
!     Write out the updated segment descriptor.
!
      CALL DASUDI ( HANDLE,  MBASE+1,  MBASE+SDSCSZ,  SEGDSC )


      CALL CHKOUT ( 'EKDELR' )
      RETURN
      END
