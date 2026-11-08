!$Procedure      ZZEKMLOC ( EK, return integer metadata location )

      SUBROUTINE ZZEKMLOC ( HANDLE, SEGNO, PAGE, BASE )

! $ Abstract
!
!     Return the integer metadata location of a specified segment.  The
!     number and DAS integer base address of the first integer
!     page of the metadata are returned.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
!     PRIVATE
!
! $ Declarations

      INCLUDE 'ekfilpar.inc'

      INTEGER               HANDLE
      INTEGER               SEGNO
      INTEGER               PAGE
      INTEGER               BASE

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     SEGNO      I   Segment number.
!     PAGE       O   Integer metadata start page number.
!     BASE       O   Page base.
!
! $ Detailed_Input
!
!     HANDLE         is an EK file handle.  The EK may be open for read
!                    or write access.
!
!     SEGNO          is the number of the segment whose integer metadata
!                    location is sought.
!
! $ Detailed_Output
!
!    PAGE            is the number of the first page containing integer
!                    metadata for the specified segment.  The segment
!                    descriptor starts at the first address of this
!                    page.
!
!    BASE            is the DAS integer base address of the page
!                    whose number is given by PAGE.  BASE is the
!                    predecessor of the first DAS integer word
!                    belonging to this page.
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
!     2)  If SEGNO is out of range, the error SPICE(INVALIDINDEX)
!         will be signaled.
!
!     3)  If an I/O error occurs while reading or writing the indicated
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
!     This routine operates by side effects:  it deletes a record
!     from an EK segment.  Deleting a record implies:
!
!        1) All column entries in the record are deleted.
!
!        2) Link counts are decremented for data pages containing
!           column entries in the record to be deleted.  Pages whose
!           link counts drop to zero are freed.
!
!        3) All column indexes are updated for the parent segment.
!
!        4) The link count is decremented for the page containing the
!           record pointer structure of the record to be deleted.  If
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
!     See EKINSR.
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
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.0, 02-OCT-2021 (JDR)
!
!        Fixed typo in the INVALIDINDEX short error message.
!
!-    SPICELIB Version 1.1.0, 18-JUN-1999 (WLT)
!
!        Added the required discovery CHKIN.
!
!-    Beta Version 1.0.0, 19-OCT-1995 (NJB)
!
!-&


!
!     SPICELIB functions
!
      INTEGER               EKNSEG

!
!     Non-SPICELIB functions
!
      INTEGER               ZZEKTRBS

!
!     Local variables
!
      INTEGER               NSEG
      INTEGER               TBASE
      INTEGER               TREE


!
!     Use discovery check-in.
!
!
!     Validate the segment number to start out.
!
!
!     Get the segment count; validate SEGNO.
!
      NSEG  =  EKNSEG ( HANDLE )

!
!     Check out SEGNO.
!
      IF (  ( SEGNO .LT. 1 ) .OR. ( SEGNO .GT. NSEG )  )  THEN

         CALL CHKIN  ( 'ZZEKMLOC'                                )
         CALL SETMSG ( 'Segment number = #; valid range is 1:#.' )
         CALL ERRINT ( '#',  SEGNO                               )
         CALL ERRINT ( '#',  NSEG                                )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                     )
         CALL CHKOUT ( 'ZZEKMLOC'                                )
         RETURN

      END IF

!
!     Find the segment in the segment tree.
!     Obtain the base address of the first integer page.
!
      TBASE = ZZEKTRBS ( 1 )

!
!     Look up the head node of the segment tree.
!
      CALL DASRDI ( HANDLE, TBASE+SGTIDX, TBASE+SGTIDX, TREE )

!
!     Get the segment pointer for the segment having index SEGNO.
!     This pointer is actually the page number we're looking for.
!
      CALL ZZEKTRDP ( HANDLE, TREE, SEGNO, PAGE )

!
!     Return the base address of the metadata page as well.
!
      BASE  =  ZZEKTRBS ( PAGE )

      END
