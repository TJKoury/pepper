!$Procedure      ZZEKDPS ( EK, delete page from segment )

      SUBROUTINE ZZEKDPS ( HANDLE, SEGDSC, TYPE, P )

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

      INCLUDE 'ekdatpag.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'

      INTEGER               HANDLE
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               TYPE
      INTEGER               P

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
      LOGICAL               FAILED

!
!     Non-SPICELIB functions
!
      INTEGER               ZZEKTRLS

!
!     Local variables
!
      INTEGER               MBASE
      INTEGER               LOC
      INTEGER               TREE


!
!     Use discovery check-in.
!
!
!     Ashes to ashes, dust to dust.  This page goes to the free list.
!
      CALL ZZEKPGFR ( HANDLE, TYPE, P )

      IF ( FAILED() ) THEN
         RETURN
      END IF

!
!     Update the segment's metadata.  For type 1 segments,
!     the new page into the page tree of the appropriate data type.
!
!     If this page is the last one in use in the segment, set the last
!     word in use of the appropriate type to the maximum number.  This
!     prevents further writing to the page we're deleting.
!
      MBASE  =  SEGDSC ( IMDIDX )


      IF ( TYPE .EQ. CHR ) THEN

         TREE  =  SEGDSC ( CPTIDX )

         IF ( SEGDSC(LCPIDX) .EQ. P ) THEN
            CALL DASUDI ( HANDLE, MBASE+LCWIDX, MBASE+LCWIDX, CPSIZE )
         END IF

         IF ( P .EQ. SEGDSC(LCPIDX) ) THEN
            SEGDSC(LCWIDX) = CPSIZE
         END IF


      ELSE IF ( TYPE .EQ. DP ) THEN

         TREE  =  SEGDSC ( DPTIDX )

         IF ( SEGDSC(LDPIDX) .EQ. P ) THEN
            CALL DASUDI ( HANDLE, MBASE+LDWIDX, MBASE+LDWIDX, DPSIZE )
         END IF

         IF ( P .EQ. SEGDSC(LDPIDX) ) THEN
            SEGDSC(LDWIDX) = DPSIZE
         END IF

      ELSE IF ( TYPE .EQ. INT ) THEN
!
!        The remaining possibility is that TYPE is INT.  If we had had
!        an unrecognized type, one of the allocation routines would have
!        complained.
!
         TREE  =  SEGDSC ( IPTIDX )

         IF ( SEGDSC(LIPIDX) .EQ. P ) THEN
            CALL DASUDI ( HANDLE, MBASE+LIWIDX, MBASE+LIWIDX, IPSIZE )
         END IF

         IF ( P .EQ. SEGDSC(LIPIDX) ) THEN
            SEGDSC(LIWIDX) = IPSIZE
         END IF

      END IF

!
!     Remove the page's number from the data page tree of the
!     appropriate type.  This removal requires finding the key that
!     points to the page to be removed.
!
      LOC  =  ZZEKTRLS ( HANDLE, TREE, P )

      CALL ZZEKTRDL (  HANDLE, TREE, LOC )

      RETURN
      END
