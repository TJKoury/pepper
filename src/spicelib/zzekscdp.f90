!$Procedure      ZZEKSCDP ( EK, set column data pointer )

      SUBROUTINE ZZEKSCDP ( HANDLE, SEGDSC, COLDSC, RECPTR, DATPTR )

! $ Abstract
!
!     Set the data pointer for a specified EK column entry.
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

      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ekrecptr.inc'

      INTEGER               HANDLE
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               COLDSC ( CDSCSZ )
      INTEGER               RECPTR
      INTEGER               DATPTR

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     SEGDSC     I   Segment descriptor.
!     COLDSC     I   Column descriptor.
!     RECPTR     I   Record pointer.
!     DATPTR     I   Data pointer of column entry.
!
! $ Detailed_Input
!
!     HANDLE         is a file handle of an EK open for write access.
!
!     SEGDSC         is the descriptor of the segment containing
!                    the specified column entry.
!
!     COLDSC         is the descriptor of the column containing
!                    the specified column entry.
!
!     RECPTR         is a pointer to the record containing the column
!                    entry whose data pointer is to be set.
!
!     DATPTR         is the data pointer of the specified column entry.
!                    When DATPTR is positive, it represents a pointer
!                    to a data value.  The interpretation of the
!                    pointer depends on the class of the column entry.
!                    DATPTR may also take on the distinguished values
!
!                       UNINIT  (indicated uninitialized entry)
!                       NULL    (indicated null entry)
!                       NOBACK  (indicated uninitialized backup entry)
!
! $ Detailed_Output
!
!     None.  See $Particulars for a description of the action of this
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
!     This routine hides details of column entry data pointer access.
!     The inverse of this routine is ZZEKGCDP.
!
! $ Examples
!
!     1)  Set a column's data pointer to indicate that a column entry
!         is uninitialized.  The parameter UNINIT is defined in
!         ekrecptr.inc
!
!           CALL ZZEKSCDP ( HANDLE, SEGDSC, COLDSC, RECPTR, UNINIT )
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
!-    SPICELIB Version 1.1.0, 09-FEB-2015 (NJB)
!
!        Now uses ERRHAN to insert DAS file name into
!        long error messages.
!
!-    Beta Version 1.0.0, 17-OCT-1995 (NJB)
!
!-&

!
!     SPICELIB functions
!
      LOGICAL               FAILED

!
!     Non-SPICELIB functions
!
      INTEGER               ZZEKRP2N

!
!     Local variables
!
      INTEGER               COLIDX
      INTEGER               NCOLS
      INTEGER               PTRLOC
      INTEGER               RECNO

!
!     Use discovery check-in.
!
!
!     Is this file handle valid--is the file open for paged write
!     access?  Signal an error if not.
!
      CALL ZZEKPGCH ( HANDLE, 'WRITE' )

      IF ( FAILED() ) THEN
         RETURN
      END IF

!
!     Make sure the column exists.
!
      NCOLS  =  SEGDSC ( NCIDX  )
      COLIDX =  COLDSC ( ORDIDX )

      IF (  ( COLIDX .LT. 1 ) .OR. ( COLIDX .GT. NCOLS )  ) THEN

         RECNO  =  ZZEKRP2N ( HANDLE, SEGDSC(SNOIDX), RECPTR )

         CALL CHKIN  ( 'ZZEKSCDP'                              )
         CALL SETMSG ( 'Column index = #; valid range is 1:#.' // &
                       'SEGNO = #; RECNO = #; EK = #'          )
         CALL ERRINT ( '#',  COLIDX                            )
         CALL ERRINT ( '#',  NCOLS                             )
         CALL ERRINT ( '#',  SEGDSC(SNOIDX)                    )
         CALL ERRINT ( '#',  RECNO                             )
         CALL ERRHAN ( '#',  HANDLE                            )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                   )
         CALL CHKOUT ( 'ZZEKSCDP'                              )
         RETURN

      END IF

!
!     Compute the data pointer location, and set the pointer.
!
      PTRLOC  =  RECPTR + DPTBAS + COLIDX

      CALL DASUDI ( HANDLE, PTRLOC, PTRLOC, DATPTR )

      RETURN
      END
