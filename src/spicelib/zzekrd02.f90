!$Procedure   ZZEKRD02 ( EK, read class 2 column entry )

      SUBROUTINE ZZEKRD02 (  HANDLE,  SEGDSC,  COLDSC, &
                             RECPTR,  DVAL,    ISNULL  )

! $ Abstract
!
!     Read a column entry from a specified record in a class 2 column.
!     Class 2 columns contain scalar double precision values.
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
      DOUBLE PRECISION      DVAL
      LOGICAL               ISNULL

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle attached to EK file.
!     SEGDSC     I   Segment descriptor.
!     COLDSC     I   Column descriptor.
!     RECPTR     I   Record pointer.
!     DVAL       O   Double precision value in column entry.
!     ISNULL     O   Flag indicating whether column entry is null.
!
! $ Detailed_Input
!
!     HANDLE         is an EK file handle.
!
!     SEGDSC         is the descriptor of the segment from which data is
!                    to be read.
!
!     COLDSC         is the descriptor of the column from which data is
!                    to be read.
!
!     RECPTR         is a pointer to the record containing the column
!                    entry to be written.
!
! $ Detailed_Output
!
!     DVAL           is the value read from the specified column entry.
!
!     ISNULL         is a logical flag indicating whether the entry is
!                    null.
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
!     2)  If the specified column entry has not been initialized, the
!         error SPICE(UNINITIALIZEDVALUE) is signaled.
!
!     3)  If the ordinal position of the column specified by COLDSC
!         is out of range, the error SPICE(INVALIDINDEX) is signaled.
!
!     4)  If an I/O error occurs while reading the indicated file,
!         the error will be diagnosed by routines called by this
!         routine.
!
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This routine is a utility for reading data from class 2 columns.
!
! $ Examples
!
!     See EKRCED.
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
!        long error messages.
!
!        Bug fix: changed max column index in long error
!        message from NREC to NCOLS.
!
!-    Beta Version 1.0.0, 18-OCT-1995 (NJB)
!
!-&

!
!     Non-SPICELIB functions
!
      INTEGER               ZZEKRP2N

!
!     Local variables
!
      INTEGER               COLIDX
      INTEGER               DATPTR
      INTEGER               NCOLS
      INTEGER               PTRLOC
      INTEGER               RECNO

!
!     Use discovery check-in.
!
!
!     Make sure the column exists.
!
      NCOLS   =  SEGDSC ( NCIDX  )
      COLIDX  =  COLDSC ( ORDIDX )

      IF (  ( COLIDX .LT. 1 ) .OR. ( COLIDX .GT. NCOLS )  ) THEN

         CALL CHKIN  ( 'ZZEKRD02'                              )
         CALL SETMSG ( 'Column index = #; valid range is 1:#.' )
         CALL ERRINT ( '#',  COLIDX                            )
         CALL ERRINT ( '#',  NCOLS                             )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                   )
         CALL CHKOUT ( 'ZZEKRD02'                              )
         RETURN

      END IF

!
!     Compute the data pointer location, and read the pointer.
!
      PTRLOC  =  RECPTR + DPTBAS + COLIDX

      CALL DASRDI ( HANDLE, PTRLOC, PTRLOC, DATPTR )


      IF ( DATPTR .GT. 0 ) THEN
!
!        Just read the value.
!
         CALL DASRDD ( HANDLE, DATPTR, DATPTR, DVAL )
         ISNULL  =  .FALSE.


      ELSE IF ( DATPTR .EQ. NULL ) THEN
!
!        The value is null.
!
         ISNULL =  .TRUE.


      ELSE IF (       ( DATPTR .EQ. UNINIT ) &
                 .OR. ( DATPTR .EQ. NOBACK )  )  THEN
!
!        The data value is absent.  This is an error.
!
         RECNO  =  ZZEKRP2N ( HANDLE, SEGDSC(SNOIDX), RECPTR )

         CALL CHKIN  ( 'ZZEKRD02'                                    )
         CALL SETMSG ( 'Attempted to read uninitialized column '    // &
                       'entry.  SEGNO = #; COLIDX = #; RECNO = #; ' // &
                       'EK = #'                                      )
         CALL ERRINT ( '#',  SEGDSC(SNOIDX)                          )
         CALL ERRINT ( '#',  COLIDX                                  )
         CALL ERRINT ( '#',  RECNO                                   )
         CALL ERRHAN ( '#',  HANDLE                                  )
         CALL SIGERR ( 'SPICE(UNINITIALIZEDVALUE)'                   )
         CALL CHKOUT ( 'ZZEKRD02'                                    )
         RETURN


      ELSE
!
!        The data pointer is corrupted.
!
         CALL CHKIN  ( 'ZZEKRD02'                                )
         CALL SETMSG ( 'Data pointer is corrupted. SEGNO = #; '  // &
                       'COLIDX =  #; RECNO = #; EK = #'          )
         CALL ERRINT ( '#',  SEGDSC(SNOIDX)                      )
         CALL ERRINT ( '#',  COLIDX                              )
         CALL ERRINT ( '#',  RECNO                               )
         CALL ERRHAN ( '#',  HANDLE                              )
         CALL SIGERR ( 'SPICE(BUG)'                              )
         CALL CHKOUT ( 'ZZEKRD02'                                )
         RETURN

      END IF


      RETURN
      END
