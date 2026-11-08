!$Procedure     ZZEKAD01 ( EK, add data to class 1 column )

      SUBROUTINE ZZEKAD01 (  HANDLE,  SEGDSC,  COLDSC, &
                             RECPTR,  IVAL,    ISNULL  )

! $ Abstract
!
!     Add a column entry to a class 1 column in a specified EK record.
!     Class 1 columns contain scalar, integer values.
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

      INCLUDE 'ekbool.inc'
      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'ekdatpag.inc'
      INCLUDE 'ekrecptr.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'

      INTEGER               HANDLE
      INTEGER               SEGDSC ( * )
      INTEGER               COLDSC ( * )
      INTEGER               RECPTR
      INTEGER               IVAL
      LOGICAL               ISNULL

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     SEGDSC     I   Segment descriptor.
!     COLDSC     I   Column descriptor.
!     RECPTR     I   Record pointer.
!     IVAL       I   Integer value.
!     ISNULL     I   Null flag.
!
! $ Detailed_Input
!
!     HANDLE         is a file handle of an EK open for write access.
!
!     SEGDSC         is the descriptor of the segment in which
!                    the specified column entry is to be written.
!
!     COLDSC         is the descriptor of the column in which
!                    the specified column entry is to be written.
!
!     RECPTR         is a pointer to the record containing the column
!                    entry to be written.
!
!     IVAL           is the integer value that will be written
!                    to the specified column entry.
!
!     ISNULL         is a logical flag indicating whether the value
!                    of the specified column entry is to be set to NULL.
!                    If so, the input IVAL is ignored.
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
!         called by this routine.  The file is not modified.
!
!     2)  If the ordinal position of the column specified by COLDSC
!         is out of range, the error SPICE(INVALIDINDEX) is signaled.
!         The file is not modified.
!
!     3)  If the input flag ISNULL is .TRUE. but the target column
!         does not allow nulls, the error SPICE(BADATTRIBUTE) is
!         signaled.  The file is not modified.
!
!     4)  If RECPTR is invalid, a DAS addressing error may occur.  The
!         error in *not* trapped in advance.  This routine assumes that
!         a valid value of RECPTR has been supplied by the caller.
!
!     3)  If an I/O error occurs while reading or writing the indicated
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
!     This routine operates by side effects:  it sets the value of a
!     column entry in an EK segment.  If the column is indexed, the
!     index is updated to reflect the presence of the new entry.  This
!     routine is intended to set values of uninitialized column entries
!     only.  To update existing entries, use the ZZEKUExx routines, or
!     at the user level, the EKUCEx routines.
!
!     This routine does not participate in shadowing functions.  If the
!     target EK is shadowed, the caller is responsible for performing
!     necessary backup operations.  If the target EK is not shadowed,
!     the target record's status is not modified.
!
!     The changes made by this routine to the target EK file become
!     permanent when the file is closed.  Failure to close the file
!     properly will leave it in an indeterminate state.
!
! $ Examples
!
!     See EKACEI.
!
! $ Restrictions
!
!     1) This routine cannot be used to update existing column entries.
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
!-    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB)
!
!        Corrected typos in comments.
!
!-    Beta Version 1.0.0, 27-SEP-1995 (NJB)
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
      INTEGER               ITYPE
      INTEGER               LASTW
      INTEGER               MBASE
      INTEGER               NCOLS
      INTEGER               NLINKS
      INTEGER               P
      INTEGER               PBASE
      INTEGER               PTRLOC
      INTEGER               RECNO

!
!     Use discovery check-in.
!
!     Make sure the column exists.
!
      NCOLS   =  SEGDSC ( NCIDX  )
      COLIDX  =  COLDSC ( ORDIDX )

      IF (  ( COLIDX .LT. 1 ) .OR. ( COLIDX .GT. NCOLS )  ) THEN

         CALL CHKIN  ( 'ZZEKAD01'                              )
         CALL SETMSG ( 'Column index = #; valid range is 1:#.' )
         CALL ERRINT ( '#',  COLIDX                            )
         CALL ERRINT ( '#',  NCOLS                             )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                   )
         CALL CHKOUT ( 'ZZEKAD01'                              )
         RETURN

      END IF

!
!     If the value is null, make sure that nulls are permitted
!     in this column.
!
      IF (  ISNULL  .AND.  ( COLDSC(NFLIDX) .NE. ITRUE )  ) THEN

         RECNO  =  ZZEKRP2N ( HANDLE, SEGDSC(SNOIDX), RECPTR )

         CALL CHKIN  ( 'ZZEKAD01'                                  )
         CALL SETMSG ( 'Column having index # in segment # does '  // &
                       'not allow nulls, but a null value was '    // &
                       'supplied for the element in record #.'     )
         CALL ERRINT ( '#',  COLIDX                                )
         CALL ERRINT ( '#',  RECNO                                 )
         CALL SIGERR ( 'SPICE(BADATTRIBUTE)'                       )
         CALL CHKOUT ( 'ZZEKAD01'                                  )
         RETURN

      END IF

!
!     Compute the data pointer location.  Check the data pointer to
!     make sure the column entry we're writing to is uninitialized.
!
      PTRLOC  =  RECPTR + DPTBAS + COLIDX

      CALL DASRDI ( HANDLE, PTRLOC, PTRLOC, DATPTR )


      IF (  ( DATPTR .NE. UNINIT ) .AND. ( DATPTR .NE. NOBACK )  ) THEN

         RECNO  =  ZZEKRP2N ( HANDLE, SEGDSC(SNOIDX), RECPTR )

         CALL CHKIN  ( 'ZZEKAD01'                                  )
         CALL SETMSG ( 'Column having index # in segment # has '   // &
                       'non-empty element in record #.'            )
         CALL ERRINT ( '#',  COLIDX                                )
         CALL ERRINT ( '#',  SEGDSC(SNOIDX)                        )
         CALL ERRINT ( '#',  RECNO                                 )
         CALL SIGERR ( 'SPICE(NONEMPTYENTRY)'                      )
         CALL CHKOUT ( 'ZZEKAD01'                                  )
         RETURN

      END IF

!
!     Set the data value.
!
      IF ( ISNULL ) THEN
!
!        All we need do is set the data pointer.  The segment's
!        metadata are not affected.
!
         CALL DASUDI ( HANDLE, PTRLOC, PTRLOC, NULL )


      ELSE
!
!        Decide where to write the data value.  If there's room left
!        in the last integer data page, append the value there.
!
         LASTW  =  SEGDSC ( LIWIDX )


         IF ( LASTW .LT. IPSIZE ) THEN
!
!           There's room in the current page.  Set the data pointer
!           and count, and write the value out to the first free
!           location.
!
            P       =   SEGDSC(LIPIDX)

            CALL ZZEKPGBS ( INT,  P,  PBASE )

            DATPTR  =   PBASE + LASTW + 1

            CALL DASUDI ( HANDLE, PTRLOC, PTRLOC, DATPTR )
            CALL DASUDI ( HANDLE, DATPTR, DATPTR, IVAL   )

!
!           The page containing the data item gains a link.
!
            CALL ZZEKGLNK ( HANDLE, INT, P, NLINKS   )
            CALL ZZEKSLNK ( HANDLE, INT, P, NLINKS+1 )

!
!           The last integer word in use must be updated.
!
            SEGDSC ( LIWIDX )  =  LASTW + 1


         ELSE
!
!           Allocate a data page.  Write the data value into the
!           first word of the new page.
!
            CALL ZZEKAPS  ( HANDLE, SEGDSC,  INT,    .FALSE., P, PBASE )
            CALL DASUDI   ( HANDLE, PBASE+1, PBASE+1, IVAL             )

!
!           The page containing the data item now has one link.
!
            CALL ZZEKSLNK ( HANDLE, INT, P, 1 )

!
!           The last integer page and word in use must be updated.
!
            SEGDSC ( LIPIDX )  =  P
            SEGDSC ( LIWIDX )  =  1

!
!           The record pointer must point to this data item.
!
            CALL DASUDI ( HANDLE, PTRLOC, PTRLOC, PBASE+1 )

         END IF

      END IF

!
!     Write out the updated segment descriptor.
!
      MBASE  =  SEGDSC(IMDIDX)

      CALL DASUDI ( HANDLE, MBASE+1, MBASE+SDSCSZ, SEGDSC )

!
!     If the column is indexed, we must update the index to account
!     for the new element.
!
      ITYPE  =  COLDSC(IXTIDX)

      IF ( ITYPE .NE. IFALSE ) THEN
!
!        The column is indexed.
!
         IF ( ITYPE .EQ. 1 ) THEN
!
!           The column has a type 1 index.  Insert the record pointer
!           of the current element at the appropriate location.
!
            CALL ZZEKIII1 (  HANDLE,  SEGDSC,  COLDSC, &
                             IVAL,    RECPTR,  ISNULL  )

         ELSE

            CALL CHKIN  ( 'ZZEKAD01'                                  )
            CALL SETMSG ( 'Column having index # in segment # has '   // &
                          'index type #.'                             )
            CALL ERRINT ( '#',  COLIDX                                )
            CALL ERRINT ( '#',  SEGDSC(SNOIDX)                        )
            CALL ERRINT ( '#',  ITYPE                                 )
            CALL SIGERR ( 'SPICE(INVALIDTYPE)'                        )
            CALL CHKOUT ( 'ZZEKAD01'                                  )
            RETURN

         END IF

      END IF


      RETURN
      END
