!$Procedure     ZZEKAD03 ( EK, add data to class 3 column )

      SUBROUTINE ZZEKAD03 (  HANDLE,  SEGDSC,  COLDSC, &
                             RECPTR,  CVAL,    ISNULL  )

! $ Abstract
!
!     Add a column entry to a class 3 column in a specified EK record.
!     Class 3 columns contain scalar, character values.
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
      CHARACTER*(*)         CVAL
      LOGICAL               ISNULL

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     SEGDSC     I   Segment descriptor.
!     COLDSC     I   Column descriptor.
!     RECPTR     I   Record pointer.
!     CVAL       I   Character string value.
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
!     CVAL           is the character string value that will be written
!                    to the specified column entry.
!
!     ISNULL         is a logical flag indicating whether the value
!                    of the specified column entry is to be set to NULL.
!                    If so, the input CVAL is ignored.
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
!     See EKACEC.
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
!     SPICELIB functions
!
      INTEGER               RTRIM

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
      INTEGER               LNB
      INTEGER               MBASE
      INTEGER               N
      INTEGER               NCOLS
      INTEGER               NLINKS
      INTEGER               NWRITE
      INTEGER               P
      INTEGER               P2
      INTEGER               PBASE
      INTEGER               PCOUNT
      INTEGER               POS
      INTEGER               PRVBAS
      INTEGER               PTRLOC
      INTEGER               RECNO
      INTEGER               STRLEN

      LOGICAL               FIXLEN

!
!     Use discovery check-in.
!
!     Make sure the column exists.
!
      NCOLS   =  SEGDSC ( NCIDX  )
      COLIDX  =  COLDSC ( ORDIDX )

      IF (  ( COLIDX .LT. 1 ) .OR. ( COLIDX .GT. NCOLS )  ) THEN

         CALL CHKIN  ( 'ZZEKAD03'                              )
         CALL SETMSG ( 'Column index = #; valid range is 1:#.' )
         CALL ERRINT ( '#',  COLIDX                            )
         CALL ERRINT ( '#',  NCOLS                             )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                   )
         CALL CHKOUT ( 'ZZEKAD03'                              )
         RETURN

      END IF

!
!     If the value is null, make sure that nulls are permitted
!     in this column.
!
      IF (  ISNULL  .AND.  ( COLDSC(NFLIDX) .NE. ITRUE )  ) THEN

         RECNO  =  ZZEKRP2N ( HANDLE, SEGDSC(SNOIDX), RECPTR )

         CALL CHKIN  ( 'ZZEKAD03'                                  )
         CALL SETMSG ( 'Column having index # in segment # does '  // &
                       'not allow nulls, but a null value was '    // &
                       'supplied for the element in record #.'     )
         CALL ERRINT ( '#',  COLIDX                                )
         CALL ERRINT ( '#',  SEGDSC(SNOIDX)                        )
         CALL ERRINT ( '#',  RECNO                                 )
         CALL SIGERR ( 'SPICE(BADATTRIBUTE)'                       )
         CALL CHKOUT ( 'ZZEKAD03'                                  )
         RETURN

      END IF

!
!     Decide the length of the string value.  If the column contains
!     variable-length strings, the effective length of the string is
!     just the non-blank length of CVAL.  Otherwise, the effective
!     string length is the minimum of the non-blank length and the
!     column's declared string length.  We don't store trailing blanks.
!
      FIXLEN  =  COLDSC(LENIDX) .NE. IFALSE
      LNB     =  RTRIM ( CVAL )

      IF ( FIXLEN ) THEN
         STRLEN  =  MIN (  COLDSC(LENIDX),  LNB )
      ELSE
         STRLEN  =  LNB
      END IF

!
!     Compute the data pointer location.
!
      PTRLOC  =  RECPTR + DPTBAS + COLIDX


      IF ( ISNULL ) THEN
!
!        All we need do is set the data pointer.  The segment's
!        metadata are not affected.
!
         CALL DASUDI ( HANDLE, PTRLOC, PTRLOC, NULL )


      ELSE
!
!        Write out the data value.  If we run out of room in the
!        page we're writing to, we allocate a new page and link
!        the previous page to it.
!
         N       =  STRLEN
         POS     =  1
         LASTW   =  SEGDSC ( LCWIDX )
         P       =  SEGDSC ( LCPIDX )
         PCOUNT  =  0


         DO WHILE ( N .GT. 0 )
!
!           Write as much data as possible into the current page.
!

            IF ( LASTW .LT. CPSIZE-ENCSIZ ) THEN
!
!              There's room in the current page.  We never split an
!              encoded character count across pages, and we always
!              write at least one data character to the current page.
!              This practice is slightly wasteful of space but greatly
!              simplifies our logic.
!
!              Keep track of the number of pages our string spans.
!
               PCOUNT  =  PCOUNT + 1

!
!              If this is the first data page, write the data pointer
!              into the record pointer and the character count into
!              the data page.
!
               IF ( PCOUNT .EQ. 1 ) THEN

                  CALL ZZEKPGBS ( CHR, P, PBASE )

                  DATPTR  =  PBASE + LASTW + 1

                  CALL DASUDI  ( HANDLE, PTRLOC, PTRLOC, DATPTR )
                  CALL ZZEKSEI ( HANDLE, DATPTR, STRLEN )
!
!                 Advance the data pointer to the first data
!                 character's position.  The last word in use
!                 increases as well.
!
                  DATPTR  =  DATPTR + ENCSIZ
                  LASTW   =  LASTW  + ENCSIZ

               ELSE
!
!                 We still need the data pointer.
!
                  DATPTR  =  PBASE + 1

               END IF

!
!              Compute the number of characters to write into this page,
!              and write that number of characters.
!
               NWRITE  =  MIN ( CPSIZE-LASTW, N )

               CALL DASUDC (  HANDLE,   DATPTR,   DATPTR+NWRITE-1, &
                              1,        NWRITE,   CVAL(POS:)        )

               N       =  N   -  NWRITE
               POS     =  POS +  NWRITE

!
!              The page containing the data item gains a link.
!
               CALL ZZEKGLNK ( HANDLE, CHR, P, NLINKS   )
               CALL ZZEKSLNK ( HANDLE, CHR, P, NLINKS+1 )

!
!              The last character word in use must be updated.
!
               LASTW              =  LASTW + NWRITE
               SEGDSC ( LCWIDX )  =  LASTW

!
!              Retain the base address of this data page.
!
               PRVBAS  =  PBASE


            ELSE
!
!              Allocate a data page.  If this is not the first data
!              page written to, link the previous page to the current
!              one.
!
               CALL ZZEKAPS ( HANDLE, SEGDSC, CHR, .FALSE., P2, PBASE )

               IF ( PCOUNT .GT. 0 ) THEN
                  CALL ZZEKSFWD ( HANDLE, CHR, P, P2 )
               END IF

!
!              The last character page and word in use must be updated.
!
               P                  =  P2
               LASTW              =  0
               SEGDSC ( LCPIDX )  =  P
               SEGDSC ( LCWIDX )  =  LASTW

!
!              Make sure the link count is zeroed out.
!
               CALL ZZEKSLNK ( HANDLE, CHR, P, 0 )

            END IF

         END DO

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
!           The column has a type 1 index.  Insert the record number
!           of the current element at the appropriate location.
!
            CALL ZZEKIIC1 (  HANDLE,  SEGDSC,  COLDSC, &
                             CVAL,    RECPTR,  ISNULL   )

         ELSE

            CALL CHKIN  ( 'ZZEKAD03'                                  )
            CALL SETMSG ( 'Column having index # in segment # has '   // &
                          'index type #.'                             )
            CALL ERRINT ( '#',  COLIDX                                )
            CALL ERRINT ( '#',  SEGDSC(SNOIDX)                        )
            CALL ERRINT ( '#',  ITYPE                                 )
            CALL SIGERR ( 'SPICE(INVALIDTYPE)'                        )
            CALL CHKOUT ( 'ZZEKAD03'                                  )
            RETURN

         END IF

      END IF

      RETURN
      END
