!$Procedure     ZZEKAD06 ( EK, add data to class 6 column )

      SUBROUTINE ZZEKAD06 (  HANDLE,  SEGDSC,  COLDSC, &
                             RECPTR,  NVALS,   CVALS,  ISNULL  )

! $ Abstract
!
!     Add a column entry to a specified record in a class 6 column.
!     The entries of class 6 columns are arrays of character string
!     values.
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
      INTEGER               NVALS
      CHARACTER*(*)         CVALS  ( * )
      LOGICAL               ISNULL

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   EK file handle.
!     SEGNO      I   Index of segment containing record.
!     RECNO      I   Record to which data is to be added.
!     COLUMN     I   Column name.
!     NVALS      I   Number of values to add to column.
!     CVALS      I   Character values to add to column.
!     ISNULL     I   Flag indicating whether column entry is null.
!
! $ Detailed_Input
!
!     HANDLE         is the handle of an EK file that is open for write
!                    access.
!
!     SEGNO          is the index of the segment to which data is to
!                    be added.
!
!     RECNO          is the index of the record to which data is to be
!                    added.  This record number is relative to the start
!                    of the segment indicated by SEGNO; the first
!                    record in the segment has index 1.
!
!     COLUMN         is the name of the column to which data is to be
!                    added.
!
!     NVALS,
!     CVALS          are, respectively, the number of values to add to
!                    the specified column and the set of values
!                    themselves.  The data values are written into the
!                    specified column and record.
!
!                    If the  column has fixed-size entries, then NVALS
!                    must equal the entry size for the specified column.
!
!                    Only one value can be added to a virtual column.
!
!
!     ISNULL         is a logical flag indicating whether the entry is
!                    null.  If ISNULL is .FALSE., the column entry
!                    defined by NVALS and CVALS is added to the
!                    specified kernel file.
!
!                    If ISNULL is .TRUE., NVALS and CVALS are ignored.
!                    The contents of the column entry are undefined.
!                    If the column has fixed-length, variable-size
!                    entries, the number of entries is considered to
!                    be 1.
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
!-    Beta Version 1.0.0, 23-OCT-1995 (NJB)
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
!     Local parameters
!
      INTEGER               MAXPAD
      PARAMETER           ( MAXPAD = 100 )

!
!     Local variables
!
      CHARACTER*(MAXPAD)    PADBUF

      INTEGER               COLIDX
      INTEGER               CVLEN
      INTEGER               DATPTR
      INTEGER               ELTIDX
      INTEGER               LASTW
      INTEGER               MBASE
      INTEGER               MNROOM
      INTEGER               N
      INTEGER               NCHRS
      INTEGER               NCOLS
      INTEGER               NLINKS
      INTEGER               NP
      INTEGER               NPAD
      INTEGER               NREC
      INTEGER               NWRITE
      INTEGER               P
      INTEGER               P2
      INTEGER               PADLEN
      INTEGER               PBASE
      INTEGER               POS
      INTEGER               PTRLOC
      INTEGER               RECNO
      INTEGER               REMAIN
      INTEGER               ROOM
      INTEGER               STRLEN
      INTEGER               WP

      LOGICAL               FIRST
      LOGICAL               FSTPAG
      LOGICAL               PAD

!
!     Saved variables
!
      SAVE

!
!     Initial values
!
      DATA                  FIRST  / .TRUE. /


!
!     Use discovery check-in.
!
      IF ( FIRST ) THEN
         PADBUF = ' '
         FIRST  = .FALSE.
      END IF

!
!     Make sure the record exists.
!
      NREC    =  SEGDSC ( NRIDX  )
      COLIDX  =  COLDSC ( ORDIDX )

!
!     Make sure the column exists.
!
      NCOLS  =  SEGDSC ( NCIDX )

      IF (  ( COLIDX .LT. 1 ) .OR. ( COLIDX .GT. NCOLS )  ) THEN

         CALL CHKIN  ( 'ZZEKAD06'                              )
         CALL SETMSG ( 'Column index = #; valid range is 1:#.' )
         CALL ERRINT ( '#',  COLIDX                            )
         CALL ERRINT ( '#',  NREC                              )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                   )
         CALL CHKOUT ( 'ZZEKAD06'                              )
         RETURN

      END IF

!
!     If the value is null, make sure that nulls are permitted
!     in this column.
!
      IF (  ISNULL  .AND.  ( COLDSC(NFLIDX) .NE. ITRUE )  ) THEN

         RECNO  =  ZZEKRP2N ( HANDLE, SEGDSC(SNOIDX), RECPTR )

         CALL CHKIN  ( 'ZZEKAD06'                                  )
         CALL SETMSG ( 'Column having index # in segment # does '  // &
                       'not allow nulls, but a null value was '    // &
                       'supplied for the element in record #.'     )
         CALL ERRINT ( '#',  COLIDX                                )
         CALL ERRINT ( '#',  SEGDSC(SNOIDX)                        )
         CALL ERRINT ( '#',  RECNO                                 )
         CALL SIGERR ( 'SPICE(BADATTRIBUTE)'                       )
         CALL CHKOUT ( 'ZZEKAD06'                                  )
         RETURN

      END IF

!
!     Check NVALS.  If the column has fixed-size entries, NVALS must
!     match the declared entry size.  In all cases, NVALS must be
!     positive.
!
      IF ( NVALS .LT. 1 ) THEN

         CALL CHKIN  ( 'ZZEKAD06'                                  )
         CALL SETMSG ( 'COLIDX = #;  segment = #; NVALS = #;  '  // &
                       'NVALS must be positive '                   )
         CALL ERRINT ( '#',  COLIDX                                )
         CALL ERRINT ( '#',  SEGDSC(SNOIDX)                        )
         CALL ERRINT ( '#',  NVALS                                 )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                       )
         CALL CHKOUT ( 'ZZEKAD06'                                  )
         RETURN

      END IF


      IF ( COLDSC(SIZIDX) .NE. IFALSE ) THEN

         IF ( NVALS .NE. COLDSC(SIZIDX) ) THEN

            CALL CHKIN  ( 'ZZEKAD06'                                   )
            CALL SETMSG ( 'COLIDX = #;  segment = #; NVALS = #; '     // &
                          'declared entry size = #.  Sizes must match.')
            CALL ERRINT ( '#',  COLIDX                                 )
            CALL ERRINT ( '#',  SEGDSC(SNOIDX)                         )
            CALL ERRINT ( '#',  NVALS                                  )
            CALL ERRINT ( '#',  COLDSC(SIZIDX)                         )
            CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                        )
            CALL CHKOUT ( 'ZZEKAD06'                                   )
            RETURN

         END IF

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
!        Decide now whether we will need to pad the input entry
!        elements with trailing blanks, and if so how much padding
!        we'll need.
!
         STRLEN  =  COLDSC ( LENIDX   )
         CVLEN   =  LEN    ( CVALS(1) )
         PAD     =  CVLEN  .LT.  STRLEN

         IF ( PAD ) THEN
            PADLEN  =  STRLEN - CVLEN
         END IF

         LASTW   =  SEGDSC ( LCWIDX )
         ROOM    =  CPSIZE - LASTW
         FSTPAG  =  .TRUE.

!
!        Initialize the page base and target data pointer, if possible.
!        If the current page is full, these functions will be performed
!        below in the code section in which a new page is allocated.
!
         IF ( LASTW .LT. CPSIZE ) THEN

            P       =  SEGDSC(LCPIDX)

            CALL ZZEKPGBS ( CHR,  P,  PBASE )

            DATPTR  =  PBASE + LASTW + 1

         END IF


         ELTIDX  =  1

         DO WHILE (  ( ELTIDX .LE. NVALS ) .AND. ( .NOT. FAILED() )  )
!
!           Write out the element having index ELTIDX.
!
            POS     =  0
            REMAIN  =  STRLEN


            DO WHILE ( REMAIN .GT. 0 )
!
!              Decide where to write the data values.  In order to write
!              a new entry, we require enough room for the count
!              and at least one character of data.
!
               IF ( FSTPAG ) THEN
                  MNROOM  =  1 + ENCSIZ
               ELSE
                  MNROOM  =  1
               END IF


               IF ( ROOM  .GE.  MNROOM ) THEN
!
!                 There's room in the current page.  If this is the
!                 first page this entry is written on, set the data
!                 pointer and count.  Write as much of the value as
!                 possible to the current page.
!
                  IF ( FSTPAG ) THEN

                     CALL DASUDI  ( HANDLE, PTRLOC, PTRLOC, DATPTR )
                     CALL ZZEKSEI ( HANDLE, DATPTR, NVALS          )

                     ROOM    =  ROOM   - ENCSIZ
                     DATPTR  =  DATPTR + ENCSIZ

!
!                    The first page containing some or all of the data
!                    item gains a link.
!
                     CALL ZZEKGLNK ( HANDLE, CHR, P, NLINKS   )
                     CALL ZZEKSLNK ( HANDLE, CHR, P, NLINKS+1 )

                  END IF

!
!                 Write the characters we can fit onto the current page.
!
                  NWRITE  =  MIN ( REMAIN, ROOM )
                  N       =  NWRITE

                  DO WHILE ( N .GT. 0 )

                     IF ( POS .LT. CVLEN )   THEN
!
!                       Take data from the input string CVALS(ELTIDX).
!
                        NCHRS  =  MIN ( N, CVLEN - POS )

                        CALL DASUDC ( HANDLE, &
                                      DATPTR, &
                                      DATPTR + NCHRS - 1, &
                                      POS    + 1, &
                                      POS    + NCHRS, &
                                      CVALS( ELTIDX )     )

                        N      =  N      - NCHRS
                        POS    =  POS    + NCHRS
                        DATPTR =  DATPTR + NCHRS


                     ELSE IF ( PAD ) THEN
!
!                       We must add trailing blanks to the column
!                       entry at this point.
!
                        NPAD  =  MIN ( N, PADLEN )
                        NP    =  NPAD

                        DO WHILE ( NP .GT. 0 )

                           WP  =  MIN ( NP, MAXPAD )

                           CALL DASUDC ( HANDLE, &
                                         DATPTR, &
                                         DATPTR + WP - 1, &
                                         1, &
                                         WP, &
                                         PADBUF          )

                           NP      =  NP     - WP
                           DATPTR  =  DATPTR + WP

                        END DO

                        N   =   N   - NPAD
                        POS =   POS + NPAD

                     END IF

                  END DO
!
!                 We've written all we can to the current page.
!
                  REMAIN  =  REMAIN - NWRITE
                  ROOM    =  ROOM   - NWRITE

!
!                 The last character word in use must be updated.
!                 Account for the count, if this is the first page on
!                 which the current entry is written.
!
                  IF ( FSTPAG ) THEN

                     LASTW              =  LASTW + ENCSIZ + NWRITE
                     SEGDSC ( LCWIDX )  =  LASTW
                     FSTPAG             =  .FALSE.

                  ELSE

                     LASTW              =  LASTW + NWRITE
                     SEGDSC ( LCWIDX )  =  LASTW

                  END IF


               ELSE
!
!                 Allocate a character data page.  If this is not the
!                 first data page written to, link the previous page to
!                 the current one.
!
                  CALL ZZEKAPS ( HANDLE,   SEGDSC,  CHR, &
                                 .FALSE.,  P2,      PBASE  )

                  IF ( .NOT. FSTPAG ) THEN
                     CALL ZZEKSFWD ( HANDLE, CHR, P, P2 )
                  END IF

                  P                  =  P2
                  LASTW              =  0
                  SEGDSC ( LCPIDX )  =  P
                  SEGDSC ( LCWIDX )  =  LASTW

                  ROOM               =  CPSIZE
                  DATPTR             =  PBASE  +  1

!
!                 Set the link count.  If this is the first page
!                 onto which the input column entry is written,
!                 just zero out the count; the count will be set above.
!                 Additional pages get one link.
!
                  IF ( FSTPAG ) THEN
                     NLINKS  =  0
                  ELSE
                     NLINKS  =  1
                  END IF

                  CALL ZZEKSLNK ( HANDLE, CHR, P, NLINKS )

               END IF


            END DO
!
!           We've written out the current element.
!

            ELTIDX  =  ELTIDX + 1

         END DO


      END IF

!
!     Write out the updated segment descriptor.
!
      MBASE  =  SEGDSC(IMDIDX)

      CALL DASUDI ( HANDLE, MBASE+1, MBASE+SDSCSZ, SEGDSC )

!
!     Class 6 columns are not indexed, so we need not update any
!     index to account for the new element.
!

      RETURN
      END
