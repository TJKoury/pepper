!$Procedure     ZZEKAD05 ( EK, add data to class 5 column )

      SUBROUTINE ZZEKAD05 (  HANDLE,  SEGDSC,  COLDSC, &
                             RECPTR,  NVALS,   DVALS,  ISNULL  )

! $ Abstract
!
!     Add a column entry to a specified record in a class 5 column.
!     The entries of class 5 columns are arrays of d.p. values.
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
!     FILES
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
      INTEGER               SEGDSC ( * )
      INTEGER               COLDSC ( * )
      INTEGER               RECPTR
      INTEGER               NVALS
      DOUBLE PRECISION      DVALS  ( * )
      LOGICAL               ISNULL

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle attached to new EK file.
!     SEGNO      I   Index of segment containing record.
!     RECNO      I   Record to which data is to be added.
!     COLUMN     I   Column name.
!     NVALS      I   Number of values to add to column.
!     DVALS      I   Double precision values to add to column.
!     ISNULL     I   Flag indicating whether column entry is null.
!
! $ Detailed_Input
!
!     HANDLE         is a file handle of an EK open for write access.
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
!     DVALS          are, respectively, the number of values to add to
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
!                    defined by NVALS and DVALS is added to the
!                    specified kernel file.
!
!                    If ISNULL is .TRUE., NVALS and DVALS are ignored.
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
!     See EKACED.
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
      INTEGER               LASTW
      INTEGER               MBASE
      INTEGER               NCOLS
      INTEGER               NLINKS
      INTEGER               NREC
      INTEGER               NWRITE
      INTEGER               P
      INTEGER               P2
      INTEGER               PBASE
      INTEGER               PRVBAS
      INTEGER               PTRLOC
      INTEGER               RECNO
      INTEGER               REMAIN
      INTEGER               ROOM
      INTEGER               START

      LOGICAL               FSTPAG

!
!     Use discovery check-in.
!
      NREC    =  SEGDSC ( NRIDX  )
      COLIDX  =  COLDSC ( ORDIDX )

!
!     Make sure the column exists.
!
      NCOLS  =  SEGDSC ( NCIDX )

      IF (  ( COLIDX .LT. 1 ) .OR. ( COLIDX .GT. NCOLS )  ) THEN

         CALL CHKIN  ( 'ZZEKAD05'                              )
         CALL SETMSG ( 'Column index = #; valid range is 1:#.' )
         CALL ERRINT ( '#',  COLIDX                            )
         CALL ERRINT ( '#',  NREC                              )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                   )
         CALL CHKOUT ( 'ZZEKAD05'                              )
         RETURN

      END IF

!
!     If the value is null, make sure that nulls are permitted
!     in this column.
!
      IF (  ISNULL  .AND.  ( COLDSC(NFLIDX) .NE. ITRUE )  ) THEN

         RECNO  =  ZZEKRP2N ( HANDLE, SEGDSC(SNOIDX), RECPTR )

         CALL CHKIN  ( 'ZZEKAD05'                                  )
         CALL SETMSG ( 'Column having index # in segment # does '  // &
                       'not allow nulls, but a null value was '    // &
                       'supplied for the element in record #.'     )
         CALL ERRINT ( '#',  COLIDX                                )
         CALL ERRINT ( '#',  SEGDSC(SNOIDX)                        )
         CALL ERRINT ( '#',  RECNO                                 )
         CALL SIGERR ( 'SPICE(BADATTRIBUTE)'                       )
         CALL CHKOUT ( 'ZZEKAD05'                                  )
         RETURN

      END IF

!
!     Check NVALS.  If the column has fixed-size entries, NVALS must
!     match the declared entry size.  In all cases, NVALS must be
!     positive.
!
      IF ( NVALS .LT. 1 ) THEN

         CALL CHKIN  ( 'ZZEKAD05'                                  )
         CALL SETMSG ( 'COLIDX = #;  segment = #; NVALS = #;  '  // &
                       'NVALS must be positive '                   )
         CALL ERRINT ( '#',  COLIDX                                )
         CALL ERRINT ( '#',  SEGDSC(SNOIDX)                        )
         CALL ERRINT ( '#',  NVALS                                 )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                       )
         CALL CHKOUT ( 'ZZEKAD05'                                  )
         RETURN

      END IF


      IF ( COLDSC(SIZIDX) .NE. IFALSE ) THEN

         IF ( NVALS .NE. COLDSC(SIZIDX) ) THEN

            CALL CHKIN  ( 'ZZEKAD05'                                   )
            CALL SETMSG ( 'COLIDX = #;  segment = #; NVALS = #; '     // &
                          'declared entry size = #.  Sizes must match.')
            CALL ERRINT ( '#',  COLIDX                                 )
            CALL ERRINT ( '#',  SEGDSC(SNOIDX)                         )
            CALL ERRINT ( '#',  NVALS                                  )
            CALL ERRINT ( '#',  COLDSC(SIZIDX)                         )
            CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                        )
            CALL CHKOUT ( 'ZZEKAD05'                                   )
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

         LASTW   =  SEGDSC ( LDWIDX )
         ROOM    =  DPSIZE - LASTW
         REMAIN  =  NVALS
         START   =  1
         FSTPAG  =  .TRUE.


         DO WHILE ( REMAIN .GT. 0 )
!
!           Decide where to write the data values.  In order to write
!           to the current page, we require enough room for the count
!           and at least one column entry element.
!
            IF ( ROOM .GE. 2 ) THEN
!
!              There's room in the current page.  If this is the first
!              page this entry is written on, set the data pointer
!              and count.  Write as much of the value as possible to
!              the current page.
!
               P       =   SEGDSC(LDPIDX)

               CALL ZZEKPGBS ( DP,  P,  PBASE )

               PRVBAS  =  PBASE
               DATPTR  =  PBASE + LASTW + 1

               IF ( FSTPAG ) THEN

                  CALL DASUDI ( HANDLE, PTRLOC, PTRLOC, DATPTR      )
                  CALL DASUDD ( HANDLE, DATPTR, DATPTR, DBLE(NVALS) )

                  ROOM    =  ROOM   - 1
                  DATPTR  =  DATPTR + 1

               END IF


               NWRITE  =  MIN ( REMAIN, ROOM )

               CALL DASUDD ( HANDLE, &
                             DATPTR, &
                             DATPTR+NWRITE-1, &
                             DVALS ( START )  )

               REMAIN  =  REMAIN - NWRITE
               ROOM    =  ROOM   - NWRITE
               START   =  START  + NWRITE

!
!              The page containing the data item gains a link.
!
               CALL ZZEKGLNK ( HANDLE, DP, P, NLINKS   )
               CALL ZZEKSLNK ( HANDLE, DP, P, NLINKS+1 )

!
!              The last d.p. word in use must be updated.  Account
!              for the count, if this is the first page on which the
!              current entry is written.
!
               IF ( FSTPAG ) THEN
                  SEGDSC ( LDWIDX )  =  LASTW + 1 + NWRITE
                  FSTPAG             =  .FALSE.
               ELSE
                  SEGDSC ( LDWIDX )  =  LASTW + NWRITE
               END IF


            ELSE
!
!              Allocate a d.p. data page.  If this is not the first data
!              page written to, link the previous page to the current
!              one.
!
               CALL ZZEKAPS ( HANDLE, SEGDSC, DP, .FALSE., P2, PBASE )

               IF ( .NOT. FSTPAG ) THEN
                  CALL ZZEKSFWD ( HANDLE, DP, P, P2 )
               END IF

!
!              The last d.p. page and word in use must be updated.
!
               P                  =  P2
               LASTW              =  0
               SEGDSC ( LDPIDX )  =  P
               SEGDSC ( LDWIDX )  =  LASTW
               ROOM               =  DPSIZE
!
!              Make sure the link count is zeroed out.
!
               CALL ZZEKSLNK ( HANDLE, DP, P, 0 )

            END IF

         END DO


      END IF

!
!     Write out the updated segment descriptor.
!
      MBASE  =  SEGDSC(IMDIDX)

      CALL DASUDI ( HANDLE, MBASE+1, MBASE+SDSCSZ, SEGDSC )

!
!     Class 5 columns are not indexed, so we need not update any
!     index to account for the new element.
!

      RETURN
      END
