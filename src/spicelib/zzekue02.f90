!$Procedure      ZZEKUE02 ( EK, update column entry, class 2 )

      SUBROUTINE ZZEKUE02 (  HANDLE,  SEGDSC,  COLDSC, &
                             RECPTR,  DVAL,    ISNULL  )

! $ Abstract
!
!     Update a specified class 2 column entry in an EK record.
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
      DOUBLE PRECISION      DVAL
      LOGICAL               ISNULL

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     SEGDSC     I   Segment descriptor.
!     COLDSC     I   Column descriptor.
!     RECPTR     I   Record pointer.
!     DVAL       I   Double precision value.
!     ISNULL     I   Null flag.
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
!                    entry to update.
!
!     DVAL           is the double precision value with which to update
!                    the specified column entry.
!
!     ISNULL         is a logical flag indicating whether the value
!                    of the specified column entry is to be set to NULL.
!                    If so, the input DVAL is ignored.
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
!     This routine operates by side effects:  it updates a column entry
!     in an EK segment.  This routine does not participate in shadowing
!     functions.  If the target EK is shadowed, the caller is
!     responsible for performing necessary backup operations.  If the
!     target EK is not shadowed, the target record's status is not
!     modified.
!
!     If the column containing the entry is indexed, the corresponding
!     index is updated.
!
!     The changes made by this routine to the target EK file become
!     permanent when the file is closed.  Failure to close the file
!     properly will leave it in an indeterminate state.
!
! $ Examples
!
!     See EKUCED.
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
!-    SPICELIB Version 1.2.0, 09-FEB-2015 (NJB)
!
!        Now uses ERRHAN to insert DAS file name into
!        long error messages.
!
!        Deleted unnecessary code and variable declarations.
!
!-    SPICELIB Version 1.1.0, 18-JUN-1999 (WLT)
!
!        Removed redundant calls to CHKIN.
!
!-    Beta Version 1.0.0, 27-SEP-1995 (NJB)
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
      INTEGER               DATPTR
      INTEGER               IDXTYP
      INTEGER               NLINKS
      INTEGER               P
      INTEGER               PBASE
      INTEGER               PTRLOC
      INTEGER               RECNO

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZEKUE02' )

!
!     Is this file handle valid--is the file open for paged write
!     access?  Signal an error if not.
!
      CALL ZZEKPGCH ( HANDLE, 'WRITE' )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZEKUE02' )
         RETURN
      END IF

!
!     Compute the data pointer location.
!
      PTRLOC  =  RECPTR + DPTBAS + COLDSC(ORDIDX)

      CALL DASRDI ( HANDLE, PTRLOC, PTRLOC, DATPTR )


      IF ( DATPTR .GT. 0 ) THEN
!
!        The column entry is non-null.  Determine whether the column is
!        indexed.
!
         IDXTYP  =  COLDSC(IXTIDX)

         IF ( IDXTYP .EQ. 1 ) THEN
!
!           The column has a type 1 index.  Delete the index entry
!           for this column.  Create an index entry for the new value.
!
            CALL ZZEKIXDL ( HANDLE,  SEGDSC,  COLDSC,  RECPTR )

            CALL ZZEKIID1 ( HANDLE,  SEGDSC,  COLDSC,  DVAL, &
                            RECPTR,  ISNULL                  )

         ELSE IF ( IDXTYP .NE. IFALSE ) THEN

            CALL SETMSG ( 'Column having index # in segment # has '   // &
                          'index type #.'                             )
            CALL ERRINT ( '#',  COLDSC(ORDIDX)                        )
            CALL ERRINT ( '#',  SEGDSC(SNOIDX)                        )
            CALL ERRINT ( '#',  IDXTYP                                )
            CALL SIGERR ( 'SPICE(INVALIDTYPE)'                        )
            CALL CHKOUT ( 'ZZEKUE02'                                  )
            RETURN

         END IF

!
!        If the new value is null, set the data pointer to indicate a
!        null value.  Otherwise, overwrite the old value with the new
!        one.
!
         IF ( ISNULL ) THEN
!
!           The data location used by the previous value is no longer
!           needed, so we have one less link to this page.
!
            CALL ZZEKPGPG ( DP,     DATPTR, P,      PBASE    )
            CALL ZZEKGLNK ( HANDLE, DP,     P,      NLINKS   )
            CALL ZZEKSLNK ( HANDLE, DP,     P,      NLINKS-1 )

            CALL DASUDI   ( HANDLE, PTRLOC, PTRLOC, NULL     )

         ELSE
!
!           No link counts change; we just have a new value.
!
            CALL DASUDD ( HANDLE, DATPTR, DATPTR, DVAL )

         END IF


      ELSE IF ( DATPTR .EQ. NULL ) THEN
!
!        If the new entry is null too, there's nothing to do.
!        We don't have to adjust link counts or indexes.
!
!        If the new entry is non-null, we must add a new column entry,
!        since no space was reserved for the old one.  The column
!        index entry must be cleaned up, if the column is indexed.
!
         IF ( .NOT. ISNULL ) THEN

            IDXTYP  =  COLDSC(IXTIDX)

            IF ( IDXTYP .EQ. 1 ) THEN
!
!              The column has a type 1 index.  Delete the index entry
!              for this column.
!
               CALL ZZEKIXDL ( HANDLE,  SEGDSC,  COLDSC, RECPTR )


            ELSE IF ( IDXTYP .NE. IFALSE ) THEN

               CALL SETMSG ( 'Column having index # in segment # has '// &
                             'index type #.'                          )
               CALL ERRINT ( '#',  COLDSC(ORDIDX)                     )
               CALL ERRINT ( '#',  SEGDSC(SNOIDX)                     )
               CALL ERRINT ( '#',  IDXTYP                             )
               CALL SIGERR ( 'SPICE(INVALIDTYPE)'                     )
               CALL CHKOUT ( 'ZZEKUE02'                               )
               RETURN

            END IF

!
!           We don't need to decrement the link count for this page.
!           Just add the new value to the column.  But first, set the
!           data pointer to indicate an uninitialized value, so the
!           data addition routine doesn't choke.
!
            CALL DASUDI   (  HANDLE,  PTRLOC,  PTRLOC,  UNINIT )

            CALL ZZEKAD02 (  HANDLE,  SEGDSC,  COLDSC, &
                             RECPTR,  DVAL,    ISNULL  )

         END IF


      ELSE IF (      ( DATPTR .EQ. UNINIT ) &
                .OR. ( DATPTR .EQ. NOBACK )  ) THEN
!
!        There is no current column entry.  Just add a new entry.
!
         CALL ZZEKAD02 ( HANDLE, SEGDSC, COLDSC, RECPTR, DVAL, ISNULL )


      ELSE
!
!        The data pointer is corrupted.
!
         RECNO  =  ZZEKRP2N ( HANDLE, SEGDSC(SNOIDX), RECPTR )

         CALL SETMSG ( 'Data pointer is corrupted. SEGNO = #; '  // &
                       'COLIDX =  #; RECNO = #; EK = #'          )
         CALL ERRINT ( '#',  SEGDSC(SNOIDX)                      )
         CALL ERRINT ( '#',  COLDSC(ORDIDX)                      )
         CALL ERRINT ( '#',  RECNO                               )
         CALL ERRHAN ( '#',  HANDLE                              )
         CALL SIGERR ( 'SPICE(BUG)'                              )
         CALL CHKOUT ( 'ZZEKUE02'                                )
         RETURN

      END IF


      CALL CHKOUT ( 'ZZEKUE02' )
      RETURN
      END
