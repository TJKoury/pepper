!$Procedure   ZZEKRD03 ( EK, read class 3 column entry elements )

      SUBROUTINE ZZEKRD03 (  HANDLE,  SEGDSC,  COLDSC, &
                             RECPTR,  CVLEN,   CVAL,    ISNULL  )

      IMPLICIT NONE

! $ Abstract
!
!     Read a column entry from a specified record in a class 3 column.
!     Class 3 columns contain scalar character values.
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
      INCLUDE 'ekcnamsz.inc'
      INCLUDE 'ekdatpag.inc'
      INCLUDE 'ekrecptr.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'

      INTEGER               HANDLE
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               COLDSC ( CDSCSZ )
      INTEGER               RECPTR
      INTEGER               CVLEN
      CHARACTER*(*)         CVAL
      LOGICAL               ISNULL

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle attached to EK file.
!     SEGDSC     I   Segment descriptor.
!     COLDSC     I   Column descriptor.
!     RECPTR     I   Record pointer.
!     CVLEN      O   Length of returned character value.
!     CVAL       O   Character value in column entry.
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
!     CVLEN          is the length of the returned string value.  This
!                    is the index of the last non-blank character of
!                    the string.  This definition applies to both fixed-
!                    and variable-length strings.
!
!                    CVLEN is set to 1 if the column entry is null.
!
!     CVAL           is the value read from the specified column entry.
!                    If CVAL has insufficient length to hold the
!                    returned string value, the output value is
!                    truncated on the right.  Entries that are shorter
!                    than the string length of CVAL are padded with
!                    trailing blanks.
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
!         error SPICE(UNINITIALIZED) is signaled.
!
!     3)  If the ordinal position of the column specified by COLDSC
!         is out of range, the error SPICE(INVALIDINDEX) is signaled.
!
!     4)  If the output string CVAL is too short to accommodate the
!         returned string value, the output value is truncated on the
!         right.  No error is signaled.
!
!     5)  If an I/O error occurs while reading the indicated file,
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
!     This routine is a utility for reading data from class 3 columns.
!
! $ Examples
!
!     See EKRCEC.
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
!-    SPICELIB Version 1.4.0, 07-FEB-2015 (NJB)
!
!        Now uses ERRHAN to insert DAS file name into
!        long error messages.
!
!        Bug fix: changed max column index in long error
!        message from NREC to NCOLS.
!
!-    SPICELIB Version 1.3.0, 31-MAY-2010 (NJB)
!
!        Bug fix: call to DASRDI was overwriting local memory. This
!        problem did not affect operation of the routine except on
!        the Mac/Intel/OSX/ifort/32-bit platform, on which it caused
!        a segmentation fault when this routine was compiled with
!        default optimization.
!
!-    SPICELIB Version 1.2.0, 23-JUL-1999 (NJB)
!
!        Error check for string truncation on output was removed.
!        This error check interfered with the use of this routine
!        (via a call to ZZEKRSC) within ZZEKJSRT, which relies on
!        being able to read into a buffer initial substrings of scalar
!        data.
!
!-    SPICELIB Version 1.1.0, 28-JUL-1997 (NJB)
!
!        Error check for string truncation on output was added.
!        SHORT error message SPICE(UNINITIALIZEDVALUE) was shortened
!        to SPICE(UNINITIALIZED).  Error messages were enhanced so
!        as to use column names rather than indices.  Miscellaneous
!        header fixes were made.
!
!-    SPICELIB Version 1.0.0, 18-OCT-1995 (NJB)
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.2.0, 23-JUL-1999 (NJB)
!
!        Error check for string truncation on output was removed.
!        This error check interfered with the use of this routine
!        (via a call to ZZEKRSC) within ZZEKJSRT, which relies on
!        being able to read into a buffer initial substrings of scalar
!        data.
!
!-    SPICELIB Version 1.1.0, 25-JUL-1997 (NJB)
!
!        Error check for string truncation on output was added.
!        SHORT error message SPICE(UNINITIALIZEDVALUE) was shortened
!        to SPICE(UNINITIALIZED), since the previous string exceeded
!        the maximum allowed length for the short error message.
!
!        Error messages were enhanced so as to use column names rather
!        than indices.
!
!-&




!
!     Non-SPICELIB functions
!
      INTEGER               ZZEKRP2N

!
!     Local variables
!
      CHARACTER*(CNAMSZ)    COLUMN

      INTEGER               AVAIL
      INTEGER               B
      INTEGER               BPOS
      INTEGER               COLIDX
      INTEGER               DATPTR
      INTEGER               E
      INTEGER               EPOS
      INTEGER               L
      INTEGER               N
      INTEGER               NCOLS
      INTEGER               P
      INTEGER               PBASE
      INTEGER               RELPTR
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

         RECNO  =  ZZEKRP2N ( HANDLE, SEGDSC(SNOIDX), RECPTR )

         CALL CHKIN  ( 'ZZEKRD03'                              )
         CALL SETMSG ( 'Column index = #; valid range is 1:#.' // &
                       'SEGNO = #; RECNO = #; EK = #'          )
         CALL ERRINT ( '#',  COLIDX                            )
         CALL ERRINT ( '#',  NCOLS                             )
         CALL ERRINT ( '#',  SEGDSC(SNOIDX)                    )
         CALL ERRINT ( '#',  RECNO                             )
         CALL ERRHAN ( '#',  HANDLE                            )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                   )
         CALL CHKOUT ( 'ZZEKRD03'                              )
         RETURN

      END IF

!
!     Compute the data pointer location, and read both the pointer
!     and the stored string size.
!
      PTRLOC  =  RECPTR + DPTBAS + COLIDX

      CALL DASRDI  ( HANDLE, PTRLOC, PTRLOC, DATPTR )


      IF ( DATPTR .GT. 0 ) THEN
!
!        Read the value.  This is slightly more complicated than
!        the numeric cases, because the value may be spread across
!        multiple pages.  Also, we must not write past the end of the
!        output string.
!
!        We'll need the number of the page at which the first character
!        of the string is stored.  This page contains at least one
!        character of the data value.
!
         CALL ZZEKGEI ( HANDLE, DATPTR, CVLEN )

!
!        Set the data pointer to the start of the string data, skipping
!        over the encoded string length.
!
         DATPTR  =  DATPTR + ENCSIZ

         N       =  MIN ( CVLEN, LEN(CVAL) )

!
!        Read the available data from the page under consideration.
!
         CALL ZZEKPGPG ( CHR, DATPTR, P, PBASE )

         RELPTR =  DATPTR - PBASE
         AVAIL  =  MIN  ( N,  CPSIZE - RELPTR + 1 )
         B      =  DATPTR
         E      =  DATPTR + AVAIL - 1
         BPOS   =  1
         EPOS   =  AVAIL
         L      =  EPOS - BPOS + 1

         CALL DASRDC ( HANDLE, B, E, BPOS, EPOS, CVAL )

         N      =  N - L


         DO WHILE ( N .GT. 0 )
!
!           Read the forward page pointer from the current page; find
!           the base address of the referenced page.
!
            CALL ZZEKGEI  ( HANDLE,  PBASE+CFPIDX,  P     )
            CALL ZZEKPGBS ( CHR,     P,             PBASE )

            AVAIL  =  MIN  ( N, CPSIZE )
            B      =  PBASE + 1
            E      =  PBASE + AVAIL
            BPOS   =  EPOS  + 1
            EPOS   =  EPOS  + AVAIL

            CALL DASRDC ( HANDLE, B, E, BPOS, EPOS, CVAL )

            N      =  N    - AVAIL
            BPOS   =  EPOS + 1

         END DO

!
!        Blank-pad CVAL if required.
!
         IF ( LEN(CVAL) .GT. EPOS ) THEN
            CVAL( EPOS+1 : )  =  ' '
         END IF


         ISNULL  =  .FALSE.


      ELSE IF ( DATPTR .EQ. NULL ) THEN
!
!        The value is null.
!
         ISNULL =  .TRUE.
         CVLEN  =   1


      ELSE IF (       ( DATPTR .EQ. UNINIT ) &
                 .OR. ( DATPTR .EQ. NOBACK )  )  THEN
!
!        The data value is absent.  This is an error.
!
         RECNO  =  ZZEKRP2N ( HANDLE, SEGDSC(SNOIDX), RECPTR )

         CALL ZZEKCNAM ( HANDLE, COLDSC, COLUMN )

         CALL CHKIN  ( 'ZZEKRD03'                                    )
         CALL SETMSG ( 'Attempted to read uninitialized column '    // &
                       'entry.  SEGNO = #; COLUMN = #; RECNO = #; ' // &
                       'EK = #'                                      )
         CALL ERRINT ( '#',  SEGDSC(SNOIDX)                          )
         CALL ERRCH  ( '#',  COLUMN                                  )
         CALL ERRINT ( '#',  RECNO                                   )
         CALL ERRHAN ( '#',  HANDLE                                  )
         CALL SIGERR ( 'SPICE(UNINITIALIZED)'                        )
         CALL CHKOUT ( 'ZZEKRD03'                                    )
         RETURN


      ELSE
!
!        The data pointer is corrupted.
!
         RECNO  =  ZZEKRP2N ( HANDLE, SEGDSC(SNOIDX), RECPTR )

         CALL ZZEKCNAM ( HANDLE,   COLDSC, COLUMN )

         CALL CHKIN  ( 'ZZEKRD03'                                )
         CALL SETMSG ( 'Data pointer is corrupted. SEGNO = #; '  // &
                       'COLUMN =  #; RECNO = #; EK = #'          )
         CALL ERRINT ( '#',  SEGDSC(SNOIDX)                      )
         CALL ERRCH  ( '#',  COLUMN                              )
         CALL ERRINT ( '#',  RECNO                               )
         CALL ERRHAN ( '#',  HANDLE                              )
         CALL SIGERR ( 'SPICE(BUG)'                              )
         CALL CHKOUT ( 'ZZEKRD03'                                )
         RETURN

      END IF


      RETURN
      END
