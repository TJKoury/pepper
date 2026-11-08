!$Procedure   ZZEKRD09 ( EK, read class 9 column entry elements )

      SUBROUTINE ZZEKRD09 (  HANDLE,  SEGDSC,  COLDSC, &
                             RECNO,   CVLEN,   CVAL,    ISNULL  )

      IMPLICIT NONE

! $ Abstract
!
!     Read a column entry from a specified record in a class 9 column.
!     Class 9 columns contain fixed record count, fixed-length,
!     scalar character values.
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
      INCLUDE 'ekclas09.inc'
      INCLUDE 'ekcnamsz.inc'
      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'ekdatpag.inc'
      INCLUDE 'ekpage.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'

      INTEGER               HANDLE
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               COLDSC ( CDSCSZ )
      INTEGER               RECNO
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
!     RECNO      I   Record number.
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
!     RECNO          is the number of the record containing the column
!                    entry to be written.
!
! $ Detailed_Output
!
!     CVLEN          is the length of the returned string value.  This
!                    is the declared string length of the column being
!                    read.  Note this definition differs from that used
!                    for class 3 columns.  In the class 9 case, no
!                    string length is stored in the file, so extra work
!                    at run time would be required to determine whether
!                    truncation would occur.
!
!     CVAL           is the value read from the specified column entry.
!                    CVAL must have sufficient length to hold the
!                    returned string value.  Entries that are shorter
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
!     2)  If the ordinal position of the column specified by COLDSC
!         is out of range, the error SPICE(INVALIDINDEX) is signaled.
!
!     3)  If the output string CVAL is too short to accommodate the
!         returned string value, the error SPICE(STRINGTRUNCATED)
!         is signaled.  CVAL must be at least as long as the declared
!         length of the column being read.
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
!     This routine is a utility for reading data from class 9 columns.
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
!-    SPICELIB Version 1.2.0, 07-FEB-2015 (NJB)
!
!        Now uses ERRHAN to insert DAS file name into
!        long error messages.
!
!        Bug fix: changed max column index in long error
!        message from NREC to NCOLS.
!
!-    SPICELIB Version 1.1.0, 28-JUL-1997 (NJB)
!
!        Error check for string truncation on output was added.
!        SHORT error message SPICE(UNINITIALIZEDVALUE) was shortened
!        to SPICE(UNINITIALIZED).
!
!        The argument RECPTR was renamed to RECNO.  The reference to
!        ZZEKRP2N was removed.
!
!        Miscellaneous header corrections were made.
!
!-    SPICELIB Version 1.0.0, 09-NOV-1995 (NJB)
!
!-&

! $ Revisions
!
!-    SPICELIB Version 1.1.0, 28-JUL-1997 (NJB)
!
!        Error check for string truncation on output was added.
!        SHORT error message SPICE(UNINITIALIZEDVALUE) was shortened
!        to SPICE(UNINITIALIZED).
!
!        The argument RECPTR was renamed to RECNO.  The reference to
!        ZZEKRP2N was removed.
!
!
!-&


!
!     Local variables
!
      CHARACTER*(1)         CFLAG
      CHARACTER*(CNAMSZ)    COLUMN

      INTEGER               ADDRSS
      INTEGER               COLIDX
      INTEGER               DATBAS
      INTEGER               L
      INTEGER               MDAT   ( MDSZ09 )
      INTEGER               METLOC
      INTEGER               NCOLS
      INTEGER               NFLBAS
      INTEGER               OFFSET
      INTEGER               Q
      INTEGER               R
      INTEGER               SPP

      LOGICAL               NULLOK

!
!     Use discovery check-in.
!
!     Make sure the column exists.
!
      NCOLS   =  SEGDSC ( NCIDX  )
      COLIDX  =  COLDSC ( ORDIDX )
      METLOC  =  COLDSC ( METIDX )
      NULLOK  =  COLDSC ( NFLIDX ) .EQ. ITRUE
      L       =  COLDSC ( LENIDX )

      IF (  ( COLIDX .LT. 1 ) .OR. ( COLIDX .GT. NCOLS )  ) THEN

         CALL CHKIN  ( 'ZZEKRD09'                              )
         CALL SETMSG ( 'Column index = #; valid range is 1:#.' // &
                       'SEGNO = #; RECNO = #; EK = #'          )
         CALL ERRINT ( '#',  COLIDX                            )
         CALL ERRINT ( '#',  NCOLS                             )
         CALL ERRINT ( '#',  SEGDSC(SNOIDX)                    )
         CALL ERRINT ( '#',  RECNO                             )
         CALL ERRHAN ( '#',  HANDLE                            )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                   )
         CALL CHKOUT ( 'ZZEKRD09'                              )
         RETURN

      END IF

!
!     Since class 9 columns have fixed-length strings, we already
!     know the string length.
!
      CVLEN  =  L

      IF ( CVLEN .GT. LEN(CVAL) ) THEN
!
!        We have a string truncation error.  Look up the column
!        name, record number, and file name before signaling an
!        error.
!
         CALL ZZEKCNAM ( HANDLE, COLDSC, COLUMN )

         CALL CHKIN  ( 'ZZEKRD09'                                  )
         CALL SETMSG ( 'String value has length #; output string '// &
                       'can hold only # characters.  COLUMN = #; '// &
                       'SEGNO = #; RECNO = #; EK = #'              )
         CALL ERRINT ( '#',  CVLEN                                 )
         CALL ERRINT ( '#',  LEN(CVAL)                             )
         CALL ERRCH  ( '#',  COLUMN                                )
         CALL ERRINT ( '#',  SEGDSC(SNOIDX)                        )
         CALL ERRINT ( '#',  RECNO                                 )
         CALL ERRHAN ( '#',  HANDLE                                )
         CALL SIGERR ( 'SPICE(STRINGTRUNCATED)'                    )
         CALL CHKOUT ( 'ZZEKRD09'                                  )
         RETURN

      END IF

!
!     Read the metadata block.  There are two items in the block:
!
!        1) The base address of the first page of the data
!        2) The base address of the null flag array, if nulls are
!           permitted.
!
      CALL DASRDI ( HANDLE, METLOC+1, METLOC+MDSZ09, MDAT )

      DATBAS = MDAT ( DBIX09 )
      NFLBAS = MDAT ( NFIX09 )

!
!     If null values are permitted, the first step is to get
!     the null flag for the value of interest.  Compute the
!     address of this flag.
!
!     There are CPSIZE null flags per page, and each page has size
!     PGSIZC.  The null flags start at the beginning of the page.
!
      IF ( NULLOK ) THEN

         Q       =  (RECNO-1)  / CPSIZE
         R       =  RECNO  - Q * CPSIZE
         OFFSET  =  R      + Q * PGSIZC
         ADDRSS  =  NFLBAS + OFFSET

         CALL DASRDC ( HANDLE, ADDRSS, ADDRSS, 1, 1, CFLAG )

         ISNULL  =  CFLAG .EQ. CTRUE

         IF ( ISNULL ) THEN
            RETURN
         END IF

      END IF

!
!     If we're still here, we'll read the data value.
!
      ISNULL  =  .FALSE.

!
!     The address calculation for the value is similar to that
!     for the null flag.  However, the string length must be
!     taken into account.
!
      SPP     =  CPSIZE     /   L
      Q       =  (RECNO-1)  /   SPP
      R       =  RECNO      -   Q*SPP
      ADDRSS  =  DATBAS     +   Q*PGSIZC  +  (R-1)*L  +  1

      CALL DASRDC ( HANDLE, ADDRSS, ADDRSS+L-1, 1, L, CVAL )

!
!     Blank-pad CVAL if required.
!
      IF ( LEN(CVAL) .GT. L ) THEN
         CVAL( L+1 : )  =  ' '
      END IF


      RETURN
      END
