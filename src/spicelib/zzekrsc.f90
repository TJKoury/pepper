!$Procedure   ZZEKRSC ( EK, read scalar, character )

      SUBROUTINE ZZEKRSC (  HANDLE,  SEGDSC,  COLDSC,  RECPTR, &
                            ELTIDX,  CVLEN,   CVAL,    ISNULL, FOUND  )

! $ Abstract
!
!     Read scalar data from a character column in a specified EK
!     record.
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

      INCLUDE 'ekcnamsz.inc'
      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'

      INTEGER               HANDLE
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               COLDSC ( CDSCSZ )
      INTEGER               RECPTR
      INTEGER               ELTIDX
      INTEGER               CVLEN
      CHARACTER*(*)         CVAL
      LOGICAL               ISNULL
      LOGICAL               FOUND

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle attached to EK file.
!     SEGDSC     I   Segment descriptor.
!     COLDSC     I   Column descriptor.
!     RECPTR     I   Pointer to record from which data is to be read.
!     ELTIDX     I   Index of column entry element to be read.
!     CVLEN      O   Length of stored string.
!     CVAL       O   Character value in column entry.
!     ISNULL     O   Flag indicating whether column entry is null.
!     FOUND      O   Flag indicting whether entry element was found.
!
! $ Detailed_Input
!
!     HANDLE         is an EK file handle.
!
!     SEGDSC         is the descriptor of the segment from which data is
!                    to be read.
!
!     COLDSC         is the column descriptor corresponding to the
!                    column from which data is to be read.
!
!     RECPTR         is a pointer to the record from which data is to be
!                    read.
!
!     ELTIDX         is the index of the column entry element to read.
!                    If the column entry is scalar, this argument is
!                    ignored.
!
! $ Detailed_Output
!
!     CVLEN          is the length of the stored character string value.
!                    This is the actual number of characters stored;
!                    CVLEN may be less than the declared length of
!                    the column to which the value belongs, if the
!                    column is declared to contain fixed-length strings.
!                    CVLEN is valid only when FOUND is set to .TRUE.
!
!     CVAL           is the specified column entry.  CVAL is valid only
!                    when FOUND is set to .TRUE.
!
!
!     ISNULL         is a logical flag indicating whether the entry is
!                    null.  ISNULL is valid only when FOUND is set to
!                    .TRUE.
!
!     FOUND          is a logical flag indicating whether the specified
!                    column entry element was found.  For vector-valued
!                    columns, if ELTIDX refers to a non-existent
!                    column entry element, FOUND is set to .FALSE.
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
!     2)  If COLDSC is not the name of a declared column, the error
!         will be diagnosed by routines called by this routine.
!
!     3)  If COLDSC specifies a column of whose data type is not
!         character, the error SPICE(WRONGDATATYPE) will be signaled.
!
!     4)  If COLDSC specifies a column of whose class is not
!         a character class known to this routine, the error
!         SPICE(NOCLASS) will be signaled.
!
!     5)  If the indicated column is array-valued, and if ELTIDX is
!         non-positive, the error will be diagnosed by routines called
!         by this routine.  However, if ELTIDX is greater than the
!         number of elements in the specified column entry, FOUND is
!         set to .FALSE. and no error is signaled.
!
!     6)  If an I/O error occurs while reading the indicated file,
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
!     The ZZEKRSx routines are low-level readers that expect column
!     entries to be defined by descriptors.  Since these routines do not
!     look up descriptors, in cases where many successive accesses to
!     the same segment and column are required, these routines are
!     considerably more efficient than the high-level readers.
!
!     These routines do not participate in tracing.
!
! $ Examples
!
!     See ZZEKECMP.
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
!-    SPICELIB Version 1.1.0, 06-FEB-2015 (NJB)
!
!        Now uses ERRHAN to insert DAS file name into
!        long error messages.
!
!-    Beta Version 1.0.0, 06-NOV-1995 (NJB)
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

      INTEGER               CLASS
      INTEGER               DTYPE
      INTEGER               RECNO
      INTEGER               SEGNO

!
!     Use discovery check-in.
!
!     Nothing found to begin with.
!
      FOUND  =  .FALSE.

!
!     This column had better be of character type.
!
      DTYPE  =  COLDSC(TYPIDX)


      IF ( DTYPE .NE. CHR ) THEN

         CALL ZZEKCNAM ( HANDLE, COLDSC, COLUMN )

         SEGNO  =  SEGDSC   ( SNOIDX )
         RECNO  =  ZZEKRP2N ( HANDLE, SEGDSC(SNOIDX), RECPTR )

         CALL CHKIN  ( 'ZZEKRSC'                                       )
         CALL SETMSG ( 'Column # is of type #; ZZEKRSC only works '   // &
                       'with integer columns.  RECNO = #; SEGNO = '   // &
                       '#; EK = #.'                                    )
         CALL ERRCH  ( '#',  COLUMN                                    )
         CALL ERRINT ( '#',  DTYPE                                     )
         CALL ERRINT ( '#',  RECNO                                     )
         CALL ERRINT ( '#',  SEGNO                                     )
         CALL ERRHAN ( '#',  HANDLE                                    )
         CALL SIGERR ( 'SPICE(WRONGDATATYPE)'                          )
         CALL CHKOUT ( 'ZZEKRSC'                                       )
         RETURN

      END IF

!
!     Now it's time to read data from the file.  Call the low-level
!     reader appropriate to the column's class.
!
      CLASS  =  COLDSC ( CLSIDX )


      IF ( CLASS .EQ. 3 ) THEN

         CALL ZZEKRD03 (  HANDLE,  SEGDSC,  COLDSC, &
                          RECPTR,  CVLEN,   CVAL,   ISNULL  )

         FOUND  =  .TRUE.


      ELSE IF ( CLASS .EQ. 6 ) THEN
!
!        Class 6 columns contain character string array entries.
!
         CALL ZZEKRD06 ( HANDLE,  SEGDSC,  COLDSC,  RECPTR, &
                         ELTIDX,  ELTIDX,  CVAL,    ISNULL,  FOUND )


      ELSE IF ( CLASS .EQ. 9 ) THEN

         CALL ZZEKRD09 (  HANDLE,  SEGDSC,  COLDSC, &
                          RECPTR,  CVLEN,   CVAL,   ISNULL  )

         FOUND  =  .TRUE.


      ELSE

!
!        This is an unsupported character column class.
!
         CALL ZZEKCNAM ( HANDLE, COLDSC, COLUMN )

         SEGNO  =  SEGDSC   ( SNOIDX )
         RECNO  =  ZZEKRP2N ( HANDLE, SEGDSC(SNOIDX), RECPTR )

         CALL CHKIN  ( 'ZZEKRSC'                                       )
         CALL SETMSG ( 'Class # from input column descriptor is not ' // &
                       'a supported character class.  COLUMN = #; '   // &
                       'RECNO = #; SEGNO = #; EK = #.'                 )
         CALL ERRINT ( '#',  CLASS                                     )
         CALL ERRCH  ( '#',  COLUMN                                    )
         CALL ERRINT ( '#',  RECNO                                     )
         CALL ERRINT ( '#',  SEGNO                                     )
         CALL ERRHAN ( '#',  HANDLE                                    )
         CALL SIGERR ( 'SPICE(NOCLASS)'                                )
         CALL CHKOUT ( 'ZZEKRSC'                                       )
         RETURN

      END IF


      RETURN
      END
