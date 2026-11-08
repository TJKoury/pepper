!$Procedure EKRCED ( EK, read column entry element, d.p. )

      SUBROUTINE EKRCED ( HANDLE,  SEGNO,  RECNO,  COLUMN, &
                          NVALS,   DVALS,  ISNULL          )

! $ Abstract
!
!     Read data from a double precision column in a specified EK
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
!     FILES
!     UTILITY
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'

      INTEGER               HANDLE
      INTEGER               SEGNO
      INTEGER               RECNO
      CHARACTER*(*)         COLUMN
      INTEGER               NVALS
      DOUBLE PRECISION      DVALS  ( * )
      LOGICAL               ISNULL

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle attached to EK file.
!     SEGNO      I   Index of segment containing record.
!     RECNO      I   Record from which data is to be read.
!     COLUMN     I   Column name.
!     NVALS      O   Number of values in column entry.
!     DVALS      O   D.p. values in column entry.
!     ISNULL     O   Flag indicating whether column entry is null.
!
! $ Detailed_Input
!
!     HANDLE   is an EK file handle. The file may be open for read or
!              write access.
!
!     SEGNO    is the index of the segment from which data is to be
!              read.
!
!     RECNO    is the index of the record from which data is to be read.
!              This record number is relative to the start of the
!              segment indicated by SEGNO; the first record in the
!              segment has index 1.
!
!     COLUMN   is the name of the column from which data is to be read.
!
! $ Detailed_Output
!
!     NVALS,
!     DVALS    are, respectively, the number of values found in the
!              specified column entry and the set of values themselves.
!
!              For columns having fixed-size entries, when column entry
!              is null, NVALS is still set to the column entry size. For
!              columns having variable- size entries, NVALS is set to 1
!              for null entries.
!
!     ISNULL   is a logical flag indicating whether the returned column
!              entry is null.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If HANDLE is invalid, an error is signaled by a routine in the
!         call tree of this routine.
!
!     2)  If SEGNO is out of range, an error is signaled by a routine in
!         the call tree of this routine.
!
!     3)  If RECNO is out of range, an error is signaled by a routine in
!         the call tree of this routine.
!
!     4)  If COLUMN is not the name of a declared column, an error
!         is signaled by a routine in the call tree of this routine.
!
!     5)  If COLUMN specifies a column of whose data type is not
!         double precision, the error SPICE(WRONGDATATYPE) is signaled.
!
!     6)  If COLUMN specifies a column of whose class is not
!         a double precision class known to this routine, the error
!         SPICE(NOCLASS) is signaled.
!
!     7)  If an attempt is made to read an uninitialized column entry,
!         an error is signaled by a routine in the call tree of this
!         routine. A null entry is considered to be initialized, but
!         entries do not contain null values by default.
!
!     8)  If an I/O error occurs while reading or writing the indicated
!         file, the error is signaled by a routine in the call tree of
!         this routine.
!
! $ Files
!
!     See the EK Required Reading ek.req for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This routine is a utility that allows an EK file to be read
!     directly without using the high-level query interface.
!
! $ Examples
!
!     1)  Read the value in the third record of the column DCOL in
!         the fifth segment of an EK file designated by HANDLE.
!
!            CALL EKRCED ( HANDLE, 5, 3, 'DCOL', N, DVAL, ISNULL )
!
! $ Restrictions
!
!     1)  EK files open for write access are not necessarily readable.
!         In particular, a column entry can be read only if it has been
!         initialized. The caller is responsible for determining
!         when it is safe to read from files open for write access.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.3.1, 26-MAY-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.3.0, 06-FEB-2015 (NJB)
!
!        Now uses ERRHAN to insert DAS file name into
!        long error messages.
!
!-    SPICELIB Version 1.2.0, 20-JUN-1999 (WLT)
!
!        Removed unbalanced call to CHKOUT.
!
!-    SPICELIB Version 1.1.0, 28-JUL-1997 (NJB)
!
!        Bug fix: Record number, not record pointer, is now supplied
!        to look up data in the class 8 case. Miscellaneous header
!        changes were made as well.
!
!-    SPICELIB Version 1.0.0, 06-NOV-1995 (NJB)
!
!-&


! $ Index_Entries
!
!     read double precision data from EK column
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.1.0, 28-JUL-1997 (NJB)
!
!        Bug fix: Record number, not record pointer, is now supplied
!        to look up data in the class 8 case. For class 8 columns,
!        column entry locations are calculated directly from record
!        numbers; no indirection is used.
!
!        Miscellaneous header changes were made as well.
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               FAILED

!
!     Non-SPICELIB functions
!
      INTEGER               ZZEKESIZ

!
!     Local variables
!
      INTEGER               COLDSC ( CDSCSZ )
      INTEGER               CLASS
      INTEGER               DTYPE
      INTEGER               RECPTR
      INTEGER               SEGDSC ( SDSCSZ )

      LOGICAL               FOUND

!
!     Use discovery check-in.
!
!     First step:  find the descriptor for the named segment.  Using
!     this descriptor, get the column descriptor.
!
      CALL ZZEKSDSC ( HANDLE, SEGNO,  SEGDSC          )
      CALL ZZEKCDSC ( HANDLE, SEGDSC, COLUMN, COLDSC  )

      IF ( FAILED() ) THEN
         RETURN
      END IF

!
!     This column had better be of d.p. or TIME type.
!
      DTYPE  =  COLDSC(TYPIDX)

      IF ( ( DTYPE .NE. DP ) .AND. ( DTYPE .NE. TIME )  ) THEN

         CALL CHKIN  ( 'EKRCED'                                        )
         CALL SETMSG ( 'Column # is of type #; EKRCED only works '    // &
                       'with d.p. or TIME columns.  RECNO = #; '      // &
                       'SEGNO =  #; EK = #.'                           )
         CALL ERRCH  ( '#',  COLUMN                                    )
         CALL ERRINT ( '#',  DTYPE                                     )
         CALL ERRINT ( '#',  RECNO                                     )
         CALL ERRINT ( '#',  SEGNO                                     )
         CALL ERRHAN ( '#',  HANDLE                                    )
         CALL SIGERR ( 'SPICE(WRONGDATATYPE)'                          )
         CALL CHKOUT ( 'EKRCED'                                        )
         RETURN

      END IF

!
!     Now it's time to read data from the file.  Call the low-level
!     reader appropriate to the column's class.
!
      CLASS  =  COLDSC ( CLSIDX )


      IF ( CLASS .EQ. 2 ) THEN
!
!        Look up the record pointer for the target record.
!
         CALL ZZEKTRDP ( HANDLE, SEGDSC(RTIDX),  RECNO,  RECPTR )

         CALL ZZEKRD02 ( HANDLE, SEGDSC, COLDSC, RECPTR, DVALS, ISNULL )
         NVALS  =  1


      ELSE IF ( CLASS .EQ. 5 ) THEN

         CALL ZZEKTRDP ( HANDLE, SEGDSC(RTIDX), RECNO, RECPTR )

         NVALS  =  ZZEKESIZ ( HANDLE, SEGDSC, COLDSC, RECPTR )

         CALL ZZEKRD05 (  HANDLE,  SEGDSC,  COLDSC,  RECPTR, &
                          1,       NVALS,   DVALS,   ISNULL,  FOUND  )


      ELSE IF ( CLASS .EQ. 8 ) THEN
!
!        Records in class 8 columns are identified by a record number
!        rather than a pointer.
!
         CALL ZZEKRD08 ( HANDLE, SEGDSC, COLDSC, RECNO, DVALS, ISNULL )
         NVALS  =  1

      ELSE

!
!        This is an unsupported d.p. column class.
!
         SEGNO  =  SEGDSC ( SNOIDX )

         CALL CHKIN  ( 'EKRCED'                                        )
         CALL SETMSG ( 'Class # from input column descriptor is not ' // &
                       'a supported d.p. class.  COLUMN = #; '        // &
                       'RECNO = #; SEGNO = #; EK = #.'                 )
         CALL ERRINT ( '#',  CLASS                                     )
         CALL ERRCH  ( '#',  COLUMN                                    )
         CALL ERRINT ( '#',  RECNO                                     )
         CALL ERRINT ( '#',  SEGNO                                     )
         CALL ERRHAN ( '#',  HANDLE                                    )
         CALL SIGERR ( 'SPICE(NOCLASS)'                                )
         CALL CHKOUT ( 'EKRCED'                                        )
         RETURN

      END IF


      RETURN
      END
