!$Procedure EKRCEC ( EK, read column entry element, character )

      subroutine ekrcec ( handle,  segno,  recno,  column, &
                          nvals,   cvals,  isnull          )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Read data from a character column in a specified EK record.
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

      implicit none

      include 'ekcoldsc.inc'
      include 'eksegdsc.inc'
      include 'ektype.inc'

      integer(ip)           handle
      integer(ip)           segno
      integer(ip)           recno
      character(*)          column
      integer(ip)           nvals
      character(*)          cvals  ( * )
      logical               isnull

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle attached to EK file.
!     SEGNO      I   Index of segment containing record.
!     RECNO      I   Record from which data is to be read.
!     COLUMN     I   Column name.
!     NVALS      O   Number of values in column entry.
!     CVALS      O   Character values in column entry.
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
!     CVALS    are, respectively, the number of values found in the
!              specified column entry and the set of values themselves.
!              The array CVALS must have sufficient string length to
!              accommodate the longest string in the returned column
!              entry.
!
!              For columns having fixed-size entries, when a column
!              entry is null, NVALS is still set to the column entry
!              size. For columns having variable- size entries, NVALS is
!              set to 1 for null entries.
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
!         character, the error SPICE(WRONGDATATYPE) is signaled.
!
!     6)  If COLUMN specifies a column of whose class is not
!         a character class known to this routine, the error
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
!     9)  If any element of the column entry would be truncated when
!         assigned to an element of CVALS, an error is signaled by a
!         routine in the call tree of this routine.
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
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) The following program demonstrates how to create a new EK and
!        add data to a character column in a given record within the
!        file, and how to read the data from it.
!
!        Example code begins here.
!
!              PROGRAM EKRCEC_EX1
!              IMPLICIT NONE
!
!        C
!        C     Include the EK Column Name Size (CNAMSZ)
!        C
!              INCLUDE 'ekcnamsz.inc'
!
!        C
!        C     Local constants.
!        C
!              CHARACTER*(*)         EKNAME
!              PARAMETER           ( EKNAME = 'ekrcec_ex1.bdb' )
!
!              CHARACTER*(*)         IFNAME
!              PARAMETER           ( IFNAME = 'Test EK'  )
!
!              CHARACTER*(*)         TABLE
!              PARAMETER           ( TABLE  = 'CHR_DATA' )
!
!              INTEGER               CVLEN
!              PARAMETER           ( CVLEN  = 9  )
!
!              INTEGER               DECLEN
!              PARAMETER           ( DECLEN = 200 )
!
!              INTEGER               MAXVAL
!              PARAMETER           ( MAXVAL = 4  )
!
!              INTEGER               NCOLS
!              PARAMETER           ( NCOLS  = 2  )
!
!              INTEGER               NROWS
!              PARAMETER           ( NROWS  = 6  )
!
!              INTEGER               NRESVC
!              PARAMETER           ( NRESVC = 0  )
!
!        C
!        C     Local variables
!        C
!              CHARACTER*(DECLEN)    CDECLS ( NCOLS  )
!              CHARACTER*(CNAMSZ)    CNAMES ( NCOLS  )
!              CHARACTER*(CVLEN)     CVALS  ( MAXVAL )
!
!              INTEGER               HANDLE
!              INTEGER               I
!              INTEGER               J
!              INTEGER               NVALS
!              INTEGER               RECNO
!              INTEGER               SEGNO
!
!              LOGICAL               ISNULL
!
!        C
!        C     Open a new EK file.  For simplicity, we won't
!        C     reserve space for the comment area, so the
!        C     number of reserved comment characters is zero.
!        C     The constant IFNAME is the internal file name.
!        C
!              CALL EKOPN ( EKNAME, IFNAME, NRESVC, HANDLE )
!
!        C
!        C     Set up the table and column names and declarations
!        C     for the CHR_DATA segment.  We'll index all of
!        C     the columns.
!        C
!              CNAMES(1) =  'CHR_COL_1'
!              CDECLS(1) =  'DATATYPE = CHARACTER*(*), '
!             .       //    'INDEXED = TRUE, NULLS_OK = TRUE'
!
!              CNAMES(2) =  'CHR_COL_2'
!              CDECLS(2) =  'DATATYPE = CHARACTER*(9), '
!             .       //    'SIZE = VARIABLE, NULLS_OK = TRUE'
!
!        C
!        C     Start the segment.
!        C
!              CALL EKBSEG ( HANDLE, TABLE,  NCOLS,
!             .              CNAMES, CDECLS, SEGNO )
!
!              DO I = 0, NROWS-1
!
!                 CALL EKAPPR ( HANDLE, SEGNO, RECNO )
!
!                 ISNULL = ( I .EQ. 1 )
!
!                 CALL INTSTR ( I, CVALS(1) )
!                 CALL EKACEC ( HANDLE, SEGNO, RECNO, CNAMES(1),
!             .                 1,      CVALS, ISNULL           )
!
!        C
!        C        Array-valued columns follow.
!        C
!                 CALL INTSTR ( 10*I,     CVALS(1) )
!                 CALL INTSTR ( 10*I + 1, CVALS(2) )
!                 CALL INTSTR ( 10*I + 2, CVALS(3) )
!                 CALL INTSTR ( 10*I + 3, CVALS(4) )
!                 CALL EKACEC ( HANDLE, SEGNO, RECNO, CNAMES(2),
!             .                 4,      CVALS, ISNULL           )
!
!              END DO
!
!        C
!        C     End the file.
!        C
!              CALL EKCLS ( HANDLE )
!
!        C
!        C     Open the created file. Show the values added.
!        C
!              CALL EKOPR ( EKNAME, HANDLE )
!
!              DO I = 1, NROWS
!
!                 CALL EKRCEC ( HANDLE, SEGNO, I,     CNAMES(1),
!             .                 NVALS,  CVALS, ISNULL           )
!
!                 IF ( .NOT. ISNULL ) THEN
!
!                    WRITE(*,*) 'Data from column: ', CNAMES(1)
!                    WRITE(*,*) '   record number: ', I
!                    WRITE(*,*) '   values       : ',
!             .                                ( CVALS(J), J=1,NVALS )
!                    WRITE(*,*) ' '
!
!                 ELSE
!
!                    WRITE(*,*) 'Record ', I, 'flag is NULL.'
!                    WRITE(*,*) ' '
!
!                 END IF
!
!        C
!        C        Array-valued columns follow.
!        C
!                 CALL EKRCEC ( HANDLE, SEGNO, I,     CNAMES(2),
!             .                 NVALS,  CVALS, ISNULL           )
!
!                 IF ( .NOT. ISNULL ) THEN
!
!                    WRITE(*,*) 'Data from column: ', CNAMES(2)
!                    WRITE(*,*) '   record number: ', I
!                    WRITE(*,*) '   values       : ',
!             .                                ( CVALS(J), J=1,NVALS )
!                    WRITE(*,*) ' '
!
!                 END IF
!
!              END DO
!
!              END
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!         Data from column: CHR_COL_1
!            record number:            1
!            values       : 0
!
!         Data from column: CHR_COL_2
!            record number:            1
!            values       : 0        1        2        3
!
!         Record            2 flag is NULL.
!
!         Data from column: CHR_COL_1
!            record number:            3
!            values       : 2
!
!         Data from column: CHR_COL_2
!            record number:            3
!            values       : 20       21       22       23
!
!         Data from column: CHR_COL_1
!            record number:            4
!            values       : 3
!
!         Data from column: CHR_COL_2
!            record number:            4
!            values       : 30       31       32       33
!
!         Data from column: CHR_COL_1
!            record number:            5
!            values       : 4
!
!         Data from column: CHR_COL_2
!            record number:            5
!            values       : 40       41       42       43
!
!         Data from column: CHR_COL_1
!            record number:            6
!            values       : 5
!
!         Data from column: CHR_COL_2
!            record number:            6
!            values       : 50       51       52       53
!
!        Note that the second record does not appear due to setting the
!        ISNULL flag to true for that record.
!
!        After run completion, a new EK exists in the output directory.
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
!-    SPICELIB Version 1.4.0, 06-JUL-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!        Added complete code example.
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
!        to look up data in the class 9 case. Miscellaneous header
!        changes were made as well. Check for string truncation on
!        output has been added.
!
!-    SPICELIB Version 1.0.0, 26-SEP-1995 (NJB)
!
!-&

! $ Index_Entries
!
!     read character data from EK column
!
!-&

! $ Revisions
!
!-    SPICELIB Version 1.1.0, 28-JUL-1997 (NJB)
!
!        Bug fix: Record number, not record pointer, is now supplied
!        to look up data in the class 9 case. For class 9 columns,
!        column entry locations are calculated directly from record
!        numbers, no indirection is used.
!
!        Miscellaneous header changes were made as well.
!
!        The routines
!
!           ZZEKRD03
!           ZZEKRD06
!           ZZEKRD09
!
!        now check for string truncation on output and signal errors
!        if truncation occurs.
!
!-&

!
!     SPICELIB functions
!
      logical               failed

!
!     Non-SPICELIB functions
!
      integer(ip)           zzekesiz

!
!     Local variables
!
      integer(ip)           coldsc ( cdscsz )
      integer(ip)           class
      integer(ip)           cvlen
      integer(ip)           dtype
      integer(ip)           recptr
      integer(ip)           segdsc ( sdscsz )

      logical               found

!
!     First step:  find the descriptor for the named segment.  Using
!     this descriptor, get the column descriptor.
!
      call zzeksdsc ( handle, segno,  segdsc          )
      call zzekcdsc ( handle, segdsc, column, coldsc  )

      if ( failed() ) return

!
!     This column had better be of character type.
!
      dtype  =  coldsc(typidx)

      if ( dtype /= chr ) then

         call setmsg ( 'Column # is of type #; EKRCEC only works '    // &
                       'with character columns.  RECNO = #; SEGNO = ' // &
                       '#; EK = #.'                                    )
         call errch  ( '#',  column                                    )
         call errint ( '#',  dtype                                     )
         call errint ( '#',  recno                                     )
         call errint ( '#',  segno                                     )
         call errhan ( '#',  handle                                    )
         call sigerr ( 'SPICE(WRONGDATATYPE)'                          )
         return

      end if

!
!     Now it's time to read data from the file.  Call the low-level
!     reader appropriate to the column's class.
!
      class  =  coldsc ( clsidx )

      if ( class == 3 ) then
!
!        Look up the record pointer for the target record.
!
         call zzektrdp (  handle,  segdsc(rtidx),  recno, recptr )

         call zzekrd03 (  handle,  segdsc, coldsc, &
                          recptr,  cvlen,  cvals,  isnull  )
         nvals  =  1

      else if ( class == 6 ) then

         call zzektrdp ( handle, segdsc(rtidx), recno, recptr )

         nvals  =  zzekesiz ( handle, segdsc, coldsc, recptr )

         call zzekrd06 (  handle,  segdsc,  coldsc,  recptr, &
                          1,       nvals,   cvals,   isnull,  found  )

      else if ( class == 9 ) then
!
!        Records in class 9 columns are identified by a record number
!        rather than a pointer.
!
         call zzekrd09 (  handle,  segdsc, coldsc, &
                          recno,   cvlen,  cvals,  isnull  )
         nvals  =  1

      else

!
!        This is an unsupported character column class.
!
         segno  =  segdsc ( snoidx )

         call setmsg ( 'Class # from input column descriptor is not ' // &
                       'a supported character class.  COLUMN = #; '   // &
                       'RECNO = #; SEGNO = #; EK = #.'                 )
         call errint ( '#',  class                                     )
         call errch  ( '#',  column                                    )
         call errint ( '#',  recno                                     )
         call errint ( '#',  segno                                     )
         call errhan ( '#',  handle                                    )
         call sigerr ( 'SPICE(NOCLASS)'                                )
         return

      end if

      return
      end
