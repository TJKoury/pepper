!$Procedure EKUCEC ( EK, update character column entry )

      subroutine ekucec (  handle,  segno,  recno,  column, &
                           nvals,   cvals,  isnull          )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Update a character column entry in a specified EK record.
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
      character*(*)         column
      integer(ip)           nvals
      character*(*)         cvals  ( * )
      logical               isnull

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle attached to EK file.
!     SEGNO      I   Index of segment containing record.
!     RECNO      I   Record in which entry is to be updated.
!     COLUMN     I   Column name.
!     NVALS      I   Number of values in new column entry.
!     CVALS      I   Character string values to add to column.
!     ISNULL     I   Flag indicating whether column entry is null.
!
! $ Detailed_Input
!
!     HANDLE   is a file handle attached to an EK open for
!              write access.
!
!     SEGNO    is the index of the segment containing the column
!              entry to be updated.
!
!     RECNO    is the index of the record containing the column
!              entry to be updated. This record number is
!              relative to the start of the segment indicated by
!              SEGNO; the first record in the segment has index 1.
!
!     COLUMN   is the name of the column containing the entry to
!              be updated.
!
!     NVALS,
!     CVALS    are, respectively, the number of values to add to
!              the specified column and the set of values
!              themselves. The data values are written in to the
!              specified column and record.
!
!              If the  column has fixed-size entries, then NVALS
!              must equal the entry size for the specified column.
!
!              For columns with variable-sized entries, the size
!              of the new entry need not match the size of the
!              entry it replaces. In particular, the new entry
!              may be larger.
!
!     ISNULL   is a logical flag indicating whether the entry is
!              null. If ISNULL is .FALSE., the column entry
!              defined by NVALS and CVALS is added to the
!              specified kernel file.
!
!              If ISNULL is .TRUE., NVALS and CVALS are ignored.
!              The contents of the column entry are undefined.
!              If the column has fixed-length, variable-size
!              entries, the number of entries is considered to
!              be 1.
!
!              The new entry may be null even though it replaces
!              a non-null value, and vice versa.
!
! $ Detailed_Output
!
!     None. See $Particulars for a description of the effect of this
!     routine.
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
!     3)  If COLUMN is not the name of a declared column, an error
!         is signaled by a routine in the call tree of this routine.
!
!     4)  If COLUMN specifies a column of whose data type is not
!         CHARACTER, the error SPICE(WRONGDATATYPE) is signaled.
!
!     5)  If RECNO is out of range, an error is signaled by a routine in
!         the call tree of this routine.
!
!     6)  If the specified column has fixed-size entries and NVALS does
!         not match this size, an error is signaled by a routine in the
!         call tree of this routine.
!
!     7)  If the specified column has variable-size entries and NVALS is
!         non-positive, an error is signaled by a routine in the call
!         tree of this routine.
!
!     8)  If an attempt is made to add a null value to a column that
!         doesn't take null values, an error is signaled by a routine in
!         the call tree of this routine.
!
!     9)  If COLUMN specifies a column of whose class is not
!         a character class known to this routine, the error
!         SPICE(NOCLASS) is signaled.
!
!     10) If an I/O error occurs while reading or writing the indicated
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
!     This routine operates by side effects: it modifies the named
!     EK file by adding data to the specified record in the specified
!     column. Data may be added to a segment in random order; it is not
!     necessary to fill in columns or rows sequentially. Data may only
!     be added one logical element at a time. Partial assignments of
!     logical elements are not supported.
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
!        file, how to update the data in this record, and how to read
!        the data from it.
!
!
!        Example code begins here.
!
!
!              PROGRAM EKUCEC_EX1
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
!              PARAMETER           ( EKNAME = 'ekucec_ex1.bdb' )
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
!        C     Open the EK for write access.
!        C
!              CALL EKOPW ( EKNAME, HANDLE )
!
!        C
!        C     Negate the values in the odd-numbered records
!        C     using the update routines.
!        C
!              DO I = 1, NROWS, 2
!
!                 RECNO  = I+1
!
!                 ISNULL = ( I .EQ. 1 )
!
!                 CALL INTSTR ( -I, CVALS(1) )
!                 CALL EKUCEC ( HANDLE, SEGNO, RECNO, CNAMES(1),
!             .                 1,      CVALS, ISNULL           )
!
!        C
!        C        Array-valued columns follow.
!        C
!                 CALL INTSTR ( -10*I,       CVALS(1) )
!                 CALL INTSTR ( -(10*I + 1), CVALS(2) )
!                 CALL INTSTR ( -(10*I + 2), CVALS(3) )
!                 CALL INTSTR ( -(10*I + 3), CVALS(4) )
!                 CALL EKUCEC ( HANDLE, SEGNO, RECNO, CNAMES(2),
!             .                 4,      CVALS, ISNULL           )
!
!              END DO
!
!        C
!        C     Close the file.
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
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
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
!            values       : -3
!
!         Data from column: CHR_COL_2
!            record number:            4
!            values       : -30      -31      -32      -33
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
!            values       : -5
!
!         Data from column: CHR_COL_2
!            record number:            6
!            values       : -50      -51      -52      -53
!
!
!        Note that the second record does not appear due to setting the
!        ISNULL flag to true for that record. The odd value record
!        numbers have negative values as a result of the update calls.
!
!        After run completion, a new EK exists in the output directory.
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.1, 06-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!        Added complete code example.
!
!-    SPICELIB Version 1.2.0, 06-FEB-2015 (NJB)
!
!        Now uses ERRHAN to insert DAS file name into
!        long error messages.
!
!        Corrected some header comment errors (cut-and-paste
!        errors referring to double precision or time data).
!
!-    SPICELIB Version 1.1.0, 20-JUN-1999 (WLT)
!
!        Removed unbalanced call to CHKOUT.
!
!-    SPICELIB Version 1.0.0, 26-SEP-1995 (NJB)
!
!-&


! $ Index_Entries
!
!     replace character entry in an EK column
!
!-&


!
!     SPICELIB functions
!
      logical               failed

!
!     Local variables
!
      integer(ip)           coldsc ( cdscsz )
      integer(ip)           class
      integer(ip)           dtype
      integer(ip)           recptr
      integer(ip)           segdsc ( sdscsz )

      logical               isshad

!
!     Use discovery check-in.
!
!     First step:  find the descriptor for the named segment.  Using
!     this descriptor, get the column descriptor.
!
      call zzeksdsc ( handle, segno,  segdsc          )
      call zzekcdsc ( handle, segdsc, column, coldsc  )

      if ( failed() ) then
         return
      end if

!
!     This column had better be of character type.
!
      dtype  =  coldsc(typidx)

      if ( dtype .ne. chr ) then

         call chkin  ( 'EKUCEC'                                        )
         call setmsg ( 'Column # is of type #; EKUCEC only works '    // &
                       'with character columns.  RECNO = #; '      // &
                       'SEGNO = #; EK = #.'                            )
         call errch  ( '#',  column                                    )
         call errint ( '#',  dtype                                     )
         call errint ( '#',  recno                                     )
         call errint ( '#',  segno                                     )
         call errhan ( '#',  handle                                    )
         call sigerr ( 'SPICE(WRONGDATATYPE)'                          )
         call chkout ( 'EKUCEC'                                        )
         return

      end if

!
!     Look up the record pointer for the target record.
!
      call zzektrdp ( handle, segdsc(rtidx), recno, recptr )

!
!     Determine whether the EK is shadowed.
!
      call ekshdw ( handle, isshad )

!
!     If the EK is shadowed, we must back up the current column entry
!     if the entry has not already been backed up.  ZZEKRBCK will
!     handle this task.
!
      if ( isshad ) then
         call zzekrbck ( 'UPDATE', handle, segdsc, coldsc, recno )
      end if

!
!     Now it's time to carry out the replacement.
!
      class  =  coldsc ( clsidx )


      if ( class .eq. 3 ) then
!
!        Class 3 columns contain scalar character data.
!
         call zzekue03 ( handle, segdsc, coldsc, recptr, cvals, isnull )


      else if ( class .eq. 6 ) then
!
!        Class 6 columns contain array-valued character data.
!
         call zzekue06 ( handle,  segdsc, coldsc, &
                         recptr,  nvals,  cvals,  isnull )

      else
!
!        This is an unsupported character column class.
!
         segno  =  segdsc ( snoidx )

         call chkin  ( 'EKUCEC'                                        )
         call setmsg ( 'Class # from input column descriptor is not ' // &
                       'a supported character class.  COLUMN = #; '   // &
                       'RECNO = #; SEGNO = #; EK = #.'                 )
         call errint ( '#',  class                                     )
         call errch  ( '#',  column                                    )
         call errint ( '#',  recno                                     )
         call errint ( '#',  segno                                     )
         call errhan ( '#',  handle                                    )
         call sigerr ( 'SPICE(NOCLASS)'                                )
         call chkout ( 'EKUCEC'                                        )
         return

      end if

      return
      end
