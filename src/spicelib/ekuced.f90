!$Procedure EKUCED ( EK, update d.p. column entry )

      subroutine ekuced (  handle,  segno,  recno,  column, &
                           nvals,   dvals,  isnull          )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Update a double precision column entry in a specified EK record.
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
      real(wp)              dvals  ( * )
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
!     DVALS      I   Double precision values to add to column.
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
!     DVALS    are, respectively, the number of values to add to
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
!              defined by NVALS and DVALS is added to the
!              specified kernel file.
!
!              If ISNULL is .TRUE., NVALS and DVALS are ignored.
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
!     4)  If COLUMN specifies a column of whose data type is not DOUBLE
!         PRECISION or TIME, the error SPICE(WRONGDATATYPE) is signaled.
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
!         a double precision class known to this routine, the error
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
!     Since columns of data type TIME are implemented using double
!     precision column classes, this routine may be used to update
!     columns of type TIME.
!
! $ Examples
!
!     1)  Replace the value in the third record of the column DCOL in
!         the fifth segment of an EK file designated by HANDLE. Set
!         the new value to 999.D0.
!
!            CALL EKUCED ( HANDLE, 5, 3, 'DCOL', 1, 999.D0, .FALSE. )
!
!     2)  Same as (1), but this time add a null value. The argument
!         999.D0 is ignored because the null flag is set to .TRUE.
!
!            CALL EKUCED ( HANDLE, 5, 3, 'DCOL', 1, 999.D0, .TRUE. )
!
!     3)  Replace the entry in the third record of the column DARRAY in
!         the fifth segment of an EK file designated by HANDLE. Set
!         the new value using an array DBUFF of 10 d.p. values.
!
!            CALL EKUCED ( HANDLE, 5, 3, 'DARRAY', 10, DBUFF, .FALSE. )
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
!
!-    SPICELIB Version 1.2.0, 06-FEB-2015 (NJB)
!
!        Now uses ERRHAN to insert DAS file name into
!        long error messages.
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
!     replace d.p. entry in an EK column
!     replace time entry in an EK column
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
!     First step:  find the descriptor for the named segment.  Using
!     this descriptor, get the column descriptor.
!
      call zzeksdsc ( handle, segno,  segdsc          )
      call zzekcdsc ( handle, segdsc, column, coldsc  )

      if ( failed() ) return

!
!     This column had better be of double precision or `time' type.
!
      dtype  =  coldsc(typidx)

      if (  ( dtype /= dp ) .and. ( dtype /= time )  ) then

         call setmsg ( 'Column # is of type #; EKUCED only works '    // &
                       'with d.p. or TIME columns.  RECNO = #; '      // &
                       'SEGNO = #; EK = #.'                            )
         call errch  ( '#',  column                                    )
         call errint ( '#',  dtype                                     )
         call errint ( '#',  recno                                     )
         call errint ( '#',  segno                                     )
         call errhan ( '#',  handle                                    )
         call sigerr ( 'SPICE(WRONGDATATYPE)'                          )
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

      if ( class == 2 ) then
!
!        Class 2 columns contain scalar d.p. data.
!
         call zzekue02 ( handle, segdsc, coldsc, recptr, dvals, isnull )

      else if ( class == 5 ) then
!
!        Class 5 columns contain array-valued d.p. data.
!
         call zzekue05 ( handle,  segdsc, coldsc, &
                         recptr,  nvals,  dvals,  isnull )

      else
!
!        This is an unsupported d.p. column class.
!
         segno  =  segdsc ( snoidx )

         call setmsg ( 'Class # from input column descriptor is not ' // &
                       'a supported d.p. class.  COLUMN = #; '        // &
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
