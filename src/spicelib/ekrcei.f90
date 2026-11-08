!$Procedure EKRCEI ( EK, read column entry element, integer )

      subroutine ekrcei ( handle,  segno,  recno,  column, &
                          nvals,   ivals,  isnull          )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Read data from an integer column in a specified EK record.
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
      integer(ip)           ivals  ( * )
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
!     IVALS      O   Integer values in column entry.
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
!     IVALS    are, respectively, the number of values found in the
!              specified column entry and the set of values themselves.
!
!              For columns having fixed-size entries, when a a column
!              entry is null, NVALS is still set to the column entry
!              size. For columns having variable-size entries, NVALS is
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
!         integer, the error SPICE(WRONGDATATYPE) is signaled.
!
!     6)  If COLUMN specifies a column of whose class is not
!         an integer class known to this routine, the error
!         SPICE(NOCLASS) is signaled.
!
!     7)  If an attempt is made to read an uninitialized column entry,
!         an error is signaled by a routine in the call tree of this
!         routine. A null entry is considered to be initialized, but
!         entries do not contain null values by default.
!
!     8)  If an I/O error occurs while reading the indicated file, the
!         error is signaled by a routine in the call tree of this
!         routine.
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
!     1)  Read the value in the third record of the column ICOL in
!         the fifth segment of an EK file designated by HANDLE.
!
!            CALL EKRCEI ( HANDLE, 5, 3, 'ICOL', N, IVAL, ISNULL )
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
!        to look up data in the class 7 case. Miscellaneous header
!        changes were made as well.
!
!-    SPICELIB Version 1.0.0, 06-NOV-1995 (NJB)
!
!-&


! $ Index_Entries
!
!     read integer data from EK column
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.1.0, 28-JUL-1997 (NJB)
!
!        Bug fix: Record number, not record pointer, is now supplied
!        to look up data in the class 7 case. For class 7 columns,
!        column entry locations are calculated directly from record
!        numbers; no indirection is used.
!
!        Miscellaneous header changes were made as well.
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
      integer(ip)           dtype
      integer(ip)           recptr
      integer(ip)           segdsc ( sdscsz )

      logical               found

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
!     This column had better be of integer type.
!
      dtype  =  coldsc(typidx)

      if ( dtype /= int ) then

         call chkin  ( 'EKRCEI'                                        )
         call setmsg ( 'Column # is of type #; EKRCEI only works '    // &
                       'with integer columns.  RECNO = #; SEGNO = '   // &
                       '#; EK = #.'                                    )
         call errch  ( '#',  column                                    )
         call errint ( '#',  dtype                                     )
         call errint ( '#',  recno                                     )
         call errint ( '#',  segno                                     )
         call errhan ( '#',  handle                                    )
         call sigerr ( 'SPICE(WRONGDATATYPE)'                          )
         call chkout ( 'EKRCEI'                                        )
         return

      end if

!
!     Now it's time to read data from the file.  Call the low-level
!     reader appropriate to the column's class.
!
      class  =  coldsc ( clsidx )


      if ( class == 1 ) then
!
!        Look up the record pointer for the target record.
!
         call zzektrdp ( handle, segdsc(rtidx),  recno,  recptr )

         call zzekrd01 ( handle, segdsc, coldsc, recptr, ivals, isnull )
         nvals  =  1


      else if ( class == 4 ) then

         call zzektrdp ( handle, segdsc(rtidx),  recno,  recptr )

         nvals  =  zzekesiz ( handle, segdsc, coldsc, recptr )

         call zzekrd04 (  handle,  segdsc,  coldsc,  recptr, &
                          1,       nvals,   ivals,   isnull,  found  )


      else if ( class == 7 ) then
!
!        Records in class 7 columns are identified by a record number
!        rather than a pointer.
!
         call zzekrd07 ( handle, segdsc, coldsc, recno, ivals, isnull )
         nvals  =  1

      else

!
!        This is an unsupported integer column class.
!
         segno  =  segdsc ( snoidx )

         call chkin  ( 'EKRCEI'                                        )
         call setmsg ( 'Class # from input column descriptor is not ' // &
                       'a supported integer class.  COLUMN = #; '     // &
                       'RECNO = #; SEGNO = #; EK = #.'                 )
         call errint ( '#',  class                                     )
         call errch  ( '#',  column                                    )
         call errint ( '#',  recno                                     )
         call errint ( '#',  segno                                     )
         call errhan ( '#',  handle                                    )
         call sigerr ( 'SPICE(NOCLASS)'                                )
         call chkout ( 'EKRCEI'                                        )
         return

      end if


      return
      end
