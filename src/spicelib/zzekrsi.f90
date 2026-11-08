!$Procedure   ZZEKRSI ( EK, read scalar, integer )

      SUBROUTINE ZZEKRSI (  HANDLE,  SEGDSC,  COLDSC,  RECPTR, &
                            ELTIDX,  IVAL,    ISNULL,  FOUND  )

! $ Abstract
!
!     Read scalar data from an integer column in a specified EK record.
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

      IMPLICIT NONE

      INCLUDE 'ekcnamsz.inc'
      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'

      INTEGER               HANDLE
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               COLDSC ( CDSCSZ )
      INTEGER               RECPTR
      INTEGER               ELTIDX
      INTEGER               IVAL
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
!     IVAL       O   Integer value in column entry.
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
!                    column from which to read.
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
!     IVAL           is the specified column entry.  IVAL is valid only
!                    when FOUND is set to .TRUE.
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
!     2)  If COLDSC is not the descriptor of a declared column, the
!         error will be diagnosed by routines called by this routine.
!
!     3)  If COLDSC specifies a column of whose data type is not
!         integer, the error SPICE(WRONGDATATYPE) will be signaled.
!
!     4)  If COLDSC specifies a column of whose class is not
!         an integer class known to this routine, the error
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
!     This column had better be of integer type.
!
      DTYPE  =  COLDSC(TYPIDX)

      IF ( DTYPE .NE. INT ) THEN

         CALL ZZEKCNAM ( HANDLE, COLDSC, COLUMN )

         SEGNO  =  SEGDSC   ( SNOIDX )
         RECNO  =  ZZEKRP2N ( HANDLE, SEGDSC(SNOIDX), RECPTR )

         CALL CHKIN  ( 'ZZEKRSI'                                       )
         CALL SETMSG ( 'Column # is of type #; ZZEKRSI only works '   // &
                       'with integer columns.  RECNO = #; SEGNO = '   // &
                       '#; EK = #.'                                    )
         CALL ERRCH  ( '#',  COLUMN                                    )
         CALL ERRINT ( '#',  DTYPE                                     )
         CALL ERRINT ( '#',  RECNO                                     )
         CALL ERRINT ( '#',  SEGNO                                     )
         CALL ERRHAN ( '#',  HANDLE                                    )
         CALL SIGERR ( 'SPICE(WRONGDATATYPE)'                          )
         CALL CHKOUT ( 'ZZEKRSI'                                       )
         RETURN

      END IF

!
!     Now it's time to read data from the file.  Call the low-level
!     reader appropriate to the column's class.
!
      CLASS  =  COLDSC ( CLSIDX )


      IF ( CLASS .EQ. 1 ) THEN
!
!        Class 1 columns contain scalar, integer entries.
!
         CALL ZZEKRD01 ( HANDLE, SEGDSC, COLDSC, RECPTR, IVAL, ISNULL )

         FOUND  =  .TRUE.


      ELSE IF ( CLASS .EQ. 4 ) THEN
!
!        Class 4 columns contain integer array entries.
!
         CALL ZZEKRD04 ( HANDLE,  SEGDSC,  COLDSC,  RECPTR, &
                         ELTIDX,  ELTIDX,  IVAL,    ISNULL,  FOUND )


      ELSE IF ( CLASS .EQ. 7 ) THEN
!
!        Class 7 columns are fixed-count columns that contain scalar,
!        integer entries.
!
         CALL ZZEKRD07 ( HANDLE, SEGDSC, COLDSC, RECPTR, IVAL, ISNULL )

         FOUND  =  .TRUE.


      ELSE

!
!        This is an unsupported integer column class.
!
         CALL ZZEKCNAM ( HANDLE, COLDSC, COLUMN )

         SEGNO  =  SEGDSC   ( SNOIDX )
         RECNO  =  ZZEKRP2N ( HANDLE, SEGDSC(SNOIDX), RECPTR )

         CALL CHKIN  ( 'ZZEKRSI'                                       )
         CALL SETMSG ( 'Class # from input column descriptor is not ' // &
                       'a supported integer class.  COLUMN = #; '     // &
                       'RECNO = #; SEGNO = #; EK = #.'                 )
         CALL ERRINT ( '#',  CLASS                                     )
         CALL ERRCH  ( '#',  COLUMN                                    )
         CALL ERRINT ( '#',  RECNO                                     )
         CALL ERRINT ( '#',  SEGNO                                     )
         CALL ERRHAN ( '#',  HANDLE                                    )
         CALL SIGERR ( 'SPICE(NOCLASS)'                                )
         CALL CHKOUT ( 'ZZEKRSI'                                       )
         RETURN

      END IF

      RETURN
      END
