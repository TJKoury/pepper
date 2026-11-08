!$Procedure   ZZEKRSC ( EK, read scalar, character )

      subroutine zzekrsc (  handle,  segdsc,  coldsc,  recptr, &
                            eltidx,  cvlen,   cval,    isnull, found  )

! $ Abstract
!
!     Read scalar data from a character column in a specified EK
!     record.
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

      include 'ekcnamsz.inc'
      include 'ekcoldsc.inc'
      include 'eksegdsc.inc'
      include 'ektype.inc'

      integer               handle
      integer               segdsc ( sdscsz )
      integer               coldsc ( cdscsz )
      integer               recptr
      integer               eltidx
      integer               cvlen
      character*(*)         cval
      logical               isnull
      logical               found

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
      integer               zzekrp2n

!
!     Local variables
!
      character*(cnamsz)    column

      integer               class
      integer               dtype
      integer               recno
      integer               segno

!
!     Use discovery check-in.
!
!     Nothing found to begin with.
!
      found  =  .false.

!
!     This column had better be of character type.
!
      dtype  =  coldsc(typidx)


      if ( dtype .ne. chr ) then

         call zzekcnam ( handle, coldsc, column )

         segno  =  segdsc   ( snoidx )
         recno  =  zzekrp2n ( handle, segdsc(snoidx), recptr )

         call chkin  ( 'ZZEKRSC'                                       )
         call setmsg ( 'Column # is of type #; ZZEKRSC only works '   // &
                       'with integer columns.  RECNO = #; SEGNO = '   // &
                       '#; EK = #.'                                    )
         call errch  ( '#',  column                                    )
         call errint ( '#',  dtype                                     )
         call errint ( '#',  recno                                     )
         call errint ( '#',  segno                                     )
         call errhan ( '#',  handle                                    )
         call sigerr ( 'SPICE(WRONGDATATYPE)'                          )
         call chkout ( 'ZZEKRSC'                                       )
         return

      end if

!
!     Now it's time to read data from the file.  Call the low-level
!     reader appropriate to the column's class.
!
      class  =  coldsc ( clsidx )


      if ( class .eq. 3 ) then

         call zzekrd03 (  handle,  segdsc,  coldsc, &
                          recptr,  cvlen,   cval,   isnull  )

         found  =  .true.


      else if ( class .eq. 6 ) then
!
!        Class 6 columns contain character string array entries.
!
         call zzekrd06 ( handle,  segdsc,  coldsc,  recptr, &
                         eltidx,  eltidx,  cval,    isnull,  found )


      else if ( class .eq. 9 ) then

         call zzekrd09 (  handle,  segdsc,  coldsc, &
                          recptr,  cvlen,   cval,   isnull  )

         found  =  .true.


      else

!
!        This is an unsupported character column class.
!
         call zzekcnam ( handle, coldsc, column )

         segno  =  segdsc   ( snoidx )
         recno  =  zzekrp2n ( handle, segdsc(snoidx), recptr )

         call chkin  ( 'ZZEKRSC'                                       )
         call setmsg ( 'Class # from input column descriptor is not ' // &
                       'a supported character class.  COLUMN = #; '   // &
                       'RECNO = #; SEGNO = #; EK = #.'                 )
         call errint ( '#',  class                                     )
         call errch  ( '#',  column                                    )
         call errint ( '#',  recno                                     )
         call errint ( '#',  segno                                     )
         call errhan ( '#',  handle                                    )
         call sigerr ( 'SPICE(NOCLASS)'                                )
         call chkout ( 'ZZEKRSC'                                       )
         return

      end if


      return
      end
