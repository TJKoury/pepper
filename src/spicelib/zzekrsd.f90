!$Procedure   ZZEKRSD ( EK, read scalar, double precision )

      subroutine zzekrsd (  handle,  segdsc,  coldsc,  recptr, &
                            eltidx,  dval,    isnull,  found  )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Read scalar data from a double precision column in a specified EK
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

      implicit none

      include 'ekcnamsz.inc'
      include 'ekcoldsc.inc'
      include 'eksegdsc.inc'
      include 'ektype.inc'

      integer(ip)           handle
      integer(ip)           segdsc ( sdscsz )
      integer(ip)           coldsc ( cdscsz )
      integer(ip)           recptr
      integer(ip)           eltidx
      real(wp)              dval
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
!     DVAL       O   D.p. value in column entry.
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
!     DVAL           is the specified column entry.  DVAL is valid only
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
!         double precision, the error SPICE(WRONGDATATYPE) will be
!         signaled.
!
!     4)  If COLDSC specifies a column of whose class is not
!         an double precision class known to this routine, the error
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
      integer(ip)           zzekrp2n

!
!     Local variables
!
      character(cnamsz)     column

      integer(ip)           class
      integer(ip)           dtype
      integer(ip)           recno
      integer(ip)           segno

!
!     Use discovery check-in.
!
!
!     Nothing found to begin with.
!
      found  =  .false.

!
!     This column had better be of d.p. or TIME type.
!
      dtype  =  coldsc(typidx)


      if ( ( dtype /= dp ) .and. ( dtype /= time )  ) then

         call zzekcnam ( handle, coldsc, column )

         segno  =  segdsc   ( snoidx )
         recno  =  zzekrp2n ( handle, segdsc(snoidx), recptr )

         call setmsg ( 'Column # is of type #; ZZEKRSD only works '   // &
                       'with DP or TIME columns.  RECNO = #; SEGNO = '// &
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


      if ( class == 2 ) then

         call zzekrd02 ( handle, segdsc, coldsc, recptr, dval, isnull  )

         found  =  .true.


      else if ( class == 5 ) then
!
!        Class 5 columns contain d.p. array entries.
!
         call zzekrd05 ( handle,  segdsc,  coldsc,  recptr, &
                         eltidx,  eltidx,  dval,    isnull,  found )


      else if ( class == 8 ) then

         call zzekrd08 ( handle, segdsc, coldsc, recptr, dval, isnull  )

         found  =  .true.


      else

!
!        This is an unsupported d.p. column class.
!
         call zzekcnam ( handle, coldsc, column )

         segno  =  segdsc   ( snoidx )
         recno  =  zzekrp2n ( handle, segdsc(snoidx), recptr )

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
