!$Procedure      ZZEKUE04 ( EK, update column entry, class 4 )

      subroutine zzekue04 (  handle,  segdsc,  coldsc, &
                             recptr,  nvals,   ivals,  isnull  )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Update a specified class 4 column entry in an EK record.
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


      include 'ekcoldsc.inc'
      include 'eksegdsc.inc'
      include 'ektype.inc'

      integer(ip)           handle
      integer(ip)           segdsc ( sdscsz )
      integer(ip)           coldsc ( cdscsz )
      integer(ip)           recptr
      integer(ip)           nvals
      integer(ip)           ivals
      logical               isnull

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     SEGDSC     I   Segment descriptor.
!     COLDSC     I   Column descriptor.
!     RECPTR     I   Record pointer.
!     NVALS      I   Number of values.
!     IVALS      I   Integer values.
!     ISNULL     I   Null flag.
!
! $ Detailed_Input
!
!     HANDLE         is a file handle of an EK open for write access.
!
!     SEGDSC         is the descriptor of the segment containing
!                    the specified column entry.
!
!     COLDSC         is the descriptor of the column containing
!                    the specified column entry.
!
!     RECPTR         is a pointer to the record containing the column
!                    entry to update.
!
!     NVALS          is the number of values in the replacement
!                    column entry.
!
!     IVALS          is an array of integer values with which to update
!                    the specified column entry.
!
!
!     ISNULL         is a logical flag indicating whether the value
!                    of the specified column entry is to be set to NULL.
!                    If so, the input IVALS is ignored.
!
! $ Detailed_Output
!
!     None.  See the $Particulars section for a description of the
!     effect of this routine.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If HANDLE is invalid, the error will be diagnosed by routines
!         called by this routine.  The file will not be modified.
!
!     2)  If an I/O error occurs while reading or writing the indicated
!         file, the error will be diagnosed by routines called by this
!         routine.  The file may be corrupted.
!
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This routine operates by side effects:  it updates a column entry
!     in an EK segment.  The status of the record containing the entry
!     is set to `updated'.  If the column containing the entry is
!     indexed, the corresponding index is updated.
!
!     The changes made by this routine to the target EK file become
!     permanent when the file is closed.  Failure to close the file
!     properly will leave it in an indeterminate state.
!
! $ Examples
!
!     See EKUCEI.
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
!-    Beta Version 1.0.0, 27-SEP-1995 (NJB)
!
!-&

!
!     SPICELIB functions
!
      logical               failed
      logical               return

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'ZZEKUE04' )
      end if

!
!     Get rid of the old column entry first.
!
      call zzekde04 ( handle, segdsc, coldsc, recptr )

      if ( failed() ) then
         call  chkout ( 'ZZEKUE04' )
         return
      end if

!
!     We've reduced the problem to a solved one:  that of adding
!     a column entry.
!
      call zzekad04 ( handle,  segdsc,  coldsc, &
                      recptr,  nvals,   ivals,  isnull )


      call chkout ( 'ZZEKUE04' )
      return
      end
