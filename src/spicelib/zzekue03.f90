!$Procedure      ZZEKUE03 ( EK, update column entry, class 3 )

      subroutine zzekue03 (  handle,  segdsc,  coldsc, &
                             recptr,  cval,    isnull  )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Update a specified class 3 column entry in an EK record.
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


      include 'ekbool.inc'
      include 'ekcoldsc.inc'
      include 'ekdatpag.inc'
      include 'ekrecptr.inc'
      include 'eksegdsc.inc'
      include 'ektype.inc'

      integer(ip)           handle
      integer(ip)           segdsc ( sdscsz )
      integer(ip)           coldsc ( cdscsz )
      integer(ip)           recptr
      character*(*)         cval
      logical               isnull

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     SEGDSC     I   Segment descriptor.
!     COLDSC     I   Column descriptor.
!     RECPTR     I   Record pointer.
!     CVAL       I   Character string value.
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
!     CVAL           is the character string value with which to update
!                    the specified column entry.
!
!     ISNULL         is a logical flag indicating whether the value
!                    of the specified column entry is to be set to NULL.
!                    If so, the input CVAL is ignored.
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
!     2)  If RECNO is out of range, the error SPICE(INVALIDINDEX)
!         will be signaled.  The file will not be modified.
!
!     3)  If an I/O error occurs while reading or writing the indicated
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
!     in an EK segment.  This routine does not participate in shadowing
!     functions.  If the target EK is shadowed, the caller is
!     responsible for performing necessary backup operations.  If the
!     target EK is not shadowed, the target record's status is not
!     modified.
!
!     If the column containing the entry is indexed, the corresponding
!     index is updated.
!
!     The changes made by this routine to the target EK file become
!     permanent when the file is closed.  Failure to close the file
!     properly will leave it in an indeterminate state.
!
! $ Examples
!
!     See EKUCEC.
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
!-    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB)
!
!        Corrected typos in comments.
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
         call chkin ( 'ZZEKUE03' )
      end if

!
!     Get rid of the old column entry first.
!
      call zzekde03 ( handle, segdsc, coldsc, recptr )

      if ( failed() ) then
         call  chkout ( 'ZZEKUE03' )
         return
      end if

!
!     We've reduced the problem to a solved one:  that of adding
!     a column entry.
!
      call zzekad03 ( handle, segdsc, coldsc, recptr, cval, isnull )


      call chkout ( 'ZZEKUE03' )
      return
      end
