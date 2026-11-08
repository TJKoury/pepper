!$Procedure      ZZEKSDSC ( EK, get segment descriptor )

      subroutine zzeksdsc ( handle, segno, segdsc )

! $ Abstract
!
!     Look up the descriptor of a specified EK segment.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     PRIVATE
!     UTILITY
!
! $ Declarations


      include 'eksegdsc.inc'

      integer               handle
      integer               segno
      integer               segdsc ( sdscsz )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     SEGNO      I   Segment number.
!     SEGDSC     O   Segment descriptor.
!
! $ Detailed_Input
!
!     HANDLE         is a file handle of an EK open for read or write
!                    access.
!
!     SEGNO          is the number of a segment whose descriptor is
!                    desired.  A segment number is simply the ordinal
!                    position of the segment in its parent EK.
!
! $ Detailed_Output
!
!     SEGDSC         is the descriptor of the specified segment.
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
!     2)  If SEGNO is invalid, the error will be diagnosed by routines
!         called by this routine.
!
!     3)  If an I/O error occurs while reading the indicated file,
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
!     This routine centralizes the coded needed to look up the
!     descriptor of a specified segment.  This is a frequently
!     performed function.
!
! $ Examples
!
!     See EKACEC.
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
!-    Beta Version 1.0.0, 10-OCT-1995 (NJB)
!
!-&

!
!     Local variables
!
      integer               mp
      integer               mbase

!
!     Use discovery check-in.
!
      call zzekmloc (  handle,  segno,    mp,            mbase   )
      call dasrdi   (  handle,  mbase+1,  mbase+sdscsz,  segdsc  )

      return
      end
