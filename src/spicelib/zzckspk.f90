!$Procedure      ZZCKSPK ( SPK or CK )

      subroutine zzckspk ( handle, ckspk )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     This routine determines whether or not a DAF file attached to
!     the supplied handle is an SPK, CK or unknown file.
!
! $ Required_Reading
!
!      None.
!
! $ Keywords
!
!      PRIVATE
!
! $ Declarations

      implicit none
      integer(ip)           handle
      character(*)          ckspk

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   the handle of a DAF file open for read access.
!     CKSPK      O   the type of the DAF file (SPK,CK or ?)
!
! $ Detailed_Input
!
!     HANDLE     is the handle of a DAF file open for read.
!
! $ Detailed_Output
!
!     CKSPK      is a string containing one of the following 3 values
!                'SPK', 'CK' or '?'
!
! $ Parameters
!
!     None.
!
! $ Files
!
!     None.
!
! $ Exceptions
!
!     None.
!
! $ Particulars
!
!     This routine examines the first segment of a DAF that is
!     a candidate for being an SPK or CK and returns a diagnosis
!     of the type of the file.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     W.L. Taber      (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW)
!
!        Replaced DAFRDA call with DAFGDA.
!        Added IMPLICIT NONE.
!
!-    SPICELIB Version 1.0.0, 03-DEC-1999 (WLT)
!
!-&

      logical               return
      logical               failed

!
!     Local parameters
!
!     The following parameters point to the various slots in the
!     integer portion of the DAF descriptor where the values are
!     located.
!
      integer(ip), parameter :: bodid = 1

      integer(ip), parameter :: cenid = bodid  + 1

      integer(ip), parameter :: spkfrm = cenid  + 1

      integer(ip), parameter :: spktyp = spkfrm + 1

      integer(ip), parameter :: start = spktyp + 1

      integer(ip), parameter :: finish = start  + 1

      integer(ip), parameter :: ckrate = spktyp

      integer(ip), parameter :: cktype = spkfrm

!
!     These parameters give the number of integer and double precision
!     components of the descriptor for SPK and CK files.
!
      integer(ip), parameter :: nd = 2

      integer(ip), parameter :: ni = 6
!
!     The size of a summary.
!
      integer(ip), parameter :: sumsiz = nd + ((ni+1)/2)

      real(wp)              chcktm
      real(wp)              dc    ( nd )
      real(wp)              lastdp
      real(wp)              frsttm
      real(wp)              sum   ( sumsiz )
      real(wp)              times ( 2 )

      integer(ip)           angvel
      integer(ip)           first
      integer(ip)           from
      integer(ip)           ic    ( ni )
      integer(ip)           last
      integer(ip)           nck2
      integer(ip)           nspk
      integer(ip)           size
      integer(ip)           thisnd
      integer(ip)           thisni
      integer(ip)           to
      integer(ip)           type

      logical               ck2ok
      logical               found
      logical               spkok

!
!     Standard SPICE error handling.
!
      if ( return() ) return

!
!     Make sure the values of ND and NI associated with this file
!     have the correct values.
!
      call dafhsf ( handle, thisnd, thisni )

      if (     thisnd /= nd &
          .or. thisni /= ni ) then
         ckspk = '?'
         return
      end if

!
!     We've got the correct values for ND and NI, examine the descriptor
!     for the first array.
!
      call dafbfs ( handle )
      call daffna ( found  )

      if ( failed () ) then
         ckspk = '?'
         return
      end if

!
!     If we don't find any segments, we don't have a clue about
!     the file type.
!
      if ( .not. found ) then

         ckspk = '?'
         return

      end if
!
!     Unpack the summary record.
!
      call dafgs ( sum                 )
      call dafus ( sum, nd, ni, dc, ic )

!
!     Look at the slot where the angular velocity flag would
!     be located if this is a CK file.
!
      angvel = ic(ckrate)
      type   = ic(cktype)

!
!     Test 1. The value of ANGVEL may do the trick
!     right at the start.
!
      if ( angvel == 0 ) then
         ckspk   = 'CK'
         return
      end if

      if ( angvel > 1 ) then
         ckspk   = 'SPK'
         return
      end if

!
!     Test 2. If this is an SPK file, it has a type 01 segment.
!     See if this is something orbiting the solar system
!     barycenter.
!
      if ( ic(cenid) == 0 ) then
         ckspk   = 'SPK'
         return
      end if

!
!     Test 3. This is the super test.  Compute the size of the
!     segment and fetch the last d.p. from the segment.
!

      first  = ic(start)
      last   = ic(finish)
      size   = last  - first +  1
!
!     Check the size of the array to see if it has any chance
!     of being an SPK and if it does get the number of MDA records.
!
      call zzsizeok ( size-1, 72, 100, 0, spkok, nspk )

      if ( .not. spkok ) then
         ckspk = 'CK'
         return
      end if

      call dafgda ( handle, last, last, lastdp )
!
!     See if the last number in the file is the allowed number of
!     MDA records.  If not, this must be a CK segment.
!
      if ( lastdp /= real(nspk, wp) ) then
         ckspk   = 'CK'
         return
      end if
!
!     If we are still here, the last d.p. in the segment matches the
!     expected number of MDA records.  If the potential CK type is
!     not 2, we must have an SPK file.
!
      if ( type /= 2 ) then
         ckspk = 'SPK'
         return
      end if
!
!     We are getting down to the nitty gritty here. See if the
!     size is compatible with a type 02 C-kernel.
!
      call zzsizeok ( size, 10, 100, 1, ck2ok, nck2 )

      if ( .not. ck2ok ) then
         ckspk = 'SPK'
         return
      end if

!
!     So much for being nice. We need to examine the structure of the
!     actual data in the segment.  There are two cases to consider:
!     when there is 1 or fewer type 02 CK directory records and when
!     there is more than 1.  Note that to get to this point there must
!     be at least 1 directory value if this is a CK type 02 segment.
!     (To see this check the sizes when ZZSIZEOK returns TRUE for
!     both type 1 SPK and type 02 CK. The only such sizes in which
!     there the number CK type 02 directory values is one or fewer
!     are SIZE = 1081, 1441, and 1801 which correspond to (NSPK,NCK2) =
!     (15,108), (20,144), (25, 180).  In all of these cases there is
!     exactly 1 ck type 02 directory value.)
!
      if ( nck2 < 201 ) then
!
!        Recall that MDA record contains its stop time as the first
!        entry of the record.  These epochs show up duplicated in the
!        epochs portion of the segment.
!
!        If this is a type 01 SPK segment, there are no directory
!        records and the first epoch shows up in the slot NSPK before
!        the last slot of the segment.  If it is a type 02 CK segment
!        the last stop tick shows up in this slot.  We need to look
!        at this value to see what's up.
!
         call dafgda ( handle, last-nspk, last-nspk, frsttm )

!
!        Now (under the assumption that we have an SPK segment) look
!        up the epoch from the last MDA record--- the NSPK'th
!        record.  This epoch must be greater than the first epoch
!        in the array of epochs.
         from = first + (nspk-1)*71
         to   = from
         call dafgda ( handle, from, to, chcktm )

!
!        If this is a type 02 segment.  The value we just picked out
!        will come from the array of stop ticks.  The array of stop
!        ticks is non-decreasing so:
!
         if ( chcktm > frsttm ) then
            ckspk = 'SPK'
         else
            ckspk = 'CK'
         end if

      else
!
!        In this case there are at least 2 directory records if we
!        have a CK.  We read the last potential tick value and the
!        first potential directory value..  Note that the last potential
!        stop tick must be greater than the first potential directory
!        record.
!
         from = last - (nck2-1)/100
         to   = from + 1

         call dafgda ( handle, from, to, times )
!
!        If we happen to have a TYPE 01 SPK segment we've just
!        read two consecutive values from the epochs sub-array of the
!        segment. Here's a sketch of why this is so:
!
!          The number of directory records for a CK type 02 segment is
!          (NCK2-1)/100  which is the same as SIZE/1001.
!
!          The number of directory records for an SPK type 01 segment is
!          (NSPK-1)/100  which is the same as SIZE/7201.
!
!          The number of stop ticks for type 02 CK is NCK2 ~ SIZE/10
!
!          The number of epochs for a type 01 SPK is  NSPK ~ SIZE/72
!
!        so NSPK directories < NCK2 directories < NCK2 directories + 1
!        < NSPK + NSPK directories < NCK2.  Consequently, the
!        two values just read are either the last stop tick and the
!        first CK directory value or two consecutive epochs.
!        In the first case TIMES(1) > TIMES(2), in the later case
!        we have TIMES(1) < TIMES(2)
!
         if ( times(1) > times(2) ) then
            ckspk = 'CK'
         else
            ckspk = 'SPK'
         end if

      end if

      return

      end
