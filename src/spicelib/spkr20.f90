!$Procedure SPKR20 ( SPK, read record from segment, type 20 )

      subroutine spkr20 ( handle, descr, et, record )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Read a single SPK data record from a segment of type 20
!     (Chebyshev, velocity coefficients only).
!
! $ Required_Reading
!
!     SPK
!
! $ Keywords
!
!     EPHEMERIS
!
! $ Declarations

      implicit none

      integer(ip)           handle
      real(wp)              descr    ( 5 )
      real(wp)              et
      real(wp)              record   ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     DESCR      I   Segment descriptor.
!     ET         I   Evaluation epoch.
!     RECORD     O   Data record.
!
! $ Detailed_Input
!
!     HANDLE,
!     DESCR    are the file handle and segment descriptor for an SPK
!              segment of type 20.
!
!     ET       is an epoch for which a data record from the specified
!              segment is required. ET is expressed as seconds past
!              J2000 TDB.
!
! $ Detailed_Output
!
!     RECORD   is an array of data from the specified segment which,
!              when evaluated at epoch ET, will give the state
!              (position and velocity) of the target body identified
!              by the input segment descriptor. The descriptor
!              specifies the center of motion and reference frame of
!              the state.
!
!              The structure of the record is as follows:
!
!                 +--------------------------------------+
!                 | record size (excluding this element) |
!                 +--------------------------------------+
!                 | Coverage interval midpoint           |
!                 +--------------------------------------+
!                 | Coverage interval radius             |
!                 +--------------------------------------+
!                 | Coeffs for X velocity component      |
!                 +--------------------------------------+
!                 | Coeffs for Y velocity component      |
!                 +--------------------------------------+
!                 | Coeffs for Z velocity component      |
!                 +--------------------------------------+
!                 | X position component                 |
!                 +--------------------------------------+
!                 | Y position component                 |
!                 +--------------------------------------+
!                 | Z position component                 |
!                 +--------------------------------------+
!
!              In the above record
!
!                 - Times are expressed as seconds past J2000 TDB.
!                 - Position components have units of km.
!                 - Velocity coefficients have units of km/s.
!
!              RECORD must be declared by the caller with size large
!              enough to accommodate the largest record that can be
!              returned by this routine. See the INCLUDE file
!              spkrec.inc for the correct record length.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If an error occurs while looking up SPK data, the error is
!         signaled by a routine in the call tree of this routine.
!
! $ Files
!
!     See argument HANDLE.
!
! $ Particulars
!
!     See the SPK Required Reading file for a description of the
!     structure of a data type 20 (Chebyshev polynomials, velocity
!     only) segment.
!
! $ Examples
!
!     The data returned by the SPKRnn routine is in its rawest form,
!     taken directly from the segment. As such, it will be meaningless
!     to a user unless he/she understands the structure of the data type
!     completely. Given that understanding, however, the SPKRxx
!     routines might be used to "dump" and check segment data for a
!     particular epoch.
!
!
!     C
!     C     Get a segment applicable to a specified body and epoch.
!     C
!           CALL SPKSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND )
!
!     C
!     C     Look at parts of the descriptor.
!     C
!           CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
!           CENTER = ICD( 2 )
!           REF    = ICD( 3 )
!           TYPE   = ICD( 4 )
!
!           IF ( TYPE .EQ. 20 ) THEN
!              CALL SPKR20 ( HANDLE, DESCR, ET, RECORD )
!                  .
!                  .  Look at the RECORD data.
!                  .
!           END IF
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
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 14-APR-2021 (JDR)
!
!        Edited the header to comply with NAIF standard. Moved SPK
!        required reading from $Literature_References to
!        $Required_Reading section.
!
!-    SPICELIB Version 1.0.0, 17-JAN-2014 (NJB) (IMU)
!
!-&


! $ Index_Entries
!
!     read record from type_20 SPK segment
!
!-&


!
!     SPICELIB functions
!
      real(wp)              j2000
      real(wp)              spd

      logical               return

!
!     Local variables
!
      real(wp)              dc       ( 2 )
      real(wp)              dscale
      real(wp)              init
      real(wp)              initfr
      real(wp)              initjd
      real(wp)              intlen
      real(wp)              intrvl
      real(wp)              mid
      real(wp)              pos      ( 3 )
      real(wp)              radius
      real(wp)              recbeg
      real(wp)              tscale

      integer(ip)           begin
      integer(ip)           end
      integer(ip)           i
      integer(ip)           ic       ( 6 )
      integer(ip)           loc
      integer(ip)           nrec
      integer(ip)           nterms
      integer(ip)           recadr
      integer(ip)           recno
      integer(ip)           recsiz
      integer(ip)           size

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

      call chkin ( 'SPKR20' )
!
!     Unpack the segment descriptor.
!
      call dafus ( descr, 2, 6, dc, ic )

      begin = ic( 5 )
      end   = ic( 6 )

!
!     The segment is made up of a number of logical records, each
!     having the same size, and covering the same length of time.
!
!     We can determine which record to return using the input epoch,
!     the integer and fractional parts of the initial time of the first
!     record's coverage interval, and the length of the interval
!     covered by each record. These constants are located at the end of
!     the segment, along with the size of each logical record and the
!     total number of records.
!
!     For convenience, we'll fetch the segment's distance and time
!     scales in the same call.
!
      call dafgda ( handle, end-6, end, record )

      dscale = record( 1 )
      tscale = record( 2 )
      initjd = record( 3 )
      initfr = record( 4 )
      intlen = record( 5 )
      recsiz = int( record( 6 ) )
      nrec   = int( record( 7 ) )

!
!     NTERMS is the number of velocity coefficients per component,
!     plus 1 (for the position component).
!
      nterms = recsiz / 3

!
!     Convert the initial epoch and interval length to
!     seconds past J2000 TDB.
!
      init   = (  ( initjd - j2000() )  +  initfr  ) * spd()
      intrvl = intlen * spd()

!
!     Locate the record containing the coefficients to use.
!
      recno  = int( (et - init) / intrvl ) + 1
      recno  = max( 1,  min( recno, nrec )  )

!
!     Compute the midpoint and radius of the record at
!     index RECNO. We want to compute the midpoint in such
!     a way that we take advantage of interval lengths that
!     are exactly representable, when we have them.
!
!     RECBEG is the record start time, minus the fractional
!     part of the segment start time, expressed as seconds
!     past J2000. We'll account for the fractional part of the
!     start time below when we compute MID.
!
      recbeg = (  ( initjd - j2000() )  +  (recno-1)*intlen  ) * spd()

      radius = intrvl / 2.0_wp

      mid    = recbeg + ( initfr * spd() ) + radius

!
!     Compute the address of the desired record.
!
      recadr = ( recno - 1 )*recsiz + begin

!
!     Along with the record, return the size, midpoint, and
!     radius of the record.
!
      record( 1 ) = real( recsiz + 2, wp )
      record( 2 ) = mid
      record( 3 ) = radius
      call dafgda ( handle, recadr, recadr + recsiz - 1, record( 4 ) )

!
!     We're going to re-arrange the record: the position components
!     will be transferred to the end of the record, and the record
!     contents will be left-shifted to fill in the free elements.
!
      do i = 1, 3
         pos( i ) = record ( 3  +  i*nterms )
      end do


      size = recsiz + 3
!
!     Remove the position elements from the record.
!
      do i = 1, 3
!
!        LOC is the index of the element to delete. After the first
!        removal, we must account for the resulting left shift when
!        calculating the indices of subsequent elements to be removed.
!
         loc = 3 + i*nterms - (i-1)

         call remlad ( 1, loc, record, size )
!
!        Note that SIZE is an in-out argument; on output it indicates
!        the size of the array after removal of the indicated
!        element(s).
!
      end do

!
!     Convert the position vector to km.
!
      call vsclip ( dscale, pos )

!
!     Append the position to the record. Since we inserted three
!     elements at the start of the record and deleted three position
!     elements, the target index is the same as if we had copied the
!     record directly to the output array.
!
      call moved ( pos, 3, record(recsiz+1) )

!
!     Convert the velocity Chebyshev coefficients to units of km/s.
!
      do i = 4, recsiz
         record(i) = record(i) * (dscale/tscale)
      end do

      call chkout ( 'SPKR20' )
      return
      end
