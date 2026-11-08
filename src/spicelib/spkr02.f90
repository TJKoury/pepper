!$Procedure SPKR02 ( SPK, read record from segment, type 2 )

      subroutine spkr02 ( handle, descr, et, record )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Read a single SPK data record from a segment of type 2
!     (Chebyshev, position only).
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
!     DESCR    are the file handle and segment descriptor for
!              an SPK segment of type 2.
!
!     ET       is an epoch for which a data record from the
!              specified segment is required. ET is expressed as
!              seconds past J2000 TDB.
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
!                 | Coeffs for X position component      |
!                 +--------------------------------------+
!                 | Coeffs for Y position component      |
!                 +--------------------------------------+
!                 | Coeffs for Z position component      |
!                 +--------------------------------------+
!
!              In the above record
!
!                 - Times are expressed as seconds past J2000 TDB.
!                 - Position components have units of km.
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
!     structure of a data type 2 (Chebyshev polynomials, position
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
!           IF ( TYPE .EQ. 2 ) THEN
!              CALL SPKR02 ( HANDLE, DESCR, ET, RECORD )
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
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.2, 14-APR-2021 (JDR)
!
!        Edited the header to comply with NAIF standard. Moved SPK
!        required reading from $Literature_References to
!        $Required_Reading section.
!
!-    SPICELIB Version 1.1.1, 18-JAN-2014 (NJB)
!
!        Enhanced header and in-line documentation.
!
!-    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW)
!
!        Replaced DAFRDA call with DAFGDA.
!        Added IMPLICIT NONE.
!
!-    SPICELIB Version 1.0.3, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.2, 23-AUG-1991 (HAN)
!
!        SPK02 was removed from the $Required_Reading section of the
!        header. The information in the SPK02 Required Reading file
!        is now part of the SPK Required Reading file.
!
!-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
!
!        Literature references added to the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
!
!-&

! $ Index_Entries
!
!     read record from type_2 SPK segment
!
!-&

!
!     SPICELIB functions
!
      logical               return

!
!     Local variables
!

      real(wp)              dc       (   2 )
      real(wp)              init
      real(wp)              intlen

      integer(ip)           ic       (   6 )
      integer(ip)           begin
      integer(ip)           end
      integer(ip)           recsiz
      integer(ip)           nrec
      integer(ip)           recno
      integer(ip)           recadr

!
!     Standard SPICE error handling.
!
      if ( return () ) return

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
!     the initial time of the first record's coverage interval, and the
!     length of the interval covered by each record. These constants
!     are located at the end of the segment, along with the size of
!     each logical record and the total number of records.
!
      call dafgda ( handle, end-3, end, record )

      init   = record( 1 )
      intlen = record( 2 )
      recsiz = int( record( 3 ) )
      nrec   = int( record( 4 ) )

      recno = int( (et - init) / intlen ) + 1
      recno = min( recno, nrec )

!
!     Compute the address of the desired record.
!
      recadr = ( recno - 1 )*recsiz + begin

!
!     Along with the record, return the size of the record.
!
      record( 1 ) = record( 3 )
      call dafgda ( handle, recadr, recadr + recsiz - 1, record( 2 ) )

      return
      end
