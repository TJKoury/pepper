!$Procedure SPKR15 ( Read SPK record from segment, type 15 )

      subroutine spkr15 ( handle, descr, et, record )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Read a single SPK data record from a segment of type 15
!     (Precessing Conic Propagation).
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
      real(wp)              descr (5)
      real(wp)              et
      real(wp)              record(*)

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     DESCR      I   Segment descriptor.
!     ET         I   Target epoch.
!     RECORD     O   Data record.
!
! $ Detailed_Input
!
!     HANDLE,
!     DESCR    are the file handle and segment descriptor for
!              a SPK segment of type 15.
!
!     ET       is a target epoch, for which a data record from
!              a specific segment is required.
!
! $ Detailed_Output
!
!     RECORD   is the record from the specified segment which,
!              when evaluated at epoch ET, will give the state
!              (position and velocity) of some body, relative
!              to some center, in some inertial reference frame.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the segment specified by DESCR is not a type 15 segment,
!         the error SPICE(WRONGSPKTYPE) is signaled.
!
!     2)  A type 15 segment should have exactly 16 values. If this
!         is not the case, the error SPICE(MALFORMEDSEGMENT) is
!         signaled.
!
! $ Files
!
!     See argument HANDLE.
!
! $ Particulars
!
!     This routine reads all of the data from a type 15 SPK segment.
!
!     The structure of the data retrieved in RECORD is:
!
!         RECORD(1)             epoch of the orbit elements at periapse
!                               in ephemeris seconds past J2000.
!         RECORD(2)-RECORD(4)   unit trajectory pole vector
!         RECORD(5)-RECORD(7)   unit periapsis vector
!         RECORD(8)             semi-latus rectum---p in the
!                               equation:
!
!                               r = p/(1 + ECC*COS(Nu))
!
!         RECORD(9)             eccentricity
!         RECORD(10)            J2 processing flag describing
!                               what J2 corrections are to be
!                               applied when the orbit is
!                               propagated.
!
!                                Value       Meaning
!                                -----  -----------------------------
!                                1      Regress line of nodes only.
!                                2      Precess line of apsides only.
!                                3      Don't use J2 corrections.
!                                Other  Regress line of nodes
!                                       and precess line of apsides.
!
!         RECORD(11)-RECORD(13) unit central body pole vector
!         RECORD(14)            central body GM
!         RECORD(15)            central body J2
!         RECORD(16)            central body radius
!
!     Except for J2, units are radians, km, seconds.
!
! $ Examples
!
!     The data returned by the SPKRnn routine is in its rawest form,
!     taken directly from the segment. As such, it will be meaningless
!     to a user unless he/she understands the structure of the data type
!     completely. Given that understanding, however, the SPKRnn
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
!           IF ( TYPE .EQ. 15 ) THEN
!              CALL SPKR15 ( HANDLE, DESCR, ET, RECORD )
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
!     J. Diaz del Rio    (ODC Space)
!     S. Schlaifer       (JPL)
!     W.L. Taber         (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.1, 27-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW)
!
!        Replaced DAFRDA call with DAFGDA.
!        Added IMPLICIT NONE.
!
!-    SPICELIB Version 1.0.0, 15-NOV-1994 (WLT) (SS)
!
!-&


! $ Index_Entries
!
!     read record from type_15 SPK segment
!
!-&


!
!     SPICELIB Functions
!
      logical               return

!
!     Local Variables
!
      integer(ip)           nd
      parameter           ( nd = 2 )

      integer(ip)           ni
      parameter           ( ni = 6 )

!
!     The difference between the first and last address of a type 15
!     segment should be 15.
!
      integer(ip)           diff
      parameter           ( diff = 15 )

      real(wp)              dc  ( nd )
      integer(ip)           ic  ( ni )

      integer(ip)           begin
      integer(ip)           end
      integer(ip)           type

!
!     Standard Spice Error Handling.
!
      if (return() ) return

!
!     Unpack the segment descriptor.
!
      call dafus( descr, nd, ni, dc, ic )

      type  = ic(4)
      begin = ic(5)
      end   = ic(6)
!
!     Make sure that this really is a type 15 data segment.
!
      if (type /= 15) then

         call setmsg ('You are attempting to locate type 15 ' // &
                      'data in a type # data segment.')
         call errint ('#', type                       )
         call sigerr ('SPICE(WRONGSPKTYPE)'           )
         return

      end if

!
!     Since it doesn't cost much we make sure that the segment has
!     the correct amount of data.
!
      if ( end - begin /= diff ) then

         call setmsg ( 'A type 15 segment should contain exactly ' &
         //            '16 double precision values.  The segment ' &
         //            'supplied had #.  The segment is badly ' &
         //            'formed. ' )

         call errint ( '#',   end-begin+1         )
         call sigerr ( 'SPICE(MALFORMEDSEGMENT)'  )
         return

      end if

!
!     Read the data for the record.
!
      call dafgda ( handle, begin, end, record )
      return
      end
