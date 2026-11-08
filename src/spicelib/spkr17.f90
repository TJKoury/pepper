!$Procedure SPKR17 ( Read SPK record from segment, type 17 )

      subroutine spkr17 ( handle, descr, et, record )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Read a single SPK data record from a segment of type 17
!     (Equinoctial Elements).
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
!              a SPK segment of type 17.
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
!     1)  If the segment specified by DESCR is not a type 17 segment,
!         the error SPICE(WRONGSPKTYPE) is signaled.
!
!     2)  A type 17 segment should have exactly 16 values. If this
!         is not the case, the error SPICE(MALFORMEDSEGMENT) is
!         signaled.
!
! $ Files
!
!     See argument HANDLE.
!
! $ Particulars
!
!     This routine reads all of the data from a type 17 SPK segment.
!
!     The structure of the data retrieved in RECORD is:
!
!         RECORD(1) is the epoch of the orbit elements at
!                   in ephemeris seconds past J2000.
!
!         RECORD(2) is the semi-major axis (A) of the orbit.
!
!         RECORD(3) is the value of H at the specified epoch.
!                   ( E*SIN(ARGP+NODE) ).
!
!         RECORD(4) is the value of K at the specified epoch
!                   ( E*COS(ARGP+NODE) ).
!
!         RECORD(5) is the mean longitude (MEAN0+ARGP+NODE)at
!                   the epoch of the elements.
!
!         RECORD(6) is the value of P (TAN(INC/2)*SIN(NODE))at
!                   the specified epoch.
!
!         RECORD(7) is the value of Q (TAN(INC/2)*COS(NODE))at
!                        the specified epoch.
!
!         RECORD(8) is the rate of the longitude of periapse
!                   (dARGP/dt + dNODE/dt ) at the epoch of
!                   the elements. This rate is assumed to hold
!                   for all time.
!
!         RECORD(9) is the derivative of the mean longitude
!                   ( dM/dt + dARGP/dt + dNODE/dt ).  This
!                   rate is assumed to be constant.
!
!         RECORD(10) is the rate of the longitude of the ascending
!                    node ( dNODE/dt).
!
!         RECORD(11) Right Ascension of the pole of the
!                    orbital reference system relative to the
!                    reference frame of the associated SPK segment.
!
!         RECORD(12) Declination of the pole of the
!                    orbital reference system relative to the
!                    reference frame of the associated SPK segment.
!
!     Units are km, radians and radians/second.
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
!           IF ( TYPE .EQ. 17 ) THEN
!              CALL SPKR17 ( HANDLE, DESCR, ET, RECORD )
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
!        Edited the header to comply with NAIF standard. Fixed textual
!        description of SPK type in $Abstract.
!
!-    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW)
!
!        Replaced DAFRDA call with DAFGDA.
!        Added IMPLICIT NONE.
!
!-    SPICELIB Version 1.0.0, 03-JAN-1997 (WLT) (SS)
!
!-&


! $ Index_Entries
!
!     read record from type_17 SPK segment
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
!     The difference between the first and last address of a type 17
!     segment should be 11.
!
      integer(ip)           diff
      parameter           ( diff = 11 )

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
!     Make sure that this really is a type 17 data segment.
!
      if (type /= 17) then

         call setmsg ('You are attempting to locate type 17 ' // &
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

         call setmsg ( 'A type 17 segment should contain exactly ' &
         //            '# double precision values.  The segment ' &
         //            'supplied had #.  The segment is badly ' &
         //            'formed. ' )

         call errint ( '#',   end-begin+1         )
         call errint ( '#',   diff+1              )
         call sigerr ( 'SPICE(MALFORMEDSEGMENT)'  )
         return

      end if

!
!     Read the data for the record.
!
      call dafgda ( handle, begin, end, record )
      return
      end
