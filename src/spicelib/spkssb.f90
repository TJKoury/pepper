!$Procedure SPKSSB ( S/P Kernel, solar system barycenter )

      subroutine spkssb ( targ, et, ref, starg )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Return the state (position and velocity) of a target body
!     relative to the solar system barycenter.
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

      integer(ip)           targ
      real(wp)              et
      character(*)          ref
      real(wp)              starg    ( 6 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     TARG       I   Target body.
!     ET         I   Target epoch.
!     REF        I   Target reference frame.
!     STARG      O   State of target.
!
! $ Detailed_Input
!
!     TARG     is the standard NAIF ID code for a target body.
!
!     ET       is the epoch (ephemeris time) at which the state of the
!              target body is to be computed.
!
!     REF      is the name of the reference frame relative to which the
!              output state vector should be expressed. This may be any
!              frame supported by the SPICELIB frame system, including
!              dynamic and other non-inertial frames.
!
! $ Detailed_Output
!
!     STARG    is a Cartesian state vector representing the position and
!              velocity of the target body, relative to the solar system
!              barycenter, at epoch ET. This vector is rotated into the
!              specified reference frame. Units are always km and
!              km/sec.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If sufficient information has not been "loaded" via the
!         routine FURNSH, SPKLEF or the PCK kernel loaders, an error is
!         signaled by a routine in the call tree of this routine.
!
! $ Files
!
!     See $Restrictions.
!
! $ Particulars
!
!     In order to compute the state of one body relative to another,
!     the states of the two bodies must be known relative to a third
!     body. One simple solution is to use the solar system barycenter
!     as the third body.
!
!     Ephemeris data from more than one segment may be required
!     to determine the state of a body relative to the barycenter.
!     SPKSSB reads as many segments as necessary, from as many
!     files as necessary, using files that have been loaded by
!     previous calls to FURNSH or SPKLEF (load ephemeris file).
!
! $ Examples
!
!     In the following code fragment, SPKSSB is used to display
!     the distance from Earth (Body 399) to Mars (body 499) at
!     a series of epochs.
!
!        CALL SPKLEF ( 'DE125.SPK', HANDLE )
!         .
!         .
!
!        EARTH = 399
!        MARS  = 499
!
!        DO WHILE ( EPOCH .LE. END )
!           CALL SPKSSB ( EARTH, EPOCH, 'J2000', SEARTH )
!           CALL SPKSSB ( MARS,  EPOCH, 'J2000', SMARS  )
!
!           CALL VSUB ( SMARS, SEARTH, SMARS )
!           WRITE (*,*) EPOCH, VNORM ( SMARS )
!
!           EPOCH = EPOCH + DELTA
!        END DO
!
! $ Restrictions
!
!     1)  The ephemeris files to be used by SPKSSB must be loaded
!         by FURNSH or SPKLEF before SPKSSB is called.
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
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.0.4, 13-APR-2021 (JDR)
!
!        Edited the header to comply with NAIF standard. Updated
!        detailed description of arguments REF and STARG.
!
!-    SPICELIB Version 2.0.3, 18-MAY-2010 (BVS)
!
!        Removed "C$" marker from text in the header.
!
!-    SPICELIB Version 2.0.2, 20-NOV-2004 (NJB)
!
!        Updated description of input argument REF to indicate all
!        frames supported by SPICELIB are allowed.
!
!-    SPICELIB Version 2.0.1, 24-JUN-1999 (WLT)
!
!        Corrected code in $Examples section of the headers
!
!-    SPICELIB Version 2.0.0, 19-SEP-1995 (WLT)
!
!        The routine was simplified by replacing all of the
!        main body of code with a call to SPKGEO. By making
!        this change the routine now supports non-inertial frames.
!
!-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
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
!     state relative to solar system barycenter
!
!-&

! $ Revisions
!
!-    SPICELIB Version 2.0.0, 19-SEP-1995 (WLT)
!
!        The routine was simplified by replacing all of the
!        main body of code with a call to SPKGEO. By making
!        this change the routine now supports non-inertial frames.
!
!-&

!
!     SPICELIB functions
!
      logical               return

!
!     Local variables
!
      real(wp)              lt
      integer(ip)           bary

!
!     Standard SPICE error handling.
!
      if ( return () ) return

      bary = 0

      call spkgeo ( targ, et, ref, bary, starg, lt )

      return
      end
