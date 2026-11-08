
!$Procedure ZZSPKSB0 ( S/P Kernel, solar system barycenter )

      subroutine zzspksb0 ( targ, et, ref, starg )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
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
      character*(*)         ref
      real(wp)              starg    ( 6 )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     TARG       I   Target body.
!     ET         I   Target epoch.
!     REF        I   Target reference frame.
!     STARG      O   State of target.
!
! $ Detailed_Input
!
!     TARG        is the standard NAIF ID code for a target body.
!
!     ET          is the epoch (ephemeris time) at which the state
!                 of the target body is to be computed.
!
!     REF         is the name of the  reference frame to which the
!                 vectors returned by the routine should be rotated.
!                 This may be any frame supported by the SPICELIB frame
!                 system, including dynamic and other non-inertial
!                 frames.
!
! $ Detailed_Output
!
!     STARG       contains the position and velocity of the target
!                 body, relative to the solar system barycenter,
!                 at epoch ET. These vectors are rotated into the
!                 specified  reference frame. Units are always
!                 km and km/sec.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) If insufficient information has not bee "loaded" via the
!        routine SPKLEF or the PCK kernel loaders, the problem will
!        be diagnosed by a routine in the call tree of this routine.
!
! $ Files
!
!     See: $Restrictions.
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
!     ZZSPKSB0 reads as many segments as necessary, from as many
!     files as necessary, using files that have been loaded by
!     previous calls to SPKLEF (load ephemeris file).
!
! $ Examples
!
!     In the following code fragment, ZZSPKSB0 is used to display
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
!           CALL ZZSPKSB0 ( EARTH, EPOCH, 'J2000', SEARTH )
!           CALL ZZSPKSB0 ( MARS,  EPOCH, 'J2000', SMARS  )
!
!           CALL VSUB ( SMARS, SEARTH, SMARS )
!           WRITE (*,*) EPOCH, VNORM ( SMARS )
!
!           EPOCH = EPOCH + DELTA
!        END DO
!
! $ Restrictions
!
!     1) SPICE Private routine.
!
!     2) The ephemeris files to be used by ZZSPKSB0 must be loaded
!        by SPKLEF before ZZSPKSB0 is called.
!
! $ Literature_References
!
!     NAIF Document 168.0, "S- and P- Kernel (SPK) Specification and
!     User's Guide"
!
! $ Author_and_Institution
!
!     W.L. Taber      (JPL)
!     I.M. Underwood  (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 12-DEC-2004 (NJB)
!
!        Based on SPICELIB Version 2.0.2, 20-NOV-2004  (NJB)
!
!-&

! $ Index_Entries
!
!     state relative to solar system barycenter
!
!-&

! $ Revisions
!
!-    SPICELIB Version 2.0.0, 19-SEP-1995  (WLT)
!
!        The routine was simplified by replacing all of the
!        main body of code with a call to SPKGEO.  By making
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
      if ( return () ) then
         return
      else
         call chkin ( 'ZZSPKSB0' )
      end if

      bary = 0

      call zzspkgo0 ( targ, et, ref, bary, starg, lt )

      call chkout ( 'ZZSPKSB0' )
      return
      end
