!$Procedure      ZZGFDIQ ( GF, return distance between objects )

      subroutine zzgfdiq ( targid, et, abcorr, obsid, dist )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Return the distance between two ephemeris objects, optionally
!     corrected for light time and stellar aberration.
!
! $ Required_Reading
!
!     GF
!     NAIF_IDS
!     SPK
!     TIME
!
! $ Keywords
!
!     DISTANCE
!     EPHEMERIS
!     GEOMETRY
!     SEARCH
!
! $ Declarations

      implicit none

      integer               targid
      double precision      et
      character*(*)         abcorr
      integer               obsid
      double precision      dist

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     TARGID     I   Target body.
!     ET         I   Observer epoch.
!     ABCORR     I   Aberration correction flag.
!     OBSID      I   Observing body.
!     DIST       O   Distance between target and observer.
!
! $ Detailed_Input
!
!     TARGID      is the NAIF ID code for a target body. The target and
!                 observer define a position vector that points from
!                 the observer to the target.
!
!     ET          is the ephemeris time, expressed as seconds past
!                 J2000 TDB, at which the position of the target body
!                 relative to the observer is to be computed. ET refers
!                 to time at the observer's location.
!
!     ABCORR      indicates the aberration corrections to be applied to
!                 the position of the target body to account for
!                 one-way light time and stellar aberration. Any
!                 aberration correction accepted by SPKEZR may be used.
!
! $ Detailed_Output
!
!     DIST        is the norm (magnitude) of the specified Cartesian
!                 3-vector representing the position of the target body
!                 relative to the specified observer, where the
!                 position is corrected for the specified aberrations.
!                 The position vector points from the observer's
!                 location at ET to the aberration-corrected location
!                 of the target.
!
!                 Units are km.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) If an error occurs while reading an SPK or other kernel file,
!        the error  will be diagnosed by a routine in the call tree
!        of this routine.
!
! $ Files
!
!     Appropriate kernels must be loaded by the calling program before
!     this routine is called.
!
!     The following data are required:
!
!        - SPK data: ephemeris data for target and observer for the
!          input epoch must be loaded. If aberration corrections are
!          used, the states of target and observer relative to the
!          solar system barycenter must be calculable from the
!          available ephemeris data. Typically ephemeris data are made
!          available by loading one or more SPK files via FURNSH.
!
!        - If non-inertial reference frames are used, then PCK
!          files, frame kernels, C-kernels, and SCLK kernels may be
!          needed.
!
!     In all cases, kernel data are normally loaded once per program
!     run, NOT every time this routine is called.
!
! $ Particulars
!
!     This routine centralizes distance computations performed by
!     entry points in the GF distance utility package ZZGFDIU.
!
! $ Examples
!
!     See the entry point ZZGFDIGQ in ZZGFDIU.
!
! $ Restrictions
!
!     This is a SPICELIB private routine; it should not be called by
!     user applications.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!     L.S. Elson     (JPL)
!     W.L. Taber     (JPL)
!     I.M. Underwood (JPL)
!     E.D. Wright    (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0 05-MAR-2009 (NJB) (LSE) (WLT) (IMU) (EDW)
!
!-&

! $ Index_Entries
!
!     compute the apparent distance between two objects
!
!-&


!
!     SPICELIB functions
!
      double precision      vnorm

      logical               failed
      logical               return

!
!     Local Variables
!
      double precision      lt
      double precision      pos   ( 3 )

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

      call chkin  ( 'ZZGFDIQ' )

!
!     Get the position of the target relative to the observer.
!
      call spkezp ( targid, et, 'J2000', abcorr, obsid, pos, lt )

      if (  failed() ) then
         call chkout (  'ZZGFDIQ' )
         return
      end if


      dist = vnorm ( pos )

      call chkout  ( 'ZZGFDIQ' )
      return
      end
