!$Procedure ZZGFDIU ( Private --- GF, distance utilities )

      subroutine zzgfdiu ( target, abcorr, obsrvr, &
                           udfunc, et,     decres, dist )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     This is the umbrella routine for the entry points used by
!     GFEVNT in order to find distance events.
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

      include 'zzabcorr.inc'

      character*(*)         target
      character*(*)         abcorr
      character*(*)         obsrvr
      double precision      et
      logical               decres
      double precision      dist

! $ Brief_I/O
!
!     VARIABLE  I/O  Entry points
!     --------  ---  --------------------------------------------------
!     TARGID     I   ZZGFDIIN
!     ABCORR     I   ZZGFDIIN
!     OBSID      I   ZZGFDIIN
!     ET         I   ZZGFDIDC, ZZGFDIGQ
!     REF        I   ZZGFDIIN
!     UDFUNC     I   ZZGFDIDC
!     DECRES     O   ZZGFDIDC
!     DIST       O   ZZGFDIGQ
!
! $ Detailed_Input
!
!     See individual entry points.
!
! $ Detailed_Output
!
!     See individual entry points.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     See individual entry points.
!
! $ Files
!
!     Appropriate kernels must be loaded by the calling program before
!     this routine is called.
!
!     The following data are required:
!
!        - SPK data: ephemeris data for target and observer, for the
!          times at which state or positions are computed, must be
!          loaded. If aberration corrections are used, the states of
!          target and observer relative to the solar system barycenter
!          must be calculable from the available ephemeris data.
!          Typically ephemeris data are made available by loading one
!          or more SPK files via FURNSH.
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
!     This is an umbrella for routines required by the GF scalar
!     quantity search algorithm to support searches involving
!     distance constraints.
!
!     The entry points of this routine are:
!
!        ZZGFDIIN   Saves the user-supplied inputs defining the
!                   distance computation to be performed. Initializes
!                   the distance search.
!
!        ZZGFDIDC   Determines whether or not distance is decreasing
!                   at a specified epoch.
!
!        ZZGFDIGQ   Returns the distance between the observer and target
!                   at a specified epoch.
!
! $ Examples
!
!     See GFEVNT.
!
! $ Restrictions
!
!     This is a SPICELIB private routine; it should not be called by
!     user applications.
!
!     ZZGFDIIN must be called prior to use of any of the other
!     entry points.
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
!-    SPICELIB version 2.0.0 18-FEB-2011 (EDW)
!
!        Code edits to implement use of ZZGFRELX.
!        These edits include removal of unneeded routines:
!
!           ZZGFDIUR
!           ZZGFDILT
!
!        and corresponding unused variables.
!
!        Update to header entries.
!
!-    SPICELIB Version 1.0.0 05-MAR-2009 (NJB) (LSE) (WLT) (IMU) (EDW)
!
!-&

! $ Index_Entries
!
!     umbrella routine for finding distance events
!
!-&

      external              udfunc

!
!     SPICELIB functions
!
      double precision      vdot

      logical               failed
      logical               return

!
!     Local parameters
!

!
!     Local Variables
!
      character*(corlen)    svcorr

      double precision      lt
      double precision      state  ( 6 )

      integer               svobs
      integer               svtarg

      logical               attblk ( nabcor )
      logical               found

!
!     Saved Variables
!
      save                  svcorr
      save                  svobs
      save                  svtarg


!
!     This routine should never be called directly.
!
      call chkin  ( 'ZZGFDIU'           )
      call sigerr ( 'SPICE(BOGUSENTRY)' )
      call chkout ( 'ZZGFDIU'           )
      return




!$Procedure  ZZGFDIIN ( Private --- GF, distance initialization )

      entry zzgfdiin ( target, abcorr, obsrvr )

! $ Abstract
!
!     Initialize the GF distance constraint search utilities.
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
!
!      CHARACTER*(*)         TARGET
!      CHARACTER*(*)         ABCORR
!      CHARACTER*(*)         OBSRVR
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     TARGET     I   Target body name.
!     ABCORR     I   Aberration correction specifier.
!     OBSRVR     I   Observer name.
!
! $ Detailed_Input
!
!     TARGET     is the name of a target body. Optionally, you may
!                supply the integer ID code for the object as
!                an integer string. For example both 'MOON' and
!                '301' are legitimate strings that indicate the
!                moon is the target body.
!
!                The target and observer define a position vector
!                which points from the observer to the target.
!
!                Case and leading or trailing blanks are not
!                significant in the string TARGET.
!
!
!     ABCORR     indicates the aberration corrections to be applied
!                when computing the target's position and orientation.
!                Any value accepted by SPKEZR may be used.
!
!                See the header of the SPICE routine SPKEZR for a
!                detailed description of the aberration correction
!                options.
!
!                Case and embedded blanks are not significant in
!                ABCORR.
!
!
!     OBSRVR     is the name of the body from which the occultation is
!                observed. Optionally, you may supply the integer NAIF
!                ID code for the body as a string.
!
!                Case and leading or trailing blanks are not
!                significant in the string OBSRVR.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If name of the target or the observer cannot be translated
!         to a NAIF ID code, the error SPICE(IDCODENOTFOUND) is
!         signaled.
!
!     2)  If  target body coincides with the observer body OBSRVR, the
!         error SPICE(BODIESNOTDISTINCT) will be signaled.
!
!     3)  If the aberration correction string is invalid, the error
!         will be diagnosed by a routine in the call tree of this
!         routine.
!
! $ Files
!
!     See the header of the umbrella routine ZZGFDIU.
!
! $ Particulars
!
!     This routine must be called once before each GF search for
!     distance events.
!
! $ Examples
!
!     See GFEVNT.
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
!-    SPICELIB version 2.0.0 18-FEB-2011 (EDW)
!
!        REFVAL removed from routine argument list due to use
!        of ZZGFRELX to calculate the events.
!
!-    SPICELIB Version 1.0.0 05-MAR-2009 (NJB) (LSE) (WLT) (IMU) (EDW)
!
!-&

! $ Index_Entries
!
!     distance initialization routine
!
!-&

      if ( return() ) then
         return
      end if

      call chkin ( 'ZZGFDIIN' )

!
!     Find NAIF IDs for TARGET and OBSRVR.
!
      call bods2c ( target, svtarg, found )

      if ( .not. found ) then

         call setmsg ( 'The target object, ' &
         //            '''#'', is not a recognized name for an ' &
         //            'ephemeris object. The cause of this ' &
         //            'problem may be that you need an updated ' &
         //            'version of the SPICE Toolkit. '           )
         call errch  ( '#', target                                )
         call sigerr ( 'SPICE(IDCODENOTFOUND)'                    )
         call chkout ( 'ZZGFDIIN'                                 )
         return

      end if


      call bods2c ( obsrvr, svobs, found )

      if ( .not. found ) then

         call setmsg ( 'The observer, ' &
         //            '''#'', is not a recognized name for an ' &
         //            'ephemeris object. The cause of this ' &
         //            'problem may be that you need an updated ' &
         //            'version of the SPICE toolkit. '           )
         call errch  ( '#', obsrvr                                )
         call sigerr ( 'SPICE(IDCODENOTFOUND)'                    )
         call chkout ( 'ZZGFDIIN'                                 )
         return

      end if

!
!     Make sure the observer and target are distinct.
!
      if ( svtarg .eq. svobs ) then

         call setmsg ( 'The observer and target must be ' &
         //            'distinct objects, but are not: ' &
         //            'OBSRVR = #; TARGET = #.'           )
         call errch  ( '#', obsrvr                         )
         call errch  ( '#', target                         )
         call sigerr ( 'SPICE(BODIESNOTDISTINCT)'          )
         call chkout ( 'ZZGFDIIN'                          )
         return

      end if

!
!     Squeeze all blanks out of the aberration correction
!     string; ensure the string is in upper case.
!
      call cmprss ( ' ', 0, abcorr, svcorr )
      call ucase  ( svcorr,         svcorr )

!
!     Check the aberration correction. If SPKEZR can't handle it,
!     neither can we.
!
      call zzvalcor ( svcorr, attblk )

      if ( failed() ) then
         call chkout ( 'ZZGFDIIN' )
         return
      end if

      call chkout ( 'ZZGFDIIN' )
      return




!$Procedure ZZGFDIDC ( Private --- GF, is distance decreasing? )

      entry zzgfdidc ( udfunc, et, decres )

! $ Abstract
!
!     Indicate whether the observer-target distance is decreasing at a
!     specified time.
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
!
!     DOUBLE PRECISION      ET
!     LOGICAL               DECRES
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ET         I   Ephemeris seconds past J2000 TDB.
!     DECRES     O   Flag indicating whether distance is decreasing.
!
! $ Detailed_Input
!
!     ET         is the time, expressed as seconds past J2000 TDB, at
!                which to determine whether or not the distance between
!                the observer and target is decreasing.
!
! $ Detailed_Output
!
!     DECRES     is a logical flag that indicates whether the
!                observer-target distance is decreasing at ET. The
!                observer, target, and aberration correction used to
!                compute the distance are defined by the latest call to
!                the initialization entry point ZZGFDIIN.
!
!                DECRES is .TRUE. if and only if the observer-target
!                distance is decreasing at ET.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) If the state of the target relative to the observer
!        at ET can not be found due to an SPK lookup failure,
!        the error will be diagnosed by routines in the call
!        tree of this routine.
!
! $ Files
!
!     See the header of the umbrella routine ZZGFDIU.
!
! $ Particulars
!
!     A function f(x) is strictly decreasing at x0 if and only if there
!     exists some delta > 0 such that for all dx satisfying
!
!        0  <  dx  < delta
!
!     we have
!
!        f(x0)       <  f(x0 + dx)
!
!     and
!
!        f(x0 - dx)  <  f(x)
!
!     Note that a strictly decreasing function need not be
!     differentiable in a neighborhood of x0; it can have jump
!     discontinuities in any neighborhood of x0 and even at x0.
!
! $ Examples
!
!     See GFREL.
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
!-    SPICELIB version 2.0.0 18-FEB-2011 (EDW)
!
!        Added UDFUNC to argument list for use of ZZGFRELX when
!        calculating the events.
!
!-    SPICELIB Version 1.0.0 05-MAR-2009 (NJB) (LSE) (WLT) (IMU) (EDW)
!
!-&

! $ Index_Entries
!
!     indicate whether distance is decreasing
!
!-&


!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

      call chkin ( 'ZZGFDIDC' )

      call spkez ( svtarg, et, 'J2000', svcorr, svobs, state, lt )

      if (  failed() ) then
          call chkout ( 'ZZGFDIDC' )
          return
      end if

!
!     The observer-target distance is decreasing if and only
!     if the dot product of the velocity and position is
!     negative.
!
      decres  =  vdot( state, state(4) )  .lt.  0.d0

      call chkout  ( 'ZZGFDIDC' )
      return




!$Procedure ZZGFDIGQ ( Private --- GF, get observer-target distance )

      entry zzgfdigq ( et, dist )

! $ Abstract
!
!     Return the distance between the target and observer
!     at a specified epoch.
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
!
!      DOUBLE PRECISION      ET
!      DOUBLE PRECISION      DIST
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ET         I   Ephemeris seconds past J2000 TDB.
!     DIST       O   Distance at time ET.
!
! $ Detailed_Input
!
!     ET         is the time, expressed as seconds past J2000 TDB, at
!                which the distance between the observer and target is
!                to be computed.
!
! $ Detailed_Output
!
!     DIST       is the distance between the observer and target as
!                seen by the observer at time ET. The observer, target,
!                and aberration correction used to compute the distance
!                are defined by the latest call to the initialization
!                entry point ZZGFDIIN.
!
!                Units are km.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) If the position of the target relative to the observer
!        at ET can not be found due to an SPK lookup failure,
!        the error will be diagnosed by routines in the call
!        tree of this routine.
!
! $ Files
!
!     See the header of the umbrella routine ZZGFDIU.
!
! $ Particulars
!
!     This routine determines the apparent distance between the target
!     and observer as seen from the observer at time ET. This
!     functionality supports GFREL's comparisons of relative extrema in
!     order to determine absolute extrema.
!
! $ Examples
!
!     See GFREL.
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
!     return distance between two bodies
!
!-&

      if ( return() ) then
         return
      end if

      call chkin ( 'ZZGFDIGQ' )

      call zzgfdiq ( svtarg, et, svcorr, svobs, dist )

      call chkout ( 'ZZGFDIGQ' )
      return

      end


