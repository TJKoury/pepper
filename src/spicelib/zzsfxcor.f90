!$Procedure ZZSFXCOR ( Ray-surface intercept core algorithm )

      subroutine zzsfxcor ( udnear, udmaxr, udrayx, trgcde, &
                            et,     abcorr, uselt,  usecn, &
                            usestl, xmit,   fixref, obscde, &
                            dfrcde, dclass, dcentr, dvec, &
                            spoint, trgepc, srfvec, found  )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Find the intersection of a ray and the surface described by
!     an ellipsoid or DSK data, where the ray's vertex and the
!     target body's center are associated with ephemeris objects.
!     Use specified aberration corrections. Use callback routines
!     to carry out low-level ray-surface intercept and ray-surface
!     near point computations.
!
! $ Required_Reading
!
!     DSK
!
! $ Keywords
!
!     GEOMETRY
!     INTERCEPT
!     INTERSECTION
!     RAY
!     SURFACE
!     TOPOGRAPHY
!
! $ Declarations

      implicit none

      include 'frmtyp.inc'
      include 'zzabcorr.inc'

      external              udnear
      external              udmaxr
      external              udrayx
      integer(ip)           trgcde
      real(wp)              et
      character(*)          abcorr
      logical               uselt
      logical               usecn
      logical               usestl
      logical               xmit
      character(*)          fixref
      integer(ip)           obscde
      integer(ip)           dfrcde
      integer(ip)           dclass
      integer(ip)           dcentr
      real(wp)              dvec    ( 3 )
      real(wp)              spoint  ( 3 )
      real(wp)              trgepc
      real(wp)              srfvec  ( 3 )
      logical               found

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     UDNEAR     I   Callback routine for ray-surface near point.
!     UDMAXR     I   Callback routine for target outer bounding surface.
!     UDRAYX     I   Callback routine for ray-surface intercept.
!     TRGCDE     I   Target body ID code.
!     ET         I   Epoch at observer.
!     ABCORR     I   Aberration correction string.
!     USELT      I   "Use light time" flag.
!     USECN      I   "Use converged Newtonian correction" flag.
!     USESTL     I   "Use stellar aberration" flag.
!     XMIT       I   "Perform transmission corrections" flag.
!     FIXREF     I   Name of target body-fixed frame.
!     OBSCDE     I   Observer body ID code.
!     DFRCDE     I   Ray direction frame ID code.
!     DCLASS     I   Ray direction frame class.
!     DCENTR     I   Ray direction frame center body ID code.
!     DVEC       I   Ray direction vector.
!     SPOINT     O   Surface intercept, if found.
!     TRGEPC     O   Target epoch, if intercept was found.
!     SRFVEC     O   Observer-to-intercept vector, if found.
!     FOUND      O   Found flag.
!
! $ Detailed_Input
!
!     UDNEAR     is an external routine that computes the nearest point
!                on a bounding surface to the input ray. The caller
!                must perform any initialization required by UDNEAR.
!
!                The calling sequence of UDNEAR is
!
!                    CALL UDNEAR ( VERTEX, RAYDIR, ET, PNEAR, DIST )
!
!                    DOUBLE PRECISION      VERTEX(3)
!                    DOUBLE PRECISION      RAYDIR(3)
!                    DOUBLE PRECISION      ET
!                    DOUBLE PRECISION      PNEAR (3)
!                    DOUBLE PRECISION      DIST
!
!     UDMAXR     is an external routine that computes the radius of an
!                outer bounding surface for the target body. The caller
!                must perform any initialization required by UDMAXR.
!
!                The calling sequence of UDMAXR is
!
!                   CALL UDMAXR ( MAXRAD )
!
!                   DOUBLE PRECISION      MAXRAD
!
!     UDRAYX     external routine that computes the intercept of the
!                input ray on the surface associated with the target
!                body. The surface may be represented by an ellipsoid
!                or by DSK data. The caller must perform any
!                initialization required by UDRAYX.
!
!                The calling sequence of UDRAYX is
!
!                    CALL UDRAYX ( VERTEX, RAYDIR, ET, SPOINT, FOUND )
!
!                    DOUBLE PRECISION      VERTEX(3)
!                    DOUBLE PRECISION      RAYDIR(3)
!                    DOUBLE PRECISION      ET
!                    DOUBLE PRECISION      SPOINT(3)
!                    LOGICAL               FOUND
!
!     TRGCDE     is the body ID of the target.
!
!     ET         is the epoch of the intersection computation,
!                expressed as seconds past J2000 TDB. This epoch
!                applies to the observer.
!
!     ABCORR     is an aberration correction string. This string
!                is present in addition to the following flags
!                because it's used as an input to SPKEZP.
!
!     USELT      is a logical flag that is .TRUE. if and only if
!                light time corrections are to be performed.
!
!     USECN      is a logical flag that is .TRUE. if and only if
!                converged Newtonian light time corrections are to be
!                performed.
!
!     USESTL     is a logical flag that is .TRUE. if and only if
!                stellar aberration corrections are to be performed.
!
!     XMIT       is a logical flag that is .TRUE. if and only if
!                the aberration corrections are to be performed for
!                the transmission case.
!
!     FIXREF     is the name of a reference frame fixed to, and
!                centered on, the target body.
!
!     OBSCDE     is the body ID code of the observer.
!
!     DFRCDE     is the frame ID code of the frame in which the ray's
!                direction vector is expressed. This may be any frame
!                supported by the SPICE system, including built-in
!                frames (documented in the Frames Required Reading) and
!                frames defined by a loaded frame kernel (FK). The
!                string DREF is case-insensitive, and leading and
!                trailing blanks in DREF are not significant.
!
!                When DRFCDE designates a non-inertial frame, the
!                orientation of the frame is evaluated at an epoch
!                dependent on the frame's center and, if the center is
!                not the observer, on the selected aberration
!                correction. See the description of the direction
!                vector DVEC for details.
!
!     DCLASS     is the frame class of the frame designated by DFRCDE.
!
!     DCENTR     is the body ID code of the center of the frame
!                designated by DFRCDE.
!
!     DVEC       Ray direction vector emanating from the observer. The
!                intercept with the target body's surface of the ray
!                defined by the observer and DVEC is sought.
!
!                DVEC is specified relative to the reference frame
!                designated by DREF.
!
!                Non-inertial reference frames are treated as follows:
!                if the center of the frame is at the observer's
!                location, the frame is evaluated at ET. If the frame's
!                center is located elsewhere, then letting LTCENT be
!                the one-way light time between the observer and the
!                central body associated with the frame, the
!                orientation of the frame is evaluated at ET-LTCENT,
!                ET+LTCENT, or ET depending on whether the requested
!                aberration correction is, respectively, for received
!                radiation, transmitted radiation, or is omitted.
!                LTCENT is computed using the method indicated by
!                ABCORR.
!
! $ Detailed_Output
!
!     SPOINT      is the surface intercept point on the target body of
!                 the ray defined by the observer and the direction
!                 vector. If the ray intersects the target body in
!                 multiple points, the selected intersection point is
!                 the one closest to the observer. The output argument
!                 FOUND (see below) indicates whether an intercept was
!                 found.
!
!                 SPOINT is expressed in Cartesian coordinates,
!                 relative to the target body-fixed frame designated by
!                 FIXREF. The body-fixed target frame is evaluated at
!                 the intercept epoch TRGEPC (see description below).
!
!                 When light time correction is used, the duration of
!                 light travel between SPOINT to the observer is
!                 considered to be the one way light time. When both
!                 light time and stellar aberration corrections are
!                 used, SPOINT is compute such that, when the vector
!                 from the observer to SPOINT is corrected for light
!                 time and stellar aberration, the resulting vector
!                 lies on the ray defined by the observer's location
!                 and DVEC.
!
!                 The components of SPOINT are given in units of km.
!
!     TRGEPC      is the "intercept epoch." TRGEPC is defined as
!                 follows: letting LT be the one-way light time between
!                 the observer and the intercept point, TRGEPC is the
!                 epoch ET-LT, ET+LT, or ET depending on whether the
!                 requested aberration correction is, respectively, for
!                 received radiation, transmitted radiation, or
!                 omitted. LT is computed using the method indicated by
!                 ABCORR.
!
!                 TRGEPC is expressed as seconds past J2000 TDB.
!
!     SRFVEC      is the vector from the observer's position at ET to
!                 the aberration-corrected (or optionally, geometric)
!                 position of SPOINT, where the aberration corrections
!                 are specified by ABCORR. SRFVEC is expressed in the
!                 target body-fixed reference frame designated by
!                 FIXREF, evaluated at TRGEPC.
!
!                 The components of SRFVEC are given in units of km.
!
!                 One can use the SPICELIB function VNORM to obtain the
!                 distance between the observer and SPOINT:
!
!                    DIST = VNORM ( SRFVEC )
!
!                 The observer's position OBSPOS, relative to the
!                 target body's center, where the center's position is
!                 corrected for aberration effects as indicated by
!                 ABCORR, can be computed via the call:
!
!                    CALL VSUB ( SPOINT, SRFVEC, OBSPOS )
!
!                 To transform the vector SRFVEC from a reference frame
!                 FIXREF at time TRGEPC to a time-dependent reference
!                 frame REF at time ET, the routine PXFRM2 should be
!                 called. Let XFORM be the 3x3 matrix representing the
!                 rotation from the reference frame FIXREF at time
!                 TRGEPC to the reference frame REF at time ET. Then
!                 SRFVEC can be transformed to the result REFVEC as
!                 follows:
!
!                     CALL PXFRM2 ( FIXREF, REF,    TRGEPC, ET, XFORM )
!                     CALL MXV    ( XFORM,  SRFVEC, REFVEC )
!
!                 The second example in the Examples header section
!                 below presents a complete program that demonstrates
!                 this procedure.
!
!     FOUND       A logical flag indicating whether or not the ray
!                 intersects the target. If an intersection exists
!                 FOUND will be returned as .TRUE. If the ray misses
!                 the target, FOUND will be returned as .FALSE.
!
! $ Parameters
!
!     See the include file frmtyp.inc for frame class parameter
!     values.
!
! $ Exceptions
!
!     1)  If the observer-target distance is zero, the error
!         SPICE(NOSEPARATION) is signaled.
!
!     2)  If an error occurs while performing a SPICE kernel lookup, or
!         if an error occurs while performing a geometric computation,
!         the error SHOULD be signaled by a routine in the call tree of
!         this routine. However, see exception (3) below.
!
!     3)  This routine assumes robust input checking has been
!         performed by the caller. This routine may fail in
!         an unspecified manner if such checking has not
!         been performed.
!
! $ Files
!
!     Appropriate kernels must be loaded by the calling program before
!     this routine is called.
!
!     The following data are required:
!
!        - SPK data: ephemeris data for target and observer must be
!          loaded. If aberration corrections are used, the states of
!          target and observer relative to the solar system barycenter
!          must be calculable from the available ephemeris data.
!          Typically ephemeris data are made available by loading one
!          or more SPK files via FURNSH.
!
!        - PCK data: if the computation method is specified as
!          "Ellipsoid," triaxial radii for the target body must be
!          loaded into the kernel pool. Typically this is done by
!          loading a text PCK file via FURNSH.
!
!        - Further PCK data: rotation data for the target body must
!          be loaded. These may be provided in a text or binary PCK
!          file.
!
!     The following data may be required:
!
!        - DSK data: if METHOD indicates that DSK data are to be used,
!          DSK files containing topographic data for the target body
!          must be loaded. If a surface list is specified, data for
!          at least one of the listed surfaces must be loaded.
!
!        - Surface name-ID associations: if surface names are specified
!          in METHOD, the association of these names with their
!          corresponding surface ID codes must be established by
!          assignments of the kernel variables
!
!             NAIF_SURFACE_NAME
!             NAIF_SURFACE_CODE
!             NAIF_SURFACE_BODY
!
!          Normally these associations are made by loading a text
!          kernel containing the necessary assignments. An example
!          of such an assignment is
!
!             NAIF_SURFACE_NAME += 'Mars MEGDR 128 PIXEL/DEG'
!             NAIF_SURFACE_CODE += 1
!             NAIF_SURFACE_BODY += 499
!
!        - Frame data: if a frame definition is required to convert
!          the observer and target states to the body-fixed frame of
!          the target, that definition must be available in the kernel
!          pool. Similarly, the frame definition required to map
!          between the frame designated by DREF and the target
!          body-fixed frame must be available. Typically the
!          definitions of frames not already built-in to SPICE are
!          supplied by loading a frame kernel.
!
!        - CK data: if the frame to which DREF refers is fixed to a
!          spacecraft instrument or structure, at least one CK file
!          will be needed to permit transformation of vectors between
!          that frame and both the J2000 and the target body-fixed
!          frames.
!
!        - SCLK data: if a CK file is needed, an associated SCLK
!          kernel is required to enable conversion between encoded SCLK
!          (used to time-tag CK data) and barycentric dynamical time
!          (TDB).
!
!     In all cases, kernel data are normally loaded once per program
!     run, NOT every time this routine is called.
!
! $ Particulars
!
!     This routine implements the core ray-surface intercept algorithm
!     of the SPICELIB routine SINCPT. Parsing and analysis of inputs
!     is largely performed by the caller of this routine.
!
!     Certain operations performed by this routine, which formerly
!     were performed in-line by SINCPT, are now handled by callback
!     routines. These are:
!
!         - Computing a ray-surface intercept, where the ray's vertex
!           is expressed in a known target body-fixed frame and
!           represents an offset from the target body center (NOT
!           from the frame's center, as is the case in lower-level
!           routines.
!
!         - Computing the radius of an outer bounding surface for
!           the target body.
!
!         - Computing the nearest point to a ray on a specified
!           bounding surface for the target body. This computation
!           enables the intercept-observer light time to be estimated
!           for cases where the first iteration of the solution results
!           in non-intersection geometry.
!
!     By using these callback routines, ZZSFXCOR is able to use a
!     single logic case to compute ray-surface intercepts for targets
!     modeled as ellipsoids or as surfaces represented by topographic
!     data.
!
! $ Examples
!
!     See usage in SINCPT.
!
! $ Restrictions
!
!     1)  This is a private routine. It is meant to be used only by the
!         DSK subsystem.
!
!     2)  This routine relies extensively on the calling routine
!         to check the inputs passed to this routine.
!
!     3)  This routine relies on the calling routine to perform any
!         initialization functions required by the input
!         callback routines.
!
!     4)  If the direction vector DVEC is the zero vector, the error
!         SPICE(ZEROVECTOR) will be signaled.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman    (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.0.0, 14-JUN-2021 (NJB)
!
!        Updated logic for limb grazing geometry to reduce chance of
!        failing to find an intercept.
!
!-    SPICELIB Version 1.0.0, 21-FEB-2017 (NJB)
!
!        Added FAILED tests.
!
!        04-OCT-2016 (NJB)
!
!        Bug fix: initializes FOUND to .FALSE.
!
!        Bug fix: routine now allows observer to be inside
!        outer bounding sphere of target.
!
!        Re-named callback routines again.
!
!        01-JUN-2016 (NJB)
!
!           Updated names of callback routines to avoid overlap with
!           names of actual SPICELIB routines.
!
!        08-FEB-2016 (NJB)
!
!           Based on version 29-JAN-2015 (NJB)
!
!        Version 1.0.1 29-JAN-2015 (NJB)
!
!           Cleaned up debugging comments.
!
!        Version 1.0.0 15-OCT-2014 (NJB)
!
!-&

! $ Index_Entries
!
!     generalized ray-surface intercept
!     ray-surface intercept core algorithm
!
!-&

! $ Revisions
!
!-    SPICELIB Version 2.0.0, 14-JUN-2021 (NJB)
!
!        Updated logic for limb grazing geometry to reduce chance of
!        failing to find an intercept.
!
!        In the main solution loop, if the iteration count has not
!        reached the termination limit minus one, and if a call to
!        UDRAYX does produce an intercept, NPEDLN and NPLNPT are used
!        to find the tangent point. That point is used to produce a new
!        light time estimate. In some cases, this allows an intercept
!        to be found again in a later pass through the loop, and a
!        solution intercept to be found at that point or later.
!-&

!
!     SPICELIB functions
!
      real(wp)              clight
      real(wp)              dasine
      real(wp)              touchd
      real(wp)              vdist
      real(wp)              vnorm
      real(wp)              vsep

      logical               failed
      logical               vzero
      logical               return

!
!     Local parameters
!
      character(*), parameter :: rname = 'ZZSFXCOR'

!
!     This value will become system-dependent when systems
!     using 128-bit d.p. numbers are supported by SPICELIB.
!     CNVLIM, when added to 1.0D0, should yield 1.0D0.
!
      real(wp), parameter :: cnvlim = 1.0e-17_wp

!
!     Round-off error limit for arc sine input:
!
      real(wp), parameter :: rndtol = 1.0e-14_wp

!
!     Fraction of target body angular radius used to define
!     region outside of which rays are immediately rejected
!     as non-intersecting.
!
      real(wp), parameter :: margin = 1.01_wp

      integer(ip), parameter :: maxitr = 10

!
!     Code for the frame J2000.
!
      integer(ip), parameter :: j2code = 1

!
!     Local variables
!
      character(corlen)     loccor
      character(corlen)     prvcor

      real(wp)              dist
      real(wp)              etdiff
      real(wp)              j2dir  ( 3 )
      real(wp)              j2est  ( 3 )
      real(wp)              j2geom ( 3 )
      real(wp)              j2pos  ( 3 )
      real(wp)              j2tmat ( 3, 3 )
      real(wp)              lt
      real(wp)              ltcent
      real(wp)              ltdiff
      real(wp)              maxrad
      real(wp)              negpos ( 3 )
      real(wp)              obspos ( 3 )
      real(wp)              pnear  ( 3 )
      real(wp)              prevet
      real(wp)              prevlt
      real(wp)              r2jmat ( 3, 3 )
      real(wp)              range
      real(wp)              rayalt
      real(wp)              refepc
      real(wp)              reject
      real(wp)              relerr
      real(wp)              rpos   ( 3 )
      real(wp)              s
      real(wp)              srflen
      real(wp)              ssbost ( 6 )
      real(wp)              ssbtst ( 6 )
      real(wp)              stldir ( 3 )
      real(wp)              stlerr ( 3 )
      real(wp)              stltmp ( 3 )
      real(wp)              tpos   ( 3 )
      real(wp)              trgdir ( 3 )
      real(wp)              udir   ( 3 )
      real(wp)              xform  ( 3, 3 )

      integer(ip)           i
      integer(ip)           nitr

      logical               first

!
!     Saved variables
!
      save                  first
      save                  loccor
      save                  prvcor

!
!     Initial values
!
      data                  first  / .true. /
      data                  prvcor / ' '    /

!
!     Standard SPICE error handling.
!
      if ( return() ) return

!
!     Nothing found yet.
!
      found = .false.

!
!     Check for a zero ray direction vector.
!
      if ( vzero(dvec) ) then

         call setmsg ( 'Input ray direction was the zero vector; this ' &
         //            'vector must be non-zero.'                      )
         call sigerr ( 'SPICE(ZEROVECTOR)'                             )
         return

      end if

!
!     Get the sign S prefixing LT in the expression for TRGEPC.
!     When light time correction is not used, setting S = 0
!     allows us to seamlessly set TRGEPC equal to ET.
!
      if ( uselt ) then

         if ( xmit ) then
            s   =  1.0_wp
         else
            s   = -1.0_wp
         end if

      else
         s = 0.0_wp
      end if

      if ( first .or. ( abcorr /= prvcor ) ) then
!
!        Construct aberration correction string without stellar
!        aberration specification.
!
         if ( uselt ) then

            if ( xmit ) then
               loccor = 'X'
            else
               loccor = ' '
            end if

            if ( usecn ) then

               call suffix ( 'CN', 0, loccor )
            else
               call suffix ( 'LT', 0, loccor )
            end if

         else
            loccor = 'NONE'
         end if

         prvcor = abcorr
         first  = .false.

      end if

!
!     Determine the position of the observer in target
!     body-fixed coordinates.
!
!         -  Call SPKEZP to compute the position of the target body as
!            seen from the observing body and the light time (LT)
!            between them. We request that the coordinates of POS be
!            returned relative to the body fixed reference frame
!            associated with the target body, using aberration
!            corrections specified by LOCCOR; these are the corrections
!            the input argument ABCORR, minus the stellar aberration
!            correction if it was called for.
!
!         -  Call VMINUS to negate the direction of the vector (OBSPOS)
!            so it will be the position of the observer as seen from
!            the target body in target body fixed coordinates.
!
!            Note that this result is not the same as the result of
!            calling SPKEZP with the target and observer switched. We
!            computed the vector FROM the observer TO the target in
!            order to get the proper light time and stellar aberration
!            corrections (if requested). Now we need the inverse of
!            that corrected vector in order to compute the intercept
!            point.
!

      call spkezp ( trgcde, et, fixref, loccor, obscde, tpos, lt )

      if ( failed() ) return

!
!     Negate the target's position to obtain the position of the
!     observer relative to the target.
!
      call vminus ( tpos, obspos )

!
!     We now need to convert the direction vector into the
!     body fixed frame associated with the target. The target
!     epoch is dependent on the aberration correction. The
!     coefficient S has been set to give us the correct answer
!     for each case.
!
      trgepc = et  +  s*lt

!
!     Transform the direction vector from frame DREF to the body-fixed
!     frame associated with the target. The epoch TRGEPC associated
!     with the body-fixed frame has been set already.
!
!     We'll compute the transformation in two parts: first
!     from frame DREF to J2000, then from J2000 to the target
!     frame.
!
      if ( dclass == inertl ) then
!
!        Inertial frames can be evaluated at any epoch.
!
         refepc = et

      else if ( .not. uselt ) then
!
!        We're not using light time corrections (converged or
!        otherwise), so there's no time offset.
!
         refepc = et

      else if ( dcentr == obscde ) then
!
!        If the center of frame DREF is the observer (which is
!        usually the case if the observer is a spacecraft), then
!        the epoch of frame DREF is simply ET.
!
!        There's no offset between the center for frame DREF
!        and the observer.
!
         refepc = et

      else
!
!        Find the light time from the observer to the center of
!        frame DREF.
!
         call spkezp ( dcentr, et, 'J2000', abcorr, obscde, &
                       rpos,   ltcent                       )

         if ( failed() ) return

         refepc  =  et  +  s * ltcent

      end if

!
!     The epoch REFEPC associated with frame DREF has been set.
!
!     Compute the transformation from frame DREF to J2000 and the
!     transformation from J2000 to the target body-fixed frame.
!
!     Map DVEC to both the J2000 and target body-fixed frames. We'll
!     store DVEC, expressed relative to the J2000 frame, in the
!     variable J2DIR. DVEC in the target body-fixed frame will be
!     stored in TRGDIR.
!
!     We may need both versions of DVEC: if we use light time
!     correction, we'll update "intercept epoch", and hence the
!     transformation between J2000 and the target body-fixed frame.
!     The transformation between DREF and J2000 doesn't change, on the
!     other hand, so we don't have to recompute J2DIR. We need TRGDIR
!     in all cases.
!
      call refchg ( dfrcde, j2code, refepc, r2jmat )

      if ( failed() ) return

      call mxv ( r2jmat, dvec, j2dir )

!
!     Save this version of J2DIR as J2GEOM. Later we'll
!     modify J2DIR, if necessary, to account for stellar
!     aberration.
!
      call vequ ( j2dir, j2geom )

!
!     Map J2DIR (in the J2000 frame) to the target body-fixed
!     frame.
!
      call pxform ( 'J2000', fixref,  trgepc,  j2tmat )

      if ( failed() ) return

      call mxv ( j2tmat, j2dir, trgdir )

!
!     At this point,
!
!        TRGEPC is set.
!        TRGDIR is set.
!        J2DIR is set.
!
!     Get the J2000-relative state of the observer relative to
!     the solar system barycenter at ET. We'll use this in
!     several places later.
!
      call spkssb ( obscde, et, 'J2000', ssbost )

      if ( failed() ) return

!
!     If we're using stellar aberration correction, at this point we'll
!     account for it. We're going to find a surface point such that
!     the radiation path from that point to the observer, after
!     correction for stellar aberration, is parallel to the ray. So
!     by applying the inverse of the correction to the ray, we obtain
!     the ray with which we must perform our intercept computation.
!
      if ( usestl ) then
!
!        We approximate the inverse stellar aberration correction by
!        using the correction for the reverse transmission direction.
!        If we're in the reception case, we apply the transmission
!        stellar aberration correction to J2DIR and vice versa.
!
!        We iterate our estimates until we have the desired level
!        of convergence or reach the iteration limit.
!
         nitr = 5

         if ( xmit ) then
!
!           Use reception stellar aberration correction
!           routine STELAB to generate a first estimate of
!           the direction vector after stellar aberration
!           has been "removed"---that is, apply the inverse
!           of the transmission stellar aberration correction
!           mapping to J2DIR.
!
            call stelab ( j2dir, ssbost(4), stldir )

!
!           Now improve our estimate.
!
            relerr = 1.0_wp
            i      = 1

            do while ( ( i <= nitr ) .and. ( relerr > cnvlim ) )
!
!              Estimate the error in our previous approximation
!              by applying the reception stellar aberration
!              to STLDIR and finding the difference with J2DIR.
!
               call stlabx ( stldir, ssbost(4), j2est  )
               call vsub   ( j2dir,  j2est,     stlerr )

!
!              Adding the error in the reception mapping to STLDIR
!              will give us an improved estimate of the inverse.
!
               call vadd ( stlerr, stldir, stltmp )
               call vequ ( stltmp,         stldir )

               relerr = vnorm(stlerr) / vnorm(stldir)
               i      = i + 1

            end do

!
!           At this point we've found a good estimate of the
!           direction vector under the inverse of the transmission
!           stellar aberration correction mapping.
!
         else
!
!           Use transmission stellar aberration correction
!           routine STLABX to generate a first estimate of
!           the direction vector after stellar aberration
!           has been "removed."
!
            call stlabx ( j2dir, ssbost(4), stldir )

!
!           Now improve our estimate.
!
            relerr = 1.0_wp
            i      = 1

            do while ( ( i <= nitr ) .and. ( relerr > cnvlim ) )
!
!              Estimate the error in our previous approximation
!              by applying the reception stellar aberration
!              to STLDIR and finding the difference with J2DIR.
!
               call stelab ( stldir, ssbost(4), j2est  )
               call vsub   ( j2dir,  j2est,     stlerr )

!
!              Adding the error in the reception mapping to STLDIR
!              will give us an improved estimate of the inverse.
!
               call vadd ( stlerr, stldir, stltmp )
               call vequ ( stltmp,         stldir )

               relerr = vnorm(stlerr) / vnorm(stldir)
               i      = i + 1

            end do

!
!           At this point we've found a good estimate of the
!           direction vector under the inverse of the reception
!           stellar aberration correction mapping.
!
         end if

!
!        Replace the J2000-relative ray direction with the corrected
!        direction.
!
         call vequ ( stldir,  j2dir )
         call mxv  ( j2tmat,  j2dir,   trgdir )

      end if

!
!     Find the surface intercept point and distance from observer to
!     intercept point using the specified geometric definition.
!
!     Find the surface intercept given the target epoch,
!     observer-target position, and target body orientation we've
!     already computed. If we're not using light time correction, this
!     is all we must do. Otherwise, our result will give us an initial
!     estimate of the target epoch, which we'll then improve.
!
!     Make an easy test to see whether we can quit now because an
!     intercept cannot exist. If the ray is separated from the
!     observer-target center vector by more than (MARGIN * the maximum
!     target radius), we're done. Let REJECT be the angular
!     separation limit.
!
      call udmaxr ( maxrad )

      range = vnorm( obspos )

      if ( range == 0.0_wp ) then
!
!        We've already ensured that observer and target are
!        distinct, so this should be a very unusual occurrence.
!
         call setmsg ( 'Observer-target distance is zero. ' &
         //            'Observer ID is #; target ID is #.' )
         call errint ( '#', obscde                         )
         call errint ( '#', trgcde                         )
         call sigerr ( 'SPICE(NOSEPARATION)'               )
         return

      end if

      if ( range > margin*maxrad ) then
!
!        Compute the arc sine with SPICE error checking.
!
         reject = dasine ( margin * maxrad / range,  rndtol )

         call vminus ( obspos, negpos )

         if (  vsep( negpos, trgdir )  >  reject  ) then
!
!           The angular separation of ray and target is too great
!           for a solution to exist, even with a better light time
!           estimate.
!
            return

         end if

      end if

!
!     Locate the intercept of the ray with the target; if there's no
!     intercept, find the closest point on the target to the ray.
!
      call udrayx ( obspos, trgdir, trgepc, spoint, found )

      if ( failed() ) return

!
!     If we found an intercept, and if we're not using light time
!     corrections, we're almost done now. We still need SRFVEC.
!     SPOINT, TRGEPC, and FOUND have already been set.
!
      if (  found  .and.  ( .not. uselt )  ) then

         call vsub ( spoint, obspos, srfvec )

         return

      end if

!
!     From this point onward, we're dealing with a case calling for
!     light time and possibly stellar aberration corrections.
!
      if ( .not. found ) then
!
!        If there's no intercept, we're probably done. However, we need
!        to guard against the possibility that the ray does intersect
!        the ellipsoid but we haven't discovered it because our first
!        light time estimate was too poor.
!
!        We'll make an improved light time estimate as follows: Find
!        the nearest point on the ellipsoid to the ray. Find the light
!        time between the observer and this point.
!
!        If we're using converged Newtonian corrections, we iterate
!        this procedure up to three times.
!
         if ( usecn ) then
            nitr = 3
         else
            nitr = 1
         end if

         i = 1

         do while (  ( i <= nitr ) .and. ( .not. found )  )

            call udnear ( obspos, trgdir, et, pnear, rayalt )

            if ( failed() ) return

            lt =  vdist ( obspos, pnear ) / clight()

!
!           Use the new light time estimate to repeat the intercept
!           computation.
!
            trgepc  =  et  +  s*lt

!
!           Get the J2000-relative state of the target relative to
!           the solar system barycenter at the target epoch.
!
            call spkssb ( trgcde, trgepc, 'J2000', ssbtst )

            if ( failed() ) return

!
!           Find the position of the observer relative to the target.
!           Convert this vector from the J2000 frame to the target
!           frame at TRGEPC.
!
            call vsub   ( ssbost,  ssbtst, j2pos         )
            call pxform ( 'J2000', fixref, trgepc, xform )

            if ( failed() ) return

!
!           Convert the observer's position relative to the target
!           from the J2000 frame to the target frame at the target
!           epoch.
!
            call mxv ( xform, j2pos, obspos )

!
!           Convert the ray's direction vector from the J2000 frame
!           to the target frame at the target epoch.
!
            call mxv ( xform, j2dir, trgdir )

!
!           Repeat the intercept computation.
!
            call udrayx ( obspos, trgdir, trgepc, spoint, found )

            if ( failed() ) return

            i = i + 1

         end do

!
!        If there's still no intercept, we're done.
!
         if ( .not. found ) return

      end if

!
!     Making it to this point means we've got an intersection, given
!     our current light time estimate. It's possible that a better
!     light time estimate will yield no intersection.
!
!     Since we're using light time corrections, we're going to make
!     an estimate of light time to the intercept point, then re-do
!     our computation of the target position and orientation using
!     the new light time value.
!
      if ( usecn ) then
         nitr = maxitr
      else
         nitr = 1
      end if

!
!     Compute new light time estimate and new target epoch.
!
      dist  =   vdist ( obspos, spoint )
      lt     =  dist / clight()
      trgepc =  et  +  s * lt

      prevlt = 0.0_wp
      prevet = trgepc

      i      = 0
      ltdiff = 1.0_wp
      etdiff = 1.0_wp

      do while (       ( i      <   nitr                ) &
                 .and. ( ltdiff > ( cnvlim * abs(lt) )  ) &
                 .and. ( etdiff >   0.0_wp                )  )

!
!        Get the J2000-relative state of the target relative to
!        the solar system barycenter at the target epoch.
!
         call spkssb ( trgcde, trgepc, 'J2000', ssbtst )

         if ( failed() ) return

!
!        Find the position of the observer relative to the target.
!        Convert this vector from the J2000 frame to the target frame
!        at TRGEPC.
!
!        Note that SSBOST contains the J2000-relative state of the
!        observer relative to the solar system barycenter at ET.
!
         call vsub   ( ssbost,  ssbtst, j2pos         )
         call pxform ( 'J2000', fixref, trgepc, xform )

         if ( failed() ) return

!
!        Convert the observer's position relative to the target from
!        the J2000 frame to the target frame at the target epoch.
!
         call mxv    ( xform,  j2pos, obspos )
         call vminus ( obspos, negpos )

!
!        Convert the ray's direction vector from the J2000 frame
!        to the target frame at the target epoch.
!
         call mxv ( xform, j2dir, trgdir )

!
!        Repeat the intercept computation.
!
         call udrayx ( obspos, trgdir, trgepc, spoint, found )

         if ( failed() ) return

!
!        If there's no intercept, try to get an estimate of the
!        intercept location and the light time by using the nearest
!        point to the ellipsoid on the line containing the ray. This is
!        useful only if the iteration count has not reached its maximum
!        value (the termination value minus one), since the point of
!        this is to make it possible to find an intercept.
!
!        Note that an intercept was already found using the initial
!        aberration corrections, so we can't get to this case unless
!        we have near-intercept geometry.
!
         if ( .not. found ) then

            if ( i < ( nitr-1 ) ) then
!
!              SPOINT is the nearest point to the ellipsoid on the
!              line containing the ray.
!
               call udnear ( obspos, trgdir, et, pnear, rayalt )

               call nplnpt ( obspos, trgdir, pnear, spoint, rayalt )

            else
!
!              We're not going to find an intercept.
!
               return

            end if

         end if

!
!        Compute the distance between intercept and observer.
!
         dist    =   vdist ( obspos, spoint )

!
!        Compute a new light time estimate and a new target epoch.
!
         lt      =  dist / clight()

         trgepc  =  et  +  s * lt

!
!        We use the d.p. identity function TOUCHD to force the compiler
!        to create double precision arguments from the differences
!        LT-PREVLT and TRGEPC-PREVET. Some compilers will perform
!        extended-precision register arithmetic, which can prevent a
!        difference from rounding to zero. Simply storing the result of
!        the subtraction in a double precision variable doesn't solve
!        the problem, because that variable can be optimized out of
!        existence.
!
         ltdiff  =   abs( touchd(lt     - prevlt) )
         etdiff  =   abs( touchd(trgepc - prevet) )
         prevlt  =   lt
         prevet  =   trgepc
         i       =   i + 1

      end do

!
!     FOUND, SPOINT, TRGEPC, and OBSPOS have been set at this point.
!     We need SRFVEC. Since OBSPOS doesn't take into account stellar
!     aberration, we can' derive SRFVEC from OBSPOS as is done in
!     the related routines SUBPNT and SUBSLR. Here, we derive
!     SRFVEC from J2GEOM, which is the input ray direction expressed in
!     the J2000 frame. We use XFORM, which is computed in the loop
!     above, to convert J2GEOM to FIXREF, evaluated at TRGEPC.
!
      call mxv    ( xform, j2geom, udir )
      call vhatip ( udir )

!
!     Let SRFLEN be the length of SRFVEC; we CAN get this
!     length from OBSPOS and SPOINT, since stellar
!     aberration correction (as implemented in SPICE)
!     doesn't change the length of the vector SPOINT-OBSPOS.
!
      srflen = vdist ( spoint, obspos )

!
!     Scale UDIR to obtain the desired value of SRFVEC.
!
      call vscl ( srflen, udir, srfvec )

      return
      end
