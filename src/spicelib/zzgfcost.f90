!$Procedure      ZZGFCOST ( GF, coordinate definition state )

      subroutine zzgfcost ( vecdef, method, trgid, et, &
                            ref,    abcorr, obsid, dref, &
                            dctr,   dvec,   radii, state, found )

! $ Abstract
!
!     SPICE private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Return a state vector used to define coordinates referenced in a
!     GF search.
!
! $ Required_Reading
!
!     GF
!     SPK
!     TIME
!     NAIF_IDS
!     FRAMES
!
! $ Keywords
!
!     GEOMETRY
!     PRIVATE
!     SEARCH
!
! $ Declarations

      implicit none

      include               'gf.inc'
      include               'zzgf.inc'

      character*(*)         vecdef
      character*(*)         method
      integer               trgid
      double precision      et
      character*(*)         ref
      character*(*)         abcorr
      integer               obsid
      character*(*)         dref
      integer               dctr
      double precision      dvec   ( 3 )
      double precision      radii  ( 3 )
      double precision      state  ( 6 )
      logical               found

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     VECDEF     I   Vector definition.
!     METHOD     I   Computation method.
!     TRGID      I   Target ID code.
!     ET         I   Computation epoch.
!     REF        I   Reference frame name.
!     ABCORR     I   Aberration correction.
!     OBSID      I   Observer ID code.
!     DREF       I   Reference frame of ray's direction vector.
!     DCTR       I   ID code of ray frame's center.
!     DVEC       I   Ray's direction vector.
!     RADII      I   Radii of reference ellipsoid.
!     STATE      O   State used to define coordinates.
!     FOUND      O   Flag indicating if state was computed.
!
! $ Detailed_Input
!
!
!     VECDEF     States computed by this routine consist of a an
!                underlying vector and the vector's velocity. VECDEF is
!                a short string describing the means by which the
!                vector of interest is defined. Only parameters from
!                the Fortran INCLUDE file zzgf.inc should be used.
!                Parameter names and meanings are:
!
!                   POSDEF               Vector is position of
!                                        target relative to observer.
!
!                   SOBDEF               Vector is sub-observer
!                                        point on target body.  Vector
!                                        points from target body
!                                        center to sub-observer point.
!                                        The target must be an extended
!                                        body modeled as a triaxial
!                                        ellipsoid.
!
!                   SINDEF               Vector is ray-surface intercept
!                                        point on target body. Vector
!                                        points from target body
!                                        center to sub-observer point.
!                                        The target must be an extended
!                                        body modeled as a triaxial
!                                        ellipsoid.
!
!                Case, leading and trailing blanks ARE significant
!                in the string VECDEF.
!
!
!     METHOD     is a string specifying the computational method
!                applicable to the vector of interest. When VECDEF
!                is the parameter
!
!                   SOBDEF
!
!                METHOD should be set to one of the values accepted
!                by the SPICELIB routine SUBPNT.
!
!                When VECDEF is the parameter
!
!                   SINDEF
!
!                METHOD should be set to one of the values accepted
!                by the SPICELIB routine SINCPT.
!
!                METHOD is ignored if VECDEF is set to
!
!                   POSDEF
!
!                Case, leading and trailing blanks are not significant
!                in the string METHOD.
!
!
!     TRGID      is the NAIF ID code of the target object.
!
!
!     ET         is the time, expressed as ephemeris seconds past J2000
!                TDB, at which the specified state is to be computed.
!
!
!     REF        is the name of the reference frame relative to which
!                the state of interest is specified.
!
!                Case, leading and trailing blanks are not significant
!                in the string REF.
!
!
!     ABCORR     indicates the aberration corrections to be applied to
!                the state of the target body to account for one-way
!                light time and stellar aberration.  The orientation
!                of the target body will also be corrected for one-way
!                light time when light time corrections are requested.
!
!                Supported aberration correction options for
!                observation (case where radiation is received by
!                observer at ET) are:
!
!                   NONE           No correction.
!                   LT             Light time only.
!                   LT+S           Light time and stellar aberration.
!                   CN             Converged Newtonian (CN) light time.
!                   CN+S           CN light time and stellar aberration.
!
!                Supported aberration correction options for
!                transmission (case where radiation is emitted from
!                observer at ET) are:
!
!                   XLT            Light time only.
!                   XLT+S          Light time and stellar aberration.
!                   XCN            Converged Newtonian (CN) light time.
!                   XCN+S          CN light time and stellar aberration.
!
!                For detailed information, see the geometry finder
!                required reading, gf.req.  Also see the header of
!                SPKEZR, which contains a detailed discussion of
!                aberration corrections.
!
!                Case, leading and trailing blanks are not significant
!                in the string ABCORR.
!
!
!     OBSID      is the NAIF ID code of the observer.
!
!
!     DREF       is the name of the reference frame relative to which a
!                ray's direction vector is expressed. This may be any
!                frame supported by the SPICE system, including
!                built-in frames (documented in the Frames Required
!                Reading) and frames defined by a loaded frame kernel
!                (FK). The string DREF is case-insensitive, and leading
!                and trailing blanks in DREF are not significant.
!
!                When DREF designates a non-inertial frame, the
!                orientation of the frame is evaluated at an epoch
!                dependent on the frame's center and, if the center is
!                not the observer, on the selected aberration
!                correction. See the description of the direction
!                vector DVEC for details.
!
!
!     DCTR       is the ID code of the object at which the frame
!                designated by DREF is centered. Although DCTR
!                can be derived from DREF, in the interest of
!                efficiency, DCTR is obtained by the caller,
!                normally during search initialization.
!
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
!
!     RADII      is a double precision array containing the three
!                radii of a reference ellipsoid associated with
!                the target body.
!
!                RADII is ignored if the input vector definition
!                is POSDEF; in this case the caller may set the
!                elements of RADII to zero.
!
!
! $ Detailed_Output
!
!     STATE      is the specified state vector, evaluated at the epoch
!                ET. The position component of STATE is the vector
!                defined by VECDEF and the other inputs. The velocity
!                component of STATE is the derivative with respect to
!                time of the position component. Units are km and km/s.
!
!                STATE is defined if and only if the output argument
!                FOUND is set to .TRUE.
!
!
!     FOUND      is a logical flag indicating whether the requested
!                state could be computed. FOUND is set to .FALSE. if
!                and only if the vector definition is SINDEF and either
!
!                - the surface intercept is not found
!
!                - the surface intercept velocity is not computable
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the vector definition VECDEF is not recognized,
!         the error SPICE(NOTSUPPORTED) is signaled.
!
!     2)  If the computation method METHOD is not recognized,
!         the error will be diagnosed by routines in the call tree
!         of this routine.
!
!     3)  If the aberration correction ABCORR is not recognized,
!         the error will be diagnosed by routines in the call tree
!         of this routine.
!
!     4)  If the frame REF is not recognized by the frames subsystem,
!         the error will be diagnosed by routines in the call tree
!         of this routine.
!
!     5)  If VECDEF calls for a computation involving a target surface
!         point and the name and ID code of the frame associated with
!         the target body is not available from the frame subsystem,
!         the error SPICE(NOFRAME) is signaled.
!
!     6)  If VECDEF calls for a computation involving a target surface
!         point and ID codes of target and observer can't be converted
!         to names, the error  will be diagnosed by routines in the
!         call tree of this routine.
!
!     7)  If ephemeris data are required but not available to compute
!         the state of the target, the coordinate frame REF's center,
!         or the input ray's frame DREF's center relative to the
!         observer, the error will be diagnosed by routines in the call
!         tree of this routine.
!
!     8)  If orientation data for the frame REF are not available,
!         the error will be diagnosed by routines in the call tree
!         of this routine.
!
!     9)  If orientation data for the frame DREF are required but
!         not available, the error will be diagnosed by routines in the
!         call tree of this routine.
!
!     10) If the input radii don't define a valid triaxial ellipsoid,
!         the error will be diagnosed by routines in the call tree of
!         this routine.
!
! $ Files
!
!     This routine doesn't directly participate in SPICE kernel loading
!     or unloading.  However, a variety of SPICE kernels must be loaded
!     in order for this routine to work:
!
!        - SPK files providing ephemeris data enabling computation of
!          the specified state vector are required.
!
!        - If non-inertial reference frames are used, then PCK
!          files, frame kernels, C-kernels, and SCLK kernels may be
!          needed.
!
!        - If the state of interest is defined in terms of a target
!          surface point, then (currently) a PCK providing radii for a
!          triaxial shape model must be loaded.
!
!     See the Files section of GFEVNT's header for further information.
!
! $ Particulars
!
!     This routine is used by the GF coordinate utility routines in
!     order to solve for time windows on which specified mathematical
!     conditions involving coordinates are satisfied. The role of
!     this routine is to provide Cartesian state vectors enabling
!     the GF coordinate utilities to determine the signs of the
!     derivatives with respect to time of coordinates of interest.
!
!     This routine has a secondary purpose: enabling the GF system
!     to determine, via a binary state search, the window over
!     which a coordinate of interest is computable. This "computability
!     window" must be found before any search involving a constraint
!     on a coordinate of a surface intercept point can be performed.
!
! $ Examples
!
!     See ZZGFCOU.
!
! $ Restrictions
!
!     1)  The interface and functionality of this set of routines may
!         change without notice.  These routines should be called only
!         by SPICELIB routines.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0 05-MAR-2009 (NJB)
!
!-&

! $ Index_Entries
!
!     compute state defining coordinate
!
!-&

!
!     SPICELIB functions
!
      logical               return

!
!     Local variables
!
      double precision      lt

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

      call chkin ( 'ZZGFCOST' )

!
!     No result was found yet.
!
      found = .false.


      if ( vecdef .eq. posdef  ) then
!
!        Find the observer-target state vector.
!
         call spkez ( trgid, et, ref, abcorr, obsid, state, lt )

         found = .true.

      else if ( vecdef .eq. sobdef ) then
!
!        The caller has requested the state of a sub-observer point.
!
         call zzgfssob ( method, trgid, et,    ref, &
                         abcorr, obsid, radii, state )

         found = .true.


      else if ( vecdef .eq. sindef ) then
!
!        The caller has requested the state of a surface intercept
!        point.
!
         call zzgfssin ( method, trgid, et,   ref,   abcorr, obsid, &
                         dref,   dctr,  dvec, radii, state,  found )

      else

         call setmsg ( 'The coordinate quantity # is not recognized.' )
         call errch  ( '#',  vecdef                                   )
         call sigerr ( 'SPICE(NOTSUPPORTED)'                          )
         call chkout ( 'ZZGFCOST'                                     )
         return

      end if

!
!     At this point, one of the following is true:
!
!        - the state vector was found and
!          FOUND is .TRUE.
!
!        - FOUND is .FALSE.
!
!        - a SPICE error occurred
!

      call chkout  ( 'ZZGFCOST' )
      return
      end
