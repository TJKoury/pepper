!$Procedure      ZZGFCOQ ( GF, return coordinate quantity )

      subroutine zzgfcoq ( vecdef, method, trgid, et, &
                           ref,    abcorr, obsid, dref, &
                           dvec,   crdsys, ctrid, re, &
                           f,      crdnam, value, found )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     SPICE private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Return the value of a specified coordinate of a vector.
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

      character(*)          vecdef
      character(*)          method
      integer(ip)           trgid
      real(wp)              et
      character(*)          ref
      character(*)          abcorr
      integer(ip)           obsid
      character(*)          dref
      real(wp)              dvec   ( 3 )
      character(*)          crdsys
      integer(ip)           ctrid
      real(wp)              re
      real(wp)              f
      character(*)          crdnam
      real(wp)              value
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
!     DVEC       I   Ray's direction vector.
!     CRDSYS     I   Coordinate system name.
!     CTRID      I   Frame center ID code.
!     RE         I   Equatorial radius of central body.
!     F          I   Flattening coefficient of central body.
!     CRDNAM     I   Coordinate name.
!     VALUE      O   Coordinate value.
!     FOUND      O   Flag indicating if coordinate was computed.
!
! $ Detailed_Input
!
!
!     VECDEF     Every coordinate computed by this routine is a
!                function of an underlying vector. VECDEF is a short
!                string describing the means by which the vector of
!                interest is defined. Only parameters from the Fortran
!                INCLUDE file zzgf.inc should be used. Parameter names
!                and meanings are:
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
!                TDB, at which the specified coordinate is to be
!                computed.
!
!
!     REF        is the name of the reference frame relative to which
!                the vector of interest is specified.  The specified
!                condition applies to the specified coordinate of
!                of this vector in frame REF.
!
!                When geodetic or planetographic coordinates are used,
!                the reference ellipsoid is assumed to be that
!                associated with the central body of the frame
!                designated by REF. In this case, the central body of
!                the frame must be an extended body.
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
!                and trailing blanks in FIXREF are not significant.
!
!                When DREF designates a non-inertial frame, the
!                orientation of the frame is evaluated at an epoch
!                dependent on the frame's center and, if the center is
!                not the observer, on the selected aberration
!                correction. See the description of the direction
!                vector DVEC for details.
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
!     CRDSYS     is the name of the coordinate system to which the
!                coordinate of interest belongs. Allowed values are
!                those defined in the GF Fortran INCLUDE file
!
!                   zzgf.inc.
!
!                Case, leading and trailing blanks ARE significant
!                in the string CRDSYS.
!
!
!     CTRID      is the NAIF ID code of the input frame REF's center.
!
!
!     RE         is the equatorial radius associated with the body
!                designated by CTRID. RE is used only when the
!                coordinate system is GEOSYS or PGRSYS; otherwise
!                RE may be set to 0.D0.
!
!     F          is the flattening coefficient associated with the body
!                designated by CTRID. RE is used only when the
!                coordinate system is GEOSYS or PGRSYS; otherwise RE
!                may be set to 0.D0.
!
!
!     CRDNAM     is the name of the coordinate of interest:  this is
!                the coordinate to which the specified condition
!                applies.  The set of coordinate names is a function of
!                the coordinate system. Allowed values are those
!                defined in the GF Fortran INCLUDE file
!
!                   zzgf.inc.
!
!                Case, leading and trailing blanks ARE significant
!                in the string CRDNAM.
!
!
! $ Detailed_Output
!
!     VALUE     is the specified coordinate, evaluated at the epoch ET.
!               Coordinates having dimensions of length have units of
!               km. Coordinates having angular dimensions have units of
!               radians.
!
!               VALUE is defined if and only if the output argument
!               FOUND is set to .TRUE.
!
!
!     FOUND     is a logical flag indicating whether the requested
!               coordinate could be computed. FOUND is set to .FALSE.
!               if and only if the vector definition is SINDEF and
!               either
!
!                  - no surface intercept is found
!
!                  - the velocity of the surface intercept is not
!                    computable
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
!     2)  If the vector definition is either SOBDEF or SINDEF
!         and the computation method METHOD is not recognized,
!         the error will be diagnosed by routines in the call tree
!         of this routine.
!
!     3)  If the aberration correction ABCORR is not recognized,
!         the error will be diagnosed by routines in the call tree
!         of this routine.
!
!     4)  If the coordinate system name CRDSYS is not recognized,
!         the error SPICE(NOTSUPPORTED) is signaled.
!
!     5)  If the coordinate name CRDNAM is not recognized,
!         the error SPICE(NOTSUPPORTED) is signaled.
!
!     6)  If the frame REF is not recognized by the frames subsystem,
!         the error will be diagnosed by routines in the call tree
!         of this routine.
!
!     7)  If VECDEF calls for a computation involving a target surface
!         point and the radii defining the reference ellipsoid
!         associated with the target body are not available in the
!         kernel pool, the error will be diagnosed by routines in the
!         call tree of this routine.
!
!     8)  If VECDEF calls for a computation involving a target surface
!         point and the name and ID code of the frame associated with
!         the target body is not available from the frame subsystem,
!         the error will be diagnosed by routines in the call tree
!         of this routine.
!
!     9)  If ephemeris data are required but not available to compute
!         the state of the target, the coordinate frame REF's center,
!         or the input ray's frame DREF's center relative to the
!         observer, the error will be diagnosed by routines in the call
!         tree of this routine.
!
!     10) If orientation data for the frame REF are not available,
!         the error will be diagnosed by routines in the call tree
!         of this routine.
!
!     11) If orientation data for the frame DREF are required but
!         not available, the error will be diagnosed by routines in the
!         call tree of this routine.
!
! $ Files
!
!     This routine doesn't directly participate in SPICE kernel loading
!     or unloading.  However, a variety of SPICE kernels must be loaded
!     in order for this routine to work:
!
!        - Since all coordinate computations supported by this routine
!          depend on observer-target vectors, at a minimum, SPK files
!          providing ephemeris data enabling computation of these
!          vectors are required.
!
!        - If non-inertial reference frames are used, then PCK
!          files, frame kernels, C-kernels, and SCLK kernels may be
!          needed.
!
!        - If the coordinate of interest is defined in terms of a target
!          surface point, then (currently) a PCK providing radii for a
!          triaxial shape model must be loaded.
!
!        - If geodetic or planetographic coordinates are used, then a
!          PCK providing radii for a triaxial shape model must be
!          loaded.
!
!     See the Files section of GFEVNT's header for further information.
!
! $ Particulars
!
!     This routine is used by the GF coordinate utility routines in
!     order to solve for time windows on which specified mathematical
!     conditions involving coordinates are satisfied.
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
!-    SPICELIB Version 2.0.0 12-MAY-2009 (NJB)
!
!        Upgraded to support targets and observers having
!        no names associated with their ID codes.
!
!-    SPICELIB Version 1.0.0 05-MAR-2009 (NJB)
!
!-&

! $ Index_Entries
!
!     compute coordinates of a vector
!
!-&


!
!     SPICELIB functions
!
      integer(ip)           isrchc

      logical               failed
      logical               return

!
!     Local parameters
!
      integer(ip)           bdnmln
      parameter           ( bdnmln = 36 )

      integer(ip)           crdlen
      parameter           ( crdlen = 32 )

      integer(ip)           nsys
      parameter           ( nsys   = 7 )

!
!     Local variables
!
      character(crdlen)     crdnms ( 3, nsys )
      character(bdnmln)     ctrnam
      character(bdnmln)     obsnam
      character(crdlen)     sysnms (    nsys )
      character(crdlen)     sysnam
      character(bdnmln)     trgnam

      real(wp)              coords ( 3 )
      real(wp)              lt
      real(wp)              pos    ( 3 )
      real(wp)              trgepc
      real(wp)              srfvec ( 3 )

      integer(ip)           crdidx
      integer(ip)           prvctr
      integer(ip)           prvobs
      integer(ip)           prvtrg
      integer(ip)           sysidx

      logical               first

!
!     Saved variables
!
      save                  crdnms
      save                  ctrnam
      save                  first
      save                  obsnam
      save                  prvctr
      save                  prvobs
      save                  prvtrg
      save                  sysnms
      save                  trgnam

!
!     Initial values
!
      data                  first / .true. /
!
!     The Ith coordinate system in the array SYSNMS has coordinates
!     in the Ith row of the array CRDNMS. This association must be
!     preserved when this routine is updated.
!
      data                  sysnms /  recsys, &
                                      latsys, &
                                      radsys, &
                                      sphsys, &
                                      cylsys, &
                                      geosys, &
                                      pgrsys  /

!
!     The order of the coordinate names in the Ith row of this array
!     matches the order of the outputs of the corresponding
!     SPICELIB routine REC*, which maps a Cartesian vector to
!     the Ith coordinate system in the array SYSNMS. Again, this
!     order must be preserved.
!
      data                  crdnms /  xcrd,     ycrd,     zcrd, &
                                      radcrd,   loncrd,   latcrd, &
                                      rngcrd,   racrd,    deccrd, &
                                      radcrd,   cltcrd,   loncrd, &
                                      radcrd,   loncrd,   zcrd, &
                                      loncrd,   latcrd,   altcrd, &
                                      loncrd,   latcrd,   altcrd  /

      data                  prvctr /  0 /
      data                  prvobs /  0 /
      data                  prvtrg /  0 /

      data                  obsnam / ' ' /
      data                  trgnam / ' ' /

!
!     Standard SPICE error handling.
!
      if ( return () ) return


!
!     No result was found yet.
!
      found = .false.

!
!     Find the index of the coordinate system name in the list of
!     supported names.
!
      sysidx = isrchc ( crdsys, nsys, sysnms )

      if ( sysidx == 0 ) then
!
!        We don't recognize this system name.
!
         call setmsg ( 'The coordinate system # is not supported.' )
         call errch  ( '#',  crdsys                                )
         call sigerr ( 'SPICE(NOTSUPPORTED)'                       )
         return

      end if

      sysnam = sysnms(sysidx)

!
!     Find the index of the coordinate name in the list of
!     supported names.
!
      crdidx = isrchc ( crdnam, 3, crdnms(1,sysidx) )

      if ( crdidx == 0 ) then
!
!        We don't recognize this coordinate name.
!
         call setmsg ( 'The coordinate name # belonging to the '  // &
                       'coordinate system # is not recognized.'   )
         call errch  ( '#',  crdnam                               )
         call errch  ( '#',  crdsys                               )
         call sigerr ( 'SPICE(NOTSUPPORTED)'                      )
         return

      end if

!
!     Look up the target and observer names if these will be
!     needed. The SUBPNT and SINCPT interfaces require them.
!     The RECPGR interface requires the frame center ID code
!     as well.
!
      if (      ( vecdef == sobdef ) &
           .or. ( vecdef == sindef ) &
           .or. ( sysnam == pgrsys )  ) then

         if (  first  .or. ( trgid /= prvtrg )  ) then

            call bodc2s ( trgid, trgnam )

            prvtrg = trgid

         end if


         if (  first  .or. ( obsid /= prvobs )  ) then

            call bodc2s ( obsid, obsnam )

            prvobs = obsid

         end if

         if (  first  .or. ( ctrid /= prvctr )  ) then

            call bodc2s ( ctrid, ctrnam )

            prvctr = ctrid

         end if

         first = .false.

      end if

      if ( vecdef == posdef  ) then
!
!        Find the observer-target position vector.
!
         call spkezp ( trgid, et, ref, abcorr, obsid, pos, lt )


      else if ( vecdef == sobdef ) then
!
!        The caller has requested a sub-observer point coordinate
!        computation.
!
         call subpnt ( method, trgnam, et,  ref, &
                       abcorr, obsnam, pos, trgepc, srfvec )


      else if ( vecdef == sindef ) then
!
!        The caller has requested a surface intercept point coordinate
!        computation.
!
         call sincpt ( method,  trgnam, et,     ref, &
                       abcorr,  obsnam, dref,   dvec, &
                       pos,     trgepc, srfvec, found  )

!
!        Without an intercept, there's nothing left to do here.
!
         if ( .not. found ) return

      else

         call setmsg ( 'The coordinate quantity # is not recognized.' )
         call errch  ( '#',  vecdef                                   )
         call sigerr ( 'SPICE(NOTSUPPORTED)'                          )
         return

      end if

!
!     If we already encountered an error while trying to compute
!     the vector of interest, return now.
!
      if (  failed() ) return

!
!     At this point we assume the vector whose coordinate is
!     to be computed resides in POS. Convert POS to the
!     specified coordinate system.
!
      if ( sysnam == recsys ) then
!
!        No conversion needed for rectangular coordinates.
!
         call moved ( pos, 3, coords )


      else if ( sysnam == latsys ) then

         call reclat ( pos, coords(1), coords(2), coords(3) )


      else if ( sysnam == radsys ) then

         call recrad ( pos, coords(1), coords(2), coords(3) )


      else if ( sysnam == sphsys ) then

         call recsph ( pos, coords(1), coords(2), coords(3) )


      else if ( sysnam == cylsys ) then

         call reccyl ( pos, coords(1), coords(2), coords(3) )


      else if ( sysnam == geosys ) then

         call recgeo ( pos, re, f, coords(1), coords(2), coords(3) )


      else if ( sysnam == pgrsys ) then

         call recpgr ( ctrnam,    pos,       re,       f, &
                       coords(1), coords(2), coords(3)    )

      else
!
!        We should never arrive here.
!
         call setmsg ( 'The coordinate system # is not supported.' )
         call errch  ( '#',  crdsys                                )
         call sigerr ( 'SPICE(BUG)'                                )
         return

      end if

!
!     Set the return value.
!
!     CRDIDX indicates the index of the coordinate of interest
!     in the list of coordinates for the input coordinate system.
!
      value = coords(crdidx)

!
!     Having made it this far means the result was found.
!
      found = .true.

      return
      end
