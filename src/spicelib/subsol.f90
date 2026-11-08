!$Procedure SUBSOL ( Sub-solar point )

      subroutine subsol ( method, target, et, abcorr, obsrvr, spoint )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Deprecated: This routine has been superseded by the SPICELIB
!     routine SUBSLR. This routine is supported for purposes of
!     backward compatibility only.
!
!     Determine the coordinates of the sub-solar point on a target
!     body as seen by a specified observer at a specified epoch,
!     optionally corrected for planetary (light time) and stellar
!     aberration.
!
! $ Required_Reading
!
!     FRAMES
!     PCK
!     SPK
!     TIME
!
! $ Keywords
!
!     GEOMETRY
!
! $ Declarations

      implicit none

      include               'zzctr.inc'

      character(*)          method
      character(*)          target
      real(wp)              et
      character(*)          abcorr
      character(*)          obsrvr
      real(wp)              spoint ( 3 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     METHOD     I   Computation method.
!     TARGET     I   Name of target body.
!     ET         I   Epoch in ephemeris seconds past J2000 TDB.
!     ABCORR     I   Aberration correction.
!     OBSRVR     I   Name of observing body.
!     SPOINT     O   Sub-solar point on the target body.
!
! $ Detailed_Input
!
!     METHOD   is a short string specifying the computation method
!              to be used. The choices are:
!
!                 'Near point'       The sub-solar point is defined
!                                    as the nearest point on the
!                                    target to the sun.
!
!                 'Intercept'        The sub-observer point is
!                                    defined as the target surface
!                                    intercept of the line
!                                    containing the target's center
!                                    and the sun's center.
!
!              In both cases, the intercept computation treats the
!              surface of the target body as a triaxial ellipsoid.
!              The ellipsoid's radii must be available in the kernel
!              pool.
!
!              Neither case nor white space are significant in
!              METHOD. For example, the string ' NEARPOINT' is
!              valid.
!
!
!     TARGET   is the name of the target body. TARGET is
!              case-insensitive, and leading and trailing blanks in
!              TARGET are not significant. Optionally, you may
!              supply a string containing the integer ID code for
!              the object. For example both 'MOON' and '301' are
!              legitimate strings that indicate the moon is the
!              target body.
!
!              This routine assumes that the target body is modeled
!              by a tri-axial ellipsoid, and that a PCK file
!              containing its radii has been loaded into the kernel
!              pool via FURNSH.
!
!
!     ET       is the epoch in ephemeris seconds past J2000 at which
!              the sub-solar point on the target body is to be
!              computed.
!
!
!     ABCORR   indicates the aberration corrections to be applied
!              when computing the observer-target state.  ABCORR
!              may be any of the following.
!
!                 'NONE'     Apply no correction. Return the
!                            geometric sub-solar point on the target
!                            body.
!
!                 'LT'       Correct for planetary (light time)
!                            aberration. Both the state and rotation
!                            of the target body are corrected for one
!                            way light time from target to observer.
!
!                            The state of the sun relative to the
!                            target is corrected for one way light
!                            from the sun to the target; this state
!                            is evaluated at the epoch obtained by
!                            retarding ET by the one way light time
!                            from target to observer.
!
!                 'LT+S'     Correct for planetary (light time) and
!                            stellar aberrations. Light time
!                            corrections are the same as in the 'LT'
!                            case above. The target state is
!                            additionally corrected for stellar
!                            aberration as seen by the observer, and
!                            the sun state is corrected for stellar
!                            aberration as seen from the target.
!
!                 'CN'       Converged Newtonian light time
!                            correction. In solving the light time
!                            equation, the 'CN' correction iterates
!                            until the solution converges (three
!                            iterations on all supported platforms).
!                            Whether the 'CN+S' solution is
!                            substantially more accurate than the
!                            'LT' solution depends on the geometry
!                            of the participating objects and on the
!                            accuracy of the input data. In all
!                            cases this routine will execute more
!                            slowly when a converged solution is
!                            computed. See the $Particulars section
!                            of SPKEZR for a discussion of precision
!                            of light time corrections. Light time
!                            corrections are applied as in the 'LT'
!                            case.
!
!                 'CN+S'     Converged Newtonian light time
!                            corrections and stellar aberration
!                            correction. Light time and stellar
!                            aberration corrections are applied as
!                            in the 'LT+S' case.
!
!
!     OBSRVR   is the name of the observing body, typically a
!              spacecraft, the earth, or a surface point on the
!              earth. OBSRVR is case-insensitive, and leading and
!              trailing blanks in OBSRVR are not significant.
!              Optionally, you may supply a string containing the
!              integer ID code for the object. For example both
!              'EARTH' and '399' are legitimate strings that indicate
!              the earth is the observer.
!
! $ Detailed_Output
!
!     SPOINT   is the sub-solar point on the target body at ET
!              expressed relative to the body-fixed frame of the
!              target body.
!
!              The sub-solar point is defined either as the point on
!              the target body that is closest to the sun, or the
!              target surface intercept of the line containing the
!              target's center and the sun's center; the input
!              argument METHOD selects the definition to be used.
!
!              The body-fixed frame, which is time-dependent, is
!              evaluated at ET if ABCORR is 'NONE'; otherwise the
!              frame is evaluated at ET-LT, where LT is the one way
!              light time from target to observer.
!
!              The state of the target body is corrected for
!              aberration as specified by ABCORR; the corrected
!              state is used in the geometric computation. As
!              indicated above, the rotation of the target is
!              retarded by one way light time if ABCORR specifies
!              that light time correction is to be done.
!
!              The state of the sun as seen from the observing
!              body is also corrected for aberration as specified
!              by ABCORR. The corrections, when selected, are
!              applied at the epoch ET-LT, where LT is the one way
!              light time from target to observer.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     If any of the listed errors occur, the output arguments are
!     left unchanged.
!
!     1)  If the input argument METHOD is not recognized, the error
!         SPICE(DUBIOUSMETHOD) is signaled.
!
!     2)  If either of the input body names TARGET or OBSRVR cannot be
!         mapped to NAIF integer codes, the error SPICE(IDCODENOTFOUND)
!         is signaled.
!
!     3)  If OBSRVR and TARGET map to the same NAIF integer ID codes,
!         the error SPICE(BODIESNOTDISTINCT) is signaled.
!
!     4)  If frame definition data enabling the evaluation of the state
!         of the target relative to the observer in target body-fixed
!         coordinates have not been loaded prior to calling SUBSOL, an
!         error is signaled by a routine in the call tree of this
!         routine.
!
!     5)  If the specified aberration correction is not recognized, an
!         error is signaled by a routine in the call tree of this
!         routine.
!
!     6)  If insufficient ephemeris data have been loaded prior to
!         calling SUBSOL, an error is signaled by a
!         routine in the call tree of this routine.
!
!     7)  If the triaxial radii of the target body have not been loaded
!         into the kernel pool prior to calling SUBSOL, an error is
!         signaled by a routine in the call tree of this routine.
!
!     8)  If the size of the TARGET body radii kernel variable is not
!         three, an error is signaled by a routine in the call tree of
!         this routine.
!
!     9)  If any of the three TARGET body radii is less-than or equal to
!         zero, an error is signaled by a routine in the call tree of
!         this routine.
!
!     10) If PCK data supplying a rotation model for the target body
!         have not been loaded prior to calling SUBSOL, an error is
!         signaled by a routine in the call tree of this routine.
!
! $ Files
!
!     Appropriate SPK, PCK, and frame data must be available to
!     the calling program before this routine is called. Typically
!     the data are made available by loading kernels; however the
!     data may be supplied via subroutine interfaces if applicable.
!
!     The following data are required:
!
!     -  SPK data: ephemeris data for sun, target, and observer must
!        be loaded. If aberration corrections are used, the states of
!        sun, target, and observer relative to the solar system
!        barycenter must be calculable from the available ephemeris
!        data. Ephemeris data are made available by loading
!        one or more SPK files via FURNSH.
!
!     -  PCK data: triaxial radii for the target body must be loaded
!        into the kernel pool. Typically this is done by loading a
!        text PCK file via FURNSH.
!
!     -  Further PCK data:  a rotation model for the target body must
!        be loaded. This may be provided in a text or binary PCK
!        file which is loaded via FURNSH.
!
!     -  Frame data: if a frame definition is required to convert
!        the sun, observer, and target states to the body-fixed frame
!        of the target, that definition must be available in the
!        kernel pool. Typically the definition is supplied by loading
!        a frame kernel via FURNSH.
!
!     In all cases, kernel data are normally loaded once per program
!     run, NOT every time this routine is called.
!
! $ Particulars
!
!     SUBSOL computes the sub-solar point on a target body, as seen by
!     a specified observer.
!
!     There are two different popular ways to define the sub-solar
!     point:  "nearest point on target to the sun" or "target surface
!     intercept of line containing target and sun." These coincide
!     when the target is spherical and generally are distinct otherwise.
!
!     When comparing sub-point computations with results from sources
!     other than SPICE, it's essential to make sure the same geometric
!     definitions are used.
!
! $ Examples
!
!     In the following example program, the file MGS.BSP is a
!     hypothetical binary SPK ephemeris file containing data for the
!     Mars Global Surveyor orbiter. The SPK file de405s.bsp contains
!     data for the planet barycenters as well as the Earth, Moon, and
!     Sun for the time period including the date 1997 Dec 31 12:000
!     UTC. MGS0000A.TPC is a planetary constants kernel file
!     containing radii and rotation model constants.  MGS00001.TLS is
!     a leapseconds file. (File names shown here that are specific
!     to MGS are not names of actual files.)
!
!           IMPLICIT NONE
!
!           CHARACTER*25          METHOD ( 2 )
!
!           INTEGER               I
!
!           DOUBLE PRECISION      DPR
!           DOUBLE PRECISION      ET
!           DOUBLE PRECISION      LAT
!           DOUBLE PRECISION      LON
!           DOUBLE PRECISION      RADIUS
!           DOUBLE PRECISION      SPOINT ( 3 )
!
!           DATA                  METHOD / 'Intercept', 'Near point' /
!
!     C
!     C     Load kernel files.
!     C
!           CALL FURNSH ( 'MGS00001.TLS' )
!           CALL FURNSH ( 'MGS0000A.TPC' )
!           CALL FURNSH ( 'de405s.bsp'   )
!           CALL FURNSH ( 'MGS.BSP'      )
!
!     C
!     C     Convert the UTC request time to ET (seconds past
!     C     J2000, TDB).
!     C
!           CALL STR2ET ( '1997 Dec 31 12:00:00', ET )
!
!     C
!     C     Compute sub-spacecraft point using light time and stellar
!     C     aberration corrections. Use the "target surface intercept"
!     C     definition of sub-spacecraft point on the first loop
!     C     iteration, and use the "near point" definition on the
!     C     second.
!     C
!           DO I = 1, 2
!
!              CALL SUBSOL ( METHOD(I),
!          .                 'MARS',  ET,  'LT+S',  'MGS',  SPOINT )
!
!     C
!     C        Convert rectangular coordinates to planetocentric
!     C        latitude and longitude. Convert radians to degrees.
!     C
!              CALL RECLAT ( SPOINT, RADIUS, LON, LAT  )
!
!              LON = LON * DPR ()
!              LAT = LAT * DPR ()
!
!     C
!     C        Write the results.
!     C
!              WRITE (*,*) ' '
!              WRITE (*,*) 'Computation method: ', METHOD(I)
!              WRITE (*,*) ' '
!              WRITE (*,*) '  Radius                   (km)  = ', RADIUS
!              WRITE (*,*) '  Planetocentric Latitude  (deg) = ', LAT
!              WRITE (*,*) '  Planetocentric Longitude (deg) = ', LON
!              WRITE (*,*) ' '
!
!           END DO
!
!           END
!
! $ Restrictions
!
!     1)  The appropriate kernel data must have been loaded before this
!         routine is called. See the $Files section above.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     J.E. McLean        (JPL)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.4.0, 01-NOV-2021 (EDW) (JDR)
!
!        Body radii accessed from kernel pool using ZZGFTREB.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.3.0, 04-JUL-2014 (NJB) (BVS)
!
!        Discussion of light time corrections was updated. Assertions
!        that converged light time corrections are unlikely to be
!        useful were removed.
!
!     Last update was 19-SEP-2013 (BVS)
!
!        Updated to save the input body names and ZZBODTRN state
!        counters and to do name-ID conversions only if the counters
!        have changed.
!
!-    SPICELIB Version 1.2.3, 18-MAY-2010 (BVS)
!
!        Index line now states that this routine is deprecated.
!
!-    SPICELIB Version 1.2.2, 17-MAR-2009 (EDW)
!
!        Typo correction in $Required_Reading, changed
!        FRAME to FRAMES.
!
!-    SPICELIB Version 1.2.1, 07-FEB-2008 (NJB)
!
!        $Abstract now states that this routine is deprecated.
!
!-    SPICELIB Version 1.2.0, 24-OCT-2005 (NJB)
!
!        Call to BODVAR was replaced with call to BODVCD.
!
!-    SPICELIB Version 1.1.0, 22-JUL-2004 (NJB)
!
!        Updated to support representations of integers in the input
!        arguments TARGET and OBSRVR. Deleted references in header to
!        kernel-specific loaders. Made miscellaneous minor corrections
!        to header comments.
!
!-    SPICELIB Version 1.0.2, 12-DEC-2002 (NJB)
!
!        Corrected and updated code example in header.
!
!-    SPICELIB Version 1.0.1, 01-NOV-1999 (WLT)
!
!        Declared routine LTIME to be external.
!
!-    SPICELIB Version 1.0.0, 03-SEP-1999 (NJB) (JEM)
!
!-&


! $ Index_Entries
!
!     DEPRECATED sub-solar point
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.1.0, 22-JUL-2004 (NJB)
!
!        Updated to support representations of integers in the
!        input arguments TARGET and OBSRVR: calls to BODN2C
!        were replaced by calls to BODS2C.
!
!-&


!
!     SPICELIB functions
!
      logical               eqstr
      logical               failed
      logical               return

      external              ltime
!
!     Local parameters
!
      integer(ip)           frnmln
      parameter           ( frnmln =  80 )

!
!     Saved body name length.
!
      integer(ip)           maxl
      parameter           ( maxl  = 36 )


!
!     Local variables
!
      character(frnmln)     frname

      real(wp)              alt
      real(wp)              ettarg
      real(wp)              lt
      real(wp)              origin ( 3 )
      real(wp)              pos    ( 3 )
      real(wp)              radii  ( 3 )
      real(wp)              sunlt

      integer(ip)           frcode
      integer(ip)           obscde
      integer(ip)           trgcde

      logical               found

!
!     Saved name/ID item declarations.
!
      integer(ip)           svctr1 ( ctrsiz )
      character(maxl)       svtarg
      integer(ip)           svtcde
      logical               svfnd1

      integer(ip)           svctr2 ( ctrsiz )
      character(maxl)       svobsr
      integer(ip)           svobsc
      logical               svfnd2

      logical               first

!
!     Saved variables
!
      save                  origin

!
!     Saved name/ID items.
!
      save                  svctr1
      save                  svtarg
      save                  svtcde
      save                  svfnd1

      save                  svctr2
      save                  svobsr
      save                  svobsc
      save                  svfnd2

      save                  first

!
!     Initial values
!
      data                  origin / 3 * 0.0_wp /

!
!     Initial values.
!
      data                  first   / .true. /


!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'SUBSOL' )
      end if

!
!     Initialization.
!
      if ( first ) then

!
!        Initialize counters.
!
         call zzctruin( svctr1 )
         call zzctruin( svctr2 )

         first = .false.

      end if

!
!     Obtain integer codes for the target and observer.
!
      call zzbods2c ( svctr1, svtarg, svtcde, svfnd1, &
                      target, trgcde, found    )

      if ( .not. found ) then

         call setmsg ( 'The target, ' &
         //            '''#'', is not a recognized name for an ' &
         //            'ephemeris object. The cause of this ' &
         //            'problem may be that you need an updated ' &
         //            'version of the SPICE Toolkit. '           )
         call errch  ( '#', target                                )
         call sigerr ( 'SPICE(IDCODENOTFOUND)'                    )
         call chkout ( 'SUBSOL'                                   )
         return

      end if


      call zzbods2c ( svctr2, svobsr, svobsc, svfnd2, &
                      obsrvr, obscde, found    )

      if ( .not. found ) then

         call setmsg ( 'The observer, ' &
         //            '''#'', is not a recognized name for an ' &
         //            'ephemeris object. The cause of this ' &
         //            'problem may be that you need an updated ' &
         //            'version of the SPICE Toolkit. '           )
         call errch  ( '#', obsrvr                                )
         call sigerr ( 'SPICE(IDCODENOTFOUND)'                    )
         call chkout ( 'SUBSOL'                                   )
         return

      end if


!
!     Check the input body codes.  If they are equal, signal
!     an error.
!
      if ( obscde == trgcde ) then

         call setmsg ( 'In computing the sub-observer point, the '    // &
                       'observing body and target body are the same. '// &
                       'Both are #.'                                  )
         call errch  ( '#',  obsrvr                                   )
         call sigerr ( 'SPICE(BODIESNOTDISTINCT)'                     )
         call chkout ( 'SUBSOL'                                       )
         return

      end if

!
!     Get the radii of the target body from the kernel pool.
!
      call zzgftreb ( trgcde, radii )

      if ( failed() ) then
            call chkout ( 'SUBSOL' )
            return
      end if

!
!     Find the name of the body-fixed frame associated with the
!     target body.  We'll want the state of the target relative to
!     the observer in this body-fixed frame.
!
      call cidfrm ( trgcde, frcode, frname, found )

      if ( .not. found ) then

         call setmsg ( 'No body-fixed frame is associated with '   // &
                       'target body #; a frame kernel must be '    // &
                       'loaded to make this association.  Consult '// &
                       'the FRAMES Required Reading for details.'   )
         call errch  ( '#', target                                  )
         call sigerr ( 'SPICE(NOFRAME)'                             )
         call chkout ( 'SUBSOL'                                     )
         return

      end if

!
!     If we're using aberration corrections, we'll need the
!     one way light time from the target to the observer.  Otherwise,
!     we set the time time to zero.
!
      if (  eqstr ( abcorr, 'NONE' )  ) then

         lt      =  0.0_wp
         ettarg  =  et

      else

         call ltime ( et, obscde, '<-', trgcde, ettarg, lt )

      end if

!
!     Determine the position of the sun in target body-fixed
!     coordinates.
!
!     Call SPKEZ to compute the position of the sun as seen from the
!     target body and the light time between them SUNLT.  This state is
!     evaluated at the target epoch ETTARG. We request that the
!     coordinates of the target-sun position vector POS be returned
!     relative to the body fixed reference frame associated with the
!     target body, using aberration corrections specified by the input
!     argument ABCORR.
!
      call spkpos ( 'SUN', ettarg, frname, abcorr, target, pos, sunlt )

!
!     Find the sub-solar point using the specified geometric definition.
!
      if (  eqstr( method, 'Near point' )  ) then
!
!        Locate the nearest point to the sun on the target.
!
         call nearpt ( pos, radii(1), radii(2), radii(3), spoint, alt )


      else if (  eqstr( method, 'Intercept' )  ) then

         call surfpt ( origin,  pos,   radii(1),  radii(2),  radii(3), &
                       spoint,  found                                  )

!
!        Since the line in question passes through the center of the
!        target, there will always be a surface intercept.  So we should
!        never have FOUND = .FALSE.
!
         if ( .not. found ) then

            call setmsg ( 'Call to SURFPT returned FOUND=FALSE even ' // &
                          'though vertex of ray is at target center. '// &
                          'This indicates a bug. Please contact NAIF.')
            call sigerr ( 'SPICE(BUG)'                                )
            call chkout ( 'SUBSOL'                                    )
            return

         end if



      else

         call setmsg ( 'The computation method # was not recognized. '// &
                       'Allowed values are "Near point" and '         // &
                       '"Intercept."'                                 )
         call errch  ( '#',  method                                   )
         call sigerr ( 'SPICE(DUBIOUSMETHOD)'                         )
         call chkout ( 'SUBSOL'                                       )
         return

      end if


      call chkout ( 'SUBSOL' )
      return
      end
