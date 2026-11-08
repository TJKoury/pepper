!$Procedure ZZGFSPU ( Private - GF, angular separation routines )

      subroutine zzgfspu ( of,     from,   shape,  frame,  et, &
                           udfunc, abcorr, decres, sep, &
                           xabcr,  xbod,   yref,   xref, &
                           xobs,   xrad,   xshp )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     This is the umbrella routine for the entry points needed by
!     GFEVNT in order to find angular separation events.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ANGLE
!     GEOMETRY
!     ROOT
!
! $ Declarations

      implicit none

      include 'zzabcorr.inc'

      character(*)          of    (2)
      character(*)          from
      character(*)          shape (2)
      character(*)          frame (2)
      real(wp)              et
      external              udfunc
      character(*)          abcorr
      logical               decres
      real(wp)              sep
      character(*)          xabcr
      integer(ip)           xbod  (2)
      character(*)          yref
      character(*)          xref  (2)
      integer(ip)           xobs
      real(wp)              xrad  (2)
      integer(ip)           xshp  (2)

! $ Brief_I/O
!
!     VARIABLE  I/O  ENTRY POINTS
!     --------  ---  --------------------------------------------------
!     OF         I   ZZGFSPIN
!     FROM       I   ZZGFSPIN
!     SHAPE      I   ZZGFSPIN
!     FRAME      I   ZZGFSPIN
!     ET         I   ZZGFSPDC, ZZGFSPGQ
!     UDFUNC     I   ZZGFSPDC
!     ABCORR     I   ZZGFSPIN
!     DECRES     O   ZZGFSPDC
!     SEP        O   ZZGFSPGQ
!     XABCR      O   ZZGFSPX
!     XBOD       O   ZZGFSPX
!     YREF       O   ZZGFSPX
!     XREF       O   ZZGFSPX
!     XOBS       O   ZZGFSPX
!     XRAD       O   ZZGFSPX
!     XSHP       O   ZZGFSPX
!
! $ Detailed_Input
!
!     OF       the string array naming the bodies whose angular
!              separation is of interest.
!
!     FROM     the string naming the observer.
!
!     SHAPE    the string array naming the geometric model used to
!              represent the shapes of OF. The relation between SHAPE
!              and OF is 1-to-1.
!
!              Models supported by this routine:
!
!                 'SPHERE'        Treat the body as a sphere with
!                                 radius equal to the maximum value of
!                                 BODYnnn_RADII
!
!                 'POINT'         Treat the body as a single point;
!                                 radius has value zero.
!
!              The SHAPE string lacks sensitivity to case and leading
!              or trailing blank.
!
!     FRAME    the string array naming the body-fixed reference frames
!              corresponding to OF. The relation between FRAME
!              and OF is 1-to-1.
!
!     ET       is the time in second past J2000 at which one wants
!              to determine an event condition.
!
!     ABCORR   the string description of the aberration corrections
!              to apply to the state evaluations to account for
!              one-way light time and stellar aberration.
!
!              This routine accepts the same aberration corrections
!              as does the SPICE routine SPKEZR. See the header of
!              SPKEZR for a detailed description of the aberration
!              correction options. For convenience, the options are
!              listed below:
!
!                 'NONE'     Apply no correction.
!
!                 'LT'       "Reception" case: correct for
!                            one-way light time using a Newtonian
!                            formulation.
!
!                 'LT+S'     "Reception" case: correct for
!                            one-way light time and stellar
!                            aberration using a Newtonian
!                            formulation.
!
!                 'CN'       "Reception" case: converged
!                            Newtonian light time correction.
!
!                 'CN+S'     "Reception" case: converged
!                            Newtonian light time and stellar
!                            aberration corrections.
!
!                 'XLT'      "Transmission" case: correct for
!                            one-way light time using a Newtonian
!                            formulation.
!
!                 'XLT+S'    "Transmission" case: correct for
!                            one-way light time and stellar
!                            aberration using a Newtonian
!                            formulation.
!
!                 'XCN'      "Transmission" case: converged
!                            Newtonian light time correction.
!
!                 'XCN+S'    "Transmission" case: converged
!                            Newtonian light time and stellar
!                            aberration corrections.
!
!                 The ABCORR string lacks sensitivity to case, leading
!                 and trailing blanks.
!
!     DECRES   is .TRUE. if the angular separation between the
!              objects is decreasing. Otherwise it is .FALSE.
!
!     SEP      is the angular separation between SVBOD1 and SVBOD2 as
!              seen from SVOBS at time ET.
!
!     For more information, see individual entry points.
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
!     1)  If a direct call to ZZGFSPU occurs, the error
!         SPICE(BOGUSENTRY) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine serves as the umbrella routine for 4 entry points
!     needed by GFEVNT in solving for angular separation conditions.
!
!     The five entry points are
!
!        ZZGFSPIN  --- an initialization routine that must be called
!                     prior to attempting to solve for any angular
!                     separation event.
!
!        ZZGFSPDC --- determines whether or not angular separation is
!                     decreasing at some time.
!
!        ZZGFSPGQ --- returns the angular separation of the two
!                     objects of interest as a function of ET.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     1)  ZZGFSPIN must be called prior to use of any of the
!         other entry points.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 3.0.0, 31-MAY-2021 (EDW) (JDR)
!
!        Edited umbrella routine and all its entry points' headers to
!        comply with NAIF standard. Extended $Brief_I/O to list all
!        arguments and refer to corresponding entry points.
!
!        Moved UDFUNC declaration to $Declarations section.
!
!        Routine ZZGFSPGQ now calls ZZSEPQ, a copy of ZZGFSPQ cast as
!        a function.
!
!-    SPICELIB Version 2.0.0, 27-JUN-2012 (EDW)
!
!        Code edits to implement use of ZZGFRELX.
!        These edits include removal of unneeded routines:
!
!           ZZGFSPUR
!           ZZGFSPLT
!
!        and corresponding unused variables.
!
!        Routine ZZGFGSEP renamed to ZZGFSPGQ to match geometry finder
!        naming convention.
!
!        Implemented a proper $Exceptions section. Update to header
!        entries.
!
!-    SPICELIB Version 1.1.0, 29-DEC-2009 (NJB) (EDW)
!
!        Edited argument descriptions. Removed mention of "ELLIPSOID"
!        shape from SHAPE as that option is not yet implemented.
!
!        Added an error check on body frame centers to enforce
!        a body frame center is the body. This check does not apply
!        to "POINT" or "SPHERE" shape targets, and so will not
!        execute for this version of the routine.
!
!        Rename of the ZZDHA call to DHFA.
!
!-    SPICELIB Version 1.0.0, 19-FEB-2009 (NJB) (EDW)
!
!-&


! $ Index_Entries
!
!     umbrella routine for finding angular separation events
!
!-&


!
!     SPICELIB functions
!
      integer(ip)           isrchc
      logical               failed
      logical               return
      real(wp)              dvsep
      real(wp)              dhfa
      real(wp)              zzsepq

!
!     Local Variables
!
      real(wp)              axes1 (3)
      real(wp)              axes2 (3)

      real(wp)              pv1   (6)
      real(wp)              pv2   (6)

      real(wp)              dtheta
      real(wp)              lt

      logical               attblk ( nabcor )
      logical               found

      integer(ip)           fcode1
      integer(ip)           fcode2
      integer(ip)           class
      integer(ip)           clssid
      integer(ip)           ctr1
      integer(ip)           ctr2


!
!     Saved Variables
!
      character(32)         svabcr
      save                  svabcr

      integer(ip)           svbod1
      save                  svbod1

      integer(ip)           svbod2
      save                  svbod2

      character(32)         svref
      save                  svref

      character(32)         svref1
      save                  svref1

      character(32)         svref2
      save                  svref2

      integer(ip)           svobs
      save                  svobs

      real(wp)              svrad1
      save                  svrad1

      real(wp)              svrad2
      save                  svrad2

      integer(ip)           svshp1
      save                  svshp1

      integer(ip)           svshp2
      save                  svshp2

      character(5)          ref
      save                  ref

!
!     Below we initialize the list of shape names.
!
      integer(ip)           spshpn
      parameter           ( spshpn = 2 )

      character(32)         svshap ( spshpn )

!
!     Define integer ID parameters for the shape names in
!     SVSHAP.
!
      integer(ip)           point
      parameter           ( point = 1 )

      integer(ip)           sphere
      parameter           ( sphere = 2 )

      save                  svshap

      data                  svshap / 'POINT', &
                                     'SPHERE' /

      data                  ref    / 'J2000'/

!
!     Never directly call this routine.
!
      call sigerr( 'SPICE(BOGUSENTRY)' )

      return




!$Procedure ZZGFSPIN ( Private - GF, angular separation initialization )

      entry zzgfspin ( of, from, shape, frame, abcorr )

! $ Abstract
!
!     This routine initializes variables that describe an angular
!     separation event of interest for solution by ZZGFSOLV.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ANGLE
!     GEOMETRY
!     ROOT
!
! $ Declarations
!
!      CHARACTER*(*)         OF   ( 2 )
!      CHARACTER*(*)         FROM
!      CHARACTER*(*)         SHAPE( 2 )
!      CHARACTER*(*)         FRAME( 2 )
!      CHARACTER*(*)         ABCORR
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     OF         I   Body id's of the angular separation objects
!     FROM       I   Observer name
!     SHAPE      I   Array of shape IDs corresponding to OF
!     FRAME      I   Array of frame names corresponding to OF
!     ABCORR     I   Aberration correction flag.
!
! $ Detailed_Input
!
!     OF       the string array naming the bodies whose angular
!              separation is of interest.
!
!     FROM     the string naming the observer.
!
!     SHAPE    the string array naming the geometric model used to
!              represent the shapes of OF. The relation between SHAPE
!              and OF is 1-to-1 and onto.
!
!              Models supported by this routine:
!
!                 'SPHERE'        Treat the body as a sphere with
!                                 radius equal to the maximum value of
!                                 BODYnnn_RADII
!
!                 'POINT'         Treat the body as a single point;
!                                 radius has value zero.
!
!              The SHAPE string lacks sensitivity to case and leading
!              or trailing blank.
!
!     FRAME    the string array naming the body-fixed reference frames
!              corresponding to OF. The relation between FRAME
!              and OF is 1-to-1.
!
!     ABCORR   the string description of the aberration corrections
!              to apply to the state evaluations to account for
!              one-way light time and stellar aberration.
!
!              This routine accepts the same aberration corrections
!              as does the SPICE routine SPKEZR. See the header of
!              SPKEZR for a detailed description of the aberration
!              correction options. For convenience, the options are
!              listed below:
!
!                 'NONE'     Apply no correction.
!
!                 'LT'       "Reception" case: correct for
!                            one-way light time using a Newtonian
!                            formulation.
!
!                 'LT+S'     "Reception" case: correct for
!                            one-way light time and stellar
!                            aberration using a Newtonian
!                            formulation.
!
!                 'CN'       "Reception" case: converged
!                            Newtonian light time correction.
!
!                 'CN+S'     "Reception" case: converged
!                            Newtonian light time and stellar
!                            aberration corrections.
!
!                 'XLT'      "Transmission" case: correct for
!                            one-way light time using a Newtonian
!                            formulation.
!
!                 'XLT+S'    "Transmission" case: correct for
!                            one-way light time and stellar
!                            aberration using a Newtonian
!                            formulation.
!
!                 'XCN'      "Transmission" case: converged
!                            Newtonian light time correction.
!
!                 'XCN+S'    "Transmission" case: converged
!                            Newtonian light time and stellar
!                            aberration corrections.
!
!                 The ABCORR string lacks sensitivity to case, leading
!                 and trailing blanks.
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
!     1)  If the object name for target 1, OF(1), is not a recognized
!         name, the error SPICE(IDCODENOTFOUND) is signaled.
!
!     2)  If the object name for target 2, OF(2), is not a recognized
!         name, the error SPICE(IDCODENOTFOUND) is signaled.
!
!     3)  If the object name for the observer, FROM, is not a recognized
!         name, the error SPICE(IDCODENOTFOUND) is signaled.
!
!     4)  If the three objects associated with an ANGULAR SEPARATION
!         search are not distinct, the error SPICE(BODIESNOTDISTINCT) is
!         signaled.
!
!     5)  If the body shape for target 1, SHAPE(1), is not recognized,
!         the error SPICE(NOTRECOGNIZED) is signaled.
!
!     6)  If the SHAPE(1) value lacks a corresponding case block, the
!         error SPICE(BUG) is signaled. This indicates a programming
!         error.
!
!     7)  If the body shape for target 2, SHAPE(2), is not recognized,
!         the error SPICE(NOTRECOGNIZED) is signaled.
!
!     8)  If the SHAPE(2) value lacks a corresponding case block, the
!         error SPICE(BUG) is signaled. This indicates a programming
!         error.
!
!     9)  If frame subsystem did not recognize frame name FRAME(1), the
!         error SPICE(NOFRAME) is signaled.
!
!     10) If the reference frame associated with target body 1, OF(1),
!         is not centered on target body 1, the error
!         SPICE(INVALIDFRAME) is signaled.
!
!     11) If frame subsystem did not recognize frame name FRAME(2), the
!         error SPICE(NOFRAME) is signaled.
!
!     12) If the reference frame associated with target body 2, OF(2),
!         is not centered on target body 2, the error
!         SPICE(INVALIDFRAME) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     None.
!
! $ Examples
!
!     None.
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.0.1, 31-MAY-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 2.0.0, 27-JUN-2012 (EDW)
!
!        REFVAL removed from routine argument list due to the use
!        of ZZGFRELX to calculate the events.
!
!        Implemented a proper $Exceptions section. Update to
!        $Author_and_Institution section.
!
!-    SPICELIB Version 1.1.0, 29-DEC-2009 (NJB) (EDW)
!
!        Edited argument descriptions. Removed mention of "ELLIPSOID"
!        shape from SHAPE as that option is not yet implemented.
!
!        Added an error check on body frame centers to enforce
!        a body frame center is the body. This check does not apply
!        to "POINT" or "SPHERE" shape targets, and so will not
!        execute for this version of the routine.
!
!-    SPICELIB Version 1.0.0, 14-APR-2008 (NJB) (EDW)
!
!-&


! $ Index_Entries
!
!     angular separation initialization routine
!
!-&


      if ( return () ) return

      call bods2c ( of(1), svbod1, found )

      if ( .not. found ) then

         call setmsg( 'The object name for target 1, ' &
            //         '''#'', is not a recognized name for an ' &
            //         'ephemeris object. The cause of this ' &
            //         'problem may be that you need an updated ' &
            //         'version of the SPICE Toolkit.'        )
         call errch ( '#', of( 1 )                            )
         call sigerr( 'SPICE(IDCODENOTFOUND)'                 )
         return

      end if


      call bods2c ( of(2), svbod2, found )

      if ( .not. found ) then

         call setmsg( 'The object name for target 2, ' &
            //         '''#'', is not a recognized name for an ' &
            //         'ephemeris object. The cause of this ' &
            //         'problem may be that you need an updated ' &
            //         'version of the SPICE Toolkit.'        )
         call errch ( '#', of ( 2 )                           )
         call sigerr( 'SPICE(IDCODENOTFOUND)'                 )
         return

      end if


      call bods2c ( from, svobs, found )

      if ( .not. found ) then

         call setmsg ( 'The object name for the observer, ' &
            //            '''#'', is not a recognized name for an ' &
            //            'ephemeris object. The cause of this ' &
            //            'problem may be that you need an updated ' &
            //            'version of the SPICE Toolkit.'          )
         call errch  ( '#', from                                   )
         call sigerr ( 'SPICE(IDCODENOTFOUND)'                     )
         return

      end if


!
!     Confirm the three bodies have unique IDs.
!
      if      (       ( svobs  == svbod1 ) &
                 .or. ( svobs  == svbod2 ) &
                 .or. ( svbod1 == svbod2) ) then

         call setmsg( 'All three objects associated ' &
            //           'with an ANGULAR SEPARATION search must be ' &
            //           'distinct. The objects whose angular ' &
            //           'separation is of interest were # and #. ' &
            //           'The observer was #.'                      )
         call errint( '#', svbod1                                   )
         call errint( '#', svbod2                                   )
         call errint( '#', svobs                                    )
         call sigerr( 'SPICE(BODIESNOTDISTINCT)'                    )
         return

      end if

!
!     Squeeze all blanks out of the aberration correction
!     string; ensure the string is in upper case.
!
      call cmprss ( ' ', 0, abcorr, svabcr )
      call ucase  ( svabcr, svabcr )

!
!     Check the aberration correction. If SPKEZR can't handle it,
!     neither can we.
!
      call zzvalcor ( svabcr, attblk )

      if ( failed() ) return

      svref  = ref
      svref1 = frame(1)
      svref2 = frame(2)

!
!     Check shapes...
!
      call ljust ( shape(1), shape(1) )
      call ucase ( shape(1), shape(1) )

!
!     If we pass the error check, then SHAPE(1) exists in SVSHAP.
!
      svshp1 = isrchc( shape(1), spshpn, svshap)

      if ( svshp1 == 0 ) then

         call setmsg ( 'The body shape, # is not recognized.  ' &
            //         'Supported quantities are: ' &
            //         'POINT, SPHERE.'                        )
         call errch  ( '#', shape(1)                           )
         call sigerr ( 'SPICE(NOTRECOGNIZED)'                  )
         return

      else if ( svshp1 == point ) then

         svrad1 = 0.0_wp

      else if ( svshp1 == sphere ) then

         call zzgftreb( svbod1, axes1 )

         if (  failed() ) return

         svrad1 = max( axes1(1), axes1(2), axes1(3) )

      else

!
!        This code executes only if someone adds a new shape
!        name to SVSHAP then fails to update the SVSHP1 condition
!        block to respond to the name. Fortran needs SWITCH...CASE.
!
         call setmsg ('Encountered uncoded shape ID for #. '     // &
                      'This indicates a bug. Please contact NAIF.')
         call errch  ('#', shape(1)                               )
         call sigerr ('SPICE(BUG)'                                )
         return

      end if

      call ljust ( shape(2), shape(2) )
      call ucase ( shape(2), shape(2) )

!
!     If we pass the error check, then SHAPE(2) exists in SVSHAP.
!
      svshp2 = isrchc( shape(2), spshpn, svshap)

      if ( svshp2 == 0 ) then

         call setmsg ( 'The body shape, # is not '                 // &
                       'recognized.  Supported quantities are: '   // &
                       'POINT, SPHERE.'                             )
         call errch ( '#', shape(2)                                 )
         call sigerr( 'SPICE(NOTRECOGNIZED)'                        )
         return

      else if ( svshp2 == point ) then

         svrad2 = 0.0_wp

      else if ( svshp2 == sphere ) then

         call zzgftreb( svbod2, axes2)

         if (  failed() ) return

         svrad2 = max( axes2(1), axes2(2), axes2(3) )

      else

!
!        This code executes only if someone adds a new shape
!        name to SVSHAP then fails to update the SVSHP2 condition
!        block to respond to the name. Fortran needs SWITCH...CASE.
!
         call setmsg ('Encountered uncoded shape ID for #. '     // &
                      'This indicates a bug. Please contact NAIF.')
         call errch  ('#', shape(2)                               )
         call sigerr ('SPICE(BUG)'                                )
         return

      end if


!
!     Confirm the center of the input reference frames correspond
!     to the target bodies for non-point, non-spherical bodies.
!
!        FRAME1 centered on TARG1
!        FRAME2 centered on TARG2
!
!     This check does not apply to POINT or SPHERE shapes.
!

      if ( (svshp1 /= point)  .and. (svshp1 /= sphere) ) then

         call namfrm ( svref1, fcode1 )

         call frinfo ( fcode1, ctr1, class, clssid, found )

         if ( .not. found ) then

            call setmsg ( 'Frame system did not recognize frame #.' )
            call errch  ( '#', svref1                               )
            call sigerr ( 'SPICE(NOFRAME)'                          )
            return

         end if

         if ( svbod1 /= ctr1 ) then

            call setmsg ( 'The reference frame #1 associated with ' // &
                      'target body #2 is not centered on #2. The '  // &
                      'frame must be centered on the target body.' )

            call errch  ( '#1', svref1                     )
            call errch  ( '#2', of(1)                      )
            call sigerr ( 'SPICE(INVALIDFRAME)'            )
            return

         end if

      end if


      if ( (svshp2 /= point)  .and. (svshp2 /= sphere) ) then

         call namfrm ( svref2, fcode2 )

         call frinfo ( fcode2, ctr2, class, clssid, found )

         if ( .not. found ) then

            call setmsg ( 'Frame system did not recognize frame #.' )
            call errch  ( '#', svref2                               )
            call sigerr ( 'SPICE(NOFRAME)'                          )
            return

         end if

         if ( svbod2 /= ctr2 ) then

            call setmsg ( 'The reference frame #1 associated with ' // &
                      'target body #2 is not centered on #2. The '  // &
                      'frame must be centered on the target body.' )

            call errch  ( '#1', svref2                     )
            call errch  ( '#2', of(2)                      )
            call sigerr ( 'SPICE(INVALIDFRAME)'            )
            return

         end if

      end if

      return




!$Procedure ZZGFSPDC ( Private - GF, angular separation decreasing)

      entry zzgfspdc ( udfunc, et, decres )

! $ Abstract
!
!     Computes whether or not the angular separation between SVBOD1 and
!     SVBOD2 is decreasing at time ET.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ANGLE
!     GEOMETRY
!     ROOT
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
!     DECRES     O   .TRUE. if angular separation is decreasing .FALSE.
!                    otherwise.
!
! $ Detailed_Input
!
!     ET       time in seconds past J2000 at which one wishes to
!              determine whether or not the angular separation of the
!              two bodies is decreasing.
!
! $ Detailed_Output
!
!     DECRES   is .TRUE. if the angular separation between the objects
!              is decreasing. Otherwise it is .FALSE.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the observer is inside one of the objects, the object will
!         be regarded as having a 90 degree apparent radius.
!
! $ Files
!
!     None.
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
!     None.
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.0.1, 30-MAY-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 2.0.0, 18-FEB-2011 (EDW)
!
!        Added UDFUNC to argument list for use of ZZGFRELX when
!        calculating the events.
!
!-    SPICELIB Version 1.0.1, 06-JUL-2009 (NJB) (EDW)
!
!        Rename of the ZZDHA call to DHFA.
!
!-    SPICELIB Version 1.0.0, 29-APR-2008 (NJB)
!
!-&


! $ Index_Entries
!
!     angular separation is decreasing
!
!-&


!
!     Standard SPICE error handling.
!
      if ( return () ) return

      call spkez ( svbod1, et, svref, svabcr, svobs, pv1, lt )

      if (  failed() ) return

      call spkez ( svbod2, et, svref, svabcr, svobs, pv2, lt )

      if (  failed() ) return

!
!     The angular separation between the bodies has the value
!
!        theta = sep - alpha1 - alpha2
!
!     With alpha1 the half angle of SVBOD1, alpha2 the half
!     angle of SVBOD2, half angle defined as (for spheres):
!
!        sin(alpha) = body_radius
!                     -----------
!                     range_to_body
!
!     The corresponding time derivative of theta:
!
!        d(theta) = d(sep) - d(alpha1) - d(alpha2)
!        --------   ------   ---------   ---------
!        dt         dt       dt          dt
!
!     Note, alpha1, alpha2 and their derivatives have value zero
!     for point objects.
!
      dtheta = dvsep(pv1, pv2)

!
!     Check for a failure caused by a numerical event.
!
      if (  failed() ) then
         decres = .true.
         return
      end if

      dtheta = dtheta - dhfa(pv1, svrad1) - dhfa(pv2, svrad2)

      if ( dtheta < 0 ) then
         decres = .true.
      else
         decres = .false.
      end if

      return




!$Procedure ZZGFSPGQ ( Private - GF, calculate angular separation )

      entry zzgfspgq ( et, sep )

! $ Abstract
!
!     Determine the angular separation between the limbs of the two
!     bodies.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ANGLE
!     GEOMETRY
!     ROOT
!
! $ Declarations
!
!      DOUBLE PRECISION      ET
!      DOUBLE PRECISION      SEP
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ET         I   Ephemeris seconds past J2000 TDB.
!     SEP        O   Separation at time ET.
!
! $ Detailed_Input
!
!     ET       time in ephemeris seconds past J2000 when the
!              angular separation between the two bodies is
!              to be computed.
!
! $ Detailed_Output
!
!     SEP      is the angular separation between SVBOD1 and SVBOD2 as
!              seen from SVOBS at time ET.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     None.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine determines the apparent angular separation between
!     the limbs of bodies SVBOD1 and SVBOD2 as seen from SVOBS at
!     time ET.
!
! $ Examples
!
!     None.
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
!     L.S. Elson         (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 3.0.0, 30-MAY-2021 (EDW) (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!        Routine now calls ZZSEPQ, a copy of ZZGFSPQ cast as
!        a function.
!
!-    SPICELIB Version 2.0.0, 17-FEB-2011 (EDW)
!
!        Routine renamed from ZZGFGSEP to ZZGFSPGQ to match geometry
!        finder naming convention.
!
!-    SPICELIB Version 1.0.0, 26-AUG-2003 (LSE)
!
!-&


! $ Index_Entries
!
!     angular separation between two bodies
!
!-&


      sep = zzsepq ( et, &
                     svbod1, svbod2, &
                     svrad1, svrad2, &
                     svobs,  svabcr, svref )

      return




!$Procedure ZZGFSPX ( Private -- GF, retrieve ZZGFSPIN values )

      entry zzgfspx ( xabcr, xbod, yref, xref, &
                      xobs,  xrad, xshp )

! $ Abstract
!
!     Retrieve values set in ZZGFSPIN.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     None.
!
! $ Declarations
!
!     CHARACTER*(*)         XABCR
!     INTEGER               XBOD  (2)
!     CHARACTER*(*)         YREF
!     CHARACTER*(*)         XREF  (2)
!     INTEGER               XOBS
!     DOUBLE PRECISION      XRAD  (2)
!     INTEGER               XSHP  (2)
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     XABCR      O   Saved value SVABCR.
!     XBOD       O   Saved values SVBOD1 and SVBOD2.
!     YREF       O   Saved value SVREF.
!     XREF       O   Saved values SVREF1 and SVREF2.
!     XOBS       O   Saved value SVOBS.
!     XRAD       O   Saved values SVRAD1 and SVRAD2.
!     XSHP       O   Saved values SVSHP1 and SVSHP2.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     XABCR    initialized via ZZGFSPIN.
!
!     XBOD1    initialized via ZZGFSPIN.
!
!     XBOD2    initialized via ZZGFSPIN.
!
!     YREF     initialized via ZZGFSPIN.
!
!     XREF1    initialized via ZZGFSPIN.
!
!     XREF2    initialized via ZZGFSPIN.
!
!     XOBS     initialized via ZZGFSPIN.
!
!     XRAD1    initialized via ZZGFSPIN.
!
!     XRAD2    initialized via ZZGFSPIN.
!
!     XSHP1    initialized via ZZGFSPIN.
!
!     XSHP2    initialized via ZZGFSPIN.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     None.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     None.
!
! $ Examples
!
!     None.
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
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 30-MAY-2021 (EDW) (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!        Completed descriptions for $Brief_I/O.
!
!-    SPICELIB Version 1.0.0, 24-SEP-2012 (EDW)
!
!-&


! $ Index_Entries
!
!     get saved separation angle parameters
!
!-&


      xabcr   = svabcr
      xbod(1) = svbod1
      xbod(2) = svbod2
      yref    = svref
      xref(1) = svref1
      xref(2) = svref2
      xobs    = svobs
      xrad(1) = svrad1
      xrad(2) = svrad2
      xshp(1) = svshp1
      xshp(2) = svshp2

      return
      end
