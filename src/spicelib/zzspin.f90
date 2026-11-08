!$Procedure ZZSPIN ( Separation quantity initializer )

      subroutine zzspin ( targ1,  shape1, frame1, &
                          targ2,  shape2, frame2, &
                          obsrvr, abcorr, &
                          bods,   frames, mxrad, obs )

! $ Abstract
!
!     Check/initialize the quantities describing an angular separation
!     between two spherical or point objects.
!
! $ Required_Reading
!
!     ABCORR.REQ
!
! $ Keywords
!
!     ANGLE
!     GEOMETRY
!
! $ Declarations

      implicit none

      include 'zzabcorr.inc'

      character*(*)         targ1
      character*(*)         shape1
      character*(*)         frame1
      character*(*)         targ2
      character*(*)         shape2
      character*(*)         frame2
      character*(*)         obsrvr
      character*(*)         abcorr
      integer               bods   (2)
      integer               frames (2)
      double precision      mxrad  (2)
      integer               obs

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     TARG1      I   Name for first target
!     SHAPE1     I   Shape corresponding to TARG1
!     FRAME1     I   Body fixed frame for TARG1
!     TARG2      I   Name for second target
!     SHAPE2     I   Shape corresponding to TARG2
!     FRAME2     I   Body fixed frame for TARG2
!     OBSRVR     I   Name for observer body
!     ABCORR     I   Aberration correction flag
!     BODS       O   Array(2) of body/SPK IDs for TARG1 and TARG2
!     FRAMES     O   Array(2) of frame IDs for TARG1 and TARG2
!     MXRAD      O   Array(2) of max radii values for TARG1 and TARG2
!     OBS        O   Body/SPK IDs of OBSRVR
!
! $ Detailed_Input
!
!     TARG1    the string naming the first body of interest. You can
!              also supply the integer ID code for the object as an
!              integer string. For example both 'MOON' and '301'
!              are legitimate strings that indicate the moon is the
!              target body.
!
!     SHAPE1   the string naming the geometric model used to
!              represent the shape of the TARG1 body. Models
!              supported by this routine:
!
!                'SPHERE'        Treat the body as a sphere with
!                                radius equal to the maximum value of
!                                BODYnnn_AXES.
!
!                'POINT'         Treat the body as a point;
!                                radius has value zero.
!
!              The SHAPE1 string lacks sensitivity to case, leading
!              and trailing blanks.
!
!     FRAME1   the string naming the body-fixed reference frame
!              corresponding to TARG1. This routine does not currently
!              use this argument's value, its use is reserved for
!              future shape models. The value 'NULL' will suffice for
!              "POINT" and "SPHERE" shaped bodies.
!
!     TARG2    the string naming the second body of interest. You can
!              also supply the integer ID code for the object as an
!              integer string. For example both 'MOON' and '301'
!              are legitimate strings that indicate the moon is the
!              target body.
!
!     SHAPE2   the string naming the geometric model used to
!              represent the shape of the TARG2 body. Models
!              supported by this routine:
!
!                'SPHERE'        Treat the body as a sphere with
!                                radius equal to the maximum value of
!                                BODYnnn_AXES.
!
!                'POINT'         Treat the body as a point;
!                                radius has value zero.
!
!              The SHAPE2 string lacks sensitivity to case, leading
!              and trailing blanks.
!
!     FRAME2   the string naming the body-fixed reference frame
!              corresponding to TARG2. This routine does not currently
!              use this argument's value, its use is reserved for
!              future shape models. The value 'NULL' will suffice for
!              "POINT" and "SPHERE" shaped bodies.
!
!     OBSRVR   the string naming the observing body. Optionally, you
!              may supply the ID code of the object as an integer
!              string. For example, both 'EARTH' and '399' are
!              legitimate strings to supply to indicate the
!              observer is Earth.
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
!     BODS     the NAIF SPK/body IDs of the two objects for which to
!              determine the angular separation.
!
!     FRAMES   the NAIF frame IDs of the body fixed reference frames
!              of two objects for which to determine the angular
!              separation.
!
!     MXRAD    the maximum radial values of the two objects.
!
!     OBS      the NAIF SPK/body ID of the observer.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the three objects TARG1, TARG2, and OBSRVR are not
!         distinct, the error SPICE(BODIESNOTDISTINCT) is signaled.
!
!     2)  If the SHAPE1 or SHAPE2 values lack a corresponding case
!         block, the error SPICE(BUG) is signaled. This indicates a
!         programming error.
!
!     3)  If the object names for TARG1 TARG2, or OBSRVR cannot resolve
!         to a NAIF body ID, the error SPICE(IDCODENOTFOUND) is
!         signaled.
!
!     4)  If the reference frame associated with TARG1, FRAME1, is not
!         centered on TARG1, or if the reference frame associated with
!         TARG2, FRAME2, is not centered on TARG2, the error
!         SPICE(INVALIDFRAME) is signaled.
!
!     5)  If the frame name for FRAME1 or FRAME2 cannot resolve to a
!         NAIF frame ID, the error SPICE(NOFRAME) is signaled.
!
!     6)  If the body shape for TARG1, SHAPE1, or the body shape for
!         TARG2, SHAPE2, is not recognized, the error
!         SPICE(NOTRECOGNIZED) is signaled.
!
!     7)  If the requested aberration correction ABCORR is not
!         recognized, an error is signaled by a routine in the call tree
!         of this routine.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Please refer to the Aberration Corrections Required Reading
!     (abcorr.req) for detailed information describing the nature and
!     calculation of the applied corrections.
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
!-    SPICELIB Version 1.0.0, 16-JAN-2021 (EDW) (JDR)
!
!        Based on code originally in zzgfspu.f.
!
!-&


! $ Index_Entries
!
!     check quantities for apparent relative angular separation
!
!-&


!
!     SPICELIB functions.
!
      integer               isrchc

      logical               failed
      logical               return
      logical               found

!
!     Local parameters
!
      integer               shapln
      parameter           ( shapln = 6 )


!
!     Local Variables
!
      character*(shapln)    shap1
      character*(shapln)    shap2

      double precision      axes1 (3)
      double precision      axes2 (3)

      integer               class
      integer               clssid
      integer               ctr1
      integer               ctr2
      integer               shp1
      integer               shp2

      logical               attblk ( nabcor )

!
!     Below we initialize the list of shape names.
!
      integer               spshpn
      parameter           ( spshpn = 2 )

      character*(32)        svshap ( spshpn )

!
!     Define integer ID parameters for the shape names in
!     SVSHAP.
!
      integer               point
      parameter           ( point = 1 )

      integer               sphere
      parameter           ( sphere = 2 )

      save                  svshap

      data                  svshap / 'POINT', &
                                     'SPHERE' /

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

      call chkin( 'ZZSPIN' )

!
!     FRAMES argument not currently used.
!
      frames(1) = -1
      frames(2) = -1

      call bods2c ( targ1, bods(1), found )

      if ( .not. found ) then

         call setmsg( 'The object name for target 1, ' &
            //         '''#'', is not a recognized name for an ' &
            //         'ephemeris object. The cause of this ' &
            //         'problem may be that you need an updated ' &
            //         'version of the SPICE Toolkit.'        )
         call errch ( '#', targ1                              )
         call sigerr( 'SPICE(IDCODENOTFOUND)'                 )
         call chkout( 'ZZSPIN'                                   )
         return

      end if


      call bods2c ( targ2, bods(2), found )

      if ( .not. found ) then

         call setmsg( 'The object name for target 2, ' &
            //         '''#'', is not a recognized name for an ' &
            //         'ephemeris object. The cause of this ' &
            //         'problem may be that you need an updated ' &
            //         'version of the SPICE Toolkit.'        )
         call errch ( '#', targ2                              )
         call sigerr( 'SPICE(IDCODENOTFOUND)'                 )
         call chkout( 'ZZSPIN'                                )
         return

      end if


      call bods2c ( obsrvr, obs, found )

      if ( .not. found ) then

         call setmsg ( 'The object name for the observer, ' &
            //            '''#'', is not a recognized name for an ' &
            //            'ephemeris object. The cause of this ' &
            //            'problem may be that you need an updated ' &
            //            'version of the SPICE Toolkit.'          )
         call errch  ( '#', obsrvr                                 )
         call sigerr ( 'SPICE(IDCODENOTFOUND)'                     )
         call chkout ( 'ZZSPIN'                                    )
         return

      end if


!
!     Confirm the three bodies have unique IDs.
!
      if      (       ( obs     .eq. bods(1) ) &
                 .or. ( obs     .eq. bods(2) ) &
                 .or. ( bods(1) .eq. bods(2)) ) then

         call setmsg( 'All three objects associated ' &
            //        'with an ANGULAR SEPARATION calculation ' &
            //        'must be distinct. The objects whose angular ' &
            //        'separation is of interest were # and #. ' &
            //        'The observer was #.'                       )
         call errint( '#', bods(1)                                )
         call errint( '#', bods(2)                                )
         call errint( '#', obs                                    )
         call sigerr( 'SPICE(BODIESNOTDISTINCT)'                  )
         call chkout( 'ZZSPIN'                                    )
         return

      end if

!
!     Check the aberration correction. If SPKEZR can't handle it,
!     neither can we.
!
      call zzvalcor ( abcorr, attblk )

      if ( failed() ) then
         call chkout ( 'ZZSPIN' )
         return
      end if


!
!     Check first shape.
!
      call ljucrs ( 1, shape1, shap1 )

!
!     If we pass the error check, then SHAPE1 exists in SVSHAP.
!
      shp1 = isrchc( shap1, spshpn, svshap)

      if ( shp1 .eq. 0 ) then

         call setmsg ( 'The body shape, # is not recognized.  ' &
            //         'Supported quantities are: ' &
            //         'POINT, SPHERE.'                      )
         call errch  ( '#', shap1                            )
         call sigerr ( 'SPICE(NOTRECOGNIZED)'                )
         call chkout ( 'ZZSPIN'                              )
         return

      else if ( shp1 .eq. point ) then

         mxrad(1) = 0.d0

      else if ( shp1 .eq. sphere ) then

         call zzgftreb( bods(1), axes1 )

         if (  failed() ) then
            call chkout( 'ZZSPIN' )
            return
         end if

         mxrad(1) = max( axes1(1), axes1(2), axes1(3) )

      else

!
!        This code executes only if someone adds a new shape
!        name to SVSHAP then fails to update the SHP1 condition
!        block to respond to the name. Fortran needs SWITCH...CASE.
!
         call setmsg ('Encountered uncoded shape ID for #. '     // &
                      'This indicates a bug. Please contact NAIF.')
         call errch  ('#', shap1                                  )
         call sigerr ('SPICE(BUG)'                                )
         call chkout ( 'ZZSPIN'                                   )
         return

      end if

!
!     Check second shape.
!
      call ljucrs ( 1, shape2, shap2 )

!
!     If we pass the error check, then SHAPE2 exists in SVSHAP.
!
      shp2 = isrchc( shap2, spshpn, svshap)

      if ( shp2 .eq. 0 ) then

         call setmsg ( 'The body shape, # is not '                 // &
                       'recognized.  Supported quantities are: '   // &
                       'POINT, SPHERE.'                             )
         call errch ( '#', shap2                                    )
         call sigerr( 'SPICE(NOTRECOGNIZED)'                        )
         call chkout( 'ZZSPIN'                                      )
         return

      else if ( shp2 .eq. point ) then

         mxrad(2) = 0.d0

      else if ( shp2 .eq. sphere ) then

         call zzgftreb( bods(2), axes2 )

         if (  failed() ) then
            call chkout( 'ZZSPIN' )
            return
         end if

         mxrad(2) = max( axes2(1), axes2(2), axes2(3) )

      else

!
!        This code executes only if someone adds a new shape
!        name to SVSHAP then fails to update the SHP2 condition
!        block to respond to the name. Fortran needs SWITCH...CASE.
!
         call setmsg ('Encountered uncoded shape ID for #. '     // &
                      'This indicates a bug. Please contact NAIF.')
         call errch  ('#', shap2                                  )
         call sigerr ('SPICE(BUG)'                                )
         call chkout ( 'ZZSPIN'                                   )
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

      if ( (shp1 .ne. point)  .and. (shp1 .ne. sphere) ) then

         call namfrm ( frame1, frames(1) )

         call frinfo ( frames(1), ctr1, class, clssid, found )

         if ( .not. found ) then

            call setmsg ( 'Frame system did not recognize frame #.' )
            call errch  ( '#', frame1                               )
            call sigerr ( 'SPICE(NOFRAME)'                          )
            call chkout ( 'ZZSPIN'                                  )
            return

         end if

         if ( bods(1) .ne. ctr1 ) then

            call setmsg ( 'The reference frame #1 associated with ' // &
                      'target body #2 is not centered on #2. The '  // &
                      'frame must be centered on the target body.' )

            call errch  ( '#1', frame1                     )
            call errch  ( '#2', targ1                      )
            call sigerr ( 'SPICE(INVALIDFRAME)'            )
            call chkout ( 'ZZSPIN'                         )
            return

         end if

      end if


      if ( (shp2 .ne. point)  .and. (shp2 .ne. sphere) ) then

         call namfrm ( frame2, frames(2) )

         call frinfo ( frames(2), ctr2, class, clssid, found )

         if ( .not. found ) then

            call setmsg ( 'Frame system did not recognize frame #.' )
            call errch  ( '#', frame2                               )
            call sigerr ( 'SPICE(NOFRAME)'                          )
            call chkout ( 'ZZSPIN'                                  )
            return

         end if

         if ( bods(2) .ne. ctr2 ) then

            call setmsg ( 'The reference frame #1 associated with ' // &
                      'target body #2 is not centered on #2. The '  // &
                      'frame must be centered on the target body.' )

            call errch  ( '#1', frame2                     )
            call errch  ( '#2', targ2                      )
            call sigerr ( 'SPICE(INVALIDFRAME)'            )
            call chkout ( 'ZZSPIN'                         )
            return

         end if

      end if

!
!     Done. Check out. Return.
!

      call chkout ('ZZSPIN')
      return
      end
