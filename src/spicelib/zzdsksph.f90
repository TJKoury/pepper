!$Procedure ZZDSKSPH ( DSK, bounding spheres for target body )

      subroutine zzdsksph ( bodyid, nsurf, srflst, minrad, maxrad )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Return radii of outer and inner bounding spheres for a given body
!     and surface list. The shape of the body is represented by DSK
!     data. The outputs of this routine are time-independent.
!
! $ Required_Reading
!
!     DSK
!
! $ Keywords
!
!     DSK
!     GEOMETRY
!     SURFACE
!     TOPOGRAPHY
!
! $ Declarations

      implicit none

      include 'errhnd.inc'
      include 'dla.inc'
      include 'dsk.inc'
      include 'dskdsc.inc'
      include 'zzctr.inc'

      integer(ip)           bodyid
      integer(ip)           nsurf
      integer(ip)           srflst ( * )
      real(wp)              minrad
      real(wp)              maxrad

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     BODYID     I   ID code of target body.
!     NSURF      I   Number of IDs in surface list.
!     SRFLST     I   List of surface IDs.
!     MINRAD     O   Radius of inner bounding sphere for body.
!     MAXRAD     O   Radius of outer bounding sphere for body.
!
! $ Detailed_Input
!
!     BODYID     is the body ID of the target for which radii
!                of bounding spheres are to be generated.
!
!     NSURF,
!     SRFLST     are, respectively, the surface list count and
!                an array containing a list of surface IDs.
!
!                If the count is zero, all surfaces for the body
!                are considered applicable.
!
! $ Detailed_Output
!
!     MINRAD     is the radius of an inner bounding sphere for the
!                surface of the body designated by BODYID, NSURF,
!                and SRFLST. The sphere is centered at the target
!                body's center. All points of the body's surface
!                are outside this sphere.
!
!                MINRAD is not necessarily a maximum lower bound.
!
!                Units are km.
!
!
!     MAXRAD     is the radius of an outer bounding sphere for the
!                surface of the body designated by BODYID, NSURF,
!                and SRFLST. The sphere is centered at the target
!                body's center. All points of the body's surface
!                are contained in this sphere.
!
!                MINRAD is not necessarily a minimum upper bound.
!
!                Units are km.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If, for a DSK segment's reference frame, frame information
!         cannot be looked up, the error SPICE(NOFRAMEDATA) is
!         signaled.
!
!     2)  If, for a DSK segment's reference frame, the frame's name
!         cannot be looked up, the error SPICE(FRAMENAMENOTFOUND) is
!         signaled.
!
!     3)  If a DSK segment descriptor has an unrecognized coordinate
!         system code, the error SPICE(NOTSUPPORTED) is signaled.
!
!     4)  If no DSK segments are found for the specified target
!         and surface set, the error SPICE(DSKDATANOTFOUND) is
!         signaled.
!
!     5)  If an error occurs while looking up DSK descriptors,
!         the error will be diagnosed by routines in the call
!         tree of this routine.
!
!     6)  If the surface list size is negative, the error
!         SPICE(VALUEOUTOFRANGE) is signaled.
!
! $ Files
!
!     This routine makes use of DSK files loaded by the ZZDSKBSR
!     subsystem.
!
!     If any loaded DSK segment has a reference frame that is not
!     centered at the segment's central (target) body, SPK data are
!     required to compute the offset between the frame's center and
!     the segment's center. The lookup epoch for a segment is the
!     midpoint of the segment's time coverage interval, so SPK data
!     must be available for that epoch.
!
!     Frame kernels may be required in order to look up a segment's
!     frame center offset. In some cases, additional kernels such
!     as CK kernels and SCLK kernels could be required to support
!     the offset vector lookup.
!
! $ Particulars
!
!     This routine is used as an initialization step by the ZZDSKSBF
!     entry point ZZSUDSKI.
!
!     The operation of this routine usually will result in physical
!     file reads, and so will be rather slow.
!
! $ Examples
!
!     See usage in ZZSUDSKI.
!
! $ Restrictions
!
!     1)  This routine does not take time into account when it
!         selects segments. All segments for the specified body
!         and surface list are selected.
!
!         This functionality might be inappropriate for some
!         future application.
!
!     2)  This is a private routine. It is meant to be used only by the
!         DSK subsystem.
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
!-    SPICELIB Version 1.1.0, 11-OCT-2021 (NJB)
!
!        Added and moved calls to FAILED() in order to prevent use
!        of uninitialized value of SEGFND. Added SAVE statement for
!        variable PRVFID.
!
!-    SPICELIB Version 1.0.0, 26-JUL-2016 (NJB)
!
!        Updated to use reference ellipsoid parameters and altitude
!        bounds to compute candidate values for bounding radii. This
!        applies to the planetodetic coordinate system.
!
!        30-JUN-2016 (NJB)
!
!        30-JAN-2015 (NJB)
!
!           Updated to provide inner radius in addition to outer
!           radius. Updated to support segment frame centers that don't
!           coincide with the target.
!
!        14-JAN-2015 (NJB)
!
!-&

! $ Index_Entries
!
!     find inner and outer bounding spheres for target body
!
!-&


!
!     SPICELIB functions
!
      real(wp)              dpmax
      real(wp)              vnorm

      integer(ip)           isrchi

      logical               failed
      logical               return

!
!     EXTERNAL routines
!
      external              zzdskbdc

!
!     Local parameters
!
      integer(ip)           frnmln
      parameter           ( frnmln = 32 )

!
!     Local variables
!
      character(lmsgln)     errmsg
      character(frnmln)     frname

      real(wp)              boxctr ( 3 )
      real(wp)              boxrad
      real(wp)              ctrmnr
      real(wp)              dskdsc ( dskdsz )
      real(wp)              f
      real(wp)              lt
      real(wp)              lx
      real(wp)              ly
      real(wp)              lz
      real(wp)              maxr
      real(wp)              midtim
      real(wp)              minr
      real(wp)              offmag
      real(wp)              offset ( 3 )
      real(wp)              re
      real(wp)              rp
      real(wp)              sgmaxr
      real(wp)              sgminr
      real(wp)              svmaxr
      real(wp)              svminr

      integer(ip)           corsys
      integer(ip)           ctr    ( ctrsiz )
      integer(ip)           dladsc ( dladsz )
      integer(ip)           framid
      integer(ip)           frcent
      integer(ip)           frclid
      integer(ip)           frclas
      integer(ip)           handle
      integer(ip)           i
      integer(ip)           prvbod
      integer(ip)           prvfid
      integer(ip)           prvlst ( maxsrf )
      integer(ip)           prvnls
      integer(ip)           surfid

      logical               first
      logical               found
      logical               newlst
      logical               same
      logical               segfnd
      logical               update

!
!     Saved variables
!
      save                  ctr
      save                  first
      save                  prvbod
      save                  prvfid
      save                  prvlst
      save                  prvnls
      save                  svmaxr
      save                  svminr

!
!     Initial values
!
      data                  ctr    / -1, -1      /
      data                  first  / .true.      /
      data                  prvfid /  0          /
      data                  prvbod /  0          /
      data                  prvlst /  maxsrf * 0 /
      data                  prvnls / -1          /
      data                  svmaxr / -1.0_wp       /
      data                  svminr / -1.0_wp       /



      if ( return() ) then
         return
      end if

      call chkin ( 'ZZDSKSPH' )

      if ( first ) then
         call zzctruin ( ctr )
      end if

!
!     Check NSURF.
!
      if ( nsurf < 0 ) then

         call setmsg ( 'NSURF must be non-negative but was #.' )
         call errint ( '#', nsurf                              )
         call sigerr ( 'SPICE(VALUEOUTOFRANGE)'                )
         call chkout ( 'ZZDSKSPH'                              )
         return

      end if

!
!     Determine whether the input body surface list matches
!     the previous values. The following code applies whether
!     or not the surface list is non-empty.
!
      newlst = .true.

      if ( .not. first ) then

         if ( bodyid == prvbod ) then

            if ( nsurf == prvnls ) then

               same = .true.
               i    = 1

               do while ( ( i <= nsurf ) .and.  same  )

                  same = srflst(i) == prvlst(i)
                  i    = i + 1
               end do
!
!              If SAME is true here, the body and surface list are the
!              same as on the previous call.
!
               newlst = .not. same

            end if

         end if

      end if

!
!     Set PRVNLS to a value that can't match a valid value, so
!     the surface list won't match after an error occurs. We'll
!     reset PRVNLS prior to exit if all goes well.
!
      prvnls = -1

!
!     Check for DSK update in ZZDSKBSR.
!
      call zzdskchk ( ctr, update )

!
!     Initialize the temporary variables MINR, MAXR.
!
      minr = svminr
      maxr = svmaxr


      if ( first .or. update .or. newlst ) then
!
!        Initialize the saved radius data.
!
         svmaxr = -1.0_wp
         svminr = dpmax()

!
!        Prepare to fetch segment data. Initialize the ZZDSKBSR
!        segment list for the body of interest.
!
         call zzdskbbl ( bodyid )

         if ( failed() ) then
            call chkout ( 'ZZDSKSPH' )
            return
         end if

!
!        Fetch segment DSK descriptors for the indicated body and
!        surface list.
!
         prvfid = 0
         call cleard ( 3, offset )

!
!        Re-initialize MINR and MAXR.
!
         maxr = -1.0_wp
         minr = dpmax()

!
!        Examine all segments for BODYID.
!
         call zzdskbss ( bodyid )

         call zzdsksbd ( bodyid )
         call zzdsksns ( zzdskbdc, handle, dladsc, dskdsc, segfnd )

         if ( failed() ) then
            call chkout ( 'ZZDSKSPH' )
            return
         end if

         do while ( segfnd )

            if ( nsurf > 0 ) then

               surfid = nint( dskdsc(srfidx) )

               i      = isrchi ( surfid, nsurf, srflst )
            else
               i      = 1
            end if

            if ( i > 0 ) then
!
!              If we're checking surface IDs, this segment qualifies.
!              Otherwise, we're not checking surface IDs, so the segment
!              qualifies by default.
!
!              Get the frame ID of this segment, and look up the frame's
!              center.
!
               framid = nint( dskdsc(frmidx) )

               if ( framid /= prvfid ) then
!
!                 Get the frame center for the current segment.
!
                  call frinfo ( framid, frcent, frclas, frclid, found )

                  if ( .not. found ) then

                     call setmsg ( 'No frame specification was found ' &
                     //            'for frame ID #.'                  )
                     call errint ( '#', framid                        )
                     call sigerr ( 'SPICE(NOFRAMEDATA)'               )
                     call chkout ( 'ZZDSKSPH'                         )
                     return

                  end if

                  if ( frcent == bodyid ) then
!
!                    The frame is centered at the target, so
!                    the frame center offset magnitude is zero.
!
                     offmag = 0.0_wp

                  else

                     call frmnam ( framid, frname )

                     if ( failed() ) then
                        call chkout ( 'ZZDSKSPH' )
                        return
                     end if

                     if ( frname == ' ' ) then

                        call setmsg ( 'No frame name was found ' &
                        //            'for frame ID #.'          )
                        call errint ( '#', framid                )
                        call sigerr ( 'SPICE(FRAMENAMENOTFOUND)' )
                        call chkout ( 'ZZDSKSPH'                 )
                        return

                     end if

                     midtim = ( dskdsc(btmidx) + dskdsc(etmidx) ) / 2

                     call spkgps ( frcent, midtim, frname, &
                                   bodyid, offset, lt     )

                     if ( failed() ) then
                        call chkout ( 'ZZDSKSPH' )
                        return
                     end if

                     offmag = vnorm( offset )

                  end if

               end if

!
!              Get the segment coordinate system and derive the maximum
!              radius of the segment.
!
               corsys = nint( dskdsc(sysidx) )

!
!              Get bounding radii for the segment relative to the
!              origin of the segment's coordinate system. We'll account
!              for the offset of the origin from the segment's central
!              body as a subsequent step.
!
               if ( corsys == latsys ) then

                  sgminr = dskdsc(mn3idx)
                  sgmaxr = dskdsc(mx3idx)


               else if ( corsys == pdtsys ) then
!
!                 Use the reference spheroid and altitude bounds to
!                 generate initial bounding radii.
!
                  re = dskdsc(paridx  )
                  f  = dskdsc(paridx+1)
                  rp = re *  (1.0_wp - f)

                  if ( f >= 0.0_wp ) then
!
!                    The spheroid is oblate. The maximum altitude over
!                    the equator is an upper bound for the distance of
!                    any surface point from the origin. The minimum
!                    altitude over either pole is a lower bound for
!                    the distance of any surface point from the origin.
!
!                    The DSK descriptor gives us the altitude bounds.
!
                     sgmaxr = re + dskdsc(mx3idx)
                     sgminr = rp + dskdsc(mn3idx)

                  else

!                    The spheroid is prolate. The maximum altitude over
!                    either pole is an upper bound for the distance of
!                    any surface point from the origin.
!
                     sgmaxr = rp + dskdsc(mx3idx)
                     sgminr = re + dskdsc(mn3idx)

                  end if



               else if ( corsys == recsys ) then

                  call zzrecbox ( dskdsc(mn1idx), boxctr, lx, &
                                  ly,             lz,     boxrad )
!
!                 SGMINR is a lower bound on the distance of the
!                 segment from the origin of the coordinate system.
!
                  sgminr = max ( vnorm(boxctr) - boxrad,  0.0_wp )

                  sgmaxr =       vnorm(boxctr) + boxrad


               else

                  call setmsg ( 'Coordinate system # is not ' &
                  //            'currently supported.'        )
                  call errint ( '#', corsys                   )
                  call sigerr ( 'SPICE(NOTSUPPORTED)'         )
                  call chkout ( 'ZZDSKSPH'                    )
                  return

               end if

!
!              Apply the triangle inequality to derive minimum and
!              maximum values of the distance of the surface from the
!              body center, given the offset between the frame center
!              and the body center, and given bounds on the distance of
!              the surface from the frame's center.
!
               if ( offmag <= sgminr ) then
!
!                 The segment's central body is inside the inner
!                 bounding sphere of the segment.
!
                  ctrmnr = sgminr - offmag

               else if ( offmag >= sgmaxr ) then
!
!                 The segment's central body is outside the outer
!                 bounding sphere of the segment.
!
                  ctrmnr = offmag - sgmaxr

               else
!
!                 The segment's central body is between the bounding
!                 spheres. No positive lower radius bound exists.
!
                  ctrmnr = 0.0_wp

               end if

!
!              Update the segment's outer bounding radius to
!              account for the frame center offset (which may
!              be zero).
!
               sgmaxr = sgmaxr + offmag

!
!              Update the global minimum and maximum radii.
!
               minr = min( minr, ctrmnr )
               maxr = max( maxr, sgmaxr )

            end if
!
!           Look at the next segment.
!
            call zzdsksbd( bodyid )
            call zzdsksns( zzdskbdc, handle, dladsc, dskdsc, segfnd )

            if ( failed() ) then
               call chkout ( 'ZZDSKSPH' )
               return
            end if

         end do

         if (  ( maxr > 0.0_wp ) .and. (.not. failed() )  ) then
!
!           Update the saved bounds.
!
            svminr = minr
            svmaxr = maxr

         end if

      end if


      if ( maxr < 0.0_wp ) then
!
!        We tried to update the radius bounds but didn't find any
!        segments for the specified body.
!
!        We have no radius data for the specified surface list.
!
         if ( nsurf == 0 ) then

            errmsg = 'No segments were found matching the body ID #.'

         else

            errmsg = 'No segments were found matching the body ID # ' &
            //       'and the surface list <@>.'

            do i = 1, nsurf-1

               call repmc  ( errmsg, '@', '*, @',    errmsg )
               call repmi  ( errmsg, '*', srflst(i), errmsg )

            end do

            call repmi  ( errmsg, '@', srflst(nsurf), errmsg )

         end if

         call setmsg ( errmsg                   )
         call errint ( '#', bodyid              )
         call sigerr ( 'SPICE(DSKDATANOTFOUND)' )
         call chkout ( 'ZZDSKSPH'               )
         return

      end if

      if ( .not. failed() ) then

         first  = .false.

         prvbod = bodyid
         prvnls = nsurf

         if ( newlst ) then

            call movei ( srflst, nsurf, prvlst )

         end if

         maxrad = svmaxr
         minrad = svminr

      end if

      call chkout ( 'ZZDSKSPH' )
      return
      end
