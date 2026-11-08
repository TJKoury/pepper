!$Procedure ZZPTPL02 ( DSK, map point to plate, type 2 )

      subroutine zzptpl02 ( handle, dladsc, dskdsc, point, &
                            plid,   plate,  verts,  found )
      use pepper_kinds, only: ip, wp
! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Map a point to the nearest plate in a specified type 2 DSK
!     segment.
!
!     The point is expressed in the reference frame of the segment and
!     represents an offset from the center of the segment's reference
!     frame.
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

      include 'dla.inc'
      include 'dskdsc.inc'
      include 'dsk02.inc'
      include 'dsktol.inc'

      integer(ip)           handle
      integer(ip)           dladsc ( * )
      real(wp)              dskdsc ( * )
      real(wp)              point  ( 3 )
      integer(ip)           plid
      integer(ip)           plate  ( 3 )
      real(wp)              verts  ( 3, 3 )
      logical               found

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   DSK file handle.
!     DLADSC     I   DLA descriptor of segment.
!     DSKDSC     I   DSK descriptor of segment.
!     POINT      I   Input point.
!     PLID       O   Plate ID.
!     PLATE      O   Plate, expressed as an array of three vertex IDs.
!     VERTS      O   Vertices of plate.
!     FOUND      O   Found flag.
!     PTMEMM     P   Default margin for point-plate distance.
!     XFRACT     P   Default plate expansion fraction.
!
! $ Detailed_Input
!
!     HANDLE     is the handle of a DSK file containing a type 2 DSK
!                segment. The input point is to be matched with the
!                closest plate in this segment.
!
!     DLADSC     is the DLA descriptor of the DSK segment to be used.
!
!     DSKDSC     is the DSK descriptor of the DSK segment to be used.
!
!     POINT      is a point that is on or very near the surface
!                described by the DSK segment. POINT is expressed in
!                the reference frame of the segment and represents an
!                offset from the center of that frame. The frame center
!                may be distinct from the central body of the segment.
!
! $ Detailed_Output
!
!     PLID       is the ID of the plate nearest to the input point, if
!                such a plate is found. A plate ID is the index of a
!                plate in the set of plates contained in the segment.
!                PLID is 1-based.
!
!                The distances between POINT and the respective plates
!                are computed using the plate expansion factor XFRACT.
!                See the Parameters section below and the routine
!                PLTEXP for details.
!
!                A plate will be found only if POINT is sufficiently
!                close to at least one plate in the segment, when plate
!                expansion is taken into account. A margin is used to
!                determine whether a plate is close enough to the point
!                to be considered as a solution. See the Parameters
!                section for details.
!
!
!     PLATE      is the plate designated by PLID. It is an array of
!                three integers. The integers are the IDs of the
!                plate's vertices.
!
!     VERTS      is an array containing the plate's vertices. These
!                a 3-dimensional, double precision vectors. The Ith
!                vertex is stored in elements
!
!                   VERTS(1:3,I)
!
!     FOUND      is a logical flag that is set to .TRUE. if and only
!                if plate nearest to the input point was found
!
!                The outputs PLID, PLATE, and VERTS are valid if and
!                only if FOUND is .TRUE.
!
! $ Parameters
!
!     See the include file
!
!        dsktol.inc
!
!     for declarations of these parameters. These defaults can be
!     overridden. See DSKSTL for details.
!
!
!     PTMEMM     is the default value of the point-plate membership
!                parameter, which is used to determine whether a point
!                is close enough to a plate to be considered as a
!                solution. Let S be the current value of this
!                parameter; the bounding radius of the segment is
!                multiplied by (1+S) to produce a distance used for
!                this comparison.
!
!     XFRACT     is the default value of an expansion factor applied to
!                each plate before the distance of POINT from the plate
!                is computed. Let S be the current value of this
!                parameter; each plate is expanded by a factor of (1+S)
!                to produce a plate used for the membership test.
!
!                This expansion is performed to keep results of this
!                routine consistent with those of DSKX02, which also
!                performs plate expansion.
!
! $ Exceptions
!
!     1)  If an invalid voxel edge length is detected, the error
!         SPICE(VALUEOUTOFRANGE) is signaled.
!
!     2)  If the coarse voxel scale is zero, the error
!         SPICE(VALUEOUTOFRANGE) is signaled.
!
!     3)  If an unrecognized coordinate system is encountered, the
!         error SPICE(NOTSUPPORTED) is signaled.
!
!     4)  If an error occurs while this routine attempts to read
!         DSK data, the error will be signaled by a routine in
!         the call tree of this routine.
!
! $ Files
!
!     Appropriate kernels must be loaded by the calling program before
!     this routine is called.
!
!     The following data are required:
!
!        - DSK data: the DSK file designated by HANDLE and containing
!          the segment having the DLA descriptor DLADSC must be loaded
!          at the time this routine is called.
!
!     Kernel data are normally loaded once per program run, NOT every
!     time this routine is called.
!
! $ Particulars
!
!     This routine supports geometry routines that must associate
!     a computed surface point with the plate to which that point
!     belongs.
!
! $ Examples
!
!     See usage in ZZDSKNRM.
!
! $ Restrictions
!
!     This is a private routine. It is meant to be used only by the DSK
!     subsystem.
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
!-    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB)
!
!        Corrected typo in comments.
!
!-    SPICELIB Version 1.0.0, 22-FEB-2017 (NJB)
!
!        Added FAILED calls.
!
!        23-AUG-2016 (NJB)
!
!        Bug fix: now saves previous handle and DLA descriptor.
!
!        17-JUN-2016 (NJB)
!
!           Added support for planetodetic coordinates. Updated
!           bounding radius computation for segments using latitudinal
!           coordinates; this routine now calls ZZSEGBOX. Changed
!           implementation to use ZZINVELT and ZZVOXCVO. Updated
!           parameter descriptions.
!
!        07-OCT-2015 (NJB)
!
!           Now uses plate expansion before testing point-plate
!           distance.
!
!        15-DEC-2014 (NJB)
!
!-&

! $ Index_Entries
!
!     map point to plate in type 2 dsk segment
!
!-&


!
!     SPICELIB functions
!
      real(wp)              dpmax
      real(wp)              vdot

      integer(ip)           brckti
      integer(ip)           zzvox2id

      logical               dlassg
      logical               failed
      logical               return

!
!     Local parameters
!
      integer(ip)           bufsiz
      parameter           ( bufsiz = 1000 )

!
!     Local variables
!
      real(wp)              boxctr ( 3 )
      real(wp)              dist
      real(wp)              dmin
      real(wp)              limit
      real(wp)              offset ( 3 )
      real(wp)              maxr
      real(wp)              normal ( 3 )
      real(wp)              pnear  ( 3 )
      real(wp)              pntoff ( 3 )
      real(wp)              ptsrfm
      real(wp)              voxori ( 3 )
      real(wp)              voxsiz
      real(wp)              vrttmp ( 3, 3 )
      real(wp)              xpdfrc
      real(wp)              xverts ( 3, 3 )

      integer(ip)           cgrcor ( 3 )
      integer(ip)           cgrext ( 3 )
      integer(ip)           cgroff ( 3 )
      integer(ip)           cgrptr
      integer(ip)           cgrscl
      integer(ip)           corsys
      integer(ip)           cgrvid
      integer(ip)           i
      integer(ip)           j
      integer(ip)           k
      integer(ip)           n
      integer(ip)           nplate
      integer(ip)           nread
      integer(ip)           pidtmp
      integer(ip)           pltbuf ( bufsiz )
      integer(ip)           pltptr
      integer(ip)           plttmp ( 3 )
      integer(ip)           ptrloc
      integer(ip)           prvdsc ( dladsz )
      integer(ip)           prvhan
      integer(ip)           ptroff
      integer(ip)           remain
      integer(ip)           start
      integer(ip)           vgrcor ( 3 )
      integer(ip)           vgrext ( 3 )
      integer(ip)           vid

      logical               inside
      logical               pass1

!
!     Saved variables
!
      save                  cgrscl
      save                  corsys
      save                  limit
      save                  maxr
      save                  pass1
      save                  prvhan
      save                  prvdsc
      save                  vgrext
      save                  voxori
      save                  voxsiz

!
!     Initial values
!
      data                  limit  / -1.0_wp      /
      data                  pass1  / .true.     /
      data                  prvdsc / dladsz * 0 /
      data                  prvhan / 0          /



      if ( return() ) then
         return
      end if

      call chkin ( 'ZZPTPL02' )

!
!     No plate has been found so far.
!
      found = .false.

!
!     Decide whether we're looking at the segment we saw
!     on the previous call.
!
      if (  pass1  .or. &
            .not.  dlassg( handle, prvhan, dladsc, prvdsc )  ) then
!
!        We'll need to look up the voxel grid parameters for this
!        segment.
!
         call dskd02 ( handle, dladsc, kwvxor, 1, 3, n, voxori )
         call dskd02 ( handle, dladsc, kwvxsz, 1, 1, n, voxsiz )
         call dski02 ( handle, dladsc, kwvgrx, 1, 3, n, vgrext )
         call dski02 ( handle, dladsc, kwcgsc, 1, 1, n, cgrscl )

         if ( failed() ) then
            call chkout ( 'ZZPTPL02' )
            return
         end if


         if ( voxsiz .eq. 0.0_wp ) then

            call setmsg ( 'Voxel edge length is zero; length ' &
            //            'must be positive.'                  )
            call sigerr ( 'SPICE(VALUEOUTOFRANGE)'             )
            call chkout ( 'ZZPTPL02'                           )
            return

         end if

         if ( cgrscl .eq. 0 ) then

            call setmsg ( 'Coarse voxel scale is zero; scale ' &
            //            'must be positive.'                  )
            call sigerr ( 'SPICE(VALUEOUTOFRANGE)'             )
            call chkout ( 'ZZPTPL02'                           )
            return

         end if

         corsys = nint( dskdsc(sysidx) )

         call zzsegbox ( dskdsc, boxctr, maxr )

         if ( failed() ) then
            call chkout ( 'ZZPTPL02' )
            return
         end if

!
!        We successfully obtained the desired segment parameters, so we
!        don't need to execute this code again until the segment
!        changes. Save the current handle and DLA descriptor.
!
         prvhan = handle
         call movei ( dladsc, dladsz, prvdsc )

         pass1  = .false.

      end if

!
!     Look up the point-plate membership margin; compute
!     the distance limit. This call must be made on every
!     call to ZZPTPL02.
!
      call dskgtl ( keyptm, ptsrfm )

      limit = ptsrfm * maxr

!
!     Look up the plate expansion fraction. This call must be made on
!     every call to ZZPTPL02.
!
      call dskgtl ( keyxfr, xpdfrc )

!
!     Find out whether the point is within the volume element
!     bounding the segment.
!
      call zzinvelt ( point,          corsys, dskdsc(paridx), &
                      dskdsc(mn1idx), ptsrfm, 0,              inside )

      if ( failed() ) then
         call chkout ( 'ZZPTPL02' )
         return
      end if


      if ( .not. inside ) then
!
!        The point is too far from the segment to be considered
!        to lie on a plate in that segment.
!
         call chkout ( 'ZZPTPL02' )
         return

      end if

!
!     Map the point to the coordinates of a voxel containing it. If the
!     point is outside the voxel grid, map the point to the closest
!     voxel.
!
      call vsub ( point, voxori, offset )

      do i = 1, 3

         j         = int( offset(i) / voxsiz ) + 1

         vgrcor(i) = brckti( j,  1,  vgrext(i) )

      end do

!
!     Compute the coordinates of the coarse voxel containing the fine
!     voxel we just identified. Get the 1-d offset of the fine voxel
!     relative the coarse voxel; this offset gives us the index of the
!     pointer associating the fine voxel with its plate list. The
!     1-d offset PTROFF is 1-based.
!
      call zzvoxcvo ( vgrcor, vgrext, cgrscl, cgrcor, cgroff, ptroff )

      if ( failed() ) then
         call chkout ( 'ZZPTPL02' )
         return
      end if

!
!     Fetch the pointer from the coarse voxel to the first element of
!     its fine voxel pointer array.
!
!     We'll need the 1-D offset of the coarse voxel from the base of
!     the coarse voxel grid.
!
      do i = 1, 3
         cgrext(i) = vgrext(i) / cgrscl
      end do

      cgrvid = zzvox2id ( cgrcor, cgrext )

      call dski02 ( handle, dladsc, kwcgpt, cgrvid, 1, n, cgrptr )

      if ( failed() ) then
         call chkout ( 'ZZPTPL02' )
         return
      end if

      if ( cgrptr .lt. 1 ) then
!
!        There are no non-empty fine voxels, hence no plates, in the
!        coarse voxel we're looking at.
!
         call chkout ( 'ZZPTPL02' )
         return

      end if
!
!     Look up the pointer to the plate list for this voxel, and if
!     the pointer is non-null, look up the plate count.
!
      ptrloc = (cgrptr - 1) + ptroff

      call dski02 ( handle, dladsc, kwvxpt, ptrloc, 1, n, pltptr )

      if (  failed()  .or.  ( pltptr .lt. 1 )  ) then
         call chkout ( 'ZZPTPL02' )
         return
      end if

      call dski02 ( handle, dladsc, kwvxpl, pltptr, 1, n, nplate )

      if (  failed()  .or.  ( nplate .lt. 1 )  ) then
         call chkout ( 'ZZPTPL02' )
         return
      end if

!
!     Loop through the plates, keeping track of the minimum plate-point
!     distance.
!
      dmin   = dpmax()
      remain = nplate
      nread  = min ( remain, bufsiz )
      i      = 1

      do while ( remain .gt. 0 )
!
!        Look up the current set of plate IDs.
!
         call dski02 ( handle,   dladsc, kwvxpl, &
                       pltptr+i, nread,  n,      pltbuf )

         if ( failed() ) then
            call chkout ( 'ZZPTPL02' )
            return
         end if
!
!        Look up the vertices of each plate in the buffer and find
!        the distance of the point from that plate. Quit if we
!        find a match.
!
         do j = 1, nread

            pidtmp = pltbuf(j)
            start  = ( 3*(pidtmp-1) ) + 1

            call dski02 ( handle, dladsc, kwplat, start, 3, n, plttmp )

            do k = 1, 3

               vid   = plttmp(k)
               start = ( 3*(vid-1) ) + 1

               call dskd02 ( handle, dladsc, kwvert, &
                             start,  3,      n,      vrttmp(1,k) )

            end do

            if ( failed() ) then
               call chkout ( 'ZZPTPL02' )
               return
            end if

!
!           Work with an expanded version of the plate.
!
            call pltexp ( vrttmp, xpdfrc, xverts )

            call pltnrm ( xverts(1,1), xverts(1,2), xverts(1,3), &
                          normal                                )

            if ( failed() ) then
               call chkout ( 'ZZPTPL02' )
               return
            end if

            call vhatip ( normal )

            call vsub ( point, xverts(1,1), pntoff )

            if ( abs( vdot(pntoff,normal) ) .le. limit ) then
!
!              The input point lies in a narrow region of space
!              bounded by two planes, both of which are parallel
!              to the plate. The plate lies between the planes.
!
!              This test does not rule out a comparison between POINT
!              and a distant plate, if POINT is close to the plane
!              containing that plate. However, the proportion of such
!              cases will normally be small.
!
               call pltnp ( point,       xverts(1,1), xverts(1,2), &
                            xverts(1,3), pnear,       dist        )

               if ( failed() ) then
                  call chkout ( 'ZZPTPL02' )
                  return
               end if

            else
               dist = dpmax()
            end if

            if ( dist .le. limit ) then
!
!              We have a reasonable candidate for the closest plate.
!
               found = .true.

               if ( dist .lt. dmin ) then

                  dmin  = dist
                  plid  = pidtmp
!
!                 Set the output vertices to the original version.
!
                  call movei ( plttmp, 3, plate )
                  call moved ( vrttmp, 9, verts )
!
!                 We'll return the above values if we don't find
!                 a better match.
!
               end if

            end if

         end do

!
!        Prepare to read the next set of plate IDs, if any.
!
         remain = remain - nread
         i      = i      + nread
         nread  = min ( remain, bufsiz )

      end do

      call chkout ( 'ZZPTPL02' )
      return
      end
