!$Procedure ZZDSKBUX ( DSK, buffered unprioritized ray intercept )

      subroutine zzdskbux ( bodyid, nsurf,  srflst, et,     fixfid, &
                            nseg,   hanbuf, dlabuf, dskbuf, offbuf, &
                            ctrbuf, radbuf, vertex, raydir, xpt, &
                            segidx, dc,     ic,     found          )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Find a ray-surface intercept on the surface of a body represented
!     by one or more DSK segments. If multiple intercepts exist, select
!     the one closest to the ray's vertex. The set of surface IDs to be
!     considered is specified by the caller.
!
!     This routine uses DSK segments in an unprioritized manner.
!     All segments meeting the body, time, and surface constraints
!     are considered.
!
!     Segment descriptor and derived bounding data are passed in.
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
      include 'dsktol.inc'
      include 'zzctr.inc'

      integer(ip)           bodyid
      integer(ip)           nsurf
      integer(ip)           srflst ( * )
      real(wp)              et
      integer(ip)           fixfid
      integer(ip)           nseg
      integer(ip)           hanbuf ( * )
      integer(ip)           dlabuf ( dladsz, * )
      real(wp)              dskbuf ( dskdsz, * )
      real(wp)              offbuf ( 3,      * )
      real(wp)              ctrbuf ( 3,      * )
      real(wp)              radbuf ( * )
      real(wp)              vertex ( 3 )
      real(wp)              raydir ( 3 )
      real(wp)              xpt    ( 3 )
      integer(ip)           segidx
      real(wp)              dc     ( * )
      integer(ip)           ic     ( * )
      logical               found

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     BODYID     I   ID code of target body.
!     NSURF      I   Number of IDs in surface list.
!     SRFLST     I   List of surface IDs.
!     ET         I   Lookup epoch, expressed as seconds past J2000 TDB.
!     FIXFID     I   Frame ID of body-fixed frame for output vectors.
!     NSEG       I   Number of segments in buffers.
!     HANBUF     I   DSK handle buffer.
!     DLABUF     I   DLA segment descriptor buffer.
!     DSKBUF     I   DSK segment descriptor buffer.
!     OFFBUF     I   Frame center offset buffer.
!     CTRBUF     I   Bounding sphere center buffer.
!     RADBUF     I   Bounding sphere radius buffer.
!     VERTEX     I   Ray's vertex.
!     RAYDIR     I   Ray's direction vector.
!     XPT        O   Intercept point.
!     SEGIDX     O   Index of segment in input buffers.
!     FOUND      O   Found flag. True if and only if intercept exists.
!     SGREED     P   Default segment boundary margin.
!
! $ Detailed_Input
!
!     BODYID     is the body ID of the target on which the input
!                point is located.
!
!     NSURF,
!     SRFLST     are, respectively, the surface list count and
!                an array containing a list of surface IDs.
!
!                If the count is zero, all surfaces for the body
!                are considered applicable.
!
!     ET         is the lookup epoch, specified as seconds past
!                J2000 TDB. Only DSK segments containing ET in
!                their time coverage intervals are considered
!                in the normal vector computation.
!
!     FIXFID     is the frame ID of a body-fixed reference frame
!                centered on the target body.
!
!     NSEG       is the number of DSK segments for which the input
!                buffers contain data.
!
!     HANBUF     is the DSK handle buffer.
!
!     DLABUF     is the DLA segment descriptor buffer.
!
!     DSKBUF     is the DSK segment descriptor buffer.
!
!     OFFBUF     is the frame offset buffer. For each DSK segment, the
!                entry in this buffer is the position of that segment's
!                reference frame center relative to the target body's
!                center. The vector is expressed in the segment's
!                reference frame. Units are km.
!
!     CTRBUF     is the bounding sphere center buffer. For each DSK
!                segment, the entry in this buffer is the position of
!                that segment's outer bounding sphere center relative
!                to the segment's reference frame center. The vector is
!                expressed in the segment's reference frame. Units are
!                km.
!
!     RADBUF     is the bounding sphere radius buffer. For each DSK
!                segment, the entry in this buffer is the radius of the
!                segment's outer bounding sphere. Units are km.
!
!     VERTEX,
!     RAYDIR     are, respectively, the vertex and direction vector of
!                the ray to be used in the intercept computation.
!
!                Both the vertex and ray must be represented in the
!                reference frame designated by FIXFID. The vertex is
!                considered to be an offset from the target body.
!
! $ Detailed_Output
!
!     XPT        is the surface intercept, if an intercept exists. If
!                multiple intercepts exist, the one closest to the
!                ray's vertex is selected. XPT is represented in the
!                reference frame designated by FIXFID. XPT is
!                considered to be an offset from the target body.
!
!     SEGIDX     is the index, within the input buffer arguments, of
!                the segment providing the surface data on which XPT
!                was found. SEGIDX is valid if and only if FOUND is
!                .TRUE.
!
!     FOUND      is a logical flag that is .TRUE. if and only if a
!                ray-surface intercept was found.
!
! $ Parameters
!
!     SGREED     is the default margin used to determine whether
!                segments should be tested for intersection. Segments
!                are effectively expanded slightly for intersection
!                tests, using this margin. For example, if a segment
!                has latitudinal coordinates, and if the ray hits the
!                eastern longitude boundary of the segment, the
!                latitude of this intercept is within the segment's
!                latitude bounds, RMAX is the segment's maximum radius,
!                and the radius R of the intercept satisfies
!
!                   RMAX < R < RMAX * (1+SGREED)
!
!                then the intercept is considered to be "on" the
!                segment's boundary, and the segment's surface data
!                are tested for intersection.
!
!                See the routines that test points for inclusion in
!                segments for details concerning use of the segment
!                margin. For latitudinal, planetodetic, and
!                rectangular coordinates respectively, these are
!
!                   ZZINLAT
!                   ZZINPDT
!                   ZZINREC
!
!                See the include file dsktol.inc for the value of
!                this parameter.
!
!                This parameter can be overridden. See the include
!                file dsktol.inc and the routine DSKSTL for details.
!
! $ Exceptions
!
!     1)  If the input segment count is non-positive, the error
!         SPICE(NODSKSEGMENTS) is signaled.
!
!     2)  If an unrecognized coordinate system code is found
!         in a DSK descriptor, the error SPICE(BADCOORDSYS) is
!         signaled.
!
!     3)  If the input point is contained in more than MAXHIT
!         segments, the error SPICE(BUFFERTOOSMALL) is signaled.
!
!     4)  Any other errors that occur while looking up DSK data or
!         mapping the surface point to a normal vector will be signaled
!         by routines in the call tree of this routine.
!
! $ Files
!
!     This routine makes use of DSK files loaded by the ZZDSKBSR
!     subsystem.
!
!     Frame kernels may be required in order to perform transformations
!     from DSK segment frames to the output frame. In some cases,
!     additional kernels such as CK kernels and SCLK kernels could be
!     required to support the offset vector lookup.
!
! $ Particulars
!
!     This routine is meant to be called from the DSK API segment
!     buffering subsystem. All of the input buffers should contain
!     values computed after changes to the set of loaded DSKs.
!
!     This routine takes advantage of the input segment bound
!     information to efficiently determine which segments may be
!     intersected by the input ray.
!
!     This routine uses DSK data in an unprioritized fashion: all
!     DSK segments having the required body, surface, and time
!     attributes are considered in the computation.
!
! $ Examples
!
!     None.
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
!-    SPICELIB Version 1.0.0, 19-JUL-2016 (NJB)
!
!        30-JAN-2015 (NJB)
!
!        Updated argument list: now returns segment
!        index and source info arrays. Added header.
!        Added support for rectangular coordinates.
!
!        30-JAN-2015 (NJB)
!
!        Updated to accommodate change in argument list of
!        ZZDSKSPH.
!
!        28-JAN-2015 (NJB)
!
!        Now uses outer bounding sphere to produce vertex near the
!        target for individual ray-segment intercept computations.
!
!        17-DEC-2014 (NJB)
!
!        Updated to allow DSK segments to use body-fixed frames
!        not centered on the target.
!
!        10-OCT-2014 (NJB)
!
!        First version.
!
!-&

! $ Index_Entries
!
!     buffered unprioritized ray surface intercept
!
!-&

!
!     SPICELIB functions
!
      real(wp)              vdist
      real(wp)              vnorm

      integer(ip)           isrchi

      logical               failed
      logical               return
      logical               vzero

!
!     Local parameters
!
      real(wp), parameter :: radscl = 1.01_wp

      integer(ip), parameter :: maxhit = 1000

!
!     Local variables
!
      real(wp)              dist
      real(wp)              dmin
      real(wp)              maxrad
      real(wp)              minrad
      real(wp)              segdir ( 3 )
      real(wp)              segvtx ( 3 )
      real(wp)              pnear  ( 3 )
      real(wp)              sgdist ( maxhit )
      real(wp)              sgmarg
      real(wp)              sgxbuf ( 3, 3, maxhit )
      real(wp)              sphvtx ( 3 )
      real(wp)              srfx   ( 3 )
      real(wp)              vtemp  ( 3 )
      real(wp)              xform  ( 3, 3 )

      integer(ip)           dtype
      integer(ip)           i
      integer(ip)           iorder ( maxhit )
      integer(ip)           j
      integer(ip)           k
      integer(ip)           nhit
      integer(ip)           nxpts
      integer(ip)           prvfrm
      integer(ip)           segfid
      integer(ip)           sghit  ( maxhit )
      integer(ip)           surfce
      integer(ip)           winner

      logical               bodyok
      logical               done
      logical               multfr
      logical               surfok
      logical               timeok
      logical               xfnd

!
!     Saved variables
!

!
!     Initial values
!
      if ( return() ) return

!
!     Check the incoming segment count.
!
      if ( nseg <= 0 ) then

         call setmsg ( 'Input segment list was empty. This may ' &
         //            'be due to no DSKs containing data for ' &
         //            'body # having been loaded.'            )
         call errint ( '#',  bodyid                            )
         call sigerr ( 'SPICE(NODSKSEGMENTS)'                  )
         return

      end if

!
!     No intercept has been found.
!
      segidx = 0
      found  = .false.

!
!     Indicate we haven't yet seen a segment frame different
!     from the one designated by FIXFID.
!
      multfr = .false.

!
!     Obtain the "greedy" segment margin.
!
      call dskgtl ( keysgr, sgmarg )

!
!     Make a local copy of the ray. We'll update this copy
!     later if need be.
!
      call vequ ( vertex, segvtx )
      call vequ ( raydir, segdir )

!
!     Obtain the radius of an outer bounding sphere for the given body
!     and surface list.
!
      call zzdsksph ( bodyid, nsurf, srflst, minrad, maxrad )

      if ( failed() ) return
!
!     Scale up the bounding sphere to avoid round-off difficulties.
!     We'll use this value in the loop below.
!
      maxrad = maxrad * radscl

!
!     If the ray's vertex is distant from the target, use a vertex on
!     the surface of the outer bounding sphere of the target.
!
!     Note that distant vertices can give rise to large transverse
!     displacements of the ray's intercepts on the segments'
!     boundaries. In cases where the ray intercept is very close to the
!     segment's spatial coverage boundaries, this can cause the ray to
!     miss all plates in type 2 segments, unless a large plate
!     expansion factor is used. Using an intercept on the outer
!     bounding sphere greatly ameliorates this problem.
!
      if ( vnorm(segvtx) > maxrad ) then
!
!        Find the intercept of the ray with the outer bounding
!        sphere. We'll use this intercept as the vertex for
!        later computations.
!
         call surfpt ( segvtx, segdir, maxrad, &
                       maxrad, maxrad, sphvtx, xfnd )

         if ( failed() .or. ( .not. xfnd ) ) then
!
!           It would be highly unusual for the SURFPT call to
!           fail to produce an intercept. Check anyway.
!
            return

         end if

         call vequ ( sphvtx, segvtx )

      end if

!
!     By default, each segment in the input list must be checked
!     for intersection.
!
!     We start out by trying to eliminate segments from consideration
!     by comparing the ray's distance from their centers to the radii
!     of their bounding spheres. Only those segments whose bounding
!     spheres are hit are examined further.
!
      nhit   = 0
      prvfrm = 0

      do i = 1, nseg
!
!        BODYOK indicates whether the input body ID matches that
!        of the current segment.
!
         surfok = .false.
         timeok = .false.

         bodyok = bodyid == nint( dskbuf(ctridx,i) )

         if ( bodyok ) then
!
!           See whether the current segment contains a surface we're
!           supposed to consider. If the surface list is empty, we
!           consider all surfaces. Otherwise, the surface of the
!           segment must be on the surface list in order to qualify.
!
            j = 0

            if ( nsurf > 0 ) then

               surfce = nint( dskbuf(srfidx,i) )
               j      = isrchi( surfce, nsurf, srflst )

            end if

            surfok = ( nsurf == 0 ) .or. ( j > 0 )

!
!           See whether the segment covers the input epoch.
!
            timeok =        ( et >= dskbuf(btmidx,i) ) &
                      .and. ( et <= dskbuf(etmidx,i) )

         end if

         if ( bodyok .and. surfok .and. timeok  ) then
!
!           This segment is to be considered.
!
!           In order to do any geometric comparison, the ray must be in
!           the same frame as the segment we're checking. Transform the
!           input vertex and ray if need be.
!
!           Get the segment's frame ID. Get the transformation from the
!           input frame to the output frame if needed.

            segfid = nint( dskbuf(frmidx,i) )

            if ( segfid /= fixfid ) then
!
!              We have a segment that uses a different frame
!              from that specified by FIXFID.
!
               multfr = .true.

               if ( segfid /= prvfrm ) then
!
!                 The frame of the current segment doesn't match
!                 that of the previous segment, so we'll need
!                 to look up the transformation from the input
!                 frame to the segment frame.
!
!                 Otherwise, XFORM already contains the correct
!                 transformation.
!
                  call refchg ( fixfid, segfid, et, xform )

                  if ( failed() ) return

!
!                 Transform the local copy of the ray to the segment's
!                 frame, and shift the local copy of the ray's vertex
!                 so that it represents an offset relative to center of
!                 the segment's frame.
!
                  call mxv  ( xform, raydir, segdir )
                  call mxv  ( xform, vertex, segvtx )

!
!                 The direction of the buffered offset is from the body
!                 to the segment frame's center. The offset is
!                 expressed in the segment's frame.
!
                  call vsub ( segvtx, offbuf(1,i), vtemp  )
                  call vequ ( vtemp,               segvtx )

               end if

            else if ( multfr ) then
!
!              The input and segment frames are the same for this
!              segment, but the current values of SEGVTX and SEGDIR
!              need to be reset.
!
               call vequ ( vertex, segvtx )
               call vequ ( raydir, segdir )

            end if

!
!           Find the distance of the ray from the "center" of the
!           segment's coverage volume.
!
            call nplnpt ( segvtx, segdir, ctrbuf(1,i), pnear, dist )

            if ( dist <= radbuf(i) ) then
!
!              The line containing the ray intersects the bounding
!              surface. We'll check the boundary of the segment for an
!              intersection.
!
               call zzrytelt ( segvtx, segdir, dskbuf(1,i), &
                               sgmarg, nxpts,  xpt         )

               if ( failed() ) return

               if ( nxpts > 0 ) then
!
!                 The ray hits the boundary of this segment. Save the
!                 index of the segment in the "hit list." Record the
!                 distance from the ray's vertex to the intercept in
!                 the parallel array SGDIST.

                  if ( nhit == maxhit ) then

                     call setmsg ( 'Too many segments were hit by the ' &
                     //            'input ray. Buffer size is #.'      )
                     call errint ( '#', maxhit                         )
                     call sigerr ( 'SPICE(BUFFERTOOSMALL)'             )
                     return

                  end if

                  nhit           = nhit + 1
                  sghit ( nhit ) = i
                  sgdist( nhit ) = vdist( segvtx, xpt )
!
!                 Save the frame transformation for this segment.
!
                  call moved ( xform, 9, sgxbuf(1,1,nhit) )

               end if

            end if
!
!           The current segment matched the input criteria.
!
!           Update the saved segment frame ID to that of the segment
!           we just examined.
!
            prvfrm = segfid

         end if

      end do

!
!     Leave now if no segments were hit.
!
      if ( nhit == 0 ) return

!
!     Find the order of the segments on the hit list, where
!     the metric is the distance of the ray intercepts from
!     the ray's vertex.
!
      call orderd ( sgdist, nhit, iorder )

!
!     Now process the segments on the hit list in order. If we find a
!     surface intercept (that is, a ray intercept with the surface
!     represented by the segment's data, as opposed to the segment's
!     boundary), compare its distance from the ray's vertex to the
!     vertex-boundary distance of the next segment. If the
!     vertex-surface distance is smaller, we terminate the search,
!     since no other segment can contribute a closer intercept.
!
      i      = 1
      done   = .false.
      winner = 0
      prvfrm = 0

      do while ( .not. done )
!
!        J is the index in the hit list of the segment
!        we're considering. K is the index of that segment
!        in the parallel input arrays.
!
         j = iorder(i)
         k = sghit (j)

         segfid = nint( dskbuf(frmidx,k) )

         if ( segfid /= fixfid ) then

            if ( segfid /= prvfrm ) then
!
!              Transform and shift the input ray.
!
!              Here J is an index in the hit list and K is
!              an index in the input arrays.
!
               call moved ( sgxbuf(1,1,j), 9, xform )

               call mxv  ( xform,  vertex, segvtx )
               call mxv  ( xform,  raydir, segdir )

               call vsub ( segvtx, offbuf(1,k), vtemp  )
               call vequ ( vtemp,               segvtx )

            end if

         else if ( multfr ) then

            call vequ ( vertex, segvtx )
            call vequ ( raydir, segdir )

         end if

!
!        Find the surface intercept using the segment topography
!        data.
!
         dtype = nint( dskbuf(typidx,k) )

         call zzdsksgx ( hanbuf(k), dlabuf(1,k), dtype, et, &
                         segvtx,    segdir,      srfx,  dc, &
                         ic,        xfnd                   )

         if ( failed() ) return

         if ( xfnd ) then
!
!           At least one surface intercept exists.
!
            if ( .not. found ) then
!
!              The intercept we just found is the first one, and
!              at least for now, it is the winner.
!
!              Save the intercept and vertex-intercept distance
!              for this segment.
!
               found  = .true.
               dmin   = vdist( srfx, segvtx )

               call vequ ( srfx, xpt )

               winner = j

            else
!
!              At least one surface intercept was found already.
!
!              Compare the vertex-intercept distance for this segment
!              to the best found so far.
!
               dist = vdist( srfx, segvtx )

               if ( dist < dmin ) then
!
!                 This intercept is closer to the ray's vertex than
!                 any we've seen yet. We have a new winner.
!
                  dmin   = dist

                  call vequ ( srfx, xpt )

                  winner = j

               end if

            end if

         end if

!
!        If there's at least one solution in hand, see whether
!        we can stop looking for better solutions.
!
         if ( found ) then

            if ( i < nhit ) then
!
!              There are more segments in the hit list. Compare the
!              minimum vertex-intercept distance of the segments we've
!              checked to the vertex-boundary distance of the next
!              segment.
!
               j = iorder(i+1)

               if ( dmin <= sgdist(j) ) then
!
!                 The best intercept we've found is closer to the
!                 vertex than any intercept that may exist in the
!                 current segment, or any of the remaining segments in
!                 the hit list.
!
                  done  = .true.

               end if

            end if

         end if

         if ( .not. done ) then

            if ( i == nhit ) then
!
!              We've looked at all of the segments.
!
               done = .true.

            else
!
!              Consider the next segment.
!
               i = i + 1

            end if

         end if

      end do

!
!     If we have an intercept, it may be represented in a frame
!     other than the input ray frame. Transform it back to the
!     input frame, and shift it as well if the segment frame and
!     input frame have different centers.
!
      if ( found ) then
!
!        K is the index in the input arrays of the "winning" segment.
!        We'll return this index as SEGIDX.
!
         k      = sghit ( winner )
         segidx = k

         segfid = nint( dskbuf(frmidx,k) )

         if ( segfid /= fixfid ) then
!
!           The segment frame and input frame differ. The intercept
!           must be converted back to the input frame. It also may
!           need to be shifted so that it is expressed as an offset
!           from the target body. If the shift is done, it must be
!           done before the frame transformation, since the offset
!           is expressed relative to the segment's frame.
!
            if ( .not. vzero( offbuf(1,k) ) ) then
!
!              OFFBUF(*,K) contains the offset of the segment frame
!              center from the segment's central body.
!
               call vadd ( xpt,   offbuf(1,k), vtemp )
               call vequ ( vtemp,              xpt   )

            end if

            call moved( sgxbuf(1,1,winner), 9, xform )

            call mtxv ( xform, xpt, vtemp )
            call vequ ( vtemp,      xpt   )

         end if

      end if
!
!     FOUND and XPT are set.
!
      return
      end
