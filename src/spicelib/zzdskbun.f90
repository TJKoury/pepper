!$Procedure ZZDSKBUN ( DSK, buffered unprioritized normal vector )

      subroutine zzdskbun ( bodyid, nsurf,  srflst, et,     fixfid, &
                            nseg,   hanbuf, dlabuf, dskbuf, offbuf, &
                            ctrbuf, radbuf, point,  normal         )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Find the outward unit normal vector at a specified point on the
!     surface of a body represented by one or more DSK segments. The
!     set of surface IDs to be considered is specified by the caller.
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
!     NORMAL
!     RAY
!     SURFACE
!     TOPOGRAPHY
!     VECTOR
!
! $ Declarations

      implicit none

      include 'dla.inc'
      include 'dskdsc.inc'
      include 'dsktol.inc'
      include 'zzctr.inc'

      integer               bodyid
      integer               nsurf
      integer               srflst ( * )
      double precision      et
      integer               fixfid
      integer               nseg
      integer               hanbuf ( * )
      integer               dlabuf ( dladsz, * )
      double precision      dskbuf ( dskdsz, * )
      double precision      offbuf ( 3,      * )
      double precision      ctrbuf ( 3,      * )
      double precision      radbuf ( * )
      double precision      point  ( 3 )
      double precision      normal ( 3 )

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
!     POINT      I   Surface point.
!     NORMAL     O   Outward unit normal vector at surface point.
!
! $ Detailed_Input
!
!     BODYID     is the body ID of the target on which the input
!                point is located.
!
!
!     NSURF,
!     SRFLST     are, respectively, the surface list count and
!                an array containing a list of surface IDs.
!
!                If the count is zero, all surfaces for the body
!                are considered applicable.
!
!
!     ET         is the lookup epoch, specified as seconds past
!                J2000 TDB. Only DSK segments containing ET in
!                their time coverage intervals are considered
!                in the normal vector computation.
!
!
!     FIXFID     is the frame ID of a body-fixed reference frame
!                centered on the target body.
!
!
!     NSEG       is the number of DSK segments for which the input
!                buffers contain data.
!
!
!     HANBUF     is the DSK handle buffer.
!
!
!     DLABUF     is the DLA segment descriptor buffer.
!
!
!     DSKBUF     is the DSK segment descriptor buffer.
!
!
!     OFFBUF     is the frame offset buffer. For each DSK segment, the
!                entry in this buffer is the position of that segment's
!                reference frame center relative to the target body's
!                center. The vector is expressed in the segment's
!                reference frame. Units are km.
!
!
!     CTRBUF     is the bounding sphere center buffer. For each DSK
!                segment, the entry in this buffer is the position of
!                that segment's outer bounding sphere center relative
!                to the segment's reference frame center. The vector is
!                expressed in the segment's reference frame. Units are
!                km.
!
!
!     RADBUF     is the bounding sphere radius buffer. For each DSK
!                segment, the entry in this buffer is the radius of the
!                segment's outer bounding sphere. Units are km.
!
!
!     POINT      is a point on the surface of the target body. POINT is
!                expressed in the reference frame designated by FIXFID.
!                POINT represents an offset from the target body
!                center.
!
!                POINT must be located on the surface of the target
!                body, within a small tolerance.
!
!
! $ Detailed_Output
!
!     NORMAL     is the unit length, outward normal vector on the
!                surface of the target body at POINT. NORMAL is
!                expressed in the reference frame designated by FIXFID.
!
! $ Parameters
!
!     None.
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
!         segments, the error SPICE(TOOMANYHITS) is signaled.
!
!     4)  If the input point is not contained in any DSK segment,
!         the error SPICE(POINTNOTINSEGMENT) is signaled.
!
!     5)  If an unrecognized segment data type is encountered,
!         the error SPICE(NOTSUPPORTED) is signaled.
!
!     6)  If the input point is not sufficiently close to the
!         surface, the error SPICE(POINTOFFSURFACE) will be
!         signaled.
!
!     7)  Any other errors that occur while looking up DSK data or
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
!     information to efficiently determine which segments may contain
!     the input point.
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
!-    SPICELIB Version 1.0.0, 22-FEB-2017 (NJB)
!
!        Added FAILED calls.
!
!        07-APR-2016 (NJB)
!
!        Based on first version 17-DEC-2014 (NJB)
!
!-&

! $ Index_Entries
!
!     compute normals on dsk surface
!
!-&



!
!     SPICELIB functions
!
      double precision      vdist

      integer               isrchi

      logical               failed
      logical               return

!
!     Local parameters
!
      integer               maxhit
      parameter           ( maxhit = 1000 )


!
!     Local variables
!
      double precision      dist
      double precision      segpt  ( 3 )
      double precision      sgmarg
      double precision      sgxbuf ( 3, 3, maxhit )
      double precision      verts  ( 3, 3 )
      double precision      vtemp  ( 3 )
      double precision      xform  ( 3, 3 )

      integer               corsys
      integer               dtype
      integer               i
      integer               j
      integer               nhit
      integer               plate  ( 3 )
      integer               plid
      integer               prvfrm
      integer               segfid
      integer               sghit  ( maxhit )
      integer               surfce
      integer               winner

      logical               bodyok
      logical               done
      logical               inside
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

      if ( return() ) then
         return
      end if

      call chkin ( 'ZZDSKBUN' )

!
!     Check the incoming segment count.
!
      if ( nseg .le. 0 ) then

         call setmsg ( 'Input segment list was empty. This may ' &
         //            'be due to no DSKs containing data for ' &
         //            'body # having been loaded.'            )
         call errint ( '#',  bodyid                            )
         call sigerr ( 'SPICE(NODSKSEGMENTS)'                  )
         call chkout ( 'ZZDSKBUN'                              )
         return

      end if

!
!     Get the segment margin from the tolerance database.
!
      call dskgtl ( keysgr, sgmarg )

!
!     Indicate we haven't yet seen a segment frame different
!     from the one designated by FIXFID.
!
      multfr = .false.

!
!     Make a local copy of the point. We'll update this copy
!     later if need be.
!
      call vequ ( point, segpt )

!
!     By default, each segment in the input list must be checked
!     for intersection.
!
!     We start out by trying to eliminate segments from consideration
!     by comparing the point's distance from their centers to the radii
!     of their bounding spheres. Only those segments whose bounding
!     spheres contain the point are examined further.
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

         bodyok = bodyid .eq. nint( dskbuf(ctridx,i) )


         if ( bodyok ) then
!
!           See whether the current segment contains a surface we're
!           supposed to consider. If the surface list is empty, we
!           consider all surfaces. Otherwise, the surface of the
!           segment must be on the surface list in order to qualify.
!
            j = 0

            if ( nsurf .gt. 0 ) then

               surfce = nint( dskbuf(srfidx,i) )
               j      = isrchi( surfce, nsurf, srflst )

            end if

            surfok = ( nsurf .eq. 0 ) .or. ( j .gt. 0 )

!
!           See whether the segment covers the input epoch.
!
            timeok =        ( et .ge. dskbuf(btmidx,i) ) &
                      .and. ( et .le. dskbuf(etmidx,i) )

         end if


         if ( bodyok .and. surfok .and. timeok  ) then
!
!           This segment is to be considered.
!
!           In order to do any geometric comparison, the point must be
!           in the same frame as the segment we're checking. Transform
!           the point if need be.
!
!           Get the segment's frame ID. Get the transformation from the
!           input frame to the output frame if needed.

            segfid = nint( dskbuf(frmidx,i) )


            if ( segfid .ne. fixfid ) then
!
!              We have a segment that uses a different frame
!              from that specified by FIXFID.
!
               multfr = .true.

               if ( segfid .ne. prvfrm ) then
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

                  if ( failed() ) then
                     call chkout ( 'ZZDSKBUN' )
                     return
                  end if

!
!                 Transform the local copy of the point to the segment's
!                 frame, and shift the local copy of the point so
!                 that it represents an offset relative to center of
!                 the segment's frame.
!
                  call mxv  ( xform, point,  segpt  )

!
!                 The direction of the buffered offset is from the body
!                 to the segment frame's center. The offset is
!                 expressed in the segment's frame.
!
                  call vsub ( segpt, offbuf(1,i), vtemp )
                  call vequ ( vtemp,              segpt )

               end if


            else if ( multfr ) then
!
!              The input and segment frames are the same for this
!              segment, but the current value of SEGPT needs to be
!              reset.
!
               call vequ ( point, segpt )

            end if

!
!           Find the distance of the point from the "center" of the
!           segment's coverage volume.
!
            dist = vdist ( ctrbuf(1,i), segpt )


            if ( dist .le. radbuf(i) ) then
!
!              The point is inside or on the bounding surface. We'll
!              check the boundary of the segment for an intersection.
!
               corsys = nint( dskbuf(sysidx,i) )

               if ( corsys .eq. latsys ) then

                  call zzinlat ( segpt,  dskbuf(mn1idx,i), &
                                 sgmarg, 0,                inside )

               else if ( corsys .eq. recsys ) then

                  call zzinrec ( segpt,  dskbuf(mn1idx,i), &
                                 sgmarg, 0,                inside )

               else if ( corsys .eq. pdtsys ) then

                  call zzinpdt ( segpt,             dskbuf(mn1idx,i), &
                                 dskbuf(paridx, i), sgmarg, &
                                 0,                 inside           )
               else

                  call setmsg( 'Coordinate system # is not supported.' )
                  call errint( '#', corsys                             )
                  call sigerr( 'SPICE(BADCOORDSYS)'                    )
                  call chkout( 'ZZDSKBUN'                              )
                  return

               end if

               if ( failed() ) then
                  call chkout( 'ZZDSKBUN' )
                  return
               end if


               if ( inside ) then
!
!                 The point in inside the region enclosed by the
!                 boundary of this segment. Save the index of the
!                 segment in the "hit list."
!
                  if ( nhit .eq. maxhit ) then

                     call setmsg ( 'Too many segments contain the ' &
                     //            'input point. Buffer size is #.'  )
                     call errint ( '#', maxhit                       )
                     call sigerr ( 'SPICE(TOOMANYHITS)'              )
                     call chkout ( 'ZZDSKBUN'                        )
                     return

                  end if

                  nhit           = nhit + 1
                  sghit ( nhit ) = i
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
!     We have an error if no segments were hit. None of that
!     "not found" nonsense here.
!
      if ( nhit .eq. 0 ) then

         call setmsg ( 'Input point (# # #) in frame # does not ' &
         //            'lie inside any segment for the ' &
         //            'specified body (#) and surfaces.'        )
         call errdp  ( '#', point(1)                             )
         call errdp  ( '#', point(2)                             )
         call errdp  ( '#', point(3)                             )
         call errint ( '#', fixfid                               )
         call errint ( '#', bodyid                               )
         call sigerr ( 'SPICE(POINTNOTINSEGMENT)'                )
         call chkout ( 'ZZDSKBUN' )
         return

      end if


!
!     Now process the segments on the hit list. If we find a segment
!     surface on which the input point is located, we compute the
!     normal vector and terminate the search.
!
      i      = 1
      done   = .false.
      winner = 0
      prvfrm = 0

      do while ( .not. done )
!
!        I is the index in the hit list of the segment
!        we're considering. J is the index of that segment
!        in the parallel input arrays.
!
         j = sghit (i)


         segfid = nint( dskbuf(frmidx,j) )

         if ( segfid .ne. fixfid ) then

            if ( segfid .ne. prvfrm ) then
!
!              Transform and shift the input point.
!
!              Here I is an index in the hit list and J is
!              an index in the input arrays.
!
               call moved ( sgxbuf(1,1,i), 9, xform )

               call mxv  ( xform, point, segpt )

               call vsub ( segpt, offbuf(1,j), vtemp )
               call vequ ( vtemp,              segpt )

            end if

         else if ( multfr ) then

            call vequ ( point, segpt )

         end if

!
!        If the point lies on the surface described by the
!        current segment, find the outward unit normal
!        vector at the point.
!
         dtype = nint( dskbuf(typidx,j) )
         xfnd  = .false.

         if ( dtype .eq. 2 ) then
!
!           Find the plate on which the point lies, if any.
!
            call zzptpl02 ( hanbuf(j), dlabuf(1,j), dskbuf(1,j), segpt, &
                            plid,      plate,       verts,       xfnd  )

            if ( failed() ) then
               call chkout( 'ZZDSKBUN' )
               return
            end if

            if ( xfnd ) then
!
!              Find the unit outward normal at SEGPT. We must
!              convert the output of PLTNRM to unit length.
!
               call pltnrm ( verts(1,1), verts(1,2), verts(1,3), &
                             normal                              )
               if ( failed() ) then
                  call chkout( 'ZZDSKBUN' )
                  return
               end if

               call vhatip ( normal )
!
!              WINNER is the index in the hit list of the current
!              segment.
!
               winner = i
               done   = .true.

            end if


         else

            call setmsg ( 'Segment type is #; this type is not ' &
            //            'currently supported.'                )
            call errint ( '#', dtype                            )
            call sigerr ( 'SPICE(NOTSUPPORTED)'                 )
            call chkout ( 'ZZDSKBUN'                            )
            return

         end if


         if ( failed() ) then
            call chkout( 'ZZDSKBUN' )
            return
         end if



         if ( .not. done ) then

            if ( i .eq. nhit ) then
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
!     If we have an result, it may be represented in a frame
!     other than the input point frame. Transform it back to the
!     input frame, if need be.
!
      if ( xfnd ) then
!
!        J is the index in the input arrays of the "winning" segment.
!
         j      = sghit ( winner )

         segfid = nint( dskbuf(frmidx,j) )

         if ( segfid .ne. fixfid ) then
!
!           The segment frame and input frame differ. The normal
!           vector must be converted back to the input frame.
!
            call moved( sgxbuf(1,1,winner), 9, xform )

            call mtxv ( xform, normal, vtemp  )
            call vequ ( vtemp,         normal )

         end if

      else
!
!        The input point was not legitimate; otherwise we
!        would have found a solution.
!
         call setmsg ( 'Input point (# # #) in frame # does not ' &
         //            'lie on the surface contained in any ' &
         //            'segment for the specified body (#) ' &
         //            'and surfaces.'                           )
         call errdp  ( '#', point(1)                             )
         call errdp  ( '#', point(2)                             )
         call errdp  ( '#', point(3)                             )
         call errint ( '#', fixfid                               )
         call errint ( '#', bodyid                               )
         call sigerr ( 'SPICE(POINTOFFSURFACE)'                  )
         call chkout ( 'ZZDSKBUN' )
         return

      end if

      call chkout ( 'ZZDSKBUN' )
      return
      end
