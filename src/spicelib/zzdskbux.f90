!$Procedure ZZDSKBUX ( DSK, buffered unprioritized ray intercept )

      SUBROUTINE ZZDSKBUX ( BODYID, NSURF,  SRFLST, ET,     FIXFID, &
                            NSEG,   HANBUF, DLABUF, DSKBUF, OFFBUF, &
                            CTRBUF, RADBUF, VERTEX, RAYDIR, XPT, &
                            SEGIDX, DC,     IC,     FOUND          )

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

      IMPLICIT NONE

      INCLUDE 'dla.inc'
      INCLUDE 'dskdsc.inc'
      INCLUDE 'dsktol.inc'
      INCLUDE 'zzctr.inc'


      INTEGER               BODYID
      INTEGER               NSURF
      INTEGER               SRFLST ( * )
      DOUBLE PRECISION      ET
      INTEGER               FIXFID
      INTEGER               NSEG
      INTEGER               HANBUF ( * )
      INTEGER               DLABUF ( DLADSZ, * )
      DOUBLE PRECISION      DSKBUF ( DSKDSZ, * )
      DOUBLE PRECISION      OFFBUF ( 3,      * )
      DOUBLE PRECISION      CTRBUF ( 3,      * )
      DOUBLE PRECISION      RADBUF ( * )
      DOUBLE PRECISION      VERTEX ( 3 )
      DOUBLE PRECISION      RAYDIR ( 3 )
      DOUBLE PRECISION      XPT    ( 3 )
      INTEGER               SEGIDX
      DOUBLE PRECISION      DC     ( * )
      INTEGER               IC     ( * )
      LOGICAL               FOUND

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
!     VERTEX,
!     RAYDIR     are, respectively, the vertex and direction vector of
!                the ray to be used in the intercept computation.
!
!                Both the vertex and ray must be represented in the
!                reference frame designated by FIXFID. The vertex is
!                considered to be an offset from the target body.
!
!
! $ Detailed_Output
!
!
!     XPT        is the surface intercept, if an intercept exists. If
!                multiple intercepts exist, the one closest to the
!                ray's vertex is selected. XPT is represented in the
!                reference frame designated by FIXFID. XPT is
!                considered to be an offset from the target body.
!
!
!     SEGIDX     is the index, within the input buffer arguments, of
!                the segment providing the surface data on which XPT
!                was found. SEGIDX is valid if and only if FOUND is
!                .TRUE.
!
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
      DOUBLE PRECISION      VDIST
      DOUBLE PRECISION      VNORM

      INTEGER               ISRCHI

      LOGICAL               FAILED
      LOGICAL               RETURN
      LOGICAL               VZERO

!
!     Local parameters
!
      DOUBLE PRECISION      RADSCL
      PARAMETER           ( RADSCL = 1.01D0 )

      INTEGER               MAXHIT
      PARAMETER           ( MAXHIT = 1000 )

!
!     Local variables
!
      DOUBLE PRECISION      DIST
      DOUBLE PRECISION      DMIN
      DOUBLE PRECISION      MAXRAD
      DOUBLE PRECISION      MINRAD
      DOUBLE PRECISION      SEGDIR ( 3 )
      DOUBLE PRECISION      SEGVTX ( 3 )
      DOUBLE PRECISION      PNEAR  ( 3 )
      DOUBLE PRECISION      SGDIST ( MAXHIT )
      DOUBLE PRECISION      SGMARG
      DOUBLE PRECISION      SGXBUF ( 3, 3, MAXHIT )
      DOUBLE PRECISION      SPHVTX ( 3 )
      DOUBLE PRECISION      SRFX   ( 3 )
      DOUBLE PRECISION      VTEMP  ( 3 )
      DOUBLE PRECISION      XFORM  ( 3, 3 )

      INTEGER               DTYPE
      INTEGER               I
      INTEGER               IORDER ( MAXHIT )
      INTEGER               J
      INTEGER               K
      INTEGER               NHIT
      INTEGER               NXPTS
      INTEGER               PRVFRM
      INTEGER               SEGFID
      INTEGER               SGHIT  ( MAXHIT )
      INTEGER               SURFCE
      INTEGER               WINNER

      LOGICAL               BODYOK
      LOGICAL               DONE
      LOGICAL               MULTFR
      LOGICAL               SURFOK
      LOGICAL               TIMEOK
      LOGICAL               XFND


!
!     Saved variables
!

!
!     Initial values
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZDSKBUX' )

!
!     Check the incoming segment count.
!
      IF ( NSEG .LE. 0 ) THEN

         CALL SETMSG ( 'Input segment list was empty. This may ' &
         //            'be due to no DSKs containing data for ' &
         //            'body # having been loaded.'            )
         CALL ERRINT ( '#',  BODYID                            )
         CALL SIGERR ( 'SPICE(NODSKSEGMENTS)'                  )
         CALL CHKOUT ( 'ZZDSKBUX'                              )
         RETURN

      END IF

!
!     No intercept has been found.
!
      SEGIDX = 0
      FOUND  = .FALSE.

!
!     Indicate we haven't yet seen a segment frame different
!     from the one designated by FIXFID.
!
      MULTFR = .FALSE.

!
!     Obtain the "greedy" segment margin.
!
      CALL DSKGTL ( KEYSGR, SGMARG )

!
!     Make a local copy of the ray. We'll update this copy
!     later if need be.
!
      CALL VEQU ( VERTEX, SEGVTX )
      CALL VEQU ( RAYDIR, SEGDIR )

!
!     Obtain the radius of an outer bounding sphere for the given body
!     and surface list.
!
      CALL ZZDSKSPH ( BODYID, NSURF, SRFLST, MINRAD, MAXRAD )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZDSKBUX' )
         RETURN
      END IF
!
!     Scale up the bounding sphere to avoid round-off difficulties.
!     We'll use this value in the loop below.
!
      MAXRAD = MAXRAD * RADSCL

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
      IF ( VNORM(SEGVTX) .GT. MAXRAD ) THEN
!
!        Find the intercept of the ray with the outer bounding
!        sphere. We'll use this intercept as the vertex for
!        later computations.
!
         CALL SURFPT ( SEGVTX, SEGDIR, MAXRAD, &
                       MAXRAD, MAXRAD, SPHVTX, XFND )

         IF ( FAILED() .OR. ( .NOT. XFND ) ) THEN
!
!           It would be highly unusual for the SURFPT call to
!           fail to produce an intercept. Check anyway.
!
            CALL CHKOUT ( 'ZZDSKBUX' )
            RETURN

         END IF

         CALL VEQU ( SPHVTX, SEGVTX )

      END IF

!
!     By default, each segment in the input list must be checked
!     for intersection.
!
!     We start out by trying to eliminate segments from consideration
!     by comparing the ray's distance from their centers to the radii
!     of their bounding spheres. Only those segments whose bounding
!     spheres are hit are examined further.
!
      NHIT   = 0
      PRVFRM = 0

      DO I = 1, NSEG
!
!        BODYOK indicates whether the input body ID matches that
!        of the current segment.
!
         SURFOK = .FALSE.
         TIMEOK = .FALSE.

         BODYOK = BODYID .EQ. NINT( DSKBUF(CTRIDX,I) )

         IF ( BODYOK ) THEN
!
!           See whether the current segment contains a surface we're
!           supposed to consider. If the surface list is empty, we
!           consider all surfaces. Otherwise, the surface of the
!           segment must be on the surface list in order to qualify.
!
            J = 0

            IF ( NSURF .GT. 0 ) THEN

               SURFCE = NINT( DSKBUF(SRFIDX,I) )
               J      = ISRCHI( SURFCE, NSURF, SRFLST )

            END IF

            SURFOK = ( NSURF .EQ. 0 ) .OR. ( J .GT. 0 )

!
!           See whether the segment covers the input epoch.
!
            TIMEOK =        ( ET .GE. DSKBUF(BTMIDX,I) ) &
                      .AND. ( ET .LE. DSKBUF(ETMIDX,I) )

         END IF

         IF ( BODYOK .AND. SURFOK .AND. TIMEOK  ) THEN
!
!           This segment is to be considered.
!
!           In order to do any geometric comparison, the ray must be in
!           the same frame as the segment we're checking. Transform the
!           input vertex and ray if need be.
!
!           Get the segment's frame ID. Get the transformation from the
!           input frame to the output frame if needed.

            SEGFID = NINT( DSKBUF(FRMIDX,I) )


            IF ( SEGFID .NE. FIXFID ) THEN
!
!              We have a segment that uses a different frame
!              from that specified by FIXFID.
!
               MULTFR = .TRUE.

               IF ( SEGFID .NE. PRVFRM ) THEN
!
!                 The frame of the current segment doesn't match
!                 that of the previous segment, so we'll need
!                 to look up the transformation from the input
!                 frame to the segment frame.
!
!                 Otherwise, XFORM already contains the correct
!                 transformation.
!
                  CALL REFCHG ( FIXFID, SEGFID, ET, XFORM )

                  IF ( FAILED() ) THEN
                     CALL CHKOUT ( 'ZZDSKBUX' )
                     RETURN
                  END IF

!
!                 Transform the local copy of the ray to the segment's
!                 frame, and shift the local copy of the ray's vertex
!                 so that it represents an offset relative to center of
!                 the segment's frame.
!
                  CALL MXV  ( XFORM, RAYDIR, SEGDIR )
                  CALL MXV  ( XFORM, VERTEX, SEGVTX )

!
!                 The direction of the buffered offset is from the body
!                 to the segment frame's center. The offset is
!                 expressed in the segment's frame.
!
                  CALL VSUB ( SEGVTX, OFFBUF(1,I), VTEMP  )
                  CALL VEQU ( VTEMP,               SEGVTX )

               END IF


            ELSE IF ( MULTFR ) THEN
!
!              The input and segment frames are the same for this
!              segment, but the current values of SEGVTX and SEGDIR
!              need to be reset.
!
               CALL VEQU ( VERTEX, SEGVTX )
               CALL VEQU ( RAYDIR, SEGDIR )

            END IF

!
!           Find the distance of the ray from the "center" of the
!           segment's coverage volume.
!
            CALL NPLNPT ( SEGVTX, SEGDIR, CTRBUF(1,I), PNEAR, DIST )


            IF ( DIST .LE. RADBUF(I) ) THEN
!
!              The line containing the ray intersects the bounding
!              surface. We'll check the boundary of the segment for an
!              intersection.
!
               CALL ZZRYTELT ( SEGVTX, SEGDIR, DSKBUF(1,I), &
                               SGMARG, NXPTS,  XPT         )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'ZZDSKBUX' )
                  RETURN
               END IF

               IF ( NXPTS .GT. 0 ) THEN
!
!                 The ray hits the boundary of this segment. Save the
!                 index of the segment in the "hit list." Record the
!                 distance from the ray's vertex to the intercept in
!                 the parallel array SGDIST.

                  IF ( NHIT .EQ. MAXHIT ) THEN

                     CALL SETMSG ( 'Too many segments were hit by the ' &
                     //            'input ray. Buffer size is #.'      )
                     CALL ERRINT ( '#', MAXHIT                         )
                     CALL SIGERR ( 'SPICE(BUFFERTOOSMALL)'             )
                     CALL CHKOUT ( 'ZZDSKBUX'                          )
                     RETURN

                  END IF

                  NHIT           = NHIT + 1
                  SGHIT ( NHIT ) = I
                  SGDIST( NHIT ) = VDIST( SEGVTX, XPT )
!
!                 Save the frame transformation for this segment.
!
                  CALL MOVED ( XFORM, 9, SGXBUF(1,1,NHIT) )

               END IF


            END IF
!
!           The current segment matched the input criteria.
!
!           Update the saved segment frame ID to that of the segment
!           we just examined.
!
            PRVFRM = SEGFID

         END IF

      END DO

!
!     Leave now if no segments were hit.
!
      IF ( NHIT .EQ. 0 ) THEN
         CALL CHKOUT ( 'ZZDSKBUX' )
         RETURN
      END IF

!
!     Find the order of the segments on the hit list, where
!     the metric is the distance of the ray intercepts from
!     the ray's vertex.
!
      CALL ORDERD ( SGDIST, NHIT, IORDER )

!
!     Now process the segments on the hit list in order. If we find a
!     surface intercept (that is, a ray intercept with the surface
!     represented by the segment's data, as opposed to the segment's
!     boundary), compare its distance from the ray's vertex to the
!     vertex-boundary distance of the next segment. If the
!     vertex-surface distance is smaller, we terminate the search,
!     since no other segment can contribute a closer intercept.
!
      I      = 1
      DONE   = .FALSE.
      WINNER = 0
      PRVFRM = 0

      DO WHILE ( .NOT. DONE )
!
!        J is the index in the hit list of the segment
!        we're considering. K is the index of that segment
!        in the parallel input arrays.
!
         J = IORDER(I)
         K = SGHIT (J)


         SEGFID = NINT( DSKBUF(FRMIDX,K) )

         IF ( SEGFID .NE. FIXFID ) THEN

            IF ( SEGFID .NE. PRVFRM ) THEN
!
!              Transform and shift the input ray.
!
!              Here J is an index in the hit list and K is
!              an index in the input arrays.
!
               CALL MOVED ( SGXBUF(1,1,J), 9, XFORM )

               CALL MXV  ( XFORM,  VERTEX, SEGVTX )
               CALL MXV  ( XFORM,  RAYDIR, SEGDIR )

               CALL VSUB ( SEGVTX, OFFBUF(1,K), VTEMP  )
               CALL VEQU ( VTEMP,               SEGVTX )

            END IF

         ELSE IF ( MULTFR ) THEN

            CALL VEQU ( VERTEX, SEGVTX )
            CALL VEQU ( RAYDIR, SEGDIR )

         END IF

!
!        Find the surface intercept using the segment topography
!        data.
!
         DTYPE = NINT( DSKBUF(TYPIDX,K) )

         CALL ZZDSKSGX ( HANBUF(K), DLABUF(1,K), DTYPE, ET, &
                         SEGVTX,    SEGDIR,      SRFX,  DC, &
                         IC,        XFND                   )

         IF ( FAILED() ) THEN
            CALL CHKOUT( 'ZZDSKBUX' )
            RETURN
         END IF


         IF ( XFND ) THEN
!
!           At least one surface intercept exists.
!
            IF ( .NOT. FOUND ) THEN
!
!              The intercept we just found is the first one, and
!              at least for now, it is the winner.
!
!              Save the intercept and vertex-intercept distance
!              for this segment.
!
               FOUND  = .TRUE.
               DMIN   = VDIST( SRFX, SEGVTX )

               CALL VEQU ( SRFX, XPT )

               WINNER = J

            ELSE
!
!              At least one surface intercept was found already.
!
!              Compare the vertex-intercept distance for this segment
!              to the best found so far.
!
               DIST = VDIST( SRFX, SEGVTX )

               IF ( DIST .LT. DMIN ) THEN
!
!                 This intercept is closer to the ray's vertex than
!                 any we've seen yet. We have a new winner.
!
                  DMIN   = DIST

                  CALL VEQU ( SRFX, XPT )

                  WINNER = J

               END IF

            END IF

         END IF

!
!        If there's at least one solution in hand, see whether
!        we can stop looking for better solutions.
!
         IF ( FOUND ) THEN

            IF ( I .LT. NHIT ) THEN
!
!              There are more segments in the hit list. Compare the
!              minimum vertex-intercept distance of the segments we've
!              checked to the vertex-boundary distance of the next
!              segment.
!
               J = IORDER(I+1)

               IF ( DMIN .LE. SGDIST(J) ) THEN
!
!                 The best intercept we've found is closer to the
!                 vertex than any intercept that may exist in the
!                 current segment, or any of the remaining segments in
!                 the hit list.
!
                  DONE  = .TRUE.

               END IF

            END IF

         END IF

         IF ( .NOT. DONE ) THEN

            IF ( I .EQ. NHIT ) THEN
!
!              We've looked at all of the segments.
!
               DONE = .TRUE.

            ELSE
!
!              Consider the next segment.
!
               I = I + 1

            END IF

         END IF

      END DO

!
!     If we have an intercept, it may be represented in a frame
!     other than the input ray frame. Transform it back to the
!     input frame, and shift it as well if the segment frame and
!     input frame have different centers.
!
      IF ( FOUND ) THEN
!
!        K is the index in the input arrays of the "winning" segment.
!        We'll return this index as SEGIDX.
!
         K      = SGHIT ( WINNER )
         SEGIDX = K

         SEGFID = NINT( DSKBUF(FRMIDX,K) )

         IF ( SEGFID .NE. FIXFID ) THEN
!
!           The segment frame and input frame differ. The intercept
!           must be converted back to the input frame. It also may
!           need to be shifted so that it is expressed as an offset
!           from the target body. If the shift is done, it must be
!           done before the frame transformation, since the offset
!           is expressed relative to the segment's frame.
!
            IF ( .NOT. VZERO( OFFBUF(1,K) ) ) THEN
!
!              OFFBUF(*,K) contains the offset of the segment frame
!              center from the segment's central body.
!
               CALL VADD ( XPT,   OFFBUF(1,K), VTEMP )
               CALL VEQU ( VTEMP,              XPT   )

            END IF

            CALL MOVED( SGXBUF(1,1,WINNER), 9, XFORM )

            CALL MTXV ( XFORM, XPT, VTEMP )
            CALL VEQU ( VTEMP,      XPT   )

         END IF

      END IF
!
!     FOUND and XPT are set.
!
      CALL CHKOUT ( 'ZZDSKBUX' )
      RETURN
      END
