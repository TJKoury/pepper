!$Procedure ZZDSKBUN ( DSK, buffered unprioritized normal vector )

      SUBROUTINE ZZDSKBUN ( BODYID, NSURF,  SRFLST, ET,     FIXFID, &
                            NSEG,   HANBUF, DLABUF, DSKBUF, OFFBUF, &
                            CTRBUF, RADBUF, POINT,  NORMAL         )

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
      DOUBLE PRECISION      POINT  ( 3 )
      DOUBLE PRECISION      NORMAL ( 3 )

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
      DOUBLE PRECISION      VDIST

      INTEGER               ISRCHI

      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local parameters
!
      INTEGER               MAXHIT
      PARAMETER           ( MAXHIT = 1000 )


!
!     Local variables
!
      DOUBLE PRECISION      DIST
      DOUBLE PRECISION      SEGPT  ( 3 )
      DOUBLE PRECISION      SGMARG
      DOUBLE PRECISION      SGXBUF ( 3, 3, MAXHIT )
      DOUBLE PRECISION      VERTS  ( 3, 3 )
      DOUBLE PRECISION      VTEMP  ( 3 )
      DOUBLE PRECISION      XFORM  ( 3, 3 )

      INTEGER               CORSYS
      INTEGER               DTYPE
      INTEGER               I
      INTEGER               J
      INTEGER               NHIT
      INTEGER               PLATE  ( 3 )
      INTEGER               PLID
      INTEGER               PRVFRM
      INTEGER               SEGFID
      INTEGER               SGHIT  ( MAXHIT )
      INTEGER               SURFCE
      INTEGER               WINNER

      LOGICAL               BODYOK
      LOGICAL               DONE
      LOGICAL               INSIDE
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

      CALL CHKIN ( 'ZZDSKBUN' )

!
!     Check the incoming segment count.
!
      IF ( NSEG .LE. 0 ) THEN

         CALL SETMSG ( 'Input segment list was empty. This may ' &
         //            'be due to no DSKs containing data for ' &
         //            'body # having been loaded.'            )
         CALL ERRINT ( '#',  BODYID                            )
         CALL SIGERR ( 'SPICE(NODSKSEGMENTS)'                  )
         CALL CHKOUT ( 'ZZDSKBUN'                              )
         RETURN

      END IF

!
!     Get the segment margin from the tolerance database.
!
      CALL DSKGTL ( KEYSGR, SGMARG )

!
!     Indicate we haven't yet seen a segment frame different
!     from the one designated by FIXFID.
!
      MULTFR = .FALSE.

!
!     Make a local copy of the point. We'll update this copy
!     later if need be.
!
      CALL VEQU ( POINT, SEGPT )

!
!     By default, each segment in the input list must be checked
!     for intersection.
!
!     We start out by trying to eliminate segments from consideration
!     by comparing the point's distance from their centers to the radii
!     of their bounding spheres. Only those segments whose bounding
!     spheres contain the point are examined further.
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
!           In order to do any geometric comparison, the point must be
!           in the same frame as the segment we're checking. Transform
!           the point if need be.
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
                     CALL CHKOUT ( 'ZZDSKBUN' )
                     RETURN
                  END IF

!
!                 Transform the local copy of the point to the segment's
!                 frame, and shift the local copy of the point so
!                 that it represents an offset relative to center of
!                 the segment's frame.
!
                  CALL MXV  ( XFORM, POINT,  SEGPT  )

!
!                 The direction of the buffered offset is from the body
!                 to the segment frame's center. The offset is
!                 expressed in the segment's frame.
!
                  CALL VSUB ( SEGPT, OFFBUF(1,I), VTEMP )
                  CALL VEQU ( VTEMP,              SEGPT )

               END IF


            ELSE IF ( MULTFR ) THEN
!
!              The input and segment frames are the same for this
!              segment, but the current value of SEGPT needs to be
!              reset.
!
               CALL VEQU ( POINT, SEGPT )

            END IF

!
!           Find the distance of the point from the "center" of the
!           segment's coverage volume.
!
            DIST = VDIST ( CTRBUF(1,I), SEGPT )


            IF ( DIST .LE. RADBUF(I) ) THEN
!
!              The point is inside or on the bounding surface. We'll
!              check the boundary of the segment for an intersection.
!
               CORSYS = NINT( DSKBUF(SYSIDX,I) )

               IF ( CORSYS .EQ. LATSYS ) THEN

                  CALL ZZINLAT ( SEGPT,  DSKBUF(MN1IDX,I), &
                                 SGMARG, 0,                INSIDE )

               ELSE IF ( CORSYS .EQ. RECSYS ) THEN

                  CALL ZZINREC ( SEGPT,  DSKBUF(MN1IDX,I), &
                                 SGMARG, 0,                INSIDE )

               ELSE IF ( CORSYS .EQ. PDTSYS ) THEN

                  CALL ZZINPDT ( SEGPT,             DSKBUF(MN1IDX,I), &
                                 DSKBUF(PARIDX, I), SGMARG, &
                                 0,                 INSIDE           )
               ELSE

                  CALL SETMSG( 'Coordinate system # is not supported.' )
                  CALL ERRINT( '#', CORSYS                             )
                  CALL SIGERR( 'SPICE(BADCOORDSYS)'                    )
                  CALL CHKOUT( 'ZZDSKBUN'                              )
                  RETURN

               END IF

               IF ( FAILED() ) THEN
                  CALL CHKOUT( 'ZZDSKBUN' )
                  RETURN
               END IF


               IF ( INSIDE ) THEN
!
!                 The point in inside the region enclosed by the
!                 boundary of this segment. Save the index of the
!                 segment in the "hit list."
!
                  IF ( NHIT .EQ. MAXHIT ) THEN

                     CALL SETMSG ( 'Too many segments contain the ' &
                     //            'input point. Buffer size is #.'  )
                     CALL ERRINT ( '#', MAXHIT                       )
                     CALL SIGERR ( 'SPICE(TOOMANYHITS)'              )
                     CALL CHKOUT ( 'ZZDSKBUN'                        )
                     RETURN

                  END IF

                  NHIT           = NHIT + 1
                  SGHIT ( NHIT ) = I
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
!     We have an error if no segments were hit. None of that
!     "not found" nonsense here.
!
      IF ( NHIT .EQ. 0 ) THEN

         CALL SETMSG ( 'Input point (# # #) in frame # does not ' &
         //            'lie inside any segment for the ' &
         //            'specified body (#) and surfaces.'        )
         CALL ERRDP  ( '#', POINT(1)                             )
         CALL ERRDP  ( '#', POINT(2)                             )
         CALL ERRDP  ( '#', POINT(3)                             )
         CALL ERRINT ( '#', FIXFID                               )
         CALL ERRINT ( '#', BODYID                               )
         CALL SIGERR ( 'SPICE(POINTNOTINSEGMENT)'                )
         CALL CHKOUT ( 'ZZDSKBUN' )
         RETURN

      END IF


!
!     Now process the segments on the hit list. If we find a segment
!     surface on which the input point is located, we compute the
!     normal vector and terminate the search.
!
      I      = 1
      DONE   = .FALSE.
      WINNER = 0
      PRVFRM = 0

      DO WHILE ( .NOT. DONE )
!
!        I is the index in the hit list of the segment
!        we're considering. J is the index of that segment
!        in the parallel input arrays.
!
         J = SGHIT (I)


         SEGFID = NINT( DSKBUF(FRMIDX,J) )

         IF ( SEGFID .NE. FIXFID ) THEN

            IF ( SEGFID .NE. PRVFRM ) THEN
!
!              Transform and shift the input point.
!
!              Here I is an index in the hit list and J is
!              an index in the input arrays.
!
               CALL MOVED ( SGXBUF(1,1,I), 9, XFORM )

               CALL MXV  ( XFORM, POINT, SEGPT )

               CALL VSUB ( SEGPT, OFFBUF(1,J), VTEMP )
               CALL VEQU ( VTEMP,              SEGPT )

            END IF

         ELSE IF ( MULTFR ) THEN

            CALL VEQU ( POINT, SEGPT )

         END IF

!
!        If the point lies on the surface described by the
!        current segment, find the outward unit normal
!        vector at the point.
!
         DTYPE = NINT( DSKBUF(TYPIDX,J) )
         XFND  = .FALSE.

         IF ( DTYPE .EQ. 2 ) THEN
!
!           Find the plate on which the point lies, if any.
!
            CALL ZZPTPL02 ( HANBUF(J), DLABUF(1,J), DSKBUF(1,J), SEGPT, &
                            PLID,      PLATE,       VERTS,       XFND  )

            IF ( FAILED() ) THEN
               CALL CHKOUT( 'ZZDSKBUN' )
               RETURN
            END IF

            IF ( XFND ) THEN
!
!              Find the unit outward normal at SEGPT. We must
!              convert the output of PLTNRM to unit length.
!
               CALL PLTNRM ( VERTS(1,1), VERTS(1,2), VERTS(1,3), &
                             NORMAL                              )
               IF ( FAILED() ) THEN
                  CALL CHKOUT( 'ZZDSKBUN' )
                  RETURN
               END IF

               CALL VHATIP ( NORMAL )
!
!              WINNER is the index in the hit list of the current
!              segment.
!
               WINNER = I
               DONE   = .TRUE.

            END IF


         ELSE

            CALL SETMSG ( 'Segment type is #; this type is not ' &
            //            'currently supported.'                )
            CALL ERRINT ( '#', DTYPE                            )
            CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                 )
            CALL CHKOUT ( 'ZZDSKBUN'                            )
            RETURN

         END IF


         IF ( FAILED() ) THEN
            CALL CHKOUT( 'ZZDSKBUN' )
            RETURN
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
!     If we have an result, it may be represented in a frame
!     other than the input point frame. Transform it back to the
!     input frame, if need be.
!
      IF ( XFND ) THEN
!
!        J is the index in the input arrays of the "winning" segment.
!
         J      = SGHIT ( WINNER )

         SEGFID = NINT( DSKBUF(FRMIDX,J) )

         IF ( SEGFID .NE. FIXFID ) THEN
!
!           The segment frame and input frame differ. The normal
!           vector must be converted back to the input frame.
!
            CALL MOVED( SGXBUF(1,1,WINNER), 9, XFORM )

            CALL MTXV ( XFORM, NORMAL, VTEMP  )
            CALL VEQU ( VTEMP,         NORMAL )

         END IF

      ELSE
!
!        The input point was not legitimate; otherwise we
!        would have found a solution.
!
         CALL SETMSG ( 'Input point (# # #) in frame # does not ' &
         //            'lie on the surface contained in any ' &
         //            'segment for the specified body (#) ' &
         //            'and surfaces.'                           )
         CALL ERRDP  ( '#', POINT(1)                             )
         CALL ERRDP  ( '#', POINT(2)                             )
         CALL ERRDP  ( '#', POINT(3)                             )
         CALL ERRINT ( '#', FIXFID                               )
         CALL ERRINT ( '#', BODYID                               )
         CALL SIGERR ( 'SPICE(POINTOFFSURFACE)'                  )
         CALL CHKOUT ( 'ZZDSKBUN' )
         RETURN

      END IF

      CALL CHKOUT ( 'ZZDSKBUN' )
      RETURN
      END
