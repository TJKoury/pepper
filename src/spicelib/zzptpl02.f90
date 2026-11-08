!$Procedure ZZPTPL02 ( DSK, map point to plate, type 2 )

      SUBROUTINE ZZPTPL02 ( HANDLE, DLADSC, DSKDSC, POINT, &
                            PLID,   PLATE,  VERTS,  FOUND )
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
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
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
      INCLUDE 'dsk02.inc'
      INCLUDE 'dsktol.inc'

      INTEGER               HANDLE
      INTEGER               DLADSC ( * )
      DOUBLE PRECISION      DSKDSC ( * )
      DOUBLE PRECISION      POINT  ( 3 )
      INTEGER               PLID
      INTEGER               PLATE  ( 3 )
      DOUBLE PRECISION      VERTS  ( 3, 3 )
      LOGICAL               FOUND

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
      DOUBLE PRECISION      DPMAX
      DOUBLE PRECISION      VDOT

      INTEGER               BRCKTI
      INTEGER               ZZVOX2ID

      LOGICAL               DLASSG
      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local parameters
!
      INTEGER               BUFSIZ
      PARAMETER           ( BUFSIZ = 1000 )

!
!     Local variables
!
      DOUBLE PRECISION      BOXCTR ( 3 )
      DOUBLE PRECISION      DIST
      DOUBLE PRECISION      DMIN
      DOUBLE PRECISION      LIMIT
      DOUBLE PRECISION      OFFSET ( 3 )
      DOUBLE PRECISION      MAXR
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      PNEAR  ( 3 )
      DOUBLE PRECISION      PNTOFF ( 3 )
      DOUBLE PRECISION      PTSRFM
      DOUBLE PRECISION      VOXORI ( 3 )
      DOUBLE PRECISION      VOXSIZ
      DOUBLE PRECISION      VRTTMP ( 3, 3 )
      DOUBLE PRECISION      XPDFRC
      DOUBLE PRECISION      XVERTS ( 3, 3 )

      INTEGER               CGRCOR ( 3 )
      INTEGER               CGREXT ( 3 )
      INTEGER               CGROFF ( 3 )
      INTEGER               CGRPTR
      INTEGER               CGRSCL
      INTEGER               CORSYS
      INTEGER               CGRVID
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               N
      INTEGER               NPLATE
      INTEGER               NREAD
      INTEGER               PIDTMP
      INTEGER               PLTBUF ( BUFSIZ )
      INTEGER               PLTPTR
      INTEGER               PLTTMP ( 3 )
      INTEGER               PTRLOC
      INTEGER               PRVDSC ( DLADSZ )
      INTEGER               PRVHAN
      INTEGER               PTROFF
      INTEGER               REMAIN
      INTEGER               START
      INTEGER               VGRCOR ( 3 )
      INTEGER               VGREXT ( 3 )
      INTEGER               VID

      LOGICAL               INSIDE
      LOGICAL               PASS1

!
!     Saved variables
!
      SAVE                  CGRSCL
      SAVE                  CORSYS
      SAVE                  LIMIT
      SAVE                  MAXR
      SAVE                  PASS1
      SAVE                  PRVHAN
      SAVE                  PRVDSC
      SAVE                  VGREXT
      SAVE                  VOXORI
      SAVE                  VOXSIZ

!
!     Initial values
!
      DATA                  LIMIT  / -1.D0      /
      DATA                  PASS1  / .TRUE.     /
      DATA                  PRVDSC / DLADSZ * 0 /
      DATA                  PRVHAN / 0          /



      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZPTPL02' )

!
!     No plate has been found so far.
!
      FOUND = .FALSE.

!
!     Decide whether we're looking at the segment we saw
!     on the previous call.
!
      IF (  PASS1  .OR. &
            .NOT.  DLASSG( HANDLE, PRVHAN, DLADSC, PRVDSC )  ) THEN
!
!        We'll need to look up the voxel grid parameters for this
!        segment.
!
         CALL DSKD02 ( HANDLE, DLADSC, KWVXOR, 1, 3, N, VOXORI )
         CALL DSKD02 ( HANDLE, DLADSC, KWVXSZ, 1, 1, N, VOXSIZ )
         CALL DSKI02 ( HANDLE, DLADSC, KWVGRX, 1, 3, N, VGREXT )
         CALL DSKI02 ( HANDLE, DLADSC, KWCGSC, 1, 1, N, CGRSCL )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZPTPL02' )
            RETURN
         END IF


         IF ( VOXSIZ .EQ. 0.D0 ) THEN

            CALL SETMSG ( 'Voxel edge length is zero; length ' &
            //            'must be positive.'                  )
            CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'             )
            CALL CHKOUT ( 'ZZPTPL02'                           )
            RETURN

         END IF

         IF ( CGRSCL .EQ. 0 ) THEN

            CALL SETMSG ( 'Coarse voxel scale is zero; scale ' &
            //            'must be positive.'                  )
            CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'             )
            CALL CHKOUT ( 'ZZPTPL02'                           )
            RETURN

         END IF

         CORSYS = NINT( DSKDSC(SYSIDX) )

         CALL ZZSEGBOX ( DSKDSC, BOXCTR, MAXR )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZPTPL02' )
            RETURN
         END IF

!
!        We successfully obtained the desired segment parameters, so we
!        don't need to execute this code again until the segment
!        changes. Save the current handle and DLA descriptor.
!
         PRVHAN = HANDLE
         CALL MOVEI ( DLADSC, DLADSZ, PRVDSC )

         PASS1  = .FALSE.

      END IF

!
!     Look up the point-plate membership margin; compute
!     the distance limit. This call must be made on every
!     call to ZZPTPL02.
!
      CALL DSKGTL ( KEYPTM, PTSRFM )

      LIMIT = PTSRFM * MAXR

!
!     Look up the plate expansion fraction. This call must be made on
!     every call to ZZPTPL02.
!
      CALL DSKGTL ( KEYXFR, XPDFRC )

!
!     Find out whether the point is within the volume element
!     bounding the segment.
!
      CALL ZZINVELT ( POINT,          CORSYS, DSKDSC(PARIDX), &
                      DSKDSC(MN1IDX), PTSRFM, 0,              INSIDE )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZPTPL02' )
         RETURN
      END IF


      IF ( .NOT. INSIDE ) THEN
!
!        The point is too far from the segment to be considered
!        to lie on a plate in that segment.
!
         CALL CHKOUT ( 'ZZPTPL02' )
         RETURN

      END IF

!
!     Map the point to the coordinates of a voxel containing it. If the
!     point is outside the voxel grid, map the point to the closest
!     voxel.
!
      CALL VSUB ( POINT, VOXORI, OFFSET )

      DO I = 1, 3

         J         = INT( OFFSET(I) / VOXSIZ ) + 1

         VGRCOR(I) = BRCKTI( J,  1,  VGREXT(I) )

      END DO

!
!     Compute the coordinates of the coarse voxel containing the fine
!     voxel we just identified. Get the 1-d offset of the fine voxel
!     relative the coarse voxel; this offset gives us the index of the
!     pointer associating the fine voxel with its plate list. The
!     1-d offset PTROFF is 1-based.
!
      CALL ZZVOXCVO ( VGRCOR, VGREXT, CGRSCL, CGRCOR, CGROFF, PTROFF )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZPTPL02' )
         RETURN
      END IF

!
!     Fetch the pointer from the coarse voxel to the first element of
!     its fine voxel pointer array.
!
!     We'll need the 1-D offset of the coarse voxel from the base of
!     the coarse voxel grid.
!
      DO I = 1, 3
         CGREXT(I) = VGREXT(I) / CGRSCL
      END DO

      CGRVID = ZZVOX2ID ( CGRCOR, CGREXT )

      CALL DSKI02 ( HANDLE, DLADSC, KWCGPT, CGRVID, 1, N, CGRPTR )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZPTPL02' )
         RETURN
      END IF

      IF ( CGRPTR .LT. 1 ) THEN
!
!        There are no non-empty fine voxels, hence no plates, in the
!        coarse voxel we're looking at.
!
         CALL CHKOUT ( 'ZZPTPL02' )
         RETURN

      END IF
!
!     Look up the pointer to the plate list for this voxel, and if
!     the pointer is non-null, look up the plate count.
!
      PTRLOC = (CGRPTR - 1) + PTROFF

      CALL DSKI02 ( HANDLE, DLADSC, KWVXPT, PTRLOC, 1, N, PLTPTR )

      IF (  FAILED()  .OR.  ( PLTPTR .LT. 1 )  ) THEN
         CALL CHKOUT ( 'ZZPTPL02' )
         RETURN
      END IF

      CALL DSKI02 ( HANDLE, DLADSC, KWVXPL, PLTPTR, 1, N, NPLATE )

      IF (  FAILED()  .OR.  ( NPLATE .LT. 1 )  ) THEN
         CALL CHKOUT ( 'ZZPTPL02' )
         RETURN
      END IF

!
!     Loop through the plates, keeping track of the minimum plate-point
!     distance.
!
      DMIN   = DPMAX()
      REMAIN = NPLATE
      NREAD  = MIN ( REMAIN, BUFSIZ )
      I      = 1

      DO WHILE ( REMAIN .GT. 0 )
!
!        Look up the current set of plate IDs.
!
         CALL DSKI02 ( HANDLE,   DLADSC, KWVXPL, &
                       PLTPTR+I, NREAD,  N,      PLTBUF )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZPTPL02' )
            RETURN
         END IF
!
!        Look up the vertices of each plate in the buffer and find
!        the distance of the point from that plate. Quit if we
!        find a match.
!
         DO J = 1, NREAD

            PIDTMP = PLTBUF(J)
            START  = ( 3*(PIDTMP-1) ) + 1

            CALL DSKI02 ( HANDLE, DLADSC, KWPLAT, START, 3, N, PLTTMP )

            DO K = 1, 3

               VID   = PLTTMP(K)
               START = ( 3*(VID-1) ) + 1

               CALL DSKD02 ( HANDLE, DLADSC, KWVERT, &
                             START,  3,      N,      VRTTMP(1,K) )

            END DO

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'ZZPTPL02' )
               RETURN
            END IF

!
!           Work with an expanded version of the plate.
!
            CALL PLTEXP ( VRTTMP, XPDFRC, XVERTS )

            CALL PLTNRM ( XVERTS(1,1), XVERTS(1,2), XVERTS(1,3), &
                          NORMAL                                )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'ZZPTPL02' )
               RETURN
            END IF

            CALL VHATIP ( NORMAL )

            CALL VSUB ( POINT, XVERTS(1,1), PNTOFF )

            IF ( ABS( VDOT(PNTOFF,NORMAL) ) .LE. LIMIT ) THEN
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
               CALL PLTNP ( POINT,       XVERTS(1,1), XVERTS(1,2), &
                            XVERTS(1,3), PNEAR,       DIST        )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'ZZPTPL02' )
                  RETURN
               END IF

            ELSE
               DIST = DPMAX()
            END IF

            IF ( DIST .LE. LIMIT ) THEN
!
!              We have a reasonable candidate for the closest plate.
!
               FOUND = .TRUE.

               IF ( DIST .LT. DMIN ) THEN

                  DMIN  = DIST
                  PLID  = PIDTMP
!
!                 Set the output vertices to the original version.
!
                  CALL MOVEI ( PLTTMP, 3, PLATE )
                  CALL MOVED ( VRTTMP, 9, VERTS )
!
!                 We'll return the above values if we don't find
!                 a better match.
!
               END IF

            END IF

         END DO

!
!        Prepare to read the next set of plate IDs, if any.
!
         REMAIN = REMAIN - NREAD
         I      = I      + NREAD
         NREAD  = MIN ( REMAIN, BUFSIZ )

      END DO

      CALL CHKOUT ( 'ZZPTPL02' )
      RETURN
      END
