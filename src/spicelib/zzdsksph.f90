!$Procedure ZZDSKSPH ( DSK, bounding spheres for target body )

      SUBROUTINE ZZDSKSPH ( BODYID, NSURF, SRFLST, MINRAD, MAXRAD )

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
!     DSK
!     GEOMETRY
!     SURFACE
!     TOPOGRAPHY
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'errhnd.inc'
      INCLUDE 'dla.inc'
      INCLUDE 'dsk.inc'
      INCLUDE 'dskdsc.inc'
      INCLUDE 'zzctr.inc'

      INTEGER               BODYID
      INTEGER               NSURF
      INTEGER               SRFLST ( * )
      DOUBLE PRECISION      MINRAD
      DOUBLE PRECISION      MAXRAD

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
      DOUBLE PRECISION      DPMAX
      DOUBLE PRECISION      VNORM

      INTEGER               ISRCHI

      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     EXTERNAL routines
!
      EXTERNAL              ZZDSKBDC

!
!     Local parameters
!
      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

!
!     Local variables
!
      CHARACTER*(LMSGLN)    ERRMSG
      CHARACTER*(FRNMLN)    FRNAME

      DOUBLE PRECISION      BOXCTR ( 3 )
      DOUBLE PRECISION      BOXRAD
      DOUBLE PRECISION      CTRMNR
      DOUBLE PRECISION      DSKDSC ( DSKDSZ )
      DOUBLE PRECISION      F
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      LX
      DOUBLE PRECISION      LY
      DOUBLE PRECISION      LZ
      DOUBLE PRECISION      MAXR
      DOUBLE PRECISION      MIDTIM
      DOUBLE PRECISION      MINR
      DOUBLE PRECISION      OFFMAG
      DOUBLE PRECISION      OFFSET ( 3 )
      DOUBLE PRECISION      RE
      DOUBLE PRECISION      RP
      DOUBLE PRECISION      SGMAXR
      DOUBLE PRECISION      SGMINR
      DOUBLE PRECISION      SVMAXR
      DOUBLE PRECISION      SVMINR

      INTEGER               CORSYS
      INTEGER               CTR    ( CTRSIZ )
      INTEGER               DLADSC ( DLADSZ )
      INTEGER               FRAMID
      INTEGER               FRCENT
      INTEGER               FRCLID
      INTEGER               FRCLAS
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               PRVBOD
      INTEGER               PRVFID
      INTEGER               PRVLST ( MAXSRF )
      INTEGER               PRVNLS
      INTEGER               SURFID

      LOGICAL               FIRST
      LOGICAL               FOUND
      LOGICAL               NEWLST
      LOGICAL               SAME
      LOGICAL               SEGFND
      LOGICAL               UPDATE

!
!     Saved variables
!
      SAVE                  CTR
      SAVE                  FIRST
      SAVE                  PRVBOD
      SAVE                  PRVFID
      SAVE                  PRVLST
      SAVE                  PRVNLS
      SAVE                  SVMAXR
      SAVE                  SVMINR

!
!     Initial values
!
      DATA                  CTR    / -1, -1      /
      DATA                  FIRST  / .TRUE.      /
      DATA                  PRVFID /  0          /
      DATA                  PRVBOD /  0          /
      DATA                  PRVLST /  MAXSRF * 0 /
      DATA                  PRVNLS / -1          /
      DATA                  SVMAXR / -1.D0       /
      DATA                  SVMINR / -1.D0       /



      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZDSKSPH' )

      IF ( FIRST ) THEN
         CALL ZZCTRUIN ( CTR )
      END IF

!
!     Check NSURF.
!
      IF ( NSURF .LT. 0 ) THEN

         CALL SETMSG ( 'NSURF must be non-negative but was #.' )
         CALL ERRINT ( '#', NSURF                              )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'                )
         CALL CHKOUT ( 'ZZDSKSPH'                              )
         RETURN

      END IF

!
!     Determine whether the input body surface list matches
!     the previous values. The following code applies whether
!     or not the surface list is non-empty.
!
      NEWLST = .TRUE.

      IF ( .NOT. FIRST ) THEN

         IF ( BODYID .EQ. PRVBOD ) THEN

            IF ( NSURF .EQ. PRVNLS ) THEN

               SAME = .TRUE.
               I    = 1

               DO WHILE ( ( I .LE. NSURF ) .AND.  SAME  )

                  SAME = SRFLST(I) .EQ. PRVLST(I)
                  I    = I + 1
               END DO
!
!              If SAME is true here, the body and surface list are the
!              same as on the previous call.
!
               NEWLST = .NOT. SAME

            END IF

         END IF

      END IF

!
!     Set PRVNLS to a value that can't match a valid value, so
!     the surface list won't match after an error occurs. We'll
!     reset PRVNLS prior to exit if all goes well.
!
      PRVNLS = -1

!
!     Check for DSK update in ZZDSKBSR.
!
      CALL ZZDSKCHK ( CTR, UPDATE )

!
!     Initialize the temporary variables MINR, MAXR.
!
      MINR = SVMINR
      MAXR = SVMAXR


      IF ( FIRST .OR. UPDATE .OR. NEWLST ) THEN
!
!        Initialize the saved radius data.
!
         SVMAXR = -1.D0
         SVMINR = DPMAX()

!
!        Prepare to fetch segment data. Initialize the ZZDSKBSR
!        segment list for the body of interest.
!
         CALL ZZDSKBBL ( BODYID )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZDSKSPH' )
            RETURN
         END IF

!
!        Fetch segment DSK descriptors for the indicated body and
!        surface list.
!
         PRVFID = 0
         CALL CLEARD ( 3, OFFSET )

!
!        Re-initialize MINR and MAXR.
!
         MAXR = -1.D0
         MINR = DPMAX()

!
!        Examine all segments for BODYID.
!
         CALL ZZDSKBSS ( BODYID )

         CALL ZZDSKSBD ( BODYID )
         CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, SEGFND )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZDSKSPH' )
            RETURN
         END IF

         DO WHILE ( SEGFND )

            IF ( NSURF .GT. 0 ) THEN

               SURFID = NINT( DSKDSC(SRFIDX) )

               I      = ISRCHI ( SURFID, NSURF, SRFLST )
            ELSE
               I      = 1
            END IF

            IF ( I .GT. 0 ) THEN
!
!              If we're checking surface IDs, this segment qualifies.
!              Otherwise, we're not checking surface IDs, so the segment
!              qualifies by default.
!
!              Get the frame ID of this segment, and look up the frame's
!              center.
!
               FRAMID = NINT( DSKDSC(FRMIDX) )

               IF ( FRAMID .NE. PRVFID ) THEN
!
!                 Get the frame center for the current segment.
!
                  CALL FRINFO ( FRAMID, FRCENT, FRCLAS, FRCLID, FOUND )

                  IF ( .NOT. FOUND ) THEN

                     CALL SETMSG ( 'No frame specification was found ' &
                     //            'for frame ID #.'                  )
                     CALL ERRINT ( '#', FRAMID                        )
                     CALL SIGERR ( 'SPICE(NOFRAMEDATA)'               )
                     CALL CHKOUT ( 'ZZDSKSPH'                         )
                     RETURN

                  END IF

                  IF ( FRCENT .EQ. BODYID ) THEN
!
!                    The frame is centered at the target, so
!                    the frame center offset magnitude is zero.
!
                     OFFMAG = 0.D0

                  ELSE

                     CALL FRMNAM ( FRAMID, FRNAME )

                     IF ( FAILED() ) THEN
                        CALL CHKOUT ( 'ZZDSKSPH' )
                        RETURN
                     END IF

                     IF ( FRNAME .EQ. ' ' ) THEN

                        CALL SETMSG ( 'No frame name was found ' &
                        //            'for frame ID #.'          )
                        CALL ERRINT ( '#', FRAMID                )
                        CALL SIGERR ( 'SPICE(FRAMENAMENOTFOUND)' )
                        CALL CHKOUT ( 'ZZDSKSPH'                 )
                        RETURN

                     END IF

                     MIDTIM = ( DSKDSC(BTMIDX) + DSKDSC(ETMIDX) ) / 2

                     CALL SPKGPS ( FRCENT, MIDTIM, FRNAME, &
                                   BODYID, OFFSET, LT     )

                     IF ( FAILED() ) THEN
                        CALL CHKOUT ( 'ZZDSKSPH' )
                        RETURN
                     END IF

                     OFFMAG = VNORM( OFFSET )

                  END IF

               END IF

!
!              Get the segment coordinate system and derive the maximum
!              radius of the segment.
!
               CORSYS = NINT( DSKDSC(SYSIDX) )

!
!              Get bounding radii for the segment relative to the
!              origin of the segment's coordinate system. We'll account
!              for the offset of the origin from the segment's central
!              body as a subsequent step.
!
               IF ( CORSYS .EQ. LATSYS ) THEN

                  SGMINR = DSKDSC(MN3IDX)
                  SGMAXR = DSKDSC(MX3IDX)


               ELSE IF ( CORSYS .EQ. PDTSYS ) THEN
!
!                 Use the reference spheroid and altitude bounds to
!                 generate initial bounding radii.
!
                  RE = DSKDSC(PARIDX  )
                  F  = DSKDSC(PARIDX+1)
                  RP = RE *  (1.D0 - F)

                  IF ( F .GE. 0.D0 ) THEN
!
!                    The spheroid is oblate. The maximum altitude over
!                    the equator is an upper bound for the distance of
!                    any surface point from the origin. The minimum
!                    altitude over either pole is a lower bound for
!                    the distance of any surface point from the origin.
!
!                    The DSK descriptor gives us the altitude bounds.
!
                     SGMAXR = RE + DSKDSC(MX3IDX)
                     SGMINR = RP + DSKDSC(MN3IDX)

                  ELSE

!                    The spheroid is prolate. The maximum altitude over
!                    either pole is an upper bound for the distance of
!                    any surface point from the origin.
!
                     SGMAXR = RP + DSKDSC(MX3IDX)
                     SGMINR = RE + DSKDSC(MN3IDX)

                  END IF



               ELSE IF ( CORSYS .EQ. RECSYS ) THEN

                  CALL ZZRECBOX ( DSKDSC(MN1IDX), BOXCTR, LX, &
                                  LY,             LZ,     BOXRAD )
!
!                 SGMINR is a lower bound on the distance of the
!                 segment from the origin of the coordinate system.
!
                  SGMINR = MAX ( VNORM(BOXCTR) - BOXRAD,  0.0D0 )

                  SGMAXR =       VNORM(BOXCTR) + BOXRAD


               ELSE

                  CALL SETMSG ( 'Coordinate system # is not ' &
                  //            'currently supported.'        )
                  CALL ERRINT ( '#', CORSYS                   )
                  CALL SIGERR ( 'SPICE(NOTSUPPORTED)'         )
                  CALL CHKOUT ( 'ZZDSKSPH'                    )
                  RETURN

               END IF

!
!              Apply the triangle inequality to derive minimum and
!              maximum values of the distance of the surface from the
!              body center, given the offset between the frame center
!              and the body center, and given bounds on the distance of
!              the surface from the frame's center.
!
               IF ( OFFMAG .LE. SGMINR ) THEN
!
!                 The segment's central body is inside the inner
!                 bounding sphere of the segment.
!
                  CTRMNR = SGMINR - OFFMAG

               ELSE IF ( OFFMAG .GE. SGMAXR ) THEN
!
!                 The segment's central body is outside the outer
!                 bounding sphere of the segment.
!
                  CTRMNR = OFFMAG - SGMAXR

               ELSE
!
!                 The segment's central body is between the bounding
!                 spheres. No positive lower radius bound exists.
!
                  CTRMNR = 0.D0

               END IF

!
!              Update the segment's outer bounding radius to
!              account for the frame center offset (which may
!              be zero).
!
               SGMAXR = SGMAXR + OFFMAG

!
!              Update the global minimum and maximum radii.
!
               MINR = MIN( MINR, CTRMNR )
               MAXR = MAX( MAXR, SGMAXR )

            END IF
!
!           Look at the next segment.
!
            CALL ZZDSKSBD( BODYID )
            CALL ZZDSKSNS( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, SEGFND )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'ZZDSKSPH' )
               RETURN
            END IF

         END DO

         IF (  ( MAXR .GT. 0.D0 ) .AND. (.NOT. FAILED() )  ) THEN
!
!           Update the saved bounds.
!
            SVMINR = MINR
            SVMAXR = MAXR

         END IF

      END IF


      IF ( MAXR .LT. 0.D0 ) THEN
!
!        We tried to update the radius bounds but didn't find any
!        segments for the specified body.
!
!        We have no radius data for the specified surface list.
!
         IF ( NSURF .EQ. 0 ) THEN

            ERRMSG = 'No segments were found matching the body ID #.'

         ELSE

            ERRMSG = 'No segments were found matching the body ID # ' &
            //       'and the surface list <@>.'

            DO I = 1, NSURF-1

               CALL REPMC  ( ERRMSG, '@', '*, @',    ERRMSG )
               CALL REPMI  ( ERRMSG, '*', SRFLST(I), ERRMSG )

            END DO

            CALL REPMI  ( ERRMSG, '@', SRFLST(NSURF), ERRMSG )

         END IF

         CALL SETMSG ( ERRMSG                   )
         CALL ERRINT ( '#', BODYID              )
         CALL SIGERR ( 'SPICE(DSKDATANOTFOUND)' )
         CALL CHKOUT ( 'ZZDSKSPH'               )
         RETURN

      END IF

      IF ( .NOT. FAILED() ) THEN

         FIRST  = .FALSE.

         PRVBOD = BODYID
         PRVNLS = NSURF

         IF ( NEWLST ) THEN

            CALL MOVEI ( SRFLST, NSURF, PRVLST )

         END IF

         MAXRAD = SVMAXR
         MINRAD = SVMINR

      END IF

      CALL CHKOUT ( 'ZZDSKSPH' )
      RETURN
      END
