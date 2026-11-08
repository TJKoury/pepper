!$Procedure ZZDSKSBA ( DSK, add entry to API segment buffer )

      SUBROUTINE ZZDSKSBA ( BODYID, &
                            MAXBOD, STSIZE, BTBODY, BTNBOD, BTSEGP, &
                            BTSTSZ, STHAN,  STDSCR, STDLAD, STFREE, &
                            STOFF,  STCTR,  STRAD                  )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Add an entry for a specified body to the DSK API segment buffer
!     data structure.
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
!     DLA
!     DSK
!     TOPOGRAPHY
!     UTILITY
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'dla.inc'
      INCLUDE 'dskdsc.inc'

      INTEGER               BODYID
      INTEGER               MAXBOD
      INTEGER               STSIZE
      INTEGER               BTBODY ( * )
      INTEGER               BTNBOD
      INTEGER               BTSEGP ( * )
      INTEGER               BTSTSZ ( * )
      INTEGER               STHAN  ( * )
      DOUBLE PRECISION      STDSCR ( DSKDSZ, * )
      INTEGER               STDLAD ( DLADSZ, * )
      INTEGER               STFREE
      DOUBLE PRECISION      STOFF  ( 3, * )
      DOUBLE PRECISION      STCTR  ( 3, * )
      DOUBLE PRECISION      STRAD  ( * )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     BODYID     I   Body ID code.
!     MAXBOD     I   Maximum size of body table.
!     STSIZE     I   Maximum size of segment table.
!     BTBODY    I-O  Body table's body ID array.
!     BTNBOD    I-O  Number of entries in body table.
!     BTSEGP    I-O  Array of pointers from bodies to segment table.
!     BTSTSZ    I-O  Array of sizes of body segment lists.
!     STHAN     I-O  Array of handles of segments.
!     STDSCR    I-O  Array of DSK descriptors of segments.
!     STDLAD    I-O  Array of DLA descriptors of segments.
!     STFREE    I-O  Index of first free entry in segment table.
!     STOFF     I-O  Offsets of segment frame centers from body.
!     STCTR     I-O  Centers of bounding spheres for segments.
!     STRAD     I-O  Radii of bounding spheres for segments.
!
! $ Detailed_Input
!
!     BODYID     is the integer ID code of a body for which a new
!                entry is to be made.
!
!     MAXBOD     is the maximum size of the body table. BTBODY, BTSEGP,
!                and BTSTSZ must each be declared by the caller with
!                size at least MAXBOD.
!
!     STSIZE     is the size of the segment table. STHAN, STDSCR,
!                STDLAD, STOFF, STCTR, and STRAD must be declared large
!                enough to hold STSIZE entries.
!
!     BTBODY     is a table of body IDs.
!
!     BTNBOD     is the number of body IDs currently in BTBODY.
!
!     BTSEGP     is an array of start indices in the segment table
!                of existing entries.
!
!     BTSTSZ     is an array of segment list sizes. The Ith entry
!                of BTSTSZ is the length of the segment list for the
!                Ith body.
!
!     STHAN      is an array of DAS handles. Each entry of STHAN that
!                is in use contains the handle of the DAS file
!                containing the DSK segment to which that entry
!                corresponds. The Ith entries of STHAN, STDSCR, STDLAD,
!                STOFF, STCTR, and STRAD correspond to the same DSK
!                segment.
!
!     STDSCR     is an array of DSK descriptors.
!
!     STDLAD     is an array of DLA descriptors.
!
!     STFREE     is the index of the first free entry in the segment
!                table.
!
!     STOFF      is an array of offsets of segment frame centers
!                from the central bodies of the segment. These offsets
!                are expressed in the reference frame of the segment.
!                They are constant vectors. Units are km.
!
!     STCTR      is an array of centers of outer bounding spheres for
!                segments. Each segment has an outer bounding sphere
!                that completely encloses that segment. Each center
!                is a vector expressed in the reference frame of the
!                the segment, and it is an offset from the frame's
!                center. Units are km.
!
!     STRAD      is an array of radii of outer bounding spheres of
!                segments. Units are km.
!
!
!
! $ Detailed_Output
!
!     BTBODY     is the input body ID table, modified by this routine.
!                If BODYID was not in the table on input, it has been
!                appended to the table.
!
!                If it was necessary to delete a body from the table
!                to make room for the body designated by BODYID, that
!                has been done.
!
!
!     BTNBOD     is the number of bodies in the body table. Depending
!                on what deletions may have been necessary, this
!                number may be greater than, equal to, or less than
!                its value on input.
!
!
!     BTSEGP     is an array of start indices in the segment table
!                of existing entries, updated to reflect appending of
!                segments corresponding to BODYID.
!
!
!     BTSTSZ     is the array of sizes of segment lists of bodies.
!                BTSTSZ contains an entry for the body designated by
!                BODYID.
!
!                If it was necessary to make room, other entries of
!                BTSTSZ may have been deleted.
!
!
!     STHAN      is the segment handle array, updated to include
!                an entry for each loaded DSK segment associated
!                with BODYID.
!
!                If it was necessary to make room, other entries of
!                STHAN may have been deleted.
!
!
!     STDSCR     is the segment DSK descriptor array, updated to include
!                an entry for each loaded DSK segment associated
!                with BODYID.
!
!                Segment entries are created in the order segments are
!                found by ZZDSKSNS, so the highest-priority segment's
!                entry is at the lowest index in STDSCR.
!
!                If it was necessary to make room, other entries of
!                STDESR may have been deleted.
!
!
!     STDLAD     is the segment DLA descriptor array, updated to include
!                an entry for each loaded DSK segment associated
!                with BODYID.
!
!                If it was necessary to make room, other entries of
!                STDLAD may have been deleted.
!
!
!     STFREE     is the index of the first free element in each
!                segment table array.
!
!
!     STOFF      is the segment frame center offset array, updated to
!                include an entry for each loaded DSK segment
!                associated with BODYID.
!
!                If it was necessary to make room, other entries of
!                STOFF may have been deleted.
!
!
!     STCTR      is the segment bounding sphere center array, updated
!                to include an entry for each loaded DSK segment
!                associated with BODYID.
!
!                If it was necessary to make room, other entries of
!                STCTR may have been deleted.
!
!
!     STRAD      is the segment bounding sphere radius array, updated
!                to include an entry for each loaded DSK segment
!                associated with BODYID.
!
!                If it was necessary to make room, other entries of
!                STRAD may have been deleted.
!
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If an attempt is made to add an entry for a body already in
!         the body table, the error SPICE(INVALIDADD) is signaled.
!
!     2)  If a DSK segment has an unrecognized coordinate system, an
!         error will be signaled by a routine in the call tree
!         of this routine.
!
!     3)  If the center of a frame of a DSK segment cannot be
!         obtained from the frame's ID code, the error
!         SPICE(NOFRAMEINFO) is signaled.
!
!     4)  If the name of a frame of a DSK segment cannot be
!         obtained from the frame's ID code, the error
!         SPICE(NOFRAMENAME) is signaled.
!
!     5)  If an error occurs while this routine attempts to
!         obtain segment information from ZZDSKBSR, an error
!         will be signaled by a routine in the call tree
!         of this routine.
!
!     6)  If the offset of a frame's center from the frame's
!         body is not computable at the TDB epoch at the
!         midpoint of the segment's time coverage interval,
!         an error will be signaled by a routine in the
!         SPK subsystem.
!
!     7)  If the segment table cannot accommodate all segments for
!         the specified body, the error SPICE(SEGMENTTABLEFULL) will
!         be signaled.
!
! $ Files
!
!     This routine makes use of DSK files loaded by the ZZDSKBSR
!     subsystem.
!
!     If any loaded DSK segment has a reference frame that is not
!     centered at the segment's central (target) body, SPK data are
!     required to compute the offset between the frame's center and
!     the segment's center.
!
!     Frame kernels may be required in order to look up a segment's
!     frame center offset. In some cases, additional kernels such
!     as CK kernels and SCLK kernels could be required to support
!     the offset vector lookup.
!
! $ Particulars
!
!     This routine is meant to be used only by the DSK subsystem.
!
!     This routine maintains data structures used by SPICELIB
!     geometry APIs to perform efficient computations using
!     DSK data. The umbrella routine in which these structures
!     are declared is ZZDSKSBF.
!
!     In a sense, ZZDSKSBF sits "above" the ZZDSKBSR subsystem.
!     High-level geometry routines access the data stored in the
!     ZZDSKSBF arrays directly; they don't call entry points of
!     ZZDSKBSR.
!
!     The ZZDSKSBF subsystem maintains two logical tables: a body
!     table and a segment table. Both tables are designed to store
!     a fixed maximum number of items; there is no "virtual storage"
!     concept that applies.
!
!     New entries are added to the tables by appending.
!
!     Items in both tables have priority assigned on a first-in,
!     first-out basis. When it becomes necessary to remove an
!     item to make room for another, the lowest-indexed item is
!     removed first, and the tables are compressed so that the
!     remaining items are contiguous.
!
!     When the state of the underlying ZZDSKBSR system changes due to
!     loading or unloading of DSK files, the ZZDSKSBF body and segment
!     tables must be re-initialized. Entry points of ZZDSKSBF are
!     expected to perform this action by calling ZZDSKSBI.
!
! $ Examples
!
!     See usage in ZZDSKSBF.
!
! $ Restrictions
!
!     1) This is a private routine.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 13-JUL-2016 (NJB)
!
!-&

! $ Index_Entries
!
!     add entry to DSK API segment buffer
!
!-&

!
!     SPICELIB functions
!
      INTEGER               ISRCHI

      LOGICAL               FAILED
      LOGICAL               RETURN
      LOGICAL               ZZDSKSBD

!
!     External routines
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
      CHARACTER*(FRNMLN)    FRNAME

      DOUBLE PRECISION      DSKDSC ( DSKDSZ )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      LT

      INTEGER               AVAIL
      INTEGER               DLADSC ( DLADSZ )
      INTEGER               FRMCTR
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               J
      INTEGER               NSEG
      INTEGER               SEGCLD
      INTEGER               SEGCLS
      INTEGER               SEGCTR
      INTEGER               SEGFID

      LOGICAL               FRMFND
      LOGICAL               SEGFND
      LOGICAL               STATUS


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZDSKSBA' )

!
!     Check the body table for presence of the new body. It's
!     an error to call this routine for a body that's already
!     present. (Such a call likely indicates the tables were
!     not re-initialized after a BSR state change.)
!
      I = ISRCHI( BODYID, BTNBOD, BTBODY )

      IF ( I .GT. 0 ) THEN

         CALL SETMSG ( 'Body # is already present in the DSK segment ' &
         //            'buffer body table. The table must be ' &
         //            're-initialized before this body can be added.' )
         CALL ERRINT ( '#', BODYID                                     )
         CALL SIGERR ( 'SPICE(INVALIDADD)'                             )
         CALL CHKOUT ( 'ZZDSKSBA'                                      )
         RETURN

      END IF

!
!     Make sure the BSR segment list for the body is up to
!     date.
!
      CALL ZZDSKBBL ( BODYID )

      IF ( FAILED() ) THEN
         CALL CHKOUT( 'ZZDSKSBA' )
         RETURN
      END IF

!
!     Count the segments in the BSR system for the body.
!
      NSEG   = 0
      STATUS = ZZDSKSBD( BODYID )

      CALL ZZDSKBSS( BODYID )
      CALL ZZDSKSNS( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, SEGFND )

      IF ( FAILED() ) THEN
         CALL CHKOUT( 'ZZDSKSBA' )
         RETURN
      END IF

      DO WHILE ( SEGFND )

         NSEG = NSEG + 1

         CALL ZZDSKSNS( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, SEGFND )

         IF ( FAILED() ) THEN
            CALL CHKOUT( 'ZZDSKSBA' )
            RETURN
         END IF

      END DO

!
!     Check the number of segments for BODY against the size of the
!     segment table. If the table isn't big enough, we can't make
!     room by deleting existing entries. This is a backstop check;
!     this situation should not occur if STSIZE is consistent with
!     the value in ZZDSKBSR.
!
      IF ( NSEG .GT. STSIZE ) THEN

         CALL SETMSG ( 'The number of segments for body # is #; ' &
         //            'the size STSIZE of the input segment table ' &
         //            'is #.'                                       )
         CALL ERRINT ( '#', BODYID                                   )
         CALL ERRINT ( '#', NSEG                                     )
         CALL ERRINT ( '#', STSIZE                                   )
         CALL SIGERR ( 'SPICE(SEGMENTTABLEFULL)'                     )
         CALL CHKOUT ( 'ZZDSKSBA'                                    )
         RETURN

      END IF

!
!     If we don't have enough room to store new entries in the body
!     table or in the segment table, make room.
!
      AVAIL = STSIZE - STFREE + 1

      IF (  ( BTNBOD .EQ. MAXBOD ) .OR. ( AVAIL .LT. NSEG )  ) THEN

         CALL ZZDSKSBR ( NSEG, &
                         MAXBOD, STSIZE, BTBODY, BTNBOD, BTSEGP, &
                         BTSTSZ, STHAN,  STDSCR, STDLAD, STFREE, &
                         STOFF,  STCTR,  STRAD                  )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZDSKSBA' )
            RETURN
         END IF

      END IF

!
!     Append the new body ID to the body table. We've ensured there's
!     room in the table.
!
      BTNBOD           = BTNBOD + 1
      BTBODY( BTNBOD ) = BODYID
      BTSEGP( BTNBOD ) = STFREE
      BTSTSZ( BTNBOD ) = NSEG

!
!     Make a second pass through the BSR segment list, this time
!     accumulating segments in the input segment table as we go.
!
      STATUS = ZZDSKSBD( BODYID )

      CALL ZZDSKBSS( BODYID )
      CALL ZZDSKSNS( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, SEGFND )

      IF ( FAILED() ) THEN
         CALL CHKOUT( 'ZZDSKSBA' )
         RETURN
      END IF

      DO WHILE ( SEGFND )
!
!        Insert handle and descriptor data for the current segment at
!        index STFREE in the segment table.
!
         STHAN( STFREE ) = HANDLE

         CALL MOVEI ( DLADSC, DLADSZ, STDLAD(1,STFREE) )
         CALL MOVED ( DSKDSC, DSKDSZ, STDSCR(1,STFREE) )

         STFREE          = STFREE + 1

         CALL ZZDSKSNS( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, SEGFND )

         IF ( FAILED() ) THEN
            CALL CHKOUT( 'ZZDSKSBA' )
            RETURN
         END IF

      END DO

!
!     Compute bounding spheres and frame center offsets for each
!     segment.
!
      DO I = 1, NSEG
!
!        J is the index in the segment table of the Ith segment
!        for BODYID.
!
         J  = BTSEGP( BTNBOD ) + I - 1

         CALL ZZSEGBOX ( STDSCR(1,J), STCTR(1,J), STRAD(J) )

         IF ( FAILED() ) THEN
            CALL CHKOUT( 'ZZDSKSBA' )
            RETURN
         END IF

!
!        Obtain the center of the frame for the Ith segment.
!
         SEGFID = NINT( STDSCR(FRMIDX,J) )

         CALL FRINFO ( SEGFID, FRMCTR, SEGCLS, SEGCLD, FRMFND )

         IF ( .NOT. FRMFND ) THEN

            CALL SETMSG ( 'Could not look up frame info for ' &
            //            'segment frame having ID #.'       )
            CALL ERRINT ( '#', SEGFID                        )
            CALL SIGERR ( 'SPICE(NOFRAMEINFO)'               )
            CALL CHKOUT ( 'ZZDSKSBA'                         )
            RETURN

         END IF
!
!        If the frame center is not the same as the central
!        body, compute the offset between the two. Otherwise
!        set the offset to zero.
!
         SEGCTR = NINT( STDSCR(CTRIDX,J) )

         IF ( SEGCTR .EQ. FRMCTR ) THEN

            CALL CLEARD ( 3, STOFF(1,J) )

         ELSE

            CALL FRMNAM ( SEGFID, FRNAME )

            IF ( FRNAME .EQ. ' ' ) THEN

               CALL SETMSG ( 'Could not look up frame info for ' &
               //            'segment frame having ID #.'       )
               CALL ERRINT ( '#', SEGFID                        )
               CALL SIGERR ( 'SPICE(NOFRAMENAME)'               )
               CALL CHKOUT ( 'ZZDSKSBA'                         )
               RETURN

            END IF

!
!           Note that SPK data must be available at the midpoint of the
!           DSK coverage epoch in order for the following call to work.
!
            ET = ( STDSCR(BTMIDX,J) + STDSCR(ETMIDX,J) ) / 2

            CALL SPKGPS ( FRMCTR, ET, FRNAME, SEGCTR, STOFF(1,J), LT )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'ZZDSKSBA' )
               RETURN
            END IF

         END IF

      END DO


      CALL CHKOUT ( 'ZZDSKSBA' )
      RETURN
      END
