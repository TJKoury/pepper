!$Procedure ZZDSKSBR ( DSK, remove entries from API segment buffer )

      SUBROUTINE ZZDSKSBR ( NEEDED, &
                            MAXBOD, STSIZE, BTBODY, BTNBOD, BTSEGP, &
                            BTSTSZ, STHAN,  STDSCR, STDLAD, STFREE, &
                            STOFF,  STCTR,  STRAD                   )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Remove entries from DSK API segment buffer data structures
!     to make room for a new entry.
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

      INTEGER               NEEDED
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
!     NEEDED     I   Number of segment table entries needed.
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
!
!     NEEDED     is the number of segment table entries needed to store
!                the segment list for a new body.
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
! $ Detailed_Output
!
!
!     BTBODY     is the input body ID table, modified by this routine.
!                If it was necessary to delete a body from the table to
!                make room, that has been done.
!
!
!     BTNBOD     is the number of bodies in the body table.
!
!
!     BTSEGP     is an array of start indices in the segment table
!                of existing entries, updated to reflect deletion of
!                segments.
!
!
!     BTSTSZ     is the array of sizes of segment lists of bodies. If
!                it was necessary to make room, entries of BTSTSZ
!                will have been deleted.
!
!
!     STHAN      is the segment handle array. If it was necessary to
!                make room, entries of STHAN will have been deleted.
!
!
!     STDSCR     is the segment DSK descriptor array. If it was
!                necessary to make room, entries of STDESR will have
!                been deleted.
!
!
!     STDLAD     is the segment DLA descriptor array. If it was
!                necessary to make room, entries of STDLAD will have
!                been deleted.
!
!
!     STFREE     is the index of the first free element in each
!                segment table array.
!
!
!     STOFF      is the segment frame center offset array.
!                If it was necessary to make room, entries of
!                STOFF will have been deleted.
!
!
!     STCTR      is the segment bounding sphere center array.
!                If it was necessary to make room, entries of
!                STCTR will have been deleted.
!
!
!     STRAD      is the segment bounding sphere radius array.
!                If it was necessary to make room, entries of
!                STRAD will have been deleted.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If MAXBOD is less than 1, the error SPICE(INVALIDTABLESIZE)
!         is signaled.
!
!     2)  If the segment table doesn't have at least NEEDED entries,
!         the error SPICE(SEGTABLETOOSMALL) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine is meant to be used only by the DSK subsystem.
!
!     This routine makes room for new entries in the data structures
!     used by SPICELIB geometry APIs to perform efficient computations
!     using DSK data. It does so by deleting existing entries, if
!     necessary. The umbrella routine in which these structures
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
!-    SPICELIB Version 1.0.0, 11-JUL-2016 (NJB)
!
!        30-JAN-2016 (NJB)
!
!           Original version.
!
!-&

! $ Index_Entries
!
!     remove entries from DSK API segment buffer
!
!-&

!
!     SPICELIB functions
!
      LOGICAL               RETURN

!
!     Local variables
!
      INTEGER               AVAIL
      INTEGER               I
      INTEGER               J
      INTEGER               NBTDEL
      INTEGER               NSTDEL


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZDSKSBR' )


      IF ( NEEDED .GT. STSIZE ) THEN
!
!        There's not enough room in the entire segment table.
!
         CALL SETMSG ( 'Size of segment table is #; number of ' &
         //            'entries requested is #. The segment ' &
         //            'table is supposed to be declared ' &
         //            'with sufficient size to accommodate all ' &
         //            'loaded DSK segments.'                    )
         CALL ERRINT ( '#', STSIZE                               )
         CALL ERRINT ( '#', NEEDED                               )
         CALL SIGERR ( 'SPICE(SEGTABLETOOSMALL)'                 )
         CALL CHKOUT ( 'ZZDSKSBR'                                )
         RETURN

      END IF

!
!     We can't make room in a body table of zero size.
!
      IF ( MAXBOD .LT. 1 ) THEN

         CALL SETMSG ( 'Body table size must be at least 1 but is #.' )
         CALL ERRINT ( '#', MAXBOD                                    )
         CALL SIGERR ( 'SPICE(INVALIDTABLESIZE)'                      )
         CALL CHKOUT ( 'ZZDSKSBR'                                     )
         RETURN

      END IF

!
!     AVAIL is the number of entries currently available.
!
      AVAIL = STSIZE - STFREE + 1

      IF ( AVAIL .LT. NEEDED ) THEN
!
!        We need to make room in the segment table.
!
!        The entries at the end of the body table have the highest
!        priority. We scan forward through this table, summing the
!        entry counts for each body, until we have enough entries. Let
!        NE represent the number of available entries. NE is initially
!        the number of unused entries.
!
         NSTDEL = 0
         I      = 1

         DO WHILE ( ( I .LE. BTNBOD ) .AND. ( AVAIL .LT. NEEDED ) )
!
!           Add the segment count for the Ith body to the total.
!
            NSTDEL = NSTDEL + BTSTSZ(I)
            AVAIL  = AVAIL  + NSTDEL
            I      = I      + 1

         END DO

!
!        Backstop: we should always have enough room in the segment
!        table at this point.
!
         IF ( AVAIL .LT. NEEDED ) THEN

            CALL SETMSG ( 'The requested number of segment entries is ' &
            //            '#; the size STSIZE of the input segment  ' &
            //            'table is #. This error should have been ' &
            //            'trapped before this point.'                )
            CALL ERRINT ( '#', NEEDED                                 )
            CALL ERRINT ( '#', STSIZE                                 )
            CALL SIGERR ( 'SPICE(BUG)'                                )
            CALL CHKOUT ( 'ZZDSKSBR'                                  )
            RETURN

         END IF

!
!        At this point, I is the index of the first retained body,
!        unless all were deleted, in which case I is BTNBOD+1. We need
!        to delete the segment table entries of the bodies indexed from
!        1 to I-1.
!
         NBTDEL = I - 1

         IF ( NSTDEL .GT. 0 ) THEN
!
!           Adjust the tables to be consistent with the deletions.
!
!           Shift the body table and update the body table pointers.
!
            DO I = NBTDEL+1, BTNBOD

               J         = I - NBTDEL
               BTBODY(J) = BTBODY(I)
               BTSTSZ(J) = BTSTSZ(I)
               BTSEGP(J) = BTSEGP(I) - NSTDEL

            END DO

!
!           Update the body table count.
!
            BTNBOD = BTNBOD - NBTDEL

!
!           Shift the segment table entries forward by NSTDEL to make
!           room at the rear of the table.
!
            DO I = NSTDEL+1, STFREE-1

               J        = I - NSTDEL
               STHAN(J) = STHAN(I)

               CALL MOVED( STDSCR(1,I), DSKDSZ, STDSCR(1,J) )
               CALL MOVEI( STDLAD(1,I), DLADSZ, STDLAD(1,J) )
               CALL MOVED( STOFF (1,I), 3,      STOFF (1,J) )
               CALL MOVED( STCTR (1,I), 3,      STCTR (1,J) )

               STRAD(J) = STRAD(I)

            END DO

            STFREE = STFREE - NSTDEL

         END IF

      END IF

      CALL CHKOUT ( 'ZZDSKSBR' )
      RETURN
      END
