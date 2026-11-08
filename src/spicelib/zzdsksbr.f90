!$Procedure ZZDSKSBR ( DSK, remove entries from API segment buffer )

      subroutine zzdsksbr ( needed, &
                            maxbod, stsize, btbody, btnbod, btsegp, &
                            btstsz, sthan,  stdscr, stdlad, stfree, &
                            stoff,  stctr,  strad                   )

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

      implicit none

      include 'dla.inc'
      include 'dskdsc.inc'

      integer               needed
      integer               maxbod
      integer               stsize
      integer               btbody ( * )
      integer               btnbod
      integer               btsegp ( * )
      integer               btstsz ( * )
      integer               sthan  ( * )
      double precision      stdscr ( dskdsz, * )
      integer               stdlad ( dladsz, * )
      integer               stfree
      double precision      stoff  ( 3, * )
      double precision      stctr  ( 3, * )
      double precision      strad  ( * )

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
      logical               return

!
!     Local variables
!
      integer               avail
      integer               i
      integer               j
      integer               nbtdel
      integer               nstdel


      if ( return() ) then
         return
      end if

      call chkin ( 'ZZDSKSBR' )


      if ( needed .gt. stsize ) then
!
!        There's not enough room in the entire segment table.
!
         call setmsg ( 'Size of segment table is #; number of ' &
         //            'entries requested is #. The segment ' &
         //            'table is supposed to be declared ' &
         //            'with sufficient size to accommodate all ' &
         //            'loaded DSK segments.'                    )
         call errint ( '#', stsize                               )
         call errint ( '#', needed                               )
         call sigerr ( 'SPICE(SEGTABLETOOSMALL)'                 )
         call chkout ( 'ZZDSKSBR'                                )
         return

      end if

!
!     We can't make room in a body table of zero size.
!
      if ( maxbod .lt. 1 ) then

         call setmsg ( 'Body table size must be at least 1 but is #.' )
         call errint ( '#', maxbod                                    )
         call sigerr ( 'SPICE(INVALIDTABLESIZE)'                      )
         call chkout ( 'ZZDSKSBR'                                     )
         return

      end if

!
!     AVAIL is the number of entries currently available.
!
      avail = stsize - stfree + 1

      if ( avail .lt. needed ) then
!
!        We need to make room in the segment table.
!
!        The entries at the end of the body table have the highest
!        priority. We scan forward through this table, summing the
!        entry counts for each body, until we have enough entries. Let
!        NE represent the number of available entries. NE is initially
!        the number of unused entries.
!
         nstdel = 0
         i      = 1

         do while ( ( i .le. btnbod ) .and. ( avail .lt. needed ) )
!
!           Add the segment count for the Ith body to the total.
!
            nstdel = nstdel + btstsz(i)
            avail  = avail  + nstdel
            i      = i      + 1

         end do

!
!        Backstop: we should always have enough room in the segment
!        table at this point.
!
         if ( avail .lt. needed ) then

            call setmsg ( 'The requested number of segment entries is ' &
            //            '#; the size STSIZE of the input segment  ' &
            //            'table is #. This error should have been ' &
            //            'trapped before this point.'                )
            call errint ( '#', needed                                 )
            call errint ( '#', stsize                                 )
            call sigerr ( 'SPICE(BUG)'                                )
            call chkout ( 'ZZDSKSBR'                                  )
            return

         end if

!
!        At this point, I is the index of the first retained body,
!        unless all were deleted, in which case I is BTNBOD+1. We need
!        to delete the segment table entries of the bodies indexed from
!        1 to I-1.
!
         nbtdel = i - 1

         if ( nstdel .gt. 0 ) then
!
!           Adjust the tables to be consistent with the deletions.
!
!           Shift the body table and update the body table pointers.
!
            do i = nbtdel+1, btnbod

               j         = i - nbtdel
               btbody(j) = btbody(i)
               btstsz(j) = btstsz(i)
               btsegp(j) = btsegp(i) - nstdel

            end do

!
!           Update the body table count.
!
            btnbod = btnbod - nbtdel

!
!           Shift the segment table entries forward by NSTDEL to make
!           room at the rear of the table.
!
            do i = nstdel+1, stfree-1

               j        = i - nstdel
               sthan(j) = sthan(i)

               call moved( stdscr(1,i), dskdsz, stdscr(1,j) )
               call movei( stdlad(1,i), dladsz, stdlad(1,j) )
               call moved( stoff (1,i), 3,      stoff (1,j) )
               call moved( stctr (1,i), 3,      stctr (1,j) )

               strad(j) = strad(i)

            end do

            stfree = stfree - nstdel

         end if

      end if

      call chkout ( 'ZZDSKSBR' )
      return
      end
