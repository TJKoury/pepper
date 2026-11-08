!$Procedure ZZDSKSBA ( DSK, add entry to API segment buffer )

      subroutine zzdsksba ( bodyid, &
                            maxbod, stsize, btbody, btnbod, btsegp, &
                            btstsz, sthan,  stdscr, stdlad, stfree, &
                            stoff,  stctr,  strad                  )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Add an entry for a specified body to the DSK API segment buffer
!     data structure.
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

      integer(ip)           bodyid
      integer(ip)           maxbod
      integer(ip)           stsize
      integer(ip)           btbody ( * )
      integer(ip)           btnbod
      integer(ip)           btsegp ( * )
      integer(ip)           btstsz ( * )
      integer(ip)           sthan  ( * )
      real(wp)              stdscr ( dskdsz, * )
      integer(ip)           stdlad ( dladsz, * )
      integer(ip)           stfree
      real(wp)              stoff  ( 3, * )
      real(wp)              stctr  ( 3, * )
      real(wp)              strad  ( * )

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
      integer(ip)           isrchi

      logical               failed
      logical               return
      logical               zzdsksbd

!
!     External routines
!
      external              zzdskbdc

!
!     Local parameters
!
      integer(ip)           frnmln
      parameter           ( frnmln = 32 )

!
!     Local variables
!
      character(frnmln)     frname

      real(wp)              dskdsc ( dskdsz )
      real(wp)              et
      real(wp)              lt

      integer(ip)           avail
      integer(ip)           dladsc ( dladsz )
      integer(ip)           frmctr
      integer(ip)           handle
      integer(ip)           i
      integer(ip)           j
      integer(ip)           nseg
      integer(ip)           segcld
      integer(ip)           segcls
      integer(ip)           segctr
      integer(ip)           segfid

      logical               frmfnd
      logical               segfnd
      logical               status


      if ( return() ) return


!
!     Check the body table for presence of the new body. It's
!     an error to call this routine for a body that's already
!     present. (Such a call likely indicates the tables were
!     not re-initialized after a BSR state change.)
!
      i = isrchi( bodyid, btnbod, btbody )

      if ( i > 0 ) then

         call setmsg ( 'Body # is already present in the DSK segment ' &
         //            'buffer body table. The table must be ' &
         //            're-initialized before this body can be added.' )
         call errint ( '#', bodyid                                     )
         call sigerr ( 'SPICE(INVALIDADD)'                             )
         return

      end if

!
!     Make sure the BSR segment list for the body is up to
!     date.
!
      call zzdskbbl ( bodyid )

      if ( failed() ) return

!
!     Count the segments in the BSR system for the body.
!
      nseg   = 0
      status = zzdsksbd( bodyid )

      call zzdskbss( bodyid )
      call zzdsksns( zzdskbdc, handle, dladsc, dskdsc, segfnd )

      if ( failed() ) return

      do while ( segfnd )

         nseg = nseg + 1

         call zzdsksns( zzdskbdc, handle, dladsc, dskdsc, segfnd )

         if ( failed() ) return

      end do

!
!     Check the number of segments for BODY against the size of the
!     segment table. If the table isn't big enough, we can't make
!     room by deleting existing entries. This is a backstop check;
!     this situation should not occur if STSIZE is consistent with
!     the value in ZZDSKBSR.
!
      if ( nseg > stsize ) then

         call setmsg ( 'The number of segments for body # is #; ' &
         //            'the size STSIZE of the input segment table ' &
         //            'is #.'                                       )
         call errint ( '#', bodyid                                   )
         call errint ( '#', nseg                                     )
         call errint ( '#', stsize                                   )
         call sigerr ( 'SPICE(SEGMENTTABLEFULL)'                     )
         return

      end if

!
!     If we don't have enough room to store new entries in the body
!     table or in the segment table, make room.
!
      avail = stsize - stfree + 1

      if (  ( btnbod == maxbod ) .or. ( avail < nseg )  ) then

         call zzdsksbr ( nseg, &
                         maxbod, stsize, btbody, btnbod, btsegp, &
                         btstsz, sthan,  stdscr, stdlad, stfree, &
                         stoff,  stctr,  strad                  )

         if ( failed() ) return

      end if

!
!     Append the new body ID to the body table. We've ensured there's
!     room in the table.
!
      btnbod           = btnbod + 1
      btbody( btnbod ) = bodyid
      btsegp( btnbod ) = stfree
      btstsz( btnbod ) = nseg

!
!     Make a second pass through the BSR segment list, this time
!     accumulating segments in the input segment table as we go.
!
      status = zzdsksbd( bodyid )

      call zzdskbss( bodyid )
      call zzdsksns( zzdskbdc, handle, dladsc, dskdsc, segfnd )

      if ( failed() ) return

      do while ( segfnd )
!
!        Insert handle and descriptor data for the current segment at
!        index STFREE in the segment table.
!
         sthan( stfree ) = handle

         call movei ( dladsc, dladsz, stdlad(1,stfree) )
         call moved ( dskdsc, dskdsz, stdscr(1,stfree) )

         stfree          = stfree + 1

         call zzdsksns( zzdskbdc, handle, dladsc, dskdsc, segfnd )

         if ( failed() ) return

      end do

!
!     Compute bounding spheres and frame center offsets for each
!     segment.
!
      do i = 1, nseg
!
!        J is the index in the segment table of the Ith segment
!        for BODYID.
!
         j  = btsegp( btnbod ) + i - 1

         call zzsegbox ( stdscr(1,j), stctr(1,j), strad(j) )

         if ( failed() ) return

!
!        Obtain the center of the frame for the Ith segment.
!
         segfid = nint( stdscr(frmidx,j) )

         call frinfo ( segfid, frmctr, segcls, segcld, frmfnd )

         if ( .not. frmfnd ) then

            call setmsg ( 'Could not look up frame info for ' &
            //            'segment frame having ID #.'       )
            call errint ( '#', segfid                        )
            call sigerr ( 'SPICE(NOFRAMEINFO)'               )
            return

         end if
!
!        If the frame center is not the same as the central
!        body, compute the offset between the two. Otherwise
!        set the offset to zero.
!
         segctr = nint( stdscr(ctridx,j) )

         if ( segctr == frmctr ) then

            call cleard ( 3, stoff(1,j) )

         else

            call frmnam ( segfid, frname )

            if ( frname == ' ' ) then

               call setmsg ( 'Could not look up frame info for ' &
               //            'segment frame having ID #.'       )
               call errint ( '#', segfid                        )
               call sigerr ( 'SPICE(NOFRAMENAME)'               )
               return

            end if

!
!           Note that SPK data must be available at the midpoint of the
!           DSK coverage epoch in order for the following call to work.
!
            et = ( stdscr(btmidx,j) + stdscr(etmidx,j) ) / 2

            call spkgps ( frmctr, et, frname, segctr, stoff(1,j), lt )

            if ( failed() ) return

         end if

      end do


      return
      end
