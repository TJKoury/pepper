!$Procedure ZZSWFXFM ( Private, get switch frame transformation )

      subroutine zzswfxfm ( infrm, et, ndim, xform, outfrm, found )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Return switch frame state or position transformation matrix.
!
! $ Required_Reading
!
!     CK
!     FRAMES
!     NAIF_IDS
!     TIME
!
! $ Keywords
!
!     None.
!
! $ Declarations

      implicit none

      include              'frmtyp.inc'
      include              'zzswtchf.inc'
      include              'zzctr.inc'

      integer(ip)           infrm
      real(wp)              et
      integer(ip)           ndim
      real(wp)              xform ( ndim, ndim )
      integer(ip)           outfrm
      logical               found

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     INFRM      I   Frame ID code of switch frame.
!     ET         I   Time, expressed as seconds past J2000 TDB.
!     NDIM       I   Dimension: 3 or 6.
!     XFORM      O   Transformation from switch to selected base frame.
!     OUTFRM     O   ID code of selected base frame.
!     FOUND      O   Flag indicating whether transformation was found.
!
! $ Detailed_Input
!
!     INFRM       is the frame ID code of switch frame for which
!                 the transformation to a base frame at time ET is
!                 sought.
!
!     ET          is a frame orientation evaluation epoch, expressed
!                 as seconds past J2000 TDB.
!
!     NDIM        is the common row and column dimension of the output
!                 matrix: 6 or 3. When NDIM is set to 6, a 6x6 state
!                 transformation matrix is returned; when NDIM is set
!                 to 3, a 3x3 position transformation matrix is
!                 returned.
!
! $ Detailed_Output
!
!     XFORM       is a matrix that transforms vectors of dimension NDIM
!                 from the switch frame designated by INFRM to the
!                 frame designated by OUTFRM, at time ET.
!
!                 By definition, a switch frame is aligned with a
!                 selected base frame at time ET. However, depending on
!                 the class of the selected base frame, OUTFRM may
!                 be set to that base frame, the base frame at ET of
!                 that base, or the frame J2000. See the description of
!                 OUTFRM below.
!
!                 XFORM is defined if and only if FOUND is .TRUE.
!
!
!     OUTFRM      is the frame ID code of a frame selected by this
!                 routine, which may be a base frame of the switch
!                 frame designated by INFRM, or which may be another
!                 frame, selected as described below. The output matrix
!                 XFORM transforms vectors of dimension NDIM from the
!                 switch frame designated by INFRM to the frame
!                 designated by OUTFRM.
!
!                 OUTFRM is defined if and only if FOUND is .TRUE.
!
!                 OUTFRM is selected as follows:
!
!                    - Base frames are obtained from the kernel
!                      variable
!
!                         FRAME_< INFRM >_ALIGNED_WITH
!
!                      Base frames are listed in order of ascending
!                      priority. A given frame may occur at multiple
!                      indices of the base frame array.
!
!                    - If time intervals are included in the switch
!                      frame specification, the highest priority
!                      base frame for which the associated time
!                      interval contains ET, and in which data are
!                      available at ET, is chosen.
!
!                      If time intervals are not provided, the highest
!                      priority base frame for which data are available
!                      at ET is chosen.
!
!                      Data availability may depend on NDIM. This is
!                      because 6x6 state transformations that rely on
!                      CK data require angular velocity to be present;
!                      3x3 position transformations don't.
!
!                      If no base frame providing data at ET is found
!                      by the process described above, FOUND is set to
!                      .FALSE. and OUTFRM is undefined.
!
!                    - If the process succeeds in finding a base frame
!                      providing data at ET, OUTFRM is set according to
!                      the class of the base frame:
!
!                         > Base is class INERTL: OUTFRM is set to
!                           the ID code of J2000.
!
!                         > Base is class PCK: OUTFRM is set to the ID
!                           code of J2000.
!
!                         > Base is class CK: OUTFRM is set to the ID
!                           code of the CK frame's base frame at time
!                           ET. This is the base returned by FRMGET for
!                           ET, if NDIM is 6, or the base returned by
!                           ROTGET at ET if NDIM is 3.
!
!                         > Base is class TK: OUTFRM is set to the ID
!                           code of the TK frame's base frame. This is
!                           the frame specified by the
!                           TKFRAME_<ID>_RELATIVE keyword of the TK
!                           frame's specification.
!
!                         > Base is class DYN: OUTFRM is set to the ID
!                           code of the base frame.
!
!                         > Base is class SWTCH: OUTFRM is set to the
!                           ID code of the base frame.
!
!
!     FOUND       is a logical flag indicating whether a frame
!                 transformation was found at time ET. XFORM and OUTFRM
!                 are valid if and only if FOUND is .TRUE.
!
! $ Parameters
!
!     See the include file zzswtchf.inc for SPICE-private parameters
!     defining sizes of buffers used by the switch frame subsystem.
!
! $ Exceptions
!
!     1)  If NDIM is a value other than 6 or 3, the error
!         SPICE(BADDIMENSION) will be signaled.
!
!     2)  If the switch frame designated by INFRM has more than MAXBAS
!         associated base frames, the error is signaled by a routine in
!         the call tree of this routine.
!
!     3)  If a frame specification cannot be found for a base frame of
!         the frame designated by INFRM, the error is signaled by a
!         routine in the call tree of this routine.
!
!     4)  If an error is found in the specification of the switch frame
!         designated by INFRM, the error is signaled by a routine in
!         the call tree of this routine.
!
!     5)  If an error occurs while computing the transformation from
!         the frame designated by INFRM to that designated by OUTFRM,
!         the error is signaled by a routine in the call tree of this
!         routine.

! $ Files
!
!     Appropriate kernels must be loaded by the calling program before
!     this routine is called.
!
!     The following data are required:
!
!         - A switch frame specification, normally provided by a
!           frame kernel.
!
!         - Frame specifications for all base frames. Normally these
!           specifications are provided by frame kernels.
!
!     The following data may be needed:
!
!         - A leapseconds kernel
!
!         - SCLK kernel parameters and data for each CK base frame.
!           This information is normally provided by SCLK kernels.
!
!         - CK data for each CK base frame. These data are provided
!           by C-kernels.
!
!         - PCK data for each PCK base frame.
!
!         - Any kernels required to specify any dynamic base frame.
!
!     In all cases, kernel data are normally loaded once per program
!     run, NOT every time this routine is called.
!
! $ Particulars
!
!     The switch frame subsystem does not participate in fake
!     recursion, hence there are no shadow routines corresponding to
!     ZZSWFXFM and ZZSWFFET.
!
!     The only frame orientation evaluations performed in the process
!     of computing the output transformation XFORM are those that
!     don't require recursive use of the frame subsystem. The frame
!     classes in this category are
!
!        Inertial
!        PCK
!        CK
!        TK
!
!     For dynamic and switch base frames, no orientation is computed.
!     Instead, the identity matrix is returned.
!
! $ Examples
!
!     None. See the routines FRMGET and ROTGET for examples of usage.
!
! $ Restrictions
!
!     1)  This routine is SPICE-private. User applications must not
!         call it.
!
!     2)  To improve efficiency, this routine buffers switch frame
!         specifications. The buffers are emptied after any update
!         to the kernel pool. Since the process of parsing, and
!         buffering parameters provided by, switch frame specifications
!         is rather slow, applications using switch frames should
!         avoid high-frequency kernel pool updates.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!     B.V. Semenov   (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 15-DEC-2021 (NJB) (BVS)
!
!-&

! $ Index_Entries
!
!     return switch frame transformation and base frame
!
!-&

!
!     SPICELIB functions
!
      logical               failed
      logical               return

      integer(ip)           lstled

!
!     Local parameters
!

!
!     ID code of the J2000 frame:
!
      integer(ip)           j2code
      parameter           ( j2code = 1 )

!
!     Size of linked list pool:
!
!     The add-only hash pool for frame IDs is singly linked.
!
      integer(ip)           fplsiz
      parameter           ( fplsiz = maxfrm - lbsngl + 1 )

!
!     Dimensions
!
      integer(ip)           d3
      parameter           ( d3 = 3 )

      integer(ip)           d6
      parameter           ( d6 = 6 )

!
!     Matrix sizes.
!
      integer(ip)           size33
      parameter           ( size33 =  9 )

      integer(ip)           size66
      parameter           ( size66 = 36 )

!
!     Local variables
!

!
!     Overview of frame storage data structures
!     =========================================
!
!     This routine uses an add-only hash to index switch frames
!     known to this routine. The hash maps input frame ID codes to
!     head nodes of collision lists stored in the array HDFRAM.
!     The collision lists are stored in the singly linked list pool
!     FRPOOL.
!
!     Each node of a collision list is the index in the array FIDLST of
!     a switch frame ID. The array FIDLST has the following associated,
!     parallel arrays:
!
!        BASBEG       Start indices in the base frame attribute arrays
!                     of values associated with specific switch frames.
!                     For example, BASLST( BASBEG(FRAMAT) ) is the
!                     index of the first base frame associated with
!                     the switch frame having ID FIDLST(FRAMAT).
!
!        BASCNT       Base count for each switch frame
!
!        USETIM       Indicates whether time intervals are associated
!                     with that frame's base frames
!
!        BINARY       Indicates whether time intervals are eligible for
!                     binary search. To be eligible, the intervals
!                     must have start times in ascending order and
!                     must be disjoint or have singleton overlap.
!
!     Base frame attributes are stored in a set of parallel arrays.
!     Attributes for bases of a given switch frame are listed in
!     contiguous elements. The arrays and their contents are:
!
!        BASLST       Base frame ID codes. The base frames of a given
!                     switch frame are listed in ascending priority
!                     order in consecutive elements of BASLST.
!
!        STARTS       Base frame interval start times, expressed as
!                     seconds past J2000 TDB. Valid for all bases of
!                     a given switch frame, or unused for that frame.
!
!        STOPS        Base frame interval start times, expressed as
!                     seconds past J2000 TDB. Valid for all bases of
!                     a given switch frame, or unused for that frame.
!
!        CLSSES       Base frame classes.
!
!        CLSIDS       Base frame class IDs.
!
!     Each base frame list for a given switch frame is ordered
!     according to ascending priority.
!
      real(wp)              starts ( maxbas )
      real(wp)              stops  ( maxbas )

      integer(ip)           basbeg ( maxfrm )
      integer(ip)           bascnt ( maxfrm )
      integer(ip)           baslst ( maxbas )
      integer(ip)           clsids ( maxbas )
      integer(ip)           clsses ( maxbas )
      integer(ip)           fidlst ( maxfrm )
      integer(ip)           frpool ( lbsngl : maxfrm )
      integer(ip)           hdfram ( maxfrm )

      logical               binary ( maxfrm )
      logical               usetim ( maxfrm )

!
!     Other declarations
!
      real(wp)              rmat   ( 3, 3 )
      real(wp)              tsipm  ( 6, 6 )
      real(wp)              xident ( 6, 6 )

      integer(ip)           basidx
      integer(ip)           framat
      integer(ip)           free
      integer(ip)           head
      integer(ip)           i
      integer(ip)           j
      integer(ip)           lb
      integer(ip)           lstbeg
      integer(ip)           polctr ( ctrsiz )
      integer(ip)           prvat
      integer(ip)           prvfrm

      logical               pass1
      logical               samfrm
      logical               timeok
      logical               update

!
!     Saved variables
!
      save                  basbeg
      save                  bascnt
      save                  baslst
      save                  clsids
      save                  clsses
      save                  binary
      save                  fidlst
      save                  free
      save                  frpool
      save                  hdfram
      save                  pass1
      save                  polctr
      save                  prvat
      save                  prvfrm
      save                  starts
      save                  stops
      save                  usetim
      save                  xident

!
!     Initial values
!
      data                  basbeg / maxfrm * 0       /
      data                  bascnt / maxfrm * 0       /
      data                  baslst / maxbas * 0       /
      data                  clsids / maxbas * 0       /
      data                  clsses / maxbas * 0       /
      data                  binary / maxfrm * .false. /
      data                  fidlst / maxfrm * 0       /
      data                  free   / 1                /
      data                  frpool / fplsiz * 0       /
      data                  hdfram / maxfrm * 0       /
      data                  pass1  / .true.           /
      data                  prvat  / 0                /
      data                  prvfrm / 0                /
      data                  starts / maxbas * 0.0_wp    /
      data                  stops  / maxbas * 0.0_wp    /
      data                  usetim / maxfrm * .false. /

      if ( return() ) return


!
!     No frame found yet.
!
      found = .false.

      if ( pass1 ) then
!
!        Initialize pool data structures.
!
         call zzswfini ( hdfram, frpool, fidlst, basbeg, &
                         free,   prvat,  prvfrm, samfrm  )
!
!        Initialize the local pool counter.
!
         call zzctruin ( polctr )
!
!        Note: no FAILED() check is needed after the two calls above.
!
!        Create a 6x6 identity matrix.
!
         call filld( 0.0_wp, size66, xident )

         do i = 1, 6
            xident(i,i) = 1.0_wp
         end do

         pass1 = .false.

      end if

!
!     Check the transformation dimension.
!
      if (  ( ndim /= d3 ) .and. ( ndim /= d6 )  ) then

         call setmsg ( 'Transformation dimension must ' &
         //            'be 3 or 6 but was #.'           )
         call errint ( '#', ndim                        )
         call sigerr ( 'SPICE(BADDIMENSION)'            )
         return

      end if

!
!     Reinitialize local database if the kernel pool has been
!     updated. The call below syncs POLCTR.
!
      call zzpctrck ( polctr, update )

      if ( update ) then
!
!        Initialize local data structures.
!
         call zzswfini ( hdfram, frpool, fidlst, basbeg, &
                         free,   prvat,  prvfrm, samfrm  )

!
!        SAMFRM is set to .FALSE. and PRVFRM is set to 0 at this point.
!
      else
!
!        SAMFRM indicates that INFRM is the same switch frame we saw on
!        the previous call, no kernel pool update has occurred since
!        that call, and that the call was successful. We don't save the
!        previous switch frame information for unsuccessful calls: for
!        those, we set PRVFRM to zero so that it won't match any valid
!        frame ID.
!
         samfrm = ( infrm /= 0 ) .and. ( infrm == prvfrm )

      end if

      if ( samfrm ) then
!
!        We can use information saved from the previous call. FRAMAT is
!        the index of the switch frame ID in the switch frame ID array.
!
         framat = prvat

      else
!
!        Look up the input frame.
!
         call zzhsichk ( hdfram, frpool, fidlst, infrm, framat )

      end if

      if ( framat == 0 ) then
!
!        This frame is unknown to the switch frame subsystem. Try to
!        look up the frame's specification from the kernel pool.
!        ZZSWFFET checks and returns the frame specification.
!
         call zzswffet ( infrm,  hdfram, frpool, fidlst, basbeg, &
                         free,   bascnt, usetim, binary, clsses, &
                         clsids, baslst, starts, stops,  framat )

         if (  failed()  .or.  ( framat == 0 )  ) then
!
!           We were unable to obtain information for the requested
!           frame. Initialize all information about the previous frame.
!           For safety, initialize the local database.
!
            call zzswfini ( hdfram, frpool, fidlst, basbeg, &
                            free,   prvat,  prvfrm, samfrm  )

            return

         end if

      end if

!
!     At this point, FRAMAT is set.
!
!     Below, HEAD is the common index in each parallel array of base
!     frame data of the first item for a given switch frame. BASIDX is
!     the current index. The index obtained by adding a switch frame's
!     base frame count to HEAD is the successor of the last index of
!     base frame data for that switch frame.
!
      head = basbeg( framat )

!
!     At this point, the frame's information is present in the
!     local database. FRAMAT is the index of the frame in the hash
!     data structure. HEAD is the start index of the base frame data
!     for the input switch frame.
!
!     Search for the highest-priority frame that provides data
!     for the request time.
!
!     For safety, reset FOUND.
!
      found = .false.

      if ( binary(framat) ) then
!
!        We'll do a binary search. Suppress follow-on linear searching
!        and frame data lookup by default: when BASIDX is zero, the
!        linear search and data fetch loop won't execute. We'll update
!        BASIDX if an interval is found.
!
         basidx = 0

!
!        Find the last interval, if any, that contains ET. Start by
!        finding the last start time that's less than or equal to ET.
!
!        Here we rely on the assumption that start and stop times for
!        each frame are stored in increasing time order.
!
         lstbeg = lstled( et, bascnt(framat), starts(head) )

!
!        If LSTBEG is zero, then ET is strictly less than all of the
!        start times, in which case there's no match.
!
         if ( lstbeg > 0 ) then
!
!           ET could belong to the interval having start time stored at
!           relative index LSTBEG, which is equivalent to the base
!           frame array index HEAD+LSTBEG-1.
!
            if (  et <= stops( head+lstbeg-1 )  ) then
!
!              This interval contains ET. Prepare to fetch data for
!              the base frame corresponding to this interval.
!
               basidx = head + lstbeg - 1

            end if

         end if

      else
!
!        Prepare for a linear search and fetching base frame data.
!
         basidx = head + bascnt(framat) - 1

      end if

!
!     Search the base list in order of decreasing priority. The search
!     will terminate once we've examined the base frame at index LB,
!     if not before.
!
      lb = head

      do while (  ( basidx >= lb ) .and. ( .not. found )  )

         if (  usetim( framat )  ) then

            timeok =       ( et >= starts(basidx) ) &
                     .and. ( et <= stops (basidx) )
         else
            timeok = .false.
         end if

         if (  timeok .or. ( .not. usetim(framat) )  ) then
!
!           This base frame is applicable.
!
            if ( clsses(basidx) == ck ) then
!
!              We can't know whether there are CK data for the request
!              time without looking up the data, so that's what we do.
!              If we find data, we return the transformation to the CK
!              frame's base frame and the frame ID of the CK frame's
!              base frame.
!
               if ( ndim == d6 ) then

                  call ckfxfm ( clsids(basidx), et, &
                                xform, outfrm,  found )
               else

                  call ckfrot ( clsids(basidx), et, &
                                xform, outfrm,  found )
               end if

               if ( failed() ) then

                  call zzswfini ( hdfram, frpool, fidlst, basbeg, &
                                  free,   prvat,  prvfrm, samfrm  )

                  return

               end if

!
!              If we got this far and we're in binary search mode, we
!              won't find a result, except possibly when ET is equal to
!              the interval start time.
!
               if ( binary(framat) .and. ( .not. found ) ) then

                  if ( et > starts(basidx) ) then
!
!                    The next lowest-indexed interval doesn't cover ET,
!                    so no data can be found to satisfy this request.
!
                     prvfrm = 0
                     prvat  = 0

                     return

                  end if

               end if
!
!              FOUND is set. If we did find data, XFORM and OUTFRM
!              are set. If FOUND is .FALSE., we'll look at the
!              remaining lower-priority bases, if any, if we're not in
!              binary search mode. If we're in binary search mode,
!              we'll look at the next lower-priority base only if ET is
!              equal to the interval start time.
!

            else if ( clsses(basidx) == tk ) then

               call tkfram ( clsids(basidx), rmat, outfrm, found )

               if ( failed() ) then

                  found  = .false.

                  call zzswfini ( hdfram, frpool, fidlst, basbeg, &
                                  free,   prvat,  prvfrm, samfrm  )

                  return

               end if

               if ( ndim == d6 ) then

                  do i = 1,3
                     do j = 1,3
                        xform(i,  j  ) = rmat(i,j)
                        xform(i+3,j+3) = rmat(i,j)
                        xform(i+3,j  ) = 0.0_wp
                        xform(i,  j+3) = 0.0_wp
                     end do
                  end do

               else
                  call moved( rmat, size33, xform )
               end if
!
!              OUTFRM and FOUND have been set by TKFRAM.
!

            else if ( clsses(basidx) == pck ) then

               if ( ndim == d6 ) then

                  call tisbod ( 'J2000', clsids(basidx), et, tsipm )
                  call invstm ( tsipm, xform )
               else
                  call tipbod ( 'J2000', clsids(basidx), et, rmat  )
                  call xpose  ( rmat,  xform )
               end if

               if ( failed() ) then

                  found  = .false.

                  call zzswfini ( hdfram, frpool, fidlst, basbeg, &
                                  free,   prvat,  prvfrm, samfrm  )

                  return

               end if

               outfrm = j2code
               found  = .true.


            else if ( clsses(basidx) == inertl ) then

               call irfrot ( baslst(basidx), j2code, rmat )

               if ( failed() ) then

                  found  = .false.

                  call zzswfini ( hdfram, frpool, fidlst, basbeg, &
                                  free,   prvat,  prvfrm, samfrm )

                  return

               end if

               if ( ndim == d6 ) then

                  do i = 1,3
                     do j = 1,3
                        xform(i,  j  ) = rmat(i,j)
                        xform(i+3,j+3) = rmat(i,j)
                        xform(i+3,j  ) = 0.0_wp
                        xform(i,  j+3) = 0.0_wp
                     end do
                  end do

               else
                  call moved( rmat, size33, xform )
               end if

               outfrm = j2code
               found  = .true.

            else
!
!              For other non-CK frames---switch and dynamic---we just
!              return the base frame and the identity matrix.
!
               if ( ndim == d6 ) then

                  call moved ( xident, size66, xform )
               else
                  call ident ( xform )
               end if

               outfrm = baslst( basidx )
               found  = .true.

            end if

         end if

!
!        If we got this far and we're in binary search mode,
!        we've already found a result. We'll exit the loop at
!        the next loop condition test.
!
!        Check the next lower priority base frame and interval.
!
         basidx = basidx - 1

      end do

!
!     FOUND is set.
!
!     We either found data, or we ran out of base frames to try.
!     If we found data, the outputs XFORM and OUTFRM are set.
!
      if ( found ) then
!
!        Save values for re-use.
!
         prvfrm = infrm
         prvat  = framat

      else
!
!        Ensure the saved values won't be re-used.
!
         prvfrm = 0

      end if

      return
      end
