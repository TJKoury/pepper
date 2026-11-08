!$Procedure REFCHG (Reference frame Change)

      subroutine refchg ( frame1, frame2, et, rotate )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Return the transformation matrix from one
!     frame to another.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     FRAMES
!
! $ Declarations

      implicit none

      include              'errhnd.inc'
      include              'frmtyp.inc'

      integer(ip)           frame1
      integer(ip)           frame2
      real(wp)              et
      real(wp)              rotate ( 3, 3 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     FRAME1     I   the frame id-code for some reference frame
!     FRAME2     I   the frame id-code for some reference frame
!     ET         I   an epoch in TDB seconds past J2000.
!     ROTATE     O   a rotation matrix
!
! $ Detailed_Input
!
!     FRAME1   is the frame id-code in which some positions
!              are known.
!
!     FRAME2   is the frame id-code for some frame in which you
!              would like to represent positions.
!
!     ET       is the epoch at which to compute the transformation
!              matrix. This epoch should be in TDB seconds past
!              the ephemeris epoch of J2000.
!
! $ Detailed_Output
!
!     ROTATE   is a 3 x 3 rotation matrix that can be used to
!              transform positions relative to the frame
!              corresponding to frame FRAME2 to positions relative
!              to the frame FRAME2. More explicitly, if POS is
!              the position of some object relative to the
!              reference frame of FRAME1 then POS2 is the position
!              of the same object relative to FRAME2 where POS2 is
!              computed via the subroutine call below
!
!                 CALL MXV ( ROTATE, POS, POS2 )
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If either of the reference frames is unrecognized, the error
!         SPICE(UNKNOWNFRAME) is signaled.
!
!     2)  If the auxiliary information needed to compute a non-inertial
!         frame is not available, an error is signaled
!         by a routine in the call tree of this routine.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine allows you to compute the rotation matrix
!     between two reference frames.
!
! $ Examples
!
!     Suppose that you have a position POS1 at epoch ET
!     relative to  FRAME1 and wish to determine its representation
!     POS2 relative to FRAME2. The following subroutine calls
!     would suffice to make this rotation.
!
!        CALL REFCHG ( FRAME1, FRAME2, ET,   ROTATE )
!        CALL MXV    ( ROTATE, POS1,   POS2 )
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.1.0, 08-OCT-2021 (JDR) (NJB)
!
!        Bug fix: added calls to FAILED after each call to
!        FRINFO and to ROTGET.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 2.0.0, 14-DEC-2008 (NJB)
!
!        Upgraded long error message associated with frame
!        connection failure.
!
!-    SPICELIB Version 1.2.0, 26-APR-2004 (NJB)
!
!        Another typo was corrected in the long error message, and
!        in a comment.
!
!-    SPICELIB Version 1.1.0, 23-MAY-2000 (WLT)
!
!        A typo was corrected in the long error message.
!
!-    SPICELIB Version 1.0.0, 09-JUL-1998 (WLT)
!
!-&


! $ Index_Entries
!
!     Rotate positions from one frame to another
!
!-&


!
!     SPICE functions
!
      integer(ip)           isrchi

      logical               failed
      logical               return

!
!     Local Parameters
!
      integer(ip)           maxnod
      parameter           ( maxnod = 10 )

      integer(ip)           maxxfm
      parameter           ( maxxfm = maxnod + 4 )

!
!     The root of all reference frames is J2000 (Frame ID = 1).
!
      integer(ip)           rootf
      parameter           ( rootf = 1 )

!
!     Local Variables
!
      character*(lmsgln)    errmsg

!
!     ROT contains the rotations from FRAME1 to FRAME2
!     ROT(1...3,1...3,I) has the rotation from FRAME(I)
!     to FRAME(I+1).  We make extra room in ROT because we
!     plan to add rotations beyond the obvious chain from
!     FRAME1 to a root node.
!
      real(wp)              rot ( 3, 3, maxxfm )

!
!     ROT2 is used to store intermediate rotation from
!     FRAME2 to some node in the chain from FRAME1 to PCK or
!     INERTL frames.
!
      real(wp)              rot2  ( 3, 3, 2     )
      real(wp)              tmprot( 3, 3        )

      integer(ip)           cent
      integer(ip)           class
      integer(ip)           clssid
      integer(ip)           cmnode
      integer(ip)           get
!
!     FRAME contains the frames we transform from in going from
!     FRAME1 to FRAME2.  FRAME(1) = FRAME1 by  construction.
!
      integer(ip)           frame ( maxnod )
      integer(ip)           i
      integer(ip)           inc
      integer(ip)           j
!
!     NODE counts the number of rotations needed to go
!     from FRAME1 to FRAME2.
!
      integer(ip)           node
      integer(ip)           put
      integer(ip)           relto
      integer(ip)           this

      logical               done
      logical               found
      logical               gotone

!
!     Standard SPICE error handling.
!
      if ( return() ) then
         return
      end if

      call chkin ( 'REFCHG')
!
!     Do the obvious thing first.  If FRAME1 and FRAME2 are the
!     same then we simply return the identity matrix.
!
      if ( frame1 .eq. frame2 ) then

         call ident  ( rotate )
         call chkout ( 'REFCHG' )
         return

      end if
!
!     Now perform the obvious check to make sure that both
!     frames are recognized.
!
      call frinfo ( frame1, cent, class, clssid, found )

      if ( failed() ) then
         call chkout ( 'REFCHG' )
         return
      end if

      if ( .not. found ) then

         call setmsg ( 'The number # is not a recognized id-code ' &
         //            'for a reference frame. ' )
         call errint ( '#', frame1 )
         call sigerr ( 'SPICE(UNKNOWNFRAME)'  )
         call chkout ( 'REFCHG' )
         return

      end if


      call frinfo ( frame2, cent, class, clssid, found )

      if ( failed() ) then
         call chkout ( 'REFCHG' )
         return
      end if

      if ( .not. found ) then

         call setmsg ( 'The number # is not a recognized id-code ' &
         //            'for a reference frame. ' )
         call errint ( '#', frame2 )
         call sigerr ( 'SPICE(UNKNOWNFRAME)'  )
         call chkout ( 'REFCHG' )
         return

      end if



      node        = 1
      frame(node) = frame1
      found       = .true.
!
!     Follow the chain of rotations until we run into
!     one that rotates to J2000 (frame id = 1) or we hit FRAME2.
!
      do while (       frame(node) .ne. rootf &
                 .and. node        .lt. maxnod &
                 .and. frame(node) .ne. frame2 &
                 .and. found   )
!
!        Find out what rotation is available for this
!        frame.
!
         call rotget ( frame(node),   et, &
                       rot(1,1,node), frame(node+1), found  )

         if ( failed() ) then
            call chkout ( 'REFCHG' )
            return
         end if

         if ( found ) then
!
!           We found a rotation matrix.  ROT(1,1,NODE)
!           now contains the rotation from FRAME(NODE)
!           to FRAME(NODE+1).  We need to look up the information
!           for the next NODE.
!
            node = node + 1

         end if

      end do

      done =      frame(node) .eq. rootf &
             .or. frame(node) .eq. frame2 &
             .or. .not. found


      do while ( .not. done )
!
!        The only way to get to this point is to have run out of
!        room in the array of reference frame rotation
!        buffers.  We will now build the rotation from
!        the previous NODE to whatever the next node in the
!        chain is.  We'll do this until we get to one of the
!        root classes or we run into FRAME2.
!
         call rotget ( frame(node), et, rot(1,1,node), relto, found )

         if ( failed() ) then
            call chkout ( 'REFCHG' )
            return
         end if

         if ( found  ) then
!
!           Recall that ROT(1,1,NODE-1) contains the rotation
!           from FRAME(NODE-1) to FRAME(NODE).  We are going to replace
!           FRAME(NODE) with the frame indicated by RELTO.  This means
!           that ROT(1,1,NODE-1) should be replaced with the
!           rotation from FRAME(NODE) to RELTO.
!
            frame(node) = relto
            call zzrxr ( rot(1,1,node-1), 2, tmprot )

            do i = 1,3
               do j = 1,3
                  rot(i,j,node-1) = tmprot(i,j)
               end do
            end do

         end if
!
!        We are done if the class of the last frame is J2000
!        or if the last frame is FRAME2 or if we simply couldn't get
!        another rotation.
!
         done =        frame(node) .eq. rootf &
                  .or. frame(node) .eq. frame2 &
                  .or. .not. found

      end do

!
!     Right now we have the following situation.  We have in hand
!     a collection of rotations between frames. (Assuming
!     that is that NODE .GT. 1.  If NODE .EQ. 1 then we have
!     no rotations computed yet.
!
!
!     ROT(1...3, 1...3, 1    )    rotates FRAME1   to FRAME(2)
!     ROT(1...3, 1...3, 2    )    rotates FRAME(2) to FRAME(3)
!     ROT(1...3, 1...3, 3    )    rotates FRAME(3) to FRAME(4)
!        .
!        .
!        .
!     ROT(1...3, 1...3, NODE-1 )  rotates FRAME(NODE-1)
!                                   to         FRAME(NODE)
!
!
!     One of the following situations is true.
!
!     1)  FRAME(NODE) is the root of all frames, J2000.
!
!     2)  FRAME(NODE) is the same as FRAME2
!
!     3)  There is no rotation from FRAME(NODE) to another
!         more fundamental frame.  The chain of rotations
!         from FRAME1 stops at FRAME(NODE).  This means that the
!         "frame atlas" is incomplete because we can't get to the
!         root frame.
!
!     We now have to do essentially the same thing for FRAME2.
!

      if ( frame(node) .eq. frame2 ) then
!
!        We can handle this one immediately with the private routine
!        ZZRXR which multiplies a series of matrices.
!
         call zzrxr (  rot, node-1, rotate )
         call chkout ( 'REFCHG' )
         return

      end if

!
!     We didn't luck out above.  So we follow the chain of
!     rotation for FRAME2.  Note that at the moment the
!     chain of rotations from FRAME2 to other frames
!     does not share a node in the chain for FRAME1.
!    ( GOTONE = .FALSE. ) .
!
      this   =  frame2
      gotone = .false.

!
!     First see if there is any chain to follow.
!
      done   =  this .eq. rootf

!
!     Set up the matrices ROT2(,,1) and ROT(,,2)  and set up
!     PUT and GET pointers so that we know where to GET the partial
!     rotation from and where to PUT partial results.
!
      if ( .not. done ) then

         put = 1
         get = 1
         inc = 1

      end if

!
!     Follow the chain of rotations until we run into
!     one that rotates to the root frame or we land in the
!     chain of nodes for FRAME1.
!
!     Note that this time we will simply keep track of the full
!     rotation from FRAME2 to the last node.
!
      do while (  .not. done )
!
!        Find out what rotation is available for this
!        frame.
!
         if ( this  .eq. frame2 ) then
!
!           This is the first pass, just put the rotation
!           directly into ROT2(,,PUT).
!
            call rotget ( this,  et, rot2(1,1,put), relto, found )

            if ( failed() ) then
               call chkout ( 'REFCHG' )
               return
            end if

            if ( found ) then
               this   = relto
               get    = put
               put    = put + inc
               inc    =     - inc
               cmnode = isrchi ( this, node, frame )
               gotone = cmnode .gt. 0
            end if

         else
!
!           Fetch the rotation into a temporary spot TMPROT
!
            call rotget ( this,  et, tmprot, relto, found )

            if ( failed() ) then
               call chkout ( 'REFCHG' )
               return
            end if

            if ( found ) then
!
!              Next multiply TMPROT on the right by the last partial
!              product (in ROT2(,,GET) ).  We do this in line.
!
               do i=1,3
                  do j=1,3

                     rot2(i,j,put) = tmprot(i,1)*rot2(1,j,get) &
                                   + tmprot(i,2)*rot2(2,j,get) &
                                   + tmprot(i,3)*rot2(3,j,get)
                  end do
               end do

!
!              Adjust GET and PUT so that GET points to the slots
!              where we just stored the result of our multiply and
!              so that PUT points to the next available storage
!              locations.
!
               get    = put
               put    = put + inc
               inc    =     - inc
               this   = relto

               cmnode = isrchi ( this, node, frame )
               gotone = cmnode .gt. 0

            end if

         end if

!
!        See if we have a common node and determine whether or not
!        we are done with this loop.
!
         done   =       this  .eq. rootf &
                  .or.  gotone &
                  .or. .not. found

      end do

!
!     There are two possible scenarios.  Either the chain of
!     rotations from FRAME2 ran into a node in the chain for
!     FRAME1 or it didn't.  (The common node might very well be
!     the root node.)  If we didn't run into a common one, then
!     the two chains don't intersect and there is no way to
!     get from FRAME1 to FRAME2.
!
      if ( .not. gotone ) then

         call zznofcon ( et, frame1, frame(node), frame2, this, errmsg )

         if ( failed() ) then
!
!           We were unable to create the error message. This
!           unfortunate situation could arise if a frame kernel
!           is corrupted.
!
            call chkout ( 'REFCHG' )
            return

         end if
!
!        The normal case: signal an error with a descriptive long
!        error message.
!
         call setmsg ( errmsg                  )
         call sigerr ( 'SPICE(NOFRAMECONNECT)' )
         call chkout ( 'REFCHG'                )
         return

      end if

!
!     Recall that we have the following.
!
!     ROT(1...3, 1...3, 1    )    rotates FRAME(1) to FRAME(2)
!     ROT(1...3, 1...3, 2    )    rotates FRAME(2) to FRAME(3)
!     ROT(1...3, 1...3, 3    )    rotates FRAME(3) to FRAME(4)
!
!     ROT(1...3, 1...3, CMNODE-1) rotates FRAME(CMNODE-1)
!                                   to         FRAME(CMNODE)
!
!     and that ROT2(1,1,GET) rotates from FRAME2 to CMNODE.
!     Hence the inverse of ROT2(1,1,GET) rotates from CMNODE
!     to FRAME2.
!
!     If we compute the inverse of ROT2 and store it in
!     the next available slot of ROT (.i.e. ROT(1,1,CMNODE)
!     we can simply apply our custom routine that multiplies a
!     sequence of rotation matrices together to get the
!     result from FRAME1 to FRAME2.
!
      call xpose ( rot2(1,1,get), rot(1,1,cmnode) )
      call zzrxr ( rot,   cmnode, rotate          )

      call chkout ( 'REFCHG' )
      return

      end
