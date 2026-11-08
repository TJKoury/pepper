!$Procedure      ZZFRMCH1 (Frame Change)

      subroutine zzfrmch1 ( frame1, frame2, et, xform )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Return the state transformation matrix from one
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
      real(wp)              xform ( 6, 6 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     FRAME1     I   the frame id-code for some reference frame
!     FRAME2     I   the frame id-code for some reference frame
!     ET         I   an epoch in TDB seconds past J2000.
!     XFORM      O   a state transformation matrix
!
! $ Detailed_Input
!
!     FRAME1      is the frame id-code in which some states are known.
!
!     FRAME2      is the frame id-code for some frame in which you
!                 would like to represent states.
!
!     ET          is the epoch at which to compute the state
!                 transformation matrix.  This epoch should be
!                 in TDB seconds past the ephemeris epoch of J2000.
!
! $ Detailed_Output
!
!     XFORM       is a 6 x 6 state transformation matrix that can
!                 be used to transform states relative to the frame
!                 corresponding to frame FRAME2 to states relative
!                 to the frame FRAME2.  More explicitly, if STATE
!                 is the state of some object relative to the reference
!                 frame of FRAME1 then STATE2 is the state of the
!                 same object relative to FRAME2 where STATE2 is
!                 computed via the subroutine call below
!
!                    CALL MXVG ( XFORM, STATE, 6, 6, STATE2 )
!
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) If either of the reference frames is unrecognized, the error
!        SPICE(UNKNOWNFRAME) will be signaled.
!
!     2) If the auxiliary information needed to compute a non-inertial
!        frame is not available an error will be diagnosed and signaled
!        by a routine in the call tree of this routine.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine allows you to compute the state transformation matrix
!     between two reference frames.
!
!     The currently supported reference frames are IAU bodyfixed frames
!     and inertial reference frames.
!
! $ Examples
!
!     Example 1.  Suppose that you have a state STATE1 at epoch ET
!     relative to  FRAME1 and wish to determine its representation
!     STATE2 relative to FRAME2.  The following subroutine calls
!     would suffice to make this transformation.
!
!        CALL FRMCHG ( FRAME1, FRAME2, ET,   XFORM )
!        CALL MXVG   ( XFORM,  STATE1, 6, 6, STATE2 )
!
!
!
!     Example 2.  Suppose that you have the angular velocity, W, of some
!     rotation relative to FRAME1 at epoch ET and that you wish to
!     express this angular velocity with respect to FRAME2.  The
!     following subroutines will suffice to perform this computation.
!
!        CALL FRMCHG ( FRAME1, FRAME2, ET, STXFRM )
!
!     Recall that a state transformation matrix has the following form.
!
!
!            -               -
!           |                 |
!           |    R        0   |
!           |                 |
!           |                 |
!           |   dR            |
!           |   --        R   |
!           |   dt            |
!           |                 |
!            -               -
!
!
!     The velocity of an arbitrary point P undergoing rotation with the
!     angular velocity W is W x P
!
!     Thus the velocity of P in FRAME2 is:
!
!
!        dR
!        --  P    +    R (W x P )
!        dt
!
!           dR  t
!     =  (  -- R  R P   +  W  x P  )            ( 1 )
!           dt
!
!
!           dR  t                                              t
!     But   -- R  is skew symmetric  (simply differentiate  R*R to see
!           dt
!                    dR  t
!     this ).  Hence -- R R P  can be written as Ax(R*P) for some fixed
!                    dt
!
!     vector A.  Moreover the vector A can be read from the upper
!
!                            dR  t
!     triangular portion of  -- R  .  So that equation (1) above can
!                            dt
!
!     be re-written as
!
!         dR  t
!     = ( -- R  R*P   +  R*(WxP)  )
!         dt
!
!     = Ax(R*P) + R*W x R*P
!
!     = ( [A+R*W] x R*P )
!
!
!     From this final expression it follows that in FRAME2 the angular
!     velocity vector is given by [A+R*W].
!
!     The code below implements these ideas.
!
!        CALL FRMCHG ( FRAME1, FRAME2, ET, STXFRM )
!
!
!        DO I = 1, 3
!           DO J = 1, 3
!
!              RT  ( I, J ) = STXFRM ( I,   J )
!              DRDT( I, J ) = STXFRM ( I+3, J )
!
!           END DO
!        END DO
!
!        CALL MXMT ( DRDT, R, AMATRIX )
!
!        Read the angular velocity of R from the skew symmetric matrix
!
!         dR  t
!         -- R
!         dt
!
!        Recall that if A has components A1, A2, A3 then the matrix
!        corresponding to the cross product linear mapping is:
!
!            -               -
!           |   0  -A3    A2  |
!           |                 |
!           |  A3   0    -A1  |
!           |                 |
!           | -A2   A1    0   |
!            -               -
!
!        A(1) = -AMATRIX(2,3)
!        A(2) =  AMATRIX(1,3)
!        A(3) = -AMATRIX(1,2)
!
!        CALL MXV  ( R, W1,  W  )
!        CALL VADD ( A, W,   W2 )
!
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
!     W.L. Taber      (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.1.0, 08-OCT-2021 (NJB)
!
!        Bug fix: added calls to FAILED after each call to
!        FRINFO and to ZZFRMGT1.
!
!        Corrected typos in comments.
!
!-    SPICELIB Version 2.0.0, 14-DEC-2008 (NJB)
!
!        Upgraded long error message associated with frame
!        connection failure.
!
!-    SPICELIB Version 1.1.0, 25-JUL-1996 (WLT)
!
!        Bug Fix:
!
!        The previous edition of the routine had a bug in the
!        first pass of the DO WHILE that looks for a frame
!        in the chain of frames associated with FRAME2 that is
!        in common with the chain of frames for FRAME1.
!
!        On machines where variables are created as static
!        variables, this error could lead to finding a frame
!        when a legitimate path between FRAME1 and FRAME2
!        did not exist.
!
!-    SPICELIB Version 1.0.1, 06-MAR-1996 (WLT)
!
!        An typo was fixed in the Brief I/O section. It used
!        to say TDT instead of the correct time system TDB.
!
!-    SPICELIB Version 1.0.0, 28-SEP-1994 (WLT)
!
!-&

! $ Index_Entries
!
!     Transform states from one frame to another
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
      character(lmsgln)     errmsg

      real(wp)              tempxf( 6, 6 )
!
!     TRANS contains the transformations from FRAME1 to FRAME2
!     TRANS(1...6,1...6,I) has the transformation from FRAME(I)
!     to FRAME(I+1).  We make extra room in TRANS because we
!     plan to add transformations beyond the obvious chain from
!     FRAME1 to a root node.
!
      real(wp)              trans ( 6, 6, maxxfm )

!
!     TRANS2 is used to store intermediate transformations from
!     FRAME2 to some node in the chain from FRAME1 to PCK or
!     INERTL frames.
!
      real(wp)              trans2( 6, 6, 2      )


      integer(ip)           cent
      integer(ip)           class
      integer(ip)           clssid
      integer(ip)           cmnode
!
!     FRAME contains the frames we transform from in going from
!     FRAME1 to FRAME2.  FRAME(1) = FRAME1 by  construction.
!
      integer(ip)           frame ( maxnod )
      integer(ip)           get
      integer(ip)           i
      integer(ip)           inc
      integer(ip)           j
      integer(ip)           k
      integer(ip)           l

!
!     NODE counts the number of transformations needed to go
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

      call chkin ( 'ZZFRMCH1')
!
!     Do the obvious thing first.  If FRAME1 and FRAME2 are the
!     same then we simply return the identity matrix.
!
      if ( frame1 == frame2 ) then

         do i = 1, 6

            xform(i,i) = 1.0_wp

            do j = 1, i-1
               xform(i,j) = 0.0_wp
               xform(j,i) = 0.0_wp
            end do

         end do

         call chkout ( 'ZZFRMCH1' )
         return

      end if
!
!     Now perform the obvious check to make sure that both
!     frames are recognized.
!
      call frinfo ( frame1, cent, class, clssid, found )

      if ( failed() ) then
         call chkout ( 'ZZFRMCH1' )
         return
      end if

      if ( .not. found ) then

         call setmsg ( 'The number # is not a recognized id-code ' &
         //            'for a reference frame. ' )
         call errint ( '#', frame1 )
         call sigerr ( 'SPICE(UNKNOWNFRAME)'  )
         call chkout ( 'ZZFRMCH1' )
         return

      end if


      call frinfo ( frame2, cent, class, clssid, found )

      if ( failed() ) then
         call chkout ( 'ZZFRMCH1' )
         return
      end if

      if ( .not. found ) then

         call setmsg ( 'The number # is not a recognized id-code ' &
         //            'for a reference frame. ' )
         call errint ( '#', frame2 )
         call sigerr ( 'SPICE(UNKNOWNFRAME)'  )
         call chkout ( 'ZZFRMCH1' )
         return

      end if



      node        = 1
      frame(node) = frame1
      found       = .true.
!
!     Follow the chain of transformations until we run into
!     one that transforms to J2000 (frame id = 1) or we hit FRAME2.
!
      do while (       frame(node) /= rootf &
                 .and. node        < maxnod &
                 .and. frame(node) /= frame2 &
                 .and. found   )
!
!        Find out what transformation is available for this
!        frame.
!
         call zzfrmgt1 ( frame(node),     et, &
                         trans(1,1,node), frame(node+1), found )

         if ( failed() ) then
            call chkout ( 'ZZFRMCH1' )
            return
         end if

         if ( found ) then
!
!           We found a transformation matrix.  TRANS(1,1,NODE)
!           now contains the transformation from FRAME(NODE)
!           to FRAME(NODE+1).  We need to look up the information
!           for the next NODE.
!
            node = node + 1

         end if

      end do

      done =      frame(node) == rootf &
             .or. frame(node) == frame2 &
             .or. .not. found


      do while ( .not. done )
!
!        The only way to get to this point is to have run out of
!        room in the array of reference frame transformation
!        buffers.  We will now build the transformation from
!        the previous NODE to whatever the next node in the
!        chain is.  We'll do this until we get to one of the
!        root classes or we run into FRAME2.
!
         call zzfrmgt1( frame(node), et, trans(1,1,node), relto, found )

         if ( failed() ) then
            call chkout ( 'ZZFRMCH1' )
            return
         end if

         if ( found  ) then
!
!           Recall that TRANS(1,1,NODE-1) contains the transformation
!           from FRAME(NODE-1) to FRAME(NODE).  We are going to replace
!           FRAME(NODE) with the frame indicated by RELTO.  This means
!           that TRANS(1,1,NODE-1) should be replaced with the
!           transformation from FRAME(NODE) to RELTO.
!
            frame(node) = relto
            call zzmsxf ( trans(1,1,node-1), 2, tempxf )

            do i = 1,6
               do j = 1,6
                  trans(i,j,node-1) = tempxf(i,j)
               end do
            end do

         end if
!
!        We are done if the class of the last frame is J2000
!        or if the last frame is FRAME2 or if we simply couldn't get
!        another transformation.
!
         done =        frame(node) == rootf &
                  .or. frame(node) == frame2 &
                  .or. .not. found

      end do

!
!     Right now we have the following situation.  We have in hand
!     a collection of transformations between frames. (Assuming
!     that is that NODE .GT. 1.  If NODE .EQ. 1 then we have
!     no transformations computed yet.
!
!
!     TRANS(1...6, 1...6, 1    )    transforms FRAME1   to FRAME(2)
!     TRANS(1...6, 1...6, 2    )    transforms FRAME(2) to FRAME(3)
!     TRANS(1...6, 1...6, 3    )    transforms FRAME(3) to FRAME(4)
!        .
!        .
!        .
!     TRANS(1...6, 1...6, NODE-1 )  transforms FRAME(NODE-1)
!                                   to         FRAME(NODE)
!
!
!     One of the following situations is true.
!
!     1)  FRAME(NODE) is the root of all frames, J2000.
!
!     2)  FRAME(NODE) is the same as FRAME2
!
!     3)  There is no transformation from FRAME(NODE) to another
!         more fundamental frame.  The chain of transformations
!         from FRAME1 stops at FRAME(NODE).  This means that the
!         "frame atlas" is incomplete because we can't get to the
!         root frame.
!
!     We now have to do essentially the same thing for FRAME2.
!

      if ( frame(node) == frame2 ) then
!
!        We can handle this one immediately with the private routine
!        ZZMSXF which multiplies a series of state transformation
!        matrices.
!
         call zzmsxf (  trans, node-1, xform )
         call chkout ( 'ZZFRMCH1' )
         return

      end if

!
!     We didn't luck out above.  So we follow the chain of
!     transformation for FRAME2.  Note that at the moment the
!     chain of transformations from FRAME2 to other frames
!     does not share a node in the chain for FRAME1.
!    ( GOTONE = .FALSE. ) .
!
      this   =  frame2
      gotone = .false.

!
!     First see if there is any chain to follow.
!
      done   =  this == rootf

!
!     Set up the matrices TRANS2(,,1) and TRANS(,,2)  and set up
!     PUT and GET pointers so that we know where to GET the partial
!     transformation from and where to PUT partial results.
!
      if ( .not. done ) then

         do k = 1, 2
            do i=1,3
               do j = 4,6
                  trans2(i,j,k) = 0.0_wp
               end do
            end do
         end do

         put = 1
         get = 1
         inc = 1

      end if

!
!     Follow the chain of transformations until we run into
!     one that transforms to the root frame or we land in the
!     chain of nodes for FRAME1.
!
!     Note that this time we will simply keep track of the full
!     translation from FRAME2 to the last node.
!
      do while (  .not. done )
!
!        Find out what transformation is available for this
!        frame.
!
         if ( this  == frame2 ) then
!
!           This is the first pass, just put the transformation
!           directly into TRANS2(,,PUT).
!
            call zzfrmgt1 ( this, et, trans2(1,1,put), relto, found )

            if ( failed() ) then
               call chkout ( 'ZZFRMCH1' )
               return
            end if

            if ( found ) then
               this   = relto
               get    = put
               put    = put + inc
               inc    =     - inc
               cmnode = isrchi ( this, node, frame )
               gotone = cmnode > 0
            end if

         else
!
!           Fetch the transformation into a temporary spot TEMPXF
!
            call zzfrmgt1 ( this, et, tempxf, relto, found )

            if ( found ) then
!
!              Next multiply TEMPXF on the right by the last partial
!              product (in TRANS2(,,GET) ).  We do this in line because
!              we can cut down the number of multiplies to 3/8 of the
!              normal result of MXMG.  For a discussion of why this
!              works see ZZMSXF.
!
               do i=1,3
                  do j=1,3

                     trans2(i,j,put) = tempxf(i,1)*trans2(1,j,get) &
                                     + tempxf(i,2)*trans2(2,j,get) &
                                     + tempxf(i,3)*trans2(3,j,get)
                  end do
               end do

               do i=4,6
                  do j=1,3

                     trans2(i,j,put) = tempxf(i,1)*trans2(1,j,get) &
                                     + tempxf(i,2)*trans2(2,j,get) &
                                     + tempxf(i,3)*trans2(3,j,get) &
                                     + tempxf(i,4)*trans2(4,j,get) &
                                     + tempxf(i,5)*trans2(5,j,get) &
                                     + tempxf(i,6)*trans2(6,j,get)
                  end do
               end do
!
!              Note that we don't have to compute the upper right
!              hand block.  It's already set to zero by construction.
!
!              Finally we can just copy the lower right hand block
!              from the upper left hand block of the matrix.
!
               do i = 4,6
                  k = i - 3
                  do j = 4,6
                     l = j - 3
                     trans2(i,j,put) = trans2(k,l,put)
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
               gotone = cmnode > 0

            end if

         end if

!
!        See if we have a common node and determine whether or not
!        we are done with this loop.
!
         done   =       this  == rootf &
                  .or.  gotone &
                  .or. .not. found


      end do

!
!     There are two possible scenarios.  Either the chain of
!     transformations from FRAME2 ran into a node in the chain for
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
            call chkout ( 'ZZFRMCH1' )
            return

         end if
!
!        The normal case: signal an error with a descriptive long
!        error message.
!
         call setmsg ( errmsg                  )
         call sigerr ( 'SPICE(NOFRAMECONNECT)' )
         call chkout ( 'ZZFRMCH1'              )
         return

      end if

!
!     Recall that we have the following.
!
!     TRANS(1...6, 1...6, 1    )    transforms FRAME(1) to FRAME(2)
!     TRANS(1...6, 1...6, 2    )    transforms FRAME(2) to FRAME(3)
!     TRANS(1...6, 1...6, 3    )    transforms FRAME(3) to FRAME(4)
!
!     TRANS(1...6, 1...6, CMNODE-1) transforms FRAME(CMNODE-1)
!                                   to         FRAME(CMNODE)
!
!     and that TRANS2(1,1,GET) transforms from FRAME2 to CMNODE.
!     Hence the inverse of TRANS2(1,1,GET) transforms from CMNODE
!     to FRAME2.
!
!     If we compute the inverse of TRANS2 and store it in
!     the next available slot of TRANS (.i.e. TRANS(1,1,CMNODE)
!     we can simply apply our custom routine that multiplies a
!     sequence of transformation matrices together to get the
!     result from FRAME1 to FRAME2.
!
      call invstm ( trans2(1,1,get), trans(1,1,cmnode) )
      call zzmsxf ( trans,   cmnode, xform             )

      call chkout ( 'ZZFRMCH1' )
      return

      end
