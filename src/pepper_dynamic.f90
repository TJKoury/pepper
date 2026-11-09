!> Dynamic frames (ZZDYNFRM, ZZDYNROT) and the copies of the frame and SPK routines they use (ZZ*0, ZZ*1): SPICELIB has no recursion, so a dynamic frame defined by states of bodies calls copies one level down.
!>
!> The procedures keep SPICELIB's names and documentation; the context is
!> their first argument (interfaces and type-bound procedures in
!> pepper_instance). Moved here from src/spicelib by
!> tools/modularize_ctx.py; the external procedures with the original
!> interface are in tspice/src/legacy/pepper_legacy_dynamic.f90.
submodule (pepper_instance) pepper_dynamic

   implicit none

contains

      module procedure zzfrmgt1
!! ## Abstract
!!
!!     Find the transformation from a user specified frame to
!!     another frame at a user specified epoch.
!!
!! ## Required Reading
!!
!!     FRAMES
!!
!! ## Keywords
!!
!!     FRAMES
!!
!! ## Declarations
!!
!! ## Detailed Input
!!
!!     INFRM       is the SPICE ID-code for some reference frame.
!!
!!     ET          is an epoch in ephemeris seconds past J2000 at
!!                 which the user wishes to retrieve a state
!!                 transformation matrix.
!!
!! ## Detailed Output
!!
!!     XFORM       is a 6x6 matrix that transforms states relative to
!!                 INFRM to states relative to OUTFRM. (Assuming such a
!!                 transformation can be found.)
!!
!!     OUTFRM      is the SPICE ID-code of a reference frame. The 6x6
!!                 matrix XFORM transforms states relative to INFRM to
!!                 states relative to OUTFRM. The state transformation
!!                 is achieved by multiplying XFORM on the right by a
!!                 state relative to INFRM.  This is easily accomplished
!!                 via the subroutine call shown below.
!!
!!                    CALL MXVG ( XFORM, STATE, 6, 6, OSTATE )
!!
!!     FOUND       is a logical flag indicating whether or not a
!!                 transformation matrix could be found from INFRM to
!!                 some other frame. If a transformation matrix cannot
!!                 be found OUTFRM will be set to zero, FOUND will be
!!                 set to FALSE and XFORM will be returned as the zero
!!                 matrix.
!!
!! ## Parameters
!!
!!     See include file.
!!
!! ## Exceptions
!!
!!     1) If a transformation matrix cannot be located, then FOUND will
!!        be set to FALSE, OUTFRM will be set to zero and XFORM will be
!!        set to the zero 6x6 matrix.
!!
!!     2) If the class of the requested frame is not recognized the
!!        exception 'SPICE(UNKNOWNFRAMETYPE)' will be signaled.
!!
!! ## Files
!!
!!     LSK, SCLK, PCK, FK, SPK, and/or CK kernels may need to be loaded
!!     to provide the needed frame definition and transformation data.
!!
!! ## Particulars
!!
!!     This is a low level routine used for determining a chain of state
!!     transformation matrices from one frame to another.
!!
!! ## Examples
!!
!!     See FRMCHG.
!!
!! ## Restrictions
!!
!!     None.
!!
!! ## Literature References
!!
!!     None.
!!
!! ## Author and Institution
!!
!!     N.J. Bachman    (JPL)
!!     B.V. Semenov    (JPL)
!!     W.L. Taber      (JPL)
!!
!! ## Version
!!
!!-    SPICELIB Version 5.0.0, 18-SEP-2020 (NJB)
!!
!!        Support for switch frames was added. VERSN is now
!!        initialized via a DATA statement. Corrected long error
!!        message to use the term "class" rather than "class id-code."
!!
!!-    SPICELIB Version 4.0.0, 05-JAN-2014 (BVS)
!!
!!        To prevent operations with un-initialized DP numbers, wrapped
!!        IF ( .NOT. FAILED() ) ... END IF around output matrix
!!        transposition/re-assignment operations in all branches where
!!        the routine returning the matrix might fail.
!!
!!        Added zero-ing out the output matrix in cases of a failed or
!!        .NOT. FOUND transformation look ups.
!!
!!-    SPICELIB Version 3.0.0, 18-DEC-2004 (NJB)
!!
!!        Added the new frame type 'DYN' to the list of frame
!!        types recognized by FRMGET.
!!
!!-    SPICELIB Version 2.0.0, 03-APR-1997 (WLT)
!!
!!        Added the new frame type 'TK' to the list of frame
!!        types recognized by FRMGET.  In addition the routine
!!        now checks FAILED after "getting" the frame transformation.
!!
!!-    SPICELIB Version 1.0.0, 20-OCT-1994 (WLT)
!!
!! ## Index Entries
!!
!!     Find a frame transformation matrix from a specified frame
!!
!!     SPICELIB Functions
!!
!!     Local Variables
!!
!!     Saved variables
!!
!!     Initial values
!!
!!     Set output flag.
!!
      use pepper_kinds, only: ip, wp
      use pepper_arrays, only: cleard
      use pepper_inc_frmtyp
      implicit none
      integer(ip)           center
      integer(ip)           i
      integer(ip)           j
      integer(ip)           type
      integer(ip)           typeid
      real(wp)              tsipm  ( 6, 6 )
      real(wp)              rotate ( 3, 3 )
      if ( .not. allocated ( ctx%zzfrmgt1_state ) ) then
         allocate ( ctx%zzfrmgt1_state )
         call init_zzfrmgt1_state ( ctx%zzfrmgt1_state )
      end if
      associate ( idnt66 => ctx%zzfrmgt1_state%idnt66, &
                  pass1 => ctx%zzfrmgt1_state%pass1, &
                  versn => ctx%zzfrmgt1_state%versn )
      found = .false.

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     On the first pass, initialize the identity matrix.
!
      if ( pass1 ) then

         call cleard( 36, idnt66 )

         do i = 1, 6
            idnt66(i,i) = 1.0_wp
         end do

         pass1 = .false.

      end if

!
!     Get all the needed information about this frame.
!
      call frinfo ( ctx, infrm, center, type, typeid, found )

      if ( .not. found ) then

         call cleard ( 36, xform )
         outfrm = 0

         return

      end if

!
!     FOUND was set to true by the FRINFO call. Compute transformation
!     based on the frame class.
!
      if ( type == inertl ) then

         call irfrot ( ctx, infrm, 1, rotate )

         if ( .not. ctx%failed() ) then

            do i = 1,3
               do j = 1,3
                  xform(i,  j  ) = rotate(i,j)
                  xform(i+3,j+3) = rotate(i,j)
                  xform(i+3,j  ) = 0.0_wp
                  xform(i,  j+3) = 0.0_wp
               end do
            end do

            outfrm = 1

         end if

      else if ( type == pck ) then

         call tisbod ( ctx, 'J2000', typeid, et, tsipm  )

         if ( .not. ctx%failed() ) then

            call invstm ( ctx, tsipm, xform )

            outfrm = 1

         end if

      else if ( type == ck ) then

         call ckfxfm ( ctx, typeid, et, xform, outfrm, found )

      else if ( type == tk ) then

         call tkfram  ( ctx, typeid, rotate, outfrm, found )

         if ( .not. ctx%failed() ) then

            do i = 1,3
               do j = 1,3
                  xform(i,  j  ) = rotate(i,j)
                  xform(i+3,j+3) = rotate(i,j)
                  xform(i+3,j  ) = 0.0_wp
                  xform(i,  j+3) = 0.0_wp
               end do
            end do

         end if

      else if ( type == dyn ) then

         call ctx%setmsg ( 'The reference frame # is a dynamic frame. ' &
         //            'Dynamic frames may not be used at ' &
         //            'recursion level 1.'                      )
         call ctx%errint ( '#', infrm                                )
         call ctx%sigerr ( 'SPICE(RECURSIONTOODEEP)'                 )
         return

      else if ( type == swtch ) then

         call zzswfxfm ( ctx, infrm, et, 6, xform, outfrm, found )

      else

         call cleard ( 36, xform )
         outfrm = 0
         found  = .false.

         call ctx%setmsg ( 'The reference frame # has class #. ' &
         //            'This form of reference frame is not ' &
         //            'supported in version # of ZZFRMGT1. You need ' &
         //            'to update your version of SPICELIB to the ' &
         //            'latest version in order to support this ' &
         //            'frame. ' )

         call ctx%errint ( '#', infrm )
         call ctx%errint ( '#', type  )
         call ctx%errch  ( '#', versn )
         call ctx%sigerr ( 'SPICE(UNKNOWNFRAMETYPE)' )
         return

      end if

!
!     Make sure to clear outputs in case of a failure as defined in
!     in the header.
!
      if ( ctx%failed() .or. .not. found ) then

         call cleard ( 36, xform )
         outfrm = 0
         found  = .false.

      end if

      return
      end associate
      end procedure zzfrmgt1

      module procedure zzrotgt1
!! ## Abstract
!!
!!     Find the rotation from a user specified frame to another frame at
!!     a user specified epoch.
!!
!! ## Required Reading
!!
!!     FRAMES
!!
!! ## Keywords
!!
!!     FRAMES
!!
!! ## Declarations
!!
!! ## Detailed Input
!!
!!     INFRM       is the SPICE ID-code for some reference frame.
!!
!!     ET          is an epoch in ephemeris seconds past J2000 at which
!!                 the user wishes to retrieve a rotation matrix.
!!
!! ## Detailed Output
!!
!!     ROTATE      is a 3x3 matrix that rotates positions relative to
!!                 INFRM to positions relative to OUTFRM. (Assuming such
!!                 a rotation can be found.)
!!
!!     OUTFRM      is the SPICE ID-code of a reference frame. The 3x3
!!                 matrix ROTATE rotates positions relative to INFRM to
!!                 positions relative to OUTFRM. The positions
!!                 transformation is achieved by multiplying ROTATE on
!!                 the right by a position relative to INFRM. This is
!!                 easily accomplished via the subroutine call shown
!!                 below.
!!
!!                    CALL MXV  ( ROTATE, INPOS,  OUTPOS )
!!
!!     FOUND       is a logical flag indicating whether or not a
!!                 rotation matrix could be found from INFRM to some
!!                 other frame. If a rotation matrix cannot be found
!!                 OUTFRM will be set to zero, FOUND will be set to
!!                 FALSE and ROTATE will be returned as the zero matrix.
!!
!! ## Parameters
!!
!!     None.
!!
!! ## Exceptions
!!
!!     1) If a rotation matrix cannot be located, then FOUND will be set
!!        to FALSE, OUTFRM will be set to zero and ROTATE will be set to
!!        the zero 3x3 matrix.
!!
!!     2) If the class of the requested frame is not recognized the
!!        exception 'SPICE(UNKNOWNFRAMETYPE)' will be signaled.
!!
!! ## Files
!!
!!     LSK, SCLK, PCK, FK, SPK, and/or CK kernels may need to be loaded
!!     to provide the needed frame definition and transformation data.
!!
!! ## Particulars
!!
!!     This is a low level routine used for determining a chain of
!!     position transformation matrices from one frame to another.
!!
!! ## Examples
!!
!!     See REFCHG.
!!
!! ## Restrictions
!!
!!     None.
!!
!! ## Literature References
!!
!!     None.
!!
!! ## Author and Institution
!!
!!     N.J. Bachman    (JPL)
!!     B.V. Semenov    (JPL)
!!     W.L. Taber      (JPL)
!!
!! ## Version
!!
!!-    SPICELIB Version 5.0.0, 18-SEP-2020 (NJB)
!!
!!        Support for switch frames was added. VERSN is now
!!        initialized via a DATA statement. Corrected long error
!!        message to use the term "class" rather than "class id-code."
!!
!!-    SPICELIB Version 4.0.0, 21-MAR-2014 (BVS)
!!
!!        To prevent operations with un-initialized DP numbers, wrapped
!!        IF ( .NOT. FAILED() ) ... END IF around output matrix
!!        transposition operation in the PCK frame branch where the
!!        routine returning the matrix might fail.
!!
!!        Incremented major version token by 2 to sync up versions with
!!        FRMGET.
!!
!!-    SPICELIB Version 2.1.0, 02-MAR-2010 (NJB)
!!
!!        Bug fix: frame ID rather than frame class ID
!!        is now passed to dynamic frame evaluation
!!        routine ZZDYNROT. Order of header sections was
!!        corrected.
!!
!!-    SPICELIB Version 2.0.0, 18-DEC-2004 (NJB)
!!
!!        Added the new frame type 'DYN' to the list of frame
!!        types recognized by ROTGET.
!!
!!-    SPICELIB Version 1.0.0, 03-MAR-1999 (WLT)
!!
!! ## Index Entries
!!
!!     Find a rotation matrix from a specified frame
!!
!!     SPICELIB Functions
!!
!!     Local Variables
!!
!!     Saved variables
!!
!!     Initial values
!!
!!     Set output flag.
!!
      use pepper_kinds, only: ip, wp
      use pepper_arrays, only: cleard
      use pepper_math, only: xpose
      use pepper_inc_frmtyp
      implicit none
      integer(ip)           center
      integer(ip)           type
      integer(ip)           typeid
      real(wp)              tipm  ( 3, 3 )
      if ( .not. allocated ( ctx%zzrotgt1_state ) ) then
         allocate ( ctx%zzrotgt1_state )
         call init_zzrotgt1_state ( ctx%zzrotgt1_state )
      end if
      associate ( versn => ctx%zzrotgt1_state%versn )
      found = .false.

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Get all the needed information about this frame.
!
      call frinfo ( ctx, infrm, center, type, typeid, found )

      if ( .not. found ) then

         call cleard ( 9, rotate )
         outfrm = 0

         return

      end if

!
!     FOUND was set to true by the FRINFO call. Compute rotation based
!     on the frame class.
!
      if ( type == inertl ) then

         call irfrot ( ctx, infrm, 1, rotate )

         if ( .not. ctx%failed() ) then

            outfrm = 1

         end if

      else if ( type == pck ) then

         call tipbod ( ctx, 'J2000', typeid, et, tipm   )

         if ( .not. ctx%failed() ) then

            call xpose ( tipm, rotate )

            outfrm = 1

         end if

      else if ( type == ck ) then

         call ckfrot ( ctx, typeid, et, rotate, outfrm, found )

      else if ( type == tk ) then

         call tkfram  ( ctx, typeid, rotate, outfrm, found )

      else if ( type == dyn ) then

         call ctx%setmsg ( 'The reference frame # is a dynamic frame. ' &
         //            'Dynamic frames may not be used at ' &
         //            'recursion level 1.'                      )
         call ctx%errint ( '#', infrm                                )
         call ctx%sigerr ( 'SPICE(RECURSIONTOODEEP)'                 )
         return

      else if ( type == swtch ) then

         call zzswfxfm ( ctx, infrm, et, 3, rotate, outfrm, found )

      else

         call cleard ( 9, rotate )
         outfrm = 0
         found  = .false.

         call ctx%setmsg ( 'The reference frame # has class #. ' &
         //            'This form of reference frame is not ' &
         //            'supported in version # of ZZROTGT1. You need ' &
         //            'to update your version of SPICELIB to the ' &
         //            'latest version in order to support this ' &
         //            'frame. ' )

         call ctx%errint ( '#', infrm )
         call ctx%errint ( '#', type  )
         call ctx%errch  ( '#', versn )
         call ctx%sigerr ( 'SPICE(UNKNOWNFRAMETYPE)' )
         return

      end if

!
!     Make sure to clear outputs in case of a failure as defined in
!     in the header.
!
      if ( ctx%failed() .or. .not. found ) then

         call cleard ( 9, rotate )
         outfrm = 0
         found  = .false.

      end if

      return
      end associate
      end procedure zzrotgt1

!$Procedure      ZZFRMCH1 (Frame Change)

      module procedure zzfrmch1
!! ## Abstract
!!
!!     Return the state transformation matrix from one
!!     frame to another.
!!
!! ## Required Reading
!!
!!     None.
!!
!! ## Keywords
!!
!!     FRAMES
!!
!! ## Declarations
!!
      use pepper_kinds, only: ip, wp
      use pepper_arrays, only: isrchi
      use pepper_math, only: zzmsxf
      use pepper_inc_errhnd
      use pepper_inc_frmtyp
      implicit none



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
!     Example 2.  Suppose that you have the angular velocity, W, of some
!     rotation relative to FRAME1 at epoch ET and that you wish to
!     express this angular velocity with respect to FRAME2.  The
!     following subroutines will suffice to perform this computation.
!
!        CALL FRMCHG ( FRAME1, FRAME2, ET, STXFRM )
!
!     Recall that a state transformation matrix has the following form.
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
!     The velocity of an arbitrary point P undergoing rotation with the
!     angular velocity W is W x P
!
!     Thus the velocity of P in FRAME2 is:
!
!        dR
!        --  P    +    R (W x P )
!        dt
!
!           dR  t
!     =  (  -- R  R P   +  W  x P  )            ( 1 )
!           dt
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
!     From this final expression it follows that in FRAME2 the angular
!     velocity vector is given by [A+R*W].
!
!     The code below implements these ideas.
!
!        CALL FRMCHG ( FRAME1, FRAME2, ET, STXFRM )
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


!
!     Local Parameters
!
      integer(ip), parameter :: maxnod = 10

      integer(ip), parameter :: maxxfm = maxnod + 4

!
!     The root of all reference frames is J2000 (Frame ID = 1).
!
      integer(ip), parameter :: rootf = 1

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
      if ( ctx%failed() ) return

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

         return

      end if
!
!     Now perform the obvious check to make sure that both
!     frames are recognized.
!
      call frinfo ( ctx, frame1, cent, class, clssid, found )

      if ( ctx%failed() ) return

      if ( .not. found ) then

         call ctx%setmsg ( 'The number # is not a recognized id-code ' &
         //            'for a reference frame. ' )
         call ctx%errint ( '#', frame1 )
         call ctx%sigerr ( 'SPICE(UNKNOWNFRAME)'  )
         return

      end if

      call frinfo ( ctx, frame2, cent, class, clssid, found )

      if ( ctx%failed() ) return

      if ( .not. found ) then

         call ctx%setmsg ( 'The number # is not a recognized id-code ' &
         //            'for a reference frame. ' )
         call ctx%errint ( '#', frame2 )
         call ctx%sigerr ( 'SPICE(UNKNOWNFRAME)'  )
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
         call zzfrmgt1 ( ctx, frame(node),     et, &
                         trans(1,1,node), frame(node+1), found )

         if ( ctx%failed() ) return

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
         call zzfrmgt1( ctx, frame(node), et, trans(1,1,node), relto, found )

         if ( ctx%failed() ) return

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
!     TRANS(1...6, 1...6, 1    )    transforms FRAME1   to FRAME(2)
!     TRANS(1...6, 1...6, 2    )    transforms FRAME(2) to FRAME(3)
!     TRANS(1...6, 1...6, 3    )    transforms FRAME(3) to FRAME(4)
!        .
!        .
!        .
!     TRANS(1...6, 1...6, NODE-1 )  transforms FRAME(NODE-1)
!                                   to         FRAME(NODE)
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
            call zzfrmgt1 ( ctx, this, et, trans2(1,1,put), relto, found )

            if ( ctx%failed() ) return

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
            call zzfrmgt1 ( ctx, this, et, tempxf, relto, found )

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

         call zznofcon ( ctx, et, frame1, frame(node), frame2, this, errmsg )

         if ( ctx%failed() ) then
!
!           We were unable to create the error message. This
!           unfortunate situation could arise if a frame kernel
!           is corrupted.
!
            return

         end if
!
!        The normal case: signal an error with a descriptive long
!        error message.
!
         call ctx%setmsg ( errmsg                  )
         call ctx%sigerr ( 'SPICE(NOFRAMECONNECT)' )
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
      call invstm ( ctx, trans2(1,1,get), trans(1,1,cmnode) )
      call zzmsxf ( trans,   cmnode, xform             )

      return

      end procedure zzfrmch1

!$Procedure      ZZREFCH1 (Reference frame Change)

      module procedure zzrefch1
!! ## Abstract
!!
!!     Return the transformation matrix from one
!!     frame to another.
!!
!! ## Required Reading
!!
!!     None.
!!
!! ## Keywords
!!
!!     FRAMES
!!
!! ## Declarations
!!
      use pepper_kinds, only: ip, wp
      use pepper_arrays, only: isrchi
      use pepper_math, only: ident, xpose, zzrxr
      use pepper_inc_errhnd
      use pepper_inc_frmtyp
      implicit none



! $ Detailed_Input
!
!     FRAME1      is the frame id-code in which some positions
!                 are known.
!
!     FRAME2      is the frame id-code for some frame in which you
!                 would like to represent positions.
!
!     ET          is the epoch at which to compute the transformation
!                 matrix.  This epoch should be in TDB seconds past
!                 the ephemeris epoch of J2000.
!
! $ Detailed_Output
!
!     ROTATE      is a 3 x 3 rotation matrix that can be used to
!                 transform positions relative to the frame
!                 corresponding to frame FRAME2 to positions relative
!                 to the frame FRAME2.  More explicitly, if POS is
!                 the position of some object relative to the
!                 reference frame of FRAME1 then POS2 is the position
!                 of the same object relative to FRAME2 where POS2 is
!                 computed via the subroutine call below
!
!                    CALL MXV ( ROTATE, POS, POS2 )
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
!     This routine allows you to compute the rotation matrix
!     between two reference frames.
!
! $ Examples
!
!     Suppose that you have a position POS1 at epoch ET
!     relative to  FRAME1 and wish to determine its representation
!     POS2 relative to FRAME2.  The following subroutine calls
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
!     W.L. Taber      (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.1.0, 08-OCT-2021 (NJB)
!
!        Bug fix: added calls to FAILED after each call to
!        FRINFO and to ZZROTGT1.
!
!        Corrected typos in comments.
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
!-    SPICELIB Version 1.0.0, 9-JUL-1998 (WLT)
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


!
!     Local Parameters
!
      integer(ip), parameter :: maxnod = 10

      integer(ip), parameter :: maxxfm = maxnod + 4

!
!     The root of all reference frames is J2000 (Frame ID = 1).
!
      integer(ip), parameter :: rootf = 1

!
!     Local Variables
!
      character(lmsgln)     errmsg

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
      if ( ctx%failed() ) return

!
!     Do the obvious thing first.  If FRAME1 and FRAME2 are the
!     same then we simply return the identity matrix.
!
      if ( frame1 == frame2 ) then

         call ident  ( rotate )
         return

      end if
!
!     Now perform the obvious check to make sure that both
!     frames are recognized.
!
      call frinfo ( ctx, frame1, cent, class, clssid, found )

      if ( ctx%failed() ) return

      if ( .not. found ) then

         call ctx%setmsg ( 'The number # is not a recognized id-code ' &
         //            'for a reference frame. ' )
         call ctx%errint ( '#', frame1 )
         call ctx%sigerr ( 'SPICE(UNKNOWNFRAME)'  )
         return

      end if

      call frinfo ( ctx, frame2, cent, class, clssid, found )

      if ( ctx%failed() ) return

      if ( .not. found ) then

         call ctx%setmsg ( 'The number # is not a recognized id-code ' &
         //            'for a reference frame. ' )
         call ctx%errint ( '#', frame2 )
         call ctx%sigerr ( 'SPICE(UNKNOWNFRAME)'  )
         return

      end if

      node        = 1
      frame(node) = frame1
      found       = .true.
!
!     Follow the chain of rotations until we run into
!     one that rotates to J2000 (frame id = 1) or we hit FRAME2.
!
      do while (       frame(node) /= rootf &
                 .and. node        < maxnod &
                 .and. frame(node) /= frame2 &
                 .and. found   )
!
!        Find out what rotation is available for this
!        frame.
!
         call zzrotgt1 ( ctx, frame(node),   et, &
                         rot(1,1,node), frame(node+1), found  )

         if ( ctx%failed() ) return

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

      done =      frame(node) == rootf &
             .or. frame(node) == frame2 &
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
         call zzrotgt1 ( ctx, frame(node), et, rot(1,1,node), relto, found )

         if ( ctx%failed() ) return

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
         done =        frame(node) == rootf &
                  .or. frame(node) == frame2 &
                  .or. .not. found

      end do

!
!     Right now we have the following situation.  We have in hand
!     a collection of rotations between frames. (Assuming
!     that is that NODE .GT. 1.  If NODE .EQ. 1 then we have
!     no rotations computed yet.
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

      if ( frame(node) == frame2 ) then
!
!        We can handle this one immediately with the private routine
!        ZZRXR which multiplies a series of matrices.
!
         call zzrxr (  rot, node-1, rotate )
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
      done   =  this == rootf

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
         if ( this  == frame2 ) then
!
!           This is the first pass, just put the rotation
!           directly into ROT2(,,PUT).
!
            call zzrotgt1 ( ctx, this,  et, rot2(1,1,put), relto, found  )

            if ( ctx%failed() ) return

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
!           Fetch the rotation into a temporary spot TMPROT
!
            call zzrotgt1 ( ctx, this,  et, tmprot, relto,  found  )

            if ( ctx%failed() ) return

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
!     rotations from FRAME2 ran into a node in the chain for
!     FRAME1 or it didn't.  (The common node might very well be
!     the root node.)  If we didn't run into a common one, then
!     the two chains don't intersect and there is no way to
!     get from FRAME1 to FRAME2.
!
      if ( .not. gotone ) then

         call zznofcon ( ctx, et, frame1, frame(node), frame2, this, errmsg )

         if ( ctx%failed() ) then
!
!           We were unable to create the error message. This
!           unfortunate situation could arise if a frame kernel
!           is corrupted.
!
            return

         end if
!
!        The normal case: signal an error with a descriptive long
!        error message.
!
         call ctx%setmsg ( errmsg                  )
         call ctx%sigerr ( 'SPICE(NOFRAMECONNECT)' )
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

      return

      end procedure zzrefch1

      module procedure zzspkgo1
!! ## Abstract
!!
!!     SPICE Private routine intended solely for the support of SPICE
!!     routines.  Users should not call this routine directly due
!!     to the volatile nature of this routine.
!!
!!     Compute the geometric state (position and velocity) of a target
!!     body relative to an observing body.
!!
!! ## Required Reading
!!
!!     SPK
!!
!! ## Keywords
!!
!!     EPHEMERIS
!!
!! ## Declarations
!!
!! ## Detailed Input
!!
!!     TARG     is the standard NAIF ID code for a target body.
!!
!!     ET       is the epoch (ephemeris time) at which the state
!!              of the target body is to be computed.
!!
!!     REF      is the name of the reference frame to
!!              which the vectors returned by the routine should
!!              be rotated. This may be any frame supported by
!!              the SPICELIB subroutine FRMCHG.
!!
!!     OBS      is the standard NAIF ID code for an observing body.
!!
!! ## Detailed Output
!!
!!     STATE    is a 6-dimensional vector that contains the geometric
!!              position and velocity of the target body, relative to the
!!              observing body, at epoch ET. STATE has six elements: the
!!              first three contain the target's position; the last three
!!              contain the target's velocity. These vectors are
!!              transformed into the specified reference frame. Units are
!!              always km and km/sec.
!!
!!     LT       is the one-way light time in seconds from the
!!              observing body to the geometric position of the
!!              target body at the specified epoch.
!!
!! ## Parameters
!!
!!     None.
!!
!! ## Exceptions
!!
!!     1)  If insufficient ephemeris data has been loaded to compute
!!         the necessary states, the error SPICE(SPKINSUFFDATA) is
!!         signaled.
!!
!! ## Files
!!
!!     See $Restrictions.
!!
!! ## Particulars
!!
!!     SPKGEO computes the geometric state, T(t), of the target
!!     body and the geometric state, O(t), of the observing body
!!     relative to the first common center of motion. Subtracting
!!     O(t) from T(t) gives the geometric state of the target
!!     body relative to the observer.
!!
!!        CENTER ----- O(t)
!!            |      /
!!            |     /
!!            |    /
!!            |   /  T(t) - O(t)
!!            |  /
!!           T(t)
!!
!!     The one-way light time, tau, is given by
!!
!!               | T(t) - O(t) |
!!        tau = -----------------
!!                      c
!!
!!     For example, if the observing body is -94, the Mars Observer
!!     spacecraft, and the target body is 401, Phobos, then the
!!     first common center is probably 4, the Mars Barycenter.
!!     O(t) is the state of -94 relative to 4 and T(t) is the
!!     state of 401 relative to 4.
!!
!!     The center could also be the Solar System Barycenter, body 0.
!!     For example, if the observer is 399, Earth, and the target
!!     is 299, Venus, then O(t) would be the state of 399 relative
!!     to 0 and T(t) would be the state of 299 relative to 0.
!!
!!     Ephemeris data from more than one segment may be required
!!     to determine the states of the target body and observer
!!     relative to a common center. SPKGEO reads as many segments
!!     as necessary, from as many files as necessary, using files
!!     that have been loaded by previous calls to FURNSH or SPKLEF
!!     (load ephemeris file).
!!
!!     SPKGEO is similar to SPKEZ but returns geometric states
!!     only, with no option to make planetary (light-time) nor
!!     stellar aberration corrections. The geometric states
!!     returned by SPKEZ and SPKGEO are the same.
!!
!! ## Examples
!!
!!     The numerical results shown for this example may differ across
!!     platforms. The results depend on the SPICE kernels used as
!!     input, the compiler and supporting libraries, and the machine
!!     specific arithmetic implementation.
!!
!!     1) Return the geometric state vector of Mars (499) as seen from
!!        Earth (399) in the J2000 frame and the one-way light time
!!        between them at the epoch July 4, 2003 11:00 AM PST.
!!
!!        Use the meta-kernel shown below to load the required SPICE
!!        kernels.
!!
!!           KPL/MK
!!
!!           File: spkgeo_ex1.tm
!!
!!           This meta-kernel is intended to support operation of SPICE
!!           example programs. The kernels shown here should not be
!!           assumed to contain adequate or correct versions of data
!!           required by SPICE-based user applications.
!!
!!           In order for an application to use this meta-kernel, the
!!           kernels referenced here must be present in the user's
!!           current working directory.
!!
!!           The names and contents of the kernels referenced
!!           by this meta-kernel are as follows:
!!
!!              File name                        Contents
!!              ---------                        --------
!!              de430.bsp                        Planetary ephemeris
!!              mar097.bsp                       Mars satellite ephemeris
!!              naif0011.tls                     Leapseconds
!!
!!           \begindata
!!
!!              KERNELS_TO_LOAD = ( 'de430.bsp',
!!                                  'mar097.bsp',
!!                                  'naif0011.tls' )
!!
!!           \begintext
!!
!!           End of meta-kernel
!!
!!        Example code begins here.
!!
!!              PROGRAM SPKGEO_EX1
!!              IMPLICIT NONE
!!
!!        C
!!        C     SPICELIB functions
!!        C
!!              DOUBLE PRECISION      VNORM
!!
!!        C
!!        C     Local parameters.
!!        C
!!              INTEGER               NAMLEN
!!              PARAMETER           ( NAMLEN = 32 )
!!
!!              INTEGER               TIMLEN
!!              PARAMETER           ( TIMLEN = 26 )
!!
!!        C
!!        C     Local variables.
!!        C
!!              CHARACTER*(TIMLEN)    EPOCH
!!              CHARACTER*(NAMLEN)    REFFRM
!!
!!              DOUBLE PRECISION      ET
!!              DOUBLE PRECISION      LT
!!              DOUBLE PRECISION      STATE ( 6 )
!!
!!              INTEGER               I
!!              INTEGER               OBSRVR
!!              INTEGER               TARGET
!!
!!        C
!!        C     Load a set of kernels: an SPK file, a PCK
!!        C     file and a leapseconds file. Use a meta
!!        C     kernel for convenience.
!!        C
!!              CALL FURNSH ( 'spkgeo_ex1.tm' )
!!
!!        C
!!        C     Define parameters for a state lookup.
!!        C
!!              TARGET = 499
!!              EPOCH  = 'July 4, 2003 11:00 AM PST'
!!              REFFRM = 'J2000'
!!              OBSRVR = 399
!!
!!        C
!!        C     Convert the epoch to ephemeris time.
!!        C
!!              CALL STR2ET ( EPOCH, ET )
!!
!!        C
!!        C     Look-up the state for the defined parameters.
!!        C
!!              CALL SPKGEO ( TARGET, ET, REFFRM, OBSRVR, STATE, LT )
!!
!!        C
!!        C     Output...
!!        C
!!              WRITE(*,'(A,I3)') 'The position of    : ', TARGET
!!              WRITE(*,'(A,I3)') 'As observed from   : ', OBSRVR
!!              WRITE(*,'(2A)')   'In reference frame : ', REFFRM
!!              WRITE(*,'(2A)')   'At epoch           : ', EPOCH
!!              WRITE(*,*) ' '
!!
!!        C
!!        C     The first three entries of state contain the
!!        C     X, Y, Z position components. The final three contain
!!        C     the Vx, Vy, Vz velocity components.
!!        C
!!              WRITE(*,'(A,3F18.6)') 'R   (km):', ( STATE(I), I=1,3 )
!!              WRITE(*,'(A,3F18.6)') 'V (km/s):', ( STATE(I), I=4,6 )
!!              WRITE(*,*) ' '
!!              WRITE(*,'(A,F19.14)') 'Light time (s) between observer '
!!             .                   // 'and target: ', LT
!!
!!              END
!!
!!        When this program was executed on a Mac/Intel/gfortran/64-bit
!!        platform, the output was:
!!
!!        The position of    : 499
!!        As observed from   : 399
!!        In reference frame : J2000
!!        At epoch           : July 4, 2003 11:00 AM PST
!!
!!        R   (km):   73826216.435288  -27128030.732406  -18741973.868287
!!        V (km/s):         -6.809504          7.513814          3.001290
!!
!!        Light time (s) between observer and target:  269.70264776317532
!!
!! ## Restrictions
!!
!!     1)  The ephemeris files to be used by SPKGEO must be loaded
!!         by FURNSH or SPKLEF before SPKGEO is called.
!!
!! ## Literature References
!!
!!     None.
!!
!! ## Author and Institution
!!
!!     N.J. Bachman       (JPL)
!!     J. Diaz del Rio    (ODC Space)
!!     J.E. McLean        (JPL)
!!     B.V. Semenov       (JPL)
!!     W.L. Taber         (JPL)
!!     E.D. Wright        (JPL)
!!
!! ## Version
!!
!!-    SPICELIB Version 3.1.0, 09-OCT-2021 (JDR) (NJB)
!!
!!        Bug fix: added calls to FAILED after calls to SPKPVN.
!!        Previously only one call to SPKPVN was followed by a FAILED
!!        call. Moved some FAILED checks so they will be hit whether
!!        or not SPKSFS finds a segment.
!!
!!        Edited the header to comply with NAIF standard. Added complete
!!        code example to $Examples section. Removed unnecessary
!!        $Revisions section.
!!
!!        Added reference to FURNSH in $Particulars and $Restrictions
!!        sections.
!!
!!-    SPICELIB Version 3.0.0, 08-JAN-2014 (BVS)
!!
!!        Updated to save the input frame name and POOL state counter
!!        and to do frame name-ID conversion only if the counter has
!!        changed.
!!
!!        Updated to map the input frame name to its ID by first calling
!!        ZZNAMFRM, and then calling IRFNUM. The side effect of this
!!        change is that now the frame with the fixed name 'DEFAULT'
!!        that can be associated with any code via CHGIRF's entry point
!!        IRFDEF will be fully masked by a frame with identical name
!!        defined via a text kernel. Previously the CHGIRF's 'DEFAULT'
!!        frame masked the text kernel frame with the same name.
!!
!!        Fixed description of STATE in Detailed Output. Replaced
!!        SPKLEF with FURNSH and fixed errors in $Examples.
!!
!!-    SPICELIB Version 2.4.0, 01-SEP-2005 (NJB)
!!
!!        Updated to remove non-standard use of duplicate arguments
!!        in VADDG calls.
!!
!!-    SPICELIB Version 2.3.0, 05-JAN-2005 (NJB)
!!
!!        Tests of routine FAILED() were added.
!!
!!-    SPICELIB Version 2.2.1, 20-OCT-2003 (EDW)
!!
!!        Added mention that LT returns in seconds.
!!
!!-    SPICELIB Version 2.2.0, 11-APR-1997 (WLT)
!!
!!        The routine was modified to take advantage of the fact
!!        that most state transformation are between inertial frames.
!!        Looking up a transformation between inertial frames is
!!        substantially faster than looking up non-inertial
!!        transformations. Consequently slightly more
!!        complex code produces about a 50% increase in speed for
!!        many users.
!!
!!-    SPICELIB Version 2.1.0, 26-JUL-1996 (WLT)
!!
!!        The routine was upgraded so that potentially redundant
!!        computations are not performed.
!!
!!-    SPICELIB Version 2.0.0, 19-SEP-1995 (WLT)
!!
!!        The routine was upgraded so that it can return states
!!        relative to rotating frames.
!!
!!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!!
!!        Comment section for permuted index source lines was added
!!        following the header.
!!
!!-    SPICELIB Version 1.0.0, 18-JUL-1991 (JEM)
!!
!! ## Index Entries
!!
!!     geometric state of one body relative to another
!!
!!     This is the idea:
!!
!!     Every body moves with respect to some center. The center
!!     is itself a body, which in turn moves about some other
!!     center.  If we begin at the target body (T), follow
!!     the chain,
!!
!!                                   T
!!                                     \
!!           SSB                        \
!!               \                     C[1]
!!                \                     /
!!                 \                   /
!!                  \                 /
!!                   \               /
!!                  C[3]-----------C[2]
!!
!!     and avoid circular definitions (A moves about B, and B moves
!!     about A), eventually we get the state relative to the solar
!!     system barycenter (which, for our purposes, doesn't move).
!!     Thus,
!!
!!        T    = T     + C[1]     + C[2]     + ... + C[n]
!!         SSB    C[1]       C[2]       [C3]             SSB
!!
!!     where
!!
!!        X
!!         Y
!!
!!     is the state of body X relative to body Y.
!!
!!     However, we don't want to follow each chain back to the SSB
!!     if it isn't necessary.  Instead we will just follow the chain
!!     of the target body and follow the chain of the observing body
!!     until we find a common node in the tree.
!!
!!     In the example below, C is the first common node.  We compute
!!     the state of TARG relative to C and the state of OBS relative
!!     to C, then subtract the two states.
!!
!!                                   TARG
!!                                     \
!!           SSB                        \
!!               \                       A
!!                \                     /            OBS
!!                 \                   /              |
!!                  \                 /               |
!!                   \               /                |
!!                    B-------------C-----------------D
!!
!!     SPICELIB functions
!!
!!     Local parameters
!!
!!     CHLEN is the maximum length of a chain.  That is,
!!     it is the maximum number of bodies in the chain from
!!     the target or observer to the SSB.
!!
!!     Saved frame name length.
!!
!!     Local variables
!!
!!     Saved frame name/ID item declarations.
!!
!!     Saved frame name/ID items.
!!
!!     Initial values.
!!
!!     In-line Function Definitions
!!
!!     Standard SPICE error handling.
!!
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: clight
      use pepper_arrays, only: cleard, isrchi, moved
      use pepper_math, only: mxv, mxvg, vaddg, vnorm, vsubg
      use pepper_strings, only: frstnp, intstr, prefix, suffix
      use pepper_inc_ninert
      use pepper_inc_zzctr
      implicit none
      character(*), parameter :: rname = 'ZZSPKGO1'
      integer(ip), parameter :: chlen = 20
      integer(ip), parameter :: ssb = 0
      integer(ip), parameter :: frnmln = 32
      character(40)         ident
      character(40)         tname
      character(40)         oname
      character(80)         tstring
      real(wp)              descr  ( 5        )
      real(wp)              sobs   ( 6        )
      real(wp)              starg  ( 6, chlen )
      real(wp)              stemp  ( 6        )
      real(wp)              stxfrm ( 6, 6     )
      real(wp)              rot    ( 3, 3     )
      real(wp)              vtemp  ( 6        )
      integer(ip)           cframe
      integer(ip)           cobs
      integer(ip)           ctarg  (    chlen )
      integer(ip)           tframe (    chlen )
      integer(ip)           ctpos
      integer(ip)           handle
      integer(ip)           i
      integer(ip)           legs
      integer(ip)           nct
      integer(ip)           refid
      integer(ip)           tmpfrm
      logical               found
      logical               nofrm
      logical               isinrt
      isinrt ( tmpfrm, cframe ) =       cframe > 0 &
                                  .and. cframe <= ninert &
                                  .and. tmpfrm > 0 &
                                  .and. tmpfrm <= ninert
      if ( .not. allocated ( ctx%zzspkgo1_state ) ) then
         allocate ( ctx%zzspkgo1_state )
         call init_zzspkgo1_state ( ctx%zzspkgo1_state )
      end if
      associate ( svctr1 => ctx%zzspkgo1_state%svctr1, &
                  svref => ctx%zzspkgo1_state%svref, &
                  svrefi => ctx%zzspkgo1_state%svrefi, &
                  first => ctx%zzspkgo1_state%first )
      if ( ctx%failed() ) return

!
!     Initialization.
!
      if ( first ) then

!
!        Initialize counter.
!
         call zzctruin( ctx, svctr1 )

         first = .false.

      end if

!
!     We take care of the obvious case first.  It TARG and OBS are the
!     same we can just fill in zero.
!
      if ( targ == obs ) then

         lt = 0.0_wp

         call cleard ( 6, state )
         return

      end if

!
!     CTARG contains the integer codes of the bodies in the
!     target body chain, beginning with TARG itself and then
!     the successive centers of motion.
!
!     STARG(1,I) is the state of the target body relative
!     to CTARG(I).  The id-code of the frame of this state is
!     stored in TFRAME(I).
!
!     COBS and SOBS will contain the centers and states of the
!     observing body.  (They are single elements instead of arrays
!     because we only need the current center and state of the
!     observer relative to it.)
!
!     First, we construct CTARG and STARG.  CTARG(1) is
!     just the target itself, and STARG(1,1) is just a zero
!     vector, that is, the state of the target relative
!     to itself.
!
!     Then we follow the chain, filling up CTARG and STARG
!     as we go.  We use SPKSFS to search through loaded
!     files to find the first segment applicable to CTARG(1)
!     and time ET.  Then we use SPKPVN to compute the state
!     of the body CTARG(1) at ET in the segment that was found
!     and get its center and frame of motion (CTARG(2) and TFRAME(2).
!
!     We repeat the process for CTARG(2) and so on, until
!     there is no data found for some CTARG(I) or until we
!     reach the SSB.
!
!     Next, we find centers and states in a similar manner
!     for the observer.  It's a similar construction as
!     described above, but I is always 1.  COBS and SOBS
!     are overwritten with each new center and state,
!     beginning at OBS.  However, we stop when we encounter
!     a common center of motion, that is when COBS is equal
!     to CTARG(I) for some I.
!
!     Finally, we compute the desired state of the target
!     relative to the observer by subtracting the state of
!     the observing body relative to the common node from
!     the state of the target body relative to the common
!     node.
!
!     CTPOS is the position in CTARG of the common node.
!
!     Since the upgrade to use hashes and counter bypass ZZNAMFRM
!     became more efficient in looking up frame IDs than IRFNUM. So the
!     original order of calls "IRFNUM first, NAMFRM second" was
!     switched to "ZZNAMFRM first, IRFNUM second".
!
!     The call to IRFNUM, now redundant for built-in inertial frames,
!     was preserved to for a sole reason -- to still support the
!     ancient and barely documented ability for the users to associate
!     a frame with the fixed name 'DEFAULT' with any CHGIRF inertial
!     frame code via CHGIRF's entry point IRFDEF.
!
!     Note that in the case of ZZNAMFRM's failure to resolve name and
!     IRFNUM's success to do so, the code returned by IRFNUM for
!     'DEFAULT' frame is *not* copied to the saved code SVREFI (which
!     would be set to 0 by ZZNAMFRM) to make sure that on subsequent
!     calls ZZNAMFRM does not do a bypass (as SVREFI always forced look
!     up) and calls IRFNUM again to reset the 'DEFAULT's frame ID
!     should it change between the calls.
!
      call zznamfrm ( ctx, svctr1, svref, svrefi, ref, refid )

      if ( refid == 0 ) then
         call irfnum ( ctx, ref, refid )
      end if

      if ( refid == 0 ) then

         if (  frstnp(ref) > 0 ) then

            call ctx%setmsg ( 'The string supplied to specify the ' &
            //            'reference frame, (''#'') contains ' &
            //            'non-printing characters.  The two ' &
            //            'most common causes for this kind ' &
            //            'of error are: 1. an error in the ' &
            //            'call to ZZSPKGO1; 2. an ' &
            //            'uninitialized variable. ' )
            call ctx%errch  ( '#', ref )

         else if ( ref == ' ' ) then

            call ctx%setmsg ( 'The string supplied to specify the ' &
            //            'reference frame is blank.  The most ' &
            //            'common cause for this kind of error ' &
            //            'is an uninitialized variable. ' )

         else

            call ctx%setmsg ( 'The string supplied to specify the ' &
            //            'reference frame was ''#''.  This ' &
            //            'frame is not recognized. Possible ' &
            //            'causes for this error are: 1. failure ' &
            //            'to load the frame definition into the ' &
            //            'kernel pool; 2. An out-of-date ' &
            //            'edition of the toolkit. ' )
            call ctx%errch  ( '#', ref )

         end if

         call ctx%sigerr ( 'SPICE(UNKNOWNFRAME)'  )

         if ( ctx%failed() ) return

      end if

!
!     Fill in CTARG and STARG until no more data is found
!     or until we reach the SSB.  If the chain gets too
!     long to fit in CTARG, that is if I equals CHLEN,
!     then overwrite the last elements of CTARG and STARG.
!
!     Note the check for FAILED in the loop.  If SPKSFS
!     or SPKPVN happens to fail during execution, and the
!     current error handling action is to NOT abort, then
!     FOUND may be stuck at TRUE, CTARG(I) will never
!     become zero, and the loop will execute indefinitely.
!
!     Construct CTARG and STARG.  Begin by assigning the
!     first elements:  TARG and the state of TARG relative
!     to itself.
!
      i        =  1
      ctarg(i) =  targ
      found    = .true.

      call cleard ( 6, starg(1,i) )

      do while (           found &
                  .and.  ( i        < chlen ) &
                  .and.  ( ctarg(i) /= obs   ) &
                  .and.  ( ctarg(i) /= ssb   )  )
!
!        Find a file and segment that has state
!        data for CTARG(I).
!
         call spksfs ( ctx, ctarg(i), et, handle, descr, ident, found )

         if ( found ) then
!
!           Get the state of CTARG(I) relative to some
!           center of motion.  This new center goes in
!           CTARG(I+1) and the state is called STEMP.
!
            i = i + 1

            call spkpvn ( ctx, handle,    descr,      et, &
                          tframe(i), starg(1,i), ctarg(i) )
!
!           Here's what we have.  STARG is the state of CTARG(I-1)
!           relative to CTARG(I) in reference frame TFRAME(I)
!
         end if
!
!        If one of the routines above failed during
!        execution, we just give up and return.
!
         if ( ctx%failed() ) return

      end do

      tframe(1) = tframe(2)
!
!     If the loop above ended because we ran out of
!     room in the arrays CTARG and STARG, then we
!     continue finding states but we overwrite the
!     last elements of CTARG and STARG.
!
!     If, as a result, the first common node is
!     overwritten, we'll just have to settle for
!     the last common node.  This will cause a small
!     loss of precision, but it's better than other
!     alternatives.
!
      if ( i == chlen ) then

         do while (      found &
                   .and. ctarg(chlen) /= ssb &
                   .and. ctarg(chlen) /= obs )
!
!           Find a file and segment that has state
!           data for CTARG(CHLEN).
!
            call spksfs ( ctx, ctarg(chlen), et, handle, descr, ident, found)

            if ( found ) then
!
!              Get the state of CTARG(CHLEN) relative to
!              some center of motion.  The new center
!              overwrites the old.  The state is called
!              STEMP.
!
               call spkpvn ( ctx, handle, descr, et, tmpfrm, stemp, &
                             ctarg(chlen))

               if ( ctx%failed() ) return

!
!              Add STEMP to the state of TARG relative to
!              the old center to get the state of TARG
!              relative to the new center.  Overwrite
!              the last element of STARG.
!
               if ( tframe(chlen) == tmpfrm ) then

                  call moved ( starg(1,chlen), 6, vtemp )

               else if ( isinrt ( tframe(chlen), tmpfrm ) ) then

                  call irfrot( ctx, tframe(chlen), tmpfrm,         rot      )
                  call mxv   ( rot,           starg(1,chlen), vtemp(1) )
                  call mxv   ( rot,           starg(4,chlen), vtemp(4) )

               else

                  call zzfrmch1( ctx, tframe(chlen), tmpfrm,   et,   stxfrm )

                  if ( ctx%failed() ) return

                  call mxvg  ( stxfrm, starg (1,chlen), 6, 6, vtemp  )

               end if

               call vaddg ( vtemp,  stemp,        6, starg(1,chlen) )

               tframe(chlen) = tmpfrm

            end if

!
!           If one of the routines above failed during
!           execution, we just give up and return.
!
            if ( ctx%failed() ) return

         end do

      end if

      nct = i
!
!     NCT is the number of elements in CTARG,
!     the chain length.  We have in hand the following information
!
!        STARG(1...6,K)  state of body
!        CTARG(K-1)      relative to body CTARG(K) in the frame
!        TFRAME(K)
!
!     For K = 2,..., NCT.
!
!     CTARG(1) = TARG
!     STARG(1...6,1) = ( 0, 0, 0, 0, 0, 0 )
!     TFRAME(1)      = TFRAME(2)
!

!
!     Now follow the observer's chain.  Assign
!     the first values for COBS and SOBS.
!
      cobs = obs
      call cleard ( 6, sobs )

!
!     Perhaps we have a common node already.
!     If so it will be the last node on the
!     list CTARG.
!
!     We let CTPOS will be the position of the common
!     node in CTARG if one is found.  It will
!     be zero if COBS is not found in CTARG.
!
      if ( ctarg(nct) == cobs ) then
         ctpos  = nct
         cframe = tframe(ctpos)
      else
         ctpos  = 0
      end if

!
!     Repeat the same loop as above, but each time
!     we encounter a new center of motion, check to
!     see if it is a common node.  (When CTPOS is
!     not zero, CTARG(CTPOS) is the first common node.)
!
!     Note that we don't need a centers array nor a
!     states array, just a single center and state
!     is sufficient --- we just keep overwriting them.
!     When the common node is found, we have everything
!     we need in that one center (COBS) and state
!     (SOBS-state of the target relative to COBS).
!
      found  = .true.
      nofrm  = .true.
      legs   =  0

      do while (           found &
                  .and.  ( cobs  /= ssb ) &
                  .and.  ( ctpos == 0   )  )
!
!        Find a file and segment that has state
!        data for COBS.
!
         call spksfs ( ctx, cobs, et, handle, descr, ident, found )

         if ( found ) then
!
!           Get the state of COBS; call it STEMP.
!           The center of motion of COBS becomes the
!           new COBS.
!
            if ( legs == 0 ) then
               call spkpvn ( ctx, handle, descr, et, tmpfrm, sobs,  cobs )
            else
               call spkpvn ( ctx, handle, descr, et, tmpfrm, stemp, cobs )
            end if

            if ( ctx%failed() ) return

            if ( nofrm ) then
               nofrm  = .false.
               cframe = tmpfrm
            end if
!
!           Add STEMP to the state of OBS relative to
!           the old COBS to get the state of OBS
!           relative to the new COBS.
!
            if ( cframe == tmpfrm ) then
!
!              On the first leg of the state of the observer, we
!              don't have to add anything, the state of the observer
!              is already in SOBS.  We only have to add when the
!              number of legs in the observer state is one or greater.
!
               if ( legs > 0 ) then
                  call vaddg ( sobs,  stemp, 6, vtemp )
                  call moved ( vtemp,        6, sobs  )
               end if

            else if ( isinrt( cframe, tmpfrm ) ) then

               call irfrot ( ctx, cframe, tmpfrm, rot )
               call mxv    ( rot,    sobs(1),   vtemp(1)  )
               call mxv    ( rot,    sobs(4),   vtemp(4)  )
               call vaddg  ( vtemp,  stemp,     6, sobs   )
               cframe = tmpfrm

            else

               call zzfrmch1 ( ctx, cframe, tmpfrm, et,   stxfrm )

               if ( ctx%failed() ) return

               call mxvg   ( stxfrm, sobs,   6, 6, vtemp  )
               call vaddg  ( vtemp,  stemp,     6, sobs   )
               cframe = tmpfrm

            end if

!
!           We now have one more leg of the path for OBS.  Set
!           LEGS to reflect this.  Then see if the new center
!           is a common node. If not, repeat the loop.
!
            legs  = legs + 1
            ctpos = isrchi ( cobs, nct, ctarg )

         end if

!
!        Check failed.  We don't want to loop indefinitely.
!
         if ( ctx%failed() ) return

      end do

!
!     If CTPOS is zero at this point, it means we
!     have not found a common node though we have
!     searched through all the available data.
!
      if ( ctpos == 0 ) then

         call bodc2n ( ctx, targ, tname, found )

         if ( found ) then
            call prefix ( '# (',  0,        tname )
            call suffix ( ')',    0,        tname )
            call repmi  ( ctx, tname, '#', targ, tname )
         else
            call intstr ( targ, tname )
         end if

         call bodc2n ( ctx, obs, oname, found )

         if ( found ) then
            call prefix ( '# (',  0,       oname )
            call suffix ( ')',    0,       oname )
            call repmi  ( ctx, oname, '#', obs, oname )
         else
            call intstr ( obs, oname )
         end if

         call ctx%setmsg ( 'Insufficient ephemeris data has been ' &
         //            'loaded to compute the state of TARG ' &
         //            'relative to OBS at the ephemeris epoch ' &
         //            '#. ' )

         call etcal  ( ctx,  et,    tstring                                 )
         call ctx%errch  ( 'TARG', tname                                   )
         call ctx%errch  ( 'OBS',  oname                                   )
         call ctx%errch  ( '#',    tstring                                 )
         call ctx%sigerr ( 'SPICE(SPKINSUFFDATA)'                          )
         return

      end if

!
!     If CTPOS is not zero, then we have reached a
!     common node, specifically,
!
!        CTARG(CTPOS) = COBS = CENTER
!
!     (in diagram below).  The STATE of the target
!     (TARG) relative to the observer (OBS) is just
!
!        STARG(1,CTPOS) - SOBS.
!
!                     SOBS
!         CENTER ---------------->OBS
!            |                  .
!            |                .
!         S  |              .   E
!         T  |            .   T
!         A  |          .   A
!         R  |        .   T
!         G  |      .   S
!            |    .
!            |  .
!            V L
!           TARG
!
!     And the light-time between them is just
!
!               | STATE |
!          LT = ---------
!                   c
!
!     Compute the state of the target relative to CTARG(CTPOS)
!
      if ( ctpos == 1 ) then
         tframe(1) = cframe
      end if

      do i = 2, ctpos-1

         if ( tframe(i) == tframe(i+1) ) then

            call vaddg  ( starg(1,i), starg(1,i+1), 6, vtemp        )
            call moved  ( vtemp,                    6, starg(1,i+1) )

         else if ( isinrt ( tframe(i), tframe(i+1) ) ) then

            call irfrot ( ctx, tframe(i),  tframe(i+1),     rot          )
            call mxv    ( rot,        starg(1,i),      stemp(1)     )
            call mxv    ( rot,        starg(4,i),      stemp(4)     )
            call vaddg  ( stemp,      starg(1,i+1), 6, vtemp        )
            call moved  ( vtemp,                    6, starg(1,i+1) )

         else

            call zzfrmch1 ( ctx, tframe(i),  tframe(i+1), et,  stxfrm       )

            if ( ctx%failed() ) return

            call mxvg   ( stxfrm,     starg(1,i),  6,6, stemp        )
            call vaddg  ( stemp,      starg(1,i+1),  6, vtemp        )
            call moved  ( vtemp,                     6, starg(1,i+1) )

         end if

      end do

!
!     To avoid unnecessary frame transformations we'll do
!     a bit of extra decision making here.  It's a lot
!     faster to make logical checks than it is to compute
!     frame transformations.
!
      if ( tframe(ctpos) == cframe ) then

         call vsubg  ( starg(1,ctpos), sobs, 6,   state  )

      else if ( tframe(ctpos) == refid ) then

!
!        If the last frame associated with the target is already
!        in the requested output frame, we convert the state of
!        the observer to that frame and then subtract the state
!        of the observer from the state of the target.
!
         if ( isinrt( cframe, refid ) ) then

            call irfrot ( ctx, cframe, refid,   rot      )
            call mxv    ( rot,    sobs(1), stemp(1) )
            call mxv    ( rot,    sobs(4), stemp(4) )

         else

            call zzfrmch1 ( ctx, cframe, refid, et,  stxfrm )

            if ( ctx%failed() ) return

            call mxvg   ( stxfrm, sobs,  6,6, stemp  )

         end if

!
!        We've now transformed SOBS into the requested reference frame.
!        Set CFRAME to reflect this.
!
         cframe = refid
         call vsubg ( starg(1,ctpos),  stemp, 6, state )

      else if ( isinrt( tframe(ctpos), cframe ) ) then
!
!        If both frames are inertial we use IRFROT instead of
!        ZZFRMCH1 to get things into a common frame.
!
         call irfrot ( ctx, tframe(ctpos),  cframe,               rot      )
         call mxv    ( rot,            starg(1,ctpos),       stemp(1) )
         call mxv    ( rot,            starg(4,ctpos),       stemp(4) )
         call vsubg  ( stemp,          sobs,            6,   state    )

      else
!
!        Use the more general routine ZZFRMCH1 to make the
!        transformation.
!
         call zzfrmch1 ( ctx, tframe(ctpos),  cframe,          et,  stxfrm )

         if ( ctx%failed() ) return

         call mxvg   ( stxfrm,         starg(1,ctpos),  6,6, stemp  )
         call vsubg  ( stemp,          sobs,            6,   state  )

      end if

!
!     Finally, rotate as needed into the requested frame.
!
      if ( cframe == refid ) then

!
!        We don't have to do anything in this case.
!

      else if ( isinrt ( cframe, refid ) ) then
!
!        Since both frames are inertial, we use the more direct
!        routine IRFROT to get the transformation to REFID.
!
         call irfrot ( ctx, cframe,  refid,      rot      )
         call mxv    ( rot,     state(1),   stemp(1) )
         call mxv    ( rot,     state(4),   stemp(4) )
         call moved  ( stemp,   6,          state    )

      else

         call zzfrmch1 ( ctx, cframe,  refid, et,  stxfrm )

         if ( ctx%failed() ) return

         call mxvg   ( stxfrm,  state, 6, 6, stemp )
         call moved  ( stemp,          6,    state )

      end if

      lt = vnorm  ( state ) / clight

      return
      end associate
      end procedure zzspkgo1

      module procedure zzspkgp1
!! ## Abstract
!!
!!     SPICE Private routine intended solely for the support of SPICE
!!     routines.  Users should not call this routine directly due
!!     to the volatile nature of this routine.
!!
!!     Compute the geometric position of a target body relative to an
!!     observing body.
!!
!! ## Required Reading
!!
!!     SPK
!!
!! ## Keywords
!!
!!     EPHEMERIS
!!
!! ## Declarations
!!
!! ## Detailed Input
!!
!!     TARG     is the standard NAIF ID code for a target body.
!!
!!     ET       is the epoch (ephemeris time) at which the position
!!              of the target body is to be computed.
!!
!!     REF      is the name of the reference frame to
!!              which the vectors returned by the routine should
!!              be rotated. This may be any frame supported by
!!              the SPICELIB subroutine REFCHG.
!!
!!     OBS      is the standard NAIF ID code for an observing body.
!!
!! ## Detailed Output
!!
!!     POS      is a 3-dimensional vector that contains the position of
!!              the target body, relative to the observing body. This
!!              vector is rotated into the specified reference frame.
!!              Units are always km.
!!
!!     LT       is the one-way light time from the observing body
!!              to the geometric position of the target body at the
!!              specified epoch.
!!
!! ## Parameters
!!
!!     None.
!!
!! ## Exceptions
!!
!!     1)  If insufficient ephemeris data has been loaded to compute
!!         the necessary positions, the error SPICE(SPKINSUFFDATA) is
!!         signaled.
!!
!! ## Files
!!
!!     See $Restrictions.
!!
!! ## Particulars
!!
!!     SPKGPS computes the geometric position, T(t), of the target
!!     body and the geometric position, O(t), of the observing body
!!     relative to the first common center of motion. Subtracting
!!     O(t) from T(t) gives the geometric position of the target
!!     body relative to the observer.
!!
!!        CENTER ----- O(t)
!!            |      /
!!            |     /
!!            |    /
!!            |   /  T(t) - O(t)
!!            |  /
!!           T(t)
!!
!!     The one-way light time, tau, is given by
!!
!!               | T(t) - O(t) |
!!        tau = -----------------
!!                      c
!!
!!     For example, if the observing body is -94, the Mars Observer
!!     spacecraft, and the target body is 401, Phobos, then the
!!     first common center is probably 4, the Mars Barycenter.
!!     O(t) is the position of -94 relative to 4 and T(t) is the
!!     position of 401 relative to 4.
!!
!!     The center could also be the Solar System Barycenter, body 0.
!!     For example, if the observer is 399, Earth, and the target
!!     is 299, Venus, then O(t) would be the position of 399 relative
!!     to 0 and T(t) would be the position of 299 relative to 0.
!!
!!     Ephemeris data from more than one segment may be required
!!     to determine the positions of the target body and observer
!!     relative to a common center. SPKGPS reads as many segments
!!     as necessary, from as many files as necessary, using files
!!     that have been loaded by previous calls to SPKLEF (load
!!     ephemeris file).
!!
!!     SPKGPS is similar to SPKGEO but returns geometric positions
!!     only.
!!
!! ## Examples
!!
!!     The following code example computes the geometric
!!     position of the moon with respect to the earth and
!!     then prints the distance of the moon from the
!!     the earth at a number of epochs.
!!
!!     Assume the SPK file SAMPLE.BSP contains ephemeris data
!!     for the moon relative to earth over the time interval
!!     from BEGIN to END.
!!
!!            INTEGER               EARTH
!!            PARAMETER           ( EARTH = 399 )
!!
!!            INTEGER               MOON
!!            PARAMETER           ( MOON  = 301 )
!!
!!            INTEGER               N
!!            PARAMETER           ( N     = 100 )
!!
!!            INTEGER               I
!!            CHARACTER*(20)        UTC
!!            DOUBLE PRECISION      BEGIN
!!            DOUBLE PRECISION      DELTA
!!            DOUBLE PRECISION      END
!!            DOUBLE PRECISION      ET
!!            DOUBLE PRECISION      POS ( 3 )
!!            DOUBLE PRECISION      LT
!!
!!            DOUBLE PRECISION      VNORM
!!
!!     C
!!     C      Load the binary SPK ephemeris file.
!!     C
!!            CALL FURNSH ( 'SAMPLE.BSP' )
!!
!!            .
!!            .
!!            .
!!
!!     C
!!     C      Divide the interval of coverage [BEGIN,END] into
!!     C      N steps. At each step, compute the position, and
!!     C      print out the epoch in UTC time and position norm.
!!     C
!!            DELTA = ( END - BEGIN ) / N
!!
!!            DO I = 0, N
!!
!!               ET = BEGIN + I*DELTA
!!
!!               CALL SPKGPS ( MOON, ET, 'J2000', EARTH, POS, LT )
!!
!!               CALL ET2UTC ( ET, 'C', 0, UTC )
!!
!!               WRITE (*,*) UTC, VNORM ( POS )
!!
!!            END DO
!!
!! ## Restrictions
!!
!!     1)  The ephemeris files to be used by SPKGPS must be loaded
!!         by SPKLEF before SPKGPS is called.
!!
!! ## Literature References
!!
!!     None.
!!
!! ## Author and Institution
!!
!!     N.J. Bachman       (JPL)
!!     J. Diaz del Rio    (ODC Space)
!!     B.V. Semenov       (JPL)
!!     W.L. Taber         (JPL)
!!
!! ## Version
!!
!!-    SPICELIB Version 2.1.0, 09-OCT-2021 (JDR) (NJB)
!!
!!        Bug fix: added calls to FAILED after calls to SPKPVN.
!!        Previously only one call to SPKPVN was followed by a FAILED
!!        call. Moved some FAILED checks so they will be hit whether
!!        or not SPKSFS finds a segment.
!!
!!        Edited the header to comply with NAIF standard. Removed
!!        unnecessary $Revisions section.
!!
!!-    SPICELIB Version 2.0.0, 08-JAN-2014 (BVS)
!!
!!        Updated to save the input frame name and POOL state counter
!!        and to do frame name-ID conversion only if the counter has
!!        changed.
!!
!!        Updated to map the input frame name to its ID by first calling
!!        ZZNAMFRM, and then calling IRFNUM. The side effect of this
!!        change is that now the frame with the fixed name 'DEFAULT'
!!        that can be associated with any code via CHGIRF's entry point
!!        IRFDEF will be fully masked by a frame with identical name
!!        defined via a text kernel. Previously the CHGIRF's 'DEFAULT'
!!        frame masked the text kernel frame with the same name.
!!
!!        Replaced SPKLEF with FURNSH and fixed errors in $Examples.
!!
!!-    SPICELIB Version 1.2.0, 05-NOV-2005 (NJB)
!!
!!        Updated to remove non-standard use of duplicate arguments
!!        in VADD calls.
!!
!!-    SPICELIB Version 1.1.0, 05-JAN-2005 (NJB)
!!
!!        Tests of routine FAILED() were added.
!!
!!-    SPICELIB Version 1.0.0, 09-JUL-1998 (WLT)
!!
!! ## Index Entries
!!
!!     geometric position of one body relative to another
!!
!!     This is the idea:
!!
!!     Every body moves with respect to some center. The center
!!     is itself a body, which in turn moves about some other
!!     center.  If we begin at the target body (T), follow
!!     the chain,
!!
!!                                   T
!!                                     \
!!           SSB                        \
!!               \                     C[1]
!!                \                     /
!!                 \                   /
!!                  \                 /
!!                   \               /
!!                  C[3]-----------C[2]
!!
!!     and avoid circular definitions (A moves about B, and B moves
!!     about A), eventually we get the position relative to the solar
!!     system barycenter (which, for our purposes, doesn't move).
!!     Thus,
!!
!!        T    = T     + C[1]     + C[2]     + ... + C[n]
!!         SSB    C[1]       C[2]       [C3]             SSB
!!
!!     where
!!
!!        X
!!         Y
!!
!!     is the position of body X relative to body Y.
!!
!!     However, we don't want to follow each chain back to the SSB
!!     if it isn't necessary.  Instead we will just follow the chain
!!     of the target body and follow the chain of the observing body
!!     until we find a common node in the tree.
!!
!!     In the example below, C is the first common node.  We compute
!!     the position of TARG relative to C and the position of OBS
!!     relative to C, then subtract the two positions.
!!
!!                                   TARG
!!                                     \
!!           SSB                        \
!!               \                       A
!!                \                     /            OBS
!!                 \                   /              |
!!                  \                 /               |
!!                   \               /                |
!!                    B-------------C-----------------D
!!
!!     SPICELIB functions
!!
!!     Local parameters
!!
!!     CHLEN is the maximum length of a chain.  That is,
!!     it is the maximum number of bodies in the chain from
!!     the target or observer to the SSB.
!!
!!     Saved frame name length.
!!
!!     Local variables
!!
!!     Saved frame name/ID item declarations.
!!
!!     Saved frame name/ID items.
!!
!!     Initial values.
!!
!!     In-line Function Definitions
!!
!!     Standard SPICE error handling.
!!
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: clight
      use pepper_arrays, only: cleard, isrchi, moved
      use pepper_math, only: mxv, vadd, vequ, vnorm, vsub
      use pepper_strings, only: frstnp, intstr, prefix, suffix
      use pepper_inc_ninert
      use pepper_inc_zzctr
      implicit none
      character(*), parameter :: rname = 'ZZSPKGP1'
      integer(ip), parameter :: chlen = 20
      integer(ip), parameter :: ssb = 0
      integer(ip), parameter :: frnmln = 32
      character(40)         ident
      character(40)         tname
      character(40)         oname
      character(80)         tstring
      real(wp)              descr  ( 5        )
      real(wp)              sobs   ( 6        )
      real(wp)              starg  ( 6, chlen )
      real(wp)              stemp  ( 6        )
      real(wp)              psxfrm ( 3, 3     )
      real(wp)              rot    ( 3, 3     )
      real(wp)              vtemp  ( 6        )
      integer(ip)           cframe
      integer(ip)           cobs
      integer(ip)           ctarg  (    chlen )
      integer(ip)           tframe (    chlen )
      integer(ip)           ctpos
      integer(ip)           handle
      integer(ip)           i
      integer(ip)           legs
      integer(ip)           nct
      integer(ip)           refid
      integer(ip)           tmpfrm
      logical               found
      logical               nofrm
      logical               isinrt
      isinrt ( tmpfrm, cframe ) =       cframe > 0 &
                                  .and. cframe <= ninert &
                                  .and. tmpfrm > 0 &
                                  .and. tmpfrm <= ninert
      if ( .not. allocated ( ctx%zzspkgp1_state ) ) then
         allocate ( ctx%zzspkgp1_state )
         call init_zzspkgp1_state ( ctx%zzspkgp1_state )
      end if
      associate ( svctr1 => ctx%zzspkgp1_state%svctr1, &
                  svref => ctx%zzspkgp1_state%svref, &
                  svrefi => ctx%zzspkgp1_state%svrefi, &
                  first => ctx%zzspkgp1_state%first )
      if ( ctx%failed() ) return

!
!     Initialization.
!
      if ( first ) then

!
!        Initialize counter.
!
         call zzctruin( ctx, svctr1 )

         first = .false.

      end if

!
!     We take care of the obvious case first.  It TARG and OBS are the
!     same we can just fill in zero.
!
      if ( targ == obs ) then

         lt = 0.0_wp

         call cleard ( 3, pos )
         return

      end if

!
!     CTARG contains the integer codes of the bodies in the
!     target body chain, beginning with TARG itself and then
!     the successive centers of motion.
!
!     STARG(1,I) is the position of the target body relative
!     to CTARG(I).  The id-code of the frame of this position is
!     stored in TFRAME(I).
!
!     COBS and SOBS will contain the centers and positions of the
!     observing body.  (They are single elements instead of arrays
!     because we only need the current center and position of the
!     observer relative to it.)
!
!     First, we construct CTARG and STARG.  CTARG(1) is
!     just the target itself, and STARG(1,1) is just a zero
!     vector, that is, the position of the target relative
!     to itself.
!
!     Then we follow the chain, filling up CTARG and STARG
!     as we go.  We use SPKSFS to search through loaded
!     files to find the first segment applicable to CTARG(1)
!     and time ET.  Then we use SPKPVN to compute the position
!     of the body CTARG(1) at ET in the segment that was found
!     and get its center and frame of motion (CTARG(2) and TFRAME(2).
!
!     We repeat the process for CTARG(2) and so on, until
!     there is no data found for some CTARG(I) or until we
!     reach the SSB.
!
!     Next, we find centers and positions in a similar manner
!     for the observer.  It's a similar construction as
!     described above, but I is always 1.  COBS and SOBS
!     are overwritten with each new center and position,
!     beginning at OBS.  However, we stop when we encounter
!     a common center of motion, that is when COBS is equal
!     to CTARG(I) for some I.
!
!     Finally, we compute the desired position of the target
!     relative to the observer by subtracting the position of
!     the observing body relative to the common node from
!     the position of the target body relative to the common
!     node.
!
!     CTPOS is the position in CTARG of the common node.
!
!     Since the upgrade to use hashes and counter bypass ZZNAMFRM
!     became more efficient in looking up frame IDs than IRFNUM. So the
!     original order of calls "IRFNUM first, NAMFRM second" was
!     switched to "ZZNAMFRM first, IRFNUM second".
!
!     The call to IRFNUM, now redundant for built-in inertial frames,
!     was preserved to for a sole reason -- to still support the
!     ancient and barely documented ability for the users to associate
!     a frame with the fixed name 'DEFAULT' with any CHGIRF inertial
!     frame code via CHGIRF's entry point IRFDEF.
!
!     Note that in the case of ZZNAMFRM's failure to resolve name and
!     IRFNUM's success to do so, the code returned by IRFNUM for
!     'DEFAULT' frame is *not* copied to the saved code SVREFI (which
!     would be set to 0 by ZZNAMFRM) to make sure that on subsequent
!     calls ZZNAMFRM does not do a bypass (as SVREFI always forced look
!     up) and calls IRFNUM again to reset the 'DEFAULT's frame ID
!     should it change between the calls.
!
      call zznamfrm ( ctx, svctr1, svref, svrefi, ref, refid )

      if ( refid == 0 ) then
         call irfnum ( ctx, ref, refid )
      end if

      if ( refid == 0 ) then

         if (  frstnp(ref) > 0 ) then

            call ctx%setmsg ( 'The string supplied to specify the ' &
            //            'reference frame, (''#'') contains ' &
            //            'non-printing characters.  The two ' &
            //            'most common causes for this kind ' &
            //            'of error are: 1. an error in the ' &
            //            'call to ZZSPKGP1; 2. an ' &
            //            'uninitialized variable. ' )
            call ctx%errch  ( '#', ref )

         else if ( ref == ' ' ) then

            call ctx%setmsg ( 'The string supplied to specify the ' &
            //            'reference frame is blank.  The most ' &
            //            'common cause for this kind of error ' &
            //            'is an uninitialized variable. ' )

         else

            call ctx%setmsg ( 'The string supplied to specify the ' &
            //            'reference frame was ''#''.  This ' &
            //            'frame is not recognized. Possible ' &
            //            'causes for this error are: 1. failure ' &
            //            'to load the frame definition into the ' &
            //            'kernel pool; 2. An out-of-date ' &
            //            'edition of the toolkit. ' )
            call ctx%errch  ( '#', ref )

         end if

         call ctx%sigerr ( 'SPICE(UNKNOWNFRAME)' )

         if ( ctx%failed() ) return

      end if

!
!     Fill in CTARG and STARG until no more data is found
!     or until we reach the SSB.  If the chain gets too
!     long to fit in CTARG, that is if I equals CHLEN,
!     then overwrite the last elements of CTARG and STARG.
!
!     Note the check for FAILED in the loop.  If SPKSFS
!     or SPKPVN happens to fail during execution, and the
!     current error handling action is to NOT abort, then
!     FOUND may be stuck at TRUE, CTARG(I) will never
!     become zero, and the loop will execute indefinitely.
!
!     Construct CTARG and STARG.  Begin by assigning the
!     first elements:  TARG and the position of TARG relative
!     to itself.
!
      i        =  1
      ctarg(i) =  targ
      found    = .true.

      call cleard ( 6, starg(1,i) )

      do while (           found &
                  .and.  ( i        < chlen ) &
                  .and.  ( ctarg(i) /= obs   ) &
                  .and.  ( ctarg(i) /= ssb   )  )

!
!        Find a file and segment that has position
!        data for CTARG(I).
!
         call spksfs ( ctx, ctarg(i), et, handle, descr, ident, found )

         if ( found ) then
!
!           Get the position of CTARG(I) relative to some
!           center of motion.  This new center goes in
!           CTARG(I+1) and the position is called STEMP.
!
            i = i + 1

            call spkpvn ( ctx, handle,    descr,      et, &
                          tframe(i), starg(1,i), ctarg(i) )
!
!           Here's what we have.  STARG is the position of CTARG(I-1)
!           relative to CTARG(I) in reference frame TFRAME(I)
!
         end if
!
!        If one of the routines above failed during
!        execution, we just give up and return.
!
         if ( ctx%failed() ) return

      end do

      tframe(1) = tframe(2)
!
!     If the loop above ended because we ran out of
!     room in the arrays CTARG and STARG, then we
!     continue finding positions but we overwrite the
!     last elements of CTARG and STARG.
!
!     If, as a result, the first common node is
!     overwritten, we'll just have to settle for
!     the last common node.  This will cause a small
!     loss of precision, but it's better than other
!     alternatives.
!
      if ( i == chlen ) then

         do while (      found &
                   .and. ctarg(chlen) /= ssb &
                   .and. ctarg(chlen) /= obs )

!
!           Find a file and segment that has position
!           data for CTARG(CHLEN).
!
            call spksfs ( ctx, ctarg(chlen), et, handle, descr, ident, found)

            if ( found ) then
!
!              Get the position of CTARG(CHLEN) relative to
!              some center of motion.  The new center
!              overwrites the old.  The position is called
!              STEMP.
!
               call spkpvn ( ctx, handle, descr, et, tmpfrm, stemp, &
                             ctarg(chlen))

               if ( ctx%failed() ) return

!
!              Add STEMP to the position of TARG relative to
!              the old center to get the position of TARG
!              relative to the new center.  Overwrite
!              the last element of STARG.
!
               if ( tframe(chlen) == tmpfrm ) then

                  call moved ( starg(1,chlen), 3, vtemp )

               else if ( isinrt ( tframe(chlen), tmpfrm ) ) then

                  call irfrot( ctx, tframe(chlen), tmpfrm,         rot      )
                  call mxv   ( rot,           starg(1,chlen), vtemp(1) )

               else

                  call zzrefch1( ctx, tframe(chlen), tmpfrm,   et,   psxfrm )

                  if ( ctx%failed() ) return

                  call mxv   ( psxfrm, starg (1,chlen),       vtemp  )

               end if

               call vadd ( vtemp,  stemp, starg(1,chlen) )

               tframe(chlen) = tmpfrm

            end if

!
!           If one of the routines above failed during
!           execution, we just give up and return.
!
            if ( ctx%failed() ) return

         end do

      end if

      nct = i
!
!     NCT is the number of elements in CTARG,
!     the chain length.  We have in hand the following information
!
!        STARG(1...3,K)  position of body
!        CTARG(K-1)      relative to body CTARG(K) in the frame
!        TFRAME(K)
!
!     For K = 2,..., NCT.
!
!     CTARG(1) = TARG
!     STARG(1...3,1) = ( 0, 0, 0 )
!     TFRAME(1)      = TFRAME(2)
!

!
!     Now follow the observer's chain.  Assign
!     the first values for COBS and SOBS.
!
      cobs = obs
      call cleard ( 6, sobs )

!
!     Perhaps we have a common node already.
!     If so it will be the last node on the
!     list CTARG.
!
!     We let CTPOS will be the position of the common
!     node in CTARG if one is found.  It will
!     be zero if COBS is not found in CTARG.
!
      if ( ctarg(nct) == cobs ) then
         ctpos  = nct
         cframe = tframe(ctpos)
      else
         ctpos  = 0
      end if

!
!     Repeat the same loop as above, but each time
!     we encounter a new center of motion, check to
!     see if it is a common node.  (When CTPOS is
!     not zero, CTARG(CTPOS) is the first common node.)
!
!     Note that we don't need a centers array nor a
!     positions array, just a single center and position
!     is sufficient --- we just keep overwriting them.
!     When the common node is found, we have everything
!     we need in that one center (COBS) and position
!     (SOBS-position of the target relative to COBS).
!
      found  = .true.
      nofrm  = .true.
      legs   =  0

      do while (           found &
                  .and.  ( cobs  /= ssb ) &
                  .and.  ( ctpos == 0   )  )

!
!        Find a file and segment that has position
!        data for COBS.
!
         call spksfs ( ctx, cobs, et, handle, descr, ident, found )

         if ( found ) then
!
!           Get the position of COBS; call it STEMP.
!           The center of motion of COBS becomes the
!           new COBS.
!
            if ( legs == 0 ) then
               call spkpvn ( ctx, handle, descr, et, tmpfrm, sobs,  cobs )
            else
               call spkpvn ( ctx, handle, descr, et, tmpfrm, stemp, cobs )
            end if

            if ( ctx%failed() ) return

            if ( nofrm ) then
               nofrm  = .false.
               cframe = tmpfrm
            end if
!
!           Add STEMP to the position of OBS relative to
!           the old COBS to get the position of OBS
!           relative to the new COBS.
!
            if ( cframe == tmpfrm ) then
!
!              On the first leg of the position of the observer, we
!              don't have to add anything, the position of the
!              observer is already in SOBS.  We only have to add when
!              the number of legs in the observer position is one or
!              greater.
!
               if ( legs > 0 ) then
                  call vadd ( sobs,  stemp, vtemp )
                  call vequ ( vtemp,        sobs  )
               end if

            else if ( isinrt( cframe, tmpfrm ) ) then

               call irfrot ( ctx, cframe, tmpfrm, rot )
               call mxv    ( rot,    sobs(1),   vtemp(1)  )
               call vadd   ( vtemp,  stemp,     sobs      )
               cframe = tmpfrm

            else

               call zzrefch1 ( ctx, cframe, tmpfrm, et,   psxfrm )

               if ( ctx%failed() ) return

               call mxv    ( psxfrm, sobs,         vtemp  )
               call vadd   ( vtemp,  stemp,        sobs   )
               cframe = tmpfrm

            end if

!
!           We now have one more leg of the path for OBS.  Set
!           LEGS to reflect this.  Then see if the new center
!           is a common node. If not, repeat the loop.
!
            legs  = legs + 1
            ctpos = isrchi ( cobs, nct, ctarg )

         end if

!
!        Check failed.  We don't want to loop indefinitely.
!
         if ( ctx%failed() ) return

      end do

!
!     If CTPOS is zero at this point, it means we
!     have not found a common node though we have
!     searched through all the available data.
!
      if ( ctpos == 0 ) then

         call bodc2n ( ctx, targ, tname,   found )

         if ( found ) then
            call prefix ( '# (',  0,        tname )
            call suffix ( ')',    0,        tname )
            call repmi  ( ctx, tname, '#', targ, tname )
         else
            call intstr ( targ, tname )
         end if

         call bodc2n ( ctx, obs,  oname,   found )

         if ( found ) then
            call prefix ( '# (',  0,       oname )
            call suffix ( ')',    0,       oname )
            call repmi  ( ctx, oname, '#', obs, oname )
         else
            call intstr ( obs, oname )
         end if

         call ctx%setmsg ( 'Insufficient ephemeris data has been ' &
         //            'loaded to compute the position of TARG ' &
         //            'relative to OBS at the ephemeris epoch ' &
         //            '#. ' )

         call etcal  ( ctx,  et,    tstring                                 )
         call ctx%errch  ( 'TARG', tname                                   )
         call ctx%errch  ( 'OBS',  oname                                   )
         call ctx%errch  ( '#',    tstring                                 )
         call ctx%sigerr ( 'SPICE(SPKINSUFFDATA)'                          )
         return

      end if

!
!     If CTPOS is not zero, then we have reached a
!     common node, specifically,
!
!        CTARG(CTPOS) = COBS = CENTER
!
!     (in diagram below).  The POSITION of the target
!     (TARG) relative to the observer (OBS) is just
!
!        STARG(1,CTPOS) - SOBS.
!
!                     SOBS
!         CENTER ---------------->OBS
!            |                  .
!            |                . N
!         S  |              . O
!         T  |            . I
!         A  |          . T
!         R  |        . I
!         G  |      . S
!            |    . O
!            |  . P
!            V L
!           TARG
!
!     And the light-time between them is just
!
!               | POSITION |
!          LT = ---------
!                   c
!
!     Compute the position of the target relative to CTARG(CTPOS)
!
      if ( ctpos == 1 ) then
         tframe(1) = cframe
      end if

      do i = 2, ctpos-1

         if ( tframe(i) == tframe(i+1) ) then

            call vadd  ( starg(1,i), starg(1,i+1),  stemp        )
            call moved ( stemp,      3,             starg(1,i+1) )

         else if ( isinrt ( tframe(i), tframe(i+1) ) ) then

            call irfrot ( ctx, tframe(i),  tframe(i+1),     rot          )
            call mxv    ( rot,        starg(1,i),      stemp(1)     )
            call vadd   ( stemp,      starg(1,i+1),    vtemp        )
            call moved  ( vtemp,      3,               starg(1,i+1) )

         else

            call zzrefch1 ( ctx, tframe(i),  tframe(i+1), et,  psxfrm       )

            if ( ctx%failed() ) return

            call mxv    ( psxfrm,     starg(1,i),       stemp        )
            call vadd   ( stemp,      starg(1,i+1),     vtemp        )
            call moved  ( vtemp,      3,                starg(1,i+1) )

         end if

      end do

!
!     To avoid unnecessary frame transformations we'll do
!     a bit of extra decision making here.  It's a lot
!     faster to make logical checks than it is to compute
!     frame transformations.
!
      if ( tframe(ctpos) == cframe ) then

         call vsub   ( starg(1,ctpos), sobs,  pos  )

      else if ( tframe(ctpos) == refid ) then

!
!        If the last frame associated with the target is already
!        in the requested output frame, we convert the position of
!        the observer to that frame and then subtract the position
!        of the observer from the position of the target.
!
         if ( isinrt( cframe, refid ) ) then

            call irfrot ( ctx, cframe, refid,   rot      )
            call mxv    ( rot,    sobs(1), stemp(1) )

         else

            call zzrefch1 ( ctx, cframe, refid, et,  psxfrm )

            if ( ctx%failed() ) return

            call mxv    ( psxfrm, sobs,       stemp  )

         end if

!
!        We've now transformed SOBS into the requested reference frame.
!        Set CFRAME to reflect this.
!
         cframe = refid
         call vsub  ( starg(1,ctpos), stemp, pos )

      else if ( isinrt( tframe(ctpos), cframe ) ) then

!
!        If both frames are inertial we use IRFROT instead of
!        ZZREFCH1 to get things into a common frame.
!
         call irfrot ( ctx, tframe(ctpos),  cframe,               rot      )
         call mxv    ( rot,            starg(1,ctpos),       stemp(1) )
         call vsub   ( stemp,          sobs,                 pos    )

      else
!
!        Use the more general routine ZZREFCH1 to make the
!        transformation.
!
         call zzrefch1 ( ctx, tframe(ctpos),  cframe,          et,  psxfrm )

         if ( ctx%failed() ) return

         call mxv    ( psxfrm,         starg(1,ctpos),       stemp  )
         call vsub   ( stemp,          sobs,                 pos  )

      end if

!
!     Finally, rotate as needed into the requested frame.
!
      if      ( cframe == refid ) then

!
!        We don't have to do anything in this case.
!

      else if ( isinrt ( cframe, refid ) ) then
!
!        Since both frames are inertial, we use the more direct
!        routine IRFROT to get the transformation to REFID.
!
         call irfrot ( ctx, cframe,  refid,    rot          )
         call mxv    ( rot,     pos(1),   stemp(1)     )
         call moved  ( stemp,        3,   pos          )

      else

         call zzrefch1 ( ctx, cframe,  refid, et,  psxfrm  )

         if ( ctx%failed() ) return

         call mxv    ( psxfrm,  pos,        stemp   )
         call moved  ( stemp,          3,   pos     )

      end if

      lt = vnorm  ( pos ) / clight

      return
      end associate
      end procedure zzspkgp1

      module procedure zzspklt1
!! ## Abstract
!!
!!     Return the state (position and velocity) of a target body
!!     relative to an observer, optionally corrected for light time,
!!     expressed relative to an inertial reference frame.
!!
!! ## Required Reading
!!
!!     SPK
!!
!! ## Keywords
!!
!!     EPHEMERIS
!!
!! ## Declarations
!!
!! ## Detailed Input
!!
!!     TARG        is the NAIF ID code for a target body.  The target
!!                 and observer define a state vector whose position
!!                 component points from the observer to the target.
!!
!!     ET          is the ephemeris time, expressed as seconds past
!!                 J2000 TDB, at which the state of the target body
!!                 relative to the observer is to be computed. ET
!!                 refers to time at the observer's location.
!!
!!     REF         is the inertial reference frame with respect to which
!!                 the input state STOBS and the output state STARG are
!!                 expressed. REF must be recognized by the SPICE
!!                 Toolkit. The acceptable frames are listed in the
!!                 Frames Required Reading, as well as in the SPICELIB
!!                 routine CHGIRF.
!!
!!                 Case and blanks are not significant in the string
!!                 REF.
!!
!!     ABCORR      indicates the aberration corrections to be applied to
!!                 the state of the target body to account for one-way
!!                 light time. See the discussion in the Particulars
!!                 section for recommendations on how to choose
!!                 aberration corrections.
!!
!!                 If ABCORR includes the stellar aberration correction
!!                 symbol '+S', this flag is simply ignored. Aside from
!!                 the possible presence of this symbol, ABCORR may be
!!                 any of the following:
!!
!!                    'NONE'     Apply no correction. Return the
!!                               geometric state of the target body
!!                               relative to the observer.
!!
!!                 The following values of ABCORR apply to the
!!                 "reception" case in which photons depart from the
!!                 target's location at the light-time corrected epoch
!!                 ET-LT and *arrive* at the observer's location at ET:
!!
!!                    'LT'       Correct for one-way light time (also
!!                               called "planetary aberration") using a
!!                               Newtonian formulation. This correction
!!                               yields the state of the target at the
!!                               moment it emitted photons arriving at
!!                               the observer at ET.
!!
!!                               The light time correction involves
!!                               iterative solution of the light time
!!                               equation (see Particulars for details).
!!                               The solution invoked by the 'LT' option
!!                               uses one iteration.
!!
!!                    'CN'       Converged Newtonian light time
!!                               correction. In solving the light time
!!                               equation, the 'CN' correction iterates
!!                               until the solution converges (three
!!                               iterations on all supported platforms).
!!                               Whether the 'CN+S' solution is
!!                               substantially more accurate than the
!!                               'LT' solution depends on the geometry
!!                               of the participating objects and on the
!!                               accuracy of the input data. In all
!!                               cases this routine will execute more
!!                               slowly when a converged solution is
!!                               computed. See the Particulars section of
!!                               SPKEZR for a discussion of precision of
!!                               light time corrections.
!!
!!                 The following values of ABCORR apply to the
!!                 "transmission" case in which photons *depart* from
!!                 the observer's location at ET and arrive at the
!!                 target's location at the light-time corrected epoch
!!                 ET+LT:
!!
!!                    'XLT'      "Transmission" case:  correct for
!!                               one-way light time using a Newtonian
!!                               formulation. This correction yields the
!!                               state of the target at the moment it
!!                               receives photons emitted from the
!!                               observer's location at ET.
!!
!!                    'XCN'      "Transmission" case:  converged
!!                               Newtonian light time correction.
!!
!!                 Neither special nor general relativistic effects are
!!                 accounted for in the aberration corrections applied
!!                 by this routine.
!!
!!                 Case and blanks are not significant in the string
!!                 ABCORR.
!!
!!     STOBS       is the geometric (uncorrected) state of the observer
!!                 relative to the solar system barycenter at epoch ET.
!!                 STOBS is a 6-vector: the first three components of
!!                 STOBS represent a Cartesian position vector; the last
!!                 three components represent the corresponding velocity
!!                 vector. STOBS is expressed relative to the inertial
!!                 reference frame designated by REF.
!!
!!                 Units are always km and km/sec.
!!
!! ## Detailed Output
!!
!!     STARG       is a Cartesian state vector representing the position
!!                 and velocity of the target body relative to the
!!                 specified observer. STARG is corrected for the
!!                 specified aberration, and is expressed with respect
!!                 to the specified inertial reference frame.  The first
!!                 three components of STARG represent the x-, y- and
!!                 z-components of the target's position; last three
!!                 components form the corresponding velocity vector.
!!
!!                 The position component of STARG points from the
!!                 observer's location at ET to the aberration-corrected
!!                 location of the target. Note that the sense of the
!!                 position vector is independent of the direction of
!!                 radiation travel implied by the aberration
!!                 correction.
!!
!!                 Units are always km and km/sec.
!!
!!     LT          is the one-way light time between the observer and
!!                 target in seconds.  If the target state is corrected
!!                 for light time, then LT is the one-way light time
!!                 between the observer and the light time-corrected
!!                 target location.
!!
!!     DLT         is the derivative with respect to barycentric
!!                 dynamical time of the one way light time between
!!                 target and observer:
!!
!!                    DLT = d(LT)/d(ET)
!!
!!                 DLT can also be described as the rate of change of
!!                 one way light time. DLT is unitless, since LT and
!!                 ET both have units of TDB seconds.
!!
!!                 If the observer and target are at the same position,
!!                 then DLT is set to zero.
!!
!! ## Parameters
!!
!!     None.
!!
!! ## Exceptions
!!
!!     1) For the convenience of the caller, the input aberration
!!        correction flag can call for stellar aberration correction via
!!        inclusion of the '+S' suffix. This portion of the aberration
!!        correction flag is ignored if present.
!!
!!     2) If the value of ABCORR is not recognized, the error
!!        is diagnosed by a routine in the call tree of this
!!        routine.
!!
!!     3) If the reference frame requested is not a recognized
!!        inertial reference frame, the error SPICE(BADFRAME)
!!        is signaled.
!!
!!     4) If the state of the target relative to the solar system
!!        barycenter cannot be computed, the error will be diagnosed
!!        by routines in the call tree of this routine.
!!
!!     5) If the observer and target are at the same position,
!!        then DLT is set to zero. This situation could arise,
!!        for example, when the observer is Mars and the target
!!        is the Mars barycenter.
!!
!!     6) If a division by zero error would occur in the computation
!!        of DLT, the error SPICE(DIVIDEBYZERO) is signaled.
!!
!! ## Files
!!
!!     This routine computes states using SPK files that have been
!!     loaded into the SPICE system, normally via the kernel loading
!!     interface routine FURNSH.  Application programs typically load
!!     kernels once before this routine is called, for example during
!!     program initialization; kernels need not be loaded repeatedly.
!!     See the routine FURNSH and the SPK and KERNEL Required Reading
!!     for further information on loading (and unloading) kernels.
!!
!!     If any of the ephemeris data used to compute STARG are expressed
!!     relative to a non-inertial frame in the SPK files providing those
!!     data, additional kernels may be needed to enable the reference
!!     frame transformations required to compute the state. Normally
!!     these additional kernels are PCK files or frame kernels. Any
!!     such kernels must already be loaded at the time this routine is
!!     called.
!!
!! ## Particulars
!!
!!     This routine supports higher-level SPK API routines that can
!!     perform both light time and stellar aberration corrections.
!!     User applications normally will not need to call this routine
!!     directly.
!!
!!     See the header of the routine SPKEZR for a detailed discussion
!!     of aberration corrections.
!!
!! ## Examples
!!
!!     The numerical results shown for this example may differ across
!!     platforms. The results depend on the SPICE kernels used as
!!     input, the compiler and supporting libraries, and the machine
!!     specific arithmetic implementation.
!!
!!    1) Look up a sequence of states of the Moon as seen from the
!!       Earth. Use light time corrections. Compute the first state for
!!       the epoch 2000 JAN 1 12:00:00 TDB; compute subsequent states at
!!       intervals of 1 hour. For each epoch, display the states, the
!!       one way light time between target and observer, and the rate of
!!       change of the one way light time.
!!
!!       Use the following meta-kernel to specify the kernels to
!!       load:
!!
!!          KPL/MK
!!
!!          File name: spkltc.tm
!!
!!          This meta-kernel is intended to support operation of SPICE
!!          example programs. The kernels shown here should not be
!!          assumed to contain adequate or correct versions of data
!!          required by SPICE-based user applications.
!!
!!          In order for an application to use this meta-kernel, the
!!          kernels referenced here must be present in the user's
!!          current working directory.
!!
!!          \begindata
!!
!!             KERNELS_TO_LOAD = ( 'de421.bsp',
!!                                 'pck00010.tpc',
!!                                 'naif0010.tls'  )
!!
!!          \begintext
!!
!!       The code example follows:
!!
!!           PROGRAM EX1
!!           IMPLICIT NONE
!!     C
!!     C     Local constants
!!     C
!!     C     The meta-kernel name shown here refers to a file whose
!!     C     contents are those shown above. This file and the kernels
!!     C     it references must exist in your current working directory.
!!     C
!!           CHARACTER*(*)         META
!!           PARAMETER           ( META   = 'spkltc.tm' )
!!     C
!!     C     Use a time step of 1 hour; look up 5 states.
!!     C
!!           DOUBLE PRECISION      STEP
!!           PARAMETER           ( STEP   = 3600.0D0 )
!!
!!           INTEGER               MAXITR
!!           PARAMETER           ( MAXITR = 5 )
!!     C
!!     C     Local variables
!!     C
!!           DOUBLE PRECISION      DLT
!!           DOUBLE PRECISION      ET
!!           DOUBLE PRECISION      ET0
!!           DOUBLE PRECISION      LT
!!           DOUBLE PRECISION      STATE ( 6 )
!!           DOUBLE PRECISION      STOBS ( 6 )
!!           INTEGER               I
!!
!!     C
!!     C     Load the SPK and LSK kernels via the meta-kernel.
!!     C
!!           CALL FURNSH ( META )
!!     C
!!     C     Convert the start time to seconds past J2000 TDB.
!!     C
!!           CALL STR2ET ( '2000 JAN 1 12:00:00 TDB', ET0 )
!!     C
!!     C     Step through a series of epochs, looking up a
!!     C     state vector at each one.
!!     C
!!           DO I = 1, MAXITR
!!
!!              ET = ET0 + (I-1)*STEP
!!
!!     C
!!     C        Look up a state vector at epoch ET using the
!!     C        following inputs:
!!     C
!!     C           Target:                 Moon (NAIF ID code 301)
!!     C           Reference frame:        J2000
!!     C           Aberration correction:  Light time ('LT')
!!     C           Observer:               Earth (NAIF ID code 399)
!!     C
!!     C        Before we can execute this computation, we'll need the
!!     C        geometric state of the observer relative to the solar
!!     C        system barycenter at ET, expressed relative to the
!!     C        J2000 reference frame:
!!     C
!!              CALL SPKSSB ( 399, ET,    'J2000', STOBS )
!!     C
!!     C        Now compute the desired state vector:
!!     C
!!              CALL SPKLTC ( 301,   ET,    'J2000', 'LT',
!!          .                 STOBS, STATE, LT,      DLT     )
!!
!!              WRITE (*,*) 'ET = ', ET
!!              WRITE (*,*) 'J2000 x-position (km):   ', STATE(1)
!!              WRITE (*,*) 'J2000 y-position (km):   ', STATE(2)
!!              WRITE (*,*) 'J2000 z-position (km):   ', STATE(3)
!!              WRITE (*,*) 'J2000 x-velocity (km/s): ', STATE(4)
!!              WRITE (*,*) 'J2000 y-velocity (km/s): ', STATE(5)
!!              WRITE (*,*) 'J2000 z-velocity (km/s): ', STATE(6)
!!              WRITE (*,*) 'One-way light time (s):  ', LT
!!              WRITE (*,*) 'Light time rate:         ', DLT
!!              WRITE (*,*) ' '
!!
!!           END DO
!!
!!           END
!!
!!     On a PC/Linux/gfortran platform, the following output was
!!     produced:
!!
!!        ET =    0.0000000000000000
!!        J2000 x-position (km):     -291569.26541282982
!!        J2000 y-position (km):     -266709.18647825718
!!        J2000 z-position (km):     -76099.155118763447
!!        J2000 x-velocity (km/s):   0.64353061322177041
!!        J2000 y-velocity (km/s):  -0.66608181700820079
!!        J2000 z-velocity (km/s):  -0.30132283179625752
!!        One-way light time (s):     1.3423106103251679
!!        Light time rate:           1.07316908698977495E-007
!!
!!        ET =    3600.0000000000000
!!        J2000 x-position (km):     -289240.78128184378
!!        J2000 y-position (km):     -269096.44087958336
!!        J2000 z-position (km):     -77180.899725757539
!!        J2000 x-velocity (km/s):   0.65006211520087476
!!        J2000 y-velocity (km/s):  -0.66016273921695667
!!        J2000 z-velocity (km/s):  -0.29964267390571342
!!        One-way light time (s):     1.3426939548635302
!!        Light time rate:           1.05652598952224259E-007
!!
!!        ET =    7200.0000000000000
!!        J2000 x-position (km):     -286888.88736709207
!!        J2000 y-position (km):     -271462.30170547962
!!        J2000 z-position (km):     -78256.555682137609
!!        J2000 x-velocity (km/s):   0.65653599154284592
!!        J2000 y-velocity (km/s):  -0.65419657680401588
!!        J2000 z-velocity (km/s):  -0.29794027307420823
!!        One-way light time (s):     1.3430713117337547
!!        Light time rate:           1.03990456898758609E-007
!!
!!        ET =    10800.000000000000
!!        J2000 x-position (km):     -284513.79173691198
!!        J2000 y-position (km):     -273806.60031034052
!!        J2000 z-position (km):     -79326.043183274567
!!        J2000 x-velocity (km/s):   0.66295190054599118
!!        J2000 y-velocity (km/s):  -0.64818380709706158
!!        J2000 z-velocity (km/s):  -0.29621577937090349
!!        One-way light time (s):     1.3434426890693671
!!        Light time rate:           1.02330665243423737E-007
!!
!!        ET =    14400.000000000000
!!        J2000 x-position (km):     -282115.70368389413
!!        J2000 y-position (km):     -276129.16976799071
!!        J2000 z-position (km):     -80389.282965712249
!!        J2000 x-velocity (km/s):   0.66930950377548726
!!        J2000 y-velocity (km/s):  -0.64212490805688027
!!        J2000 z-velocity (km/s):  -0.29446934336246899
!!        One-way light time (s):     1.3438080956559786
!!        Light time rate:           1.00673403630050830E-007
!!
!! ## Restrictions
!!
!!     1) The routine SPKGEO should be used instead of this routine
!!        to compute geometric states. SPKGEO introduces less
!!        round-off error when the observer and target have common
!!        center that is closer to both objects than is the solar
!!        system barycenter.
!!
!!     2) The kernel files to be used by SPKLTC must be loaded
!!        (normally by the SPICELIB kernel loader FURNSH) before
!!        this routine is called.
!!
!!     3) Unlike most other SPK state computation routines, this
!!        routine requires that the output state be relative to an
!!        inertial reference frame.
!!
!! ## Literature References
!!
!!     None.
!!
!! ## Author and Institution
!!
!!     N.J. Bachman    (JPL)
!!
!! ## Version
!!
!!-    SPICELIB Version 2.0.0, 04-JUL-2014 (NJB)
!!
!!        Discussion of light time corrections was updated. Assertions
!!        that converged light time corrections are unlikely to be
!!        useful were removed.
!!
!!     Last update was 02-MAY-2012 (NJB)
!!
!!        Updated to ensure convergence when CN or XCN light time
!!        corrections are used. The new algorithm also terminates early
!!        (after fewer than three iterations) when convergence is
!!        attained.
!!
!!        Call to ZZPRSCOR was replaced by a call to ZZVALCOR.
!!
!!-    SPICELIB Version 1.0.0, 11-JAN-2008 (NJB)
!!
!! ## Index Entries
!!
!!     low-level light time correction
!!     light-time corrected state from spk file
!!     get light-time corrected state
!!
!! ## Revisions
!!
!!     None.
!!
!!     SPICELIB functions
!!
!!     Local parameters
!!
!!     TOL is the tolerance used for a division-by-zero test
!!     performed prior to computation of DLT.
!!
!!     Convergence limit:
!!
!!     Maximum number of light time iterations for any
!!     aberration correction:
!!
!!     Local variables
!!
!!     Saved variables
!!
!!     Initial values
!!
!!     Standard SPICE error handling.
!!
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: clight
      use pepper_arrays, only: touchd
      use pepper_math, only: vdot, vlcom, vnorm, vsubg
      use pepper_inc_zzabcorr
      implicit none
      character(*), parameter :: rname = 'ZZSPKLT1'
      real(wp), parameter :: tol = 1.0e-10_wp
      real(wp), parameter :: cnvlim = 1.0e-17_wp
      integer(ip), parameter :: maxitr = 5
      integer(ip), parameter :: ssb = 0
      real(wp)              a
      real(wp)              b
      real(wp)              c
      real(wp)              dist
      real(wp)              epoch
      real(wp)              lterr
      real(wp)              prvlt
      real(wp)              ssblt
      real(wp)              ssbtrg ( 6 )
      integer(ip)           i
      integer(ip)           ltsign
      integer(ip)           numitr
      integer(ip)           refid
      logical               attblk ( nabcor )
      logical               usestl
      if ( .not. allocated ( ctx%zzspklt1_state ) ) then
         allocate ( ctx%zzspklt1_state )
         call init_zzspklt1_state ( ctx%zzspklt1_state )
      end if
      associate ( pass1 => ctx%zzspklt1_state%pass1, &
                  prvcor => ctx%zzspklt1_state%prvcor, &
                  usecn => ctx%zzspklt1_state%usecn, &
                  uselt => ctx%zzspklt1_state%uselt, &
                  xmit => ctx%zzspklt1_state%xmit )
      if ( ctx%failed() ) return

      if (  pass1  .or.  ( abcorr /= prvcor )  ) then
!
!        The aberration correction flag differs from the value it
!        had on the previous call, if any.  Analyze the new flag.
!
         call zzvalcor ( ctx, abcorr, attblk )

         if ( ctx%failed() ) return

!
!        The aberration correction flag is recognized; save it.
!
         prvcor = abcorr

!
!        Set logical flags indicating the attributes of the requested
!        correction:
!
!           XMIT is .TRUE. when the correction is for transmitted
!           radiation.
!
!           USELT is .TRUE. when any type of light time correction
!           (normal or converged Newtonian) is specified.
!
!           USECN indicates converged Newtonian light time correction.
!
!        The above definitions are consistent with those used by
!        ZZVALCOR.
!
         xmit    =  attblk ( xmtidx )
         uselt   =  attblk ( ltidx )
         usecn   =  attblk ( cnvidx )
         usestl  =  attblk ( stlidx )

         pass1   = .false.

      end if

!
!     See if the reference frame is a recognized inertial frame.
!
      call irfnum ( ctx, ref, refid )

      if ( refid == 0 ) then

         call ctx%setmsg ( 'The requested frame ''#'' is not a ' &
         //            'recognized inertial frame. '        )
         call ctx%errch  ( '#', ref                             )
         call ctx%sigerr ( 'SPICE(BADFRAME)'                    )
         return

      end if

!
!     Find the geometric state of the target body with respect to
!     the solar system barycenter. Subtract the state of the
!     observer to get the relative state. Use this to compute the
!     one-way light time.
!
      call zzspkgo1 ( ctx, targ,   et,    ref, ssb,  ssbtrg, ssblt )

      if ( ctx%failed() ) return

      call vsubg  ( ssbtrg, stobs, 6,   starg  )

      dist = vnorm(starg)
      lt   = dist / clight

      if ( lt == 0.0_wp ) then
!
!        This can happen only if the observer and target are at the
!        same position. We don't consider this an error, but we're not
!        going to compute the light time derivative.
!
         dlt = 0

         return

      end if

      if ( .not. uselt ) then
!
!        This is a special case: we're not using light time
!        corrections, so the derivative
!        of light time is just
!
!           (1/c) * d(VNORM(STARG))/dt
!
         dlt = vdot( starg, starg(4) ) / ( dist * clight )

!
!        LT and DLT are both set, so we can return.
!
         return

      end if

!
!     To correct for light time, find the state of the target body
!     at the current epoch minus the one-way light time. Note that
!     the observer remains where it is.
!
!     Determine the sign of the light time offset.
!
      if ( xmit ) then
         ltsign =  1
      else
         ltsign = -1
      end if

!
!     Let NUMITR be the number of iterations we'll perform to
!     compute the light time.
!
      if ( usecn ) then
         numitr = maxitr
      else
         numitr = 1
      end if

      i     = 0
      lterr = 1.0_wp

      do while (  ( i < numitr ) .and. ( lterr > cnvlim )  )
!
!        LT was set either prior to this loop or
!        during the previous loop iteration.
!
         epoch  =  et + ( ltsign * lt )

         call zzspkgo1 ( ctx, targ, epoch, ref, ssb, ssbtrg, ssblt )

         if ( ctx%failed() ) return

         call vsubg  ( ssbtrg, stobs, 6, starg )

         prvlt = lt
         lt    = touchd(  vnorm( starg ) / clight  )

!        LTERR is the magnitude of the change between the current
!        estimate of light time and the previous estimate, relative to
!        the previous light time corrected epoch.
!
         lterr = touchd(    abs( lt  - prvlt      ) &
                          / max( 1.0_wp, abs(epoch) )  )
         i     = i + 1

      end do

!
!     At this point, STARG contains the light time corrected
!     state of the target relative to the observer.
!
!     Compute the derivative of light time with respect
!     to time: dLT/dt.  Below we derive the formula for
!     this quantity for the reception case. Let
!
!        POBS be the position of the observer relative to the
!        solar system barycenter.
!
!        VOBS be the velocity of the observer relative to the
!        solar system barycenter.
!
!        PTARG be the position of the target relative to the
!        solar system barycenter.
!
!        VTARG be the velocity of the target relative to the
!        solar system barycenter.
!
!        S be the sign of the light time correction. S is
!        negative for the reception case.
!
!     The light-time corrected position of the target relative to
!     the observer at observation time ET, given the one-way
!     light time LT is:
!
!         PTARG(ET+S*LT) - POBS(ET)
!
!     The light-time corrected velocity of the target relative to
!     the observer at observation time ET is
!
!         VTARG(ET+S*LT)*( 1 + S*d(LT)/d(ET) ) - VOBS(ET)
!
!     We need to compute dLT/dt. Below, we use the facts that,
!     for a time-dependent vector X(t),
!
!          ||X||     = <X,X> ** (1/2)
!
!        d(||X||)/dt = (1/2)<X,X>**(-1/2) * 2 * <X,dX/dt>
!
!                    = <X,X>**(-1/2) *  <X,dX/dt>
!
!                    = <X,dX/dt> / ||X||
!
!     Newtonian light time equation:
!
!        LT     =   (1/c) * || PTARG(ET+S*LT) - POBS(ET)||
!
!     Differentiate both sides:
!
!        dLT/dt =   (1/c) * ( 1 / || PTARG(ET+S*LT) - POBS(ET) || )
!
!                  * < PTARG(ET+S*LT) - POBS(ET),
!                      VTARG(ET+S*LT)*(1+S*d(LT)/d(ET)) - VOBS(ET) >
!
!               = (1/c) * ( 1 / || PTARG(ET+S*LT) - POBS(ET) || )
!
!                 * (  < PTARG(ET+S*LT) - POBS(ET),
!                        VTARG(ET+S*LT) - VOBS(ET) >
!
!                   +  < PTARG(ET+S*LT) - POBS(ET),
!                        VTARG(ET+S*LT)           > * (S*d(LT)/d(ET))  )
!
!     Let
!
!        A =   (1/c) * ( 1 / || PTARG(ET+S*LT) - POBS(ET) || )
!
!        B =   < PTARG(ET+S*LT) - POBS(ET), VTARG(ET+S*LT) - VOBS(ET) >
!
!        C =   < PTARG(ET+S*LT) - POBS(ET), VTARG(ET+S*LT) >
!
!     Then
!
!        d(LT)/d(ET) =  A * ( B  +  C * S*d(LT)/d(ET) )
!
!     which implies
!
!        d(LT)/d(ET) =  A*B / ( 1 - S*C*A )
!
      a   =  1.0_wp / ( clight*vnorm(starg) )

      b   =  vdot ( starg, starg(4)  )

      c   =  vdot ( starg, ssbtrg(4) )

!
!     For physically realistic target velocities, S*C*A cannot equal 1.
!     We'll check for this case anyway.
!
      if (  ltsign*c*a  >  1.0_wp-tol ) then

         call ctx%setmsg ( 'Target range rate magnitude is ' &
      //               'approximately the speed of light. The ' &
      //               'light time derivative cannot be ' &
      //               'computed.'                            )
         call ctx%sigerr ( 'SPICE(DIVIDEBYZERO)'                  )
         return

      end if

!
!     Compute DLT: the rate of change of light time.
!
      dlt = (a*b)/( 1.0_wp - ltsign*c*a )

!
!     Overwrite the velocity portion of the output state
!     with the light-time corrected velocity.
!
      call vlcom ( 1.0_wp + ltsign*dlt, ssbtrg(4), &
                  -1.0_wp,              stobs(4),   starg(4) )

      return
      end associate
      end procedure zzspklt1

      module procedure zzspkpa1
!! ## Abstract
!!
!!     Return the position of a target body relative to an observer,
!!     optionally corrected for light time and stellar aberration.
!!
!! ## Required Reading
!!
!!     SPK
!!
!! ## Keywords
!!
!!     EPHEMERIS
!!
!! ## Declarations
!!
!! ## Detailed Input
!!
!!     TARG        is the NAIF ID code for a target body.  The target
!!                 and observer define a position vector which points
!!                 from the observer to the target.
!!
!!     ET          is the ephemeris time, expressed as seconds past
!!                 J2000 TDB, at which the position of the target body
!!                 relative to the observer is to be computed.  ET
!!                 refers to time at the observer's location.
!!
!!     REF         is the inertial reference frame with respect to which
!!                 the observer's state SOBS is expressed. REF must be
!!                 recognized by the SPICE Toolkit.  The acceptable
!!                 frames are listed in the Frames Required Reading, as
!!                 well as in the SPICELIB routine CHGIRF.
!!
!!                 Case and blanks are not significant in the string REF.
!!
!!     SOBS        is the geometric (uncorrected) state of the observer
!!                 relative to the solar system barycenter at epoch ET.
!!                 SOBS is a 6-vector:  the first three components of
!!                 SOBS represent a Cartesian position vector; the last
!!                 three components represent the corresponding velocity
!!                 vector.  SOBS is expressed relative to the inertial
!!                 reference frame designated by REF.
!!
!!                 Units are always km and km/sec.
!!
!!     ABCORR      indicates the aberration corrections to be applied to
!!                 the position of the target body to account for
!!                 one-way light time and stellar aberration.  See the
!!                 discussion in the Particulars section for
!!                 recommendations on how to choose aberration
!!                 corrections.
!!
!!                 ABCORR may be any of the following:
!!
!!                    'NONE'     Apply no correction. Return the
!!                               geometric position of the target body
!!                               relative to the observer.
!!
!!                 The following values of ABCORR apply to the
!!                 "reception" case in which photons depart from the
!!                 target's location at the light-time corrected epoch
!!                 ET-LT and *arrive* at the observer's location at ET:
!!
!!                    'LT'       Correct for one-way light time (also
!!                               called "planetary aberration") using a
!!                               Newtonian formulation. This correction
!!                               yields the position of the target at the
!!                               moment it emitted photons arriving at
!!                               the observer at ET.
!!
!!                               The light time correction involves
!!                               iterative solution of the light time
!!                               equation (see Particulars for details).
!!                               The solution invoked by the 'LT' option
!!                               uses one iteration.
!!
!!                    'LT+S'     Correct for one-way light time and
!!                               stellar aberration using a Newtonian
!!                               formulation. This option modifies the
!!                               position obtained with the 'LT' option
!!                               to account for the observer's velocity
!!                               relative to the solar system
!!                               barycenter. The result is the apparent
!!                               position of the target---the position
!!                               of the target as seen by the observer.
!!
!!                    'CN'       Converged Newtonian light time
!!                               correction. In solving the light time
!!                               equation, the 'CN' correction iterates
!!                               until the solution converges (three
!!                               iterations on all supported platforms).
!!                               Whether the 'CN+S' solution is
!!                               substantially more accurate than the
!!                               'LT' solution depends on the geometry
!!                               of the participating objects and on the
!!                               accuracy of the input data. In all
!!                               cases this routine will execute more
!!                               slowly when a converged solution is
!!                               computed. See the Particulars section
!!                               of SPKEZR for a discussion of precision
!!                               of light time corrections.
!!
!!                    'CN+S'     Converged Newtonian light time
!!                               correction and stellar aberration
!!                               correction.
!!
!!                 The following values of ABCORR apply to the
!!                 "transmission" case in which photons *depart* from
!!                 the observer's location at ET and arrive at the
!!                 target's location at the light-time corrected epoch
!!                 ET+LT:
!!
!!                    'XLT'      "Transmission" case:  correct for
!!                               one-way light time using a Newtonian
!!                               formulation. This correction yields the
!!                               position of the target at the moment it
!!                               receives photons emitted from the
!!                               observer's location at ET.
!!
!!                    'XLT+S'    "Transmission" case:  correct for
!!                               one-way light time and stellar
!!                               aberration using a Newtonian
!!                               formulation  This option modifies the
!!                               position obtained with the 'XLT' option
!!                               to account for the observer's velocity
!!                               relative to the solar system
!!                               barycenter. The target position
!!                               indicates the direction that photons
!!                               emitted from the observer's location
!!                               must be "aimed" to hit the target.
!!
!!                    'XCN'      "Transmission" case:  converged
!!                               Newtonian light time correction.
!!
!!                    'XCN+S'    "Transmission" case:  converged
!!                               Newtonian light time correction and
!!                               stellar aberration correction.
!!
!!                 Neither special nor general relativistic effects are
!!                 accounted for in the aberration corrections applied
!!                 by this routine.
!!
!!                 Case and blanks are not significant in the string
!!                 ABCORR.
!!
!! ## Detailed Output
!!
!!     PTARG       is a Cartesian 3-vector representing the position of
!!                 the target body relative to the specified observer.
!!                 PTARG is corrected for the specified aberrations, and
!!                 is expressed with respect to the specified inertial
!!                 reference frame.  The components of PTARG represent
!!                 the x-, y- and z-components of the target's position.
!!
!!                 The vector PTARG points from the observer's position
!!                 at ET to the aberration-corrected location of the
!!                 target. Note that the sense of the position vector is
!!                 independent of the direction of radiation travel
!!                 implied by the aberration correction.
!!
!!                 Units are always km.
!!
!!     LT          is the one-way light time between the observer and
!!                 target in seconds.  If the target position is
!!                 corrected for aberrations, then LT is the one-way
!!                 light time between the observer and the light time
!!                 corrected target location.
!!
!! ## Parameters
!!
!!     None.
!!
!! ## Exceptions
!!
!!     1) If the value of ABCORR is not recognized, the error
!!        'SPICE(SPKINVALIDOPTION)' is signaled.
!!
!!     2) If the reference frame requested is not a recognized
!!        inertial reference frame the error 'SPICE(BADFRAME)' is
!!        signaled.
!!
!!     3) If the position of the target relative to the solar system
!!        barycenter cannot be computed, the error will be diagnosed
!!        by routines in the call tree of this routine.
!!
!! ## Files
!!
!!     This routine computes positions using SPK files that have been
!!     loaded into the SPICE system, normally via the kernel loading
!!     interface routine FURNSH.  Application programs typically load
!!     kernels once before this routine is called, for example during
!!     program initialization; kernels need not be loaded repeatedly.
!!     See the routine FURNSH and the SPK and KERNEL Required Reading
!!     for further information on loading (and unloading) kernels.
!!
!!     If any of the ephemeris data used to compute PTARG are expressed
!!     relative to a non-inertial frame in the SPK files providing those
!!     data, additional kernels may be needed to enable the reference
!!     frame transformations required to compute PTARG.  Normally
!!     these additional kernels are PCK files or frame kernels.  Any
!!     such kernels must already be loaded at the time this routine is
!!     called.
!!
!! ## Particulars
!!
!!     In space science or engineering applications one frequently
!!     wishes to know where to point a remote sensing instrument, such
!!     as an optical camera or radio antenna, in order to observe or
!!     otherwise receive radiation from a target.  This pointing problem
!!     is complicated by the finite speed of light:  one needs to point
!!     to where the target appears to be as opposed to where it actually
!!     is at the epoch of observation.  We use the adjectives
!!     "geometric," "uncorrected," or "true" to refer to an actual
!!     position or state of a target at a specified epoch.  When a
!!     geometric position or state vector is modified to reflect how it
!!     appears to an observer, we describe that vector by any of the
!!     terms "apparent," "corrected," "aberration corrected," or "light
!!     time and stellar aberration corrected."
!!
!!     The SPICE Toolkit can correct for two phenomena affecting the
!!     apparent location of an object:  one-way light time (also called
!!     "planetary aberration") and stellar aberration.  Correcting for
!!     one-way light time is done by computing, given an observer and
!!     observation epoch, where a target was when the observed photons
!!     departed the target's location.  The vector from the observer to
!!     this computed target location is called a "light time corrected"
!!     vector.  The light time correction depends on the motion of the
!!     target, but it is independent of the velocity of the observer
!!     relative to the solar system barycenter. Relativistic effects
!!     such as light bending and gravitational delay are not accounted
!!     for in the light time correction performed by this routine.
!!
!!     The velocity of the observer also affects the apparent location
!!     of a target:  photons arriving at the observer are subject to a
!!     "raindrop effect" whereby their velocity relative to the observer
!!     is, using a Newtonian approximation, the photons' velocity
!!     relative to the solar system barycenter minus the velocity of the
!!     observer relative to the solar system barycenter.  This effect is
!!     called "stellar aberration."  Stellar aberration is independent
!!     of the motion of the target.  The stellar aberration formula used
!!     by this routine is non- relativistic.
!!
!!     Stellar aberration corrections are applied after light time
!!     corrections:  the light time corrected target position vector is
!!     used as an input to the stellar aberration correction.
!!
!!     When light time and stellar aberration corrections are both
!!     applied to a geometric position vector, the resulting position
!!     vector indicates where the target "appears to be" from the
!!     observer's location.
!!
!!     As opposed to computing the apparent position of a target, one
!!     may wish to compute the pointing direction required for
!!     transmission of photons to the target.  This requires correction
!!     of the geometric target position for the effects of light time and
!!     stellar aberration, but in this case the corrections are computed
!!     for radiation traveling from the observer to the target.
!!
!!     The "transmission" light time correction yields the target's
!!     location as it will be when photons emitted from the observer's
!!     location at ET arrive at the target.  The transmission stellar
!!     aberration correction is the inverse of the traditional stellar
!!     aberration correction:  it indicates the direction in which
!!     radiation should be emitted so that, using a Newtonian
!!     approximation, the sum of the velocity of the radiation relative
!!     to the observer and of the observer's velocity, relative to the
!!     solar system barycenter, yields a velocity vector that points in
!!     the direction of the light time corrected position of the target.
!!
!!     The traditional aberration corrections applicable to observation
!!     and those applicable to transmission are related in a simple way:
!!     one may picture the geometry of the "transmission" case by
!!     imagining the "observation" case running in reverse time order,
!!     and vice versa.
!!
!!     One may reasonably object to using the term "observer" in the
!!     transmission case, in which radiation is emitted from the
!!     observer's location.  The terminology was retained for
!!     consistency with earlier documentation.
!!
!!     Below, we indicate the aberration corrections to use for some
!!     common applications:
!!
!!        1) Find the apparent direction of a target for a remote-sensing
!!           observation.
!!
!!              Use 'LT+S' or 'CN+S: apply both light time and stellar
!!              aberration corrections.
!!
!!           Note that using light time corrections alone ('LT' or 'CN')
!!           is generally not a good way to obtain an approximation to
!!           an apparent target vector: since light time and stellar
!!           aberration corrections often partially cancel each other,
!!           it may be more accurate to use no correction at all than to
!!           use light time alone.
!!
!!        2) Find the corrected pointing direction to radiate a signal
!!           to a target. This computation is often applicable for
!!           implementing communications sessions.
!!
!!              Use 'XLT+S' or 'XCN+S: apply both light time and stellar
!!              aberration corrections for transmission.
!!
!!        3) Compute the apparent position of a target body relative
!!           to a star or other distant object.
!!
!!              Use 'LT', 'CN', 'LT+S', or 'CN+S' as needed to match the
!!              correction applied to the position of the distant
!!              object. For example, if a star position is obtained from
!!              a catalog, the position vector may not be corrected for
!!              stellar aberration. In this case, to find the angular
!!              separation of the star and the limb of a planet, the
!!              vector from the observer to the planet should be
!!              corrected for light time but not stellar aberration.
!!
!!        4) Obtain an uncorrected state vector derived directly from
!!           data in an SPK file.
!!
!!              Use 'NONE'.
!!
!!        5) Use a geometric position vector as a low-accuracy estimate
!!           of the apparent position for an application where execution
!!           speed is critical:
!!
!!              Use 'NONE'.
!!
!!        6) While this routine cannot perform the relativistic
!!           aberration corrections required to compute positions
!!           with the highest possible accuracy, it can supply the
!!           geometric positions required as inputs to these
!!           computations:
!!
!!              Use 'NONE', then apply high-accuracy aberration
!!              corrections (not available in the SPICE Toolkit).
!!
!!     Below, we discuss in more detail how the aberration corrections
!!     applied by this routine are computed.
!!
!!     Geometric case
!!     ==============
!!
!!        SPKAPO begins by computing the geometric position T(ET) of the
!!        target body relative to the solar system barycenter (SSB).
!!        Subtracting the geometric position of the observer O(ET) gives
!!        the geometric position of the target body relative to the
!!        observer. The one-way light time, LT, is given by
!!
!!                  | T(ET) - O(ET) |
!!           LT = -------------------
!!                          c
!!
!!        The geometric relationship between the observer, target, and
!!        solar system barycenter is as shown:
!!
!!           SSB ---> O(ET)
!!            |      /
!!            |     /
!!            |    /
!!            |   /  T(ET) - O(ET)
!!            V  V
!!           T(ET)
!!
!!        The returned position vector is
!!
!!           T(ET) - O(ET)
!!
!!     Reception case
!!     ==============
!!
!!        When any of the options 'LT', 'CN', 'LT+S', 'CN+S' are
!!        selected, SPKAPO computes the position of the target body at
!!        epoch ET-LT, where LT is the one-way light time.  Let T(t)
!!        and O(t) represent the positions of the target and observer
!!        relative to the solar system barycenter at time t; then LT
!!        is the solution of the
!!        light-time equation
!!
!!                  | T(ET-LT) - O(ET) |
!!           LT = ------------------------                            (1)
!!                           c
!!
!!        The ratio
!!
!!            | T(ET) - O(ET) |
!!          ---------------------                                     (2)
!!                    c
!!
!!        is used as a first approximation to LT; inserting (2) into the
!!        RHS of the light-time equation (1) yields the "one-iteration"
!!        estimate of the one-way light time. Repeating the process
!!        until the estimates of LT converge yields the "converged
!!        Newtonian" light time estimate.
!!
!!        Subtracting the geometric position of the observer O(ET) gives
!!        the position of the target body relative to the observer:
!!        T(ET-LT) - O(ET).
!!
!!           SSB ---> O(ET)
!!            | \     |
!!            |  \    |
!!            |   \   | T(ET-LT) - O(ET)
!!            |    \  |
!!            V     V V
!!           T(ET)  T(ET-LT)
!!
!!        The light-time corrected position is the vector
!!
!!           T(ET-LT) - O(ET)
!!
!!        If correction for stellar aberration is requested, the target
!!        position is rotated toward the solar system barycenter-relative
!!        velocity vector of the observer. The magnitude of the rotation
!!        depends on the magnitude of the observer's velocity relative
!!        to the solar system barycenter and the angle between
!!        this velocity and the observer-target vector.  The rotation
!!        is computed as follows:
!!
!!           Let r be the light time corrected vector from the observer
!!           to the object, and v be the velocity of the observer with
!!           respect to the solar system barycenter. Let w be the angle
!!           between them. The aberration angle phi is given by
!!
!!              sin(phi) = v sin(w) / c
!!
!!           Let h be the vector given by the cross product
!!
!!              h = r X v
!!
!!           Rotate r by phi radians about h to obtain the apparent
!!           position of the object.
!!
!!     Transmission case
!!     ==================
!!
!!        When any of the options 'XLT', 'XCN', 'XLT+S', 'XCN+S' are
!!        selected, SPKAPO computes the position of the target body T at
!!        epoch ET+LT, where LT is the one-way light time.  LT is the
!!        solution of the light-time equation
!!
!!                  | T(ET+LT) - O(ET) |
!!           LT = ------------------------                            (3)
!!                            c
!!
!!        Subtracting the geometric position of the observer, O(ET),
!!        gives the position of the target body relative to the
!!        observer: T(ET-LT) - O(ET).
!!
!!                   SSB --> O(ET)
!!                  / |    *
!!                 /  |  *  T(ET+LT) - O(ET)
!!                /   |*
!!               /   *|
!!              V  V  V
!!          T(ET+LT)  T(ET)
!!
!!        The light-time corrected position is
!!
!!           T(ET+LT) - O(ET)
!!
!!        If correction for stellar aberration is requested, the target
!!        position is rotated away from the solar system barycenter-
!!        relative velocity vector of the observer.  The magnitude of the
!!        rotation depends on the magnitude of the velocity and the
!!        angle between the velocity and the observer-target vector.
!!        The rotation is computed as in the reception case, but the
!!        sign of the rotation angle is negated.
!!
!!     Neither special nor general relativistic effects are accounted
!!     for in the aberration corrections performed by this routine.
!!
!! ## Examples
!!
!!     In the following code fragment, SPKSSB and SPKAPO are used
!!     to display the position of Io (body 501) as seen from the
!!     Voyager 2 spacecraft (Body -32) at a series of epochs.
!!
!!     Normally, one would call the high-level reader SPKPOS to obtain
!!     position vectors.  The example below illustrates the interface
!!     of this routine, but is not intended as a recommendation on
!!     how to use the SPICE SPK subsystem.
!!
!!     The use of integer ID codes is necessitated by the low-level
!!     interface of this routine.
!!
!!        IO    = 501
!!        VGR2  = -32
!!
!!        DO WHILE ( EPOCH .LE. END )
!!
!!           CALL SPKSSB (  VGR2,  EPOCH, 'J2000', STVGR2  )
!!           CALL SPKAPO (  IO,    EPOCH, 'J2000', STVGR2,
!!       .                 'LT+S', STIO,   LT              )
!!
!!           CALL RECRAD (  STIO,  RANGE,  RA,     DEC     )
!!           WRITE (*,*)  RA * DPR(),  DEC * DPR()
!!
!!           EPOCH = EPOCH + DELTA
!!
!!        END DO
!!
!! ## Restrictions
!!
!!     1) The ephemeris files to be used by SPKAPO must be loaded
!!        (normally by the SPICELIB kernel loader FURNSH) before
!!        this routine is called.
!!
!!     2) Unlike most other SPK position computation routines, this
!!        routine requires that the input state be relative to an
!!        inertial reference frame.  Non-inertial frames are not
!!        supported by this routine.
!!
!!     3) In a future version of this routine, the implementation
!!        of the aberration corrections may be enhanced to improve
!!        accuracy.
!!
!! ## Literature References
!!
!!     SPK Required Reading.
!!
!! ## Author and Institution
!!
!!     N.J. Bachman    (JPL)
!!     H.A. Neilan     (JPL)
!!     B.V. Semenov    (JPL)
!!     W.L. Taber      (JPL)
!!     I.M. Underwood  (JPL)
!!
!! ## Version
!!
!!-    SPICELIB Version 2.3.0, 03-JUL-2014 (NJB) (BVS)
!!
!!        Discussion of light time corrections was updated. Assertions
!!        that converged light time corrections are unlikely to be
!!        useful were removed.
!!
!!     Last update was 21-SEP-2013 (BVS)
!!
!!        Updated to call LJUCRS instead of CMPRSS/UCASE.
!!
!!-    SPICELIB Version 2.2.0, 17-MAY-2010 (NJB)
!!
!!        Bug fix: routine now returns immediately after
!!        state lookup failure.
!!
!!-    SPICELIB Version 2.1.0, 31-AUG-2005 (NJB)
!!
!!        Updated to remove non-standard use of duplicate arguments
!!        in VSUB call.
!!
!!-    SPICELIB Version 2.0.1, 20-OCT-2003 (EDW)
!!
!!        Added mention that LT returns in seconds.
!!        Corrected spelling errors.
!!
!!-    SPICELIB Version 2.0.0, 18-DEC-2001 (NJB)
!!
!!        Updated to handle aberration corrections for transmission
!!        of radiation.  Formerly, only the reception case was
!!        supported.  The header was revised and expanded to explain
!!        the functionality of this routine in more detail.
!!
!!-    SPICELIB Version 1.0.0, 03-MAR-1999 (WLT)
!!
!! ## Index Entries
!!
!!     apparent position from spk file
!!     get apparent position
!!
!! ## Revisions
!!
!!-    SPICELIB Version 1.1.0, 31-AUG-2005 (NJB)
!!
!!        Updated to remove non-standard use of duplicate arguments
!!        in VSUB call.
!!
!!     SPICELIB functions
!!
!!     Local parameters
!!
!!     Indices of flags in the FLAGS array:
!!
!!     NAIF ID code for the solar system barycenter:
!!
!!     Local variables
!!
!!     Saved variables
!!
!!     Initial values
!!
!!     Standard SPICE error handling.
!!
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: clight
      use pepper_arrays, only: isrchc
      use pepper_math, only: odd, vequ, vnorm, vsub
      use pepper_strings, only: ljucrs
      implicit none
      integer(ip), parameter :: corlen = 5
      integer(ip), parameter :: nflags = 9
      integer(ip), parameter :: ixnone = 1
      integer(ip), parameter :: ixlt = ixnone + 1
      integer(ip), parameter :: ixlts = ixlt   + 1
      integer(ip), parameter :: ixcn = ixlts  + 1
      integer(ip), parameter :: ixcns = ixcn   + 1
      integer(ip), parameter :: ixxlt = ixcns  + 1
      integer(ip), parameter :: ixxlts = ixxlt  + 1
      integer(ip), parameter :: ixxcn = ixxlts + 1
      integer(ip), parameter :: ixxcns = ixxcn  + 1
      integer(ip), parameter :: ssb = 0
      character(corlen)     corr
      real(wp)              tpos   ( 3 )
      integer(ip)           i
      integer(ip)           ltsign
      integer(ip)           maxitr
      integer(ip)           refid
      if ( .not. allocated ( ctx%zzspkpa1_state ) ) then
         allocate ( ctx%zzspkpa1_state )
         call init_zzspkpa1_state ( ctx%zzspkpa1_state )
      end if
      associate ( first => ctx%zzspkpa1_state%first, &
                  flags => ctx%zzspkpa1_state%flags, &
                  prvcor => ctx%zzspkpa1_state%prvcor, &
                  usecn => ctx%zzspkpa1_state%usecn, &
                  usestl => ctx%zzspkpa1_state%usestl, &
                  uselt => ctx%zzspkpa1_state%uselt, &
                  xmit => ctx%zzspkpa1_state%xmit )
      if ( ctx%failed() ) return

      if (  first  .or.  ( abcorr /= prvcor )  ) then
!
!        The aberration correction flag differs from the value it
!        had on the previous call, if any.  Analyze the new flag.
!
!        Remove leading and embedded white space from the aberration
!        correction flag, then convert to upper case.
!
         call ljucrs ( 0, abcorr, corr )

!
!        Locate the flag in our list of flags.
!
         i = isrchc ( corr, nflags, flags )

         if ( i == 0 ) then

            call ctx%setmsg ( 'Requested aberration correction was #.' )
            call ctx%errch  ( '#', abcorr                              )
            call ctx%sigerr ( 'SPICE(SPKINVALIDOPTION)'                )
            return

         end if

!
!        The aberration correction flag is recognized; save it.
!
         prvcor = abcorr

!
!        Set logical flags indicating the attributes of the requested
!        correction.
!
         xmit    =  i > ixcns

         uselt   =       ( i == ixlt  ) .or. ( i == ixlts  ) &
                    .or. ( i == ixxlt ) .or. ( i == ixxlts )

         usestl  =  ( i > 1 )  .and.  odd(i)

         usecn   =       ( i == ixcn  ) .or. ( i == ixcns  ) &
                    .or. ( i == ixxcn ) .or. ( i == ixxcns )

         first   = .false.

      end if

!
!     See if the reference frame is a recognized inertial frame.
!
      call irfnum ( ctx, ref, refid )

      if ( refid == 0 ) then

         call ctx%setmsg ( 'The requested frame ''#'' is not a ' &
         //            'recognized inertial frame. '        )
         call ctx%errch  ( '#', ref                             )
         call ctx%sigerr ( 'SPICE(BADFRAME)'                    )
         return

      end if

!
!     Determine the sign of the light time offset.
!
      if ( xmit ) then
         ltsign = 1
      else
         ltsign = -1
      end if

!
!     Find the geometric position of the target body with respect to the
!     solar system barycenter. Subtract the position of the observer
!     to get the relative position. Use this to compute the one-way
!     light time.
!
      call zzspkgp1 ( ctx, targ,   et,   ref, ssb, ptarg, lt )

      if ( ctx%failed() ) return

      call vsub   ( ptarg,  sobs,           tpos      )
      call vequ   ( tpos,   ptarg                     )
      lt = vnorm  ( ptarg ) / clight

!
!     To correct for light time, find the position of the target body
!     at the current epoch minus the one-way light time. Note that
!     the observer remains where he is.
!
      if ( uselt ) then
         maxitr = 1
      else if ( usecn ) then
         maxitr = 3
      else
         maxitr = 0
      end if

      do i = 1, maxitr

         call zzspkgp1 ( ctx, targ,   et + ltsign*lt, ref, ssb, ptarg, lt )

         if ( ctx%failed() ) return

         call vsub   ( ptarg,  sobs,     tpos )
         call vequ   ( tpos,   ptarg )
         lt = vnorm  ( ptarg ) / clight

      end do

!
!     At this point, PTARG contains the geometric or light-time
!     corrected position of the target relative to the observer,
!     depending on the specified correction.
!
!     If stellar aberration correction is requested, perform it now.
!
      if ( usestl ) then

         if ( xmit ) then
!
!           This is the transmission case.
!
!           Compute the position vector obtained by applying
!           "reception" stellar aberration to PTARG.
!
            call stlabx ( ctx, ptarg, sobs(4), tpos )
            call vequ   ( tpos,  ptarg )

         else
!
!           This is the reception case.
!
!           Compute the position vector obtained by applying
!           "reception" stellar aberration to PTARG.
!
            call stelab ( ctx, ptarg, sobs(4), tpos )
            call vequ   ( tpos,  ptarg )

         end if

      end if

      return
      end associate
      end procedure zzspkpa1


!$Procedure ZZSPKSB1 ( S/P Kernel, solar system barycenter )

      module procedure zzspksb1
!! ## Abstract
!!
!!     SPICE Private routine intended solely for the support of SPICE
!!     routines.  Users should not call this routine directly due
!!     to the volatile nature of this routine.
!!
!!     Return the state (position and velocity) of a target body
!!     relative to the solar system barycenter.
!!
!! ## Required Reading
!!
!!     SPK
!!
!! ## Keywords
!!
!!     EPHEMERIS
!!
!! ## Declarations
!!
      use pepper_kinds, only: ip, wp
      implicit none

! $ Detailed_Input
!
!     TARG        is the standard NAIF ID code for a target body.
!
!     ET          is the epoch (ephemeris time) at which the state
!                 of the target body is to be computed.
!
!     REF         is the name of the  reference frame to which the
!                 vectors returned by the routine should be rotated.
!                 This may be any frame supported by the SPICELIB frame
!                 system, including dynamic and other non-inertial
!                 frames.
!
! $ Detailed_Output
!
!     STARG       contains the position and velocity of the target
!                 body, relative to the solar system barycenter,
!                 at epoch ET. These vectors are rotated into the
!                 specified  reference frame. Units are always
!                 km and km/sec.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) If insufficient information has not bee "loaded" via the
!        routine SPKLEF or the PCK kernel loaders, the problem will
!        be diagnosed by a routine in the call tree of this routine.
!
! $ Files
!
!     See: $Restrictions.
!
! $ Particulars
!
!     In order to compute the state of one body relative to another,
!     the states of the two bodies must be known relative to a third
!     body. One simple solution is to use the solar system barycenter
!     as the third body.
!
!     Ephemeris data from more than one segment may be required
!     to determine the state of a body relative to the barycenter.
!     ZZSPKSB1 reads as many segments as necessary, from as many
!     files as necessary, using files that have been loaded by
!     previous calls to SPKLEF (load ephemeris file).
!
! $ Examples
!
!     In the following code fragment, ZZSPKSB1 is used to display
!     the distance from Earth (Body 399) to Mars (body 499) at
!     a series of epochs.
!
!        CALL SPKLEF ( 'DE125.SPK', HANDLE )
!         .
!         .
!
!        EARTH = 399
!        MARS  = 499
!
!        DO WHILE ( EPOCH .LE. END )
!           CALL ZZSPKSB1 ( EARTH, EPOCH, 'J2000', SEARTH )
!           CALL ZZSPKSB1 ( MARS,  EPOCH, 'J2000', SMARS  )
!
!           CALL VSUB ( SMARS, SEARTH, SMARS )
!           WRITE (*,*) EPOCH, VNORM ( SMARS )
!
!           EPOCH = EPOCH + DELTA
!        END DO
!
! $ Restrictions
!
!     1) SPICE Private routine.
!
!     2) The ephemeris files to be used by ZZSPKSB1 must be loaded
!        by SPKLEF before ZZSPKSB1 is called.
!
! $ Literature_References
!
!     NAIF Document 168.0, "S- and P- Kernel (SPK) Specification and
!     User's Guide"
!
! $ Author_and_Institution
!
!     W.L. Taber      (JPL)
!     I.M. Underwood  (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 12-DEC-2004 (NJB)
!
!        Based on SPICELIB Version 2.0.2, 20-NOV-2004  (NJB)
!
!-&

! $ Index_Entries
!
!     state relative to solar system barycenter
!
!-&

! $ Revisions
!
!-    SPICELIB Version 2.0.0, 19-SEP-1995  (WLT)
!
!        The routine was simplified by replacing all of the
!        main body of code with a call to SPKGEO.  By making
!        this change the routine now supports non-inertial frames.
!
!-&

!
!     SPICELIB functions
!

!
!     Local variables
!
      real(wp)              lt
      integer(ip)           bary

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

      bary = 0

      call zzspkgo1 ( ctx, targ, et, ref, bary, starg, lt )

      return
      end procedure zzspksb1

      module procedure zzspkas1
!! ## Abstract
!!
!!     Given the state and acceleration of an observer relative to the
!!     solar system barycenter, return the state (position and velocity)
!!     of a target body relative to the observer, optionally corrected
!!     for light time and stellar aberration. All input and output
!!     vectors are expressed relative to an inertial reference frame.
!!
!!     This routine supersedes SPKAPP.
!!
!!     SPICE users normally should call the high-level API routines
!!     SPKEZR or SPKEZ rather than this routine.
!!
!! ## Required Reading
!!
!!     SPK
!!
!! ## Keywords
!!
!!     EPHEMERIS
!!
!! ## Declarations
!!
!! ## Detailed Input
!!
!!     TARG        is the NAIF ID code for a target body.  The target
!!                 and observer define a state vector whose position
!!                 component points from the observer to the target.
!!
!!     ET          is the ephemeris time, expressed as seconds past
!!                 J2000 TDB, at which the state of the target body
!!                 relative to the observer is to be computed.  ET
!!                 refers to time at the observer's location.
!!
!!     REF         is the inertial reference frame with respect to which
!!                 the input state STOBS, the input acceleration ACCOBS,
!!                 and the output state STARG are expressed. REF must be
!!                 recognized by the SPICE Toolkit.  The acceptable
!!                 frames are listed in the Frames Required Reading, as
!!                 well as in the SPICELIB routine CHGIRF.
!!
!!                 Case and blanks are not significant in the string
!!                 REF.
!!
!!     ABCORR      indicates the aberration corrections to be applied
!!                 to the state of the target body to account for one-way
!!                 light time and stellar aberration. See the discussion
!!                 in the header of SPKEZR for recommendations on
!!                 how to choose aberration corrections.
!!
!!                 ABCORR may be any of the following:
!!
!!                    'NONE'     Apply no correction. Return the
!!                               geometric state of the target body
!!                               relative to the observer.
!!
!!                 The following values of ABCORR apply to the
!!                 "reception" case in which photons depart from the
!!                 target's location at the light-time corrected epoch
!!                 ET-LT and *arrive* at the observer's location at ET:
!!
!!                    'LT'       Correct for one-way light time (also
!!                               called "planetary aberration") using a
!!                               Newtonian formulation. This correction
!!                               yields the state of the target at the
!!                               moment it emitted photons arriving at
!!                               the observer at ET.
!!
!!                               The light time correction uses an
!!                               iterative solution of the light time
!!                               equation (see Particulars for details).
!!                               The solution invoked by the 'LT' option
!!                               uses one iteration.
!!
!!                    'LT+S'     Correct for one-way light time and
!!                               stellar aberration using a Newtonian
!!                               formulation. This option modifies the
!!                               state obtained with the 'LT' option to
!!                               account for the observer's velocity
!!                               relative to the solar system
!!                               barycenter. The result is the apparent
!!                               state of the target---the position and
!!                               velocity of the target as seen by the
!!                               observer.
!!
!!                    'CN'       Converged Newtonian light time
!!                               correction. In solving the light time
!!                               equation, the 'CN' correction iterates
!!                               until the solution converges (three
!!                               iterations on all supported platforms).
!!                               Whether the 'CN+S' solution is
!!                               substantially more accurate than the
!!                               'LT' solution depends on the geometry
!!                               of the participating objects and on the
!!                               accuracy of the input data. In all
!!                               cases this routine will execute more
!!                               slowly when a converged solution is
!!                               computed. See the Particulars section of
!!                               SPKEZR for a discussion of precision of
!!                               light time corrections.
!!
!!                    'CN+S'     Converged Newtonian light time
!!                               correction and stellar aberration
!!                               correction.
!!
!!                 The following values of ABCORR apply to the
!!                 "transmission" case in which photons *depart* from
!!                 the observer's location at ET and arrive at the
!!                 target's location at the light-time corrected epoch
!!                 ET+LT:
!!
!!                    'XLT'      "Transmission" case:  correct for
!!                               one-way light time using a Newtonian
!!                               formulation. This correction yields the
!!                               state of the target at the moment it
!!                               receives photons emitted from the
!!                               observer's location at ET.
!!
!!                    'XLT+S'    "Transmission" case:  correct for
!!                               one-way light time and stellar
!!                               aberration using a Newtonian
!!                               formulation  This option modifies the
!!                               state obtained with the 'XLT' option to
!!                               account for the observer's velocity
!!                               relative to the solar system
!!                               barycenter. The position component of
!!                               the computed target state indicates the
!!                               direction that photons emitted from the
!!                               observer's location must be "aimed" to
!!                               hit the target.
!!
!!                    'XCN'      "Transmission" case:  converged
!!                               Newtonian light time correction.
!!
!!                    'XCN+S'    "Transmission" case:  converged
!!                               Newtonian light time correction and
!!                               stellar aberration correction.
!!
!!                 Neither special nor general relativistic effects are
!!                 accounted for in the aberration corrections applied
!!                 by this routine.
!!
!!                 Case and blanks are not significant in the string
!!                 ABCORR.
!!
!!     STOBS       is the geometric state of the observer relative to
!!                 the solar system barycenter at ET. STOBS is expressed
!!                 relative to the reference frame designated by REF.
!!                 The target and observer define a state vector whose
!!                 position component points from the observer to the
!!                 target.
!!
!!     ACCOBS      is the geometric acceleration of the observer
!!                 relative to the solar system barycenter at ET. This
!!                 is the derivative with respect to time of the
!!                 velocity portion of STOBS. ACCOBS is expressed
!!                 relative to the reference frame designated by REF.
!!
!!                 ACCOBS is used for computing stellar aberration
!!                 corrected velocity. If stellar aberration corrections
!!                 are not specified by ABCORR, ACCOBS is ignored; the
!!                 caller need not provide a valid input value in this
!!                 case.
!!
!! ## Detailed Output
!!
!!     STARG       is a Cartesian state vector representing the position
!!                 and velocity of the target body relative to the
!!                 specified observer. STARG is corrected for the
!!                 specified aberrations, and is expressed with respect
!!                 to the inertial reference frame designated by REF.
!!                 The first three components of STARG represent the x-,
!!                 y- and z-components of the target's position; last
!!                 three components form the corresponding velocity
!!                 vector.
!!
!!                 The position component of STARG points from the
!!                 observer's location at ET to the aberration-corrected
!!                 location of the target. Note that the sense of the
!!                 position vector is independent of the direction of
!!                 radiation travel implied by the aberration
!!                 correction.
!!
!!                 Units are always km and km/sec.
!!
!!     LT          is the one-way light time between the observer and
!!                 target in seconds.  If the target state is corrected
!!                 for light time, then LT is the one-way light time
!!                 between the observer and the light time-corrected
!!                 target location.
!!
!!     DLT         is the derivative with respect to barycentric
!!                 dynamical time of the one way light time between
!!                 target and observer:
!!
!!                    DLT = d(LT)/d(ET)
!!
!!                 DLT can also be described as the rate of change of
!!                 one way light time. DLT is unitless, since LT and
!!                 ET both have units of TDB seconds.
!!
!!                 If the observer and target are at the same position,
!!                 then DLT is set to zero.
!!
!! ## Parameters
!!
!!     None.
!!
!! ## Exceptions
!!
!!     1) If the value of ABCORR is not recognized, the error
!!        is diagnosed by a routine in the call tree of this
!!        routine.
!!
!!     2) If ABCORR calls for stellar aberration but not light
!!        time corrections, the error SPICE(NOTSUPPORTED) is
!!        signaled.
!!
!!     3) If ABCORR calls for relativistic light time corrections, the
!!        error SPICE(NOTSUPPORTED) is signaled.
!!
!!     4) If the reference frame requested is not a recognized
!!        inertial reference frame, the error SPICE(BADFRAME)
!!        is signaled.
!!
!!     5) If the state of the target relative to the solar system
!!        barycenter cannot be computed, the error will be diagnosed
!!        by routines in the call tree of this routine.
!!
!!     6) If the observer and target are at the same position,
!!        then DLT is set to zero. This situation could arise,
!!        for example, when the observer is Mars and the target
!!        is the Mars barycenter.
!!
!! ## Files
!!
!!     This routine computes states using SPK files that have been
!!     loaded into the SPICE system, normally via the kernel loading
!!     interface routine FURNSH.  Application programs typically load
!!     kernels once before this routine is called, for example during
!!     program initialization; kernels need not be loaded repeatedly.
!!     See the routine FURNSH and the SPK and KERNEL Required Reading
!!     for further information on loading (and unloading) kernels.
!!
!!     If any of the ephemeris data used to compute STARG are expressed
!!     relative to a non-inertial frame in the SPK files providing those
!!     data, additional kernels may be needed to enable the reference
!!     frame transformations required to compute the state. Normally
!!     these additional kernels are PCK files or frame kernels. Any such
!!     kernels must already be loaded at the time this routine is
!!     called.
!!
!! ## Particulars
!!
!!     This routine supports higher-level SPK API routines that can
!!     perform both light time and stellar aberration corrections.
!!
!!     User applications normally will not need to call this routine
!!     directly. However, this routine can improve run-time efficiency
!!     in situations where many targets are observed from the same
!!     location at the same time. In such cases, the state and
!!     acceleration of the observer relative to the solar system
!!     barycenter need be computed only once per look-up epoch.
!!
!!     When apparent positions, rather than apparent states, are
!!     required, consider using the high-level position-only API
!!     routines
!!
!!        SPKPOS
!!        SPKEZP
!!
!!     or the low-level, position-only analog of this routine
!!
!!        SPKAPO
!!
!!     In general, the position-only routines are more efficient.
!!
!!     See the header of the routine SPKEZR for a detailed discussion
!!     of aberration corrections.
!!
!! ## Examples
!!
!!    1) Look up a sequence of states of the Moon as seen from the
!!       Earth. Use light time and stellar aberration corrections.
!!       Compute the first state for the epoch 2000 JAN 1 12:00:00 TDB;
!!       compute subsequent states at intervals of 1 hour. For each
!!       epoch, display the states, the one way light time between
!!       target and observer, and the rate of change of the one way
!!       light time.
!!
!!       Use the following meta-kernel to specify the kernels to
!!       load:
!!
!!          KPL/MK
!!
!!          This meta-kernel is intended to support operation of SPICE
!!          example programs. The kernels shown here should not be
!!          assumed to contain adequate or correct versions of data
!!          required by SPICE-based user applications.
!!
!!          In order for an application to use this meta-kernel, the
!!          kernels referenced here must be present in the user's
!!          current working directory.
!!
!!          \begindata
!!
!!             KERNELS_TO_LOAD = ( 'de418.bsp',
!!                                 'pck00008.tpc',
!!                                 'naif0008.tls'  )
!!
!!          \begintext
!!
!!       The code example follows:
!!
!!           PROGRAM EX1
!!           IMPLICIT NONE
!!     C
!!     C     Local constants
!!     C
!!     C     The meta-kernel name shown here refers to a file whose
!!     C     contents are those shown above. This file and the kernels
!!     C     it references must exist in your current working directory.
!!     C
!!           CHARACTER*(*)         META
!!           PARAMETER           ( META   = 'example.mk' )
!!     C
!!     C     Use a time step of 1 hour; look up 5 states.
!!     C
!!           DOUBLE PRECISION      STEP
!!           PARAMETER           ( STEP   = 3600.0D0 )
!!
!!           INTEGER               MAXITR
!!           PARAMETER           ( MAXITR = 5 )
!!     C
!!     C     Local variables
!!     C
!!           DOUBLE PRECISION      ACC    ( 3 )
!!           DOUBLE PRECISION      DLT
!!           DOUBLE PRECISION      ET
!!           DOUBLE PRECISION      ET0
!!           DOUBLE PRECISION      LT
!!           DOUBLE PRECISION      STATE  ( 6 )
!!           DOUBLE PRECISION      STATE0 ( 6 )
!!           DOUBLE PRECISION      STATE2 ( 6 )
!!           DOUBLE PRECISION      STOBS  ( 6 )
!!           DOUBLE PRECISION      TDELTA
!!           INTEGER               I
!!
!!     C
!!     C     Load the SPK and LSK kernels via the meta-kernel.
!!     C
!!           CALL FURNSH ( META )
!!     C
!!     C     Convert the start time to seconds past J2000 TDB.
!!     C
!!           CALL STR2ET ( '2000 JAN 1 12:00:00 TDB', ET0 )
!!     C
!!     C     Step through a series of epochs, looking up a
!!     C     state vector at each one.
!!     C
!!           DO I = 1, MAXITR
!!
!!              ET = ET0 + (I-1)*STEP
!!
!!     C
!!     C        Look up a state vector at epoch ET using the
!!     C        following inputs:
!!     C
!!     C           Target:                 Moon (NAIF ID code 301)
!!     C           Reference frame:        J2000
!!     C           Aberration correction:  Light time and stellar
!!     C                                   aberration ('LT+S')
!!     C           Observer:               Earth (NAIF ID code 399)
!!     C
!!     C        Before we can execute this computation, we'll need the
!!     C        geometric state and acceleration of the observer
!!     C        relative to the solar system barycenter at ET, expressed
!!     C        relative to the J2000 reference frame. First find
!!     C        the state:
!!     C
!!              CALL SPKSSB ( 399, ET, 'J2000', STOBS )
!!     C
!!     C        Next compute the acceleration. We numerically
!!     C        differentiate the velocity using a quadratic
!!     C        approximation:
!!     C
!!              TDELTA = 1.D0
!!
!!              CALL SPKSSB ( 399, ET-TDELTA, 'J2000', STATE0 )
!!              CALL SPKSSB ( 399, ET+TDELTA, 'J2000', STATE2 )
!!
!!              CALL QDERIV ( 3, STATE0(4), STATE2(4), TDELTA, ACC )
!!     C
!!     C        Now compute the desired state vector:
!!     C
!!              CALL SPKAPS ( 301,   ET,  'J2000', 'LT+S',
!!          .                 STOBS, ACC, STATE,    LT,   DLT )
!!
!!              WRITE (*,*) 'ET = ', ET
!!              WRITE (*,*) 'J2000 x-position (km):   ', STATE(1)
!!              WRITE (*,*) 'J2000 y-position (km):   ', STATE(2)
!!              WRITE (*,*) 'J2000 z-position (km):   ', STATE(3)
!!              WRITE (*,*) 'J2000 x-velocity (km/s): ', STATE(4)
!!              WRITE (*,*) 'J2000 y-velocity (km/s): ', STATE(5)
!!              WRITE (*,*) 'J2000 z-velocity (km/s): ', STATE(6)
!!              WRITE (*,*) 'One-way light time (s):  ', LT
!!              WRITE (*,*) 'Light time rate:         ', DLT
!!              WRITE (*,*) ' '
!!
!!           END DO
!!
!!           END
!!
!!     The output produced by this program will vary somewhat as
!!     a function of the platform on which the program is built and
!!     executed. On a PC/Linux/g77 platform, the following output
!!     was produced:
!!
!!        ET =   0.
!!        J2000 x-position (km):    -291584.614
!!        J2000 y-position (km):    -266693.406
!!        J2000 z-position (km):    -76095.6532
!!        J2000 x-velocity (km/s):   0.643439157
!!        J2000 y-velocity (km/s):  -0.666065874
!!        J2000 z-velocity (km/s):  -0.301310063
!!        One-way light time (s):    1.34231061
!!        Light time rate:           1.07316909E-07
!!
!!        ET =   3600.
!!        J2000 x-position (km):    -289256.459
!!        J2000 y-position (km):    -269080.605
!!        J2000 z-position (km):    -77177.3528
!!        J2000 x-velocity (km/s):   0.64997032
!!        J2000 y-velocity (km/s):  -0.660148253
!!        J2000 z-velocity (km/s):  -0.299630418
!!        One-way light time (s):    1.34269395
!!        Light time rate:           1.05652599E-07
!!
!!        ET =   7200.
!!        J2000 x-position (km):    -286904.897
!!        J2000 y-position (km):    -271446.417
!!        J2000 z-position (km):    -78252.9655
!!        J2000 x-velocity (km/s):   0.656443883
!!        J2000 y-velocity (km/s):  -0.654183552
!!        J2000 z-velocity (km/s):  -0.297928533
!!        One-way light time (s):    1.34307131
!!        Light time rate:           1.03990457E-07
!!
!!        ET =   10800.
!!        J2000 x-position (km):    -284530.133
!!        J2000 y-position (km):    -273790.671
!!        J2000 z-position (km):    -79322.4117
!!        J2000 x-velocity (km/s):   0.662859505
!!        J2000 y-velocity (km/s):  -0.648172247
!!        J2000 z-velocity (km/s):  -0.296204558
!!        One-way light time (s):    1.34344269
!!        Light time rate:           1.02330665E-07
!!
!!        ET =   14400.
!!        J2000 x-position (km):    -282132.378
!!        J2000 y-position (km):    -276113.202
!!        J2000 z-position (km):    -80385.612
!!        J2000 x-velocity (km/s):   0.669216846
!!        J2000 y-velocity (km/s):  -0.642114815
!!        J2000 z-velocity (km/s):  -0.294458645
!!        One-way light time (s):    1.3438081
!!        Light time rate:           1.00673404E-07
!!
!! ## Restrictions
!!
!!     1) This routine should not be used to compute geometric states.
!!        Instead, use SPKEZR, SPKEZ, or SPKGEO. SPKGEO, which is called
!!        by SPKEZR and SPKEZ, introduces less round-off error when the
!!        observer and target have a common center that is closer to
!!        both objects than is the solar system barycenter.
!!
!!     2) The kernel files to be used by SPKAPS must be loaded
!!        (normally by the SPICELIB kernel loader FURNSH) before
!!        this routine is called.
!!
!!     3) Unlike most other SPK state computation routines, this
!!        routine requires that the output state be relative to an
!!        inertial reference frame.
!!
!! ## Literature References
!!
!!     SPK Required Reading.
!!
!! ## Author and Institution
!!
!!     N.J. Bachman    (JPL)
!!
!! ## Version
!!
!!-    SPICELIB Version 1.2.0, 12-SEP-2021 (NJB)
!!
!!        Bug fix: aberration correction strings are now parsed using
!!        ZZVALCOR. This improves error checking. Corrected header
!!        comment typo.
!!
!!-    SPICELIB Version 1.1.0, 04-JUL-2014 (NJB)
!!
!!        Discussion of light time corrections was updated. Assertions
!!        that converged light time corrections are unlikely to be
!!        useful were removed.
!!
!!     Last update was 15-APR-2014 (NJB)
!!
!!        Added a FAILED() call to prevent numeric problems
!!        resulting from uninitialized values.
!!
!!-    SPICELIB Version 1.0.0, 11-JAN-2008 (NJB)
!!
!! ## Index Entries
!!
!!     low-level aberration-corrected state computation
!!     low-level light time and stellar aberration correction
!!
!! ## Revisions
!!
!!     None.
!!
!!     SPICELIB functions
!!
!!     Local parameters
!!
!!     Local variables
!!
!!     Saved variables
!!
!!     Initial values
!!
!!     Standard SPICE error handling.
!!
      use pepper_kinds, only: ip, wp
      use pepper_math, only: vadd, vequ
      use pepper_inc_zzabcorr
      implicit none
      character(*), parameter :: rname = 'ZZSPKAS1'
      real(wp)              corpos ( 3 )
      real(wp)              corvel ( 3 )
      real(wp)              dpcorr ( 3 )
      real(wp)              pcorr  ( 3 )
      integer(ip)           refid
      logical               attblk ( nabcor )
      if ( .not. allocated ( ctx%zzspkas1_state ) ) then
         allocate ( ctx%zzspkas1_state )
         call init_zzspkas1_state ( ctx%zzspkas1_state )
      end if
      associate ( first => ctx%zzspkas1_state%first, &
                  prvcor => ctx%zzspkas1_state%prvcor, &
                  uselt => ctx%zzspkas1_state%uselt, &
                  usestl => ctx%zzspkas1_state%usestl, &
                  xmit => ctx%zzspkas1_state%xmit )
      if ( ctx%failed() ) return

      if (  first  .or.  ( abcorr /= prvcor )  ) then
!
!        The aberration correction flag differs from the value it
!        had on the previous call, if any.  Analyze the new flag.
!
         call zzvalcor ( ctx, abcorr, attblk )

         if ( ctx%failed() ) return

!
!        The aberration correction flag is recognized; save it.
!
         prvcor = abcorr

!
!        Set logical flags indicating the attributes of the requested
!        correction:
!
!           XMIT is .TRUE. when the correction is for transmitted
!           radiation.
!
!           USELT is .TRUE. when any type of light time correction
!           (normal or converged Newtonian) is specified.
!
!           USECN indicates converged Newtonian light time correction.
!
!        The above definitions are consistent with those used by
!        ZZVALCOR.
!
         xmit    =  attblk ( xmtidx )
         uselt   =  attblk ( ltidx  )
         usestl  =  attblk ( stlidx )

         if (  usestl  .and.  ( .not. uselt )  ) then

            call ctx%setmsg ( 'Aberration correction flag # calls for ' &
            //            'stellar aberration but not light time ' &
            //            'corrections. This combination is not ' &
            //            'expected.'                              )
            call ctx%errch  ( '#', abcorr                              )
            call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'                    )
            return

         else if ( attblk(relidx) ) then

            call ctx%setmsg ( 'Aberration correction flag # calls for ' &
            //            'relativistic light time correction.'    )
            call ctx%errch  ( '#', abcorr                              )
            call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'                    )
            return

         end if

         first   = .false.

      end if

!
!     See if the reference frame is a recognized inertial frame.
!
      call irfnum ( ctx, ref, refid )

      if ( refid == 0 ) then

         call ctx%setmsg ( 'The requested frame ''#'' is not a ' &
         //            'recognized inertial frame. '        )
         call ctx%errch  ( '#', ref                             )
         call ctx%sigerr ( 'SPICE(BADFRAME)'                    )
         return

      end if

!
!     Get the state of the target relative to the observer,
!     optionally corrected for light time.
!
      call zzspklt1 ( ctx, targ,  et,    ref, abcorr, &
                      stobs, starg, lt,  dlt     )

      if ( ctx%failed() ) return

!
!     If stellar aberration corrections are not needed, we're
!     already done.
!
      if ( .not. usestl ) return

!
!     Get the stellar aberration correction and its time derivative.
!
      call zzstelab ( ctx, xmit, accobs, stobs(4), starg, pcorr, dpcorr )

!
!     Adding the stellar aberration correction to the light
!     time-corrected target position yields the position corrected for
!     both light time and stellar aberration.
!
      call vadd ( pcorr,  starg, corpos )
      call vequ ( corpos, starg         )

!
!     Velocity is treated in an analogous manner.
!
      call vadd ( dpcorr, starg(4), corvel )
      call vequ ( corvel, starg(4)         )

      return
      end associate
      end procedure zzspkas1

      module procedure zzspkzp1
!! ## Abstract
!!
!!     Return the position of a target body relative to an observing
!!     body, optionally corrected for light time (planetary aberration)
!!     and stellar aberration.
!!
!! ## Required Reading
!!
!!     SPK
!!     NAIF_IDS
!!     FRAMES
!!     TIME
!!
!! ## Keywords
!!
!!     EPHEMERIS
!!
!! ## Declarations
!!
!! ## Detailed Input
!!
!!     TARG        is the NAIF ID code for a target body.  The target
!!                 and observer define a position vector which points
!!                 from the observer to the target.
!!
!!     ET          is the ephemeris time, expressed as seconds past
!!                 J2000 TDB, at which the position of the target body
!!                 relative to the observer is to be computed.  ET
!!                 refers to time at the observer's location.
!!
!!     REF         is the name of the reference frame relative to which
!!                 the output position vector should be expressed. This
!!                 may be any frame supported by the SPICE system,
!!                 including built-in frames (documented in the Frames
!!                 Required Reading) and frames defined by a loaded
!!                 frame kernel (FK).
!!
!!                 When REF designates a non-inertial frame, the
!!                 orientation of the frame is evaluated at an epoch
!!                 dependent on the selected aberration correction. See
!!                 the description of the output position vector PTARG
!!                 for details.
!!
!!     ABCORR      indicates the aberration corrections to be applied to
!!                 the position of the target body to account for
!!                 one-way light time and stellar aberration.  See the
!!                 discussion in the Particulars section for
!!                 recommendations on how to choose aberration
!!                 corrections.
!!
!!                 ABCORR may be any of the following:
!!
!!                    'NONE'     Apply no correction. Return the
!!                               geometric position of the target body
!!                               relative to the observer.
!!
!!                 The following values of ABCORR apply to the
!!                 "reception" case in which photons depart from the
!!                 target's location at the light-time corrected epoch
!!                 ET-LT and *arrive* at the observer's location at ET:
!!
!!                    'LT'       Correct for one-way light time (also
!!                               called "planetary aberration") using a
!!                               Newtonian formulation. This correction
!!                               yields the position of the target at
!!                               the moment it emitted photons arriving
!!                               at the observer at ET.
!!
!!                               The light time correction uses an
!!                               iterative solution of the light time
!!                               equation (see Particulars for details).
!!                               The solution invoked by the 'LT' option
!!                               uses one iteration.
!!
!!                    'LT+S'     Correct for one-way light time and
!!                               stellar aberration using a Newtonian
!!                               formulation. This option modifies the
!!                               position obtained with the 'LT' option
!!                               to account for the observer's velocity
!!                               relative to the solar system
!!                               barycenter. The result is the apparent
!!                               position of the target---the position
!!                               as seen by the observer.
!!
!!                    'CN'       Converged Newtonian light time
!!                               correction. In solving the light time
!!                               equation, the 'CN' correction iterates
!!                               until the solution converges (three
!!                               iterations on all supported platforms).
!!                               Whether the 'CN+S' solution is
!!                               substantially more accurate than the
!!                               'LT' solution depends on the geometry
!!                               of the participating objects and on the
!!                               accuracy of the input data. In all
!!                               cases this routine will execute more
!!                               slowly when a converged solution is
!!                               computed. See the Particulars section
!!                               below for a discussion of precision of
!!                               light time corrections.
!!
!!                    'CN+S'     Converged Newtonian light time
!!                               correction and stellar aberration
!!                               correction.
!!
!!                 The following values of ABCORR apply to the
!!                 "transmission" case in which photons *depart* from
!!                 the observer's location at ET and arrive at the
!!                 target's location at the light-time corrected epoch
!!                 ET+LT:
!!
!!                    'XLT'      "Transmission" case:  correct for
!!                               one-way light time using a Newtonian
!!                               formulation. This correction yields the
!!                               position of the target at the moment it
!!                               receives photons emitted from the
!!                               observer's location at ET.
!!
!!                    'XLT+S'    "Transmission" case:  correct for
!!                               one-way light time and stellar
!!                               aberration using a Newtonian
!!                               formulation. This option modifies the
!!                               position obtained with the 'XLT' option
!!                               to account for the observer's velocity
!!                               relative to the solar system
!!                               barycenter. The computed target
!!                               position indicates the direction that
!!                               photons emitted from the observer's
!!                               location must be "aimed" to hit the
!!                               target.
!!
!!                    'XCN'      "Transmission" case:  converged
!!                               Newtonian light time correction.
!!
!!                    'XCN+S'    "Transmission" case:  converged
!!                               Newtonian light time correction and
!!                               stellar aberration correction.
!!
!!                 Neither special nor general relativistic effects are
!!                 accounted for in the aberration corrections applied
!!                 by this routine.
!!
!!                 Case and blanks are not significant in the string
!!                 ABCORR.
!!
!!     OBS         is the NAIF ID code for the observing body.
!!
!! ## Detailed Output
!!
!!     PTARG       is a Cartesian 3-vector representing the position of
!!                 the target body relative to the specified observer.
!!                 PTARG is corrected for the specified aberrations, and
!!                 is expressed with respect to the reference frame
!!                 specified by REF.  The three components of PTARG
!!                 represent the x-, y- and z-components of the target's
!!                 position.
!!
!!                 PTARG points from the observer's location at ET to
!!                 the aberration-corrected location of the target.
!!                 Note that the sense of this position vector is
!!                 independent of the direction of radiation travel
!!                 implied by the aberration correction.
!!
!!                 Units are always km.
!!
!!                 Non-inertial frames are treated as follows: letting
!!                 LTCENT be the one-way light time between the observer
!!                 and the central body associated with the frame, the
!!                 orientation of the frame is evaluated at ET-LTCENT,
!!                 ET+LTCENT, or ET depending on whether the requested
!!                 aberration correction is, respectively, for received
!!                 radiation, transmitted radiation, or is omitted.
!!                 LTCENT is computed using the method indicated by
!!                 ABCORR.
!!
!!     LT          is the one-way light time between the observer and
!!                 target in seconds.  If the target position is
!!                 corrected for aberrations, then LT is the one-way
!!                 light time between the observer and the light time
!!                 corrected target location.
!!
!! ## Parameters
!!
!!     None.
!!
!! ## Exceptions
!!
!!     1) If name of target or observer cannot be translated to its
!!        NAIF ID code, the error SPICE(IDCODENOTFOUND) is signaled.
!!
!!     2) If the reference frame REF is not a recognized reference
!!        frame the error 'SPICE(UNKNOWNFRAME)' is signaled.
!!
!!     3) If the loaded kernels provide insufficient data to
!!        compute the requested position vector, the deficiency will
!!        be diagnosed by a routine in the call tree of this routine.
!!
!!     4) If an error occurs while reading an SPK or other kernel file,
!!        the error  will be diagnosed by a routine in the call tree
!!        of this routine.
!!
!!     5) If any of the required attributes of the reference frame REF
!!        cannot be determined, 'SPICE(UNKNOWNFRAME2)' is signaled.
!!
!! ## Files
!!
!!     This routine computes positions using SPK files that have been
!!     loaded into the SPICE system, normally via the kernel loading
!!     interface routine FURNSH. See the routine FURNSH and the SPK
!!     and KERNEL Required Reading for further information on loading
!!     (and unloading) kernels.
!!
!!     If the output position PTARG is to be expressed relative to a
!!     non-inertial frame, or if any of the ephemeris data used to
!!     compute PTARG are expressed relative to a non-inertial frame in
!!     the SPK files providing those data, additional kernels may be
!!     needed to enable the reference frame transformations required to
!!     compute the position.  Normally these additional kernels are PCK
!!     files or frame kernels.  Any such kernels must already be loaded
!!     at the time this routine is called.
!!
!! ## Particulars
!!
!!     This routine is part of the user interface to the SPICE ephemeris
!!     system.  It allows you to retrieve position information for any
!!     ephemeris object relative to any other in a reference frame that
!!     is convenient for further computations.
!!
!!     Aberration corrections
!!     ======================
!!
!!     In space science or engineering applications one frequently
!!     wishes to know where to point a remote sensing instrument, such
!!     as an optical camera or radio antenna, in order to observe or
!!     otherwise receive radiation from a target.  This pointing problem
!!     is complicated by the finite speed of light:  one needs to point
!!     to where the target appears to be as opposed to where it actually
!!     is at the epoch of observation.  We use the adjectives
!!     "geometric," "uncorrected," or "true" to refer to an actual
!!     position or state of a target at a specified epoch.  When a
!!     geometric position or state vector is modified to reflect how it
!!     appears to an observer, we describe that vector by any of the
!!     terms "apparent," "corrected," "aberration corrected," or "light
!!     time and stellar aberration corrected." The SPICE Toolkit can
!!     correct for two phenomena affecting the apparent location of an
!!     object:  one-way light time (also called "planetary aberration")
!!     and stellar aberration.
!!
!!     One-way light time
!!     ------------------
!!
!!     Correcting for one-way light time is done by computing, given an
!!     observer and observation epoch, where a target was when the
!!     observed photons departed the target's location.  The vector from
!!     the observer to this computed target location is called a "light
!!     time corrected" vector.  The light time correction depends on the
!!     motion of the target relative to the solar system barycenter, but
!!     it is independent of the velocity of the observer relative to the
!!     solar system barycenter. Relativistic effects such as light
!!     bending and gravitational delay are not accounted for in the
!!     light time correction performed by this routine.
!!
!!     Stellar aberration
!!     ------------------
!!
!!     The velocity of the observer also affects the apparent location
!!     of a target:  photons arriving at the observer are subject to a
!!     "raindrop effect" whereby their velocity relative to the observer
!!     is, using a Newtonian approximation, the photons' velocity
!!     relative to the solar system barycenter minus the velocity of the
!!     observer relative to the solar system barycenter.  This effect is
!!     called "stellar aberration."  Stellar aberration is independent
!!     of the velocity of the target.  The stellar aberration formula
!!     used by this routine does not include (the much smaller)
!!     relativistic effects.
!!
!!     Stellar aberration corrections are applied after light time
!!     corrections:  the light time corrected target position vector is
!!     used as an input to the stellar aberration correction.
!!
!!     When light time and stellar aberration corrections are both
!!     applied to a geometric position vector, the resulting position
!!     vector indicates where the target "appears to be" from the
!!     observer's location.
!!
!!     As opposed to computing the apparent position of a target, one
!!     may wish to compute the pointing direction required for
!!     transmission of photons to the target.  This also requires
!!     correction of the geometric target position for the effects of
!!     light time and stellar aberration, but in this case the
!!     corrections are computed for radiation traveling *from* the
!!     observer to the target.
!!
!!     The "transmission" light time correction yields the target's
!!     location as it will be when photons emitted from the observer's
!!     location at ET arrive at the target.  The transmission stellar
!!     aberration correction is the inverse of the traditional stellar
!!     aberration correction:  it indicates the direction in which
!!     radiation should be emitted so that, using a Newtonian
!!     approximation, the sum of the velocity of the radiation relative
!!     to the observer and of the observer's velocity, relative to the
!!     solar system barycenter, yields a velocity vector that points in
!!     the direction of the light time corrected position of the target.
!!
!!     One may object to using the term "observer" in the transmission
!!     case, in which radiation is emitted from the observer's location.
!!     The terminology was retained for consistency with earlier
!!     documentation.
!!
!!     Below, we indicate the aberration corrections to use for some
!!     common applications:
!!
!!        1) Find the apparent direction of a target for a remote-sensing
!!           observation.
!!
!!              Use 'LT+S' or 'CN+S: apply both light time and stellar
!!              aberration corrections.
!!
!!           Note that using light time corrections alone ('LT' or 'CN')
!!           is generally not a good way to obtain an approximation to
!!           an apparent target vector: since light time and stellar
!!           aberration corrections often partially cancel each other,
!!           it may be more accurate to use no correction at all than to
!!           use light time alone.
!!
!!        2) Find the corrected pointing direction to radiate a signal
!!           to a target. This computation is often applicable for
!!           implementing communications sessions.
!!
!!              Use 'XLT+S' or 'XCN+S: apply both light time and stellar
!!              aberration corrections for transmission.
!!
!!        3) Compute the apparent position of a target body relative
!!           to a star or other distant object.
!!
!!              Use 'LT', 'CN', 'LT+S', or 'CN+S' as needed to match the
!!              correction applied to the position of the distant
!!              object. For example, if a star position is obtained from
!!              a catalog, the position vector may not be corrected for
!!              stellar aberration. In this case, to find the angular
!!              separation of the star and the limb of a planet, the
!!              vector from the observer to the planet should be
!!              corrected for light time but not stellar aberration.
!!
!!        4) Obtain an uncorrected position vector derived directly from
!!           data in an SPK file.
!!
!!              Use 'NONE'.
!!
!!        5) Use a geometric position vector as a low-accuracy estimate
!!           of the apparent position for an application where execution
!!           speed is critical.
!!
!!              Use 'NONE'.
!!
!!        6) While this routine cannot perform the relativistic
!!           aberration corrections required to compute positions
!!           with the highest possible accuracy, it can supply the
!!           geometric positions required as inputs to these
!!           computations.
!!
!!              Use 'NONE', then apply high-accuracy aberration
!!              corrections (not available in the SPICE Toolkit).
!!
!!     Below, we discuss in more detail how the aberration corrections
!!     applied by this routine are computed.
!!
!!        Geometric case
!!        ==============
!!
!!        SPKEZP begins by computing the geometric position T(ET) of the
!!        target body relative to the solar system barycenter (SSB).
!!        Subtracting the geometric position of the observer O(ET) gives
!!        the geometric position of the target body relative to the
!!        observer. The one-way light time, LT, is given by
!!
!!                  | T(ET) - O(ET) |
!!           LT = -------------------
!!                          c
!!
!!        The geometric relationship between the observer, target, and
!!        solar system barycenter is as shown:
!!
!!           SSB ---> O(ET)
!!            |      /
!!            |     /
!!            |    /
!!            |   /  T(ET) - O(ET)
!!            V  V
!!           T(ET)
!!
!!        The returned position vector is
!!
!!           T(ET) - O(ET)
!!
!!        Reception case
!!        ==============
!!
!!        When any of the options 'LT', 'CN', 'LT+S', 'CN+S' is selected
!!        for ABCORR, SPKEZP computes the position of the target body at
!!        epoch ET-LT, where LT is the one-way light time.  Let T(t) and
!!        O(t) represent the positions of the target and observer
!!        relative to the solar system barycenter at time t; then LT is
!!        the solution of the light-time equation
!!
!!                  | T(ET-LT) - O(ET) |
!!           LT = ------------------------                            (1)
!!                           c
!!
!!        The ratio
!!
!!            | T(ET) - O(ET) |
!!          ---------------------                                     (2)
!!                    c
!!
!!        is used as a first approximation to LT; inserting (2) into the
!!        right hand side of the light-time equation (1) yields the
!!        "one-iteration" estimate of the one-way light time ("LT").
!!        Repeating the process until the estimates of LT converge
!!        yields the "converged Newtonian" light time estimate ("CN").
!!
!!        Subtracting the geometric position of the observer O(ET) gives
!!        the position of the target body relative to the observer:
!!        T(ET-LT) - O(ET).
!!
!!           SSB ---> O(ET)
!!            | \     |
!!            |  \    |
!!            |   \   | T(ET-LT) - O(ET)
!!            |    \  |
!!            V     V V
!!           T(ET)  T(ET-LT)
!!
!!        The light time corrected position vector is
!!
!!           T(ET-LT) - O(ET)
!!
!!        If correction for stellar aberration is requested, the target
!!        position is rotated toward the solar system barycenter-
!!        relative velocity vector of the observer.  The rotation is
!!        computed as follows:
!!
!!           Let r be the light time corrected vector from the observer
!!           to the object, and v be the velocity of the observer with
!!           respect to the solar system barycenter. Let w be the angle
!!           between them. The aberration angle phi is given by
!!
!!              sin(phi) = v sin(w) / c
!!
!!           Let h be the vector given by the cross product
!!
!!              h = r X v
!!
!!           Rotate r by phi radians about h to obtain the apparent
!!           position of the object.
!!
!!        Transmission case
!!        ==================
!!
!!        When any of the options 'XLT', 'XCN', 'XLT+S', 'XCN+S' is
!!        selected, SPKEZP computes the position of the target body T at
!!        epoch ET+LT, where LT is the one-way light time.  LT is the
!!        solution of the light-time equation
!!
!!                  | T(ET+LT) - O(ET) |
!!           LT = ------------------------                            (3)
!!                            c
!!
!!        Subtracting the geometric position of the observer, O(ET),
!!        gives the position of the target body relative to the
!!        observer: T(ET-LT) - O(ET).
!!
!!                   SSB --> O(ET)
!!                  / |    *
!!                 /  |  *  T(ET+LT) - O(ET)
!!                /   |*
!!               /   *|
!!              V  V  V
!!          T(ET+LT)  T(ET)
!!
!!        The light-time corrected position vector is
!!
!!           T(ET+LT) - O(ET)
!!
!!        If correction for stellar aberration is requested, the target
!!        position is rotated away from the solar system barycenter-
!!        relative velocity vector of the observer. The rotation is
!!        computed as in the reception case, but the sign of the
!!        rotation angle is negated.
!!
!!     Precision of light time corrections
!!     ===================================
!!
!!        Corrections using one iteration of the light time solution
!!        ----------------------------------------------------------
!!
!!        When the requested aberration correction is 'LT', 'LT+S',
!!        'XLT', or 'XLT+S', only one iteration is performed in the
!!        algorithm used to compute LT.
!!
!!        The relative error in this computation
!!
!!           | LT_ACTUAL - LT_COMPUTED |  /  LT_ACTUAL
!!
!!        is at most
!!
!!            (V/C)**2
!!           ----------
!!            1 - (V/C)
!!
!!        which is well approximated by (V/C)**2, where V is the
!!        velocity of the target relative to an inertial frame and C is
!!        the speed of light.
!!
!!        For nearly all objects in the solar system V is less than 60
!!        km/sec. The value of C is ~300000 km/sec. Thus the
!!        one-iteration solution for LT has a potential relative error
!!        of not more than 4e-8. This is a potential light time error of
!!        approximately 2e-5 seconds per astronomical unit of distance
!!        separating the observer and target. Given the bound on V cited
!!        above:
!!
!!           As long as the observer and target are separated by less
!!           than 50 astronomical units, the error in the light time
!!           returned using the one-iteration light time corrections is
!!           less than 1 millisecond.
!!
!!           The magnitude of the corresponding position error, given
!!           the above assumptions, may be as large as (V/C)**2 * the
!!           distance between the observer and the uncorrected target
!!           position: 300 km or equivalently 6 km/AU.
!!
!!        In practice, the difference between positions obtained using
!!        one-iteration and converged light time is usually much smaller
!!        than the value computed above and can be insignificant. For
!!        example, for the spacecraft Mars Reconnaissance Orbiter and
!!        Mars Express, the position error for the one-iteration light
!!        time correction, applied to the spacecraft-to-Mars center
!!        vector, is at the 1 cm level.
!!
!!        Comparison of results obtained using the one-iteration and
!!        converged light time solutions is recommended when adequacy of
!!        the one-iteration solution is in doubt.
!!
!!        Converged corrections
!!        ---------------------
!!
!!        When the requested aberration correction is 'CN', 'CN+S',
!!        'XCN', or 'XCN+S', as many iterations as are required for
!!        convergence are performed in the computation of LT. Usually
!!        the solution is found after three iterations. The relative
!!        error present in this case is at most
!!
!!            (V/C)**4
!!           ----------
!!            1 - (V/C)
!!
!!        which is well approximated by (V/C)**4.
!!
!!           The precision of this computation (ignoring round-off
!!           error) is better than 4e-11 seconds for any pair of objects
!!           less than 50 AU apart, and having speed relative to the
!!           solar system barycenter less than 60 km/s.
!!
!!           The magnitude of the corresponding position error, given
!!           the above assumptions, may be as large as (V/C)**4 * the
!!           distance between the observer and the uncorrected target
!!           position: 1.2 cm at 50 AU or equivalently 0.24 mm/AU.
!!
!!        However, to very accurately model the light time between
!!        target and observer one must take into account effects due to
!!        general relativity. These may be as high as a few hundredths
!!        of a millisecond for some objects.
!!
!!     Relativistic Corrections
!!     =========================
!!
!!     This routine does not attempt to perform either general or
!!     special relativistic corrections in computing the various
!!     aberration corrections.  For many applications relativistic
!!     corrections are not worth the expense of added computation
!!     cycles.  If however, your application requires these additional
!!     corrections we suggest you consult the astronomical almanac (page
!!     B36) for a discussion of how to carry out these corrections.
!!
!! ## Examples
!!
!!     1)  Load a planetary ephemeris SPK, then look up a series of
!!         geometric positions of the moon relative to the earth,
!!         referenced to the J2000 frame.
!!
!!               IMPLICIT NONE
!!         C
!!         C     Local constants
!!         C
!!               CHARACTER*(*)         FRAME
!!               PARAMETER           ( FRAME  = 'J2000' )
!!
!!               CHARACTER*(*)         ABCORR
!!               PARAMETER           ( ABCORR = 'NONE' )
!!
!!         C
!!         C     The name of the SPK file shown here is fictitious;
!!         C     you must supply the name of an SPK file available
!!         C     on your own computer system.
!!         C
!!               CHARACTER*(*)         SPK
!!               PARAMETER           ( SPK    = 'planet.bsp' )
!!
!!         C
!!         C     ET0 represents the date 2000 Jan 1 12:00:00 TDB.
!!         C
!!               DOUBLE PRECISION      ET0
!!               PARAMETER           ( ET0    = 0.0D0 )
!!
!!         C
!!         C     Use a time step of 1 hour; look up 100 positions.
!!         C
!!               DOUBLE PRECISION      STEP
!!               PARAMETER           ( STEP   = 3600.0D0 )
!!
!!               INTEGER               MAXITR
!!               PARAMETER           ( MAXITR = 100 )
!!
!!         C
!!         C     The NAIF IDs of the earth and moon are 399 and 301
!!         C     respectively.
!!         C
!!               INTEGER               OBSRVR
!!               PARAMETER           ( OBSRVR = 399 )
!!
!!               INTEGER               TARGET
!!               PARAMETER           ( TARGET = 301 )
!!
!!         C
!!         C     Local variables
!!         C
!!               DOUBLE PRECISION      ET
!!               DOUBLE PRECISION      LT
!!               DOUBLE PRECISION      POS ( 3 )
!!
!!               INTEGER               I
!!
!!         C
!!         C     Load the SPK file.
!!         C
!!               CALL FURNSH ( SPK )
!!
!!         C
!!         C     Step through a series of epochs, looking up a
!!         C     position vector at each one.
!!         C
!!               DO I = 1, MAXITR
!!
!!                  ET = ET0 + (I-1)*STEP
!!
!!                  CALL SPKEZP ( TARGET, ET, FRAME, ABCORR, OBSRVR,
!!              .                 POS,    LT                        )
!!
!!                  WRITE (*,*) 'ET = ', ET
!!                  WRITE (*,*) 'J2000 x-position (km):   ', POS(1)
!!                  WRITE (*,*) 'J2000 y-position (km):   ', POS(2)
!!                  WRITE (*,*) 'J2000 z-position (km):   ', POS(3)
!!                  WRITE (*,*) ' '
!!
!!               END DO
!!
!!               END
!!
!! ## Restrictions
!!
!!     None.
!!
!! ## Literature References
!!
!!     SPK Required Reading.
!!
!! ## Author and Institution
!!
!!     C.H. Acton      (JPL)
!!     B.V. Semenov    (JPL)
!!     N.J. Bachman    (JPL)
!!     W.L. Taber      (JPL)
!!
!! ## Version
!!
!!-    SPICELIB Version 3.2.0, 03-JUL-2014 (NJB) (BVS)
!!
!!        Discussion of light time corrections was updated. Assertions
!!        that converged light time corrections are unlikely to be
!!        useful were removed.
!!
!!     Last update was 23-SEP-2013 (BVS)
!!
!!        Bug fix: added a check and an exception for the FOUND flag
!!        returned by FRINFO.
!!
!!        Updated to save the input frame name and POOL state counter
!!        and to do frame name-ID conversion only if the counter has
!!        changed.
!!
!!-    SPICELIB Version 3.1.1, 04-APR-2008 (NJB)
!!
!!        Corrected minor error in description of XLT+S aberration
!!        correction.
!!
!!-    SPICELIB Version 3.1.0, 06-JAN-2005 (NJB)
!!
!!        Tests of routine FAILED() were added.
!!
!!-    SPICELIB Version 3.0.3, 12-DEC-2004 (NJB)
!!
!!        Minor header error was corrected.
!!
!!-    SPICELIB Version 3.0.2, 20-OCT-2003 (EDW)
!!
!!        Added mention that LT returns in seconds.
!!
!!-    SPICELIB Version 3.0.1, 29-JUL-2003 (NJB) (CHA)
!!
!!        Various minor header changes were made to improve clarity.
!!
!!-    SPICELIB Version 3.0.0, 31-DEC-2001 (NJB)
!!
!!        Updated to handle aberration corrections for transmission
!!        of radiation.  Formerly, only the reception case was
!!        supported.  The header was revised and expanded to explain
!!        the functionality of this routine in more detail.
!!
!!-    SPICELIB Version 1.0.0, 03-MAR-1999 (WLT)
!!
!! ## Index Entries
!!
!!     using body names get position relative to an observer
!!     get position relative observer corrected for aberrations
!!     read ephemeris data
!!     read trajectory data
!!
!! ## Revisions
!!
!!-    SPICELIB Version 4.1.0, 05-JAN-2005 (NJB)
!!
!!        Tests of routine FAILED() were added.  The new checks
!!        are intended to prevent arithmetic operations from
!!        being performed with uninitialized or invalid data.
!!
!!     SPICELIB functions
!!
!!     Local parameters
!!
!!     Saved frame name length.
!!
!!     Local variables
!!
!!     Saved frame name/ID item declarations.
!!
!!     Saved variables
!!
!!     Initial values
!!
!!     Standard SPICE error handling.
!!
      use pepper_kinds, only: ip, wp
      use pepper_math, only: mxv
      use pepper_strings, only: eqchr, eqstr, ltrim
      use pepper_inc_frmtyp
      use pepper_inc_zzctr
      implicit none
      character(*), parameter :: rname = 'ZZSPKZP1'
      integer(ip), parameter :: frnmln = 32
      real(wp) :: ltcent
      real(wp) :: temp     ( 3    )
      logical :: xmit
      logical :: found
      integer(ip) :: i
      real(wp) :: sobs     ( 6    )
      real(wp) :: xform    ( 3, 3 )
      real(wp) :: postn    ( 3    )
      integer(ip) :: type
      integer(ip) :: typeid
      integer(ip) :: reqfrm
      integer(ip) :: center
      if ( .not. allocated ( ctx%zzspkzp1_state ) ) then
         allocate ( ctx%zzspkzp1_state )
         call init_zzspkzp1_state ( ctx%zzspkzp1_state )
      end if
      associate ( fj2000 => ctx%zzspkzp1_state%fj2000, &
                  first => ctx%zzspkzp1_state%first, &
                  svctr1 => ctx%zzspkzp1_state%svctr1, &
                  svref => ctx%zzspkzp1_state%svref, &
                  svreqf => ctx%zzspkzp1_state%svreqf )
      if ( ctx%failed() ) return

!
!     Get the frame id for J2000 on the first call to this routine.
!
      if ( first ) then

         call namfrm( ctx, 'J2000', fj2000 )

!
!        Initialize counter.
!
         call zzctruin( ctx, svctr1 )

         first = .false.

      end if

!
!     Decide whether the aberration correction is for received or
!     transmitted radiation.
!
      i    = ltrim(abcorr)
      xmit = eqchr ( abcorr(i:i),  'X' )

!
!     If we only want geometric positions, then compute just that.
!
!     Otherwise, compute the state of the observer relative to
!     the SSB.  Then feed that position into SPKAPO to compute the
!     apparent position of the target body relative to the observer
!     with the requested aberration corrections.
!
      if ( eqstr( abcorr, 'NONE' ) ) then

         call zzspkgp1 ( ctx, targ, et, ref, obs, ptarg, lt )

      else
!
!        Get the auxiliary information about the requested output
!        frame.
!
         call zznamfrm ( ctx, svctr1, svref, svreqf, ref, reqfrm )

         if ( reqfrm == 0 ) then
            call ctx%setmsg ( 'The requested output frame ''#'' is ' &
            //            'not recognized by the reference frame ' &
            //            'subsystem. Please check that the ' &
            //            'appropriate kernels have been loaded ' &
            //            'and that you have correctly entered ' &
            //            'the name of the output frame. ' )
            call ctx%errch  ( '#', ref )
            call ctx%sigerr ( 'SPICE(UNKNOWNFRAME)'  )
            return

         end if

         call frinfo ( ctx, reqfrm, center, type, typeid, found )

         if ( ctx%failed() ) return

         if ( .not. found ) then

            call ctx%setmsg ( 'The requested output frame ''#'' is ' &
            //            'not recognized by the reference frame ' &
            //            'subsystem. Please check that the ' &
            //            'appropriate kernels have been loaded ' &
            //            'and that you have correctly entered ' &
            //            'the name of the output frame. ' )
            call ctx%errch  ( '#',  ref )
            call ctx%sigerr ( 'SPICE(UNKNOWNFRAME2)' )

            return

         end if

!
!        If we are dealing with an inertial frame, we can simply
!        call SPKSSB, SPKAPO and return.
!
         if ( type == inertl ) then

            call zzspksb1 ( ctx, obs,  et, ref, sobs )
            call zzspkpa1 ( ctx, targ, et, ref, sobs, abcorr, ptarg, lt )
            return

         end if

!
!        Still here?
!
!        We are dealing with a non-inertial frame.  But we need to
!        do light time and stellar aberration in an inertial frame.
!        Get the "apparent" position of TARG in the intermediary
!        inertial reference frame J2000.
!
!        We also need the light time to the center of the frame.
!
         call zzspksb1 ( ctx, obs,  et, 'J2000', sobs )
         call zzspkpa1 ( ctx, targ, et, 'J2000', sobs, abcorr, postn, lt )

         if ( ctx%failed() ) return

         if ( center == obs ) then
            ltcent = 0.0_wp
         else if ( center == targ ) then
            ltcent = lt
         else
            call zzspkpa1 ( ctx, center, et, 'J2000', sobs, abcorr, temp, &
                            ltcent )
         end if

!
!        If something went wrong (like we couldn't get the position of
!        the center relative to the observer) now it is time to quit.
!
         if ( ctx%failed() ) return

!
!        If the aberration corrections are for transmission, negate
!        the light time, since we wish to compute the orientation
!        of the non-inertial frame at an epoch later than ET by
!        the one-way light time.
!
         if ( xmit ) then
            ltcent = -ltcent
         end if

!
!        Get the rotation from J2000 to the requested frame
!        and convert the position.
!
         call zzrefch1 ( ctx, fj2000, reqfrm, et-ltcent, xform )

         if ( ctx%failed() ) return

         call mxv    ( xform,  postn,             ptarg )

      end if

      return
      end associate
      end procedure zzspkzp1

      module procedure zzspkac1
!! ## Abstract
!!
!!     Return the state (position and velocity) of a target body
!!     relative to an observer, optionally corrected for light time
!!     and stellar aberration, expressed relative to an inertial
!!     reference frame.
!!
!! ## Required Reading
!!
!!     SPK
!!
!! ## Keywords
!!
!!     EPHEMERIS
!!
!! ## Declarations
!!
!! ## Detailed Input
!!
!!     TARG        is the NAIF ID code for a target body.  The target
!!                 and observer define a state vector whose position
!!                 component points from the observer to the target.
!!
!!     ET          is the ephemeris time, expressed as seconds past
!!                 J2000 TDB, at which the state of the target body
!!                 relative to the observer is to be computed.  ET
!!                 refers to time at the observer's location.
!!
!!     REF         is the inertial reference frame with respect to which
!!                 the output state STARG is expressed. REF must be
!!                 recognized by the SPICE Toolkit.  The acceptable
!!                 frames are listed in the Frames Required Reading, as
!!                 well as in the SPICELIB routine CHGIRF.
!!
!!                 Case and blanks are not significant in the string
!!                 REF.
!!
!!     ABCORR      indicates the aberration corrections to be applied
!!                 to the state of the target body to account for one-way
!!                 light time and stellar aberration.  See the discussion
!!                 in the Particulars section for recommendations on
!!                 how to choose aberration corrections.
!!
!!                 ABCORR may be any of the following:
!!
!!                    'NONE'     Apply no correction. Return the
!!                               geometric state of the target body
!!                               relative to the observer.
!!
!!                 The following values of ABCORR apply to the
!!                 "reception" case in which photons depart from the
!!                 target's location at the light-time corrected epoch
!!                 ET-LT and *arrive* at the observer's location at ET:
!!
!!                    'LT'       Correct for one-way light time (also
!!                               called "planetary aberration") using a
!!                               Newtonian formulation. This correction
!!                               yields the state of the target at the
!!                               moment it emitted photons arriving at
!!                               the observer at ET.
!!
!!                               The light time correction uses an
!!                               iterative solution of the light time
!!                               equation (see Particulars for details).
!!                               The solution invoked by the 'LT' option
!!                               uses one iteration.
!!
!!                    'LT+S'     Correct for one-way light time and
!!                               stellar aberration using a Newtonian
!!                               formulation. This option modifies the
!!                               state obtained with the 'LT' option to
!!                               account for the observer's velocity
!!                               relative to the solar system
!!                               barycenter. The result is the apparent
!!                               state of the target---the position and
!!                               velocity of the target as seen by the
!!                               observer.
!!
!!                    'CN'       Converged Newtonian light time
!!                               correction. In solving the light time
!!                               equation, the 'CN' correction iterates
!!                               until the solution converges (three
!!                               iterations on all supported platforms).
!!                               Whether the 'CN+S' solution is
!!                               substantially more accurate than the
!!                               'LT' solution depends on the geometry
!!                               of the participating objects and on the
!!                               accuracy of the input data. In all
!!                               cases this routine will execute more
!!                               slowly when a converged solution is
!!                               computed. See the Particulars section of
!!                               SPKEZR for a discussion of precision of
!!                               light time corrections.
!!
!!                    'CN+S'     Converged Newtonian light time
!!                               correction and stellar aberration
!!                               correction.
!!
!!                 The following values of ABCORR apply to the
!!                 "transmission" case in which photons *depart* from
!!                 the observer's location at ET and arrive at the
!!                 target's location at the light-time corrected epoch
!!                 ET+LT:
!!
!!                    'XLT'      "Transmission" case:  correct for
!!                               one-way light time using a Newtonian
!!                               formulation. This correction yields the
!!                               state of the target at the moment it
!!                               receives photons emitted from the
!!                               observer's location at ET.
!!
!!                    'XLT+S'    "Transmission" case:  correct for
!!                               one-way light time and stellar
!!                               aberration using a Newtonian
!!                               formulation  This option modifies the
!!                               state obtained with the 'XLT' option to
!!                               account for the observer's velocity
!!                               relative to the solar system
!!                               barycenter. The position component of
!!                               the computed target state indicates the
!!                               direction that photons emitted from the
!!                               observer's location must be "aimed" to
!!                               hit the target.
!!
!!                    'XCN'      "Transmission" case:  converged
!!                               Newtonian light time correction.
!!
!!                    'XCN+S'    "Transmission" case:  converged
!!                               Newtonian light time correction and
!!                               stellar aberration correction.
!!
!!     OBS         is the NAIF ID code for the observer body.  The
!!                 target and observer define a state vector whose
!!                 position component points from the observer to the
!!                 target.
!!
!! ## Detailed Output
!!
!!     STARG       is a Cartesian state vector representing the position
!!                 and velocity of the target body relative to the
!!                 specified observer. STARG is corrected for the
!!                 specified aberrations, and is expressed with respect
!!                 to the specified inertial reference frame.  The first
!!                 three components of STARG represent the x-, y- and
!!                 z-components of the target's position; last three
!!                 components form the corresponding velocity vector.
!!
!!                 The position component of STARG points from the
!!                 observer's location at ET to the aberration-corrected
!!                 location of the target. Note that the sense of the
!!                 position vector is independent of the direction of
!!                 radiation travel implied by the aberration
!!                 correction.
!!
!!                 Units are always km and km/sec.
!!
!!     LT          is the one-way light time between the observer and
!!                 target in seconds.  If the target state is corrected
!!                 for aberrations, then LT is the one-way light time
!!                 between the observer and the light time corrected
!!                 target location.
!!
!!     DLT         is the derivative with respect to barycentric
!!                 dynamical time of the one way light time between
!!                 target and observer:
!!
!!                    DLT = d(LT)/d(ET)
!!
!!                 DLT can also be described as the rate of change of
!!                 one way light time. DLT is unitless, since LT and
!!                 ET both have units of TDB seconds.
!!
!!                 If the observer and target are at the same position,
!!                 then DLT is set to zero.
!!
!! ## Parameters
!!
!!     None.
!!
!! ## Exceptions
!!
!!     1) If the value of ABCORR is not recognized, the error
!!        is diagnosed by a routine in the call tree of this
!!        routine.
!!
!!     2) If the reference frame requested is not a recognized
!!        inertial reference frame, the error SPICE(BADFRAME)
!!        is signaled.
!!
!!     3) If the state of the target relative to the solar system
!!        barycenter cannot be computed, the error will be diagnosed
!!        by routines in the call tree of this routine.
!!
!!     4) If the observer and target are at the same position,
!!        then DLT is set to zero. This situation could arise,
!!        for example, when the observer is Mars and the target
!!        is the Mars barycenter.
!!
!! ## Files
!!
!!     This routine computes states using SPK files that have been
!!     loaded into the SPICE system, normally via the kernel loading
!!     interface routine FURNSH.  Application programs typically load
!!     kernels once before this routine is called, for example during
!!     program initialization; kernels need not be loaded repeatedly.
!!     See the routine FURNSH and the SPK and KERNEL Required Reading
!!     for further information on loading (and unloading) kernels.
!!
!!     If any of the ephemeris data used to compute STARG are expressed
!!     relative to a non-inertial frame in the SPK files providing those
!!     data, additional kernels may be needed to enable the reference
!!     frame transformations required to compute the state. Normally
!!     these additional kernels are PCK files or frame kernels. Any
!!     such kernels must already be loaded at the time this routine is
!!     called.
!!
!! ## Particulars
!!
!!     This routine supports higher-level SPK API routines that can
!!     perform both light time and stellar aberration corrections.
!!     User applications normally will not need to call this routine
!!     directly.
!!
!!     See the header of the routine SPKEZR for a detailed discussion
!!     of aberration corrections.
!!
!! ## Examples
!!
!!    1) Look up a sequence of states of the Moon as seen from the
!!       Earth. Use light time and stellar aberration corrections.
!!       Compute the first state for the epoch 2000 JAN 1 12:00:00 TDB;
!!       compute subsequent states at intervals of 1 hour. For each
!!       epoch, display the states, the one way light time between
!!       target and observer, and the rate of change of the one way
!!       light time.
!!
!!       Use the following meta-kernel to specify the kernels to
!!       load:
!!
!!          KPL/MK
!!
!!          This meta-kernel is intended to support operation of SPICE
!!          example programs. The kernels shown here should not be
!!          assumed to contain adequate or correct versions of data
!!          required by SPICE-based user applications.
!!
!!          In order for an application to use this meta-kernel, the
!!          kernels referenced here must be present in the user's
!!          current working directory.
!!
!!          \begindata
!!
!!             KERNELS_TO_LOAD = ( 'de418.bsp',
!!                                 'pck00008.tpc',
!!                                 'naif0008.tls'  )
!!
!!          \begintext
!!
!!       The code example follows:
!!
!!           PROGRAM EX1
!!           IMPLICIT NONE
!!     C
!!     C     Local constants
!!     C
!!     C     The meta-kernel name shown here refers to a file whose
!!     C     contents are those shown above. This file and the kernels
!!     C     it references must exist in your current working directory.
!!     C
!!     C
!!           CHARACTER*(*)         META
!!           PARAMETER           ( META   = 'example.mk' )
!!     C
!!     C     Use a time step of 1 hour; look up 5 states.
!!     C
!!           DOUBLE PRECISION      STEP
!!           PARAMETER           ( STEP   = 3600.0D0 )
!!
!!           INTEGER               MAXITR
!!           PARAMETER           ( MAXITR = 5 )
!!     C
!!     C     Local variables
!!     C
!!           DOUBLE PRECISION      DLT
!!           DOUBLE PRECISION      ET
!!           DOUBLE PRECISION      ET0
!!           DOUBLE PRECISION      LT
!!           DOUBLE PRECISION      STATE ( 6 )
!!           INTEGER               I
!!
!!     C
!!     C     Load the SPK and LSK kernels via the meta-kernel.
!!     C
!!           CALL FURNSH ( META )
!!     C
!!     C     Convert the start time to seconds past J2000 TDB.
!!     C
!!           CALL STR2ET ( '2000 JAN 1 12:00:00 TDB', ET0 )
!!     C
!!     C     Step through a series of epochs, looking up a
!!     C     state vector at each one.
!!     C
!!           DO I = 1, MAXITR
!!
!!              ET = ET0 + (I-1)*STEP
!!     C
!!     C        Look up a state vector at epoch ET using the
!!     C        following inputs:
!!     C
!!     C           Target:                 Moon (NAIF ID code 301)
!!     C           Reference frame:        J2000
!!     C           Aberration correction:  Light time and stellar
!!     C                                   aberration ('LT+S')
!!     C           Observer:               Earth (NAIF ID code 399)
!!     C
!!
!!              CALL SPKACS ( 301, ET,    'J2000', 'LT+S',
!!          .                 399, STATE, LT,      DLT     )
!!
!!              WRITE (*,*) 'ET = ', ET
!!              WRITE (*,*) 'J2000 x-position (km):   ', STATE(1)
!!              WRITE (*,*) 'J2000 y-position (km):   ', STATE(2)
!!              WRITE (*,*) 'J2000 z-position (km):   ', STATE(3)
!!              WRITE (*,*) 'J2000 x-velocity (km/s): ', STATE(4)
!!              WRITE (*,*) 'J2000 y-velocity (km/s): ', STATE(5)
!!              WRITE (*,*) 'J2000 z-velocity (km/s): ', STATE(6)
!!              WRITE (*,*) 'One-way light time (s):  ', LT
!!              WRITE (*,*) 'Light time rate:         ', DLT
!!              WRITE (*,*) ' '
!!
!!           END DO
!!
!!           END
!!
!!     The output produced by this program will vary somewhat as
!!     a function of the platform on which the program is built and
!!     executed. On a PC/Linux/g77 platform, the following output
!!     was produced:
!!
!!        ET =   0.
!!        J2000 x-position (km):    -291584.614
!!        J2000 y-position (km):    -266693.406
!!        J2000 z-position (km):    -76095.6532
!!        J2000 x-velocity (km/s):   0.643439157
!!        J2000 y-velocity (km/s):  -0.666065874
!!        J2000 z-velocity (km/s):  -0.301310063
!!        One-way light time (s):    1.34231061
!!        Light time rate:           1.07316909E-07
!!
!!        ET =   3600.
!!        J2000 x-position (km):    -289256.459
!!        J2000 y-position (km):    -269080.605
!!        J2000 z-position (km):    -77177.3528
!!        J2000 x-velocity (km/s):   0.64997032
!!        J2000 y-velocity (km/s):  -0.660148253
!!        J2000 z-velocity (km/s):  -0.299630418
!!        One-way light time (s):    1.34269395
!!        Light time rate:           1.05652599E-07
!!
!!        ET =   7200.
!!        J2000 x-position (km):    -286904.897
!!        J2000 y-position (km):    -271446.417
!!        J2000 z-position (km):    -78252.9655
!!        J2000 x-velocity (km/s):   0.656443883
!!        J2000 y-velocity (km/s):  -0.654183552
!!        J2000 z-velocity (km/s):  -0.297928533
!!        One-way light time (s):    1.34307131
!!        Light time rate:           1.03990457E-07
!!
!!        ET =   10800.
!!        J2000 x-position (km):    -284530.133
!!        J2000 y-position (km):    -273790.671
!!        J2000 z-position (km):    -79322.4117
!!        J2000 x-velocity (km/s):   0.662859505
!!        J2000 y-velocity (km/s):  -0.648172247
!!        J2000 z-velocity (km/s):  -0.296204558
!!        One-way light time (s):    1.34344269
!!        Light time rate:           1.02330665E-07
!!
!!        ET =   14400.
!!        J2000 x-position (km):    -282132.378
!!        J2000 y-position (km):    -276113.202
!!        J2000 z-position (km):    -80385.612
!!        J2000 x-velocity (km/s):   0.669216846
!!        J2000 y-velocity (km/s):  -0.642114815
!!        J2000 z-velocity (km/s):  -0.294458645
!!        One-way light time (s):    1.3438081
!!        Light time rate:           1.00673404E-07
!!
!! ## Restrictions
!!
!!     1) The kernel files to be used by SPKACS must be loaded
!!        (normally by the SPICELIB kernel loader FURNSH) before
!!        this routine is called.
!!
!!     2) Unlike most other SPK state computation routines, this
!!        routine requires that the output state be relative to an
!!        inertial reference frame.
!!
!! ## Literature References
!!
!!     SPK Required Reading.
!!
!! ## Author and Institution
!!
!!     N.J. Bachman    (JPL)
!!
!! ## Version
!!
!!-    SPICELIB Version 1.1.0, 23-AUG-2021 (NJB)
!!
!!        Bug fix: aberration correction strings are now parsed using
!!        ZZVALCOR. This improves error checking.
!!
!!-    SPICELIB Version 1.0.1, 04-JUL-2014 (NJB)
!!
!!        Discussion of light time corrections was updated. Assertions
!!        that converged light time corrections are unlikely to be
!!        useful were removed.
!!
!!-    SPICELIB Version 1.0.0, 11-JAN-2008 (NJB)
!!
!! ## Index Entries
!!
!!     low-level aberration correction
!!     aberration-corrected state from spk file
!!     get light time and stellar aberration-corrected state
!!
!! ## Revisions
!!
!!     None.
!!
!!     SPICELIB functions
!!
!!     Local parameters
!!
!!     Local variables
!!
!!     Saved variables
!!
!!     Initial values
!!
!!     Standard SPICE error handling.
!!
      use pepper_kinds, only: ip, wp
      use pepper_arrays, only: cleard
      use pepper_inc_zzabcorr
      implicit none
      character(*), parameter :: rname = 'ZZSPKAC1'
      real(wp), parameter :: delta = 1.0_wp
      integer(ip), parameter :: ssb = 0
      real(wp)              acc    ( 3 )
      real(wp)              ltssb
      real(wp)              ssblt
      real(wp)              ssbobs ( 6 )
      real(wp)              stobs  ( 6, 2 )
      real(wp)              t
      integer(ip)           i
      integer(ip)           refid
      logical               attblk ( nabcor )
      if ( .not. allocated ( ctx%zzspkac1_state ) ) then
         allocate ( ctx%zzspkac1_state )
         call init_zzspkac1_state ( ctx%zzspkac1_state )
      end if
      associate ( first => ctx%zzspkac1_state%first, &
                  prvcor => ctx%zzspkac1_state%prvcor, &
                  usestl => ctx%zzspkac1_state%usestl )
      if ( ctx%failed() ) return

      if (  first  .or.  ( abcorr /= prvcor )  ) then
!
!        The aberration correction flag differs from the value it
!        had on the previous call, if any.  Analyze the new flag.
!
         call zzvalcor ( ctx, abcorr, attblk )

         if ( ctx%failed() ) return

!
!        The aberration correction flag is recognized; save it.
!
         prvcor = abcorr

!
!        Set logical flags indicating the attributes of the requested
!        correction:
!
!           USESTL is .TRUE. when stellar aberration correction is
!           specified.
!
!        The above definitions are consistent with those used by
!        ZZVALCOR.
!
         usestl  =  attblk ( stlidx )

         first   = .false.

      end if

!
!     See if the reference frame is a recognized inertial frame.
!
      call irfnum ( ctx, ref, refid )

      if ( refid == 0 ) then

         call ctx%setmsg ( 'The requested frame ''#'' is not a ' &
         //            'recognized inertial frame. '        )
         call ctx%errch  ( '#', ref                             )
         call ctx%sigerr ( 'SPICE(BADFRAME)'                    )
         return

      end if

!
!     Prepare to look up the apparent state of the target
!     as seen by the observer. We'll need the geometric
!     state of the observer relative to the solar system
!     barycenter. If we're using stellar aberration
!     corrections, we'll need the observer's acceleration
!     as well.
!
!     Get the geometric state of the observer relative to the SSB,
!     which we'll call SSBOBS.
!
      call zzspkgo1 ( ctx, obs, et, ref, ssb, ssbobs, ssblt )

      if ( usestl ) then
!
!        Numerically differentiate the observer velocity relative to
!        the SSB to obtain acceleration. We first evaluate the
!        geometric state of the observer relative to the solar system
!        barycenter at ET +/- DELTA.

         do i = 1, 2

            t = et + (2*i - 3)*delta

            call zzspkgo1 ( ctx, obs, t, ref, ssb, stobs(1,i), ltssb )

         end do

         call qderiv ( ctx, 3, stobs(4,1), stobs(4,2), delta, acc )

      else

         call cleard ( 3, acc )

      end if

!
!     Look up the apparent state. The light time and light
!     rate are returned as well.
!
      call zzspkas1 ( ctx, targ,   et,  ref,   abcorr, &
                      ssbobs, acc, starg, lt,     dlt )

      return
      end associate
      end procedure zzspkac1

      module procedure zzspkez1
!! ## Abstract
!!
!!     Return the state (position and velocity) of a target body
!!     relative to an observing body, optionally corrected for light
!!     time (planetary aberration) and stellar aberration.
!!
!! ## Required Reading
!!
!!     SPK
!!     NAIF_IDS
!!     FRAMES
!!     TIME
!!
!! ## Keywords
!!
!!     EPHEMERIS
!!
!! ## Declarations
!!
!! ## Detailed Input
!!
!!     TARG        is the NAIF ID code for a target body. The target
!!                 and observer define a state vector whose position
!!                 component points from the observer to the target.
!!
!!     ET          is the ephemeris time, expressed as seconds past J2000
!!                 TDB, at which the state of the target body relative to
!!                 the observer is to be computed. ET refers to time at
!!                 the observer's location.
!!
!!     REF         is the name of the reference frame relative to which
!!                 the output state vector should be expressed. This may
!!                 be any frame supported by the SPICE system, including
!!                 built-in frames (documented in the Frames Required
!!                 Reading) and frames defined by a loaded frame kernel
!!                 (FK).
!!
!!                 When REF designates a non-inertial frame, the
!!                 orientation of the frame is evaluated at an epoch
!!                 dependent on the selected aberration correction.
!!                 See the description of the output state vector STARG
!!                 for details.
!!
!!     ABCORR      indicates the aberration corrections to be applied
!!                 to the state of the target body to account for one-way
!!                 light time and stellar aberration. See the discussion
!!                 in the Particulars section for recommendations on
!!                 how to choose aberration corrections.
!!
!!                 ABCORR may be any of the following:
!!
!!                    'NONE'     Apply no correction. Return the
!!                               geometric state of the target body
!!                               relative to the observer.
!!
!!                 The following values of ABCORR apply to the
!!                 "reception" case in which photons depart from the
!!                 target's location at the light-time corrected epoch
!!                 ET-LT and *arrive* at the observer's location at ET:
!!
!!                    'LT'       Correct for one-way light time (also
!!                               called "planetary aberration") using a
!!                               Newtonian formulation. This correction
!!                               yields the state of the target at the
!!                               moment it emitted photons arriving at
!!                               the observer at ET.
!!
!!                               The light time correction uses an
!!                               iterative solution of the light time
!!                               equation (see Particulars for details).
!!                               The solution invoked by the 'LT' option
!!                               uses one iteration.
!!
!!                    'LT+S'     Correct for one-way light time and
!!                               stellar aberration using a Newtonian
!!                               formulation. This option modifies the
!!                               state obtained with the 'LT' option to
!!                               account for the observer's velocity
!!                               relative to the solar system
!!                               barycenter. The result is the apparent
!!                               state of the target---the position and
!!                               velocity of the target as seen by the
!!                               observer.
!!
!!                    'CN'       Converged Newtonian light time
!!                               correction. In solving the light time
!!                               equation, the 'CN' correction iterates
!!                               until the solution converges (three
!!                               iterations on all supported platforms).
!!                               Whether the 'CN+S' solution is
!!                               substantially more accurate than the
!!                               'LT' solution depends on the geometry
!!                               of the participating objects and on the
!!                               accuracy of the input data. In all
!!                               cases this routine will execute more
!!                               slowly when a converged solution is
!!                               computed. See the Particulars section
!!                               below for a discussion of precision of
!!                               light time corrections.
!!
!!                    'CN+S'     Converged Newtonian light time
!!                               correction and stellar aberration
!!                               correction.
!!
!!                 The following values of ABCORR apply to the
!!                 "transmission" case in which photons *depart* from
!!                 the observer's location at ET and arrive at the
!!                 target's location at the light-time corrected epoch
!!                 ET+LT:
!!
!!                    'XLT'      "Transmission" case:  correct for
!!                               one-way light time using a Newtonian
!!                               formulation. This correction yields the
!!                               state of the target at the moment it
!!                               receives photons emitted from the
!!                               observer's location at ET.
!!
!!                    'XLT+S'    "Transmission" case:  correct for
!!                               one-way light time and stellar
!!                               aberration using a Newtonian
!!                               formulation  This option modifies the
!!                               state obtained with the 'XLT' option to
!!                               account for the observer's velocity
!!                               relative to the solar system
!!                               barycenter. The position component of
!!                               the computed target state indicates the
!!                               direction that photons emitted from the
!!                               observer's location must be "aimed" to
!!                               hit the target.
!!
!!                    'XCN'      "Transmission" case:  converged
!!                               Newtonian light time correction.
!!
!!                    'XCN+S'    "Transmission" case:  converged
!!                               Newtonian light time correction and
!!                               stellar aberration correction.
!!
!!                 Neither special nor general relativistic effects are
!!                 accounted for in the aberration corrections applied
!!                 by this routine.
!!
!!                 Case and blanks are not significant in the string
!!                 ABCORR.
!!
!!     OBS         is the NAIF ID code for an observing body.
!!
!! ## Detailed Output
!!
!!     STARG       is a Cartesian state vector representing the position
!!                 and velocity of the target body relative to the
!!                 specified observer. STARG is corrected for the
!!                 specified aberrations, and is expressed with respect
!!                 to the reference frame specified by REF. The first
!!                 three components of STARG represent the x-, y- and
!!                 z-components of the target's position; the last three
!!                 components form the corresponding velocity vector.
!!
!!                 The position component of STARG points from the
!!                 observer's location at ET to the aberration-corrected
!!                 location of the target. Note that the sense of the
!!                 position vector is independent of the direction of
!!                 radiation travel implied by the aberration
!!                 correction.
!!
!!                 The velocity component of STARG is the derivative
!!                 with respect to time of the position component of
!!                 STARG.
!!
!!                 Units are always km and km/sec.
!!
!!                 Non-inertial frames are treated as follows: letting
!!                 LTCENT be the one-way light time between the observer
!!                 and the central body associated with the frame, the
!!                 orientation of the frame is evaluated at ET-LTCENT,
!!                 ET+LTCENT, or ET depending on whether the requested
!!                 aberration correction is, respectively, for received
!!                 radiation, transmitted radiation, or is omitted.
!!                 LTCENT is computed using the method indicated by
!!                 ABCORR.
!!
!!     LT          is the one-way light time between the observer and
!!                 target in seconds. If the target state is corrected
!!                 for aberrations, then LT is the one-way light time
!!                 between the observer and the light time corrected
!!                 target location.
!!
!! ## Parameters
!!
!!     None.
!!
!! ## Exceptions
!!
!!     1) If the reference frame REF is not a recognized reference
!!        frame the error 'SPICE(UNKNOWNFRAME)' is signaled.
!!
!!     2) If the loaded kernels provide insufficient data to
!!        compute the requested state vector, the deficiency will
!!        be diagnosed by a routine in the call tree of this routine.
!!
!!     3) If an error occurs while reading an SPK or other kernel file,
!!        the error  will be diagnosed by a routine in the call tree
!!        of this routine.
!!
!!     4) If any of the required attributes of the reference frame REF
!!        cannot be determined, 'SPICE(UNKNOWNFRAME2)' is signaled.
!!
!! ## Files
!!
!!     This routine computes states using SPK files that have been
!!     loaded into the SPICE system, normally via the kernel loading
!!     interface routine FURNSH. See the routine FURNSH and the SPK
!!     and KERNEL Required Reading for further information on loading
!!     (and unloading) kernels.
!!
!!     If the output state STARG is to be expressed relative to a
!!     non-inertial frame, or if any of the ephemeris data used to
!!     compute STARG are expressed relative to a non-inertial frame in
!!     the SPK files providing those data, additional kernels may be
!!     needed to enable the reference frame transformations required to
!!     compute the state. Normally these additional kernels are PCK
!!     files or frame kernels. Any such kernels must already be loaded
!!     at the time this routine is called.
!!
!! ## Particulars
!!
!!     This routine is part of the user interface to the SPICE ephemeris
!!     system. It allows you to retrieve state information for any
!!     ephemeris object relative to any other in a reference frame that
!!     is convenient for further computations.
!!
!!     Aberration corrections
!!     ======================
!!
!!     In space science or engineering applications one frequently
!!     wishes to know where to point a remote sensing instrument, such
!!     as an optical camera or radio antenna, in order to observe or
!!     otherwise receive radiation from a target. This pointing problem
!!     is complicated by the finite speed of light:  one needs to point
!!     to where the target appears to be as opposed to where it actually
!!     is at the epoch of observation. We use the adjectives
!!     "geometric," "uncorrected," or "true" to refer to an actual
!!     position or state of a target at a specified epoch. When a
!!     geometric position or state vector is modified to reflect how it
!!     appears to an observer, we describe that vector by any of the
!!     terms "apparent," "corrected," "aberration corrected," or "light
!!     time and stellar aberration corrected." The SPICE Toolkit can
!!     correct for two phenomena affecting the apparent location of an
!!     object:  one-way light time (also called "planetary aberration")
!!     and stellar aberration.
!!
!!     One-way light time
!!     ------------------
!!
!!     Correcting for one-way light time is done by computing, given an
!!     observer and observation epoch, where a target was when the
!!     observed photons departed the target's location. The vector from
!!     the observer to this computed target location is called a "light
!!     time corrected" vector. The light time correction depends on the
!!     motion of the target relative to the solar system barycenter, but
!!     it is independent of the velocity of the observer relative to the
!!     solar system barycenter. Relativistic effects such as light
!!     bending and gravitational delay are not accounted for in the
!!     light time correction performed by this routine.
!!
!!     Stellar aberration
!!     ------------------
!!
!!     The velocity of the observer also affects the apparent location
!!     of a target:  photons arriving at the observer are subject to a
!!     "raindrop effect" whereby their velocity relative to the observer
!!     is, using a Newtonian approximation, the photons' velocity
!!     relative to the solar system barycenter minus the velocity of the
!!     observer relative to the solar system barycenter. This effect is
!!     called "stellar aberration."  Stellar aberration is independent
!!     of the velocity of the target. The stellar aberration formula
!!     used by this routine does not include (the much smaller)
!!     relativistic effects.
!!
!!     Stellar aberration corrections are applied after light time
!!     corrections:  the light time corrected target position vector is
!!     used as an input to the stellar aberration correction.
!!
!!     When light time and stellar aberration corrections are both
!!     applied to a geometric position vector, the resulting position
!!     vector indicates where the target "appears to be" from the
!!     observer's location.
!!
!!     As opposed to computing the apparent position of a target, one
!!     may wish to compute the pointing direction required for
!!     transmission of photons to the target. This also requires
!!     correction of the geometric target position for the effects of
!!     light time and stellar aberration, but in this case the
!!     corrections are computed for radiation traveling *from* the
!!     observer to the target.
!!
!!     The "transmission" light time correction yields the target's
!!     location as it will be when photons emitted from the observer's
!!     location at ET arrive at the target. The transmission stellar
!!     aberration correction is the inverse of the traditional stellar
!!     aberration correction:  it indicates the direction in which
!!     radiation should be emitted so that, using a Newtonian
!!     approximation, the sum of the velocity of the radiation relative
!!     to the observer and of the observer's velocity, relative to the
!!     solar system barycenter, yields a velocity vector that points in
!!     the direction of the light time corrected position of the target.
!!
!!     One may object to using the term "observer" in the transmission
!!     case, in which radiation is emitted from the observer's location.
!!     The terminology was retained for consistency with earlier
!!     documentation.
!!
!!     Below, we indicate the aberration corrections to use for some
!!     common applications:
!!
!!        1) Find the apparent direction of a target for a remote-sensing
!!           observation.
!!
!!              Use 'LT+S' or 'CN+S: apply both light time and stellar
!!              aberration corrections.
!!
!!           Note that using light time corrections alone ('LT' or 'CN')
!!           is generally not a good way to obtain an approximation to
!!           an apparent target vector: since light time and stellar
!!           aberration corrections often partially cancel each other,
!!           it may be more accurate to use no correction at all than to
!!           use light time alone.
!!
!!        2) Find the corrected pointing direction to radiate a signal
!!           to a target. This computation is often applicable for
!!           implementing communications sessions.
!!
!!              Use 'XLT+S' or 'XCN+S: apply both light time and stellar
!!              aberration corrections for transmission.
!!
!!        3) Compute the apparent position of a target body relative
!!           to a star or other distant object.
!!
!!              Use 'LT', 'CN', 'LT+S', or 'CN+S' as needed to match the
!!              correction applied to the position of the distant
!!              object. For example, if a star position is obtained from
!!              a catalog, the position vector may not be corrected for
!!              stellar aberration. In this case, to find the angular
!!              separation of the star and the limb of a planet, the
!!              vector from the observer to the planet should be
!!              corrected for light time but not stellar aberration.
!!
!!        4) Obtain an uncorrected state vector derived directly from
!!           data in an SPK file.
!!
!!              Use 'NONE'.
!!
!!        5) Use a geometric state vector as a low-accuracy estimate
!!           of the apparent state for an application where execution
!!           speed is critical.
!!
!!              Use 'NONE'.
!!
!!        6) While this routine cannot perform the relativistic
!!           aberration corrections required to compute states
!!           with the highest possible accuracy, it can supply the
!!           geometric states required as inputs to these computations.
!!
!!              Use 'NONE', then apply relativistic aberration
!!              corrections (not available in the SPICE Toolkit).
!!
!!     Below, we discuss in more detail how the aberration corrections
!!     applied by this routine are computed.
!!
!!        Geometric case
!!        ==============
!!
!!        SPKEZ begins by computing the geometric position T(ET) of the
!!        target body relative to the solar system barycenter (SSB).
!!        Subtracting the geometric position of the observer O(ET) gives
!!        the geometric position of the target body relative to the
!!        observer. The one-way light time, LT, is given by
!!
!!                  | T(ET) - O(ET) |
!!           LT = -------------------
!!                          c
!!
!!        The geometric relationship between the observer, target, and
!!        solar system barycenter is as shown:
!!
!!           SSB ---> O(ET)
!!            |      /
!!            |     /
!!            |    /
!!            |   /  T(ET) - O(ET)
!!            V  V
!!           T(ET)
!!
!!        The returned state consists of the position vector
!!
!!           T(ET) - O(ET)
!!
!!        and a velocity obtained by taking the difference of the
!!        corresponding velocities. In the geometric case, the
!!        returned velocity is actually the time derivative of the
!!        position.
!!
!!        Reception case
!!        ==============
!!
!!        When any of the options 'LT', 'CN', 'LT+S', 'CN+S' is selected
!!        for ABCORR, SPKEZ computes the position of the target body at
!!        epoch ET-LT, where LT is the one-way light time. Let T(t) and
!!        O(t) represent the positions of the target and observer
!!        relative to the solar system barycenter at time t; then LT is
!!        the solution of the light-time equation
!!
!!                  | T(ET-LT) - O(ET) |
!!           LT = ------------------------                            (1)
!!                           c
!!
!!        The ratio
!!
!!            | T(ET) - O(ET) |
!!          ---------------------                                     (2)
!!                    c
!!
!!        is used as a first approximation to LT; inserting (2) into the
!!        right hand side of the light-time equation (1) yields the
!!        "one-iteration" estimate of the one-way light time ("LT").
!!        Repeating the process until the estimates of LT converge
!!        yields the "converged Newtonian" light time estimate ("CN").
!!
!!        Subtracting the geometric position of the observer O(ET) gives
!!        the position of the target body relative to the observer:
!!        T(ET-LT) - O(ET).
!!
!!           SSB ---> O(ET)
!!            | \     |
!!            |  \    |
!!            |   \   | T(ET-LT) - O(ET)
!!            |    \  |
!!            V     V V
!!           T(ET)  T(ET-LT)
!!
!!        The position component of the light time corrected state
!!        is the vector
!!
!!           T(ET-LT) - O(ET)
!!
!!        The velocity component of the light time corrected state
!!        is the difference
!!
!!           T_vel(ET-LT)*(1-dLT/dET) - O_vel(ET)
!!
!!        where T_vel and O_vel are, respectively, the velocities of the
!!        target and observer relative to the solar system barycenter at
!!        the epochs ET-LT and ET.
!!
!!        If correction for stellar aberration is requested, the target
!!        position is rotated toward the solar system barycenter-
!!        relative velocity vector of the observer. The rotation is
!!        computed as follows:
!!
!!           Let r be the light time corrected vector from the observer
!!           to the object, and v be the velocity of the observer with
!!           respect to the solar system barycenter. Let w be the angle
!!           between them. The aberration angle phi is given by
!!
!!              sin(phi) = v sin(w) / c
!!
!!           Let h be the vector given by the cross product
!!
!!              h = r X v
!!
!!           Rotate r by phi radians about h to obtain the apparent
!!           position of the object.
!!
!!        When stellar aberration corrections are used, the rate of
!!        change of the stellar aberration correction is accounted for
!!        in the computation of the output velocity.
!!
!!        Transmission case
!!        ==================
!!
!!        When any of the options 'XLT', 'XCN', 'XLT+S', 'XCN+S' is
!!        selected, SPKEZ computes the position of the target body T at
!!        epoch ET+LT, where LT is the one-way light time. LT is the
!!        solution of the light-time equation
!!
!!                  | T(ET+LT) - O(ET) |
!!           LT = ------------------------                            (3)
!!                            c
!!
!!        Subtracting the geometric position of the observer, O(ET),
!!        gives the position of the target body relative to the
!!        observer: T(ET-LT) - O(ET).
!!
!!                   SSB --> O(ET)
!!                  / |    *
!!                 /  |  *  T(ET+LT) - O(ET)
!!                /   |*
!!               /   *|
!!              V  V  V
!!          T(ET+LT)  T(ET)
!!
!!        The position component of the light-time corrected state
!!        is the vector
!!
!!           T(ET+LT) - O(ET)
!!
!!        The velocity component of the light-time corrected state
!!        consists of the difference
!!
!!           T_vel(ET+LT)*(1+dLT/dET) - O_vel(ET)
!!
!!        where T_vel and O_vel are, respectively, the velocities of the
!!        target and observer relative to the solar system barycenter at
!!        the epochs ET+LT and ET.
!!
!!        If correction for stellar aberration is requested, the target
!!        position is rotated away from the solar system barycenter-
!!        relative velocity vector of the observer. The rotation is
!!        computed as in the reception case, but the sign of the
!!        rotation angle is negated. Velocities are adjusted to account
!!        for the rate of change of the stellar aberration correction.
!!
!!     Precision of light time corrections
!!     ===================================
!!
!!        Corrections using one iteration of the light time solution
!!        ----------------------------------------------------------
!!
!!        When the requested aberration correction is 'LT', 'LT+S',
!!        'XLT', or 'XLT+S', only one iteration is performed in the
!!        algorithm used to compute LT.
!!
!!        The relative error in this computation
!!
!!           | LT_ACTUAL - LT_COMPUTED |  /  LT_ACTUAL
!!
!!        is at most
!!
!!            (V/C)**2
!!           ----------
!!            1 - (V/C)
!!
!!        which is well approximated by (V/C)**2, where V is the
!!        velocity of the target relative to an inertial frame and C is
!!        the speed of light.
!!
!!        For nearly all objects in the solar system V is less than 60
!!        km/sec. The value of C is ~300000 km/sec. Thus the
!!        one-iteration solution for LT has a potential relative error
!!        of not more than 4e-8. This is a potential light time error of
!!        approximately 2e-5 seconds per astronomical unit of distance
!!        separating the observer and target. Given the bound on V cited
!!        above:
!!
!!           As long as the observer and target are separated by less
!!           than 50 astronomical units, the error in the light time
!!           returned using the one-iteration light time corrections is
!!           less than 1 millisecond.
!!
!!           The magnitude of the corresponding position error, given
!!           the above assumptions, may be as large as (V/C)**2 * the
!!           distance between the observer and the uncorrected target
!!           position: 300 km or equivalently 6 km/AU.
!!
!!        In practice, the difference between positions obtained using
!!        one-iteration and converged light time is usually much smaller
!!        than the value computed above and can be insignificant. For
!!        example, for the spacecraft Mars Reconnaissance Orbiter and
!!        Mars Express, the position error for the one-iteration light
!!        time correction, applied to the spacecraft-to-Mars center
!!        vector, is at the 1 cm level.
!!
!!        Comparison of results obtained using the one-iteration and
!!        converged light time solutions is recommended when adequacy of
!!        the one-iteration solution is in doubt.
!!
!!        Converged corrections
!!        ---------------------
!!
!!        When the requested aberration correction is 'CN', 'CN+S',
!!        'XCN', or 'XCN+S', as many iterations as are required for
!!        convergence are performed in the computation of LT. Usually
!!        the solution is found after three iterations. The relative
!!        error present in this case is at most
!!
!!            (V/C)**4
!!           ----------
!!            1 - (V/C)
!!
!!        which is well approximated by (V/C)**4.
!!
!!           The precision of this computation (ignoring round-off
!!           error) is better than 4e-11 seconds for any pair of objects
!!           less than 50 AU apart, and having speed relative to the
!!           solar system barycenter less than 60 km/s.
!!
!!           The magnitude of the corresponding position error, given
!!           the above assumptions, may be as large as (V/C)**4 * the
!!           distance between the observer and the uncorrected target
!!           position: 1.2 cm at 50 AU or equivalently 0.24 mm/AU.
!!
!!        However, to very accurately model the light time between
!!        target and observer one must take into account effects due to
!!        general relativity. These may be as high as a few hundredths
!!        of a millisecond for some objects.
!!
!!     Relativistic Corrections
!!     =========================
!!
!!     This routine does not attempt to perform either general or
!!     special relativistic corrections in computing the various
!!     aberration corrections. For many applications relativistic
!!     corrections are not worth the expense of added computation
!!     cycles. If however, your application requires these additional
!!     corrections we suggest you consult the astronomical almanac (page
!!     B36) for a discussion of how to carry out these corrections.
!!
!! ## Examples
!!
!!     1)  Load a planetary ephemeris SPK; then look up a series of
!!         geometric states of the moon relative to the earth,
!!         referenced to the J2000 frame.
!!
!!               IMPLICIT NONE
!!         C
!!         C     Local constants
!!         C
!!               CHARACTER*(*)         FRAME
!!               PARAMETER           ( FRAME  = 'J2000' )
!!
!!               CHARACTER*(*)         ABCORR
!!               PARAMETER           ( ABCORR = 'NONE' )
!!
!!         C
!!         C     The name of the SPK file shown here is fictitious;
!!         C     you must supply the name of an SPK file available
!!         C     on your own computer system.
!!         C
!!               CHARACTER*(*)         SPK
!!               PARAMETER           ( SPK    = 'planet.bsp' )
!!
!!         C
!!         C     ET0 represents the date 2000 Jan 1 12:00:00 TDB.
!!         C
!!               DOUBLE PRECISION      ET0
!!               PARAMETER           ( ET0    = 0.0D0 )
!!
!!         C
!!         C     Use a time step of 1 hour; look up 100 states.
!!         C
!!               DOUBLE PRECISION      STEP
!!               PARAMETER           ( STEP   = 3600.0D0 )
!!
!!               INTEGER               MAXITR
!!               PARAMETER           ( MAXITR = 100 )
!!
!!         C
!!         C     The NAIF IDs of the earth and moon are 399 and 301
!!         C     respectively.
!!         C
!!               INTEGER               OBSRVR
!!               PARAMETER           ( OBSRVR = 399 )
!!
!!               INTEGER               TARGET
!!               PARAMETER           ( TARGET = 301 )
!!
!!         C
!!         C     Local variables
!!         C
!!               DOUBLE PRECISION      ET
!!               DOUBLE PRECISION      LT
!!               DOUBLE PRECISION      STATE ( 6 )
!!
!!               INTEGER               I
!!
!!         C
!!         C     Load the SPK file.
!!         C
!!               CALL FURNSH ( SPK )
!!
!!         C
!!         C     Step through a series of epochs, looking up a
!!         C     state vector at each one.
!!         C
!!               DO I = 1, MAXITR
!!
!!                  ET = ET0 + (I-1)*STEP
!!
!!                  CALL SPKEZ ( TARGET, ET, FRAME, ABCORR, OBSRVR,
!!              .                STATE,  LT                        )
!!
!!                  WRITE (*,*) 'ET = ', ET
!!                  WRITE (*,*) 'J2000 x-position (km):   ', STATE(1)
!!                  WRITE (*,*) 'J2000 y-position (km):   ', STATE(2)
!!                  WRITE (*,*) 'J2000 z-position (km):   ', STATE(3)
!!                  WRITE (*,*) 'J2000 x-velocity (km/s): ', STATE(4)
!!                  WRITE (*,*) 'J2000 y-velocity (km/s): ', STATE(5)
!!                  WRITE (*,*) 'J2000 z-velocity (km/s): ', STATE(6)
!!                  WRITE (*,*) ' '
!!
!!               END DO
!!
!!               END
!!
!! ## Restrictions
!!
!!     None.
!!
!! ## Literature References
!!
!!     SPK Required Reading.
!!
!! ## Author and Institution
!!
!!     C.H. Acton      (JPL)
!!     W.L. Taber      (JPL)
!!     N.J. Bachman    (JPL)
!!     J.E. McLean     (JPL)
!!     H.A. Neilan     (JPL)
!!     B.V. Semenov    (JPL)
!!     M.J. Spencer    (JPL)
!!     I.M. Underwood  (JPL)
!!
!! ## Version
!!
!!-    SPICELIB Version 5.1.0, 03-JUL-2014 (NJB) (BVS)
!!
!!        Discussion of light time corrections was updated. Assertions
!!        that converged light time corrections are unlikely to be
!!        useful were removed.
!!
!!-    Last update was 23-SEP-2013 (NJB) (BVS)
!!
!!        Bug fix: replaced calls to ZZPRSCOR with calls to
!!        ZZVALCOR. The latter routine rejects all aberration
!!        corrections not supported by the SPK subsystem.
!!
!!        Bug fix: added a check and an exception for the FOUND flag
!!        returned by FRINFO.
!!
!!        Updated to save the input frame name and POOL state counter
!!        and to do frame name-ID conversion only if the counter has
!!        changed.
!!
!!        Updated various in-line comments.
!!
!!-    SPICELIB Version 5.0.1, 18-MAY-2010 (BVS)
!!
!!        Removed "C$" marker from text in the header.
!!
!!-    SPICELIB Version 5.0.0, 27-DEC-2007 (NJB)
!!
!!        This routine was upgraded to more accurately compute
!!        aberration-corrected velocity, and in particular, make it
!!        more consistent with observer-target positions.
!!
!!        When light time corrections are used, the derivative of light
!!        time with respect to time is now accounted for in the
!!        computation of observer-target velocities. When the reference
!!        frame associated with the output state is time-dependent, the
!!        derivative of light time with respect to time is now accounted
!!        for in the computation of the rate of change of orientation of
!!        the reference frame.
!!
!!        When stellar aberration corrections are used, velocities
!!        now reflect the rate of range of the stellar aberration
!!        correction.
!!
!!-    SPICELIB Version 4.1.0, 05-JAN-2005 (NJB)
!!
!!        Tests of routine FAILED() were added.
!!        Minor header error was corrected.
!!
!!-    SPICELIB Version 4.0.2, 20-OCT-2003 (EDW)
!!
!!        Added mention that LT returns in seconds.
!!
!!-    SPICELIB Version 4.0.1, 29-JUL-2003 (NJB) (CHA)
!!
!!        Various minor header changes were made to improve clarity.
!!
!!-    SPICELIB Version 4.0.0, 28-DEC-2001 (NJB)
!!
!!        Updated to handle aberration corrections for transmission
!!        of radiation. Formerly, only the reception case was
!!        supported. The header was revised and expanded to explain
!!        the functionality of this routine in more detail.
!!
!!-    SPICELIB Version 3.1.0, 09-JUL-1996 (WLT)
!!
!!        Corrected the description of LT in the Detailed Output
!!        section of the header.
!!
!!-    SPICELIB Version 3.0.0, 26-MAY-1995 (WLT)
!!
!!        The routine was upgraded to support non-inertial frames.
!!
!!-    SPICELIB Version 2.1.1, 5-AUG-1994 (HAN) (MJS)
!!
!!        Added code so that routine accepts lower case, mixed case
!!        and upper case versions of the string ABCORR.
!!
!!-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
!!
!!        Comment section for permuted index source lines was added
!!        following the header.
!!
!!-    SPICELIB Version 2.0.0, 18-JUL-1991 (JEM) (NJB)
!!
!!        The old SPKEZ did not compute the geometric state of one body
!!        with respect to another unless data existed for each body with
!!        respect to the solar system barycenter.
!!
!!-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
!!
!!        Literature references added to the header.
!!
!!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
!!
!! ## Index Entries
!!
!!     easy reader for spk file
!!     get state relative observer corrected for aberrations
!!     read ephemeris data
!!     read trajectory data
!!
!! ## Revisions
!!
!!-    SPICELIB Version 5.0.0, 22-JUL-2007 (NJB)
!!
!!        Routine was upgraded to more accurately compute aberration-
!!        corrected velocity, and in particular, make it more consistent
!!        with observer-target positions. When light time corrections
!!        are used:
!!
!!           1) The derivative of light time with respect
!!              to time is now accounted for in the computation
!!              of observer-target velocities, for all types
!!              of reference frames.
!!
!!           2) The derivative of light time with respect
!!              to time is now accounted for in the computation of the
!!              rate of change of orientation of time-dependent
!!              reference frames for the output state. This rate of
!!              change affects observer-target velocities.
!!
!!        When stellar aberration corrections are used, velocities
!!        now reflect the rate of range of the stellar aberration
!!        correction.
!!
!!        This routine was modified as follows:
!!
!!           - SPKAPP is no longer called; it has been superseded
!!             by SPKACS. Aberration-corrected states relative to
!!             inertial frames are computed by SPKACS.
!!
!!           - The effect of the rate of change of light time on the
!!             rate of change of orientation of non-inertial output
!!             frames is accounted for in this routine. See the code
!!             near the end of this source file.
!!
!!        The header of this routine has been updated to reflect the
!!        upgrades described here.
!!
!!        As a separate upgrade, the method by which the aberration
!!        correction flag is parsed has been made more robust: parsing
!!        is now done by the routine ZZZPRSCOR. The new parsing
!!        technique calls for parsing the input string only when it
!!        differs from the previous value.
!!
!!-    SPICELIB Version 4.1.0, 05-JAN-2005 (NJB)
!!
!!        Tests of routine FAILED() were added. The new checks
!!        are intended to prevent arithmetic operations from
!!        being performed with uninitialized or invalid data.
!!
!!        Minor header error was corrected.
!!
!!-    SPICELIB Version 3.1.0, 09-JUL-1996 (WLT)
!!
!!        Corrected the description of LT in the Detailed Output
!!        section of the header.
!!
!!-    SPICELIB Version 3.0.0, 26-MAY-1995 (WLT)
!!
!!        The routine was upgraded so that it can now support
!!        non-inertial reference frames. In additions some
!!        of the error messages were slightly enhanced.
!!
!!-    SPICELIB Version 2.1.1, 5-AUG-1994 (HAN) (MJS)
!!
!!        Added code so that routine accepts lower case, mixed case
!!        and upper case versions of the string ABCORR.
!!
!!-    SPICELIB Version 2.0.0, 18-JUL-1991 (JEM) (NJB)
!!
!!        The previous version of SPKEZ could not
!!        compute the geometric state (no aberration
!!        correction) of one body with respect to
!!        another if the ephemeris data for each
!!        body relative to the Solar System Barycenter
!!        (body 0) had not been loaded. Now, if
!!        sufficient data is loaded, SPKEZ can always
!!        compute the state.
!!
!!        For example, suppose the file GLL.BSP contains
!!        segments of SPK data for the Galileo spacecraft
!!        (body -77) relative to the Jupiter Barycenter
!!        (body 5) over a period of time. If SPKEZ Version
!!        1.0.0 was called to compute the geometric state of
!!        -77 relative to 5 (or vice versa), a routine that
!!        SPKEZ calls, SPKSSB, would signal an error stating
!!        that there is insufficient data for computing the
!!        state of body 5 (relative to 0). Version 1.0.0
!!        of SPKEZ could not compute the requested state even
!!        though sufficient data had been loaded.
!!
!!        It is necessary to compute the states of each
!!        of the target and observing bodies relative to
!!        the solar system barycenter when aberration
!!        corrections are being applied. However, when
!!        computing geometric states, it is only necessary
!!        to trace back to the first common node. Positive
!!        side effects include the maintenance of precision
!!        and reduction in number of look ups.
!!
!!        The changes to the code in SPKEZ involved calling a new
!!        routine, SPKGEO, which computes the geometric state if
!!        no aberration corrections are requested.
!!
!!        The other cosmetic changes include the removal of a reference
!!        to the SPK User's Guide in Literature_References because
!!        the User's Guide is the same as SPK Required Reading.
!!
!!        Also, the item in Restrictions previously said
!!
!!           1) The ephemeris files to be used by SPKEZ must be loaded
!!              by SPKLEF before SPKSSB is called.
!!
!!        SPKSSB was replaced with SPKEZ.
!!
!!        The location of the position and velocity information in the
!!        output state vector argument STARG is now spelled out.
!!
!!        Finally, the Particulars section was updated. In Version
!!        1.0.0 it said that calling SPKEZ was equivalent to calling
!!        SPKSSB and SPKAPP.
!!
!!     SPICELIB functions
!!
!!     Local parameters
!!
!!     Saved frame name length.
!!
!!     Local variables
!!
!!     Saved frame name/ID item declarations.
!!
!!     Saved variables
!!
!!     Initial values
!!
!!     Standard SPICE error handling.
!!
      use pepper_kinds, only: ip, wp
      use pepper_math, only: mxvg, vsclip
      use pepper_inc_frmtyp
      use pepper_inc_zzabcorr
      use pepper_inc_zzctr
      implicit none
      character(*), parameter :: rname = 'ZZSPKEZ1'
      integer(ip), parameter :: frnmln = 32
      real(wp) :: state    ( 6    )
      real(wp) :: ltcent
      integer(ip) :: ltsign
      real(wp) :: dltctr
      real(wp) :: dlt
      real(wp) :: temp     ( 6    )
      logical :: attblk ( nabcor )
      logical :: found
      integer(ip) :: i
      real(wp) :: xform    ( 6, 6 )
      integer(ip) :: type
      real(wp) :: stobs    ( 6    )
      integer(ip) :: typeid
      integer(ip) :: reqfrm
      integer(ip) :: center
      if ( .not. allocated ( ctx%zzspkez1_state ) ) then
         allocate ( ctx%zzspkez1_state )
         call init_zzspkez1_state ( ctx%zzspkez1_state )
      end if
      associate ( prvcor => ctx%zzspkez1_state%prvcor, &
                  fj2000 => ctx%zzspkez1_state%fj2000, &
                  usegeo => ctx%zzspkez1_state%usegeo, &
                  xmit => ctx%zzspkez1_state%xmit, &
                  svctr1 => ctx%zzspkez1_state%svctr1, &
                  svref => ctx%zzspkez1_state%svref, &
                  svreqf => ctx%zzspkez1_state%svreqf, &
                  first => ctx%zzspkez1_state%first )
      if ( ctx%failed() ) return

!
!     Counter initialization is done separately.
!
      if ( first ) then

!
!        Initialize counter.
!
         call zzctruin( ctx, svctr1 )

      end if

      if (  first  .or.  ( abcorr /= prvcor )  ) then
!
!        The aberration correction flag differs from the value it
!        had on the previous call, if any. Analyze the new flag.
!
         call zzvalcor ( ctx, abcorr, attblk )

         if ( ctx%failed() ) return

!
!        The aberration correction flag is recognized; save it.
!
         prvcor = abcorr

!
!        Set logical flags indicating the attributes of the requested
!        correction:
!
!           XMIT is .TRUE. when the correction is for transmitted
!           radiation.
!
!           USEGEO indicates geometric state computation.
!
!        The above definitions are consistent with those used by
!        ZZVALCOR.
!
         xmit    =  attblk ( xmtidx )
         usegeo  =  attblk ( geoidx )

!
!        Get the frame ID for J2000 on the first call to this routine.
!
         if ( first ) then

            call namfrm( ctx, 'J2000', fj2000 )

            first   = .false.

         end if

      end if

!
!     If we only want a geometric state, then use SPKGEO to compute
!     just that.
!
!     Otherwise, if REF is inertial, compute the state of the target
!     relative to the observer via SPKACS. If REF is non-inertial,
!     compute the requested state in the J2000 frame, then transform it
!     to the frame designated by REF.
!
      if ( usegeo ) then

         call zzspkgo1 ( ctx, targ, et, ref, obs, starg, lt )

      else
!
!        Get the auxiliary information about the requested output
!        frame.
!
         call zznamfrm ( ctx, svctr1, svref, svreqf, ref, reqfrm )

         if ( reqfrm == 0 ) then
            call ctx%setmsg ( 'The requested output frame ''#'' is ' &
            //            'not recognized by the reference frame ' &
            //            'subsystem. Please check that the ' &
            //            'appropriate kernels have been loaded ' &
            //            'and that you have correctly entered ' &
            //            'the name of the output frame. ' )
            call ctx%errch  ( '#', ref )
            call ctx%sigerr ( 'SPICE(UNKNOWNFRAME)'  )
            return

         end if

         call frinfo ( ctx, reqfrm, center, type, typeid, found )

         if ( ctx%failed() ) return

         if ( .not. found ) then

            call ctx%setmsg ( 'The requested output frame ''#'' is ' &
            //            'not recognized by the reference frame ' &
            //            'subsystem. Please check that the ' &
            //            'appropriate kernels have been loaded ' &
            //            'and that you have correctly entered ' &
            //            'the name of the output frame. ' )
            call ctx%errch  ( '#', ref )
            call ctx%sigerr ( 'SPICE(UNKNOWNFRAME2)' )

            return

         end if

!
!        If we are dealing with an inertial frame, we can simply
!        call SPKACS and return.
!
         if ( type == inertl ) then

            call zzspkac1 ( ctx, targ, et, ref, abcorr, obs, starg, lt, dlt )
            return

         end if
!
!        Still here?
!
!        We are dealing with a non-inertial frame. But we need to do
!        light time and stellar aberration corrections in an inertial
!        frame. Get the "apparent" state of TARG in the intermediary
!        inertial reference frame J2000.
!
!        We also need the light time to the center of the frame.
!        We compute that first so that we can re-use the temporary
!        variable STATE when we compute the inertial apparent state
!        of the target relative to the observer.
!
         call zzspkac1 ( ctx, targ, et,   'J2000', abcorr, &
                         obs,  state, lt,     dlt    )

         if ( ctx%failed() ) return

         if ( center == obs ) then

            ltcent = 0.0_wp
            dltctr = 0.0_wp

         else if ( center == targ ) then

            ltcent = lt
            dltctr = dlt

         else

            call zzspksb1 ( ctx, obs,    et,     'J2000',         stobs  )
            call zzspklt1 ( ctx, center, et,     'J2000', abcorr, stobs, &
                            temp,   ltcent, dltctr                  )
         end if

!
!        If something went wrong (like we couldn't get the state of
!        the center relative to the observer) now it is time to quit.
!
         if ( ctx%failed() ) return

!
!        If the aberration corrections are for transmission, make the
!        sign of the light time positive, since we wish to compute the
!        orientation of the non-inertial frame at an epoch later than
!        ET by the one-way light time.
!
         if ( xmit ) then

            ltsign =  1
         else
            ltsign = -1
         end if

!
!        Get the state transformation from J2000 to the requested frame
!        and convert the state.
!
         call zzfrmch1 ( ctx, fj2000, reqfrm, et + ltsign*ltcent, xform )

         if ( ctx%failed() ) return

!
!        There's a tricky bit here:  since XFORM is evaluated
!        at time
!
!           ET + LTSIGN*LTCENT
!
!        XFORM is actually dependent on LTCENT. We need to account for
!        this dependency in our velocity transformation.
!
!        Let P and V be the target position and velocity respectively,
!        and R, DR be the rotation and rotation derivative
!        corresponding to XFORM.
!
!        The state transformation we need to perform is not
!
!           R * V   +  DR * P
!
!        but rather
!
!           R * V   +  ( (1 + LTSIGN*DLTCTR) * DR )  * P
!
!        So we'll scale the derivative block of XFORM accordingly.
!
         do i = 1, 3

            call vsclip ( 1.0_wp + ltsign*dltctr, xform(4,i))

         end do

!
!        Now apply the frame transformation XFORM to produce the
!        state expressed relative to the request frame REQFRM.
!
         call mxvg ( xform, state, 6, 6, starg )

      end if

      return
      end associate
      end procedure zzspkez1

      module procedure zzdynfr0
!! ## Abstract
!!
!!     SPICE Private routine intended solely for the support of SPICE
!!     routines.  Users should not call this routine directly due
!!     to the volatile nature of this routine.
!!
!!     For a specified dynamic frame, find the transformation
!!     that maps states from the dynamic frame to its base frame.
!!
!! ## Required Reading
!!
!!     CK
!!     FRAMES
!!     PCK
!!     SPK
!!
!! ## Keywords
!!
!!     FRAMES
!!     PRIVATE
!!
!! ## Declarations
!!
!! ## Detailed Input
!!
!!     INFRAM      is the frame ID code for a dynamic reference frame.
!!                 Note that this interface differs from that of TKFRAM,
!!                 which uses a class ID to identify the frame.
!!
!!                 In this routine, we refer this frame both as the
!!                 "input frame" and the "defined frame."
!!
!!     CENTER      is NAIF ID code for the center of the frame
!!                 designated by INFRAM.  This code, although derivable
!!                 from INFRAM, is passed in for convenience.
!!
!!     ET          is an epoch in ephemeris seconds past J2000 for which
!!                 the caller requests a state transformation matrix.
!!
!! ## Detailed Output
!!
!!     XFORM       is a 6x6 matrix that transforms states relative to
!!                 INFRAM to states relative to BASFRM.
!!
!!     BASFRM      is the frame ID code of the base frame associated
!!                 with INFRAM.  The 6x6 matrix XFORM transforms states
!!                 relative to INFRAM to states relative to BASFRM. The
!!                 state transformation is performed by left-multiplying
!!                 by XFORM a state expressed relative to INFRAM. This
!!                 is easily accomplished via the subroutine call shown
!!                 below.
!!
!!                    CALL MXVG ( XFORM, STATE, 6, 6, OSTATE )
!!
!! ## Parameters
!!
!!     See include file zzdyn.inc.
!!
!! ## Exceptions
!!
!!     1)  If a dynamic frame evaluation requires unavailable kernel
!!         data, the error will be diagnosed by routines in the call
!!         tree of this routine.
!!
!!     2)  If a precession model is used to implement a frame centered
!!         at a body for which the model is not applicable, the error
!!         SPICE(INVALIDSELECTION) will be signaled.
!!
!!     3)  If a nutation model is used to implement a frame centered
!!         at a body for which the model is not applicable, the error
!!         SPICE(INVALIDSELECTION) will be signaled.
!!
!!     4)  If an obliquity model is used to implement a frame centered
!!         at a body for which the model is not applicable, the error
!!         SPICE(INVALIDSELECTION) will be signaled.
!!
!!     5)  If an unrecognized precession model is specified, the
!!         error SPICE(NOTSUPPORTED) is signaled.
!!
!!     6)  If an unrecognized nutation model is specified, the
!!         error SPICE(NOTSUPPORTED) is signaled.
!!
!!     7)  If an unrecognized obliquity model is specified, the
!!         error SPICE(NOTSUPPORTED) is signaled.
!!
!!     8)  If an attempt to look up the center of a frame does
!!         not yield data, the error SPICE(FRAMEDATANOTFOUND) is
!!         signaled.
!!
!!     9)  In a two-vector frame definition, if a constant vector
!!         specification method is not recognized, the error
!!         SPICE(NOTSUPPORTED) is signaled.
!!
!!     10) In a two-vector frame definition, if a vector definition
!!         method is not recognized, the error SPICE(NOTSUPPORTED)
!!         is signaled.
!!
!!     11) If an unrecognized dynamic frame family is specified, the
!!          error SPICE(NOTSUPPORTED) is signaled.
!!
!!     12) If an unrecognized dynamic frame definition style is
!!         specified, the error SPICE(NOTSUPPORTED) is signaled.
!!
!!     13) If an unrecognized dynamic frame rotation state is
!!         specified, the error SPICE(NOTSUPPORTED) is signaled.
!!
!!     14) If both a freeze epoch and a rotation state are specified,
!!         the error SPICE(FRAMEDEFERROR) is signaled.
!!
!!     15) If neither a freeze epoch nor a rotation state are specified
!!         for an "of date" frame, the error SPICE(FRAMEDEFERROR) is
!!         signaled.
!!
!!     16) In a two-vector frame definition, if an invalid axis
!!         specification is encountered, the error SPICE(INVALIDAXIS) is
!!         signaled.
!!
!!     17) In a two-vector frame definition using a target near point
!!         vector, if the body-fixed frame associated with the target
!!         is not found, the error SPICE(FRAMEDATANOTFOUND) is signaled.
!!
!!     18) If the state of the near point on a target as seen from
!!         an observer cannot be computed, the error
!!         SPICE(DEGENERATECASE) is signaled.
!!
!!     19) If a dynamic frame evaluation requires excessive recursion
!!         depth, the error will be diagnosed by routines in the
!!         call tree of this routine.
!!
!!     20) When a two-vector dynamic frame is evaluated, if the
!!         primary and secondary vectors have angular separation less
!!         than the minimum allowed value, or if the angular separation
!!         differs from Pi by less than the minimum allowed value, the
!!         error SPICE(DEGENERATECASE) is signaled.  The default minimum
!!         separation is given by the parameter LBSEP; this value may be
!!         overridden by supplying a different value in the frame
!!         definition.
!!
!!     21) If invalid units occur in a frame definition, the error
!!         will be diagnosed by a routine in the call tree of this
!!         routine.
!!
!!     22) If an invalid Euler axis sequence occurs in a frame
!!         definition, the error will be diagnosed by a routine in the
!!         call tree of this routine.
!!
!! ## Files
!!
!!     1) SPK files containing data for each observer and target
!!        are required to support two-vector frames.  Note that
!!        observer-target pairs can be implicit, as in the case
!!        of a constant vector whose frame is evaluated at a
!!        light-time corrected epoch:  the light time the frame
!!        center to an observer must be computable in this case,
!!        which implies the state of the frame center as seen by
!!        the observer must be computable.
!!
!!     2) Any of SPK, CK, PCK, and frame kernels will also be required
!!        if any frames referenced in the definition of INFRAM (as a
!!        base frame, velocity vector frame, or constant vector frame)
!!        require them, or if any vectors used to define INFRAM require
!!        these data in order to be computable.
!!
!!     3) When CK data are required, one or more associated SCLK kernels
!!        ---normally, one kernel per spacecraft clock---are
!!        required as well.  A leapseconds kernel may be required
!!        whenever an SCLK kernel is required.
!!
!!     4) When a two-vector frame is defined using a target near point,
!!        a PCK file giving orientation and providing a triaxial shape
!!        model for the target body is required.
!!
!! ## Particulars
!!
!!     Currently only parameterized dynamic frames are supported by
!!     this routine.
!!
!!     Currently supported parameterized dynamic families are:
!!
!!        Two-vector
!!        ==========
!!
!!           Vector definitions
!!           ------------------
!!           Observer-target position
!!           Observer-target velocity
!!           Near point on target
!!           Constant vector in specified frame
!!
!!        Mean Equator and Equinox of Date
!!        ================================
!!
!!           Bodies and models
!!           -----------------
!!           Earth:  1976 IAU precession model
!!
!!        Mean Ecliptic and Equinox of Date
!!        ================================
!!
!!           Bodies and models
!!           -----------------
!!           Earth:  1976 IAU precession model
!!                   1980 IAU mean obliquity model
!!
!!        True Equator and Equinox of Date
!!        ================================
!!
!!           Bodies and models
!!           -----------------
!!           Earth:  1976 IAU precession model
!!                   1980 IAU nutation model
!!
!!        Euler frames
!!        ============
!!
!!           Euler angle definitions
!!           -----------------------
!!           Polynomial
!!
!!        Product frames
!!        ==============
!!
!!           Product definitions
!!           -------------------
!!           Array of "from" frames
!!           Array of "to" frames
!!
!! ## Examples
!!
!!     See FRMGET.
!!
!! ## Restrictions
!!
!!     1) This is a SPICE private routine; the routine is subject
!!        to change without notice.  User applications should not
!!        call this routine.
!!
!!     2) Many numerical problems can occur when dynamic frames
!!        are evaluated.  Users must determine whether dynamic frame
!!        definitions are suitable for their applications. See the
!!        Exceptions section for a list of possible problems.
!!
!!     3) Use of aberration corrections may lead to severe loss of
!!        accuracy in state transformation derivatives.
!!
!!     4) Two-vector frame definitions can suffer extreme loss of
!!         precision due to near-singular geometry.
!!
!!     5) Two-vector frame definitions involving velocity vectors
!!        require numerical differentiation in order to compute the
!!        derivative of the state transformation.  Such derivatives may
!!        have low accuracy.
!!
!!     6)  If a body RADII vector has other than exactly thee elements,
!!         the error SPICE(INVALIDCOUNT) is signaled by a routine in
!!         the call tree of this routine.
!!
!!     7)  If a body RADII vector has any element less-than or
!!         equal to zero, the error SPICE(BADAXISLENGTH) is signaled by
!!         a routine in the call tree of this routine.
!!
!! ## Literature References
!!
!!     None.
!!
!! ## Author and Institution
!!
!!     N.J. Bachman    (JPL)
!!
!! ## Version
!!
!!-    SPICELIB Version 2.0.0, 09-OCT-2021 (NJB) (EDW)
!!
!!        Support for product frames was added. Re-ordered header
!!        sections.
!!
!!        Body radii accessed from kernel pool using ZZGFTREB.
!!
!!        Bug fix: aberration correction strings for observer-target
!!        velocity vectors are now parsed using ZZVALCOR. This improves
!!        error checking. Note that aberration correction strings for
!!        constant vectors used in two-vector frame specifications are
!!        still parsed using ZZPRSCOR.
!!
!!        Bug fix: calls to FAILED after calls to ZZDYNVAC were added.
!!
!!        Added some FAILED() calls.
!!
!!-    SPICELIB Version 1.1.1, 02-MAR-2010 (NJB)
!!
!!        Typo in Brief_I/O section was corrected: "Class ID"
!!        was changed to "Frame ID." Corrected order of header
!!        sections.
!!
!!-    SPICELIB Version 1.1.0, 23-OCT-2005 (NJB)
!!
!!        Parameters KWX, KWY, KWZ were renamed to KVX, KVY, KVZ.
!!
!!        Call to ZZBODVCD was replaced with call to BODVCD.
!!
!!-    SPICELIB Version 1.0.0, 10-JAN-2005 (NJB)
!!
!!     SPICELIB functions
!!
!!     Local parameters
!!
!!     Local variables
!!
!!     Saved variables
!!
!!     Initial values
!!
      use pepper_kinds, only: ip, wp
      use pepper_arrays, only: cleard, isrchc, moved
      use pepper_math, only: latrec, mxmg, mxvg, polyds, vequ, vhat, vminug, vpack, vsep, vsubg
      use pepper_strings, only: cmprss, eqstr, intstr, ucase
      use pepper_inc_frmtyp
      use pepper_inc_zzabcorr
      use pepper_inc_zzdyn
      implicit none
      real(wp) :: epoch_1(1)  !! one-element copy of epoch (array argument)
      real(wp) :: minsep_1(1)  !! one-element copy of minsep (array argument)
      real(wp) :: dec_1(1)  !! one-element copy of dec (array argument)
      real(wp) :: ra_1(1)  !! one-element copy of ra (array argument)
      real(wp) :: lat_1(1)  !! one-element copy of lat (array argument)
      real(wp) :: lon_1(1)  !! one-element copy of lon (array argument)
      real(wp) :: t0_1(1)  !! one-element copy of t0 (array argument)
      character(*), parameter :: rname = 'ZZDYNFR0'
      integer(ip), parameter :: timlen = 50
      integer(ip), parameter :: vnamln = 4
      character(corlen)     abcorr
      character(kvlen)      axname
      character(frnmln)     basnam
      character(frnmln)     cfrmnm
      character(bdnmln)     ctrnam
      character(corlen)     cvcorr
      character(kvlen)      dynstl
      character(kvlen)      dynfam
      character(frnmln)     fframs ( mxnfac )
      character(frnmln)     inname
      character(kvlen)      nutmod
      character(kvlen)      oblmod
      character(kvlen)      prcmod
      character(kvlen)      rotsta
      character(kvlen)      spec
      character(frnmln)     tframs ( mxnfac )
      character(timlen)     timstr
      character(kvlen)      tmpfam
      character(kvlen)      units
      character(kvlen)      vecdef ( 2 )
      character(frnmln)     velfrm
      character(vnamln), parameter :: vname  ( 2 ) = &
         [ character(vnamln) :: kwpri, kwsec ]
      real(wp)              acc    ( 3 )
      real(wp)              angles ( 2 )
      real(wp)              coeffs ( maxcof, 3 )
      real(wp)              ctrpos ( 3 )
      real(wp)              dec
      real(wp)              delta
      real(wp)              dirvec ( 3 )
      real(wp)              dmob
      real(wp)              epoch
      real(wp)              eulang ( 6 )
      real(wp)              fet
      real(wp)              lat
      real(wp)              lon
      real(wp)              lt
      real(wp)              minsep
      real(wp)              mob
      real(wp)              nutxf  ( 6, 6 )
      real(wp)              oblxf  ( 6, 6 )
      real(wp)              poly   ( 0 : 1 )
      real(wp)              precxf ( 6, 6 )
      real(wp)              ra
      real(wp)              radii  ( 3 )
      real(wp)              s2     ( 6, 2 )
      real(wp)              sep
      real(wp)              stalt  ( 2 )
      real(wp)              stemp  ( 6 )
      real(wp)              stnear ( 6 )
      real(wp)              stobs  ( 6 )
      real(wp)              tarray ( 3 )
      real(wp)              t0
      real(wp)              varray ( 3, 3 )
      real(wp)              vet
      real(wp)              vflt
      real(wp)              xf2000 ( 6, 6 )
      real(wp)              xfinv  ( 6, 6 )
      real(wp)              xftemp ( 6, 6 )
      real(wp)              xipm   ( 6, 6 )
      real(wp)              xout   ( 6, 6 )
      integer(ip)           axis   ( 2 )
      integer(ip)           cfrmid
      integer(ip)           cvobs
      integer(ip)           degs   ( 3 )
      integer(ip)           frcid
      integer(ip)           frcls
      integer(ip)           frctr
      integer(ip)           frid
      integer(ip)           fromid
      integer(ip)           i
      integer(ip)           iaxes  ( 3 )
      integer(ip)           j
      integer(ip)           m
      integer(ip)           n
      integer(ip)           obs
      integer(ip)           targ
      integer(ip)           toid
      logical               corblk ( nabcor )
      logical               fnd
      logical               frozen
      logical               inert
      logical               meanec
      logical               meaneq
      logical               negate
      logical               ofdate
      logical               trueeq
      if ( .not. allocated ( ctx%zzdynfr0_state ) ) then
         allocate ( ctx%zzdynfr0_state )
         call init_zzdynfr0_state ( ctx%zzdynfr0_state )
      end if
      associate ( axes => ctx%zzdynfr0_state%axes, &
                  earth => ctx%zzdynfr0_state%earth, &
                  first => ctx%zzdynfr0_state%first, &
                  j2000 => ctx%zzdynfr0_state%j2000, &
                  itmabc => ctx%zzdynfr0_state%itmabc, &
                  itmaxe => ctx%zzdynfr0_state%itmaxe, &
                  itmcof => ctx%zzdynfr0_state%itmcof, &
                  itmdec => ctx%zzdynfr0_state%itmdec, &
                  itmfrm => ctx%zzdynfr0_state%itmfrm, &
                  itmlat => ctx%zzdynfr0_state%itmlat, &
                  itmlon => ctx%zzdynfr0_state%itmlon, &
                  itmobs => ctx%zzdynfr0_state%itmobs, &
                  itmra => ctx%zzdynfr0_state%itmra, &
                  itmsep => ctx%zzdynfr0_state%itmsep, &
                  itmspc => ctx%zzdynfr0_state%itmspc, &
                  itmtrg => ctx%zzdynfr0_state%itmtrg, &
                  itmunt => ctx%zzdynfr0_state%itmunt, &
                  itmvdf => ctx%zzdynfr0_state%itmvdf, &
                  itmvec => ctx%zzdynfr0_state%itmvec )
      if ( ctx%failed() ) return

      if ( first ) then
!
!        Get the ID code for the J2000 frame.
!
         call irfnum ( ctx, 'J2000', j2000 )

!
!        Get the ID code for the earth (we needn't check the found
!        flag).
!
         call bodn2c ( ctx, 'EARTH', earth, fnd )

!
!        Initialize "item" strings used to create kernel variable
!        names.
!
         do i = 1, 2
!
!           Vector axis:
!
            itmaxe(i) = vname(i) // kwvaxi
!
!           Vector definition:
!
            itmvdf(i) = vname(i) // kwvcdf
!
!           Vector aberration correction:
!
            itmabc(i) = vname(i) // kwvabc
!
!           Vector frame:
!
            itmfrm(i) = vname(i) // kwvfrm
!
!           Vector observer:
!
            itmobs(i) = vname(i) // kwvobs
!
!           Vector target:
!
            itmtrg(i) = vname(i) // kwvtrg
!
!           Vector longitude:
!
            itmlon(i) = vname(i) // kwlon
!
!           Vector latitude:
!
            itmlat(i) = vname(i) // kwlat
!
!           Vector right ascension:
!
            itmra(i)  = vname(i) // kwra
!
!           Vector declination:
!
            itmdec(i) = vname(i) // kwdec
!
!           Vector units:
!
            itmunt(i) = vname(i) // kwunit
!
!           Constant vector coordinate specification:
!
            itmspc(i) = vname(i) // kwvspc
!
!           Constant vector in Cartesian coordinates, literal value:
!
            itmvec(i) = vname(i) // kwvect

         end do

         first = .false.

      end if

!
!     Initialize the output arguments.
!
      call cleard ( 36, xform )

      basfrm = 0

!
!     Initialize certain variables to ensure that we don't do
!     arithmetic operations using bogus, possibly large,
!     undefined values.
!
      call cleard ( 36, nutxf  )
      call cleard ( 36, oblxf  )
      call cleard ( 36, precxf )
      call cleard ( 36, xf2000 )
      call cleard ( 36, xfinv  )
      call cleard ( 36, xipm   )

      mob    = 0.0_wp
      dmob   = 0.0_wp
      t0     = 0.0_wp
      frozen = .false.

!
!     Get the input frame name.
!
      call frmnam ( ctx, infram, inname )

!
!     We need the name of the base frame.
!
      call zzdynfid ( ctx, inname, infram, kwbfrm, basfrm )

      if ( ctx%failed() ) return

      call frmnam   ( ctx, basfrm, basnam )

!
!     The output frame code and name are set.
!
!     Look up the dynamic frame definition style from the kernel pool.
!     The kernel variable's name might be specified by name or ID.
!
      call zzdynvac ( ctx, inname, infram, kwstyl, 1, n, dynstl )

      if ( ctx%failed() ) return

!
!     At this time, the only supported dynamic frame definition style is
!     PARAMETERIZED.
!
      if (  eqstr( dynstl, kvparm )  ) then
!
!        Parameterized dynamic frames belong to families.  Look up
!        the family for this frame.
!
         call zzdynvac ( ctx, inname, infram, kwffam, 1, n, dynfam )

         if ( ctx%failed() ) return

         call cmprss ( ' ', 0, dynfam, tmpfam )
         call ucase  ( tmpfam,         dynfam )

!
!        Determine whether we have an "of-date" frame family.
!        The logical flags used here and respective meanings are:
!
!           MEANEQ   Mean equator and equinox of date
!           TRUEEQ   True equator and equinox of date
!           MEANEC   Mean ecliptic and equinox of date
!
         meaneq  =  dynfam == kvmeqt
         trueeq  =  dynfam == kvteqt
         meanec  =  dynfam == kvmecl

         ofdate  =  meaneq .or. meanec .or. trueeq

!
!        Set the evaluation epoch T0.  Normally this epoch is ET,
!        but if the frame is frozen, the freeze epoch from the
!        frame definition is used.
!
!        Read the freeze epoch into T0 if a freeze epoch was
!        specified; let FROZEN receive the FOUND flag value
!        returned by ZZDYNOAD.
!
         call zzdynoad ( ctx, inname, infram, kwfrez, 1, n, t0_1, frozen )
         t0 = t0_1(1)

         if ( ctx%failed() ) return

         if ( .not. frozen ) then
!
!           Normal case:  just use the input epoch.
!
            t0 = et

         end if

!
!        Look up the rotation state keyword.  Rather than checking
!        FAILED() after every call, we'll do it after we're
!        done with processing the rotation state.
!
         call zzdynoac ( ctx, inname, infram, kwrsta, 1, n, rotsta, fnd )

         if ( ctx%failed() ) return

!
!        If the frame is frozen, the rotation state keyword *must be
!        absent*.
!
         if ( frozen .and. fnd ) then

            call ctx%setmsg ( 'Definition of frame # contains both ' // &
                          '# and # '                             // &
                          'keywords; at most one of these must ' // &
                          'be present in the frame definition. ' // &
                          'This situation is usually caused by ' // &
                          'an error in a frame kernel in which ' // &
                          'the frame is defined.'                )
            call ctx%errch  ( '#',  inname                           )
            call ctx%errch  ( '#',  kwfrez                           )
            call ctx%errch  ( '#',  kwrsta                           )
            call ctx%sigerr ( 'SPICE(FRAMEDEFERROR)'                 )
            return

         end if

!
!        If the frame belongs to an "of date" family, either the
!        rotation state must be specified or the frame must be
!        frozen.
!
         if (  ofdate  .and.  (.not. frozen) .and. (.not. fnd )  ) then

            call ctx%setmsg ( 'Definition of frame #, which belongs to ' // &
                          'parameterized dynamic frame family #, '   // &
                          'contains neither # nor # keywords; '      // &
                          'frames in this family require exactly '   // &
                          'one of these in their frame definitions. '// &
                          'This situation is usually caused by '     // &
                          'an error in a frame kernel in which '     // &
                          'the frame is defined.'                    )
            call ctx%errch  ( '#',  inname                               )
            call ctx%errch  ( '#',  dynfam                               )
            call ctx%errch  ( '#',  kwfrez                               )
            call ctx%errch  ( '#',  kwrsta                               )
            call ctx%sigerr ( 'SPICE(FRAMEDEFERROR)'                     )
            return

         end if

!
!        Set the rotation state logical flag indicating whether
!        the state is 'INERTIAL'.
!
         if ( fnd ) then
!
!           A rotation state keyword was found.
!
!           We know the state is not frozen if we arrive here.
!
            inert  =  eqstr ( rotsta, kvinrt )

            if ( .not. inert ) then
!
!              Catch invalid rotation states here.
!
               if (  .not.  eqstr ( rotsta, kvrotg )  ) then

                  call ctx%setmsg ( 'Definition of frame # contains '     // &
                                '# specification #. The only valid '  // &
                                'rotation states are # or #. '        // &
                                'This situation is usually caused by '// &
                                'an error in a frame kernel in which '// &
                                'the frame is defined.'               )
                  call ctx%errch  ( '#',  inname                          )
                  call ctx%errch  ( '#',  kwrsta                          )
                  call ctx%errch  ( '#',  rotsta                          )
                  call ctx%errch  ( '#',  kvrotg                          )
                  call ctx%errch  ( '#',  kvinrt                          )
                  call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'                 )
                  return

               end if

            end if

         else
!
!           The state is not inertial unless there's a ROTATION_STATE
!           keyword assignment telling us it is.
!
            inert  = .false.

         end if

!
!        INERT and FROZEN are both set. The evaluation epoch T0 is also
!        set.
!
!        The following code block performs actions specific to
!        the various dynamic frame families.
!
         if ( ofdate ) then
!
!           Fetch the name of the true equator and equinox of date
!           precession model.
!
            call zzdynvac ( ctx, inname, infram, kwprcm, 1, n, prcmod )

            if ( ctx%failed() ) return

!
!           Get the precession transformation.
!
            if (  eqstr( prcmod, kvm001 )  ) then
!
!              This is the 1976 IAU earth precession model.
!
!              Make sure the center of the input frame is the earth.
!
               if ( center /= earth ) then

                  call bodc2n ( ctx, center, ctrnam, fnd )

                  if ( .not. fnd ) then
                     call intstr ( center, ctrnam )
                  end if

                  call ctx%setmsg ( 'Definition of frame # specifies '    // &
                                'frame center # and precession '      // &
                                'model #. This precession model '     // &
                                'is not applicable to body #. '       // &
                                'This situation is usually caused by '// &
                                'an error in a frame kernel in which '// &
                                'the frame is defined.'               )
                  call ctx%errch  ( '#',  inname                          )
                  call ctx%errch  ( '#',  ctrnam                          )
                  call ctx%errch  ( '#',  kvm001                          )
                  call ctx%errch  ( '#',  ctrnam                          )
                  call ctx%sigerr ( 'SPICE(INVALIDSELECTION)'             )
                  return

               end if
!
!              Look up the precession transformation.
!
               call zzeprc76 ( ctx, t0, precxf )

!
!              If we're in the mean-of-date case, invert this
!              transformation to obtain the mapping from the
!              mean-of-date frame to J2000.
!
               if ( meaneq ) then
                  call invstm ( ctx, precxf, xftemp )
               end if

            else

               call ctx%setmsg ( 'Definition of frame # specifies '    // &
                             'precession model #, which is not '   // &
                             'recognized. '                        // &
                             'This situation is usually caused by '// &
                             'an error in a frame kernel in which '// &
                             'the frame is defined.'               )
               call ctx%errch  ( '#',  inname                          )
               call ctx%errch  ( '#',  prcmod                          )
               call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'                 )
               return

            end if

!
!           At this point the precession transformation PRECXF is set.
!           If INFRAM is a mean equator and equinox of date frame, the
!           inverse of PRECXF is currently stored in XFTEMP.

            if ( trueeq ) then
!
!              We need a nutation transformation as well. Get the name
!              of the nutation model.
!
               call zzdynvac ( ctx, inname, infram, kwnutm, 1, n, nutmod )

               if ( ctx%failed() ) return

!
!              Get the nutation transformation.
!
               if (  eqstr( nutmod, kvm002 )  ) then
!
!                 This is the 1980 IAU earth nutation model.
!
!                 Make sure the center is the earth.
!
                  if ( center /= earth ) then

                     call bodc2n ( ctx, center, ctrnam, fnd )

                     if ( .not. fnd ) then
                        call intstr ( center, ctrnam )
                     end if

                     call ctx%setmsg ( 'Definition of frame # specifies ' // &
                                   'frame center # and nutation '     // &
                                   'model #. This '                   // &
                                   'nutation model is not applicable '// &
                                   'to body #.  This situation is '   // &
                                   'usually caused by an error in a ' // &
                                   'frame kernel in which '           // &
                                   'the frame is defined.'            )
                     call ctx%errch  ( '#',  inname                       )
                     call ctx%errch  ( '#',  ctrnam                       )
                     call ctx%errch  ( '#',  kvm002                       )
                     call ctx%errch  ( '#',  ctrnam                       )
                     call ctx%sigerr ( 'SPICE(INVALIDSELECTION)'          )
                     return

                  end if

!
!                 Look up the nutation transformation.
!
                  call zzenut80 ( ctx, t0, nutxf )

!
!                 Find the transformation from the J2000 frame to the
!                 earth true of date frame.  Invert.
!
                  call mxmg   ( nutxf, precxf, 6, 6, 6, xfinv )
                  call invstm ( ctx, xfinv, xftemp )

               else

                  call ctx%setmsg ( 'Definition of frame # specifies '    // &
                                'nutation model #, which is not '     // &
                                'recognized. '                        // &
                                'This situation is usually caused by '// &
                                'an error in a frame kernel in which '// &
                                'the frame is defined.'               )
                  call ctx%errch  ( '#',  inname                          )
                  call ctx%errch  ( '#',  nutmod                          )
                  call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'                 )
                  return

               end if

            else if ( meanec ) then
!
!              We need a mean obliquity transformation as well.
!              Get the name of the obliquity model.
!
               call zzdynvac ( ctx, inname, infram, kwobqm, 1, n, oblmod )

               if ( ctx%failed() ) return

!
!              Get the obliquity transformation.
!
               if (  eqstr( oblmod, kvm003 )  ) then
!
!                 This is the 1980 IAU earth mean obliquity of
!                 date model.
!
!                 Make sure the center is the earth.
!
                  if ( center /= earth ) then

                     call bodc2n ( ctx, center, ctrnam, fnd )

                     if ( .not. fnd ) then
                        call intstr ( center, ctrnam )
                     end if

                     call ctx%setmsg ( 'Definition of frame # specifies ' // &
                                   'frame center # and obliquity '    // &
                                   'model #.  This obliquity model '  // &
                                   'is not applicable to body #. '    // &
                                   'This situation is '               // &
                                   'usually caused by an error in a ' // &
                                   'frame kernel in which '           // &
                                   'the frame is defined.'            )
                     call ctx%errch  ( '#',  inname                       )
                     call ctx%errch  ( '#',  ctrnam                       )
                     call ctx%errch  ( '#',  kvm003                       )
                     call ctx%errch  ( '#',  ctrnam                       )
                     call ctx%sigerr ( 'SPICE(INVALIDSELECTION)'          )
                     return

                  end if

!
!                 Create the obliquity transformation. First look up
!                 the obliquity state (angle and angular rate).
!
                  call zzmobliq ( ctx, t0, mob, dmob )

!
!                 The obliquity rotation is about the mean-of-date
!                 x-axis.  The other Euler angles are identically
!                 zero; the axes are arbitrary, as long as the
!                 middle axis is distinct from the other two.
!
                  call cleard ( 6, eulang )
                  eulang(3) = mob
                  eulang(6) = dmob

                  call eul2xf ( ctx, eulang, 1, 3, 1, oblxf )

!
!                 Find the transformation from the J2000 to the
!                 earth mean ecliptic of date frame.  Invert.
!
                  call mxmg   ( oblxf, precxf, 6, 6, 6, xfinv )
                  call invstm ( ctx, xfinv, xftemp )

               else

                  call ctx%setmsg ( 'Definition of frame # specifies '    // &
                                'obliquity model #, which is not '    // &
                                'recognized. '                        // &
                                'This situation is usually caused by '// &
                                'an error in a frame kernel in which '// &
                                'the frame is defined.'               )
                  call ctx%errch  ( '#',  inname                          )
                  call ctx%errch  ( '#',  oblmod                          )
                  call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'                 )
                  return

               end if

            end if

!
!           At this point, XFTEMP contains the mapping from the
!           specified mean of date or true of date frame to J2000.
!
!           If the base frame is not J2000, we must find the
!           transformation from J2000 to the base frame.
!
            if ( basfrm /= j2000 ) then

               call zzfrmch1 ( ctx, j2000,  basfrm, t0,      xf2000 )
               call mxmg     ( xf2000, xftemp, 6, 6, 6, xform  )

            else
!
!              Otherwise, XFTEMP is the matrix we want.
!
               call moved ( xftemp, 36, xform )

            end if

!
!           Now XFORM is the state transformation mapping from
!           the input frame INFRAM to the base frame BASFRM.
!
!           This is the end of the work specific to "of-date" frames.
!           From here we drop out of the IF block.  At the end of this
!           routine, the derivative block of XFORM will be zeroed out
!           if the frame is frozen.  If the rotation state is
!           'INERTIAL', we will make sure the transformation between
!           the defined frame and the J2000 frame has time derivative
!           zero.
!

         else if ( dynfam == kv2vec ) then
!
!           The frame belongs to the TWO-VECTOR family.
!
!           Initialize the array S2.
!
            call cleard ( 12, s2 )

!
!           Fetch the specifications of the primary and secondary
!           axes.
!
            do i = 1, 2
!
!              Get the name of the axis associated with the Ith
!              defining vector.
!
               call zzdynvac ( ctx, inname, infram, itmaxe(i), 1, n, axname )

               if ( ctx%failed() ) return

               call cmprss ( ' ', 0, axname, axname )
               call ucase  ( axname,         axname )

!
!              Set the sign flag associated with the axis.
!
               negate  =  axname(1:1) == '-'

               call cmprss ( '-', 0, axname, axname )
               call cmprss ( '+', 0, axname, axname )

               axis(i) = isrchc ( axname, 3, axes )

               if ( axis(i) == 0 ) then

                  call ctx%setmsg ( 'Definition of frame # associates '   // &
                                'vector # with axis #.  The only '    // &
                                'valid axis values are { X, -X, Y, '  // &
                                '-Y, Z, -Z }. '                       // &
                                'This situation is usually caused by '// &
                                'an error in a frame kernel in which '// &
                                'the frame is defined.'               )
                  call ctx%errch  ( '#',  inname                          )
                  call ctx%errint ( '#',  i                               )
                  call ctx%errch  ( '#',  axname                          )
                  call ctx%sigerr ( 'SPICE(INVALIDAXIS)'                  )
                  return

               end if

!
!              Find out how the vector is defined:
!
!                 - Observer-target position vector
!                 - Observer-target velocity vector
!                 - Observer-target near point vector
!                 - Constant vector
!
!              VECDEF(I) indicates the vector definition method
!              for the Ith vector.
!
               call zzdynvac ( ctx, inname,  infram,    itmvdf(i), &
                               1,       n,         vecdef(i) )

               if ( ctx%failed() ) return

               call cmprss ( ' ',    0, vecdef(i), vecdef(i) )
               call ucase  ( vecdef(i),            vecdef(i) )

               if (  vecdef(i) == kvposv  ) then
!
!                 The vector is the position of a target relative
!                 to an observer.
!
!                 We need a target, observer, and aberration correction.
!
                  call zzdynbid ( ctx, inname, infram, itmtrg(i), targ )

                  call zzdynbid ( ctx, inname, infram, itmobs(i), obs  )

                  call zzdynvac ( ctx, inname, infram, itmabc(i), &
                                  1,      n,      abcorr     )

!
!                 Look up the Ith state vector in the J2000 frame.
!
                  call zzspkez1 ( ctx, targ,    t0,   'J2000', &
                                  abcorr,  obs,  s2(1,i),  lt )

                  if ( ctx%failed() ) return

!
!                 At this point, S2(*,I) contains position and
!                 velocity relative to frame J2000.
!

               else if ( vecdef(i) == kvvelv ) then
!
!                 The vector is the velocity of a target relative
!                 to an observer.
!
!                 We need a target, observer, and aberration correction.
!
                  call zzdynbid ( ctx, inname, infram, itmtrg(i), targ )

                  call zzdynbid ( ctx, inname, infram, itmobs(i), obs  )

                  call zzdynvac ( ctx, inname, infram, itmabc(i), &
                                  1,      n,      abcorr     )

!
!                 We need to know the frame in which the velocity is
!                 defined.
!
                  call zzdynfid ( ctx, inname, infram, itmfrm(i), frid )
                  call frmnam   ( ctx, frid,   velfrm  )

                  if ( ctx%failed() ) return

!
!                 Obtain the velocity vector in the specified velocity
!                 frame.  Also obtain bracketing vectors to support
!                 discrete differentiation.  (See notes in zzdyn.inc
!                 regarding definition of DELTA.)
!
                  delta = max ( 1.0_wp,  (2.0_wp**qexp) * t0 )

                  call vpack ( t0-delta, t0, t0+delta, tarray )

                  do j = 1, 3

                     call zzspkez1 ( ctx, targ,   tarray(j), velfrm, &
                                     abcorr, obs,       stemp,  lt )
!
!                    We compute the derivative using unit
!                    velocity vectors.
!
                     if ( ctx%failed() ) return

                     call vhat ( stemp(4), varray(1,j) )

                  end do

!
!                 Compute acceleration and fill in the velocity state
!                 vector S2(*,I).
!
                  call qderiv ( ctx, 3, &
                                varray(1,1),  varray(1,3),  delta, acc )

                  call vequ   ( varray(1,2),  s2(1,i) )
                  call vequ   ( acc,          s2(4,i) )

!
!                 We need the epoch VET at which VELFRM is evaluated.
!                 This epoch will be used to transform the velocity's
!                 "state" vector from VELFRM to J2000.
!
!                 Set the default value of VET here.
!
                  vet = t0

!
!                 Parse the aberration correction.  Find the epoch used
!                 to evaluate the velocity vector's frame.
!
                  call zzvalcor ( ctx, abcorr, corblk )

                  if ( corblk(ltidx) ) then
!
!                    Light time correction is used.  The epoch used
!                    to evaluate the velocity vector's frame depends
!                    on the frame's observer and center.
!
!                    Look up the velocity frame's center.
!
                     call frinfo ( ctx, frid, frctr, frcls, frcid, fnd )

                     if ( .not. fnd ) then

                        call ctx%setmsg ( 'In definition of frame #, ' // &
                                      'the frame associated with ' // &
                                      'a velocity vector has '     // &
                                      'frame ID code #, but no '   // &
                                      'frame center, frame class, '// &
                                      'or frame class ID was '     // &
                                      'found by FRINFO.  This '    // &
                                      'situation MAY be caused '   // &
                                      'by an error in a frame '    // &
                                      'kernel in which the frame ' // &
                                      'is defined. The problem '   // &
                                      'also could be indicative '  // &
                                      'of a SPICELIB bug.'         )
                        call ctx%errch  ( '#',  inname                 )
                        call ctx%errint ( '#',  frid                   )
                        call ctx%sigerr ( 'SPICE(FRAMEDATANOTFOUND)'   )
                        return

                     end if

!
!                    If the velocity frame is non-inertial, we'll need
!                    to compute the evaluation epoch for this frame.
!
                     if ( frcls /= inertl ) then
!
!                       Obtain light time from the observer to the
!                       frame's center; find the evaluation epoch VET
!                       for the frame.

                        call zzspkzp1 ( ctx, frctr,  t0,  'J2000', &
                                        abcorr, obs, ctrpos,  vflt )
                        call zzcorepc ( ctx, abcorr, t0,  vflt,    vet  )

                        if ( ctx%failed() ) return

                     end if

                  end if

!
!                 The velocity frame's evaluation epoch VET is now set.
!
!                 We must rotate the velocity vector and transform the
!                 acceleration from the velocity frame (evaluated at
!                 VET) to the output frame at T0.  We'll do this in two
!                 stages, first mapping velocity and acceleration into
!                 the J2000 frame.
!
                  if ( frid /= j2000 ) then

                     call zzfrmch1 ( ctx, frid,    j2000,  vet,   xf2000  )

                     if ( ctx%failed() ) return

                     call mxvg     ( xf2000,  s2(1,i), 6, 6, stemp   )
                     call moved    ( stemp,   6,             s2(1,i) )

                  end if

!
!                 At this point, S2(*,I) contains velocity and
!                 acceleration relative to frame J2000.
!

               else if ( vecdef(i) == kvnear ) then
!
!                 The vector points from an observer to the
!                 sub-observer point (nearest point to the observer) on
!                 the target body.
!
!                 We need a target, observer, and aberration correction.
!
                  call zzdynbid ( ctx, inname, infram, itmtrg(i), targ )

                  call zzdynbid ( ctx, inname, infram, itmobs(i), obs  )

                  call zzdynvac ( ctx, inname, infram, itmabc(i), &
                                  1,      n,      abcorr     )

!
!                 The vector points from the observer to the nearest
!                 point on the target. We need the state of the near
!                 point relative to the observer.
!
!                 We'll look up the state of the target center relative
!                 to the observer and the state of the near point
!                 relative to the target center, both in the body-fixed
!                 frame associated with the target.
!
!                 Look up the body-fixed frame associated with the
!                 target body.
!
                  call cidfrm ( ctx, targ, cfrmid, cfrmnm, fnd )

                  if ( .not. fnd ) then

                     call ctx%setmsg ( 'Definition of frame # requires ' // &
                                   'definition of body-fixed frame ' // &
                                   'associated with target body #. ' // &
                                   'A call to CIDFRM indicated no '  // &
                                   'body-fixed frame is associated ' // &
                                   'with the target body.  This '    // &
                                   'situation can arise when a '     // &
                                   'frame kernel defining the '      // &
                                   'target''s body-fixed frame  '    // &
                                   'lacks the OBJECT_<ID>_FRAME '    // &
                                   'or OBJECT_<name>_FRAME '         // &
                                   'keywords.  The problem also '    // &
                                   'could be caused by an '          // &
                                   'error in a frame kernel in '     // &
                                   'which the parameterized '        // &
                                   'two-vector dynamic frame # '     // &
                                   'is defined.'                     )
                     call ctx%errch  ( '#',  inname                      )
                     call ctx%errint ( '#',  targ                        )
                     call ctx%errch  ( '#',  inname                      )
                     call ctx%sigerr ( 'SPICE(FRAMEDATANOTFOUND)'        )
                     return

                  end if

!
!                 Get the radii of the target body.
!
                  call zzgftreb ( ctx, targ, radii )

!
!                 Look up the Ith state vector in the target-fixed
!                 frame.  Negate the vector to obtain the target-to-
!                 observer vector.
!
                  call zzspkez1 ( ctx, targ,    t0,       cfrmnm, &
                                  abcorr,  obs,      stemp,    lt )

!
!                 We check FAILED() here because VMINUG is a simple
!                 arithmetic routine that doesn't return on entry.
!
                  if ( ctx%failed() ) return

                  call vminug   ( stemp,   6,        stobs )

                  call dnearp   ( ctx, stobs,   radii(1), radii(2), radii(3), &
                                  stnear,  stalt,    fnd               )

                  if ( ctx%failed() ) return

                  if ( .not. fnd ) then

                     call ctx%setmsg ( 'In definition of frame #, '      // &
                                   'vector # is defined by the '     // &
                                   'near point on body # as seen '   // &
                                   'from body #.  The state of this '// &
                                   'near point was not found. '      // &
                                   'See the routine DNEARP for '     // &
                                   'an explanation.  This '          // &
                                   'situation MAY be caused '        // &
                                   'by an error in a frame '         // &
                                   'kernel in which the frame '      // &
                                   'is defined. The problem '        // &
                                   'also could be indicative '       // &
                                   'of a SPICELIB bug.'              )
                     call ctx%errch  ( '#',  inname                      )
                     call ctx%errint ( '#',  i                           )
                     call ctx%errint ( '#',  targ                        )
                     call ctx%errint ( '#',  obs                         )
                     call ctx%sigerr ( 'SPICE(DEGENERATECASE)'           )
                     return

                  end if

!
!                 Find the observer-near point state in the target
!                 body-fixed frame.
!
                  call vsubg ( stnear, stobs, 6, stemp  )

!
!                 Transform the state to frame J2000.  To get the
!                 required transformation matrix, we'll need to obtain
!                 the epoch associated with CNMFRM.  Parse the
!                 aberration correction and adjust the frame evaluation
!                 epoch as needed.
!
                  call zzcorepc ( ctx, abcorr, t0, lt, fet )

!
!                 Obtain the matrix for transforming state vectors
!                 from the target center frame to the J2000 frame and
!                 apply it to the observer-to-near point state vector.
!
                  call zzfrmch1 ( ctx, cfrmid,  j2000,  fet,   xipm    )

                  if ( ctx%failed() ) return

                  call mxvg     ( xipm,    stemp,  6, 6,  s2(1,i) )

!
!                 At this point, S2(*,I) contains position and
!                 velocity of the near point on the target as
!                 seen by the observer, relative to frame J2000.
!

               else if ( vecdef(i) == kvcons ) then
!
!                 The vector is constant in a specified frame.
!
!                 We need a 3-vector and an associated reference
!                 frame relative to which the vector is specified.
!
!                 Look up the ID of the frame first.
!
                  call zzdynfid ( ctx, inname, infram, itmfrm(i), frid )

!
!                 Let FET ("frame ET") be the evaluation epoch for
!                 the constant vector's frame.  By default, this
!                 frame is just T0, but if we're using light time
!                 corrections, FET must be adjusted for one-way
!                 light time between the frame's center and the
!                 observer.
!
!                 Set the default value of FET here.
!
                  fet = t0

!
!                 Optionally, there is an aberration correction
!                 associated with the constant vector's frame.
!                 If so, an observer must be associated with the
!                 frame.  Look up the correction first.
!
                  call zzdynoac ( ctx, inname, infram, itmabc(i), &
                                  1,      n,      cvcorr,    fnd )

                  if ( .not. fnd ) then
                     cvcorr = 'NONE'
                  end if

                  call zzprscor ( ctx, cvcorr, corblk )

                  if ( ctx%failed() ) return

                  if ( .not. corblk(geoidx) ) then
!
!                    We need to apply an aberration correction to
!                    the constant vector.
!
!                    Check for errors in the aberration correction
!                    specification.
!
!                       - Light time and stellar aberration corrections
!                         are mutually exclusive.
!
                     if ( corblk(ltidx) .and. corblk(stlidx) ) then

                        call ctx%setmsg ( 'Definition of frame # '       // &
                                      'specifies aberration '        // &
                                      'correction # for constant '   // &
                                      'vector.  Light time and '     // &
                                      'stellar aberration '          // &
                                      'corrections are mutually '    // &
                                      'exclusive for constant '      // &
                                      'vectors used in two-vector '  // &
                                      'parameterized dynamic frame ' // &
                                      'definitions.  This situation '// &
                                      'is usually caused by an '     // &
                                      'error in a frame kernel in '  // &
                                      'which the frame is defined.'  )
                        call ctx%errch  ( '#',  inname                   )
                        call ctx%errch  ( '#',  cvcorr                   )
                        call ctx%sigerr ( 'SPICE(INVALIDOPTION)'         )
                        return

                     end if

                     if ( corblk(ltidx) ) then
!
!                       Light time correction is used.  The epoch used
!                       to evaluate the constant vector's frame depends
!                       on the frame's observer and center.
!
!                       Look up the constant vector frame's center.
!
                        call frinfo ( ctx, frid, frctr, frcls, frcid, fnd )

                        if ( .not. fnd ) then

                           call ctx%setmsg ( 'In definition of frame #, ' // &
                                         'the frame associated with ' // &
                                         'a constant vector has '     // &
                                         'frame ID code #, but no '   // &
                                         'frame center, frame class, '// &
                                         'or frame class ID was '     // &
                                         'found by FRINFO.  This '    // &
                                         'situation MAY be caused '   // &
                                         'by an error in a frame '    // &
                                         'kernel in which the frame ' // &
                                         'is defined. The problem '   // &
                                         'also could be indicative '  // &
                                         'of a SPICELIB bug.'         )
                           call ctx%errch  ( '#',  inname                 )
                           call ctx%errint ( '#',  frid                   )
                           call ctx%sigerr ( 'SPICE(FRAMEDATANOTFOUND)'   )
                           return

                        end if
!
!                       If the constant vector frame is non-inertial,
!                       we'll need to compute the evaluation epoch for
!                       this frame.
!
                        if ( frcls /= inertl ) then
!
!                          Look up the observer associated with the
!                          constant vector's frame.  This observer,
!                          together with the frame's center, determines
!                          the evaluation epoch for the frame.
!
                           call zzdynbid ( ctx, inname,    infram, &
                                           itmobs(i), cvobs   )

!
!                          Obtain light time from the observer to the
!                          frame's center.
!
                           call zzspkzp1 ( ctx, frctr,  t0,   'J2000', &
                                           cvcorr, cvobs, ctrpos,  lt )

!
!                          Re-set the evaluation epoch for the frame.
!
                           call zzcorepc ( ctx, cvcorr, t0, lt, fet )

                           if ( ctx%failed() ) return

                        end if
!
!                       The constant vector frame's evaluation epoch
!                       FET has been set.
!

                     else if ( corblk(stlidx) ) then
!
!                       Stellar aberration case.
!
!                       The constant vector must be corrected for
!                       stellar aberration induced by the observer's
!                       velocity relative to the solar system
!                       barycenter.  First, find this velocity in
!                       the J2000 frame.  We'll apply the correction
!                       later, when the constant vector has been
!                       transformed to the J2000 frame.
!
                        call zzdynbid ( ctx, inname,    infram, &
                                        itmobs(i), cvobs   )

                        call zzspksb1 ( ctx, cvobs, t0, 'J2000', stobs )

                        if ( ctx%failed() ) return

                     end if

                  end if

!
!                 At this point FET is the frame evaluation epoch
!                 for the frame associated with the constant vector.
!
!                 If stellar aberration correction has been specified,
!                 STOBS is the state of the observer relative to the
!                 solar system barycenter, expressed in the J2000
!                 frame.
!
!                 Get the constant vector specification.
!
                  call zzdynvac ( ctx, inname,  infram,  itmspc(i), &
                                  1,       n,       spec       )

                  if ( ctx%failed() ) return

                  call cmprss ( ' ', 0, spec, spec )
                  call ucase  ( spec,         spec )

                  if ( spec == kvrecc ) then
!
!                    The coordinate system is rectangular.
!
!                    Look up the constant vector.
!
                     call zzdynvad ( ctx, inname, infram, itmvec(i), &
                                     3,      n,      dirvec    )

                  else if (      ( spec == kvlatc ) &
                            .or. ( spec == kvradc ) ) then
!
!                    The coordinate system is latitudinal or RA/DEC.
!
!                    Look up the units associated with the angles.
!
                     call zzdynvac ( ctx, inname,  infram,  itmunt(i), &
                                     1,       n,       units     )

                     if ( spec == kvlatc ) then
!
!                       Look up longitude and latitude.
!
                        call zzdynvad ( ctx, inname, infram, itmlon(i), 1, n, lon_1 )
                        lon = lon_1(1)

                        call zzdynvad ( ctx, inname, infram, itmlat(i), 1, n, lat_1 )
                        lat = lat_1(1)

!
!                       Convert angles from input units to radians.
!
                        call convrt ( ctx, lon, units, kvradn, angles(1) )
                        call convrt ( ctx, lat, units, kvradn, angles(2) )

                     else
!
!                       Look up RA and DEC.
!
                        call zzdynvad ( ctx, inname, infram, itmra(i), 1, n, ra_1 )
                        ra = ra_1(1)

                        call zzdynvad ( ctx, inname, infram, itmdec(i), 1, n, dec_1 )
                        dec = dec_1(1)

!
!                       Convert angles from input units to radians.
!
                        call convrt ( ctx, ra,  units, kvradn, angles(1) )
                        call convrt ( ctx, dec, units, kvradn, angles(2) )

                     end if

                     if ( ctx%failed() ) return

!
!                    Now  produce a direction vector.
!
                     call latrec ( 1.0_wp, angles(1), angles(2), dirvec )

                  else

                     call ctx%setmsg ( 'Definition of two-vector '       // &
                                   'parameterized dynamic frame # '  // &
                                   'includes constant vector '       // &
                                   'specification #, which is '      // &
                                   'not supported.  This situation ' // &
                                   'is usually caused by an error '  // &
                                   'in a frame kernel in which '     // &
                                   'the frame is defined.'           )
                     call ctx%errch  ( '#',  inname                      )
                     call ctx%errch  ( '#',  spec                        )
                     call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'             )
                     return

                  end if

!
!                 At this point, the Cartesian coordinates of the
!                 vector relative to the constant vector frame
!                 are stored in DIRVEC.
!
!                 Convert the direction vector to the J2000 frame.
!                 Fill in the state vector.  The velocity in the
!                 constant vector's frame is zero.
!
                  call vequ     ( dirvec, s2(1,i) )
                  call cleard   ( 3,      s2(4,i) )

                  if ( frid /= j2000 ) then

                     call zzfrmch1 ( ctx, frid, j2000, fet, xipm )

                     if ( ctx%failed() ) return

                     call mxvg     ( xipm,   s2(1,i), 6, 6,  stemp   )
                     call moved    ( stemp,  6,              s2(1,i) )

                  end if

!
!                 The state of the constant vector is now represented
!                 in the J2000 frame, but we may still need to
!                 apply a stellar aberration correction.
!
                  if ( corblk(stlidx) ) then
!
!                    Perform the stellar aberration correction
!                    appropriate to the radiation travel sense.

                     if ( corblk(xmtidx) ) then
!
!                       The correction is for transmission.
!
                        call stlabx ( ctx, s2(1,i), stobs(4), stemp )

                     else
!
!                       The correction is for reception.
!
                        call stelab ( ctx, s2(1,i), stobs(4), stemp )

                     end if

                     if ( ctx%failed() ) return

!
!                    Update the position portion of S2(*,I).
!
                     call vequ ( stemp, s2(1,i) )

                  end if

!
!                 At this point, S2(*,I) contains position and velocity
!                 of the constant (constant relative to its associated
!                 frame, that is) vector as seen by the observer,
!                 relative to frame J2000.
!

               else

                  call ctx%setmsg ( 'Definition of two-vector '       // &
                                'parameterized dynamic frame # '  // &
                                'includes vector '                // &
                                'definition #, which is '         // &
                                'not supported.  This situation ' // &
                                'is usually caused by an error '  // &
                                'in a frame kernel in which '     // &
                                'the frame is defined.'           )
                  call ctx%errch  ( '#',  inname                      )
                  call ctx%errch  ( '#',  vecdef(i)                   )
                  call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'             )
                  return

               end if

!
!              Negate the state vector if the axis has negative sign.
!
               if ( negate ) then
                  call vminug ( s2(1,i), 6, stemp   )
                  call moved  ( stemp,   6, s2(1,i) )
               end if

            end do

!
!           Look up the lower bound for the angular separation of
!           the defining vectors.  Use the default value if none
!           was supplied.
!
            call zzdynoad ( ctx, inname, infram, itmsep, 1, n, minsep_1, fnd )
            minsep = minsep_1(1)

            if ( ctx%failed() ) return

            if ( .not. fnd ) then
               minsep = lbsep
            end if

!
!           Now use our states to compute our state transformation
!           matrix.
!
!           Check the angular separation of the defining vectors. We
!           want to ensure that the vectors are not too close to being
!           linearly dependent.  We can handle both cases---separation
!           close to 0 or separation close to Pi---by comparing the
!           sine of the separation to the sine of the separation limit.
!
            sep = vsep ( s2(1,1), s2(1,2) )

            if ( sin(sep) < sin(minsep) ) then

               call etcal  ( ctx, t0, timstr )

               call ctx%setmsg ( 'Angular separation of vectors defining '// &
                             'two-vector parameterized dynamic frame '// &
                             '# is # (radians); minimum allowed diff' // &
                             'erence of separation from 0 or Pi is # '// &
                             'radians.  Evaluation epoch is #.  Extre'// &
                             'me loss of precision can occur when '   // &
                             'defining vectors are nearly linearly '  // &
                             'dependent.  This type of error can be ' // &
                             'due to using a dynamic frame outside of'// &
                             ' the time range for which it is meant. '// &
                             'It also can be due to a conceptual '    // &
                             'error pertaining to the frame''s defin' // &
                             'ition, or to an implementation error '  // &
                             'in the frame kernel containing the '    // &
                             'frame definition. However, if you wish '// &
                             'to proceed with this computation, '     // &
                             'the # keyword can be used in '          // &
                             'the frame definition to adjust the '    // &
                             'separation limit.'                      )
               call ctx%errch  ( '#',  inname                             )
               call ctx%errdp  ( '#',  sep                                )
               call ctx%errdp  ( '#',  minsep                             )
               call ctx%errch  ( '#',  timstr                             )
               call ctx%errch  ( '#',  kwatol                             )
               call ctx%sigerr ( 'SPICE(DEGENERATECASE)'                  )
               return

            end if

!
!           We have both states expressed relative to frame J2000
!           at this point.  Find the transformation from INNAME to
!           the frame J2000, then from J2000 to frame BASNAM.
!
            call zztwovxf ( ctx, s2(1,1), axis(1), s2(1,2), axis(2), xform )

            if ( basfrm /= j2000 ) then

               call moved ( xform, 36, xftemp )

               call zzfrmch1 ( ctx, j2000, basfrm, t0, xf2000 )

               if ( ctx%failed() ) return

               call mxmg ( xf2000,  xftemp,  6,  6,  6, xform  )

            end if

!
!           This is the end of the work specific to two-vector frames.
!           From here we drop out of the IF block.  At the end of this
!           routine, the derivative block of XFORM will be zeroed out
!           if the frame is frozen.  If the rotation state is
!           'INERTIAL', we will make sure the transformation between
!           the defined frame and the J2000 frame has time derivative
!           zero.
!

         else if ( dynfam == kveulr ) then
!
!           The frame belongs to the Euler family.
!
!           We expect to see specifications of an axis sequence, units,
!           and angles via polynomial coefficients.  We also expect
!           to see an ET epoch.
!
!           Look up the epoch first.  Let DELTA represent the offset
!           of T0 relative to the epoch.
!
!           Initialize EPOCH so subtraction doesn't overflow if EPOCH
!           is invalid due to a lookup error.
!
            epoch = 0.0_wp

            call zzdynvad ( ctx, inname, infram, kwepoc, 1, n, epoch_1 )
            epoch = epoch_1(1)

            delta = t0 - epoch

!
!           Now the axis sequence.
!
            call zzdynvai ( ctx, inname, infram, kweuax, 3, n, iaxes )

!
!           Now the coefficients for the angles.
!
            do i = 1, 3
!
!              Initialize N so subtraction doesn't overflow if N
!              is invalid due to a lookup error.
!
               n = 0

               call zzdynvad ( ctx, inname,  infram,  itmcof(i), &
                               maxcof,  n,       coeffs(1,i) )

!
!              Set the polynomial degree for the Ith angle.
!
               degs(i) = n - 1

            end do

!
!           Look up the units associated with the angles.
!
            call zzdynvac ( ctx, inname, infram, kwunit, 1, n, units )

            if ( ctx%failed() ) return

!
!           Evaluate the angles and their derivatives at DELTA.  Convert
!           angles from input units to radians and radians/sec.
!
            do i = 1, 3

               call polyds ( coeffs(1,i), degs(i), 1, delta, poly )
!
!              Convert units.  Fill in the Euler angle state vector.
!
               call convrt ( ctx, poly(0), units, kvradn, eulang(i  ) )
               call convrt ( ctx, poly(1), units, kvradn, eulang(i+3) )

            end do

!
!           Produce a state transformation matrix that maps from
!           the defined frame to the base frame.
!
            call eul2xf ( ctx, eulang, iaxes(1), iaxes(2), iaxes(3), xform )

!
!           This is the end of the work specific to Euler frames.
!           From here we drop out of the IF block.  At the end of this
!           routine, the derivative block of XFORM will be zeroed out
!           if the frame is frozen.  If the rotation state is
!           'INERTIAL', we will make sure the transformation between
!           the defined frame and the J2000 frame has time derivative
!           zero.
!

         else if ( dynfam == kvprod ) then
!
!           The frame belongs to the product family.
!
!           We expect to see specifications of "from" and "to" frames
!           that make up the product.
!
!           Look up the "from" and "to" frames that define the product.
!
            call zzdynvac ( ctx, inname, infram, kwffrm, mxnfac, m, fframs )
            call zzdynvac ( ctx, inname, infram, kwtfrm, mxnfac, n, tframs )

            if ( ctx%failed() ) return

            if ( n /= m ) then

               call ctx%setmsg ( 'Definition of product ' &
               //            'parameterized dynamic frame # ' &
               //            'has # "from" frames and # "to" ' &
               //            'frames. These counts must match.' )
               call ctx%errch  ( '#',  inname                       )
               call ctx%errint ( '#',  m                            )
               call ctx%errint ( '#',  n                            )
               call ctx%sigerr ( 'SPICE(BADFRAMECOUNT)'             )
               return

            end if

!
!           The product of the factors is the transformation from the
!           base frame to the output frame. We need the inverse of
!           this transformation.
!
!           Compute the inverse of the product of the factors; place
!           the result in XFORM.
!
            call namfrm ( ctx, fframs(1), fromid )
            call namfrm ( ctx, tframs(1), toid   )

            call zzfrmch1 ( ctx, toid, fromid, t0, xform )

            if ( ctx%failed() ) return

            do i = 2, n

               call moved  ( xform, 36, xftemp )

               call namfrm ( ctx, fframs(i), fromid )
               call namfrm ( ctx, tframs(i), toid   )

               call zzfrmch1 ( ctx, toid, fromid, t0, xform )

               if ( ctx%failed() ) return

               call mxmg  ( xform, xftemp, 6, 6, 6, xout  )
               call moved ( xout,  36,              xform )

            end do

         else

            call ctx%setmsg ( 'Dynamic frame family # (in definition of '// &
                          'frame #) is not supported. '              // &
                          'This situation is usually caused by '     // &
                          'an error in a frame kernel in which '     // &
                          'the frame is defined.'                    )
            call ctx%errch  ( '#',  dynfam                               )
            call ctx%errch  ( '#',  inname                               )
            call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'                      )
            return

         end if
!
!        This is the end of the IF block that processes the
!        parameterized dynamic frame families.
!

      else

         call ctx%setmsg ( 'Dynamic frame style # (in definition of ' // &
                       'frame #) is not supported. '              // &
                       'This situation is usually caused by '     // &
                       'an error in a frame kernel in which '     // &
                       'the frame is defined.'                    )
         call ctx%errch  ( '#',  dynstl                               )
         call ctx%errch  ( '#',  inname                               )
         call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'                      )
         return

      end if

      if ( ctx%failed() ) return

!
!     At this point XFORM is the state transformation matrix mapping
!     from the input frame INFRAM to the base frame BASFRM.
!
!     If the frame has rotation state 'INERTIAL', the frame must have
!     zero derivative with respect to any inertial frame. Set the
!     derivative block accordingly.
!
      if ( inert ) then
!
!        See whether the base frame is inertial.
!
         call irfnum ( ctx, basnam, j )

         if ( j > 0 ) then
!
!           The base frame is  a recognized inertial frame.  Zero
!           out the derivative block.
!
            do i = 1, 3
               call cleard ( 3, xform(4,i) )
            end do

         else
!
!           The base frame is *not* a recognized inertial frame.
!
!           Create the state transformation matrix that maps from the
!           defined frame to J2000.  Zero out the derivative block of
!           this matrix.  Convert the resulting matrix to the state
!           transformation from the defined frame to the output frame.
!
            call zzfrmch1 ( ctx, basfrm, j2000, t0,      xf2000 )

            if ( ctx%failed() ) return

            call mxmg     ( xf2000, xform, 6, 6, 6, xftemp )

            do i = 1, 3
               call cleard ( 3, xftemp(4,i) )
            end do

!
!           XFTEMP now represents the transformation from a
!           constant frame matching the defined frame at T0 to the
!           J2000 frame. Produce the transformation from this constant
!           frame to the output frame.
!
!           To avoid introducing additional round-off error into
!           the rotation blocks of XFORM, we overwrite only the
!           derivative block of XFORM with the derivative block
!           of the "inertial" transformation.
!
            call invstm ( ctx, xf2000, xfinv )
            call mxmg   ( xfinv,  xftemp, 6, 6, 6, xout )

            do i = 1, 3
               call vequ ( xout(4,i), xform(4,i) )
            end do

         end if

      end if

!
!     If the frame is frozen, zero out the derivative block of the
!     transformation matrix.
!
      if ( frozen ) then

         do i = 1, 3
            call cleard ( 3, xform(4,i) )
         end do

      end if

!
!     XFORM and BASFRM are set.
!

      return
      end associate
      end procedure zzdynfr0

      module procedure zzfrmgt0
!! ## Abstract
!!
!!     Find the transformation from a user specified frame to
!!     another frame at a user specified epoch.
!!
!! ## Required Reading
!!
!!     FRAMES
!!
!! ## Keywords
!!
!!     FRAMES
!!
!! ## Declarations
!!
!! ## Detailed Input
!!
!!     INFRM       is the SPICE ID-code for some reference frame.
!!
!!     ET          is an epoch in ephemeris seconds past J2000 at
!!                 which the user wishes to retrieve a state
!!                 transformation matrix.
!!
!! ## Detailed Output
!!
!!     XFORM       is a 6x6 matrix that transforms states relative to
!!                 INFRM to states relative to OUTFRM. (Assuming such a
!!                 transformation can be found.)
!!
!!     OUTFRM      is the SPICE ID-code of a reference frame. The 6x6
!!                 matrix XFORM transforms states relative to INFRM to
!!                 states relative to OUTFRM. The state transformation
!!                 is achieved by multiplying XFORM on the right by a
!!                 state relative to INFRM.  This is easily accomplished
!!                 via the subroutine call shown below.
!!
!!                    CALL MXVG ( XFORM, STATE, 6, 6, OSTATE )
!!
!!     FOUND       is a logical flag indicating whether or not a
!!                 transformation matrix could be found from INFRM to
!!                 some other frame. If a transformation matrix cannot
!!                 be found OUTFRM will be set to zero, FOUND will be
!!                 set to FALSE and XFORM will be returned as the zero
!!                 matrix.
!!
!! ## Parameters
!!
!!     See include file.
!!
!! ## Exceptions
!!
!!     1) If a transformation matrix cannot be located, then FOUND will
!!        be set to FALSE, OUTFRM will be set to zero and XFORM will be
!!        set to the zero 6x6 matrix.
!!
!!     2) If the class of the requested frame is not recognized the
!!        exception 'SPICE(UNKNOWNFRAMETYPE)' will be signaled.
!!
!! ## Files
!!
!!     LSK, SCLK, PCK, FK, SPK, and/or CK kernels may need to be loaded
!!     to provide the needed frame definition and transformation data.
!!
!! ## Particulars
!!
!!     This is a low level routine used for determining a chain of state
!!     transformation matrices from one frame to another.
!!
!! ## Examples
!!
!!     See FRMCHG.
!!
!! ## Restrictions
!!
!!     None.
!!
!! ## Literature References
!!
!!     None.
!!
!! ## Author and Institution
!!
!!     N.J. Bachman    (JPL)
!!     B.V. Semenov    (JPL)
!!     W.L. Taber      (JPL)
!!
!! ## Version
!!
!!-    SPICELIB Version 5.0.0, 18-SEP-2020 (NJB)
!!
!!        Support for switch frames was added. VERSN is now
!!        initialized via a DATA statement. Corrected long error
!!        message to use the term "class" rather than "class id-code."
!!
!!-    SPICELIB Version 4.0.0, 05-JAN-2014 (BVS)
!!
!!        To prevent operations with un-initialized DP numbers, wrapped
!!        IF ( .NOT. FAILED() ) ... END IF around output matrix
!!        transposition/re-assignment operations in all branches where
!!        the routine returning the matrix might fail.
!!
!!        Added zero-ing out the output matrix in cases of a failed or
!!        .NOT. FOUND transformation look ups.
!!
!!-    SPICELIB Version 3.0.0, 18-DEC-2004 (NJB)
!!
!!        Added the new frame type 'DYN' to the list of frame
!!        types recognized by FRMGET.
!!
!!-    SPICELIB Version 2.0.0, 03-APR-1997 (WLT)
!!
!!        Added the new frame type 'TK' to the list of frame
!!        types recognized by FRMGET.  In addition the routine
!!        now checks FAILED after "getting" the frame transformation.
!!
!!-    SPICELIB Version 1.0.0, 20-OCT-1994 (WLT)
!!
!! ## Index Entries
!!
!!     Find a frame transformation matrix from a specified frame
!!
!!     SPICELIB Functions
!!
!!     Local Variables
!!
!!     Saved variables
!!
!!     Initial values
!!
!!     Set output flag.
!!
      use pepper_kinds, only: ip, wp
      use pepper_arrays, only: cleard
      use pepper_inc_frmtyp
      implicit none
      integer(ip)           center
      integer(ip)           i
      integer(ip)           j
      integer(ip)           type
      integer(ip)           typeid
      real(wp)              tsipm  ( 6, 6 )
      real(wp)              rotate ( 3, 3 )
      if ( .not. allocated ( ctx%zzfrmgt0_state ) ) then
         allocate ( ctx%zzfrmgt0_state )
         call init_zzfrmgt0_state ( ctx%zzfrmgt0_state )
      end if
      associate ( idnt66 => ctx%zzfrmgt0_state%idnt66, &
                  pass1 => ctx%zzfrmgt0_state%pass1, &
                  versn => ctx%zzfrmgt0_state%versn )
      found = .false.

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     On the first pass, initialize the identity matrix.
!
      if ( pass1 ) then

         call cleard( 36, idnt66 )

         do i = 1, 6
            idnt66(i,i) = 1.0_wp
         end do

         pass1 = .false.

      end if

!
!     Get all the needed information about this frame.
!
      call frinfo ( ctx, infrm, center, type, typeid, found )

      if ( .not. found ) then

         call cleard ( 36, xform )
         outfrm = 0

         return

      end if

!
!     FOUND was set to true by the FRINFO call. Compute transformation
!     based on the frame class.
!
      if ( type == inertl ) then

         call irfrot ( ctx, infrm, 1, rotate )

         if ( .not. ctx%failed() ) then

            do i = 1,3
               do j = 1,3
                  xform(i,  j  ) = rotate(i,j)
                  xform(i+3,j+3) = rotate(i,j)
                  xform(i+3,j  ) = 0.0_wp
                  xform(i,  j+3) = 0.0_wp
               end do
            end do

            outfrm = 1

         end if

      else if ( type == pck ) then

         call tisbod ( ctx, 'J2000', typeid, et, tsipm  )

         if ( .not. ctx%failed() ) then

            call invstm ( ctx, tsipm, xform )

            outfrm = 1

         end if

      else if ( type == ck ) then

         call ckfxfm ( ctx, typeid, et, xform, outfrm, found )

      else if ( type == tk ) then

         call tkfram  ( ctx, typeid, rotate, outfrm, found )

         if ( .not. ctx%failed() ) then

            do i = 1,3
               do j = 1,3
                  xform(i,  j  ) = rotate(i,j)
                  xform(i+3,j+3) = rotate(i,j)
                  xform(i+3,j  ) = 0.0_wp
                  xform(i,  j+3) = 0.0_wp
               end do
            end do

         end if

      else if ( type == dyn ) then
!
!        Unlike the other frame classes, the dynamic frame evaluation
!        routine ZZDYNFRM requires the input frame ID rather than the
!        dynamic frame class ID. ZZDYNFRM also requires the center ID
!        we found via the FRINFO call.
!
         call zzdynfr0 ( ctx, infrm, center, et, xform, outfrm )

!
!        The FOUND flag was set by FRINFO earlier; we don't touch
!        it here. If ZZDYNFRM signaled an error, FOUND will be set
!        to .FALSE. at end of this routine.
!
      else if ( type == swtch ) then

         call zzswfxfm ( ctx, infrm, et, 6, xform, outfrm, found )

      else

         call cleard ( 36, xform )
         outfrm = 0
         found  = .false.

         call ctx%setmsg ( 'The reference frame # has class #. ' &
         //            'This form of reference frame is not ' &
         //            'supported in version # of ZZFRMGT0. You need ' &
         //            'to update your version of SPICELIB to the ' &
         //            'latest version in order to support this ' &
         //            'frame. ' )

         call ctx%errint ( '#', infrm )
         call ctx%errint ( '#', type  )
         call ctx%errch  ( '#', versn )
         call ctx%sigerr ( 'SPICE(UNKNOWNFRAMETYPE)' )
         return

      end if

!
!     Make sure to clear outputs in case of a failure as defined in
!     in the header.
!
      if ( ctx%failed() .or. .not. found ) then

         call cleard ( 36, xform )
         outfrm = 0
         found  = .false.

      end if

      return
      end associate
      end procedure zzfrmgt0

!$Procedure      ZZFRMCH0 (Frame Change)

      module procedure zzfrmch0
!! ## Abstract
!!
!!     Return the state transformation matrix from one
!!     frame to another.
!!
!! ## Required Reading
!!
!!     None.
!!
!! ## Keywords
!!
!!     FRAMES
!!
!! ## Declarations
!!
      use pepper_kinds, only: ip, wp
      use pepper_arrays, only: isrchi
      use pepper_math, only: zzmsxf
      use pepper_inc_errhnd
      use pepper_inc_frmtyp
      implicit none



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
!     Example 2.  Suppose that you have the angular velocity, W, of some
!     rotation relative to FRAME1 at epoch ET and that you wish to
!     express this angular velocity with respect to FRAME2.  The
!     following subroutines will suffice to perform this computation.
!
!        CALL FRMCHG ( FRAME1, FRAME2, ET, STXFRM )
!
!     Recall that a state transformation matrix has the following form.
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
!     The velocity of an arbitrary point P undergoing rotation with the
!     angular velocity W is W x P
!
!     Thus the velocity of P in FRAME2 is:
!
!        dR
!        --  P    +    R (W x P )
!        dt
!
!           dR  t
!     =  (  -- R  R P   +  W  x P  )            ( 1 )
!           dt
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
!     From this final expression it follows that in FRAME2 the angular
!     velocity vector is given by [A+R*W].
!
!     The code below implements these ideas.
!
!        CALL FRMCHG ( FRAME1, FRAME2, ET, STXFRM )
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
!        FRINFO and to ZZFRMGT0.
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


!
!     Local Parameters
!
      integer(ip), parameter :: maxnod = 10

      integer(ip), parameter :: maxxfm = maxnod + 4

!
!     The root of all reference frames is J2000 (Frame ID = 1).
!
      integer(ip), parameter :: rootf = 1

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
      if ( ctx%failed() ) return

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

         return

      end if
!
!     Now perform the obvious check to make sure that both
!     frames are recognized.
!
      call frinfo ( ctx, frame1, cent, class, clssid, found )

      if ( ctx%failed() ) return

      if ( .not. found ) then

         call ctx%setmsg ( 'The number # is not a recognized id-code ' &
         //            'for a reference frame. ' )
         call ctx%errint ( '#', frame1 )
         call ctx%sigerr ( 'SPICE(UNKNOWNFRAME)'  )
         return

      end if

      call frinfo ( ctx, frame2, cent, class, clssid, found )

      if ( ctx%failed() ) return

      if ( .not. found ) then

         call ctx%setmsg ( 'The number # is not a recognized id-code ' &
         //            'for a reference frame. ' )
         call ctx%errint ( '#', frame2 )
         call ctx%sigerr ( 'SPICE(UNKNOWNFRAME)'  )
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
         call zzfrmgt0 ( ctx, frame(node),     et, &
                         trans(1,1,node), frame(node+1), found )

         if ( ctx%failed() ) return

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
         call zzfrmgt0( ctx, frame(node), et, trans(1,1,node), relto, found )

         if ( ctx%failed() ) return

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
!     TRANS(1...6, 1...6, 1    )    transforms FRAME1   to FRAME(2)
!     TRANS(1...6, 1...6, 2    )    transforms FRAME(2) to FRAME(3)
!     TRANS(1...6, 1...6, 3    )    transforms FRAME(3) to FRAME(4)
!        .
!        .
!        .
!     TRANS(1...6, 1...6, NODE-1 )  transforms FRAME(NODE-1)
!                                   to         FRAME(NODE)
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
            call zzfrmgt0 ( ctx, this, et, trans2(1,1,put), relto, found )

            if ( ctx%failed() ) return

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
            call zzfrmgt0 ( ctx, this, et, tempxf, relto, found )

            if ( ctx%failed() ) return

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

         call zznofcon ( ctx, et, frame1, frame(node), frame2, this, errmsg )

         if ( ctx%failed() ) then
!
!           We were unable to create the error message. This
!           unfortunate situation could arise if a frame kernel
!           is corrupted.
!
            return

         end if
!
!        The normal case: signal an error with a descriptive long
!        error message.
!
         call ctx%setmsg ( errmsg                  )
         call ctx%sigerr ( 'SPICE(NOFRAMECONNECT)' )
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
      call invstm ( ctx, trans2(1,1,get), trans(1,1,cmnode) )
      call zzmsxf ( trans,   cmnode, xform             )

      return

      end procedure zzfrmch0

      module procedure zzspkgo0
!! ## Abstract
!!
!!     SPICE Private routine intended solely for the support of SPICE
!!     routines.  Users should not call this routine directly due
!!     to the volatile nature of this routine.
!!
!!     Compute the geometric state (position and velocity) of a target
!!     body relative to an observing body.
!!
!! ## Required Reading
!!
!!     SPK
!!
!! ## Keywords
!!
!!     EPHEMERIS
!!
!! ## Declarations
!!
!! ## Detailed Input
!!
!!     TARG     is the standard NAIF ID code for a target body.
!!
!!     ET       is the epoch (ephemeris time) at which the state
!!              of the target body is to be computed.
!!
!!     REF      is the name of the reference frame to
!!              which the vectors returned by the routine should
!!              be rotated. This may be any frame supported by
!!              the SPICELIB subroutine FRMCHG.
!!
!!     OBS      is the standard NAIF ID code for an observing body.
!!
!! ## Detailed Output
!!
!!     STATE    is a 6-dimensional vector that contains the geometric
!!              position and velocity of the target body, relative to the
!!              observing body, at epoch ET. STATE has six elements: the
!!              first three contain the target's position; the last three
!!              contain the target's velocity. These vectors are
!!              transformed into the specified reference frame. Units are
!!              always km and km/sec.
!!
!!     LT       is the one-way light time in seconds from the
!!              observing body to the geometric position of the
!!              target body at the specified epoch.
!!
!! ## Parameters
!!
!!     None.
!!
!! ## Exceptions
!!
!!     1)  If insufficient ephemeris data has been loaded to compute
!!         the necessary states, the error SPICE(SPKINSUFFDATA) is
!!         signaled.
!!
!! ## Files
!!
!!     See $Restrictions.
!!
!! ## Particulars
!!
!!     SPKGEO computes the geometric state, T(t), of the target
!!     body and the geometric state, O(t), of the observing body
!!     relative to the first common center of motion. Subtracting
!!     O(t) from T(t) gives the geometric state of the target
!!     body relative to the observer.
!!
!!        CENTER ----- O(t)
!!            |      /
!!            |     /
!!            |    /
!!            |   /  T(t) - O(t)
!!            |  /
!!           T(t)
!!
!!     The one-way light time, tau, is given by
!!
!!               | T(t) - O(t) |
!!        tau = -----------------
!!                      c
!!
!!     For example, if the observing body is -94, the Mars Observer
!!     spacecraft, and the target body is 401, Phobos, then the
!!     first common center is probably 4, the Mars Barycenter.
!!     O(t) is the state of -94 relative to 4 and T(t) is the
!!     state of 401 relative to 4.
!!
!!     The center could also be the Solar System Barycenter, body 0.
!!     For example, if the observer is 399, Earth, and the target
!!     is 299, Venus, then O(t) would be the state of 399 relative
!!     to 0 and T(t) would be the state of 299 relative to 0.
!!
!!     Ephemeris data from more than one segment may be required
!!     to determine the states of the target body and observer
!!     relative to a common center. SPKGEO reads as many segments
!!     as necessary, from as many files as necessary, using files
!!     that have been loaded by previous calls to FURNSH or SPKLEF
!!     (load ephemeris file).
!!
!!     SPKGEO is similar to SPKEZ but returns geometric states
!!     only, with no option to make planetary (light-time) nor
!!     stellar aberration corrections. The geometric states
!!     returned by SPKEZ and SPKGEO are the same.
!!
!! ## Examples
!!
!!     The numerical results shown for this example may differ across
!!     platforms. The results depend on the SPICE kernels used as
!!     input, the compiler and supporting libraries, and the machine
!!     specific arithmetic implementation.
!!
!!     1) Return the geometric state vector of Mars (499) as seen from
!!        Earth (399) in the J2000 frame and the one-way light time
!!        between them at the epoch July 4, 2003 11:00 AM PST.
!!
!!        Use the meta-kernel shown below to load the required SPICE
!!        kernels.
!!
!!           KPL/MK
!!
!!           File: spkgeo_ex1.tm
!!
!!           This meta-kernel is intended to support operation of SPICE
!!           example programs. The kernels shown here should not be
!!           assumed to contain adequate or correct versions of data
!!           required by SPICE-based user applications.
!!
!!           In order for an application to use this meta-kernel, the
!!           kernels referenced here must be present in the user's
!!           current working directory.
!!
!!           The names and contents of the kernels referenced
!!           by this meta-kernel are as follows:
!!
!!              File name                        Contents
!!              ---------                        --------
!!              de430.bsp                        Planetary ephemeris
!!              mar097.bsp                       Mars satellite ephemeris
!!              naif0011.tls                     Leapseconds
!!
!!           \begindata
!!
!!              KERNELS_TO_LOAD = ( 'de430.bsp',
!!                                  'mar097.bsp',
!!                                  'naif0011.tls' )
!!
!!           \begintext
!!
!!           End of meta-kernel
!!
!!        Example code begins here.
!!
!!              PROGRAM SPKGEO_EX1
!!              IMPLICIT NONE
!!
!!        C
!!        C     SPICELIB functions
!!        C
!!              DOUBLE PRECISION      VNORM
!!
!!        C
!!        C     Local parameters.
!!        C
!!              INTEGER               NAMLEN
!!              PARAMETER           ( NAMLEN = 32 )
!!
!!              INTEGER               TIMLEN
!!              PARAMETER           ( TIMLEN = 26 )
!!
!!        C
!!        C     Local variables.
!!        C
!!              CHARACTER*(TIMLEN)    EPOCH
!!              CHARACTER*(NAMLEN)    REFFRM
!!
!!              DOUBLE PRECISION      ET
!!              DOUBLE PRECISION      LT
!!              DOUBLE PRECISION      STATE ( 6 )
!!
!!              INTEGER               I
!!              INTEGER               OBSRVR
!!              INTEGER               TARGET
!!
!!        C
!!        C     Load a set of kernels: an SPK file, a PCK
!!        C     file and a leapseconds file. Use a meta
!!        C     kernel for convenience.
!!        C
!!              CALL FURNSH ( 'spkgeo_ex1.tm' )
!!
!!        C
!!        C     Define parameters for a state lookup.
!!        C
!!              TARGET = 499
!!              EPOCH  = 'July 4, 2003 11:00 AM PST'
!!              REFFRM = 'J2000'
!!              OBSRVR = 399
!!
!!        C
!!        C     Convert the epoch to ephemeris time.
!!        C
!!              CALL STR2ET ( EPOCH, ET )
!!
!!        C
!!        C     Look-up the state for the defined parameters.
!!        C
!!              CALL SPKGEO ( TARGET, ET, REFFRM, OBSRVR, STATE, LT )
!!
!!        C
!!        C     Output...
!!        C
!!              WRITE(*,'(A,I3)') 'The position of    : ', TARGET
!!              WRITE(*,'(A,I3)') 'As observed from   : ', OBSRVR
!!              WRITE(*,'(2A)')   'In reference frame : ', REFFRM
!!              WRITE(*,'(2A)')   'At epoch           : ', EPOCH
!!              WRITE(*,*) ' '
!!
!!        C
!!        C     The first three entries of state contain the
!!        C     X, Y, Z position components. The final three contain
!!        C     the Vx, Vy, Vz velocity components.
!!        C
!!              WRITE(*,'(A,3F18.6)') 'R   (km):', ( STATE(I), I=1,3 )
!!              WRITE(*,'(A,3F18.6)') 'V (km/s):', ( STATE(I), I=4,6 )
!!              WRITE(*,*) ' '
!!              WRITE(*,'(A,F19.14)') 'Light time (s) between observer '
!!             .                   // 'and target: ', LT
!!
!!              END
!!
!!        When this program was executed on a Mac/Intel/gfortran/64-bit
!!        platform, the output was:
!!
!!        The position of    : 499
!!        As observed from   : 399
!!        In reference frame : J2000
!!        At epoch           : July 4, 2003 11:00 AM PST
!!
!!        R   (km):   73826216.435288  -27128030.732406  -18741973.868287
!!        V (km/s):         -6.809504          7.513814          3.001290
!!
!!        Light time (s) between observer and target:  269.70264776317532
!!
!! ## Restrictions
!!
!!     1)  The ephemeris files to be used by SPKGEO must be loaded
!!         by FURNSH or SPKLEF before SPKGEO is called.
!!
!! ## Literature References
!!
!!     None.
!!
!! ## Author and Institution
!!
!!     N.J. Bachman       (JPL)
!!     J. Diaz del Rio    (ODC Space)
!!     J.E. McLean        (JPL)
!!     B.V. Semenov       (JPL)
!!     W.L. Taber         (JPL)
!!     E.D. Wright        (JPL)
!!
!! ## Version
!!
!!-    SPICELIB Version 3.1.0, 09-OCT-2021 (JDR) (NJB)
!!
!!        Bug fix: added calls to FAILED after calls to SPKPVN.
!!        Previously only one call to SPKPVN was followed by a FAILED
!!        call. Moved some FAILED checks so they will be hit whether
!!        or not SPKSFS finds a segment.
!!
!!        Edited the header to comply with NAIF standard. Added complete
!!        code example to $Examples section. Removed unnecessary
!!        $Revisions section.
!!
!!        Added reference to FURNSH in $Particulars and $Restrictions
!!        sections.
!!
!!-    SPICELIB Version 3.0.0, 08-JAN-2014 (BVS)
!!
!!        Updated to save the input frame name and POOL state counter
!!        and to do frame name-ID conversion only if the counter has
!!        changed.
!!
!!        Updated to map the input frame name to its ID by first calling
!!        ZZNAMFRM, and then calling IRFNUM. The side effect of this
!!        change is that now the frame with the fixed name 'DEFAULT'
!!        that can be associated with any code via CHGIRF's entry point
!!        IRFDEF will be fully masked by a frame with identical name
!!        defined via a text kernel. Previously the CHGIRF's 'DEFAULT'
!!        frame masked the text kernel frame with the same name.
!!
!!        Fixed description of STATE in Detailed Output. Replaced
!!        SPKLEF with FURNSH and fixed errors in $Examples.
!!
!!-    SPICELIB Version 2.4.0, 01-SEP-2005 (NJB)
!!
!!        Updated to remove non-standard use of duplicate arguments
!!        in VADDG calls.
!!
!!-    SPICELIB Version 2.3.0, 05-JAN-2005 (NJB)
!!
!!        Tests of routine FAILED() were added.
!!
!!-    SPICELIB Version 2.2.1, 20-OCT-2003 (EDW)
!!
!!        Added mention that LT returns in seconds.
!!
!!-    SPICELIB Version 2.2.0, 11-APR-1997 (WLT)
!!
!!        The routine was modified to take advantage of the fact
!!        that most state transformation are between inertial frames.
!!        Looking up a transformation between inertial frames is
!!        substantially faster than looking up non-inertial
!!        transformations. Consequently slightly more
!!        complex code produces about a 50% increase in speed for
!!        many users.
!!
!!-    SPICELIB Version 2.1.0, 26-JUL-1996 (WLT)
!!
!!        The routine was upgraded so that potentially redundant
!!        computations are not performed.
!!
!!-    SPICELIB Version 2.0.0, 19-SEP-1995 (WLT)
!!
!!        The routine was upgraded so that it can return states
!!        relative to rotating frames.
!!
!!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!!
!!        Comment section for permuted index source lines was added
!!        following the header.
!!
!!-    SPICELIB Version 1.0.0, 18-JUL-1991 (JEM)
!!
!! ## Index Entries
!!
!!     geometric state of one body relative to another
!!
!!     This is the idea:
!!
!!     Every body moves with respect to some center. The center
!!     is itself a body, which in turn moves about some other
!!     center.  If we begin at the target body (T), follow
!!     the chain,
!!
!!                                   T
!!                                     \
!!           SSB                        \
!!               \                     C[1]
!!                \                     /
!!                 \                   /
!!                  \                 /
!!                   \               /
!!                  C[3]-----------C[2]
!!
!!     and avoid circular definitions (A moves about B, and B moves
!!     about A), eventually we get the state relative to the solar
!!     system barycenter (which, for our purposes, doesn't move).
!!     Thus,
!!
!!        T    = T     + C[1]     + C[2]     + ... + C[n]
!!         SSB    C[1]       C[2]       [C3]             SSB
!!
!!     where
!!
!!        X
!!         Y
!!
!!     is the state of body X relative to body Y.
!!
!!     However, we don't want to follow each chain back to the SSB
!!     if it isn't necessary.  Instead we will just follow the chain
!!     of the target body and follow the chain of the observing body
!!     until we find a common node in the tree.
!!
!!     In the example below, C is the first common node.  We compute
!!     the state of TARG relative to C and the state of OBS relative
!!     to C, then subtract the two states.
!!
!!                                   TARG
!!                                     \
!!           SSB                        \
!!               \                       A
!!                \                     /            OBS
!!                 \                   /              |
!!                  \                 /               |
!!                   \               /                |
!!                    B-------------C-----------------D
!!
!!     SPICELIB functions
!!
!!     Local parameters
!!
!!     CHLEN is the maximum length of a chain.  That is,
!!     it is the maximum number of bodies in the chain from
!!     the target or observer to the SSB.
!!
!!     Saved frame name length.
!!
!!     Local variables
!!
!!     Saved frame name/ID item declarations.
!!
!!     Saved frame name/ID items.
!!
!!     Initial values.
!!
!!     In-line Function Definitions
!!
!!     Standard SPICE error handling.
!!
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: clight
      use pepper_arrays, only: cleard, isrchi, moved
      use pepper_math, only: mxv, mxvg, vaddg, vnorm, vsubg
      use pepper_strings, only: frstnp, intstr, prefix, suffix
      use pepper_inc_ninert
      use pepper_inc_zzctr
      implicit none
      character(*), parameter :: rname = 'ZZSPKGO0'
      integer(ip), parameter :: chlen = 20
      integer(ip), parameter :: ssb = 0
      integer(ip), parameter :: frnmln = 32
      character(40)         ident
      character(40)         tname
      character(40)         oname
      character(80)         tstring
      real(wp)              descr  ( 5        )
      real(wp)              sobs   ( 6        )
      real(wp)              starg  ( 6, chlen )
      real(wp)              stemp  ( 6        )
      real(wp)              stxfrm ( 6, 6     )
      real(wp)              rot    ( 3, 3     )
      real(wp)              vtemp  ( 6        )
      integer(ip)           cframe
      integer(ip)           cobs
      integer(ip)           ctarg  (    chlen )
      integer(ip)           tframe (    chlen )
      integer(ip)           ctpos
      integer(ip)           handle
      integer(ip)           i
      integer(ip)           legs
      integer(ip)           nct
      integer(ip)           refid
      integer(ip)           tmpfrm
      logical               found
      logical               nofrm
      logical               isinrt
      isinrt ( tmpfrm, cframe ) =       cframe > 0 &
                                  .and. cframe <= ninert &
                                  .and. tmpfrm > 0 &
                                  .and. tmpfrm <= ninert
      if ( .not. allocated ( ctx%zzspkgo0_state ) ) then
         allocate ( ctx%zzspkgo0_state )
         call init_zzspkgo0_state ( ctx%zzspkgo0_state )
      end if
      associate ( svctr1 => ctx%zzspkgo0_state%svctr1, &
                  svref => ctx%zzspkgo0_state%svref, &
                  svrefi => ctx%zzspkgo0_state%svrefi, &
                  first => ctx%zzspkgo0_state%first )
      if ( ctx%failed() ) return

!
!     Initialization.
!
      if ( first ) then

!
!        Initialize counter.
!
         call zzctruin( ctx, svctr1 )

         first = .false.

      end if

!
!     We take care of the obvious case first.  It TARG and OBS are the
!     same we can just fill in zero.
!
      if ( targ == obs ) then

         lt = 0.0_wp

         call cleard ( 6, state )
         return

      end if

!
!     CTARG contains the integer codes of the bodies in the
!     target body chain, beginning with TARG itself and then
!     the successive centers of motion.
!
!     STARG(1,I) is the state of the target body relative
!     to CTARG(I).  The id-code of the frame of this state is
!     stored in TFRAME(I).
!
!     COBS and SOBS will contain the centers and states of the
!     observing body.  (They are single elements instead of arrays
!     because we only need the current center and state of the
!     observer relative to it.)
!
!     First, we construct CTARG and STARG.  CTARG(1) is
!     just the target itself, and STARG(1,1) is just a zero
!     vector, that is, the state of the target relative
!     to itself.
!
!     Then we follow the chain, filling up CTARG and STARG
!     as we go.  We use SPKSFS to search through loaded
!     files to find the first segment applicable to CTARG(1)
!     and time ET.  Then we use SPKPVN to compute the state
!     of the body CTARG(1) at ET in the segment that was found
!     and get its center and frame of motion (CTARG(2) and TFRAME(2).
!
!     We repeat the process for CTARG(2) and so on, until
!     there is no data found for some CTARG(I) or until we
!     reach the SSB.
!
!     Next, we find centers and states in a similar manner
!     for the observer.  It's a similar construction as
!     described above, but I is always 1.  COBS and SOBS
!     are overwritten with each new center and state,
!     beginning at OBS.  However, we stop when we encounter
!     a common center of motion, that is when COBS is equal
!     to CTARG(I) for some I.
!
!     Finally, we compute the desired state of the target
!     relative to the observer by subtracting the state of
!     the observing body relative to the common node from
!     the state of the target body relative to the common
!     node.
!
!     CTPOS is the position in CTARG of the common node.
!
!     Since the upgrade to use hashes and counter bypass ZZNAMFRM
!     became more efficient in looking up frame IDs than IRFNUM. So the
!     original order of calls "IRFNUM first, NAMFRM second" was
!     switched to "ZZNAMFRM first, IRFNUM second".
!
!     The call to IRFNUM, now redundant for built-in inertial frames,
!     was preserved to for a sole reason -- to still support the
!     ancient and barely documented ability for the users to associate
!     a frame with the fixed name 'DEFAULT' with any CHGIRF inertial
!     frame code via CHGIRF's entry point IRFDEF.
!
!     Note that in the case of ZZNAMFRM's failure to resolve name and
!     IRFNUM's success to do so, the code returned by IRFNUM for
!     'DEFAULT' frame is *not* copied to the saved code SVREFI (which
!     would be set to 0 by ZZNAMFRM) to make sure that on subsequent
!     calls ZZNAMFRM does not do a bypass (as SVREFI always forced look
!     up) and calls IRFNUM again to reset the 'DEFAULT's frame ID
!     should it change between the calls.
!
      call zznamfrm ( ctx, svctr1, svref, svrefi, ref, refid )

      if ( refid == 0 ) then
         call irfnum ( ctx, ref, refid )
      end if

      if ( refid == 0 ) then

         if (  frstnp(ref) > 0 ) then

            call ctx%setmsg ( 'The string supplied to specify the ' &
            //            'reference frame, (''#'') contains ' &
            //            'non-printing characters.  The two ' &
            //            'most common causes for this kind ' &
            //            'of error are: 1. an error in the ' &
            //            'call to ZZSPKGO0; 2. an ' &
            //            'uninitialized variable. ' )
            call ctx%errch  ( '#', ref )

         else if ( ref == ' ' ) then

            call ctx%setmsg ( 'The string supplied to specify the ' &
            //            'reference frame is blank.  The most ' &
            //            'common cause for this kind of error ' &
            //            'is an uninitialized variable. ' )

         else

            call ctx%setmsg ( 'The string supplied to specify the ' &
            //            'reference frame was ''#''.  This ' &
            //            'frame is not recognized. Possible ' &
            //            'causes for this error are: 1. failure ' &
            //            'to load the frame definition into the ' &
            //            'kernel pool; 2. An out-of-date ' &
            //            'edition of the toolkit. ' )
            call ctx%errch  ( '#', ref )

         end if

         call ctx%sigerr ( 'SPICE(UNKNOWNFRAME)'  )

         if ( ctx%failed() ) return

      end if

!
!     Fill in CTARG and STARG until no more data is found
!     or until we reach the SSB.  If the chain gets too
!     long to fit in CTARG, that is if I equals CHLEN,
!     then overwrite the last elements of CTARG and STARG.
!
!     Note the check for FAILED in the loop.  If SPKSFS
!     or SPKPVN happens to fail during execution, and the
!     current error handling action is to NOT abort, then
!     FOUND may be stuck at TRUE, CTARG(I) will never
!     become zero, and the loop will execute indefinitely.
!
!     Construct CTARG and STARG.  Begin by assigning the
!     first elements:  TARG and the state of TARG relative
!     to itself.
!
      i        =  1
      ctarg(i) =  targ
      found    = .true.

      call cleard ( 6, starg(1,i) )

      do while (           found &
                  .and.  ( i        < chlen ) &
                  .and.  ( ctarg(i) /= obs   ) &
                  .and.  ( ctarg(i) /= ssb   )  )
!
!        Find a file and segment that has state
!        data for CTARG(I).
!
         call spksfs ( ctx, ctarg(i), et, handle, descr, ident, found )

         if ( found ) then
!
!           Get the state of CTARG(I) relative to some
!           center of motion.  This new center goes in
!           CTARG(I+1) and the state is called STEMP.
!
            i = i + 1

            call spkpvn ( ctx, handle,    descr,      et, &
                          tframe(i), starg(1,i), ctarg(i) )
!
!           Here's what we have.  STARG is the state of CTARG(I-1)
!           relative to CTARG(I) in reference frame TFRAME(I)
!
         end if
!
!        If one of the routines above failed during
!        execution, we just give up and return.
!
         if ( ctx%failed() ) return

      end do

      tframe(1) = tframe(2)
!
!     If the loop above ended because we ran out of
!     room in the arrays CTARG and STARG, then we
!     continue finding states but we overwrite the
!     last elements of CTARG and STARG.
!
!     If, as a result, the first common node is
!     overwritten, we'll just have to settle for
!     the last common node.  This will cause a small
!     loss of precision, but it's better than other
!     alternatives.
!
      if ( i == chlen ) then

         do while (      found &
                   .and. ctarg(chlen) /= ssb &
                   .and. ctarg(chlen) /= obs )
!
!           Find a file and segment that has state
!           data for CTARG(CHLEN).
!
            call spksfs ( ctx, ctarg(chlen), et, handle, descr, ident, found)

            if ( found ) then
!
!              Get the state of CTARG(CHLEN) relative to
!              some center of motion.  The new center
!              overwrites the old.  The state is called
!              STEMP.
!
               call spkpvn ( ctx, handle, descr, et, tmpfrm, stemp, &
                             ctarg(chlen))

               if ( ctx%failed() ) return

!
!              Add STEMP to the state of TARG relative to
!              the old center to get the state of TARG
!              relative to the new center.  Overwrite
!              the last element of STARG.
!
               if ( tframe(chlen) == tmpfrm ) then

                  call moved ( starg(1,chlen), 6, vtemp )

               else if ( isinrt ( tframe(chlen), tmpfrm ) ) then

                  call irfrot( ctx, tframe(chlen), tmpfrm,         rot      )
                  call mxv   ( rot,           starg(1,chlen), vtemp(1) )
                  call mxv   ( rot,           starg(4,chlen), vtemp(4) )

               else

                  call zzfrmch0( ctx, tframe(chlen), tmpfrm,   et,   stxfrm )

                  if ( ctx%failed() ) return

                  call mxvg  ( stxfrm, starg (1,chlen), 6, 6, vtemp  )

               end if

               call vaddg ( vtemp,  stemp,        6, starg(1,chlen) )

               tframe(chlen) = tmpfrm

            end if

!
!           If one of the routines above failed during
!           execution, we just give up and return.
!
            if ( ctx%failed() ) return

         end do

      end if

      nct = i
!
!     NCT is the number of elements in CTARG,
!     the chain length.  We have in hand the following information
!
!        STARG(1...6,K)  state of body
!        CTARG(K-1)      relative to body CTARG(K) in the frame
!        TFRAME(K)
!
!     For K = 2,..., NCT.
!
!     CTARG(1) = TARG
!     STARG(1...6,1) = ( 0, 0, 0, 0, 0, 0 )
!     TFRAME(1)      = TFRAME(2)
!

!
!     Now follow the observer's chain.  Assign
!     the first values for COBS and SOBS.
!
      cobs = obs
      call cleard ( 6, sobs )

!
!     Perhaps we have a common node already.
!     If so it will be the last node on the
!     list CTARG.
!
!     We let CTPOS will be the position of the common
!     node in CTARG if one is found.  It will
!     be zero if COBS is not found in CTARG.
!
      if ( ctarg(nct) == cobs ) then
         ctpos  = nct
         cframe = tframe(ctpos)
      else
         ctpos  = 0
      end if

!
!     Repeat the same loop as above, but each time
!     we encounter a new center of motion, check to
!     see if it is a common node.  (When CTPOS is
!     not zero, CTARG(CTPOS) is the first common node.)
!
!     Note that we don't need a centers array nor a
!     states array, just a single center and state
!     is sufficient --- we just keep overwriting them.
!     When the common node is found, we have everything
!     we need in that one center (COBS) and state
!     (SOBS-state of the target relative to COBS).
!
      found  = .true.
      nofrm  = .true.
      legs   =  0

      do while (           found &
                  .and.  ( cobs  /= ssb ) &
                  .and.  ( ctpos == 0   )  )
!
!        Find a file and segment that has state
!        data for COBS.
!
         call spksfs ( ctx, cobs, et, handle, descr, ident, found )

         if ( found ) then
!
!           Get the state of COBS; call it STEMP.
!           The center of motion of COBS becomes the
!           new COBS.
!
            if ( legs == 0 ) then
               call spkpvn ( ctx, handle, descr, et, tmpfrm, sobs,  cobs )
            else
               call spkpvn ( ctx, handle, descr, et, tmpfrm, stemp, cobs )
            end if

            if ( ctx%failed() ) return

            if ( nofrm ) then
               nofrm  = .false.
               cframe = tmpfrm
            end if
!
!           Add STEMP to the state of OBS relative to
!           the old COBS to get the state of OBS
!           relative to the new COBS.
!
            if ( cframe == tmpfrm ) then
!
!              On the first leg of the state of the observer, we
!              don't have to add anything, the state of the observer
!              is already in SOBS.  We only have to add when the
!              number of legs in the observer state is one or greater.
!
               if ( legs > 0 ) then
                  call vaddg ( sobs,  stemp, 6, vtemp )
                  call moved ( vtemp,        6, sobs  )
               end if

            else if ( isinrt( cframe, tmpfrm ) ) then

               call irfrot ( ctx, cframe, tmpfrm, rot )
               call mxv    ( rot,    sobs(1),   vtemp(1)  )
               call mxv    ( rot,    sobs(4),   vtemp(4)  )
               call vaddg  ( vtemp,  stemp,     6, sobs   )
               cframe = tmpfrm

            else

               call zzfrmch0 ( ctx, cframe, tmpfrm, et,   stxfrm )

               if ( ctx%failed() ) return

               call mxvg   ( stxfrm, sobs,   6, 6, vtemp  )
               call vaddg  ( vtemp,  stemp,     6, sobs   )
               cframe = tmpfrm

            end if

!
!           We now have one more leg of the path for OBS.  Set
!           LEGS to reflect this.  Then see if the new center
!           is a common node. If not, repeat the loop.
!
            legs  = legs + 1
            ctpos = isrchi ( cobs, nct, ctarg )

         end if

!
!        Check failed.  We don't want to loop indefinitely.
!
         if ( ctx%failed() ) return

      end do

!
!     If CTPOS is zero at this point, it means we
!     have not found a common node though we have
!     searched through all the available data.
!
      if ( ctpos == 0 ) then

         call bodc2n ( ctx, targ, tname, found )

         if ( found ) then
            call prefix ( '# (',  0,        tname )
            call suffix ( ')',    0,        tname )
            call repmi  ( ctx, tname, '#', targ, tname )
         else
            call intstr ( targ, tname )
         end if

         call bodc2n ( ctx, obs, oname, found )

         if ( found ) then
            call prefix ( '# (',  0,       oname )
            call suffix ( ')',    0,       oname )
            call repmi  ( ctx, oname, '#', obs, oname )
         else
            call intstr ( obs, oname )
         end if

         call ctx%setmsg ( 'Insufficient ephemeris data has been ' &
         //            'loaded to compute the state of TARG ' &
         //            'relative to OBS at the ephemeris epoch ' &
         //            '#. ' )

         call etcal  ( ctx,  et,    tstring                                 )
         call ctx%errch  ( 'TARG', tname                                   )
         call ctx%errch  ( 'OBS',  oname                                   )
         call ctx%errch  ( '#',    tstring                                 )
         call ctx%sigerr ( 'SPICE(SPKINSUFFDATA)'                          )
         return

      end if

!
!     If CTPOS is not zero, then we have reached a
!     common node, specifically,
!
!        CTARG(CTPOS) = COBS = CENTER
!
!     (in diagram below).  The STATE of the target
!     (TARG) relative to the observer (OBS) is just
!
!        STARG(1,CTPOS) - SOBS.
!
!                     SOBS
!         CENTER ---------------->OBS
!            |                  .
!            |                .
!         S  |              .   E
!         T  |            .   T
!         A  |          .   A
!         R  |        .   T
!         G  |      .   S
!            |    .
!            |  .
!            V L
!           TARG
!
!     And the light-time between them is just
!
!               | STATE |
!          LT = ---------
!                   c
!
!     Compute the state of the target relative to CTARG(CTPOS)
!
      if ( ctpos == 1 ) then
         tframe(1) = cframe
      end if

      do i = 2, ctpos-1

         if ( tframe(i) == tframe(i+1) ) then

            call vaddg  ( starg(1,i), starg(1,i+1), 6, vtemp        )
            call moved  ( vtemp,                    6, starg(1,i+1) )

         else if ( isinrt ( tframe(i), tframe(i+1) ) ) then

            call irfrot ( ctx, tframe(i),  tframe(i+1),     rot          )
            call mxv    ( rot,        starg(1,i),      stemp(1)     )
            call mxv    ( rot,        starg(4,i),      stemp(4)     )
            call vaddg  ( stemp,      starg(1,i+1), 6, vtemp        )
            call moved  ( vtemp,                    6, starg(1,i+1) )

         else

            call zzfrmch0 ( ctx, tframe(i),  tframe(i+1), et,  stxfrm       )

            if ( ctx%failed() ) return

            call mxvg   ( stxfrm,     starg(1,i),  6,6, stemp        )
            call vaddg  ( stemp,      starg(1,i+1),  6, vtemp        )
            call moved  ( vtemp,                     6, starg(1,i+1) )

         end if

      end do

!
!     To avoid unnecessary frame transformations we'll do
!     a bit of extra decision making here.  It's a lot
!     faster to make logical checks than it is to compute
!     frame transformations.
!
      if ( tframe(ctpos) == cframe ) then

         call vsubg  ( starg(1,ctpos), sobs, 6,   state  )

      else if ( tframe(ctpos) == refid ) then

!
!        If the last frame associated with the target is already
!        in the requested output frame, we convert the state of
!        the observer to that frame and then subtract the state
!        of the observer from the state of the target.
!
         if ( isinrt( cframe, refid ) ) then

            call irfrot ( ctx, cframe, refid,   rot      )
            call mxv    ( rot,    sobs(1), stemp(1) )
            call mxv    ( rot,    sobs(4), stemp(4) )

         else

            call zzfrmch0 ( ctx, cframe, refid, et,  stxfrm )

            if ( ctx%failed() ) return

            call mxvg   ( stxfrm, sobs,  6,6, stemp  )

         end if

!
!        We've now transformed SOBS into the requested reference frame.
!        Set CFRAME to reflect this.
!
         cframe = refid
         call vsubg ( starg(1,ctpos),  stemp, 6, state )

      else if ( isinrt( tframe(ctpos), cframe ) ) then
!
!        If both frames are inertial we use IRFROT instead of
!        ZZFRMCH0 to get things into a common frame.
!
         call irfrot ( ctx, tframe(ctpos),  cframe,               rot      )
         call mxv    ( rot,            starg(1,ctpos),       stemp(1) )
         call mxv    ( rot,            starg(4,ctpos),       stemp(4) )
         call vsubg  ( stemp,          sobs,            6,   state    )

      else
!
!        Use the more general routine ZZFRMCH0 to make the
!        transformation.
!
         call zzfrmch0 ( ctx, tframe(ctpos),  cframe,          et,  stxfrm )

         if ( ctx%failed() ) return

         call mxvg   ( stxfrm,         starg(1,ctpos),  6,6, stemp  )
         call vsubg  ( stemp,          sobs,            6,   state  )

      end if

!
!     Finally, rotate as needed into the requested frame.
!
      if ( cframe == refid ) then

!
!        We don't have to do anything in this case.
!

      else if ( isinrt ( cframe, refid ) ) then
!
!        Since both frames are inertial, we use the more direct
!        routine IRFROT to get the transformation to REFID.
!
         call irfrot ( ctx, cframe,  refid,      rot      )
         call mxv    ( rot,     state(1),   stemp(1) )
         call mxv    ( rot,     state(4),   stemp(4) )
         call moved  ( stemp,   6,          state    )

      else

         call zzfrmch0 ( ctx, cframe,  refid, et,  stxfrm )

         if ( ctx%failed() ) return

         call mxvg   ( stxfrm,  state, 6, 6, stemp )
         call moved  ( stemp,          6,    state )

      end if

      lt = vnorm  ( state ) / clight

      return
      end associate
      end procedure zzspkgo0

      module procedure zzspklt0
!! ## Abstract
!!
!!     Return the state (position and velocity) of a target body
!!     relative to an observer, optionally corrected for light time,
!!     expressed relative to an inertial reference frame.
!!
!! ## Required Reading
!!
!!     SPK
!!
!! ## Keywords
!!
!!     EPHEMERIS
!!
!! ## Declarations
!!
!! ## Detailed Input
!!
!!     TARG        is the NAIF ID code for a target body.  The target
!!                 and observer define a state vector whose position
!!                 component points from the observer to the target.
!!
!!     ET          is the ephemeris time, expressed as seconds past
!!                 J2000 TDB, at which the state of the target body
!!                 relative to the observer is to be computed. ET
!!                 refers to time at the observer's location.
!!
!!     REF         is the inertial reference frame with respect to which
!!                 the input state STOBS and the output state STARG are
!!                 expressed. REF must be recognized by the SPICE
!!                 Toolkit. The acceptable frames are listed in the
!!                 Frames Required Reading, as well as in the SPICELIB
!!                 routine CHGIRF.
!!
!!                 Case and blanks are not significant in the string
!!                 REF.
!!
!!     ABCORR      indicates the aberration corrections to be applied to
!!                 the state of the target body to account for one-way
!!                 light time. See the discussion in the Particulars
!!                 section for recommendations on how to choose
!!                 aberration corrections.
!!
!!                 If ABCORR includes the stellar aberration correction
!!                 symbol '+S', this flag is simply ignored. Aside from
!!                 the possible presence of this symbol, ABCORR may be
!!                 any of the following:
!!
!!                    'NONE'     Apply no correction. Return the
!!                               geometric state of the target body
!!                               relative to the observer.
!!
!!                 The following values of ABCORR apply to the
!!                 "reception" case in which photons depart from the
!!                 target's location at the light-time corrected epoch
!!                 ET-LT and *arrive* at the observer's location at ET:
!!
!!                    'LT'       Correct for one-way light time (also
!!                               called "planetary aberration") using a
!!                               Newtonian formulation. This correction
!!                               yields the state of the target at the
!!                               moment it emitted photons arriving at
!!                               the observer at ET.
!!
!!                               The light time correction involves
!!                               iterative solution of the light time
!!                               equation (see Particulars for details).
!!                               The solution invoked by the 'LT' option
!!                               uses one iteration.
!!
!!                    'CN'       Converged Newtonian light time
!!                               correction. In solving the light time
!!                               equation, the 'CN' correction iterates
!!                               until the solution converges (three
!!                               iterations on all supported platforms).
!!                               Whether the 'CN+S' solution is
!!                               substantially more accurate than the
!!                               'LT' solution depends on the geometry
!!                               of the participating objects and on the
!!                               accuracy of the input data. In all
!!                               cases this routine will execute more
!!                               slowly when a converged solution is
!!                               computed. See the Particulars section of
!!                               SPKEZR for a discussion of precision of
!!                               light time corrections.
!!
!!                 The following values of ABCORR apply to the
!!                 "transmission" case in which photons *depart* from
!!                 the observer's location at ET and arrive at the
!!                 target's location at the light-time corrected epoch
!!                 ET+LT:
!!
!!                    'XLT'      "Transmission" case:  correct for
!!                               one-way light time using a Newtonian
!!                               formulation. This correction yields the
!!                               state of the target at the moment it
!!                               receives photons emitted from the
!!                               observer's location at ET.
!!
!!                    'XCN'      "Transmission" case:  converged
!!                               Newtonian light time correction.
!!
!!                 Neither special nor general relativistic effects are
!!                 accounted for in the aberration corrections applied
!!                 by this routine.
!!
!!                 Case and blanks are not significant in the string
!!                 ABCORR.
!!
!!     STOBS       is the geometric (uncorrected) state of the observer
!!                 relative to the solar system barycenter at epoch ET.
!!                 STOBS is a 6-vector: the first three components of
!!                 STOBS represent a Cartesian position vector; the last
!!                 three components represent the corresponding velocity
!!                 vector. STOBS is expressed relative to the inertial
!!                 reference frame designated by REF.
!!
!!                 Units are always km and km/sec.
!!
!! ## Detailed Output
!!
!!     STARG       is a Cartesian state vector representing the position
!!                 and velocity of the target body relative to the
!!                 specified observer. STARG is corrected for the
!!                 specified aberration, and is expressed with respect
!!                 to the specified inertial reference frame.  The first
!!                 three components of STARG represent the x-, y- and
!!                 z-components of the target's position; last three
!!                 components form the corresponding velocity vector.
!!
!!                 The position component of STARG points from the
!!                 observer's location at ET to the aberration-corrected
!!                 location of the target. Note that the sense of the
!!                 position vector is independent of the direction of
!!                 radiation travel implied by the aberration
!!                 correction.
!!
!!                 Units are always km and km/sec.
!!
!!     LT          is the one-way light time between the observer and
!!                 target in seconds.  If the target state is corrected
!!                 for light time, then LT is the one-way light time
!!                 between the observer and the light time-corrected
!!                 target location.
!!
!!     DLT         is the derivative with respect to barycentric
!!                 dynamical time of the one way light time between
!!                 target and observer:
!!
!!                    DLT = d(LT)/d(ET)
!!
!!                 DLT can also be described as the rate of change of
!!                 one way light time. DLT is unitless, since LT and
!!                 ET both have units of TDB seconds.
!!
!!                 If the observer and target are at the same position,
!!                 then DLT is set to zero.
!!
!! ## Parameters
!!
!!     None.
!!
!! ## Exceptions
!!
!!     1) For the convenience of the caller, the input aberration
!!        correction flag can call for stellar aberration correction via
!!        inclusion of the '+S' suffix. This portion of the aberration
!!        correction flag is ignored if present.
!!
!!     2) If the value of ABCORR is not recognized, the error
!!        is diagnosed by a routine in the call tree of this
!!        routine.
!!
!!     3) If the reference frame requested is not a recognized
!!        inertial reference frame, the error SPICE(BADFRAME)
!!        is signaled.
!!
!!     4) If the state of the target relative to the solar system
!!        barycenter cannot be computed, the error will be diagnosed
!!        by routines in the call tree of this routine.
!!
!!     5) If the observer and target are at the same position,
!!        then DLT is set to zero. This situation could arise,
!!        for example, when the observer is Mars and the target
!!        is the Mars barycenter.
!!
!!     6) If a division by zero error would occur in the computation
!!        of DLT, the error SPICE(DIVIDEBYZERO) is signaled.
!!
!! ## Files
!!
!!     This routine computes states using SPK files that have been
!!     loaded into the SPICE system, normally via the kernel loading
!!     interface routine FURNSH.  Application programs typically load
!!     kernels once before this routine is called, for example during
!!     program initialization; kernels need not be loaded repeatedly.
!!     See the routine FURNSH and the SPK and KERNEL Required Reading
!!     for further information on loading (and unloading) kernels.
!!
!!     If any of the ephemeris data used to compute STARG are expressed
!!     relative to a non-inertial frame in the SPK files providing those
!!     data, additional kernels may be needed to enable the reference
!!     frame transformations required to compute the state. Normally
!!     these additional kernels are PCK files or frame kernels. Any
!!     such kernels must already be loaded at the time this routine is
!!     called.
!!
!! ## Particulars
!!
!!     This routine supports higher-level SPK API routines that can
!!     perform both light time and stellar aberration corrections.
!!     User applications normally will not need to call this routine
!!     directly.
!!
!!     See the header of the routine SPKEZR for a detailed discussion
!!     of aberration corrections.
!!
!! ## Examples
!!
!!     The numerical results shown for this example may differ across
!!     platforms. The results depend on the SPICE kernels used as
!!     input, the compiler and supporting libraries, and the machine
!!     specific arithmetic implementation.
!!
!!    1) Look up a sequence of states of the Moon as seen from the
!!       Earth. Use light time corrections. Compute the first state for
!!       the epoch 2000 JAN 1 12:00:00 TDB; compute subsequent states at
!!       intervals of 1 hour. For each epoch, display the states, the
!!       one way light time between target and observer, and the rate of
!!       change of the one way light time.
!!
!!       Use the following meta-kernel to specify the kernels to
!!       load:
!!
!!          KPL/MK
!!
!!          File name: spkltc.tm
!!
!!          This meta-kernel is intended to support operation of SPICE
!!          example programs. The kernels shown here should not be
!!          assumed to contain adequate or correct versions of data
!!          required by SPICE-based user applications.
!!
!!          In order for an application to use this meta-kernel, the
!!          kernels referenced here must be present in the user's
!!          current working directory.
!!
!!          \begindata
!!
!!             KERNELS_TO_LOAD = ( 'de421.bsp',
!!                                 'pck00010.tpc',
!!                                 'naif0010.tls'  )
!!
!!          \begintext
!!
!!       The code example follows:
!!
!!           PROGRAM EX1
!!           IMPLICIT NONE
!!     C
!!     C     Local constants
!!     C
!!     C     The meta-kernel name shown here refers to a file whose
!!     C     contents are those shown above. This file and the kernels
!!     C     it references must exist in your current working directory.
!!     C
!!           CHARACTER*(*)         META
!!           PARAMETER           ( META   = 'spkltc.tm' )
!!     C
!!     C     Use a time step of 1 hour; look up 5 states.
!!     C
!!           DOUBLE PRECISION      STEP
!!           PARAMETER           ( STEP   = 3600.0D0 )
!!
!!           INTEGER               MAXITR
!!           PARAMETER           ( MAXITR = 5 )
!!     C
!!     C     Local variables
!!     C
!!           DOUBLE PRECISION      DLT
!!           DOUBLE PRECISION      ET
!!           DOUBLE PRECISION      ET0
!!           DOUBLE PRECISION      LT
!!           DOUBLE PRECISION      STATE ( 6 )
!!           DOUBLE PRECISION      STOBS ( 6 )
!!           INTEGER               I
!!
!!     C
!!     C     Load the SPK and LSK kernels via the meta-kernel.
!!     C
!!           CALL FURNSH ( META )
!!     C
!!     C     Convert the start time to seconds past J2000 TDB.
!!     C
!!           CALL STR2ET ( '2000 JAN 1 12:00:00 TDB', ET0 )
!!     C
!!     C     Step through a series of epochs, looking up a
!!     C     state vector at each one.
!!     C
!!           DO I = 1, MAXITR
!!
!!              ET = ET0 + (I-1)*STEP
!!
!!     C
!!     C        Look up a state vector at epoch ET using the
!!     C        following inputs:
!!     C
!!     C           Target:                 Moon (NAIF ID code 301)
!!     C           Reference frame:        J2000
!!     C           Aberration correction:  Light time ('LT')
!!     C           Observer:               Earth (NAIF ID code 399)
!!     C
!!     C        Before we can execute this computation, we'll need the
!!     C        geometric state of the observer relative to the solar
!!     C        system barycenter at ET, expressed relative to the
!!     C        J2000 reference frame:
!!     C
!!              CALL SPKSSB ( 399, ET,    'J2000', STOBS )
!!     C
!!     C        Now compute the desired state vector:
!!     C
!!              CALL SPKLTC ( 301,   ET,    'J2000', 'LT',
!!          .                 STOBS, STATE, LT,      DLT     )
!!
!!              WRITE (*,*) 'ET = ', ET
!!              WRITE (*,*) 'J2000 x-position (km):   ', STATE(1)
!!              WRITE (*,*) 'J2000 y-position (km):   ', STATE(2)
!!              WRITE (*,*) 'J2000 z-position (km):   ', STATE(3)
!!              WRITE (*,*) 'J2000 x-velocity (km/s): ', STATE(4)
!!              WRITE (*,*) 'J2000 y-velocity (km/s): ', STATE(5)
!!              WRITE (*,*) 'J2000 z-velocity (km/s): ', STATE(6)
!!              WRITE (*,*) 'One-way light time (s):  ', LT
!!              WRITE (*,*) 'Light time rate:         ', DLT
!!              WRITE (*,*) ' '
!!
!!           END DO
!!
!!           END
!!
!!     On a PC/Linux/gfortran platform, the following output was
!!     produced:
!!
!!        ET =    0.0000000000000000
!!        J2000 x-position (km):     -291569.26541282982
!!        J2000 y-position (km):     -266709.18647825718
!!        J2000 z-position (km):     -76099.155118763447
!!        J2000 x-velocity (km/s):   0.64353061322177041
!!        J2000 y-velocity (km/s):  -0.66608181700820079
!!        J2000 z-velocity (km/s):  -0.30132283179625752
!!        One-way light time (s):     1.3423106103251679
!!        Light time rate:           1.07316908698977495E-007
!!
!!        ET =    3600.0000000000000
!!        J2000 x-position (km):     -289240.78128184378
!!        J2000 y-position (km):     -269096.44087958336
!!        J2000 z-position (km):     -77180.899725757539
!!        J2000 x-velocity (km/s):   0.65006211520087476
!!        J2000 y-velocity (km/s):  -0.66016273921695667
!!        J2000 z-velocity (km/s):  -0.29964267390571342
!!        One-way light time (s):     1.3426939548635302
!!        Light time rate:           1.05652598952224259E-007
!!
!!        ET =    7200.0000000000000
!!        J2000 x-position (km):     -286888.88736709207
!!        J2000 y-position (km):     -271462.30170547962
!!        J2000 z-position (km):     -78256.555682137609
!!        J2000 x-velocity (km/s):   0.65653599154284592
!!        J2000 y-velocity (km/s):  -0.65419657680401588
!!        J2000 z-velocity (km/s):  -0.29794027307420823
!!        One-way light time (s):     1.3430713117337547
!!        Light time rate:           1.03990456898758609E-007
!!
!!        ET =    10800.000000000000
!!        J2000 x-position (km):     -284513.79173691198
!!        J2000 y-position (km):     -273806.60031034052
!!        J2000 z-position (km):     -79326.043183274567
!!        J2000 x-velocity (km/s):   0.66295190054599118
!!        J2000 y-velocity (km/s):  -0.64818380709706158
!!        J2000 z-velocity (km/s):  -0.29621577937090349
!!        One-way light time (s):     1.3434426890693671
!!        Light time rate:           1.02330665243423737E-007
!!
!!        ET =    14400.000000000000
!!        J2000 x-position (km):     -282115.70368389413
!!        J2000 y-position (km):     -276129.16976799071
!!        J2000 z-position (km):     -80389.282965712249
!!        J2000 x-velocity (km/s):   0.66930950377548726
!!        J2000 y-velocity (km/s):  -0.64212490805688027
!!        J2000 z-velocity (km/s):  -0.29446934336246899
!!        One-way light time (s):     1.3438080956559786
!!        Light time rate:           1.00673403630050830E-007
!!
!! ## Restrictions
!!
!!     1) The routine SPKGEO should be used instead of this routine
!!        to compute geometric states. SPKGEO introduces less
!!        round-off error when the observer and target have common
!!        center that is closer to both objects than is the solar
!!        system barycenter.
!!
!!     2) The kernel files to be used by SPKLTC must be loaded
!!        (normally by the SPICELIB kernel loader FURNSH) before
!!        this routine is called.
!!
!!     3) Unlike most other SPK state computation routines, this
!!        routine requires that the output state be relative to an
!!        inertial reference frame.
!!
!! ## Literature References
!!
!!     None.
!!
!! ## Author and Institution
!!
!!     N.J. Bachman    (JPL)
!!
!! ## Version
!!
!!-    SPICELIB Version 2.0.0, 04-JUL-2014 (NJB)
!!
!!        Discussion of light time corrections was updated. Assertions
!!        that converged light time corrections are unlikely to be
!!        useful were removed.
!!
!!     Last update was 02-MAY-2012 (NJB)
!!
!!        Updated to ensure convergence when CN or XCN light time
!!        corrections are used. The new algorithm also terminates early
!!        (after fewer than three iterations) when convergence is
!!        attained.
!!
!!        Call to ZZPRSCOR was replaced by a call to ZZVALCOR.
!!
!!-    SPICELIB Version 1.0.0, 11-JAN-2008 (NJB)
!!
!! ## Index Entries
!!
!!     low-level light time correction
!!     light-time corrected state from spk file
!!     get light-time corrected state
!!
!! ## Revisions
!!
!!     None.
!!
!!     SPICELIB functions
!!
!!     Local parameters
!!
!!     TOL is the tolerance used for a division-by-zero test
!!     performed prior to computation of DLT.
!!
!!     Convergence limit:
!!
!!     Maximum number of light time iterations for any
!!     aberration correction:
!!
!!     Local variables
!!
!!     Saved variables
!!
!!     Initial values
!!
!!     Standard SPICE error handling.
!!
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: clight
      use pepper_arrays, only: touchd
      use pepper_math, only: vdot, vlcom, vnorm, vsubg
      use pepper_inc_zzabcorr
      implicit none
      character(*), parameter :: rname = 'ZZSPKLT0'
      real(wp), parameter :: tol = 1.0e-10_wp
      real(wp), parameter :: cnvlim = 1.0e-17_wp
      integer(ip), parameter :: maxitr = 5
      integer(ip), parameter :: ssb = 0
      real(wp)              a
      real(wp)              b
      real(wp)              c
      real(wp)              dist
      real(wp)              epoch
      real(wp)              lterr
      real(wp)              prvlt
      real(wp)              ssblt
      real(wp)              ssbtrg ( 6 )
      integer(ip)           i
      integer(ip)           ltsign
      integer(ip)           numitr
      integer(ip)           refid
      logical               attblk ( nabcor )
      logical               usestl
      if ( .not. allocated ( ctx%zzspklt0_state ) ) then
         allocate ( ctx%zzspklt0_state )
         call init_zzspklt0_state ( ctx%zzspklt0_state )
      end if
      associate ( pass1 => ctx%zzspklt0_state%pass1, &
                  prvcor => ctx%zzspklt0_state%prvcor, &
                  usecn => ctx%zzspklt0_state%usecn, &
                  uselt => ctx%zzspklt0_state%uselt, &
                  xmit => ctx%zzspklt0_state%xmit )
      if ( ctx%failed() ) return

      if (  pass1  .or.  ( abcorr /= prvcor )  ) then
!
!        The aberration correction flag differs from the value it
!        had on the previous call, if any.  Analyze the new flag.
!
         call zzvalcor ( ctx, abcorr, attblk )

         if ( ctx%failed() ) return

!
!        The aberration correction flag is recognized; save it.
!
         prvcor = abcorr

!
!        Set logical flags indicating the attributes of the requested
!        correction:
!
!           XMIT is .TRUE. when the correction is for transmitted
!           radiation.
!
!           USELT is .TRUE. when any type of light time correction
!           (normal or converged Newtonian) is specified.
!
!           USECN indicates converged Newtonian light time correction.
!
!        The above definitions are consistent with those used by
!        ZZVALCOR.
!
         xmit    =  attblk ( xmtidx )
         uselt   =  attblk ( ltidx )
         usecn   =  attblk ( cnvidx )
         usestl  =  attblk ( stlidx )

         pass1   = .false.

      end if

!
!     See if the reference frame is a recognized inertial frame.
!
      call irfnum ( ctx, ref, refid )

      if ( refid == 0 ) then

         call ctx%setmsg ( 'The requested frame ''#'' is not a ' &
         //            'recognized inertial frame. '        )
         call ctx%errch  ( '#', ref                             )
         call ctx%sigerr ( 'SPICE(BADFRAME)'                    )
         return

      end if

!
!     Find the geometric state of the target body with respect to
!     the solar system barycenter. Subtract the state of the
!     observer to get the relative state. Use this to compute the
!     one-way light time.
!
      call zzspkgo0 ( ctx, targ,   et,    ref, ssb,  ssbtrg, ssblt )

      if ( ctx%failed() ) return

      call vsubg  ( ssbtrg, stobs, 6,   starg  )

      dist = vnorm(starg)
      lt   = dist / clight

      if ( lt == 0.0_wp ) then
!
!        This can happen only if the observer and target are at the
!        same position. We don't consider this an error, but we're not
!        going to compute the light time derivative.
!
         dlt = 0

         return

      end if

      if ( .not. uselt ) then
!
!        This is a special case: we're not using light time
!        corrections, so the derivative
!        of light time is just
!
!           (1/c) * d(VNORM(STARG))/dt
!
         dlt = vdot( starg, starg(4) ) / ( dist * clight )

!
!        LT and DLT are both set, so we can return.
!
         return

      end if

!
!     To correct for light time, find the state of the target body
!     at the current epoch minus the one-way light time. Note that
!     the observer remains where it is.
!
!     Determine the sign of the light time offset.
!
      if ( xmit ) then
         ltsign =  1
      else
         ltsign = -1
      end if

!
!     Let NUMITR be the number of iterations we'll perform to
!     compute the light time.
!
      if ( usecn ) then
         numitr = maxitr
      else
         numitr = 1
      end if

      i     = 0
      lterr = 1.0_wp

      do while (  ( i < numitr ) .and. ( lterr > cnvlim )  )
!
!        LT was set either prior to this loop or
!        during the previous loop iteration.
!
         epoch  =  et + ( ltsign * lt )

         call zzspkgo0 ( ctx, targ, epoch, ref, ssb, ssbtrg, ssblt )

         if ( ctx%failed() ) return

         call vsubg  ( ssbtrg, stobs, 6, starg )

         prvlt = lt
         lt    = touchd(  vnorm( starg ) / clight  )

!        LTERR is the magnitude of the change between the current
!        estimate of light time and the previous estimate, relative to
!        the previous light time corrected epoch.
!
         lterr = touchd(    abs( lt  - prvlt      ) &
                          / max( 1.0_wp, abs(epoch) )  )
         i     = i + 1

      end do

!
!     At this point, STARG contains the light time corrected
!     state of the target relative to the observer.
!
!     Compute the derivative of light time with respect
!     to time: dLT/dt.  Below we derive the formula for
!     this quantity for the reception case. Let
!
!        POBS be the position of the observer relative to the
!        solar system barycenter.
!
!        VOBS be the velocity of the observer relative to the
!        solar system barycenter.
!
!        PTARG be the position of the target relative to the
!        solar system barycenter.
!
!        VTARG be the velocity of the target relative to the
!        solar system barycenter.
!
!        S be the sign of the light time correction. S is
!        negative for the reception case.
!
!     The light-time corrected position of the target relative to
!     the observer at observation time ET, given the one-way
!     light time LT is:
!
!         PTARG(ET+S*LT) - POBS(ET)
!
!     The light-time corrected velocity of the target relative to
!     the observer at observation time ET is
!
!         VTARG(ET+S*LT)*( 1 + S*d(LT)/d(ET) ) - VOBS(ET)
!
!     We need to compute dLT/dt. Below, we use the facts that,
!     for a time-dependent vector X(t),
!
!          ||X||     = <X,X> ** (1/2)
!
!        d(||X||)/dt = (1/2)<X,X>**(-1/2) * 2 * <X,dX/dt>
!
!                    = <X,X>**(-1/2) *  <X,dX/dt>
!
!                    = <X,dX/dt> / ||X||
!
!     Newtonian light time equation:
!
!        LT     =   (1/c) * || PTARG(ET+S*LT) - POBS(ET)||
!
!     Differentiate both sides:
!
!        dLT/dt =   (1/c) * ( 1 / || PTARG(ET+S*LT) - POBS(ET) || )
!
!                  * < PTARG(ET+S*LT) - POBS(ET),
!                      VTARG(ET+S*LT)*(1+S*d(LT)/d(ET)) - VOBS(ET) >
!
!               = (1/c) * ( 1 / || PTARG(ET+S*LT) - POBS(ET) || )
!
!                 * (  < PTARG(ET+S*LT) - POBS(ET),
!                        VTARG(ET+S*LT) - VOBS(ET) >
!
!                   +  < PTARG(ET+S*LT) - POBS(ET),
!                        VTARG(ET+S*LT)           > * (S*d(LT)/d(ET))  )
!
!     Let
!
!        A =   (1/c) * ( 1 / || PTARG(ET+S*LT) - POBS(ET) || )
!
!        B =   < PTARG(ET+S*LT) - POBS(ET), VTARG(ET+S*LT) - VOBS(ET) >
!
!        C =   < PTARG(ET+S*LT) - POBS(ET), VTARG(ET+S*LT) >
!
!     Then
!
!        d(LT)/d(ET) =  A * ( B  +  C * S*d(LT)/d(ET) )
!
!     which implies
!
!        d(LT)/d(ET) =  A*B / ( 1 - S*C*A )
!
      a   =  1.0_wp / ( clight*vnorm(starg) )

      b   =  vdot ( starg, starg(4)  )

      c   =  vdot ( starg, ssbtrg(4) )

!
!     For physically realistic target velocities, S*C*A cannot equal 1.
!     We'll check for this case anyway.
!
      if (  ltsign*c*a  >  1.0_wp-tol ) then

         call ctx%setmsg ( 'Target range rate magnitude is ' &
      //               'approximately the speed of light. The ' &
      //               'light time derivative cannot be ' &
      //               'computed.'                            )
         call ctx%sigerr ( 'SPICE(DIVIDEBYZERO)'                  )
         return

      end if

!
!     Compute DLT: the rate of change of light time.
!
      dlt = (a*b)/( 1.0_wp - ltsign*c*a )

!
!     Overwrite the velocity portion of the output state
!     with the light-time corrected velocity.
!
      call vlcom ( 1.0_wp + ltsign*dlt, ssbtrg(4), &
                  -1.0_wp,              stobs(4),   starg(4) )

      return
      end associate
      end procedure zzspklt0


!$Procedure ZZSPKSB0 ( S/P Kernel, solar system barycenter )

      module procedure zzspksb0
!! ## Abstract
!!
!!     SPICE Private routine intended solely for the support of SPICE
!!     routines.  Users should not call this routine directly due
!!     to the volatile nature of this routine.
!!
!!     Return the state (position and velocity) of a target body
!!     relative to the solar system barycenter.
!!
!! ## Required Reading
!!
!!     SPK
!!
!! ## Keywords
!!
!!     EPHEMERIS
!!
!! ## Declarations
!!
      use pepper_kinds, only: ip, wp
      implicit none

! $ Detailed_Input
!
!     TARG        is the standard NAIF ID code for a target body.
!
!     ET          is the epoch (ephemeris time) at which the state
!                 of the target body is to be computed.
!
!     REF         is the name of the  reference frame to which the
!                 vectors returned by the routine should be rotated.
!                 This may be any frame supported by the SPICELIB frame
!                 system, including dynamic and other non-inertial
!                 frames.
!
! $ Detailed_Output
!
!     STARG       contains the position and velocity of the target
!                 body, relative to the solar system barycenter,
!                 at epoch ET. These vectors are rotated into the
!                 specified  reference frame. Units are always
!                 km and km/sec.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) If insufficient information has not bee "loaded" via the
!        routine SPKLEF or the PCK kernel loaders, the problem will
!        be diagnosed by a routine in the call tree of this routine.
!
! $ Files
!
!     See: $Restrictions.
!
! $ Particulars
!
!     In order to compute the state of one body relative to another,
!     the states of the two bodies must be known relative to a third
!     body. One simple solution is to use the solar system barycenter
!     as the third body.
!
!     Ephemeris data from more than one segment may be required
!     to determine the state of a body relative to the barycenter.
!     ZZSPKSB0 reads as many segments as necessary, from as many
!     files as necessary, using files that have been loaded by
!     previous calls to SPKLEF (load ephemeris file).
!
! $ Examples
!
!     In the following code fragment, ZZSPKSB0 is used to display
!     the distance from Earth (Body 399) to Mars (body 499) at
!     a series of epochs.
!
!        CALL SPKLEF ( 'DE125.SPK', HANDLE )
!         .
!         .
!
!        EARTH = 399
!        MARS  = 499
!
!        DO WHILE ( EPOCH .LE. END )
!           CALL ZZSPKSB0 ( EARTH, EPOCH, 'J2000', SEARTH )
!           CALL ZZSPKSB0 ( MARS,  EPOCH, 'J2000', SMARS  )
!
!           CALL VSUB ( SMARS, SEARTH, SMARS )
!           WRITE (*,*) EPOCH, VNORM ( SMARS )
!
!           EPOCH = EPOCH + DELTA
!        END DO
!
! $ Restrictions
!
!     1) SPICE Private routine.
!
!     2) The ephemeris files to be used by ZZSPKSB0 must be loaded
!        by SPKLEF before ZZSPKSB0 is called.
!
! $ Literature_References
!
!     NAIF Document 168.0, "S- and P- Kernel (SPK) Specification and
!     User's Guide"
!
! $ Author_and_Institution
!
!     W.L. Taber      (JPL)
!     I.M. Underwood  (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 12-DEC-2004 (NJB)
!
!        Based on SPICELIB Version 2.0.2, 20-NOV-2004  (NJB)
!
!-&

! $ Index_Entries
!
!     state relative to solar system barycenter
!
!-&

! $ Revisions
!
!-    SPICELIB Version 2.0.0, 19-SEP-1995  (WLT)
!
!        The routine was simplified by replacing all of the
!        main body of code with a call to SPKGEO.  By making
!        this change the routine now supports non-inertial frames.
!
!-&

!
!     SPICELIB functions
!

!
!     Local variables
!
      real(wp)              lt
      integer(ip)           bary

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

      bary = 0

      call zzspkgo0 ( ctx, targ, et, ref, bary, starg, lt )

      return
      end procedure zzspksb0

      module procedure zzspkas0
!! ## Abstract
!!
!!     Given the state and acceleration of an observer relative to the
!!     solar system barycenter, return the state (position and velocity)
!!     of a target body relative to the observer, optionally corrected
!!     for light time and stellar aberration. All input and output
!!     vectors are expressed relative to an inertial reference frame.
!!
!!     This routine supersedes SPKAPP.
!!
!!     SPICE users normally should call the high-level API routines
!!     SPKEZR or SPKEZ rather than this routine.
!!
!! ## Required Reading
!!
!!     SPK
!!
!! ## Keywords
!!
!!     EPHEMERIS
!!
!! ## Declarations
!!
!! ## Detailed Input
!!
!!     TARG        is the NAIF ID code for a target body.  The target
!!                 and observer define a state vector whose position
!!                 component points from the observer to the target.
!!
!!     ET          is the ephemeris time, expressed as seconds past
!!                 J2000 TDB, at which the state of the target body
!!                 relative to the observer is to be computed.  ET
!!                 refers to time at the observer's location.
!!
!!     REF         is the inertial reference frame with respect to which
!!                 the input state STOBS, the input acceleration ACCOBS,
!!                 and the output state STARG are expressed. REF must be
!!                 recognized by the SPICE Toolkit.  The acceptable
!!                 frames are listed in the Frames Required Reading, as
!!                 well as in the SPICELIB routine CHGIRF.
!!
!!                 Case and blanks are not significant in the string
!!                 REF.
!!
!!     ABCORR      indicates the aberration corrections to be applied
!!                 to the state of the target body to account for one-way
!!                 light time and stellar aberration. See the discussion
!!                 in the header of SPKEZR for recommendations on
!!                 how to choose aberration corrections.
!!
!!                 ABCORR may be any of the following:
!!
!!                    'NONE'     Apply no correction. Return the
!!                               geometric state of the target body
!!                               relative to the observer.
!!
!!                 The following values of ABCORR apply to the
!!                 "reception" case in which photons depart from the
!!                 target's location at the light-time corrected epoch
!!                 ET-LT and *arrive* at the observer's location at ET:
!!
!!                    'LT'       Correct for one-way light time (also
!!                               called "planetary aberration") using a
!!                               Newtonian formulation. This correction
!!                               yields the state of the target at the
!!                               moment it emitted photons arriving at
!!                               the observer at ET.
!!
!!                               The light time correction uses an
!!                               iterative solution of the light time
!!                               equation (see Particulars for details).
!!                               The solution invoked by the 'LT' option
!!                               uses one iteration.
!!
!!                    'LT+S'     Correct for one-way light time and
!!                               stellar aberration using a Newtonian
!!                               formulation. This option modifies the
!!                               state obtained with the 'LT' option to
!!                               account for the observer's velocity
!!                               relative to the solar system
!!                               barycenter. The result is the apparent
!!                               state of the target---the position and
!!                               velocity of the target as seen by the
!!                               observer.
!!
!!                    'CN'       Converged Newtonian light time
!!                               correction. In solving the light time
!!                               equation, the 'CN' correction iterates
!!                               until the solution converges (three
!!                               iterations on all supported platforms).
!!                               Whether the 'CN+S' solution is
!!                               substantially more accurate than the
!!                               'LT' solution depends on the geometry
!!                               of the participating objects and on the
!!                               accuracy of the input data. In all
!!                               cases this routine will execute more
!!                               slowly when a converged solution is
!!                               computed. See the Particulars section of
!!                               SPKEZR for a discussion of precision of
!!                               light time corrections.
!!
!!                    'CN+S'     Converged Newtonian light time
!!                               correction and stellar aberration
!!                               correction.
!!
!!                 The following values of ABCORR apply to the
!!                 "transmission" case in which photons *depart* from
!!                 the observer's location at ET and arrive at the
!!                 target's location at the light-time corrected epoch
!!                 ET+LT:
!!
!!                    'XLT'      "Transmission" case:  correct for
!!                               one-way light time using a Newtonian
!!                               formulation. This correction yields the
!!                               state of the target at the moment it
!!                               receives photons emitted from the
!!                               observer's location at ET.
!!
!!                    'XLT+S'    "Transmission" case:  correct for
!!                               one-way light time and stellar
!!                               aberration using a Newtonian
!!                               formulation  This option modifies the
!!                               state obtained with the 'XLT' option to
!!                               account for the observer's velocity
!!                               relative to the solar system
!!                               barycenter. The position component of
!!                               the computed target state indicates the
!!                               direction that photons emitted from the
!!                               observer's location must be "aimed" to
!!                               hit the target.
!!
!!                    'XCN'      "Transmission" case:  converged
!!                               Newtonian light time correction.
!!
!!                    'XCN+S'    "Transmission" case:  converged
!!                               Newtonian light time correction and
!!                               stellar aberration correction.
!!
!!                 Neither special nor general relativistic effects are
!!                 accounted for in the aberration corrections applied
!!                 by this routine.
!!
!!                 Case and blanks are not significant in the string
!!                 ABCORR.
!!
!!     STOBS       is the geometric state of the observer relative to
!!                 the solar system barycenter at ET. STOBS is expressed
!!                 relative to the reference frame designated by REF.
!!                 The target and observer define a state vector whose
!!                 position component points from the observer to the
!!                 target.
!!
!!     ACCOBS      is the geometric acceleration of the observer
!!                 relative to the solar system barycenter at ET. This
!!                 is the derivative with respect to time of the
!!                 velocity portion of STOBS. ACCOBS is expressed
!!                 relative to the reference frame designated by REF.
!!
!!                 ACCOBS is used for computing stellar aberration
!!                 corrected velocity. If stellar aberration corrections
!!                 are not specified by ABCORR, ACCOBS is ignored; the
!!                 caller need not provide a valid input value in this
!!                 case.
!!
!! ## Detailed Output
!!
!!     STARG       is a Cartesian state vector representing the position
!!                 and velocity of the target body relative to the
!!                 specified observer. STARG is corrected for the
!!                 specified aberrations, and is expressed with respect
!!                 to the inertial reference frame designated by REF.
!!                 The first three components of STARG represent the x-,
!!                 y- and z-components of the target's position; last
!!                 three components form the corresponding velocity
!!                 vector.
!!
!!                 The position component of STARG points from the
!!                 observer's location at ET to the aberration-corrected
!!                 location of the target. Note that the sense of the
!!                 position vector is independent of the direction of
!!                 radiation travel implied by the aberration
!!                 correction.
!!
!!                 Units are always km and km/sec.
!!
!!     LT          is the one-way light time between the observer and
!!                 target in seconds.  If the target state is corrected
!!                 for light time, then LT is the one-way light time
!!                 between the observer and the light time-corrected
!!                 target location.
!!
!!     DLT         is the derivative with respect to barycentric
!!                 dynamical time of the one way light time between
!!                 target and observer:
!!
!!                    DLT = d(LT)/d(ET)
!!
!!                 DLT can also be described as the rate of change of
!!                 one way light time. DLT is unitless, since LT and
!!                 ET both have units of TDB seconds.
!!
!!                 If the observer and target are at the same position,
!!                 then DLT is set to zero.
!!
!! ## Parameters
!!
!!     None.
!!
!! ## Exceptions
!!
!!     1) If the value of ABCORR is not recognized, the error
!!        is diagnosed by a routine in the call tree of this
!!        routine.
!!
!!     2) If ABCORR calls for stellar aberration but not light
!!        time corrections, the error SPICE(NOTSUPPORTED) is
!!        signaled.
!!
!!     3) If ABCORR calls for relativistic light time corrections, the
!!        error SPICE(NOTSUPPORTED) is signaled.
!!
!!     4) If the reference frame requested is not a recognized
!!        inertial reference frame, the error SPICE(BADFRAME)
!!        is signaled.
!!
!!     5) If the state of the target relative to the solar system
!!        barycenter cannot be computed, the error will be diagnosed
!!        by routines in the call tree of this routine.
!!
!!     6) If the observer and target are at the same position,
!!        then DLT is set to zero. This situation could arise,
!!        for example, when the observer is Mars and the target
!!        is the Mars barycenter.
!!
!! ## Files
!!
!!     This routine computes states using SPK files that have been
!!     loaded into the SPICE system, normally via the kernel loading
!!     interface routine FURNSH.  Application programs typically load
!!     kernels once before this routine is called, for example during
!!     program initialization; kernels need not be loaded repeatedly.
!!     See the routine FURNSH and the SPK and KERNEL Required Reading
!!     for further information on loading (and unloading) kernels.
!!
!!     If any of the ephemeris data used to compute STARG are expressed
!!     relative to a non-inertial frame in the SPK files providing those
!!     data, additional kernels may be needed to enable the reference
!!     frame transformations required to compute the state. Normally
!!     these additional kernels are PCK files or frame kernels. Any such
!!     kernels must already be loaded at the time this routine is
!!     called.
!!
!! ## Particulars
!!
!!     This routine supports higher-level SPK API routines that can
!!     perform both light time and stellar aberration corrections.
!!
!!     User applications normally will not need to call this routine
!!     directly. However, this routine can improve run-time efficiency
!!     in situations where many targets are observed from the same
!!     location at the same time. In such cases, the state and
!!     acceleration of the observer relative to the solar system
!!     barycenter need be computed only once per look-up epoch.
!!
!!     When apparent positions, rather than apparent states, are
!!     required, consider using the high-level position-only API
!!     routines
!!
!!        SPKPOS
!!        SPKEZP
!!
!!     or the low-level, position-only analog of this routine
!!
!!        SPKAPO
!!
!!     In general, the position-only routines are more efficient.
!!
!!     See the header of the routine SPKEZR for a detailed discussion
!!     of aberration corrections.
!!
!! ## Examples
!!
!!    1) Look up a sequence of states of the Moon as seen from the
!!       Earth. Use light time and stellar aberration corrections.
!!       Compute the first state for the epoch 2000 JAN 1 12:00:00 TDB;
!!       compute subsequent states at intervals of 1 hour. For each
!!       epoch, display the states, the one way light time between
!!       target and observer, and the rate of change of the one way
!!       light time.
!!
!!       Use the following meta-kernel to specify the kernels to
!!       load:
!!
!!          KPL/MK
!!
!!          This meta-kernel is intended to support operation of SPICE
!!          example programs. The kernels shown here should not be
!!          assumed to contain adequate or correct versions of data
!!          required by SPICE-based user applications.
!!
!!          In order for an application to use this meta-kernel, the
!!          kernels referenced here must be present in the user's
!!          current working directory.
!!
!!          \begindata
!!
!!             KERNELS_TO_LOAD = ( 'de418.bsp',
!!                                 'pck00008.tpc',
!!                                 'naif0008.tls'  )
!!
!!          \begintext
!!
!!       The code example follows:
!!
!!           PROGRAM EX1
!!           IMPLICIT NONE
!!     C
!!     C     Local constants
!!     C
!!     C     The meta-kernel name shown here refers to a file whose
!!     C     contents are those shown above. This file and the kernels
!!     C     it references must exist in your current working directory.
!!     C
!!           CHARACTER*(*)         META
!!           PARAMETER           ( META   = 'example.mk' )
!!     C
!!     C     Use a time step of 1 hour; look up 5 states.
!!     C
!!           DOUBLE PRECISION      STEP
!!           PARAMETER           ( STEP   = 3600.0D0 )
!!
!!           INTEGER               MAXITR
!!           PARAMETER           ( MAXITR = 5 )
!!     C
!!     C     Local variables
!!     C
!!           DOUBLE PRECISION      ACC    ( 3 )
!!           DOUBLE PRECISION      DLT
!!           DOUBLE PRECISION      ET
!!           DOUBLE PRECISION      ET0
!!           DOUBLE PRECISION      LT
!!           DOUBLE PRECISION      STATE  ( 6 )
!!           DOUBLE PRECISION      STATE0 ( 6 )
!!           DOUBLE PRECISION      STATE2 ( 6 )
!!           DOUBLE PRECISION      STOBS  ( 6 )
!!           DOUBLE PRECISION      TDELTA
!!           INTEGER               I
!!
!!     C
!!     C     Load the SPK and LSK kernels via the meta-kernel.
!!     C
!!           CALL FURNSH ( META )
!!     C
!!     C     Convert the start time to seconds past J2000 TDB.
!!     C
!!           CALL STR2ET ( '2000 JAN 1 12:00:00 TDB', ET0 )
!!     C
!!     C     Step through a series of epochs, looking up a
!!     C     state vector at each one.
!!     C
!!           DO I = 1, MAXITR
!!
!!              ET = ET0 + (I-1)*STEP
!!
!!     C
!!     C        Look up a state vector at epoch ET using the
!!     C        following inputs:
!!     C
!!     C           Target:                 Moon (NAIF ID code 301)
!!     C           Reference frame:        J2000
!!     C           Aberration correction:  Light time and stellar
!!     C                                   aberration ('LT+S')
!!     C           Observer:               Earth (NAIF ID code 399)
!!     C
!!     C        Before we can execute this computation, we'll need the
!!     C        geometric state and acceleration of the observer
!!     C        relative to the solar system barycenter at ET, expressed
!!     C        relative to the J2000 reference frame. First find
!!     C        the state:
!!     C
!!              CALL SPKSSB ( 399, ET, 'J2000', STOBS )
!!     C
!!     C        Next compute the acceleration. We numerically
!!     C        differentiate the velocity using a quadratic
!!     C        approximation:
!!     C
!!              TDELTA = 1.D0
!!
!!              CALL SPKSSB ( 399, ET-TDELTA, 'J2000', STATE0 )
!!              CALL SPKSSB ( 399, ET+TDELTA, 'J2000', STATE2 )
!!
!!              CALL QDERIV ( 3, STATE0(4), STATE2(4), TDELTA, ACC )
!!     C
!!     C        Now compute the desired state vector:
!!     C
!!              CALL SPKAPS ( 301,   ET,  'J2000', 'LT+S',
!!          .                 STOBS, ACC, STATE,    LT,   DLT )
!!
!!              WRITE (*,*) 'ET = ', ET
!!              WRITE (*,*) 'J2000 x-position (km):   ', STATE(1)
!!              WRITE (*,*) 'J2000 y-position (km):   ', STATE(2)
!!              WRITE (*,*) 'J2000 z-position (km):   ', STATE(3)
!!              WRITE (*,*) 'J2000 x-velocity (km/s): ', STATE(4)
!!              WRITE (*,*) 'J2000 y-velocity (km/s): ', STATE(5)
!!              WRITE (*,*) 'J2000 z-velocity (km/s): ', STATE(6)
!!              WRITE (*,*) 'One-way light time (s):  ', LT
!!              WRITE (*,*) 'Light time rate:         ', DLT
!!              WRITE (*,*) ' '
!!
!!           END DO
!!
!!           END
!!
!!     The output produced by this program will vary somewhat as
!!     a function of the platform on which the program is built and
!!     executed. On a PC/Linux/g77 platform, the following output
!!     was produced:
!!
!!        ET =   0.
!!        J2000 x-position (km):    -291584.614
!!        J2000 y-position (km):    -266693.406
!!        J2000 z-position (km):    -76095.6532
!!        J2000 x-velocity (km/s):   0.643439157
!!        J2000 y-velocity (km/s):  -0.666065874
!!        J2000 z-velocity (km/s):  -0.301310063
!!        One-way light time (s):    1.34231061
!!        Light time rate:           1.07316909E-07
!!
!!        ET =   3600.
!!        J2000 x-position (km):    -289256.459
!!        J2000 y-position (km):    -269080.605
!!        J2000 z-position (km):    -77177.3528
!!        J2000 x-velocity (km/s):   0.64997032
!!        J2000 y-velocity (km/s):  -0.660148253
!!        J2000 z-velocity (km/s):  -0.299630418
!!        One-way light time (s):    1.34269395
!!        Light time rate:           1.05652599E-07
!!
!!        ET =   7200.
!!        J2000 x-position (km):    -286904.897
!!        J2000 y-position (km):    -271446.417
!!        J2000 z-position (km):    -78252.9655
!!        J2000 x-velocity (km/s):   0.656443883
!!        J2000 y-velocity (km/s):  -0.654183552
!!        J2000 z-velocity (km/s):  -0.297928533
!!        One-way light time (s):    1.34307131
!!        Light time rate:           1.03990457E-07
!!
!!        ET =   10800.
!!        J2000 x-position (km):    -284530.133
!!        J2000 y-position (km):    -273790.671
!!        J2000 z-position (km):    -79322.4117
!!        J2000 x-velocity (km/s):   0.662859505
!!        J2000 y-velocity (km/s):  -0.648172247
!!        J2000 z-velocity (km/s):  -0.296204558
!!        One-way light time (s):    1.34344269
!!        Light time rate:           1.02330665E-07
!!
!!        ET =   14400.
!!        J2000 x-position (km):    -282132.378
!!        J2000 y-position (km):    -276113.202
!!        J2000 z-position (km):    -80385.612
!!        J2000 x-velocity (km/s):   0.669216846
!!        J2000 y-velocity (km/s):  -0.642114815
!!        J2000 z-velocity (km/s):  -0.294458645
!!        One-way light time (s):    1.3438081
!!        Light time rate:           1.00673404E-07
!!
!! ## Restrictions
!!
!!     1) This routine should not be used to compute geometric states.
!!        Instead, use SPKEZR, SPKEZ, or SPKGEO. SPKGEO, which is called
!!        by SPKEZR and SPKEZ, introduces less round-off error when the
!!        observer and target have a common center that is closer to
!!        both objects than is the solar system barycenter.
!!
!!     2) The kernel files to be used by SPKAPS must be loaded
!!        (normally by the SPICELIB kernel loader FURNSH) before
!!        this routine is called.
!!
!!     3) Unlike most other SPK state computation routines, this
!!        routine requires that the output state be relative to an
!!        inertial reference frame.
!!
!! ## Literature References
!!
!!     SPK Required Reading.
!!
!! ## Author and Institution
!!
!!     N.J. Bachman    (JPL)
!!
!! ## Version
!!
!!-    SPICELIB Version 1.2.0, 12-SEP-2021 (NJB)
!!
!!        Bug fix: aberration correction strings are now parsed using
!!        ZZVALCOR. This improves error checking.
!!
!!        Corrected a header comment typo.
!!
!!-    SPICELIB Version 1.1.0, 04-JUL-2014 (NJB)
!!
!!        Discussion of light time corrections was updated. Assertions
!!        that converged light time corrections are unlikely to be
!!        useful were removed.
!!
!!     Last update was 15-APR-2014 (NJB)
!!
!!        Added a FAILED() call to prevent numeric problems
!!        resulting from uninitialized values.
!!
!!-    SPICELIB Version 1.0.0, 11-JAN-2008 (NJB)
!!
!! ## Index Entries
!!
!!     low-level aberration-corrected state computation
!!     low-level light time and stellar aberration correction
!!
!! ## Revisions
!!
!!     None.
!!
!!     SPICELIB functions
!!
!!     Local parameters
!!
!!     Local variables
!!
!!     Saved variables
!!
!!     Initial values
!!
!!     Standard SPICE error handling.
!!
      use pepper_kinds, only: ip, wp
      use pepper_math, only: vadd, vequ
      use pepper_inc_zzabcorr
      implicit none
      character(*), parameter :: rname = 'ZZSPKAS0'
      real(wp)              corpos ( 3 )
      real(wp)              corvel ( 3 )
      real(wp)              dpcorr ( 3 )
      real(wp)              pcorr  ( 3 )
      integer(ip)           refid
      logical               attblk ( nabcor )
      if ( .not. allocated ( ctx%zzspkas0_state ) ) then
         allocate ( ctx%zzspkas0_state )
         call init_zzspkas0_state ( ctx%zzspkas0_state )
      end if
      associate ( first => ctx%zzspkas0_state%first, &
                  prvcor => ctx%zzspkas0_state%prvcor, &
                  uselt => ctx%zzspkas0_state%uselt, &
                  usestl => ctx%zzspkas0_state%usestl, &
                  xmit => ctx%zzspkas0_state%xmit )
      if ( ctx%failed() ) return

      if (  first  .or.  ( abcorr /= prvcor )  ) then
!
!        The aberration correction flag differs from the value it
!        had on the previous call, if any.  Analyze the new flag.
!
         call zzvalcor ( ctx, abcorr, attblk )

         if ( ctx%failed() ) return

!
!        The aberration correction flag is recognized; save it.
!
         prvcor = abcorr

!
!        Set logical flags indicating the attributes of the requested
!        correction:
!
!           XMIT is .TRUE. when the correction is for transmitted
!           radiation.
!
!           USELT is .TRUE. when any type of light time correction
!           (normal or converged Newtonian) is specified.
!
!           USECN indicates converged Newtonian light time correction.
!
!        The above definitions are consistent with those used by
!        ZZVALCOR.
!
         xmit    =  attblk ( xmtidx )
         uselt   =  attblk ( ltidx  )
         usestl  =  attblk ( stlidx )

         if (  usestl  .and.  ( .not. uselt )  ) then

            call ctx%setmsg ( 'Aberration correction flag # calls for ' &
            //            'stellar aberration but not light time ' &
            //            'corrections. This combination is not ' &
            //            'expected.'                              )
            call ctx%errch  ( '#', abcorr                              )
            call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'                    )
            return

         else if ( attblk(relidx) ) then

            call ctx%setmsg ( 'Aberration correction flag # calls for ' &
            //            'relativistic light time correction.'    )
            call ctx%errch  ( '#', abcorr                              )
            call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'                    )
            return

         end if

         first   = .false.

      end if

!
!     See if the reference frame is a recognized inertial frame.
!
      call irfnum ( ctx, ref, refid )

      if ( refid == 0 ) then

         call ctx%setmsg ( 'The requested frame ''#'' is not a ' &
         //            'recognized inertial frame. '        )
         call ctx%errch  ( '#', ref                             )
         call ctx%sigerr ( 'SPICE(BADFRAME)'                    )
         return

      end if

!
!     Get the state of the target relative to the observer,
!     optionally corrected for light time.
!
      call zzspklt0 ( ctx, targ,  et,    ref, abcorr, &
                      stobs, starg, lt,  dlt     )

      if ( ctx%failed() ) return

!
!     If stellar aberration corrections are not needed, we're
!     already done.
!
      if ( .not. usestl ) return

!
!     Get the stellar aberration correction and its time derivative.
!
      call zzstelab ( ctx, xmit, accobs, stobs(4), starg, pcorr, dpcorr )

!
!     Adding the stellar aberration correction to the light
!     time-corrected target position yields the position corrected for
!     both light time and stellar aberration.
!
      call vadd ( pcorr,  starg, corpos )
      call vequ ( corpos, starg         )

!
!     Velocity is treated in an analogous manner.
!
      call vadd ( dpcorr, starg(4), corvel )
      call vequ ( corvel, starg(4)         )

      return
      end associate
      end procedure zzspkas0

      module procedure zzspkac0
!! ## Abstract
!!
!!     Return the state (position and velocity) of a target body
!!     relative to an observer, optionally corrected for light time
!!     and stellar aberration, expressed relative to an inertial
!!     reference frame.
!!
!! ## Required Reading
!!
!!     SPK
!!
!! ## Keywords
!!
!!     EPHEMERIS
!!
!! ## Declarations
!!
!! ## Detailed Input
!!
!!     TARG        is the NAIF ID code for a target body.  The target
!!                 and observer define a state vector whose position
!!                 component points from the observer to the target.
!!
!!     ET          is the ephemeris time, expressed as seconds past
!!                 J2000 TDB, at which the state of the target body
!!                 relative to the observer is to be computed.  ET
!!                 refers to time at the observer's location.
!!
!!     REF         is the inertial reference frame with respect to which
!!                 the output state STARG is expressed. REF must be
!!                 recognized by the SPICE Toolkit.  The acceptable
!!                 frames are listed in the Frames Required Reading, as
!!                 well as in the SPICELIB routine CHGIRF.
!!
!!                 Case and blanks are not significant in the string
!!                 REF.
!!
!!     ABCORR      indicates the aberration corrections to be applied
!!                 to the state of the target body to account for one-way
!!                 light time and stellar aberration.  See the discussion
!!                 in the Particulars section for recommendations on
!!                 how to choose aberration corrections.
!!
!!                 ABCORR may be any of the following:
!!
!!                    'NONE'     Apply no correction. Return the
!!                               geometric state of the target body
!!                               relative to the observer.
!!
!!                 The following values of ABCORR apply to the
!!                 "reception" case in which photons depart from the
!!                 target's location at the light-time corrected epoch
!!                 ET-LT and *arrive* at the observer's location at ET:
!!
!!                    'LT'       Correct for one-way light time (also
!!                               called "planetary aberration") using a
!!                               Newtonian formulation. This correction
!!                               yields the state of the target at the
!!                               moment it emitted photons arriving at
!!                               the observer at ET.
!!
!!                               The light time correction uses an
!!                               iterative solution of the light time
!!                               equation (see Particulars for details).
!!                               The solution invoked by the 'LT' option
!!                               uses one iteration.
!!
!!                    'LT+S'     Correct for one-way light time and
!!                               stellar aberration using a Newtonian
!!                               formulation. This option modifies the
!!                               state obtained with the 'LT' option to
!!                               account for the observer's velocity
!!                               relative to the solar system
!!                               barycenter. The result is the apparent
!!                               state of the target---the position and
!!                               velocity of the target as seen by the
!!                               observer.
!!
!!                    'CN'       Converged Newtonian light time
!!                               correction. In solving the light time
!!                               equation, the 'CN' correction iterates
!!                               until the solution converges (three
!!                               iterations on all supported platforms).
!!                               Whether the 'CN+S' solution is
!!                               substantially more accurate than the
!!                               'LT' solution depends on the geometry
!!                               of the participating objects and on the
!!                               accuracy of the input data. In all
!!                               cases this routine will execute more
!!                               slowly when a converged solution is
!!                               computed. See the Particulars section of
!!                               SPKEZR for a discussion of precision of
!!                               light time corrections.
!!
!!                    'CN+S'     Converged Newtonian light time
!!                               correction and stellar aberration
!!                               correction.
!!
!!                 The following values of ABCORR apply to the
!!                 "transmission" case in which photons *depart* from
!!                 the observer's location at ET and arrive at the
!!                 target's location at the light-time corrected epoch
!!                 ET+LT:
!!
!!                    'XLT'      "Transmission" case:  correct for
!!                               one-way light time using a Newtonian
!!                               formulation. This correction yields the
!!                               state of the target at the moment it
!!                               receives photons emitted from the
!!                               observer's location at ET.
!!
!!                    'XLT+S'    "Transmission" case:  correct for
!!                               one-way light time and stellar
!!                               aberration using a Newtonian
!!                               formulation  This option modifies the
!!                               state obtained with the 'XLT' option to
!!                               account for the observer's velocity
!!                               relative to the solar system
!!                               barycenter. The position component of
!!                               the computed target state indicates the
!!                               direction that photons emitted from the
!!                               observer's location must be "aimed" to
!!                               hit the target.
!!
!!                    'XCN'      "Transmission" case:  converged
!!                               Newtonian light time correction.
!!
!!                    'XCN+S'    "Transmission" case:  converged
!!                               Newtonian light time correction and
!!                               stellar aberration correction.
!!
!!     OBS         is the NAIF ID code for the observer body.  The
!!                 target and observer define a state vector whose
!!                 position component points from the observer to the
!!                 target.
!!
!! ## Detailed Output
!!
!!     STARG       is a Cartesian state vector representing the position
!!                 and velocity of the target body relative to the
!!                 specified observer. STARG is corrected for the
!!                 specified aberrations, and is expressed with respect
!!                 to the specified inertial reference frame.  The first
!!                 three components of STARG represent the x-, y- and
!!                 z-components of the target's position; last three
!!                 components form the corresponding velocity vector.
!!
!!                 The position component of STARG points from the
!!                 observer's location at ET to the aberration-corrected
!!                 location of the target. Note that the sense of the
!!                 position vector is independent of the direction of
!!                 radiation travel implied by the aberration
!!                 correction.
!!
!!                 Units are always km and km/sec.
!!
!!     LT          is the one-way light time between the observer and
!!                 target in seconds.  If the target state is corrected
!!                 for aberrations, then LT is the one-way light time
!!                 between the observer and the light time corrected
!!                 target location.
!!
!!     DLT         is the derivative with respect to barycentric
!!                 dynamical time of the one way light time between
!!                 target and observer:
!!
!!                    DLT = d(LT)/d(ET)
!!
!!                 DLT can also be described as the rate of change of
!!                 one way light time. DLT is unitless, since LT and
!!                 ET both have units of TDB seconds.
!!
!!                 If the observer and target are at the same position,
!!                 then DLT is set to zero.
!!
!! ## Parameters
!!
!!     None.
!!
!! ## Exceptions
!!
!!     1) If the value of ABCORR is not recognized, the error
!!        is diagnosed by a routine in the call tree of this
!!        routine.
!!
!!     2) If the reference frame requested is not a recognized
!!        inertial reference frame, the error SPICE(BADFRAME)
!!        is signaled.
!!
!!     3) If the state of the target relative to the solar system
!!        barycenter cannot be computed, the error will be diagnosed
!!        by routines in the call tree of this routine.
!!
!!     4) If the observer and target are at the same position,
!!        then DLT is set to zero. This situation could arise,
!!        for example, when the observer is Mars and the target
!!        is the Mars barycenter.
!!
!! ## Files
!!
!!     This routine computes states using SPK files that have been
!!     loaded into the SPICE system, normally via the kernel loading
!!     interface routine FURNSH.  Application programs typically load
!!     kernels once before this routine is called, for example during
!!     program initialization; kernels need not be loaded repeatedly.
!!     See the routine FURNSH and the SPK and KERNEL Required Reading
!!     for further information on loading (and unloading) kernels.
!!
!!     If any of the ephemeris data used to compute STARG are expressed
!!     relative to a non-inertial frame in the SPK files providing those
!!     data, additional kernels may be needed to enable the reference
!!     frame transformations required to compute the state. Normally
!!     these additional kernels are PCK files or frame kernels. Any
!!     such kernels must already be loaded at the time this routine is
!!     called.
!!
!! ## Particulars
!!
!!     This routine supports higher-level SPK API routines that can
!!     perform both light time and stellar aberration corrections.
!!     User applications normally will not need to call this routine
!!     directly.
!!
!!     See the header of the routine SPKEZR for a detailed discussion
!!     of aberration corrections.
!!
!! ## Examples
!!
!!    1) Look up a sequence of states of the Moon as seen from the
!!       Earth. Use light time and stellar aberration corrections.
!!       Compute the first state for the epoch 2000 JAN 1 12:00:00 TDB;
!!       compute subsequent states at intervals of 1 hour. For each
!!       epoch, display the states, the one way light time between
!!       target and observer, and the rate of change of the one way
!!       light time.
!!
!!       Use the following meta-kernel to specify the kernels to
!!       load:
!!
!!          KPL/MK
!!
!!          This meta-kernel is intended to support operation of SPICE
!!          example programs. The kernels shown here should not be
!!          assumed to contain adequate or correct versions of data
!!          required by SPICE-based user applications.
!!
!!          In order for an application to use this meta-kernel, the
!!          kernels referenced here must be present in the user's
!!          current working directory.
!!
!!          \begindata
!!
!!             KERNELS_TO_LOAD = ( 'de418.bsp',
!!                                 'pck00008.tpc',
!!                                 'naif0008.tls'  )
!!
!!          \begintext
!!
!!       The code example follows:
!!
!!           PROGRAM EX1
!!           IMPLICIT NONE
!!     C
!!     C     Local constants
!!     C
!!     C     The meta-kernel name shown here refers to a file whose
!!     C     contents are those shown above. This file and the kernels
!!     C     it references must exist in your current working directory.
!!     C
!!     C
!!           CHARACTER*(*)         META
!!           PARAMETER           ( META   = 'example.mk' )
!!     C
!!     C     Use a time step of 1 hour; look up 5 states.
!!     C
!!           DOUBLE PRECISION      STEP
!!           PARAMETER           ( STEP   = 3600.0D0 )
!!
!!           INTEGER               MAXITR
!!           PARAMETER           ( MAXITR = 5 )
!!     C
!!     C     Local variables
!!     C
!!           DOUBLE PRECISION      DLT
!!           DOUBLE PRECISION      ET
!!           DOUBLE PRECISION      ET0
!!           DOUBLE PRECISION      LT
!!           DOUBLE PRECISION      STATE ( 6 )
!!           INTEGER               I
!!
!!     C
!!     C     Load the SPK and LSK kernels via the meta-kernel.
!!     C
!!           CALL FURNSH ( META )
!!     C
!!     C     Convert the start time to seconds past J2000 TDB.
!!     C
!!           CALL STR2ET ( '2000 JAN 1 12:00:00 TDB', ET0 )
!!     C
!!     C     Step through a series of epochs, looking up a
!!     C     state vector at each one.
!!     C
!!           DO I = 1, MAXITR
!!
!!              ET = ET0 + (I-1)*STEP
!!     C
!!     C        Look up a state vector at epoch ET using the
!!     C        following inputs:
!!     C
!!     C           Target:                 Moon (NAIF ID code 301)
!!     C           Reference frame:        J2000
!!     C           Aberration correction:  Light time and stellar
!!     C                                   aberration ('LT+S')
!!     C           Observer:               Earth (NAIF ID code 399)
!!     C
!!
!!              CALL SPKACS ( 301, ET,    'J2000', 'LT+S',
!!          .                 399, STATE, LT,      DLT     )
!!
!!              WRITE (*,*) 'ET = ', ET
!!              WRITE (*,*) 'J2000 x-position (km):   ', STATE(1)
!!              WRITE (*,*) 'J2000 y-position (km):   ', STATE(2)
!!              WRITE (*,*) 'J2000 z-position (km):   ', STATE(3)
!!              WRITE (*,*) 'J2000 x-velocity (km/s): ', STATE(4)
!!              WRITE (*,*) 'J2000 y-velocity (km/s): ', STATE(5)
!!              WRITE (*,*) 'J2000 z-velocity (km/s): ', STATE(6)
!!              WRITE (*,*) 'One-way light time (s):  ', LT
!!              WRITE (*,*) 'Light time rate:         ', DLT
!!              WRITE (*,*) ' '
!!
!!           END DO
!!
!!           END
!!
!!     The output produced by this program will vary somewhat as
!!     a function of the platform on which the program is built and
!!     executed. On a PC/Linux/g77 platform, the following output
!!     was produced:
!!
!!        ET =   0.
!!        J2000 x-position (km):    -291584.614
!!        J2000 y-position (km):    -266693.406
!!        J2000 z-position (km):    -76095.6532
!!        J2000 x-velocity (km/s):   0.643439157
!!        J2000 y-velocity (km/s):  -0.666065874
!!        J2000 z-velocity (km/s):  -0.301310063
!!        One-way light time (s):    1.34231061
!!        Light time rate:           1.07316909E-07
!!
!!        ET =   3600.
!!        J2000 x-position (km):    -289256.459
!!        J2000 y-position (km):    -269080.605
!!        J2000 z-position (km):    -77177.3528
!!        J2000 x-velocity (km/s):   0.64997032
!!        J2000 y-velocity (km/s):  -0.660148253
!!        J2000 z-velocity (km/s):  -0.299630418
!!        One-way light time (s):    1.34269395
!!        Light time rate:           1.05652599E-07
!!
!!        ET =   7200.
!!        J2000 x-position (km):    -286904.897
!!        J2000 y-position (km):    -271446.417
!!        J2000 z-position (km):    -78252.9655
!!        J2000 x-velocity (km/s):   0.656443883
!!        J2000 y-velocity (km/s):  -0.654183552
!!        J2000 z-velocity (km/s):  -0.297928533
!!        One-way light time (s):    1.34307131
!!        Light time rate:           1.03990457E-07
!!
!!        ET =   10800.
!!        J2000 x-position (km):    -284530.133
!!        J2000 y-position (km):    -273790.671
!!        J2000 z-position (km):    -79322.4117
!!        J2000 x-velocity (km/s):   0.662859505
!!        J2000 y-velocity (km/s):  -0.648172247
!!        J2000 z-velocity (km/s):  -0.296204558
!!        One-way light time (s):    1.34344269
!!        Light time rate:           1.02330665E-07
!!
!!        ET =   14400.
!!        J2000 x-position (km):    -282132.378
!!        J2000 y-position (km):    -276113.202
!!        J2000 z-position (km):    -80385.612
!!        J2000 x-velocity (km/s):   0.669216846
!!        J2000 y-velocity (km/s):  -0.642114815
!!        J2000 z-velocity (km/s):  -0.294458645
!!        One-way light time (s):    1.3438081
!!        Light time rate:           1.00673404E-07
!!
!! ## Restrictions
!!
!!     1) The kernel files to be used by SPKACS must be loaded
!!        (normally by the SPICELIB kernel loader FURNSH) before
!!        this routine is called.
!!
!!     2) Unlike most other SPK state computation routines, this
!!        routine requires that the output state be relative to an
!!        inertial reference frame.
!!
!! ## Literature References
!!
!!     SPK Required Reading.
!!
!! ## Author and Institution
!!
!!     N.J. Bachman    (JPL)
!!
!! ## Version
!!
!!-    SPICELIB Version 1.1.0, 23-AUG-2021 (NJB)
!!
!!        Bug fix: aberration correction strings are now parsed using
!!        ZZVALCOR. This improves error checking.
!!
!!-    SPICELIB Version 1.0.1, 04-JUL-2014 (NJB)
!!
!!        Discussion of light time corrections was updated. Assertions
!!        that converged light time corrections are unlikely to be
!!        useful were removed.
!!
!!-    SPICELIB Version 1.0.0, 11-JAN-2008 (NJB)
!!
!! ## Index Entries
!!
!!     low-level aberration correction
!!     aberration-corrected state from spk file
!!     get light time and stellar aberration-corrected state
!!
!! ## Revisions
!!
!!     None.
!!
!!     SPICELIB functions
!!
!!     Local parameters
!!
!!     Local variables
!!
!!     Saved variables
!!
!!     Initial values
!!
!!     Standard SPICE error handling.
!!
      use pepper_kinds, only: ip, wp
      use pepper_arrays, only: cleard
      use pepper_inc_zzabcorr
      implicit none
      character(*), parameter :: rname = 'ZZSPKAC0'
      real(wp), parameter :: delta = 1.0_wp
      integer(ip), parameter :: ssb = 0
      real(wp)              acc    ( 3 )
      real(wp)              ltssb
      real(wp)              ssblt
      real(wp)              ssbobs ( 6 )
      real(wp)              stobs  ( 6, 2 )
      real(wp)              t
      integer(ip)           i
      integer(ip)           refid
      logical               attblk ( nabcor )
      if ( .not. allocated ( ctx%zzspkac0_state ) ) then
         allocate ( ctx%zzspkac0_state )
         call init_zzspkac0_state ( ctx%zzspkac0_state )
      end if
      associate ( first => ctx%zzspkac0_state%first, &
                  prvcor => ctx%zzspkac0_state%prvcor, &
                  usestl => ctx%zzspkac0_state%usestl )
      if ( ctx%failed() ) return

      if (  first  .or.  ( abcorr /= prvcor )  ) then
!
!        The aberration correction flag differs from the value it
!        had on the previous call, if any.  Analyze the new flag.
!
         call zzvalcor ( ctx, abcorr, attblk )

         if ( ctx%failed() ) return

!
!        The aberration correction flag is recognized; save it.
!
         prvcor = abcorr

!
!        Set logical flags indicating the attributes of the requested
!        correction:
!
!           USESTL is .TRUE. when stellar aberration correction is
!           specified.
!
!        The above definitions are consistent with those used by
!        ZZVALCOR.
!
         usestl  =  attblk ( stlidx )

         first   = .false.

      end if

!
!     See if the reference frame is a recognized inertial frame.
!
      call irfnum ( ctx, ref, refid )

      if ( refid == 0 ) then

         call ctx%setmsg ( 'The requested frame ''#'' is not a ' &
         //            'recognized inertial frame. '        )
         call ctx%errch  ( '#', ref                             )
         call ctx%sigerr ( 'SPICE(BADFRAME)'                    )
         return

      end if

!
!     Prepare to look up the apparent state of the target
!     as seen by the observer. We'll need the geometric
!     state of the observer relative to the solar system
!     barycenter. If we're using stellar aberration
!     corrections, we'll need the observer's acceleration
!     as well.
!
!     Get the geometric state of the observer relative to the SSB,
!     which we'll call SSBOBS.
!
      call zzspkgo0 ( ctx, obs, et, ref, ssb, ssbobs, ssblt )

      if ( usestl ) then
!
!        Numerically differentiate the observer velocity relative to
!        the SSB to obtain acceleration. We first evaluate the
!        geometric state of the observer relative to the solar system
!        barycenter at ET +/- DELTA.

         do i = 1, 2

            t = et + (2*i - 3)*delta

            call zzspkgo0 ( ctx, obs, t, ref, ssb, stobs(1,i), ltssb )

         end do

         call qderiv ( ctx, 3, stobs(4,1), stobs(4,2), delta, acc )

      else

         call cleard ( 3, acc )

      end if

!
!     Look up the apparent state. The light time and light
!     rate are returned as well.
!
      call zzspkas0 ( ctx, targ,   et,  ref,   abcorr, &
                      ssbobs, acc, starg, lt,     dlt )

      return
      end associate
      end procedure zzspkac0

      module procedure zzspkez0
!! ## Abstract
!!
!!     Return the state (position and velocity) of a target body
!!     relative to an observing body, optionally corrected for light
!!     time (planetary aberration) and stellar aberration.
!!
!! ## Required Reading
!!
!!     SPK
!!     NAIF_IDS
!!     FRAMES
!!     TIME
!!
!! ## Keywords
!!
!!     EPHEMERIS
!!
!! ## Declarations
!!
!! ## Detailed Input
!!
!!     TARG        is the NAIF ID code for a target body. The target
!!                 and observer define a state vector whose position
!!                 component points from the observer to the target.
!!
!!     ET          is the ephemeris time, expressed as seconds past J2000
!!                 TDB, at which the state of the target body relative to
!!                 the observer is to be computed. ET refers to time at
!!                 the observer's location.
!!
!!     REF         is the name of the reference frame relative to which
!!                 the output state vector should be expressed. This may
!!                 be any frame supported by the SPICE system, including
!!                 built-in frames (documented in the Frames Required
!!                 Reading) and frames defined by a loaded frame kernel
!!                 (FK).
!!
!!                 When REF designates a non-inertial frame, the
!!                 orientation of the frame is evaluated at an epoch
!!                 dependent on the selected aberration correction.
!!                 See the description of the output state vector STARG
!!                 for details.
!!
!!     ABCORR      indicates the aberration corrections to be applied
!!                 to the state of the target body to account for one-way
!!                 light time and stellar aberration. See the discussion
!!                 in the Particulars section for recommendations on
!!                 how to choose aberration corrections.
!!
!!                 ABCORR may be any of the following:
!!
!!                    'NONE'     Apply no correction. Return the
!!                               geometric state of the target body
!!                               relative to the observer.
!!
!!                 The following values of ABCORR apply to the
!!                 "reception" case in which photons depart from the
!!                 target's location at the light-time corrected epoch
!!                 ET-LT and *arrive* at the observer's location at ET:
!!
!!                    'LT'       Correct for one-way light time (also
!!                               called "planetary aberration") using a
!!                               Newtonian formulation. This correction
!!                               yields the state of the target at the
!!                               moment it emitted photons arriving at
!!                               the observer at ET.
!!
!!                               The light time correction uses an
!!                               iterative solution of the light time
!!                               equation (see Particulars for details).
!!                               The solution invoked by the 'LT' option
!!                               uses one iteration.
!!
!!                    'LT+S'     Correct for one-way light time and
!!                               stellar aberration using a Newtonian
!!                               formulation. This option modifies the
!!                               state obtained with the 'LT' option to
!!                               account for the observer's velocity
!!                               relative to the solar system
!!                               barycenter. The result is the apparent
!!                               state of the target---the position and
!!                               velocity of the target as seen by the
!!                               observer.
!!
!!                    'CN'       Converged Newtonian light time
!!                               correction. In solving the light time
!!                               equation, the 'CN' correction iterates
!!                               until the solution converges (three
!!                               iterations on all supported platforms).
!!                               Whether the 'CN+S' solution is
!!                               substantially more accurate than the
!!                               'LT' solution depends on the geometry
!!                               of the participating objects and on the
!!                               accuracy of the input data. In all
!!                               cases this routine will execute more
!!                               slowly when a converged solution is
!!                               computed. See the Particulars section
!!                               below for a discussion of precision of
!!                               light time corrections.
!!
!!                    'CN+S'     Converged Newtonian light time
!!                               correction and stellar aberration
!!                               correction.
!!
!!                 The following values of ABCORR apply to the
!!                 "transmission" case in which photons *depart* from
!!                 the observer's location at ET and arrive at the
!!                 target's location at the light-time corrected epoch
!!                 ET+LT:
!!
!!                    'XLT'      "Transmission" case:  correct for
!!                               one-way light time using a Newtonian
!!                               formulation. This correction yields the
!!                               state of the target at the moment it
!!                               receives photons emitted from the
!!                               observer's location at ET.
!!
!!                    'XLT+S'    "Transmission" case:  correct for
!!                               one-way light time and stellar
!!                               aberration using a Newtonian
!!                               formulation  This option modifies the
!!                               state obtained with the 'XLT' option to
!!                               account for the observer's velocity
!!                               relative to the solar system
!!                               barycenter. The position component of
!!                               the computed target state indicates the
!!                               direction that photons emitted from the
!!                               observer's location must be "aimed" to
!!                               hit the target.
!!
!!                    'XCN'      "Transmission" case:  converged
!!                               Newtonian light time correction.
!!
!!                    'XCN+S'    "Transmission" case:  converged
!!                               Newtonian light time correction and
!!                               stellar aberration correction.
!!
!!                 Neither special nor general relativistic effects are
!!                 accounted for in the aberration corrections applied
!!                 by this routine.
!!
!!                 Case and blanks are not significant in the string
!!                 ABCORR.
!!
!!     OBS         is the NAIF ID code for an observing body.
!!
!! ## Detailed Output
!!
!!     STARG       is a Cartesian state vector representing the position
!!                 and velocity of the target body relative to the
!!                 specified observer. STARG is corrected for the
!!                 specified aberrations, and is expressed with respect
!!                 to the reference frame specified by REF. The first
!!                 three components of STARG represent the x-, y- and
!!                 z-components of the target's position; the last three
!!                 components form the corresponding velocity vector.
!!
!!                 The position component of STARG points from the
!!                 observer's location at ET to the aberration-corrected
!!                 location of the target. Note that the sense of the
!!                 position vector is independent of the direction of
!!                 radiation travel implied by the aberration
!!                 correction.
!!
!!                 The velocity component of STARG is the derivative
!!                 with respect to time of the position component of
!!                 STARG.
!!
!!                 Units are always km and km/sec.
!!
!!                 Non-inertial frames are treated as follows: letting
!!                 LTCENT be the one-way light time between the observer
!!                 and the central body associated with the frame, the
!!                 orientation of the frame is evaluated at ET-LTCENT,
!!                 ET+LTCENT, or ET depending on whether the requested
!!                 aberration correction is, respectively, for received
!!                 radiation, transmitted radiation, or is omitted.
!!                 LTCENT is computed using the method indicated by
!!                 ABCORR.
!!
!!     LT          is the one-way light time between the observer and
!!                 target in seconds. If the target state is corrected
!!                 for aberrations, then LT is the one-way light time
!!                 between the observer and the light time corrected
!!                 target location.
!!
!! ## Parameters
!!
!!     None.
!!
!! ## Exceptions
!!
!!     1) If the reference frame REF is not a recognized reference
!!        frame the error 'SPICE(UNKNOWNFRAME)' is signaled.
!!
!!     2) If the loaded kernels provide insufficient data to
!!        compute the requested state vector, the deficiency will
!!        be diagnosed by a routine in the call tree of this routine.
!!
!!     3) If an error occurs while reading an SPK or other kernel file,
!!        the error  will be diagnosed by a routine in the call tree
!!        of this routine.
!!
!!     4) If any of the required attributes of the reference frame REF
!!        cannot be determined, 'SPICE(UNKNOWNFRAME2)' is signaled.
!!
!! ## Files
!!
!!     This routine computes states using SPK files that have been
!!     loaded into the SPICE system, normally via the kernel loading
!!     interface routine FURNSH. See the routine FURNSH and the SPK
!!     and KERNEL Required Reading for further information on loading
!!     (and unloading) kernels.
!!
!!     If the output state STARG is to be expressed relative to a
!!     non-inertial frame, or if any of the ephemeris data used to
!!     compute STARG are expressed relative to a non-inertial frame in
!!     the SPK files providing those data, additional kernels may be
!!     needed to enable the reference frame transformations required to
!!     compute the state. Normally these additional kernels are PCK
!!     files or frame kernels. Any such kernels must already be loaded
!!     at the time this routine is called.
!!
!! ## Particulars
!!
!!     This routine is part of the user interface to the SPICE ephemeris
!!     system. It allows you to retrieve state information for any
!!     ephemeris object relative to any other in a reference frame that
!!     is convenient for further computations.
!!
!!     Aberration corrections
!!     ======================
!!
!!     In space science or engineering applications one frequently
!!     wishes to know where to point a remote sensing instrument, such
!!     as an optical camera or radio antenna, in order to observe or
!!     otherwise receive radiation from a target. This pointing problem
!!     is complicated by the finite speed of light:  one needs to point
!!     to where the target appears to be as opposed to where it actually
!!     is at the epoch of observation. We use the adjectives
!!     "geometric," "uncorrected," or "true" to refer to an actual
!!     position or state of a target at a specified epoch. When a
!!     geometric position or state vector is modified to reflect how it
!!     appears to an observer, we describe that vector by any of the
!!     terms "apparent," "corrected," "aberration corrected," or "light
!!     time and stellar aberration corrected." The SPICE Toolkit can
!!     correct for two phenomena affecting the apparent location of an
!!     object:  one-way light time (also called "planetary aberration")
!!     and stellar aberration.
!!
!!     One-way light time
!!     ------------------
!!
!!     Correcting for one-way light time is done by computing, given an
!!     observer and observation epoch, where a target was when the
!!     observed photons departed the target's location. The vector from
!!     the observer to this computed target location is called a "light
!!     time corrected" vector. The light time correction depends on the
!!     motion of the target relative to the solar system barycenter, but
!!     it is independent of the velocity of the observer relative to the
!!     solar system barycenter. Relativistic effects such as light
!!     bending and gravitational delay are not accounted for in the
!!     light time correction performed by this routine.
!!
!!     Stellar aberration
!!     ------------------
!!
!!     The velocity of the observer also affects the apparent location
!!     of a target:  photons arriving at the observer are subject to a
!!     "raindrop effect" whereby their velocity relative to the observer
!!     is, using a Newtonian approximation, the photons' velocity
!!     relative to the solar system barycenter minus the velocity of the
!!     observer relative to the solar system barycenter. This effect is
!!     called "stellar aberration."  Stellar aberration is independent
!!     of the velocity of the target. The stellar aberration formula
!!     used by this routine does not include (the much smaller)
!!     relativistic effects.
!!
!!     Stellar aberration corrections are applied after light time
!!     corrections:  the light time corrected target position vector is
!!     used as an input to the stellar aberration correction.
!!
!!     When light time and stellar aberration corrections are both
!!     applied to a geometric position vector, the resulting position
!!     vector indicates where the target "appears to be" from the
!!     observer's location.
!!
!!     As opposed to computing the apparent position of a target, one
!!     may wish to compute the pointing direction required for
!!     transmission of photons to the target. This also requires
!!     correction of the geometric target position for the effects of
!!     light time and stellar aberration, but in this case the
!!     corrections are computed for radiation traveling *from* the
!!     observer to the target.
!!
!!     The "transmission" light time correction yields the target's
!!     location as it will be when photons emitted from the observer's
!!     location at ET arrive at the target. The transmission stellar
!!     aberration correction is the inverse of the traditional stellar
!!     aberration correction:  it indicates the direction in which
!!     radiation should be emitted so that, using a Newtonian
!!     approximation, the sum of the velocity of the radiation relative
!!     to the observer and of the observer's velocity, relative to the
!!     solar system barycenter, yields a velocity vector that points in
!!     the direction of the light time corrected position of the target.
!!
!!     One may object to using the term "observer" in the transmission
!!     case, in which radiation is emitted from the observer's location.
!!     The terminology was retained for consistency with earlier
!!     documentation.
!!
!!     Below, we indicate the aberration corrections to use for some
!!     common applications:
!!
!!        1) Find the apparent direction of a target for a remote-sensing
!!           observation.
!!
!!              Use 'LT+S' or 'CN+S: apply both light time and stellar
!!              aberration corrections.
!!
!!           Note that using light time corrections alone ('LT' or 'CN')
!!           is generally not a good way to obtain an approximation to
!!           an apparent target vector: since light time and stellar
!!           aberration corrections often partially cancel each other,
!!           it may be more accurate to use no correction at all than to
!!           use light time alone.
!!
!!        2) Find the corrected pointing direction to radiate a signal
!!           to a target. This computation is often applicable for
!!           implementing communications sessions.
!!
!!              Use 'XLT+S' or 'XCN+S: apply both light time and stellar
!!              aberration corrections for transmission.
!!
!!        3) Compute the apparent position of a target body relative
!!           to a star or other distant object.
!!
!!              Use 'LT', 'CN', 'LT+S', or 'CN+S' as needed to match the
!!              correction applied to the position of the distant
!!              object. For example, if a star position is obtained from
!!              a catalog, the position vector may not be corrected for
!!              stellar aberration. In this case, to find the angular
!!              separation of the star and the limb of a planet, the
!!              vector from the observer to the planet should be
!!              corrected for light time but not stellar aberration.
!!
!!        4) Obtain an uncorrected state vector derived directly from
!!           data in an SPK file.
!!
!!              Use 'NONE'.
!!
!!        5) Use a geometric state vector as a low-accuracy estimate
!!           of the apparent state for an application where execution
!!           speed is critical.
!!
!!              Use 'NONE'.
!!
!!        6) While this routine cannot perform the relativistic
!!           aberration corrections required to compute states
!!           with the highest possible accuracy, it can supply the
!!           geometric states required as inputs to these computations.
!!
!!              Use 'NONE', then apply relativistic aberration
!!              corrections (not available in the SPICE Toolkit).
!!
!!     Below, we discuss in more detail how the aberration corrections
!!     applied by this routine are computed.
!!
!!        Geometric case
!!        ==============
!!
!!        SPKEZ begins by computing the geometric position T(ET) of the
!!        target body relative to the solar system barycenter (SSB).
!!        Subtracting the geometric position of the observer O(ET) gives
!!        the geometric position of the target body relative to the
!!        observer. The one-way light time, LT, is given by
!!
!!                  | T(ET) - O(ET) |
!!           LT = -------------------
!!                          c
!!
!!        The geometric relationship between the observer, target, and
!!        solar system barycenter is as shown:
!!
!!           SSB ---> O(ET)
!!            |      /
!!            |     /
!!            |    /
!!            |   /  T(ET) - O(ET)
!!            V  V
!!           T(ET)
!!
!!        The returned state consists of the position vector
!!
!!           T(ET) - O(ET)
!!
!!        and a velocity obtained by taking the difference of the
!!        corresponding velocities. In the geometric case, the
!!        returned velocity is actually the time derivative of the
!!        position.
!!
!!        Reception case
!!        ==============
!!
!!        When any of the options 'LT', 'CN', 'LT+S', 'CN+S' is selected
!!        for ABCORR, SPKEZ computes the position of the target body at
!!        epoch ET-LT, where LT is the one-way light time. Let T(t) and
!!        O(t) represent the positions of the target and observer
!!        relative to the solar system barycenter at time t; then LT is
!!        the solution of the light-time equation
!!
!!                  | T(ET-LT) - O(ET) |
!!           LT = ------------------------                            (1)
!!                           c
!!
!!        The ratio
!!
!!            | T(ET) - O(ET) |
!!          ---------------------                                     (2)
!!                    c
!!
!!        is used as a first approximation to LT; inserting (2) into the
!!        right hand side of the light-time equation (1) yields the
!!        "one-iteration" estimate of the one-way light time ("LT").
!!        Repeating the process until the estimates of LT converge
!!        yields the "converged Newtonian" light time estimate ("CN").
!!
!!        Subtracting the geometric position of the observer O(ET) gives
!!        the position of the target body relative to the observer:
!!        T(ET-LT) - O(ET).
!!
!!           SSB ---> O(ET)
!!            | \     |
!!            |  \    |
!!            |   \   | T(ET-LT) - O(ET)
!!            |    \  |
!!            V     V V
!!           T(ET)  T(ET-LT)
!!
!!        The position component of the light time corrected state
!!        is the vector
!!
!!           T(ET-LT) - O(ET)
!!
!!        The velocity component of the light time corrected state
!!        is the difference
!!
!!           T_vel(ET-LT)*(1-dLT/dET) - O_vel(ET)
!!
!!        where T_vel and O_vel are, respectively, the velocities of the
!!        target and observer relative to the solar system barycenter at
!!        the epochs ET-LT and ET.
!!
!!        If correction for stellar aberration is requested, the target
!!        position is rotated toward the solar system barycenter-
!!        relative velocity vector of the observer. The rotation is
!!        computed as follows:
!!
!!           Let r be the light time corrected vector from the observer
!!           to the object, and v be the velocity of the observer with
!!           respect to the solar system barycenter. Let w be the angle
!!           between them. The aberration angle phi is given by
!!
!!              sin(phi) = v sin(w) / c
!!
!!           Let h be the vector given by the cross product
!!
!!              h = r X v
!!
!!           Rotate r by phi radians about h to obtain the apparent
!!           position of the object.
!!
!!        When stellar aberration corrections are used, the rate of
!!        change of the stellar aberration correction is accounted for
!!        in the computation of the output velocity.
!!
!!        Transmission case
!!        ==================
!!
!!        When any of the options 'XLT', 'XCN', 'XLT+S', 'XCN+S' is
!!        selected, SPKEZ computes the position of the target body T at
!!        epoch ET+LT, where LT is the one-way light time. LT is the
!!        solution of the light-time equation
!!
!!                  | T(ET+LT) - O(ET) |
!!           LT = ------------------------                            (3)
!!                            c
!!
!!        Subtracting the geometric position of the observer, O(ET),
!!        gives the position of the target body relative to the
!!        observer: T(ET-LT) - O(ET).
!!
!!                   SSB --> O(ET)
!!                  / |    *
!!                 /  |  *  T(ET+LT) - O(ET)
!!                /   |*
!!               /   *|
!!              V  V  V
!!          T(ET+LT)  T(ET)
!!
!!        The position component of the light-time corrected state
!!        is the vector
!!
!!           T(ET+LT) - O(ET)
!!
!!        The velocity component of the light-time corrected state
!!        consists of the difference
!!
!!           T_vel(ET+LT)*(1+dLT/dET) - O_vel(ET)
!!
!!        where T_vel and O_vel are, respectively, the velocities of the
!!        target and observer relative to the solar system barycenter at
!!        the epochs ET+LT and ET.
!!
!!        If correction for stellar aberration is requested, the target
!!        position is rotated away from the solar system barycenter-
!!        relative velocity vector of the observer. The rotation is
!!        computed as in the reception case, but the sign of the
!!        rotation angle is negated. Velocities are adjusted to account
!!        for the rate of change of the stellar aberration correction.
!!
!!     Precision of light time corrections
!!     ===================================
!!
!!        Corrections using one iteration of the light time solution
!!        ----------------------------------------------------------
!!
!!        When the requested aberration correction is 'LT', 'LT+S',
!!        'XLT', or 'XLT+S', only one iteration is performed in the
!!        algorithm used to compute LT.
!!
!!        The relative error in this computation
!!
!!           | LT_ACTUAL - LT_COMPUTED |  /  LT_ACTUAL
!!
!!        is at most
!!
!!            (V/C)**2
!!           ----------
!!            1 - (V/C)
!!
!!        which is well approximated by (V/C)**2, where V is the
!!        velocity of the target relative to an inertial frame and C is
!!        the speed of light.
!!
!!        For nearly all objects in the solar system V is less than 60
!!        km/sec. The value of C is ~300000 km/sec. Thus the
!!        one-iteration solution for LT has a potential relative error
!!        of not more than 4e-8. This is a potential light time error of
!!        approximately 2e-5 seconds per astronomical unit of distance
!!        separating the observer and target. Given the bound on V cited
!!        above:
!!
!!           As long as the observer and target are separated by less
!!           than 50 astronomical units, the error in the light time
!!           returned using the one-iteration light time corrections is
!!           less than 1 millisecond.
!!
!!           The magnitude of the corresponding position error, given
!!           the above assumptions, may be as large as (V/C)**2 * the
!!           distance between the observer and the uncorrected target
!!           position: 300 km or equivalently 6 km/AU.
!!
!!        In practice, the difference between positions obtained using
!!        one-iteration and converged light time is usually much smaller
!!        than the value computed above and can be insignificant. For
!!        example, for the spacecraft Mars Reconnaissance Orbiter and
!!        Mars Express, the position error for the one-iteration light
!!        time correction, applied to the spacecraft-to-Mars center
!!        vector, is at the 1 cm level.
!!
!!        Comparison of results obtained using the one-iteration and
!!        converged light time solutions is recommended when adequacy of
!!        the one-iteration solution is in doubt.
!!
!!        Converged corrections
!!        ---------------------
!!
!!        When the requested aberration correction is 'CN', 'CN+S',
!!        'XCN', or 'XCN+S', as many iterations as are required for
!!        convergence are performed in the computation of LT. Usually
!!        the solution is found after three iterations. The relative
!!        error present in this case is at most
!!
!!            (V/C)**4
!!           ----------
!!            1 - (V/C)
!!
!!        which is well approximated by (V/C)**4.
!!
!!           The precision of this computation (ignoring round-off
!!           error) is better than 4e-11 seconds for any pair of objects
!!           less than 50 AU apart, and having speed relative to the
!!           solar system barycenter less than 60 km/s.
!!
!!           The magnitude of the corresponding position error, given
!!           the above assumptions, may be as large as (V/C)**4 * the
!!           distance between the observer and the uncorrected target
!!           position: 1.2 cm at 50 AU or equivalently 0.24 mm/AU.
!!
!!        However, to very accurately model the light time between
!!        target and observer one must take into account effects due to
!!        general relativity. These may be as high as a few hundredths
!!        of a millisecond for some objects.
!!
!!     Relativistic Corrections
!!     =========================
!!
!!     This routine does not attempt to perform either general or
!!     special relativistic corrections in computing the various
!!     aberration corrections. For many applications relativistic
!!     corrections are not worth the expense of added computation
!!     cycles. If however, your application requires these additional
!!     corrections we suggest you consult the astronomical almanac (page
!!     B36) for a discussion of how to carry out these corrections.
!!
!! ## Examples
!!
!!     1)  Load a planetary ephemeris SPK; then look up a series of
!!         geometric states of the moon relative to the earth,
!!         referenced to the J2000 frame.
!!
!!               IMPLICIT NONE
!!         C
!!         C     Local constants
!!         C
!!               CHARACTER*(*)         FRAME
!!               PARAMETER           ( FRAME  = 'J2000' )
!!
!!               CHARACTER*(*)         ABCORR
!!               PARAMETER           ( ABCORR = 'NONE' )
!!
!!         C
!!         C     The name of the SPK file shown here is fictitious;
!!         C     you must supply the name of an SPK file available
!!         C     on your own computer system.
!!         C
!!               CHARACTER*(*)         SPK
!!               PARAMETER           ( SPK    = 'planet.bsp' )
!!
!!         C
!!         C     ET0 represents the date 2000 Jan 1 12:00:00 TDB.
!!         C
!!               DOUBLE PRECISION      ET0
!!               PARAMETER           ( ET0    = 0.0D0 )
!!
!!         C
!!         C     Use a time step of 1 hour; look up 100 states.
!!         C
!!               DOUBLE PRECISION      STEP
!!               PARAMETER           ( STEP   = 3600.0D0 )
!!
!!               INTEGER               MAXITR
!!               PARAMETER           ( MAXITR = 100 )
!!
!!         C
!!         C     The NAIF IDs of the earth and moon are 399 and 301
!!         C     respectively.
!!         C
!!               INTEGER               OBSRVR
!!               PARAMETER           ( OBSRVR = 399 )
!!
!!               INTEGER               TARGET
!!               PARAMETER           ( TARGET = 301 )
!!
!!         C
!!         C     Local variables
!!         C
!!               DOUBLE PRECISION      ET
!!               DOUBLE PRECISION      LT
!!               DOUBLE PRECISION      STATE ( 6 )
!!
!!               INTEGER               I
!!
!!         C
!!         C     Load the SPK file.
!!         C
!!               CALL FURNSH ( SPK )
!!
!!         C
!!         C     Step through a series of epochs, looking up a
!!         C     state vector at each one.
!!         C
!!               DO I = 1, MAXITR
!!
!!                  ET = ET0 + (I-1)*STEP
!!
!!                  CALL SPKEZ ( TARGET, ET, FRAME, ABCORR, OBSRVR,
!!              .                STATE,  LT                        )
!!
!!                  WRITE (*,*) 'ET = ', ET
!!                  WRITE (*,*) 'J2000 x-position (km):   ', STATE(1)
!!                  WRITE (*,*) 'J2000 y-position (km):   ', STATE(2)
!!                  WRITE (*,*) 'J2000 z-position (km):   ', STATE(3)
!!                  WRITE (*,*) 'J2000 x-velocity (km/s): ', STATE(4)
!!                  WRITE (*,*) 'J2000 y-velocity (km/s): ', STATE(5)
!!                  WRITE (*,*) 'J2000 z-velocity (km/s): ', STATE(6)
!!                  WRITE (*,*) ' '
!!
!!               END DO
!!
!!               END
!!
!! ## Restrictions
!!
!!     None.
!!
!! ## Literature References
!!
!!     SPK Required Reading.
!!
!! ## Author and Institution
!!
!!     C.H. Acton      (JPL)
!!     W.L. Taber      (JPL)
!!     N.J. Bachman    (JPL)
!!     J.E. McLean     (JPL)
!!     H.A. Neilan     (JPL)
!!     B.V. Semenov    (JPL)
!!     M.J. Spencer    (JPL)
!!     I.M. Underwood  (JPL)
!!
!! ## Version
!!
!!-    SPICELIB Version 5.1.0, 03-JUL-2014 (NJB) (BVS)
!!
!!        Discussion of light time corrections was updated. Assertions
!!        that converged light time corrections are unlikely to be
!!        useful were removed.
!!
!!-    Last update was 23-SEP-2013 (NJB) (BVS)
!!
!!        Bug fix: replaced calls to ZZPRSCOR with calls to
!!        ZZVALCOR. The latter routine rejects all aberration
!!        corrections not supported by the SPK subsystem.
!!
!!        Bug fix: added a check and an exception for the FOUND flag
!!        returned by FRINFO.
!!
!!        Updated to save the input frame name and POOL state counter
!!        and to do frame name-ID conversion only if the counter has
!!        changed.
!!
!!        Updated various in-line comments.
!!
!!-    SPICELIB Version 5.0.1, 18-MAY-2010 (BVS)
!!
!!        Removed "C$" marker from text in the header.
!!
!!-    SPICELIB Version 5.0.0, 27-DEC-2007 (NJB)
!!
!!        This routine was upgraded to more accurately compute
!!        aberration-corrected velocity, and in particular, make it
!!        more consistent with observer-target positions.
!!
!!        When light time corrections are used, the derivative of light
!!        time with respect to time is now accounted for in the
!!        computation of observer-target velocities. When the reference
!!        frame associated with the output state is time-dependent, the
!!        derivative of light time with respect to time is now accounted
!!        for in the computation of the rate of change of orientation of
!!        the reference frame.
!!
!!        When stellar aberration corrections are used, velocities
!!        now reflect the rate of range of the stellar aberration
!!        correction.
!!
!!-    SPICELIB Version 4.1.0, 05-JAN-2005 (NJB)
!!
!!        Tests of routine FAILED() were added.
!!        Minor header error was corrected.
!!
!!-    SPICELIB Version 4.0.2, 20-OCT-2003 (EDW)
!!
!!        Added mention that LT returns in seconds.
!!
!!-    SPICELIB Version 4.0.1, 29-JUL-2003 (NJB) (CHA)
!!
!!        Various minor header changes were made to improve clarity.
!!
!!-    SPICELIB Version 4.0.0, 28-DEC-2001 (NJB)
!!
!!        Updated to handle aberration corrections for transmission
!!        of radiation. Formerly, only the reception case was
!!        supported. The header was revised and expanded to explain
!!        the functionality of this routine in more detail.
!!
!!-    SPICELIB Version 3.1.0, 09-JUL-1996 (WLT)
!!
!!        Corrected the description of LT in the Detailed Output
!!        section of the header.
!!
!!-    SPICELIB Version 3.0.0, 26-MAY-1995 (WLT)
!!
!!        The routine was upgraded to support non-inertial frames.
!!
!!-    SPICELIB Version 2.1.1, 5-AUG-1994 (HAN) (MJS)
!!
!!        Added code so that routine accepts lower case, mixed case
!!        and upper case versions of the string ABCORR.
!!
!!-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
!!
!!        Comment section for permuted index source lines was added
!!        following the header.
!!
!!-    SPICELIB Version 2.0.0, 18-JUL-1991 (JEM) (NJB)
!!
!!        The old SPKEZ did not compute the geometric state of one body
!!        with respect to another unless data existed for each body with
!!        respect to the solar system barycenter.
!!
!!-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
!!
!!        Literature references added to the header.
!!
!!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
!!
!! ## Index Entries
!!
!!     easy reader for spk file
!!     get state relative observer corrected for aberrations
!!     read ephemeris data
!!     read trajectory data
!!
!! ## Revisions
!!
!!-    SPICELIB Version 5.0.0, 22-JUL-2007 (NJB)
!!
!!        Routine was upgraded to more accurately compute aberration-
!!        corrected velocity, and in particular, make it more consistent
!!        with observer-target positions. When light time corrections
!!        are used:
!!
!!           1) The derivative of light time with respect
!!              to time is now accounted for in the computation
!!              of observer-target velocities, for all types
!!              of reference frames.
!!
!!           2) The derivative of light time with respect
!!              to time is now accounted for in the computation of the
!!              rate of change of orientation of time-dependent
!!              reference frames for the output state. This rate of
!!              change affects observer-target velocities.
!!
!!        When stellar aberration corrections are used, velocities
!!        now reflect the rate of range of the stellar aberration
!!        correction.
!!
!!        This routine was modified as follows:
!!
!!           - SPKAPP is no longer called; it has been superseded
!!             by SPKACS. Aberration-corrected states relative to
!!             inertial frames are computed by SPKACS.
!!
!!           - The effect of the rate of change of light time on the
!!             rate of change of orientation of non-inertial output
!!             frames is accounted for in this routine. See the code
!!             near the end of this source file.
!!
!!        The header of this routine has been updated to reflect the
!!        upgrades described here.
!!
!!        As a separate upgrade, the method by which the aberration
!!        correction flag is parsed has been made more robust: parsing
!!        is now done by the routine ZZZPRSCOR. The new parsing
!!        technique calls for parsing the input string only when it
!!        differs from the previous value.
!!
!!-    SPICELIB Version 4.1.0, 05-JAN-2005 (NJB)
!!
!!        Tests of routine FAILED() were added. The new checks
!!        are intended to prevent arithmetic operations from
!!        being performed with uninitialized or invalid data.
!!
!!        Minor header error was corrected.
!!
!!-    SPICELIB Version 3.1.0, 09-JUL-1996 (WLT)
!!
!!        Corrected the description of LT in the Detailed Output
!!        section of the header.
!!
!!-    SPICELIB Version 3.0.0, 26-MAY-1995 (WLT)
!!
!!        The routine was upgraded so that it can now support
!!        non-inertial reference frames. In additions some
!!        of the error messages were slightly enhanced.
!!
!!-    SPICELIB Version 2.1.1, 5-AUG-1994 (HAN) (MJS)
!!
!!        Added code so that routine accepts lower case, mixed case
!!        and upper case versions of the string ABCORR.
!!
!!-    SPICELIB Version 2.0.0, 18-JUL-1991 (JEM) (NJB)
!!
!!        The previous version of SPKEZ could not
!!        compute the geometric state (no aberration
!!        correction) of one body with respect to
!!        another if the ephemeris data for each
!!        body relative to the Solar System Barycenter
!!        (body 0) had not been loaded. Now, if
!!        sufficient data is loaded, SPKEZ can always
!!        compute the state.
!!
!!        For example, suppose the file GLL.BSP contains
!!        segments of SPK data for the Galileo spacecraft
!!        (body -77) relative to the Jupiter Barycenter
!!        (body 5) over a period of time. If SPKEZ Version
!!        1.0.0 was called to compute the geometric state of
!!        -77 relative to 5 (or vice versa), a routine that
!!        SPKEZ calls, SPKSSB, would signal an error stating
!!        that there is insufficient data for computing the
!!        state of body 5 (relative to 0). Version 1.0.0
!!        of SPKEZ could not compute the requested state even
!!        though sufficient data had been loaded.
!!
!!        It is necessary to compute the states of each
!!        of the target and observing bodies relative to
!!        the solar system barycenter when aberration
!!        corrections are being applied. However, when
!!        computing geometric states, it is only necessary
!!        to trace back to the first common node. Positive
!!        side effects include the maintenance of precision
!!        and reduction in number of look ups.
!!
!!        The changes to the code in SPKEZ involved calling a new
!!        routine, SPKGEO, which computes the geometric state if
!!        no aberration corrections are requested.
!!
!!        The other cosmetic changes include the removal of a reference
!!        to the SPK User's Guide in Literature_References because
!!        the User's Guide is the same as SPK Required Reading.
!!
!!        Also, the item in Restrictions previously said
!!
!!           1) The ephemeris files to be used by SPKEZ must be loaded
!!              by SPKLEF before SPKSSB is called.
!!
!!        SPKSSB was replaced with SPKEZ.
!!
!!        The location of the position and velocity information in the
!!        output state vector argument STARG is now spelled out.
!!
!!        Finally, the Particulars section was updated. In Version
!!        1.0.0 it said that calling SPKEZ was equivalent to calling
!!        SPKSSB and SPKAPP.
!!
!!     SPICELIB functions
!!
!!     Local parameters
!!
!!     Saved frame name length.
!!
!!     Local variables
!!
!!     Saved frame name/ID item declarations.
!!
!!     Saved variables
!!
!!     Initial values
!!
!!     Standard SPICE error handling.
!!
      use pepper_kinds, only: ip, wp
      use pepper_math, only: mxvg, vsclip
      use pepper_inc_frmtyp
      use pepper_inc_zzabcorr
      use pepper_inc_zzctr
      implicit none
      character(*), parameter :: rname = 'ZZSPKEZ0'
      integer(ip), parameter :: frnmln = 32
      real(wp) :: state    ( 6    )
      real(wp) :: ltcent
      integer(ip) :: ltsign
      real(wp) :: dltctr
      real(wp) :: dlt
      real(wp) :: temp     ( 6    )
      logical :: attblk ( nabcor )
      logical :: found
      integer(ip) :: i
      real(wp) :: xform    ( 6, 6 )
      integer(ip) :: type
      real(wp) :: stobs    ( 6    )
      integer(ip) :: typeid
      integer(ip) :: reqfrm
      integer(ip) :: center
      if ( .not. allocated ( ctx%zzspkez0_state ) ) then
         allocate ( ctx%zzspkez0_state )
         call init_zzspkez0_state ( ctx%zzspkez0_state )
      end if
      associate ( prvcor => ctx%zzspkez0_state%prvcor, &
                  fj2000 => ctx%zzspkez0_state%fj2000, &
                  usegeo => ctx%zzspkez0_state%usegeo, &
                  xmit => ctx%zzspkez0_state%xmit, &
                  svctr1 => ctx%zzspkez0_state%svctr1, &
                  svref => ctx%zzspkez0_state%svref, &
                  svreqf => ctx%zzspkez0_state%svreqf, &
                  first => ctx%zzspkez0_state%first )
      if ( ctx%failed() ) return

!
!     Counter initialization is done separately.
!
      if ( first ) then

!
!        Initialize counter.
!
         call zzctruin( ctx, svctr1 )

      end if

      if (  first  .or.  ( abcorr /= prvcor )  ) then
!
!        The aberration correction flag differs from the value it
!        had on the previous call, if any. Analyze the new flag.
!
         call zzvalcor ( ctx, abcorr, attblk )

         if ( ctx%failed() ) return

!
!        The aberration correction flag is recognized; save it.
!
         prvcor = abcorr

!
!        Set logical flags indicating the attributes of the requested
!        correction:
!
!           XMIT is .TRUE. when the correction is for transmitted
!           radiation.
!
!           USEGEO indicates geometric state computation.
!
!        The above definitions are consistent with those used by
!        ZZVALCOR.
!
         xmit    =  attblk ( xmtidx )
         usegeo  =  attblk ( geoidx )

!
!        Get the frame ID for J2000 on the first call to this routine.
!
         if ( first ) then

            call namfrm( ctx, 'J2000', fj2000 )

            first   = .false.

         end if

      end if

!
!     If we only want a geometric state, then use SPKGEO to compute
!     just that.
!
!     Otherwise, if REF is inertial, compute the state of the target
!     relative to the observer via SPKACS. If REF is non-inertial,
!     compute the requested state in the J2000 frame, then transform it
!     to the frame designated by REF.
!
      if ( usegeo ) then

         call zzspkgo0 ( ctx, targ, et, ref, obs, starg, lt )

      else
!
!        Get the auxiliary information about the requested output
!        frame.
!
         call zznamfrm ( ctx, svctr1, svref, svreqf, ref, reqfrm )

         if ( reqfrm == 0 ) then
            call ctx%setmsg ( 'The requested output frame ''#'' is ' &
            //            'not recognized by the reference frame ' &
            //            'subsystem. Please check that the ' &
            //            'appropriate kernels have been loaded ' &
            //            'and that you have correctly entered ' &
            //            'the name of the output frame. ' )
            call ctx%errch  ( '#', ref )
            call ctx%sigerr ( 'SPICE(UNKNOWNFRAME)'  )
            return

         end if

         call frinfo ( ctx, reqfrm, center, type, typeid, found )

         if ( ctx%failed() ) return

         if ( .not. found ) then

            call ctx%setmsg ( 'The requested output frame ''#'' is ' &
            //            'not recognized by the reference frame ' &
            //            'subsystem. Please check that the ' &
            //            'appropriate kernels have been loaded ' &
            //            'and that you have correctly entered ' &
            //            'the name of the output frame. ' )
            call ctx%errch  ( '#', ref )
            call ctx%sigerr ( 'SPICE(UNKNOWNFRAME2)' )

            return

         end if

!
!        If we are dealing with an inertial frame, we can simply
!        call SPKACS and return.
!
         if ( type == inertl ) then

            call zzspkac0 ( ctx, targ, et, ref, abcorr, obs, starg, lt, dlt )
            return

         end if
!
!        Still here?
!
!        We are dealing with a non-inertial frame. But we need to do
!        light time and stellar aberration corrections in an inertial
!        frame. Get the "apparent" state of TARG in the intermediary
!        inertial reference frame J2000.
!
!        We also need the light time to the center of the frame.
!        We compute that first so that we can re-use the temporary
!        variable STATE when we compute the inertial apparent state
!        of the target relative to the observer.
!
         call zzspkac0 ( ctx, targ, et,    'J2000', abcorr, &
                         obs,  state, lt,      dlt     )

         if ( ctx%failed() ) return

         if ( center == obs ) then

            ltcent = 0.0_wp
            dltctr = 0.0_wp

         else if ( center == targ ) then

            ltcent = lt
            dltctr = dlt

         else

            call zzspksb0 ( ctx, obs,    et,     'J2000',         stobs )
            call zzspklt0 ( ctx, center, et,     'J2000', abcorr, stobs, &
                            temp,   ltcent, dltctr                 )
         end if

!
!        If something went wrong (like we couldn't get the state of
!        the center relative to the observer) now it is time to quit.
!
         if ( ctx%failed() ) return

!
!        If the aberration corrections are for transmission, make the
!        sign of the light time positive, since we wish to compute the
!        orientation of the non-inertial frame at an epoch later than
!        ET by the one-way light time.
!
         if ( xmit ) then

            ltsign =  1
         else
            ltsign = -1
         end if

!
!        Get the state transformation from J2000 to the requested frame
!        and convert the state.
!
         call zzfrmch0 ( ctx, fj2000, reqfrm, et + ltsign*ltcent, xform )

         if ( ctx%failed() ) return

!
!        There's a tricky bit here:  since XFORM is evaluated
!        at time
!
!           ET + LTSIGN*LTCENT
!
!        XFORM is actually dependent on LTCENT. We need to account for
!        this dependency in our velocity transformation.
!
!        Let P and V be the target position and velocity respectively,
!        and R, DR be the rotation and rotation derivative
!        corresponding to XFORM.
!
!        The state transformation we need to perform is not
!
!           R * V   +  DR * P
!
!        but rather
!
!           R * V   +  ( (1 + LTSIGN*DLTCTR) * DR )  * P
!
!        So we'll scale the derivative block of XFORM accordingly.
!
         do i = 1, 3

            call vsclip ( 1.0_wp + ltsign*dltctr, xform(4,i))

         end do

!
!        Now apply the frame transformation XFORM to produce the
!        state expressed relative to the request frame REQFRM.
!
         call mxvg ( xform, state, 6, 6, starg )

      end if

      return
      end associate
      end procedure zzspkez0

      module procedure zzdynrt0
!! ## Abstract
!!
!!     SPICE Private routine intended solely for the support of SPICE
!!     routines.  Users should not call this routine directly due
!!     to the volatile nature of this routine.
!!
!!     For a specified dynamic frame, find the rotation that maps
!!     positions from the dynamic frame to its base frame.
!!
!! ## Required Reading
!!
!!     CK
!!     FRAMES
!!     PCK
!!     SPK
!!
!! ## Keywords
!!
!!     FRAMES
!!     PRIVATE
!!
!! ## Declarations
!!
!! ## Detailed Input
!!
!!     INFRAM      is the frame ID code for a dynamic reference frame.
!!                 Note that this interface differs from that of TKFRAM,
!!                 which uses a class ID to identify the frame.
!!
!!                 In this routine, we refer this frame both as the
!!                 "input frame" and the "defined frame."
!!
!!     CENTER      is NAIF ID code for the center of the frame
!!                 designated by INFRAM.  This code, although derivable
!!                 from INFRAM, is passed in for convenience.
!!
!!     ET          is an epoch in ephemeris seconds past J2000 for which
!!                 the caller requests a rotation matrix.
!!
!! ## Detailed Output
!!
!!     ROTATE      is a 3x3 rotation matrix that transforms positions
!!                 relative to INFRAM to positions relative to BASFRM.
!!
!!     BASFRM      is the frame ID code of the base frame associated
!!                 with INFRAM.  The 3x3 matrix ROTATE transforms
!!                 positions relative to INFRAM to positions relative to
!!                 BASFRM. The position transformation is performed by
!!                 left-multiplying by ROTATE a position expressed
!!                 relative to INFRAM. This is easily accomplished via
!!                 the subroutine call shown below.
!!
!!                    CALL MXV ( ROTATE, INPOS, OUTPOS )
!!
!! ## Parameters
!!
!!     See include file zzdyn.inc.
!!
!! ## Exceptions
!!
!!     1)  If a dynamic frame evaluation requires unavailable kernel
!!         data, the error will be diagnosed by routines in the
!!         call tree of this routine.
!!
!!     2)  If a precession model is used to implement a frame centered
!!         at a body for which the model is not applicable, the error
!!         SPICE(INVALIDSELECTION) will be signaled.
!!
!!     3)  If a nutation model is used to implement a frame centered
!!         at a body for which the model is not applicable, the error
!!         SPICE(INVALIDSELECTION) will be signaled.
!!
!!     4)  If an obliquity model is used to implement a frame centered
!!         at a body for which the model is not applicable, the error
!!         SPICE(INVALIDSELECTION) will be signaled.
!!
!!     5)  If an unrecognized precession model is specified, the
!!         error SPICE(NOTSUPPORTED) is signaled.
!!
!!     6)  If an unrecognized nutation model is specified, the
!!         error SPICE(NOTSUPPORTED) is signaled.
!!
!!     7)  If an unrecognized obliquity model is specified, the
!!         error SPICE(NOTSUPPORTED) is signaled.
!!
!!     8)  If an attempt to look up the center of a frame does
!!         not yield data, the error SPICE(FRAMEDATANOTFOUND) is
!!         signaled.
!!
!!     9)  In a two-vector frame definition, if a constant vector
!!         specification method is not recognized, the error
!!         SPICE(NOTSUPPORTED) is signaled.
!!
!!     10) In a two-vector frame definition, if a vector definition
!!         method is not recognized, the error SPICE(NOTSUPPORTED)
!!         is signaled.
!!
!!     11) If an unrecognized dynamic frame family is specified, the
!!          error SPICE(NOTSUPPORTED) is signaled.
!!
!!     12) If an unrecognized dynamic frame definition style is
!!         specified, the error SPICE(NOTSUPPORTED) is signaled.
!!
!!     13) If an unrecognized dynamic frame rotation state is
!!         specified, the error SPICE(NOTSUPPORTED) is signaled.
!!
!!     14) If both a freeze epoch and a rotation state are specified,
!!         the error SPICE(FRAMEDEFERROR) is signaled.
!!
!!     15) If neither a freeze epoch nor a rotation state are specified
!!         for an "of date" frame, the error SPICE(FRAMEDEFERROR) is
!!         signaled.
!!
!!     16) In a two-vector frame definition, if an invalid axis
!!         specification is encountered, the error SPICE(INVALIDAXIS) is
!!         signaled.
!!
!!     17) In a two-vector frame definition using a target near point
!!         vector, if the body-fixed frame associated with the target
!!         is not found, the error SPICE(FRAMEDATANOTFOUND) is signaled.
!!
!!     18) If a dynamic frame evaluation requires excessive recursion
!!         depth, the error will be diagnosed by routines in the call
!!         tree of this routine.
!!
!!     19) When a two-vector dynamic frame is evaluated, if the
!!         primary and secondary vectors have angular separation less
!!         than the minimum allowed value, or if the angular separation
!!         differs from Pi by less than the minimum allowed value, the
!!         error SPICE(DEGENERATECASE) is signaled.  The default minimum
!!         separation is given by the parameter LBSEP; this value may be
!!         overridden by supplying a different value in the frame
!!         definition.
!!
!!     20) If invalid units occur in a frame definition, the error
!!         will be diagnosed by a routine in the call tree of this
!!         routine.
!!
!!     21) If an invalid Euler axis sequence occurs in a frame
!!         definition, the error will be diagnosed by a routine in the
!!         call tree of this routine.
!!
!!     22) If a body RADII vector has other than exactly thee elements,
!!         the error SPICE(INVALIDCOUNT) is signaled by a routine in
!!         the call tree of this routine.
!!
!!     23) If a body RADII vector has any element less-than or
!!         equal to zero, the error SPICE(BADAXISLENGTH) is signaled by
!!         a routine in the call tree of this routine.
!!
!! ## Files
!!
!!     1) SPK files containing data for each observer and target
!!        are required to support two-vector frames.  Note that
!!        observer-target pairs can be implicit, as in the case
!!        of a constant vector whose frame is evaluated at a
!!        light-time corrected epoch:  the light time the frame
!!        center to an observer must be computable in this case,
!!        which implies the state of the frame center as seen by
!!        the observer must be computable.
!!
!!     2) Any of SPK, CK, PCK, and frame kernels will also be required
!!        if any frames referenced in the definition of INFRAM (as a
!!        base frame, velocity vector frame, or constant vector frame)
!!        require them, or if any vectors used to define INFRAM require
!!        these data in order to be computable.
!!
!!     3) When CK data are required, one or more associated SCLK kernels
!!        ---normally, one kernel per spacecraft clock---are
!!        required as well.  A leapseconds kernel may be required
!!        whenever an SCLK kernel is required.
!!
!!     4) When a two-vector frame is defined using a target near point,
!!        a PCK file giving orientation and providing a triaxial shape
!!        model for the target body is required.
!!
!! ## Particulars
!!
!!     Currently only parameterized dynamic frames are supported by
!!     this routine.
!!
!!     Currently supported parameterized dynamic families are:
!!
!!        Two-vector
!!        ==========
!!
!!           Vector definitions
!!           ------------------
!!           Observer-target position
!!           Observer-target velocity
!!           Near point on target
!!           Constant vector in specified frame
!!
!!        Mean Equator and Equinox of Date
!!        ================================
!!
!!           Bodies and models
!!           -----------------
!!           Earth:  1976 IAU precession model
!!
!!        Mean Ecliptic and Equinox of Date
!!        ================================
!!
!!           Bodies and models
!!           -----------------
!!           Earth:  1976 IAU precession model
!!                   1980 IAU mean obliquity model
!!
!!        True Equator and Equinox of Date
!!        ================================
!!
!!           Bodies and models
!!           -----------------
!!           Earth:  1976 IAU precession model
!!                   1980 IAU nutation model
!!
!!        Euler frames
!!        ============
!!
!!           Euler angle definitions
!!           -----------------------
!!           Polynomial
!!
!!        Product frames
!!        ==============
!!
!!           Product definitions
!!           -------------------
!!           Array of "from" frames
!!           Array of "to" frames
!!
!! ## Examples
!!
!!     See ROTGET.
!!
!! ## Restrictions
!!
!!     1) This is a SPICE private routine; the routine is subject
!!        to change without notice.  User applications should not
!!        call this routine.
!!
!!     2) Many numerical problems can occur when dynamic frames
!!        are evaluated.  Users must determine whether dynamic frame
!!        definitions are suitable for their applications. See the
!!        Exceptions section for a list of possible problems.
!!
!!     3) Two-vector frame definitions can suffer extreme loss of
!!         precision due to near-singular geometry.
!!
!! ## Literature References
!!
!!     None.
!!
!! ## Author and Institution
!!
!!     N.J. Bachman    (JPL)
!!
!! ## Version
!!
!!-    SPICELIB Version 2.0.0, 09-OCT-2021 (NJB) (EDW)
!!
!!        Support for product frames was added.
!!
!!        Bug fix: aberration correction strings for observer-target
!!        velocity vectors are now parsed using ZZVALCOR. This improves
!!        error checking. Note that aberration correction strings for
!!        constant vectors used in two-vector frame specifications are
!!        still parsed using ZZPRSCOR.
!!
!!        Code clean-up: unnecessary call to ZZPRSCOR was removed.
!!
!!        Added and removed some FAILED() calls.
!!
!!        Body radii accessed from kernel pool using ZZGFTREB.
!!
!!        Re-ordered header sections.
!!
!!-    SPICELIB Version 1.1.0, 24-OCT-2005 (NJB)
!!
!!        Parameters KWX, KWY, KWZ were renamed to KVX, KVY, KVZ.
!!
!!        Call to ZZBODVCD was replaced with call to BODVCD.
!!
!!-    SPICELIB Version 1.0.0, 10-JAN-2005 (NJB)
!!
!!     SPICELIB functions
!!
!!     Local parameters
!!
!!     Local variables
!!
!!     Saved variables
!!
!!     Initial values
!!
      use pepper_kinds, only: ip, wp
      use pepper_arrays, only: cleard, isrchc, moved
      use pepper_math, only: latrec, mxm, mxv, polyds, vequ, vhat, vminus, vsep, vsub, xpose
      use pepper_strings, only: cmprss, eqstr, intstr, ucase
      use pepper_inc_frmtyp
      use pepper_inc_zzabcorr
      use pepper_inc_zzdyn
      implicit none
      real(wp) :: epoch_1(1)  !! one-element copy of epoch (array argument)
      real(wp) :: minsep_1(1)  !! one-element copy of minsep (array argument)
      real(wp) :: dec_1(1)  !! one-element copy of dec (array argument)
      real(wp) :: ra_1(1)  !! one-element copy of ra (array argument)
      real(wp) :: lat_1(1)  !! one-element copy of lat (array argument)
      real(wp) :: lon_1(1)  !! one-element copy of lon (array argument)
      real(wp) :: t0_1(1)  !! one-element copy of t0 (array argument)
      character(*), parameter :: rname = 'ZZDYNRT0'
      integer(ip), parameter :: timlen = 50
      integer(ip), parameter :: vnamln = 4
      character(corlen)     abcorr
      character(kvlen)      axname
      character(frnmln)     basnam
      character(frnmln)     cfrmnm
      character(bdnmln)     ctrnam
      character(corlen)     cvcorr
      character(kvlen)      dynstl
      character(kvlen)      dynfam
      character(frnmln)     fframs ( mxnfac )
      character(frnmln)     inname
      character(kvlen)      nutmod
      character(kvlen)      oblmod
      character(kvlen)      prcmod
      character(kvlen)      rotsta
      character(kvlen)      spec
      character(frnmln)     tframs ( mxnfac )
      character(timlen)     timstr
      character(kvlen)      tmpfam
      character(kvlen)      units
      character(kvlen)      vecdef ( 2 )
      character(frnmln)     velfrm
      character(vnamln), parameter :: vname  ( 2 ) = &
         [ character(vnamln) :: kwpri, kwsec ]
      real(wp)              alt
      real(wp)              angles ( 2 )
      real(wp)              coeffs ( maxcof, 3 )
      real(wp)              ctrpos ( 3 )
      real(wp)              dec
      real(wp)              delta
      real(wp)              dirvec ( 3 )
      real(wp)              dmob
      real(wp)              epoch
      real(wp)              eulang ( 3 )
      real(wp)              fet
      real(wp)              lat
      real(wp)              lon
      real(wp)              lt
      real(wp)              minsep
      real(wp)              mob
      real(wp)              nutxf  ( 6, 6 )
      real(wp)              oblr   ( 3, 3 )
      real(wp)              pnear  ( 3 )
      real(wp)              pobs   ( 3 )
      real(wp)              poly   ( 0 : 1 )
      real(wp)              precxf ( 6, 6 )
      real(wp)              ptemp  ( 3 )
      real(wp)              r2000  ( 3, 3 )
      real(wp)              ra
      real(wp)              radii  ( 3 )
      real(wp)              rinv   ( 3, 3 )
      real(wp)              rnut   ( 3, 3 )
      real(wp)              rout   ( 3, 3 )
      real(wp)              rprec  ( 3, 3 )
      real(wp)              rtemp  ( 3, 3 )
      real(wp)              sep
      real(wp)              stemp  ( 6 )
      real(wp)              stobs  ( 6 )
      real(wp)              tipm   ( 3, 3 )
      real(wp)              t0
      real(wp)              v2     ( 3, 2 )
      real(wp)              vet
      real(wp)              vflt
      integer(ip)           axis   ( 2 )
      integer(ip)           cfrmid
      integer(ip)           cvobs
      integer(ip)           degs   ( 3 )
      integer(ip)           frcls
      integer(ip)           frctr
      integer(ip)           frcid
      integer(ip)           frid
      integer(ip)           fromid
      integer(ip)           i
      integer(ip)           iaxes  ( 3 )
      integer(ip)           m
      integer(ip)           n
      integer(ip)           obs
      integer(ip)           targ
      integer(ip)           toid
      logical               corblk ( nabcor )
      logical               fnd
      logical               frozen
      logical               meanec
      logical               meaneq
      logical               negate
      logical               ofdate
      logical               trueeq
      if ( .not. allocated ( ctx%zzdynrt0_state ) ) then
         allocate ( ctx%zzdynrt0_state )
         call init_zzdynrt0_state ( ctx%zzdynrt0_state )
      end if
      associate ( axes => ctx%zzdynrt0_state%axes, &
                  earth => ctx%zzdynrt0_state%earth, &
                  first => ctx%zzdynrt0_state%first, &
                  j2000 => ctx%zzdynrt0_state%j2000, &
                  itmabc => ctx%zzdynrt0_state%itmabc, &
                  itmaxe => ctx%zzdynrt0_state%itmaxe, &
                  itmcof => ctx%zzdynrt0_state%itmcof, &
                  itmdec => ctx%zzdynrt0_state%itmdec, &
                  itmfrm => ctx%zzdynrt0_state%itmfrm, &
                  itmlat => ctx%zzdynrt0_state%itmlat, &
                  itmlon => ctx%zzdynrt0_state%itmlon, &
                  itmobs => ctx%zzdynrt0_state%itmobs, &
                  itmra => ctx%zzdynrt0_state%itmra, &
                  itmsep => ctx%zzdynrt0_state%itmsep, &
                  itmspc => ctx%zzdynrt0_state%itmspc, &
                  itmtrg => ctx%zzdynrt0_state%itmtrg, &
                  itmunt => ctx%zzdynrt0_state%itmunt, &
                  itmvdf => ctx%zzdynrt0_state%itmvdf, &
                  itmvec => ctx%zzdynrt0_state%itmvec )
      if ( ctx%failed() ) return

      if ( first ) then
!
!        Get the ID code for the J2000 frame.
!
         call irfnum ( ctx, 'J2000', j2000 )

!
!        Get the ID code for the earth (we needn't check the found
!        flag).
!
         call bodn2c ( ctx, 'EARTH', earth, fnd )

!
!        Initialize "item" strings used to create kernel variable
!        names.
!
         do i = 1, 2
!
!           Vector axis:
!
            itmaxe(i) = vname(i) // kwvaxi
!
!           Vector definition:
!
            itmvdf(i) = vname(i) // kwvcdf
!
!           Vector aberration correction:
!
            itmabc(i) = vname(i) // kwvabc
!
!           Vector frame:
!
            itmfrm(i) = vname(i) // kwvfrm
!
!           Vector observer:
!
            itmobs(i) = vname(i) // kwvobs
!
!           Vector target:
!
            itmtrg(i) = vname(i) // kwvtrg
!
!           Vector longitude:
!
            itmlon(i) = vname(i) // kwlon
!
!           Vector latitude:
!
            itmlat(i) = vname(i) // kwlat
!
!           Vector right ascension:
!
            itmra(i)  = vname(i) // kwra
!
!           Vector declination:
!
            itmdec(i) = vname(i) // kwdec
!
!           Vector units:
!
            itmunt(i) = vname(i) // kwunit
!
!           Constant vector coordinate specification:
!
            itmspc(i) = vname(i) // kwvspc
!
!           Constant vector in Cartesian coordinates, literal value:
!
            itmvec(i) = vname(i) // kwvect

         end do

         first = .false.

      end if

!
!     Initialize the output arguments.
!
      call cleard ( 9, rotate )

      basfrm = 0

!
!     Initialize certain variables to ensure that we don't do
!     arithmetic operations using bogus, possibly large,
!     undefined values.
!
      call cleard ( 36, nutxf  )
      call cleard (  9, oblr   )
      call cleard ( 36, precxf )
      call cleard (  9, r2000  )
      call cleard (  9, rtemp  )
      call cleard (  9, rinv   )
      call cleard (  9, tipm   )

      mob    = 0.0_wp
      dmob   = 0.0_wp
      t0     = 0.0_wp
      frozen = .false.

!
!     Get the input frame name.
!
      call frmnam ( ctx, infram, inname )

!
!     We need the name of the base frame.
!
      call zzdynfid ( ctx, inname, infram, kwbfrm, basfrm )

      if ( ctx%failed() ) return

      call frmnam   ( ctx, basfrm, basnam )

!
!     The output frame code and name are set.
!
!     Look up the dynamic frame definition style from the kernel pool.
!     The kernel variable's name might be specified by name or ID.
!
      call zzdynvac ( ctx, inname, infram, kwstyl, 1, n, dynstl )

      if ( ctx%failed() ) return

!
!     At this time, the only supported dynamic frame definition style is
!     PARAMETERIZED.
!
      if (  eqstr( dynstl, kvparm )  ) then
!
!        Parameterized dynamic frames belong to families.  Look up
!        the family for this frame.
!
         call zzdynvac ( ctx, inname, infram, kwffam, 1, n, dynfam )

         if ( ctx%failed() ) return

         call cmprss ( ' ', 0, dynfam, tmpfam )
         call ucase  ( tmpfam,         dynfam )

!
!        Determine whether we have an "of-date" frame family.
!        The logical flags used here and respective meanings are:
!
!           MEANEQ   Mean equator and equinox of date
!           TRUEEQ   True equator and equinox of date
!           MEANEC   Mean ecliptic and equinox of date
!
         meaneq  =  dynfam == kvmeqt
         trueeq  =  dynfam == kvteqt
         meanec  =  dynfam == kvmecl

         ofdate  =  meaneq .or. meanec .or. trueeq

!
!        Set the evaluation epoch T0.  Normally this epoch is ET,
!        but if the frame is frozen, the freeze epoch from the
!        frame definition is used.
!
!        Read the freeze epoch into T0 if a freeze epoch was
!        specified; let FROZEN receive the FOUND flag value
!        returned by ZZDYNOAD.
!
         call zzdynoad ( ctx, inname, infram, kwfrez, 1, n, t0_1, frozen )
         t0 = t0_1(1)

         if ( ctx%failed() ) return

         if ( .not. frozen ) then
!
!           Normal case:  just use the input epoch.
!
            t0 = et

         end if

!
!        Look up the rotation state keyword.  In this routine,
!        the rotation state keyword is examined only to support
!        semantic checking:  there's no use made of the fact that
!        the rotation state is 'ROTATING' or 'INERTIAL'.
!
         call zzdynoac ( ctx, inname, infram, kwrsta, 1, n, rotsta, fnd )

         if ( ctx%failed() ) return

         if ( fnd ) then
!
!           Catch invalid rotation states here.
!
            if (      ( .not. eqstr( rotsta, kvrotg ) ) &
                .and. ( .not. eqstr( rotsta, kvinrt ) )  ) then

               call ctx%setmsg ( 'Definition of frame # contains '     // &
                             '# specification #. The only valid '  // &
                             'rotation states are # or #. '        // &
                             'This situation is usually caused by '// &
                             'an error in a frame kernel in which '// &
                             'the frame is defined.'               )
               call ctx%errch  ( '#',  inname                          )
               call ctx%errch  ( '#',  kwrsta                          )
               call ctx%errch  ( '#',  rotsta                          )
               call ctx%errch  ( '#',  kvrotg                          )
               call ctx%errch  ( '#',  kvinrt                          )
               call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'                 )
               return

            end if

         end if
!
!        If the frame is frozen, the rotation state keyword *must be
!        absent*.
!
         if ( frozen .and. fnd ) then

            call ctx%setmsg ( 'Definition of frame # contains both ' // &
                          '# and # '                             // &
                          'keywords; at most one of these must ' // &
                          'be present in the frame definition. ' // &
                          'This situation is usually caused by ' // &
                          'an error in a frame kernel in which ' // &
                          'the frame is defined.'                )
            call ctx%errch  ( '#',  inname                           )
            call ctx%errch  ( '#',  kwfrez                           )
            call ctx%errch  ( '#',  kwrsta                           )
            call ctx%sigerr ( 'SPICE(FRAMEDEFERROR)'                 )
            return

         end if

!
!        If the frame belongs to an "of date" family, either the
!        rotation state must be specified or the frame must be
!        frozen.
!
         if (  ofdate  .and.  (.not. frozen) .and. (.not. fnd )  ) then

            call ctx%setmsg ( 'Definition of frame #, which belongs to ' // &
                          'parameterized dynamic frame family #, '   // &
                          'contains neither # nor # keywords; '      // &
                          'frames in this family require exactly '   // &
                          'one of these in their frame definitions. '// &
                          'This situation is usually caused by '     // &
                          'an error in a frame kernel in which '     // &
                          'the frame is defined.'                    )
            call ctx%errch  ( '#',  inname                               )
            call ctx%errch  ( '#',  dynfam                               )
            call ctx%errch  ( '#',  kwfrez                               )
            call ctx%errch  ( '#',  kwrsta                               )
            call ctx%sigerr ( 'SPICE(FRAMEDEFERROR)'                     )
            return

         end if

         if ( ctx%failed() ) return

!
!        The evaluation epoch T0 is set.
!
!        In this routine, unlike its companion ZZDYNFRM, there is no
!        need to make further reference to the rotation state.  Hence
!        the flag INERT used in ZZDYNFRM doesn't appear here.
!
!        The following code block performs actions specific to
!        the various dynamic frame families.
!
         if ( ofdate ) then
!
!           Fetch the name of the true equator and equinox of date
!           precession model.
!
            call zzdynvac ( ctx, inname, infram, kwprcm, 1, n, prcmod )

            if ( ctx%failed() ) return

!
!           Get the precession transformation.
!
            if (  eqstr( prcmod, kvm001 )  ) then
!
!              This is the 1976 IAU earth precession model.
!
!              Make sure the center of the input frame is the earth.
!
               if ( center /= earth ) then

                  call bodc2n ( ctx, center, ctrnam, fnd )

                  if ( .not. fnd ) then
                     call intstr ( center, ctrnam )
                  end if

                  call ctx%setmsg ( 'Definition of frame # specifies '    // &
                                'frame center # and precession '      // &
                                'model #. This precession model '     // &
                                'is not applicable to body #. '       // &
                                'This situation is usually caused by '// &
                                'an error in a frame kernel in which '// &
                                'the frame is defined.'               )
                  call ctx%errch  ( '#',  inname                          )
                  call ctx%errch  ( '#',  ctrnam                          )
                  call ctx%errch  ( '#',  kvm001                          )
                  call ctx%errch  ( '#',  ctrnam                          )
                  call ctx%sigerr ( 'SPICE(INVALIDSELECTION)'             )
                  return

               end if

!
!              Look up the precession transformation.  Extract
!              the precession rotation matrix.
!
               call zzeprc76 ( ctx, t0, precxf )

               call moved ( precxf(1,1), 3, rprec(1,1) )
               call moved ( precxf(1,2), 3, rprec(1,2) )
               call moved ( precxf(1,3), 3, rprec(1,3) )

!
!              If we're in the mean-of-date case, invert this
!              transformation to obtain the mapping from the
!              mean-of-date frame to J2000.
!
               if ( meaneq ) then
                  call xpose ( rprec, rtemp )
               end if

            else

               call ctx%setmsg ( 'Definition of frame # specifies '    // &
                             'precession model #, which is not '   // &
                             'recognized. '                        // &
                             'This situation is usually caused by '// &
                             'an error in a frame kernel in which '// &
                             'the frame is defined.'               )
               call ctx%errch  ( '#',  inname                          )
               call ctx%errch  ( '#',  prcmod                          )
               call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'                 )
               return

            end if

!
!           At this point the precession transformation REPREC is set.
!           If INFRAM is a mean equator and equinox of date frame, the
!           inverse of REPREC is currently stored in RTEMP.

            if ( trueeq ) then
!
!              We need a nutation transformation as well. Get the name
!              of the nutation model.
!
               call zzdynvac ( ctx, inname, infram, kwnutm, 1, n, nutmod )

               if ( ctx%failed() ) return

!
!              Get the nutation transformation.
!
               if (  eqstr( nutmod, kvm002 )  ) then
!
!                 This is the 1980 IAU earth nutation model.
!
!                 Make sure the center is the earth.
!
                  if ( center /= earth ) then

                     call bodc2n ( ctx, center, ctrnam, fnd )

                     if ( .not. fnd ) then
                        call intstr ( center, ctrnam )
                     end if

                     call ctx%setmsg ( 'Definition of frame # specifies ' // &
                                   'frame center # and nutation '     // &
                                   'model #. This '                   // &
                                   'nutation model is not applicable '// &
                                   'to body #.  This situation is '   // &
                                   'usually caused by an error in a ' // &
                                   'frame kernel in which '           // &
                                   'the frame is defined.'            )
                     call ctx%errch  ( '#',  inname                       )
                     call ctx%errch  ( '#',  ctrnam                       )
                     call ctx%errch  ( '#',  kvm002                       )
                     call ctx%errch  ( '#',  ctrnam                       )
                     call ctx%sigerr ( 'SPICE(INVALIDSELECTION)'          )
                     return

                  end if

!
!                 Look up the nutation transformation.  Extract
!                 the nutation rotation matrix.
!
                  call zzenut80 ( ctx, t0, nutxf )

                  call moved ( nutxf(1,1), 3, rnut(1,1) )
                  call moved ( nutxf(1,2), 3, rnut(1,2) )
                  call moved ( nutxf(1,3), 3, rnut(1,3) )

!
!                 Find the rotation from the J2000 frame to the earth
!                 true of date frame.  Invert.
!
                  call mxm   ( rnut, rprec, rinv )
                  call xpose ( rinv, rtemp )

               else

                  call ctx%setmsg ( 'Definition of frame # specifies '    // &
                                'nutation model #, which is not '     // &
                                'recognized. '                        // &
                                'This situation is usually caused by '// &
                                'an error in a frame kernel in which '// &
                                'the frame is defined.'               )
                  call ctx%errch  ( '#',  inname                          )
                  call ctx%errch  ( '#',  nutmod                          )
                  call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'                 )
                  return

               end if

            else if ( meanec ) then
!
!              We need a mean obliquity transformation as well.
!              Get the name of the obliquity model.
!
               call zzdynvac ( ctx, inname, infram, kwobqm, 1, n, oblmod )

               if ( ctx%failed() ) return

!
!              Get the obliquity transformation.
!
               if (  eqstr( oblmod, kvm003 )  ) then
!
!                 This is the 1980 IAU earth mean obliquity of
!                 date model.
!
!                 Make sure the center is the earth.
!
                  if ( center /= earth ) then

                     call bodc2n ( ctx, center, ctrnam, fnd )

                     if ( .not. fnd ) then
                        call intstr ( center, ctrnam )
                     end if

                     call ctx%setmsg ( 'Definition of frame # specifies ' // &
                                   'frame center # and obliquity '    // &
                                   'model #.  This obliquity model '  // &
                                   'is not applicable to body #. '    // &
                                   'This situation is '               // &
                                   'usually caused by an error in a ' // &
                                   'frame kernel in which '           // &
                                   'the frame is defined.'            )
                     call ctx%errch  ( '#',  inname                       )
                     call ctx%errch  ( '#',  ctrnam                       )
                     call ctx%errch  ( '#',  kvm003                       )
                     call ctx%errch  ( '#',  ctrnam                       )
                     call ctx%sigerr ( 'SPICE(INVALIDSELECTION)'          )
                     return

                  end if

!
!                 Create the obliquity transformation.
!                 First look up the obliquity state.
!
                  call zzmobliq ( ctx, t0, mob, dmob )

!
!                 The obliquity rotation is about the mean-of-date
!                 x-axis.  The other Euler angles are identically
!                 zero; the axes are arbitrary, as long as the
!                 middle axis is distinct from the other two.
!
                  call eul2m ( ctx, 0.0_wp, 0.0_wp, mob, 1, 3, 1, oblr )

!
!                 Find the rotation from the J2000 to the
!                 earth mean ecliptic of date frame.  Invert.
!
                  call mxm   ( oblr, rprec, rinv )
                  call xpose ( rinv, rtemp )

               else

                  call ctx%setmsg ( 'Definition of frame # specifies '    // &
                                'obliquity model #, which is not '    // &
                                'recognized. '                        // &
                                'This situation is usually caused by '// &
                                'an error in a frame kernel in which '// &
                                'the frame is defined.'               )
                  call ctx%errch  ( '#',  inname                          )
                  call ctx%errch  ( '#',  oblmod                          )
                  call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'                 )
                  return

               end if

            end if

!
!           At this point, RTEMP contains the rotation from the
!           specified mean of date or true of date frame to J2000.
!
!           If the base frame is not J2000, we must find the
!           transformation from J2000 to the base frame.

            if ( basfrm /= j2000 ) then

               call zzrefch1 ( ctx, j2000,  basfrm, t0,  r2000  )
               call mxm      ( r2000,  rtemp,       rotate )

            else
!
!              Otherwise, RTEMP is the matrix we want.
!
               call moved ( rtemp, 9, rotate )

            end if
!
!           Now ROTATE is the state transformation mapping from
!           the input frame INFRAM to the base frame BASFRM.
!
!           This is the end of the work specific to "of-date" frames.
!           From here we drop out of the IF block.
!

         else if ( dynfam == kv2vec ) then
!
!           The frame belongs to the TWO-VECTOR family.
!
!           Fetch the specifications of the primary and secondary
!           axes.
!
            call cleard ( 6, v2 )

            do i = 1, 2
!
!              Get the name of the axis associated with the Ith
!              defining vector.
!
               call zzdynvac ( ctx, inname, infram, itmaxe(i), 1, n, axname )

               if ( ctx%failed() ) return

               call cmprss ( ' ', 0, axname, axname )
               call ucase  ( axname,         axname )

!
!              Set the sign flag associated with the axis.
!
               negate  =  axname(1:1) == '-'

               call cmprss ( '-', 0, axname, axname )
               call cmprss ( '+', 0, axname, axname )

               axis(i) = isrchc ( axname, 3, axes )

               if ( axis(i) == 0 ) then

                  call ctx%setmsg ( 'Definition of frame # associates '   // &
                                'vector # with axis #.  The only '    // &
                                'valid axis values are { X, -X, Y, '  // &
                                '-Y, Z, -Z }. '                       // &
                                'This situation is usually caused by '// &
                                'an error in a frame kernel in which '// &
                                'the frame is defined.'               )
                  call ctx%errch  ( '#',  inname                          )
                  call ctx%errint ( '#',  i                               )
                  call ctx%errch  ( '#',  axname                          )
                  call ctx%sigerr ( 'SPICE(INVALIDAXIS)'                  )
                  return

               end if

!
!              Find out how the vector is defined:
!
!                 - Observer-target position vector
!                 - Observer-target velocity vector
!                 - Observer-target near point vector
!                 - Constant vector
!
!              VECDEF(I) indicates the vector definition method
!              for the Ith vector.
!
               call zzdynvac ( ctx, inname,  infram,  itmvdf(i), &
                               1,       n,       vecdef(i)   )

               if ( ctx%failed() ) return

               call cmprss ( ' ',    0, vecdef(i), vecdef(i) )
               call ucase  ( vecdef(i),            vecdef(i) )

               if (  vecdef(i) == kvposv  ) then
!
!                 The vector is the position of a target relative
!                 to an observer.
!
!                 We need a target, observer, and aberration correction.
!
                  call zzdynbid ( ctx, inname, infram, itmtrg(i), targ )

                  call zzdynbid ( ctx, inname, infram, itmobs(i), obs  )

                  call zzdynvac ( ctx, inname, infram, itmabc(i), &
                                  1,      n,      abcorr     )

!
!                 Look up the Ith position vector in the J2000 frame.
!
                  call zzspkzp1 ( ctx, targ,    t0,   'J2000', &
                                  abcorr,  obs,  v2(1,i),  lt )

                  if ( ctx%failed() ) return

!
!                 At this point, V2(*,I) contains position relative to
!                 frame J2000.
!

               else if ( vecdef(i) == kvvelv ) then
!
!                 The vector is the velocity of a target relative
!                 to an observer.
!
!                 We need a target, observer, and aberration correction.
!
                  call zzdynbid ( ctx, inname, infram, itmtrg(i), targ )

                  call zzdynbid ( ctx, inname, infram, itmobs(i), obs  )

                  call zzdynvac ( ctx, inname, infram, itmabc(i), &
                                  1,      n,      abcorr     )

!
!                 We need to know the frame in which the velocity is
!                 defined.
!
                  call zzdynfid ( ctx, inname, infram, itmfrm(i), frid )
                  call frmnam   ( ctx, frid,   velfrm  )

!
!                 Look up the Ith velocity vector in the velocity frame.
!
                  call zzspkez0 ( ctx, targ,    t0,   velfrm, &
                                  abcorr,  obs,  stemp,  lt )

                  if ( ctx%failed() ) return

!
!                 We'll work with the unit velocity vector.
!
                  call vhat ( stemp(4), v2(1,i) )

!
!                 We need the epoch VET at which VELFRM is evaluated.
!                 This epoch will be used to transform the velocity
!                 vector from VELFRM to J2000.
!
!                 Set the default value of VET here.
!
                  vet = t0

!
!                 Parse the aberration correction.  Capture the
!                 epoch used to evaluate the velocity vector's frame.
!
                  call zzvalcor ( ctx, abcorr, corblk )

                  if ( ctx%failed() ) return

                  if ( corblk(ltidx) ) then
!
!                    Light time correction is used.  The epoch used
!                    to evaluate the velocity vector's frame depends
!                    on the frame's observer and center.
!
!                    Look up the velocity frame's center.
!
                     call frinfo ( ctx, frid, frctr, frcls, frcid, fnd )

                     if ( .not. fnd ) then

                        call ctx%setmsg ( 'In definition of frame #, ' // &
                                      'the frame associated with ' // &
                                      'a velocity vector has '     // &
                                      'frame ID code #, but no '   // &
                                      'frame center, frame class, '// &
                                      'or frame class ID was '     // &
                                      'found by FRINFO.  This '    // &
                                      'situation MAY be caused '   // &
                                      'by an error in a frame '    // &
                                      'kernel in which the frame ' // &
                                      'is defined. The problem '   // &
                                      'also could be indicative '  // &
                                      'of a SPICELIB bug.'         )
                        call ctx%errch  ( '#',  inname                 )
                        call ctx%errint ( '#',  frid                   )
                        call ctx%sigerr ( 'SPICE(FRAMEDATANOTFOUND)'   )
                        return

                     end if

                     if ( frcls /= inertl ) then
!
!                       Obtain light time from the observer to the
!                       frame's center.
!
                        call zzspkzp1 ( ctx, frctr,  t0,  'J2000', &
                                        abcorr, obs, ctrpos,  vflt )
                        call zzcorepc ( ctx, abcorr, t0,  vflt,    vet  )

                        if ( ctx%failed() ) return

                     end if

                  end if

!
!                 The velocity frame evaluation epoch VET is now set.
!
!                 We must rotate the velocity vector from the velocity
!                 frame (evaluated at VET) to the output frame at T0.
!                 We'll do this in two stages, first mapping velocity
!                 into the J2000 frame.
!
                  if ( frid /= j2000 ) then

                     call zzrefch1 ( ctx, frid,   j2000,  vet,      r2000 )

                     if ( ctx%failed() ) return

                     call mxv      ( r2000,          v2(1,i),  ptemp )
                     call moved    ( ptemp,  3,      v2(1,i)         )

                  end if

!
!                 At this point, V2(*,I) contains velocity
!                 relative to frame J2000.

               else if ( vecdef(i) == kvnear ) then
!
!                 The vector points from an observer to the near
!                 point to the observer on the target body.
!
!                 We need a target, observer, and aberration correction.
!
                  call zzdynbid ( ctx, inname, infram, itmtrg(i), targ )

                  call zzdynbid ( ctx, inname, infram, itmobs(i), obs  )

                  call zzdynvac ( ctx, inname, infram, itmabc(i), &
                                  1,      n,      abcorr     )

!
!                 The vector points from an observer to the
!                 sub-observer point (nearest point to the observer) on
!                 the target body.  We need the position of the near
!                 point relative to the observer.
!
!                 We'll look up the position of the target center
!                 relative to the observer, as well as the position of
!                 the near point relative to the target center, both in
!                 the body-fixed frame associated with the target.
!
!                 Look up the body-fixed frame associated with the
!                 target body.
!
                  call cidfrm ( ctx, targ, cfrmid, cfrmnm, fnd )

                  if ( .not. fnd ) then

                     call ctx%setmsg ( 'Definition of frame # requires ' // &
                                   'definition of body-fixed frame ' // &
                                   'associated with target body #. ' // &
                                   'A call to CIDFRM indicated no '  // &
                                   'body-fixed frame is associated ' // &
                                   'with the target body.  This '    // &
                                   'situation can arise when a '     // &
                                   'frame kernel defining the '      // &
                                   'target''s body-fixed frame  '    // &
                                   'lacks the OBJECT_<ID>_FRAME '    // &
                                   'or OBJECT_<name>_FRAME '         // &
                                   'keywords.  The problem also '    // &
                                   'could be caused by an '          // &
                                   'error in a frame kernel in '     // &
                                   'which the parameterized '        // &
                                   'two-vector dynamic frame # '     // &
                                   'is defined.'                     )
                     call ctx%errch  ( '#',  inname                      )
                     call ctx%errint ( '#',  targ                        )
                     call ctx%errch  ( '#',  inname                      )
                     call ctx%sigerr ( 'SPICE(FRAMEDATANOTFOUND)'        )
                     return

                  end if

!
!                 Get the radii of the target body.
!
                  call zzgftreb ( ctx, targ, radii )

                  if ( ctx%failed() ) return

!
!                 Look up the Ith position vector in the target-fixed
!                 frame.  Negate the vector to obtain the target-to-
!                 observer vector.
!
                  call zzspkzp1 ( ctx, targ,   t0,   cfrmnm, &
                                  abcorr, obs,  ptemp,   lt )

!
!                 We check FAILED() here because VMINUS is a simple
!                 arithmetic routine that doesn't return on entry.
!
                  if ( ctx%failed() ) return

                  call vminus   ( ptemp,  pobs )

                  call nearpt   ( ctx, pobs,   radii(1), radii(2), radii(3), &
                                  pnear,  alt                          )

                  if ( ctx%failed() ) return

!
!                 Find the observer-near point vector in the current
!                 frame CFRMNM.
!
                  call vsub ( pnear, pobs, ptemp )

!
!                 Rotate the vector to frame J2000.  To get the required
!                 rotation matrix, we'll need to obtain the epoch
!                 associated with CNMFRM.  Parse the aberration
!                 correction and adjust the frame evaluation epoch as
!                 needed.
!
                  call zzcorepc ( ctx, abcorr, t0, lt, fet )

!
!                 Obtain the matrix for transforming position vectors
!                 from the target center frame to the J2000 frame and
!                 apply it to the observer-to-near point position
!                 vector.
!
                  call zzrefch1 ( ctx, cfrmid, j2000, fet, tipm )

                  if ( ctx%failed() ) return

                  call mxv ( tipm, ptemp, v2(1,i) )

!
!                 At this point, V2(*,I) contains position of the near
!                 point on the target as seen by the observer, relative
!                 to frame J2000.
!

               else if ( vecdef(i) == kvcons ) then
!
!                 The vector is constant in a specified frame.
!
!                 We need a 3-vector and an associated reference
!                 frame relative to which the vector is specified.
!
!                 Look up the ID of the frame first.
!
                  call zzdynfid ( ctx, inname, infram, itmfrm(i), frid )

!
!                 Let FET ("frame ET") be the evaluation epoch for
!                 the constant vector's frame.  By default, this
!                 frame is just T0, but if we're using light time
!                 corrections, FET must be adjusted for one-way
!                 light time between the frame's center and the
!                 observer.
!
!                 Set the default value of FET here.
!
                  fet = t0

!
!                 Optionally, there is an aberration correction
!                 associated with the constant vector's frame.
!                 If so, an observer must be associated with the
!                 frame.  Look up the correction first.
!
                  call zzdynoac ( ctx, inname, infram, itmabc(i), &
                                  1,      n,      cvcorr,    fnd )

                  if ( ctx%failed() ) return

                  if ( .not. fnd ) then
                     cvcorr = 'NONE'
                  end if

                  call zzprscor ( ctx, cvcorr, corblk )

                  if ( ctx%failed() ) return

                  if ( .not. corblk(geoidx) ) then
!
!                    We need to apply an aberration correction to
!                    the constant vector.

!                    Check for errors in the aberration correction
!                    specification.
!
!                       - Light time and stellar aberration corrections
!                         are mutually exclusive.
!
                     if ( corblk(ltidx) .and. corblk(stlidx) ) then

                        call ctx%setmsg ( 'Definition of frame # '       // &
                                      'specifies aberration '        // &
                                      'correction # for constant '   // &
                                      'vector.  Light time and '     // &
                                      'stellar aberration '          // &
                                      'corrections are mutually '    // &
                                      'exclusive for constant '      // &
                                      'vectors used in two-vector '  // &
                                      'parameterized dynamic frame ' // &
                                      'definitions.  This situation '// &
                                      'is usually caused by an '     // &
                                      'error in a frame kernel in '  // &
                                      'which the frame is defined.'  )
                        call ctx%errch  ( '#',  inname                   )
                        call ctx%errch  ( '#',  cvcorr                   )
                        call ctx%sigerr ( 'SPICE(INVALIDOPTION)'         )
                        return

                     end if

                     if ( corblk(ltidx) ) then
!
!                       Light time correction is used.  The epoch used
!                       to evaluate the constant vector's frame depends
!                       on the frame's observer and center.
!
!                       Look up the constant vector frame's center.
!
                        call frinfo ( ctx, frid, frctr, frcls, frcid, fnd )

                        if ( .not. fnd ) then

                           call ctx%setmsg ( 'In definition of frame #, ' // &
                                         'the frame associated with ' // &
                                         'a constant vector has '     // &
                                         'frame ID code #, but no '   // &
                                         'frame center, frame class, '// &
                                         'or frame class ID was '     // &
                                         'found by FRINFO.  This '    // &
                                         'situation MAY be caused '   // &
                                         'by an error in a frame '    // &
                                         'kernel in which the frame ' // &
                                         'is defined. The problem '   // &
                                         'also could be indicative '  // &
                                         'of a SPICELIB bug.'         )
                           call ctx%errch  ( '#',  inname                 )
                           call ctx%errint ( '#',  frid                   )
                           call ctx%sigerr ( 'SPICE(FRAMEDATANOTFOUND)'   )
                           return

                        end if

                        if ( frcls /= inertl ) then
!
!                          Look up the observer associated with the
!                          constant vector's frame.  This observer,
!                          together with the frame's center, determines
!                          the evaluation epoch for the frame.
!
                           call zzdynbid ( ctx, inname,    infram, &
                                           itmobs(i), cvobs   )

!
!                          Obtain light time from the observer to the
!                          frame's center.
!
                           call zzspkzp1 ( ctx, frctr,  t0,   'J2000', &
                                           cvcorr, cvobs, ctrpos,  lt )

!
!                          Find the evaluation epoch for the frame.
!
                           call zzcorepc ( ctx, cvcorr, t0, lt, fet )

                           if ( ctx%failed() ) return

                        end if

                     else if ( corblk(stlidx) ) then
!
!                       Stellar aberration case.
!
!                       The constant vector must be corrected for
!                       stellar aberration induced by the observer's
!                       velocity relative to the solar system
!                       barycenter.  First, find this velocity in
!                       the J2000 frame.  We'll apply the correction
!                       later, when the constant vector has been
!                       transformed to the J2000 frame.
!
                        call zzdynbid ( ctx, inname,    infram, &
                                        itmobs(i), cvobs   )

                        call zzspksb1 ( ctx, cvobs, t0, 'J2000', stobs )

                        if ( ctx%failed() ) return

                     end if

                  end if

!
!                 Get the constant vector specification.
!
                  call zzdynvac ( ctx, inname,  infram,  itmspc(i), &
                                  1,       n,       spec       )

                  if ( ctx%failed() ) return

                  call cmprss ( ' ', 0, spec, spec )
                  call ucase  ( spec,         spec )

                  if ( spec == kvrecc ) then
!
!                    The coordinate system is rectangular.
!
!                    Look up the constant vector.
!
                     call zzdynvad ( ctx, inname, infram, itmvec(i), &
                                     3,      n,      dirvec    )

                  else if (      ( spec == kvlatc ) &
                            .or. ( spec == kvradc ) ) then
!
!                    The coordinate system is latitudinal or RA/DEC.
!
!                    Look up the units associated with the angles.
!
                     call zzdynvac ( ctx, inname,  infram,  itmunt(i), &
                                     1,       n,       units     )

                     if ( spec == kvlatc ) then
!
!                       Look up longitude and latitude.
!
                        call zzdynvad ( ctx, inname, infram, itmlon(i), 1, n, lon_1 )
                        lon = lon_1(1)

                        call zzdynvad ( ctx, inname, infram, itmlat(i), 1, n, lat_1 )
                        lat = lat_1(1)

!
!                       Convert angles from input units to radians.
!
                        call convrt ( ctx, lon, units, kvradn, angles(1) )
                        call convrt ( ctx, lat, units, kvradn, angles(2) )

                     else
!
!                       Look up RA and DEC.
!
                        call zzdynvad ( ctx, inname, infram, itmra(i), 1, n, ra_1 )
                        ra = ra_1(1)

                        call zzdynvad ( ctx, inname, infram, itmdec(i), 1, n, dec_1 )
                        dec = dec_1(1)

!
!                       Convert angles from input units to radians.
!
                        call convrt ( ctx, ra,  units, kvradn, angles(1) )
                        call convrt ( ctx, dec, units, kvradn, angles(2) )

                     end if

                     if ( ctx%failed() ) return

!
!                    Now  produce a direction vector.
!
                     call latrec ( 1.0_wp, angles(1), angles(2), dirvec )

                  else

                     call ctx%setmsg ( 'Definition of two-vector '       // &
                                   'parameterized dynamic frame # '  // &
                                   'includes constant vector '       // &
                                   'specification #, which is '      // &
                                   'not supported.  This situation ' // &
                                   'is usually caused by an error '  // &
                                   'in a frame kernel in which '     // &
                                   'the frame is defined.'           )
                     call ctx%errch  ( '#',  inname                      )
                     call ctx%errch  ( '#',  spec                        )
                     call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'             )
                     return

                  end if

!
!                 At this point, the Cartesian coordinates of the
!                 vector relative to the constant vector frame
!                 are stored in DIRVEC.
!
                  if ( frid == j2000 ) then

                     call vequ ( dirvec, v2(1,i) )

                  else
!
!                    Convert the direction vector to the J2000 frame.
!
                     call zzrefch1 ( ctx, frid, j2000, fet, r2000 )

                     if ( ctx%failed() ) return

                     call mxv ( r2000, dirvec, v2(1,i) )

                  end if

!
!                 The constant vector is now represented
!                 in the J2000 frame, but we may still need to
!                 apply a stellar aberration correction.
!
                  if ( corblk(stlidx) ) then
!
!                    Perform the correction appropriate to the
!                    radiation travel sense.
!
                     if ( corblk(xmtidx) ) then
!
!                       The correction is for transmission.
!
                        call stlabx ( ctx, v2(1,i), stobs(4), ptemp )

                     else
!
!                       The correction is for reception.
!
                        call stelab ( ctx, v2(1,i), stobs(4), ptemp )

                     end if

                     if ( ctx%failed() ) return

                     call vequ ( ptemp, v2(1,i) )

                  end if
!
!                 At this point, V2(*,I) contains the constant
!                 (constant relative to its associated frame, that is)
!                 vector as seen by the observer, relative to frame
!                 J2000.
!

               else

                  call ctx%setmsg ( 'Definition of two-vector '       // &
                                'parameterized dynamic frame # '  // &
                                'includes vector '                // &
                                'definition #, which is '         // &
                                'not supported.  This situation ' // &
                                'is usually caused by an error '  // &
                                'in a frame kernel in which '     // &
                                'the frame is defined.'           )
                  call ctx%errch  ( '#',  inname                      )
                  call ctx%errch  ( '#',  vecdef(i)                   )
                  call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'             )
                  return

               end if

!
!              Negate the vector if the axis has negative sign.
!
               if ( negate ) then
                  call vminus ( v2(1,i),     ptemp   )
                  call moved  ( ptemp,   3,  v2(1,i) )
               end if

            end do

!
!           Look up the lower bound for the angular separation of
!           the defining vectors.  Use the default value if none
!           was supplied.
!
            call zzdynoad ( ctx, inname, infram, itmsep, 1, n, minsep_1, fnd )
            minsep = minsep_1(1)

            if ( ctx%failed() ) return

            if ( .not. fnd ) then
               minsep = lbsep
            end if

!
!           Now use our vectors to compute our position transformation
!           matrix.
!
!           Check the angular separation of the defining vectors. We
!           want to ensure that the vectors are not too close to being
!           linearly dependent.  We can handle both cases---separation
!           close to 0 or separation close to Pi---by comparing the
!           sine of the separation to the sine of the separation limit.
!
            sep = vsep ( v2(1,1), v2(1,2) )

            if ( sin(sep) < sin(minsep) ) then

               call etcal  ( ctx, t0, timstr )

               call ctx%setmsg ( 'Angular separation of vectors defining '// &
                             'two-vector parameterized dynamic frame '// &
                             '# is # (radians); minimum allowed diff' // &
                             'erence of separation from 0 or Pi is # '// &
                             'radians.  Evaluation epoch is #.  Extre'// &
                             'me loss of precision can occur when '   // &
                             'defining vectors are nearly linearly '  // &
                             'dependent.  This type of error can be ' // &
                             'due to using a dynamic frame outside of'// &
                             ' the time range for which it is meant. '// &
                             'It also can be due to a conceptual '    // &
                             'error pertaining to the frame''s defin' // &
                             'ition, or to an implementation error '  // &
                             'in the frame kernel containing the '    // &
                             'frame definition. However, if you wish '// &
                             'to proceed with this computation, '     // &
                             'the # keyword can be used in '          // &
                             'the frame definition to adjust the '    // &
                             'separation limit.'                      )
               call ctx%errch  ( '#',  inname                             )
               call ctx%errdp  ( '#',  sep                                )
               call ctx%errdp  ( '#',  minsep                             )
               call ctx%errch  ( '#',  timstr                             )
               call ctx%errch  ( '#',  kwatol                             )
               call ctx%sigerr ( 'SPICE(DEGENERATECASE)'                  )
               return

            end if
!
!           We have both positions expressed relative to frame J2000
!           at this point.  Find the transformation from INNAME to
!           the frame J2000, then from J2000 to frame BASNAM.
!
            call twovec   ( ctx, v2(1,1), axis(1), v2(1,2), axis(2), rinv )

            call xpose    ( rinv,    rotate )

            if ( basfrm /= j2000 ) then

               call moved ( rotate, 9, rtemp )

               call zzrefch1 ( ctx, j2000,   basfrm,  t0,  r2000  )

               if ( ctx%failed() ) return

               call mxm      ( r2000,   rtemp,        rotate )

            end if

!
!           ROTATE is set.
!
!           This is the end of the work specific to two-vector frames.
!           From here we drop out of the IF block.
!

         else if ( dynfam == kveulr ) then
!
!           The frame belongs to the Euler family.
!
!           We expect to specifications of an axis sequence, units,
!           and angles via polynomial coefficients.  We also expect
!           to see an ET epoch.
!
!           Look up the epoch first.  Let DELTA represent the offset
!           of T0 relative to the epoch.
!
!           Initialize EPOCH so subtraction doesn't overflow if EPOCH
!           is invalid due to a lookup error.
!
            epoch = 0.0_wp

            call zzdynvad ( ctx, inname, infram, kwepoc, 1, n, epoch_1 )
            epoch = epoch_1(1)

            delta = t0 - epoch

!
!           Now the axis sequence.
!
            call zzdynvai ( ctx, inname, infram, kweuax, 3, n, iaxes )

!
!           Now the coefficients for the angles.
!
            do i = 1, 3
!
!              Initialize N so subtraction doesn't overflow if N
!              is invalid due to a lookup error.
!
               n = 0

               call zzdynvad ( ctx, inname,  infram,  itmcof(i), &
                               maxcof,  n,       coeffs(1,i) )

!
!              Set the polynomial degree for the Ith angle.
!
               degs(i) = n - 1

            end do

!
!           Look up the units associated with the angles.
!
            call zzdynvac ( ctx, inname, infram, kwunit, 1, n, units )

            if ( ctx%failed() ) return

!
!           Evaluate the angles at DELTA.  Convert angles from input
!           units to radians.
!
            do i = 1, 3

               call polyds ( coeffs(1,i), degs(i), 0, delta, poly )
!
!              Convert units.  Fill in the Euler angle vector.
!
               call convrt ( ctx, poly(0), units, kvradn, eulang(i) )

            end do

!
!           Produce a position transformation matrix that maps from
!           the defined frame to the base frame.
!
            call eul2m ( ctx, eulang(1), eulang(2), eulang(3), &
                         iaxes(1),  iaxes(2),  iaxes(3),  rotate )

!
!           This is the end of the work specific to Euler frames.
!           From here we drop out of the IF block.
!

         else if ( dynfam == kvprod ) then
!
!           The frame belongs to the product family.
!
!           We expect to see specifications of "from" and "to" frames
!           that make up the product.
!
!           Look up the "from" and "to" frames that define the product.
!
            call zzdynvac ( ctx, inname, infram, kwffrm, mxnfac, m, fframs )
            call zzdynvac ( ctx, inname, infram, kwtfrm, mxnfac, n, tframs )

            if ( ctx%failed() ) return

            if ( n /= m ) then

               call ctx%setmsg ( 'Definition of product ' &
               //            'parameterized dynamic frame # ' &
               //            'has # "from" frames and # "to" ' &
               //            'frames. These counts must match.' )
               call ctx%errch  ( '#',  inname                       )
               call ctx%errint ( '#',  m                            )
               call ctx%errint ( '#',  n                            )
               call ctx%sigerr ( 'SPICE(BADFRAMECOUNT)'             )
               return

            end if

!
!           The product of the factors is the transformation from the
!           base frame to the output frame. We need the inverse of
!           this transformation.
!
!           Compute the inverse of the product of the factors; place
!           the result in XFORM.
!
            call namfrm ( ctx, fframs(1), fromid )
            call namfrm ( ctx, tframs(1), toid   )

            call zzrefch1 ( ctx, toid, fromid, t0, rotate )

            if ( ctx%failed() ) return

            do i = 2, n

               call moved  ( rotate, 9,  rtemp )

               call namfrm ( ctx, fframs(i), fromid )
               call namfrm ( ctx, tframs(i), toid   )

               call zzrefch1 ( ctx, toid, fromid, t0, rotate )

               if ( ctx%failed() ) return

               call mxm   ( rotate, rtemp, rout   )
               call moved ( rout,  9,      rotate )

            end do

         else

            call ctx%setmsg ( 'Dynamic frame family # (in definition of '// &
                          'frame #) is not supported. '              // &
                          'This situation is usually caused by '     // &
                          'an error in a frame kernel in which '     // &
                          'the frame is defined.'                    )
            call ctx%errch  ( '#',  dynfam                               )
            call ctx%errch  ( '#',  inname                               )
            call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'                      )
            return

         end if

!
!        This is the end of the IF block that processes the
!        parameterized dynamic frame families.
!

      else

         call ctx%setmsg ( 'Dynamic frame style # (in definition of ' // &
                       'frame #) is not supported. '              // &
                       'This situation is usually caused by '     // &
                       'an error in a frame kernel in which '     // &
                       'the frame is defined.'                    )
         call ctx%errch  ( '#',  dynstl                               )
         call ctx%errch  ( '#',  inname                               )
         call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'                      )
         return

      end if

!
!     At this point ROTATE is the position transformation matrix
!     mapping from the input frame INFRAM to the base frame BASFRM.
!
!     ROTATE and BASFRM is set.
!
      return
      end associate
      end procedure zzdynrt0

      module procedure zzrotgt0
!! ## Abstract
!!
!!     Find the rotation from a user specified frame to another frame at
!!     a user specified epoch.
!!
!! ## Required Reading
!!
!!     FRAMES
!!
!! ## Keywords
!!
!!     FRAMES
!!
!! ## Declarations
!!
!! ## Detailed Input
!!
!!     INFRM       is the SPICE ID-code for some reference frame.
!!
!!     ET          is an epoch in ephemeris seconds past J2000 at which
!!                 the user wishes to retrieve a rotation matrix.
!!
!! ## Detailed Output
!!
!!     ROTATE      is a 3x3 matrix that rotates positions relative to
!!                 INFRM to positions relative to OUTFRM. (Assuming such
!!                 a rotation can be found.)
!!
!!     OUTFRM      is the SPICE ID-code of a reference frame. The 3x3
!!                 matrix ROTATE rotates positions relative to INFRM to
!!                 positions relative to OUTFRM. The positions
!!                 transformation is achieved by multiplying ROTATE on
!!                 the right by a position relative to INFRM. This is
!!                 easily accomplished via the subroutine call shown
!!                 below.
!!
!!                    CALL MXV  ( ROTATE, INPOS,  OUTPOS )
!!
!!     FOUND       is a logical flag indicating whether or not a
!!                 rotation matrix could be found from INFRM to some
!!                 other frame. If a rotation matrix cannot be found
!!                 OUTFRM will be set to zero, FOUND will be set to
!!                 FALSE and ROTATE will be returned as the zero matrix.
!!
!! ## Parameters
!!
!!     None.
!!
!! ## Exceptions
!!
!!     1) If a rotation matrix cannot be located, then FOUND will be set
!!        to FALSE, OUTFRM will be set to zero and ROTATE will be set to
!!        the zero 3x3 matrix.
!!
!!     2) If the class of the requested frame is not recognized the
!!        exception 'SPICE(UNKNOWNFRAMETYPE)' will be signaled.
!!
!! ## Files
!!
!!     LSK, SCLK, PCK, FK, SPK, and/or CK kernels may need to be loaded
!!     to provide the needed frame definition and transformation data.
!!
!! ## Particulars
!!
!!     This is a low level routine used for determining a chain of
!!     position transformation matrices from one frame to another.
!!
!! ## Examples
!!
!!     See REFCHG.
!!
!! ## Restrictions
!!
!!     None.
!!
!! ## Literature References
!!
!!     None.
!!
!! ## Author and Institution
!!
!!     N.J. Bachman    (JPL)
!!     B.V. Semenov    (JPL)
!!     W.L. Taber      (JPL)
!!
!! ## Version
!!
!!-    SPICELIB Version 5.0.0, 18-SEP-2020 (NJB)
!!
!!        Support for switch frames was added. VERSN is now
!!        initialized via a DATA statement. Corrected long error
!!        message to use the term "class" rather than "class id-code."
!!
!!-    SPICELIB Version 4.0.0, 21-MAR-2014 (BVS)
!!
!!        To prevent operations with un-initialized DP numbers, wrapped
!!        IF ( .NOT. FAILED() ) ... END IF around output matrix
!!        transposition operation in the PCK frame branch where the
!!        routine returning the matrix might fail.
!!
!!        Incremented major version token by 2 to sync up versions with
!!        FRMGET.
!!
!!-    SPICELIB Version 2.1.0, 02-MAR-2010 (NJB)
!!
!!        Bug fix: frame ID rather than frame class ID
!!        is now passed to dynamic frame evaluation
!!        routine ZZDYNROT. Order of header sections was
!!        corrected.
!!
!!-    SPICELIB Version 2.0.0, 18-DEC-2004 (NJB)
!!
!!        Added the new frame type 'DYN' to the list of frame
!!        types recognized by ROTGET.
!!
!!-    SPICELIB Version 1.0.0, 03-MAR-1999 (WLT)
!!
!! ## Index Entries
!!
!!     Find a rotation matrix from a specified frame
!!
!!     SPICELIB Functions
!!
!!     Local Variables
!!
!!     Saved variables
!!
!!     Initial values
!!
!!     Set output flag.
!!
      use pepper_kinds, only: ip, wp
      use pepper_arrays, only: cleard
      use pepper_math, only: xpose
      use pepper_inc_frmtyp
      implicit none
      integer(ip)           center
      integer(ip)           type
      integer(ip)           typeid
      real(wp)              tipm  ( 3, 3 )
      if ( .not. allocated ( ctx%zzrotgt0_state ) ) then
         allocate ( ctx%zzrotgt0_state )
         call init_zzrotgt0_state ( ctx%zzrotgt0_state )
      end if
      associate ( versn => ctx%zzrotgt0_state%versn )
      found = .false.

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Get all the needed information about this frame.
!
      call frinfo ( ctx, infrm, center, type, typeid, found )

      if ( .not. found ) then

         call cleard ( 9, rotate )
         outfrm = 0

         return

      end if

!
!     FOUND was set to true by the FRINFO call. Compute rotation based
!     on the frame class.
!
      if ( type == inertl ) then

         call irfrot ( ctx, infrm, 1, rotate )

         if ( .not. ctx%failed() ) then

            outfrm = 1

         end if

      else if ( type == pck ) then

         call tipbod ( ctx, 'J2000', typeid, et, tipm   )

         if ( .not. ctx%failed() ) then

            call xpose ( tipm, rotate )

            outfrm = 1

         end if

      else if ( type == ck ) then

         call ckfrot ( ctx, typeid, et, rotate, outfrm, found )

      else if ( type == tk ) then

         call tkfram  ( ctx, typeid, rotate, outfrm, found )

      else if ( type == dyn ) then

!
!        Unlike the other frame classes, the dynamic frame evaluation
!        routine ZZDYNROT requires the input frame ID rather than the
!        dynamic frame class ID. ZZDYNROT also requires the center ID
!        we found via the FRINFO call.
!
         call zzdynrt0 ( ctx, infrm, center, et, rotate, outfrm )

!
!        The FOUND flag was set by FRINFO earlier; we don't touch
!        it here. If ZZDYNROT signaled an error, FOUND will be set
!        to .FALSE. at end of this routine.
!

      else if ( type == swtch ) then

         call zzswfxfm ( ctx, infrm, et, 3, rotate, outfrm, found )

      else

         call cleard ( 9, rotate )
         outfrm = 0
         found  = .false.

         call ctx%setmsg ( 'The reference frame # has class #. ' &
         //            'This form of reference frame is not ' &
         //            'supported in version # of ZZROTGT0. You need ' &
         //            'to update your version of SPICELIB to the ' &
         //            'latest version in order to support this ' &
         //            'frame. ' )

         call ctx%errint ( '#', infrm )
         call ctx%errint ( '#', type  )
         call ctx%errch  ( '#', versn )
         call ctx%sigerr ( 'SPICE(UNKNOWNFRAMETYPE)' )
         return

      end if

!
!     Make sure to clear outputs in case of a failure as defined in
!     in the header.
!
      if ( ctx%failed() .or. .not. found ) then

         call cleard ( 9, rotate )
         outfrm = 0
         found  = .false.

      end if

      return
      end associate
      end procedure zzrotgt0

!$Procedure      ZZREFCH0 (Reference frame Change)

      module procedure zzrefch0
!! ## Abstract
!!
!!     Return the transformation matrix from one
!!     frame to another.
!!
!! ## Required Reading
!!
!!     None.
!!
!! ## Keywords
!!
!!     FRAMES
!!
!! ## Declarations
!!
      use pepper_kinds, only: ip, wp
      use pepper_arrays, only: isrchi
      use pepper_math, only: ident, xpose, zzrxr
      use pepper_inc_errhnd
      use pepper_inc_frmtyp
      implicit none



! $ Detailed_Input
!
!     FRAME1      is the frame id-code in which some positions
!                 are known.
!
!     FRAME2      is the frame id-code for some frame in which you
!                 would like to represent positions.
!
!     ET          is the epoch at which to compute the transformation
!                 matrix.  This epoch should be in TDB seconds past
!                 the ephemeris epoch of J2000.
!
! $ Detailed_Output
!
!     ROTATE      is a 3 x 3 rotation matrix that can be used to
!                 transform positions relative to the frame
!                 corresponding to frame FRAME2 to positions relative
!                 to the frame FRAME2.  More explicitly, if POS is
!                 the position of some object relative to the
!                 reference frame of FRAME1 then POS2 is the position
!                 of the same object relative to FRAME2 where POS2 is
!                 computed via the subroutine call below
!
!                    CALL MXV ( ROTATE, POS, POS2 )
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
!     This routine allows you to compute the rotation matrix
!     between two reference frames.
!
! $ Examples
!
!     Suppose that you have a position POS1 at epoch ET
!     relative to  FRAME1 and wish to determine its representation
!     POS2 relative to FRAME2.  The following subroutine calls
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
!     W.L. Taber      (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.1.0, 08-OCT-2021 (NJB)
!
!        Bug fix: added calls to FAILED after each call to
!        FRINFO and to ZZROTGT0.
!
!        Corrected typos in comments.
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
!-    SPICELIB Version 1.0.0, 9-JUL-1998 (WLT)
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


!
!     Local Parameters
!
      integer(ip), parameter :: maxnod = 10

      integer(ip), parameter :: maxxfm = maxnod + 4

!
!     The root of all reference frames is J2000 (Frame ID = 1).
!
      integer(ip), parameter :: rootf = 1

!
!     Local Variables
!
      character(lmsgln)     errmsg

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
      if ( ctx%failed() ) return

!
!     Do the obvious thing first.  If FRAME1 and FRAME2 are the
!     same then we simply return the identity matrix.
!
      if ( frame1 == frame2 ) then

         call ident  ( rotate )
         return

      end if
!
!     Now perform the obvious check to make sure that both
!     frames are recognized.
!
      call frinfo ( ctx, frame1, cent, class, clssid, found )

      if ( ctx%failed() ) return

      if ( .not. found ) then

         call ctx%setmsg ( 'The number # is not a recognized id-code ' &
         //            'for a reference frame. ' )
         call ctx%errint ( '#', frame1 )
         call ctx%sigerr ( 'SPICE(UNKNOWNFRAME)'  )
         return

      end if

      call frinfo ( ctx, frame2, cent, class, clssid, found )

      if ( ctx%failed() ) return

      if ( .not. found ) then

         call ctx%setmsg ( 'The number # is not a recognized id-code ' &
         //            'for a reference frame. ' )
         call ctx%errint ( '#', frame2 )
         call ctx%sigerr ( 'SPICE(UNKNOWNFRAME)'  )
         return

      end if

      node        = 1
      frame(node) = frame1
      found       = .true.
!
!     Follow the chain of rotations until we run into
!     one that rotates to J2000 (frame id = 1) or we hit FRAME2.
!
      do while (       frame(node) /= rootf &
                 .and. node        < maxnod &
                 .and. frame(node) /= frame2 &
                 .and. found   )
!
!        Find out what rotation is available for this
!        frame.
!
         call zzrotgt0 ( ctx, frame(node),   et, &
                         rot(1,1,node), frame(node+1), found  )

         if ( ctx%failed() ) return

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

      done =      frame(node) == rootf &
             .or. frame(node) == frame2 &
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
         call zzrotgt0 ( ctx, frame(node), et, rot(1,1,node), relto, found )

         if ( ctx%failed() ) return

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
         done =        frame(node) == rootf &
                  .or. frame(node) == frame2 &
                  .or. .not. found

      end do

!
!     Right now we have the following situation.  We have in hand
!     a collection of rotations between frames. (Assuming
!     that is that NODE .GT. 1.  If NODE .EQ. 1 then we have
!     no rotations computed yet.
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

      if ( frame(node) == frame2 ) then
!
!        We can handle this one immediately with the private routine
!        ZZRXR which multiplies a series of matrices.
!
         call zzrxr (  rot, node-1, rotate )
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
      done   =  this == rootf

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
         if ( this  == frame2 ) then
!
!           This is the first pass, just put the rotation
!           directly into ROT2(,,PUT).
!
            call zzrotgt0 ( ctx, this,  et, rot2(1,1,put), relto, found  )

            if ( ctx%failed() ) return

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
!           Fetch the rotation into a temporary spot TMPROT
!
            call zzrotgt0 ( ctx, this,  et, tmprot, relto,  found  )

            if ( ctx%failed() ) return

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
!     rotations from FRAME2 ran into a node in the chain for
!     FRAME1 or it didn't.  (The common node might very well be
!     the root node.)  If we didn't run into a common one, then
!     the two chains don't intersect and there is no way to
!     get from FRAME1 to FRAME2.
!
      if ( .not. gotone ) then

         call zznofcon ( ctx, et, frame1, frame(node), frame2, this, errmsg )

         if ( ctx%failed() ) then
!
!           We were unable to create the error message. This
!           unfortunate situation could arise if a frame kernel
!           is corrupted.
!
            return

         end if
!
!        The normal case: signal an error with a descriptive long
!        error message.
!
         call ctx%setmsg ( errmsg                  )
         call ctx%sigerr ( 'SPICE(NOFRAMECONNECT)' )
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

      return

      end procedure zzrefch0

      module procedure zzspkgp0
!! ## Abstract
!!
!!     SPICE Private routine intended solely for the support of SPICE
!!     routines.  Users should not call this routine directly due
!!     to the volatile nature of this routine.
!!
!!     Compute the geometric position of a target body relative to an
!!     observing body.
!!
!! ## Required Reading
!!
!!     SPK
!!
!! ## Keywords
!!
!!     EPHEMERIS
!!
!! ## Declarations
!!
!! ## Detailed Input
!!
!!     TARG     is the standard NAIF ID code for a target body.
!!
!!     ET       is the epoch (ephemeris time) at which the position
!!              of the target body is to be computed.
!!
!!     REF      is the name of the reference frame to
!!              which the vectors returned by the routine should
!!              be rotated. This may be any frame supported by
!!              the SPICELIB subroutine REFCHG.
!!
!!     OBS      is the standard NAIF ID code for an observing body.
!!
!! ## Detailed Output
!!
!!     POS      is a 3-dimensional vector that contains the position of
!!              the target body, relative to the observing body. This
!!              vector is rotated into the specified reference frame.
!!              Units are always km.
!!
!!     LT       is the one-way light time from the observing body
!!              to the geometric position of the target body at the
!!              specified epoch.
!!
!! ## Parameters
!!
!!     None.
!!
!! ## Exceptions
!!
!!     1)  If insufficient ephemeris data has been loaded to compute
!!         the necessary positions, the error SPICE(SPKINSUFFDATA) is
!!         signaled.
!!
!! ## Files
!!
!!     See $Restrictions.
!!
!! ## Particulars
!!
!!     SPKGPS computes the geometric position, T(t), of the target
!!     body and the geometric position, O(t), of the observing body
!!     relative to the first common center of motion. Subtracting
!!     O(t) from T(t) gives the geometric position of the target
!!     body relative to the observer.
!!
!!        CENTER ----- O(t)
!!            |      /
!!            |     /
!!            |    /
!!            |   /  T(t) - O(t)
!!            |  /
!!           T(t)
!!
!!     The one-way light time, tau, is given by
!!
!!               | T(t) - O(t) |
!!        tau = -----------------
!!                      c
!!
!!     For example, if the observing body is -94, the Mars Observer
!!     spacecraft, and the target body is 401, Phobos, then the
!!     first common center is probably 4, the Mars Barycenter.
!!     O(t) is the position of -94 relative to 4 and T(t) is the
!!     position of 401 relative to 4.
!!
!!     The center could also be the Solar System Barycenter, body 0.
!!     For example, if the observer is 399, Earth, and the target
!!     is 299, Venus, then O(t) would be the position of 399 relative
!!     to 0 and T(t) would be the position of 299 relative to 0.
!!
!!     Ephemeris data from more than one segment may be required
!!     to determine the positions of the target body and observer
!!     relative to a common center. SPKGPS reads as many segments
!!     as necessary, from as many files as necessary, using files
!!     that have been loaded by previous calls to SPKLEF (load
!!     ephemeris file).
!!
!!     SPKGPS is similar to SPKGEO but returns geometric positions
!!     only.
!!
!! ## Examples
!!
!!     The following code example computes the geometric
!!     position of the moon with respect to the earth and
!!     then prints the distance of the moon from the
!!     the earth at a number of epochs.
!!
!!     Assume the SPK file SAMPLE.BSP contains ephemeris data
!!     for the moon relative to earth over the time interval
!!     from BEGIN to END.
!!
!!            INTEGER               EARTH
!!            PARAMETER           ( EARTH = 399 )
!!
!!            INTEGER               MOON
!!            PARAMETER           ( MOON  = 301 )
!!
!!            INTEGER               N
!!            PARAMETER           ( N     = 100 )
!!
!!            INTEGER               I
!!            CHARACTER*(20)        UTC
!!            DOUBLE PRECISION      BEGIN
!!            DOUBLE PRECISION      DELTA
!!            DOUBLE PRECISION      END
!!            DOUBLE PRECISION      ET
!!            DOUBLE PRECISION      POS ( 3 )
!!            DOUBLE PRECISION      LT
!!
!!            DOUBLE PRECISION      VNORM
!!
!!     C
!!     C      Load the binary SPK ephemeris file.
!!     C
!!            CALL FURNSH ( 'SAMPLE.BSP' )
!!
!!            .
!!            .
!!            .
!!
!!     C
!!     C      Divide the interval of coverage [BEGIN,END] into
!!     C      N steps. At each step, compute the position, and
!!     C      print out the epoch in UTC time and position norm.
!!     C
!!            DELTA = ( END - BEGIN ) / N
!!
!!            DO I = 0, N
!!
!!               ET = BEGIN + I*DELTA
!!
!!               CALL SPKGPS ( MOON, ET, 'J2000', EARTH, POS, LT )
!!
!!               CALL ET2UTC ( ET, 'C', 0, UTC )
!!
!!               WRITE (*,*) UTC, VNORM ( POS )
!!
!!            END DO
!!
!! ## Restrictions
!!
!!     1)  The ephemeris files to be used by SPKGPS must be loaded
!!         by SPKLEF before SPKGPS is called.
!!
!! ## Literature References
!!
!!     None.
!!
!! ## Author and Institution
!!
!!     N.J. Bachman       (JPL)
!!     J. Diaz del Rio    (ODC Space)
!!     B.V. Semenov       (JPL)
!!     W.L. Taber         (JPL)
!!
!! ## Version
!!
!!-    SPICELIB Version 2.1.0, 09-OCT-2021 (JDR) (NJB)
!!
!!        Bug fix: added calls to FAILED after calls to SPKPVN.
!!        Previously only one call to SPKPVN was followed by a FAILED
!!        call. Moved some FAILED checks so they will be hit whether
!!        or not SPKSFS finds a segment.
!!
!!        Edited the header to comply with NAIF standard. Removed
!!        unnecessary $Revisions section.
!!
!!-    SPICELIB Version 2.0.0, 08-JAN-2014 (BVS)
!!
!!        Updated to save the input frame name and POOL state counter
!!        and to do frame name-ID conversion only if the counter has
!!        changed.
!!
!!        Updated to map the input frame name to its ID by first calling
!!        ZZNAMFRM, and then calling IRFNUM. The side effect of this
!!        change is that now the frame with the fixed name 'DEFAULT'
!!        that can be associated with any code via CHGIRF's entry point
!!        IRFDEF will be fully masked by a frame with identical name
!!        defined via a text kernel. Previously the CHGIRF's 'DEFAULT'
!!        frame masked the text kernel frame with the same name.
!!
!!        Replaced SPKLEF with FURNSH and fixed errors in $Examples.
!!
!!-    SPICELIB Version 1.2.0, 05-NOV-2005 (NJB)
!!
!!        Updated to remove non-standard use of duplicate arguments
!!        in VADD calls.
!!
!!-    SPICELIB Version 1.1.0, 05-JAN-2005 (NJB)
!!
!!        Tests of routine FAILED() were added.
!!
!!-    SPICELIB Version 1.0.0, 09-JUL-1998 (WLT)
!!
!! ## Index Entries
!!
!!     geometric position of one body relative to another
!!
!!     This is the idea:
!!
!!     Every body moves with respect to some center. The center
!!     is itself a body, which in turn moves about some other
!!     center.  If we begin at the target body (T), follow
!!     the chain,
!!
!!                                   T
!!                                     \
!!           SSB                        \
!!               \                     C[1]
!!                \                     /
!!                 \                   /
!!                  \                 /
!!                   \               /
!!                  C[3]-----------C[2]
!!
!!     and avoid circular definitions (A moves about B, and B moves
!!     about A), eventually we get the position relative to the solar
!!     system barycenter (which, for our purposes, doesn't move).
!!     Thus,
!!
!!        T    = T     + C[1]     + C[2]     + ... + C[n]
!!         SSB    C[1]       C[2]       [C3]             SSB
!!
!!     where
!!
!!        X
!!         Y
!!
!!     is the position of body X relative to body Y.
!!
!!     However, we don't want to follow each chain back to the SSB
!!     if it isn't necessary.  Instead we will just follow the chain
!!     of the target body and follow the chain of the observing body
!!     until we find a common node in the tree.
!!
!!     In the example below, C is the first common node.  We compute
!!     the position of TARG relative to C and the position of OBS
!!     relative to C, then subtract the two positions.
!!
!!                                   TARG
!!                                     \
!!           SSB                        \
!!               \                       A
!!                \                     /            OBS
!!                 \                   /              |
!!                  \                 /               |
!!                   \               /                |
!!                    B-------------C-----------------D
!!
!!     SPICELIB functions
!!
!!     Local parameters
!!
!!     CHLEN is the maximum length of a chain.  That is,
!!     it is the maximum number of bodies in the chain from
!!     the target or observer to the SSB.
!!
!!     Saved frame name length.
!!
!!     Local variables
!!
!!     Saved frame name/ID item declarations.
!!
!!     Saved frame name/ID items.
!!
!!     Initial values.
!!
!!     In-line Function Definitions
!!
!!     Standard SPICE error handling.
!!
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: clight
      use pepper_arrays, only: cleard, isrchi, moved
      use pepper_math, only: mxv, vadd, vequ, vnorm, vsub
      use pepper_strings, only: frstnp, intstr, prefix, suffix
      use pepper_inc_ninert
      use pepper_inc_zzctr
      implicit none
      character(*), parameter :: rname = 'ZZSPKGP0'
      integer(ip), parameter :: chlen = 20
      integer(ip), parameter :: ssb = 0
      integer(ip), parameter :: frnmln = 32
      character(40)         ident
      character(40)         tname
      character(40)         oname
      character(80)         tstring
      real(wp)              descr  ( 5        )
      real(wp)              sobs   ( 6        )
      real(wp)              starg  ( 6, chlen )
      real(wp)              stemp  ( 6        )
      real(wp)              psxfrm ( 3, 3     )
      real(wp)              rot    ( 3, 3     )
      real(wp)              vtemp  ( 6        )
      integer(ip)           cframe
      integer(ip)           cobs
      integer(ip)           ctarg  (    chlen )
      integer(ip)           tframe (    chlen )
      integer(ip)           ctpos
      integer(ip)           handle
      integer(ip)           i
      integer(ip)           legs
      integer(ip)           nct
      integer(ip)           refid
      integer(ip)           tmpfrm
      logical               found
      logical               nofrm
      logical               isinrt
      isinrt ( tmpfrm, cframe ) =       cframe > 0 &
                                  .and. cframe <= ninert &
                                  .and. tmpfrm > 0 &
                                  .and. tmpfrm <= ninert
      if ( .not. allocated ( ctx%zzspkgp0_state ) ) then
         allocate ( ctx%zzspkgp0_state )
         call init_zzspkgp0_state ( ctx%zzspkgp0_state )
      end if
      associate ( svctr1 => ctx%zzspkgp0_state%svctr1, &
                  svref => ctx%zzspkgp0_state%svref, &
                  svrefi => ctx%zzspkgp0_state%svrefi, &
                  first => ctx%zzspkgp0_state%first )
      if ( ctx%failed() ) return

!
!     Initialization.
!
      if ( first ) then

!
!        Initialize counter.
!
         call zzctruin( ctx, svctr1 )

         first = .false.

      end if

!
!     We take care of the obvious case first.  It TARG and OBS are the
!     same we can just fill in zero.
!
      if ( targ == obs ) then

         lt = 0.0_wp

         call cleard ( 3, pos )
         return

      end if

!
!     CTARG contains the integer codes of the bodies in the
!     target body chain, beginning with TARG itself and then
!     the successive centers of motion.
!
!     STARG(1,I) is the position of the target body relative
!     to CTARG(I).  The id-code of the frame of this position is
!     stored in TFRAME(I).
!
!     COBS and SOBS will contain the centers and positions of the
!     observing body.  (They are single elements instead of arrays
!     because we only need the current center and position of the
!     observer relative to it.)
!
!     First, we construct CTARG and STARG.  CTARG(1) is
!     just the target itself, and STARG(1,1) is just a zero
!     vector, that is, the position of the target relative
!     to itself.
!
!     Then we follow the chain, filling up CTARG and STARG
!     as we go.  We use SPKSFS to search through loaded
!     files to find the first segment applicable to CTARG(1)
!     and time ET.  Then we use SPKPVN to compute the position
!     of the body CTARG(1) at ET in the segment that was found
!     and get its center and frame of motion (CTARG(2) and TFRAME(2).
!
!     We repeat the process for CTARG(2) and so on, until
!     there is no data found for some CTARG(I) or until we
!     reach the SSB.
!
!     Next, we find centers and positions in a similar manner
!     for the observer.  It's a similar construction as
!     described above, but I is always 1.  COBS and SOBS
!     are overwritten with each new center and position,
!     beginning at OBS.  However, we stop when we encounter
!     a common center of motion, that is when COBS is equal
!     to CTARG(I) for some I.
!
!     Finally, we compute the desired position of the target
!     relative to the observer by subtracting the position of
!     the observing body relative to the common node from
!     the position of the target body relative to the common
!     node.
!
!     CTPOS is the position in CTARG of the common node.
!
!     Since the upgrade to use hashes and counter bypass ZZNAMFRM
!     became more efficient in looking up frame IDs than IRFNUM. So the
!     original order of calls "IRFNUM first, NAMFRM second" was
!     switched to "ZZNAMFRM first, IRFNUM second".
!
!     The call to IRFNUM, now redundant for built-in inertial frames,
!     was preserved to for a sole reason -- to still support the
!     ancient and barely documented ability for the users to associate
!     a frame with the fixed name 'DEFAULT' with any CHGIRF inertial
!     frame code via CHGIRF's entry point IRFDEF.
!
!     Note that in the case of ZZNAMFRM's failure to resolve name and
!     IRFNUM's success to do so, the code returned by IRFNUM for
!     'DEFAULT' frame is *not* copied to the saved code SVREFI (which
!     would be set to 0 by ZZNAMFRM) to make sure that on subsequent
!     calls ZZNAMFRM does not do a bypass (as SVREFI always forced look
!     up) and calls IRFNUM again to reset the 'DEFAULT's frame ID
!     should it change between the calls.
!
      call zznamfrm ( ctx, svctr1, svref, svrefi, ref, refid )

      if ( refid == 0 ) then
         call irfnum ( ctx, ref, refid )
      end if

      if ( refid == 0 ) then

         if (  frstnp(ref) > 0 ) then

            call ctx%setmsg ( 'The string supplied to specify the ' &
            //            'reference frame, (''#'') contains ' &
            //            'non-printing characters.  The two ' &
            //            'most common causes for this kind ' &
            //            'of error are: 1. an error in the ' &
            //            'call to ZZSPKGP0; 2. an ' &
            //            'uninitialized variable. ' )
            call ctx%errch  ( '#', ref )

         else if ( ref == ' ' ) then

            call ctx%setmsg ( 'The string supplied to specify the ' &
            //            'reference frame is blank.  The most ' &
            //            'common cause for this kind of error ' &
            //            'is an uninitialized variable. ' )

         else

            call ctx%setmsg ( 'The string supplied to specify the ' &
            //            'reference frame was ''#''.  This ' &
            //            'frame is not recognized. Possible ' &
            //            'causes for this error are: 1. failure ' &
            //            'to load the frame definition into the ' &
            //            'kernel pool; 2. An out-of-date ' &
            //            'edition of the toolkit. ' )
            call ctx%errch  ( '#', ref )

         end if

         call ctx%sigerr ( 'SPICE(UNKNOWNFRAME)' )

         if ( ctx%failed() ) return

      end if

!
!     Fill in CTARG and STARG until no more data is found
!     or until we reach the SSB.  If the chain gets too
!     long to fit in CTARG, that is if I equals CHLEN,
!     then overwrite the last elements of CTARG and STARG.
!
!     Note the check for FAILED in the loop.  If SPKSFS
!     or SPKPVN happens to fail during execution, and the
!     current error handling action is to NOT abort, then
!     FOUND may be stuck at TRUE, CTARG(I) will never
!     become zero, and the loop will execute indefinitely.
!
!     Construct CTARG and STARG.  Begin by assigning the
!     first elements:  TARG and the position of TARG relative
!     to itself.
!
      i        =  1
      ctarg(i) =  targ
      found    = .true.

      call cleard ( 6, starg(1,i) )

      do while (           found &
                  .and.  ( i        < chlen ) &
                  .and.  ( ctarg(i) /= obs   ) &
                  .and.  ( ctarg(i) /= ssb   )  )

!
!        Find a file and segment that has position
!        data for CTARG(I).
!
         call spksfs ( ctx, ctarg(i), et, handle, descr, ident, found )

         if ( found ) then
!
!           Get the position of CTARG(I) relative to some
!           center of motion.  This new center goes in
!           CTARG(I+1) and the position is called STEMP.
!
            i = i + 1

            call spkpvn ( ctx, handle,    descr,      et, &
                          tframe(i), starg(1,i), ctarg(i) )
!
!           Here's what we have.  STARG is the position of CTARG(I-1)
!           relative to CTARG(I) in reference frame TFRAME(I)
!
         end if
!
!        If one of the routines above failed during
!        execution, we just give up and return.
!
         if ( ctx%failed() ) return

      end do

      tframe(1) = tframe(2)
!
!     If the loop above ended because we ran out of
!     room in the arrays CTARG and STARG, then we
!     continue finding positions but we overwrite the
!     last elements of CTARG and STARG.
!
!     If, as a result, the first common node is
!     overwritten, we'll just have to settle for
!     the last common node.  This will cause a small
!     loss of precision, but it's better than other
!     alternatives.
!
      if ( i == chlen ) then

         do while (      found &
                   .and. ctarg(chlen) /= ssb &
                   .and. ctarg(chlen) /= obs )

!
!           Find a file and segment that has position
!           data for CTARG(CHLEN).
!
            call spksfs ( ctx, ctarg(chlen), et, handle, descr, ident, found)

            if ( found ) then
!
!              Get the position of CTARG(CHLEN) relative to
!              some center of motion.  The new center
!              overwrites the old.  The position is called
!              STEMP.
!
               call spkpvn ( ctx, handle, descr, et, tmpfrm, stemp, &
                             ctarg(chlen))

               if ( ctx%failed() ) return

!
!              Add STEMP to the position of TARG relative to
!              the old center to get the position of TARG
!              relative to the new center.  Overwrite
!              the last element of STARG.
!
               if ( tframe(chlen) == tmpfrm ) then

                  call moved ( starg(1,chlen), 3, vtemp )

               else if ( isinrt ( tframe(chlen), tmpfrm ) ) then

                  call irfrot( ctx, tframe(chlen), tmpfrm,         rot      )
                  call mxv   ( rot,           starg(1,chlen), vtemp(1) )

               else

                  call zzrefch0( ctx, tframe(chlen), tmpfrm,   et,   psxfrm )

                  if ( ctx%failed() ) return

                  call mxv   ( psxfrm, starg (1,chlen),       vtemp  )

               end if

               call vadd ( vtemp,  stemp, starg(1,chlen) )

               tframe(chlen) = tmpfrm

            end if

!
!           If one of the routines above failed during
!           execution, we just give up and return.
!
            if ( ctx%failed() ) return

         end do

      end if

      nct = i
!
!     NCT is the number of elements in CTARG,
!     the chain length.  We have in hand the following information
!
!        STARG(1...3,K)  position of body
!        CTARG(K-1)      relative to body CTARG(K) in the frame
!        TFRAME(K)
!
!     For K = 2,..., NCT.
!
!     CTARG(1) = TARG
!     STARG(1...3,1) = ( 0, 0, 0 )
!     TFRAME(1)      = TFRAME(2)
!

!
!     Now follow the observer's chain.  Assign
!     the first values for COBS and SOBS.
!
      cobs = obs
      call cleard ( 6, sobs )

!
!     Perhaps we have a common node already.
!     If so it will be the last node on the
!     list CTARG.
!
!     We let CTPOS will be the position of the common
!     node in CTARG if one is found.  It will
!     be zero if COBS is not found in CTARG.
!
      if ( ctarg(nct) == cobs ) then
         ctpos  = nct
         cframe = tframe(ctpos)
      else
         ctpos  = 0
      end if

!
!     Repeat the same loop as above, but each time
!     we encounter a new center of motion, check to
!     see if it is a common node.  (When CTPOS is
!     not zero, CTARG(CTPOS) is the first common node.)
!
!     Note that we don't need a centers array nor a
!     positions array, just a single center and position
!     is sufficient --- we just keep overwriting them.
!     When the common node is found, we have everything
!     we need in that one center (COBS) and position
!     (SOBS-position of the target relative to COBS).
!
      found  = .true.
      nofrm  = .true.
      legs   =  0

      do while (           found &
                  .and.  ( cobs  /= ssb ) &
                  .and.  ( ctpos == 0   )  )

!
!        Find a file and segment that has position
!        data for COBS.
!
         call spksfs ( ctx, cobs, et, handle, descr, ident, found )

         if ( found ) then
!
!           Get the position of COBS; call it STEMP.
!           The center of motion of COBS becomes the
!           new COBS.
!
            if ( legs == 0 ) then
               call spkpvn ( ctx, handle, descr, et, tmpfrm, sobs,  cobs )
            else
               call spkpvn ( ctx, handle, descr, et, tmpfrm, stemp, cobs )
            end if

            if ( ctx%failed() ) return

            if ( nofrm ) then
               nofrm  = .false.
               cframe = tmpfrm
            end if
!
!           Add STEMP to the position of OBS relative to
!           the old COBS to get the position of OBS
!           relative to the new COBS.
!
            if ( cframe == tmpfrm ) then
!
!              On the first leg of the position of the observer, we
!              don't have to add anything, the position of the
!              observer is already in SOBS.  We only have to add when
!              the number of legs in the observer position is one or
!              greater.
!
               if ( legs > 0 ) then
                  call vadd ( sobs,  stemp, vtemp )
                  call vequ ( vtemp,        sobs  )
               end if

            else if ( isinrt( cframe, tmpfrm ) ) then

               call irfrot ( ctx, cframe, tmpfrm, rot )
               call mxv    ( rot,    sobs(1),   vtemp(1)  )
               call vadd   ( vtemp,  stemp,     sobs      )
               cframe = tmpfrm

            else

               call zzrefch0 ( ctx, cframe, tmpfrm, et,   psxfrm )

               if ( ctx%failed() ) return

               call mxv    ( psxfrm, sobs,         vtemp  )
               call vadd   ( vtemp,  stemp,        sobs   )
               cframe = tmpfrm

            end if

!
!           We now have one more leg of the path for OBS.  Set
!           LEGS to reflect this.  Then see if the new center
!           is a common node. If not, repeat the loop.
!
            legs  = legs + 1
            ctpos = isrchi ( cobs, nct, ctarg )

         end if

!
!        Check failed.  We don't want to loop indefinitely.
!
         if ( ctx%failed() ) return

      end do

!
!     If CTPOS is zero at this point, it means we
!     have not found a common node though we have
!     searched through all the available data.
!
      if ( ctpos == 0 ) then

         call bodc2n ( ctx, targ, tname,   found )

         if ( found ) then
            call prefix ( '# (',  0,        tname )
            call suffix ( ')',    0,        tname )
            call repmi  ( ctx, tname, '#', targ, tname )
         else
            call intstr ( targ, tname )
         end if

         call bodc2n ( ctx, obs,  oname,   found )

         if ( found ) then
            call prefix ( '# (',  0,       oname )
            call suffix ( ')',    0,       oname )
            call repmi  ( ctx, oname, '#', obs, oname )
         else
            call intstr ( obs, oname )
         end if

         call ctx%setmsg ( 'Insufficient ephemeris data has been ' &
         //            'loaded to compute the position of TARG ' &
         //            'relative to OBS at the ephemeris epoch ' &
         //            '#. ' )

         call etcal  ( ctx,  et,    tstring                                 )
         call ctx%errch  ( 'TARG', tname                                   )
         call ctx%errch  ( 'OBS',  oname                                   )
         call ctx%errch  ( '#',    tstring                                 )
         call ctx%sigerr ( 'SPICE(SPKINSUFFDATA)'                          )
         return

      end if

!
!     If CTPOS is not zero, then we have reached a
!     common node, specifically,
!
!        CTARG(CTPOS) = COBS = CENTER
!
!     (in diagram below).  The POSITION of the target
!     (TARG) relative to the observer (OBS) is just
!
!        STARG(1,CTPOS) - SOBS.
!
!                     SOBS
!         CENTER ---------------->OBS
!            |                  .
!            |                . N
!         S  |              . O
!         T  |            . I
!         A  |          . T
!         R  |        . I
!         G  |      . S
!            |    . O
!            |  . P
!            V L
!           TARG
!
!     And the light-time between them is just
!
!               | POSITION |
!          LT = ---------
!                   c
!
!     Compute the position of the target relative to CTARG(CTPOS)
!
      if ( ctpos == 1 ) then
         tframe(1) = cframe
      end if

      do i = 2, ctpos-1

         if ( tframe(i) == tframe(i+1) ) then

            call vadd  ( starg(1,i), starg(1,i+1),  stemp        )
            call moved ( stemp,      3,             starg(1,i+1) )

         else if ( isinrt ( tframe(i), tframe(i+1) ) ) then

            call irfrot ( ctx, tframe(i),  tframe(i+1),     rot          )
            call mxv    ( rot,        starg(1,i),      stemp(1)     )
            call vadd   ( stemp,      starg(1,i+1),    vtemp        )
            call moved  ( vtemp,      3,               starg(1,i+1) )

         else

            call zzrefch0 ( ctx, tframe(i),  tframe(i+1), et,  psxfrm       )

            if ( ctx%failed() ) return

            call mxv    ( psxfrm,     starg(1,i),       stemp        )
            call vadd   ( stemp,      starg(1,i+1),     vtemp        )
            call moved  ( vtemp,      3,                starg(1,i+1) )

         end if

      end do

!
!     To avoid unnecessary frame transformations we'll do
!     a bit of extra decision making here.  It's a lot
!     faster to make logical checks than it is to compute
!     frame transformations.
!
      if ( tframe(ctpos) == cframe ) then

         call vsub   ( starg(1,ctpos), sobs,  pos  )

      else if ( tframe(ctpos) == refid ) then

!
!        If the last frame associated with the target is already
!        in the requested output frame, we convert the position of
!        the observer to that frame and then subtract the position
!        of the observer from the position of the target.
!
         if ( isinrt( cframe, refid ) ) then

            call irfrot ( ctx, cframe, refid,   rot      )
            call mxv    ( rot,    sobs(1), stemp(1) )

         else

            call zzrefch0 ( ctx, cframe, refid, et,  psxfrm )

            if ( ctx%failed() ) return

            call mxv    ( psxfrm, sobs,       stemp  )

         end if

!
!        We've now transformed SOBS into the requested reference frame.
!        Set CFRAME to reflect this.
!
         cframe = refid
         call vsub  ( starg(1,ctpos), stemp, pos )

      else if ( isinrt( tframe(ctpos), cframe ) ) then

!
!        If both frames are inertial we use IRFROT instead of
!        ZZREFCH0 to get things into a common frame.
!
         call irfrot ( ctx, tframe(ctpos),  cframe,               rot      )
         call mxv    ( rot,            starg(1,ctpos),       stemp(1) )
         call vsub   ( stemp,          sobs,                 pos    )

      else
!
!        Use the more general routine ZZREFCH0 to make the
!        transformation.
!
         call zzrefch0 ( ctx, tframe(ctpos),  cframe,          et,  psxfrm )

         if ( ctx%failed() ) return

         call mxv    ( psxfrm,         starg(1,ctpos),       stemp  )
         call vsub   ( stemp,          sobs,                 pos  )

      end if

!
!     Finally, rotate as needed into the requested frame.
!
      if      ( cframe == refid ) then

!
!        We don't have to do anything in this case.
!

      else if ( isinrt ( cframe, refid ) ) then
!
!        Since both frames are inertial, we use the more direct
!        routine IRFROT to get the transformation to REFID.
!
         call irfrot ( ctx, cframe,  refid,    rot          )
         call mxv    ( rot,     pos(1),   stemp(1)     )
         call moved  ( stemp,        3,   pos          )

      else

         call zzrefch0 ( ctx, cframe,  refid, et,  psxfrm  )

         if ( ctx%failed() ) return

         call mxv    ( psxfrm,  pos,        stemp   )
         call moved  ( stemp,          3,   pos     )

      end if

      lt = vnorm  ( pos ) / clight

      return
      end associate
      end procedure zzspkgp0

      module procedure zzspkpa0
!! ## Abstract
!!
!!     Return the position of a target body relative to an observer,
!!     optionally corrected for light time and stellar aberration.
!!
!! ## Required Reading
!!
!!     SPK
!!
!! ## Keywords
!!
!!     EPHEMERIS
!!
!! ## Declarations
!!
!! ## Detailed Input
!!
!!     TARG        is the NAIF ID code for a target body.  The target
!!                 and observer define a position vector which points
!!                 from the observer to the target.
!!
!!     ET          is the ephemeris time, expressed as seconds past
!!                 J2000 TDB, at which the position of the target body
!!                 relative to the observer is to be computed.  ET
!!                 refers to time at the observer's location.
!!
!!     REF         is the inertial reference frame with respect to which
!!                 the observer's state SOBS is expressed. REF must be
!!                 recognized by the SPICE Toolkit.  The acceptable
!!                 frames are listed in the Frames Required Reading, as
!!                 well as in the SPICELIB routine CHGIRF.
!!
!!                 Case and blanks are not significant in the string REF.
!!
!!     SOBS        is the geometric (uncorrected) state of the observer
!!                 relative to the solar system barycenter at epoch ET.
!!                 SOBS is a 6-vector:  the first three components of
!!                 SOBS represent a Cartesian position vector; the last
!!                 three components represent the corresponding velocity
!!                 vector.  SOBS is expressed relative to the inertial
!!                 reference frame designated by REF.
!!
!!                 Units are always km and km/sec.
!!
!!     ABCORR      indicates the aberration corrections to be applied to
!!                 the position of the target body to account for
!!                 one-way light time and stellar aberration.  See the
!!                 discussion in the Particulars section for
!!                 recommendations on how to choose aberration
!!                 corrections.
!!
!!                 ABCORR may be any of the following:
!!
!!                    'NONE'     Apply no correction. Return the
!!                               geometric position of the target body
!!                               relative to the observer.
!!
!!                 The following values of ABCORR apply to the
!!                 "reception" case in which photons depart from the
!!                 target's location at the light-time corrected epoch
!!                 ET-LT and *arrive* at the observer's location at ET:
!!
!!                    'LT'       Correct for one-way light time (also
!!                               called "planetary aberration") using a
!!                               Newtonian formulation. This correction
!!                               yields the position of the target at the
!!                               moment it emitted photons arriving at
!!                               the observer at ET.
!!
!!                               The light time correction involves
!!                               iterative solution of the light time
!!                               equation (see Particulars for details).
!!                               The solution invoked by the 'LT' option
!!                               uses one iteration.
!!
!!                    'LT+S'     Correct for one-way light time and
!!                               stellar aberration using a Newtonian
!!                               formulation. This option modifies the
!!                               position obtained with the 'LT' option
!!                               to account for the observer's velocity
!!                               relative to the solar system
!!                               barycenter. The result is the apparent
!!                               position of the target---the position
!!                               of the target as seen by the observer.
!!
!!                    'CN'       Converged Newtonian light time
!!                               correction. In solving the light time
!!                               equation, the 'CN' correction iterates
!!                               until the solution converges (three
!!                               iterations on all supported platforms).
!!                               Whether the 'CN+S' solution is
!!                               substantially more accurate than the
!!                               'LT' solution depends on the geometry
!!                               of the participating objects and on the
!!                               accuracy of the input data. In all
!!                               cases this routine will execute more
!!                               slowly when a converged solution is
!!                               computed. See the Particulars section
!!                               of SPKEZR for a discussion of precision
!!                               of light time corrections.
!!
!!                    'CN+S'     Converged Newtonian light time
!!                               correction and stellar aberration
!!                               correction.
!!
!!                 The following values of ABCORR apply to the
!!                 "transmission" case in which photons *depart* from
!!                 the observer's location at ET and arrive at the
!!                 target's location at the light-time corrected epoch
!!                 ET+LT:
!!
!!                    'XLT'      "Transmission" case:  correct for
!!                               one-way light time using a Newtonian
!!                               formulation. This correction yields the
!!                               position of the target at the moment it
!!                               receives photons emitted from the
!!                               observer's location at ET.
!!
!!                    'XLT+S'    "Transmission" case:  correct for
!!                               one-way light time and stellar
!!                               aberration using a Newtonian
!!                               formulation  This option modifies the
!!                               position obtained with the 'XLT' option
!!                               to account for the observer's velocity
!!                               relative to the solar system
!!                               barycenter. The target position
!!                               indicates the direction that photons
!!                               emitted from the observer's location
!!                               must be "aimed" to hit the target.
!!
!!                    'XCN'      "Transmission" case:  converged
!!                               Newtonian light time correction.
!!
!!                    'XCN+S'    "Transmission" case:  converged
!!                               Newtonian light time correction and
!!                               stellar aberration correction.
!!
!!                 Neither special nor general relativistic effects are
!!                 accounted for in the aberration corrections applied
!!                 by this routine.
!!
!!                 Case and blanks are not significant in the string
!!                 ABCORR.
!!
!! ## Detailed Output
!!
!!     PTARG       is a Cartesian 3-vector representing the position of
!!                 the target body relative to the specified observer.
!!                 PTARG is corrected for the specified aberrations, and
!!                 is expressed with respect to the specified inertial
!!                 reference frame.  The components of PTARG represent
!!                 the x-, y- and z-components of the target's position.
!!
!!                 The vector PTARG points from the observer's position
!!                 at ET to the aberration-corrected location of the
!!                 target. Note that the sense of the position vector is
!!                 independent of the direction of radiation travel
!!                 implied by the aberration correction.
!!
!!                 Units are always km.
!!
!!     LT          is the one-way light time between the observer and
!!                 target in seconds.  If the target position is
!!                 corrected for aberrations, then LT is the one-way
!!                 light time between the observer and the light time
!!                 corrected target location.
!!
!! ## Parameters
!!
!!     None.
!!
!! ## Exceptions
!!
!!     1) If the value of ABCORR is not recognized, the error
!!        'SPICE(SPKINVALIDOPTION)' is signaled.
!!
!!     2) If the reference frame requested is not a recognized
!!        inertial reference frame the error 'SPICE(BADFRAME)' is
!!        signaled.
!!
!!     3) If the position of the target relative to the solar system
!!        barycenter cannot be computed, the error will be diagnosed
!!        by routines in the call tree of this routine.
!!
!! ## Files
!!
!!     This routine computes positions using SPK files that have been
!!     loaded into the SPICE system, normally via the kernel loading
!!     interface routine FURNSH.  Application programs typically load
!!     kernels once before this routine is called, for example during
!!     program initialization; kernels need not be loaded repeatedly.
!!     See the routine FURNSH and the SPK and KERNEL Required Reading
!!     for further information on loading (and unloading) kernels.
!!
!!     If any of the ephemeris data used to compute PTARG are expressed
!!     relative to a non-inertial frame in the SPK files providing those
!!     data, additional kernels may be needed to enable the reference
!!     frame transformations required to compute PTARG.  Normally
!!     these additional kernels are PCK files or frame kernels.  Any
!!     such kernels must already be loaded at the time this routine is
!!     called.
!!
!! ## Particulars
!!
!!     In space science or engineering applications one frequently
!!     wishes to know where to point a remote sensing instrument, such
!!     as an optical camera or radio antenna, in order to observe or
!!     otherwise receive radiation from a target.  This pointing problem
!!     is complicated by the finite speed of light:  one needs to point
!!     to where the target appears to be as opposed to where it actually
!!     is at the epoch of observation.  We use the adjectives
!!     "geometric," "uncorrected," or "true" to refer to an actual
!!     position or state of a target at a specified epoch.  When a
!!     geometric position or state vector is modified to reflect how it
!!     appears to an observer, we describe that vector by any of the
!!     terms "apparent," "corrected," "aberration corrected," or "light
!!     time and stellar aberration corrected."
!!
!!     The SPICE Toolkit can correct for two phenomena affecting the
!!     apparent location of an object:  one-way light time (also called
!!     "planetary aberration") and stellar aberration.  Correcting for
!!     one-way light time is done by computing, given an observer and
!!     observation epoch, where a target was when the observed photons
!!     departed the target's location.  The vector from the observer to
!!     this computed target location is called a "light time corrected"
!!     vector.  The light time correction depends on the motion of the
!!     target, but it is independent of the velocity of the observer
!!     relative to the solar system barycenter. Relativistic effects
!!     such as light bending and gravitational delay are not accounted
!!     for in the light time correction performed by this routine.
!!
!!     The velocity of the observer also affects the apparent location
!!     of a target:  photons arriving at the observer are subject to a
!!     "raindrop effect" whereby their velocity relative to the observer
!!     is, using a Newtonian approximation, the photons' velocity
!!     relative to the solar system barycenter minus the velocity of the
!!     observer relative to the solar system barycenter.  This effect is
!!     called "stellar aberration."  Stellar aberration is independent
!!     of the motion of the target.  The stellar aberration formula used
!!     by this routine is non- relativistic.
!!
!!     Stellar aberration corrections are applied after light time
!!     corrections:  the light time corrected target position vector is
!!     used as an input to the stellar aberration correction.
!!
!!     When light time and stellar aberration corrections are both
!!     applied to a geometric position vector, the resulting position
!!     vector indicates where the target "appears to be" from the
!!     observer's location.
!!
!!     As opposed to computing the apparent position of a target, one
!!     may wish to compute the pointing direction required for
!!     transmission of photons to the target.  This requires correction
!!     of the geometric target position for the effects of light time and
!!     stellar aberration, but in this case the corrections are computed
!!     for radiation traveling from the observer to the target.
!!
!!     The "transmission" light time correction yields the target's
!!     location as it will be when photons emitted from the observer's
!!     location at ET arrive at the target.  The transmission stellar
!!     aberration correction is the inverse of the traditional stellar
!!     aberration correction:  it indicates the direction in which
!!     radiation should be emitted so that, using a Newtonian
!!     approximation, the sum of the velocity of the radiation relative
!!     to the observer and of the observer's velocity, relative to the
!!     solar system barycenter, yields a velocity vector that points in
!!     the direction of the light time corrected position of the target.
!!
!!     The traditional aberration corrections applicable to observation
!!     and those applicable to transmission are related in a simple way:
!!     one may picture the geometry of the "transmission" case by
!!     imagining the "observation" case running in reverse time order,
!!     and vice versa.
!!
!!     One may reasonably object to using the term "observer" in the
!!     transmission case, in which radiation is emitted from the
!!     observer's location.  The terminology was retained for
!!     consistency with earlier documentation.
!!
!!     Below, we indicate the aberration corrections to use for some
!!     common applications:
!!
!!        1) Find the apparent direction of a target for a remote-sensing
!!           observation.
!!
!!              Use 'LT+S' or 'CN+S: apply both light time and stellar
!!              aberration corrections.
!!
!!           Note that using light time corrections alone ('LT' or 'CN')
!!           is generally not a good way to obtain an approximation to
!!           an apparent target vector: since light time and stellar
!!           aberration corrections often partially cancel each other,
!!           it may be more accurate to use no correction at all than to
!!           use light time alone.
!!
!!        2) Find the corrected pointing direction to radiate a signal
!!           to a target. This computation is often applicable for
!!           implementing communications sessions.
!!
!!              Use 'XLT+S' or 'XCN+S: apply both light time and stellar
!!              aberration corrections for transmission.
!!
!!        3) Compute the apparent position of a target body relative
!!           to a star or other distant object.
!!
!!              Use 'LT', 'CN', 'LT+S', or 'CN+S' as needed to match the
!!              correction applied to the position of the distant
!!              object. For example, if a star position is obtained from
!!              a catalog, the position vector may not be corrected for
!!              stellar aberration. In this case, to find the angular
!!              separation of the star and the limb of a planet, the
!!              vector from the observer to the planet should be
!!              corrected for light time but not stellar aberration.
!!
!!        4) Obtain an uncorrected state vector derived directly from
!!           data in an SPK file.
!!
!!              Use 'NONE'.
!!
!!        5) Use a geometric position vector as a low-accuracy estimate
!!           of the apparent position for an application where execution
!!           speed is critical:
!!
!!              Use 'NONE'.
!!
!!        6) While this routine cannot perform the relativistic
!!           aberration corrections required to compute positions
!!           with the highest possible accuracy, it can supply the
!!           geometric positions required as inputs to these
!!           computations:
!!
!!              Use 'NONE', then apply high-accuracy aberration
!!              corrections (not available in the SPICE Toolkit).
!!
!!     Below, we discuss in more detail how the aberration corrections
!!     applied by this routine are computed.
!!
!!     Geometric case
!!     ==============
!!
!!        SPKAPO begins by computing the geometric position T(ET) of the
!!        target body relative to the solar system barycenter (SSB).
!!        Subtracting the geometric position of the observer O(ET) gives
!!        the geometric position of the target body relative to the
!!        observer. The one-way light time, LT, is given by
!!
!!                  | T(ET) - O(ET) |
!!           LT = -------------------
!!                          c
!!
!!        The geometric relationship between the observer, target, and
!!        solar system barycenter is as shown:
!!
!!           SSB ---> O(ET)
!!            |      /
!!            |     /
!!            |    /
!!            |   /  T(ET) - O(ET)
!!            V  V
!!           T(ET)
!!
!!        The returned position vector is
!!
!!           T(ET) - O(ET)
!!
!!     Reception case
!!     ==============
!!
!!        When any of the options 'LT', 'CN', 'LT+S', 'CN+S' are
!!        selected, SPKAPO computes the position of the target body at
!!        epoch ET-LT, where LT is the one-way light time.  Let T(t)
!!        and O(t) represent the positions of the target and observer
!!        relative to the solar system barycenter at time t; then LT
!!        is the solution of the
!!        light-time equation
!!
!!                  | T(ET-LT) - O(ET) |
!!           LT = ------------------------                            (1)
!!                           c
!!
!!        The ratio
!!
!!            | T(ET) - O(ET) |
!!          ---------------------                                     (2)
!!                    c
!!
!!        is used as a first approximation to LT; inserting (2) into the
!!        RHS of the light-time equation (1) yields the "one-iteration"
!!        estimate of the one-way light time. Repeating the process
!!        until the estimates of LT converge yields the "converged
!!        Newtonian" light time estimate.
!!
!!        Subtracting the geometric position of the observer O(ET) gives
!!        the position of the target body relative to the observer:
!!        T(ET-LT) - O(ET).
!!
!!           SSB ---> O(ET)
!!            | \     |
!!            |  \    |
!!            |   \   | T(ET-LT) - O(ET)
!!            |    \  |
!!            V     V V
!!           T(ET)  T(ET-LT)
!!
!!        The light-time corrected position is the vector
!!
!!           T(ET-LT) - O(ET)
!!
!!        If correction for stellar aberration is requested, the target
!!        position is rotated toward the solar system barycenter-relative
!!        velocity vector of the observer. The magnitude of the rotation
!!        depends on the magnitude of the observer's velocity relative
!!        to the solar system barycenter and the angle between
!!        this velocity and the observer-target vector.  The rotation
!!        is computed as follows:
!!
!!           Let r be the light time corrected vector from the observer
!!           to the object, and v be the velocity of the observer with
!!           respect to the solar system barycenter. Let w be the angle
!!           between them. The aberration angle phi is given by
!!
!!              sin(phi) = v sin(w) / c
!!
!!           Let h be the vector given by the cross product
!!
!!              h = r X v
!!
!!           Rotate r by phi radians about h to obtain the apparent
!!           position of the object.
!!
!!     Transmission case
!!     ==================
!!
!!        When any of the options 'XLT', 'XCN', 'XLT+S', 'XCN+S' are
!!        selected, SPKAPO computes the position of the target body T at
!!        epoch ET+LT, where LT is the one-way light time.  LT is the
!!        solution of the light-time equation
!!
!!                  | T(ET+LT) - O(ET) |
!!           LT = ------------------------                            (3)
!!                            c
!!
!!        Subtracting the geometric position of the observer, O(ET),
!!        gives the position of the target body relative to the
!!        observer: T(ET-LT) - O(ET).
!!
!!                   SSB --> O(ET)
!!                  / |    *
!!                 /  |  *  T(ET+LT) - O(ET)
!!                /   |*
!!               /   *|
!!              V  V  V
!!          T(ET+LT)  T(ET)
!!
!!        The light-time corrected position is
!!
!!           T(ET+LT) - O(ET)
!!
!!        If correction for stellar aberration is requested, the target
!!        position is rotated away from the solar system barycenter-
!!        relative velocity vector of the observer.  The magnitude of the
!!        rotation depends on the magnitude of the velocity and the
!!        angle between the velocity and the observer-target vector.
!!        The rotation is computed as in the reception case, but the
!!        sign of the rotation angle is negated.
!!
!!     Neither special nor general relativistic effects are accounted
!!     for in the aberration corrections performed by this routine.
!!
!! ## Examples
!!
!!     In the following code fragment, SPKSSB and SPKAPO are used
!!     to display the position of Io (body 501) as seen from the
!!     Voyager 2 spacecraft (Body -32) at a series of epochs.
!!
!!     Normally, one would call the high-level reader SPKPOS to obtain
!!     position vectors.  The example below illustrates the interface
!!     of this routine, but is not intended as a recommendation on
!!     how to use the SPICE SPK subsystem.
!!
!!     The use of integer ID codes is necessitated by the low-level
!!     interface of this routine.
!!
!!        IO    = 501
!!        VGR2  = -32
!!
!!        DO WHILE ( EPOCH .LE. END )
!!
!!           CALL SPKSSB (  VGR2,  EPOCH, 'J2000', STVGR2  )
!!           CALL SPKAPO (  IO,    EPOCH, 'J2000', STVGR2,
!!       .                 'LT+S', STIO,   LT              )
!!
!!           CALL RECRAD (  STIO,  RANGE,  RA,     DEC     )
!!           WRITE (*,*)  RA * DPR(),  DEC * DPR()
!!
!!           EPOCH = EPOCH + DELTA
!!
!!        END DO
!!
!! ## Restrictions
!!
!!     1) The ephemeris files to be used by SPKAPO must be loaded
!!        (normally by the SPICELIB kernel loader FURNSH) before
!!        this routine is called.
!!
!!     2) Unlike most other SPK position computation routines, this
!!        routine requires that the input state be relative to an
!!        inertial reference frame.  Non-inertial frames are not
!!        supported by this routine.
!!
!!     3) In a future version of this routine, the implementation
!!        of the aberration corrections may be enhanced to improve
!!        accuracy.
!!
!! ## Literature References
!!
!!     SPK Required Reading.
!!
!! ## Author and Institution
!!
!!     N.J. Bachman    (JPL)
!!     H.A. Neilan     (JPL)
!!     B.V. Semenov    (JPL)
!!     W.L. Taber      (JPL)
!!     I.M. Underwood  (JPL)
!!
!! ## Version
!!
!!-    SPICELIB Version 2.3.0, 03-JUL-2014 (NJB) (BVS)
!!
!!        Discussion of light time corrections was updated. Assertions
!!        that converged light time corrections are unlikely to be
!!        useful were removed.
!!
!!     Last update was 21-SEP-2013 (BVS)
!!
!!        Updated to call LJUCRS instead of CMPRSS/UCASE.
!!
!!-    SPICELIB Version 2.2.0, 17-MAY-2010 (NJB)
!!
!!        Bug fix: routine now returns immediately after
!!        state lookup failure.
!!
!!-    SPICELIB Version 2.1.0, 31-AUG-2005 (NJB)
!!
!!        Updated to remove non-standard use of duplicate arguments
!!        in VSUB call.
!!
!!-    SPICELIB Version 2.0.1, 20-OCT-2003 (EDW)
!!
!!        Added mention that LT returns in seconds.
!!        Corrected spelling errors.
!!
!!-    SPICELIB Version 2.0.0, 18-DEC-2001 (NJB)
!!
!!        Updated to handle aberration corrections for transmission
!!        of radiation.  Formerly, only the reception case was
!!        supported.  The header was revised and expanded to explain
!!        the functionality of this routine in more detail.
!!
!!-    SPICELIB Version 1.0.0, 03-MAR-1999 (WLT)
!!
!! ## Index Entries
!!
!!     apparent position from spk file
!!     get apparent position
!!
!! ## Revisions
!!
!!-    SPICELIB Version 1.1.0, 31-AUG-2005 (NJB)
!!
!!        Updated to remove non-standard use of duplicate arguments
!!        in VSUB call.
!!
!!     SPICELIB functions
!!
!!     Local parameters
!!
!!     Indices of flags in the FLAGS array:
!!
!!     NAIF ID code for the solar system barycenter:
!!
!!     Local variables
!!
!!     Saved variables
!!
!!     Initial values
!!
!!     Standard SPICE error handling.
!!
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: clight
      use pepper_arrays, only: isrchc
      use pepper_math, only: odd, vequ, vnorm, vsub
      use pepper_strings, only: ljucrs
      implicit none
      integer(ip), parameter :: corlen = 5
      integer(ip), parameter :: nflags = 9
      integer(ip), parameter :: ixnone = 1
      integer(ip), parameter :: ixlt = ixnone + 1
      integer(ip), parameter :: ixlts = ixlt   + 1
      integer(ip), parameter :: ixcn = ixlts  + 1
      integer(ip), parameter :: ixcns = ixcn   + 1
      integer(ip), parameter :: ixxlt = ixcns  + 1
      integer(ip), parameter :: ixxlts = ixxlt  + 1
      integer(ip), parameter :: ixxcn = ixxlts + 1
      integer(ip), parameter :: ixxcns = ixxcn  + 1
      integer(ip), parameter :: ssb = 0
      character(corlen)     corr
      real(wp)              tpos   ( 3 )
      integer(ip)           i
      integer(ip)           ltsign
      integer(ip)           maxitr
      integer(ip)           refid
      if ( .not. allocated ( ctx%zzspkpa0_state ) ) then
         allocate ( ctx%zzspkpa0_state )
         call init_zzspkpa0_state ( ctx%zzspkpa0_state )
      end if
      associate ( first => ctx%zzspkpa0_state%first, &
                  flags => ctx%zzspkpa0_state%flags, &
                  prvcor => ctx%zzspkpa0_state%prvcor, &
                  usecn => ctx%zzspkpa0_state%usecn, &
                  usestl => ctx%zzspkpa0_state%usestl, &
                  uselt => ctx%zzspkpa0_state%uselt, &
                  xmit => ctx%zzspkpa0_state%xmit )
      if ( ctx%failed() ) return

      if (  first  .or.  ( abcorr /= prvcor )  ) then
!
!        The aberration correction flag differs from the value it
!        had on the previous call, if any.  Analyze the new flag.
!
!        Remove leading and embedded white space from the aberration
!        correction flag, then convert to upper case.
!
         call ljucrs ( 0, abcorr, corr )

!
!        Locate the flag in our list of flags.
!
         i = isrchc ( corr, nflags, flags )

         if ( i == 0 ) then

            call ctx%setmsg ( 'Requested aberration correction was #.' )
            call ctx%errch  ( '#', abcorr                              )
            call ctx%sigerr ( 'SPICE(SPKINVALIDOPTION)'                )
            return

         end if

!
!        The aberration correction flag is recognized; save it.
!
         prvcor = abcorr

!
!        Set logical flags indicating the attributes of the requested
!        correction.
!
         xmit    =  i > ixcns

         uselt   =       ( i == ixlt  ) .or. ( i == ixlts  ) &
                    .or. ( i == ixxlt ) .or. ( i == ixxlts )

         usestl  =  ( i > 1 )  .and.  odd(i)

         usecn   =       ( i == ixcn  ) .or. ( i == ixcns  ) &
                    .or. ( i == ixxcn ) .or. ( i == ixxcns )

         first   = .false.

      end if

!
!     See if the reference frame is a recognized inertial frame.
!
      call irfnum ( ctx, ref, refid )

      if ( refid == 0 ) then

         call ctx%setmsg ( 'The requested frame ''#'' is not a ' &
         //            'recognized inertial frame. '        )
         call ctx%errch  ( '#', ref                             )
         call ctx%sigerr ( 'SPICE(BADFRAME)'                    )
         return

      end if

!
!     Determine the sign of the light time offset.
!
      if ( xmit ) then
         ltsign = 1
      else
         ltsign = -1
      end if

!
!     Find the geometric position of the target body with respect to the
!     solar system barycenter. Subtract the position of the observer
!     to get the relative position. Use this to compute the one-way
!     light time.
!
      call zzspkgp0 ( ctx, targ,   et,   ref, ssb, ptarg, lt )

      if ( ctx%failed() ) return

      call vsub   ( ptarg,  sobs,           tpos      )
      call vequ   ( tpos,   ptarg                     )
      lt = vnorm  ( ptarg ) / clight

!
!     To correct for light time, find the position of the target body
!     at the current epoch minus the one-way light time. Note that
!     the observer remains where he is.
!
      if ( uselt ) then
         maxitr = 1
      else if ( usecn ) then
         maxitr = 3
      else
         maxitr = 0
      end if

      do i = 1, maxitr

         call zzspkgp0 ( ctx, targ,   et + ltsign*lt, ref, ssb, ptarg, lt )

         if ( ctx%failed() ) return

         call vsub   ( ptarg,  sobs,     tpos )
         call vequ   ( tpos,   ptarg )
         lt = vnorm  ( ptarg ) / clight

      end do

!
!     At this point, PTARG contains the geometric or light-time
!     corrected position of the target relative to the observer,
!     depending on the specified correction.
!
!     If stellar aberration correction is requested, perform it now.
!
      if ( usestl ) then

         if ( xmit ) then
!
!           This is the transmission case.
!
!           Compute the position vector obtained by applying
!           "reception" stellar aberration to PTARG.
!
            call stlabx ( ctx, ptarg, sobs(4), tpos )
            call vequ   ( tpos,  ptarg )

         else
!
!           This is the reception case.
!
!           Compute the position vector obtained by applying
!           "reception" stellar aberration to PTARG.
!
            call stelab ( ctx, ptarg, sobs(4), tpos )
            call vequ   ( tpos,  ptarg )

         end if

      end if

      return
      end associate
      end procedure zzspkpa0

      module procedure zzspkzp0
!! ## Abstract
!!
!!     Return the position of a target body relative to an observing
!!     body, optionally corrected for light time (planetary aberration)
!!     and stellar aberration.
!!
!! ## Required Reading
!!
!!     SPK
!!     NAIF_IDS
!!     FRAMES
!!     TIME
!!
!! ## Keywords
!!
!!     EPHEMERIS
!!
!! ## Declarations
!!
!! ## Detailed Input
!!
!!     TARG        is the NAIF ID code for a target body.  The target
!!                 and observer define a position vector which points
!!                 from the observer to the target.
!!
!!     ET          is the ephemeris time, expressed as seconds past
!!                 J2000 TDB, at which the position of the target body
!!                 relative to the observer is to be computed.  ET
!!                 refers to time at the observer's location.
!!
!!     REF         is the name of the reference frame relative to which
!!                 the output position vector should be expressed. This
!!                 may be any frame supported by the SPICE system,
!!                 including built-in frames (documented in the Frames
!!                 Required Reading) and frames defined by a loaded
!!                 frame kernel (FK).
!!
!!                 When REF designates a non-inertial frame, the
!!                 orientation of the frame is evaluated at an epoch
!!                 dependent on the selected aberration correction. See
!!                 the description of the output position vector PTARG
!!                 for details.
!!
!!     ABCORR      indicates the aberration corrections to be applied to
!!                 the position of the target body to account for
!!                 one-way light time and stellar aberration.  See the
!!                 discussion in the Particulars section for
!!                 recommendations on how to choose aberration
!!                 corrections.
!!
!!                 ABCORR may be any of the following:
!!
!!                    'NONE'     Apply no correction. Return the
!!                               geometric position of the target body
!!                               relative to the observer.
!!
!!                 The following values of ABCORR apply to the
!!                 "reception" case in which photons depart from the
!!                 target's location at the light-time corrected epoch
!!                 ET-LT and *arrive* at the observer's location at ET:
!!
!!                    'LT'       Correct for one-way light time (also
!!                               called "planetary aberration") using a
!!                               Newtonian formulation. This correction
!!                               yields the position of the target at
!!                               the moment it emitted photons arriving
!!                               at the observer at ET.
!!
!!                               The light time correction uses an
!!                               iterative solution of the light time
!!                               equation (see Particulars for details).
!!                               The solution invoked by the 'LT' option
!!                               uses one iteration.
!!
!!                    'LT+S'     Correct for one-way light time and
!!                               stellar aberration using a Newtonian
!!                               formulation. This option modifies the
!!                               position obtained with the 'LT' option
!!                               to account for the observer's velocity
!!                               relative to the solar system
!!                               barycenter. The result is the apparent
!!                               position of the target---the position
!!                               as seen by the observer.
!!
!!                    'CN'       Converged Newtonian light time
!!                               correction. In solving the light time
!!                               equation, the 'CN' correction iterates
!!                               until the solution converges (three
!!                               iterations on all supported platforms).
!!                               Whether the 'CN+S' solution is
!!                               substantially more accurate than the
!!                               'LT' solution depends on the geometry
!!                               of the participating objects and on the
!!                               accuracy of the input data. In all
!!                               cases this routine will execute more
!!                               slowly when a converged solution is
!!                               computed. See the Particulars section
!!                               below for a discussion of precision of
!!                               light time corrections.
!!
!!                    'CN+S'     Converged Newtonian light time
!!                               correction and stellar aberration
!!                               correction.
!!
!!                 The following values of ABCORR apply to the
!!                 "transmission" case in which photons *depart* from
!!                 the observer's location at ET and arrive at the
!!                 target's location at the light-time corrected epoch
!!                 ET+LT:
!!
!!                    'XLT'      "Transmission" case:  correct for
!!                               one-way light time using a Newtonian
!!                               formulation. This correction yields the
!!                               position of the target at the moment it
!!                               receives photons emitted from the
!!                               observer's location at ET.
!!
!!                    'XLT+S'    "Transmission" case:  correct for
!!                               one-way light time and stellar
!!                               aberration using a Newtonian
!!                               formulation. This option modifies the
!!                               position obtained with the 'XLT' option
!!                               to account for the observer's velocity
!!                               relative to the solar system
!!                               barycenter. The computed target
!!                               position indicates the direction that
!!                               photons emitted from the observer's
!!                               location must be "aimed" to hit the
!!                               target.
!!
!!                    'XCN'      "Transmission" case:  converged
!!                               Newtonian light time correction.
!!
!!                    'XCN+S'    "Transmission" case:  converged
!!                               Newtonian light time correction and
!!                               stellar aberration correction.
!!
!!                 Neither special nor general relativistic effects are
!!                 accounted for in the aberration corrections applied
!!                 by this routine.
!!
!!                 Case and blanks are not significant in the string
!!                 ABCORR.
!!
!!     OBS         is the NAIF ID code for the observing body.
!!
!! ## Detailed Output
!!
!!     PTARG       is a Cartesian 3-vector representing the position of
!!                 the target body relative to the specified observer.
!!                 PTARG is corrected for the specified aberrations, and
!!                 is expressed with respect to the reference frame
!!                 specified by REF.  The three components of PTARG
!!                 represent the x-, y- and z-components of the target's
!!                 position.
!!
!!                 PTARG points from the observer's location at ET to
!!                 the aberration-corrected location of the target.
!!                 Note that the sense of this position vector is
!!                 independent of the direction of radiation travel
!!                 implied by the aberration correction.
!!
!!                 Units are always km.
!!
!!                 Non-inertial frames are treated as follows: letting
!!                 LTCENT be the one-way light time between the observer
!!                 and the central body associated with the frame, the
!!                 orientation of the frame is evaluated at ET-LTCENT,
!!                 ET+LTCENT, or ET depending on whether the requested
!!                 aberration correction is, respectively, for received
!!                 radiation, transmitted radiation, or is omitted.
!!                 LTCENT is computed using the method indicated by
!!                 ABCORR.
!!
!!     LT          is the one-way light time between the observer and
!!                 target in seconds.  If the target position is
!!                 corrected for aberrations, then LT is the one-way
!!                 light time between the observer and the light time
!!                 corrected target location.
!!
!! ## Parameters
!!
!!     None.
!!
!! ## Exceptions
!!
!!     1) If name of target or observer cannot be translated to its
!!        NAIF ID code, the error SPICE(IDCODENOTFOUND) is signaled.
!!
!!     2) If the reference frame REF is not a recognized reference
!!        frame the error 'SPICE(UNKNOWNFRAME)' is signaled.
!!
!!     3) If the loaded kernels provide insufficient data to
!!        compute the requested position vector, the deficiency will
!!        be diagnosed by a routine in the call tree of this routine.
!!
!!     4) If an error occurs while reading an SPK or other kernel file,
!!        the error  will be diagnosed by a routine in the call tree
!!        of this routine.
!!
!!     5) If any of the required attributes of the reference frame REF
!!        cannot be determined, 'SPICE(UNKNOWNFRAME2)' is signaled.
!!
!! ## Files
!!
!!     This routine computes positions using SPK files that have been
!!     loaded into the SPICE system, normally via the kernel loading
!!     interface routine FURNSH. See the routine FURNSH and the SPK
!!     and KERNEL Required Reading for further information on loading
!!     (and unloading) kernels.
!!
!!     If the output position PTARG is to be expressed relative to a
!!     non-inertial frame, or if any of the ephemeris data used to
!!     compute PTARG are expressed relative to a non-inertial frame in
!!     the SPK files providing those data, additional kernels may be
!!     needed to enable the reference frame transformations required to
!!     compute the position.  Normally these additional kernels are PCK
!!     files or frame kernels.  Any such kernels must already be loaded
!!     at the time this routine is called.
!!
!! ## Particulars
!!
!!     This routine is part of the user interface to the SPICE ephemeris
!!     system.  It allows you to retrieve position information for any
!!     ephemeris object relative to any other in a reference frame that
!!     is convenient for further computations.
!!
!!     Aberration corrections
!!     ======================
!!
!!     In space science or engineering applications one frequently
!!     wishes to know where to point a remote sensing instrument, such
!!     as an optical camera or radio antenna, in order to observe or
!!     otherwise receive radiation from a target.  This pointing problem
!!     is complicated by the finite speed of light:  one needs to point
!!     to where the target appears to be as opposed to where it actually
!!     is at the epoch of observation.  We use the adjectives
!!     "geometric," "uncorrected," or "true" to refer to an actual
!!     position or state of a target at a specified epoch.  When a
!!     geometric position or state vector is modified to reflect how it
!!     appears to an observer, we describe that vector by any of the
!!     terms "apparent," "corrected," "aberration corrected," or "light
!!     time and stellar aberration corrected." The SPICE Toolkit can
!!     correct for two phenomena affecting the apparent location of an
!!     object:  one-way light time (also called "planetary aberration")
!!     and stellar aberration.
!!
!!     One-way light time
!!     ------------------
!!
!!     Correcting for one-way light time is done by computing, given an
!!     observer and observation epoch, where a target was when the
!!     observed photons departed the target's location.  The vector from
!!     the observer to this computed target location is called a "light
!!     time corrected" vector.  The light time correction depends on the
!!     motion of the target relative to the solar system barycenter, but
!!     it is independent of the velocity of the observer relative to the
!!     solar system barycenter. Relativistic effects such as light
!!     bending and gravitational delay are not accounted for in the
!!     light time correction performed by this routine.
!!
!!     Stellar aberration
!!     ------------------
!!
!!     The velocity of the observer also affects the apparent location
!!     of a target:  photons arriving at the observer are subject to a
!!     "raindrop effect" whereby their velocity relative to the observer
!!     is, using a Newtonian approximation, the photons' velocity
!!     relative to the solar system barycenter minus the velocity of the
!!     observer relative to the solar system barycenter.  This effect is
!!     called "stellar aberration."  Stellar aberration is independent
!!     of the velocity of the target.  The stellar aberration formula
!!     used by this routine does not include (the much smaller)
!!     relativistic effects.
!!
!!     Stellar aberration corrections are applied after light time
!!     corrections:  the light time corrected target position vector is
!!     used as an input to the stellar aberration correction.
!!
!!     When light time and stellar aberration corrections are both
!!     applied to a geometric position vector, the resulting position
!!     vector indicates where the target "appears to be" from the
!!     observer's location.
!!
!!     As opposed to computing the apparent position of a target, one
!!     may wish to compute the pointing direction required for
!!     transmission of photons to the target.  This also requires
!!     correction of the geometric target position for the effects of
!!     light time and stellar aberration, but in this case the
!!     corrections are computed for radiation traveling *from* the
!!     observer to the target.
!!
!!     The "transmission" light time correction yields the target's
!!     location as it will be when photons emitted from the observer's
!!     location at ET arrive at the target.  The transmission stellar
!!     aberration correction is the inverse of the traditional stellar
!!     aberration correction:  it indicates the direction in which
!!     radiation should be emitted so that, using a Newtonian
!!     approximation, the sum of the velocity of the radiation relative
!!     to the observer and of the observer's velocity, relative to the
!!     solar system barycenter, yields a velocity vector that points in
!!     the direction of the light time corrected position of the target.
!!
!!     One may object to using the term "observer" in the transmission
!!     case, in which radiation is emitted from the observer's location.
!!     The terminology was retained for consistency with earlier
!!     documentation.
!!
!!     Below, we indicate the aberration corrections to use for some
!!     common applications:
!!
!!        1) Find the apparent direction of a target for a remote-sensing
!!           observation.
!!
!!              Use 'LT+S' or 'CN+S: apply both light time and stellar
!!              aberration corrections.
!!
!!           Note that using light time corrections alone ('LT' or 'CN')
!!           is generally not a good way to obtain an approximation to
!!           an apparent target vector: since light time and stellar
!!           aberration corrections often partially cancel each other,
!!           it may be more accurate to use no correction at all than to
!!           use light time alone.
!!
!!        2) Find the corrected pointing direction to radiate a signal
!!           to a target. This computation is often applicable for
!!           implementing communications sessions.
!!
!!              Use 'XLT+S' or 'XCN+S: apply both light time and stellar
!!              aberration corrections for transmission.
!!
!!        3) Compute the apparent position of a target body relative
!!           to a star or other distant object.
!!
!!              Use 'LT', 'CN', 'LT+S', or 'CN+S' as needed to match the
!!              correction applied to the position of the distant
!!              object. For example, if a star position is obtained from
!!              a catalog, the position vector may not be corrected for
!!              stellar aberration. In this case, to find the angular
!!              separation of the star and the limb of a planet, the
!!              vector from the observer to the planet should be
!!              corrected for light time but not stellar aberration.
!!
!!        4) Obtain an uncorrected position vector derived directly from
!!           data in an SPK file.
!!
!!              Use 'NONE'.
!!
!!        5) Use a geometric position vector as a low-accuracy estimate
!!           of the apparent position for an application where execution
!!           speed is critical.
!!
!!              Use 'NONE'.
!!
!!        6) While this routine cannot perform the relativistic
!!           aberration corrections required to compute positions
!!           with the highest possible accuracy, it can supply the
!!           geometric positions required as inputs to these
!!           computations.
!!
!!              Use 'NONE', then apply high-accuracy aberration
!!              corrections (not available in the SPICE Toolkit).
!!
!!     Below, we discuss in more detail how the aberration corrections
!!     applied by this routine are computed.
!!
!!        Geometric case
!!        ==============
!!
!!        SPKEZP begins by computing the geometric position T(ET) of the
!!        target body relative to the solar system barycenter (SSB).
!!        Subtracting the geometric position of the observer O(ET) gives
!!        the geometric position of the target body relative to the
!!        observer. The one-way light time, LT, is given by
!!
!!                  | T(ET) - O(ET) |
!!           LT = -------------------
!!                          c
!!
!!        The geometric relationship between the observer, target, and
!!        solar system barycenter is as shown:
!!
!!           SSB ---> O(ET)
!!            |      /
!!            |     /
!!            |    /
!!            |   /  T(ET) - O(ET)
!!            V  V
!!           T(ET)
!!
!!        The returned position vector is
!!
!!           T(ET) - O(ET)
!!
!!        Reception case
!!        ==============
!!
!!        When any of the options 'LT', 'CN', 'LT+S', 'CN+S' is selected
!!        for ABCORR, SPKEZP computes the position of the target body at
!!        epoch ET-LT, where LT is the one-way light time.  Let T(t) and
!!        O(t) represent the positions of the target and observer
!!        relative to the solar system barycenter at time t; then LT is
!!        the solution of the light-time equation
!!
!!                  | T(ET-LT) - O(ET) |
!!           LT = ------------------------                            (1)
!!                           c
!!
!!        The ratio
!!
!!            | T(ET) - O(ET) |
!!          ---------------------                                     (2)
!!                    c
!!
!!        is used as a first approximation to LT; inserting (2) into the
!!        right hand side of the light-time equation (1) yields the
!!        "one-iteration" estimate of the one-way light time ("LT").
!!        Repeating the process until the estimates of LT converge
!!        yields the "converged Newtonian" light time estimate ("CN").
!!
!!        Subtracting the geometric position of the observer O(ET) gives
!!        the position of the target body relative to the observer:
!!        T(ET-LT) - O(ET).
!!
!!           SSB ---> O(ET)
!!            | \     |
!!            |  \    |
!!            |   \   | T(ET-LT) - O(ET)
!!            |    \  |
!!            V     V V
!!           T(ET)  T(ET-LT)
!!
!!        The light time corrected position vector is
!!
!!           T(ET-LT) - O(ET)
!!
!!        If correction for stellar aberration is requested, the target
!!        position is rotated toward the solar system barycenter-
!!        relative velocity vector of the observer.  The rotation is
!!        computed as follows:
!!
!!           Let r be the light time corrected vector from the observer
!!           to the object, and v be the velocity of the observer with
!!           respect to the solar system barycenter. Let w be the angle
!!           between them. The aberration angle phi is given by
!!
!!              sin(phi) = v sin(w) / c
!!
!!           Let h be the vector given by the cross product
!!
!!              h = r X v
!!
!!           Rotate r by phi radians about h to obtain the apparent
!!           position of the object.
!!
!!        Transmission case
!!        ==================
!!
!!        When any of the options 'XLT', 'XCN', 'XLT+S', 'XCN+S' is
!!        selected, SPKEZP computes the position of the target body T at
!!        epoch ET+LT, where LT is the one-way light time.  LT is the
!!        solution of the light-time equation
!!
!!                  | T(ET+LT) - O(ET) |
!!           LT = ------------------------                            (3)
!!                            c
!!
!!        Subtracting the geometric position of the observer, O(ET),
!!        gives the position of the target body relative to the
!!        observer: T(ET-LT) - O(ET).
!!
!!                   SSB --> O(ET)
!!                  / |    *
!!                 /  |  *  T(ET+LT) - O(ET)
!!                /   |*
!!               /   *|
!!              V  V  V
!!          T(ET+LT)  T(ET)
!!
!!        The light-time corrected position vector is
!!
!!           T(ET+LT) - O(ET)
!!
!!        If correction for stellar aberration is requested, the target
!!        position is rotated away from the solar system barycenter-
!!        relative velocity vector of the observer. The rotation is
!!        computed as in the reception case, but the sign of the
!!        rotation angle is negated.
!!
!!     Precision of light time corrections
!!     ===================================
!!
!!        Corrections using one iteration of the light time solution
!!        ----------------------------------------------------------
!!
!!        When the requested aberration correction is 'LT', 'LT+S',
!!        'XLT', or 'XLT+S', only one iteration is performed in the
!!        algorithm used to compute LT.
!!
!!        The relative error in this computation
!!
!!           | LT_ACTUAL - LT_COMPUTED |  /  LT_ACTUAL
!!
!!        is at most
!!
!!            (V/C)**2
!!           ----------
!!            1 - (V/C)
!!
!!        which is well approximated by (V/C)**2, where V is the
!!        velocity of the target relative to an inertial frame and C is
!!        the speed of light.
!!
!!        For nearly all objects in the solar system V is less than 60
!!        km/sec. The value of C is ~300000 km/sec. Thus the
!!        one-iteration solution for LT has a potential relative error
!!        of not more than 4e-8. This is a potential light time error of
!!        approximately 2e-5 seconds per astronomical unit of distance
!!        separating the observer and target. Given the bound on V cited
!!        above:
!!
!!           As long as the observer and target are separated by less
!!           than 50 astronomical units, the error in the light time
!!           returned using the one-iteration light time corrections is
!!           less than 1 millisecond.
!!
!!           The magnitude of the corresponding position error, given
!!           the above assumptions, may be as large as (V/C)**2 * the
!!           distance between the observer and the uncorrected target
!!           position: 300 km or equivalently 6 km/AU.
!!
!!        In practice, the difference between positions obtained using
!!        one-iteration and converged light time is usually much smaller
!!        than the value computed above and can be insignificant. For
!!        example, for the spacecraft Mars Reconnaissance Orbiter and
!!        Mars Express, the position error for the one-iteration light
!!        time correction, applied to the spacecraft-to-Mars center
!!        vector, is at the 1 cm level.
!!
!!        Comparison of results obtained using the one-iteration and
!!        converged light time solutions is recommended when adequacy of
!!        the one-iteration solution is in doubt.
!!
!!        Converged corrections
!!        ---------------------
!!
!!        When the requested aberration correction is 'CN', 'CN+S',
!!        'XCN', or 'XCN+S', as many iterations as are required for
!!        convergence are performed in the computation of LT. Usually
!!        the solution is found after three iterations. The relative
!!        error present in this case is at most
!!
!!            (V/C)**4
!!           ----------
!!            1 - (V/C)
!!
!!        which is well approximated by (V/C)**4.
!!
!!           The precision of this computation (ignoring round-off
!!           error) is better than 4e-11 seconds for any pair of objects
!!           less than 50 AU apart, and having speed relative to the
!!           solar system barycenter less than 60 km/s.
!!
!!           The magnitude of the corresponding position error, given
!!           the above assumptions, may be as large as (V/C)**4 * the
!!           distance between the observer and the uncorrected target
!!           position: 1.2 cm at 50 AU or equivalently 0.24 mm/AU.
!!
!!        However, to very accurately model the light time between
!!        target and observer one must take into account effects due to
!!        general relativity. These may be as high as a few hundredths
!!        of a millisecond for some objects.
!!
!!     Relativistic Corrections
!!     =========================
!!
!!     This routine does not attempt to perform either general or
!!     special relativistic corrections in computing the various
!!     aberration corrections.  For many applications relativistic
!!     corrections are not worth the expense of added computation
!!     cycles.  If however, your application requires these additional
!!     corrections we suggest you consult the astronomical almanac (page
!!     B36) for a discussion of how to carry out these corrections.
!!
!! ## Examples
!!
!!     1)  Load a planetary ephemeris SPK, then look up a series of
!!         geometric positions of the moon relative to the earth,
!!         referenced to the J2000 frame.
!!
!!               IMPLICIT NONE
!!         C
!!         C     Local constants
!!         C
!!               CHARACTER*(*)         FRAME
!!               PARAMETER           ( FRAME  = 'J2000' )
!!
!!               CHARACTER*(*)         ABCORR
!!               PARAMETER           ( ABCORR = 'NONE' )
!!
!!         C
!!         C     The name of the SPK file shown here is fictitious;
!!         C     you must supply the name of an SPK file available
!!         C     on your own computer system.
!!         C
!!               CHARACTER*(*)         SPK
!!               PARAMETER           ( SPK    = 'planet.bsp' )
!!
!!         C
!!         C     ET0 represents the date 2000 Jan 1 12:00:00 TDB.
!!         C
!!               DOUBLE PRECISION      ET0
!!               PARAMETER           ( ET0    = 0.0D0 )
!!
!!         C
!!         C     Use a time step of 1 hour; look up 100 positions.
!!         C
!!               DOUBLE PRECISION      STEP
!!               PARAMETER           ( STEP   = 3600.0D0 )
!!
!!               INTEGER               MAXITR
!!               PARAMETER           ( MAXITR = 100 )
!!
!!         C
!!         C     The NAIF IDs of the earth and moon are 399 and 301
!!         C     respectively.
!!         C
!!               INTEGER               OBSRVR
!!               PARAMETER           ( OBSRVR = 399 )
!!
!!               INTEGER               TARGET
!!               PARAMETER           ( TARGET = 301 )
!!
!!         C
!!         C     Local variables
!!         C
!!               DOUBLE PRECISION      ET
!!               DOUBLE PRECISION      LT
!!               DOUBLE PRECISION      POS ( 3 )
!!
!!               INTEGER               I
!!
!!         C
!!         C     Load the SPK file.
!!         C
!!               CALL FURNSH ( SPK )
!!
!!         C
!!         C     Step through a series of epochs, looking up a
!!         C     position vector at each one.
!!         C
!!               DO I = 1, MAXITR
!!
!!                  ET = ET0 + (I-1)*STEP
!!
!!                  CALL SPKEZP ( TARGET, ET, FRAME, ABCORR, OBSRVR,
!!              .                 POS,    LT                        )
!!
!!                  WRITE (*,*) 'ET = ', ET
!!                  WRITE (*,*) 'J2000 x-position (km):   ', POS(1)
!!                  WRITE (*,*) 'J2000 y-position (km):   ', POS(2)
!!                  WRITE (*,*) 'J2000 z-position (km):   ', POS(3)
!!                  WRITE (*,*) ' '
!!
!!               END DO
!!
!!               END
!!
!! ## Restrictions
!!
!!     None.
!!
!! ## Literature References
!!
!!     SPK Required Reading.
!!
!! ## Author and Institution
!!
!!     C.H. Acton      (JPL)
!!     B.V. Semenov    (JPL)
!!     N.J. Bachman    (JPL)
!!     W.L. Taber      (JPL)
!!
!! ## Version
!!
!!-    SPICELIB Version 3.2.0, 03-JUL-2014 (NJB) (BVS)
!!
!!        Discussion of light time corrections was updated. Assertions
!!        that converged light time corrections are unlikely to be
!!        useful were removed.
!!
!!     Last update was 23-SEP-2013 (BVS)
!!
!!        Bug fix: added a check and an exception for the FOUND flag
!!        returned by FRINFO.
!!
!!        Updated to save the input frame name and POOL state counter
!!        and to do frame name-ID conversion only if the counter has
!!        changed.
!!
!!-    SPICELIB Version 3.1.1, 04-APR-2008 (NJB)
!!
!!        Corrected minor error in description of XLT+S aberration
!!        correction.
!!
!!-    SPICELIB Version 3.1.0, 06-JAN-2005 (NJB)
!!
!!        Tests of routine FAILED() were added.
!!
!!-    SPICELIB Version 3.0.3, 12-DEC-2004 (NJB)
!!
!!        Minor header error was corrected.
!!
!!-    SPICELIB Version 3.0.2, 20-OCT-2003 (EDW)
!!
!!        Added mention that LT returns in seconds.
!!
!!-    SPICELIB Version 3.0.1, 29-JUL-2003 (NJB) (CHA)
!!
!!        Various minor header changes were made to improve clarity.
!!
!!-    SPICELIB Version 3.0.0, 31-DEC-2001 (NJB)
!!
!!        Updated to handle aberration corrections for transmission
!!        of radiation.  Formerly, only the reception case was
!!        supported.  The header was revised and expanded to explain
!!        the functionality of this routine in more detail.
!!
!!-    SPICELIB Version 1.0.0, 03-MAR-1999 (WLT)
!!
!! ## Index Entries
!!
!!     using body names get position relative to an observer
!!     get position relative observer corrected for aberrations
!!     read ephemeris data
!!     read trajectory data
!!
!! ## Revisions
!!
!!-    SPICELIB Version 4.1.0, 05-JAN-2005 (NJB)
!!
!!        Tests of routine FAILED() were added.  The new checks
!!        are intended to prevent arithmetic operations from
!!        being performed with uninitialized or invalid data.
!!
!!     SPICELIB functions
!!
!!     Local parameters
!!
!!     Saved frame name length.
!!
!!     Local variables
!!
!!     Saved frame name/ID item declarations.
!!
!!     Saved variables
!!
!!     Initial values
!!
!!     Standard SPICE error handling.
!!
      use pepper_kinds, only: ip, wp
      use pepper_math, only: mxv
      use pepper_strings, only: eqchr, eqstr, ltrim
      use pepper_inc_frmtyp
      use pepper_inc_zzctr
      implicit none
      character(*), parameter :: rname = 'ZZSPKZP0'
      integer(ip), parameter :: frnmln = 32
      real(wp) :: ltcent
      real(wp) :: temp     ( 3    )
      logical :: xmit
      logical :: found
      integer(ip) :: i
      real(wp) :: sobs     ( 6    )
      real(wp) :: xform    ( 3, 3 )
      real(wp) :: postn    ( 3    )
      integer(ip) :: type
      integer(ip) :: typeid
      integer(ip) :: reqfrm
      integer(ip) :: center
      if ( .not. allocated ( ctx%zzspkzp0_state ) ) then
         allocate ( ctx%zzspkzp0_state )
         call init_zzspkzp0_state ( ctx%zzspkzp0_state )
      end if
      associate ( fj2000 => ctx%zzspkzp0_state%fj2000, &
                  first => ctx%zzspkzp0_state%first, &
                  svctr1 => ctx%zzspkzp0_state%svctr1, &
                  svref => ctx%zzspkzp0_state%svref, &
                  svreqf => ctx%zzspkzp0_state%svreqf )
      if ( ctx%failed() ) return

!
!     Get the frame id for J2000 on the first call to this routine.
!
      if ( first ) then

         call namfrm( ctx, 'J2000', fj2000 )

!
!        Initialize counter.
!
         call zzctruin( ctx, svctr1 )

         first = .false.

      end if

!
!     Decide whether the aberration correction is for received or
!     transmitted radiation.
!
      i    = ltrim(abcorr)
      xmit = eqchr ( abcorr(i:i),  'X' )

!
!     If we only want geometric positions, then compute just that.
!
!     Otherwise, compute the state of the observer relative to
!     the SSB.  Then feed that position into SPKAPO to compute the
!     apparent position of the target body relative to the observer
!     with the requested aberration corrections.
!
      if ( eqstr( abcorr, 'NONE' ) ) then

         call zzspkgp0 ( ctx, targ, et, ref, obs, ptarg, lt )

      else
!
!        Get the auxiliary information about the requested output
!        frame.
!
         call zznamfrm ( ctx, svctr1, svref, svreqf, ref, reqfrm )

         if ( reqfrm == 0 ) then
            call ctx%setmsg ( 'The requested output frame ''#'' is ' &
            //            'not recognized by the reference frame ' &
            //            'subsystem. Please check that the ' &
            //            'appropriate kernels have been loaded ' &
            //            'and that you have correctly entered ' &
            //            'the name of the output frame. ' )
            call ctx%errch  ( '#', ref )
            call ctx%sigerr ( 'SPICE(UNKNOWNFRAME)'  )
            return

         end if

         call frinfo ( ctx, reqfrm, center, type, typeid, found )

         if ( ctx%failed() ) return

         if ( .not. found ) then

            call ctx%setmsg ( 'The requested output frame ''#'' is ' &
            //            'not recognized by the reference frame ' &
            //            'subsystem. Please check that the ' &
            //            'appropriate kernels have been loaded ' &
            //            'and that you have correctly entered ' &
            //            'the name of the output frame. ' )
            call ctx%errch  ( '#',  ref )
            call ctx%sigerr ( 'SPICE(UNKNOWNFRAME2)' )

            return

         end if

!
!        If we are dealing with an inertial frame, we can simply
!        call SPKSSB, SPKAPO and return.
!
         if ( type == inertl ) then

            call zzspksb0 ( ctx, obs,  et, ref, sobs )
            call zzspkpa0 ( ctx, targ, et, ref, sobs, abcorr, ptarg, lt )
            return

         end if

!
!        Still here?
!
!        We are dealing with a non-inertial frame.  But we need to
!        do light time and stellar aberration in an inertial frame.
!        Get the "apparent" position of TARG in the intermediary
!        inertial reference frame J2000.
!
!        We also need the light time to the center of the frame.
!
         call zzspksb0 ( ctx, obs,  et, 'J2000', sobs )
         call zzspkpa0 ( ctx, targ, et, 'J2000', sobs, abcorr, postn, lt )

         if ( ctx%failed() ) return

         if ( center == obs ) then
            ltcent = 0.0_wp
         else if ( center == targ ) then
            ltcent = lt
         else
            call zzspkpa0 ( ctx, center, et, 'J2000', sobs, abcorr, temp, &
                            ltcent )
         end if

!
!        If something went wrong (like we couldn't get the position of
!        the center relative to the observer) now it is time to quit.
!
         if ( ctx%failed() ) return

!
!        If the aberration corrections are for transmission, negate
!        the light time, since we wish to compute the orientation
!        of the non-inertial frame at an epoch later than ET by
!        the one-way light time.
!
         if ( xmit ) then
            ltcent = -ltcent
         end if

!
!        Get the rotation from J2000 to the requested frame
!        and convert the position.
!
         call zzrefch0 ( ctx, fj2000, reqfrm, et-ltcent, xform )

         if ( ctx%failed() ) return

         call mxv    ( xform,  postn,             ptarg )

      end if

      return
      end associate
      end procedure zzspkzp0

      module procedure zzdynfrm
!! ## Abstract
!!
!!     SPICE Private routine intended solely for the support of SPICE
!!     routines.  Users should not call this routine directly due
!!     to the volatile nature of this routine.
!!
!!     For a specified dynamic frame, find the transformation
!!     that maps states from the dynamic frame to its base frame.
!!
!! ## Required Reading
!!
!!     CK
!!     FRAMES
!!     PCK
!!     SPK
!!
!! ## Keywords
!!
!!     FRAMES
!!     PRIVATE
!!
!! ## Declarations
!!
!! ## Detailed Input
!!
!!     INFRAM      is the frame ID code for a dynamic reference frame.
!!                 Note that this interface differs from that of TKFRAM,
!!                 which uses a class ID to identify the frame.
!!
!!                 In this routine, we refer this frame both as the
!!                 "input frame" and the "defined frame."
!!
!!     CENTER      is NAIF ID code for the center of the frame
!!                 designated by INFRAM.  This code, although derivable
!!                 from INFRAM, is passed in for convenience.
!!
!!     ET          is an epoch in ephemeris seconds past J2000 for which
!!                 the caller requests a state transformation matrix.
!!
!! ## Detailed Output
!!
!!     XFORM       is a 6x6 matrix that transforms states relative to
!!                 INFRAM to states relative to BASFRM.
!!
!!     BASFRM      is the frame ID code of the base frame associated
!!                 with INFRAM.  The 6x6 matrix XFORM transforms states
!!                 relative to INFRAM to states relative to BASFRM. The
!!                 state transformation is performed by left-multiplying
!!                 by XFORM a state expressed relative to INFRAM. This
!!                 is easily accomplished via the subroutine call shown
!!                 below.
!!
!!                    CALL MXVG ( XFORM, STATE, 6, 6, OSTATE )
!!
!! ## Parameters
!!
!!     See include file zzdyn.inc.
!!
!! ## Exceptions
!!
!!     1)  If a dynamic frame evaluation requires unavailable kernel
!!         data, the error will be diagnosed by routines in the call
!!         tree of this routine.
!!
!!     2)  If a precession model is used to implement a frame centered
!!         at a body for which the model is not applicable, the error
!!         SPICE(INVALIDSELECTION) will be signaled.
!!
!!     3)  If a nutation model is used to implement a frame centered
!!         at a body for which the model is not applicable, the error
!!         SPICE(INVALIDSELECTION) will be signaled.
!!
!!     4)  If an obliquity model is used to implement a frame centered
!!         at a body for which the model is not applicable, the error
!!         SPICE(INVALIDSELECTION) will be signaled.
!!
!!     5)  If an unrecognized precession model is specified, the
!!         error SPICE(NOTSUPPORTED) is signaled.
!!
!!     6)  If an unrecognized nutation model is specified, the
!!         error SPICE(NOTSUPPORTED) is signaled.
!!
!!     7)  If an unrecognized obliquity model is specified, the
!!         error SPICE(NOTSUPPORTED) is signaled.
!!
!!     8)  If an attempt to look up the center of a frame does
!!         not yield data, the error SPICE(FRAMEDATANOTFOUND) is
!!         signaled.
!!
!!     9)  In a two-vector frame definition, if a constant vector
!!         specification method is not recognized, the error
!!         SPICE(NOTSUPPORTED) is signaled.
!!
!!     10) In a two-vector frame definition, if a vector definition
!!         method is not recognized, the error SPICE(NOTSUPPORTED)
!!         is signaled.
!!
!!     11) If an unrecognized dynamic frame family is specified, the
!!          error SPICE(NOTSUPPORTED) is signaled.
!!
!!     12) If an unrecognized dynamic frame definition style is
!!         specified, the error SPICE(NOTSUPPORTED) is signaled.
!!
!!     13) If an unrecognized dynamic frame rotation state is
!!         specified, the error SPICE(NOTSUPPORTED) is signaled.
!!
!!     14) If both a freeze epoch and a rotation state are specified,
!!         the error SPICE(FRAMEDEFERROR) is signaled.
!!
!!     15) If neither a freeze epoch nor a rotation state are specified
!!         for an "of date" frame, the error SPICE(FRAMEDEFERROR) is
!!         signaled.
!!
!!     16) In a two-vector frame definition, if an invalid axis
!!         specification is encountered, the error SPICE(INVALIDAXIS) is
!!         signaled.
!!
!!     17) In a two-vector frame definition using a target near point
!!         vector, if the body-fixed frame associated with the target
!!         is not found, the error SPICE(FRAMEDATANOTFOUND) is signaled.
!!
!!     18) If the state of the near point on a target as seen from
!!         an observer cannot be computed, the error
!!         SPICE(DEGENERATECASE) is signaled.
!!
!!     19) If a dynamic frame evaluation requires excessive recursion
!!         depth, the error will be diagnosed by routines in the
!!         call tree of this routine.
!!
!!     20) When a two-vector dynamic frame is evaluated, if the
!!         primary and secondary vectors have angular separation less
!!         than the minimum allowed value, or if the angular separation
!!         differs from Pi by less than the minimum allowed value, the
!!         error SPICE(DEGENERATECASE) is signaled.  The default minimum
!!         separation is given by the parameter LBSEP; this value may be
!!         overridden by supplying a different value in the frame
!!         definition.
!!
!!     21) If invalid units occur in a frame definition, the error
!!         will be diagnosed by a routine in the call tree of this
!!         routine.
!!
!!     22) If an invalid Euler axis sequence occurs in a frame
!!         definition, the error will be diagnosed by a routine in the
!!         call tree of this routine.
!!
!!     23) If a body RADII vector has other than exactly thee elements,
!!         the error SPICE(INVALIDCOUNT) is signaled by a routine in
!!         the call tree of this routine.
!!
!!     24) If a body RADII vector has any element less-than or
!!         equal to zero, the error SPICE(BADAXISLENGTH) is signaled by
!!         a routine in the call tree of this routine.
!!
!! ## Files
!!
!!     1) SPK files containing data for each observer and target
!!        are required to support two-vector frames.  Note that
!!        observer-target pairs can be implicit, as in the case
!!        of a constant vector whose frame is evaluated at a
!!        light-time corrected epoch:  the light time the frame
!!        center to an observer must be computable in this case,
!!        which implies the state of the frame center as seen by
!!        the observer must be computable.
!!
!!     2) Any of SPK, CK, PCK, and frame kernels will also be required
!!        if any frames referenced in the definition of INFRAM (as a
!!        base frame, velocity vector frame, or constant vector frame)
!!        require them, or if any vectors used to define INFRAM require
!!        these data in order to be computable.
!!
!!     3) When CK data are required, one or more associated SCLK kernels
!!        ---normally, one kernel per spacecraft clock---are
!!        required as well.  A leapseconds kernel may be required
!!        whenever an SCLK kernel is required.
!!
!!     4) When a two-vector frame is defined using a target near point,
!!        a PCK file giving orientation and providing a triaxial shape
!!        model for the target body is required.
!!
!! ## Particulars
!!
!!     Currently only parameterized dynamic frames are supported by
!!     this routine.
!!
!!     Currently supported parameterized dynamic families are:
!!
!!        Two-vector
!!        ==========
!!
!!           Vector definitions
!!           ------------------
!!           Observer-target position
!!           Observer-target velocity
!!           Near point on target
!!           Constant vector in specified frame
!!
!!        Mean Equator and Equinox of Date
!!        ================================
!!
!!           Bodies and models
!!           -----------------
!!           Earth:  1976 IAU precession model
!!
!!        Mean Ecliptic and Equinox of Date
!!        ================================
!!
!!           Bodies and models
!!           -----------------
!!           Earth:  1976 IAU precession model
!!                   1980 IAU mean obliquity model
!!
!!        True Equator and Equinox of Date
!!        ================================
!!
!!           Bodies and models
!!           -----------------
!!           Earth:  1976 IAU precession model
!!                   1980 IAU nutation model
!!
!!        Euler frames
!!        ============
!!
!!           Euler angle definitions
!!           -----------------------
!!           Polynomial
!!
!!        Product frames
!!        ==============
!!
!!           Product definitions
!!           -------------------
!!           Array of "from" frames
!!           Array of "to" frames
!!
!! ## Examples
!!
!!     See FRMGET.
!!
!! ## Restrictions
!!
!!     1) This is a SPICE private routine; the routine is subject
!!        to change without notice.  User applications should not
!!        call this routine.
!!
!!     2) Many numerical problems can occur when dynamic frames
!!        are evaluated.  Users must determine whether dynamic frame
!!        definitions are suitable for their applications. See the
!!        Exceptions section for a list of possible problems.
!!
!!     3) Use of aberration corrections may lead to severe loss of
!!        accuracy in state transformation derivatives.
!!
!!     4) Two-vector frame definitions can suffer extreme loss of
!!         precision due to near-singular geometry.
!!
!!     5) Two-vector frame definitions involving velocity vectors
!!        require numerical differentiation in order to compute the
!!        derivative of the state transformation.  Such derivatives may
!!        have low accuracy.
!!
!! ## Literature References
!!
!!     None.
!!
!! ## Author and Institution
!!
!!     N.J. Bachman    (JPL)
!!
!! ## Version
!!
!!-    SPICELIB Version 2.0.0, 09-OCT-2021 (NJB) (EDW)
!!
!!        Support for product frames was added. Re-ordered header
!!        sections.
!!
!!        Body radii accessed from kernel pool using ZZGFTREB.
!!
!!        Bug fix: aberration correction strings for observer-target
!!        velocity vectors are now parsed using ZZVALCOR. This improves
!!        error checking. Note that aberration correction strings for
!!        constant vectors used in two-vector frame specifications are
!!        still parsed using ZZPRSCOR.
!!
!!        Bug fix: calls to FAILED after calls to ZZDYNVAC were added.
!!
!!        Added some FAILED() calls.
!!
!!-    SPICELIB Version 1.1.1, 02-MAR-2010 (NJB)
!!
!!        Typo in Brief_I/O section was corrected: "Class ID"
!!        was changed to "Frame ID." Corrected order of header
!!        sections.
!!
!!-    SPICELIB Version 1.1.0, 23-OCT-2005 (NJB)
!!
!!        Parameters KWX, KWY, KWZ were renamed to KVX, KVY, KVZ.
!!
!!        Call to ZZBODVCD was replaced with call to BODVCD.
!!
!!-    SPICELIB Version 1.0.0, 10-JAN-2005 (NJB)
!!
!!     SPICELIB functions
!!
!!     Local parameters
!!
!!     Local variables
!!
!!     Saved variables
!!
!!     Initial values
!!
      use pepper_kinds, only: ip, wp
      use pepper_arrays, only: cleard, isrchc, moved
      use pepper_math, only: latrec, mxmg, mxvg, polyds, vequ, vhat, vminug, vpack, vsep, vsubg
      use pepper_strings, only: cmprss, eqstr, intstr, ucase
      use pepper_inc_frmtyp
      use pepper_inc_zzabcorr
      use pepper_inc_zzdyn
      implicit none
      real(wp) :: epoch_1(1)  !! one-element copy of epoch (array argument)
      real(wp) :: minsep_1(1)  !! one-element copy of minsep (array argument)
      real(wp) :: dec_1(1)  !! one-element copy of dec (array argument)
      real(wp) :: ra_1(1)  !! one-element copy of ra (array argument)
      real(wp) :: lat_1(1)  !! one-element copy of lat (array argument)
      real(wp) :: lon_1(1)  !! one-element copy of lon (array argument)
      real(wp) :: t0_1(1)  !! one-element copy of t0 (array argument)
      character(*), parameter :: rname = 'ZZDYNFRM'
      integer(ip), parameter :: timlen = 50
      integer(ip), parameter :: vnamln = 4
      character(corlen)     abcorr
      character(kvlen)      axname
      character(frnmln)     basnam
      character(frnmln)     cfrmnm
      character(bdnmln)     ctrnam
      character(corlen)     cvcorr
      character(kvlen)      dynstl
      character(kvlen)      dynfam
      character(frnmln)     fframs ( mxnfac )
      character(frnmln)     inname
      character(kvlen)      nutmod
      character(kvlen)      oblmod
      character(kvlen)      prcmod
      character(kvlen)      rotsta
      character(kvlen)      spec
      character(frnmln)     tframs ( mxnfac )
      character(timlen)     timstr
      character(kvlen)      tmpfam
      character(kvlen)      units
      character(kvlen)      vecdef ( 2 )
      character(frnmln)     velfrm
      character(vnamln), parameter :: vname  ( 2 ) = &
         [ character(vnamln) :: kwpri, kwsec ]
      real(wp)              acc    ( 3 )
      real(wp)              angles ( 2 )
      real(wp)              coeffs ( maxcof, 3 )
      real(wp)              ctrpos ( 3 )
      real(wp)              dec
      real(wp)              delta
      real(wp)              dirvec ( 3 )
      real(wp)              dmob
      real(wp)              epoch
      real(wp)              eulang ( 6 )
      real(wp)              fet
      real(wp)              lat
      real(wp)              lon
      real(wp)              lt
      real(wp)              minsep
      real(wp)              mob
      real(wp)              nutxf  ( 6, 6 )
      real(wp)              oblxf  ( 6, 6 )
      real(wp)              poly   ( 0 : 1 )
      real(wp)              precxf ( 6, 6 )
      real(wp)              ra
      real(wp)              radii  ( 3 )
      real(wp)              s2     ( 6, 2 )
      real(wp)              sep
      real(wp)              stalt  ( 2 )
      real(wp)              stemp  ( 6 )
      real(wp)              stnear ( 6 )
      real(wp)              stobs  ( 6 )
      real(wp)              tarray ( 3 )
      real(wp)              t0
      real(wp)              varray ( 3, 3 )
      real(wp)              vet
      real(wp)              vflt
      real(wp)              xf2000 ( 6, 6 )
      real(wp)              xfinv  ( 6, 6 )
      real(wp)              xftemp ( 6, 6 )
      real(wp)              xipm   ( 6, 6 )
      real(wp)              xout   ( 6, 6 )
      integer(ip)           axis   ( 2 )
      integer(ip)           cfrmid
      integer(ip)           cvobs
      integer(ip)           degs   ( 3 )
      integer(ip)           frcid
      integer(ip)           frcls
      integer(ip)           frctr
      integer(ip)           frid
      integer(ip)           fromid
      integer(ip)           i
      integer(ip)           iaxes  ( 3 )
      integer(ip)           j
      integer(ip)           m
      integer(ip)           n
      integer(ip)           obs
      integer(ip)           targ
      integer(ip)           toid
      logical               corblk ( nabcor )
      logical               fnd
      logical               frozen
      logical               inert
      logical               meanec
      logical               meaneq
      logical               negate
      logical               ofdate
      logical               trueeq
      if ( .not. allocated ( ctx%zzdynfrm_state ) ) then
         allocate ( ctx%zzdynfrm_state )
         call init_zzdynfrm_state ( ctx%zzdynfrm_state )
      end if
      associate ( axes => ctx%zzdynfrm_state%axes, &
                  earth => ctx%zzdynfrm_state%earth, &
                  first => ctx%zzdynfrm_state%first, &
                  j2000 => ctx%zzdynfrm_state%j2000, &
                  itmabc => ctx%zzdynfrm_state%itmabc, &
                  itmaxe => ctx%zzdynfrm_state%itmaxe, &
                  itmcof => ctx%zzdynfrm_state%itmcof, &
                  itmdec => ctx%zzdynfrm_state%itmdec, &
                  itmfrm => ctx%zzdynfrm_state%itmfrm, &
                  itmlat => ctx%zzdynfrm_state%itmlat, &
                  itmlon => ctx%zzdynfrm_state%itmlon, &
                  itmobs => ctx%zzdynfrm_state%itmobs, &
                  itmra => ctx%zzdynfrm_state%itmra, &
                  itmsep => ctx%zzdynfrm_state%itmsep, &
                  itmspc => ctx%zzdynfrm_state%itmspc, &
                  itmtrg => ctx%zzdynfrm_state%itmtrg, &
                  itmunt => ctx%zzdynfrm_state%itmunt, &
                  itmvdf => ctx%zzdynfrm_state%itmvdf, &
                  itmvec => ctx%zzdynfrm_state%itmvec )
      if ( ctx%failed() ) return

      if ( first ) then
!
!        Get the ID code for the J2000 frame.
!
         call irfnum ( ctx, 'J2000', j2000 )

!
!        Get the ID code for the earth (we needn't check the found
!        flag).
!
         call bodn2c ( ctx, 'EARTH', earth, fnd )

!
!        Initialize "item" strings used to create kernel variable
!        names.
!
         do i = 1, 2
!
!           Vector axis:
!
            itmaxe(i) = vname(i) // kwvaxi
!
!           Vector definition:
!
            itmvdf(i) = vname(i) // kwvcdf
!
!           Vector aberration correction:
!
            itmabc(i) = vname(i) // kwvabc
!
!           Vector frame:
!
            itmfrm(i) = vname(i) // kwvfrm
!
!           Vector observer:
!
            itmobs(i) = vname(i) // kwvobs
!
!           Vector target:
!
            itmtrg(i) = vname(i) // kwvtrg
!
!           Vector longitude:
!
            itmlon(i) = vname(i) // kwlon
!
!           Vector latitude:
!
            itmlat(i) = vname(i) // kwlat
!
!           Vector right ascension:
!
            itmra(i)  = vname(i) // kwra
!
!           Vector declination:
!
            itmdec(i) = vname(i) // kwdec
!
!           Vector units:
!
            itmunt(i) = vname(i) // kwunit
!
!           Constant vector coordinate specification:
!
            itmspc(i) = vname(i) // kwvspc
!
!           Constant vector in Cartesian coordinates, literal value:
!
            itmvec(i) = vname(i) // kwvect

         end do

         first = .false.

      end if

!
!     Initialize the output arguments.
!
      call cleard ( 36, xform )

      basfrm = 0

!
!     Initialize certain variables to ensure that we don't do
!     arithmetic operations using bogus, possibly large,
!     undefined values.
!
      call cleard ( 36, nutxf  )
      call cleard ( 36, oblxf  )
      call cleard ( 36, precxf )
      call cleard ( 36, xf2000 )
      call cleard ( 36, xfinv  )
      call cleard ( 36, xipm   )

      mob    = 0.0_wp
      dmob   = 0.0_wp
      t0     = 0.0_wp
      frozen = .false.

!
!     Get the input frame name.
!
      call frmnam ( ctx, infram, inname )

!
!     We need the name of the base frame.
!
      call zzdynfid ( ctx, inname, infram, kwbfrm, basfrm )

      if ( ctx%failed() ) return

      call frmnam   ( ctx, basfrm, basnam )

!
!     The output frame code and name are set.
!
!     Look up the dynamic frame definition style from the kernel pool.
!     The kernel variable's name might be specified by name or ID.
!
      call zzdynvac ( ctx, inname, infram, kwstyl, 1, n, dynstl )

      if ( ctx%failed() ) return

!
!     At this time, the only supported dynamic frame definition style is
!     PARAMETERIZED.
!
      if (  eqstr( dynstl, kvparm )  ) then
!
!        Parameterized dynamic frames belong to families.  Look up
!        the family for this frame.
!
         call zzdynvac ( ctx, inname, infram, kwffam, 1, n, dynfam )

         if ( ctx%failed() ) return

         call cmprss ( ' ', 0, dynfam, tmpfam )
         call ucase  ( tmpfam,         dynfam )

!
!        Determine whether we have an "of-date" frame family.
!        The logical flags used here and respective meanings are:
!
!           MEANEQ   Mean equator and equinox of date
!           TRUEEQ   True equator and equinox of date
!           MEANEC   Mean ecliptic and equinox of date
!
         meaneq  =  dynfam == kvmeqt
         trueeq  =  dynfam == kvteqt
         meanec  =  dynfam == kvmecl

         ofdate  =  meaneq .or. meanec .or. trueeq

!
!        Set the evaluation epoch T0.  Normally this epoch is ET,
!        but if the frame is frozen, the freeze epoch from the
!        frame definition is used.
!
!        Read the freeze epoch into T0 if a freeze epoch was
!        specified; let FROZEN receive the FOUND flag value
!        returned by ZZDYNOAD.
!
         call zzdynoad ( ctx, inname, infram, kwfrez, 1, n, t0_1, frozen )
         t0 = t0_1(1)

         if ( ctx%failed() ) return

         if ( .not. frozen ) then
!
!           Normal case:  just use the input epoch.
!
            t0 = et

         end if

!
!        Look up the rotation state keyword.  Rather than checking
!        FAILED() after every call, we'll do it after we're
!        done with processing the rotation state.
!
         call zzdynoac ( ctx, inname, infram, kwrsta, 1, n, rotsta, fnd )

         if ( ctx%failed() ) return

!
!        If the frame is frozen, the rotation state keyword *must be
!        absent*.
!
         if ( frozen .and. fnd ) then

            call ctx%setmsg ( 'Definition of frame # contains both ' // &
                          '# and # '                             // &
                          'keywords; at most one of these must ' // &
                          'be present in the frame definition. ' // &
                          'This situation is usually caused by ' // &
                          'an error in a frame kernel in which ' // &
                          'the frame is defined.'                )
            call ctx%errch  ( '#',  inname                           )
            call ctx%errch  ( '#',  kwfrez                           )
            call ctx%errch  ( '#',  kwrsta                           )
            call ctx%sigerr ( 'SPICE(FRAMEDEFERROR)'                 )
            return

         end if

!
!        If the frame belongs to an "of date" family, either the
!        rotation state must be specified or the frame must be
!        frozen.
!
         if (  ofdate  .and.  (.not. frozen) .and. (.not. fnd )  ) then

            call ctx%setmsg ( 'Definition of frame #, which belongs to ' // &
                          'parameterized dynamic frame family #, '   // &
                          'contains neither # nor # keywords; '      // &
                          'frames in this family require exactly '   // &
                          'one of these in their frame definitions. '// &
                          'This situation is usually caused by '     // &
                          'an error in a frame kernel in which '     // &
                          'the frame is defined.'                    )
            call ctx%errch  ( '#',  inname                               )
            call ctx%errch  ( '#',  dynfam                               )
            call ctx%errch  ( '#',  kwfrez                               )
            call ctx%errch  ( '#',  kwrsta                               )
            call ctx%sigerr ( 'SPICE(FRAMEDEFERROR)'                     )
            return

         end if

!
!        Set the rotation state logical flag indicating whether
!        the state is 'INERTIAL'.
!
         if ( fnd ) then
!
!           A rotation state keyword was found.
!
!           We know the state is not frozen if we arrive here.
!
            inert  =  eqstr ( rotsta, kvinrt )

            if ( .not. inert ) then
!
!              Catch invalid rotation states here.
!
               if (  .not.  eqstr ( rotsta, kvrotg )  ) then

                  call ctx%setmsg ( 'Definition of frame # contains '     // &
                                '# specification #. The only valid '  // &
                                'rotation states are # or #. '        // &
                                'This situation is usually caused by '// &
                                'an error in a frame kernel in which '// &
                                'the frame is defined.'               )
                  call ctx%errch  ( '#',  inname                          )
                  call ctx%errch  ( '#',  kwrsta                          )
                  call ctx%errch  ( '#',  rotsta                          )
                  call ctx%errch  ( '#',  kvrotg                          )
                  call ctx%errch  ( '#',  kvinrt                          )
                  call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'                 )
                  return

               end if

            end if

         else
!
!           The state is not inertial unless there's a ROTATION_STATE
!           keyword assignment telling us it is.
!
            inert  = .false.

         end if

!
!        INERT and FROZEN are both set. The evaluation epoch T0 is also
!        set.
!
!        The following code block performs actions specific to
!        the various dynamic frame families.
!
         if ( ofdate ) then
!
!           Fetch the name of the true equator and equinox of date
!           precession model.
!
            call zzdynvac ( ctx, inname, infram, kwprcm, 1, n, prcmod )

            if ( ctx%failed() ) return

!
!           Get the precession transformation.
!
            if (  eqstr( prcmod, kvm001 )  ) then
!
!              This is the 1976 IAU earth precession model.
!
!              Make sure the center of the input frame is the earth.
!
               if ( center /= earth ) then

                  call bodc2n ( ctx, center, ctrnam, fnd )

                  if ( .not. fnd ) then
                     call intstr ( center, ctrnam )
                  end if

                  call ctx%setmsg ( 'Definition of frame # specifies '    // &
                                'frame center # and precession '      // &
                                'model #. This precession model '     // &
                                'is not applicable to body #. '       // &
                                'This situation is usually caused by '// &
                                'an error in a frame kernel in which '// &
                                'the frame is defined.'               )
                  call ctx%errch  ( '#',  inname                          )
                  call ctx%errch  ( '#',  ctrnam                          )
                  call ctx%errch  ( '#',  kvm001                          )
                  call ctx%errch  ( '#',  ctrnam                          )
                  call ctx%sigerr ( 'SPICE(INVALIDSELECTION)'             )
                  return

               end if
!
!              Look up the precession transformation.
!
               call zzeprc76 ( ctx, t0, precxf )

!
!              If we're in the mean-of-date case, invert this
!              transformation to obtain the mapping from the
!              mean-of-date frame to J2000.
!
               if ( meaneq ) then
                  call invstm ( ctx, precxf, xftemp )
               end if

            else

               call ctx%setmsg ( 'Definition of frame # specifies '    // &
                             'precession model #, which is not '   // &
                             'recognized. '                        // &
                             'This situation is usually caused by '// &
                             'an error in a frame kernel in which '// &
                             'the frame is defined.'               )
               call ctx%errch  ( '#',  inname                          )
               call ctx%errch  ( '#',  prcmod                          )
               call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'                 )
               return

            end if

!
!           At this point the precession transformation PRECXF is set.
!           If INFRAM is a mean equator and equinox of date frame, the
!           inverse of PRECXF is currently stored in XFTEMP.

            if ( trueeq ) then
!
!              We need a nutation transformation as well. Get the name
!              of the nutation model.
!
               call zzdynvac ( ctx, inname, infram, kwnutm, 1, n, nutmod )

               if ( ctx%failed() ) return

!
!              Get the nutation transformation.
!
               if (  eqstr( nutmod, kvm002 )  ) then
!
!                 This is the 1980 IAU earth nutation model.
!
!                 Make sure the center is the earth.
!
                  if ( center /= earth ) then

                     call bodc2n ( ctx, center, ctrnam, fnd )

                     if ( .not. fnd ) then
                        call intstr ( center, ctrnam )
                     end if

                     call ctx%setmsg ( 'Definition of frame # specifies ' // &
                                   'frame center # and nutation '     // &
                                   'model #. This '                   // &
                                   'nutation model is not applicable '// &
                                   'to body #.  This situation is '   // &
                                   'usually caused by an error in a ' // &
                                   'frame kernel in which '           // &
                                   'the frame is defined.'            )
                     call ctx%errch  ( '#',  inname                       )
                     call ctx%errch  ( '#',  ctrnam                       )
                     call ctx%errch  ( '#',  kvm002                       )
                     call ctx%errch  ( '#',  ctrnam                       )
                     call ctx%sigerr ( 'SPICE(INVALIDSELECTION)'          )
                     return

                  end if

!
!                 Look up the nutation transformation.
!
                  call zzenut80 ( ctx, t0, nutxf )

!
!                 Find the transformation from the J2000 frame to the
!                 earth true of date frame.  Invert.
!
                  call mxmg   ( nutxf, precxf, 6, 6, 6, xfinv )
                  call invstm ( ctx, xfinv, xftemp )

               else

                  call ctx%setmsg ( 'Definition of frame # specifies '    // &
                                'nutation model #, which is not '     // &
                                'recognized. '                        // &
                                'This situation is usually caused by '// &
                                'an error in a frame kernel in which '// &
                                'the frame is defined.'               )
                  call ctx%errch  ( '#',  inname                          )
                  call ctx%errch  ( '#',  nutmod                          )
                  call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'                 )
                  return

               end if

            else if ( meanec ) then
!
!              We need a mean obliquity transformation as well.
!              Get the name of the obliquity model.
!
               call zzdynvac ( ctx, inname, infram, kwobqm, 1, n, oblmod )

               if ( ctx%failed() ) return

!
!              Get the obliquity transformation.
!
               if (  eqstr( oblmod, kvm003 )  ) then
!
!                 This is the 1980 IAU earth mean obliquity of
!                 date model.
!
!                 Make sure the center is the earth.
!
                  if ( center /= earth ) then

                     call bodc2n ( ctx, center, ctrnam, fnd )

                     if ( .not. fnd ) then
                        call intstr ( center, ctrnam )
                     end if

                     call ctx%setmsg ( 'Definition of frame # specifies ' // &
                                   'frame center # and obliquity '    // &
                                   'model #.  This obliquity model '  // &
                                   'is not applicable to body #. '    // &
                                   'This situation is '               // &
                                   'usually caused by an error in a ' // &
                                   'frame kernel in which '           // &
                                   'the frame is defined.'            )
                     call ctx%errch  ( '#',  inname                       )
                     call ctx%errch  ( '#',  ctrnam                       )
                     call ctx%errch  ( '#',  kvm003                       )
                     call ctx%errch  ( '#',  ctrnam                       )
                     call ctx%sigerr ( 'SPICE(INVALIDSELECTION)'          )
                     return

                  end if

!
!                 Create the obliquity transformation. First look up
!                 the obliquity state (angle and angular rate).
!
                  call zzmobliq ( ctx, t0, mob, dmob )

!
!                 The obliquity rotation is about the mean-of-date
!                 x-axis.  The other Euler angles are identically
!                 zero; the axes are arbitrary, as long as the
!                 middle axis is distinct from the other two.
!
                  call cleard ( 6, eulang )
                  eulang(3) = mob
                  eulang(6) = dmob

                  call eul2xf ( ctx, eulang, 1, 3, 1, oblxf )

!
!                 Find the transformation from the J2000 to the
!                 earth mean ecliptic of date frame.  Invert.
!
                  call mxmg   ( oblxf, precxf, 6, 6, 6, xfinv )
                  call invstm ( ctx, xfinv, xftemp )

               else

                  call ctx%setmsg ( 'Definition of frame # specifies '    // &
                                'obliquity model #, which is not '    // &
                                'recognized. '                        // &
                                'This situation is usually caused by '// &
                                'an error in a frame kernel in which '// &
                                'the frame is defined.'               )
                  call ctx%errch  ( '#',  inname                          )
                  call ctx%errch  ( '#',  oblmod                          )
                  call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'                 )
                  return

               end if

            end if

!
!           At this point, XFTEMP contains the mapping from the
!           specified mean of date or true of date frame to J2000.
!
!           If the base frame is not J2000, we must find the
!           transformation from J2000 to the base frame.
!
            if ( basfrm /= j2000 ) then

               call zzfrmch0 ( ctx, j2000,  basfrm, t0,      xf2000 )
               call mxmg     ( xf2000, xftemp, 6, 6, 6, xform  )

            else
!
!              Otherwise, XFTEMP is the matrix we want.
!
               call moved ( xftemp, 36, xform )

            end if

!
!           Now XFORM is the state transformation mapping from
!           the input frame INFRAM to the base frame BASFRM.
!
!           This is the end of the work specific to "of-date" frames.
!           From here we drop out of the IF block.  At the end of this
!           routine, the derivative block of XFORM will be zeroed out
!           if the frame is frozen.  If the rotation state is
!           'INERTIAL', we will make sure the transformation between
!           the defined frame and the J2000 frame has time derivative
!           zero.
!

         else if ( dynfam == kv2vec ) then
!
!           The frame belongs to the TWO-VECTOR family.
!
!           Initialize the array S2.
!
            call cleard ( 12, s2 )

!
!           Fetch the specifications of the primary and secondary
!           axes.
!
            do i = 1, 2
!
!              Get the name of the axis associated with the Ith
!              defining vector.
!
               call zzdynvac ( ctx, inname, infram, itmaxe(i), 1, n, axname )

               if ( ctx%failed() ) return

               call cmprss ( ' ', 0, axname, axname )
               call ucase  ( axname,         axname )

!
!              Set the sign flag associated with the axis.
!
               negate  =  axname(1:1) == '-'

               call cmprss ( '-', 0, axname, axname )
               call cmprss ( '+', 0, axname, axname )

               axis(i) = isrchc ( axname, 3, axes )

               if ( axis(i) == 0 ) then

                  call ctx%setmsg ( 'Definition of frame # associates '   // &
                                'vector # with axis #.  The only '    // &
                                'valid axis values are { X, -X, Y, '  // &
                                '-Y, Z, -Z }. '                       // &
                                'This situation is usually caused by '// &
                                'an error in a frame kernel in which '// &
                                'the frame is defined.'               )
                  call ctx%errch  ( '#',  inname                          )
                  call ctx%errint ( '#',  i                               )
                  call ctx%errch  ( '#',  axname                          )
                  call ctx%sigerr ( 'SPICE(INVALIDAXIS)'                  )
                  return

               end if

!
!              Find out how the vector is defined:
!
!                 - Observer-target position vector
!                 - Observer-target velocity vector
!                 - Observer-target near point vector
!                 - Constant vector
!
!              VECDEF(I) indicates the vector definition method
!              for the Ith vector.
!
               call zzdynvac ( ctx, inname,  infram,    itmvdf(i), &
                               1,       n,         vecdef(i) )

               if ( ctx%failed() ) return

               call cmprss ( ' ',    0, vecdef(i), vecdef(i) )
               call ucase  ( vecdef(i),            vecdef(i) )

               if (  vecdef(i) == kvposv  ) then
!
!                 The vector is the position of a target relative
!                 to an observer.
!
!                 We need a target, observer, and aberration correction.
!
                  call zzdynbid ( ctx, inname, infram, itmtrg(i), targ )

                  call zzdynbid ( ctx, inname, infram, itmobs(i), obs  )

                  call zzdynvac ( ctx, inname, infram, itmabc(i), &
                                  1,      n,      abcorr     )

!
!                 Look up the Ith state vector in the J2000 frame.
!
                  call zzspkez0 ( ctx, targ,    t0,   'J2000', &
                                  abcorr,  obs,  s2(1,i),  lt )

                  if ( ctx%failed() ) return

!
!                 At this point, S2(*,I) contains position and
!                 velocity relative to frame J2000.
!

               else if ( vecdef(i) == kvvelv ) then
!
!                 The vector is the velocity of a target relative
!                 to an observer.
!
!                 We need a target, observer, and aberration correction.
!
                  call zzdynbid ( ctx, inname, infram, itmtrg(i), targ )

                  call zzdynbid ( ctx, inname, infram, itmobs(i), obs  )

                  call zzdynvac ( ctx, inname, infram, itmabc(i), &
                                  1,      n,      abcorr     )

!
!                 We need to know the frame in which the velocity is
!                 defined.
!
                  call zzdynfid ( ctx, inname, infram, itmfrm(i), frid )
                  call frmnam   ( ctx, frid,   velfrm  )

                  if ( ctx%failed() ) return

!
!                 Obtain the velocity vector in the specified velocity
!                 frame.  Also obtain bracketing vectors to support
!                 discrete differentiation.  (See notes in zzdyn.inc
!                 regarding definition of DELTA.)
!
                  delta = max ( 1.0_wp,  (2.0_wp**qexp) * t0 )

                  call vpack ( t0-delta, t0, t0+delta, tarray )

                  do j = 1, 3

                     call zzspkez0 ( ctx, targ,   tarray(j), velfrm, &
                                     abcorr, obs,       stemp,  lt )
!
!                    We compute the derivative using unit
!                    velocity vectors.
!
                     if ( ctx%failed() ) return

                     call vhat ( stemp(4), varray(1,j) )

                  end do

!
!                 Compute acceleration and fill in the velocity state
!                 vector S2(*,I).
!
                  call qderiv ( ctx, 3, &
                                varray(1,1),  varray(1,3),  delta, acc )

                  call vequ   ( varray(1,2),  s2(1,i) )
                  call vequ   ( acc,          s2(4,i) )

!
!                 We need the epoch VET at which VELFRM is evaluated.
!                 This epoch will be used to transform the velocity's
!                 "state" vector from VELFRM to J2000.
!
!                 Set the default value of VET here.
!
                  vet = t0

!
!                 Parse the aberration correction.  Find the epoch used
!                 to evaluate the velocity vector's frame.
!
                  call zzvalcor ( ctx, abcorr, corblk )

                  if ( corblk(ltidx) ) then
!
!                    Light time correction is used.  The epoch used
!                    to evaluate the velocity vector's frame depends
!                    on the frame's observer and center.
!
!                    Look up the velocity frame's center.
!
                     call frinfo ( ctx, frid, frctr, frcls, frcid, fnd )

                     if ( .not. fnd ) then

                        call ctx%setmsg ( 'In definition of frame #, ' // &
                                      'the frame associated with ' // &
                                      'a velocity vector has '     // &
                                      'frame ID code #, but no '   // &
                                      'frame center, frame class, '// &
                                      'or frame class ID was '     // &
                                      'found by FRINFO.  This '    // &
                                      'situation MAY be caused '   // &
                                      'by an error in a frame '    // &
                                      'kernel in which the frame ' // &
                                      'is defined. The problem '   // &
                                      'also could be indicative '  // &
                                      'of a SPICELIB bug.'         )
                        call ctx%errch  ( '#',  inname                 )
                        call ctx%errint ( '#',  frid                   )
                        call ctx%sigerr ( 'SPICE(FRAMEDATANOTFOUND)'   )
                        return

                     end if

!
!                    If the velocity frame is non-inertial, we'll need
!                    to compute the evaluation epoch for this frame.
!
                     if ( frcls /= inertl ) then
!
!                       Obtain light time from the observer to the
!                       frame's center; find the evaluation epoch VET
!                       for the frame.

                        call zzspkzp0 ( ctx, frctr,  t0,  'J2000', &
                                        abcorr, obs, ctrpos,  vflt )
                        call zzcorepc ( ctx, abcorr, t0,  vflt,    vet  )

                        if ( ctx%failed() ) return

                     end if

                  end if

!
!                 The velocity frame's evaluation epoch VET is now set.
!
!                 We must rotate the velocity vector and transform the
!                 acceleration from the velocity frame (evaluated at
!                 VET) to the output frame at T0.  We'll do this in two
!                 stages, first mapping velocity and acceleration into
!                 the J2000 frame.
!
                  if ( frid /= j2000 ) then

                     call zzfrmch0 ( ctx, frid,    j2000,  vet,   xf2000  )

                     if ( ctx%failed() ) return

                     call mxvg     ( xf2000,  s2(1,i), 6, 6, stemp   )
                     call moved    ( stemp,   6,             s2(1,i) )

                  end if

!
!                 At this point, S2(*,I) contains velocity and
!                 acceleration relative to frame J2000.
!

               else if ( vecdef(i) == kvnear ) then
!
!                 The vector points from an observer to the
!                 sub-observer point (nearest point to the observer) on
!                 the target body.
!
!                 We need a target, observer, and aberration correction.
!
                  call zzdynbid ( ctx, inname, infram, itmtrg(i), targ )

                  call zzdynbid ( ctx, inname, infram, itmobs(i), obs  )

                  call zzdynvac ( ctx, inname, infram, itmabc(i), &
                                  1,      n,      abcorr     )

!
!                 The vector points from the observer to the nearest
!                 point on the target. We need the state of the near
!                 point relative to the observer.
!
!                 We'll look up the state of the target center relative
!                 to the observer and the state of the near point
!                 relative to the target center, both in the body-fixed
!                 frame associated with the target.
!
!                 Look up the body-fixed frame associated with the
!                 target body.
!
                  call cidfrm ( ctx, targ, cfrmid, cfrmnm, fnd )

                  if ( .not. fnd ) then

                     call ctx%setmsg ( 'Definition of frame # requires ' // &
                                   'definition of body-fixed frame ' // &
                                   'associated with target body #. ' // &
                                   'A call to CIDFRM indicated no '  // &
                                   'body-fixed frame is associated ' // &
                                   'with the target body.  This '    // &
                                   'situation can arise when a '     // &
                                   'frame kernel defining the '      // &
                                   'target''s body-fixed frame  '    // &
                                   'lacks the OBJECT_<ID>_FRAME '    // &
                                   'or OBJECT_<name>_FRAME '         // &
                                   'keywords.  The problem also '    // &
                                   'could be caused by an '          // &
                                   'error in a frame kernel in '     // &
                                   'which the parameterized '        // &
                                   'two-vector dynamic frame # '     // &
                                   'is defined.'                     )
                     call ctx%errch  ( '#',  inname                      )
                     call ctx%errint ( '#',  targ                        )
                     call ctx%errch  ( '#',  inname                      )
                     call ctx%sigerr ( 'SPICE(FRAMEDATANOTFOUND)'        )
                     return

                  end if

!
!                 Get the radii of the target body.
!
                  call zzgftreb ( ctx, targ, radii )

!
!                 Look up the Ith state vector in the target-fixed
!                 frame.  Negate the vector to obtain the target-to-
!                 observer vector.
!
                  call zzspkez0 ( ctx, targ,    t0,       cfrmnm, &
                                  abcorr,  obs,      stemp,    lt )

!
!                 We check FAILED() here because VMINUG is a simple
!                 arithmetic routine that doesn't return on entry.
!
                  if ( ctx%failed() ) return

                  call vminug   ( stemp,   6,        stobs )

                  call dnearp   ( ctx, stobs,   radii(1), radii(2), radii(3), &
                                  stnear,  stalt,    fnd               )

                  if ( ctx%failed() ) return

                  if ( .not. fnd ) then

                     call ctx%setmsg ( 'In definition of frame #, '      // &
                                   'vector # is defined by the '     // &
                                   'near point on body # as seen '   // &
                                   'from body #.  The state of this '// &
                                   'near point was not found. '      // &
                                   'See the routine DNEARP for '     // &
                                   'an explanation.  This '          // &
                                   'situation MAY be caused '        // &
                                   'by an error in a frame '         // &
                                   'kernel in which the frame '      // &
                                   'is defined. The problem '        // &
                                   'also could be indicative '       // &
                                   'of a SPICELIB bug.'              )
                     call ctx%errch  ( '#',  inname                      )
                     call ctx%errint ( '#',  i                           )
                     call ctx%errint ( '#',  targ                        )
                     call ctx%errint ( '#',  obs                         )
                     call ctx%sigerr ( 'SPICE(DEGENERATECASE)'           )
                     return

                  end if

!
!                 Find the observer-near point state in the target
!                 body-fixed frame.
!
                  call vsubg ( stnear, stobs, 6, stemp  )

!
!                 Transform the state to frame J2000.  To get the
!                 required transformation matrix, we'll need to obtain
!                 the epoch associated with CNMFRM.  Parse the
!                 aberration correction and adjust the frame evaluation
!                 epoch as needed.
!
                  call zzcorepc ( ctx, abcorr, t0, lt, fet )

!
!                 Obtain the matrix for transforming state vectors
!                 from the target center frame to the J2000 frame and
!                 apply it to the observer-to-near point state vector.
!
                  call zzfrmch0 ( ctx, cfrmid, j2000, fet, xipm )

                  if ( ctx%failed() ) return

                  call mxvg ( xipm, stemp, 6, 6, s2(1,i) )

!
!                 At this point, S2(*,I) contains position and
!                 velocity of the near point on the target as
!                 seen by the observer, relative to frame J2000.
!

               else if ( vecdef(i) == kvcons ) then
!
!                 The vector is constant in a specified frame.
!
!                 We need a 3-vector and an associated reference
!                 frame relative to which the vector is specified.
!
!                 Look up the ID of the frame first.
!
                  call zzdynfid ( ctx, inname, infram, itmfrm(i), frid )

!
!                 Let FET ("frame ET") be the evaluation epoch for
!                 the constant vector's frame.  By default, this
!                 frame is just T0, but if we're using light time
!                 corrections, FET must be adjusted for one-way
!                 light time between the frame's center and the
!                 observer.
!
!                 Set the default value of FET here.
!
                  fet = t0

!
!                 Optionally, there is an aberration correction
!                 associated with the constant vector's frame.
!                 If so, an observer must be associated with the
!                 frame.  Look up the correction first.
!
                  call zzdynoac ( ctx, inname, infram, itmabc(i), &
                                  1,      n,      cvcorr,    fnd )

                  if ( .not. fnd ) then
                     cvcorr = 'NONE'
                  end if

                  call zzprscor ( ctx, cvcorr, corblk )

                  if ( ctx%failed() ) return

                  if ( .not. corblk(geoidx) ) then
!
!                    We need to apply an aberration correction to
!                    the constant vector.
!
!                    Check for errors in the aberration correction
!                    specification.
!
!                       - Light time and stellar aberration corrections
!                         are mutually exclusive.
!
                     if ( corblk(ltidx) .and. corblk(stlidx) ) then

                        call ctx%setmsg ( 'Definition of frame # '       // &
                                      'specifies aberration '        // &
                                      'correction # for constant '   // &
                                      'vector.  Light time and '     // &
                                      'stellar aberration '          // &
                                      'corrections are mutually '    // &
                                      'exclusive for constant '      // &
                                      'vectors used in two-vector '  // &
                                      'parameterized dynamic frame ' // &
                                      'definitions.  This situation '// &
                                      'is usually caused by an '     // &
                                      'error in a frame kernel in '  // &
                                      'which the frame is defined.'  )
                        call ctx%errch  ( '#',  inname                   )
                        call ctx%errch  ( '#',  cvcorr                   )
                        call ctx%sigerr ( 'SPICE(INVALIDOPTION)'         )
                        return

                     end if

                     if ( corblk(ltidx) ) then
!
!                       Light time correction is used.  The epoch used
!                       to evaluate the constant vector's frame depends
!                       on the frame's observer and center.
!
!                       Look up the constant vector frame's center.
!
                        call frinfo ( ctx, frid, frctr, frcls, frcid, fnd )

                        if ( .not. fnd ) then

                           call ctx%setmsg ( 'In definition of frame #, ' // &
                                         'the frame associated with ' // &
                                         'a constant vector has '     // &
                                         'frame ID code #, but no '   // &
                                         'frame center, frame class, '// &
                                         'or frame class ID was '     // &
                                         'found by FRINFO.  This '    // &
                                         'situation MAY be caused '   // &
                                         'by an error in a frame '    // &
                                         'kernel in which the frame ' // &
                                         'is defined. The problem '   // &
                                         'also could be indicative '  // &
                                         'of a SPICELIB bug.'         )
                           call ctx%errch  ( '#',  inname                 )
                           call ctx%errint ( '#',  frid                   )
                           call ctx%sigerr ( 'SPICE(FRAMEDATANOTFOUND)'   )
                           return

                        end if
!
!                       If the constant vector frame is non-inertial,
!                       we'll need to compute the evaluation epoch for
!                       this frame.
!
                        if ( frcls /= inertl ) then
!
!                          Look up the observer associated with the
!                          constant vector's frame.  This observer,
!                          together with the frame's center, determines
!                          the evaluation epoch for the frame.
!
                           call zzdynbid ( ctx, inname,    infram, &
                                           itmobs(i), cvobs   )

!
!                          Obtain light time from the observer to the
!                          frame's center.
!
                           call zzspkzp0 ( ctx, frctr,  t0,   'J2000', &
                                           cvcorr, cvobs, ctrpos,  lt )

!
!                          Re-set the evaluation epoch for the frame.
!
                           call zzcorepc ( ctx, cvcorr, t0, lt, fet )

                           if ( ctx%failed() ) return

                        end if
!
!                       The constant vector frame's evaluation epoch
!                       FET has been set.
!

                     else if ( corblk(stlidx) ) then
!
!                       Stellar aberration case.
!
!                       The constant vector must be corrected for
!                       stellar aberration induced by the observer's
!                       velocity relative to the solar system
!                       barycenter.  First, find this velocity in
!                       the J2000 frame.  We'll apply the correction
!                       later, when the constant vector has been
!                       transformed to the J2000 frame.
!
                        call zzdynbid ( ctx, inname,    infram, &
                                        itmobs(i), cvobs   )

                        call zzspksb0 ( ctx, cvobs, t0, 'J2000', stobs )

                        if ( ctx%failed() ) return

                     end if

                  end if

!
!                 At this point FET is the frame evaluation epoch
!                 for the frame associated with the constant vector.
!
!                 If stellar aberration correction has been specified,
!                 STOBS is the state of the observer relative to the
!                 solar system barycenter, expressed in the J2000
!                 frame.
!
!                 Get the constant vector specification.
!
                  call zzdynvac ( ctx, inname,  infram,  itmspc(i), &
                                  1,       n,       spec       )

                  if ( ctx%failed() ) return

                  call cmprss ( ' ', 0, spec, spec )
                  call ucase  ( spec,         spec )

                  if ( spec == kvrecc ) then
!
!                    The coordinate system is rectangular.
!
!                    Look up the constant vector.
!
                     call zzdynvad ( ctx, inname, infram, itmvec(i), &
                                     3,      n,      dirvec    )

                  else if (      ( spec == kvlatc ) &
                            .or. ( spec == kvradc ) ) then
!
!                    The coordinate system is latitudinal or RA/DEC.
!
!                    Look up the units associated with the angles.
!
                     call zzdynvac ( ctx, inname,  infram,  itmunt(i), &
                                     1,       n,       units     )

                     if ( spec == kvlatc ) then
!
!                       Look up longitude and latitude.
!
                        call zzdynvad ( ctx, inname, infram, itmlon(i), 1, n, lon_1 )
                        lon = lon_1(1)

                        call zzdynvad ( ctx, inname, infram, itmlat(i), 1, n, lat_1 )
                        lat = lat_1(1)

!
!                       Convert angles from input units to radians.
!
                        call convrt ( ctx, lon, units, kvradn, angles(1) )
                        call convrt ( ctx, lat, units, kvradn, angles(2) )

                     else
!
!                       Look up RA and DEC.
!
                        call zzdynvad ( ctx, inname, infram, itmra(i), 1, n, ra_1 )
                        ra = ra_1(1)

                        call zzdynvad ( ctx, inname, infram, itmdec(i), 1, n, dec_1 )
                        dec = dec_1(1)

!
!                       Convert angles from input units to radians.
!
                        call convrt ( ctx, ra,  units, kvradn, angles(1) )
                        call convrt ( ctx, dec, units, kvradn, angles(2) )

                     end if

                     if ( ctx%failed() ) return

!
!                    Now  produce a direction vector.
!
                     call latrec ( 1.0_wp, angles(1), angles(2), dirvec )

                  else

                     call ctx%setmsg ( 'Definition of two-vector '       // &
                                   'parameterized dynamic frame # '  // &
                                   'includes constant vector '       // &
                                   'specification #, which is '      // &
                                   'not supported.  This situation ' // &
                                   'is usually caused by an error '  // &
                                   'in a frame kernel in which '     // &
                                   'the frame is defined.'           )
                     call ctx%errch  ( '#',  inname                      )
                     call ctx%errch  ( '#',  spec                        )
                     call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'             )
                     return

                  end if

!
!                 At this point, the Cartesian coordinates of the
!                 vector relative to the constant vector frame
!                 are stored in DIRVEC.
!
!                 Convert the direction vector to the J2000 frame.
!                 Fill in the state vector.  The velocity in the
!                 constant vector's frame is zero.
!
                  call vequ  ( dirvec, s2(1,i) )
                  call cleard( 3,      s2(4,i) )

                  if ( frid /= j2000 ) then

                     call zzfrmch0 ( ctx, frid, j2000, fet, xipm )

                     if ( ctx%failed() ) return

                     call mxvg ( xipm,  s2(1,i), 6, 6, stemp   )
                     call moved( stemp, 6,             s2(1,i) )

                  end if

!
!                 The state of the constant vector is now represented
!                 in the J2000 frame, but we may still need to
!                 apply a stellar aberration correction.
!
                  if ( corblk(stlidx) ) then
!
!                    Perform the stellar aberration correction
!                    appropriate to the radiation travel sense.

                     if ( corblk(xmtidx) ) then
!
!                       The correction is for transmission.
!
                        call stlabx ( ctx, s2(1,i), stobs(4), stemp )

                     else
!
!                       The correction is for reception.
!
                        call stelab ( ctx, s2(1,i), stobs(4), stemp )

                     end if

                     if ( ctx%failed() ) return

!
!                    Update the position portion of S2(*,I).
!
                     call vequ ( stemp, s2(1,i) )

                  end if

!
!                 At this point, S2(*,I) contains position and velocity
!                 of the constant (constant relative to its associated
!                 frame, that is) vector as seen by the observer,
!                 relative to frame J2000.
!

               else

                  call ctx%setmsg ( 'Definition of two-vector '       // &
                                'parameterized dynamic frame # '  // &
                                'includes vector '                // &
                                'definition #, which is '         // &
                                'not supported.  This situation ' // &
                                'is usually caused by an error '  // &
                                'in a frame kernel in which '     // &
                                'the frame is defined.'           )
                  call ctx%errch  ( '#',  inname                      )
                  call ctx%errch  ( '#',  vecdef(i)                   )
                  call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'             )
                  return

               end if

!
!              Negate the state vector if the axis has negative sign.
!
               if ( negate ) then
                  call vminug ( s2(1,i), 6, stemp   )
                  call moved  ( stemp,   6, s2(1,i) )
               end if

            end do

!
!           Look up the lower bound for the angular separation of
!           the defining vectors.  Use the default value if none
!           was supplied.
!
            call zzdynoad ( ctx, inname, infram, itmsep, 1, n, minsep_1, fnd )
            minsep = minsep_1(1)

            if ( ctx%failed() ) return

            if ( .not. fnd ) then
               minsep = lbsep
            end if

!
!           Now use our states to compute our state transformation
!           matrix.
!
!           Check the angular separation of the defining vectors. We
!           want to ensure that the vectors are not too close to being
!           linearly dependent.  We can handle both cases---separation
!           close to 0 or separation close to Pi---by comparing the
!           sine of the separation to the sine of the separation limit.
!
            sep = vsep ( s2(1,1), s2(1,2) )

            if ( sin(sep) < sin(minsep) ) then

               call etcal  ( ctx, t0, timstr )

               call ctx%setmsg ( 'Angular separation of vectors defining '// &
                             'two-vector parameterized dynamic frame '// &
                             '# is # (radians); minimum allowed diff' // &
                             'erence of separation from 0 or Pi is # '// &
                             'radians.  Evaluation epoch is #.  Extre'// &
                             'me loss of precision can occur when '   // &
                             'defining vectors are nearly linearly '  // &
                             'dependent.  This type of error can be ' // &
                             'due to using a dynamic frame outside of'// &
                             ' the time range for which it is meant. '// &
                             'It also can be due to a conceptual '    // &
                             'error pertaining to the frame''s defin' // &
                             'ition, or to an implementation error '  // &
                             'in the frame kernel containing the '    // &
                             'frame definition. However, if you wish '// &
                             'to proceed with this computation, '     // &
                             'the # keyword can be used in '          // &
                             'the frame definition to adjust the '    // &
                             'separation limit.'                      )
               call ctx%errch  ( '#',  inname                             )
               call ctx%errdp  ( '#',  sep                                )
               call ctx%errdp  ( '#',  minsep                             )
               call ctx%errch  ( '#',  timstr                             )
               call ctx%errch  ( '#',  kwatol                             )
               call ctx%sigerr ( 'SPICE(DEGENERATECASE)'                  )
               return

            end if

!
!           We have both states expressed relative to frame J2000
!           at this point.  Find the transformation from INNAME to
!           the frame J2000, then from J2000 to frame BASNAM.
!
            call zztwovxf ( ctx, s2(1,1), axis(1), s2(1,2), axis(2), xform )

            if ( basfrm /= j2000 ) then

               call moved ( xform, 36, xftemp )

               call zzfrmch0 ( ctx, j2000, basfrm,  t0, xf2000 )

               if ( ctx%failed() ) return

               call mxmg ( xf2000,  xftemp,  6,  6,  6, xform  )

            end if

!
!           This is the end of the work specific to two-vector frames.
!           From here we drop out of the IF block.  At the end of this
!           routine, the derivative block of XFORM will be zeroed out
!           if the frame is frozen.  If the rotation state is
!           'INERTIAL', we will make sure the transformation between
!           the defined frame and the J2000 frame has time derivative
!           zero.
!

         else if ( dynfam == kveulr ) then
!
!           The frame belongs to the Euler family.
!
!           We expect to see specifications of an axis sequence, units,
!           and angles via polynomial coefficients.  We also expect
!           to see an ET epoch.
!
!           Look up the epoch first.  Let DELTA represent the offset
!           of T0 relative to the epoch.
!
!           Initialize EPOCH so subtraction doesn't overflow if EPOCH
!           is invalid due to a lookup error.
!
            epoch = 0.0_wp

            call zzdynvad ( ctx, inname, infram, kwepoc, 1, n, epoch_1 )
            epoch = epoch_1(1)

            delta = t0 - epoch

!
!           Now the axis sequence.
!
            call zzdynvai ( ctx, inname, infram, kweuax, 3, n, iaxes )

!
!           Now the coefficients for the angles.
!
            do i = 1, 3
!
!              Initialize N so subtraction doesn't overflow if N
!              is invalid due to a lookup error.
!
               n = 0

               call zzdynvad ( ctx, inname,  infram,  itmcof(i), &
                               maxcof,  n,       coeffs(1,i) )

!
!              Set the polynomial degree for the Ith angle.
!
               degs(i) = n - 1

            end do

!
!           Look up the units associated with the angles.
!
            call zzdynvac ( ctx, inname, infram, kwunit, 1, n, units )

            if ( ctx%failed() ) return

!
!           Evaluate the angles and their derivatives at DELTA.  Convert
!           angles from input units to radians and radians/sec.
!
            do i = 1, 3

               call polyds ( coeffs(1,i), degs(i), 1, delta, poly )
!
!              Convert units.  Fill in the Euler angle state vector.
!
               call convrt ( ctx, poly(0), units, kvradn, eulang(i  ) )
               call convrt ( ctx, poly(1), units, kvradn, eulang(i+3) )

            end do

!
!           Produce a state transformation matrix that maps from
!           the defined frame to the base frame.
!
            call eul2xf ( ctx, eulang, iaxes(1), iaxes(2), iaxes(3), xform )

!
!           This is the end of the work specific to Euler frames.
!           From here we drop out of the IF block.  At the end of this
!           routine, the derivative block of XFORM will be zeroed out
!           if the frame is frozen.  If the rotation state is
!           'INERTIAL', we will make sure the transformation between
!           the defined frame and the J2000 frame has time derivative
!           zero.
!

         else if ( dynfam == kvprod ) then
!
!           The frame belongs to the product family.
!
!           We expect to see specifications of "from" and "to" frames
!           that make up the product.
!
!           Look up the "from" and "to" frames that define the product.
!
            call zzdynvac ( ctx, inname, infram, kwffrm, mxnfac, m, fframs )
            call zzdynvac ( ctx, inname, infram, kwtfrm, mxnfac, n, tframs )

            if ( ctx%failed() ) return

            if ( n /= m ) then

               call ctx%setmsg ( 'Definition of product ' &
               //            'parameterized dynamic frame # ' &
               //            'has # "from" frames and # "to" ' &
               //            'frames. These counts must match.' )
               call ctx%errch  ( '#',  inname                       )
               call ctx%errint ( '#',  m                            )
               call ctx%errint ( '#',  n                            )
               call ctx%sigerr ( 'SPICE(BADFRAMECOUNT)'             )
               return

            end if

!
!           The product of the factors is the transformation from the
!           base frame to the output frame. We need the inverse of
!           this transformation.
!
!           Compute the inverse of the product of the factors; place
!           the result in XFORM.
!
            call namfrm ( ctx, fframs(1), fromid )
            call namfrm ( ctx, tframs(1), toid   )

            call zzfrmch0 ( ctx, toid, fromid, t0, xform )

            if ( ctx%failed() ) return

            do i = 2, n

               call moved  ( xform, 36, xftemp )

               call namfrm ( ctx, fframs(i), fromid )
               call namfrm ( ctx, tframs(i), toid   )

               call zzfrmch0 ( ctx, toid, fromid, t0, xform )

               if ( ctx%failed() ) return

               call mxmg  ( xform, xftemp, 6, 6, 6, xout  )
               call moved ( xout,  36,              xform )

            end do

         else

            call ctx%setmsg ( 'Dynamic frame family # (in definition of '// &
                          'frame #) is not supported. '              // &
                          'This situation is usually caused by '     // &
                          'an error in a frame kernel in which '     // &
                          'the frame is defined.'                    )
            call ctx%errch  ( '#',  dynfam                               )
            call ctx%errch  ( '#',  inname                               )
            call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'                      )
            return

         end if
!
!        This is the end of the IF block that processes the
!        parameterized dynamic frame families.
!

      else

         call ctx%setmsg ( 'Dynamic frame style # (in definition of ' // &
                       'frame #) is not supported. '              // &
                       'This situation is usually caused by '     // &
                       'an error in a frame kernel in which '     // &
                       'the frame is defined.'                    )
         call ctx%errch  ( '#',  dynstl                               )
         call ctx%errch  ( '#',  inname                               )
         call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'                      )
         return

      end if

      if ( ctx%failed() ) return

!
!     At this point XFORM is the state transformation matrix mapping
!     from the input frame INFRAM to the base frame BASFRM.
!
!     If the frame has rotation state 'INERTIAL', the frame must have
!     zero derivative with respect to any inertial frame. Set the
!     derivative block accordingly.
!
      if ( inert ) then
!
!        See whether the base frame is inertial.
!
         call irfnum ( ctx, basnam, j )

         if ( j > 0 ) then
!
!           The base frame is  a recognized inertial frame.  Zero
!           out the derivative block.
!
            do i = 1, 3
               call cleard ( 3, xform(4,i) )
            end do

         else
!
!           The base frame is *not* a recognized inertial frame.
!
!           Create the state transformation matrix that maps from the
!           defined frame to J2000.  Zero out the derivative block of
!           this matrix.  Convert the resulting matrix to the state
!           transformation from the defined frame to the output frame.
!
            call zzfrmch0 ( ctx, basfrm, j2000, t0, xf2000 )

            if ( ctx%failed() ) return

            call mxmg ( xf2000, xform, 6, 6, 6, xftemp )

            do i = 1, 3
               call cleard ( 3, xftemp(4,i) )
            end do

!
!           XFTEMP now represents the transformation from a
!           constant frame matching the defined frame at T0 to the
!           J2000 frame. Produce the transformation from this constant
!           frame to the output frame.
!
!           To avoid introducing additional round-off error into
!           the rotation blocks of XFORM, we overwrite only the
!           derivative block of XFORM with the derivative block
!           of the "inertial" transformation.
!
            call invstm ( ctx, xf2000, xfinv )
            call mxmg   ( xfinv,  xftemp, 6, 6, 6, xout )

            do i = 1, 3
               call vequ ( xout(4,i), xform(4,i) )
            end do

         end if

      end if

!
!     If the frame is frozen, zero out the derivative block of the
!     transformation matrix.
!
      if ( frozen ) then

         do i = 1, 3
            call cleard ( 3, xform(4,i) )
         end do

      end if

!
!     XFORM and BASFRM are set.
!

      return
      end associate
      end procedure zzdynfrm

      module procedure zzdynrot
!! ## Abstract
!!
!!     SPICE Private routine intended solely for the support of SPICE
!!     routines.  Users should not call this routine directly due
!!     to the volatile nature of this routine.
!!
!!     For a specified dynamic frame, find the rotation that maps
!!     positions from the dynamic frame to its base frame.
!!
!! ## Required Reading
!!
!!     CK
!!     FRAMES
!!     PCK
!!     SPK
!!
!! ## Keywords
!!
!!     FRAMES
!!     PRIVATE
!!
!! ## Declarations
!!
!! ## Detailed Input
!!
!!     INFRAM      is the frame ID code for a dynamic reference frame.
!!                 Note that this interface differs from that of TKFRAM,
!!                 which uses a class ID to identify the frame.
!!
!!                 In this routine, we refer this frame both as the
!!                 "input frame" and the "defined frame."
!!
!!     CENTER      is NAIF ID code for the center of the frame
!!                 designated by INFRAM.  This code, although derivable
!!                 from INFRAM, is passed in for convenience.
!!
!!     ET          is an epoch in ephemeris seconds past J2000 for which
!!                 the caller requests a rotation matrix.
!!
!! ## Detailed Output
!!
!!     ROTATE      is a 3x3 rotation matrix that transforms positions
!!                 relative to INFRAM to positions relative to BASFRM.
!!
!!     BASFRM      is the frame ID code of the base frame associated
!!                 with INFRAM.  The 3x3 matrix ROTATE transforms
!!                 positions relative to INFRAM to positions relative to
!!                 BASFRM. The position transformation is performed by
!!                 left-multiplying by ROTATE a position expressed
!!                 relative to INFRAM. This is easily accomplished via
!!                 the subroutine call shown below.
!!
!!                    CALL MXV ( ROTATE, INPOS, OUTPOS )
!!
!! ## Parameters
!!
!!     See include file zzdyn.inc.
!!
!! ## Exceptions
!!
!!     1)  If a dynamic frame evaluation requires unavailable kernel
!!         data, the error will be diagnosed by routines in the
!!         call tree of this routine.
!!
!!     2)  If a precession model is used to implement a frame centered
!!         at a body for which the model is not applicable, the error
!!         SPICE(INVALIDSELECTION) will be signaled.
!!
!!     3)  If a nutation model is used to implement a frame centered
!!         at a body for which the model is not applicable, the error
!!         SPICE(INVALIDSELECTION) will be signaled.
!!
!!     4)  If an obliquity model is used to implement a frame centered
!!         at a body for which the model is not applicable, the error
!!         SPICE(INVALIDSELECTION) will be signaled.
!!
!!     5)  If an unrecognized precession model is specified, the
!!         error SPICE(NOTSUPPORTED) is signaled.
!!
!!     6)  If an unrecognized nutation model is specified, the
!!         error SPICE(NOTSUPPORTED) is signaled.
!!
!!     7)  If an unrecognized obliquity model is specified, the
!!         error SPICE(NOTSUPPORTED) is signaled.
!!
!!     8)  If an attempt to look up the center of a frame does
!!         not yield data, the error SPICE(FRAMEDATANOTFOUND) is
!!         signaled.
!!
!!     9)  In a two-vector frame definition, if a constant vector
!!         specification method is not recognized, the error
!!         SPICE(NOTSUPPORTED) is signaled.
!!
!!     10) In a two-vector frame definition, if a vector definition
!!         method is not recognized, the error SPICE(NOTSUPPORTED)
!!         is signaled.
!!
!!     11) If an unrecognized dynamic frame family is specified, the
!!          error SPICE(NOTSUPPORTED) is signaled.
!!
!!     12) If an unrecognized dynamic frame definition style is
!!         specified, the error SPICE(NOTSUPPORTED) is signaled.
!!
!!     13) If an unrecognized dynamic frame rotation state is
!!         specified, the error SPICE(NOTSUPPORTED) is signaled.
!!
!!     14) If both a freeze epoch and a rotation state are specified,
!!         the error SPICE(FRAMEDEFERROR) is signaled.
!!
!!     15) If neither a freeze epoch nor a rotation state are specified
!!         for an "of date" frame, the error SPICE(FRAMEDEFERROR) is
!!         signaled.
!!
!!     16) In a two-vector frame definition, if an invalid axis
!!         specification is encountered, the error SPICE(INVALIDAXIS) is
!!         signaled.
!!
!!     17) In a two-vector frame definition using a target near point
!!         vector, if the body-fixed frame associated with the target
!!         is not found, the error SPICE(FRAMEDATANOTFOUND) is signaled.
!!
!!     18) If a dynamic frame evaluation requires excessive recursion
!!         depth, the error will be diagnosed by routines in the call
!!         tree of this routine.
!!
!!     19) When a two-vector dynamic frame is evaluated, if the
!!         primary and secondary vectors have angular separation less
!!         than the minimum allowed value, or if the angular separation
!!         differs from Pi by less than the minimum allowed value, the
!!         error SPICE(DEGENERATECASE) is signaled.  The default minimum
!!         separation is given by the parameter LBSEP; this value may be
!!         overridden by supplying a different value in the frame
!!         definition.
!!
!!     20) If invalid units occur in a frame definition, the error
!!         will be diagnosed by a routine in the call tree of this
!!         routine.
!!
!!     21) If an invalid Euler axis sequence occurs in a frame
!!         definition, the error will be diagnosed by a routine in the
!!         call tree of this routine.
!!
!!     22) If a body RADII vector has other than exactly thee elements,
!!         the error SPICE(INVALIDCOUNT) is signaled by a routine in
!!         the call tree of this routine.
!!
!!     23) If a body RADII vector has any element less-than or
!!         equal to zero, the error SPICE(BADAXISLENGTH) is signaled by
!!         a routine in the call tree of this routine.
!!
!! ## Files
!!
!!     1) SPK files containing data for each observer and target
!!        are required to support two-vector frames.  Note that
!!        observer-target pairs can be implicit, as in the case
!!        of a constant vector whose frame is evaluated at a
!!        light-time corrected epoch:  the light time the frame
!!        center to an observer must be computable in this case,
!!        which implies the state of the frame center as seen by
!!        the observer must be computable.
!!
!!     2) Any of SPK, CK, PCK, and frame kernels will also be required
!!        if any frames referenced in the definition of INFRAM (as a
!!        base frame, velocity vector frame, or constant vector frame)
!!        require them, or if any vectors used to define INFRAM require
!!        these data in order to be computable.
!!
!!     3) When CK data are required, one or more associated SCLK kernels
!!        ---normally, one kernel per spacecraft clock---are
!!        required as well.  A leapseconds kernel may be required
!!        whenever an SCLK kernel is required.
!!
!!     4) When a two-vector frame is defined using a target near point,
!!        a PCK file giving orientation and providing a triaxial shape
!!        model for the target body is required.
!!
!! ## Particulars
!!
!!     Currently only parameterized dynamic frames are supported by
!!     this routine.
!!
!!     Currently supported parameterized dynamic families are:
!!
!!        Two-vector
!!        ==========
!!
!!           Vector definitions
!!           ------------------
!!           Observer-target position
!!           Observer-target velocity
!!           Near point on target
!!           Constant vector in specified frame
!!
!!        Mean Equator and Equinox of Date
!!        ================================
!!
!!           Bodies and models
!!           -----------------
!!           Earth:  1976 IAU precession model
!!
!!        Mean Ecliptic and Equinox of Date
!!        ================================
!!
!!           Bodies and models
!!           -----------------
!!           Earth:  1976 IAU precession model
!!                   1980 IAU mean obliquity model
!!
!!        True Equator and Equinox of Date
!!        ================================
!!
!!           Bodies and models
!!           -----------------
!!           Earth:  1976 IAU precession model
!!                   1980 IAU nutation model
!!
!!        Euler frames
!!        ============
!!
!!           Euler angle definitions
!!           -----------------------
!!           Polynomial
!!
!!        Product frames
!!        ==============
!!
!!           Product definitions
!!           -------------------
!!           Array of "from" frames
!!           Array of "to" frames
!!
!! ## Examples
!!
!!     See ROTGET.
!!
!! ## Restrictions
!!
!!     1) This is a SPICE private routine; the routine is subject
!!        to change without notice.  User applications should not
!!        call this routine.
!!
!!     2) Many numerical problems can occur when dynamic frames
!!        are evaluated.  Users must determine whether dynamic frame
!!        definitions are suitable for their applications. See the
!!        Exceptions section for a list of possible problems.
!!
!!     3) Two-vector frame definitions can suffer extreme loss of
!!         precision due to near-singular geometry.
!!
!! ## Literature References
!!
!!     None.
!!
!! ## Author and Institution
!!
!!     N.J. Bachman    (JPL)
!!
!! ## Version
!!
!!-    SPICELIB Version 2.0.0, 09-OCT-2021 (NJB) (EDW)
!!
!!        Support for product frames was added.
!!
!!        Body radii accessed from kernel pool using ZZGFTREB.
!!
!!        Bug fix: aberration correction strings for observer-target
!!        velocity vectors are now parsed using ZZVALCOR. This improves
!!        error checking. Note that aberration correction strings for
!!        constant vectors used in two-vector frame specifications are
!!        still parsed using ZZPRSCOR.
!!
!!        Code clean-up: unnecessary call to ZZPRSCOR was removed.
!!
!!        Added and removed some FAILED() calls.
!!
!!-    SPICELIB Version 1.1.0, 24-OCT-2005 (NJB)
!!
!!        Parameters KWX, KWY, KWZ were renamed to KVX, KVY, KVZ.
!!
!!        Call to ZZBODVCD was replaced with call to BODVCD.
!!
!!-    SPICELIB Version 1.0.0, 10-JAN-2005 (NJB)
!!
!!     SPICELIB functions
!!
!!     Local parameters
!!
!!     Local variables
!!
!!     Saved variables
!!
!!     Initial values
!!
      use pepper_kinds, only: ip, wp
      use pepper_arrays, only: cleard, isrchc, moved
      use pepper_math, only: latrec, mxm, mxv, polyds, vequ, vhat, vminus, vsep, vsub, xpose
      use pepper_strings, only: cmprss, eqstr, intstr, ucase
      use pepper_inc_frmtyp
      use pepper_inc_zzabcorr
      use pepper_inc_zzdyn
      implicit none
      real(wp) :: epoch_1(1)  !! one-element copy of epoch (array argument)
      real(wp) :: minsep_1(1)  !! one-element copy of minsep (array argument)
      real(wp) :: dec_1(1)  !! one-element copy of dec (array argument)
      real(wp) :: ra_1(1)  !! one-element copy of ra (array argument)
      real(wp) :: lat_1(1)  !! one-element copy of lat (array argument)
      real(wp) :: lon_1(1)  !! one-element copy of lon (array argument)
      real(wp) :: t0_1(1)  !! one-element copy of t0 (array argument)
      character(*), parameter :: rname = 'ZZDYNROT'
      integer(ip), parameter :: timlen = 50
      integer(ip), parameter :: vnamln = 4
      character(corlen)     abcorr
      character(kvlen)      axname
      character(frnmln)     basnam
      character(frnmln)     cfrmnm
      character(bdnmln)     ctrnam
      character(corlen)     cvcorr
      character(kvlen)      dynstl
      character(kvlen)      dynfam
      character(frnmln)     fframs ( mxnfac )
      character(frnmln)     inname
      character(kvlen)      nutmod
      character(kvlen)      oblmod
      character(kvlen)      prcmod
      character(kvlen)      rotsta
      character(kvlen)      spec
      character(frnmln)     tframs ( mxnfac )
      character(timlen)     timstr
      character(kvlen)      tmpfam
      character(kvlen)      units
      character(kvlen)      vecdef ( 2 )
      character(frnmln)     velfrm
      character(vnamln), parameter :: vname  ( 2 ) = &
         [ character(vnamln) :: kwpri, kwsec ]
      real(wp)              alt
      real(wp)              angles ( 2 )
      real(wp)              coeffs ( maxcof, 3 )
      real(wp)              ctrpos ( 3 )
      real(wp)              dec
      real(wp)              delta
      real(wp)              dirvec ( 3 )
      real(wp)              dmob
      real(wp)              epoch
      real(wp)              eulang ( 3 )
      real(wp)              fet
      real(wp)              lat
      real(wp)              lon
      real(wp)              lt
      real(wp)              minsep
      real(wp)              mob
      real(wp)              nutxf  ( 6, 6 )
      real(wp)              oblr   ( 3, 3 )
      real(wp)              pnear  ( 3 )
      real(wp)              pobs   ( 3 )
      real(wp)              poly   ( 0 : 1 )
      real(wp)              precxf ( 6, 6 )
      real(wp)              ptemp  ( 3 )
      real(wp)              r2000  ( 3, 3 )
      real(wp)              ra
      real(wp)              radii  ( 3 )
      real(wp)              rinv   ( 3, 3 )
      real(wp)              rnut   ( 3, 3 )
      real(wp)              rout   ( 3, 3 )
      real(wp)              rprec  ( 3, 3 )
      real(wp)              rtemp  ( 3, 3 )
      real(wp)              sep
      real(wp)              stemp  ( 6 )
      real(wp)              stobs  ( 6 )
      real(wp)              tipm   ( 3, 3 )
      real(wp)              t0
      real(wp)              v2     ( 3, 2 )
      real(wp)              vet
      real(wp)              vflt
      integer(ip)           axis   ( 2 )
      integer(ip)           cfrmid
      integer(ip)           cvobs
      integer(ip)           degs   ( 3 )
      integer(ip)           frcls
      integer(ip)           frctr
      integer(ip)           frcid
      integer(ip)           frid
      integer(ip)           fromid
      integer(ip)           i
      integer(ip)           iaxes  ( 3 )
      integer(ip)           m
      integer(ip)           n
      integer(ip)           obs
      integer(ip)           targ
      integer(ip)           toid
      logical               corblk ( nabcor )
      logical               fnd
      logical               frozen
      logical               meanec
      logical               meaneq
      logical               negate
      logical               ofdate
      logical               trueeq
      if ( .not. allocated ( ctx%zzdynrot_state ) ) then
         allocate ( ctx%zzdynrot_state )
         call init_zzdynrot_state ( ctx%zzdynrot_state )
      end if
      associate ( axes => ctx%zzdynrot_state%axes, &
                  earth => ctx%zzdynrot_state%earth, &
                  first => ctx%zzdynrot_state%first, &
                  j2000 => ctx%zzdynrot_state%j2000, &
                  itmabc => ctx%zzdynrot_state%itmabc, &
                  itmaxe => ctx%zzdynrot_state%itmaxe, &
                  itmcof => ctx%zzdynrot_state%itmcof, &
                  itmdec => ctx%zzdynrot_state%itmdec, &
                  itmfrm => ctx%zzdynrot_state%itmfrm, &
                  itmlat => ctx%zzdynrot_state%itmlat, &
                  itmlon => ctx%zzdynrot_state%itmlon, &
                  itmobs => ctx%zzdynrot_state%itmobs, &
                  itmra => ctx%zzdynrot_state%itmra, &
                  itmsep => ctx%zzdynrot_state%itmsep, &
                  itmspc => ctx%zzdynrot_state%itmspc, &
                  itmtrg => ctx%zzdynrot_state%itmtrg, &
                  itmunt => ctx%zzdynrot_state%itmunt, &
                  itmvdf => ctx%zzdynrot_state%itmvdf, &
                  itmvec => ctx%zzdynrot_state%itmvec )
      if ( ctx%failed() ) return

      if ( first ) then
!
!        Get the ID code for the J2000 frame.
!
         call irfnum ( ctx, 'J2000', j2000 )

!
!        Get the ID code for the earth (we needn't check the found
!        flag).
!
         call bodn2c ( ctx, 'EARTH', earth, fnd )

!
!        Initialize "item" strings used to create kernel variable
!        names.
!
         do i = 1, 2
!
!           Vector axis:
!
            itmaxe(i) = vname(i) // kwvaxi
!
!           Vector definition:
!
            itmvdf(i) = vname(i) // kwvcdf
!
!           Vector aberration correction:
!
            itmabc(i) = vname(i) // kwvabc
!
!           Vector frame:
!
            itmfrm(i) = vname(i) // kwvfrm
!
!           Vector observer:
!
            itmobs(i) = vname(i) // kwvobs
!
!           Vector target:
!
            itmtrg(i) = vname(i) // kwvtrg
!
!           Vector longitude:
!
            itmlon(i) = vname(i) // kwlon
!
!           Vector latitude:
!
            itmlat(i) = vname(i) // kwlat
!
!           Vector right ascension:
!
            itmra(i)  = vname(i) // kwra
!
!           Vector declination:
!
            itmdec(i) = vname(i) // kwdec
!
!           Vector units:
!
            itmunt(i) = vname(i) // kwunit
!
!           Constant vector coordinate specification:
!
            itmspc(i) = vname(i) // kwvspc
!
!           Constant vector in cartesian coordinates, literal value:
!
            itmvec(i) = vname(i) // kwvect

         end do

         first = .false.

      end if

!
!     Initialize the output arguments.
!
      call cleard ( 9, rotate )

      basfrm = 0

!
!     Initialize certain variables to ensure that we don't do
!     arithmetic operations using bogus, possibly large,
!     undefined values.
!
      call cleard ( 36, nutxf  )
      call cleard (  9, oblr   )
      call cleard ( 36, precxf )
      call cleard (  9, r2000  )
      call cleard (  9, rtemp  )
      call cleard (  9, rinv   )
      call cleard (  9, tipm   )

      mob    = 0.0_wp
      dmob   = 0.0_wp
      t0     = 0.0_wp
      frozen = .false.

!
!     Get the input frame name.
!
      call frmnam ( ctx, infram, inname )

!
!     We need the name of the base frame.
!
      call zzdynfid ( ctx, inname, infram, kwbfrm, basfrm )

      if ( ctx%failed() ) return

      call frmnam   ( ctx, basfrm, basnam )

!
!     The output frame code and name are set.
!
!     Look up the dynamic frame definition style from the kernel pool.
!     The kernel variable's name might be specified by name or ID.
!
      call zzdynvac ( ctx, inname, infram, kwstyl, 1, n, dynstl )

      if ( ctx%failed() ) return

!
!     At this time, the only supported dynamic frame definition style is
!     PARAMETERIZED.
!
      if (  eqstr( dynstl, kvparm )  ) then
!
!        Parameterized dynamic frames belong to families.  Look up
!        the family for this frame.
!
         call zzdynvac ( ctx, inname, infram, kwffam, 1, n, dynfam )

         if ( ctx%failed() ) return

         call cmprss ( ' ', 0, dynfam, tmpfam )
         call ucase  ( tmpfam,         dynfam )

!
!        Determine whether we have an "of-date" frame family.
!        The logical flags used here and respective meanings are:
!
!           MEANEQ   Mean equator and equinox of date
!           TRUEEQ   True equator and equinox of date
!           MEANEC   Mean ecliptic and equinox of date
!
         meaneq  =  dynfam == kvmeqt
         trueeq  =  dynfam == kvteqt
         meanec  =  dynfam == kvmecl

         ofdate  =  meaneq .or. meanec .or. trueeq

!
!        Set the evaluation epoch T0.  Normally this epoch is ET,
!        but if the frame is frozen, the freeze epoch from the
!        frame definition is used.
!
!        Read the freeze epoch into T0 if a freeze epoch was
!        specified; let FROZEN receive the FOUND flag value
!        returned by ZZDYNOAD.
!
         call zzdynoad ( ctx, inname, infram, kwfrez, 1, n, t0_1, frozen )
         t0 = t0_1(1)

         if ( ctx%failed() ) return

         if ( .not. frozen ) then
!
!           Normal case:  just use the input epoch.
!
            t0 = et

         end if

!
!        Look up the rotation state keyword.  In this routine,
!        the rotation state keyword is examined only to support
!        semantic checking:  there's no use made of the fact that
!        the rotation state is 'ROTATING' or 'INERTIAL'.
!
         call zzdynoac ( ctx, inname, infram, kwrsta, 1, n, rotsta, fnd )

         if ( ctx%failed() ) return

         if ( fnd ) then
!
!           Catch invalid rotation states here.
!
            if (      ( .not. eqstr( rotsta, kvrotg ) ) &
                .and. ( .not. eqstr( rotsta, kvinrt ) )  ) then

               call ctx%setmsg ( 'Definition of frame # contains '     // &
                             '# specification #. The only valid '  // &
                             'rotation states are # or #. '        // &
                             'This situation is usually caused by '// &
                             'an error in a frame kernel in which '// &
                             'the frame is defined.'               )
               call ctx%errch  ( '#',  inname                          )
               call ctx%errch  ( '#',  kwrsta                          )
               call ctx%errch  ( '#',  rotsta                          )
               call ctx%errch  ( '#',  kvrotg                          )
               call ctx%errch  ( '#',  kvinrt                          )
               call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'                 )
               return

            end if

         end if
!
!        If the frame is frozen, the rotation state keyword *must be
!        absent*.
!
         if ( frozen .and. fnd ) then

            call ctx%setmsg ( 'Definition of frame # contains both ' // &
                          '# and # '                             // &
                          'keywords; at most one of these must ' // &
                          'be present in the frame definition. ' // &
                          'This situation is usually caused by ' // &
                          'an error in a frame kernel in which ' // &
                          'the frame is defined.'                )
            call ctx%errch  ( '#',  inname                           )
            call ctx%errch  ( '#',  kwfrez                           )
            call ctx%errch  ( '#',  kwrsta                           )
            call ctx%sigerr ( 'SPICE(FRAMEDEFERROR)'                 )
            return

         end if

!
!        If the frame belongs to an "of date" family, either the
!        rotation state must be specified or the frame must be
!        frozen.
!
         if (  ofdate  .and.  (.not. frozen) .and. (.not. fnd )  ) then

            call ctx%setmsg ( 'Definition of frame #, which belongs to ' // &
                          'parameterized dynamic frame family #, '   // &
                          'contains neither # nor # keywords; '      // &
                          'frames in this family require exactly '   // &
                          'one of these in their frame definitions. '// &
                          'This situation is usually caused by '     // &
                          'an error in a frame kernel in which '     // &
                          'the frame is defined.'                    )
            call ctx%errch  ( '#',  inname                               )
            call ctx%errch  ( '#',  dynfam                               )
            call ctx%errch  ( '#',  kwfrez                               )
            call ctx%errch  ( '#',  kwrsta                               )
            call ctx%sigerr ( 'SPICE(FRAMEDEFERROR)'                     )
            return

         end if

!
!        The evaluation epoch T0 is set.
!
!        In this routine, unlike its companion ZZDYNFRM, there is no
!        need to make further reference to the rotation state.  Hence
!        the flag INERT used in ZZDYNFRM doesn't appear here.
!
!        The following code block performs actions specific to
!        the various dynamic frame families.
!
         if ( ofdate ) then
!
!           Fetch the name of the true equator and equinox of date
!           precession model.
!
            call zzdynvac ( ctx, inname, infram, kwprcm, 1, n, prcmod )

            if ( ctx%failed() ) return

!
!           Get the precession transformation.
!
            if (  eqstr( prcmod, kvm001 )  ) then
!
!              This is the 1976 IAU earth precession model.
!
!              Make sure the center of the input frame is the earth.
!
               if ( center /= earth ) then

                  call bodc2n ( ctx, center, ctrnam, fnd )

                  if ( .not. fnd ) then
                     call intstr ( center, ctrnam )
                  end if

                  call ctx%setmsg ( 'Definition of frame # specifies '    // &
                                'frame center # and precession '      // &
                                'model #. This precession model '     // &
                                'is not applicable to body #. '       // &
                                'This situation is usually caused by '// &
                                'an error in a frame kernel in which '// &
                                'the frame is defined.'               )
                  call ctx%errch  ( '#',  inname                          )
                  call ctx%errch  ( '#',  ctrnam                          )
                  call ctx%errch  ( '#',  kvm001                          )
                  call ctx%errch  ( '#',  ctrnam                          )
                  call ctx%sigerr ( 'SPICE(INVALIDSELECTION)'             )
                  return

               end if

!
!              Look up the precession transformation.  Extract
!              the precession rotation matrix.
!
               call zzeprc76 ( ctx, t0, precxf )

               call moved ( precxf(1,1), 3, rprec(1,1) )
               call moved ( precxf(1,2), 3, rprec(1,2) )
               call moved ( precxf(1,3), 3, rprec(1,3) )

!
!              If we're in the mean-of-date case, invert this
!              transformation to obtain the mapping from the
!              mean-of-date frame to J2000.
!
               if ( meaneq ) then
                  call xpose ( rprec, rtemp )
               end if

            else

               call ctx%setmsg ( 'Definition of frame # specifies '    // &
                             'precession model #, which is not '   // &
                             'recognized. '                        // &
                             'This situation is usually caused by '// &
                             'an error in a frame kernel in which '// &
                             'the frame is defined.'               )
               call ctx%errch  ( '#',  inname                          )
               call ctx%errch  ( '#',  prcmod                          )
               call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'                 )
               return

            end if

!
!           At this point the precession transformation REPREC is set.
!           If INFRAM is a mean equator and equinox of date frame, the
!           inverse of REPREC is currently stored in RTEMP.

            if ( trueeq ) then
!
!              We need a nutation transformation as well. Get the name
!              of the nutation model.
!
               call zzdynvac ( ctx, inname, infram, kwnutm, 1, n, nutmod )

               if ( ctx%failed() ) return

!
!              Get the nutation transformation.
!
               if (  eqstr( nutmod, kvm002 )  ) then
!
!                 This is the 1980 IAU earth nutation model.
!
!                 Make sure the center is the earth.
!
                  if ( center /= earth ) then

                     call bodc2n ( ctx, center, ctrnam, fnd )

                     if ( .not. fnd ) then
                        call intstr ( center, ctrnam )
                     end if

                     call ctx%setmsg ( 'Definition of frame # specifies ' // &
                                   'frame center # and nutation '     // &
                                   'model #. This '                   // &
                                   'nutation model is not applicable '// &
                                   'to body #.  This situation is '   // &
                                   'usually caused by an error in a ' // &
                                   'frame kernel in which '           // &
                                   'the frame is defined.'            )
                     call ctx%errch  ( '#',  inname                       )
                     call ctx%errch  ( '#',  ctrnam                       )
                     call ctx%errch  ( '#',  kvm002                       )
                     call ctx%errch  ( '#',  ctrnam                       )
                     call ctx%sigerr ( 'SPICE(INVALIDSELECTION)'          )
                     return

                  end if

!
!                 Look up the nutation transformation.  Extract
!                 the nutation rotation matrix.
!
                  call zzenut80 ( ctx, t0, nutxf )

                  call moved ( nutxf(1,1), 3, rnut(1,1) )
                  call moved ( nutxf(1,2), 3, rnut(1,2) )
                  call moved ( nutxf(1,3), 3, rnut(1,3) )

!
!                 Find the rotation from the J2000 frame to the earth
!                 true of date frame.  Invert.
!
                  call mxm   ( rnut, rprec, rinv )
                  call xpose ( rinv, rtemp )

               else

                  call ctx%setmsg ( 'Definition of frame # specifies '    // &
                                'nutation model #, which is not '     // &
                                'recognized. '                        // &
                                'This situation is usually caused by '// &
                                'an error in a frame kernel in which '// &
                                'the frame is defined.'               )
                  call ctx%errch  ( '#',  inname                          )
                  call ctx%errch  ( '#',  nutmod                          )
                  call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'                 )
                  return

               end if

            else if ( meanec ) then
!
!              We need a mean obliquity transformation as well.
!              Get the name of the obliquity model.
!
               call zzdynvac ( ctx, inname, infram, kwobqm, 1, n, oblmod )

               if ( ctx%failed() ) return

!
!              Get the obliquity transformation.
!
               if (  eqstr( oblmod, kvm003 )  ) then
!
!                 This is the 1980 IAU earth mean obliquity of
!                 date model.
!
!                 Make sure the center is the earth.
!
                  if ( center /= earth ) then

                     call bodc2n ( ctx, center, ctrnam, fnd )

                     if ( .not. fnd ) then
                        call intstr ( center, ctrnam )
                     end if

                     call ctx%setmsg ( 'Definition of frame # specifies ' // &
                                   'frame center # and obliquity '    // &
                                   'model #.  This obliquity model '  // &
                                   'is not applicable to body #. '    // &
                                   'This situation is '               // &
                                   'usually caused by an error in a ' // &
                                   'frame kernel in which '           // &
                                   'the frame is defined.'            )
                     call ctx%errch  ( '#',  inname                       )
                     call ctx%errch  ( '#',  ctrnam                       )
                     call ctx%errch  ( '#',  kvm003                       )
                     call ctx%errch  ( '#',  ctrnam                       )
                     call ctx%sigerr ( 'SPICE(INVALIDSELECTION)'          )
                     return

                  end if

!
!                 Create the obliquity transformation.
!                 First look up the obliquity state.
!
                  call zzmobliq ( ctx, t0, mob, dmob )

!
!                 The obliquity rotation is about the mean-of-date
!                 x-axis.  The other Euler angles are identically
!                 zero; the axes are arbitrary, as long as the
!                 middle axis is distinct from the other two.
!
                  call eul2m ( ctx, 0.0_wp, 0.0_wp, mob, 1, 3, 1, oblr )

!
!                 Find the rotation from the J2000 to the
!                 earth mean ecliptic of date frame.  Invert.
!
                  call mxm   ( oblr, rprec, rinv )
                  call xpose ( rinv, rtemp )

               else

                  call ctx%setmsg ( 'Definition of frame # specifies '    // &
                                'obliquity model #, which is not '    // &
                                'recognized. '                        // &
                                'This situation is usually caused by '// &
                                'an error in a frame kernel in which '// &
                                'the frame is defined.'               )
                  call ctx%errch  ( '#',  inname                          )
                  call ctx%errch  ( '#',  oblmod                          )
                  call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'                 )
                  return

               end if

            end if

!
!           At this point, RTEMP contains the rotation from the
!           specified mean of date or true of date frame to J2000.
!
!           If the base frame is not J2000, we must find the
!           transformation from J2000 to the base frame.

            if ( basfrm /= j2000 ) then

               call zzrefch0 ( ctx, j2000,  basfrm, t0,  r2000  )
               call mxm      ( r2000,  rtemp,       rotate )

            else
!
!              Otherwise, RTEMP is the matrix we want.
!
               call moved ( rtemp, 9, rotate )

            end if
!
!           Now ROTATE is the state transformation mapping from
!           the input frame INFRAM to the base frame BASFRM.
!
!           This is the end of the work specific to "of-date" frames.
!           From here we drop out of the IF block.
!

         else if ( dynfam == kv2vec ) then
!
!           The frame belongs to the TWO-VECTOR family.
!
!           Fetch the specifications of the primary and secondary
!           axes.
!
            call cleard ( 6, v2 )

            do i = 1, 2
!
!              Get the name of the axis associated with the Ith
!              defining vector.
!
               call zzdynvac ( ctx, inname, infram, itmaxe(i), 1, n, axname )

               if ( ctx%failed() ) return

               call cmprss ( ' ', 0, axname, axname )
               call ucase  ( axname,         axname )

!
!              Set the sign flag associated with the axis.
!
               negate  =  axname(1:1) == '-'

               call cmprss ( '-', 0, axname, axname )
               call cmprss ( '+', 0, axname, axname )

               axis(i) = isrchc ( axname, 3, axes )

               if ( axis(i) == 0 ) then

                  call ctx%setmsg ( 'Definition of frame # associates '   // &
                                'vector # with axis #.  The only '    // &
                                'valid axis values are { X, -X, Y, '  // &
                                '-Y, Z, -Z }. '                       // &
                                'This situation is usually caused by '// &
                                'an error in a frame kernel in which '// &
                                'the frame is defined.'               )
                  call ctx%errch  ( '#',  inname                          )
                  call ctx%errint ( '#',  i                               )
                  call ctx%errch  ( '#',  axname                          )
                  call ctx%sigerr ( 'SPICE(INVALIDAXIS)'                  )
                  return

               end if

!
!              Find out how the vector is defined:
!
!                 - Observer-target position vector
!                 - Observer-target velocity vector
!                 - Observer-target near point vector
!                 - Constant vector
!
!              VECDEF(I) indicates the vector definition method
!              for the Ith vector.
!
               call zzdynvac ( ctx, inname,  infram,  itmvdf(i), &
                               1,       n,       vecdef(i)   )

               if ( ctx%failed() ) return

               call cmprss ( ' ',    0, vecdef(i), vecdef(i) )
               call ucase  ( vecdef(i),            vecdef(i) )

               if (  vecdef(i) == kvposv  ) then
!
!                 The vector is the position of a target relative
!                 to an observer.
!
!                 We need a target, observer, and aberration correction.
!
                  call zzdynbid ( ctx, inname, infram, itmtrg(i), targ )

                  call zzdynbid ( ctx, inname, infram, itmobs(i), obs  )

                  call zzdynvac ( ctx, inname, infram, itmabc(i), &
                                  1,      n,      abcorr     )

!
!                 Look up the Ith position vector in the J2000 frame.
!
                  call zzspkzp0 ( ctx, targ,    t0,   'J2000', &
                                  abcorr,  obs,  v2(1,i),  lt )

                  if ( ctx%failed() ) return

!
!                 At this point, V2(*,I) contains position relative to
!                 frame J2000.
!

               else if ( vecdef(i) == kvvelv ) then
!
!                 The vector is the velocity of a target relative
!                 to an observer.
!
!                 We need a target, observer, and aberration correction.
!
                  call zzdynbid ( ctx, inname, infram, itmtrg(i), targ )

                  call zzdynbid ( ctx, inname, infram, itmobs(i), obs  )

                  call zzdynvac ( ctx, inname, infram, itmabc(i), &
                                  1,      n,      abcorr     )

!
!                 We need to know the frame in which the velocity is
!                 defined.
!
                  call zzdynfid ( ctx, inname, infram, itmfrm(i), frid )
                  call frmnam   ( ctx, frid,   velfrm  )

!
!                 Look up the Ith velocity vector in the velocity frame.
!
                  call zzspkez0 ( ctx, targ,    t0,   velfrm, &
                                  abcorr,  obs,  stemp,  lt )

                  if ( ctx%failed() ) return

!
!                 We'll work with the unit velocity vector.
!
                  call vhat ( stemp(4), v2(1,i) )

!
!                 We need the epoch VET at which VELFRM is evaluated.
!                 This epoch will be used to transform the velocity
!                 vector from VELFRM to J2000.
!
!                 Set the default value of VET here.
!
                  vet = t0

!
!                 Parse the aberration correction.  Capture the
!                 epoch used to evaluate the velocity vector's frame.
!
                  call zzvalcor ( ctx, abcorr, corblk )

                  if ( ctx%failed() ) return

                  if ( corblk(ltidx) ) then
!
!                    Light time correction is used.  The epoch used
!                    to evaluate the velocity vector's frame depends
!                    on the frame's observer and center.
!
!                    Look up the velocity frame's center.
!
                     call frinfo ( ctx, frid, frctr, frcls, frcid, fnd )

                     if ( .not. fnd ) then

                        call ctx%setmsg ( 'In definition of frame #, ' // &
                                      'the frame associated with ' // &
                                      'a velocity vector has '     // &
                                      'frame ID code #, but no '   // &
                                      'frame center, frame class, '// &
                                      'or frame class ID was '     // &
                                      'found by FRINFO.  This '    // &
                                      'situation MAY be caused '   // &
                                      'by an error in a frame '    // &
                                      'kernel in which the frame ' // &
                                      'is defined. The problem '   // &
                                      'also could be indicative '  // &
                                      'of a SPICELIB bug.'         )
                        call ctx%errch  ( '#',  inname                 )
                        call ctx%errint ( '#',  frid                   )
                        call ctx%sigerr ( 'SPICE(FRAMEDATANOTFOUND)'   )
                        return

                     end if

                     if ( frcls /= inertl ) then
!
!                       Obtain light time from the observer to the
!                       frame's center.
!
                        call zzspkzp0 ( ctx, frctr,  t0,  'J2000', &
                                        abcorr, obs, ctrpos,  vflt )
                        call zzcorepc ( ctx, abcorr, t0,  vflt,    vet  )

                        if ( ctx%failed() ) return

                     end if

                  end if

!
!                 The velocity frame evaluation epoch VET is now set.
!
!                 We must rotate the velocity vector from the velocity
!                 frame (evaluated at VET) to the output frame at T0.
!                 We'll do this in two stages, first mapping velocity
!                 into the J2000 frame.
!
                  if ( frid /= j2000 ) then

                     call zzrefch0 ( ctx, frid,   j2000,  vet,      r2000 )

                     if ( ctx%failed() ) return

                     call mxv      ( r2000,          v2(1,i),  ptemp )
                     call moved    ( ptemp,  3,      v2(1,i)         )

                  end if

!
!                 At this point, V2(*,I) contains velocity
!                 relative to frame J2000.

               else if ( vecdef(i) == kvnear ) then
!
!                 The vector points from an observer to the near
!                 point to the observer on the target body.
!
!                 We need a target, observer, and aberration correction.
!
                  call zzdynbid ( ctx, inname, infram, itmtrg(i), targ )

                  call zzdynbid ( ctx, inname, infram, itmobs(i), obs  )

                  call zzdynvac ( ctx, inname, infram, itmabc(i), &
                                  1,      n,      abcorr     )

!
!                 The vector points from an observer to the
!                 sub-observer point (nearest point to the observer) on
!                 the target body.  We need the position of the near
!                 point relative to the observer.
!
!                 We'll look up the position of the target center
!                 relative to the observer, as well as the position of
!                 the near point relative to the target center, both in
!                 the body-fixed frame associated with the target.
!
!                 Look up the body-fixed frame associated with the
!                 target body.
!
                  call cidfrm ( ctx, targ, cfrmid, cfrmnm, fnd )

                  if ( .not. fnd ) then

                     call ctx%setmsg ( 'Definition of frame # requires ' // &
                                   'definition of body-fixed frame ' // &
                                   'associated with target body #. ' // &
                                   'A call to CIDFRM indicated no '  // &
                                   'body-fixed frame is associated ' // &
                                   'with the target body.  This '    // &
                                   'situation can arise when a '     // &
                                   'frame kernel defining the '      // &
                                   'target''s body-fixed frame  '    // &
                                   'lacks the OBJECT_<ID>_FRAME '    // &
                                   'or OBJECT_<name>_FRAME '         // &
                                   'keywords.  The problem also '    // &
                                   'could be caused by an '          // &
                                   'error in a frame kernel in '     // &
                                   'which the parameterized '        // &
                                   'two-vector dynamic frame # '     // &
                                   'is defined.'                     )
                     call ctx%errch  ( '#',  inname                      )
                     call ctx%errint ( '#',  targ                        )
                     call ctx%errch  ( '#',  inname                      )
                     call ctx%sigerr ( 'SPICE(FRAMEDATANOTFOUND)'        )
                     return

                  end if

!
!                 Get the radii of the target body.
!
                  call zzgftreb ( ctx, targ, radii )

                  if ( ctx%failed() ) return

!
!                 Look up the Ith position vector in the target-fixed
!                 frame.  Negate the vector to obtain the target-to-
!                 observer vector.
!
                  call zzspkzp0 ( ctx, targ,   t0,   cfrmnm, &
                                  abcorr, obs,  ptemp,   lt )

!
!                 We check FAILED() here because VMINUS is a simple
!                 arithmetic routine that doesn't return on entry.
!
                  if ( ctx%failed() ) return

                  call vminus   ( ptemp,  pobs )

                  call nearpt   ( ctx, pobs,   radii(1), radii(2), radii(3), &
                                  pnear,  alt                          )

                  if ( ctx%failed() ) return

!
!                 Find the observer-near point vector in the current
!                 frame CFRMNM.
!
                  call vsub ( pnear, pobs, ptemp )

!
!                 Rotate the vector to frame J2000.  To get the required
!                 rotation matrix, we'll need to obtain the epoch
!                 associated with CNMFRM.  Parse the aberration
!                 correction and adjust the frame evaluation epoch as
!                 needed.
!
                  call zzcorepc ( ctx, abcorr, t0, lt, fet )

!
!                 Obtain the matrix for transforming position vectors
!                 from the target center frame to the J2000 frame and
!                 apply it to the observer-to-near point position
!                 vector.
!
                  call zzrefch0 ( ctx, cfrmid, j2000, fet, tipm )

                  if ( ctx%failed() ) return

                  call mxv ( tipm, ptemp, v2(1,i) )

!
!                 At this point, V2(*,I) contains position of the near
!                 point on the target as seen by the observer, relative
!                 to frame J2000.
!

               else if ( vecdef(i) == kvcons ) then
!
!                 The vector is constant in a specified frame.
!
!                 We need a 3-vector and an associated reference
!                 frame relative to which the vector is specified.
!
!                 Look up the ID of the frame first.
!
                  call zzdynfid ( ctx, inname, infram, itmfrm(i), frid )

!
!                 Let FET ("frame ET") be the evaluation epoch for
!                 the constant vector's frame.  By default, this
!                 frame is just T0, but if we're using light time
!                 corrections, FET must be adjusted for one-way
!                 light time between the frame's center and the
!                 observer.
!
!                 Set the default value of FET here.
!
                  fet = t0

!
!                 Optionally, there is an aberration correction
!                 associated with the constant vector's frame.
!                 If so, an observer must be associated with the
!                 frame.  Look up the correction first.
!
                  call zzdynoac ( ctx, inname, infram, itmabc(i), &
                                  1,      n,      cvcorr,    fnd )

                  if ( ctx%failed() ) return

                  if ( .not. fnd ) then
                     cvcorr = 'NONE'
                  end if

                  call zzprscor ( ctx, cvcorr, corblk )

                  if ( ctx%failed() ) return

                  if ( .not. corblk(geoidx) ) then
!
!                    We need to apply an aberration correction to
!                    the constant vector.
!
!                    Check for errors in the aberration correction
!                    specification.
!
!                       - Light time and stellar aberration corrections
!                         are mutually exclusive.
!
                     if ( corblk(ltidx) .and. corblk(stlidx) ) then

                        call ctx%setmsg ( 'Definition of frame # '       // &
                                      'specifies aberration '        // &
                                      'correction # for constant '   // &
                                      'vector.  Light time and '     // &
                                      'stellar aberration '          // &
                                      'corrections are mutually '    // &
                                      'exclusive for constant '      // &
                                      'vectors used in two-vector '  // &
                                      'parameterized dynamic frame ' // &
                                      'definitions.  This situation '// &
                                      'is usually caused by an '     // &
                                      'error in a frame kernel in '  // &
                                      'which the frame is defined.'  )
                        call ctx%errch  ( '#',  inname                   )
                        call ctx%errch  ( '#',  cvcorr                   )
                        call ctx%sigerr ( 'SPICE(INVALIDOPTION)'         )
                        return

                     end if

                     if ( corblk(ltidx) ) then
!
!                       Light time correction is used.  The epoch used
!                       to evaluate the constant vector's frame depends
!                       on the frame's observer and center.
!
!                       Look up the constant vector frame's center.
!
                        call frinfo ( ctx, frid, frctr, frcls, frcid, fnd )

                        if ( .not. fnd ) then

                           call ctx%setmsg ( 'In definition of frame #, ' // &
                                         'the frame associated with ' // &
                                         'a constant vector has '     // &
                                         'frame ID code #, but no '   // &
                                         'frame center, frame class, '// &
                                         'or frame class ID was '     // &
                                         'found by FRINFO.  This '    // &
                                         'situation MAY be caused '   // &
                                         'by an error in a frame '    // &
                                         'kernel in which the frame ' // &
                                         'is defined. The problem '   // &
                                         'also could be indicative '  // &
                                         'of a SPICELIB bug.'         )
                           call ctx%errch  ( '#',  inname                 )
                           call ctx%errint ( '#',  frid                   )
                           call ctx%sigerr ( 'SPICE(FRAMEDATANOTFOUND)'   )
                           return

                        end if

                        if ( frcls /= inertl ) then
!
!                          Look up the observer associated with the
!                          constant vector's frame.  This observer,
!                          together with the frame's center, determines
!                          the evaluation epoch for the frame.
!
                           call zzdynbid ( ctx, inname,    infram, &
                                           itmobs(i), cvobs   )

!
!                          Obtain light time from the observer to the
!                          frame's center.
!
                           call zzspkzp0 ( ctx, frctr,  t0,   'J2000', &
                                           cvcorr, cvobs, ctrpos,  lt )

!
!                          Find the evaluation epoch for the frame.
!
                           call zzcorepc ( ctx, cvcorr, t0, lt, fet )

                           if ( ctx%failed() ) return

                        end if

                     else if ( corblk(stlidx) ) then
!
!                       Stellar aberration case.
!
!                       The constant vector must be corrected for
!                       stellar aberration induced by the observer's
!                       velocity relative to the solar system
!                       barycenter.  First, find this velocity in
!                       the J2000 frame.  We'll apply the correction
!                       later, when the constant vector has been
!                       transformed to the J2000 frame.
!
                        call zzdynbid ( ctx, inname,    infram, &
                                        itmobs(i), cvobs   )

                        call zzspksb0 ( ctx, cvobs, t0, 'J2000', stobs )

                        if ( ctx%failed() ) return

                     end if

                  end if

!
!                 Get the constant vector specification.
!
                  call zzdynvac ( ctx, inname,  infram,  itmspc(i), &
                                  1,       n,       spec       )

                  if ( ctx%failed() ) return

                  call cmprss ( ' ', 0, spec, spec )
                  call ucase  ( spec,         spec )

                  if ( spec == kvrecc ) then
!
!                    The coordinate system is rectangular.
!
!                    Look up the constant vector.
!
                     call zzdynvad ( ctx, inname, infram, itmvec(i), &
                                     3,      n,      dirvec    )

                  else if (      ( spec == kvlatc ) &
                            .or. ( spec == kvradc ) ) then
!
!                    The coordinate system is latitudinal or RA/DEC.
!
!                    Look up the units associated with the angles.
!
                     call zzdynvac ( ctx, inname,  infram,  itmunt(i), &
                                     1,       n,       units     )

                     if ( spec == kvlatc ) then
!
!                       Look up longitude and latitude.
!
                        call zzdynvad ( ctx, inname, infram, itmlon(i), 1, n, lon_1 )
                        lon = lon_1(1)

                        call zzdynvad ( ctx, inname, infram, itmlat(i), 1, n, lat_1 )
                        lat = lat_1(1)

!
!                       Convert angles from input units to radians.
!
                        call convrt ( ctx, lon, units, kvradn, angles(1) )
                        call convrt ( ctx, lat, units, kvradn, angles(2) )

                     else
!
!                       Look up RA and DEC.
!
                        call zzdynvad ( ctx, inname, infram, itmra(i), 1, n, ra_1 )
                        ra = ra_1(1)

                        call zzdynvad ( ctx, inname, infram, itmdec(i), 1, n, dec_1 )
                        dec = dec_1(1)

!
!                       Convert angles from input units to radians.
!
                        call convrt ( ctx, ra,  units, kvradn, angles(1) )
                        call convrt ( ctx, dec, units, kvradn, angles(2) )

                     end if

                     if ( ctx%failed() ) return

!
!                    Now  produce a direction vector.
!
                     call latrec ( 1.0_wp, angles(1), angles(2), dirvec )

                  else

                     call ctx%setmsg ( 'Definition of two-vector '       // &
                                   'parameterized dynamic frame # '  // &
                                   'includes constant vector '       // &
                                   'specification #, which is '      // &
                                   'not supported.  This situation ' // &
                                   'is usually caused by an error '  // &
                                   'in a frame kernel in which '     // &
                                   'the frame is defined.'           )
                     call ctx%errch  ( '#',  inname                      )
                     call ctx%errch  ( '#',  spec                        )
                     call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'             )
                     return

                  end if

!
!                 At this point, the Cartesian coordinates of the
!                 vector relative to the constant vector frame
!                 are stored in DIRVEC.
!
                  if ( frid == j2000 ) then

                     call vequ ( dirvec, v2(1,i) )

                  else
!
!                    Convert the direction vector to the J2000 frame.
!
                     call zzrefch0 ( ctx, frid, j2000, fet, r2000 )

                     if ( ctx%failed() ) return

                     call mxv ( r2000, dirvec, v2(1,i) )

                  end if

!
!                 The constant vector is now represented
!                 in the J2000 frame, but we may still need to
!                 apply a stellar aberration correction.
!
                  if ( corblk(stlidx) ) then
!
!                    Perform the correction appropriate to the
!                    radiation travel sense.
!
                     if ( corblk(xmtidx) ) then
!
!                       The correction is for transmission.
!
                        call stlabx ( ctx, v2(1,i), stobs(4), ptemp )

                     else
!
!                       The correction is for reception.
!
                        call stelab ( ctx, v2(1,i), stobs(4), ptemp )

                     end if

                     if ( ctx%failed() ) return

                     call vequ ( ptemp, v2(1,i) )

                  end if
!
!                 At this point, V2(*,I) contains the constant
!                 (constant relative to its associated frame, that is)
!                 vector as seen by the observer, relative to frame
!                 J2000.
!

               else

                  call ctx%setmsg ( 'Definition of two-vector '       // &
                                'parameterized dynamic frame # '  // &
                                'includes vector '                // &
                                'definition #, which is '         // &
                                'not supported.  This situation ' // &
                                'is usually caused by an error '  // &
                                'in a frame kernel in which '     // &
                                'the frame is defined.'           )
                  call ctx%errch  ( '#',  inname                      )
                  call ctx%errch  ( '#',  vecdef(i)                   )
                  call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'             )
                  return

               end if

!
!              Negate the vector if the axis has negative sign.
!
               if ( negate ) then
                  call vminus ( v2(1,i),     ptemp   )
                  call moved  ( ptemp,   3,  v2(1,i) )
               end if

            end do

!
!           Look up the lower bound for the angular separation of
!           the defining vectors.  Use the default value if none
!           was supplied.
!
            call zzdynoad ( ctx, inname, infram, itmsep, 1, n, minsep_1, fnd )
            minsep = minsep_1(1)

            if ( ctx%failed() ) return

            if ( .not. fnd ) then
               minsep = lbsep
            end if

!
!           Now use our vectors to compute our position transformation
!           matrix.
!
!           Check the angular separation of the defining vectors. We
!           want to ensure that the vectors are not too close to being
!           linearly dependent.  We can handle both cases---separation
!           close to 0 or separation close to Pi---by comparing the
!           sine of the separation to the sine of the separation limit.
!
            sep = vsep ( v2(1,1), v2(1,2) )

            if ( sin(sep) < sin(minsep) ) then

               call etcal  ( ctx, t0, timstr )

               call ctx%setmsg ( 'Angular separation of vectors defining '// &
                             'two-vector parameterized dynamic frame '// &
                             '# is # (radians); minimum allowed diff' // &
                             'erence of separation from 0 or Pi is # '// &
                             'radians.  Evaluation epoch is #.  Extre'// &
                             'me loss of precision can occur when '   // &
                             'defining vectors are nearly linearly '  // &
                             'dependent.  This type of error can be ' // &
                             'due to using a dynamic frame outside of'// &
                             ' the time range for which it is meant. '// &
                             'It also can be due to a conceptual '    // &
                             'error pertaining to the frame''s defin' // &
                             'ition, or to an implementation error '  // &
                             'in the frame kernel containing the '    // &
                             'frame definition. However, if you wish '// &
                             'to proceed with this computation, '     // &
                             'the # keyword can be used in '          // &
                             'the frame definition to adjust the '    // &
                             'separation limit.'                      )
               call ctx%errch  ( '#',  inname                             )
               call ctx%errdp  ( '#',  sep                                )
               call ctx%errdp  ( '#',  minsep                             )
               call ctx%errch  ( '#',  timstr                             )
               call ctx%errch  ( '#',  kwatol                             )
               call ctx%sigerr ( 'SPICE(DEGENERATECASE)'                  )
               return

            end if
!
!           We have both positions expressed relative to frame J2000
!           at this point.  Find the transformation from INNAME to
!           the frame J2000, then from J2000 to frame BASNAM.
!
            call twovec   ( ctx, v2(1,1), axis(1), v2(1,2), axis(2), rinv )

            call xpose    ( rinv,    rotate )

            if ( basfrm /= j2000 ) then

               call moved ( rotate, 9, rtemp )

               call zzrefch0 ( ctx, j2000,   basfrm,  t0,  r2000  )

               if ( ctx%failed() ) return

               call mxm ( r2000, rtemp, rotate )

            end if

!
!           ROTATE is set.
!
!           This is the end of the work specific to two-vector frames.
!           From here we drop out of the IF block.
!

         else if ( dynfam == kveulr ) then
!
!           The frame belongs to the Euler family.
!
!           We expect to specifications of an axis sequence, units,
!           and angles via polynomial coefficients.  We also expect
!           to see an ET epoch.
!
!           Look up the epoch first.  Let DELTA represent the offset
!           of T0 relative to the epoch.
!
!           Initialize EPOCH so subtraction doesn't overflow if EPOCH
!           is invalid due to a lookup error.
!
            epoch = 0.0_wp

            call zzdynvad ( ctx, inname, infram, kwepoc, 1, n, epoch_1 )
            epoch = epoch_1(1)

            delta = t0 - epoch

!
!           Now the axis sequence.
!
            call zzdynvai ( ctx, inname, infram, kweuax, 3, n, iaxes )

!
!           Now the coefficients for the angles.
!
            do i = 1, 3
!
!              Initialize N so subtraction doesn't overflow if N
!              is invalid due to a lookup error.
!
               n = 0

               call zzdynvad ( ctx, inname,  infram,  itmcof(i), &
                               maxcof,  n,       coeffs(1,i) )

!
!              Set the polynomial degree for the Ith angle.
!
               degs(i) = n - 1

            end do

!
!           Look up the units associated with the angles.
!
            call zzdynvac ( ctx, inname, infram, kwunit, 1, n, units )

            if ( ctx%failed() ) return

!
!           Evaluate the angles at DELTA.  Convert angles from input
!           units to radians.
!
            do i = 1, 3

               call polyds ( coeffs(1,i), degs(i), 0, delta, poly )
!
!              Convert units.  Fill in the Euler angle vector.
!
               call convrt ( ctx, poly(0), units, kvradn, eulang(i) )

            end do

!
!           Produce a position transformation matrix that maps from
!           the defined frame to the base frame.
!
            call eul2m ( ctx, eulang(1), eulang(2), eulang(3), &
                         iaxes(1),  iaxes(2),  iaxes(3),  rotate )

!
!           This is the end of the work specific to Euler frames.
!           From here we drop out of the IF block.
!

         else if ( dynfam == kvprod ) then
!
!           The frame belongs to the product family.
!
!           We expect to see specifications of "from" and "to" frames
!           that make up the product.
!
!           Look up the "from" and "to" frames that define the product.
!
            call zzdynvac ( ctx, inname, infram, kwffrm, mxnfac, m, fframs )
            call zzdynvac ( ctx, inname, infram, kwtfrm, mxnfac, n, tframs )

            if ( ctx%failed() ) return

            if ( n /= m ) then

               call ctx%setmsg ( 'Definition of product ' &
               //            'parameterized dynamic frame # ' &
               //            'has # "from" frames and # "to" ' &
               //            'frames. These counts must match.' )
               call ctx%errch  ( '#',  inname                       )
               call ctx%errint ( '#',  m                            )
               call ctx%errint ( '#',  n                            )
               call ctx%sigerr ( 'SPICE(BADFRAMECOUNT)'             )
               return

            end if

!
!           The product of the factors is the transformation from the
!           base frame to the output frame. We need the inverse of
!           this transformation.
!
!           Compute the inverse of the product of the factors; place
!           the result in ROTATE.
!
            call namfrm ( ctx, fframs(1), fromid )
            call namfrm ( ctx, tframs(1), toid   )

            call zzrefch0 ( ctx, toid, fromid, t0, rotate )

            if ( ctx%failed() ) return

            do i = 2, n

               call moved  ( rotate, 9,  rtemp )

               call namfrm ( ctx, fframs(i), fromid )
               call namfrm ( ctx, tframs(i), toid   )

               call zzrefch0 ( ctx, toid, fromid, t0, rotate )

               if ( ctx%failed() ) return

               call mxm   ( rotate, rtemp, rout   )
               call moved ( rout,  9,      rotate )

            end do

         else

            call ctx%setmsg ( 'Dynamic frame family # (in definition of '// &
                          'frame #) is not supported. '              // &
                          'This situation is usually caused by '     // &
                          'an error in a frame kernel in which '     // &
                          'the frame is defined.'                    )
            call ctx%errch  ( '#',  dynfam                               )
            call ctx%errch  ( '#',  inname                               )
            call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'                      )
            return

         end if
!
!        This is the end of the IF block that processes the
!        parameterized dynamic frame families.
!

      else

         call ctx%setmsg ( 'Dynamic frame style # (in definition of ' // &
                       'frame #) is not supported. '              // &
                       'This situation is usually caused by '     // &
                       'an error in a frame kernel in which '     // &
                       'the frame is defined.'                    )
         call ctx%errch  ( '#',  dynstl                               )
         call ctx%errch  ( '#',  inname                               )
         call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'                      )
         return

      end if

!
!     At this point ROTATE is the position transformation matrix
!     mapping from the input frame INFRAM to the base frame BASFRM.
!
!     ROTATE and BASFRM is set.
!
      return
      end associate
      end procedure zzdynrot

end submodule pepper_dynamic
