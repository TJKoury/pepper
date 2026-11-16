!> Frame transformations: [[frmget]] and [[rotget]] (one frame to its base), [[frmchg]] and [[refchg]] (between any two frames), [[pxform]] and [[sxform]].
!>
!> The procedures keep SPICELIB's names and documentation; the context is
!> their first argument (interfaces and type-bound procedures in
!> [[pepper_instance_module]]).
submodule (pepper_instance_module) pepper_transforms

   implicit none


   ! Parameters shared by this file's procedures (PEPPER: SPICELIB
   ! declared them in each routine that used them).
   integer(ip), parameter :: maxnod = 10  !! maximum number of nodes of a frame tree
   integer(ip), parameter :: maxxfm = maxnod + 4  !! maximum number of transformations of a chain
   integer(ip), parameter :: rootf = 1  !! index of the root frame

contains

      !> Find the transformation from a user specified frame to
      !> another frame at a user specified epoch.
      !>
      !>## Detailed Input
      !>
      !>     INFRM    is the SPICE ID-code for some reference frame.
      !>
      !>     ET       is an epoch in ephemeris seconds past J2000 at
      !>              which the user wishes to retrieve a state
      !>              transformation matrix.
      !>
      !>## Detailed Output
      !>
      !>     XFORM    is a 6x6 matrix that transforms states relative to
      !>              INFRM to states relative to OUTFRM. (Assuming such a
      !>              transformation can be found.)
      !>
      !>     OUTFRM   is the SPICE ID-code of a reference frame. The 6x6
      !>              matrix XFORM transforms states relative to INFRM to
      !>              states relative to OUTFRM. The state transformation
      !>              is achieved by multiplying XFORM on the right by a
      !>              state relative to INFRM. This is easily accomplished
      !>              via the subroutine call shown below.
      !>
      !>                 CALL MXVG ( XFORM, STATE, 6, 6, OSTATE )
      !>
      !>     FOUND    is a logical flag indicating whether or not a
      !>              transformation matrix could be found from INFRM to
      !>              some other frame. If a transformation matrix cannot
      !>              be found OUTFRM will be set to zero, FOUND will be
      !>              set to .FALSE. and XFORM will be returned as the zero
      !>              matrix.
      !>
      !>## Parameters
      !>
      !>     See include file.
      !>
      !>## Exceptions
      !>
      !>     1)  If a transformation matrix cannot be located, then FOUND will
      !>         be set to .FALSE., OUTFRM will be set to zero and XFORM will
      !>         be set to the zero 6x6 matrix.
      !>
      !>     2)  If the class of the requested frame is not recognized the
      !>         exception, the error SPICE(UNKNOWNFRAMETYPE) is signaled.
      !>
      !>## Files
      !>
      !>     LSK, SCLK, PCK, FK, SPK, and/or CK kernels may need to be loaded
      !>     to provide the needed frame definition and transformation data.
      !>
      !>## Particulars
      !>
      !>     This is a low level routine used for determining a chain of state
      !>     transformation matrices from one frame to another.
      !>
      !>## Examples
      !>
      !>     See FRMCHG.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     B.V. Semenov       (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 5.0.0, 15-MAR-2021 (NJB) (JDR)
      !>
      !>        **Updated shadow routines ZZFRMGT0 and ZZFRMGT1, as must be
      !>          done each time this routine is updated.**
      !>
      !>        Support for switch frames was added. VERSN is now
      !>        initialized via a DATA statement. Corrected long error
      !>        message to use the term "class" rather than "class id-code."
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 4.0.0, 05-JAN-2014 (BVS)
      !>
      !>        To prevent operations with un-initialized DP numbers, wrapped
      !>        IF ( .NOT. FAILED() ) ... END IF around output matrix
      !>        transposition/re-assignment operations in all branches where
      !>        the routine returning the matrix might fail.
      !>
      !>        Added zero-ing out the output matrix in cases of a failed or
      !>        .NOT. FOUND transformation look ups.
      !>
      !>-    SPICELIB Version 3.0.0, 18-DEC-2004 (NJB)
      !>
      !>        Added the new frame type 'DYN' to the list of frame
      !>        types recognized by FRMGET.
      !>
      !>-    SPICELIB Version 2.0.0, 03-APR-1997 (WLT)
      !>
      !>        Added the new frame type 'TK' to the list of frame
      !>        types recognized by FRMGET. In addition the routine
      !>        now checks FAILED after "getting" the frame transformation.
      !>
      !>-    SPICELIB Version 1.0.0, 20-OCT-1994 (WLT)
      recursive module subroutine frmget ( ctx, infrm, et, xform, outfrm, found )
      use pepper_kinds, only: ip, wp
      use pepper_inc_frmtyp, frmtyp_all => all  ! PEPPER: ALL (unused) would hide the intrinsic
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: infrm  !! The integer code for a SPICE reference frame.
      real(wp), intent(in) :: et  !! An epoch in seconds past J2000.
      real(wp), intent(out) :: xform ( 6, 6 )  !! A state transformation matrix.
      integer(ip), intent(out) :: outfrm  !! The frame that XFORM transforms INFRM to.
      logical, intent(out) :: found  !! .TRUE. if a frame transformation can be found.
      integer(ip) :: center
      integer(ip) :: i
      integer(ip) :: j
      integer(ip) :: type
      integer(ip) :: typeid
      real(wp) :: tsipm  ( 6, 6 )
      real(wp) :: rotate ( 3, 3 )
      if ( .not. allocated ( ctx%frmget_state ) ) allocate ( ctx%frmget_state )
      associate ( idnt66 => ctx%frmget_state%idnt66, &
                  pass1 => ctx%frmget_state%pass1, &
                  versn => ctx%frmget_state%versn )
      found = .false.

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     On the first pass, initialize the identity matrix.
!
      if ( pass1 ) then

         idnt66 = 0.0_wp

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

         xform = 0.0_wp
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

         call zzdynfrm ( ctx, infrm, center, et, xform, outfrm )
!
!        The FOUND flag was set by FRINFO earlier; we don't touch
!        it here. If ZZDYNFRM signaled an error, FOUND will be set
!        to .FALSE. at end of this routine.
!

      else if ( type == swtch ) then

         call zzswfxfm ( ctx, infrm, et, 6, xform, outfrm, found )

      else if ( type == prgmtc ) then
!
!        PEPPER: a programmatic frame (ADD_FRAME): its provider.
!
         call provided_transform ( ctx, infrm, et, xform, outfrm, found )

      else

         xform = 0.0_wp
         outfrm = 0
         found  = .false.

         call ctx%setmsg ( 'The reference frame # has class #. ' &
         //            'This form of reference frame is not ' &
         //            'supported in version # of FRMGET. You need ' &
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

         xform = 0.0_wp
         outfrm = 0
         found  = .false.

      end if

      return
      end associate
      end subroutine frmget

      !> Find the rotation from a user specified frame to another frame at
      !> a user specified epoch.
      !>
      !>## Detailed Input
      !>
      !>     INFRM    is the SPICE ID-code for some reference frame.
      !>
      !>     ET       is an epoch in ephemeris seconds past J2000 at which
      !>              the user wishes to retrieve a rotation matrix.
      !>
      !>## Detailed Output
      !>
      !>     ROTATE   is a 3x3 matrix that rotates positions relative to
      !>              INFRM to positions relative to OUTFRM. (Assuming such
      !>              a rotation can be found.)
      !>
      !>     OUTFRM   is the SPICE ID-code of a reference frame. The 3x3
      !>              matrix ROTATE rotates positions relative to INFRM to
      !>              positions relative to OUTFRM. The positions
      !>              transformation is achieved by multiplying ROTATE on
      !>              the right by a position relative to INFRM. This is
      !>              easily accomplished via the subroutine call shown
      !>              below.
      !>
      !>                 CALL MXV  ( ROTATE, INPOS,  OUTPOS )
      !>
      !>     FOUND    is a logical flag indicating whether or not a
      !>              rotation matrix could be found from INFRM to some
      !>              other frame. If a rotation matrix cannot be found
      !>              OUTFRM will be set to zero, FOUND will be set to
      !>              .FALSE. and ROTATE will be returned as the zero matrix.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If a rotation matrix cannot be located, then FOUND will be set
      !>         to .FALSE., OUTFRM will be set to zero and ROTATE will be set
      !>         to the zero 3x3 matrix.
      !>
      !>     2)  If the class of the requested frame is not recognized the
      !>         exception, the error SPICE(UNKNOWNFRAMETYPE) is signaled.
      !>
      !>## Files
      !>
      !>     LSK, SCLK, PCK, FK, SPK, and/or CK kernels may need to be loaded
      !>     to provide the needed frame definition and transformation data.
      !>
      !>## Particulars
      !>
      !>     This is a low level routine used for determining a chain of
      !>     position transformation matrices from one frame to another.
      !>
      !>## Examples
      !>
      !>     See REFCHG.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     B.V. Semenov       (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 5.0.0, 15-MAR-2021 (NJB) (JDR)
      !>
      !>        **Updated shadow routines ZZROTGT0 and ZZROTGT1, as must be
      !>          done each time this routine is updated.**
      !>
      !>        Support for switch frames was added. VERSN is now
      !>        initialized via a DATA statement. Corrected long error
      !>        message to use the term "class" rather than "class id-code."
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 4.0.0, 21-MAR-2014 (BVS)
      !>
      !>        To prevent operations with un-initialized DP numbers, wrapped
      !>        IF ( .NOT. FAILED() ) ... END IF around output matrix
      !>        transposition operation in the PCK frame branch where the
      !>        routine returning the matrix might fail.
      !>
      !>        Incremented major version token by 2 to sync up versions with
      !>        FRMGET.
      !>
      !>-    SPICELIB Version 2.1.0, 02-MAR-2010 (NJB)
      !>
      !>        Bug fix: frame ID rather than frame class ID
      !>        is now passed to dynamic frame evaluation
      !>        routine ZZDYNROT. Order of header sections was
      !>        corrected.
      !>
      !>-    SPICELIB Version 2.0.0, 18-DEC-2004 (NJB)
      !>
      !>        Added the new frame type 'DYN' to the list of frame
      !>        types recognized by ROTGET.
      !>
      !>-    SPICELIB Version 1.0.0, 03-MAR-1999 (WLT)
      recursive module subroutine rotget ( ctx, infrm, et, rotate, outfrm, found )
      use pepper_kinds, only: ip, wp
      use pepper_inc_frmtyp, frmtyp_all => all  ! PEPPER: ALL (unused) would hide the intrinsic
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: infrm  !! The integer code for a SPICE reference frame.
      real(wp), intent(in) :: et  !! An epoch in seconds past J2000.
      real(wp), intent(out) :: rotate ( 3, 3 )  !! A rotation matrix.
      integer(ip), intent(out) :: outfrm  !! The frame that [[rotate]] transforms INFRM to.
      logical, intent(out) :: found  !! .TRUE. if a rotation can be found.
      integer(ip) :: center
      integer(ip) :: type
      integer(ip) :: typeid
      real(wp) :: tipm  ( 3, 3 )
      if ( .not. allocated ( ctx%rotget_state ) ) allocate ( ctx%rotget_state )
      associate ( versn => ctx%rotget_state%versn )
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

         rotate = 0.0_wp
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

            rotate = transpose(tipm)

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

         call zzdynrot ( ctx, infrm, center, et, rotate, outfrm )

!
!        The FOUND flag was set by FRINFO earlier; we don't touch
!        it here. If ZZDYNROT signaled an error, FOUND will be set
!        to .FALSE. at end of this routine.
!

      else if ( type == swtch ) then

         call zzswfxfm ( ctx, infrm, et, 3, rotate, outfrm, found )

      else if ( type == prgmtc ) then
!
!        PEPPER: a programmatic frame (ADD_FRAME): its provider.
!
         call provided_rotation ( ctx, infrm, et, rotate, outfrm, found )

      else

         rotate = 0.0_wp
         outfrm = 0
         found  = .false.

         call ctx%setmsg ( 'The reference frame # has class #. ' &
         //            'This form of reference frame is not ' &
         //            'supported in version # of ROTGET. You need ' &
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

         rotate = 0.0_wp
         outfrm = 0
         found  = .false.

      end if

      return
      end associate
      end subroutine rotget

      !> Return the state transformation matrix from one
      !> frame to another.
      !>
      !>## Detailed Input
      !>
      !>     FRAME1   is the frame id-code in which some states are known.
      !>
      !>     FRAME2   is the frame id-code for some frame in which you
      !>              would like to represent states.
      !>
      !>     ET       is the epoch at which to compute the state
      !>              transformation matrix. This epoch should be
      !>              in TDB seconds past the ephemeris epoch of J2000.
      !>
      !>## Detailed Output
      !>
      !>     XFORM    is a 6 x 6 state transformation matrix that can
      !>              be used to transform states relative to the frame
      !>              corresponding to frame FRAME2 to states relative
      !>              to the frame FRAME2. More explicitly, if STATE
      !>              is the state of some object relative to the reference
      !>              frame of FRAME1 then STATE2 is the state of the
      !>              same object relative to FRAME2 where STATE2 is
      !>              computed via the subroutine call below
      !>
      !>                 CALL MXVG ( XFORM, STATE, 6, 6, STATE2 )
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If either of the reference frames is unrecognized, the error
      !>         SPICE(UNKNOWNFRAME) is signaled.
      !>
      !>     2)  If the auxiliary information needed to compute a non-inertial
      !>         frame is not available, an error is signaled
      !>         by a routine in the call tree of this routine.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine allows you to compute the state transformation matrix
      !>     between two reference frames.
      !>
      !>     The currently supported reference frames are IAU bodyfixed frames
      !>     and inertial reference frames.
      !>
      !>## Examples
      !>
      !>     Example 1. Suppose that you have a state STATE1 at epoch ET
      !>     relative to  FRAME1 and wish to determine its representation
      !>     STATE2 relative to FRAME2. The following subroutine calls
      !>     would suffice to make this transformation.
      !>
      !>        CALL FRMCHG ( FRAME1, FRAME2, ET,   XFORM )
      !>        CALL MXVG   ( XFORM,  STATE1, 6, 6, STATE2 )
      !>
      !>     Example 2. Suppose that you have the angular velocity, W, of some
      !>     rotation relative to FRAME1 at epoch ET and that you wish to
      !>     express this angular velocity with respect to FRAME2. The
      !>     following subroutines will suffice to perform this computation.
      !>
      !>        CALL FRMCHG ( FRAME1, FRAME2, ET, STXFRM )
      !>
      !>     Recall that a state transformation matrix has the following form.
      !>
      !>            -               -
      !>           |                 |
      !>           |    R        0   |
      !>           |                 |
      !>           |                 |
      !>           |   dR            |
      !>           |   --        R   |
      !>           |   dt            |
      !>           |                 |
      !>            -               -
      !>
      !>     The velocity of an arbitrary point P undergoing rotation with the
      !>     angular velocity W is W x P
      !>
      !>     Thus the velocity of P in FRAME2 is:
      !>
      !>        dR
      !>        --  P    +    R*(W x P )
      !>        dt
      !>
      !>           dR  t
      !>     =  (  -- R  R P   +  R*(W x P)  )            ( 1 )
      !>           dt
      !>
      !>           dR  t                                              t
      !>     But   -- R  is skew symmetric  (simply differentiate  R*R to see
      !>           dt
      !>                    dR  t
      !>     this ).  Hence -- R R P  can be written as Ax(R*P) for some fixed
      !>                    dt
      !>
      !>     vector A. Moreover the vector A can be read from the upper
      !>
      !>                            dR  t
      !>     triangular portion of  -- R  .  So that equation (1) above can
      !>                            dt
      !>
      !>     be re-written as
      !>
      !>         dR  t
      !>     = ( -- R  R*P   +  R*(WxP)  )
      !>         dt
      !>
      !>     = Ax(R*P) + R*W x R*P
      !>
      !>     = ( [A+R*W] x R*P )
      !>
      !>     From this final expression it follows that in FRAME2 the angular
      !>     velocity vector is given by [A+R*W].
      !>
      !>     The code below implements these ideas.
      !>
      !>        CALL FRMCHG ( FRAME1, FRAME2, ET, STXFRM )
      !>
      !>        DO I = 1, 3
      !>           DO J = 1, 3
      !>
      !>              RT  ( I, J ) = STXFRM ( I,   J )
      !>              DRDT( I, J ) = STXFRM ( I+3, J )
      !>
      !>           END DO
      !>        END DO
      !>
      !>        CALL MXMT ( DRDT, R, AMATRIX )
      !>
      !>        Read the angular velocity of R from the skew symmetric matrix
      !>
      !>         dR  t
      !>         -- R
      !>         dt
      !>
      !>        Recall that if A has components A1, A2, A3 then the matrix
      !>        corresponding to the cross product linear mapping is:
      !>
      !>            -               -
      !>           |   0  -A3    A2  |
      !>           |                 |
      !>           |  A3   0    -A1  |
      !>           |                 |
      !>           | -A2   A1    0   |
      !>            -               -
      !>
      !>        A(1) = -AMATRIX(2,3)
      !>        A(2) =  AMATRIX(1,3)
      !>        A(3) = -AMATRIX(1,2)
      !>
      !>        CALL MXV  ( R, W1,  W  )
      !>        CALL VADD ( A, W,   W2 )
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.1.0, 08-OCT-2021 (JDR) (NJB)
      !>
      !>        Bug fix: added calls to FAILED after each call to
      !>        FRINFO and to FRMGET.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 2.0.1, 16-JAN-2014 (NJB)
      !>
      !>        Corrected equation 1 in header comments. Corrected
      !>        numerous spelling errors in comments.
      !>
      !>-    SPICELIB Version 2.0.0, 14-DEC-2008 (NJB)
      !>
      !>        Upgraded long error message associated with frame
      !>        connection failure.
      !>
      !>-    SPICELIB Version 1.1.0, 25-JUL-1996 (WLT)
      !>
      !>        Bug Fix:
      !>
      !>        The previous edition of the routine had a bug in the
      !>        first pass of the DO WHILE that looks for a frame
      !>        in the chain of frames associated with FRAME2 that is
      !>        in common with the chain of frames for FRAME1.
      !>
      !>        On machines where variables are created as static
      !>        variables, this error could lead to finding a frame
      !>        when a legitimate path between FRAME1 and FRAME2
      !>        did not exist.
      !>
      !>-    SPICELIB Version 1.0.1, 06-MAR-1996 (WLT)
      !>
      !>        An typo was fixed in the Brief I/O section. It used
      !>        to say TDT instead of the correct time system TDB.
      !>
      !>-    SPICELIB Version 1.0.0, 28-SEP-1994 (WLT)
      recursive module subroutine frmchg ( ctx, frame1, frame2, et, xform )
      use pepper_kinds, only: ip, wp
      use pepper_math, only: zzmsxf
      use pepper_inc_errhnd
      use pepper_inc_frmtyp, frmtyp_all => all  ! PEPPER: ALL (unused) would hide the intrinsic
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: frame1  !! the frame id-code for some reference frame
      integer(ip), intent(in) :: frame2  !! the frame id-code for some reference frame
      real(wp), intent(in) :: et  !! an epoch in TDB seconds past J2000.
      real(wp), intent(out) :: xform ( 6, 6 )  !! a state transformation matrix

!
!     SPICE functions
!



!
!     The root of all reference frames is J2000 (Frame ID = 1).
!

      character(lmsgln) :: errmsg

      real(wp) :: tempxf( 6, 6 )
!
!     TRANS contains the transformations from FRAME1 to FRAME2
!     TRANS(1...6,1...6,I) has the transformation from FRAME(I)
!     to FRAME(I+1).  We make extra room in TRANS because we
!     plan to add transformations beyond the obvious chain from
!     FRAME1 to a root node.
!
      real(wp) :: trans ( 6, 6, maxxfm )

!
!     TRANS2 is used to store intermediate transformations from
!     FRAME2 to some node in the chain from FRAME1 to PCK or
!     INERTL frames.
!
      real(wp) :: trans2( 6, 6, 2      )

      integer(ip) :: cent
      integer(ip) :: class
      integer(ip) :: clssid
      integer(ip) :: cmnode
!
!     FRAME contains the frames we transform from in going from
!     FRAME1 to FRAME2.  FRAME(1) = FRAME1 by  construction.
!
      integer(ip) :: frame ( maxnod )
      integer(ip) :: get
      integer(ip) :: i
      integer(ip) :: inc
      integer(ip) :: j
      integer(ip) :: k
      integer(ip) :: l

!
!     NODE counts the number of transformations needed to go
!     from FRAME1 to FRAME2.
!
      integer(ip) :: node

      integer(ip) :: put
      integer(ip) :: relto
      integer(ip) :: this

      logical :: done
      logical :: found
      logical :: gotone

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
         call frmget ( ctx, frame(node),     et, &
                       trans(1,1,node), frame(node+1), found  )

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
         call frmget ( ctx, frame(node), et, trans(1,1,node), relto, found  )

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
            call zzmsxf ( trans(1,1,node-1), 2,     tempxf      )

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
            call frmget ( ctx, this,  et, trans2(1,1,put), relto, found  )

            if ( ctx%failed() ) return

            if ( found ) then
               this   = relto
               get    = put
               put    = put + inc
               inc    =     - inc
               cmnode = findloc(frame(1:node), this, dim=1)
               gotone = cmnode > 0
            end if

         else
!
!           Fetch the transformation into a temporary spot TEMPXF
!
            call frmget ( ctx, this,  et, tempxf, relto,  found  )

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

               cmnode = findloc(frame(1:node), this, dim=1)
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

      end subroutine frmchg

      !> Return the transformation matrix from one
      !> frame to another.
      !>
      !>## Detailed Input
      !>
      !>     FRAME1   is the frame id-code in which some positions
      !>              are known.
      !>
      !>     FRAME2   is the frame id-code for some frame in which you
      !>              would like to represent positions.
      !>
      !>     ET       is the epoch at which to compute the transformation
      !>              matrix. This epoch should be in TDB seconds past
      !>              the ephemeris epoch of J2000.
      !>
      !>## Detailed Output
      !>
      !>     ROTATE   is a 3 x 3 rotation matrix that can be used to
      !>              transform positions relative to the frame
      !>              corresponding to frame FRAME2 to positions relative
      !>              to the frame FRAME2. More explicitly, if POS is
      !>              the position of some object relative to the
      !>              reference frame of FRAME1 then POS2 is the position
      !>              of the same object relative to FRAME2 where POS2 is
      !>              computed via the subroutine call below
      !>
      !>                 CALL MXV ( ROTATE, POS, POS2 )
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If either of the reference frames is unrecognized, the error
      !>         SPICE(UNKNOWNFRAME) is signaled.
      !>
      !>     2)  If the auxiliary information needed to compute a non-inertial
      !>         frame is not available, an error is signaled
      !>         by a routine in the call tree of this routine.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine allows you to compute the rotation matrix
      !>     between two reference frames.
      !>
      !>## Examples
      !>
      !>     Suppose that you have a position POS1 at epoch ET
      !>     relative to  FRAME1 and wish to determine its representation
      !>     POS2 relative to FRAME2. The following subroutine calls
      !>     would suffice to make this rotation.
      !>
      !>        CALL REFCHG ( FRAME1, FRAME2, ET,   ROTATE )
      !>        CALL MXV    ( ROTATE, POS1,   POS2 )
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.1.0, 08-OCT-2021 (JDR) (NJB)
      !>
      !>        Bug fix: added calls to FAILED after each call to
      !>        FRINFO and to ROTGET.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 2.0.0, 14-DEC-2008 (NJB)
      !>
      !>        Upgraded long error message associated with frame
      !>        connection failure.
      !>
      !>-    SPICELIB Version 1.2.0, 26-APR-2004 (NJB)
      !>
      !>        Another typo was corrected in the long error message, and
      !>        in a comment.
      !>
      !>-    SPICELIB Version 1.1.0, 23-MAY-2000 (WLT)
      !>
      !>        A typo was corrected in the long error message.
      !>
      !>-    SPICELIB Version 1.0.0, 09-JUL-1998 (WLT)
      recursive module subroutine refchg ( ctx, frame1, frame2, et, rotate )
      use pepper_kinds, only: ip, wp
      use pepper_math, only: ident, zzrxr
      use pepper_inc_errhnd
      use pepper_inc_frmtyp, frmtyp_all => all  ! PEPPER: ALL (unused) would hide the intrinsic
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: frame1  !! the frame id-code for some reference frame
      integer(ip), intent(in) :: frame2  !! the frame id-code for some reference frame
      real(wp), intent(in) :: et  !! an epoch in TDB seconds past J2000.
      real(wp), intent(out) :: rotate ( 3, 3 )  !! a rotation matrix

!
!     SPICE functions
!



!
!     The root of all reference frames is J2000 (Frame ID = 1).
!

      character(lmsgln) :: errmsg

!
!     ROT contains the rotations from FRAME1 to FRAME2
!     ROT(1...3,1...3,I) has the rotation from FRAME(I)
!     to FRAME(I+1).  We make extra room in ROT because we
!     plan to add rotations beyond the obvious chain from
!     FRAME1 to a root node.
!
      real(wp) :: rot ( 3, 3, maxxfm )

!
!     ROT2 is used to store intermediate rotation from
!     FRAME2 to some node in the chain from FRAME1 to PCK or
!     INERTL frames.
!
      real(wp) :: rot2  ( 3, 3, 2     )
      real(wp) :: tmprot( 3, 3        )

      integer(ip) :: cent
      integer(ip) :: class
      integer(ip) :: clssid
      integer(ip) :: cmnode
      integer(ip) :: get
!
!     FRAME contains the frames we transform from in going from
!     FRAME1 to FRAME2.  FRAME(1) = FRAME1 by  construction.
!
      integer(ip) :: frame ( maxnod )
      integer(ip) :: i
      integer(ip) :: inc
      integer(ip) :: j
!
!     NODE counts the number of rotations needed to go
!     from FRAME1 to FRAME2.
!
      integer(ip) :: node
      integer(ip) :: put
      integer(ip) :: relto
      integer(ip) :: this

      logical :: done
      logical :: found
      logical :: gotone

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
         call rotget ( ctx, frame(node),   et, &
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
         call rotget ( ctx, frame(node), et, rot(1,1,node), relto, found )

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
            call rotget ( ctx, this,  et, rot2(1,1,put), relto, found )

            if ( ctx%failed() ) return

            if ( found ) then
               this   = relto
               get    = put
               put    = put + inc
               inc    =     - inc
               cmnode = findloc(frame(1:node), this, dim=1)
               gotone = cmnode > 0
            end if

         else
!
!           Fetch the rotation into a temporary spot TMPROT
!
            call rotget ( ctx, this,  et, tmprot, relto, found )

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

               cmnode = findloc(frame(1:node), this, dim=1)
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
      rot(:,:,cmnode) = transpose(rot2(:,:,get))
      call zzrxr ( rot,   cmnode, rotate          )

      return

      end subroutine refchg

      !> Return the matrix that transforms position vectors from one
      !> specified frame to another at a specified epoch.
      !>
      !>## Detailed Input
      !>
      !>     FROM     is the name of a reference frame in which a position
      !>              vector is known.
      !>
      !>     TO       is the name of a reference frame in which it is desired
      !>              to represent a position vector.
      !>
      !>     ET       is the epoch in ephemeris seconds past the epoch of J2000
      !>              (TDB) at which the position transformation matrix ROTATE
      !>              should be evaluated.
      !>
      !>## Detailed Output
      !>
      !>     ROTATE   is the matrix that transforms position vectors from
      !>              the reference frame FROM to the frame TO at epoch ET.
      !>              If (x, y, z) is a position relative to the frame FROM
      !>              then the vector ( x', y', z') is the same position
      !>              relative to the frame TO at epoch ET. Here the
      !>              vector ( x', y', z' ) is defined by the equation:
      !>
      !>                 .-   -.     .-        -.   .-  -.
      !>                 | x'  |     |          |   | x  |
      !>                 | y'  |  =  |  ROTATE  |   | y  |
      !>                 | z'  |     |          |   | z  |
      !>                 `-   -'     `-        -'   `-  -'
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If sufficient information has not been supplied via loaded
      !>         SPICE kernels to compute the transformation between the
      !>         two frames, an error is signaled by a routine
      !>         in the call tree of this routine.
      !>
      !>     2)  If either frame FROM or TO is not recognized, the error
      !>         SPICE(UNKNOWNFRAME) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine provides the user level interface to computing
      !>     position transformations from one reference frame to another.
      !>
      !>     Note that the reference frames may be inertial or non-inertial.
      !>     However, the user must take care that sufficient SPICE kernel
      !>     information is loaded to provide a complete position
      !>     transformation path from the FROM frame to the TO frame.
      !>
      !>     A common type of reference frame transformation is one from one
      !>     time-dependent frame to another, where the orientations of the
      !>     frames are computed at different epochs. For example, a remote
      !>     sensing application may compute the transformation from a target
      !>     body-fixed frame, with its orientation evaluated at the epoch of
      !>     photon emission, to a spacecraft instrument frame, with its
      !>     orientation evaluated at the epoch of photon reception. The
      !>     SPICELIB routine PXFRM2 computes this type of frame
      !>     transformation.
      !>
      !>## Examples
      !>
      !>     Suppose that you have geodetic coordinates of a station on the
      !>     surface of the earth and that you need the inertial (J2000)
      !>     position of this station. The following code fragment
      !>     illustrates how to transform the position of the station to a
      !>     J2000 position.
      !>
      !>        CALL BODVRD ( 'EARTH', RADII, 3, N, ABC  )
      !>
      !>        EQUATR   =  ABC(1)
      !>        POLAR    =  ABC(3)
      !>        F        = (EQUATR - POLAR) / EQUATR
      !>
      !>        CALL GEOREC ( LONG, LAT, 0.0D0,  EQUATR, F, EPOS )
      !>
      !>        CALL PXFORM ( 'IAU_EARTH', 'J2000', ET,  ROTATE  )
      !>        CALL MXV    (  ROTATE,      EPOS,   JPOS         )
      !>
      !>     The position JPOS is the desired J2000 position of the station.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     C.H. Acton         (JPL)
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     B.V. Semenov       (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.1, 09-AUG-2021 (NJB) (JDR)
      !>
      !>        Edited the header to comply with NAIF standard. Updated the
      !>        $Particulars header section to mention PXFRM2.
      !>
      !>-    SPICELIB Version 1.1.0, 23-SEP-2013 (BVS)
      !>
      !>        Updated to save the input frame names and POOL state counters
      !>        and to do frame name-ID conversions only if the counters have
      !>        changed.
      !>
      !>-    SPICELIB Version 1.0.3, 27-FEB-2008 (BVS)
      !>
      !>        Added FRAMES to the $Required_Reading section.
      !>
      !>-    SPICELIB Version 1.0.2, 23-OCT-2005 (NJB)
      !>
      !>        Header example had invalid flattening factor computation;
      !>        this was corrected. Reference to BODVAR in header was
      !>        replaced with reference to BODVRD.
      !>
      !>-    SPICELIB Version 1.0.1, 29-JUL-2003 (NJB) (CHA)
      !>
      !>        Various header corrections were made.
      !>
      !>-    SPICELIB Version 1.0.0, 05-APR-1999 (WLT)
      recursive module subroutine pxform ( ctx, from, to, et, rotate )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzctr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: from  !! Name of the frame to transform from.
      character(*), intent(in) :: to  !! Name of the frame to transform to.
      real(wp), intent(in) :: et  !! Epoch of the rotation matrix.
      real(wp), intent(out) :: rotate  ( 3, 3 )  !! A rotation matrix.
      integer(ip) :: fcode
      integer(ip) :: tcode
      if ( .not. allocated ( ctx%pxform_state ) ) allocate ( ctx%pxform_state )
      associate ( svctr1 => ctx%pxform_state%svctr1, &
                  svfrom => ctx%pxform_state%svfrom, &
                  svfcod => ctx%pxform_state%svfcod, &
                  svctr2 => ctx%pxform_state%svctr2, &
                  svto => ctx%pxform_state%svto, &
                  svtcde => ctx%pxform_state%svtcde, &
                  first => ctx%pxform_state%first )
      if ( ctx%failed() ) return

!
!     Initialization.
!
      if ( first ) then

!
!        Initialize counters.
!
         call zzctruin( ctx, svctr1 )
         call zzctruin( ctx, svctr2 )

         first = .false.

      end if

      call zznamfrm ( ctx, svctr1, svfrom, svfcod, from, fcode )
      call zznamfrm ( ctx, svctr2, svto,   svtcde, to,   tcode )

!
!     Only non-zero id-codes are legitimate frame id-codes.  Zero
!     indicates that the frame wasn't recognized.
!
      if      ( fcode /= 0 .and. tcode /= 0 ) then

         call refchg ( ctx, fcode, tcode, et, rotate )

      else if ( fcode == 0 .and. tcode == 0 ) then

         call ctx%setmsg ( 'Neither of the frames # or # was ' &
         //            'recognized as a known reference frame. ' )
         call ctx%errch  ( '#', from )
         call ctx%errch  ( '#', to   )
         call ctx%sigerr ( 'SPICE(UNKNOWNFRAME)'  )

      else if ( fcode == 0 ) then

         call ctx%setmsg ( 'The frame # was not recognized as a ' &
         //            'known reference frame. ' )
         call ctx%errch  ( '#', from )
         call ctx%sigerr ( 'SPICE(UNKNOWNFRAME)'  )

      else if ( tcode == 0 ) then

         call ctx%setmsg ( 'The frame # was not recognized as a ' &
         //            'known reference frame. ' )
         call ctx%errch  ( '#', to   )
         call ctx%sigerr ( 'SPICE(UNKNOWNFRAME)'  )

      end if

      return
      end associate
      end subroutine pxform

      !> Return the state transformation matrix from one frame to
      !> another at a specified epoch.
      !>
      !>## Detailed Input
      !>
      !>     FROM     is the name of a reference frame in which a state is
      !>              known.
      !>
      !>     TO       is the name of a reference frame in which it is
      !>              desired to represent the state.
      !>
      !>     ET       is the epoch in ephemeris seconds past the epoch of
      !>              J2000 (TDB) at which the state transformation matrix
      !>              should be evaluated.
      !>
      !>## Detailed Output
      !>
      !>     XFORM    is the state transformation matrix that transforms
      !>              states from the reference frame FROM to the frame TO
      !>              at epoch ET. If (x, y, z, dx, dy, dz) is a state
      !>              relative to the frame FROM then the vector ( x', y',
      !>              z', dx', dy', dz' ) is the same state relative to the
      !>              frame TO at epoch ET. Here the vector ( x', y', z',
      !>              dx', dy', dz' ) is defined by the equation:
      !>
      !>                 .-   -.     .-          -.   .-  -.
      !>                 | x'  |     |            |   | x  |
      !>                 | y'  |     |            |   | y  |
      !>                 | z'  |  =  |   XFORM    |   | z  |
      !>                 | dx' |     |            |   | dx |
      !>                 | dy' |     |            |   | dy |
      !>                 | dz' |     |            |   | dz |
      !>                 `-   -'     `-          -'   `-  -'
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If sufficient information has not been supplied via loaded
      !>         SPICE kernels to compute the transformation between the two
      !>         frames, an error is signaled by a routine in the call tree of
      !>         this routine.
      !>
      !>     2)  If either frame FROM or TO is not recognized, the error
      !>         SPICE(UNKNOWNFRAME) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine provides the user level interface to computing state
      !>     transformations from one reference frame to another.
      !>
      !>     Note that the reference frames may be inertial or non-inertial.
      !>     However, the user must take care that sufficient SPICE kernel
      !>     information is loaded to provide a complete state transformation
      !>     path from the FROM frame to the TO frame.
      !>
      !>## Examples
      !>
      !>     Suppose that you have geodetic coordinates of a station on
      !>     the surface of the earth and that you need the inertial
      !>     (J2000) state of this station. The following code fragment
      !>     illustrates how to transform the position of the station to
      !>     a J2000 state.
      !>
      !>        CALL BODVRD ( 'EARTH', RADII, 3, N, ABC  )
      !>
      !>        EQUATR   =  ABC(1)
      !>        POLAR    =  ABC(3)
      !>        F        = (EQUATR - POLAR) / EQUATR
      !>
      !>        CALL GEOREC ( LONG, LAT, 0.0D0,  EQUATR, F,  ESTATE )
      !>
      !>        ESTATE(4) = 0.0D0
      !>        ESTATE(5) = 0.0D0
      !>        ESTATE(6) = 0.0D0
      !>
      !>        CALL SXFORM ( 'IAU_EARTH', 'J2000',   ET, XFORM  )
      !>        CALL MXVG   (  XFORM,       ESTATE, 6, 6, JSTATE )
      !>
      !>     The state JSTATE is the desired J2000 state of the station.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     C.H. Acton         (JPL)
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     B.V. Semenov       (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.1, 05-JUL-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.1.0, 23-SEP-2013 (BVS)
      !>
      !>        Updated to save the input frame names and POOL state counters
      !>        and to do frame name-ID conversions only if the counters have
      !>        changed.
      !>
      !>-    SPICELIB Version 1.0.3, 27-FEB-2008 (BVS)
      !>
      !>        Added FRAMES to the $Required_Reading section.
      !>
      !>-    SPICELIB Version 1.0.2, 23-OCT-2005 (NJB)
      !>
      !>        Header example had invalid flattening factor computation;
      !>        this was corrected. Reference to BODVAR in header was
      !>        replaced with reference to BODVRD.
      !>
      !>-    SPICELIB Version 1.0.1, 29-JUL-2003 (NJB) (CHA)
      !>
      !>        Minor header changes were made to improve clarity.
      !>
      !>-    SPICELIB Version 1.0.0, 19-SEP-1995 (WLT)
      recursive module subroutine sxform ( ctx, from, to, et, xform )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzctr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: from  !! Name of the frame to transform from.
      character(*), intent(in) :: to  !! Name of the frame to transform to.
      real(wp), intent(in) :: et  !! Epoch of the state transformation matrix.
      real(wp), intent(out) :: xform  ( 6, 6 )  !! A state transformation matrix.
      integer(ip) :: fcode
      integer(ip) :: tcode
      if ( .not. allocated ( ctx%sxform_state ) ) allocate ( ctx%sxform_state )
      associate ( svctr1 => ctx%sxform_state%svctr1, &
                  svfrom => ctx%sxform_state%svfrom, &
                  svfcod => ctx%sxform_state%svfcod, &
                  svctr2 => ctx%sxform_state%svctr2, &
                  svto => ctx%sxform_state%svto, &
                  svtcde => ctx%sxform_state%svtcde, &
                  first => ctx%sxform_state%first )
      if ( ctx%failed() ) return

!
!     Initialization.
!
      if ( first ) then

!
!        Initialize counters
!
         call zzctruin( ctx, svctr1 )
         call zzctruin( ctx, svctr2 )

         first = .false.

      end if

      call zznamfrm ( ctx, svctr1, svfrom, svfcod, from, fcode )
      call zznamfrm ( ctx, svctr2, svto,   svtcde, to,   tcode )

!
!     Only non-zero id-codes are legitimate frame id-codes.  Zero
!     indicates that the frame wasn't recognized.
!
      if      ( fcode /= 0 .and. tcode /= 0 ) then

         call frmchg ( ctx, fcode, tcode, et, xform )

      else if ( fcode == 0 .and. tcode == 0 ) then

         call ctx%setmsg ( 'Neither of the frames # or # was ' &
         //            'recognized as a known reference frame. ' )
         call ctx%errch  ( '#', from )
         call ctx%errch  ( '#', to   )
         call ctx%sigerr ( 'SPICE(UNKNOWNFRAME)'  )

      else if ( fcode == 0 ) then

         call ctx%setmsg ( 'The frame # was not recognized as a ' &
         //            'known reference frame. ' )
         call ctx%errch  ( '#', from )
         call ctx%sigerr ( 'SPICE(UNKNOWNFRAME)'  )

      else if ( tcode == 0 ) then

         call ctx%setmsg ( 'The frame # was not recognized as a ' &
         //            'known reference frame. ' )
         call ctx%errch  ( '#', to   )
         call ctx%sigerr ( 'SPICE(UNKNOWNFRAME)'  )

      end if

      return
      end associate
      end subroutine sxform

end submodule pepper_transforms
