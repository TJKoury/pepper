!$Procedure FRMGET ( Frame get transformation )

      subroutine frmget ( infrm, et, xform, outfrm, found )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Find the transformation from a user specified frame to
!     another frame at a user specified epoch.
!
! $ Required_Reading
!
!     FRAMES
!
! $ Keywords
!
!     FRAMES
!
! $ Declarations

      implicit none

      include              'frmtyp.inc'

      integer(ip)           infrm
      real(wp)              et
      real(wp)              xform ( 6, 6 )
      integer(ip)           outfrm
      logical               found

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     INFRM      I   The integer code for a SPICE reference frame.
!     ET         I   An epoch in seconds past J2000.
!     XFORM      O   A state transformation matrix.
!     OUTFRM     O   The frame that XFORM transforms INFRM to.
!     FOUND      O   .TRUE. if a frame transformation can be found.
!
! $ Detailed_Input
!
!     INFRM    is the SPICE ID-code for some reference frame.
!
!     ET       is an epoch in ephemeris seconds past J2000 at
!              which the user wishes to retrieve a state
!              transformation matrix.
!
! $ Detailed_Output
!
!     XFORM    is a 6x6 matrix that transforms states relative to
!              INFRM to states relative to OUTFRM. (Assuming such a
!              transformation can be found.)
!
!     OUTFRM   is the SPICE ID-code of a reference frame. The 6x6
!              matrix XFORM transforms states relative to INFRM to
!              states relative to OUTFRM. The state transformation
!              is achieved by multiplying XFORM on the right by a
!              state relative to INFRM. This is easily accomplished
!              via the subroutine call shown below.
!
!                 CALL MXVG ( XFORM, STATE, 6, 6, OSTATE )
!
!     FOUND    is a logical flag indicating whether or not a
!              transformation matrix could be found from INFRM to
!              some other frame. If a transformation matrix cannot
!              be found OUTFRM will be set to zero, FOUND will be
!              set to .FALSE. and XFORM will be returned as the zero
!              matrix.
!
! $ Parameters
!
!     See include file.
!
! $ Exceptions
!
!     1)  If a transformation matrix cannot be located, then FOUND will
!         be set to .FALSE., OUTFRM will be set to zero and XFORM will
!         be set to the zero 6x6 matrix.
!
!     2)  If the class of the requested frame is not recognized the
!         exception, the error SPICE(UNKNOWNFRAMETYPE) is signaled.
!
! $ Files
!
!     LSK, SCLK, PCK, FK, SPK, and/or CK kernels may need to be loaded
!     to provide the needed frame definition and transformation data.
!
! $ Particulars
!
!     This is a low level routine used for determining a chain of state
!     transformation matrices from one frame to another.
!
! $ Examples
!
!     See FRMCHG.
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
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 5.0.0, 15-MAR-2021 (NJB) (JDR)
!
!        **Updated shadow routines ZZFRMGT0 and ZZFRMGT1, as must be
!          done each time this routine is updated.**
!
!        Support for switch frames was added. VERSN is now
!        initialized via a DATA statement. Corrected long error
!        message to use the term "class" rather than "class id-code."
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 4.0.0, 05-JAN-2014 (BVS)
!
!        To prevent operations with un-initialized DP numbers, wrapped
!        IF ( .NOT. FAILED() ) ... END IF around output matrix
!        transposition/re-assignment operations in all branches where
!        the routine returning the matrix might fail.
!
!        Added zero-ing out the output matrix in cases of a failed or
!        .NOT. FOUND transformation look ups.
!
!-    SPICELIB Version 3.0.0, 18-DEC-2004 (NJB)
!
!        Added the new frame type 'DYN' to the list of frame
!        types recognized by FRMGET.
!
!-    SPICELIB Version 2.0.0, 03-APR-1997 (WLT)
!
!        Added the new frame type 'TK' to the list of frame
!        types recognized by FRMGET. In addition the routine
!        now checks FAILED after "getting" the frame transformation.
!
!-    SPICELIB Version 1.0.0, 20-OCT-1994 (WLT)
!
!-&


! $ Index_Entries
!
!     Find a frame transformation matrix from a specified frame
!
!-&


!
!     SPICELIB Functions
!
      logical               return
      logical               failed

!
!     Local Variables
!
      character(6)          versn

      integer(ip)           center
      integer(ip)           i
      integer(ip)           j
      integer(ip)           type
      integer(ip)           typeid

      real(wp)              idnt66 ( 6, 6 )
      real(wp)              tsipm  ( 6, 6 )
      real(wp)              rotate ( 3, 3 )

      logical               pass1
!
!     Saved variables
!
      save                  idnt66
      save                  pass1
      save                  versn

!
!     Initial values
!
      data                 pass1 / .true.  /
      data                 versn / '5.0.0' /

!
!     Set output flag.
!
      found = .false.

!
!     Standard SPICE error handling.
!
      if ( return() ) return


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
      call frinfo ( infrm, center, type, typeid, found )

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

         call irfrot ( infrm, 1, rotate )

         if ( .not. failed() ) then

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

         call tisbod ( 'J2000', typeid, et, tsipm  )

         if ( .not. failed() ) then

            call invstm ( tsipm, xform )

            outfrm = 1

         end if


      else if ( type == ck ) then

         call ckfxfm ( typeid, et, xform, outfrm, found )


      else if ( type == tk ) then

         call tkfram  ( typeid, rotate, outfrm, found )

         if ( .not. failed() ) then

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

         call zzdynfrm ( infrm, center, et, xform, outfrm )
!
!        The FOUND flag was set by FRINFO earlier; we don't touch
!        it here. If ZZDYNFRM signaled an error, FOUND will be set
!        to .FALSE. at end of this routine.
!

      else if ( type == swtch ) then

         call zzswfxfm ( infrm, et, 6, xform, outfrm, found )

      else

         call cleard ( 36, xform )
         outfrm = 0
         found  = .false.

         call setmsg ( 'The reference frame # has class #. ' &
         //            'This form of reference frame is not ' &
         //            'supported in version # of FRMGET. You need ' &
         //            'to update your version of SPICELIB to the ' &
         //            'latest version in order to support this ' &
         //            'frame. ' )

         call errint ( '#', infrm )
         call errint ( '#', type  )
         call errch  ( '#', versn )
         call sigerr ( 'SPICE(UNKNOWNFRAMETYPE)' )
         return

      end if

!
!     Make sure to clear outputs in case of a failure as defined in
!     in the header.
!
      if ( failed() .or. .not. found ) then

         call cleard ( 36, xform )
         outfrm = 0
         found  = .false.

      end if

      return
      end
