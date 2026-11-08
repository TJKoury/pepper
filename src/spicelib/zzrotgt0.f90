!$Procedure      ZZROTGT0 ( Frame get rotation )

      subroutine zzrotgt0 ( infrm, et, rotate, outfrm, found )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Find the rotation from a user specified frame to another frame at
!     a user specified epoch.
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
      real(wp)              rotate ( 3, 3 )
      integer(ip)           outfrm
      logical               found

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     INFRM      I   The integer code for a SPICE reference frame.
!     ET         I   An epoch in seconds past J2000.
!     ROTATE     O   A rotation matrix.
!     OUTFRM     O   The frame that ROTATE transforms INFRM to.
!     FOUND      O   TRUE if a rotation can be found.
!
! $ Detailed_Input
!
!     INFRM       is the SPICE ID-code for some reference frame.
!
!     ET          is an epoch in ephemeris seconds past J2000 at which
!                 the user wishes to retrieve a rotation matrix.
!
! $ Detailed_Output
!
!     ROTATE      is a 3x3 matrix that rotates positions relative to
!                 INFRM to positions relative to OUTFRM. (Assuming such
!                 a rotation can be found.)
!
!     OUTFRM      is the SPICE ID-code of a reference frame. The 3x3
!                 matrix ROTATE rotates positions relative to INFRM to
!                 positions relative to OUTFRM. The positions
!                 transformation is achieved by multiplying ROTATE on
!                 the right by a position relative to INFRM. This is
!                 easily accomplished via the subroutine call shown
!                 below.
!
!                    CALL MXV  ( ROTATE, INPOS,  OUTPOS )
!
!     FOUND       is a logical flag indicating whether or not a
!                 rotation matrix could be found from INFRM to some
!                 other frame. If a rotation matrix cannot be found
!                 OUTFRM will be set to zero, FOUND will be set to
!                 FALSE and ROTATE will be returned as the zero matrix.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) If a rotation matrix cannot be located, then FOUND will be set
!        to FALSE, OUTFRM will be set to zero and ROTATE will be set to
!        the zero 3x3 matrix.
!
!     2) If the class of the requested frame is not recognized the
!        exception 'SPICE(UNKNOWNFRAMETYPE)' will be signaled.
!
! $ Files
!
!     LSK, SCLK, PCK, FK, SPK, and/or CK kernels may need to be loaded
!     to provide the needed frame definition and transformation data.
!
! $ Particulars
!
!     This is a low level routine used for determining a chain of
!     position transformation matrices from one frame to another.
!
! $ Examples
!
!     See REFCHG.
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
!     N.J. Bachman    (JPL)
!     B.V. Semenov    (JPL)
!     W.L. Taber      (JPL)
!
! $ Version
!
!-    SPICELIB Version 5.0.0, 18-SEP-2020 (NJB)
!
!        Support for switch frames was added. VERSN is now
!        initialized via a DATA statement. Corrected long error
!        message to use the term "class" rather than "class id-code."
!
!-    SPICELIB Version 4.0.0, 21-MAR-2014 (BVS)
!
!        To prevent operations with un-initialized DP numbers, wrapped
!        IF ( .NOT. FAILED() ) ... END IF around output matrix
!        transposition operation in the PCK frame branch where the
!        routine returning the matrix might fail.
!
!        Incremented major version token by 2 to sync up versions with
!        FRMGET.
!
!-    SPICELIB Version 2.1.0, 02-MAR-2010 (NJB)
!
!        Bug fix: frame ID rather than frame class ID
!        is now passed to dynamic frame evaluation
!        routine ZZDYNROT. Order of header sections was
!        corrected.
!
!-    SPICELIB Version 2.0.0, 18-DEC-2004 (NJB)
!
!        Added the new frame type 'DYN' to the list of frame
!        types recognized by ROTGET.
!
!-    SPICELIB Version 1.0.0, 03-MAR-1999 (WLT)
!
!-&

! $ Index_Entries
!
!     Find a rotation matrix from a specified frame
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
      integer(ip)           type
      integer(ip)           typeid

      real(wp)              tipm  ( 3, 3 )

!
!     Saved variables
!
      save                  versn

!
!     Initial values
!
      data                 versn / '4.0.0' /

!
!     Set output flag.
!
      found = .false.

!
!     Standard SPICE error handling.
!
      if ( return() ) return

!
!     Get all the needed information about this frame.
!
      call frinfo ( infrm, center, type, typeid, found )

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

         call irfrot ( infrm, 1, rotate )

         if ( .not. failed() ) then

            outfrm = 1

         end if

      else if ( type == pck ) then

         call tipbod ( 'J2000', typeid, et, tipm   )

         if ( .not. failed() ) then

            call xpose ( tipm, rotate )

            outfrm = 1

         end if

      else if ( type == ck ) then

         call ckfrot ( typeid, et, rotate, outfrm, found )

      else if ( type == tk ) then

         call tkfram  ( typeid, rotate, outfrm, found )

      else if ( type == dyn ) then

!
!        Unlike the other frame classes, the dynamic frame evaluation
!        routine ZZDYNROT requires the input frame ID rather than the
!        dynamic frame class ID. ZZDYNROT also requires the center ID
!        we found via the FRINFO call.
!
         call zzdynrt0 ( infrm, center, et, rotate, outfrm )

!
!        The FOUND flag was set by FRINFO earlier; we don't touch
!        it here. If ZZDYNROT signaled an error, FOUND will be set
!        to .FALSE. at end of this routine.
!

      else if ( type == swtch ) then

         call zzswfxfm ( infrm, et, 3, rotate, outfrm, found )

      else

         call cleard ( 9, rotate )
         outfrm = 0
         found  = .false.

         call setmsg ( 'The reference frame # has class #. ' &
         //            'This form of reference frame is not ' &
         //            'supported in version # of ZZROTGT0. You need ' &
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

         call cleard ( 9, rotate )
         outfrm = 0
         found  = .false.

      end if

      return
      end
