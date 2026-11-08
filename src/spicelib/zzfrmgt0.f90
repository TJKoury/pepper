!$Procedure      ZZFRMGT0 ( Frame get transformation )

      SUBROUTINE ZZFRMGT0 ( INFRM, ET, XFORM, OUTFRM, FOUND )

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

      IMPLICIT NONE

      INCLUDE              'frmtyp.inc'

      INTEGER               INFRM
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      XFORM ( 6, 6 )
      INTEGER               OUTFRM
      LOGICAL               FOUND

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     INFRM      I   The integer code for a SPICE reference frame.
!     ET         I   An epoch in seconds past J2000.
!     XFORM      O   A state transformation matrix.
!     OUTFRM     O   The frame that XFORM transforms INFRM to.
!     FOUND      O   TRUE if a frame transformation can be found.
!
! $ Detailed_Input
!
!     INFRM       is the SPICE ID-code for some reference frame.
!
!     ET          is an epoch in ephemeris seconds past J2000 at
!                 which the user wishes to retrieve a state
!                 transformation matrix.
!
! $ Detailed_Output
!
!     XFORM       is a 6x6 matrix that transforms states relative to
!                 INFRM to states relative to OUTFRM. (Assuming such a
!                 transformation can be found.)
!
!     OUTFRM      is the SPICE ID-code of a reference frame. The 6x6
!                 matrix XFORM transforms states relative to INFRM to
!                 states relative to OUTFRM. The state transformation
!                 is achieved by multiplying XFORM on the right by a
!                 state relative to INFRM.  This is easily accomplished
!                 via the subroutine call shown below.
!
!                    CALL MXVG ( XFORM, STATE, 6, 6, OSTATE )
!
!     FOUND       is a logical flag indicating whether or not a
!                 transformation matrix could be found from INFRM to
!                 some other frame. If a transformation matrix cannot
!                 be found OUTFRM will be set to zero, FOUND will be
!                 set to FALSE and XFORM will be returned as the zero
!                 matrix.
!
! $ Parameters
!
!     See include file.
!
! $ Exceptions
!
!     1) If a transformation matrix cannot be located, then FOUND will
!        be set to FALSE, OUTFRM will be set to zero and XFORM will be
!        set to the zero 6x6 matrix.
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
!        types recognized by FRMGET.  In addition the routine
!        now checks FAILED after "getting" the frame transformation.
!
!-    SPICELIB Version 1.0.0, 20-OCT-1994 (WLT)
!
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
      LOGICAL               RETURN
      LOGICAL               FAILED

!
!     Local Variables
!
      CHARACTER*(6)         VERSN

      INTEGER               CENTER
      INTEGER               I
      INTEGER               J
      INTEGER               TYPE
      INTEGER               TYPEID

      DOUBLE PRECISION      IDNT66 ( 6, 6 )
      DOUBLE PRECISION      TSIPM  ( 6, 6 )
      DOUBLE PRECISION      ROTATE ( 3, 3 )

      LOGICAL               PASS1
!
!     Saved variables
!
      SAVE                  IDNT66
      SAVE                  PASS1
      SAVE                  VERSN

!
!     Initial values
!
      DATA                 PASS1 / .TRUE.  /
      DATA                 VERSN / '4.0.0' /

!
!     Set output flag.
!
      FOUND = .FALSE.

!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZFRMGT0' )

!
!     On the first pass, initialize the identity matrix.
!
      IF ( PASS1 ) then

         CALL CLEARD( 36, IDNT66 )

         DO I = 1, 6
            IDNT66(I,I) = 1.D0
         END DO

         PASS1 = .FALSE.

      END IF

!
!     Get all the needed information about this frame.
!
      CALL FRINFO ( INFRM, CENTER, TYPE, TYPEID, FOUND )

      IF ( .NOT. FOUND ) THEN

         CALL CLEARD ( 36, XFORM )
         OUTFRM = 0

         CALL CHKOUT ( 'ZZFRMGT0' )
         RETURN

      END IF

!
!     FOUND was set to true by the FRINFO call. Compute transformation
!     based on the frame class.
!
      IF ( TYPE .EQ. INERTL ) THEN

         CALL IRFROT ( INFRM, 1, ROTATE )

         IF ( .NOT. FAILED() ) THEN

            DO I = 1,3
               DO J = 1,3
                  XFORM(I,  J  ) = ROTATE(I,J)
                  XFORM(I+3,J+3) = ROTATE(I,J)
                  XFORM(I+3,J  ) = 0.0D0
                  XFORM(I,  J+3) = 0.0D0
               END DO
            END DO

            OUTFRM = 1

         END IF


      ELSE IF ( TYPE .EQ. PCK ) THEN

         CALL TISBOD ( 'J2000', TYPEID, ET, TSIPM  )

         IF ( .NOT. FAILED() ) THEN

            CALL INVSTM ( TSIPM, XFORM )

            OUTFRM = 1

         END IF


      ELSE IF ( TYPE .EQ. CK ) THEN

         CALL CKFXFM ( TYPEID, ET, XFORM, OUTFRM, FOUND )


      ELSE IF ( TYPE .EQ. TK ) THEN

         CALL TKFRAM  ( TYPEID, ROTATE, OUTFRM, FOUND )

         IF ( .NOT. FAILED() ) THEN

            DO I = 1,3
               DO J = 1,3
                  XFORM(I,  J  ) = ROTATE(I,J)
                  XFORM(I+3,J+3) = ROTATE(I,J)
                  XFORM(I+3,J  ) = 0.0D0
                  XFORM(I,  J+3) = 0.0D0
               END DO
            END DO

         END IF


      ELSE IF ( TYPE .EQ. DYN ) THEN
!
!        Unlike the other frame classes, the dynamic frame evaluation
!        routine ZZDYNFRM requires the input frame ID rather than the
!        dynamic frame class ID. ZZDYNFRM also requires the center ID
!        we found via the FRINFO call.
!
         CALL ZZDYNFR0 ( INFRM, CENTER, ET, XFORM, OUTFRM )

!
!        The FOUND flag was set by FRINFO earlier; we don't touch
!        it here. If ZZDYNFRM signaled an error, FOUND will be set
!        to .FALSE. at end of this routine.
!
      ELSE IF ( TYPE .EQ. SWTCH ) THEN

         CALL ZZSWFXFM ( INFRM, ET, 6, XFORM, OUTFRM, FOUND )

      ELSE

         CALL CLEARD ( 36, XFORM )
         OUTFRM = 0
         FOUND  = .FALSE.

         CALL SETMSG ( 'The reference frame # has class #. ' &
         //            'This form of reference frame is not ' &
         //            'supported in version # of ZZFRMGT0. You need ' &
         //            'to update your version of SPICELIB to the ' &
         //            'latest version in order to support this ' &
         //            'frame. ' )

         CALL ERRINT ( '#', INFRM )
         CALL ERRINT ( '#', TYPE  )
         CALL ERRCH  ( '#', VERSN )
         CALL SIGERR ( 'SPICE(UNKNOWNFRAMETYPE)' )
         CALL CHKOUT ( 'ZZFRMGT0'                  )
         RETURN

      END IF

!
!     Make sure to clear outputs in case of a failure as defined in
!     in the header.
!
      IF ( FAILED() .OR. .NOT. FOUND ) THEN

         CALL CLEARD ( 36, XFORM )
         OUTFRM = 0
         FOUND  = .FALSE.

      END IF

      CALL CHKOUT ( 'ZZFRMGT0' )
      RETURN
      END
