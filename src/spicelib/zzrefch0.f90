!$Procedure      ZZREFCH0 (Reference frame Change)

      SUBROUTINE ZZREFCH0 ( FRAME1, FRAME2, ET, ROTATE )

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

      IMPLICIT NONE

      INCLUDE              'errhnd.inc'
      INCLUDE              'frmtyp.inc'

      INTEGER               FRAME1
      INTEGER               FRAME2
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ROTATE ( 3, 3 )

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
      INTEGER               ISRCHI

      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local Parameters
!
      INTEGER               MAXNOD
      PARAMETER           ( MAXNOD = 10 )

      INTEGER               MAXXFM
      PARAMETER           ( MAXXFM = MAXNOD + 4 )

!
!     The root of all reference frames is J2000 (Frame ID = 1).
!
      INTEGER               ROOTF
      PARAMETER           ( ROOTF = 1 )

!
!     Local Variables
!
      CHARACTER*(LMSGLN)    ERRMSG

!
!     ROT contains the rotations from FRAME1 to FRAME2
!     ROT(1...3,1...3,I) has the rotation from FRAME(I)
!     to FRAME(I+1).  We make extra room in ROT because we
!     plan to add rotations beyond the obvious chain from
!     FRAME1 to a root node.
!
      DOUBLE PRECISION      ROT ( 3, 3, MAXXFM )

!
!     ROT2 is used to store intermediate rotation from
!     FRAME2 to some node in the chain from FRAME1 to PCK or
!     INERTL frames.
!
      DOUBLE PRECISION      ROT2  ( 3, 3, 2     )
      DOUBLE PRECISION      TMPROT( 3, 3        )

      INTEGER               CENT
      INTEGER               CLASS
      INTEGER               CLSSID
      INTEGER               CMNODE
      INTEGER               GET
!
!     FRAME contains the frames we transform from in going from
!     FRAME1 to FRAME2.  FRAME(1) = FRAME1 by  construction.
!
      INTEGER               FRAME ( MAXNOD )
      INTEGER               I
      INTEGER               INC
      INTEGER               J
!
!     NODE counts the number of rotations needed to go
!     from FRAME1 to FRAME2.
!
      INTEGER               NODE
      INTEGER               PUT
      INTEGER               RELTO
      INTEGER               THIS

      LOGICAL               DONE
      LOGICAL               FOUND
      LOGICAL               GOTONE

!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZREFCH0')
!
!     Do the obvious thing first.  If FRAME1 and FRAME2 are the
!     same then we simply return the identity matrix.
!
      IF ( FRAME1 .EQ. FRAME2 ) THEN

         CALL IDENT  ( ROTATE )
         CALL CHKOUT ( 'ZZREFCH0' )
         RETURN

      END IF
!
!     Now perform the obvious check to make sure that both
!     frames are recognized.
!
      CALL FRINFO ( FRAME1, CENT, CLASS, CLSSID, FOUND )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZREFCH0' )
         RETURN
      END IF

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'The number # is not a recognized id-code ' &
         //            'for a reference frame. ' )
         CALL ERRINT ( '#', FRAME1 )
         CALL SIGERR ( 'SPICE(UNKNOWNFRAME)'  )
         CALL CHKOUT ( 'ZZREFCH0' )
         RETURN

      END IF


      CALL FRINFO ( FRAME2, CENT, CLASS, CLSSID, FOUND )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZREFCH0' )
         RETURN
      END IF

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'The number # is not a recognized id-code ' &
         //            'for a reference frame. ' )
         CALL ERRINT ( '#', FRAME2 )
         CALL SIGERR ( 'SPICE(UNKNOWNFRAME)'  )
         CALL CHKOUT ( 'ZZREFCH0' )
         RETURN

      END IF



      NODE        = 1
      FRAME(NODE) = FRAME1
      FOUND       = .TRUE.
!
!     Follow the chain of rotations until we run into
!     one that rotates to J2000 (frame id = 1) or we hit FRAME2.
!
      DO WHILE (       FRAME(NODE) .NE. ROOTF &
                 .AND. NODE        .LT. MAXNOD &
                 .AND. FRAME(NODE) .NE. FRAME2 &
                 .AND. FOUND   )
!
!        Find out what rotation is available for this
!        frame.
!
         CALL ZZROTGT0 ( FRAME(NODE),   ET, &
                         ROT(1,1,NODE), FRAME(NODE+1), FOUND  )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZREFCH0' )
            RETURN
         END IF

         IF ( FOUND ) THEN
!
!           We found a rotation matrix.  ROT(1,1,NODE)
!           now contains the rotation from FRAME(NODE)
!           to FRAME(NODE+1).  We need to look up the information
!           for the next NODE.
!
            NODE = NODE + 1

         END IF

      END DO

      DONE =      FRAME(NODE) .EQ. ROOTF &
             .OR. FRAME(NODE) .EQ. FRAME2 &
             .OR. .NOT. FOUND


      DO WHILE ( .NOT. DONE )
!
!        The only way to get to this point is to have run out of
!        room in the array of reference frame rotation
!        buffers.  We will now build the rotation from
!        the previous NODE to whatever the next node in the
!        chain is.  We'll do this until we get to one of the
!        root classes or we run into FRAME2.
!
         CALL ZZROTGT0 ( FRAME(NODE), ET, ROT(1,1,NODE), RELTO, FOUND )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZREFCH0' )
            RETURN
         END IF

         IF ( FOUND  ) THEN
!
!           Recall that ROT(1,1,NODE-1) contains the rotation
!           from FRAME(NODE-1) to FRAME(NODE).  We are going to replace
!           FRAME(NODE) with the frame indicated by RELTO.  This means
!           that ROT(1,1,NODE-1) should be replaced with the
!           rotation from FRAME(NODE) to RELTO.
!
            FRAME(NODE) = RELTO
            CALL ZZRXR ( ROT(1,1,NODE-1), 2, TMPROT )

            DO I = 1,3
               DO J = 1,3
                  ROT(I,J,NODE-1) = TMPROT(I,J)
               END DO
            END DO

         END IF
!
!        We are done if the class of the last frame is J2000
!        or if the last frame is FRAME2 or if we simply couldn't get
!        another rotation.
!
         DONE =        FRAME(NODE) .EQ. ROOTF &
                  .OR. FRAME(NODE) .EQ. FRAME2 &
                  .OR. .NOT. FOUND

      END DO

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

      IF ( FRAME(NODE) .EQ. FRAME2 ) THEN
!
!        We can handle this one immediately with the private routine
!        ZZRXR which multiplies a series of matrices.
!
         CALL ZZRXR (  ROT, NODE-1, ROTATE )
         CALL CHKOUT ( 'ZZREFCH0' )
         RETURN

      END IF

!
!     We didn't luck out above.  So we follow the chain of
!     rotation for FRAME2.  Note that at the moment the
!     chain of rotations from FRAME2 to other frames
!     does not share a node in the chain for FRAME1.
!    ( GOTONE = .FALSE. ) .
!
      THIS   =  FRAME2
      GOTONE = .FALSE.

!
!     First see if there is any chain to follow.
!
      DONE   =  THIS .EQ. ROOTF

!
!     Set up the matrices ROT2(,,1) and ROT(,,2)  and set up
!     PUT and GET pointers so that we know where to GET the partial
!     rotation from and where to PUT partial results.
!
      IF ( .NOT. DONE ) THEN

         PUT = 1
         GET = 1
         INC = 1

      END IF

!
!     Follow the chain of rotations until we run into
!     one that rotates to the root frame or we land in the
!     chain of nodes for FRAME1.
!
!     Note that this time we will simply keep track of the full
!     rotation from FRAME2 to the last node.
!
      DO WHILE (  .NOT. DONE )
!
!        Find out what rotation is available for this
!        frame.
!
         IF ( THIS  .EQ. FRAME2 ) THEN
!
!           This is the first pass, just put the rotation
!           directly into ROT2(,,PUT).
!
            CALL ZZROTGT0 ( THIS,  ET, ROT2(1,1,PUT), RELTO, FOUND  )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'ZZREFCH0' )
               RETURN
            END IF

            IF ( FOUND ) THEN
               THIS   = RELTO
               GET    = PUT
               PUT    = PUT + INC
               INC    =     - INC
               CMNODE = ISRCHI ( THIS, NODE, FRAME )
               GOTONE = CMNODE .GT. 0
            END IF

         ELSE
!
!           Fetch the rotation into a temporary spot TMPROT
!
            CALL ZZROTGT0 ( THIS,  ET, TMPROT, RELTO,  FOUND  )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'ZZREFCH0' )
               RETURN
            END IF

            IF ( FOUND ) THEN
!
!              Next multiply TMPROT on the right by the last partial
!              product (in ROT2(,,GET) ).  We do this in line.
!
               DO I=1,3
                  DO J=1,3

                     ROT2(I,J,PUT) = TMPROT(I,1)*ROT2(1,J,GET) &
                                   + TMPROT(I,2)*ROT2(2,J,GET) &
                                   + TMPROT(I,3)*ROT2(3,J,GET)
                  END DO
               END DO



!
!              Adjust GET and PUT so that GET points to the slots
!              where we just stored the result of our multiply and
!              so that PUT points to the next available storage
!              locations.
!
               GET    = PUT
               PUT    = PUT + INC
               INC    =     - INC
               THIS   = RELTO

               CMNODE = ISRCHI ( THIS, NODE, FRAME )
               GOTONE = CMNODE .GT. 0

            END IF

         END IF

!
!        See if we have a common node and determine whether or not
!        we are done with this loop.
!
         DONE   =       THIS  .EQ. ROOTF &
                  .OR.  GOTONE &
                  .OR. .NOT. FOUND


      END DO

!
!     There are two possible scenarios.  Either the chain of
!     rotations from FRAME2 ran into a node in the chain for
!     FRAME1 or it didn't.  (The common node might very well be
!     the root node.)  If we didn't run into a common one, then
!     the two chains don't intersect and there is no way to
!     get from FRAME1 to FRAME2.
!
      IF ( .NOT. GOTONE ) THEN

         CALL ZZNOFCON ( ET, FRAME1, FRAME(NODE), FRAME2, THIS, ERRMSG )

         IF ( FAILED() ) THEN
!
!           We were unable to create the error message. This
!           unfortunate situation could arise if a frame kernel
!           is corrupted.
!
            CALL CHKOUT ( 'ZZREFCH0' )
            RETURN

         END IF
!
!        The normal case: signal an error with a descriptive long
!        error message.
!
         CALL SETMSG ( ERRMSG                  )
         CALL SIGERR ( 'SPICE(NOFRAMECONNECT)' )
         CALL CHKOUT ( 'ZZREFCH0'              )
         RETURN

      END IF

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
      CALL XPOSE ( ROT2(1,1,GET), ROT(1,1,CMNODE) )
      CALL ZZRXR ( ROT,   CMNODE, ROTATE          )

      CALL CHKOUT ( 'ZZREFCH0' )
      RETURN

      END
