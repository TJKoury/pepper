!$Procedure FRMCHG (Frame Change)

      SUBROUTINE FRMCHG ( FRAME1, FRAME2, ET, XFORM )

! $ Abstract
!
!     Return the state transformation matrix from one
!     frame to another.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
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
      DOUBLE PRECISION      XFORM ( 6, 6 )

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
!     FRAME1   is the frame id-code in which some states are known.
!
!     FRAME2   is the frame id-code for some frame in which you
!              would like to represent states.
!
!     ET       is the epoch at which to compute the state
!              transformation matrix. This epoch should be
!              in TDB seconds past the ephemeris epoch of J2000.
!
! $ Detailed_Output
!
!     XFORM    is a 6 x 6 state transformation matrix that can
!              be used to transform states relative to the frame
!              corresponding to frame FRAME2 to states relative
!              to the frame FRAME2. More explicitly, if STATE
!              is the state of some object relative to the reference
!              frame of FRAME1 then STATE2 is the state of the
!              same object relative to FRAME2 where STATE2 is
!              computed via the subroutine call below
!
!                 CALL MXVG ( XFORM, STATE, 6, 6, STATE2 )
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
!     This routine allows you to compute the state transformation matrix
!     between two reference frames.
!
!     The currently supported reference frames are IAU bodyfixed frames
!     and inertial reference frames.
!
! $ Examples
!
!     Example 1. Suppose that you have a state STATE1 at epoch ET
!     relative to  FRAME1 and wish to determine its representation
!     STATE2 relative to FRAME2. The following subroutine calls
!     would suffice to make this transformation.
!
!        CALL FRMCHG ( FRAME1, FRAME2, ET,   XFORM )
!        CALL MXVG   ( XFORM,  STATE1, 6, 6, STATE2 )
!
!
!
!     Example 2. Suppose that you have the angular velocity, W, of some
!     rotation relative to FRAME1 at epoch ET and that you wish to
!     express this angular velocity with respect to FRAME2. The
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
!        --  P    +    R*(W x P )
!        dt
!
!           dR  t
!     =  (  -- R  R P   +  R*(W x P)  )            ( 1 )
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
!     vector A. Moreover the vector A can be read from the upper
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
!        FRINFO and to FRMGET.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 2.0.1, 16-JAN-2014 (NJB)
!
!        Corrected equation 1 in header comments. Corrected
!        numerous spelling errors in comments.
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

      DOUBLE PRECISION      TEMPXF( 6, 6 )
!
!     TRANS contains the transformations from FRAME1 to FRAME2
!     TRANS(1...6,1...6,I) has the transformation from FRAME(I)
!     to FRAME(I+1).  We make extra room in TRANS because we
!     plan to add transformations beyond the obvious chain from
!     FRAME1 to a root node.
!
      DOUBLE PRECISION      TRANS ( 6, 6, MAXXFM )

!
!     TRANS2 is used to store intermediate transformations from
!     FRAME2 to some node in the chain from FRAME1 to PCK or
!     INERTL frames.
!
      DOUBLE PRECISION      TRANS2( 6, 6, 2      )


      INTEGER               CENT
      INTEGER               CLASS
      INTEGER               CLSSID
      INTEGER               CMNODE
!
!     FRAME contains the frames we transform from in going from
!     FRAME1 to FRAME2.  FRAME(1) = FRAME1 by  construction.
!
      INTEGER               FRAME ( MAXNOD )
      INTEGER               GET
      INTEGER               I
      INTEGER               INC
      INTEGER               J
      INTEGER               K
      INTEGER               L

!
!     NODE counts the number of transformations needed to go
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

      CALL CHKIN ( 'FRMCHG')
!
!     Do the obvious thing first.  If FRAME1 and FRAME2 are the
!     same then we simply return the identity matrix.
!
      IF ( FRAME1 .EQ. FRAME2 ) THEN

         DO I = 1, 6

            XFORM(I,I) = 1.0D0

            DO J = 1, I-1
               XFORM(I,J) = 0.0D0
               XFORM(J,I) = 0.0D0
            END DO

         END DO

         CALL CHKOUT ( 'FRMCHG' )
         RETURN

      END IF
!
!     Now perform the obvious check to make sure that both
!     frames are recognized.
!
      CALL FRINFO ( FRAME1, CENT, CLASS, CLSSID, FOUND )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'FRMCHG' )
         RETURN
      END IF

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'The number # is not a recognized id-code ' &
         //            'for a reference frame. ' )
         CALL ERRINT ( '#', FRAME1 )
         CALL SIGERR ( 'SPICE(UNKNOWNFRAME)'  )
         CALL CHKOUT ( 'FRMCHG' )
         RETURN

      END IF


      CALL FRINFO ( FRAME2, CENT, CLASS, CLSSID, FOUND )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'FRMCHG' )
         RETURN
      END IF

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'The number # is not a recognized id-code ' &
         //            'for a reference frame. ' )
         CALL ERRINT ( '#', FRAME2 )
         CALL SIGERR ( 'SPICE(UNKNOWNFRAME)'  )
         CALL CHKOUT ( 'FRMCHG' )
         RETURN

      END IF


      NODE        = 1
      FRAME(NODE) = FRAME1
      FOUND       = .TRUE.
!
!     Follow the chain of transformations until we run into
!     one that transforms to J2000 (frame id = 1) or we hit FRAME2.
!
      DO WHILE (       FRAME(NODE) .NE. ROOTF &
                 .AND. NODE        .LT. MAXNOD &
                 .AND. FRAME(NODE) .NE. FRAME2 &
                 .AND. FOUND   )
!
!        Find out what transformation is available for this
!        frame.
!
         CALL FRMGET ( FRAME(NODE),     ET, &
                       TRANS(1,1,NODE), FRAME(NODE+1), FOUND  )

         IF ( FOUND ) THEN
!
!           We found a transformation matrix.  TRANS(1,1,NODE)
!           now contains the transformation from FRAME(NODE)
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
!        room in the array of reference frame transformation
!        buffers.  We will now build the transformation from
!        the previous NODE to whatever the next node in the
!        chain is.  We'll do this until we get to one of the
!        root classes or we run into FRAME2.
!
         CALL FRMGET ( FRAME(NODE), ET, TRANS(1,1,NODE), RELTO, FOUND  )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'FRMCHG' )
            RETURN
         END IF

         IF ( FOUND  ) THEN
!
!           Recall that TRANS(1,1,NODE-1) contains the transformation
!           from FRAME(NODE-1) to FRAME(NODE).  We are going to replace
!           FRAME(NODE) with the frame indicated by RELTO.  This means
!           that TRANS(1,1,NODE-1) should be replaced with the
!           transformation from FRAME(NODE) to RELTO.
!
            FRAME(NODE) = RELTO
            CALL ZZMSXF ( TRANS(1,1,NODE-1), 2,     TEMPXF      )

            DO I = 1,6
               DO J = 1,6
                  TRANS(I,J,NODE-1) = TEMPXF(I,J)
               END DO
            END DO

         END IF
!
!        We are done if the class of the last frame is J2000
!        or if the last frame is FRAME2 or if we simply couldn't get
!        another transformation.
!
         DONE =        FRAME(NODE) .EQ. ROOTF &
                  .OR. FRAME(NODE) .EQ. FRAME2 &
                  .OR. .NOT. FOUND

      END DO

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

      IF ( FRAME(NODE) .EQ. FRAME2 ) THEN
!
!        We can handle this one immediately with the private routine
!        ZZMSXF which multiplies a series of state transformation
!        matrices.
!
         CALL ZZMSXF (  TRANS, NODE-1, XFORM )
         CALL CHKOUT ( 'FRMCHG' )
         RETURN

      END IF

!
!     We didn't luck out above.  So we follow the chain of
!     transformation for FRAME2.  Note that at the moment the
!     chain of transformations from FRAME2 to other frames
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
!     Set up the matrices TRANS2(,,1) and TRANS(,,2)  and set up
!     PUT and GET pointers so that we know where to GET the partial
!     transformation from and where to PUT partial results.
!
      IF ( .NOT. DONE ) THEN

         DO K = 1, 2
            DO I=1,3
               DO J = 4,6
                  TRANS2(I,J,K) = 0.0D0
               END DO
            END DO
         END DO

         PUT = 1
         GET = 1
         INC = 1

      END IF

!
!     Follow the chain of transformations until we run into
!     one that transforms to the root frame or we land in the
!     chain of nodes for FRAME1.
!
!     Note that this time we will simply keep track of the full
!     translation from FRAME2 to the last node.
!
      DO WHILE (  .NOT. DONE )
!
!        Find out what transformation is available for this
!        frame.
!
         IF ( THIS  .EQ. FRAME2 ) THEN
!
!           This is the first pass, just put the transformation
!           directly into TRANS2(,,PUT).
!
            CALL FRMGET ( THIS,  ET, TRANS2(1,1,PUT), RELTO, FOUND  )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'FRMCHG' )
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
!           Fetch the transformation into a temporary spot TEMPXF
!
            CALL FRMGET ( THIS,  ET, TEMPXF, RELTO,  FOUND  )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'FRMCHG' )
               RETURN
            END IF

            IF ( FOUND ) THEN
!
!              Next multiply TEMPXF on the right by the last partial
!              product (in TRANS2(,,GET) ).  We do this in line because
!              we can cut down the number of multiplies to 3/8 of the
!              normal result of MXMG.  For a discussion of why this
!              works see ZZMSXF.
!
               DO I=1,3
                  DO J=1,3

                     TRANS2(I,J,PUT) = TEMPXF(I,1)*TRANS2(1,J,GET) &
                                     + TEMPXF(I,2)*TRANS2(2,J,GET) &
                                     + TEMPXF(I,3)*TRANS2(3,J,GET)
                  END DO
               END DO

               DO I=4,6
                  DO J=1,3

                     TRANS2(I,J,PUT) = TEMPXF(I,1)*TRANS2(1,J,GET) &
                                     + TEMPXF(I,2)*TRANS2(2,J,GET) &
                                     + TEMPXF(I,3)*TRANS2(3,J,GET) &
                                     + TEMPXF(I,4)*TRANS2(4,J,GET) &
                                     + TEMPXF(I,5)*TRANS2(5,J,GET) &
                                     + TEMPXF(I,6)*TRANS2(6,J,GET)
                  END DO
               END DO
!
!              Note that we don't have to compute the upper right
!              hand block.  It's already set to zero by construction.
!
!              Finally we can just copy the lower right hand block
!              from the upper left hand block of the matrix.
!
               DO I = 4,6
                  K = I - 3
                  DO J = 4,6
                     L = J - 3
                     TRANS2(I,J,PUT) = TRANS2(K,L,PUT)
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
!     transformations from FRAME2 ran into a node in the chain for
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
            CALL CHKOUT ( 'FRMCHG' )
            RETURN

         END IF
!
!        The normal case: signal an error with a descriptive long
!        error message.
!
         CALL SETMSG ( ERRMSG                  )
         CALL SIGERR ( 'SPICE(NOFRAMECONNECT)' )
         CALL CHKOUT ( 'FRMCHG'                )
         RETURN

      END IF

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
      CALL INVSTM ( TRANS2(1,1,GET), TRANS(1,1,CMNODE) )
      CALL ZZMSXF ( TRANS,   CMNODE, XFORM             )

      CALL CHKOUT ( 'FRMCHG' )
      RETURN

      END
