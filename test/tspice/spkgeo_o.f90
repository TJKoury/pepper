!$Procedure SPKGEO_O ( S/P Kernel, geometric state )

      SUBROUTINE SPKGEO_O ( TARG, ET, REF, OBS, STATE, LT )

! $ Abstract
!
!     Compute the geometric state (position and velocity) of a target
!     body relative to an observing body.
!
! $ Required_Reading
!
!     SPK
!
! $ Keywords
!
!     EPHEMERIS
!
! $ Declarations

      IMPLICIT NONE
      INCLUDE              'ninert.inc'
      INTEGER               TARG
      DOUBLE PRECISION      ET
      CHARACTER*(*)         REF
      INTEGER               OBS
      DOUBLE PRECISION      STATE    ( 6 )
      DOUBLE PRECISION      LT

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     TARG       I   Target body.
!     ET         I   Target epoch.
!     REF        I   Target reference frame.
!     OBS        I   Observing body.
!     STATE      O   State of target.
!     LT         O   Light time.
!
! $ Detailed_Input
!
!     TARG        is the standard NAIF ID code for a target body.
!
!     ET          is the epoch (ephemeris time) at which the state
!                 of the target body is to be computed.
!
!     REF         is the name of the reference frame to
!                 which the vectors returned by the routine should
!                 be rotated. This may be any frame supported by
!                 the SPICELIB subroutine FRMCHG.
!
!     OBS         is the standard NAIF ID code for an observing body.
!
! $ Detailed_Output
!
!     STATE       contains the position and velocity of the target
!                 body, relative to the observing body, corrected
!                 for the specified aberrations, at epoch ET.  STATE
!                 has six elements:  the first three contain the
!                 target's position; the last three contain the target's
!                 velocity.  These vectors are rotated into the
!                 specified reference frame. Units are always
!                 km and km/sec.
!
!     LT          is the one-way light time in seconds from the
!                 observing body to the geometric position of the
!                 target body at the specified epoch.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) If insufficient ephemeris data has been loaded to compute
!        the necessary states, the error SPICE(SPKINSUFFDATA) is
!        signaled.
!
! $ Files
!
!     See: $Restrictions.
!
! $ Particulars
!
!     SPKGEO_O computes the geometric state, T(t), of the target
!     body and the geometric state, O(t), of the observing body
!     relative to the first common center of motion.  Subtracting
!     O(t) from T(t) gives the geometric state of the target
!     body relative to the observer.
!
!
!        CENTER ----- O(t)
!            |      /
!            |     /
!            |    /
!            |   /  T(t) - O(t)
!            |  /
!           T(t)
!
!
!     The one-way light time, tau, is given by
!
!
!               | T(t) - O(t) |
!        tau = -----------------
!                      c
!
!
!     For example, if the observing body is -94, the Mars Observer
!     spacecraft, and the target body is 401, Phobos, then the
!     first common center is probably 4, the Mars Barycenter.
!     O(t) is the state of -94 relative to 4 and T(t) is the
!     state of 401 relative to 4.
!
!     The center could also be the Solar System Barycenter, body 0.
!     For example, if the observer is 399, Earth, and the target
!     is 299, Venus, then O(t) would be the state of 399 relative
!     to 0 and T(t) would be the state of 299 relative to 0.
!
!     Ephemeris data from more than one segment may be required
!     to determine the states of the target body and observer
!     relative to a common center.  SPKGEO_O reads as many segments
!     as necessary, from as many files as necessary, using files
!     that have been loaded by previous calls to SPKLEF (load
!     ephemeris file).
!
!     SPKGEO_O is similar to SPKEZ but returns geometric states
!     only, with no option to make planetary (light-time) nor
!     stellar aberration corrections.  The geometric states
!     returned by SPKEZ and SPKGEO_O are the same.
!
! $ Examples
!
!     The following code example computes the geometric
!     state of the moon with respect to the earth and
!     then prints the distance of the moon from the
!     the earth at a number of epochs.
!
!     Assume the SPK file SAMPLE.BSP contains ephemeris data
!     for the moon relative to earth over the time interval
!     from BEGIN to END.
!
!            INTEGER               EARTH
!            PARAMETER           ( EARTH = 399 )
!
!            INTEGER               MOON
!            PARAMETER           ( MOON  = 301 )
!
!            INTEGER               N
!            PARAMETER           ( N     = 100 )
!
!            INTEGER               HANDLE
!            CHARACTER*(20)        UTC
!            DOUBLE PRECISION      BEGIN
!            DOUBLE PRECISION      DELTA
!            DOUBLE PRECISION      END
!            DOUBLE PRECISION      ET
!            DOUBLE PRECISION      STATE ( 6 )
!
!     C
!     C      Load the binary SPK ephemeris file.
!     C
!            CALL SPKLEF ( 'SAMPLE.BSP', HANDLE )
!
!            .
!            .
!            .
!
!     C
!     C      Divide the interval of coverage [BEGIN,END] into
!     C      N steps.  At each step, compute the state, and
!     C      print out the epoch in UTC time and position norm.
!     C
!            DELTA = ( END - BEGIN ) / N
!
!            DO I = 0, N
!
!               ET = BEGIN + I*DELTA
!
!               CALL SPKGEO_O ( MOON, ET, 'J2000', EARTH, STATE, LT )
!
!               CALL ET2UTC ( ET, 'C', 0, UTC )
!
!               WRITE (*,*) UTC, VNORM ( STATE )
!
!            END DO
!
! $ Restrictions
!
!     1) The ephemeris files to be used by SPKGEO_O must be loaded
!        by SPKLEF before SPKGEO_O is called.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J.E. McLean (JPL)
!     W.L. Taber  (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.4.0, 01-SEP-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in VADDG calls.
!
!-    SPICELIB Version 2.3.0, 05-JAN-2005 (NJB)
!
!        Tests of routine FAILED() were added.
!
!-    SPICELIB Version 2.2.1, 20-OCT-2003 (EDW)
!
!        Added mention that LT returns in seconds.
!
!-    SPICELIB Version 2.2.0, 11-APR-1997 (WLT)
!
!        The routine was modified to take advantage of the fact
!        that most state transformation are between inertial frames.
!        Looking up a transformation between inertial frames is
!        substantially faster than looking up non-inertial
!        transformations.  Consequently slightly more
!        complex code produces about a 50% increase in speed for
!        many users.
!
!-    SPICELIB Version 2.1.0, 26-JUL-1996 (WLT)
!
!        The routine was upgraded so that potentially redundant
!        computations are not performed.
!
!-    SPICELIB Version 2.0.0, 19-SEP-1995 (WLT)
!
!        The routine was upgraded so that it can return states
!        relative to rotating frames.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 18-JUL-1991 (JEM)
!
!-&

! $ Index_Entries
!
!     geometric state of one body relative to another
!
!-&

! $ Revisions
!
!-    SPICELIB Version 2.4.0, 01-SEP-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in VADDG calls.
!
!-&


!
!     This is the idea:
!
!     Every body moves with respect to some center. The center
!     is itself a body, which in turn moves about some other
!     center.  If we begin at the target body (T), follow
!     the chain,
!
!                                   T
!                                     \
!           SSB                        \
!               \                     C[1]
!                \                     /
!                 \                   /
!                  \                 /
!                   \               /
!                  C[3]-----------C[2]
!
!     and avoid circular definitions (A moves about B, and B moves
!     about A), eventually we get the state relative to the solar
!     system barycenter (which, for our purposes, doesn't move).
!     Thus,
!
!        T    = T     + C[1]     + C[2]     + ... + C[n]
!         SSB    C[1]       C[2]       [C3]             SSB
!
!     where
!
!        X
!         Y
!
!     is the state of body X relative to body Y.
!
!     However, we don't want to follow each chain back to the SSB
!     if it isn't necessary.  Instead we will just follow the chain
!     of the target body and follow the chain of the observing body
!     until we find a common node in the tree.
!
!     In the example below, C is the first common node.  We compute
!     the state of TARG relative to C and the state of OBS relative
!     to C, then subtract the two states.
!
!                                   TARG
!                                     \
!           SSB                        \
!               \                       A
!                \                     /            OBS
!                 \                   /              |
!                  \                 /               |
!                   \               /                |
!                    B-------------C-----------------D
!
!

!
!
!     SPICELIB functions
!
      LOGICAL               FAILED
      LOGICAL               RETURN

      DOUBLE PRECISION      CLIGHT
      DOUBLE PRECISION      VNORM

      INTEGER               FRSTNP
      INTEGER               ISRCHI

!
!     Local parameters
!
      CHARACTER*(*)         RNAME
      PARAMETER           ( RNAME  =  'SPKGEO_O' )

!
!     CHLEN is the maximum length of a chain.  That is,
!     it is the maximum number of bodies in the chain from
!     the target or observer to the SSB.
!
      INTEGER               CHLEN
      PARAMETER           ( CHLEN = 20 )

      INTEGER               SSB
      PARAMETER           ( SSB   =  0 )

!
!     Local variables
!
      CHARACTER*(40)        IDENT
      CHARACTER*(40)        TNAME
      CHARACTER*(40)        ONAME
      CHARACTER*(80)        TSTRING

      DOUBLE PRECISION      DESCR  ( 5        )
      DOUBLE PRECISION      SOBS   ( 6        )
      DOUBLE PRECISION      STARG  ( 6, CHLEN )
      DOUBLE PRECISION      STEMP  ( 6        )

      DOUBLE PRECISION      STXFRM ( 6, 6     )
      DOUBLE PRECISION      ROT    ( 3, 3     )
      DOUBLE PRECISION      VTEMP  ( 6        )

      INTEGER               CFRAME
      INTEGER               COBS
      INTEGER               CTARG  (    CHLEN )
      INTEGER               TFRAME (    CHLEN )
      INTEGER               CTPOS
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               LEGS
      INTEGER               NCT
      INTEGER               REFID
      INTEGER               TMPFRM

      LOGICAL               FOUND
      LOGICAL               NOFRM
      LOGICAL               ISINRT

!
!     In-line Function Definitions
!
      ISINRT ( TMPFRM, CFRAME ) =       CFRAME .GT. 0 &
                                  .AND. CFRAME .LE. NINERT &
                                  .AND. TMPFRM .GT. 0 &
                                  .AND. TMPFRM .LE. NINERT

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( RNAME )
      END IF
!
!     We take care of the obvious case first.  It TARG and OBS are the
!     same we can just fill in zero.
!
      IF ( TARG .EQ. OBS ) THEN

         LT = 0.0D0

         CALL CLEARD ( 6, STATE )
         CALL CHKOUT ( RNAME )
         RETURN

      END IF


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
!
!     Since Inertial frames are the most extensively used frames
!     we use the more restrictive routine IRFNUM to attempt to
!     look up the id-code for REF.  If IRFNUM comes up empty handed
!     we then call the more general routine NAMFRM.
!
      CALL IRFNUM ( REF, REFID )

      IF ( REFID .EQ. 0 ) THEN
         CALL NAMFRM ( REF, REFID )
      END IF

      IF ( REFID .EQ. 0 ) THEN

         IF (  FRSTNP(REF) .GT. 0 ) THEN

            CALL SETMSG ( 'The string supplied to specify the ' &
            //            'reference frame, (''#'') contains ' &
            //            'non-printing characters.  The two ' &
            //            'most common causes for this kind ' &
            //            'of error are: 1. an error in the ' &
            //            'call to SPKGEO_O; 2. an ' &
            //            'uninitialized variable. ' )
            CALL ERRCH  ( '#', REF )


         ELSE IF ( REF .EQ. ' ' ) THEN

            CALL SETMSG ( 'The string supplied to specify the ' &
            //            'reference frame is blank.  The most ' &
            //            'common cause for this kind of error ' &
            //            'is an uninitialized variable. ' )


         ELSE

            CALL SETMSG ( 'The string supplied to specify the ' &
            //            'reference frame was ''#''.  This ' &
            //            'frame is not recognized. Possible ' &
            //            'causes for this error are: 1. failure ' &
            //            'to load the frame definition into the ' &
            //            'kernel pool; 2. An out-of-date ' &
            //            'edition of the toolkit. ' )
            CALL ERRCH  ( '#', REF )

         END IF

         CALL SIGERR ( 'SPICE(UNKNOWNFRAME)'  )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( RNAME )
            RETURN
         END IF

      END IF



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
!
!     Construct CTARG and STARG.  Begin by assigning the
!     first elements:  TARG and the state of TARG relative
!     to itself.
!
      I        =  1
      CTARG(I) =  TARG
      FOUND    = .TRUE.

      CALL CLEARD ( 6, STARG(1,I) )

      DO WHILE (           FOUND &
                  .AND.  ( I        .LT. CHLEN ) &
                  .AND.  ( CTARG(I) .NE. OBS   ) &
                  .AND.  ( CTARG(I) .NE. SSB   )  )

!
!        Find a file and segment that has state
!        data for CTARG(I).
!
         CALL SPKSFS ( CTARG(I), ET, HANDLE, DESCR, IDENT, FOUND )

         IF ( FOUND ) THEN
!
!           Get the state of CTARG(I) relative to some
!           center of motion.  This new center goes in
!           CTARG(I+1) and the state is called STEMP.
!
            I = I + 1

            CALL SPKPVN ( HANDLE,    DESCR,      ET, &
                          TFRAME(I), STARG(1,I), CTARG(I) )
!
!           Here's what we have.  STARG is the state of CTARG(I-1)
!           relative to CTARG(I) in reference frame TFRAME(I)
!
!           If one of the routines above failed during
!           execution, we just give up and check out.
!
            IF ( FAILED() ) THEN
               CALL CHKOUT ( RNAME )
               RETURN
            END IF

         END IF

      END DO

      TFRAME(1) = TFRAME(2)
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
      IF ( I .EQ. CHLEN ) THEN

         DO WHILE (      FOUND &
                   .AND. CTARG(CHLEN) .NE. SSB &
                   .AND. CTARG(CHLEN) .NE. OBS )


!
!           Find a file and segment that has state
!           data for CTARG(CHLEN).
!
            CALL SPKSFS ( CTARG(CHLEN), ET, HANDLE, DESCR, IDENT, FOUND)

            IF ( FOUND ) THEN
!
!              Get the state of CTARG(CHLEN) relative to
!              some center of motion.  The new center
!              overwrites the old.  The state is called
!              STEMP.
!
               CALL SPKPVN ( HANDLE, DESCR, ET, TMPFRM, STEMP, &
                             CTARG(CHLEN))

!
!              Add STEMP to the state of TARG relative to
!              the old center to get the state of TARG
!              relative to the new center.  Overwrite
!              the last element of STARG.
!
               IF ( TFRAME(CHLEN) .EQ. TMPFRM ) THEN

                  CALL MOVED ( STARG(1,CHLEN), 6, VTEMP )

               ELSE IF ( ISINRT ( TFRAME(CHLEN), TMPFRM ) ) THEN

                  CALL IRFROT( TFRAME(CHLEN), TMPFRM,         ROT      )
                  CALL MXV   ( ROT,           STARG(1,CHLEN), VTEMP(1) )
                  CALL MXV   ( ROT,           STARG(4,CHLEN), VTEMP(4) )

               ELSE

                  CALL FRMCHG( TFRAME(CHLEN), TMPFRM,   ET,   STXFRM )

                  IF ( FAILED() ) THEN
                     CALL CHKOUT ( RNAME )
                     RETURN
                  END IF

                  CALL MXVG  ( STXFRM, STARG (1,CHLEN), 6, 6, VTEMP  )

               END IF

               CALL VADDG ( VTEMP,  STEMP,        6, STARG(1,CHLEN) )

               TFRAME(CHLEN) = TMPFRM

!
!              If one of the routines above failed during
!              execution, we just give up and check out.
!
               IF ( FAILED() ) THEN
                  CALL CHKOUT ( RNAME )
                  RETURN
               END IF

            END IF

         END DO

      END IF

      NCT = I
!
!     NCT is the number of elements in CTARG,
!     the chain length.  We have in hand the following information
!
!        STARG(1...6,K)  state of body
!        CTARG(K-1)      relative to body CTARG(K) in the frame
!        TFRAME(K)
!
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
      COBS = OBS
      CALL CLEARD ( 6, SOBS )

!
!     Perhaps we have a common node already.
!     If so it will be the last node on the
!     list CTARG.
!
!     We let CTPOS will be the position of the common
!     node in CTARG if one is found.  It will
!     be zero if COBS is not found in CTARG.
!
      IF ( CTARG(NCT) .EQ. COBS ) THEN
         CTPOS  = NCT
         CFRAME = TFRAME(CTPOS)
      ELSE
         CTPOS  = 0
      END IF

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
      FOUND  = .TRUE.
      NOFRM  = .TRUE.
      LEGS   =  0

      DO WHILE (           FOUND &
                  .AND.  ( COBS  .NE. SSB ) &
                  .AND.  ( CTPOS .EQ. 0   )  )

!
!        Find a file and segment that has state
!        data for COBS.
!
         CALL SPKSFS ( COBS, ET, HANDLE, DESCR, IDENT, FOUND )

         IF ( FOUND ) THEN
!
!           Get the state of COBS; call it STEMP.
!           The center of motion of COBS becomes the
!           new COBS.
!
            IF ( LEGS .EQ. 0 ) THEN
               CALL SPKPVN ( HANDLE, DESCR, ET, TMPFRM, SOBS,  COBS )
            ELSE
               CALL SPKPVN ( HANDLE, DESCR, ET, TMPFRM, STEMP, COBS )
            END IF

            IF ( NOFRM ) THEN
               NOFRM  = .FALSE.
               CFRAME = TMPFRM
            END IF
!
!           Add STEMP to the state of OBS relative to
!           the old COBS to get the state of OBS
!           relative to the new COBS.
!
            IF ( CFRAME .EQ. TMPFRM ) THEN
!
!              On the first leg of the state of the observer, we
!              don't have to add anything, the state of the observer
!              is already in SOBS.  We only have to add when the
!              number of legs in the observer state is one or greater.
!
               IF ( LEGS .GT. 0 ) THEN
                  CALL VADDG ( SOBS,  STEMP, 6, VTEMP )
                  CALL MOVED ( VTEMP,        6, SOBS  )
               END IF

            ELSE IF ( ISINRT( CFRAME, TMPFRM ) ) THEN

               CALL IRFROT ( CFRAME, TMPFRM, ROT )
               CALL MXV    ( ROT,    SOBS(1),   VTEMP(1)  )
               CALL MXV    ( ROT,    SOBS(4),   VTEMP(4)  )
               CALL VADDG  ( VTEMP,  STEMP,     6, SOBS   )
               CFRAME = TMPFRM

            ELSE

               CALL FRMCHG ( CFRAME, TMPFRM, ET,   STXFRM )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( RNAME )
                  RETURN
               END IF

               CALL MXVG   ( STXFRM, SOBS,   6, 6, VTEMP  )
               CALL VADDG  ( VTEMP,  STEMP,     6, SOBS   )
               CFRAME = TMPFRM

            END IF

!
!           Check failed.  We don't want to loop
!           indefinitely.
!
            IF ( FAILED() ) THEN
               CALL CHKOUT ( RNAME )
               RETURN
            END IF

!
!           We now have one more leg of the path for OBS.  Set
!           LEGS to reflect this.  Then see if the new center
!           is a common node. If not, repeat the loop.
!
            LEGS  = LEGS + 1
            CTPOS = ISRCHI ( COBS, NCT, CTARG )

         END IF

      END DO


!
!     If CTPOS is zero at this point, it means we
!     have not found a common node though we have
!     searched through all the available data.
!
      IF ( CTPOS .EQ. 0 ) THEN

         CALL BODC2N ( TARG, TNAME,   FOUND )

         IF ( FOUND ) THEN
            CALL PREFIX ( '# (',  0,        TNAME )
            CALL SUFFIX ( ')',    0,        TNAME )
            CALL REPMI  ( TNAME, '#', TARG, TNAME )
         ELSE
            CALL INTSTR ( TARG, TNAME )
         END IF


         CALL BODC2N ( OBS,  ONAME,   FOUND )

         IF ( FOUND ) THEN
            CALL PREFIX ( '# (',  0,       ONAME )
            CALL SUFFIX ( ')',    0,       ONAME )
            CALL REPMI  ( ONAME, '#', OBS, ONAME )
         ELSE
            CALL INTSTR ( OBS, ONAME )
         END IF

         CALL SETMSG ( 'Insufficient ephemeris data has been ' &
         //            'loaded to compute the state of TARG ' &
         //            'relative to OBS at the ephemeris epoch ' &
         //            '#. ' )

         CALL ETCAL  (  ET,    TSTRING                                 )
         CALL ERRCH  ( 'TARG', TNAME                                   )
         CALL ERRCH  ( 'OBS',  ONAME                                   )
         CALL ERRCH  ( '#',    TSTRING                                 )
         CALL SIGERR ( 'SPICE(SPKINSUFFDATA)'                          )
         CALL CHKOUT ( RNAME                                           )
         RETURN

      END IF

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
!
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
!
!     And the light-time between them is just
!
!               | STATE |
!          LT = ---------
!                   c
!
!
!     Compute the state of the target relative to CTARG(CTPOS)
!
      IF ( CTPOS .EQ. 1 ) THEN
         TFRAME(1) = CFRAME
      END IF

      DO I = 2, CTPOS-1

         IF ( TFRAME(I) .EQ. TFRAME(I+1) ) THEN

            CALL VADDG  ( STARG(1,I), STARG(1,I+1), 6, VTEMP        )
            CALL MOVED  ( VTEMP,                    6, STARG(1,I+1) )

         ELSE IF ( ISINRT ( TFRAME(I), TFRAME(I+1) ) ) THEN

            CALL IRFROT ( TFRAME(I),  TFRAME(I+1),     ROT          )
            CALL MXV    ( ROT,        STARG(1,I),      STEMP(1)     )
            CALL MXV    ( ROT,        STARG(4,I),      STEMP(4)     )
            CALL VADDG  ( STEMP,      STARG(1,I+1), 6, VTEMP        )
            CALL MOVED  ( VTEMP,                    6, STARG(1,I+1) )

         ELSE

            CALL FRMCHG ( TFRAME(I),  TFRAME(I+1), ET,  STXFRM       )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( RNAME )
               RETURN
            END IF

            CALL MXVG   ( STXFRM,     STARG(1,I),  6,6, STEMP        )
            CALL VADDG  ( STEMP,      STARG(1,I+1),  6, VTEMP        )
            CALL MOVED  ( VTEMP,                     6, STARG(1,I+1) )

         END IF

      END DO


!
!     To avoid unnecessary frame transformations we'll do
!     a bit of extra decision making here.  It's a lot
!     faster to make logical checks than it is to compute
!     frame transformations.
!
      IF ( TFRAME(CTPOS) .EQ. CFRAME ) THEN

         CALL VSUBG  ( STARG(1,CTPOS), SOBS, 6,   STATE  )

      ELSE IF ( TFRAME(CTPOS) .EQ. REFID ) THEN

!
!        If the last frame associated with the target is already
!        in the requested output frame, we convert the state of
!        the observer to that frame and then subtract the state
!        of the observer from the state of the target.
!
         IF ( ISINRT( CFRAME, REFID ) ) THEN

            CALL IRFROT ( CFRAME, REFID,   ROT      )
            CALL MXV    ( ROT,    SOBS(1), STEMP(1) )
            CALL MXV    ( ROT,    SOBS(4), STEMP(4) )

         ELSE

            CALL FRMCHG ( CFRAME, REFID, ET,  STXFRM )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( RNAME )
               RETURN
            END IF

            CALL MXVG   ( STXFRM, SOBS,  6,6, STEMP  )

         END IF

!
!        We've now transformed SOBS into the requested reference frame.
!        Set CFRAME to reflect this.
!
         CFRAME = REFID
         CALL VSUBG ( STARG(1,CTPOS),  STEMP, 6, STATE )


      ELSE IF ( ISINRT( TFRAME(CTPOS), CFRAME ) ) THEN

!
!        If both frames are inertial we use IRFROT instead of
!        FRMCHG to get things into a common frame.
!
         CALL IRFROT ( TFRAME(CTPOS),  CFRAME,               ROT      )
         CALL MXV    ( ROT,            STARG(1,CTPOS),       STEMP(1) )
         CALL MXV    ( ROT,            STARG(4,CTPOS),       STEMP(4) )
         CALL VSUBG  ( STEMP,          SOBS,            6,   STATE    )

      ELSE
!
!        Use the more general routine FRMCHG to make the transformation.
!
         CALL FRMCHG ( TFRAME(CTPOS),  CFRAME,          ET,  STXFRM )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( RNAME )
            RETURN
         END IF

         CALL MXVG   ( STXFRM,         STARG(1,CTPOS),  6,6, STEMP  )
         CALL VSUBG  ( STEMP,          SOBS,            6,   STATE  )

      END IF

!
!     Finally, rotate as needed into the requested frame.
!
      IF ( CFRAME .EQ. REFID ) THEN

!
!        We don't have to do anything in this case.
!

      ELSE IF ( ISINRT ( CFRAME, REFID ) ) THEN
!
!        Since both frames are inertial, we use the more direct
!        routine IRFROT to get the transformation to REFID.
!
         CALL IRFROT ( CFRAME,  REFID,      ROT      )
         CALL MXV    ( ROT,     STATE(1),   STEMP(1) )
         CALL MXV    ( ROT,     STATE(4),   STEMP(4) )
         CALL MOVED  ( STEMP,   6,          STATE    )

      ELSE

         CALL FRMCHG ( CFRAME,  REFID, ET,  STXFRM )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( RNAME )
            RETURN
         END IF

         CALL MXVG   ( STXFRM,  STATE, 6, 6, STEMP )
         CALL MOVED  ( STEMP,          6,    STATE )

      END IF

      LT = VNORM  ( STATE ) / CLIGHT()

      CALL CHKOUT ( RNAME )
      RETURN
      END
