!$Procedure SPKGPS ( S/P Kernel, geometric position )

      SUBROUTINE SPKGPS ( TARG, ET, REF, OBS, POS, LT )

! $ Abstract
!
!     Compute the geometric position of a target body relative to an
!     observing body.
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
!     SPK
!
! $ Keywords
!
!     EPHEMERIS
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE               'ninert.inc'
      INCLUDE               'zzctr.inc'

      INTEGER               TARG
      DOUBLE PRECISION      ET
      CHARACTER*(*)         REF
      INTEGER               OBS
      DOUBLE PRECISION      POS ( 3 )
      DOUBLE PRECISION      LT

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     TARG       I   Target body.
!     ET         I   Target epoch.
!     REF        I   Target reference frame.
!     OBS        I   Observing body.
!     POS        O   Position of target.
!     LT         O   Light time.
!
! $ Detailed_Input
!
!     TARG     is the standard NAIF ID code for a target body.
!
!     ET       is the epoch (ephemeris time) at which the position
!              of the target body is to be computed.
!
!     REF      is the name of the reference frame to
!              which the vectors returned by the routine should
!              be rotated. This may be any frame supported by
!              the SPICELIB subroutine REFCHG.
!
!     OBS      is the standard NAIF ID code for an observing body.
!
! $ Detailed_Output
!
!     POS      is a 3-dimensional vector that contains the position of
!              the target body, relative to the observing body. This
!              vector is rotated into the specified reference frame.
!              Units are always km.
!
!     LT       is the one-way light time from the observing body
!              to the geometric position of the target body at the
!              specified epoch.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If insufficient ephemeris data has been loaded to compute
!         the necessary positions, the error SPICE(SPKINSUFFDATA) is
!         signaled.
!
! $ Files
!
!     See $Restrictions.
!
! $ Particulars
!
!     SPKGPS computes the geometric position, T(t), of the target
!     body and the geometric position, O(t), of the observing body
!     relative to the first common center of motion. Subtracting
!     O(t) from T(t) gives the geometric position of the target
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
!     O(t) is the position of -94 relative to 4 and T(t) is the
!     position of 401 relative to 4.
!
!     The center could also be the Solar System Barycenter, body 0.
!     For example, if the observer is 399, Earth, and the target
!     is 299, Venus, then O(t) would be the position of 399 relative
!     to 0 and T(t) would be the position of 299 relative to 0.
!
!     Ephemeris data from more than one segment may be required
!     to determine the positions of the target body and observer
!     relative to a common center. SPKGPS reads as many segments
!     as necessary, from as many files as necessary, using files
!     that have been loaded by previous calls to SPKLEF (load
!     ephemeris file).
!
!     SPKGPS is similar to SPKGEO but returns geometric positions
!     only.
!
! $ Examples
!
!     The following code example computes the geometric
!     position of the moon with respect to the earth and
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
!            INTEGER               I
!            CHARACTER*(20)        UTC
!            DOUBLE PRECISION      BEGIN
!            DOUBLE PRECISION      DELTA
!            DOUBLE PRECISION      END
!            DOUBLE PRECISION      ET
!            DOUBLE PRECISION      POS ( 3 )
!            DOUBLE PRECISION      LT
!
!            DOUBLE PRECISION      VNORM
!
!     C
!     C      Load the binary SPK ephemeris file.
!     C
!            CALL FURNSH ( 'SAMPLE.BSP' )
!
!            .
!            .
!            .
!
!     C
!     C      Divide the interval of coverage [BEGIN,END] into
!     C      N steps. At each step, compute the position, and
!     C      print out the epoch in UTC time and position norm.
!     C
!            DELTA = ( END - BEGIN ) / N
!
!            DO I = 0, N
!
!               ET = BEGIN + I*DELTA
!
!               CALL SPKGPS ( MOON, ET, 'J2000', EARTH, POS, LT )
!
!               CALL ET2UTC ( ET, 'C', 0, UTC )
!
!               WRITE (*,*) UTC, VNORM ( POS )
!
!            END DO
!
! $ Restrictions
!
!     1)  The ephemeris files to be used by SPKGPS must be loaded
!         by SPKLEF before SPKGPS is called.
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
!-    SPICELIB Version 2.1.0, 09-OCT-2021 (JDR) (NJB)
!
!        Bug fix: added calls to FAILED after calls to SPKPVN.
!        Previously only one call to SPKPVN was followed by a FAILED
!        call. Moved some FAILED checks so they will be hit whether
!        or not SPKSFS finds a segment.
!
!        Edited the header to comply with NAIF standard. Removed
!        unnecessary $Revisions section.
!
!-    SPICELIB Version 2.0.0, 08-JAN-2014 (BVS)
!
!        Updated to save the input frame name and POOL state counter
!        and to do frame name-ID conversion only if the counter has
!        changed.
!
!        Updated to map the input frame name to its ID by first calling
!        ZZNAMFRM, and then calling IRFNUM. The side effect of this
!        change is that now the frame with the fixed name 'DEFAULT'
!        that can be associated with any code via CHGIRF's entry point
!        IRFDEF will be fully masked by a frame with identical name
!        defined via a text kernel. Previously the CHGIRF's 'DEFAULT'
!        frame masked the text kernel frame with the same name.
!
!        Replaced SPKLEF with FURNSH and fixed errors in $Examples.
!
!-    SPICELIB Version 1.2.0, 05-NOV-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in VADD calls.
!
!-    SPICELIB Version 1.1.0, 05-JAN-2005 (NJB)
!
!        Tests of routine FAILED() were added.
!
!-    SPICELIB Version 1.0.0, 09-JUL-1998 (WLT)
!
!-&


! $ Index_Entries
!
!     geometric position of one body relative to another
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
!     about A), eventually we get the position relative to the solar
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
!     is the position of body X relative to body Y.
!
!     However, we don't want to follow each chain back to the SSB
!     if it isn't necessary.  Instead we will just follow the chain
!     of the target body and follow the chain of the observing body
!     until we find a common node in the tree.
!
!     In the example below, C is the first common node.  We compute
!     the position of TARG relative to C and the position of OBS
!     relative to C, then subtract the two positions.
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
      PARAMETER           ( RNAME  =  'SPKGPS' )

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
!     Saved frame name length.
!
      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )


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

      DOUBLE PRECISION      PSXFRM ( 3, 3     )
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
!     Saved frame name/ID item declarations.
!
      INTEGER               SVCTR1 ( CTRSIZ )
      CHARACTER*(FRNMLN)    SVREF
      INTEGER               SVREFI

      LOGICAL               FIRST

!
!     Saved frame name/ID items.
!
      SAVE                  SVCTR1
      SAVE                  SVREF
      SAVE                  SVREFI

      SAVE                  FIRST

!
!     Initial values.
!
      DATA                  FIRST   / .TRUE. /


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
!     Initialization.
!
      IF ( FIRST ) THEN

!
!        Initialize counter.
!
         CALL ZZCTRUIN( SVCTR1 )

         FIRST = .FALSE.

      END IF

!
!     We take care of the obvious case first.  It TARG and OBS are the
!     same we can just fill in zero.
!
      IF ( TARG .EQ. OBS ) THEN

         LT = 0.0D0

         CALL CLEARD ( 3, POS )
         CALL CHKOUT ( RNAME )
         RETURN

      END IF


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
      CALL ZZNAMFRM ( SVCTR1, SVREF, SVREFI, REF, REFID )

      IF ( REFID .EQ. 0 ) THEN
         CALL IRFNUM ( REF, REFID )
      END IF

      IF ( REFID .EQ. 0 ) THEN

         IF (  FRSTNP(REF) .GT. 0 ) THEN

            CALL SETMSG ( 'The string supplied to specify the ' &
            //            'reference frame, (''#'') contains ' &
            //            'non-printing characters.  The two ' &
            //            'most common causes for this kind ' &
            //            'of error are: 1. an error in the ' &
            //            'call to SPKGPS; 2. an ' &
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

         CALL SIGERR ( 'SPICE(UNKNOWNFRAME)' )

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
!     first elements:  TARG and the position of TARG relative
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
!        Find a file and segment that has position
!        data for CTARG(I).
!
         CALL SPKSFS ( CTARG(I), ET, HANDLE, DESCR, IDENT, FOUND )

         IF ( FOUND ) THEN
!
!           Get the position of CTARG(I) relative to some
!           center of motion.  This new center goes in
!           CTARG(I+1) and the position is called STEMP.
!
            I = I + 1

            CALL SPKPVN ( HANDLE,    DESCR,      ET, &
                          TFRAME(I), STARG(1,I), CTARG(I) )
!
!           Here's what we have.  STARG is the position of CTARG(I-1)
!           relative to CTARG(I) in reference frame TFRAME(I)
!
         END IF
!
!        If one of the routines above failed during
!        execution, we just give up and check out.
!
         IF ( FAILED() ) THEN
            CALL CHKOUT ( RNAME )
            RETURN
         END IF

      END DO

      TFRAME(1) = TFRAME(2)
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
      IF ( I .EQ. CHLEN ) THEN

         DO WHILE (      FOUND &
                   .AND. CTARG(CHLEN) .NE. SSB &
                   .AND. CTARG(CHLEN) .NE. OBS )


!
!           Find a file and segment that has position
!           data for CTARG(CHLEN).
!
            CALL SPKSFS ( CTARG(CHLEN), ET, HANDLE, DESCR, IDENT, FOUND)

            IF ( FOUND ) THEN
!
!              Get the position of CTARG(CHLEN) relative to
!              some center of motion.  The new center
!              overwrites the old.  The position is called
!              STEMP.
!
               CALL SPKPVN ( HANDLE, DESCR, ET, TMPFRM, STEMP, &
                             CTARG(CHLEN))

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( RNAME )
                  RETURN
               END IF

!
!              Add STEMP to the position of TARG relative to
!              the old center to get the position of TARG
!              relative to the new center.  Overwrite
!              the last element of STARG.
!
               IF ( TFRAME(CHLEN) .EQ. TMPFRM ) THEN

                  CALL MOVED ( STARG(1,CHLEN), 3, VTEMP )

               ELSE IF ( ISINRT ( TFRAME(CHLEN), TMPFRM ) ) THEN

                  CALL IRFROT( TFRAME(CHLEN), TMPFRM,         ROT      )
                  CALL MXV   ( ROT,           STARG(1,CHLEN), VTEMP(1) )

               ELSE

                  CALL REFCHG( TFRAME(CHLEN), TMPFRM,   ET,   PSXFRM )

                  IF ( FAILED() ) THEN
                     CALL CHKOUT ( RNAME )
                     RETURN
                  END IF

                  CALL MXV   ( PSXFRM, STARG (1,CHLEN),       VTEMP  )

               END IF

               CALL VADD ( VTEMP,  STEMP, STARG(1,CHLEN) )

               TFRAME(CHLEN) = TMPFRM

            END IF

!
!           If one of the routines above failed during
!           execution, we just give up and check out.
!
            IF ( FAILED() ) THEN
               CALL CHKOUT ( RNAME )
               RETURN
            END IF

         END DO

      END IF

      NCT = I
!
!     NCT is the number of elements in CTARG,
!     the chain length.  We have in hand the following information
!
!        STARG(1...3,K)  position of body
!        CTARG(K-1)      relative to body CTARG(K) in the frame
!        TFRAME(K)
!
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
!     positions array, just a single center and position
!     is sufficient --- we just keep overwriting them.
!     When the common node is found, we have everything
!     we need in that one center (COBS) and position
!     (SOBS-position of the target relative to COBS).
!
      FOUND  = .TRUE.
      NOFRM  = .TRUE.
      LEGS   =  0

      DO WHILE (           FOUND &
                  .AND.  ( COBS  .NE. SSB ) &
                  .AND.  ( CTPOS .EQ. 0   )  )

!
!        Find a file and segment that has position
!        data for COBS.
!
         CALL SPKSFS ( COBS, ET, HANDLE, DESCR, IDENT, FOUND )

         IF ( FOUND ) THEN
!
!           Get the position of COBS; call it STEMP.
!           The center of motion of COBS becomes the
!           new COBS.
!
            IF ( LEGS .EQ. 0 ) THEN
               CALL SPKPVN ( HANDLE, DESCR, ET, TMPFRM, SOBS,  COBS )
            ELSE
               CALL SPKPVN ( HANDLE, DESCR, ET, TMPFRM, STEMP, COBS )
            END IF

            IF ( FAILED() ) THEN
               CALL CHKOUT ( RNAME )
               RETURN
            END IF

            IF ( NOFRM ) THEN
               NOFRM  = .FALSE.
               CFRAME = TMPFRM
            END IF
!
!           Add STEMP to the position of OBS relative to
!           the old COBS to get the position of OBS
!           relative to the new COBS.
!
            IF ( CFRAME .EQ. TMPFRM ) THEN
!
!              On the first leg of the position of the observer, we
!              don't have to add anything, the position of the
!              observer is already in SOBS.  We only have to add when
!              the number of legs in the observer position is one or
!              greater.
!
               IF ( LEGS .GT. 0 ) THEN
                  CALL VADD ( SOBS,  STEMP, VTEMP )
                  CALL VEQU ( VTEMP,        SOBS  )
               END IF

            ELSE IF ( ISINRT( CFRAME, TMPFRM ) ) THEN

               CALL IRFROT ( CFRAME, TMPFRM, ROT )
               CALL MXV    ( ROT,    SOBS(1),   VTEMP(1)  )
               CALL VADD   ( VTEMP,  STEMP,     SOBS      )
               CFRAME = TMPFRM

            ELSE

               CALL REFCHG ( CFRAME, TMPFRM, ET,   PSXFRM )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( RNAME )
                  RETURN
               END IF

               CALL MXV    ( PSXFRM, SOBS,         VTEMP  )
               CALL VADD   ( VTEMP,  STEMP,        SOBS   )
               CFRAME = TMPFRM

            END IF

!
!           We now have one more leg of the path for OBS.  Set
!           LEGS to reflect this.  Then see if the new center
!           is a common node. If not, repeat the loop.
!
            LEGS  = LEGS + 1
            CTPOS = ISRCHI ( COBS, NCT, CTARG )

         END IF

!
!        Check failed.  We don't want to loop indefinitely.
!
         IF ( FAILED() ) THEN
            CALL CHKOUT ( RNAME )
            RETURN
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
         //            'loaded to compute the position of TARG ' &
         //            'relative to OBS at the ephemeris epoch ' &
         //            '#. ' )

         CALL ETCAL  (  ET,    TSTRING                                 )
         CALL ERRCH  ( 'TARG', TNAME                                   )
         CALL ERRCH  ( 'OBS',  ONAME                                   )
         CALL ERRCH  ( '#',    TSTRING                                 )
         CALL SIGERR ( 'SPICE(SPKINSUFFDATA)'                          )
         CALL CHKOUT ( RNAME                                        )
         RETURN

      END IF

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
!
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
!
!     And the light-time between them is just
!
!               | POSITION |
!          LT = ---------
!                   c
!
!
!     Compute the position of the target relative to CTARG(CTPOS)
!
      IF ( CTPOS .EQ. 1 ) THEN
         TFRAME(1) = CFRAME
      END IF

      DO I = 2, CTPOS-1

         IF ( TFRAME(I) .EQ. TFRAME(I+1) ) THEN

            CALL VADD  ( STARG(1,I), STARG(1,I+1),  STEMP        )
            CALL MOVED ( STEMP,      3,             STARG(1,I+1) )

         ELSE IF ( ISINRT ( TFRAME(I), TFRAME(I+1) ) ) THEN

            CALL IRFROT ( TFRAME(I),  TFRAME(I+1),     ROT          )
            CALL MXV    ( ROT,        STARG(1,I),      STEMP(1)     )
            CALL VADD   ( STEMP,      STARG(1,I+1),    VTEMP        )
            CALL MOVED  ( VTEMP,      3,               STARG(1,I+1) )

         ELSE

            CALL REFCHG ( TFRAME(I),  TFRAME(I+1), ET,  PSXFRM       )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( RNAME )
               RETURN
            END IF

            CALL MXV    ( PSXFRM,     STARG(1,I),       STEMP        )
            CALL VADD   ( STEMP,      STARG(1,I+1),     VTEMP        )
            CALL MOVED  ( VTEMP,      3,                STARG(1,I+1) )

         END IF

      END DO


!
!     To avoid unnecessary frame transformations we'll do
!     a bit of extra decision making here.  It's a lot
!     faster to make logical checks than it is to compute
!     frame transformations.
!
      IF ( TFRAME(CTPOS) .EQ. CFRAME ) THEN

         CALL VSUB   ( STARG(1,CTPOS), SOBS,  POS  )

      ELSE IF ( TFRAME(CTPOS) .EQ. REFID ) THEN

!
!        If the last frame associated with the target is already
!        in the requested output frame, we convert the position of
!        the observer to that frame and then subtract the position
!        of the observer from the position of the target.
!
         IF ( ISINRT( CFRAME, REFID ) ) THEN

            CALL IRFROT ( CFRAME, REFID,   ROT      )
            CALL MXV    ( ROT,    SOBS(1), STEMP(1) )

         ELSE

            CALL REFCHG ( CFRAME, REFID, ET,  PSXFRM )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( RNAME )
               RETURN
            END IF

            CALL MXV    ( PSXFRM, SOBS,       STEMP  )

         END IF

!
!        We've now transformed SOBS into the requested reference frame.
!        Set CFRAME to reflect this.
!
         CFRAME = REFID
         CALL VSUB  ( STARG(1,CTPOS), STEMP, POS )


      ELSE IF ( ISINRT( TFRAME(CTPOS), CFRAME ) ) THEN

!
!        If both frames are inertial we use IRFROT instead of
!        REFCHG to get things into a common frame.
!
         CALL IRFROT ( TFRAME(CTPOS),  CFRAME,               ROT      )
         CALL MXV    ( ROT,            STARG(1,CTPOS),       STEMP(1) )
         CALL VSUB   ( STEMP,          SOBS,                 POS    )

      ELSE
!
!        Use the more general routine REFCHG to make the transformation.
!
         CALL REFCHG ( TFRAME(CTPOS),  CFRAME,          ET,  PSXFRM )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( RNAME )
            RETURN
         END IF

         CALL MXV    ( PSXFRM,         STARG(1,CTPOS),       STEMP  )
         CALL VSUB   ( STEMP,          SOBS,                 POS  )

      END IF

!
!     Finally, rotate as needed into the requested frame.
!
      IF      ( CFRAME .EQ. REFID ) THEN

!
!        We don't have to do anything in this case.
!

      ELSE IF ( ISINRT ( CFRAME, REFID ) ) THEN
!
!        Since both frames are inertial, we use the more direct
!        routine IRFROT to get the transformation to REFID.
!
         CALL IRFROT ( CFRAME,  REFID,    ROT          )
         CALL MXV    ( ROT,     POS(1),   STEMP(1)     )
         CALL MOVED  ( STEMP,        3,   POS          )

      ELSE

         CALL REFCHG ( CFRAME,  REFID, ET,  PSXFRM  )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( RNAME )
            RETURN
         END IF

         CALL MXV    ( PSXFRM,  POS,        STEMP   )
         CALL MOVED  ( STEMP,          3,   POS     )

      END IF

      LT = VNORM  ( POS ) / CLIGHT()

      CALL CHKOUT ( RNAME )
      RETURN
      END
