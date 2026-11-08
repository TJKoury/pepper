!$Procedure ZZSPKGO0 ( S/P Kernel, geometric state )

      SUBROUTINE ZZSPKGO0 ( TARG, ET, REF, OBS, STATE, LT )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Compute the geometric state (position and velocity) of a target
!     body relative to an observing body.
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
      DOUBLE PRECISION      STATE    ( 6 )
      DOUBLE PRECISION      LT

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
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
!     TARG     is the standard NAIF ID code for a target body.
!
!     ET       is the epoch (ephemeris time) at which the state
!              of the target body is to be computed.
!
!     REF      is the name of the reference frame to
!              which the vectors returned by the routine should
!              be rotated. This may be any frame supported by
!              the SPICELIB subroutine FRMCHG.
!
!     OBS      is the standard NAIF ID code for an observing body.
!
! $ Detailed_Output
!
!     STATE    is a 6-dimensional vector that contains the geometric
!              position and velocity of the target body, relative to the
!              observing body, at epoch ET. STATE has six elements: the
!              first three contain the target's position; the last three
!              contain the target's velocity. These vectors are
!              transformed into the specified reference frame. Units are
!              always km and km/sec.
!
!     LT       is the one-way light time in seconds from the
!              observing body to the geometric position of the
!              target body at the specified epoch.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If insufficient ephemeris data has been loaded to compute
!         the necessary states, the error SPICE(SPKINSUFFDATA) is
!         signaled.
!
! $ Files
!
!     See $Restrictions.
!
! $ Particulars
!
!     SPKGEO computes the geometric state, T(t), of the target
!     body and the geometric state, O(t), of the observing body
!     relative to the first common center of motion. Subtracting
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
!     relative to a common center. SPKGEO reads as many segments
!     as necessary, from as many files as necessary, using files
!     that have been loaded by previous calls to FURNSH or SPKLEF
!     (load ephemeris file).
!
!     SPKGEO is similar to SPKEZ but returns geometric states
!     only, with no option to make planetary (light-time) nor
!     stellar aberration corrections. The geometric states
!     returned by SPKEZ and SPKGEO are the same.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Return the geometric state vector of Mars (499) as seen from
!        Earth (399) in the J2000 frame and the one-way light time
!        between them at the epoch July 4, 2003 11:00 AM PST.
!
!        Use the meta-kernel shown below to load the required SPICE
!        kernels.
!
!
!           KPL/MK
!
!           File: spkgeo_ex1.tm
!
!           This meta-kernel is intended to support operation of SPICE
!           example programs. The kernels shown here should not be
!           assumed to contain adequate or correct versions of data
!           required by SPICE-based user applications.
!
!           In order for an application to use this meta-kernel, the
!           kernels referenced here must be present in the user's
!           current working directory.
!
!           The names and contents of the kernels referenced
!           by this meta-kernel are as follows:
!
!              File name                        Contents
!              ---------                        --------
!              de430.bsp                        Planetary ephemeris
!              mar097.bsp                       Mars satellite ephemeris
!              naif0011.tls                     Leapseconds
!
!
!           \begindata
!
!              KERNELS_TO_LOAD = ( 'de430.bsp',
!                                  'mar097.bsp',
!                                  'naif0011.tls' )
!
!           \begintext
!
!           End of meta-kernel
!
!
!        Example code begins here.
!
!
!              PROGRAM SPKGEO_EX1
!              IMPLICIT NONE
!
!        C
!        C     SPICELIB functions
!        C
!              DOUBLE PRECISION      VNORM
!
!        C
!        C     Local parameters.
!        C
!              INTEGER               NAMLEN
!              PARAMETER           ( NAMLEN = 32 )
!
!              INTEGER               TIMLEN
!              PARAMETER           ( TIMLEN = 26 )
!
!        C
!        C     Local variables.
!        C
!              CHARACTER*(TIMLEN)    EPOCH
!              CHARACTER*(NAMLEN)    REFFRM
!
!              DOUBLE PRECISION      ET
!              DOUBLE PRECISION      LT
!              DOUBLE PRECISION      STATE ( 6 )
!
!              INTEGER               I
!              INTEGER               OBSRVR
!              INTEGER               TARGET
!
!
!        C
!        C     Load a set of kernels: an SPK file, a PCK
!        C     file and a leapseconds file. Use a meta
!        C     kernel for convenience.
!        C
!              CALL FURNSH ( 'spkgeo_ex1.tm' )
!
!        C
!        C     Define parameters for a state lookup.
!        C
!              TARGET = 499
!              EPOCH  = 'July 4, 2003 11:00 AM PST'
!              REFFRM = 'J2000'
!              OBSRVR = 399
!
!        C
!        C     Convert the epoch to ephemeris time.
!        C
!              CALL STR2ET ( EPOCH, ET )
!
!        C
!        C     Look-up the state for the defined parameters.
!        C
!              CALL SPKGEO ( TARGET, ET, REFFRM, OBSRVR, STATE, LT )
!
!        C
!        C     Output...
!        C
!              WRITE(*,'(A,I3)') 'The position of    : ', TARGET
!              WRITE(*,'(A,I3)') 'As observed from   : ', OBSRVR
!              WRITE(*,'(2A)')   'In reference frame : ', REFFRM
!              WRITE(*,'(2A)')   'At epoch           : ', EPOCH
!              WRITE(*,*) ' '
!
!        C
!        C     The first three entries of state contain the
!        C     X, Y, Z position components. The final three contain
!        C     the Vx, Vy, Vz velocity components.
!        C
!              WRITE(*,'(A,3F18.6)') 'R   (km):', ( STATE(I), I=1,3 )
!              WRITE(*,'(A,3F18.6)') 'V (km/s):', ( STATE(I), I=4,6 )
!              WRITE(*,*) ' '
!              WRITE(*,'(A,F19.14)') 'Light time (s) between observer '
!             .                   // 'and target: ', LT
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        The position of    : 499
!        As observed from   : 399
!        In reference frame : J2000
!        At epoch           : July 4, 2003 11:00 AM PST
!
!        R   (km):   73826216.435288  -27128030.732406  -18741973.868287
!        V (km/s):         -6.809504          7.513814          3.001290
!
!        Light time (s) between observer and target:  269.70264776317532
!
!
! $ Restrictions
!
!     1)  The ephemeris files to be used by SPKGEO must be loaded
!         by FURNSH or SPKLEF before SPKGEO is called.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     J.E. McLean        (JPL)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 3.1.0, 09-OCT-2021 (JDR) (NJB)
!
!        Bug fix: added calls to FAILED after calls to SPKPVN.
!        Previously only one call to SPKPVN was followed by a FAILED
!        call. Moved some FAILED checks so they will be hit whether
!        or not SPKSFS finds a segment.
!
!        Edited the header to comply with NAIF standard. Added complete
!        code example to $Examples section. Removed unnecessary
!        $Revisions section.
!
!        Added reference to FURNSH in $Particulars and $Restrictions
!        sections.
!
!-    SPICELIB Version 3.0.0, 08-JAN-2014 (BVS)
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
!        Fixed description of STATE in Detailed Output. Replaced
!        SPKLEF with FURNSH and fixed errors in $Examples.
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
!        transformations. Consequently slightly more
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
      PARAMETER           ( RNAME  =  'ZZSPKGO0' )

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
            //            'call to ZZSPKGO0; 2. an ' &
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

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( RNAME )
                  RETURN
               END IF

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

                  CALL ZZFRMCH0( TFRAME(CHLEN), TMPFRM,   ET,   STXFRM )

                  IF ( FAILED() ) THEN
                     CALL CHKOUT ( RNAME )
                     RETURN
                  END IF

                  CALL MXVG  ( STXFRM, STARG (1,CHLEN), 6, 6, VTEMP  )

               END IF

               CALL VADDG ( VTEMP,  STEMP,        6, STARG(1,CHLEN) )

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

            IF ( FAILED() ) THEN
               CALL CHKOUT ( RNAME )
               RETURN
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

               CALL ZZFRMCH0 ( CFRAME, TMPFRM, ET,   STXFRM )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( RNAME )
                  RETURN
               END IF

               CALL MXVG   ( STXFRM, SOBS,   6, 6, VTEMP  )
               CALL VADDG  ( VTEMP,  STEMP,     6, SOBS   )
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

         CALL BODC2N ( TARG, TNAME, FOUND )

         IF ( FOUND ) THEN
            CALL PREFIX ( '# (',  0,        TNAME )
            CALL SUFFIX ( ')',    0,        TNAME )
            CALL REPMI  ( TNAME, '#', TARG, TNAME )
         ELSE
            CALL INTSTR ( TARG, TNAME )
         END IF


         CALL BODC2N ( OBS, ONAME, FOUND )

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

            CALL ZZFRMCH0 ( TFRAME(I),  TFRAME(I+1), ET,  STXFRM       )

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

            CALL ZZFRMCH0 ( CFRAME, REFID, ET,  STXFRM )

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
!        ZZFRMCH0 to get things into a common frame.
!
         CALL IRFROT ( TFRAME(CTPOS),  CFRAME,               ROT      )
         CALL MXV    ( ROT,            STARG(1,CTPOS),       STEMP(1) )
         CALL MXV    ( ROT,            STARG(4,CTPOS),       STEMP(4) )
         CALL VSUBG  ( STEMP,          SOBS,            6,   STATE    )

      ELSE
!
!        Use the more general routine ZZFRMCH0 to make the
!        transformation.
!
         CALL ZZFRMCH0 ( TFRAME(CTPOS),  CFRAME,          ET,  STXFRM )

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

         CALL ZZFRMCH0 ( CFRAME,  REFID, ET,  STXFRM )

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
