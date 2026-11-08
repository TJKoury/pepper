!$Procedure ZZSPKGP0 ( S/P Kernel, geometric position )

      subroutine zzspkgp0 ( targ, et, ref, obs, pos, lt )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Compute the geometric position of a target body relative to an
!     observing body.
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

      implicit none

      include               'ninert.inc'
      include               'zzctr.inc'

      integer(ip)           targ
      real(wp)              et
      character*(*)         ref
      integer(ip)           obs
      real(wp)              pos ( 3 )
      real(wp)              lt

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
      logical               failed
      logical               return

      real(wp)              clight
      real(wp)              vnorm

      integer(ip)           frstnp
      integer(ip)           isrchi

!
!     Local parameters
!
      character*(*)         rname
      parameter           ( rname  =  'ZZSPKGP0' )

!
!     CHLEN is the maximum length of a chain.  That is,
!     it is the maximum number of bodies in the chain from
!     the target or observer to the SSB.
!
      integer(ip)           chlen
      parameter           ( chlen = 20 )

      integer(ip)           ssb
      parameter           ( ssb   =  0 )

!
!     Saved frame name length.
!
      integer(ip)           frnmln
      parameter           ( frnmln = 32 )


!
!     Local variables
!
      character*(40)        ident
      character*(40)        tname
      character*(40)        oname
      character*(80)        tstring

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

!
!     Saved frame name/ID item declarations.
!
      integer(ip)           svctr1 ( ctrsiz )
      character*(frnmln)    svref
      integer(ip)           svrefi

      logical               first

!
!     Saved frame name/ID items.
!
      save                  svctr1
      save                  svref
      save                  svrefi

      save                  first

!
!     Initial values.
!
      data                  first   / .true. /


!
!     In-line Function Definitions
!
      isinrt ( tmpfrm, cframe ) =       cframe .gt. 0 &
                                  .and. cframe .le. ninert &
                                  .and. tmpfrm .gt. 0 &
                                  .and. tmpfrm .le. ninert

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( rname )
      end if

!
!     Initialization.
!
      if ( first ) then

!
!        Initialize counter.
!
         call zzctruin( svctr1 )

         first = .false.

      end if

!
!     We take care of the obvious case first.  It TARG and OBS are the
!     same we can just fill in zero.
!
      if ( targ .eq. obs ) then

         lt = 0.0_wp

         call cleard ( 3, pos )
         call chkout ( rname )
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
      call zznamfrm ( svctr1, svref, svrefi, ref, refid )

      if ( refid .eq. 0 ) then
         call irfnum ( ref, refid )
      end if

      if ( refid .eq. 0 ) then

         if (  frstnp(ref) .gt. 0 ) then

            call setmsg ( 'The string supplied to specify the ' &
            //            'reference frame, (''#'') contains ' &
            //            'non-printing characters.  The two ' &
            //            'most common causes for this kind ' &
            //            'of error are: 1. an error in the ' &
            //            'call to ZZSPKGP0; 2. an ' &
            //            'uninitialized variable. ' )
            call errch  ( '#', ref )


         else if ( ref .eq. ' ' ) then

            call setmsg ( 'The string supplied to specify the ' &
            //            'reference frame is blank.  The most ' &
            //            'common cause for this kind of error ' &
            //            'is an uninitialized variable. ' )


         else

            call setmsg ( 'The string supplied to specify the ' &
            //            'reference frame was ''#''.  This ' &
            //            'frame is not recognized. Possible ' &
            //            'causes for this error are: 1. failure ' &
            //            'to load the frame definition into the ' &
            //            'kernel pool; 2. An out-of-date ' &
            //            'edition of the toolkit. ' )
            call errch  ( '#', ref )

         end if

         call sigerr ( 'SPICE(UNKNOWNFRAME)' )

         if ( failed() ) then
            call chkout ( rname )
            return
         end if

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
                  .and.  ( i        .lt. chlen ) &
                  .and.  ( ctarg(i) .ne. obs   ) &
                  .and.  ( ctarg(i) .ne. ssb   )  )

!
!        Find a file and segment that has position
!        data for CTARG(I).
!
         call spksfs ( ctarg(i), et, handle, descr, ident, found )

         if ( found ) then
!
!           Get the position of CTARG(I) relative to some
!           center of motion.  This new center goes in
!           CTARG(I+1) and the position is called STEMP.
!
            i = i + 1

            call spkpvn ( handle,    descr,      et, &
                          tframe(i), starg(1,i), ctarg(i) )
!
!           Here's what we have.  STARG is the position of CTARG(I-1)
!           relative to CTARG(I) in reference frame TFRAME(I)
!
         end if
!
!        If one of the routines above failed during
!        execution, we just give up and check out.
!
         if ( failed() ) then
            call chkout ( rname )
            return
         end if

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
      if ( i .eq. chlen ) then

         do while (      found &
                   .and. ctarg(chlen) .ne. ssb &
                   .and. ctarg(chlen) .ne. obs )


!
!           Find a file and segment that has position
!           data for CTARG(CHLEN).
!
            call spksfs ( ctarg(chlen), et, handle, descr, ident, found)

            if ( found ) then
!
!              Get the position of CTARG(CHLEN) relative to
!              some center of motion.  The new center
!              overwrites the old.  The position is called
!              STEMP.
!
               call spkpvn ( handle, descr, et, tmpfrm, stemp, &
                             ctarg(chlen))

               if ( failed() ) then
                  call chkout ( rname )
                  return
               end if

!
!              Add STEMP to the position of TARG relative to
!              the old center to get the position of TARG
!              relative to the new center.  Overwrite
!              the last element of STARG.
!
               if ( tframe(chlen) .eq. tmpfrm ) then

                  call moved ( starg(1,chlen), 3, vtemp )

               else if ( isinrt ( tframe(chlen), tmpfrm ) ) then

                  call irfrot( tframe(chlen), tmpfrm,         rot      )
                  call mxv   ( rot,           starg(1,chlen), vtemp(1) )

               else

                  call zzrefch0( tframe(chlen), tmpfrm,   et,   psxfrm )

                  if ( failed() ) then
                     call chkout ( rname )
                     return
                  end if

                  call mxv   ( psxfrm, starg (1,chlen),       vtemp  )

               end if

               call vadd ( vtemp,  stemp, starg(1,chlen) )

               tframe(chlen) = tmpfrm

            end if

!
!           If one of the routines above failed during
!           execution, we just give up and check out.
!
            if ( failed() ) then
               call chkout ( rname )
               return
            end if

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
      if ( ctarg(nct) .eq. cobs ) then
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
                  .and.  ( cobs  .ne. ssb ) &
                  .and.  ( ctpos .eq. 0   )  )

!
!        Find a file and segment that has position
!        data for COBS.
!
         call spksfs ( cobs, et, handle, descr, ident, found )

         if ( found ) then
!
!           Get the position of COBS; call it STEMP.
!           The center of motion of COBS becomes the
!           new COBS.
!
            if ( legs .eq. 0 ) then
               call spkpvn ( handle, descr, et, tmpfrm, sobs,  cobs )
            else
               call spkpvn ( handle, descr, et, tmpfrm, stemp, cobs )
            end if

            if ( failed() ) then
               call chkout ( rname )
               return
            end if

            if ( nofrm ) then
               nofrm  = .false.
               cframe = tmpfrm
            end if
!
!           Add STEMP to the position of OBS relative to
!           the old COBS to get the position of OBS
!           relative to the new COBS.
!
            if ( cframe .eq. tmpfrm ) then
!
!              On the first leg of the position of the observer, we
!              don't have to add anything, the position of the
!              observer is already in SOBS.  We only have to add when
!              the number of legs in the observer position is one or
!              greater.
!
               if ( legs .gt. 0 ) then
                  call vadd ( sobs,  stemp, vtemp )
                  call vequ ( vtemp,        sobs  )
               end if

            else if ( isinrt( cframe, tmpfrm ) ) then

               call irfrot ( cframe, tmpfrm, rot )
               call mxv    ( rot,    sobs(1),   vtemp(1)  )
               call vadd   ( vtemp,  stemp,     sobs      )
               cframe = tmpfrm

            else

               call zzrefch0 ( cframe, tmpfrm, et,   psxfrm )

               if ( failed() ) then
                  call chkout ( rname )
                  return
               end if

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
         if ( failed() ) then
            call chkout ( rname )
            return
         end if

      end do


!
!     If CTPOS is zero at this point, it means we
!     have not found a common node though we have
!     searched through all the available data.
!
      if ( ctpos .eq. 0 ) then

         call bodc2n ( targ, tname,   found )

         if ( found ) then
            call prefix ( '# (',  0,        tname )
            call suffix ( ')',    0,        tname )
            call repmi  ( tname, '#', targ, tname )
         else
            call intstr ( targ, tname )
         end if


         call bodc2n ( obs,  oname,   found )

         if ( found ) then
            call prefix ( '# (',  0,       oname )
            call suffix ( ')',    0,       oname )
            call repmi  ( oname, '#', obs, oname )
         else
            call intstr ( obs, oname )
         end if

         call setmsg ( 'Insufficient ephemeris data has been ' &
         //            'loaded to compute the position of TARG ' &
         //            'relative to OBS at the ephemeris epoch ' &
         //            '#. ' )

         call etcal  (  et,    tstring                                 )
         call errch  ( 'TARG', tname                                   )
         call errch  ( 'OBS',  oname                                   )
         call errch  ( '#',    tstring                                 )
         call sigerr ( 'SPICE(SPKINSUFFDATA)'                          )
         call chkout ( rname                                        )
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
      if ( ctpos .eq. 1 ) then
         tframe(1) = cframe
      end if

      do i = 2, ctpos-1

         if ( tframe(i) .eq. tframe(i+1) ) then

            call vadd  ( starg(1,i), starg(1,i+1),  stemp        )
            call moved ( stemp,      3,             starg(1,i+1) )

         else if ( isinrt ( tframe(i), tframe(i+1) ) ) then

            call irfrot ( tframe(i),  tframe(i+1),     rot          )
            call mxv    ( rot,        starg(1,i),      stemp(1)     )
            call vadd   ( stemp,      starg(1,i+1),    vtemp        )
            call moved  ( vtemp,      3,               starg(1,i+1) )

         else

            call zzrefch0 ( tframe(i),  tframe(i+1), et,  psxfrm       )

            if ( failed() ) then
               call chkout ( rname )
               return
            end if

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
      if ( tframe(ctpos) .eq. cframe ) then

         call vsub   ( starg(1,ctpos), sobs,  pos  )

      else if ( tframe(ctpos) .eq. refid ) then

!
!        If the last frame associated with the target is already
!        in the requested output frame, we convert the position of
!        the observer to that frame and then subtract the position
!        of the observer from the position of the target.
!
         if ( isinrt( cframe, refid ) ) then

            call irfrot ( cframe, refid,   rot      )
            call mxv    ( rot,    sobs(1), stemp(1) )

         else

            call zzrefch0 ( cframe, refid, et,  psxfrm )

            if ( failed() ) then
               call chkout ( rname )
               return
            end if

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
         call irfrot ( tframe(ctpos),  cframe,               rot      )
         call mxv    ( rot,            starg(1,ctpos),       stemp(1) )
         call vsub   ( stemp,          sobs,                 pos    )

      else
!
!        Use the more general routine ZZREFCH0 to make the
!        transformation.
!
         call zzrefch0 ( tframe(ctpos),  cframe,          et,  psxfrm )

         if ( failed() ) then
            call chkout ( rname )
            return
         end if

         call mxv    ( psxfrm,         starg(1,ctpos),       stemp  )
         call vsub   ( stemp,          sobs,                 pos  )

      end if

!
!     Finally, rotate as needed into the requested frame.
!
      if      ( cframe .eq. refid ) then

!
!        We don't have to do anything in this case.
!

      else if ( isinrt ( cframe, refid ) ) then
!
!        Since both frames are inertial, we use the more direct
!        routine IRFROT to get the transformation to REFID.
!
         call irfrot ( cframe,  refid,    rot          )
         call mxv    ( rot,     pos(1),   stemp(1)     )
         call moved  ( stemp,        3,   pos          )

      else

         call zzrefch0 ( cframe,  refid, et,  psxfrm  )

         if ( failed() ) then
            call chkout ( rname )
            return
         end if

         call mxv    ( psxfrm,  pos,        stemp   )
         call moved  ( stemp,          3,   pos     )

      end if

      lt = vnorm  ( pos ) / clight()

      call chkout ( rname )
      return
      end
