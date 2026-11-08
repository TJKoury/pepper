!$Procedure TKFRAM ( TK frame, find position rotation )

      subroutine tkfram ( frcode, rot, frame, found )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Find the position rotation matrix from a Text Kernel (TK) frame
!     with the specified frame class ID to its base frame.
!
! $ Required_Reading
!
!     FRAMES
!
! $ Keywords
!
!     POINTING
!
! $ Declarations

      implicit none

      integer(ip)           frcode
      real(wp)              rot   ( 3, 3 )
      integer(ip)           frame
      logical               found

      integer(ip), parameter :: bufsiz = 200

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  ----------------------------------------------
!     FRCODE     I   Frame class ID of a TK frame.
!     ROT        O   Rotation matrix from TK frame to frame FRAME.
!     FRAME      O   Frame ID of the base reference.
!     FOUND      O   .TRUE. if the rotation could be determined.
!
! $ Detailed_Input
!
!     FRCODE   is the unique frame class ID of the TK frame for which
!              data is being requested. For TK frames the frame class
!              ID is always equal to the frame ID.
!
! $ Detailed_Output
!
!     ROT      is a position rotation matrix that converts positions
!              relative to the TK frame given by its frame class ID,
!              FRCODE, to positions relative to the base frame given by
!              its frame ID, FRAME.
!
!              Thus, if a position S has components x,y,z in the TK
!              frame, then S has components x', y', z' in the base
!              frame.
!
!                 .-  -.     .-     -. .- -.
!                 | x' |     |       | | x |
!                 | y' |  =  |  ROT  | | y |
!                 | z' |     |       | | z |
!                 `-  -'     `-     -' `- -'
!
!
!     FRAME    is the ID code of the base reference frame to which ROT
!              will transform positions.
!
!     FOUND    is a logical indicating whether or not a frame definition
!              for the TK frame with the frame class ID, FRCODE, was
!              constructed from kernel pool data. If ROT and FRAME were
!              constructed, FOUND will be returned with the value .TRUE.
!              Otherwise it will be returned with the value .FALSE.
!
! $ Parameters
!
!     BUFSIZ   is the number of rotation, frame class ID pairs that can
!              have their instance data buffered for the sake of
!              improving run-time performance. This value MUST be
!              positive and should probably be at least 10.
!
! $ Exceptions
!
!     1)  If some kernel variable associated with this frame is not
!         present in the kernel pool, or does not have the proper type
!         or dimension, an error is signaled by a routine in the call
!         tree of this routine. In such a case FOUND will be set to
!         .FALSE.
!
!     2)  If the input FRCODE has the value 0, the error
!         SPICE(ZEROFRAMEID) is signaled. FOUND will be set to .FALSE.
!
!     3)  If the name of the frame corresponding to FRCODE cannot be
!         determined, the error SPICE(INCOMPLETEFRAME) is signaled.
!
!     4)  If the frame given by FRCODE is defined relative to a frame
!         that is unrecognized, the error SPICE(BADFRAMESPEC) is
!         signaled. FOUND will be set to .FALSE.
!
!     5)  If the kernel pool specification for the frame given by
!         FRCODE is not one of 'MATRIX', 'ANGLES' or 'QUATERNION',
!         the error SPICE(UNKNOWNFRAMESPEC) is signaled. FOUND will be
!         set to .FALSE.
!
!     6)  If the frame FRCODE is equal to the relative frame ID (i.e.
!         the frame is defined relative to itself), the error
!         SPICE(BADFRAMESPEC2) is signaled. FOUND will be set to .FALSE.
!
!     7)  If name-based and ID-based forms of any TKFRAME_ keyword
!         are detected in the kernel pool at the same time, the error
!         SPICE(COMPETINGFRAMESPEC) is signaled. FOUND will be set to
!         .FALSE.
!
! $ Files
!
!     This routine makes use of the loaded text kernels to determine
!     the rotation from a constant offset TK frame to its base frame.
!
! $ Particulars
!
!     This routine is used to construct the rotation from some frame
!     that is a constant rotation offset from some other reference
!     frame. This rotation is derived from data stored in the kernel
!     pool.
!
!     This routine is intended to be used as a low level routine by the
!     frame system software. However, you could use this routine to
!     directly retrieve the rotation from an fixed offset TK frame to
!     its base frame.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Compute the rotation from the DSS-34 topocentric frame to
!        its base Earth body-fixed frame and use it to determine the
!        geodetic latitude and longitude of the DSS-34 site.
!
!
!        Use the FK kernel below to load the required topocentric
!        reference frame definition for the DSS-34 site.
!
!           earth_topo_050714.tf
!
!
!        Example code begins here.
!
!
!              PROGRAM TKFRAM_EX1
!              IMPLICIT NONE
!
!        C
!        C     SPICELIB functions.
!        C
!              DOUBLE PRECISION      DPR
!
!        C
!        C     Local parameters
!        C
!              CHARACTER*(*)         MYTOPO
!              PARAMETER           ( MYTOPO = 'DSS-34_TOPO' )
!
!              INTEGER               MXFRLN
!              PARAMETER           ( MXFRLN = 26 )
!
!        C
!        C     Local variables
!        C
!              CHARACTER*(MXFRLN)    FRNAME
!
!              DOUBLE PRECISION      LAT
!              DOUBLE PRECISION      LON
!              DOUBLE PRECISION      RAD
!              DOUBLE PRECISION      ROT   ( 3, 3 )
!              DOUBLE PRECISION      Z     ( 3    )
!
!              INTEGER               FRAME
!              INTEGER               FRCODE
!
!              LOGICAL               FOUND
!
!        C
!        C     Load the FK that contains the topocentric reference
!        C     frame definition for DSS-34.
!        C
!              CALL FURNSH ( 'earth_topo_050714.tf' )
!
!        C
!        C     The name of the topocentric frame is MYTOPO.
!        C     First we get the ID code of the topocentric frame.
!        C
!              CALL NAMFRM ( MYTOPO, FRCODE )
!
!        C
!        C     Next get the rotation from the topocentric frame to
!        C     the body-fixed frame. We can use the TK frame ID in
!        C     place of the TK frame class ID in this call because
!        C     for TK frames these IDs are identical.
!        C
!              CALL TKFRAM ( FRCODE, ROT, FRAME, FOUND )
!
!        C
!        C     Make sure the topocentric frame is relative to one of
!        C     the Earth fixed frames.
!        C
!              CALL FRMNAM( FRAME, FRNAME )
!
!              IF (       FRNAME .NE. 'IAU_EARTH'
!             .     .AND. FRNAME .NE. 'EARTH_FIXED'
!             .     .AND. FRNAME .NE. 'ITRF93'  ) THEN
!
!                 WRITE (*,*) 'The frame ', MYTOPO,
!             .               ' does not appear to be '
!                 WRITE (*,*) 'defined relative to an '
!             .            // 'Earth fixed frame.'
!                 STOP
!
!              END IF
!
!        C
!        C     Things look ok. Get the location of the Z-axis in the
!        C     topocentric frame.
!        C
!              Z(1) = ROT(1,3)
!              Z(2) = ROT(2,3)
!              Z(3) = ROT(3,3)
!
!        C
!        C     Convert the Z vector to latitude, longitude and radius.
!        C
!              CALL RECLAT ( Z, RAD, LAT, LON )
!
!              WRITE (*,'(A)') 'The geodetic coordinates of the center'
!              WRITE (*,'(A)') 'of the topographic frame are:'
!              WRITE (*,*)
!              WRITE (*,'(A,F20.13)') '   Latitude  (deg): ', LAT*DPR()
!              WRITE (*,'(A,F20.13)') '   Longitude (deg): ', LON*DPR()
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        The geodetic coordinates of the center
!        of the topographic frame are:
!
!           Latitude  (deg):    148.9819650021110
!           Longitude (deg):    -35.3984778756552
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     F.S. Turner        (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.3.0, 20-AUG-2021 (JDR) (BVS) (NJB)
!
!        BUG FIX: the routine now signals an error if it detects
!        name-based and ID-based forms of any TKFRAME_ keyword present
!        in the POOL at the same time. This prevents name-based
!        keywords from frame definitions loaded with lower priority
!        from being used instead of ID-based keywords from frame
!        definitions loaded with higher priority.
!
!        BUG FIX: when failing to fetch any frame keywords from the
!        POOL or for any other reason, the routine now always returns
!        FOUND = .FALSE. Previously FOUND could be set to .TRUE. by a
!        DTPOOL call preceding the failure.
!
!        BUG FIX: when failing due to a frame defined relative to
!        itself or due to an unrecognized _SPEC, the routine now always
!        returns FRAME = 0. Previously FRAME was set to the _RELATIVE
!        keyword.
!
!        BUG FIX: the misspelled short error message
!        SPICE(INCOMPLETEFRAME) was corrected. The message had been
!        spelled correctly in header comments but not in the code.
!
!        Changed to return ROT as identity for all failures; previously
!        it was returned this way only for some failures.
!
!        Changed the input argument name ID to FRCODE for consistency
!        with other routines.
!
!        Fixed minor typo on the UNKNOWNFRAMESPEC long error message.
!
!        Edited the header to comply with NAIF standard and modern
!        SPICE CK and frames terminology.
!
!        Added complete code example based on existing fragments.
!
!        Construction of kernel variable names now uses trimmed
!        strings in order to suppress gfortran compile warnings.
!
!        Added DATA statements to initialize BUFFI, BUFFD, and IDENTS.
!        This change suppresses ftnchek warnings for variables possibly
!        not initialized before use. It is not a bug fix.
!
!        Minor inline comment typos were corrected.
!
!-    SPICELIB Version 2.2.0, 08-JAN-2014 (BVS)
!
!        Added an error check for frames defined relative to
!        themselves.
!
!        Increased BUFSIZ from 20 to 200.
!
!-    SPICELIB Version 2.1.0, 23-APR-2009 (NJB)
!
!        Bug fix: watch is deleted only for frames
!        that are deleted from the buffer.
!
!-    SPICELIB Version 2.0.0, 19-MAR-2009 (NJB)
!
!        Bug fix: this routine now deletes watches set on
!        kernel variables of frames that are discarded from
!        the local buffering system.
!
!-    SPICELIB Version 1.2.0, 09-SEP-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in CONVRT, UCRSS, VHATG and VSCL calls.
!
!-    SPICELIB Version 1.1.0, 21-NOV-2001 (FST)
!
!        Updated this routine to dump the buffer of frame ID codes
!        it saves when it or one of the modules in its call tree
!        signals an error. This fixes a bug where a frame's ID code is
!        buffered, but the matrix and kernel pool watcher were not set
!        properly.
!
!-    SPICELIB Version 1.0.0, 18-NOV-1996 (WLT)
!
!-&


! $ Index_Entries
!
!     Fetch the rotation and frame of a text kernel frame
!     Fetch the rotation and frame of a constant offset frame
!
!-&


!
!     Spicelib Functions
!
      real(wp)              vdot

      integer(ip)           lnknfn
      integer(ip)           lnktl
      integer(ip)           rtrim

      logical               badkpv
      logical               failed
      logical               return

!
!     Local Parameters
!

      integer(ip), parameter :: wdsize = 32

      integer(ip), parameter :: nitems = 14

      integer(ip), parameter :: nvars = 14

      integer(ip), parameter :: ndps = 9

      integer(ip), parameter :: dbufsz = ndps * bufsiz

      integer(ip), parameter :: nints = 1

      integer(ip), parameter :: ibufsz = nints * bufsiz

      integer(ip), parameter :: lbpool = -5


!
!     Local Variables
!
      character(wdsize)     agent
      character(wdsize)     alt    ( nitems )
      character(wdsize)     altnat
      character(wdsize)     frname
      character(wdsize)     idstr
      character(wdsize)     item   ( nvars  )
      character(wdsize)     oldagt
      character(wdsize)     name
      character(wdsize)     spec
      character(1)          type
      character(wdsize)     units

      real(wp)              angles ( 3 )
      real(wp)              buffd  ( ndps, bufsiz )
      real(wp)              matrix ( 3,    3      )
      real(wp)              quatrn ( 0:3 )
      real(wp)              qtmp   ( 0:3 )
      real(wp)              tempd

      integer(ip)           ar
      integer(ip)           at
      integer(ip)           axes   ( 3 )
      integer(ip)           buffi  ( nints,     bufsiz )
      integer(ip)           i
      integer(ip)           idents ( 1,         bufsiz )
      integer(ip)           idnt   ( 1 )
      integer(ip)           n
      integer(ip)           oldid
      integer(ip)           pool   ( 2, lbpool: bufsiz )
      integer(ip)           r
      integer(ip)           tail

      logical               buffrd
      logical               first
      logical               found1
      logical               found2
      logical               fnd
      logical               full
      logical               update

!
!     Saved variables
!
      save

!
!     Initial values
!
      data                  at     /  0            /
      data                  buffd  / dbufsz * 0.0_wp /
      data                  buffi  / ibufsz * 0    /
      data                  first  / .true.        /
      data                  idents / bufsiz * 0    /


!
!     Programmer's note: this routine makes use of the *implementation*
!     of LOCATI. If that routine is changed, the logic this routine
!     uses to locate buffered, old frame IDs may need to change as well.
!


!
!     Before we even check in, if N is less than 1 we can
!     just return.
!
!
!     Perform any initializations that might be needed for this
!     routine.
!
      if ( first ) then

         first = .false.

         call lnkini ( bufsiz, pool )

      end if

!
!     Now do the standard SPICE error handling.  Sure this is
!     a bit unconventional, but nothing will be hurt by doing
!     the stuff above first.
!
      if ( return() ) return


!
!     So far, we've not FOUND the rotation to the specified frame.
!
      found = .false.

!
!     Check the ID to make sure it is non-zero.
!
      if (  frcode == 0 ) then

         call lnkini ( bufsiz, pool )

         call setmsg ( 'Frame identification codes are ' &
         //            'required to be non-zero.  You''ve ' &
         //            'specified a frame with ID value ' &
         //            'zero. '                            )
         call sigerr ( 'SPICE(ZEROFRAMEID)'                )
         return

      end if

!
!     Find out whether our linked list pool is already full.
!     We'll use this information later to decide whether we're
!     going to have to delete a watcher.
!
      full = lnknfn(pool) == 0

      if ( full ) then
!
!        If the input frame ID is not buffered, we'll need to
!        overwrite an existing buffer entry. In this case
!        the call to LOCATI we're about to make will overwrite
!        the ID code in the slot we're about to use. We need
!        this ID code, so extract it now while we have the
!        opportunity. The old ID sits at the tail of the list
!        whose head node is AT.
!
         tail  = lnktl  ( at, pool )

         oldid = idents ( 1,  tail )

!
!        Create the name of the agent associated with the old
!        frame.
!
         oldagt = 'TKFRAME_#'
         call repmi ( oldagt, '#', oldid, oldagt )

      end if

!
!     Look up the address of the instance data.
!
      idnt(1) = frcode
      call locati ( idnt, 1, idents, pool, at, buffrd )


      if (  full  .and. ( .not. buffrd )  ) then
!
!        Since the buffer is already full, we'll delete the watcher for
!        the kernel variables associated with OLDID, since there's no
!        longer a need for that watcher.
!
!        First clear the update status of the old agent; DWPOOL won't
!        delete an agent with a unchecked update.
!
         call cvpool ( oldagt, update )
         call dwpool ( oldagt )

      end if

!
!     Until we have better information we put the identity matrix
!     into the output rotation and set FRAME to zero.
!
      call ident ( rot )
      frame = 0

!
!     If we have to look up the data for our frame, we do
!     it now and perform any conversions and computations that
!     will be needed when it's time to convert coordinates to
!     directions.
!
!     Construct the name of the agent associated with the
!     requested frame.  (Each frame has its own agent).
!
      call intstr ( frcode, idstr  )
      call frmnam ( frcode, frname )

      if ( frname == ' ' ) then

         call lnkini ( bufsiz, pool )

         call setmsg ( 'The Text Kernel (TK) frame with ID code ' &
         //            '# does not have a recognized name. '      )
         call errint ( '#', frcode                                )
         call sigerr ( 'SPICE(INCOMPLETEFRAME)'                   )
         return

      end if

      agent  = 'TKFRAME_' // idstr( :rtrim(idstr) )
      r      =  rtrim(agent)

      altnat = 'TKFRAME_' // frname( :rtrim(frname) )
      ar     =  rtrim(altnat)

!
!     If the frame is buffered, we check the kernel pool to
!     see if there has been an update to this frame.
!
      if ( buffrd ) then

         call cvpool ( agent(1:r), update )

      else
!
!        If the frame is not buffered we definitely need to update
!        things.

         update = .true.

      end if


      if ( .not. update ) then
!
!        Just look up the rotation matrix and relative-to
!        information from the local buffer.
!
         rot(1,1)  = buffd ( 1, at )
         rot(2,1)  = buffd ( 2, at )
         rot(3,1)  = buffd ( 3, at )
         rot(1,2)  = buffd ( 4, at )
         rot(2,2)  = buffd ( 5, at )
         rot(3,2)  = buffd ( 6, at )
         rot(1,3)  = buffd ( 7, at )
         rot(2,3)  = buffd ( 8, at )
         rot(3,3)  = buffd ( 9, at )

         frame     = buffi ( 1, at )

      else
!
!        Determine how the frame is specified and what it
!        is relative to.  The variables that specify
!        how the frame is represented and what it is relative to
!        are TKFRAME_#_SPEC and TKFRAME_#_RELATIVE where # is
!        replaced by the text value of ID or the frame name.
!
         item(1) = agent(1:r) // '_SPEC'
         item(2) = agent(1:r) // '_RELATIVE'

         alt (1) = altnat(1:ar) // '_SPEC'
         alt (2) = altnat(1:ar) // '_RELATIVE'
!
!        See if the friendlier version of the kernel pool variables
!        are available.
!
!        If both forms are present, we signal an error.
!
         do i = 1, 2

            call dtpool ( item(i), found1, n, type )
            call dtpool ( alt(i),  found2, n, type )

            if ( found1 .and. found2 ) then
               call lnkini ( bufsiz, pool )
               frame =  0
               call ident  ( rot      )
               call setmsg ( 'Frame name-based and frame '    // &
                             'ID-based text kernel (fixed-'   // &
                             'offset) frame definition '      // &
                             'keywords ''#'' and ''#'' are '  // &
                             'both present in the '           // &
                             'POOL. Most likely this is '     // &
                             'because loaded text kernels '   // &
                             'contain competing definitions ' // &
                             'of the ''#'' frame using '      // &
                             'different keyword styles, '     // &
                             'which is not allowed. '         )
               call errch  ( '#', item(i) )
               call errch  ( '#', alt(i) )
               call errch  ( '#', frname )
               call sigerr ( 'SPICE(COMPETINGFRAMESPEC)' )
               return
            end if

            if ( found2 ) then
               item(i) = alt(i)
            end if

         end do

!
!        If either the SPEC or RELATIVE frame are missing from
!        the kernel pool, we simply return.
!
         if (     badkpv ( 'TKFRAM', item(1), '=', 1, 1, 'C' ) &
             .or. badkpv ( 'TKFRAM', item(2), '=', 1, 1, 'C' ) ) then

            call lnkini ( bufsiz, pool )
            frame =  0
            call ident  (  rot     )
            return

         end if

!
!        If we make it this far, look up the SPEC and RELATIVE frame.
!
         call gcpool ( item(1),   1, 1, n, spec,  fnd )
         call gcpool ( item(2),   1, 1, n, name,  fnd )

!
!        Look up the ID code for this frame.
!
         call namfrm ( name, frame )

         if ( frame == 0 ) then

            call lnkini ( bufsiz, pool )
            call ident  (  rot     )
            call setmsg ( 'The frame to which frame # is ' &
            //            'relatively defined is not ' &
            //            'recognized. The kernel pool ' &
            //            'specification of the relative ' &
            //            'frame is ''#''.  This is not a ' &
            //            'recognized frame. '              )
            call errint ( '#', frcode                       )
            call errch  ( '#', name                         )
            call sigerr ( 'SPICE(BADFRAMESPEC)'             )
            return

         end if

!
!        Make sure that the RELATIVE frame ID is distinct from the
!        frame ID. If they are the same, SPICE will go into an
!        indefinite loop.
!
         if ( frame == frcode ) then

            call lnkini ( bufsiz, pool )
            frame =  0
            call ident  (  rot     )
            call setmsg ( 'Bad fixed offset frame specification: ' &
            //            'the frame ''#'' (frame ID #) is ' &
            //            'defined relative to itself. SPICE ' &
            //            'cannot work with such frames. '         )
            call errch  ( '#', name                                )
            call errint ( '#', frcode                              )
            call sigerr ( 'SPICE(BADFRAMESPEC2)'                   )
            return

         end if

!
!        Convert SPEC to upper case so that we can easily check
!        to see if this is one of the expected specification types.
!
         call ucase ( spec, spec )

         if ( spec == 'MATRIX' ) then
!
!           This is the easiest case.  Just grab the matrix
!           from the kernel pool (and polish it up a bit just
!           to make sure we have a rotation matrix).
!
!           We give preference to the kernel pool variable
!           TKFRAME_<name>_MATRIX if it is available.
!
!           If both forms are present, we signal an error.
!
            item(3) = agent (1:r)  // '_MATRIX'
            alt (3) = altnat(1:ar) // '_MATRIX'

            call dtpool ( item(3), found1, n, type )
            call dtpool ( alt(3),  found2, n, type )

            if ( found1 .and. found2 ) then
               call lnkini ( bufsiz, pool )
               frame =  0
               call ident  ( rot      )
               call setmsg ( 'Frame name-based and frame '    // &
                             'ID-based text kernel (fixed-'   // &
                             'offset) frame definition '      // &
                             'keywords ''#'' and ''#'' are '  // &
                             'both present in the '           // &
                             'POOL. Most likely this is '     // &
                             'because loaded text kernels '   // &
                             'contain competing definitions ' // &
                             'of the ''#'' frame using '      // &
                             'different keyword styles, '     // &
                             'which is not allowed. '         )
               call errch  ( '#', item(3) )
               call errch  ( '#', alt(3) )
               call errch  ( '#', frname )
               call sigerr ( 'SPICE(COMPETINGFRAMESPEC)' )
               return
            end if

            if ( found2 ) then
               item(3) = alt(3)
            end if

            if ( badkpv ( 'TKFRAM', item(3), '=', 9, 1, 'N' ) ) then

               call lnkini ( bufsiz, pool )
               frame =  0
               call ident  ( rot      )
               return

            end if

!
!           The variable meets current expectations, look it up
!           from the kernel pool.
!
            call gdpool ( item(3),   1, 9, n, matrix,  fnd )

!
!           In this case the full transformation matrix has been
!           specified.  We simply polish it up a bit.
!
            call moved  ( matrix, 9, rot )
            call sharpr (            rot )

!
!           The matrix might not be right-handed, so correct
!           the sense of the second and third columns if necessary.
!
            if ( vdot( rot(1,2), matrix(1,2) ) < 0.0_wp ) then
               call vsclip ( -1.0_wp, rot(1,2) )
            end if

            if ( vdot( rot(1,3), matrix(1,3) ) < 0.0_wp ) then
               call vsclip ( -1.0_wp, rot(1,3) )
            end if


         else if ( spec == 'ANGLES' ) then
!
!           Look up the angles, their units and axes for the
!           frame specified by ID. (Note that UNITS are optional).
!           As in the previous case we give preference to the
!           form TKFRAME_<name>_<item> over TKFRAME_<id>_<item>.
!
            item(3) = agent(1:r)   // '_ANGLES'
            item(4) = agent(1:r)   // '_AXES'
            item(5) = agent(1:r)   // '_UNITS'

            alt (3) = altnat(1:ar) // '_ANGLES'
            alt (4) = altnat(1:ar) // '_AXES'
            alt (5) = altnat(1:ar) // '_UNITS'
!
!           Again, we give preference to the more friendly form
!           of TKFRAME specification.
!
!           If both forms are present, we signal an error.
!
            do i = 3, 5

               call dtpool ( item(i), found1, n, type )
               call dtpool ( alt(i),  found2, n, type )

               if ( found1 .and. found2 ) then
                  call lnkini ( bufsiz, pool )
                  frame =  0
                  call ident  ( rot      )
                  call setmsg ( 'Frame name-based and frame '    // &
                                'ID-based text kernel (fixed-'   // &
                                'offset) frame definition '      // &
                                'keywords ''#'' and ''#'' are '  // &
                                'both present in the '           // &
                                'POOL. Most likely this is '     // &
                                'because loaded text kernels '   // &
                                'contain competing definitions ' // &
                                'of the ''#'' frame using '      // &
                                'different keyword styles, '     // &
                                'which is not allowed. '         )
                  call errch  ( '#', item(i) )
                  call errch  ( '#', alt(i) )
                  call errch  ( '#', frname )
                  call sigerr ( 'SPICE(COMPETINGFRAMESPEC)' )
                  return
               end if

               if ( found2 ) then
                  item(i) = alt(i)
               end if

            end do


            if (    badkpv( 'TKFRAM', item(3), '=', 3, 1, 'N' ) &
               .or. badkpv( 'TKFRAM', item(4), '=', 3, 1, 'N' ) ) &
            then

               call lnkini ( bufsiz, pool )
               frame =  0
               call ident  ( rot      )
               return

            end if

            units = 'RADIANS'

            call gdpool ( item(3),   1, 3, n, angles, fnd )
            call gipool ( item(4),   1, 3, n, axes,   fnd )
            call gcpool ( item(5),   1, 1, n, units,  fnd )

!
!           Convert angles to radians.
!
            do i = 1, 3

               call convrt ( angles(i), units, 'RADIANS', tempd )
               angles(i) = tempd

            end do

!
!           Compute the rotation from instrument frame to CK frame.
!
            call eul2m (  angles(1),   angles(2),   angles(3), &
                          axes  (1),   axes  (2),   axes  (3), rot )

            if ( failed() ) then
               call lnkini ( bufsiz, pool )
               frame =  0
               call ident  ( rot      )
               return
            end if

         else if ( spec == 'QUATERNION' ) then
!
!           Look up the quaternion and convert it to a rotation
!           matrix. Again there are two possible variables that
!           may point to the quaternion. We give preference to
!           the form TKFRAME_<name>_Q over the form TKFRAME_<id>_Q.
!
!           If both forms are present, we signal an error.
!
            item(3) = agent(1:r)   // '_Q'
            alt (3) = altnat(1:ar) // '_Q'

            call dtpool ( item(3), found1, n, type )
            call dtpool ( alt(3),  found2, n, type )

            if ( found1 .and. found2 ) then
               call lnkini ( bufsiz, pool )
               frame =  0
               call ident  ( rot      )
               call setmsg ( 'Frame name-based and frame '    // &
                             'ID-based text kernel (fixed-'   // &
                             'offset) frame definition '      // &
                             'keywords ''#'' and ''#'' are '  // &
                             'both present in the '           // &
                             'POOL. Most likely this is '     // &
                             'because loaded text kernels '   // &
                             'contain competing definitions ' // &
                             'of the ''#'' frame using '      // &
                             'different keyword styles, '     // &
                             'which is not allowed. '         )
               call errch  ( '#', item(3) )
               call errch  ( '#', alt(3) )
               call errch  ( '#', frname )
               call sigerr ( 'SPICE(COMPETINGFRAMESPEC)' )
               return
            end if

            if ( found2 ) then
               item(3) = alt(3)
            end if


            if ( badkpv ( 'TKFRAM', item(3), '=', 4, 1, 'N' ) ) then
               call lnkini ( bufsiz, pool )
               frame =  0
               call ident  ( rot      )
               return
            end if
!
!           In this case we have the quaternion representation.
!           Again, we do a small amount of polishing of the input.
!
            call gdpool ( item(3), 1, 4, n, quatrn, fnd )
            call vhatg  ( quatrn,  4,       qtmp   )
            call q2m    ( qtmp,             rot    )

         else
!
!           We don't recognize the SPEC for this frame.  Say
!           so.  Also note that perhaps the user needs to upgrade
!           the toolkit.
!
            call lnkini ( bufsiz, pool )
            frame =  0
            call ident  ( rot      )
            call setmsg ( 'The frame specification "# = ' &
            //            '''#''" is not one of the recognized ' &
            //            'means of specifying a text-kernel ' &
            //            'constant offset frame. ' &
            //            'This may reflect a typographical ' &
            //            'error or may indicate that you ' &
            //            'need to consider updating your ' &
            //            'version of the SPICE toolkit. ' )

            call errch  ( '#', item(1) )
            call errch  ( '#', spec    )
            call sigerr ( 'SPICE(UNKNOWNFRAMESPEC)' )
            return

         end if

!
!        Buffer the identifier, relative frame and rotation matrix.
!
         buffd ( 1, at ) = rot(1,1)
         buffd ( 2, at ) = rot(2,1)
         buffd ( 3, at ) = rot(3,1)
         buffd ( 4, at ) = rot(1,2)
         buffd ( 5, at ) = rot(2,2)
         buffd ( 6, at ) = rot(3,2)
         buffd ( 7, at ) = rot(1,3)
         buffd ( 8, at ) = rot(2,3)
         buffd ( 9, at ) = rot(3,3)

         buffi ( 1, at ) = frame

!
!        If these were not previously buffered, we need to set
!        a watch on the various items that might be used to define
!        this frame.
!
         if ( .not. buffrd ) then
!
!           Immediately check for an update so that we will
!           not redundantly look for this item the next time this
!           routine is called.
!
            item( 1) = agent(1:r)   // '_RELATIVE'
            item( 2) = agent(1:r)   // '_SPEC'
            item( 3) = agent(1:r)   // '_AXES'
            item( 4) = agent(1:r)   // '_MATRIX'
            item( 5) = agent(1:r)   // '_Q'
            item( 6) = agent(1:r)   // '_ANGLES'
            item( 7) = agent(1:r)   // '_UNITS'

            item( 8) = altnat(1:ar) // '_RELATIVE'
            item( 9) = altnat(1:ar) // '_SPEC'
            item(10) = altnat(1:ar) // '_AXES'
            item(11) = altnat(1:ar) // '_MATRIX'
            item(12) = altnat(1:ar) // '_Q'
            item(13) = altnat(1:ar) // '_ANGLES'
            item(14) = altnat(1:ar) // '_UNITS'

            call swpool ( agent, 14, item )
            call cvpool ( agent, update   )

         end if

      end if

      if ( failed() ) then
         call lnkini ( bufsiz, pool )
         frame =  0
         call ident  ( rot      )
         return
      end if

!
!     All errors cause the routine to exit before we get to this
!     point.  If we reach this point we didn't have an error and
!     hence did find the rotation from ID to FRAME.
!
      found = .true.

!
!     That's it
!
      return

      end
