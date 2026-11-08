!$Procedure TKFRAM ( TK frame, find position rotation )

      SUBROUTINE TKFRAM ( FRCODE, ROT, FRAME, FOUND )

! $ Abstract
!
!     Find the position rotation matrix from a Text Kernel (TK) frame
!     with the specified frame class ID to its base frame.
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
!     FRAMES
!
! $ Keywords
!
!     POINTING
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               FRCODE
      DOUBLE PRECISION      ROT   ( 3, 3 )
      INTEGER               FRAME
      LOGICAL               FOUND

      INTEGER               BUFSIZ
      PARAMETER           ( BUFSIZ = 200 )

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
      DOUBLE PRECISION      VDOT

      INTEGER               LNKNFN
      INTEGER               LNKTL
      INTEGER               RTRIM

      LOGICAL               BADKPV
      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local Parameters
!

      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )

      INTEGER               NITEMS
      PARAMETER           ( NITEMS = 14 )

      INTEGER               NVARS
      PARAMETER           ( NVARS = 14 )

      INTEGER               NDPS
      PARAMETER           ( NDPS  =  9 )

      INTEGER               DBUFSZ
      PARAMETER           ( DBUFSZ = NDPS * BUFSIZ )

      INTEGER               NINTS
      PARAMETER           ( NINTS =  1 )

      INTEGER               IBUFSZ
      PARAMETER           ( IBUFSZ = NINTS * BUFSIZ )

      INTEGER               LBPOOL
      PARAMETER           ( LBPOOL = -5 )


!
!     Local Variables
!
      CHARACTER*(WDSIZE)    AGENT
      CHARACTER*(WDSIZE)    ALT    ( NITEMS )
      CHARACTER*(WDSIZE)    ALTNAT
      CHARACTER*(WDSIZE)    FRNAME
      CHARACTER*(WDSIZE)    IDSTR
      CHARACTER*(WDSIZE)    ITEM   ( NVARS  )
      CHARACTER*(WDSIZE)    OLDAGT
      CHARACTER*(WDSIZE)    NAME
      CHARACTER*(WDSIZE)    SPEC
      CHARACTER*(1)         TYPE
      CHARACTER*(WDSIZE)    UNITS

      DOUBLE PRECISION      ANGLES ( 3 )
      DOUBLE PRECISION      BUFFD  ( NDPS, BUFSIZ )
      DOUBLE PRECISION      MATRIX ( 3,    3      )
      DOUBLE PRECISION      QUATRN ( 0:3 )
      DOUBLE PRECISION      QTMP   ( 0:3 )
      DOUBLE PRECISION      TEMPD

      INTEGER               AR
      INTEGER               AT
      INTEGER               AXES   ( 3 )
      INTEGER               BUFFI  ( NINTS,     BUFSIZ )
      INTEGER               I
      INTEGER               IDENTS ( 1,         BUFSIZ )
      INTEGER               IDNT   ( 1 )
      INTEGER               N
      INTEGER               OLDID
      INTEGER               POOL   ( 2, LBPOOL: BUFSIZ )
      INTEGER               R
      INTEGER               TAIL

      LOGICAL               BUFFRD
      LOGICAL               FIRST
      LOGICAL               FOUND1
      LOGICAL               FOUND2
      LOGICAL               FND
      LOGICAL               FULL
      LOGICAL               UPDATE

!
!     Saved variables
!
      SAVE

!
!     Initial values
!
      DATA                  AT     /  0            /
      DATA                  BUFFD  / DBUFSZ * 0.D0 /
      DATA                  BUFFI  / IBUFSZ * 0    /
      DATA                  FIRST  / .TRUE.        /
      DATA                  IDENTS / BUFSIZ * 0    /


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
      IF ( FIRST ) THEN

         FIRST = .FALSE.

         CALL LNKINI ( BUFSIZ, POOL )

      END IF

!
!     Now do the standard SPICE error handling.  Sure this is
!     a bit unconventional, but nothing will be hurt by doing
!     the stuff above first.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'TKFRAM' )

!
!     So far, we've not FOUND the rotation to the specified frame.
!
      FOUND = .FALSE.

!
!     Check the ID to make sure it is non-zero.
!
      IF (  FRCODE .EQ. 0 ) THEN

         CALL LNKINI ( BUFSIZ, POOL )

         CALL SETMSG ( 'Frame identification codes are ' &
         //            'required to be non-zero.  You''ve ' &
         //            'specified a frame with ID value ' &
         //            'zero. '                            )
         CALL SIGERR ( 'SPICE(ZEROFRAMEID)'                )
         CALL CHKOUT ( 'TKFRAM'                            )
         RETURN

      END IF

!
!     Find out whether our linked list pool is already full.
!     We'll use this information later to decide whether we're
!     going to have to delete a watcher.
!
      FULL = LNKNFN(POOL) .EQ. 0

      IF ( FULL ) THEN
!
!        If the input frame ID is not buffered, we'll need to
!        overwrite an existing buffer entry. In this case
!        the call to LOCATI we're about to make will overwrite
!        the ID code in the slot we're about to use. We need
!        this ID code, so extract it now while we have the
!        opportunity. The old ID sits at the tail of the list
!        whose head node is AT.
!
         TAIL  = LNKTL  ( AT, POOL )

         OLDID = IDENTS ( 1,  TAIL )

!
!        Create the name of the agent associated with the old
!        frame.
!
         OLDAGT = 'TKFRAME_#'
         CALL REPMI ( OLDAGT, '#', OLDID, OLDAGT )

      END IF

!
!     Look up the address of the instance data.
!
      IDNT(1) = FRCODE
      CALL LOCATI ( IDNT, 1, IDENTS, POOL, AT, BUFFRD )


      IF (  FULL  .AND. ( .NOT. BUFFRD )  ) THEN
!
!        Since the buffer is already full, we'll delete the watcher for
!        the kernel variables associated with OLDID, since there's no
!        longer a need for that watcher.
!
!        First clear the update status of the old agent; DWPOOL won't
!        delete an agent with a unchecked update.
!
         CALL CVPOOL ( OLDAGT, UPDATE )
         CALL DWPOOL ( OLDAGT )

      END IF

!
!     Until we have better information we put the identity matrix
!     into the output rotation and set FRAME to zero.
!
      CALL IDENT ( ROT )
      FRAME = 0

!
!     If we have to look up the data for our frame, we do
!     it now and perform any conversions and computations that
!     will be needed when it's time to convert coordinates to
!     directions.
!
!     Construct the name of the agent associated with the
!     requested frame.  (Each frame has its own agent).
!
      CALL INTSTR ( FRCODE, IDSTR  )
      CALL FRMNAM ( FRCODE, FRNAME )

      IF ( FRNAME .EQ. ' ' ) THEN

         CALL LNKINI ( BUFSIZ, POOL )

         CALL SETMSG ( 'The Text Kernel (TK) frame with ID code ' &
         //            '# does not have a recognized name. '      )
         CALL ERRINT ( '#', FRCODE                                )
         CALL SIGERR ( 'SPICE(INCOMPLETEFRAME)'                   )
         CALL CHKOUT ( 'TKFRAM'                                   )
         RETURN

      END IF

      AGENT  = 'TKFRAME_' // IDSTR( :RTRIM(IDSTR) )
      R      =  RTRIM(AGENT)

      ALTNAT = 'TKFRAME_' // FRNAME( :RTRIM(FRNAME) )
      AR     =  RTRIM(ALTNAT)

!
!     If the frame is buffered, we check the kernel pool to
!     see if there has been an update to this frame.
!
      IF ( BUFFRD ) THEN

         CALL CVPOOL ( AGENT(1:R), UPDATE )

      ELSE
!
!        If the frame is not buffered we definitely need to update
!        things.

         UPDATE = .TRUE.

      END IF


      IF ( .NOT. UPDATE ) THEN
!
!        Just look up the rotation matrix and relative-to
!        information from the local buffer.
!
         ROT(1,1)  = BUFFD ( 1, AT )
         ROT(2,1)  = BUFFD ( 2, AT )
         ROT(3,1)  = BUFFD ( 3, AT )
         ROT(1,2)  = BUFFD ( 4, AT )
         ROT(2,2)  = BUFFD ( 5, AT )
         ROT(3,2)  = BUFFD ( 6, AT )
         ROT(1,3)  = BUFFD ( 7, AT )
         ROT(2,3)  = BUFFD ( 8, AT )
         ROT(3,3)  = BUFFD ( 9, AT )

         FRAME     = BUFFI ( 1, AT )

      ELSE
!
!        Determine how the frame is specified and what it
!        is relative to.  The variables that specify
!        how the frame is represented and what it is relative to
!        are TKFRAME_#_SPEC and TKFRAME_#_RELATIVE where # is
!        replaced by the text value of ID or the frame name.
!
         ITEM(1) = AGENT(1:R) // '_SPEC'
         ITEM(2) = AGENT(1:R) // '_RELATIVE'

         ALT (1) = ALTNAT(1:AR) // '_SPEC'
         ALT (2) = ALTNAT(1:AR) // '_RELATIVE'
!
!        See if the friendlier version of the kernel pool variables
!        are available.
!
!        If both forms are present, we signal an error.
!
         DO I = 1, 2

            CALL DTPOOL ( ITEM(I), FOUND1, N, TYPE )
            CALL DTPOOL ( ALT(I),  FOUND2, N, TYPE )

            IF ( FOUND1 .AND. FOUND2 ) THEN
               CALL LNKINI ( BUFSIZ, POOL )
               FRAME =  0
               CALL IDENT  ( ROT      )
               CALL SETMSG ( 'Frame name-based and frame '    // &
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
               CALL ERRCH  ( '#', ITEM(I) )
               CALL ERRCH  ( '#', ALT(I) )
               CALL ERRCH  ( '#', FRNAME )
               CALL SIGERR ( 'SPICE(COMPETINGFRAMESPEC)' )
               CALL CHKOUT ( 'TKFRAM' )
               RETURN
            END IF

            IF ( FOUND2 ) THEN
               ITEM(I) = ALT(I)
            END IF

         END DO

!
!        If either the SPEC or RELATIVE frame are missing from
!        the kernel pool, we simply return.
!
         IF (     BADKPV ( 'TKFRAM', ITEM(1), '=', 1, 1, 'C' ) &
             .OR. BADKPV ( 'TKFRAM', ITEM(2), '=', 1, 1, 'C' ) ) THEN

            CALL LNKINI ( BUFSIZ, POOL )
            FRAME =  0
            CALL IDENT  (  ROT     )
            CALL CHKOUT ( 'TKFRAM' )
            RETURN

         END IF

!
!        If we make it this far, look up the SPEC and RELATIVE frame.
!
         CALL GCPOOL ( ITEM(1),   1, 1, N, SPEC,  FND )
         CALL GCPOOL ( ITEM(2),   1, 1, N, NAME,  FND )

!
!        Look up the ID code for this frame.
!
         CALL NAMFRM ( NAME, FRAME )

         IF ( FRAME .EQ. 0 ) THEN

            CALL LNKINI ( BUFSIZ, POOL )
            CALL IDENT  (  ROT     )
            CALL SETMSG ( 'The frame to which frame # is ' &
            //            'relatively defined is not ' &
            //            'recognized. The kernel pool ' &
            //            'specification of the relative ' &
            //            'frame is ''#''.  This is not a ' &
            //            'recognized frame. '              )
            CALL ERRINT ( '#', FRCODE                       )
            CALL ERRCH  ( '#', NAME                         )
            CALL SIGERR ( 'SPICE(BADFRAMESPEC)'             )
            CALL CHKOUT ( 'TKFRAM'                          )
            RETURN

         END IF

!
!        Make sure that the RELATIVE frame ID is distinct from the
!        frame ID. If they are the same, SPICE will go into an
!        indefinite loop.
!
         IF ( FRAME .EQ. FRCODE ) THEN

            CALL LNKINI ( BUFSIZ, POOL )
            FRAME =  0
            CALL IDENT  (  ROT     )
            CALL SETMSG ( 'Bad fixed offset frame specification: ' &
            //            'the frame ''#'' (frame ID #) is ' &
            //            'defined relative to itself. SPICE ' &
            //            'cannot work with such frames. '         )
            CALL ERRCH  ( '#', NAME                                )
            CALL ERRINT ( '#', FRCODE                              )
            CALL SIGERR ( 'SPICE(BADFRAMESPEC2)'                   )
            CALL CHKOUT ( 'TKFRAM'                                 )
            RETURN

         END IF

!
!        Convert SPEC to upper case so that we can easily check
!        to see if this is one of the expected specification types.
!
         CALL UCASE ( SPEC, SPEC )

         IF ( SPEC .EQ. 'MATRIX' ) THEN
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
            ITEM(3) = AGENT (1:R)  // '_MATRIX'
            ALT (3) = ALTNAT(1:AR) // '_MATRIX'

            CALL DTPOOL ( ITEM(3), FOUND1, N, TYPE )
            CALL DTPOOL ( ALT(3),  FOUND2, N, TYPE )

            IF ( FOUND1 .AND. FOUND2 ) THEN
               CALL LNKINI ( BUFSIZ, POOL )
               FRAME =  0
               CALL IDENT  ( ROT      )
               CALL SETMSG ( 'Frame name-based and frame '    // &
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
               CALL ERRCH  ( '#', ITEM(3) )
               CALL ERRCH  ( '#', ALT(3) )
               CALL ERRCH  ( '#', FRNAME )
               CALL SIGERR ( 'SPICE(COMPETINGFRAMESPEC)' )
               CALL CHKOUT ( 'TKFRAM' )
               RETURN
            END IF

            IF ( FOUND2 ) THEN
               ITEM(3) = ALT(3)
            END IF

            IF ( BADKPV ( 'TKFRAM', ITEM(3), '=', 9, 1, 'N' ) ) THEN

               CALL LNKINI ( BUFSIZ, POOL )
               FRAME =  0
               CALL IDENT  ( ROT      )
               CALL CHKOUT ( 'TKFRAM' )
               RETURN

            END IF

!
!           The variable meets current expectations, look it up
!           from the kernel pool.
!
            CALL GDPOOL ( ITEM(3),   1, 9, N, MATRIX,  FND )

!
!           In this case the full transformation matrix has been
!           specified.  We simply polish it up a bit.
!
            CALL MOVED  ( MATRIX, 9, ROT )
            CALL SHARPR (            ROT )

!
!           The matrix might not be right-handed, so correct
!           the sense of the second and third columns if necessary.
!
            IF ( VDOT( ROT(1,2), MATRIX(1,2) ) .LT. 0.0D0 ) THEN
               CALL VSCLIP ( -1.0D0, ROT(1,2) )
            END IF

            IF ( VDOT( ROT(1,3), MATRIX(1,3) ) .LT. 0.0D0 ) THEN
               CALL VSCLIP ( -1.0D0, ROT(1,3) )
            END IF


         ELSE IF ( SPEC .EQ. 'ANGLES' ) THEN
!
!           Look up the angles, their units and axes for the
!           frame specified by ID. (Note that UNITS are optional).
!           As in the previous case we give preference to the
!           form TKFRAME_<name>_<item> over TKFRAME_<id>_<item>.
!
            ITEM(3) = AGENT(1:R)   // '_ANGLES'
            ITEM(4) = AGENT(1:R)   // '_AXES'
            ITEM(5) = AGENT(1:R)   // '_UNITS'

            ALT (3) = ALTNAT(1:AR) // '_ANGLES'
            ALT (4) = ALTNAT(1:AR) // '_AXES'
            ALT (5) = ALTNAT(1:AR) // '_UNITS'
!
!           Again, we give preference to the more friendly form
!           of TKFRAME specification.
!
!           If both forms are present, we signal an error.
!
            DO I = 3, 5

               CALL DTPOOL ( ITEM(I), FOUND1, N, TYPE )
               CALL DTPOOL ( ALT(I),  FOUND2, N, TYPE )

               IF ( FOUND1 .AND. FOUND2 ) THEN
                  CALL LNKINI ( BUFSIZ, POOL )
                  FRAME =  0
                  CALL IDENT  ( ROT      )
                  CALL SETMSG ( 'Frame name-based and frame '    // &
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
                  CALL ERRCH  ( '#', ITEM(I) )
                  CALL ERRCH  ( '#', ALT(I) )
                  CALL ERRCH  ( '#', FRNAME )
                  CALL SIGERR ( 'SPICE(COMPETINGFRAMESPEC)' )
                  CALL CHKOUT ( 'TKFRAM' )
                  RETURN
               END IF

               IF ( FOUND2 ) THEN
                  ITEM(I) = ALT(I)
               END IF

            END DO


            IF (    BADKPV( 'TKFRAM', ITEM(3), '=', 3, 1, 'N' ) &
               .OR. BADKPV( 'TKFRAM', ITEM(4), '=', 3, 1, 'N' ) ) &
            THEN

               CALL LNKINI ( BUFSIZ, POOL )
               FRAME =  0
               CALL IDENT  ( ROT      )
               CALL CHKOUT ( 'TKFRAM' )
               RETURN

            END IF

            UNITS = 'RADIANS'

            CALL GDPOOL ( ITEM(3),   1, 3, N, ANGLES, FND )
            CALL GIPOOL ( ITEM(4),   1, 3, N, AXES,   FND )
            CALL GCPOOL ( ITEM(5),   1, 1, N, UNITS,  FND )

!
!           Convert angles to radians.
!
            DO I = 1, 3

               CALL CONVRT ( ANGLES(I), UNITS, 'RADIANS', TEMPD )
               ANGLES(I) = TEMPD

            END DO

!
!           Compute the rotation from instrument frame to CK frame.
!
            CALL EUL2M (  ANGLES(1),   ANGLES(2),   ANGLES(3), &
                          AXES  (1),   AXES  (2),   AXES  (3), ROT )

            IF ( FAILED() ) THEN
               CALL LNKINI ( BUFSIZ, POOL )
               FRAME =  0
               CALL IDENT  ( ROT      )
               CALL CHKOUT ( 'TKFRAM' )
               RETURN
            END IF

         ELSE IF ( SPEC .EQ. 'QUATERNION' ) THEN
!
!           Look up the quaternion and convert it to a rotation
!           matrix. Again there are two possible variables that
!           may point to the quaternion. We give preference to
!           the form TKFRAME_<name>_Q over the form TKFRAME_<id>_Q.
!
!           If both forms are present, we signal an error.
!
            ITEM(3) = AGENT(1:R)   // '_Q'
            ALT (3) = ALTNAT(1:AR) // '_Q'

            CALL DTPOOL ( ITEM(3), FOUND1, N, TYPE )
            CALL DTPOOL ( ALT(3),  FOUND2, N, TYPE )

            IF ( FOUND1 .AND. FOUND2 ) THEN
               CALL LNKINI ( BUFSIZ, POOL )
               FRAME =  0
               CALL IDENT  ( ROT      )
               CALL SETMSG ( 'Frame name-based and frame '    // &
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
               CALL ERRCH  ( '#', ITEM(3) )
               CALL ERRCH  ( '#', ALT(3) )
               CALL ERRCH  ( '#', FRNAME )
               CALL SIGERR ( 'SPICE(COMPETINGFRAMESPEC)' )
               CALL CHKOUT ( 'TKFRAM' )
               RETURN
            END IF

            IF ( FOUND2 ) THEN
               ITEM(3) = ALT(3)
            END IF


            IF ( BADKPV ( 'TKFRAM', ITEM(3), '=', 4, 1, 'N' ) ) THEN
               CALL LNKINI ( BUFSIZ, POOL )
               FRAME =  0
               CALL IDENT  ( ROT      )
               CALL CHKOUT ( 'TKFRAM' )
               RETURN
            END IF
!
!           In this case we have the quaternion representation.
!           Again, we do a small amount of polishing of the input.
!
            CALL GDPOOL ( ITEM(3), 1, 4, N, QUATRN, FND )
            CALL VHATG  ( QUATRN,  4,       QTMP   )
            CALL Q2M    ( QTMP,             ROT    )

         ELSE
!
!           We don't recognize the SPEC for this frame.  Say
!           so.  Also note that perhaps the user needs to upgrade
!           the toolkit.
!
            CALL LNKINI ( BUFSIZ, POOL )
            FRAME =  0
            CALL IDENT  ( ROT      )
            CALL SETMSG ( 'The frame specification "# = ' &
            //            '''#''" is not one of the recognized ' &
            //            'means of specifying a text-kernel ' &
            //            'constant offset frame. ' &
            //            'This may reflect a typographical ' &
            //            'error or may indicate that you ' &
            //            'need to consider updating your ' &
            //            'version of the SPICE toolkit. ' )

            CALL ERRCH  ( '#', ITEM(1) )
            CALL ERRCH  ( '#', SPEC    )
            CALL SIGERR ( 'SPICE(UNKNOWNFRAMESPEC)' )
            CALL CHKOUT ( 'TKFRAM' )
            RETURN

         END IF

!
!        Buffer the identifier, relative frame and rotation matrix.
!
         BUFFD ( 1, AT ) = ROT(1,1)
         BUFFD ( 2, AT ) = ROT(2,1)
         BUFFD ( 3, AT ) = ROT(3,1)
         BUFFD ( 4, AT ) = ROT(1,2)
         BUFFD ( 5, AT ) = ROT(2,2)
         BUFFD ( 6, AT ) = ROT(3,2)
         BUFFD ( 7, AT ) = ROT(1,3)
         BUFFD ( 8, AT ) = ROT(2,3)
         BUFFD ( 9, AT ) = ROT(3,3)

         BUFFI ( 1, AT ) = FRAME

!
!        If these were not previously buffered, we need to set
!        a watch on the various items that might be used to define
!        this frame.
!
         IF ( .NOT. BUFFRD ) THEN
!
!           Immediately check for an update so that we will
!           not redundantly look for this item the next time this
!           routine is called.
!
            ITEM( 1) = AGENT(1:R)   // '_RELATIVE'
            ITEM( 2) = AGENT(1:R)   // '_SPEC'
            ITEM( 3) = AGENT(1:R)   // '_AXES'
            ITEM( 4) = AGENT(1:R)   // '_MATRIX'
            ITEM( 5) = AGENT(1:R)   // '_Q'
            ITEM( 6) = AGENT(1:R)   // '_ANGLES'
            ITEM( 7) = AGENT(1:R)   // '_UNITS'

            ITEM( 8) = ALTNAT(1:AR) // '_RELATIVE'
            ITEM( 9) = ALTNAT(1:AR) // '_SPEC'
            ITEM(10) = ALTNAT(1:AR) // '_AXES'
            ITEM(11) = ALTNAT(1:AR) // '_MATRIX'
            ITEM(12) = ALTNAT(1:AR) // '_Q'
            ITEM(13) = ALTNAT(1:AR) // '_ANGLES'
            ITEM(14) = ALTNAT(1:AR) // '_UNITS'

            CALL SWPOOL ( AGENT, 14, ITEM )
            CALL CVPOOL ( AGENT, UPDATE   )

         END IF

      END IF

      IF ( FAILED() ) THEN
         CALL LNKINI ( BUFSIZ, POOL )
         FRAME =  0
         CALL IDENT  ( ROT      )
         CALL CHKOUT ( 'TKFRAM' )
         RETURN
      END IF

!
!     All errors cause the routine to exit before we get to this
!     point.  If we reach this point we didn't have an error and
!     hence did find the rotation from ID to FRAME.
!
      FOUND = .TRUE.

!
!     That's it
!
      CALL CHKOUT ( 'TKFRAM' )
      RETURN

      END
