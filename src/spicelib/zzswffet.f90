!$Procedure ZZSWFFET ( Private, switch frame kernel pool data fetch )

      SUBROUTINE ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG, &
                            FREE,   BASCNT, USETIM, BINARY, CLSSES, &
                            CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Fetch kernel variables for a specified switch frame. Data
!     for the new frame are added to the switch frame database.
!
! $ Required_Reading
!
!     FRAMES
!     KERNEL
!     NAIF_IDS
!     TIME
!
! $ Keywords
!
!     Private
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE              'zzswtchf.inc'

      INTEGER               FRAMID
      INTEGER               HDFRAM ( * )
      INTEGER               FRPOOL ( LBSNGL : * )
      INTEGER               FIDLST ( * )
      INTEGER               BASBEG ( * )
      INTEGER               FREE
      INTEGER               BASCNT ( * )
      LOGICAL               USETIM ( * )
      LOGICAL               BINARY ( * )
      INTEGER               CLSSES ( * )
      INTEGER               CLSIDS ( * )
      INTEGER               BASLST ( * )
      DOUBLE PRECISION      STARTS ( * )
      DOUBLE PRECISION      STOPS ( * )
      INTEGER               FRAMAT

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     FRAMID     I   The frame ID of a switch frame.
!     HDFRAM    I-O  Array of hash collision list head nodes.
!     FRPOOL    I-O  Hash collision list pool.
!     FIDLST    I-O  Frame IDs corresponding to list nodes.
!     BASBEG    I-O  Start indices of base lists of switch frames.
!     FREE      I-O  First free index in base frame arrays.
!     BASCNT    I-O  Counts of base frames for each switch frame.
!     USETIM    I-O  Logical flags indicating presence of time bounds.
!     BINARY    I-O  Logical flags indicating binary search usage.
!     CLSSES    I-O  Classes of base frames.
!     CLSIDS    I-O  Frame class IDs of base frames.
!     BASLST    I-O  Array containing lists of base frame IDs.
!     STARTS    I-O  Lists of interval start times.
!     STOPS     I-O  Lists of interval stop times.
!     FRAMAT     O   Index in FIDLST of input switch frame ID.
!
! $ Detailed_Input
!
!     FRAMID      is the integer ID code for a switch reference frame
!                 for which kernel pool data are to be fetched.
!
!     HDFRAM      is an array containing head nodes of hash collision
!                 lists in FRPOOL.
!
!     FRPOOL      is a singly linked list pool containing collision
!                 lists corresponding to hash values.
!
!     FIDLST      is an array of switch frame ID codes. The Ith element
!                 of FRPOOL corresponds to the ID code at index I in
!                 FIDLST.
!
!     BASBEG      is an array of start indices of base frame lists and
!                 base frame attributes. The Ith element of BASBEG is
!                 the start index in BASLST of the base frame list of
!                 the switch frame having ID at index I in FIDLST. The
!                 attributes of the base frame at a given index in
!                 BASLST are stored at the same index in parallel
!                 arrays.
!
!     FREE        is the index of the first free element in the array
!                 BASLST. This is also the index of the first free
!                 elements in the parallel arrays CLSSES, CLSIDS,
!                 STARTS and STOPS.
!
!     BASCNT      is an array of counts of base frames associated with
!                 switch frames. The Ith element of BASCNT is the base
!                 frame count of the switch frame having ID at index I
!                 in FIDLST.
!
!     USETIM      is an array of logical flags indicating the existence
!                 of time intervals associated with the base frames of
!                 a given switch frame. The flag at index I of USETIM
!                 is .TRUE. if and only if the switch frame having ID
!                 at index I of FIDLST has time intervals associated
!                 with its base frames.
!
!     BINARY      is an array of logical flags indicating the
!                 eligibility of a given switch frame for use of a
!                 binary search to locate the base frame time interval
!                 containing a given request time. Eligibility means
!                 that time intervals exist and that the stop time of
!                 the interval at index J is less than or equal to the
!                 start time at index J+1. The flag at index I of
!                 BINARY is .TRUE. if and only if the switch frame at
!                 index I of FIDLST is eligible for binary search.
!
!     CLSSES      is an array of classes of base frames. The element at
!                 index I is the class of the base frame at index I of
!                 BASLST, if that element of BASLST is defined.
!
!     CLSIDS      is of frame class IDs of base frames. The element
!                 at index I is the frame class ID of the base frame
!                 at index I of BASLST, if that element of BASLST is
!                 defined.
!
!     BASLST      is an array of ID codes of base frames associated
!                 with switch frames. The base frame having ID at index
!                 BASBEG(I)-1+J of BASLST is the Jth base frame of the
!                 switch frame having frame ID at index I of FIDLST.
!
!     STARTS      is an array of base frame applicability interval
!                 start times. The Ith element of STARTS is the start
!                 time of the base frame at index I of BASLST, if that
!                 element is defined and that frame has associated time
!                 intervals. For a given switch frame, all bases have
!                 associated time intervals or none do.
!
!     STOPS       is an array of base frame applicability interval
!                 start times. The Ith element of STARTS is the start
!                 time of the base frame at index I of BASLST, if that
!                 element is defined and that frame has associated time
!                 intervals. For a given switch frame, all bases have
!                 associated time intervals or none do.
!
! $ Detailed_Output
!
!     HDFRAM      is the input collision list head node array, updated
!                 if necessary to reflect the addition of a node for
!                 the switch frame designated by the input frame ID.
!
!     FRPOOL      is the hash collision list pool, updated to reflect
!                 the addition of a node for the switch frame
!                 designated by the input frame ID.
!
!     FIDLST      is the switch frame ID list, updated to reflect the
!                 addition of the input frame ID.
!
!     BASBEG      is the input base frame head list, updated to reflect
!                 the addition of an entry for the switch frame
!                 designated by the input frame ID.
!
!     FREE        is the common index in BASLST and its parallel arrays
!                 of their first free elements, following addition of
!                 data associated with the input frame ID.
!
!     BASCNT      is the input base frame count list, updated to
!                 reflect the addition of the base frames associated
!                 with the input frame ID.
!
!     USETIM      is the input time interval presence flag array,
!                 updated to reflect the addition of the flag for the
!                 the switch frame designated by the input frame ID.
!
!     BINARY      is the input binary search eligibility array, updated
!                 to reflect the addition of the flag for the switch
!                 frame designated by the input frame ID.
!
!     CLSSES      is the input class array, updated to reflect the
!                 addition of base frame classes for the switch frame
!                 designated by the input frame ID.
!
!     CLSIDS      is the input frame class ID array, updated to reflect
!                 the addition of base frame frame class IDs for the
!                 switch frame designated by the input frame ID.
!
!     BASLST      is the input base frame array, updated to reflect
!                 the addition of base frames associated with the
!                 switch frame designated by the input frame ID.
!
!     STARTS      is the input base frame interval start time array,
!                 updated to reflect the addition of base frames
!                 associated with the switch frame designated by the
!                 input frame ID.
!
!     STOPS       is the input base frame interval stop time array,
!                 updated to reflect the addition of base frames
!                 associated with the switch frame designated by the
!                 input frame ID.
!
!     FRAMAT      is the common index in the arrays
!
!                    FIDLST, BASBEG, BASCNT, USETIM, BINARY
!
!                 of information associated with the switch frame
!                 designated by the input frame ID.
!
! $ Parameters
!
!     LBSNGL      is the lower bound of a singly linked list pool
!                 managed by the integer hash subsystem.
!
!     See the include file zzswtchf.inc for SPICE-private parameters
!     defining sizes of buffers used by the switch frame subsystem.
!
! $ Exceptions
!
!     1)  If the input frame ID maps to a frame specification for which
!         the ID doesn't match, the error SPICE(BADFRAMESPEC) is
!         signaled.
!
!     2)  If kernel variables for any of the universal frame
!         specification items
!
!            frame ID
!            frame name
!            frame center
!            frame class
!            frame class ID
!
!         or the switch frame item
!
!            base frame list
!
!        are not found, the error SPICE(MISSINGFRAMEVAR) is signaled.
!
!     3) If the ID of a base frame cannot be mapped to a name,
!        the error SPICE(FRAMENAMENOTFOUND) is signaled.
!
!     4) If the stop time for a given interval is not strictly greater
!        than the corresponding start time, the error
!        SPICE(BADTIMEBOUNDS) is signaled.
!
!     5) If only one of the start time or stop time kernel variables
!        is supplied, the error SPICE(PARTIALFRAMESPEC) is signaled.
!
!     6) If the count of start times doesn't match the count of
!        stop times, or if either count doesn't match the count of
!        base frames, the error SPICE(COUNTMISMATCH) is signaled.
!
!     7) If an error occurs during kernel variable lookup, the error
!        is signaled by a routine in the call tree of this routine.
!
!     8) If an error occurs during time conversion, the error
!        is signaled by a routine in the call tree of this routine.
!
! $ Files
!
!     Appropriate kernels must be loaded by the calling program before
!     this routine is called.
!
!     The following data are required:
!
!         - A switch frame specification, normally provided by a
!           frame kernel.
!
!              > Base frames may be specified by name or frame ID.
!
!              > Time interval bounds may be specified by singly
!                quoted strings or by data using the text kernel "@"
!                syntax.
!
!         - Frame specifications for base frames. Normally these
!           specifications are provided by frame kernels.
!
!           Even though specifications of frames identified by ID code
!           are not explicitly required by this routine, they are
!           required by the intended caller of this routine ZZSWFXFM.
!
!         - A leapseconds kernel, if time interval bounds are
!           specified by time strings.
!
!     In all cases, kernel data are normally loaded once per program
!     run, NOT every time this routine is called.
!
! $ Particulars
!
!     This routine obtains switch frame specifications from the kernel
!     pool and adds them to the switch frame database.
!
!     If, upon entry, the database already contains the maximum number
!     of switch frames, the database will be cleared in order to make
!     room for the new switch frame specification.
!
!     If, upon entry, the database doesn't have enough room to buffer
!     the base frame data associated with a new switch frame, the
!     database will be cleared in order to make room for the new switch
!     frame specification.
!
!     Complete error checking of the new switch frame specification is
!     performed on each call.
!
! $ Examples
!
!     None. See usage in ZZSWFXFM.
!
! $ Restrictions
!
!     1)  This routine is SPICE-private. User applications must not
!         call it.
!
!     2)  To improve efficiency, this routine buffers switch frame
!         specifications. The buffers are emptied after any update
!         to the kernel pool. Since the process of parsing, and
!         buffering parameters provided by, switch frame specifications
!         is rather slow, applications using switch frames should
!         avoid high-frequency kernel pool updates.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!     B.V. Semenov   (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 15-DEC-2021 (NJB) (BVS)
!
!-&

! $ Index_Entries
!
!     fetch switch frame data from kernel pool
!
!-&

!
!     SPICELIB functions
!
      LOGICAL               RETURN
      LOGICAL               FAILED

!
!     Local parameters
!

!
!     Kernel variable name templates:
!
      CHARACTER*(*)         TMALGN
      PARAMETER           ( TMALGN = 'FRAME_#_ALIGNED_WITH' )

      CHARACTER*(*)         TMCENT
      PARAMETER           ( TMCENT = 'FRAME_#_CENTER' )

      CHARACTER*(*)         TMCLAS
      PARAMETER           ( TMCLAS = 'FRAME_#_CLASS' )

      CHARACTER*(*)         TMCLID
      PARAMETER           ( TMCLID = 'FRAME_#_CLASS_ID' )

      CHARACTER*(*)         TMID
      PARAMETER           ( TMID   = 'FRAME_#' )

      CHARACTER*(*)         TMFNAM
      PARAMETER           ( TMFNAM = 'FRAME_#_NAME' )

      CHARACTER*(*)         TMSTRT
      PARAMETER           ( TMSTRT = 'FRAME_#_START' )

      CHARACTER*(*)         TMSTOP
      PARAMETER           ( TMSTOP = 'FRAME_#_STOP' )

!
!     Other parameters
!
      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               TIMLEN
      PARAMETER           ( TIMLEN = 80 )

!
!     Indices of items in the arrays of kernel variables:
!
      INTEGER               IXFNAM
      PARAMETER           ( IXFNAM = 1 )

      INTEGER               IXID
      PARAMETER           ( IXID   = IXFNAM + 1 )

      INTEGER               IXCENT
      PARAMETER           ( IXCENT = IXID    + 1 )

      INTEGER               IXCLAS
      PARAMETER           ( IXCLAS = IXCENT + 1 )

      INTEGER               IXCLID
      PARAMETER           ( IXCLID = IXCLAS + 1 )

      INTEGER               IXALGN
      PARAMETER           ( IXALGN = IXCLID + 1 )

      INTEGER               IXSTRT
      PARAMETER           ( IXSTRT = IXALGN + 1 )

      INTEGER               IXSTOP
      PARAMETER           ( IXSTOP = IXSTRT + 1 )


      INTEGER               KVNMLN
      PARAMETER           ( KVNMLN = 32 )

      INTEGER               NKV
      PARAMETER           ( NKV    = 8 )

!
!     Local variables
!
      CHARACTER*(FRNMLN)    BASNAM
      CHARACTER*(1)         BASTYP
      CHARACTER*(FRNMLN)    FRNAME
      CHARACTER*(KVNMLN)    KVALGN
      CHARACTER*(KVNMLN)    KVCLID
      CHARACTER*(KVNMLN)    KVCENT
      CHARACTER*(KVNMLN)    KVCLAS
      CHARACTER*(KVNMLN)    KVID
      CHARACTER*(KVNMLN)    KVFNAM
      CHARACTER*(KVNMLN)    KVNAMS ( NKV )
      CHARACTER*(KVNMLN)    KVSTRT
      CHARACTER*(KVNMLN)    KVSTOP
      CHARACTER*(1)         STPTYP
      CHARACTER*(1)         STRTYP
      CHARACTER*(TIMLEN)    TIMSTR

      INTEGER               CENTER
      INTEGER               FID
      INTEGER               FRCENT
      INTEGER               FRCLAS
      INTEGER               FRCLID
      INTEGER               I
      INTEGER               J
      INTEGER               N
      INTEGER               NBASES
      INTEGER               NFRAVL
      INTEGER               NSTART
      INTEGER               NSTOP
      INTEGER               ROOM

!
!     KVFND(*) indicates whether a specified kernel variable was found.
!
      LOGICAL               HAVTIM
      LOGICAL               INFFND
      LOGICAL               KVFND ( NKV )
      LOGICAL               NEW

      LOGICAL               STRXST
      LOGICAL               STPXST

!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZSWFFET' )

!
!     No result found yet.
!
      FRAMAT = 0

!
!     Create names of kernel variables, using the input frame ID.
!
!     Basic frame specification variables, excluding that for the
!     frame ID. We'll need the frame name before we can create the
!     name of the kernel variable for the frame ID.
!
      CALL REPMI( TMCENT, '#', FRAMID, KVCENT )
      CALL REPMI( TMCLAS, '#', FRAMID, KVCLAS )
      CALL REPMI( TMCLID, '#', FRAMID, KVCLID )
      CALL REPMI( TMFNAM, '#', FRAMID, KVFNAM )

!
!     Switch frame specification variables:
!
      CALL REPMI( TMALGN, '#', FRAMID, KVALGN )
      CALL REPMI( TMSTRT, '#', FRAMID, KVSTRT )
      CALL REPMI( TMSTOP, '#', FRAMID, KVSTOP )

!
!     Below, we use logical flags to describe the status
!     of kernel variable availability:
!
!         KVFND(*) indicates that a variable exists.
!
!     Look up the basic variables.
!
      CALL GIPOOL( KVCLAS, 1, 1, N, FRCLAS, KVFND(IXCLAS) )
      CALL GIPOOL( KVCLID, 1, 1, N, FRCLID, KVFND(IXCLID) )
      CALL GCPOOL( KVFNAM, 1, 1, N, FRNAME, KVFND(IXFNAM) )

      IF ( FAILED() ) THEN

         CALL ZZSWFCLN ( HDFRAM, FRPOOL, BASBEG, FRAMAT )

         CALL CHKOUT ( 'ZZSWFFET' )
         RETURN

      END IF

!
!     Fetch the switch frame ID variable.
!
      IF ( KVFND(IXFNAM) ) THEN
!
!        Use the frame name to fetch the switch frame ID variable.
!
         CALL REPMC ( TMID, '#', FRNAME, KVID )

         CALL GIPOOL( KVID, 1, 1, N, FID, KVFND(IXID) )

         IF ( FAILED() ) THEN

            CALL ZZSWFCLN ( HDFRAM, FRPOOL, BASBEG, FRAMAT )

            CALL CHKOUT( 'ZZSWFFET' )
            RETURN

         END IF

!
!        The frame ID of the frame specification had better match
!        the input frame ID.
!
         IF ( KVFND(IXID) .AND. ( FID .NE. FRAMID ) ) THEN

            CALL ZZSWFCLN ( HDFRAM, FRPOOL, BASBEG, FRAMAT )

            CALL SETMSG ( 'Input frame ID was #, but ID in frame ' &
            //            'specification from kernel pool was #. ' )
            CALL ERRINT ( '#', FRAMID           )
            CALL ERRINT ( '#', FID              )
            CALL SIGERR ( 'SPICE(BADFRAMESPEC)' )
            CALL CHKOUT ( 'ZZSWFFET'            )
            RETURN

         END IF
!
!        The frame name must always be found. If not, the error
!        will be diagnosed in the block below.
!
      ELSE
!
!        We couldn't find the frame name. Indicate the frame ID
!        variable [sic] wasn't found, since its found flag won't be set
!        by GIPOOL.
!
         KVFND(IXID) = .FALSE.

      END IF

!
!     Look up the central body of the frame. The name of the kernel
!     variable for the body could refer to the frame by name or frame
!     ID; the body itself could be specified by name or body ID.
!
      IF ( KVFND(IXFNAM) ) THEN

         CALL ZZDYNBID ( FRNAME, FRAMID, 'CENTER', FRCENT )

         IF ( FAILED() ) THEN

            CALL ZZSWFCLN ( HDFRAM, FRPOOL, BASBEG, FRAMAT )

            CALL CHKOUT ( 'ZZSWFFET' )
            RETURN

         END IF

         KVFND(IXCENT) = .TRUE.
      ELSE
         KVFND(IXCENT) = .FALSE.
      END IF

!
!     Look up the type and count of the base frame list. It
!     may have either string or numeric type.
!
      CALL DTPOOL( KVALGN, KVFND(IXALGN), NBASES, BASTYP )

      IF ( FAILED() ) THEN
!
!        This code should be unreachable but is provided for safety.
!
         CALL ZZSWFCLN ( HDFRAM, FRPOOL, BASBEG, FRAMAT )

         CALL CHKOUT ( 'ZZSWFFET' )
         RETURN

      END IF

!
!     Store the kernel variable names in order to prepare for
!     checking availability of required variables.
!
      KVNAMS(IXID)   = KVID
      KVNAMS(IXFNAM) = KVFNAM
      KVNAMS(IXCENT) = KVCENT
      KVNAMS(IXCLAS) = KVCLAS
      KVNAMS(IXCLID) = KVCLID
      KVNAMS(IXALGN) = KVALGN

!
!     Check for required variables that haven't been supplied.
!
      DO I = 1, IXALGN
!
!        The first 6 items are needed; start and stop times are needed
!        if at least one is present.
!
         IF ( .NOT. KVFND(I) ) THEN

            CALL ZZSWFCLN ( HDFRAM, FRPOOL, BASBEG, FRAMAT )

            CALL SETMSG ( 'Kernel variable #, needed for ' &
            //            'specification of switch frame having ' &
            //            'frame ID #, was not ' &
            //            'found in the kernel pool. This can occur ' &
            //            'when a frame kernel providing the required ' &
            //            'switch frame specification has not been ' &
            //            'loaded, or if the specification is present ' &
            //            'but is incorrect.'                         )
            CALL ERRCH  ( '#', KVNAMS(I)                              )
            CALL ERRINT ( '#', FRAMID                                 )
            CALL SIGERR ( 'SPICE(MISSINGFRAMEVAR)'                    )
            CALL CHKOUT ( 'ZZSWFFET'                                  )
            RETURN

         END IF

      END DO

!
!     Find out whether time bounds are supplied, and if so, which data
!     type is used to represent them.
!
      KVNAMS(IXSTRT) = KVSTRT
      KVNAMS(IXSTOP) = KVSTOP

      CALL DTPOOL( KVSTRT, KVFND(IXSTRT), NSTART, STRTYP )
      CALL DTPOOL( KVSTOP, KVFND(IXSTOP), NSTOP,  STPTYP )

      IF ( FAILED() ) THEN
!
!        This code should be unreachable but is provided for safety.
!
         CALL ZZSWFCLN ( HDFRAM, FRPOOL, BASBEG, FRAMAT )

         CALL CHKOUT ( 'ZZSWFFET' )
         RETURN

      END IF

!
!     Availability of starts and stops is optional...however both
!     must be provided if either is, and the counts must match
!     those of the base frames.
!
!     Note that it's necessary to perform these checks before buffering
!     base frame data, since that process assumes that the number of
!     start and stop times is either zero or matches the number of
!     base frames.
!
      STRXST = KVFND(IXSTRT)
      STPXST = KVFND(IXSTOP)

      HAVTIM = STRXST .AND. STPXST

      IF ( HAVTIM ) THEN
!
!        Make sure that counts of base frames, start times, and
!        stop times are equal.
!
         IF ( ( NSTART .NE. NSTOP ) .OR. ( NSTART .NE. NBASES ) ) THEN

            CALL ZZSWFCLN ( HDFRAM, FRPOOL, BASBEG, FRAMAT )

            CALL SETMSG ( 'Kernel variables for the switch frame ' &
            //            'having frame ID # have mismatched sizes: ' &
            //            'number of base frames = #; number of start ' &
            //            'times = #; number of stop times = #.'      )
            CALL ERRINT ( '#', FRAMID            )
            CALL ERRINT ( '#', NBASES            )
            CALL ERRINT ( '#', NSTART            )
            CALL ERRINT ( '#', NSTOP             )
            CALL SIGERR ( 'SPICE(COUNTMISMATCH)' )
            CALL CHKOUT ( 'ZZSWFFET'             )
            RETURN

         END IF

      ELSE
!
!        Check for inconsistent presence of start and stop times.
!
         IF ( STRXST .OR. STPXST ) THEN

            CALL ZZSWFCLN ( HDFRAM, FRPOOL, BASBEG, FRAMAT )

!
!           Create long message template to be filled in.
!
            CALL SETMSG ( 'Kernel variable #, which specifies base ' &
            //            'frame applicability # times, was not ' &
            //            'provided for the switch frame having ' &
            //            'frame ID #, while the kernel variable # ' &
            //            'specifying base frame applicability ' &
            //            '# times was provided. Switch frame ' &
            //            'applicability start and stop times are ' &
            //            'optional, but both must be provided if ' &
            //            'either is.'  )

            IF ( STRXST ) THEN
!
!              Stop times are missing.
!
               CALL ERRCH  ( '#', KVSTOP  )
               CALL ERRCH  ( '#', 'stop'  )
               CALL ERRINT ( '#', FRAMID  )
               CALL ERRCH  ( '#', KVSTRT  )
               CALL ERRCH  ( '#', 'start' )

            ELSE
!
!              Start times are missing.
!
               CALL ERRCH  ( '#', KVSTRT  )
               CALL ERRCH  ( '#', 'start' )
               CALL ERRINT ( '#', FRAMID  )
               CALL ERRCH  ( '#', KVSTOP  )
               CALL ERRCH  ( '#', 'stop'  )

            END IF

            CALL SIGERR ( 'SPICE(PARTIALFRAMESPEC)' )
            CALL CHKOUT ( 'ZZSWFFET'                )
            RETURN

         END IF

      END IF

!
!     Compute room for the output data.
!
      ROOM = MAXBAS + 1 - FREE

!
!     Check the available room in the frame ID pool and in the frame
!     base arrays.
!
      CALL ZZHSIAVL ( FRPOOL, NFRAVL )

      IF ( ( NFRAVL .EQ. 0 ) .OR. ( ROOM .LT. NBASES ) ) THEN
!
!        There's no room for another frame in the frame pool, or
!        there's no room for the new frame's data in the base
!        frame arrays.
!
!        If we can make enough room by re-initializing the whole
!        local frame database, do so. If the new frame has too
!        much data to fit in the empty base frame arrays, that's
!        an error.
!
         IF ( NBASES .LE. MAXBAS ) THEN
!
!           We can fit the frame data in by clearing out the
!           database.
!
            CALL ZZSWFCLN ( HDFRAM, FRPOOL, BASBEG, FRAMAT )

!
!           The database is now initialized.
!
            FREE = 1
            ROOM = MAXBAS

         ELSE
!
!           We can't make enough room.
!
!           Initialize local data structures, for safety.
!
            CALL ZZSWFCLN ( HDFRAM, FRPOOL, BASBEG, FRAMAT )

            CALL SETMSG ( 'The requested frame # has # associated ' &
            //            'base frames. The maximum number that ' &
            //            'can be supported is #.'   )
            CALL ERRINT ( '#', FRAMID                )
            CALL ERRINT ( '#', NBASES                )
            CALL ERRINT ( '#', MAXBAS                )
            CALL SIGERR ( 'SPICE(TOOMANYBASEFRAMES)' )
            CALL CHKOUT ( 'ZZSWFFET'                 )
            RETURN

         END IF

      END IF

!
!     Add the frame and its data to the local database.
!
!     Start by adding the frame ID to the frame hash structure.
!     The output argument NEW indicates whether the item is new;
!     we don't need to check this argument. We also don't need to
!     check for failure of ZZHSIADD, since we've already ensured
!     there's room in the hash for a new clock.
!
      CALL ZZHSIADD ( HDFRAM, FRPOOL, FIDLST, FRAMID, FRAMAT, NEW )

!
!     At this point, FRAMAT is set.
!
!     Store the frame ID, base start index, base count, and time
!     interval availability attributes for this frame.
!
      FIDLST( FRAMAT ) = FRAMID
      BASBEG( FRAMAT ) = FREE
      BASCNT( FRAMAT ) = NBASES
      USETIM( FRAMAT ) = HAVTIM

!
!     Look up the base frame variables. The base frame list may have
!     either string or numeric type.
!
      KVFND(IXALGN) = .FALSE.

      IF ( BASTYP .EQ. 'N' ) THEN
!
!        The frames are specified by ID code. We can buffer the frame
!        ID codes immediately.
!
         CALL GIPOOL ( KVALGN,       1,            ROOM, &
                       BASCNT(FREE), BASLST(FREE), KVFND(IXALGN) )

         IF ( FAILED() ) THEN

            CALL ZZSWFCLN ( HDFRAM, FRPOOL, BASBEG, FRAMAT )

            CALL CHKOUT( 'ZZSWFFET' )
            RETURN

         END IF

      ELSE IF ( BASTYP .EQ. 'C' ) THEN
!
!        The frames are specified by name. We must convert the
!        names to frame ID codes before we can can buffer the IDs.
!
         KVFND(IXALGN) = .FALSE.

         DO I = 1, NBASES

            CALL GCPOOL( KVALGN,  I,       1, &
                         N,       BASNAM,  KVFND(IXALGN) )

            IF ( FAILED() ) THEN
!
!              This code should be unreachable but is provided for
!              safety.
!
               CALL ZZSWFCLN ( HDFRAM, FRPOOL, BASBEG, FRAMAT )

               CALL CHKOUT( 'ZZSWFFET' )
               RETURN

            END IF

            CALL NAMFRM( BASNAM, BASLST(FREE-1+I) )

            IF ( BASLST(FREE-1+I) .EQ. 0 ) THEN

               CALL ZZSWFCLN ( HDFRAM, FRPOOL, BASBEG, FRAMAT )

               CALL SETMSG( 'Base frame name # of switch frame ' &
               //           '# could not be translated to a ' &
               //           'frame ID code '           )
               CALL ERRCH ( '#', BASNAM                )
               CALL ERRINT( '#', FRAMID                )
               CALL SIGERR( 'SPICE(FRAMENAMENOTFOUND)' )
               CALL CHKOUT( 'ZZSWFFET'                 )
               RETURN

            END IF

         END DO

      ELSE
!
!        Backstop: this code should be unreachable.
!
         CALL ZZSWFCLN ( HDFRAM, FRPOOL, BASBEG, FRAMAT )

         CALL SETMSG ( 'Base frame kernel variable # exists but ' &
         //            'DTPOOL returned data type # rather than ' &
         //            'one of the expected values: ''C'' or ''N''.' )
         CALL ERRCH  ( '#', KVALGN  )
         CALL ERRCH  ( '#', BASTYP  )
         CALL SIGERR ( 'SPICE(BUG)' )
         CALL CHKOUT( 'ZZSWFFET'    )
         RETURN

      END IF

!
!     Get attributes of each base frame.
!
      DO I = 1, NBASES
!
!        Store the frame class and frame class ID for this base frame.
!
         J = FREE - 1 + I

         CALL FRINFO ( BASLST(J), CENTER, &
                       CLSSES(J), CLSIDS(J), INFFND )

         IF ( FAILED() ) THEN

            CALL ZZSWFCLN ( HDFRAM, FRPOOL, BASBEG, FRAMAT )

            CALL CHKOUT ( 'ZZSWFFET' )
            RETURN

         END IF

         IF ( .NOT. INFFND ) THEN

            CALL ZZSWFCLN ( HDFRAM, FRPOOL, BASBEG, FRAMAT )

            CALL SETMSG ( 'No specification was found for ' &
            //            'base frame # of switch frame #.' )
            CALL ERRINT ( '#', BASLST(J)                    )
            CALL ERRINT ( '#', FRAMID                       )
            CALL SIGERR ( 'SPICE(FRAMEINFONOTFOUND)'        )
            CALL CHKOUT ( 'ZZSWFFET' )
            RETURN

         END IF

      END DO

!
!     Fetch interval bounds if they're present.
!
      IF ( USETIM(FRAMAT) ) THEN
!
!        Fetch start times.
!
!        Note that DTPOOL sets the type to 'X' if the target
!        kernel variable is not found.
!
         IF ( STRTYP .EQ. 'N' ) THEN
!
!           The start times are represented by double precision
!           numbers.
!
            CALL GDPOOL( KVSTRT, 1,            ROOM, &
                         NSTART, STARTS(FREE), KVFND(IXSTRT) )

         ELSE IF ( STRTYP .EQ. 'C' ) THEN
!
!           The start times are represented by strings.
!
!           We must convert each string to a numeric value and store
!           the value in the start time buffer.
!
            KVFND(IXSTRT) = .FALSE.

            DO I = 1, NBASES

               CALL GCPOOL( KVSTRT,  I,      1, &
                            N,       TIMSTR, KVFND(IXSTRT) )

               CALL STR2ET( TIMSTR, STARTS(FREE-1+I) )

            END DO

         ELSE
!
!           Backstop: this code should be unreachable.
!
            CALL ZZSWFCLN ( HDFRAM, FRPOOL, BASBEG, FRAMAT )

            CALL SETMSG ( 'Start time kernel variable # exists but ' &
            //            'DTPOOL returned data type # rather than ' &
            //            'one of the expected values: ''C'' ' &
            //            'or ''N''.' )
            CALL ERRCH  ( '#', KVSTRT  )
            CALL ERRCH  ( '#', STRTYP  )
            CALL SIGERR ( 'SPICE(BUG)' )
            CALL CHKOUT( 'ZZSWFFET'    )
            RETURN

         END IF

         IF ( FAILED() ) THEN

            CALL ZZSWFCLN ( HDFRAM, FRPOOL, BASBEG, FRAMAT )

            CALL CHKOUT( 'ZZSWFFET' )
            RETURN

         END IF

!
!        Fetch stop times.
!
         IF ( STPTYP .EQ. 'N' ) THEN
!
!           The stop times are represented by double precision numbers.
!
            CALL GDPOOL( KVSTOP, 1,           ROOM, &
                         NSTOP,  STOPS(FREE), KVFND(IXSTOP) )

         ELSE IF ( STPTYP .EQ. 'C' ) THEN
!
!           The stop times are represented by strings.
!
!           We must convert each string to a numeric value and store
!           the value in the stop time buffer.
!
            KVFND(IXSTOP) = .FALSE.

            DO I = 1, NSTOP

               CALL GCPOOL( KVSTOP,  I,      1, &
                            N,       TIMSTR, KVFND(IXSTOP) )

               CALL STR2ET( TIMSTR, STOPS(FREE-1+I) )

            END DO

         ELSE
!
!           Backstop: this code should be unreachable.
!
            CALL ZZSWFCLN ( HDFRAM, FRPOOL, BASBEG, FRAMAT )

            CALL SETMSG ( 'Stop time kernel variable # exists but ' &
            //            'DTPOOL returned data type # rather than ' &
            //            'one of the expected values: ''C'' ' &
            //            'or ''N''.' )
            CALL ERRCH  ( '#', KVSTOP  )
            CALL ERRCH  ( '#', STPTYP  )
            CALL SIGERR ( 'SPICE(BUG)' )
            CALL CHKOUT( 'ZZSWFFET'    )
            RETURN

         END IF

         IF ( FAILED() ) THEN

            CALL ZZSWFCLN ( HDFRAM, FRPOOL, BASBEG, FRAMAT )

            CALL CHKOUT( 'ZZSWFFET' )
            RETURN
         END IF

!
!        Singleton intervals and out-of-order intervals are not
!        allowed.
!
         DO I = 1, NSTART

            J = FREE - 1 + I

            IF ( STARTS(J) .GE. STOPS(J) ) THEN

               CALL ZZSWFCLN ( HDFRAM, FRPOOL, BASBEG, FRAMAT )

               CALL SETMSG ( 'Interval time bounds are not strictly ' &
               //            'increasing at interval index # for ' &
               //            'switch frame #. Time bounds are #:# TDB ' &
               //            '(# TDB : # TDB)'      )
               CALL ERRINT ( '#', I                 )
               CALL ERRINT ( '#', FRAMID            )
               CALL ERRDP  ( '#', STARTS(J)         )
               CALL ERRDP  ( '#', STOPS(J)          )
               CALL ETCAL  ( STARTS(J), TIMSTR      )
               CALL ERRCH  ( '#',       TIMSTR      )
               CALL ETCAL  ( STOPS(J),  TIMSTR      )
               CALL ERRCH  ( '#',       TIMSTR      )
               CALL SIGERR ( 'SPICE(BADTIMEBOUNDS)' )
               CALL CHKOUT ( 'ZZSWFFET'             )
               RETURN

            END IF

         END DO

      END IF

!
!     Determine whether binary search on the time intervals is
!     possible.
!
      IF ( USETIM(FRAMAT) ) THEN

         I = 2
         BINARY( FRAMAT ) = .TRUE.

         DO WHILE (  ( I .LE. NBASES ) .AND.  BINARY(FRAMAT)  )
!
!           Note that proper ordering of start and stop times for
!           each interval has already been verified.
!
            J = BASBEG(FRAMAT) - 1 + I

            IF ( STOPS(J-1) .GT. STARTS(J) ) THEN

               BINARY( FRAMAT ) = .FALSE.

            END IF

            I = I + 1

         END DO

      ELSE
         BINARY( FRAMAT ) = .FALSE.
      END IF

!
!     We've found the data. The switch frame database has been
!     updated. FRAMAT is already set.
!
!     Account for the base frame storage used.
!
      FREE = FREE + NBASES

      CALL CHKOUT ( 'ZZSWFFET' )
      RETURN
      END
