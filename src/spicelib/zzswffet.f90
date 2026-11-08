!$Procedure ZZSWFFET ( Private, switch frame kernel pool data fetch )

      subroutine zzswffet ( framid, hdfram, frpool, fidlst, basbeg, &
                            free,   bascnt, usetim, binary, clsses, &
                            clsids, baslst, starts, stops,  framat )
      use pepper_kinds, only: ip, wp

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

      implicit none

      include              'zzswtchf.inc'

      integer(ip)           framid
      integer(ip)           hdfram ( * )
      integer(ip)           frpool ( lbsngl : * )
      integer(ip)           fidlst ( * )
      integer(ip)           basbeg ( * )
      integer(ip)           free
      integer(ip)           bascnt ( * )
      logical               usetim ( * )
      logical               binary ( * )
      integer(ip)           clsses ( * )
      integer(ip)           clsids ( * )
      integer(ip)           baslst ( * )
      real(wp)              starts ( * )
      real(wp)              stops ( * )
      integer(ip)           framat

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
      logical               return
      logical               failed

!
!     Local parameters
!

!
!     Kernel variable name templates:
!
      character(*)          tmalgn
      parameter           ( tmalgn = 'FRAME_#_ALIGNED_WITH' )

      character(*)          tmcent
      parameter           ( tmcent = 'FRAME_#_CENTER' )

      character(*)          tmclas
      parameter           ( tmclas = 'FRAME_#_CLASS' )

      character(*)          tmclid
      parameter           ( tmclid = 'FRAME_#_CLASS_ID' )

      character(*)          tmid
      parameter           ( tmid   = 'FRAME_#' )

      character(*)          tmfnam
      parameter           ( tmfnam = 'FRAME_#_NAME' )

      character(*)          tmstrt
      parameter           ( tmstrt = 'FRAME_#_START' )

      character(*)          tmstop
      parameter           ( tmstop = 'FRAME_#_STOP' )

!
!     Other parameters
!
      integer(ip)           frnmln
      parameter           ( frnmln = 32 )

      integer(ip)           timlen
      parameter           ( timlen = 80 )

!
!     Indices of items in the arrays of kernel variables:
!
      integer(ip)           ixfnam
      parameter           ( ixfnam = 1 )

      integer(ip)           ixid
      parameter           ( ixid   = ixfnam + 1 )

      integer(ip)           ixcent
      parameter           ( ixcent = ixid    + 1 )

      integer(ip)           ixclas
      parameter           ( ixclas = ixcent + 1 )

      integer(ip)           ixclid
      parameter           ( ixclid = ixclas + 1 )

      integer(ip)           ixalgn
      parameter           ( ixalgn = ixclid + 1 )

      integer(ip)           ixstrt
      parameter           ( ixstrt = ixalgn + 1 )

      integer(ip)           ixstop
      parameter           ( ixstop = ixstrt + 1 )


      integer(ip)           kvnmln
      parameter           ( kvnmln = 32 )

      integer(ip)           nkv
      parameter           ( nkv    = 8 )

!
!     Local variables
!
      character(frnmln)     basnam
      character(1)          bastyp
      character(frnmln)     frname
      character(kvnmln)     kvalgn
      character(kvnmln)     kvclid
      character(kvnmln)     kvcent
      character(kvnmln)     kvclas
      character(kvnmln)     kvid
      character(kvnmln)     kvfnam
      character(kvnmln)     kvnams ( nkv )
      character(kvnmln)     kvstrt
      character(kvnmln)     kvstop
      character(1)          stptyp
      character(1)          strtyp
      character(timlen)     timstr

      integer(ip)           center
      integer(ip)           fid
      integer(ip)           frcent
      integer(ip)           frclas
      integer(ip)           frclid
      integer(ip)           i
      integer(ip)           j
      integer(ip)           n
      integer(ip)           nbases
      integer(ip)           nfravl
      integer(ip)           nstart
      integer(ip)           nstop
      integer(ip)           room

!
!     KVFND(*) indicates whether a specified kernel variable was found.
!
      logical               havtim
      logical               inffnd
      logical               kvfnd ( nkv )
      logical               new

      logical               strxst
      logical               stpxst

!
!     Standard SPICE error handling.
!
      if ( return() ) return


!
!     No result found yet.
!
      framat = 0

!
!     Create names of kernel variables, using the input frame ID.
!
!     Basic frame specification variables, excluding that for the
!     frame ID. We'll need the frame name before we can create the
!     name of the kernel variable for the frame ID.
!
      call repmi( tmcent, '#', framid, kvcent )
      call repmi( tmclas, '#', framid, kvclas )
      call repmi( tmclid, '#', framid, kvclid )
      call repmi( tmfnam, '#', framid, kvfnam )

!
!     Switch frame specification variables:
!
      call repmi( tmalgn, '#', framid, kvalgn )
      call repmi( tmstrt, '#', framid, kvstrt )
      call repmi( tmstop, '#', framid, kvstop )

!
!     Below, we use logical flags to describe the status
!     of kernel variable availability:
!
!         KVFND(*) indicates that a variable exists.
!
!     Look up the basic variables.
!
      call gipool( kvclas, 1, 1, n, frclas, kvfnd(ixclas) )
      call gipool( kvclid, 1, 1, n, frclid, kvfnd(ixclid) )
      call gcpool( kvfnam, 1, 1, n, frname, kvfnd(ixfnam) )

      if ( failed() ) then

         call zzswfcln ( hdfram, frpool, basbeg, framat )

         return

      end if

!
!     Fetch the switch frame ID variable.
!
      if ( kvfnd(ixfnam) ) then
!
!        Use the frame name to fetch the switch frame ID variable.
!
         call repmc ( tmid, '#', frname, kvid )

         call gipool( kvid, 1, 1, n, fid, kvfnd(ixid) )

         if ( failed() ) then

            call zzswfcln ( hdfram, frpool, basbeg, framat )

            return

         end if

!
!        The frame ID of the frame specification had better match
!        the input frame ID.
!
         if ( kvfnd(ixid) .and. ( fid /= framid ) ) then

            call zzswfcln ( hdfram, frpool, basbeg, framat )

            call setmsg ( 'Input frame ID was #, but ID in frame ' &
            //            'specification from kernel pool was #. ' )
            call errint ( '#', framid           )
            call errint ( '#', fid              )
            call sigerr ( 'SPICE(BADFRAMESPEC)' )
            return

         end if
!
!        The frame name must always be found. If not, the error
!        will be diagnosed in the block below.
!
      else
!
!        We couldn't find the frame name. Indicate the frame ID
!        variable [sic] wasn't found, since its found flag won't be set
!        by GIPOOL.
!
         kvfnd(ixid) = .false.

      end if

!
!     Look up the central body of the frame. The name of the kernel
!     variable for the body could refer to the frame by name or frame
!     ID; the body itself could be specified by name or body ID.
!
      if ( kvfnd(ixfnam) ) then

         call zzdynbid ( frname, framid, 'CENTER', frcent )

         if ( failed() ) then

            call zzswfcln ( hdfram, frpool, basbeg, framat )

            return

         end if

         kvfnd(ixcent) = .true.
      else
         kvfnd(ixcent) = .false.
      end if

!
!     Look up the type and count of the base frame list. It
!     may have either string or numeric type.
!
      call dtpool( kvalgn, kvfnd(ixalgn), nbases, bastyp )

      if ( failed() ) then
!
!        This code should be unreachable but is provided for safety.
!
         call zzswfcln ( hdfram, frpool, basbeg, framat )

         return

      end if

!
!     Store the kernel variable names in order to prepare for
!     checking availability of required variables.
!
      kvnams(ixid)   = kvid
      kvnams(ixfnam) = kvfnam
      kvnams(ixcent) = kvcent
      kvnams(ixclas) = kvclas
      kvnams(ixclid) = kvclid
      kvnams(ixalgn) = kvalgn

!
!     Check for required variables that haven't been supplied.
!
      do i = 1, ixalgn
!
!        The first 6 items are needed; start and stop times are needed
!        if at least one is present.
!
         if ( .not. kvfnd(i) ) then

            call zzswfcln ( hdfram, frpool, basbeg, framat )

            call setmsg ( 'Kernel variable #, needed for ' &
            //            'specification of switch frame having ' &
            //            'frame ID #, was not ' &
            //            'found in the kernel pool. This can occur ' &
            //            'when a frame kernel providing the required ' &
            //            'switch frame specification has not been ' &
            //            'loaded, or if the specification is present ' &
            //            'but is incorrect.'                         )
            call errch  ( '#', kvnams(i)                              )
            call errint ( '#', framid                                 )
            call sigerr ( 'SPICE(MISSINGFRAMEVAR)'                    )
            return

         end if

      end do

!
!     Find out whether time bounds are supplied, and if so, which data
!     type is used to represent them.
!
      kvnams(ixstrt) = kvstrt
      kvnams(ixstop) = kvstop

      call dtpool( kvstrt, kvfnd(ixstrt), nstart, strtyp )
      call dtpool( kvstop, kvfnd(ixstop), nstop,  stptyp )

      if ( failed() ) then
!
!        This code should be unreachable but is provided for safety.
!
         call zzswfcln ( hdfram, frpool, basbeg, framat )

         return

      end if

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
      strxst = kvfnd(ixstrt)
      stpxst = kvfnd(ixstop)

      havtim = strxst .and. stpxst

      if ( havtim ) then
!
!        Make sure that counts of base frames, start times, and
!        stop times are equal.
!
         if ( ( nstart /= nstop ) .or. ( nstart /= nbases ) ) then

            call zzswfcln ( hdfram, frpool, basbeg, framat )

            call setmsg ( 'Kernel variables for the switch frame ' &
            //            'having frame ID # have mismatched sizes: ' &
            //            'number of base frames = #; number of start ' &
            //            'times = #; number of stop times = #.'      )
            call errint ( '#', framid            )
            call errint ( '#', nbases            )
            call errint ( '#', nstart            )
            call errint ( '#', nstop             )
            call sigerr ( 'SPICE(COUNTMISMATCH)' )
            return

         end if

      else
!
!        Check for inconsistent presence of start and stop times.
!
         if ( strxst .or. stpxst ) then

            call zzswfcln ( hdfram, frpool, basbeg, framat )

!
!           Create long message template to be filled in.
!
            call setmsg ( 'Kernel variable #, which specifies base ' &
            //            'frame applicability # times, was not ' &
            //            'provided for the switch frame having ' &
            //            'frame ID #, while the kernel variable # ' &
            //            'specifying base frame applicability ' &
            //            '# times was provided. Switch frame ' &
            //            'applicability start and stop times are ' &
            //            'optional, but both must be provided if ' &
            //            'either is.'  )

            if ( strxst ) then
!
!              Stop times are missing.
!
               call errch  ( '#', kvstop  )
               call errch  ( '#', 'stop'  )
               call errint ( '#', framid  )
               call errch  ( '#', kvstrt  )
               call errch  ( '#', 'start' )

            else
!
!              Start times are missing.
!
               call errch  ( '#', kvstrt  )
               call errch  ( '#', 'start' )
               call errint ( '#', framid  )
               call errch  ( '#', kvstop  )
               call errch  ( '#', 'stop'  )

            end if

            call sigerr ( 'SPICE(PARTIALFRAMESPEC)' )
            return

         end if

      end if

!
!     Compute room for the output data.
!
      room = maxbas + 1 - free

!
!     Check the available room in the frame ID pool and in the frame
!     base arrays.
!
      call zzhsiavl ( frpool, nfravl )

      if ( ( nfravl == 0 ) .or. ( room < nbases ) ) then
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
         if ( nbases <= maxbas ) then
!
!           We can fit the frame data in by clearing out the
!           database.
!
            call zzswfcln ( hdfram, frpool, basbeg, framat )

!
!           The database is now initialized.
!
            free = 1
            room = maxbas

         else
!
!           We can't make enough room.
!
!           Initialize local data structures, for safety.
!
            call zzswfcln ( hdfram, frpool, basbeg, framat )

            call setmsg ( 'The requested frame # has # associated ' &
            //            'base frames. The maximum number that ' &
            //            'can be supported is #.'   )
            call errint ( '#', framid                )
            call errint ( '#', nbases                )
            call errint ( '#', maxbas                )
            call sigerr ( 'SPICE(TOOMANYBASEFRAMES)' )
            return

         end if

      end if

!
!     Add the frame and its data to the local database.
!
!     Start by adding the frame ID to the frame hash structure.
!     The output argument NEW indicates whether the item is new;
!     we don't need to check this argument. We also don't need to
!     check for failure of ZZHSIADD, since we've already ensured
!     there's room in the hash for a new clock.
!
      call zzhsiadd ( hdfram, frpool, fidlst, framid, framat, new )

!
!     At this point, FRAMAT is set.
!
!     Store the frame ID, base start index, base count, and time
!     interval availability attributes for this frame.
!
      fidlst( framat ) = framid
      basbeg( framat ) = free
      bascnt( framat ) = nbases
      usetim( framat ) = havtim

!
!     Look up the base frame variables. The base frame list may have
!     either string or numeric type.
!
      kvfnd(ixalgn) = .false.

      if ( bastyp == 'N' ) then
!
!        The frames are specified by ID code. We can buffer the frame
!        ID codes immediately.
!
         call gipool ( kvalgn,       1,            room, &
                       bascnt(free), baslst(free), kvfnd(ixalgn) )

         if ( failed() ) then

            call zzswfcln ( hdfram, frpool, basbeg, framat )

            return

         end if

      else if ( bastyp == 'C' ) then
!
!        The frames are specified by name. We must convert the
!        names to frame ID codes before we can can buffer the IDs.
!
         kvfnd(ixalgn) = .false.

         do i = 1, nbases

            call gcpool( kvalgn,  i,       1, &
                         n,       basnam,  kvfnd(ixalgn) )

            if ( failed() ) then
!
!              This code should be unreachable but is provided for
!              safety.
!
               call zzswfcln ( hdfram, frpool, basbeg, framat )

               return

            end if

            call namfrm( basnam, baslst(free-1+i) )

            if ( baslst(free-1+i) == 0 ) then

               call zzswfcln ( hdfram, frpool, basbeg, framat )

               call setmsg( 'Base frame name # of switch frame ' &
               //           '# could not be translated to a ' &
               //           'frame ID code '           )
               call errch ( '#', basnam                )
               call errint( '#', framid                )
               call sigerr( 'SPICE(FRAMENAMENOTFOUND)' )
               return

            end if

         end do

      else
!
!        Backstop: this code should be unreachable.
!
         call zzswfcln ( hdfram, frpool, basbeg, framat )

         call setmsg ( 'Base frame kernel variable # exists but ' &
         //            'DTPOOL returned data type # rather than ' &
         //            'one of the expected values: ''C'' or ''N''.' )
         call errch  ( '#', kvalgn  )
         call errch  ( '#', bastyp  )
         call sigerr ( 'SPICE(BUG)' )
         return

      end if

!
!     Get attributes of each base frame.
!
      do i = 1, nbases
!
!        Store the frame class and frame class ID for this base frame.
!
         j = free - 1 + i

         call frinfo ( baslst(j), center, &
                       clsses(j), clsids(j), inffnd )

         if ( failed() ) then

            call zzswfcln ( hdfram, frpool, basbeg, framat )

            return

         end if

         if ( .not. inffnd ) then

            call zzswfcln ( hdfram, frpool, basbeg, framat )

            call setmsg ( 'No specification was found for ' &
            //            'base frame # of switch frame #.' )
            call errint ( '#', baslst(j)                    )
            call errint ( '#', framid                       )
            call sigerr ( 'SPICE(FRAMEINFONOTFOUND)'        )
            return

         end if

      end do

!
!     Fetch interval bounds if they're present.
!
      if ( usetim(framat) ) then
!
!        Fetch start times.
!
!        Note that DTPOOL sets the type to 'X' if the target
!        kernel variable is not found.
!
         if ( strtyp == 'N' ) then
!
!           The start times are represented by double precision
!           numbers.
!
            call gdpool( kvstrt, 1,            room, &
                         nstart, starts(free), kvfnd(ixstrt) )

         else if ( strtyp == 'C' ) then
!
!           The start times are represented by strings.
!
!           We must convert each string to a numeric value and store
!           the value in the start time buffer.
!
            kvfnd(ixstrt) = .false.

            do i = 1, nbases

               call gcpool( kvstrt,  i,      1, &
                            n,       timstr, kvfnd(ixstrt) )

               call str2et( timstr, starts(free-1+i) )

            end do

         else
!
!           Backstop: this code should be unreachable.
!
            call zzswfcln ( hdfram, frpool, basbeg, framat )

            call setmsg ( 'Start time kernel variable # exists but ' &
            //            'DTPOOL returned data type # rather than ' &
            //            'one of the expected values: ''C'' ' &
            //            'or ''N''.' )
            call errch  ( '#', kvstrt  )
            call errch  ( '#', strtyp  )
            call sigerr ( 'SPICE(BUG)' )
            return

         end if

         if ( failed() ) then

            call zzswfcln ( hdfram, frpool, basbeg, framat )

            return

         end if

!
!        Fetch stop times.
!
         if ( stptyp == 'N' ) then
!
!           The stop times are represented by double precision numbers.
!
            call gdpool( kvstop, 1,           room, &
                         nstop,  stops(free), kvfnd(ixstop) )

         else if ( stptyp == 'C' ) then
!
!           The stop times are represented by strings.
!
!           We must convert each string to a numeric value and store
!           the value in the stop time buffer.
!
            kvfnd(ixstop) = .false.

            do i = 1, nstop

               call gcpool( kvstop,  i,      1, &
                            n,       timstr, kvfnd(ixstop) )

               call str2et( timstr, stops(free-1+i) )

            end do

         else
!
!           Backstop: this code should be unreachable.
!
            call zzswfcln ( hdfram, frpool, basbeg, framat )

            call setmsg ( 'Stop time kernel variable # exists but ' &
            //            'DTPOOL returned data type # rather than ' &
            //            'one of the expected values: ''C'' ' &
            //            'or ''N''.' )
            call errch  ( '#', kvstop  )
            call errch  ( '#', stptyp  )
            call sigerr ( 'SPICE(BUG)' )
            return

         end if

         if ( failed() ) then

            call zzswfcln ( hdfram, frpool, basbeg, framat )

            return
         end if

!
!        Singleton intervals and out-of-order intervals are not
!        allowed.
!
         do i = 1, nstart

            j = free - 1 + i

            if ( starts(j) >= stops(j) ) then

               call zzswfcln ( hdfram, frpool, basbeg, framat )

               call setmsg ( 'Interval time bounds are not strictly ' &
               //            'increasing at interval index # for ' &
               //            'switch frame #. Time bounds are #:# TDB ' &
               //            '(# TDB : # TDB)'      )
               call errint ( '#', i                 )
               call errint ( '#', framid            )
               call errdp  ( '#', starts(j)         )
               call errdp  ( '#', stops(j)          )
               call etcal  ( starts(j), timstr      )
               call errch  ( '#',       timstr      )
               call etcal  ( stops(j),  timstr      )
               call errch  ( '#',       timstr      )
               call sigerr ( 'SPICE(BADTIMEBOUNDS)' )
               return

            end if

         end do

      end if

!
!     Determine whether binary search on the time intervals is
!     possible.
!
      if ( usetim(framat) ) then

         i = 2
         binary( framat ) = .true.

         do while (  ( i <= nbases ) .and.  binary(framat)  )
!
!           Note that proper ordering of start and stop times for
!           each interval has already been verified.
!
            j = basbeg(framat) - 1 + i

            if ( stops(j-1) > starts(j) ) then

               binary( framat ) = .false.

            end if

            i = i + 1

         end do

      else
         binary( framat ) = .false.
      end if

!
!     We've found the data. The switch frame database has been
!     updated. FRAMAT is already set.
!
!     Account for the base frame storage used.
!
      free = free + nbases

      return
      end
