!$Procedure SPKW05 ( Write SPK segment, type 5 )

      subroutine spkw05 ( handle, body,  center, frame,  first, last, &
                          segid,  gm,    n,      states, epochs      )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Write an SPK segment of type 5 given a time-ordered set of
!     discrete states and epochs, and the gravitational parameter
!     of a central body.
!
! $ Required_Reading
!
!     SPK
!     SPC
!     NAIF_IDS
!
! $ Keywords
!
!     EPHEMERIS
!
! $ Declarations

      implicit none

      integer(ip)           handle
      integer(ip)           body
      integer(ip)           center
      character(*)          frame
      real(wp)              first
      real(wp)              last
      character(*)          segid
      real(wp)              gm
      integer(ip)           n
      real(wp)              states   ( 6, * )
      real(wp)              epochs   (    * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of an SPK file open for writing.
!     BODY       I   Body code for ephemeris object.
!     CENTER     I   Body code for the center of motion of the body.
!     FRAME      I   The reference frame of the states.
!     FIRST      I   First valid time for which states can be computed.
!     LAST       I   Last valid time for which states can be computed.
!     SEGID      I   Segment identifier.
!     GM         I   Gravitational parameter of central body.
!     N          I   Number of states and epochs.
!     STATES     I   States.
!     EPOCHS     I   Epochs.
!
! $ Detailed_Input
!
!     HANDLE   is the file handle of an SPK file
!              opened for writing.
!
!     BODY     is the NAIF ID for the body whose states are
!              to be recorded in an SPK file.
!
!     CENTER   is the NAIF ID for the center of motion associated
!              with BODY.
!
!     FRAME    is the reference frame that states are referenced to,
!              for example 'J2000'.
!
!     FIRST,
!     LAST     are the bounds on the ephemeris times, expressed as
!              seconds past J2000, for which the states can be used
!              to interpolate a state for BODY.
!
!     SEGID    is the segment identifier. An SPK segment identifier
!              may contain up to 40 characters.
!
!     GM       is the gravitational parameter of the central body
!              ( in units of kilometers **3 / seconds **2 ).
!
!     N        is the number of states and epochs to be stored
!              in the segment.
!
!     STATES   contains a time-ordered array of geometric states
!              ( x, y, z, dx/dt, dy/dt, dz/dt, in kilometers and
!              kilometers per second ) of the target body with
!              respect to the central body specified in the segment
!              descriptor.
!
!     EPOCHS   contains the epochs (ephemeris seconds past J2000)
!              corresponding to the states in STATES. Epochs must
!              form a strictly increasing sequence.
!
! $ Detailed_Output
!
!     None. A type 5 segment is written to the file attached to HANDLE.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If GM is not positive, the error SPICE(NONPOSITIVEMASS)
!         is signaled.
!
!     2)  If the input epochs do not form an increasing sequence, the
!         error SPICE(UNORDEREDTIMES) is signaled.
!
!     3)  If the number of states and epochs is not positive, the
!         error SPICE(NUMSTATESNOTPOS) is signaled.
!
!     4)  If FIRST is greater than LAST, the error
!         SPICE(BADDESCRTIMES) is signaled.
!
!     5)  If SEGID is more than 40 characters long, the error
!         SPICE(SEGIDTOOLONG) is signaled.
!
!     6)  If SEGID contains any nonprintable characters, the error
!         SPICE(NONPRINTABLECHARS) is signaled.
!
!     7)  If a file I/O problem occurs, an error is signaled by a
!         routine in the call tree of this routine.
!
! $ Files
!
!     A new type 05 SPK segment is written to the SPK file attached
!     to HANDLE.
!
! $ Particulars
!
!     This routine writes an SPK type 05 data segment to the open SPK
!     file according to the format described in the type 05 section of
!     the SPK Required Reading. The SPK file must have been opened with
!     write access.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) This example demonstrates how to create an SPK type 5 kernel
!        containing only one segment, given a time-ordered set of
!        discrete states and epochs, and the gravitational parameter
!        of a central body.
!
!
!        Example code begins here.
!
!
!              PROGRAM SPKW05_EX1
!              IMPLICIT NONE
!
!        C
!        C     Local parameters.
!        C
!              INTEGER               NAMLEN
!              PARAMETER           ( NAMLEN = 40 )
!
!        C
!        C     Define the segment identifier parameters.
!        C
!              CHARACTER*(*)         SPK5
!              PARAMETER           ( SPK5  = 'spkw05_ex1.bsp' )
!
!              CHARACTER*(*)         REF
!              PARAMETER           ( REF    = 'J2000'          )
!
!              DOUBLE PRECISION      GMSUN
!              PARAMETER           ( GMSUN  = 132712440023.310D0 )
!
!              INTEGER               BODY
!              PARAMETER           ( BODY   = 3  )
!
!              INTEGER               CENTER
!              PARAMETER           ( CENTER = 10 )
!
!              INTEGER               NSTATS
!              PARAMETER           ( NSTATS = 9  )
!
!        C
!        C     Local variables.
!        C
!              CHARACTER*(NAMLEN)    IFNAME
!              CHARACTER*(NAMLEN)    SEGID
!
!              DOUBLE PRECISION      EPOCHS (    NSTATS )
!              DOUBLE PRECISION      FIRST
!              DOUBLE PRECISION      LAST
!              DOUBLE PRECISION      STATES ( 6, NSTATS )
!
!              INTEGER               HANDLE
!              INTEGER               NCOMCH
!
!        C
!        C     Define the states and epochs.
!        C
!              DATA                  STATES /
!             .       101.D0, 201.D0, 301.D0, 401.D0, 501.D0, 601.D0,
!             .       102.D0, 202.D0, 302.D0, 402.D0, 502.D0, 602.D0,
!             .       103.D0, 203.D0, 303.D0, 403.D0, 503.D0, 603.D0,
!             .       104.D0, 204.D0, 304.D0, 404.D0, 504.D0, 604.D0,
!             .       105.D0, 205.D0, 305.D0, 405.D0, 505.D0, 605.D0,
!             .       106.D0, 206.D0, 306.D0, 406.D0, 506.D0, 606.D0,
!             .       107.D0, 207.D0, 307.D0, 407.D0, 507.D0, 607.D0,
!             .       108.D0, 208.D0, 308.D0, 408.D0, 508.D0, 608.D0,
!             .       109.D0, 209.D0, 309.D0, 409.D0, 509.D0, 609.D0 /
!
!              DATA                  EPOCHS / 100.D0, 200.D0, 300.D0,
!             .                               400.D0, 500.D0, 600.D0,
!             .                               700.D0, 800.D0, 900.D0 /
!
!        C
!        C     Set the start and end times of interval covered by
!        C     segment.
!        C
!              FIRST  = EPOCHS(1)
!              LAST   = EPOCHS(NSTATS)
!
!        C
!        C     NCOMCH is the number of characters to reserve for the
!        C     kernel's comment area. This example doesn't write
!        C     comments, so set to zero.
!        C
!              NCOMCH = 0
!
!        C
!        C     Internal file name and segment ID.
!        C
!              IFNAME = 'Type 5 SPK internal file name.'
!              SEGID  = 'SPK type 5 test segment'
!
!        C
!        C     Open a new SPK file.
!        C
!              CALL SPKOPN( SPK5, IFNAME, NCOMCH, HANDLE )
!
!        C
!        C     Write the segment.
!        C
!              CALL SPKW05 ( HANDLE, BODY,   CENTER, REF,
!             .              FIRST,  LAST,   SEGID,  GMSUN,
!             .              NSTATS, STATES, EPOCHS        )
!
!        C
!        C     Close the SPK file.
!        C
!              CALL SPKCLS ( HANDLE )
!
!              END
!
!
!        When this program is executed, no output is presented on
!        screen. After run completion, a new SPK type 5 exists in
!        the output directory.
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
!     J. Diaz del Rio    (ODC Space)
!     K.R. Gehringer     (JPL)
!     J.M. Lynch         (JPL)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.0, 17-JUN-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Added complete
!        example code from existing fragment. Removed unnecessary
!        $Revisions section.
!
!-    SPICELIB Version 1.1.0, 30-OCT-2006 (BVS)
!
!        Removed restriction that the input reference frame should be
!        inertial by changing the routine that determines the frame ID
!        from the name from IRFNUM to NAMFRM.
!
!-    SPICELIB Version 1.0.2, 27-JAN-2003 (EDW)
!
!        Added error check to catch non-positive gravitational
!        parameter GM.
!
!-    SPICELIB Version 1.0.1, 05-OCT-1993 (KRG)
!
!        Removed all references to a specific method of opening the SPK
!        file in the $Brief_I/O, $Detailed_Input, $Particulars and
!        $Examples sections of the header. It is assumed that a person
!        using this routine has some knowledge of the DAF system and the
!        methods for obtaining file handles.
!
!-    SPICELIB Version 1.0.0, 01-APR-1992 (JML) (WLT) (IMU)
!
!-&


! $ Index_Entries
!
!     write SPK type_5 ephemeris data segment
!
!-&


!
!     SPICELIB functions
!
      logical               failed
      logical               return
      integer(ip)           lastnb

!
!     Local parameters
!
!     SIDLEN is the maximum number of characters allowed in an
!     SPK segment identifier.
!
!     NS is the size of a packed SPK segment descriptor.
!
!     ND is the number of double precision components in an SPK
!     segment descriptor.
!
!     NI is the number of integer components in an SPK segment
!     descriptor.
!
!     DTYPE is the data type.
!
!     FPRINT is the integer value of the first printable ASCII
!     character.
!
!     LPRINT is the integer value of the last printable ASCII character.
!
!
      integer(ip)           sidlen
      parameter           ( sidlen  =  40 )

      integer(ip)           ns
      parameter           ( ns      =   5 )

      integer(ip)           nd
      parameter           ( nd      =   2 )

      integer(ip)           ni
      parameter           ( ni      =   6 )

      integer(ip)           dtype
      parameter           ( dtype   =   5 )

      integer(ip)           fprint
      parameter           ( fprint  =  32 )

      integer(ip)           lprint
      parameter           ( lprint  = 126 )


!
!     Local variables
!

      real(wp)              dcd      ( nd     )
      real(wp)              descr    ( ns     )

      integer(ip)           i
      integer(ip)           refcod
      integer(ip)           value
      integer(ip)           icd      ( ni )


!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'SPKW05' )
      end if


      if ( gm <= 0.0_wp ) then

         call setmsg ( 'GM = #; Non-positive gravitational parameter' )
         call errdp  ( '#',  gm                   )
         call sigerr ( 'SPICE(NONPOSITIVEMASS)'   )
         call chkout ( 'SPKW05'                   )
         return

      end if


!
!     Get the NAIF integer code for the reference frame.
!
      call namfrm ( frame, refcod )

      if ( refcod == 0 ) then

         call setmsg ( 'The reference frame # is not supported.'   )
         call errch  ( '#', frame                                  )
         call sigerr ( 'SPICE(INVALIDREFFRAME)'                    )
         call chkout ( 'SPKW05'                                    )
         return

      end if

!
!     Make sure that the number of states and epochs is positive.
!
      if ( n <= 0 ) then

         call setmsg ( 'The number of states and epochs is not '  // &
                       'positive. N = #'                           )
         call errint ( '#', n                                      )
         call sigerr ( 'SPICE(NUMSTATESNOTPOS)'                    )
         call chkout ( 'SPKW05'                                    )
         return

      end if
!
!     Check the input epochs to make sure that they form a
!     strictly increasing sequence.
!
      do i = 2, n

         if ( epochs(i) <= epochs(i-1) ) then

            call setmsg ( 'Epoch # is out of order. ' )
            call errdp  ( '#',  epochs(i)             )
            call sigerr ( 'SPICE(UNORDEREDTIMES)'     )
            call chkout ( 'SPKW05'                    )
            return

         end if

      end do

!
!     The segment stop time should be greater then the begin time.
!
      if ( first > last ) then

         call setmsg ( 'The segment start time: # is greater then ' // &
                       'the segment end time: #'                   )
         call errdp  ( '#', first                                  )
         call errdp  ( '#', last                                   )
         call sigerr ( 'SPICE(BADDESCRTIMES)'                      )
         call chkout ( 'SPKW05'                                    )
         return

      end if

!
!     Now check that all the characters in the segid can be printed.
!
      do i = 1, lastnb(segid)

         value = ichar( segid(i:i) )

         if ( ( value < fprint ) .or. ( value > lprint ) ) then

            call setmsg ( 'The segment identifier contains '  // &
                          'nonprintable characters'               )
            call sigerr ( 'SPICE(NONPRINTABLECHARS)'              )
            call chkout ( 'SPKW05'                                )
            return

         end if

      end do

!
!     Also check to see if the segment identifier is too long.
!
      if ( lastnb(segid) > sidlen ) then

         call setmsg ( 'Segment identifier contains more than ' // &
                       '40 characters.'                          )
         call sigerr ( 'SPICE(SEGIDTOOLONG)'                     )
         call chkout ( 'SPKW05'                                  )
         return

      end if

!
!     Store the start and end times to be associated
!     with this segment.
!
      dcd(1) = first
      dcd(2) = last

!
!     Create the integer portion of the descriptor.
!
      icd(1) = body
      icd(2) = center
      icd(3) = refcod
      icd(4) = dtype

!
!     Pack the segment descriptor.
!
      call dafps ( nd, ni, dcd, icd, descr )

!
!     Begin a new segment.
!
      call dafbna ( handle, descr, segid )

      if ( failed() ) then
         call chkout ( 'SPKW05' )
         return
      end if

!
!     This could hardly be simpler. Stuff the states into the segment,
!     followed by the epochs.
!
      call dafada ( states, 6 * n )
      call dafada ( epochs,     n )

!
!     If there are at least 100 state/epoch pairs, write a directory
!     containing every 100'th epoch.
!
      i = 100

      do while ( i <= n )
         call dafada ( epochs(i), 1 )
         i = i + 100
      end do

!
!     Store the GM of the central body, and the number of states.
!
      call dafada ( gm,         1 )
      call dafada ( real( n, wp ), 1 )

!
!     If anything went wrong, don't end the segment.
!
      if ( .not. failed() ) then
         call dafena
      end if

      call chkout ( 'SPKW05' )
      return
      end
