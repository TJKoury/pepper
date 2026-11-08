!$Procedure SPKW12 ( SPK, write segment, type 12 )

      subroutine spkw12 (  handle,  body,    center,  frame, &
                           first,   last,    segid,   degree, &
                           n,       states,  begtim,  step     )

! $ Abstract
!
!     Write a type 12 segment to an SPK file.
!
! $ Required_Reading
!
!     NAIF_IDS
!     SPC
!     SPK
!     TIME
!
! $ Keywords
!
!     EPHEMERIS
!     FILES
!
! $ Declarations

      implicit none

      include 'spk12.inc'

      integer               handle
      integer               body
      integer               center
      character*(*)         frame
      double precision      first
      double precision      last
      character*(*)         segid
      integer               degree
      integer               n
      double precision      states ( 6, * )
      double precision      begtim
      double precision      step

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     MAXDEG     P   Maximum degree of interpolating polynomials.
!     TOLSCL     P   Scale factor used to compute time bound tolerance.
!     HANDLE     I   Handle of an SPK file open for writing.
!     BODY       I   NAIF code for an ephemeris object.
!     CENTER     I   NAIF code for center of motion of BODY.
!     FRAME      I   Reference frame name.
!     FIRST      I   Start time of interval covered by segment.
!     LAST       I   End time of interval covered by segment.
!     SEGID      I   Segment identifier.
!     DEGREE     I   Degree of interpolating polynomials.
!     N          I   Number of states.
!     STATES     I   Array of states.
!     BEGTIM     I   Epoch of first state in STATES array.
!     STEP       I   Time step separating epochs of states.
!
! $ Detailed_Input
!
!     HANDLE   is the file handle of an SPK file that has been
!              opened for writing.
!
!     BODY     is the NAIF integer code for an ephemeris object
!              whose state relative to another body is described
!              by the segment to be created.
!
!     CENTER   is the NAIF integer code for the center of motion
!              of the object identified by BODY.
!
!     FRAME    is the NAIF name for a reference frame
!              relative to which the state information for BODY
!              is specified.
!
!     FIRST,
!     LAST     are, respectively, the start and stop times of
!              the time interval over which the segment defines
!              the state of BODY.
!
!     SEGID    is the segment identifier. An SPK segment
!              identifier may contain up to 40 characters.
!
!     DEGREE   is the degree of the Lagrange polynomials used to
!              interpolate the states. All components of the
!              state vectors are interpolated by polynomials of
!              fixed degree.
!
!     N        is the number of states in the input state vector
!              array.
!
!     STATES   contains a time-ordered array of geometric states
!              ( x, y, z, dx/dt, dy/dt, dz/dt, in kilometers and
!              kilometers per second ) of BODY relative to CENTER,
!              specified relative to FRAME.
!
!     BEGTIM   is the epoch corresponding to the first state in
!              the state array. Because extra states are needed
!              at the beginning and end of the segment in order
!              for the interpolation method to work, BEGTIM will
!              normally precede FIRST.
!
!     STEP     is the time step separating the epochs of adjacent
!              states in the input state array. STEP is specified
!              in seconds.
!
! $ Detailed_Output
!
!     None. See $Particulars for a description of the effect of this
!     routine.
!
! $ Parameters
!
!     See the include file spk12.inc for declarations of the
!     parameters described below.
!
!
!     TOLSCL   is a tolerance scale factor (also called a
!              "relative tolerance") used for time coverage
!              bound checking. TOLSCL is unitless. TOLSCL
!              produces a tolerance value via the formula
!
!                 TOL = TOLSCL * MAX( ABS(FIRST), ABS(LAST) )
!
!              where FIRST and LAST are the coverage time bounds
!              of a type 12 segment, expressed as seconds past
!              J2000 TDB.
!
!              The resulting parameter TOL is used as a tolerance
!              for comparing the input segment descriptor time
!              bounds to the first and last epoch covered by the
!              sequence of time intervals defined by the inputs
!              to SPKW12:
!
!                 BEGTIM
!                 STEP
!                 N
!
!
!     MAXDEG   is the maximum degree of Hermite polynomials that
!              can be used to interpolate states from the segment
!              written by this routine.
!
! $ Exceptions
!
!     If any of the following exceptions occur, this routine will return
!     without creating a new segment.
!
!     1)  If FRAME is not a recognized name, the error
!         SPICE(INVALIDREFFRAME) is signaled.
!
!     2)  If the last non-blank character of SEGID occurs past index 40,
!         the error SPICE(SEGIDTOOLONG) is signaled.
!
!     3)  If SEGID contains any nonprintable characters, the error
!         SPICE(NONPRINTABLECHARS) is signaled.
!
!     4)  If DEGREE is not at least 1 or is greater than MAXDEG, the
!         error SPICE(INVALIDDEGREE) is signaled.
!
!     5)  If DEGREE is not odd, the error SPICE(INVALIDDEGREE) is
!         signaled.
!
!     6)  If the number of states N is not at least (DEGREE+1)/2, the
!         error SPICE(TOOFEWSTATES) is signaled.
!
!     7)  If FIRST is greater than LAST, the error
!         SPICE(BADDESCRTIMES) is signaled.
!
!     8)  If STEP is non-positive, the error SPICE(INVALIDSTEPSIZE) is
!         signaled.
!
!     9)  If the first epoch BEGTIM is greater than FIRST, the error
!         SPICE(BADDESCRTIMES) is signaled.
!
!     10) If the start time of the first record exceeds the descriptor
!         begin time by more than a computed tolerance, or if the end
!         time of the last record precedes the descriptor end time by
!         more than a computed tolerance, the error SPICE(COVERAGEGAP)
!         is signaled. See the $Parameters section for a description of
!         the tolerance.
!
! $ Files
!
!     A new type 12 SPK segment is written to the SPK file attached
!     to HANDLE.
!
! $ Particulars
!
!     This routine writes an SPK type 12 data segment to the open SPK
!     file according to the format described in the type 12 section of
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
!     1) This example demonstrates how to create an SPK type 12 kernel
!        containing only one segment, given an evenly-spaced
!        time-ordered set of discrete states.
!
!
!        Example code begins here.
!
!
!              PROGRAM SPKW12_EX1
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
!              CHARACTER*(*)         SPK12
!              PARAMETER           ( SPK12 = 'spkw12_ex1.bsp' )
!
!              CHARACTER*(*)         REF
!              PARAMETER           ( REF    = 'J2000'          )
!
!              INTEGER               BODY
!              PARAMETER           ( BODY   = 3  )
!
!              INTEGER               CENTER
!              PARAMETER           ( CENTER = 10 )
!
!              INTEGER               DEGREE
!              PARAMETER           ( DEGREE = 3  )
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
!              DOUBLE PRECISION      FIRST
!              DOUBLE PRECISION      LAST
!              DOUBLE PRECISION      STATES ( 6, NSTATS )
!              DOUBLE PRECISION      STEP
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
!        C
!        C     Set the start and end times of interval covered by
!        C     segment, and the time step.
!        C
!              FIRST  = 100.D0
!              LAST   = 900.D0
!              STEP   = 100.D0
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
!              IFNAME = 'Type 12 SPK internal file name.'
!              SEGID  = 'SPK type 12 test segment'
!
!        C
!        C     Open a new SPK file.
!        C
!              CALL SPKOPN( SPK12, IFNAME, NCOMCH, HANDLE )
!
!        C
!        C     Write the segment.
!        C
!              CALL SPKW12 (  HANDLE, BODY,   CENTER, REF,
!             .               FIRST,  LAST,   SEGID,  DEGREE,
!             .               NSTATS, STATES, FIRST,  STEP   )
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
!        screen. After run completion, a new SPK type 12 exists in
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!
! $ Version
!
!-    SPICELIB Version 2.1.0, 03-JUN-2021 (JDR)
!
!        Changed the input argument name EPOCH1 to BEGTIM for
!        consistency with other routines.
!
!        Edited the header to comply with NAIF standard. Added
!        complete example code from existing fragment.
!
!-    SPICELIB Version 2.0.0, 18-JAN-2013 (NJB)
!
!        Relaxed test on relationship between the time bounds of the
!        input record set (determined by BEGTIM, STEP, and N) and the
!        descriptor bounds FIRST and LAST. Now the descriptor bounds
!        may extend beyond the time bounds of the record set by a ratio
!        computed using the parameter TOLSCL (see $Parameters above for
!        details). MAXDEG was increased to 27.
!
!        Corrected long error message for even degree case.
!
!-    SPICELIB Version 1.0.0, 25-FEB-2000 (NJB)
!
!-&


! $ Index_Entries
!
!     write SPK type_12 ephemeris data segment
!
!-&


!
!     SPICELIB functions
!
      logical               even
      logical               failed
      logical               return

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
      integer               sidlen
      parameter           ( sidlen  =  40 )

      integer               ns
      parameter           ( ns      =   5 )

      integer               dtype
      parameter           ( dtype   =  12 )

      integer               fprint
      parameter           ( fprint  =  32 )

      integer               lprint
      parameter           ( lprint  = 126 )

      integer               statsz
      parameter           ( statsz  =   6 )

      integer               timlen
      parameter           ( timlen  =  40 )

!
!     Local variables
!
      character*(timlen)    etstr

      double precision      descr  ( ns )
      double precision      ltime
      double precision      tol

      integer               chrcod
      integer               i
      integer               lastnb
      integer               refcod
      integer               winsiz

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'SPKW12' )
      end if

!
!     Set the window size corresponding to the input degree.  This
!     size will be used in various places below.
!
      winsiz  =  ( degree + 1 ) / 2

!
!     Get the NAIF integer code for the reference frame.
!
      call namfrm ( frame, refcod )

      if ( refcod .eq. 0 ) then

         call setmsg ( 'The reference frame # is not supported.'   )
         call errch  ( '#', frame                                  )
         call sigerr ( 'SPICE(INVALIDREFFRAME)'                    )
         call chkout ( 'SPKW12'                                    )
         return

      end if

!
!     Check to see if the segment identifier is too long.
!
      if ( lastnb(segid) .gt. sidlen ) then

         call setmsg ( 'Segment identifier contains more than ' // &
                       '40 characters.'                          )
         call sigerr ( 'SPICE(SEGIDTOOLONG)'                     )
         call chkout ( 'SPKW12'                                  )
         return

      end if

!
!     Now check that all the characters in the segment identifier
!     can be printed.
!
      do i = 1, lastnb(segid)

         chrcod = ichar( segid(i:i) )

         if ( ( chrcod .lt. fprint ) .or. ( chrcod .gt. lprint ) ) then

            call setmsg ( 'The segment identifier contains '  // &
                          'nonprintable characters'               )
            call sigerr ( 'SPICE(NONPRINTABLECHARS)'              )
            call chkout ( 'SPKW12'                                )
            return

         end if

      end do

!
!     Make sure that the degree of the interpolating polynomials is
!     in range.
!
      if (  ( degree .lt. 1 ) .or. ( degree .gt. maxdeg )  )  then

         call setmsg ( 'The interpolating polynomials have degree #; '// &
                       'the valid degree range is [1, #]'              )
         call errint ( '#', degree                                     )
         call errint ( '#', maxdeg                                     )
         call sigerr ( 'SPICE(INVALIDDEGREE)'                          )
         call chkout ( 'SPKW12'                                        )
         return

      end if

!
!     Make sure that the degree of the interpolating polynomials is odd.
!
      if (  even( degree )  )  then

         call setmsg ( 'The interpolating polynomials have degree #; '// &
                       'for SPK type 12, the degree must be odd.'      )
         call errint ( '#', degree                                     )
         call sigerr ( 'SPICE(INVALIDDEGREE)'                          )
         call chkout ( 'SPKW12'                                        )
         return

      end if

!
!     Make sure that the number of states is sufficient to define a
!     polynomial whose degree is DEGREE.
!
      if ( n .lt. winsiz ) then

         call setmsg ( 'At least # states are required to define a '  // &
                       'Hermite polynomial of degree #.  Number of '  // &
                       'states supplied:  #'                           )
         call errint ( '#', winsiz                                     )
         call errint ( '#', degree                                     )
         call errint ( '#', n                                          )
         call sigerr ( 'SPICE(TOOFEWSTATES)'                           )
         call chkout ( 'SPKW12'                                        )
         return

      end if

!
!     The segment stop time should be greater then the begin time.
!
      if ( first .ge. last ) then

         call setmsg ( 'The segment start time: # is greater then ' // &
                       'the segment end time: #'                   )
         call errdp  ( '#', first                                  )
         call errdp  ( '#', last                                   )
         call sigerr ( 'SPICE(BADDESCRTIMES)'                      )
         call chkout ( 'SPKW12'                                    )
         return

      end if

!
!     The step size must be positive.
!
      if ( step .le. 0.d0 ) then

         call setmsg ( 'The step size must be > 0 but was #. ' )
         call errdp  ( '#', step                               )
         call sigerr ( 'SPICE(INVALIDSTEPSIZE)'                )
         call chkout ( 'SPKW12'                                )
         return

      end if

!
!     Compute the tolerance to use for descriptor time bound checks.
!
      tol = tolscl * max( abs(first), abs(last) )

      if ( first .lt. begtim - tol ) then

         call setmsg ( 'The segment descriptor start time # is too ' &
         //            'much less than the beginning time of the  ' &
         //            'segment data # (in seconds past J2000: #). ' &
         //            'The difference is # seconds; the  ' &
         //            'tolerance is # seconds.'                     )
         call etcal  ( first,  etstr                                 )
         call errch  ( '#',    etstr                                 )
         call etcal  ( begtim, etstr                                 )
         call errch  ( '#',    etstr                                 )
         call errdp  ( '#',    first                                 )
         call errdp  ( '#',    begtim-first                          )
         call errdp  ( '#',    tol                                   )
         call sigerr ( 'SPICE(COVERAGEGAP)'                          )
         call chkout ( 'SPKW12'                                      )
         return

      end if

!
!     The end time of the final record must be greater than or
!     equal to the end time of the segment.
!
      ltime = begtim  +  ( (n-1) * step )

      if ( last .gt. ltime + tol ) then

         call setmsg ( 'The segment descriptor end time # is too ' &
         //            'much greater than the end time of the segment ' &
         //            'data # (in seconds past J2000: #). The ' &
         //            'difference is # seconds; the tolerance is # ' &
         //            'seconds.'                                    )
         call etcal  ( last,  etstr                                  )
         call errch  ( '#',   etstr                                  )
         call etcal  ( ltime, etstr                                  )
         call errch  ( '#',   etstr                                  )
         call errdp  ( '#',   last                                   )
         call errdp  ( '#',   last-ltime                             )
         call errdp  ( '#',   tol                                    )
         call sigerr ( 'SPICE(COVERAGEGAP)'                          )
         call chkout ( 'SPKW12'                                      )
         return

      end if

!
!     If we made it this far, we're ready to start writing the segment.
!
!
!     Create the segment descriptor.
!
      call spkpds ( body, center, frame, dtype, first, last, descr )

!
!     Begin a new segment.
!
      call dafbna ( handle, descr, segid )

      if ( failed() ) then
         call chkout ( 'SPKW12' )
         return
      end if

!
!     The type 12 segment structure is eloquently described by this
!     diagram from the SPK Required Reading:
!
!        +-----------------------+
!        | State 1               |
!        +-----------------------+
!        | State 2               |
!        +-----------------------+
!                    .
!                    .
!                    .
!        +-----------------------+
!        | State N               |
!        +-----------------------+
!        | Epoch of state 1 (ET) |
!        +-----------------------+
!        | Step size             |
!        +-----------------------+
!        | Window size - 1       |
!        +-----------------------+
!        | Number of states      |
!        +-----------------------+
!
!
      call dafada ( states,              n*statsz )
      call dafada ( begtim,              1        )
      call dafada ( step,                1        )
      call dafada ( dble( winsiz - 1 ),  1        )
      call dafada ( dble( n          ),  1        )

!
!     As long as nothing went wrong, end the segment.
!
      if ( .not. failed() ) then
         call dafena
      end if

      call chkout ( 'SPKW12' )
      return
      end
