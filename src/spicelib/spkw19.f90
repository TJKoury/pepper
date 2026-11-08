!$Procedure SPKW19 ( Write SPK segment, type 19 )

      subroutine spkw19 ( handle,  body,    center,  frame, &
                          first,   last,    segid,   nintvl, &
                          npkts,   subtps,  degres,  packts, &
                          epochs,  ivlbds,  sellst          )

! $ Abstract
!
!     Write a type 19 segment to an SPK file.
!
! $ Required_Reading
!
!     DAF
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

      include 'spk19.inc'
      include 'spkrec.inc'


      integer               handle
      integer               body
      integer               center
      character*(*)         frame
      double precision      first
      double precision      last
      character*(*)         segid
      integer               nintvl
      integer               npkts  ( * )
      integer               subtps ( * )
      integer               degres ( * )
      double precision      packts ( * )
      double precision      epochs ( * )
      double precision      ivlbds ( * )
      logical               sellst

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of an SPK file open for writing.
!     BODY       I   NAIF ID code for an ephemeris object.
!     CENTER     I   NAIF ID code for center of motion of BODY.
!     FRAME      I   Reference frame name.
!     FIRST      I   Start time of interval covered by segment.
!     LAST       I   End time of interval covered by segment.
!     SEGID      I   Segment identifier.
!     NINTVL     I   Number of mini-segments and interpolation
!                    intervals.
!     NPKTS      I   Array of packet counts of mini-segments.
!     SUBTPS     I   Array of segment subtypes of mini-segments.
!     DEGRES     I   Array of polynomial degrees of mini-segments.
!     PACKTS     I   Array of data packets of mini-segments.
!     EPOCHS     I   Array of epochs of mini-segments.
!     IVLBDS     I   Interpolation interval bounds.
!     SELLST     I   Interval selection flag.
!     MAXDEG     P   Maximum allowed degree of interpolating polynomial.
!
! $ Detailed_Input
!
!     HANDLE   is the handle of an SPK file that has been opened
!              for writing.
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
!     LAST     are, respectively, the bounds of the time interval
!              over which the segment defines the state of BODY.
!
!              FIRST must be greater than or equal to the first
!              interpolation interval start time; LAST must be
!              less than or equal to the last interpolation
!              interval stop time. See the description of IVLBDS
!              below.
!
!     SEGID    is the segment identifier. An SPK segment
!              identifier may contain up to 40 characters.
!
!     NINTVL   is the number of interpolation intervals
!              associated with the input data. The interpolation
!              intervals are associated with data sets referred
!              to as "mini-segments."
!
!              The input data comprising each mini-segment are:
!
!                 - a packet count
!                 - a type 19 subtype
!                 - an interpolating polynomial degree
!                 - a sequence of type 19 data packets
!                 - a sequence of packet epochs
!
!              These inputs are described below.
!
!     NPKTS    is an array of packet counts. The Ith element of
!              NPKTS is the packet count of the Ith interpolation
!              interval/mini-segment.
!
!              NPKTS has dimension NINTVL.
!
!     SUBTPS   is an array of type 19 subtypes. The Ith element
!              of SUBTPS is the subtype of the packets associated
!              with the Ith interpolation interval/mini-segment.
!
!              SUBTPS has dimension NINTVL.
!
!     DEGRES   is an array of interpolating polynomial degrees.
!              The Ith element of DEGRES is the polynomial degree
!              of the packets associated with the Ith
!              interpolation interval/mini-segment.
!
!              For subtype 0, interpolation degrees must be
!              equivalent to 3 mod 4, that is, they must be in
!              the set
!
!                 { 3, 7, 11, ..., MAXDEG }
!
!              For subtype 1, interpolation degrees must be odd
!              and must be in the range 1:MAXDEG.
!
!              DEGRES has dimension NINTVL.
!
!     PACKTS   is an array containing data packets for all input
!              mini-segments. The packets for a given
!              mini-segment are stored contiguously in increasing
!              time order. The order of the sets of packets for
!              different mini-segments is the same as the order
!              of their corresponding interpolation intervals.
!
!              Each packet represents geometric states of BODY
!              relative to CENTER, specified relative to FRAME.
!              The packet structure depends on the segment
!              subtype as follows:
!
!                 Type 0 (indicated by code S19TP0):
!
!                     x,  y,  z,  dx/dt,  dy/dt,  dz/dt,
!                     vx, vy, vz, dvx/dt, dvy/dt, dvz/dt
!
!                 where x, y, z represent Cartesian position
!                 components and  vx, vy, vz represent Cartesian
!                 velocity components. Note well: vx, vy, and
!                 vz *are not necessarily equal* to the time
!                 derivatives of x, y, and z. This packet
!                 structure mimics that of the Rosetta/MEX orbit
!                 file.
!
!                 Type 1 (indicated by code S19TP1):
!
!                     x,  y,  z,  dx/dt,  dy/dt,  dz/dt
!
!                 where x, y, z represent Cartesian position
!                 components and  vx, vy, vz represent Cartesian
!                 velocity components.
!
!
!                 Type 2 (indicated by code S19TP2):
!
!                     Data are identical to type 1; only the
!                     interpolation algorithm is different.
!
!              Position units are kilometers, velocity units
!              are kilometers per second, and acceleration units
!              are kilometers per second per second.
!
!     EPOCHS   is an array containing epochs for all input
!              mini-segments. Each epoch is expressed as seconds
!              past J2000 TDB. The epochs have a one-to-one
!              relationship with the packets in the input packet
!              array.
!
!              The epochs for a given mini-segment are stored
!              contiguously in increasing order. The order of the
!              sets of epochs for different mini-segments is the
!              same as the order of their corresponding
!              interpolation intervals.
!
!              For each mini-segment, "padding" is allowed: the
!              sequence of epochs for that mini-segment may start
!              before the corresponding interpolation interval
!              start time and end after the corresponding
!              interpolation interval stop time. Padding is used
!              to control behavior of interpolating polynomials
!              near interpolation interval boundaries.
!
!              Due to possible use of padding, the elements of
!              EPOCHS, taken as a whole, may not be in increasing
!              order.
!
!     IVLBDS   is an array of interpolation interval boundary
!              times. This array is an ordered list of the
!              interpolation interval start times, to which the
!              the end time for the last interval is appended.
!
!              The Ith interpolation interval is the time
!              coverage interval of the Ith mini-segment (see the
!              description of NPKTS above).
!
!              For each mini-segment, the corresponding
!              interpolation interval's start time is greater
!              than or equal to the mini-segment's first epoch,
!              and the interval's stop time is less than or equal
!              to the mini-segment's last epoch.
!
!              For each interpolation interval other than the
!              last, the interval's coverage stop time coincides
!              with the coverage start time of the next interval.
!              There are no coverage gaps, and coverage overlap
!              for adjacent intervals consists of a single epoch.
!
!              IVLBDS has dimension NINTVL+1.
!
!     SELLST   is a logical flag indicating to the SPK type 19
!              segment reader SPKR19 how to select the
!              interpolation interval when a request time
!              coincides with a time boundary shared by two
!              interpolation intervals. When SELLST ("select
!              last") is .TRUE., the later interval is selected;
!              otherwise the earlier interval is selected.
!
! $ Detailed_Output
!
!     None. See $Particulars for a description of the effect of this
!     routine.
!
! $ Parameters
!
!     MAXDEG   is the maximum allowed degree of the interpolating
!              polynomial.
!
!              See the INCLUDE file spk19.inc for the value of
!              MAXDEG.
!
! $ Exceptions
!
!     If any of the following exceptions occur, this routine will
!     return without creating a new segment.
!
!     1)  If FIRST is greater than LAST, the error
!         SPICE(BADDESCRTIMES) is signaled.
!
!     2)  If FRAME is not a recognized name, the error
!         SPICE(INVALIDREFFRAME) is signaled.
!
!     3)  If the last non-blank character of SEGID occurs past index
!         40, the error SPICE(SEGIDTOOLONG) is signaled.
!
!     4)  If SEGID contains any nonprintable characters, the error
!         SPICE(NONPRINTABLECHARS) is signaled.
!
!     5)  If NINTVL is not at least 1, the error SPICE(INVALIDCOUNT)
!         is signaled.
!
!     6)  If the elements of the array IVLBDS are not in strictly
!         increasing order, the error SPICE(BOUNDSOUTOFORDER) is
!         signaled.
!
!     7)  If the first interval start time IVLBDS(1) is greater than
!         FIRST, or if the last interval end time IVLBDS(N+1) is less
!         than LAST, the error SPICE(COVERAGEGAP) is signaled.
!
!     8)  If any packet count in the array NPKTS is not at least 2, the
!         error SPICE(TOOFEWPACKETS) is signaled.
!
!     9)  If any subtype code in the array SUBTPS is not recognized,
!         the error SPICE(INVALIDSUBTYPE) is signaled.
!
!     10) If any interpolation degree in the array DEGRES
!         is not at least 1 or is greater than MAXDEG, the
!         error SPICE(INVALIDDEGREE) is signaled.
!
!     11) If the window size implied by any element of the array DEGRES
!         is odd, the error SPICE(BADWINDOWSIZE) is signaled.
!
!     12) If the elements of the array EPOCHS corresponding to a given
!         mini-segment are not in strictly increasing order, the error
!         SPICE(TIMESOUTOFORDER) is signaled.
!
!     13) If the first epoch of a mini-segment exceeds the start
!         time of the associated interpolation interval, or if the
!         last epoch of the mini-segment precedes the end time of the
!         interpolation interval, the error SPICE(BOUNDSDISAGREE)
!         is signaled.
!
!     14) If an error occurs while writing the output segment, the error
!         is signaled by a routine in the call tree of this routine.
!
! $ Files
!
!     A new type 19 SPK segment is written to the SPK file attached
!     to HANDLE.
!
! $ Particulars
!
!     This routine writes an SPK type 19 data segment to the open SPK
!     file according to the format described in the type 19 section of
!     the SPK Required Reading. The SPK file must have been opened with
!     write access.
!
! $ Examples
!
!     Suppose that you have states and are prepared to produce
!     a segment of type 19 in an SPK file.
!
!     The following code fragment could be used to add the new segment
!     to a previously opened SPK file attached to HANDLE. The file must
!     have been opened with write access.
!
!        C
!        C     Create a segment identifier.
!        C
!                  SEGID = 'MY_SAMPLE_SPK_TYPE_19_SEGMENT'
!
!        C
!        C     Write the segment.
!        C
!              CALL SPKW19 ( HANDLE,  BODY,    CENTER,  FRAME,
!             .              FIRST,   LAST,    SEGID,   NINTVL,
!             .              NPKTS,   SUBTPS,  DEGRES,  PACKTS,
!             .              EPOCHS,  IVLBDS,  SELLST           )
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
!
! $ Version
!
!-    SPICELIB Version 2.0.1, 03-JUN-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 2.0.0, 21-DEC-2015 (NJB)
!
!        Updated to support subtype 2.
!
!-    SPICELIB Version 1.0.0, 05-FEB-2014 (NJB) (BVS)
!
!-&


! $ Index_Entries
!
!     write SPK type_19 ephemeris data segment
!
!-&


!
!     SPICELIB functions
!
      integer               lastnb

      logical               failed
      logical               odd
      logical               return

!
!     Local parameters
!
      integer               sidlen
      parameter           ( sidlen =  40 )

      integer               fprint
      parameter           ( fprint =  32 )

      integer               lprint
      parameter           ( lprint = 126 )

      integer               nd
      parameter           ( nd     =   2 )

      integer               ni
      parameter           ( ni     =   6 )

      integer               dscsiz
      parameter           ( dscsiz =   5 )

      integer               dtype
      parameter           ( dtype  =  19 )

      integer               dirsiz
      parameter           ( dirsiz = 100 )

!
!     Local variables
!
      double precision      dc     ( nd )
      double precision      descr  ( dscsiz )


      integer               bepix
      integer               chrcod
      integer               eepix
      integer               i
      integer               ic     ( ni )
      integer               isel
      integer               j
      integer               k
      integer               minisz
      integer               ndir
      integer               pktbeg
      integer               pktdsz
      integer               pktend
      integer               pktsiz
      integer               pktszs ( 0 : s19nst-1 )
      integer               refcod
      integer               segbeg
      integer               segend
      integer               subtyp
      integer               winsiz

!
!     Saved values
!
      save                  pktszs

!
!     Initial values
!
      data                  pktszs  /  s19ps0, s19ps1, s19ps2 /

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

      call chkin ( 'SPKW19' )

!
!     Start with a parameter compatibility check.
!
      if ( maxrec .lt. maxrsz ) then

         call setmsg ( 'SPK type 19 record size may be as ' &
         //            'large as #, but SPKPVN record size ' &
         //            'is #.'                              )
         call errint ( '#',  maxrsz                         )
         call errint ( '#',  maxrec                         )
         call sigerr ( 'SPICE(BUG0)'                        )
         call chkout ( 'SPKW19'                             )
         return

      end if

!
!     Make sure the segment descriptor bounds are
!     correctly ordered.
!
      if ( last .lt. first ) then

         call setmsg ( 'Segment start time is #; stop time is #; ' &
         //            'bounds must be in nondecreasing order.'    )
         call errdp  ( '#', first                                  )
         call errdp  ( '#', last                                   )
         call sigerr ( 'SPICE(BADDESCRTIMES)'                      )
         call chkout ( 'SPKW19'                                    )
         return

      end if

!
!     Get the NAIF integer code for the reference frame.
!
      call namfrm ( frame, refcod )

      if ( refcod .eq. 0 ) then

         call setmsg ( 'The reference frame # is not supported.'   )
         call errch  ( '#', frame                                  )
         call sigerr ( 'SPICE(INVALIDREFFRAME)'                    )
         call chkout ( 'SPKW19'                                  )
         return

      end if

!
!     Check to see if the segment identifier is too long.
!
      if ( lastnb(segid) .gt. sidlen ) then

         call setmsg ( 'Segment identifier contains more than ' &
         //            '40 characters.'                        )
         call sigerr ( 'SPICE(SEGIDTOOLONG)'                   )
         call chkout ( 'SPKW19'                                )
         return

      end if

!
!     Now check that all the characters in the segment identifier
!     can be printed.
!
      do i = 1, lastnb(segid)

         chrcod = ichar( segid(i:i) )

         if ( ( chrcod .lt. fprint ) .or. ( chrcod .gt. lprint ) ) then

            call setmsg ( 'The segment identifier contains ' &
            //            'nonprintable characters'         )
            call sigerr ( 'SPICE(NONPRINTABLECHARS)'        )
            call chkout ( 'SPKW19'                          )
            return

         end if

      end do

!
!     The mini-segment/interval count must be positive.
!
      if ( nintvl .lt. 1 ) then

         call setmsg ( 'Mini-segment/interval count was #; ' &
         //            'this count must be positive.'       )
         call errint ( '#', nintvl                          )
         call sigerr ( 'SPICE(INVALIDCOUNT)'                )
         call chkout ( 'SPKW19'                             )
         return

      end if

!
!     Make sure the interval bounds form a strictly
!     increasing sequence.
!
!     Note that there are NINTVL+1 bounds.
!
      do i = 1, nintvl

         if ( ivlbds(i) .ge. ivlbds(i+1) ) then

            call setmsg ( 'Interval bounds at indices # and # ' &
            //            'are # and # respectively. The ' &
            //            'difference is #. The bounds are ' &
            //            'required to be strictly increasing.' )
            call errint ( '#', i                                )
            call errint ( '#', i+1                              )
            call errdp  ( '#', ivlbds(i)                        )
            call errdp  ( '#', ivlbds(i+1)                      )
            call errdp  ( '#', ivlbds(i+1) - ivlbds(i)          )
            call sigerr ( 'SPICE(BOUNDSOUTOFORDER)'             )
            call chkout ( 'SPKW19'                              )
            return

         end if

      end do

!
!     Make sure the time span of the descriptor doesn't extend
!     beyond the span of the interval bounds.
!
      if (      ( first .lt. ivlbds(1       ) ) &
           .or. ( last  .gt. ivlbds(nintvl+1) ) ) then

         call setmsg ( 'First interval start time is #; ' &
         //            'segment start time is #; segment ' &
         //            'stop time is #; last interval stop ' &
         //            'time is #. This sequence of times ' &
         //            'is required to be non-decreasing: ' &
         //            'segment coverage must be contained ' &
         //            'within the union of the interpolation ' &
         //            'intervals.'                            )
         call errdp  ( '#', ivlbds(1)                          )
         call errdp  ( '#', first                              )
         call errdp  ( '#', last                               )
         call errdp  ( '#', ivlbds(nintvl+1)                   )
         call sigerr ( 'SPICE(COVERAGEGAP)'                    )
         call chkout ( 'SPKW19'                                )
         return

      end if

!
!     Check the input data before writing to the file.
!
!     This order of operations entails some redundant
!     calculations, but it allows for rapid error
!     detection.
!
!     Initialize the mini-segment packet array indices,
!     and those of the mini-segment epoch array as well.
!
      pktbeg = 0
      pktend = 0

      bepix  = 0
      eepix  = 0

      do i = 1, nintvl
!
!        First, just make sure the packet count for the current
!        mini-segment is at least two. This check reduces our chances
!        of a subscript range violation.
!
!        Check the number of packets.
!
         if ( npkts(i) .lt. 2 ) then

            call setmsg ( 'At least 2 packets are required ' &
            //            'for SPK type 19. Number of packets ' &
            //            'supplied was # in mini-segment at index #.' )
            call errint ( '#', npkts(i)                                )
            call errint ( '#', i                                       )
            call sigerr ( 'SPICE(TOOFEWPACKETS)'                       )
            call chkout ( 'SPKW19'                                     )
            return

         end if

!
!        Set the packet size, which is a function of the subtype. Also
!        set the window size. First check the subtype, which will be
!        used as an array index.
!
         subtyp = subtps(i)

         if (  ( subtyp .lt. 0 ) .or. ( subtyp .gt. s19nst-1 ) ) then

            call setmsg ( 'Unexpected SPK type 19 subtype # ' &
            //            'found in mini-segment #.'          )
            call errint ( '#',  subtyp                        )
            call errint ( '#',  i                             )
            call sigerr ( 'SPICE(INVALIDSUBTYPE)'             )
            call chkout ( 'SPKW19'                            )
            return

         end if

         pktsiz = pktszs ( subtyp )

         if ( subtyp .eq. s19tp0 ) then

            winsiz = ( degres(i) + 1 ) / 2

         else if ( subtyp .eq. s19tp1 ) then

            winsiz =   degres(i) + 1

         else if ( subtyp .eq. s19tp2 ) then

            winsiz = ( degres(i) + 1 ) / 2

         else
            call setmsg ( 'Subtype = #; not expected.' )
            call errint ( '#', subtyp                  )
            call sigerr ( 'SPICE(BUG1)'                )
         end if

!
!        Make sure that the degree of the interpolating polynomials is
!        in range.
!
         if (      ( degres(i) .lt. 1      ) &
              .or. ( degres(i) .gt. maxdeg )  )  then

            call setmsg ( 'The interpolating polynomials of ' &
            //            'mini-segment # have degree #; the ' &
            //            'valid degree range is [1, #]'      )
            call errint ( '#', i                              )
            call errint ( '#', degres(i)                      )
            call errint ( '#', maxdeg                         )
            call sigerr ( 'SPICE(INVALIDDEGREE)'              )
            call chkout ( 'SPKW19'                            )
            return

         end if

!
!        Make sure that the window size is even.
!
         if (   odd( winsiz )   )  then

            call setmsg ( 'The interpolating polynomials of ' &
            //            'mini-segment # have window size # ' &
            //            'and degree # for SPK type 19. ' &
            //            'The mini-segment subtype is #. ' &
            //            'The degree must be equivalent to 3 ' &
            //            'mod 4 for subtype 0 (Hermite ' &
            //            'interpolation) and be odd for subtype ' &
            //            '1 (Lagrange interpolation).'          )
            call errint ( '#', i                                 )
            call errint ( '#', winsiz                            )
            call errint ( '#', degres(i)                         )
            call errint ( '#', subtps(i)                         )
            call sigerr ( 'SPICE(BADWINDOWSIZE)'                 )
            call chkout ( 'SPKW19'                               )
            return

         end if


!
!        Make sure the epochs of the Ith mini-segment form a
!        strictly increasing sequence.
!
!        To start out, determine the indices of the epoch sequence
!        of the Ith mini-segment. We'll call the begin and end
!        epoch indices BEPIX and EEPIX respectively.
!
         bepix  =  eepix + 1
         eepix  =  bepix - 1 +  npkts(i)

         do j = 1, npkts(i) - 1

            k = bepix + j - 1

            if ( epochs(k) .ge. epochs(k+1) ) then

               call setmsg ( 'In mini-segment #, epoch # having ' &
               //            'index # in array EPOCHS and index # ' &
               //            'in the mini-segment is greater than ' &
               //            'or equal to its successor #.'         )
               call errint ( '#',  i                                )
               call errdp  ( '#',  epochs(k  )                      )
               call errint ( '#',  k                                )
               call errint ( '#',  j                                )
               call errdp  ( '#',  epochs(k+1)                      )
               call sigerr ( 'SPICE(TIMESOUTOFORDER)'               )
               call chkout ( 'SPKW19'                               )
               return

            end if

         end do

!
!        Make sure that the span of the input epochs of the Ith
!        mini-segment includes the Ith interpolation interval.
!
         if (  epochs(bepix) .gt. ivlbds(i) ) then

            call setmsg ( 'Interpolation interval # start time # ' &
            //            'precedes mini-segment''s first epoch #.' )
            call errint ( '#',  i                                   )
            call errdp  ( '#',  ivlbds(i)                           )
            call errdp  ( '#',  epochs(bepix)                       )
            call sigerr ( 'SPICE(BOUNDSDISAGREE)'                   )
            call chkout ( 'SPKW19'                                  )
            return

         else if (  epochs(eepix) .lt. ivlbds(i+1) ) then

            call setmsg ( 'Interpolation interval # end time # ' &
            //            'exceeds mini-segment''s last epoch #.'   )
            call errint ( '#',  i                                   )
            call errdp  ( '#',  ivlbds(i+1)                         )
            call errdp  ( '#',  epochs(eepix)                       )
            call sigerr ( 'SPICE(BOUNDSDISAGREE)'                   )
            call chkout ( 'SPKW19'                                  )
            return

         end if

      end do

!
!     If we made it this far, we're ready to start writing the segment.
!
!     The type 19 segment structure is eloquently described by this
!     diagram from the SPK Required Reading:
!
!        +--------------------------------+
!        | Interval 1 mini-segment        |
!        +--------------------------------+
!              .
!              .
!              .
!        +--------------------------------+
!        | Interval N mini-segment        |
!        +--------------------------------+
!        | Interval 1 start time          |
!        +--------------------------------+
!              .
!              .
!              .
!        +--------------------------------+
!        | Interval N start time          |
!        +--------------------------------+
!        | Interval N stop time           |
!        +--------------------------------+
!        | Interval start 100             | (First interval directory)
!        +--------------------------------+
!              .
!              .
!              .
!        +--------------------------------+
!        | Interval start (N/100)*100     | (Last interval directory)
!        +--------------------------------+
!        | Interval 1 start pointer       |
!        +--------------------------------+
!              .
!              .
!              .
!        +--------------------------------+
!        | Interval N start pointer       |
!        +--------------------------------+
!        | Interval N stop pointer + 1    |
!        +--------------------------------+
!        | Boundary choice flag           |
!        +--------------------------------+
!        | Number of intervals            |
!        +--------------------------------+
!
!
!     SPK type 19 mini-segments have the following structure:
!
!        +-----------------------+
!        | Packet 1              |
!        +-----------------------+
!                    .
!                    .
!                    .
!        +-----------------------+
!        | Packet M              |
!        +-----------------------+
!        | Epoch 1               |
!        +-----------------------+
!                    .
!                    .
!                    .
!        +-----------------------+
!        | Epoch M               |
!        +-----------------------+
!        | Epoch 100             | (First time tag directory)
!        +-----------------------+
!                    .
!                    .
!                    .
!        +-----------------------+
!        | Epoch ((M-1)/100)*100 | (Last time tag directory)
!        +-----------------------+
!        | Subtype code          |
!        +-----------------------+
!        | Window size           |
!        +-----------------------+
!        | Number of packets     |
!        +-----------------------+
!
!
!     Create the segment descriptor. We don't use SPKPDS because
!     that routine doesn't allow creation of a singleton segment.
!
      ic(1) = body
      ic(2) = center
      ic(3) = refcod
      ic(4) = dtype

      dc(1) = first
      dc(2) = last

      call dafps  ( nd, ni, dc, ic, descr )

!
!     Begin a new segment.
!
      call dafbna ( handle, descr, segid )

      if ( failed() ) then
         call chkout ( 'SPKW19' )
         return
      end if

!
!     Re-initialize the mini-segment packet array indices,
!     and those of the mini-segment epoch array as well.
!
      pktbeg = 0
      pktend = 0

      bepix  = 0
      eepix  = 0

!
!     Write data for each mini-segment to the file.
!
      do i = 1, nintvl
!
!        Set the packet size, which is a function of the subtype.
!
         subtyp = subtps ( i )

         pktsiz = pktszs ( subtyp )

         if ( subtyp .eq. s19tp0 ) then

            winsiz = ( degres(i) + 1 ) / 2

         else if ( subtyp .eq. s19tp1 ) then

            winsiz =   degres(i) + 1

         else if ( subtyp .eq. s19tp2 ) then

            winsiz = ( degres(i) + 1 ) / 2

         else
            call setmsg ( 'Subtype = #; not expected.' )
            call errint ( '#', subtyp                  )
            call sigerr ( 'SPICE(BUG2)'                )
         end if

!
!        Now that we have the packet size, we can compute
!        mini-segment packet index range. We'll let PKTDSZ
!        be the total count of packet data entries for this
!        mini-segment.
!
         pktdsz =  npkts(i) * pktsiz

         pktbeg = pktend  +  1
         pktend = pktbeg  -  1  +  pktdsz

!
!        At this point, we're read to start writing the
!        current mini-segment to the file. Start with the
!        packet data.
!
         call dafada ( packts(pktbeg), pktdsz )

!
!        Write the epochs for this mini-segment.
!
         bepix = eepix + 1
         eepix = bepix - 1 + npkts(i)

         call dafada ( epochs(bepix), npkts(i) )

!
!        Compute the number of epoch directories for the
!        current mini-segment.
!
         ndir = ( npkts(i) - 1 ) / dirsiz

!
!        Write the epoch directories to the segment.
!
         do j = 1, ndir

            k = bepix  -  1  +  ( j * dirsiz )

            call dafada ( epochs(k), 1 )

         end do

!
!        Write the mini-segment's subtype, window size, and packet
!        count to the segment.
!
         call dafada ( dble( subtps(i) ),  1 )
         call dafada ( dble( winsiz    ),  1 )
         call dafada ( dble( npkts(i)  ),  1 )

         if ( failed() ) then
            call chkout ( 'SPKW19' )
            return
         end if

      end do

!
!     We've finished writing the mini-segments.
!
!     Next write the interpolation interval bounds.
!
      call dafada ( ivlbds, nintvl+1 )

!
!     Create and write directories for the interval
!     bounds.
!
!     The directory count is the interval bound count
!     (N+1), minus 1, divided by the directory size.
!
      ndir = nintvl / dirsiz

      do i = 1, ndir
         call dafada (  ivlbds( dirsiz*i ),  1  )
      end do

!
!     Now we compute and write the start/stop pointers
!     for each mini-segment.
!
!     The pointers are relative to the DAF address
!     preceding the segment. For example, a pointer
!     to the first DAF address in the segment has
!     value 1.
!
      segend = 0

      do i = 1, nintvl
!
!        Set the packet size, which is a function of the subtype.
!
         pktsiz = pktszs ( subtps(i) )

!
!        In order to compute the end pointer of the current
!        mini-segment, we must compute the size, in terms
!        of DAF addresses, of this mini-segment. The formula
!        for the size is
!
!            size =     n_packets * packet_size
!                    +  n_epochs
!                    +  n_epoch_directories
!                    +  3
!
!                 =     n_packets * ( packet_size + 1 )
!                    +  ( n_packets - 1 ) / DIRSIZ
!                    +  3
!
         minisz =       npkts(i) * ( pktsiz + 1 ) &
                    + ( npkts(i) - 1 ) / dirsiz &
                    +   3


         segbeg = segend + 1
         segend = segbeg + minisz - 1

!
!        Write the mini-segment begin pointer.
!
!        After the loop terminates, the final end pointer, incremented
!        by 1, will be written.
!
         call dafada ( dble(segbeg), 1 )

      end do

!
!     Write the last mini-segment end pointer, incremented by one.
!     SEGEND was computed on the last iteration of the above loop.
!
      call dafada ( dble(segend+1), 1 )

!
!     Write out the interval selection flag. The input
!     boolean value is represented by a numeric constant.
!
      if ( sellst ) then

         isel = itrue
      else
         isel = ifalse
      end if

      call dafada ( dble( isel ),  1 )

!
!     Write the mini-segment/interpolation interval count.
!
      call dafada ( dble(nintvl), 1 )

!
!     End the segment.
!
      call dafena

      call chkout ( 'SPKW19' )
      return
      end
