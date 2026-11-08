!$Procedure SPKW18 ( Write SPK segment, type 18 )

      subroutine spkw18 (  handle,  subtyp,  body,    center, &
                           frame,   first,   last,    segid, &
                           degree,  n,       packts,  epochs  )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Write a type 18 segment to an SPK file.
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

      include 'spk18.inc'

      integer(ip)           handle
      integer(ip)           subtyp
      integer(ip)           body
      integer(ip)           center
      character(*)          frame
      real(wp)              first
      real(wp)              last
      character(*)          segid
      integer(ip)           degree
      integer(ip)           n
      real(wp)              packts ( * )
      real(wp)              epochs ( * )

      integer(ip)           maxdeg
      parameter           ( maxdeg = 15 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of an SPK file open for writing.
!     SUBTYP     I   SPK type 18 subtype code.
!     BODY       I   NAIF code for an ephemeris object.
!     CENTER     I   NAIF code for center of motion of BODY.
!     FRAME      I   Reference frame name.
!     FIRST      I   Start time of interval covered by segment.
!     LAST       I   End time of interval covered by segment.
!     SEGID      I   Segment identifier.
!     DEGREE     I   Degree of interpolating polynomials.
!     N          I   Number of packets.
!     PACKTS     I   Array of packets.
!     EPOCHS     I   Array of epochs corresponding to packets.
!     MAXDEG     P   Maximum allowed degree of interpolating polynomial.
!
! $ Detailed_Input
!
!     HANDLE   is the file handle of an SPK file that has been opened
!              for writing.
!
!     SUBTYP   is an integer code indicating the subtype of the
!              segment to be created.
!
!     BODY     is the NAIF integer code for an ephemeris object whose
!              state relative to another body is described by the
!              segment to be created.
!
!     CENTER   is the NAIF integer code for the center of motion of the
!              object identified by BODY.
!
!     FRAME    is the NAIF name for a reference frame relative to which
!              the state information for BODY is specified.
!
!     FIRST,
!     LAST     are, respectively, the start and stop times of the time
!              interval over which the segment defines the state of
!              BODY.
!
!     SEGID    is the segment identifier. An SPK segment identifier may
!              contain up to 40 characters.
!
!     DEGREE   is the nominal degree of the polynomials used to
!              interpolate the states contained in the input packets.
!              All components of the state vectors are interpolated by
!              polynomials of the specified degree, except near the
!              segment boundaries, or if the total number of states in
!              the segment is too few to allow interpolation using the
!              specified degree.
!
!              If the actual interpolation degree is reduced, the
!              highest degree feasible degree valid for the
!              interpolation type is used.
!
!     N        is the number of packets in the input packet array.
!
!     PACKTS   is a time-ordered array of data packets representing
!              geometric states of BODY relative to CENTER, specified
!              relative to FRAME. The packet structure depends on the
!              segment subtype as follows:
!
!                 Type 0 (indicated by code S18TP0):
!
!                    x,  y,  z,  dx/dt,  dy/dt,  dz/dt,
!                    vx, vy, vz, dvx/dt, dvy/dt, dvz/dt
!
!                 where x, y, z represent Cartesian position components
!                 and  vx, vy, vz represent Cartesian velocity
!                 components. Note well: vx, vy, and vz *are not
!                 necessarily equal* to the time derivatives of x, y,
!                 and z. This packet structure mimics that of the
!                 Rosetta/MEX orbit file from which the data are taken.
!
!                 Type 1 (indicated by code S18TP1):
!
!                    x,  y,  z,  dx/dt,  dy/dt,  dz/dt
!
!                 where x, y, z represent Cartesian position components
!                 and  vx, vy, vz represent Cartesian velocity
!                 components.
!
!              Position units are kilometers, velocity units are
!              kilometers per second, and acceleration units are
!              kilometers per second per second.
!
!     EPOCHS   is an array of epochs corresponding to the members of the
!              packets array. The epochs are specified as seconds past
!              J2000, TDB.
!
! $ Detailed_Output
!
!     None. See $Particulars for a description of the effect of this
!     routine.
!
! $ Parameters
!
!     MAXDEG   is the maximum allowed degree of the interpolating
!              polynomial. If the value of MAXDEG is increased, the
!              SPICELIB routine SPKPVN must be changed accordingly. In
!              particular, the size of the record passed to SPKRnn and
!              SPKEnn must be increased, and comments describing the
!              record size must be changed.
!
! $ Exceptions
!
!     If any of the following exceptions occur, this routine will
!     return without creating a new segment.
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
!     5)  If the window size implied by DEGREE is odd, the error
!         SPICE(INVALIDDEGREE) is signaled.
!
!     6)  If the number of packets N is not at least 2, the error
!         SPICE(TOOFEWSTATES) is signaled.
!
!     7)  If FIRST is greater than or equal to LAST, the error
!         SPICE(BADDESCRTIMES) is signaled.
!
!     8)  If the elements of the array EPOCHS are not in strictly
!         increasing order, the error SPICE(TIMESOUTOFORDER) is
!         signaled.
!
!     9)  If the first epoch EPOCHS(1) is greater than FIRST, the error
!         SPICE(BADDESCRTIMES) is signaled.
!
!     10) If the last epoch EPOCHS(N) is less than LAST, the error
!         SPICE(BADDESCRTIMES) is signaled.
!
!     11) If the subtype code is not recognized, the error
!         SPICE(INVALIDVALUE) is signaled.
!
! $ Files
!
!     A new type 18 SPK segment is written to the SPK file attached
!     to HANDLE.
!
! $ Particulars
!
!     This routine writes an SPK type 18 data segment to the open SPK
!     file according to the format described in the type 18 section of
!     the SPK Required Reading. The SPK file must have been opened with
!     write access.
!
! $ Examples
!
!     Suppose that you have states and are prepared to produce
!     a segment of type 18 in an SPK file.
!
!     The following code fragment could be used to add the new segment
!     to a previously opened SPK file attached to HANDLE. The file must
!     have been opened with write access.
!
!        C
!        C     Create a segment identifier.
!        C
!                  SEGID = 'MY_SAMPLE_SPK_TYPE_18_SEGMENT'
!
!        C
!        C     Write the segment.
!        C
!              CALL SPKW18 (  HANDLE,  BODY,    CENTER,  FRAME,
!             .               FIRST,   LAST,    SEGID,   DEGREE,
!             .               N,       STATES,  EPOCHS          )
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
!-    SPICELIB Version 1.1.1, 09-APR-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.1.0, 21-DEC-2012 (NJB)
!
!        Increased the minimum packet count from 1 to 2.
!
!-    SPICELIB Version 1.0.1, 29-APR-2003 (NJB)
!
!        Description of error condition arising from invalid window
!        size was corrected.
!
!-    SPICELIB Version 1.0.0, 13-MAY-2002 (NJB)
!
!-&


! $ Index_Entries
!
!     write SPK type_18 ephemeris data segment
!
!-&


!
!     SPICELIB functions
!
      integer(ip)           lastnb

      logical               failed
      logical               odd
      logical               return

!
!     Local parameters
!
      integer(ip)           sidlen
      parameter           ( sidlen  =  40 )

      integer(ip)           fprint
      parameter           ( fprint  =  32 )

      integer(ip)           lprint
      parameter           ( lprint  = 126 )

      integer(ip)           nd
      parameter           ( nd      =   2 )

      integer(ip)           ni
      parameter           ( ni      =   6 )

      integer(ip)           dscsiz
      parameter           ( dscsiz  =   5 )

      integer(ip)           typidx
      parameter           ( typidx  =   4 )

      integer(ip)           dtype
      parameter           ( dtype   =  18 )

      integer(ip)           dirsiz
      parameter           ( dirsiz  = 100 )

      integer(ip)           statsz
      parameter           ( statsz  =   6 )

!
!     Local variables
!
      real(wp)              dc     ( nd )
      real(wp)              descr  ( dscsiz )
      real(wp)              maxtim

      integer(ip)           chrcod
      integer(ip)           i
      integer(ip)           ic     ( ni )
      integer(ip)           packsz
      integer(ip)           refcod
      integer(ip)           winsiz

!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     Set the packet size, which is a function of the subtype.
!
      if ( subtyp == s18tp0 ) then

         packsz = s18ps0

      else if ( subtyp == s18tp1 ) then

         packsz = s18ps1

      else

         call setmsg ( 'Unexpected SPK type 18 subtype requested: #' )
         call errint ( '#',  subtyp                                  )
         call sigerr ( 'SPICE(INVALIDVALUE)'                         )
         return

      end if


!
!     Set the window size corresponding to the input degree.  This
!     size will be used in various places below.
!
      if ( subtyp == s18tp0 ) then

         winsiz  =  ( degree + 1 ) / 2

      else if ( subtyp == s18tp1 ) then

         winsiz  =  degree + 1

      else

         call setmsg ( 'This point should not be reached. Getting ' // &
                       'here may indicate that the code needs to '  // &
                       'updated to handle new subtypes.'            )
         call sigerr ( 'SPICE(BUG)'                                 )
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
         return

      end if

!
!     Check to see if the segment identifier is too long.
!
      if ( lastnb(segid) > sidlen ) then

         call setmsg ( 'Segment identifier contains more than ' // &
                       '40 characters.'                          )
         call sigerr ( 'SPICE(SEGIDTOOLONG)'                     )
         return

      end if

!
!     Now check that all the characters in the segment identifier
!     can be printed.
!
      do i = 1, lastnb(segid)

         chrcod = ichar( segid(i:i) )

         if ( ( chrcod < fprint ) .or. ( chrcod > lprint ) ) then

            call setmsg ( 'The segment identifier contains '  // &
                          'nonprintable characters'               )
            call sigerr ( 'SPICE(NONPRINTABLECHARS)'              )
            return

         end if

      end do

!
!     Make sure that the degree of the interpolating polynomials is
!     in range.
!
      if (  ( degree < 1 ) .or. ( degree > maxdeg )  )  then

         call setmsg ( 'The interpolating polynomials have degree #; '// &
                       'the valid degree range is [1, #]'              )
         call errint ( '#', degree                                     )
         call errint ( '#', maxdeg                                     )
         call sigerr ( 'SPICE(INVALIDDEGREE)'                          )
         return

      end if

!
!     Make sure that the window size is even.  If not, the input
!     DEGREE is incompatible with the subtype.
!
      if (  odd(winsiz)  )  then

         call setmsg ( 'The interpolating polynomials have degree #; '// &
                       'for SPK type 18, the degree must be '         // &
                       'equivalent to 3 mod 4 for Hermite '           // &
                       'interpolation and odd for for Lagrange '      // &
                       'interpolation.'                                )
         call errint ( '#', degree                                     )
         call sigerr ( 'SPICE(INVALIDDEGREE)'                          )
         return

      end if

!
!     Make sure that the number of packets is sufficient to define a
!     polynomial whose degree is DEGREE.
!
      if ( n < 2 ) then

         call setmsg ( 'At least 2 packets are required for SPK type '// &
                       '18.  Number of packets supplied:  #'           )
         call errint ( '#', n                                          )
         call sigerr ( 'SPICE(TOOFEWSTATES)'                           )
         return

      end if

!
!     The segment stop time should be greater than or equal to
!     the begin time.
!
      if ( first > last ) then

         call setmsg ( 'The segment start time: # is greater then ' // &
                       'the segment end time: #'                   )
         call errdp  ( '#', first                                  )
         call errdp  ( '#', last                                   )
         call sigerr ( 'SPICE(BADDESCRTIMES)'                      )
         return

      end if

!
!     Make sure the epochs form a strictly increasing sequence.
!
      maxtim = epochs(1)

      do i = 2, n

         if ( epochs(i) <= maxtim ) then

            call setmsg ( 'EPOCH # having index # is not greater '    // &
                          'than its predecessor #.'                   )
            call errdp  ( '#',  epochs(i)                             )
            call errint ( '#',  i                                     )
            call errdp  ( '#',  epochs(i-1)                           )
            call sigerr ( 'SPICE(TIMESOUTOFORDER)'                    )
            return
         else
            maxtim = epochs(i)
         end if

      end do

!
!     Make sure that the span of the input epochs includes the interval
!     defined by the segment descriptor.
!
      if (  epochs(1) > first ) then

         call setmsg ( 'Segment start time # precedes first epoch #.' )
         call errdp  ( '#',  first                                    )
         call errdp  ( '#',  epochs(1)                                )
         call sigerr ( 'SPICE(BADDESCRTIMES)'                         )
         return

      else if (  epochs(n) < last ) then

         call setmsg ( 'Segment end time # follows last epoch #.'     )
         call errdp  ( '#',  last                                     )
         call errdp  ( '#',  epochs(n)                                )
         call sigerr ( 'SPICE(BADDESCRTIMES)'                         )
         return

      end if



!
!     If we made it this far, we're ready to start writing the segment.
!
!
!     Create the segment descriptor.  We don't use SPKPDS because
!     that routine doesn't allow creation of a singleton segment.
!
      ic(1) = body
      ic(2) = center

      call namfrm ( frame, ic(3) )

      if ( failed() ) return

      ic(4) = dtype

      dc(1) = first
      dc(2) = last

      call dafps  ( nd, ni, dc, ic, descr )

!
!     Begin a new segment.
!
      call dafbna ( handle, descr, segid )

      if ( failed() ) return


!
!     The type 18 segment structure is eloquently described by this
!     diagram from the SPK Required Reading:
!
!        +-----------------------+
!        | Packet 1              |
!        +-----------------------+
!        | Packet 2              |
!        +-----------------------+
!                    .
!                    .
!                    .
!        +-----------------------+
!        | Packet N              |
!        +-----------------------+
!        | Epoch 1               |
!        +-----------------------+
!        | Epoch 2               |
!        +-----------------------+
!                    .
!                    .
!                    .
!        +-----------------------+
!        | Epoch N               |
!        +-----------------------+
!        | Epoch 100             | (First directory)
!        +-----------------------+
!                    .
!                    .
!                    .
!        +-----------------------+
!        | Epoch ((N-1)/100)*100 | (Last directory)
!        +-----------------------+
!        | Subtype code          |
!        +-----------------------+
!        | Window size           |
!        +-----------------------+
!        | Number of packets     |
!        +-----------------------+
!
!
      call dafada ( packts,  n*packsz )
      call dafada ( epochs,  n        )

      do i = 1,   (n-1) / dirsiz
         call dafada (  epochs( dirsiz*i ),  1  )
      end do

      call dafada ( real( subtyp, wp ),  1 )
      call dafada ( real( winsiz, wp ),  1 )
      call dafada ( real( n, wp      ),  1 )

!
!     As long as nothing went wrong, end the segment.
!
      if ( .not. failed() ) then
         call dafena
      end if

      return
      end
