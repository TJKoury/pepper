!$Procedure SPKW19 ( Write SPK segment, type 19 )

      SUBROUTINE SPKW19 ( HANDLE,  BODY,    CENTER,  FRAME, &
                          FIRST,   LAST,    SEGID,   NINTVL, &
                          NPKTS,   SUBTPS,  DEGRES,  PACKTS, &
                          EPOCHS,  IVLBDS,  SELLST          )

! $ Abstract
!
!     Write a type 19 segment to an SPK file.
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

      IMPLICIT NONE

      INCLUDE 'spk19.inc'
      INCLUDE 'spkrec.inc'


      INTEGER               HANDLE
      INTEGER               BODY
      INTEGER               CENTER
      CHARACTER*(*)         FRAME
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      LAST
      CHARACTER*(*)         SEGID
      INTEGER               NINTVL
      INTEGER               NPKTS  ( * )
      INTEGER               SUBTPS ( * )
      INTEGER               DEGRES ( * )
      DOUBLE PRECISION      PACKTS ( * )
      DOUBLE PRECISION      EPOCHS ( * )
      DOUBLE PRECISION      IVLBDS ( * )
      LOGICAL               SELLST

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
      INTEGER               LASTNB

      LOGICAL               FAILED
      LOGICAL               ODD
      LOGICAL               RETURN

!
!     Local parameters
!
      INTEGER               SIDLEN
      PARAMETER           ( SIDLEN =  40 )

      INTEGER               FPRINT
      PARAMETER           ( FPRINT =  32 )

      INTEGER               LPRINT
      PARAMETER           ( LPRINT = 126 )

      INTEGER               ND
      PARAMETER           ( ND     =   2 )

      INTEGER               NI
      PARAMETER           ( NI     =   6 )

      INTEGER               DSCSIZ
      PARAMETER           ( DSCSIZ =   5 )

      INTEGER               DTYPE
      PARAMETER           ( DTYPE  =  19 )

      INTEGER               DIRSIZ
      PARAMETER           ( DIRSIZ = 100 )

!
!     Local variables
!
      DOUBLE PRECISION      DC     ( ND )
      DOUBLE PRECISION      DESCR  ( DSCSIZ )


      INTEGER               BEPIX
      INTEGER               CHRCOD
      INTEGER               EEPIX
      INTEGER               I
      INTEGER               IC     ( NI )
      INTEGER               ISEL
      INTEGER               J
      INTEGER               K
      INTEGER               MINISZ
      INTEGER               NDIR
      INTEGER               PKTBEG
      INTEGER               PKTDSZ
      INTEGER               PKTEND
      INTEGER               PKTSIZ
      INTEGER               PKTSZS ( 0 : S19NST-1 )
      INTEGER               REFCOD
      INTEGER               SEGBEG
      INTEGER               SEGEND
      INTEGER               SUBTYP
      INTEGER               WINSIZ

!
!     Saved values
!
      SAVE                  PKTSZS

!
!     Initial values
!
      DATA                  PKTSZS  /  S19PS0, S19PS1, S19PS2 /

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'SPKW19' )

!
!     Start with a parameter compatibility check.
!
      IF ( MAXREC .LT. MAXRSZ ) THEN

         CALL SETMSG ( 'SPK type 19 record size may be as ' &
         //            'large as #, but SPKPVN record size ' &
         //            'is #.'                              )
         CALL ERRINT ( '#',  MAXRSZ                         )
         CALL ERRINT ( '#',  MAXREC                         )
         CALL SIGERR ( 'SPICE(BUG0)'                        )
         CALL CHKOUT ( 'SPKW19'                             )
         RETURN

      END IF

!
!     Make sure the segment descriptor bounds are
!     correctly ordered.
!
      IF ( LAST .LT. FIRST ) THEN

         CALL SETMSG ( 'Segment start time is #; stop time is #; ' &
         //            'bounds must be in nondecreasing order.'    )
         CALL ERRDP  ( '#', FIRST                                  )
         CALL ERRDP  ( '#', LAST                                   )
         CALL SIGERR ( 'SPICE(BADDESCRTIMES)'                      )
         CALL CHKOUT ( 'SPKW19'                                    )
         RETURN

      END IF

!
!     Get the NAIF integer code for the reference frame.
!
      CALL NAMFRM ( FRAME, REFCOD )

      IF ( REFCOD .EQ. 0 ) THEN

         CALL SETMSG ( 'The reference frame # is not supported.'   )
         CALL ERRCH  ( '#', FRAME                                  )
         CALL SIGERR ( 'SPICE(INVALIDREFFRAME)'                    )
         CALL CHKOUT ( 'SPKW19'                                  )
         RETURN

      END IF

!
!     Check to see if the segment identifier is too long.
!
      IF ( LASTNB(SEGID) .GT. SIDLEN ) THEN

         CALL SETMSG ( 'Segment identifier contains more than ' &
         //            '40 characters.'                        )
         CALL SIGERR ( 'SPICE(SEGIDTOOLONG)'                   )
         CALL CHKOUT ( 'SPKW19'                                )
         RETURN

      END IF

!
!     Now check that all the characters in the segment identifier
!     can be printed.
!
      DO I = 1, LASTNB(SEGID)

         CHRCOD = ICHAR( SEGID(I:I) )

         IF ( ( CHRCOD .LT. FPRINT ) .OR. ( CHRCOD .GT. LPRINT ) ) THEN

            CALL SETMSG ( 'The segment identifier contains ' &
            //            'nonprintable characters'         )
            CALL SIGERR ( 'SPICE(NONPRINTABLECHARS)'        )
            CALL CHKOUT ( 'SPKW19'                          )
            RETURN

         END IF

      END DO

!
!     The mini-segment/interval count must be positive.
!
      IF ( NINTVL .LT. 1 ) THEN

         CALL SETMSG ( 'Mini-segment/interval count was #; ' &
         //            'this count must be positive.'       )
         CALL ERRINT ( '#', NINTVL                          )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                )
         CALL CHKOUT ( 'SPKW19'                             )
         RETURN

      END IF

!
!     Make sure the interval bounds form a strictly
!     increasing sequence.
!
!     Note that there are NINTVL+1 bounds.
!
      DO I = 1, NINTVL

         IF ( IVLBDS(I) .GE. IVLBDS(I+1) ) THEN

            CALL SETMSG ( 'Interval bounds at indices # and # ' &
            //            'are # and # respectively. The ' &
            //            'difference is #. The bounds are ' &
            //            'required to be strictly increasing.' )
            CALL ERRINT ( '#', I                                )
            CALL ERRINT ( '#', I+1                              )
            CALL ERRDP  ( '#', IVLBDS(I)                        )
            CALL ERRDP  ( '#', IVLBDS(I+1)                      )
            CALL ERRDP  ( '#', IVLBDS(I+1) - IVLBDS(I)          )
            CALL SIGERR ( 'SPICE(BOUNDSOUTOFORDER)'             )
            CALL CHKOUT ( 'SPKW19'                              )
            RETURN

         END IF

      END DO

!
!     Make sure the time span of the descriptor doesn't extend
!     beyond the span of the interval bounds.
!
      IF (      ( FIRST .LT. IVLBDS(1       ) ) &
           .OR. ( LAST  .GT. IVLBDS(NINTVL+1) ) ) THEN

         CALL SETMSG ( 'First interval start time is #; ' &
         //            'segment start time is #; segment ' &
         //            'stop time is #; last interval stop ' &
         //            'time is #. This sequence of times ' &
         //            'is required to be non-decreasing: ' &
         //            'segment coverage must be contained ' &
         //            'within the union of the interpolation ' &
         //            'intervals.'                            )
         CALL ERRDP  ( '#', IVLBDS(1)                          )
         CALL ERRDP  ( '#', FIRST                              )
         CALL ERRDP  ( '#', LAST                               )
         CALL ERRDP  ( '#', IVLBDS(NINTVL+1)                   )
         CALL SIGERR ( 'SPICE(COVERAGEGAP)'                    )
         CALL CHKOUT ( 'SPKW19'                                )
         RETURN

      END IF

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
      PKTBEG = 0
      PKTEND = 0

      BEPIX  = 0
      EEPIX  = 0

      DO I = 1, NINTVL
!
!        First, just make sure the packet count for the current
!        mini-segment is at least two. This check reduces our chances
!        of a subscript range violation.
!
!        Check the number of packets.
!
         IF ( NPKTS(I) .LT. 2 ) THEN

            CALL SETMSG ( 'At least 2 packets are required ' &
            //            'for SPK type 19. Number of packets ' &
            //            'supplied was # in mini-segment at index #.' )
            CALL ERRINT ( '#', NPKTS(I)                                )
            CALL ERRINT ( '#', I                                       )
            CALL SIGERR ( 'SPICE(TOOFEWPACKETS)'                       )
            CALL CHKOUT ( 'SPKW19'                                     )
            RETURN

         END IF

!
!        Set the packet size, which is a function of the subtype. Also
!        set the window size. First check the subtype, which will be
!        used as an array index.
!
         SUBTYP = SUBTPS(I)

         IF (  ( SUBTYP .LT. 0 ) .OR. ( SUBTYP .GT. S19NST-1 ) ) THEN

            CALL SETMSG ( 'Unexpected SPK type 19 subtype # ' &
            //            'found in mini-segment #.'          )
            CALL ERRINT ( '#',  SUBTYP                        )
            CALL ERRINT ( '#',  I                             )
            CALL SIGERR ( 'SPICE(INVALIDSUBTYPE)'             )
            CALL CHKOUT ( 'SPKW19'                            )
            RETURN

         END IF

         PKTSIZ = PKTSZS ( SUBTYP )

         IF ( SUBTYP .EQ. S19TP0 ) THEN

            WINSIZ = ( DEGRES(I) + 1 ) / 2

         ELSE IF ( SUBTYP .EQ. S19TP1 ) THEN

            WINSIZ =   DEGRES(I) + 1

         ELSE IF ( SUBTYP .EQ. S19TP2 ) THEN

            WINSIZ = ( DEGRES(I) + 1 ) / 2

         ELSE
            CALL SETMSG ( 'Subtype = #; not expected.' )
            CALL ERRINT ( '#', SUBTYP                  )
            CALL SIGERR ( 'SPICE(BUG1)'                )
         END IF

!
!        Make sure that the degree of the interpolating polynomials is
!        in range.
!
         IF (      ( DEGRES(I) .LT. 1      ) &
              .OR. ( DEGRES(I) .GT. MAXDEG )  )  THEN

            CALL SETMSG ( 'The interpolating polynomials of ' &
            //            'mini-segment # have degree #; the ' &
            //            'valid degree range is [1, #]'      )
            CALL ERRINT ( '#', I                              )
            CALL ERRINT ( '#', DEGRES(I)                      )
            CALL ERRINT ( '#', MAXDEG                         )
            CALL SIGERR ( 'SPICE(INVALIDDEGREE)'              )
            CALL CHKOUT ( 'SPKW19'                            )
            RETURN

         END IF

!
!        Make sure that the window size is even.
!
         IF (   ODD( WINSIZ )   )  THEN

            CALL SETMSG ( 'The interpolating polynomials of ' &
            //            'mini-segment # have window size # ' &
            //            'and degree # for SPK type 19. ' &
            //            'The mini-segment subtype is #. ' &
            //            'The degree must be equivalent to 3 ' &
            //            'mod 4 for subtype 0 (Hermite ' &
            //            'interpolation) and be odd for subtype ' &
            //            '1 (Lagrange interpolation).'          )
            CALL ERRINT ( '#', I                                 )
            CALL ERRINT ( '#', WINSIZ                            )
            CALL ERRINT ( '#', DEGRES(I)                         )
            CALL ERRINT ( '#', SUBTPS(I)                         )
            CALL SIGERR ( 'SPICE(BADWINDOWSIZE)'                 )
            CALL CHKOUT ( 'SPKW19'                               )
            RETURN

         END IF


!
!        Make sure the epochs of the Ith mini-segment form a
!        strictly increasing sequence.
!
!        To start out, determine the indices of the epoch sequence
!        of the Ith mini-segment. We'll call the begin and end
!        epoch indices BEPIX and EEPIX respectively.
!
         BEPIX  =  EEPIX + 1
         EEPIX  =  BEPIX - 1 +  NPKTS(I)

         DO J = 1, NPKTS(I) - 1

            K = BEPIX + J - 1

            IF ( EPOCHS(K) .GE. EPOCHS(K+1) ) THEN

               CALL SETMSG ( 'In mini-segment #, epoch # having ' &
               //            'index # in array EPOCHS and index # ' &
               //            'in the mini-segment is greater than ' &
               //            'or equal to its successor #.'         )
               CALL ERRINT ( '#',  I                                )
               CALL ERRDP  ( '#',  EPOCHS(K  )                      )
               CALL ERRINT ( '#',  K                                )
               CALL ERRINT ( '#',  J                                )
               CALL ERRDP  ( '#',  EPOCHS(K+1)                      )
               CALL SIGERR ( 'SPICE(TIMESOUTOFORDER)'               )
               CALL CHKOUT ( 'SPKW19'                               )
               RETURN

            END IF

         END DO

!
!        Make sure that the span of the input epochs of the Ith
!        mini-segment includes the Ith interpolation interval.
!
         IF (  EPOCHS(BEPIX) .GT. IVLBDS(I) ) THEN

            CALL SETMSG ( 'Interpolation interval # start time # ' &
            //            'precedes mini-segment''s first epoch #.' )
            CALL ERRINT ( '#',  I                                   )
            CALL ERRDP  ( '#',  IVLBDS(I)                           )
            CALL ERRDP  ( '#',  EPOCHS(BEPIX)                       )
            CALL SIGERR ( 'SPICE(BOUNDSDISAGREE)'                   )
            CALL CHKOUT ( 'SPKW19'                                  )
            RETURN

         ELSE IF (  EPOCHS(EEPIX) .LT. IVLBDS(I+1) ) THEN

            CALL SETMSG ( 'Interpolation interval # end time # ' &
            //            'exceeds mini-segment''s last epoch #.'   )
            CALL ERRINT ( '#',  I                                   )
            CALL ERRDP  ( '#',  IVLBDS(I+1)                         )
            CALL ERRDP  ( '#',  EPOCHS(EEPIX)                       )
            CALL SIGERR ( 'SPICE(BOUNDSDISAGREE)'                   )
            CALL CHKOUT ( 'SPKW19'                                  )
            RETURN

         END IF

      END DO

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
      IC(1) = BODY
      IC(2) = CENTER
      IC(3) = REFCOD
      IC(4) = DTYPE

      DC(1) = FIRST
      DC(2) = LAST

      CALL DAFPS  ( ND, NI, DC, IC, DESCR )

!
!     Begin a new segment.
!
      CALL DAFBNA ( HANDLE, DESCR, SEGID )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SPKW19' )
         RETURN
      END IF

!
!     Re-initialize the mini-segment packet array indices,
!     and those of the mini-segment epoch array as well.
!
      PKTBEG = 0
      PKTEND = 0

      BEPIX  = 0
      EEPIX  = 0

!
!     Write data for each mini-segment to the file.
!
      DO I = 1, NINTVL
!
!        Set the packet size, which is a function of the subtype.
!
         SUBTYP = SUBTPS ( I )

         PKTSIZ = PKTSZS ( SUBTYP )

         IF ( SUBTYP .EQ. S19TP0 ) THEN

            WINSIZ = ( DEGRES(I) + 1 ) / 2

         ELSE IF ( SUBTYP .EQ. S19TP1 ) THEN

            WINSIZ =   DEGRES(I) + 1

         ELSE IF ( SUBTYP .EQ. S19TP2 ) THEN

            WINSIZ = ( DEGRES(I) + 1 ) / 2

         ELSE
            CALL SETMSG ( 'Subtype = #; not expected.' )
            CALL ERRINT ( '#', SUBTYP                  )
            CALL SIGERR ( 'SPICE(BUG2)'                )
         END IF

!
!        Now that we have the packet size, we can compute
!        mini-segment packet index range. We'll let PKTDSZ
!        be the total count of packet data entries for this
!        mini-segment.
!
         PKTDSZ =  NPKTS(I) * PKTSIZ

         PKTBEG = PKTEND  +  1
         PKTEND = PKTBEG  -  1  +  PKTDSZ

!
!        At this point, we're read to start writing the
!        current mini-segment to the file. Start with the
!        packet data.
!
         CALL DAFADA ( PACKTS(PKTBEG), PKTDSZ )

!
!        Write the epochs for this mini-segment.
!
         BEPIX = EEPIX + 1
         EEPIX = BEPIX - 1 + NPKTS(I)

         CALL DAFADA ( EPOCHS(BEPIX), NPKTS(I) )

!
!        Compute the number of epoch directories for the
!        current mini-segment.
!
         NDIR = ( NPKTS(I) - 1 ) / DIRSIZ

!
!        Write the epoch directories to the segment.
!
         DO J = 1, NDIR

            K = BEPIX  -  1  +  ( J * DIRSIZ )

            CALL DAFADA ( EPOCHS(K), 1 )

         END DO

!
!        Write the mini-segment's subtype, window size, and packet
!        count to the segment.
!
         CALL DAFADA ( DBLE( SUBTPS(I) ),  1 )
         CALL DAFADA ( DBLE( WINSIZ    ),  1 )
         CALL DAFADA ( DBLE( NPKTS(I)  ),  1 )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SPKW19' )
            RETURN
         END IF

      END DO

!
!     We've finished writing the mini-segments.
!
!     Next write the interpolation interval bounds.
!
      CALL DAFADA ( IVLBDS, NINTVL+1 )

!
!     Create and write directories for the interval
!     bounds.
!
!     The directory count is the interval bound count
!     (N+1), minus 1, divided by the directory size.
!
      NDIR = NINTVL / DIRSIZ

      DO I = 1, NDIR
         CALL DAFADA (  IVLBDS( DIRSIZ*I ),  1  )
      END DO

!
!     Now we compute and write the start/stop pointers
!     for each mini-segment.
!
!     The pointers are relative to the DAF address
!     preceding the segment. For example, a pointer
!     to the first DAF address in the segment has
!     value 1.
!
      SEGEND = 0

      DO I = 1, NINTVL
!
!        Set the packet size, which is a function of the subtype.
!
         PKTSIZ = PKTSZS ( SUBTPS(I) )

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
         MINISZ =       NPKTS(I) * ( PKTSIZ + 1 ) &
                    + ( NPKTS(I) - 1 ) / DIRSIZ &
                    +   3


         SEGBEG = SEGEND + 1
         SEGEND = SEGBEG + MINISZ - 1

!
!        Write the mini-segment begin pointer.
!
!        After the loop terminates, the final end pointer, incremented
!        by 1, will be written.
!
         CALL DAFADA ( DBLE(SEGBEG), 1 )

      END DO

!
!     Write the last mini-segment end pointer, incremented by one.
!     SEGEND was computed on the last iteration of the above loop.
!
      CALL DAFADA ( DBLE(SEGEND+1), 1 )

!
!     Write out the interval selection flag. The input
!     boolean value is represented by a numeric constant.
!
      IF ( SELLST ) THEN

         ISEL = ITRUE
      ELSE
         ISEL = IFALSE
      END IF

      CALL DAFADA ( DBLE( ISEL ),  1 )

!
!     Write the mini-segment/interpolation interval count.
!
      CALL DAFADA ( DBLE(NINTVL), 1 )

!
!     End the segment.
!
      CALL DAFENA

      CALL CHKOUT ( 'SPKW19' )
      RETURN
      END
