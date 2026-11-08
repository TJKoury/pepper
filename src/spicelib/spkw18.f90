!$Procedure SPKW18 ( Write SPK segment, type 18 )

      SUBROUTINE SPKW18 (  HANDLE,  SUBTYP,  BODY,    CENTER, &
                           FRAME,   FIRST,   LAST,    SEGID, &
                           DEGREE,  N,       PACKTS,  EPOCHS  )

! $ Abstract
!
!     Write a type 18 segment to an SPK file.
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

      INCLUDE 'spk18.inc'

      INTEGER               HANDLE
      INTEGER               SUBTYP
      INTEGER               BODY
      INTEGER               CENTER
      CHARACTER*(*)         FRAME
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      LAST
      CHARACTER*(*)         SEGID
      INTEGER               DEGREE
      INTEGER               N
      DOUBLE PRECISION      PACKTS ( * )
      DOUBLE PRECISION      EPOCHS ( * )

      INTEGER               MAXDEG
      PARAMETER           ( MAXDEG = 15 )

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
      INTEGER               LASTNB

      LOGICAL               FAILED
      LOGICAL               ODD
      LOGICAL               RETURN

!
!     Local parameters
!
      INTEGER               SIDLEN
      PARAMETER           ( SIDLEN  =  40 )

      INTEGER               FPRINT
      PARAMETER           ( FPRINT  =  32 )

      INTEGER               LPRINT
      PARAMETER           ( LPRINT  = 126 )

      INTEGER               ND
      PARAMETER           ( ND      =   2 )

      INTEGER               NI
      PARAMETER           ( NI      =   6 )

      INTEGER               DSCSIZ
      PARAMETER           ( DSCSIZ  =   5 )

      INTEGER               TYPIDX
      PARAMETER           ( TYPIDX  =   4 )

      INTEGER               DTYPE
      PARAMETER           ( DTYPE   =  18 )

      INTEGER               DIRSIZ
      PARAMETER           ( DIRSIZ  = 100 )

      INTEGER               STATSZ
      PARAMETER           ( STATSZ  =   6 )

!
!     Local variables
!
      DOUBLE PRECISION      DC     ( ND )
      DOUBLE PRECISION      DESCR  ( DSCSIZ )
      DOUBLE PRECISION      MAXTIM

      INTEGER               CHRCOD
      INTEGER               I
      INTEGER               IC     ( NI )
      INTEGER               PACKSZ
      INTEGER               REFCOD
      INTEGER               WINSIZ

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SPKW18' )
      END IF

!
!     Set the packet size, which is a function of the subtype.
!
      IF ( SUBTYP .EQ. S18TP0 ) THEN

         PACKSZ = S18PS0

      ELSE IF ( SUBTYP .EQ. S18TP1 ) THEN

         PACKSZ = S18PS1

      ELSE

         CALL SETMSG ( 'Unexpected SPK type 18 subtype requested: #' )
         CALL ERRINT ( '#',  SUBTYP                                  )
         CALL SIGERR ( 'SPICE(INVALIDVALUE)'                         )
         CALL CHKOUT ( 'SPKW18'                                      )
         RETURN

      END IF


!
!     Set the window size corresponding to the input degree.  This
!     size will be used in various places below.
!
      IF ( SUBTYP .EQ. S18TP0 ) THEN

         WINSIZ  =  ( DEGREE + 1 ) / 2

      ELSE IF ( SUBTYP .EQ. S18TP1 ) THEN

         WINSIZ  =  DEGREE + 1

      ELSE

         CALL SETMSG ( 'This point should not be reached. Getting ' // &
                       'here may indicate that the code needs to '  // &
                       'updated to handle new subtypes.'            )
         CALL SIGERR ( 'SPICE(BUG)'                                 )
         CALL CHKOUT ( 'SPKW18'                                     )
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
         CALL CHKOUT ( 'SPKW18'                                    )
         RETURN

      END IF

!
!     Check to see if the segment identifier is too long.
!
      IF ( LASTNB(SEGID) .GT. SIDLEN ) THEN

         CALL SETMSG ( 'Segment identifier contains more than ' // &
                       '40 characters.'                          )
         CALL SIGERR ( 'SPICE(SEGIDTOOLONG)'                     )
         CALL CHKOUT ( 'SPKW18'                                  )
         RETURN

      END IF

!
!     Now check that all the characters in the segment identifier
!     can be printed.
!
      DO I = 1, LASTNB(SEGID)

         CHRCOD = ICHAR( SEGID(I:I) )

         IF ( ( CHRCOD .LT. FPRINT ) .OR. ( CHRCOD .GT. LPRINT ) ) THEN

            CALL SETMSG ( 'The segment identifier contains '  // &
                          'nonprintable characters'               )
            CALL SIGERR ( 'SPICE(NONPRINTABLECHARS)'              )
            CALL CHKOUT ( 'SPKW18'                                )
            RETURN

         END IF

      END DO

!
!     Make sure that the degree of the interpolating polynomials is
!     in range.
!
      IF (  ( DEGREE .LT. 1 ) .OR. ( DEGREE .GT. MAXDEG )  )  THEN

         CALL SETMSG ( 'The interpolating polynomials have degree #; '// &
                       'the valid degree range is [1, #]'              )
         CALL ERRINT ( '#', DEGREE                                     )
         CALL ERRINT ( '#', MAXDEG                                     )
         CALL SIGERR ( 'SPICE(INVALIDDEGREE)'                          )
         CALL CHKOUT ( 'SPKW18'                                        )
         RETURN

      END IF

!
!     Make sure that the window size is even.  If not, the input
!     DEGREE is incompatible with the subtype.
!
      IF (  ODD(WINSIZ)  )  THEN

         CALL SETMSG ( 'The interpolating polynomials have degree #; '// &
                       'for SPK type 18, the degree must be '         // &
                       'equivalent to 3 mod 4 for Hermite '           // &
                       'interpolation and odd for for Lagrange '      // &
                       'interpolation.'                                )
         CALL ERRINT ( '#', DEGREE                                     )
         CALL SIGERR ( 'SPICE(INVALIDDEGREE)'                          )
         CALL CHKOUT ( 'SPKW18'                                        )
         RETURN

      END IF

!
!     Make sure that the number of packets is sufficient to define a
!     polynomial whose degree is DEGREE.
!
      IF ( N .LT. 2 ) THEN

         CALL SETMSG ( 'At least 2 packets are required for SPK type '// &
                       '18.  Number of packets supplied:  #'           )
         CALL ERRINT ( '#', N                                          )
         CALL SIGERR ( 'SPICE(TOOFEWSTATES)'                           )
         CALL CHKOUT ( 'SPKW18'                                        )
         RETURN

      END IF

!
!     The segment stop time should be greater than or equal to
!     the begin time.
!
      IF ( FIRST .GT. LAST ) THEN

         CALL SETMSG ( 'The segment start time: # is greater then ' // &
                       'the segment end time: #'                   )
         CALL ERRDP  ( '#', FIRST                                  )
         CALL ERRDP  ( '#', LAST                                   )
         CALL SIGERR ( 'SPICE(BADDESCRTIMES)'                      )
         CALL CHKOUT ( 'SPKW18'                                    )
         RETURN

      END IF

!
!     Make sure the epochs form a strictly increasing sequence.
!
      MAXTIM = EPOCHS(1)

      DO I = 2, N

         IF ( EPOCHS(I) .LE. MAXTIM ) THEN

            CALL SETMSG ( 'EPOCH # having index # is not greater '    // &
                          'than its predecessor #.'                   )
            CALL ERRDP  ( '#',  EPOCHS(I)                             )
            CALL ERRINT ( '#',  I                                     )
            CALL ERRDP  ( '#',  EPOCHS(I-1)                           )
            CALL SIGERR ( 'SPICE(TIMESOUTOFORDER)'                    )
            CALL CHKOUT ( 'SPKW18'                                    )
            RETURN
         ELSE
            MAXTIM = EPOCHS(I)
         END IF

      END DO

!
!     Make sure that the span of the input epochs includes the interval
!     defined by the segment descriptor.
!
      IF (  EPOCHS(1) .GT. FIRST ) THEN

         CALL SETMSG ( 'Segment start time # precedes first epoch #.' )
         CALL ERRDP  ( '#',  FIRST                                    )
         CALL ERRDP  ( '#',  EPOCHS(1)                                )
         CALL SIGERR ( 'SPICE(BADDESCRTIMES)'                         )
         CALL CHKOUT ( 'SPKW18'                                       )
         RETURN

      ELSE IF (  EPOCHS(N) .LT. LAST ) THEN

         CALL SETMSG ( 'Segment end time # follows last epoch #.'     )
         CALL ERRDP  ( '#',  LAST                                     )
         CALL ERRDP  ( '#',  EPOCHS(N)                                )
         CALL SIGERR ( 'SPICE(BADDESCRTIMES)'                         )
         CALL CHKOUT ( 'SPKW18'                                       )
         RETURN

      END IF



!
!     If we made it this far, we're ready to start writing the segment.
!
!
!     Create the segment descriptor.  We don't use SPKPDS because
!     that routine doesn't allow creation of a singleton segment.
!
      IC(1) = BODY
      IC(2) = CENTER

      CALL NAMFRM ( FRAME, IC(3) )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SPKW18' )
         RETURN
      END IF

      IC(4) = DTYPE

      DC(1) = FIRST
      DC(2) = LAST

      CALL DAFPS  ( ND, NI, DC, IC, DESCR )

!
!     Begin a new segment.
!
      CALL DAFBNA ( HANDLE, DESCR, SEGID )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SPKW18' )
         RETURN
      END IF


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
      CALL DAFADA ( PACKTS,  N*PACKSZ )
      CALL DAFADA ( EPOCHS,  N        )

      DO I = 1,   (N-1) / DIRSIZ
         CALL DAFADA (  EPOCHS( DIRSIZ*I ),  1  )
      END DO

      CALL DAFADA ( DBLE( SUBTYP ),  1 )
      CALL DAFADA ( DBLE( WINSIZ ),  1 )
      CALL DAFADA ( DBLE( N      ),  1 )

!
!     As long as nothing went wrong, end the segment.
!
      IF ( .NOT. FAILED() ) THEN
         CALL DAFENA
      END IF

      CALL CHKOUT ( 'SPKW18' )
      RETURN
      END
