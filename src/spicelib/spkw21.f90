!$Procedure SPKW21 ( Write SPK segment, type 21 )

      SUBROUTINE SPKW21 ( HANDLE, BODY,   CENTER, FRAME, &
                          FIRST,  LAST,   SEGID,  N, &
                          DLSIZE, DLINES, EPOCHS        )

! $ Abstract
!
!     Write a type 21 segment to an SPK file.
!
! $ Required_Reading
!
!     NAIF_IDS
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

      INCLUDE 'spk21.inc'

      INTEGER               HANDLE
      INTEGER               BODY
      INTEGER               CENTER
      CHARACTER*(*)         FRAME
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      LAST
      CHARACTER*(*)         SEGID
      INTEGER               N
      INTEGER               DLSIZE
      DOUBLE PRECISION      DLINES ( DLSIZE, * )
      DOUBLE PRECISION      EPOCHS (         * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of an SPK file open for writing.
!     BODY       I   NAIF code for an ephemeris object.
!     CENTER     I   NAIF code for center of motion of BODY.
!     FRAME      I   Reference frame name.
!     FIRST      I   Start time of interval covered by segment.
!     LAST       I   End time of interval covered by segment.
!     SEGID      I   Segment identifier.
!     N          I   Number of difference lines in segment.
!     DLSIZE     I   Difference line size.
!     DLINES     I   Array of difference lines.
!     EPOCHS     I   Coverage end times of difference lines.
!     MAXTRM     P   Maximum number of terms per difference table
!                    component.
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
!     FRAME    is the NAIF name for a reference frame relative to
!              which the state information for BODY is specified.
!
!     FIRST,
!     LAST     are, respectively, the start and stop times of
!              the time interval over which the segment defines
!              the state of BODY.
!
!     SEGID    is the segment identifier. An SPK segment
!              identifier may contain up to 40 characters.
!
!     N        is the number of difference lines in the input
!              difference line array.
!
!     DLSIZE   is the size of each difference line data structure
!              in the difference line array input DLINES. Let
!              MAXDIM be the dimension of each component of the
!              difference table within each difference line. Then
!              the size DLSIZE of the difference line is
!
!                 ( 4 * MAXDIM ) + 11
!
!     DLINES   contains a time-ordered array of difference lines.
!              The Ith difference line occupies elements (1,I)
!              through (MAXDIM,I) of DLINES, where MAXDIM is
!              as described above in the description of DLSIZE.
!              Each difference line represents the state (x, y,
!              z, dx/dt, dy/dt, dz/dt, in kilometers and
!              kilometers per second) of BODY relative to CENTER,
!              specified relative to FRAME, for an interval of
!              time. The time interval covered by the Ith
!              difference line ends at the Ith element of the
!              array EPOCHS (described below). The interval
!              covered by the first difference line starts at the
!              segment start time.
!
!              The contents of a difference line are as shown
!              below:
!
!                 Dimension  Description
!                 ---------  ----------------------------------
!                 1          Reference epoch of difference line
!                 MAXDIM     Stepsize function vector
!                 1          Reference position vector,  x
!                 1          Reference velocity vector,  x
!                 1          Reference position vector,  y
!                 1          Reference velocity vector,  y
!                 1          Reference position vector,  z
!                 1          Reference velocity vector,  z
!                 MAXDIM,3   Modified divided difference
!                            arrays (MDAs)
!                 1          Maximum integration order plus 1
!                 3          Integration order array
!
!              The reference position and velocity are those of
!              BODY relative to CENTER at the reference epoch.
!              (A difference line is essentially a polynomial
!              expansion of acceleration about the reference
!              epoch.)
!
!     EPOCHS   is an array of epochs corresponding to the members
!              of the difference line array. The epochs are
!              specified as seconds past J2000 TDB.
!
!              The first difference line covers the time interval
!              from the segment start time to EPOCHS(1). For
!              I > 1, the Ith difference line covers the half-open
!              time interval from, but not including, EPOCHS(I-1)
!              through EPOCHS(I).
!
!              The elements of EPOCHS must be strictly increasing.
!
! $ Detailed_Output
!
!     None. See $Particulars for a description of the effect of this
!     routine.
!
! $ Parameters
!
!     MAXTRM   is the maximum number of terms allowed in
!              each component of the difference table
!              contained in the input argument RECORD.
!              See the INCLUDE file spk21.inc for the value
!              of MAXTRM.
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
!     4)  If the number of difference lines N is not at least one,
!         the error SPICE(INVALIDCOUNT) is signaled.
!
!     5)  If FIRST is greater than LAST, the error
!         SPICE(BADDESCRTIMES) is signaled.
!
!     6)  If the elements of the array EPOCHS are not in strictly
!         increasing order, the error SPICE(TIMESOUTOFORDER) is
!         signaled.
!
!     7)  If the last epoch EPOCHS(N) is less than LAST, the error
!         SPICE(COVERAGEGAP) is signaled.
!
!     8)  If DLSIZE is greater than the limit
!
!            ( 4 * MAXTRM ) + 11
!
!         the error SPICE(DIFFLINETOOLARGE) is signaled. If
!         DLSIZE is less than 71, the error SPICE(DIFFLINETOOSMALL)
!         is signaled.
!
!     9)  Let KQMAX1 be the maximum integration order for a given
!         difference line. If any value at index KQMAX1-1 or greater
!         in the step size array of that difference line is zero, the
!         error SPICE(ZEROSTEP) is signaled. The checked portion
!         of the step size vector lies in the index range 2:KQMAX-1
!         of the difference line containing the vector.
!
! $ Files
!
!     A new type 21 SPK segment is written to the SPK file attached
!     to HANDLE.
!
! $ Particulars
!
!     This routine writes an SPK type 21 data segment to the open SPK
!     file according to the format described in the type 21 section of
!     the SPK Required Reading. The SPK file must have been opened with
!     write access.
!
! $ Examples
!
!     Suppose that you have difference lines and are prepared to
!     produce a segment of type 21 in an SPK file.
!
!     The following code fragment could be used to add the new segment
!     to a previously opened SPK file attached to HANDLE. The file must
!     have been opened with write access.
!
!        C
!        C     Create a segment identifier.
!        C
!              SEGID = 'MY_SAMPLE_SPK_TYPE_21_SEGMENT'
!
!        C
!        C     Write the segment.
!        C
!              CALL SPKW21 (  HANDLE,  BODY,    CENTER,  FRAME,
!             .               FIRST,   LAST,    SEGID,   N,
!             .               DLSIZE,  DLINES,  EPOCHS         )
!
! $ Restrictions
!
!     1)  The validity of the difference lines is not checked by
!         this routine.
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
!-    SPICELIB Version 2.0.1, 03-JUN-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 2.0.0, 21-AUG-2015 (NJB)
!
!        Relaxed step size error check to allow zero
!        values in elements indexed greater than KQMAX1-2.
!
!-    SPICELIB Version 1.0.0, 03-FEB-2014 (NJB)
!
!-&


! $ Index_Entries
!
!     write SPK type_21 ephemeris data segment
!
!-&


!
!     SPICELIB functions
!
      INTEGER               LASTNB

      LOGICAL               FAILED
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

!
!     MINDSZ is the minimum MDA size; this is the size
!     of type 1 MDAs.
!
      INTEGER               MINDSZ
      PARAMETER           ( MINDSZ =   71 )

!
!     Local variables
!
      INTEGER               DSCSIZ
      PARAMETER           ( DSCSIZ  =   5 )

      INTEGER               DTYPE
      PARAMETER           ( DTYPE   =  21 )

      INTEGER               DIRSIZ
      PARAMETER           ( DIRSIZ  = 100 )


!
!     Local variables
!
      DOUBLE PRECISION      DESCR  ( DSCSIZ )
      DOUBLE PRECISION      PRVEPC

      INTEGER               CHRCOD
      INTEGER               I
      INTEGER               J
      INTEGER               KQMAX1
      INTEGER               KQMLOC
      INTEGER               MAXDIM
      INTEGER               MAXDSZ
      INTEGER               REFCOD


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'SPKW21' )

!
!     Make sure the difference line size is within limits.
!
      MAXDSZ = ( 4 * MAXTRM ) + 11

      IF ( DLSIZE .GT. MAXDSZ ) THEN

         CALL SETMSG ( 'The input difference line size is #, ' &
         //            'while the maximum supported by ' &
         //            'this routine is #. It is possible ' &
         //            'that this problem is due to your ' &
         //            'SPICE Toolkit being out of date.'      )
         CALL ERRINT ( '#',  DLSIZE                            )
         CALL ERRINT ( '#',  MAXDSZ                            )
         CALL SIGERR ( 'SPICE(DIFFLINETOOLARGE)'               )
         CALL CHKOUT ( 'SPKW21'                                )
         RETURN

      END IF

      IF ( DLSIZE .LT. MINDSZ ) THEN

         CALL SETMSG ( 'The input difference line size is #, ' &
         //            'while the minimum supported by ' &
         //            'this routine is #. It is possible ' &
         //            'that this problem is due to your ' &
         //            'SPICE Toolkit being out of date.'      )
         CALL ERRINT ( '#',  DLSIZE                            )
         CALL ERRINT ( '#',  MINDSZ                            )
         CALL SIGERR ( 'SPICE(DIFFLINETOOSMALL)'               )
         CALL CHKOUT ( 'SPKW21'                                )
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
         CALL CHKOUT ( 'SPKW21'                                    )
         RETURN

      END IF

!
!     Check to see if the segment identifier is too long.
!
      IF ( LASTNB(SEGID) .GT. SIDLEN ) THEN

         CALL SETMSG ( 'Segment identifier contains more than ' // &
                       '40 characters.'                          )
         CALL SIGERR ( 'SPICE(SEGIDTOOLONG)'                     )
         CALL CHKOUT ( 'SPKW21'                                  )
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
            CALL CHKOUT ( 'SPKW21'                                )
            RETURN

         END IF

      END DO

!
!     The difference line count must be at least one.
!
      IF ( N .LT. 1 ) THEN

         CALL SETMSG ( 'The difference line count was #; the count ' // &
                       'must be at least one.'                       )
         CALL ERRINT ( '#', N                                        )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                         )
         CALL CHKOUT ( 'SPKW21'                                      )
         RETURN

      END IF

!
!     The segment stop time should be greater than or equal to
!     the begin time.
!
      IF ( FIRST .GT. LAST ) THEN

         CALL SETMSG ( 'The segment start time: # is greater than ' // &
                       'the segment end time: #'                   )
         CALL ERRDP  ( '#', FIRST                                  )
         CALL ERRDP  ( '#', LAST                                   )
         CALL SIGERR ( 'SPICE(BADDESCRTIMES)'                      )
         CALL CHKOUT ( 'SPKW21'                                    )
         RETURN

      END IF

!
!     Make sure the epochs form a strictly increasing sequence.
!
      PRVEPC = EPOCHS(1)

      DO I = 2, N

         IF ( EPOCHS(I) .LE. PRVEPC ) THEN

            CALL SETMSG ( 'EPOCH # having index # is not greater '    // &
                          'than its predecessor #.'                   )
            CALL ERRDP  ( '#',  EPOCHS(I)                             )
            CALL ERRINT ( '#',  I                                     )
            CALL ERRDP  ( '#',  EPOCHS(I-1)                           )
            CALL SIGERR ( 'SPICE(TIMESOUTOFORDER)'                    )
            CALL CHKOUT ( 'SPKW21'                                    )
            RETURN

         END IF

         PRVEPC = EPOCHS(I)

      END DO

!
!     Make sure there's no gap between the last difference line
!     epoch and the end of the time interval defined by the segment
!     descriptor.
!
      IF (  EPOCHS(N) .LT. LAST ) THEN

         CALL SETMSG ( 'Segment has coverage gap: segment end time # ' &
         //            'follows last epoch #.'                        )
         CALL ERRDP  ( '#',  LAST                                     )
         CALL ERRDP  ( '#',  EPOCHS(N)                                )
         CALL SIGERR ( 'SPICE(COVERAGEGAP)'                           )
         CALL CHKOUT ( 'SPKW21'                                       )
         RETURN

      END IF

!
!     Check the step size vectors in the difference lines.
!
      MAXDIM = ( DLSIZE - 11 ) / 4
      KQMLOC = ( 4 * MAXDIM  ) + 8


      DO I = 1, N
!
!        Check only the first KQMAX1-2 elements of the step size
!        vector. The higher-indexed elements are allowed to be zero.
!
         KQMAX1 = NINT( DLINES(KQMLOC,I) )

         DO J = 2, KQMAX1-1

            IF ( DLINES(J,I) .EQ. 0.D0 ) THEN

               CALL SETMSG ( 'Step size was zero at step size vector ' &
               //            'index # within difference line #.'      )
               CALL ERRINT ( '#',  J-1                                )
               CALL ERRINT ( '#',  I                                  )
               CALL SIGERR ( 'SPICE(ZEROSTEP)'                        )
               CALL CHKOUT ( 'SPKW21'                                 )
               RETURN

            END IF

         END DO

      END DO

!
!     If we made it this far, we're ready to start writing the segment.
!
!     Create the segment descriptor.
!
      CALL SPKPDS ( BODY, CENTER, FRAME, DTYPE, FIRST, LAST, DESCR )

!
!     Begin a new segment.
!
      CALL DAFBNA ( HANDLE, DESCR, SEGID )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SPKW21' )
         RETURN
      END IF

!
!     The type 21 segment structure is shown below:
!
!        +-----------------------+
!        | Difference line 1     |
!        +-----------------------+
!        | Difference line 2     |
!        +-----------------------+
!                   ...
!        +-----------------------+
!        | Difference line N     |
!        +-----------------------+
!        | Epoch 1               |
!        +-----------------------+
!        | Epoch 2               |
!        +-----------------------+
!                   ...
!        +-----------------------+
!        | Epoch N               |
!        +-----------------------+
!        | Epoch 100             | (First directory)
!        +-----------------------+
!                   ...
!        +-----------------------+
!        | Epoch (N/100)*100     | (Last directory)
!        +-----------------------+
!        | Max diff table size   |
!        +-----------------------+
!        | Number of diff lines  |
!        +-----------------------+
!

      CALL DAFADA ( DLINES,  N*DLSIZE )
      CALL DAFADA ( EPOCHS,  N        )

      DO I = 1,   N / DIRSIZ
         CALL DAFADA (  EPOCHS( DIRSIZ*I ),  1  )
      END DO

      CALL DAFADA ( DBLE( MAXDIM ),  1 )
      CALL DAFADA ( DBLE( N      ),  1 )

!
!     As long as nothing went wrong, end the segment.
!
      IF ( .NOT. FAILED() ) THEN
         CALL DAFENA
      END IF

      CALL CHKOUT ( 'SPKW21' )
      RETURN
      END
