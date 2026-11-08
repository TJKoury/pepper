!$Procedure SPKW01 ( Write SPK segment, type 1 )

      SUBROUTINE SPKW01 (  HANDLE,  BODY,    CENTER,  FRAME,   FIRST, &
                           LAST,    SEGID,   N,       DLINES,  EPOCHS )

! $ Abstract
!
!     Write a type 1 segment to an SPK file.
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

      INTEGER               DLSIZE
      PARAMETER           ( DLSIZE = 71 )

      INTEGER               HANDLE
      INTEGER               BODY
      INTEGER               CENTER
      CHARACTER*(*)         FRAME
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      LAST
      CHARACTER*(*)         SEGID
      INTEGER               N
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
!     DLINES     I   Array of difference lines.
!     EPOCHS     I   Coverage end times of difference lines.
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
!     DLINES   contains a time-ordered array of difference lines
!              The Ith difference line occupies elements (1,I)
!              through (71,I) of DLINES. Each difference line
!              represents the state (x, y, z, dx/dt, dy/dt,
!              dz/dt, in kilometers and kilometers per second)
!              of BODY relative to CENTER, specified relative to
!              FRAME, for an interval of time. The time interval
!              covered by the Ith difference line ends at the
!              Ith element of the array EPOCHS (described below).
!              The interval covered by the first difference line
!              starts at the segment start time.
!
!              The contents of a difference line are as shown
!              below:
!
!                 Dimension  Description
!                 ---------  ----------------------------------
!                 1          Reference epoch of difference line
!                 15         Stepsize function vector
!                 1          Reference position vector,  x
!                 1          Reference velocity vector,  x
!                 1          Reference position vector,  y
!                 1          Reference velocity vector,  y
!                 1          Reference position vector,  z
!                 1          Reference velocity vector,  z
!                 15,3       Modified divided difference
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
!
!     EPOCHS   is an array of epochs corresponding to the members
!              of the state array. The epochs are specified as
!              seconds past J2000, TDB.
!
!              The first difference line covers the time interval
!              from the segment start time to EPOCHS(1).  For
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
!     None.
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
!     5)  If FIRST is greater than or equal to LAST, the error
!         SPICE(BADDESCRTIMES) is signaled.
!
!     6)  If the elements of the array EPOCHS are not in strictly
!         increasing order, the error SPICE(TIMESOUTOFORDER) is
!         signaled.
!
!     7)  If the last epoch EPOCHS(N) is less than LAST, the error
!         SPICE(BADDESCRTIMES) is signaled.
!
! $ Files
!
!     A new type 1 SPK segment is written to the SPK file attached
!     to HANDLE.
!
! $ Particulars
!
!     This routine writes an SPK type 1 data segment to the open SPK
!     file according to the format described in the type 1 section of
!     the SPK Required Reading. The SPK file must have been opened with
!     write access.
!
! $ Examples
!
!     Suppose that you have difference lines and are prepared to
!     produce a segment of type 1 in an SPK file.
!
!     The following code fragment could be used to add the new segment
!     to a previously opened SPK file attached to HANDLE. The file must
!     have been opened with write access.
!
!        C
!        C     Create a segment identifier.
!        C
!                  SEGID = 'MY_SAMPLE_SPK_TYPE_1_SEGMENT'
!
!        C
!        C     Write the segment.
!        C
!              CALL SPKW01 (  HANDLE,  BODY,    CENTER,  FRAME,
!             .               FIRST,   LAST,    SEGID,   N,
!             .               DLINES,  EPOCHS                  )
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
!-    SPICELIB Version 1.0.2, 03-JUN-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.1, 07-APR-2010 (NJB)
!
!        Updated $Detailed_Input to state that the elements
!        of EPOCHS must be strictly increasing. The $Exceptions
!        section already described this error condition.
!
!-    SPICELIB Version 1.0.0, 30-JAN-2003 (NJB)
!
!-&


! $ Index_Entries
!
!     write SPK type_1 ephemeris data segment
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
!     Local variables
!
      INTEGER               DSCSIZ
      PARAMETER           ( DSCSIZ  =   5 )

      INTEGER               TYPIDX
      PARAMETER           ( TYPIDX  =   4 )

      INTEGER               DTYPE
      PARAMETER           ( DTYPE   =   1 )

      INTEGER               DIRSIZ
      PARAMETER           ( DIRSIZ  = 100 )


!
!     Local variables
!
      DOUBLE PRECISION      DESCR  ( DSCSIZ )
      DOUBLE PRECISION      MAXTIM

      INTEGER               CHRCOD
      INTEGER               I
      INTEGER               REFCOD


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SPKW01' )
      END IF

!
!     Get the NAIF integer code for the reference frame.
!
      CALL NAMFRM ( FRAME, REFCOD )

      IF ( REFCOD .EQ. 0 ) THEN

         CALL SETMSG ( 'The reference frame # is not supported.'   )
         CALL ERRCH  ( '#', FRAME                                  )
         CALL SIGERR ( 'SPICE(INVALIDREFFRAME)'                    )
         CALL CHKOUT ( 'SPKW01'                                    )
         RETURN

      END IF

!
!     Check to see if the segment identifier is too long.
!
      IF ( LASTNB(SEGID) .GT. SIDLEN ) THEN

         CALL SETMSG ( 'Segment identifier contains more than ' // &
                       '40 characters.'                          )
         CALL SIGERR ( 'SPICE(SEGIDTOOLONG)'                     )
         CALL CHKOUT ( 'SPKW01'                                  )
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
            CALL CHKOUT ( 'SPKW01'                                )
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
         CALL CHKOUT ( 'SPKW01'                                      )
         RETURN

      END IF

!
!     The segment stop time should be greater then the begin time.
!
      IF ( FIRST .GE. LAST ) THEN

         CALL SETMSG ( 'The segment start time: # is greater then ' // &
                       'the segment end time: #'                   )
         CALL ERRDP  ( '#', FIRST                                  )
         CALL ERRDP  ( '#', LAST                                   )
         CALL SIGERR ( 'SPICE(BADDESCRTIMES)'                      )
         CALL CHKOUT ( 'SPKW01'                                    )
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
            CALL CHKOUT ( 'SPKW01'                                    )
            RETURN
         ELSE
            MAXTIM = EPOCHS(I)
         END IF

      END DO

!
!     Make sure there's no gap between the last difference line
!     epoch and the end of the time interval defined by the segment
!     descriptor.
!
      IF (  EPOCHS(N) .LT. LAST ) THEN

         CALL SETMSG ( 'Segment end time # follows last epoch #.'     )
         CALL ERRDP  ( '#',  LAST                                     )
         CALL ERRDP  ( '#',  EPOCHS(N)                                )
         CALL SIGERR ( 'SPICE(BADDESCRTIMES)'                         )
         CALL CHKOUT ( 'SPKW01'                                       )
         RETURN

      END IF

!
!     If we made it this far, we're ready to start writing the segment.
!
!
!     Create the segment descriptor.
!
      CALL SPKPDS ( BODY, CENTER, FRAME, DTYPE, FIRST, LAST, DESCR )

!
!     Begin a new segment.
!
      CALL DAFBNA ( HANDLE, DESCR, SEGID )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SPKW01' )
         RETURN
      END IF


!
!     The type 1 segment structure is shown below:
!
!        +-----------------------+
!        | Difference line 1     |
!        +-----------------------+
!        | Difference line 2     |
!        +-----------------------+
!                    .
!                    .
!                    .
!        +-----------------------+
!        | Difference line N     |
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
!        | Epoch (N/100)*100     | (Last directory)
!        +-----------------------+
!        | Number of diff lines  |
!        +-----------------------+
!
!

      CALL DAFADA ( DLINES,  N*DLSIZE )
      CALL DAFADA ( EPOCHS,  N        )

      DO I = 1,   N / DIRSIZ
         CALL DAFADA (  EPOCHS( DIRSIZ*I ),  1  )
      END DO

      CALL DAFADA ( DBLE( N ),  1 )

!
!     As long as nothing went wrong, end the segment.
!
      IF ( .NOT. FAILED() ) THEN
         CALL DAFENA
      END IF


      CALL CHKOUT ( 'SPKW01' )
      RETURN
      END
