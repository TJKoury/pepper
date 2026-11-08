!$Procedure SPKW09 ( Write SPK segment, type 9 )

      SUBROUTINE SPKW09 (  HANDLE,  BODY,    CENTER,  FRAME, &
                           FIRST,   LAST,    SEGID,   DEGREE, &
                           N,       STATES,  EPOCHS           )

! $ Abstract
!
!     Write a type 9 segment to an SPK file.
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

      INTEGER               HANDLE
      INTEGER               BODY
      INTEGER               CENTER
      CHARACTER*(*)         FRAME
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      LAST
      CHARACTER*(*)         SEGID
      INTEGER               DEGREE
      INTEGER               N
      DOUBLE PRECISION      STATES ( 6, * )
      DOUBLE PRECISION      EPOCHS (    * )

      INTEGER               MAXDEG
      PARAMETER           ( MAXDEG = 27 )

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
!     DEGREE     I   Degree of interpolating polynomials.
!     N          I   Number of states.
!     STATES     I   Array of states.
!     EPOCHS     I   Array of epochs corresponding to states.
!     MAXDEG     P   Maximum allowed degree of interpolating polynomial.
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
!     EPOCHS   is an array of epochs corresponding to the members
!              of the state array. The epochs are specified as
!              seconds past J2000, TDB.
!
! $ Detailed_Output
!
!     None. See $Particulars for a description of the effect of this
!     routine.
!
! $ Parameters
!
!     MAXDEG   is the maximum allowed degree of the interpolating
!              polynomial. If the value of MAXDEG is increased,
!              the SPICELIB routine SPKPV must be changed
!              accordingly. In particular, the size of the
!              record passed to SPKRnn and SPKEnn must be
!              increased, and comments describing the record size
!              must be changed.
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
!     5)  If the number of states N is not at least DEGREE+1, the error
!         SPICE(TOOFEWSTATES) is signaled.
!
!     6)  If FIRST is greater than or equal to LAST, the error
!         SPICE(BADDESCRTIMES) is signaled.
!
!     7)  If the elements of the array EPOCHS are not in strictly
!         increasing order, the error SPICE(TIMESOUTOFORDER) is
!         signaled.
!
!     8)  If the first epoch, EPOCHS(1), is greater than FIRST, the
!         error SPICE(BADDESCRTIMES) is signaled.
!
!     9)  If the last epoch, EPOCHS(N), is less than LAST, the error
!         SPICE(BADDESCRTIMES) is signaled.
!
! $ Files
!
!     A new type 9 SPK segment is written to the SPK file attached
!     to HANDLE.
!
! $ Particulars
!
!     This routine writes an SPK type 09 data segment to the open SPK
!     file according to the format described in the type 09 section of
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
!     1) Suppose that you have a time-ordered array of geometric states
!        of a new object that follows Phobos, with a delay of 1 hour,
!        in its orbit around Mars and are prepared to produce a segment
!        of type 09 in an SPK file. Create a new SPK file with this
!        segment. Use an existing SPK to create the input data for the
!        SPK segment.
!
!        Use the meta-kernel shown below to load the required SPICE
!        kernels.
!
!
!           KPL/MK
!
!           File: spkw09_ex1.tm
!
!           This meta-kernel is intended to support operation of SPICE
!           example programs. The kernels shown here should not be
!           assumed to contain adequate or correct versions of data
!           required by SPICE-based user applications.
!
!           In order for an application to use this meta-kernel, the
!           kernels referenced here must be present in the user's
!           current working directory.
!
!           The names and contents of the kernels referenced
!           by this meta-kernel are as follows:
!
!              File name                        Contents
!              ---------                        --------
!              mar097.bsp                       Mars satellite ephemeris
!              naif0012.tls                     Leapseconds
!
!           \begindata
!
!              KERNELS_TO_LOAD = ( 'mar097.bsp',
!                                  'naif0012.tls' )
!
!           \begintext
!
!           End of meta-kernel
!
!
!        Example code begins here.
!
!
!              PROGRAM SPKW09_EX1
!              IMPLICIT NONE
!
!        C
!        C     SPICELIB functions
!        C
!              DOUBLE PRECISION      HALFPI
!
!        C
!        C     Local parameters.
!        C
!              CHARACTER*(*)         SPKNAM
!              PARAMETER           ( SPKNAM = 'spkw09_ex1.bsp' )
!
!              INTEGER               DEGREE
!              PARAMETER           ( DEGREE = 3   )
!
!              INTEGER               MARS
!              PARAMETER           ( MARS   = 499 )
!
!              INTEGER               NAMLEN
!              PARAMETER           ( NAMLEN = 255 )
!
!              INTEGER               NEPOCS
!              PARAMETER           ( NEPOCS = 800 )
!
!              INTEGER               NOBJ
!              PARAMETER           ( NOBJ   = 403 )
!
!        C
!        C     Local variables.
!        C
!              CHARACTER*(NAMLEN)    IFNAME
!              CHARACTER*(NAMLEN)    SEGID
!
!              DOUBLE PRECISION      DELTA
!              DOUBLE PRECISION      ET
!              DOUBLE PRECISION      EPOCHS ( NEPOCS )
!              DOUBLE PRECISION      LT
!              DOUBLE PRECISION      STATE  ( 6 )
!              DOUBLE PRECISION      STATES ( 6, NEPOCS )
!              DOUBLE PRECISION      STEP
!              DOUBLE PRECISION      TIME
!
!              INTEGER               I
!              INTEGER               HANDLE
!
!        C
!        C     Load the input SPK file.
!        C
!              CALL FURNSH ( 'spkw09_ex1.tm' )
!
!        C
!        C     Convert the input UTC to ephemeris time
!        C
!              CALL STR2ET ( '2018 Apr 03 08:35', ET )
!
!        C
!        C     Create the time-ordered array of geometric states,
!        C     at unequal time steps.
!        C
!              TIME  = ET
!              STEP  = 60.D0
!              DELTA = 10.D0
!
!              DO I=1, NEPOCS
!
!                 CALL SPKEZR ( 'PHOBOS', TIME,       'J2000', 'NONE',
!             .                 'MARS',   STATES(1,I), LT             )
!
!                 EPOCHS(I) = TIME + 3600.D0
!                 TIME = TIME + STEP +
!             .          SIN( HALFPI() * I / 2.D0 ) * DELTA
!
!              END DO
!
!        C
!        C     Open a new SPK file, with 5000 characters reserved
!        C     for comments.
!        C
!              IFNAME = 'Test SPK type 9 internal filename.'
!              CALL SPKOPN ( SPKNAM, IFNAME, 5000, HANDLE )
!
!        C
!        C     Create a segment identifier.
!        C
!              SEGID = 'MY_SAMPLE_SPK_TYPE_9_SEGMENT'
!
!
!        C
!        C     Write the segment.
!        C
!              CALL SPKW09 ( HANDLE,    NOBJ,           MARS,  'J2000',
!             .              EPOCHS(1), EPOCHS(NEPOCS), SEGID,  DEGREE,
!             .              NEPOCS,    STATES,         EPOCHS        )
!
!        C
!        C     Close the new SPK file.
!        C
!              CALL SPKCLS ( HANDLE )
!
!        C
!        C     Compute the state of Phobos as seen from Mars,
!        C     12 hours after the input UTC time.
!        C
!              ET = ET + 43200.0D0
!              CALL SPKEZR ( 'PHOBOS', ET, 'J2000', 'NONE', 'MARS',
!             .               STATE,   LT                          )
!
!              WRITE (*,'(A)') 'Phobos as seen from Mars'
!              WRITE (*,'(A,F20.6)') '   Epoch       (s):', ET
!              WRITE (*,'(A,3F14.6)') '   Position   (km):',
!             .                                   (STATE(I), I=1,3)
!              WRITE (*,'(A,3F14.6)') '   Velocity (km/s):',
!             .                                   (STATE(I), I=4,6)
!              WRITE (*,*)
!
!        C
!        C     Load the newly created kernel, and compute the state
!        C     of the new object as seen from Mars, 13 hours after
!        C     the input UTC time.
!        C
!              CALL FURNSH ( SPKNAM )
!              ET = ET + 3600.0D0
!
!              CALL SPKEZR ( '403', ET, 'J2000', 'NONE', 'MARS',
!             .               STATE,   LT                       )
!
!              WRITE (*,'(A)') 'Object 403 as seen from Mars'
!              WRITE (*,'(A,F20.6)') '   Epoch       (s):', ET
!              WRITE (*,'(A,3F14.6)') '   Position   (km):',
!             .                                   (STATE(I), I=1,3)
!              WRITE (*,'(A,3F14.6)') '   Velocity (km/s):',
!             .                                   (STATE(I), I=4,6)
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        Phobos as seen from Mars
!           Epoch       (s):    576059769.185657
!           Position   (km):  -7327.262770   2414.326550   5207.106376
!           Velocity (km/s):     -0.942893     -1.894731     -0.396715
!
!        Object 403 as seen from Mars
!           Epoch       (s):    576063369.185657
!           Position   (km):  -7327.262770   2414.326550   5207.106376
!           Velocity (km/s):     -0.942893     -1.894731     -0.396715
!
!
!        Note that after run completion, a new SPK file exists in
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
!     K.R. Gehringer     (JPL)
!     J.M. Lynch         (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 3.0.1, 05-JUL-2021 (JDR)
!
!        Edited the header to comply with NAIF standard. Added complete
!        code example to $Examples section. Removed unnecessary
!        $Revisions section.
!
!-    SPICELIB Version 3.0.0, 24-DEC-2013 (NJB)
!
!        Increased MAXDEG to 27 for compatibility
!        with SPK type 21.
!
!-    SPICELIB Version 2.0.0, 19-SEP-1995 (WLT)
!
!        The routine was upgraded to support non-inertial reference
!        frames.
!
!-    SPICELIB Version 1.0.1, 05-OCT-1993 (KRG)
!
!        Removed all references to a specific method of opening the SPK
!        file in the $Brief_I/O, $Detailed_Input, $Particulars and
!        $Examples sections of the header. It is assumed that a person
!        using this routine has some knowledge of the DAF system and the
!        methods for obtaining file handles.
!
!-    SPICELIB Version 1.0.0, 05-AUG-1993 (NJB) (JML) (WLT)
!
!-&


! $ Index_Entries
!
!     write SPK type_9 ephemeris data segment
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
      INTEGER               SIDLEN
      PARAMETER           ( SIDLEN  =  40 )

      INTEGER               NS
      PARAMETER           ( NS      =   5 )

      INTEGER               ND
      PARAMETER           ( ND      =   2 )

      INTEGER               NI
      PARAMETER           ( NI      =   6 )

      INTEGER               DTYPE
      PARAMETER           ( DTYPE   =   9 )

      INTEGER               FPRINT
      PARAMETER           ( FPRINT  =  32 )

      INTEGER               LPRINT
      PARAMETER           ( LPRINT  = 126 )

      INTEGER               STATSZ
      PARAMETER           ( STATSZ  =   6 )

      INTEGER               DIRSIZ
      PARAMETER           ( DIRSIZ  = 100 )

!
!     Local variables
!
      DOUBLE PRECISION      DCD    ( ND )
      DOUBLE PRECISION      DESCR  ( NS )
      DOUBLE PRECISION      MAXTIM

      INTEGER               CHRCOD
      INTEGER               I
      INTEGER               ICD    ( NI )
      INTEGER               REFCOD


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SPKW09' )
      END IF

!
!     Get the NAIF integer code for the reference frame.
!
      CALL NAMFRM ( FRAME, REFCOD )

      IF ( REFCOD .EQ. 0 ) THEN

         CALL SETMSG ( 'The reference frame # is not supported.'   )
         CALL ERRCH  ( '#', FRAME                                  )
         CALL SIGERR ( 'SPICE(INVALIDREFFRAME)'                    )
         CALL CHKOUT ( 'SPKW09'                                    )
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
         CALL CHKOUT ( 'SPKW09'                                    )
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
            CALL CHKOUT ( 'SPKW09'                                )
            RETURN

         END IF

      END DO


!
!     Also check to see if the segment identifier is too long.
!
      IF ( LASTNB(SEGID) .GT. SIDLEN ) THEN

         CALL SETMSG ( 'Segment identifier contains more than ' // &
                       '40 characters.'                          )
         CALL SIGERR ( 'SPICE(SEGIDTOOLONG)'                     )
         CALL CHKOUT ( 'SPKW09'                                  )
         RETURN

      END IF


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
         CALL CHKOUT ( 'SPKW09'                                        )
         RETURN

      END IF

!
!     Make sure that the number of states is sufficient to define a
!     polynomial whose degree is DEGREE.
!
      IF ( N .LE. DEGREE ) THEN

         CALL SETMSG ( 'At least # states are required to define a '  // &
                       'polynomial of degree #.  Number of states '   // &
                       'supplied:  #'                                  )
         CALL ERRINT ( '#', DEGREE + 1                                 )
         CALL ERRINT ( '#', DEGREE                                     )
         CALL ERRINT ( '#', N                                          )
         CALL SIGERR ( 'SPICE(TOOFEWSTATES)'                           )
         CALL CHKOUT ( 'SPKW09'                                        )
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
            CALL CHKOUT ( 'SPKW09'                                    )
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
         CALL CHKOUT ( 'SPKW09'                                       )
         RETURN

      ELSE IF (  EPOCHS(N) .LT. LAST ) THEN

         CALL SETMSG ( 'Segment end time # follows last epoch #.'     )
         CALL ERRDP  ( '#',  LAST                                     )
         CALL ERRDP  ( '#',  EPOCHS(N)                                )
         CALL SIGERR ( 'SPICE(BADDESCRTIMES)'                         )
         CALL CHKOUT ( 'SPKW09'                                       )
         RETURN

      END IF


!
!     That concludes the error checks.  Make the segment.
!
!     Store the start and end times to be associated
!     with this segment.
!
      DCD(1) = FIRST
      DCD(2) = LAST

!
!     Create the integer portion of the descriptor.
!
      ICD(1) = BODY
      ICD(2) = CENTER
      ICD(3) = REFCOD
      ICD(4) = DTYPE

!
!     Pack the segment descriptor.
!
      CALL DAFPS ( ND, NI, DCD, ICD, DESCR )

!
!     Begin a new segment.
!
      CALL DAFBNA ( HANDLE, DESCR, SEGID )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SPKW09' )
         RETURN
      END IF
!
!     The type 9 segment structure is eloquently described by this
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
!        | Polynomial degree     |
!        +-----------------------+
!        | Number of states      |
!        +-----------------------+
!
!

      CALL DAFADA ( STATES,  N*STATSZ )
      CALL DAFADA ( EPOCHS,  N        )

      DO I = 1,   (N-1) / DIRSIZ
         CALL DAFADA (  EPOCHS( DIRSIZ*I ),  1  )
      END DO

      CALL DAFADA ( DBLE(DEGREE),   1 )
      CALL DAFADA ( DBLE(N),        1 )

!
!     As long as nothing went wrong, end the segment.
!
      IF ( .NOT. FAILED() ) THEN
         CALL DAFENA
      END IF


      CALL CHKOUT ( 'SPKW09' )
      RETURN
      END
