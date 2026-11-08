!$Procedure SPKW02 ( SPK, write segment, type 2 )

      SUBROUTINE SPKW02 (  HANDLE,  BODY,    CENTER,  FRAME, &
                           FIRST,   LAST,    SEGID,   INTLEN, &
                           N,       POLYDG,  CDATA,   BTIME  )

! $ Abstract
!
!     Write a type 2 segment to an SPK file.
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
!
! $ Keywords
!
!     EPHEMERIS
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'spk02.inc'

      INTEGER               HANDLE
      INTEGER               BODY
      INTEGER               CENTER
      CHARACTER*(*)         FRAME
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      LAST
      CHARACTER*(*)         SEGID
      DOUBLE PRECISION      INTLEN
      INTEGER               N
      INTEGER               POLYDG
      DOUBLE PRECISION      CDATA (*)
      DOUBLE PRECISION      BTIME

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     MAXDEG     P   Maximum degree of Chebyshev expansions.
!     TOLSCL     P   Scale factor used to compute time bound tolerance.
!     HANDLE     I   Handle of SPK file open for writing.
!     BODY       I   NAIF code for ephemeris object.
!     CENTER     I   NAIF code for the center of motion of the body.
!     FRAME      I   Reference frame name.
!     FIRST      I   Start time of interval covered by segment.
!     LAST       I   End time of interval covered by segment.
!     SEGID      I   Segment identifier.
!     INTLEN     I   Length of time covered by logical record.
!     N          I   Number of logical records in segment.
!     POLYDG     I   Chebyshev polynomial degree.
!     CDATA      I   Array of Chebyshev coefficients.
!     BTIME      I   Begin time of first logical record.
!
! $ Detailed_Input
!
!     HANDLE   is the DAF handle of an SPK file to which a type 2
!              segment is to be added. The SPK file must be open for
!              writing.
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
!     LAST     are the start and stop times of the time interval over
!              which the segment defines the state of body.
!
!     SEGID    is the segment identifier. An SPK segment identifier may
!              contain up to 40 characters.
!
!     INTLEN   is the length of time, in seconds, covered by each set of
!              Chebyshev polynomial coefficients (each logical record).
!              Each set of Chebyshev coefficients must cover this fixed
!              time interval, INTLEN.
!
!     N        is the number of sets of Chebyshev polynomial
!              coefficients for coordinates (number of logical records)
!              to be stored in the segment. There is one set of
!              Chebyshev coefficients for each time period.
!
!     POLYDG   is the degree of each set of Chebyshev polynomials used
!              to represent the ephemeris information. That is, the
!              number of Chebyshev coefficients per coordinate minus
!              one. POLYDG must not exceed MAXDEG (see $Parameters
!              below).
!
!     CDATA    is a time-ordered array of N sets of Chebyshev polynomial
!              coefficients to be placed in the segment of the SPK file.
!              Each set has size SETSZ = 3*(POLYDG+1). The coefficients
!              are stored in CDATA in order as follows:
!
!                 the (POLYDG + 1) coefficients for the first
!                 coordinate of the first logical record,
!
!                 the coefficients for the second coordinate,
!
!                 the coefficients for the third coordinate,
!
!                 the coefficients for the first coordinate for
!                 the second logical record, ...
!
!                 and so on.
!
!     BTIME    is the begin time (seconds past J2000 TDB) of first set
!              of Chebyshev polynomial coefficients (first logical
!              record). FIRST is an appropriate value for BTIME.
!
! $ Detailed_Output
!
!     None.
!
!     The routine writes to the SPK file referred to by HANDLE a type 02
!     SPK segment containing the data in CDATA.
!
!     See the $Particulars section for details about the structure of a
!     type 02 SPK segment.
!
! $ Parameters
!
!     See the include file spk02.inc for declarations of the
!     parameters described below.
!
!     TOLSCL   is a tolerance scale for coverage gap at endpoints
!              of the segment coverage interval.
!
!     MAXDEG   is the maximum allowed degree of the input
!              Chebyshev expansions.
!
! $ Exceptions
!
!     1)  If the number of sets of coefficients is not positive,
!         the error SPICE(NUMCOEFFSNOTPOS) is signaled.
!
!     2)  If the interval length is not positive, the error
!         SPICE(INTLENNOTPOS) is signaled.
!
!     3)  If the name of the reference frame is not recognized,
!         the error SPICE(INVALIDREFFRAME) is signaled.
!
!     4)  If segment stop time is not greater then the begin time,
!         the error SPICE(BADDESCRTIMES) is signaled.
!
!     5)  If the start time of the first record exceeds the descriptor
!         begin time by more than a computed tolerance, or if the end
!         time of the last record precedes the descriptor end time by
!         more than a computed tolerance, the error SPICE(COVERAGEGAP)
!         is signaled. See the $Parameters section above for a
!         description of the tolerance.
!
!     6)  If the input degree POLYDG is less than 0 or greater than
!         MAXDEG, the error SPICE(INVALIDDEGREE) is signaled.
!
!     7)  If the last non-blank character of SEGID occurs past index 40,
!         or if SEGID contains any nonprintable characters, an error is
!         signaled by a routine in the call tree of this routine.
!
! $ Files
!
!     A new type 2 SPK segment is written to the SPK file attached
!     to HANDLE.
!
! $ Particulars
!
!     This routine writes an SPK type 2 data segment to the designated
!     SPK file, according to the format described in the SPK Required
!     Reading.
!
!     Each segment can contain data for only one target, central body,
!     and reference frame. The Chebyshev polynomial degree and length
!     of time covered by each logical record are also fixed. However,
!     an arbitrary number of logical records of Chebyshev polynomial
!     coefficients can be written in each segment. Minimizing the
!     number of segments in an SPK file will help optimize how the SPICE
!     system accesses the file.
!
!     The ephemeris data supplied to the type 2 SPK writer is packed
!     into an array as a sequence of records. The logical data records
!     are stored contiguously:
!
!        +----------+
!        | Record 1 |
!        +----------+
!        | Record 2 |
!        +----------+
!            ...
!        +----------+
!        | Record N |
!        +----------+
!
!     The contents of an individual record are:
!
!        +--------------------------------------+
!        | Coeff set for X position component   |
!        +--------------------------------------+
!        | Coeff set for Y position component   |
!        +--------------------------------------+
!        | Coeff set for Z position component   |
!        +--------------------------------------+
!
!     Each coefficient set has the structure:
!
!        +--------------------------------------+
!        | Coefficient of T_0                   |
!        +--------------------------------------+
!        | Coefficient of T_1                   |
!        +--------------------------------------+
!                          ...
!        +--------------------------------------+
!        | Coefficient of T_POLYDG              |
!        +--------------------------------------+
!
!     Where T_n represents the Chebyshev polynomial
!     of the first kind of degree n.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) This example demonstrates how to create an SPK type 2 kernel
!        containing only one segment, given a set of Chebyshev
!        coefficients and their associated epochs.
!
!
!        Example code begins here.
!
!
!              PROGRAM SPKW02_EX1
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
!              CHARACTER*(*)         SPK2
!              PARAMETER           ( SPK2  = 'spkw02_ex1.bsp' )
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
!              INTEGER               CHBDEG
!              PARAMETER           ( CHBDEG = 2  )
!
!              INTEGER               NRECS
!              PARAMETER           ( NRECS  = 4  )
!
!              INTEGER               RECSIZ
!              PARAMETER           ( RECSIZ = 3*(CHBDEG+1) )
!
!        C
!        C     Local variables.
!        C
!              CHARACTER*(NAMLEN)    IFNAME
!              CHARACTER*(NAMLEN)    SEGID
!
!              DOUBLE PRECISION      FIRST
!              DOUBLE PRECISION      INTLEN
!              DOUBLE PRECISION      LAST
!              DOUBLE PRECISION      RECRDS ( RECSIZ, NRECS )
!
!              INTEGER               HANDLE
!              INTEGER               NCOMCH
!
!        C
!        C     Define the coefficients.
!        C
!              DATA                  RECRDS /
!             .                      1.0101D0, 1.0102D0, 1.0103D0,
!             .                      1.0201D0, 1.0202D0, 1.0203D0,
!             .                      1.0301D0, 1.0302D0, 1.0303D0,
!             .                      2.0101D0, 2.0102D0, 2.0103D0,
!             .                      2.0201D0, 2.0202D0, 2.0203D0,
!             .                      2.0301D0, 2.0302D0, 2.0303D0,
!             .                      3.0101D0, 3.0102D0, 3.0103D0,
!             .                      3.0201D0, 3.0202D0, 3.0203D0,
!             .                      3.0301D0, 3.0302D0, 3.0303D0,
!             .                      4.0101D0, 4.0102D0, 4.0103D0,
!             .                      4.0201D0, 4.0202D0, 4.0203D0,
!             .                      4.0301D0, 4.0302D0, 4.0303D0 /
!
!
!        C
!        C     Set the start and end times of interval covered by
!        C     segment, and the length of time covered by logical
!        C     record.
!        C
!              FIRST  = 100.D0
!              LAST   = 500.D0
!              INTLEN = 100.D0
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
!              IFNAME = 'Type 2 SPK internal file name.'
!              SEGID  = 'SPK type 2 test segment'
!
!        C
!        C     Open a new SPK file.
!        C
!              CALL SPKOPN( SPK2, IFNAME, NCOMCH, HANDLE )
!
!        C
!        C     Write the segment.
!        C
!              CALL SPKW02 ( HANDLE, BODY,   CENTER, REF,
!             .              FIRST,  LAST,   SEGID,  INTLEN,
!             .              NRECS,  CHBDEG, RECRDS, FIRST  )
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
!        screen. After run completion, a new SPK type 2 exists in
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
!     B.V. Semenov       (JPL)
!     E.D. Wright        (JPL)
!     K.S. Zukor         (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.0.1, 20-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard. Added complete
!        example code from existing fragment.
!
!        Extended POLYDG and CDATA arguments description to provide the
!        size of the Chebyshev polynomials sets. Moved the details of
!        the SPK structure from CDATA argument description to
!        $Particulars section.
!
!-    SPICELIB Version 2.0.0, 18-JAN-2014 (NJB)
!
!        Relaxed test on relationship between the time bounds of the
!        input record set (determined by BTIME, INTLEN, and N) and the
!        descriptor bounds FIRST and LAST. Now the descriptor bounds
!        may extend beyond the time bounds of the record set by a ratio
!        computed using the parameter TOLSCL (see $Parameters above for
!        details). Added checks on input polynomial degree.
!
!-    SPICELIB Version 1.1.0, 30-OCT-2006 (BVS)
!
!        Removed restriction that the input reference frame should be
!        inertial by changing the routine that determines the frame ID
!        from the name from IRFNUM to NAMFRM.
!
!-    SPICELIB Version 1.0.1, 24-AUG-1998 (EDW)
!
!        Changed a 2 to 2.D0 for a double precision computation. Added
!        some comments to the header. Corrected spelling mistakes.
!
!-    SPICELIB Version 1.0.0, 01-AUG-1995 (KSZ)
!
!-&


! $ Index_Entries
!
!     write SPK type_2 data segment
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local Parameters
!
!     DTYPE is the SPK data type.
!
      INTEGER               DTYPE
      PARAMETER           ( DTYPE   =   2 )
!
!     ND is the number of double precision components in an SPK
!     segment descriptor. SPK uses ND = 2.
!
      INTEGER               ND
      PARAMETER           ( ND      =   2 )
!
!     NI is the number of integer components in an SPK segment
!     descriptor. SPK uses NI = 6.
!
      INTEGER               NI
      PARAMETER           ( NI      =   6 )
!
!     NS is the size of a packed SPK segment descriptor.
!
      INTEGER               NS
      PARAMETER           ( NS      =   5 )
!
!     SIDLEN is the maximum number of characters allowed in an
!     SPK segment identifier.
!
      INTEGER               SIDLEN
      PARAMETER           ( SIDLEN  =  40 )

!
!     Local variables
!
      CHARACTER*(SIDLEN)    ETSTR
      CHARACTER*(SIDLEN)    NETSTR

      DOUBLE PRECISION      DCD   ( ND )
      DOUBLE PRECISION      DESCR ( NS )
      DOUBLE PRECISION      LTIME
      DOUBLE PRECISION      MID
      DOUBLE PRECISION      NUMREC
      DOUBLE PRECISION      RADIUS
      DOUBLE PRECISION      RSIZE
      DOUBLE PRECISION      TOL

      INTEGER               I
      INTEGER               ICD   ( NI )
      INTEGER               K
      INTEGER               NINREC
      INTEGER               REFCOD

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'SPKW02' )

!
!     The number of sets of coefficients must be positive.
!
      IF ( N .LE. 0 ) THEN

         CALL SETMSG ( 'The number of sets of coordinate' &
         //             'coefficients is not positive. N = #' )
         CALL ERRINT ( '#', N                                 )
         CALL SIGERR ( 'SPICE(NUMCOEFFSNOTPOS)'               )
         CALL CHKOUT ( 'SPKW02'                               )
         RETURN

      END IF

!
!     Make sure that the degree of the interpolating polynomials is
!     in range.
!
      IF (  ( POLYDG .LT. 0 ) .OR. ( POLYDG .GT. MAXDEG )  )  THEN

         CALL SETMSG ( 'The interpolating polynomials have degree #; ' &
         //            'the valid degree range is [0, #].'             )
         CALL ERRINT ( '#', POLYDG                                     )
         CALL ERRINT ( '#', MAXDEG                                     )
         CALL SIGERR ( 'SPICE(INVALIDDEGREE)'                          )
         CALL CHKOUT ( 'SPKW02'                                        )
         RETURN

      END IF

!
!     The interval length must be positive.
!
      IF ( INTLEN .LE. 0 ) THEN

         CALL SETMSG ( 'The interval length is not positive.' &
         //            'N = #'                               )
         CALL ERRDP  ( '#', INTLEN                           )
         CALL SIGERR ( 'SPICE(INTLENNOTPOS)'                 )
         CALL CHKOUT ( 'SPKW02'                              )
         RETURN

      END IF

!
!     Get the NAIF integer code for the reference frame.
!
      CALL NAMFRM ( FRAME, REFCOD )

      IF ( REFCOD .EQ. 0 ) THEN

         CALL SETMSG ( 'The reference frame # is not supported.' )
         CALL ERRCH  ( '#', FRAME                                )
         CALL SIGERR ( 'SPICE(INVALIDREFFRAME)'                  )
         CALL CHKOUT ( 'SPKW02'                                  )
         RETURN

      END IF

!
!     The segment stop time must be greater than the begin time.
!
      IF ( FIRST .GT. LAST ) THEN

         CALL SETMSG ( 'The segment start time: # is greater than ' &
         //            'the segment end time: #'                   )
         CALL ETCAL  ( FIRST, ETSTR                                )
         CALL ERRCH  ( '#',   ETSTR                                )
         CALL ETCAL  ( LAST,  NETSTR                               )
         CALL ERRCH  ( '#',   NETSTR                               )
         CALL SIGERR ( 'SPICE(BADDESCRTIMES)'                      )
         CALL CHKOUT ( 'SPKW02'                                    )
         RETURN

      END IF

!
!     Compute the tolerance to use for descriptor time bound checks.
!
      TOL = TOLSCL * MAX( ABS(FIRST), ABS(LAST) )

      IF ( FIRST .LT. BTIME - TOL ) THEN

         CALL SETMSG ( 'The segment descriptor start time # is too ' &
         //            'much less than the beginning time of the  ' &
         //            'segment data # (in seconds past J2000: #). ' &
         //            'The difference is # seconds; the  ' &
         //            'tolerance is # seconds.'                     )
         CALL ETCAL  ( FIRST, ETSTR                                  )
         CALL ERRCH  ( '#',   ETSTR                                  )
         CALL ETCAL  ( BTIME, ETSTR                                  )
         CALL ERRCH  ( '#',   ETSTR                                  )
         CALL ERRDP  ( '#',   FIRST                                  )
         CALL ERRDP  ( '#',   BTIME-FIRST                            )
         CALL ERRDP  ( '#',   TOL                                    )
         CALL SIGERR ( 'SPICE(COVERAGEGAP)'                          )
         CALL CHKOUT ( 'SPKW02'                                      )
         RETURN

      END IF

!
!     The end time of the final record must be greater than or
!     equal to the end time of the segment.
!
      LTIME = BTIME  +  ( N * INTLEN )

      IF ( LAST .GT. LTIME + TOL ) THEN

         CALL SETMSG ( 'The segment descriptor end time # is too ' &
         //            'much greater than the end time of the segment ' &
         //            'data # (in seconds past J2000: #). The ' &
         //            'difference is # seconds; the tolerance is # ' &
         //            'seconds.'                                    )
         CALL ETCAL  ( LAST,  ETSTR                                  )
         CALL ERRCH  ( '#',   ETSTR                                  )
         CALL ETCAL  ( LTIME, ETSTR                                  )
         CALL ERRCH  ( '#',   ETSTR                                  )
         CALL ERRDP  ( '#',   LAST                                   )
         CALL ERRDP  ( '#',   LAST-LTIME                             )
         CALL ERRDP  ( '#',   TOL                                    )
         CALL SIGERR ( 'SPICE(COVERAGEGAP)'                          )
         CALL CHKOUT ( 'SPKW02'                                      )
         RETURN

      END IF

!
!     Now check the validity of the segment identifier.
!
      CALL CHCKID ( 'SPK segment identifier', SIDLEN, SEGID )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SPKW02' )
         RETURN
      END IF

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
!     Begin a new segment of SPK type 2 form:
!
!        Record 1
!        Record 2
!        ...
!        Record N
!        INIT       ( initial epoch of first record )
!        INTLEN     ( length of interval covered by each record )
!        RSIZE      ( number of data elements in each record )
!        N          ( number of records in segment )
!
!     Each record will have the form:
!
!        MID        ( midpoint of time interval )
!        RADIUS     ( radius of time interval )
!        X coefficients, Y coefficients, Z coefficients
!
      CALL DAFBNA ( HANDLE, DESCR, SEGID )

!
!     Calculate the number of entries in a record.
!
      NINREC = ( POLYDG + 1 ) * 3

!
!     Fill segment with N records of data.
!
      DO I = 1, N
!
!        Calculate the midpoint and radius of the time of each
!        record, and put that at the beginning of each record.
!
         RADIUS = INTLEN / 2.D0
         MID    = BTIME + RADIUS + ( I - 1 ) * INTLEN

         CALL DAFADA ( MID,    1)
         CALL DAFADA ( RADIUS, 1)

!
!        Put one set of coefficients into segment.
!
         K = 1 + ( I - 1 ) * NINREC

         CALL DAFADA ( CDATA(K), NINREC )

      END DO

!
!     Store the initial epoch of the first record.
!
      CALL DAFADA ( BTIME, 1 )

!
!     Store the length of interval covered by each record.
!
      CALL DAFADA ( INTLEN, 1 )

!
!     Store the size of each record (total number of array elements).
!
      RSIZE = 2 + NINREC
      CALL DAFADA ( RSIZE, 1 )

!
!     Store the number of records contained in the segment.
!
      NUMREC = N
      CALL DAFADA ( NUMREC, 1 )

!
!     End this segment.
!
      CALL DAFENA

!
!     We're done.  Checkout of error trace.
!
      CALL CHKOUT ( 'SPKW02' )
      RETURN
      END
