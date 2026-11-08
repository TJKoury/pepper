!$Procedure PCKW20 ( PCK, write segment, type 20 )

      SUBROUTINE PCKW20 (  HANDLE,  CLSSID,  FRAME,   FIRST, &
                           LAST,    SEGID,   INTLEN,  N, &
                           POLYDG,  CDATA,   ASCALE,  TSCALE, &
                           INITJD,  INITFR                   )

! $ Abstract
!
!     Write a type 20 segment to a PCK file.
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
!     TIME
!     PCK
!     ROTATION
!
! $ Keywords
!
!     ORIENTATION
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'spk20.inc'

      INTEGER               HANDLE
      INTEGER               CLSSID
      CHARACTER*(*)         FRAME
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      LAST
      CHARACTER*(*)         SEGID
      DOUBLE PRECISION      INTLEN
      INTEGER               N
      INTEGER               POLYDG
      DOUBLE PRECISION      CDATA  ( * )
      DOUBLE PRECISION      ASCALE
      DOUBLE PRECISION      TSCALE
      DOUBLE PRECISION      INITJD
      DOUBLE PRECISION      INITFR

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of PCK file open for writing.
!     CLSSID     I   NAIF PCK frame class ID.
!     FRAME      I   Reference frame name.
!     FIRST      I   Start time of interval covered by segment.
!     LAST       I   End time of interval covered by segment.
!     SEGID      I   Segment identifier.
!     INTLEN     I   Length of time covered by logical record (days).
!     N          I   Number of logical records in segment.
!     POLYDG     I   Chebyshev polynomial degree.
!     CDATA      I   Array of Chebyshev coefficients and angles.
!     ASCALE     I   Angular scale of data.
!     TSCALE     I   Time scale of data.
!     INITJD     I   Integer part of begin time (TDB Julian date) of
!                    first record.
!     INITFR     I   Fractional part of begin time (TDB Julian date) of
!                    first record.
!     MAXDEG     P   Maximum allowed degree of Chebyshev expansions.
!     TOLSCL     P   Tolerance scale factor for coverage bound checking.
!
! $ Detailed_Input
!
!     HANDLE   is the DAF handle of a PCK file to which a type 20
!              segment is to be added. The PCK file must be open
!              for writing.
!
!     CLSSID   is the integer NAIF PCK frame class ID code of the
!              reference frame whose orientation relative to its
!              base frame is described by the segment to be
!              created. See the Frames Required Reading for
!              details.
!
!     FRAME    is the NAIF name for a reference frame relative to
!              which the orientation information for CLSSID is
!              specified. This frame is called the "base frame."
!
!     FIRST,
!     LAST     are the start and stop times of the time interval
!              over which the segment defines the orientation of
!              the reference frame identified by CLSSID.
!
!     SEGID    is a segment identifier. A PCK segment identifier
!              may contain up to 40 characters.
!
!     INTLEN   is the length of time, in TDB Julian days, covered
!              by each set of Chebyshev polynomial coefficients
!              (each logical record).
!
!     N        is the number of logical records to be stored in
!              the segment. There is one logical record for each
!              time period. Each logical record contains three
!              sets of Chebyshev coefficients---one for each
!              coordinate---and three position vector components.
!
!     POLYDG   is the degree of each set of Chebyshev
!              polynomials, i.e. the number of Chebyshev
!              coefficients per angle minus one. POLYDG must be
!              less than or equal to the parameter MAXDEG.
!
!     CDATA    is an array containing sets of Chebyshev
!              polynomial coefficients and angles to be placed in
!              the new segment of the PCK file. The Chebyshev
!              coefficients represent Euler angle rates; the
!              angles are values of the Euler angles at each
!              interval midpoint. The angular and time units of
!              the data are defined by the inputs ASCALE and
!              TSCALE, which are described below.
!
!              The Euler angles represent the orientation of the
!              reference frame designated by CLSSID relative to
!              its base frame. The angles, which are numbered
!              according to their ordinal position in the logical
!              records, define a transformation matrix R as
!              follows:
!
!                 R = [ A*ANGLE_3 ]  [ A*ANGLE_2 ]  [ A*ANGLE_1 ]
!                                  3              1              3
!
!              where A is the angular scale ASCALE. Here the
!              notation
!
!                 [ THETA ]
!                          i
!
!              denotes a reference frame rotation of THETA
!              radians in the right-hand sense about the ith
!              coordinate axis. See the Rotation Required Reading
!              for further discussion of this notation.
!
!              The matrix R transforms vectors expressed in the
!              base frame to vectors expressed in the frame
!              associated with CLSSID by left multiplication:
!
!                 V       = R * V
!                  CLSSID        FRAME
!
!              In cases where the frame designated by CLSSID
!              (which we'll abbreviate as "the CLSSID frame") is
!              a body-fixed, right-handed frame with its +Z axis
!              aligned with a body's north pole, the orientation
!              angles are related to right ascension (RA) and
!              declination (DEC) of the CLSSID frame's north
!              pole, and prime meridian orientation (W), by the
!              equations
!
!                 ANGLE_1 * ASCALE = RA   + pi/2 radians
!                 ANGLE_2 * ASCALE = pi/2 - DEC  radians
!                 ANGLE_3 * ASCALE = W           radians
!
!              The coefficients and angles are stored in CDATA in
!              order as follows:
!
!                 the (POLYDG + 1) coefficients for the rate of
!                 the first angle of the first logical record,
!                 followed by the value of the first angle at the
!                 first interval midpoint.
!
!                 the coefficients for the rate of the second
!                 angle of the first logical record, followed by
!                 the value of the second angle at the first
!                 interval midpoint.
!
!                 the coefficients for the rate of the third
!                 angle of the first logical record, followed by
!                 the value of the third angle at the first
!                 interval midpoint.
!
!                 the (degree + 1) coefficients for the rate of
!                 the first angle of the second logical record,
!                 followed by the value of the first angle at the
!                 second interval midpoint.
!
!                 and so on.
!
!              The logical data records are stored contiguously:
!
!                 +----------+
!                 | Record 1 |
!                 +----------+
!                 | Record 2 |
!                 +----------+
!                     ...
!                 +----------+
!                 | Record N |
!                 +----------+
!
!              The contents of an individual record are:
!
!                 +--------------------------------------+
!                 | Coeff set for ANGLE_1 rate           |
!                 +--------------------------------------+
!                 | ANGLE_1                              |
!                 +--------------------------------------+
!                 | Coeff set for ANGLE_2 rate           |
!                 +--------------------------------------+
!                 | ANGLE_2                              |
!                 +--------------------------------------+
!                 | Coeff set for ANGLE_3 rate           |
!                 +--------------------------------------+
!                 | ANGLE_3                              |
!                 +--------------------------------------+
!
!                   Each coefficient set has the structure:
!
!                 +--------------------------------------+
!                 | Coefficient of T_0                   |
!                 +--------------------------------------+
!                 | Coefficient of T_1                   |
!                 +--------------------------------------+
!                                   ...
!                 +--------------------------------------+
!                 | Coefficient of T_POLYDG              |
!                 +--------------------------------------+
!
!              Where T_n represents the Chebyshev polynomial
!              of the first kind of degree n.
!
!     ASCALE,
!     TSCALE   are, respectively, the angular scale of the input
!              angle and angular rate data in radians, and the
!              time scale of the input rate data in TDB
!              seconds.
!
!              For example, if the input angular data have units
!              of degrees, ASCALE should be set to the number of
!              radians in one degree. If the input rate data have
!              time units of Julian days, then TSCALE should be
!              set to the number of seconds per Julian day
!              (86400).
!
!
!     INITJD   is the integer part of the Julian ephemeris date
!              of initial epoch of the first record. INITJD may
!              be less than, equal to, or greater than the
!              initial epoch.
!
!     INITFR   is the fractional part of the Julian ephemeris
!              date of initial epoch of the first record. INITFR
!              has units of Julian days. INITFR has magnitude
!              strictly less than 1 day. The sum
!
!                 INITJD + INITFR
!
!              equals the Julian ephemeris date of the initial
!              epoch of the first record.
!
! $ Detailed_Output
!
!     None. This routine writes data to a PCK file.
!
! $ Parameters
!
!     MAXDEG   is the maximum allowed degree of the input
!              Chebyshev expansions. MAXDEG is declared in the
!              Fortran INCLUDE file pck20.inc.
!
!     TOLSCL   is a tolerance scale factor (also called a
!              "relative tolerance") used for time coverage
!              bound checking. TOLSCL is unitless. TOLSCL
!              produces a tolerance value via the formula
!
!                 TOL = TOLSCL * MAX( ABS(FIRST), ABS(LAST) )
!
!              where FIRST and LAST are the coverage time bounds
!              of a type 20 segment, expressed as seconds past
!              J2000 TDB.
!
!              The resulting parameter TOL is used as a tolerance
!              for comparing the input segment descriptor time
!              bounds to the first and last epoch covered by the
!              sequence of time intervals defined by the inputs
!              to PCKW20:
!
!                 INITJD
!                 INITFR
!                 INTLEN
!                 N
!
!              TOLSCL is declared in the Fortran INCLUDE file
!              pck20.inc.
!
!              See the $Exceptions section below for a description
!              of the error check using this tolerance.
!
! $ Exceptions
!
!     1)  If the number of sets of coefficients is not positive,
!         the error SPICE(INVALIDCOUNT) is signaled.
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
!     8)  If either the angle or time scale is non-positive, the
!         error SPICE(NONPOSITIVESCALE) is signaled.
!
! $ Files
!
!     A new type 20 PCK segment is written to the PCK file attached
!     to HANDLE.
!
! $ Particulars
!
!     This routine writes a PCK type 20 data segment to the designated
!     PCK file, according to the format described in the PCK Required
!     Reading.
!
!     Each segment can contain data for only one reference frame
!     and base frame. The Chebyshev polynomial degree and length
!     of time covered by each logical record are also fixed. However,
!     an arbitrary number of logical records of Chebyshev polynomial
!     coefficients can be written in each segment. Minimizing the
!     number of segments in a PCK file will help optimize how the
!     SPICE system accesses the file.
!
! $ Examples
!
!     Suppose that you have in an array CDATA sets of Chebyshev
!     polynomial coefficients and angles representing the orientation
!     of the moon, relative to the J2000 reference frame, and you want
!     to put these into a type 20 segment in an existing PCK file. The
!     following code could be used to add one new type 20 segment. To
!     add multiple segments, put the call to PCKW20 in a loop.
!
!     C
!     C      First open the PCK file and get a handle for it.
!     C
!            CALL DAFOPW ( PCKNAM, HANDLE )
!
!     C
!     C      Create a segment identifier.
!     C
!            SEGID = 'MY_SAMPLE_PCK_TYPE_20_SEGMENT'
!
!     C
!     C      Note that the interval length INTLEN has units
!     C      of Julian days. The start time of the first record
!     C      is expressed using two inputs: integer and fractional
!     C      portions of the Julian ephemeris date of the start
!     C      time.
!     C
!     C      The PCK frame class ID code is stored in the
!     C      variable CLSSID. This ID must be associated in
!     C      with a PCK frame; usually such an association is
!     C      made via a frame kernel.
!     C
!     C      Write the segment.
!     C
!            CALL PCKW20 ( HANDLE, CLSSID, 'J2000', FIRST,
!          .               LAST,   SEGID,  INTLEN,  N,
!          .               POLYDG, CDATA,  ASCALE,  TSCALE
!          .               INITJD, INITFR                  )
!
!     C
!     C      Close the file.
!     C
!            CALL DAFCLS ( HANDLE )
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
!     K.S. Zukor         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 03-JUN-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 17-JAN-2014 (NJB) (KSZ)
!
!-&


! $ Index_Entries
!
!     write PCK type_20 data segment
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      J2000
      DOUBLE PRECISION      SPD

      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local Parameters
!

!
!     DTYPE is the PCK data type.
!
      INTEGER               DTYPE
      PARAMETER           ( DTYPE   =   20 )

!
!     ND is the number of double precision components in a PCK
!     segment descriptor. PCK uses ND = 2.
!
      INTEGER               ND
      PARAMETER           ( ND      =   2 )

!
!     NI is the number of integer components in a PCK segment
!     descriptor. PCK uses NI = 5.
!
      INTEGER               NI
      PARAMETER           ( NI      =   5 )

!
!     NS is the size of a packed PCK segment descriptor.
!
      INTEGER               NS
      PARAMETER           ( NS      =   5 )

!
!     SIDLEN is the maximum number of characters allowed in an
!     PCK segment identifier.
!
      INTEGER               SIDLEN
      PARAMETER           ( SIDLEN  =  40 )

!
!     Local variables
!
      CHARACTER*(SIDLEN)    ETSTR
      CHARACTER*(SIDLEN)    NETSTR

      DOUBLE PRECISION      BTIME
      DOUBLE PRECISION      DCD   ( ND )
      DOUBLE PRECISION      DESCR ( NS )
      DOUBLE PRECISION      LTIME
      DOUBLE PRECISION      NUMREC
      DOUBLE PRECISION      TOL

      INTEGER               ICD   ( NI )
      INTEGER               NINREC
      INTEGER               REFCOD

!
!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'PCKW20' )

!
!     The number of sets of coefficients must be positive.
!
      IF ( N .LE. 0 ) THEN

         CALL SETMSG ( 'The number of sets of coordinate ' &
         //            'coefficients is not positive. N = #.' )
         CALL ERRINT ( '#', N                                 )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                  )
         CALL CHKOUT ( 'PCKW20'                               )
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
         CALL CHKOUT ( 'PCKW20'                                        )
         RETURN

      END IF

!
!     The interval length must be positive.
!
      IF ( INTLEN .LE. 0 ) THEN

         CALL SETMSG ( 'The interval length is not positive.' &
         //            'N = #'                                )
         CALL ERRDP  ( '#', INTLEN                            )
         CALL SIGERR ( 'SPICE(INTLENNOTPOS)'                  )
         CALL CHKOUT ( 'PCKW20'                               )
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
         CALL CHKOUT ( 'PCKW20'                                  )
         RETURN

      END IF

!
!     The segment stop time must be greater than the begin time.
!
      IF ( FIRST .GE. LAST ) THEN

         CALL SETMSG ( 'The segment start time: # (# TDB) is ' &
         //            'not less than the segment end time: ' &
         //            '(# TDB).'                               )
         CALL ETCAL  ( FIRST, ETSTR                             )
         CALL ERRCH  ( '#',   ETSTR                             )
         CALL ERRDP  ( '#',   FIRST                             )
         CALL ETCAL  ( LAST,  NETSTR                            )
         CALL ERRCH  ( '#',   NETSTR                            )
         CALL ERRDP  ( '#',   LAST                              )
         CALL SIGERR ( 'SPICE(BADDESCRTIMES)'                   )
         CALL CHKOUT ( 'PCKW20'                                 )
         RETURN

      END IF

!
!     The angle and time scales must be positive.
!
      IF ( ASCALE .LE. 0.D0 ) THEN

         CALL SETMSG ( 'The angle scale is not positive.'  // &
                       'ASCALE = #'                           )
         CALL ERRDP  ( '#', ASCALE                            )
         CALL SIGERR ( 'SPICE(NONPOSITIVESCALE)'              )
         CALL CHKOUT ( 'PCKW20'                               )
         RETURN

      END IF

      IF ( TSCALE .LE. 0.D0 ) THEN

         CALL SETMSG ( 'The time scale is not positive.'     // &
                       'TSCALE = #'                           )
         CALL ERRDP  ( '#', TSCALE                            )
         CALL SIGERR ( 'SPICE(NONPOSITIVESCALE)'              )
         CALL CHKOUT ( 'PCKW20'                               )
         RETURN

      END IF

!
!     The begin time of the first record must be less than or equal
!     to the begin time of the segment. Convert the two-part input
!     epoch to seconds past J2000 for the purpose of this check.
!
      BTIME = SPD()  *  (  ( INITJD - J2000() ) + INITFR  )

      LTIME = BTIME  +  ( N * INTLEN * SPD() )

!
!     Compute the tolerance to use for descriptor time bound checks.
!
      TOL = TOLSCL * MAX( ABS(BTIME), ABS(LTIME) )

      IF ( FIRST .LT. BTIME - TOL ) THEN

         CALL SETMSG ( 'The segment descriptor start time # is too ' &
         //            'much less than the beginning time of the ' &
         //            'segment data # (in seconds past J2000: #). ' &
         //            'The difference is # seconds; the ' &
         //            'tolerance is # seconds.'                     )
         CALL ETCAL  ( FIRST, ETSTR                                  )
         CALL ERRCH  ( '#',   ETSTR                                  )
         CALL ETCAL  ( BTIME, ETSTR                                  )
         CALL ERRCH  ( '#',   ETSTR                                  )
         CALL ERRDP  ( '#',   FIRST                                  )
         CALL ERRDP  ( '#',   BTIME-FIRST                            )
         CALL ERRDP  ( '#',   TOL                                    )
         CALL SIGERR ( 'SPICE(COVERAGEGAP)'                          )
         CALL CHKOUT ( 'PCKW20'                                      )
         RETURN

      END IF

!
!     The end time of the final record must be greater than or
!     equal to the end time of the segment.
!
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
         CALL CHKOUT ( 'PCKW20'                                      )
         RETURN

      END IF

!
!     Now check the validity of the segment identifier.
!
      CALL CHCKID ( 'PCK segment identifier', SIDLEN, SEGID )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'PCKW20' )
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
      ICD(1) = CLSSID
      ICD(2) = REFCOD
      ICD(3) = DTYPE

!
!     Pack the segment descriptor.
!
      CALL DAFPS ( ND, NI, DCD, ICD, DESCR )

!
!     Begin a new segment of PCK type 20 form:
!
!        Record 1
!        Record 2
!        ...
!        Record N
!        ASCALE     ( angular scale in radians )
!        TSCALE     ( time scale in seconds )
!        INITJD     ( integer part of initial epoch of first record,
!                     expressed as a TDB Julian date )
!        INITFR     ( fractional part of initial epoch, in units of
!                     TDB Julian days )
!        INTLEN     ( length of interval covered by each record, in
!                     units of TDB Julian days )
!        RSIZE      ( number of data elements in each record )
!        N          ( number of records in segment )
!
!     Each record will have the form:
!
!        ANGLE_1 coefficients
!        ANGLE_1 angle at interval midpoint
!        ANGLE_2 coefficients
!        ANGLE_2 angle at interval midpoint
!        ANGLE_3 coefficients
!        ANGLE_3 angle at interval midpoint
!
!
      CALL DAFBNA ( HANDLE, DESCR, SEGID )

!
!     Calculate the number of entries in a record.
!
      NINREC = ( POLYDG + 2 ) * 3

!
!     Fill segment with N records of data.
!
      CALL DAFADA ( CDATA, N * NINREC )

!
!     Store the angle and time scales.
!
      CALL DAFADA ( ASCALE, 1 )
      CALL DAFADA ( TSCALE, 1 )

!
!     Store the integer and fractional parts of the initial epoch of
!     the first record.
!
      CALL DAFADA ( INITJD, 1 )
      CALL DAFADA ( INITFR, 1 )

!
!     Store the length of interval covered by each record.
!
      CALL DAFADA ( INTLEN, 1 )

!
!     Store the size of each record (total number of array elements).
!     Note that this size is smaller by 2 than the size of a type 2
!     record of the same degree, since the record coverage midpoint
!     and radius are not stored.
!
      CALL DAFADA ( DBLE( NINREC ), 1 )

!
!     Store the number of records contained in the segment.
!
      NUMREC = N
      CALL DAFADA ( NUMREC, 1 )

!
!     End this segment.
!
      CALL DAFENA

      CALL CHKOUT ( 'PCKW20' )
      RETURN
      END
