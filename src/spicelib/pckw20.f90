!$Procedure PCKW20 ( PCK, write segment, type 20 )

      subroutine pckw20 (  handle,  clssid,  frame,   first, &
                           last,    segid,   intlen,  n, &
                           polydg,  cdata,   ascale,  tscale, &
                           initjd,  initfr                   )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Write a type 20 segment to a PCK file.
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

      implicit none

      include 'spk20.inc'

      integer(ip)           handle
      integer(ip)           clssid
      character*(*)         frame
      real(wp)              first
      real(wp)              last
      character*(*)         segid
      real(wp)              intlen
      integer(ip)           n
      integer(ip)           polydg
      real(wp)              cdata  ( * )
      real(wp)              ascale
      real(wp)              tscale
      real(wp)              initjd
      real(wp)              initfr

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
      real(wp)              j2000
      real(wp)              spd

      logical               failed
      logical               return

!
!     Local Parameters
!

!
!     DTYPE is the PCK data type.
!
      integer(ip)           dtype
      parameter           ( dtype   =   20 )

!
!     ND is the number of double precision components in a PCK
!     segment descriptor. PCK uses ND = 2.
!
      integer(ip)           nd
      parameter           ( nd      =   2 )

!
!     NI is the number of integer components in a PCK segment
!     descriptor. PCK uses NI = 5.
!
      integer(ip)           ni
      parameter           ( ni      =   5 )

!
!     NS is the size of a packed PCK segment descriptor.
!
      integer(ip)           ns
      parameter           ( ns      =   5 )

!
!     SIDLEN is the maximum number of characters allowed in an
!     PCK segment identifier.
!
      integer(ip)           sidlen
      parameter           ( sidlen  =  40 )

!
!     Local variables
!
      character*(sidlen)    etstr
      character*(sidlen)    netstr

      real(wp)              btime
      real(wp)              dcd   ( nd )
      real(wp)              descr ( ns )
      real(wp)              ltime
      real(wp)              numrec
      real(wp)              tol

      integer(ip)           icd   ( ni )
      integer(ip)           ninrec
      integer(ip)           refcod

!
!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

      call chkin ( 'PCKW20' )

!
!     The number of sets of coefficients must be positive.
!
      if ( n .le. 0 ) then

         call setmsg ( 'The number of sets of coordinate ' &
         //            'coefficients is not positive. N = #.' )
         call errint ( '#', n                                 )
         call sigerr ( 'SPICE(INVALIDCOUNT)'                  )
         call chkout ( 'PCKW20'                               )
         return

      end if

!
!     Make sure that the degree of the interpolating polynomials is
!     in range.
!
      if (  ( polydg .lt. 0 ) .or. ( polydg .gt. maxdeg )  )  then

         call setmsg ( 'The interpolating polynomials have degree #; ' &
         //            'the valid degree range is [0, #].'             )
         call errint ( '#', polydg                                     )
         call errint ( '#', maxdeg                                     )
         call sigerr ( 'SPICE(INVALIDDEGREE)'                          )
         call chkout ( 'PCKW20'                                        )
         return

      end if

!
!     The interval length must be positive.
!
      if ( intlen .le. 0 ) then

         call setmsg ( 'The interval length is not positive.' &
         //            'N = #'                                )
         call errdp  ( '#', intlen                            )
         call sigerr ( 'SPICE(INTLENNOTPOS)'                  )
         call chkout ( 'PCKW20'                               )
         return

      end if

!
!     Get the NAIF integer code for the reference frame.
!
      call namfrm ( frame, refcod )

      if ( refcod .eq. 0 ) then

         call setmsg ( 'The reference frame # is not supported.' )
         call errch  ( '#', frame                                )
         call sigerr ( 'SPICE(INVALIDREFFRAME)'                  )
         call chkout ( 'PCKW20'                                  )
         return

      end if

!
!     The segment stop time must be greater than the begin time.
!
      if ( first .ge. last ) then

         call setmsg ( 'The segment start time: # (# TDB) is ' &
         //            'not less than the segment end time: ' &
         //            '(# TDB).'                               )
         call etcal  ( first, etstr                             )
         call errch  ( '#',   etstr                             )
         call errdp  ( '#',   first                             )
         call etcal  ( last,  netstr                            )
         call errch  ( '#',   netstr                            )
         call errdp  ( '#',   last                              )
         call sigerr ( 'SPICE(BADDESCRTIMES)'                   )
         call chkout ( 'PCKW20'                                 )
         return

      end if

!
!     The angle and time scales must be positive.
!
      if ( ascale .le. 0.0_wp ) then

         call setmsg ( 'The angle scale is not positive.'  // &
                       'ASCALE = #'                           )
         call errdp  ( '#', ascale                            )
         call sigerr ( 'SPICE(NONPOSITIVESCALE)'              )
         call chkout ( 'PCKW20'                               )
         return

      end if

      if ( tscale .le. 0.0_wp ) then

         call setmsg ( 'The time scale is not positive.'     // &
                       'TSCALE = #'                           )
         call errdp  ( '#', tscale                            )
         call sigerr ( 'SPICE(NONPOSITIVESCALE)'              )
         call chkout ( 'PCKW20'                               )
         return

      end if

!
!     The begin time of the first record must be less than or equal
!     to the begin time of the segment. Convert the two-part input
!     epoch to seconds past J2000 for the purpose of this check.
!
      btime = spd()  *  (  ( initjd - j2000() ) + initfr  )

      ltime = btime  +  ( n * intlen * spd() )

!
!     Compute the tolerance to use for descriptor time bound checks.
!
      tol = tolscl * max( abs(btime), abs(ltime) )

      if ( first .lt. btime - tol ) then

         call setmsg ( 'The segment descriptor start time # is too ' &
         //            'much less than the beginning time of the ' &
         //            'segment data # (in seconds past J2000: #). ' &
         //            'The difference is # seconds; the ' &
         //            'tolerance is # seconds.'                     )
         call etcal  ( first, etstr                                  )
         call errch  ( '#',   etstr                                  )
         call etcal  ( btime, etstr                                  )
         call errch  ( '#',   etstr                                  )
         call errdp  ( '#',   first                                  )
         call errdp  ( '#',   btime-first                            )
         call errdp  ( '#',   tol                                    )
         call sigerr ( 'SPICE(COVERAGEGAP)'                          )
         call chkout ( 'PCKW20'                                      )
         return

      end if

!
!     The end time of the final record must be greater than or
!     equal to the end time of the segment.
!
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
         call chkout ( 'PCKW20'                                      )
         return

      end if

!
!     Now check the validity of the segment identifier.
!
      call chckid ( 'PCK segment identifier', sidlen, segid )

      if ( failed() ) then
         call chkout ( 'PCKW20' )
         return
      end if

!
!     Store the start and end times to be associated
!     with this segment.
!
      dcd(1) = first
      dcd(2) = last

!
!     Create the integer portion of the descriptor.
!
      icd(1) = clssid
      icd(2) = refcod
      icd(3) = dtype

!
!     Pack the segment descriptor.
!
      call dafps ( nd, ni, dcd, icd, descr )

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
      call dafbna ( handle, descr, segid )

!
!     Calculate the number of entries in a record.
!
      ninrec = ( polydg + 2 ) * 3

!
!     Fill segment with N records of data.
!
      call dafada ( cdata, n * ninrec )

!
!     Store the angle and time scales.
!
      call dafada ( ascale, 1 )
      call dafada ( tscale, 1 )

!
!     Store the integer and fractional parts of the initial epoch of
!     the first record.
!
      call dafada ( initjd, 1 )
      call dafada ( initfr, 1 )

!
!     Store the length of interval covered by each record.
!
      call dafada ( intlen, 1 )

!
!     Store the size of each record (total number of array elements).
!     Note that this size is smaller by 2 than the size of a type 2
!     record of the same degree, since the record coverage midpoint
!     and radius are not stored.
!
      call dafada ( real( ninrec, wp ), 1 )

!
!     Store the number of records contained in the segment.
!
      numrec = n
      call dafada ( numrec, 1 )

!
!     End this segment.
!
      call dafena

      call chkout ( 'PCKW20' )
      return
      end
