!$Procedure SPKW20 ( SPK, write segment, type 20 )

      subroutine spkw20 (  handle,  body,    center,  frame, &
                           first,   last,    segid,   intlen, &
                           n,       polydg,  cdata,   dscale, &
                           tscale,  initjd,  initfr          )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Write a type 20 segment to an SPK file.
!
! $ Required_Reading
!
!     DAF
!     NAIF_IDS
!     TIME
!     SPK
!
! $ Keywords
!
!     EPHEMERIS
!
! $ Declarations

      implicit none

      include 'spk20.inc'

      integer(ip)           handle
      integer(ip)           body
      integer(ip)           center
      character(*)          frame
      real(wp)              first
      real(wp)              last
      character(*)          segid
      real(wp)              intlen
      integer(ip)           n
      integer(ip)           polydg
      real(wp)              cdata  ( * )
      real(wp)              dscale
      real(wp)              tscale
      real(wp)              initjd
      real(wp)              initfr

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of SPK file open for writing.
!     BODY       I   NAIF code for ephemeris object.
!     CENTER     I   NAIF code for the center of motion of the body.
!     FRAME      I   Reference frame name.
!     FIRST      I   Start time of interval covered by segment.
!     LAST       I   End time of interval covered by segment.
!     SEGID      I   Segment identifier.
!     INTLEN     I   Length of time covered by logical record (days).
!     N          I   Number of logical records in segment.
!     POLYDG     I   Chebyshev polynomial degree.
!     CDATA      I   Array of Chebyshev coefficients and positions.
!     DSCALE     I   Distance scale of data.
!     TSCALE     I   Time scale of data.
!     INITJD     I   Integer part of begin time (TDB Julian date) of
!                    first record.
!     INITFR     I   Fractional part of begin time of first record.
!
! $ Detailed_Input
!
!     HANDLE   is the DAF handle of an SPK file to which a type 20
!              segment is to be added. The SPK file must be open
!              for writing.
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
!     LAST     are the start and stop times of the time interval
!              over which the segment defines the state of the
!              object identified by BODY.
!
!     SEGID    is a segment identifier. An SPK segment identifier
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
!              coefficients per coordinate minus one. POLYDG must
!              be less than or equal to the parameter MAXDEG.
!
!     CDATA    is an array containing all the sets of Chebyshev
!              polynomial coefficients and position components to
!              be placed in the new segment of the SPK file.
!              There are three sets of coefficients and position
!              components for each time interval covered by the
!              segment.
!
!              The coefficients and position components are
!              stored in CDATA in order as follows:
!
!                 the (POLYDG + 1) coefficients for the first
!                 coordinate of the first logical record,
!                 followed by the X component of position at the
!                 first interval midpoint. The first coefficient
!                 is that of the constant term of the expansion.
!
!                 the coefficients for the second coordinate,
!                 followed by the Y component of position at the
!                 first interval midpoint.
!
!                 the coefficients for the third coordinate,
!                 followed by the Z component of position at the
!                 first interval midpoint.
!
!                 the coefficients for the first coordinate for
!                 the second logical record, followed by the X
!                 component of position at the second interval
!                 midpoint.
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
!                 | Coeff set for X velocity component   |
!                 +--------------------------------------+
!                 | X position component                 |
!                 +--------------------------------------+
!                 | Coeff set for Y velocity component   |
!                 +--------------------------------------+
!                 | Y position component                 |
!                 +--------------------------------------+
!                 | Coeff set for Z velocity component   |
!                 +--------------------------------------+
!                 | Z position component                 |
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
!     DSCALE,
!     TSCALE   are, respectively, the distance scale of the input
!              position and velocity data in km, and the time
!              scale of the input velocity data in TDB seconds.
!
!              For example, if the input distance data have units
!              of astronomical units (AU), DSCALE should be set
!              to the number of km in one AU. If the input
!              velocity data have time units of Julian days, then
!              TSCALE should be set to the number of seconds per
!              Julian day (86400).
!
!     INITJD   is the integer part of the Julian ephemeris date
!              of initial epoch of the first record. INITJD may
!              be less than, equal to, or greater than the
!              initial epoch.
!
!     INITFR   is the fractional part of the Julian ephemeris date
!              of initial epoch of the first record. INITFR has
!              units of Julian days. INITFR has magnitude
!              strictly less than 1 day. The sum
!
!                 INITJD + INITFR
!
!              equals the Julian ephemeris date of the initial
!              epoch of the first record.
!
! $ Detailed_Output
!
!     None. This routine writes data to an SPK file.
!
! $ Parameters
!
!     The parameters described in this section are declared in the
!     Fortran INCLUDE file spk20.inc
!
!     MAXDEG   is the maximum allowed degree of the input
!              Chebyshev expansions.
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
!
!                 INITJD
!                 INITFR
!                 INTLEN
!                 N
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
!     4)  If segment stop time is not greater than or equal to
!         the begin time, the error SPICE(BADDESCRTIMES) is signaled.
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
!     8)  If either the distance or time scale is non-positive, the
!         error SPICE(NONPOSITIVESCALE) is signaled.
!
! $ Files
!
!     A new type 20 SPK segment is written to the SPK file attached
!     to HANDLE.
!
! $ Particulars
!
!     This routine writes an SPK type 20 data segment to the designated
!     SPK file, according to the format described in the SPK Required
!     Reading.
!
!     Each segment can contain data for only one target, central body,
!     and reference frame. The Chebyshev polynomial degree and length
!     of time covered by each logical record are also fixed. However,
!     an arbitrary number of logical records of Chebyshev polynomial
!     coefficients can be written in each segment. Minimizing the
!     number of segments in an SPK file will help optimize how the
!     SPICE system accesses the file.
!
! $ Examples
!
!     Suppose that you have in an array CDATA sets of Chebyshev
!     polynomial coefficients and position vectors representing the
!     state of the moon (NAIF ID = 301), relative to the Earth-moon
!     barycenter (NAIF ID = 3), in the J2000 reference frame, and you
!     want to put these into a type 20 segment in an existing SPK file.
!     The following code could be used to add one new type 20 segment.
!     To add multiple segments, put the call to SPKW20 in a loop.
!
!     C
!     C      First open the SPK file and get a handle for it.
!     C
!            CALL DAFOPW ( SPKNAM, HANDLE )
!
!     C
!     C      Create a segment identifier.
!     C
!            SEGID = 'MY_SAMPLE_SPK_TYPE_20_SEGMENT'
!
!     C
!     C      Note that the interval length INTLEN has units
!     C      of Julian days. The start time of the first record
!     C      is expressed using two inputs: integer and fractional
!     C      portions of the Julian ephemeris date of the start
!     C      time.
!     C
!     C      Write the segment.
!     C
!            CALL SPKW20 ( HANDLE, 301,    3,      'J2000',
!          .               FIRST,  LAST,   SEGID,  INTLEN,
!          .               N,      POLYDG, CDATA,  DSCALE,
!          .               TSCALE, INITJD, INITFR           )
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
!-    SPICELIB Version 1.0.1, 05-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!        Added missing description of INITFR to $Brief_I/O section.
!
!-    SPICELIB Version 1.0.0, 17-JAN-2017 (NJB) (KSZ)
!
!-&


! $ Index_Entries
!
!     write SPK type_20 data segment
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
!     DTYPE is the SPK data type.
!
      integer(ip)           dtype
      parameter           ( dtype   =   20 )

!
!     ND is the number of double precision components in an SPK
!     segment descriptor. SPK uses ND = 2.
!
      integer(ip)           nd
      parameter           ( nd      =   2 )

!
!     NI is the number of integer components in an SPK segment
!     descriptor. SPK uses NI = 6.
!
      integer(ip)           ni
      parameter           ( ni      =   6 )

!
!     NS is the size of a packed SPK segment descriptor.
!
      integer(ip)           ns
      parameter           ( ns      =   5 )

!
!     SIDLEN is the maximum number of characters allowed in an
!     SPK segment identifier.
!
      integer(ip)           sidlen
      parameter           ( sidlen  =  40 )

!
!     Local variables
!
      character(sidlen)     etstr
      character(sidlen)     netstr

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

      call chkin ( 'SPKW20' )

!
!     The number of sets of coefficients must be positive.
!
      if ( n <= 0 ) then

         call setmsg ( 'The number of sets of coordinate ' &
         //            'coefficients is not positive. N = # ' )
         call errint ( '#', n                                 )
         call sigerr ( 'SPICE(INVALIDCOUNT)'                  )
         call chkout ( 'SPKW20'                               )
         return

      end if

!
!     Make sure that the degree of the interpolating polynomials is
!     in range.
!
      if (  ( polydg < 0 ) .or. ( polydg > maxdeg )  )  then

         call setmsg ( 'The interpolating polynomials have degree #; ' &
         //            'the valid degree range is [0, #].'             )
         call errint ( '#', polydg                                     )
         call errint ( '#', maxdeg                                     )
         call sigerr ( 'SPICE(INVALIDDEGREE)'                          )
         call chkout ( 'SPKW20'                                        )
         return

      end if

!
!     The interval length must be positive.
!
      if ( intlen <= 0 ) then

         call setmsg ( 'The interval length is not positive.' &
         //            'N = #'                                )
         call errdp  ( '#', intlen                            )
         call sigerr ( 'SPICE(INTLENNOTPOS)'                  )
         call chkout ( 'SPKW20'                               )
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
         call chkout ( 'SPKW20'                                    )
         return

      end if

!
!     The segment stop time must be greater than or equal to the begin
!     time.
!
      if ( first > last ) then

         call setmsg ( 'The segment start time: # (# TDB) is ' &
         //            'greater than the segment end time: ' &
         //            '(# TDB).'                               )
         call etcal  ( first, etstr                             )
         call errch  ( '#',   etstr                             )
         call errdp  ( '#',   first                             )
         call etcal  ( last,  netstr                            )
         call errch  ( '#',   netstr                            )
         call errdp  ( '#',   last                              )
         call sigerr ( 'SPICE(BADDESCRTIMES)'                   )
         call chkout ( 'SPKW20'                                 )
         return

      end if

!
!     The distance and time scales must be positive.
!
      if ( dscale <= 0.0_wp ) then

         call setmsg ( 'The distance scale is not positive.' &
         //            'DSCALE = #'                           )
         call errdp  ( '#', dscale                            )
         call sigerr ( 'SPICE(NONPOSITIVESCALE)'              )
         call chkout ( 'SPKW20'                               )
         return

      end if

      if ( tscale <= 0.0_wp ) then

         call setmsg ( 'The time scale is not positive.' &
         //            'TSCALE = #'                      )
         call errdp  ( '#', tscale                       )
         call sigerr ( 'SPICE(NONPOSITIVESCALE)'         )
         call chkout ( 'SPKW20'                          )
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

      if ( first < btime - tol ) then

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
         call chkout ( 'SPKW20'                                      )
         return

      end if

!
!     The end time of the final record must be greater than or
!     equal to the end time of the segment.
!
      if ( last > ltime + tol ) then

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
         call chkout ( 'SPKW20'                                      )
         return

      end if

!
!     Now check the validity of the segment identifier.
!
      call chckid ( 'SPK segment identifier', sidlen, segid )

      if ( failed() ) then
         call chkout ( 'SPKW20' )
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
      icd(1) = body
      icd(2) = center
      icd(3) = refcod
      icd(4) = dtype

!
!     Pack the segment descriptor.
!
      call dafps ( nd, ni, dcd, icd, descr )

!
!     Begin a new segment of SPK type 20 form:
!
!        Record 1
!        Record 2
!        ...
!        Record N
!        DSCALE     ( distance scale in km )
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
!        X coefficients
!        X position component at interval midpoint
!        Y coefficients
!        Y position component at interval midpoint
!        Z coefficients
!        Z position component at interval midpoint
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
!     Store the distance and time scales.
!
      call dafada ( dscale, 1 )
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

      call chkout ( 'SPKW20' )
      return
      end
