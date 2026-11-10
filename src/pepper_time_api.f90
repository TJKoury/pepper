!> Time conversion and output: ET2UTC, UTC2ET, TIMOUT, TPICTR, DELTET, and the scanner they use (SCANIT).
!>
!> The procedures keep SPICELIB's names and documentation; the context is
!> their first argument (interfaces and type-bound procedures in
!> pepper_instance). Moved here from src/spicelib by
!> tools/modularize_ctx.py; the external procedures with the original
!> interface are in tspice/src/legacy/pepper_legacy_time_api.f90.
submodule (pepper_instance) pepper_time_api

   implicit none

contains

      module procedure deltet
!! ## Abstract
!!
!!     Return the value of Delta ET (ET-UTC) for an input epoch.
!!
!! ## Required Reading
!!
!!     TIME
!!     KERNEL
!!
!! ## Keywords
!!
!!     TIME
!!
!! ## Declarations
!!
!! ## Detailed Input
!!
!!     EPOCH    is the epoch at which "delta ET" is to be computed.
!!              EPOCH may be either UTC or ephemeris seconds past
!!              J2000, as specified by EPTYPE.
!!
!!     EPTYPE   is the type of input epoch. It may be either
!!              of the following:
!!
!!                 'UTC'    UTC seconds past J2000 UTC.
!!
!!                 'ET'     Ephemeris seconds past J2000 TDB,
!!                          also known as barycentric dynamical
!!                          time (TDB).
!!
!! ## Detailed Output
!!
!!     DELTA    is the value of
!!
!!                 "delta ET" = ET - UTC
!!
!!              at the input epoch. This is added to UTC to give
!!              ET, or subtracted from ET to give UTC. The routine
!!              is reversible: that is, given the following calls,
!!
!!                 CALL DELTET ( UTC,      'UTC', DEL1 )
!!                 CALL DELTET ( UTC+DEL1, 'ET',  DEL2 )
!!
!!              the expression
!!
!!                 ( DEL1 .EQ. DEL2 )
!!
!!              is always .TRUE.
!!
!! ## Parameters
!!
!!     None.
!!
!! ## Exceptions
!!
!!     1)  If the input epoch is not recognized, the error
!!         SPICE(INVALIDEPOCH) is signaled.
!!
!!     2)  If the variables necessary for the computation of DELTA
!!         have not been loaded into the kernel pool, the error
!!         SPICE(KERNELVARNOTFOUND) is signaled.
!!
!!     3)  PEPPER: the leapseconds buffer holds all the leapseconds in
!!         the pool (SPICELIB: 200, then the error
!!         SPICE(BUFFERTOOSMALL), documented as SPICE(BUFFEROVERFLOW)).
!!
!! ## Files
!!
!!     None.
!!
!! ## Particulars
!!
!!     The constants necessary for computing the offset are taken
!!     from the kernel pool, where they are assumed to have been
!!     loaded from a kernel file.
!!
!!     The tables are consulted to determine the number of leap seconds
!!     preceding the input epoch. Also, an approximation to the periodic
!!     yearly variation (which has an amplitude of just under two
!!     milliseconds) in the difference between ET and TAI (Atomic Time)
!!     is computed. The final value of Delta ET is given by
!!
!!        Delta ET = ( ET - TAI ) + leap seconds
!!
!! ## Examples
!!
!!     The following example shows how DELTET may be used to convert
!!     from UTC seconds past J2000 to ephemeris seconds past J2000.
!!
!!        CALL DELTET ( UTCSEC, 'UTC', DELTA )
!!        ET = UTCSEC + DELTA
!!
!!     The following example shows how DELTET may be used to convert
!!     from ephemeris seconds past J2000 to UTC seconds past J2000.
!!
!!        CALL DELTET ( ET, 'ET', DELTA )
!!        UTCSEC = ET - DELTA
!!
!!     See the Time required reading time.req for further examples.
!!
!! ## Restrictions
!!
!!     1)  The routines STR2ET and ET2UTC are preferred for conversions
!!         between UTC and ET. This routine is provided mainly as a
!!         utility for STR2ET and ET2UTC.
!!
!!     2)  A leapseconds kernel containing leapseconds and relativistic
!!         terms MUST be loaded prior to calling this subroutine.
!!         Examples demonstrating how to load a kernel pool are included
!!         in the Required Reading file time.req and in the $Examples
!!         section of this header. For more general information about
!!         kernel pools, please consult the Required Reading file
!!         kernel.req.
!!
!! ## Literature References
!!
!!     [1]  "The Astronomical Almanac for the Year 1990," United States
!!          Naval Observatory, U.S. Government Printing Office,
!!          Washington, D.C., 1989.
!!
!! ## Author and Institution
!!
!!     N.J. Bachman       (JPL)
!!     J. Diaz del Rio    (ODC Space)
!!     W.M. Owen          (JPL)
!!     B.V. Semenov       (JPL)
!!     W.L. Taber         (JPL)
!!     I.M. Underwood     (JPL)
!!
!! ## Version
!!
!!-    SPICELIB Version 1.3.0, 24-AUG-2021 (JDR)
!!
!!        Added IMPLICIT NONE statement.
!!
!!        Edited the header to comply with NAIF standard. Removed
!!        unnecessary entries in $Revisions section.
!!
!!        Replaced reference to UTC2ET with STR2ET in $Restrictions
!!        section.
!!
!!-    SPICELIB Version 1.2.2, 18-APR-2014 (BVS)
!!
!!        Minor header edits.
!!
!!-    SPICELIB Version 1.2.1, 18-MAY-2010 (BVS)
!!
!!        Removed "C$" marker from text in the header.
!!
!!-    SPICELIB Version 1.2.0, 24-AUG-1998 (WLT)
!!
!!        The previous upgrade introduced an error in the fetch
!!        of the variable DELTET/M from the kernel pool. This
!!        error was corrected.
!!
!!-    SPICELIB Version 1.1.0, 20-APR-1998 (NJB)
!!
!!        Calls to RTPOOL were replaced with calls to GDPOOL, which
!!        does more robust error checking. Check for buffer overflow
!!        was added. Local declarations were re-organized.
!!
!!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!!
!!        Comment section for permuted index source lines was added
!!        following the header.
!!
!!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) (IMU)
!!
!! ## Index Entries
!!
!!     difference between ephemeris time and utc
!!
!! ## Revisions
!!
!!-    Beta Version 1.1.0, 06-OCT-1988 (IMU)
!!
!!        Tim Colvin of Rand noticed that times returned by UTC2ET
!!        and TPARSE differed by one second. Upon closer
!!        inspection, crack NAIF staff members deduced that in fact
!!        Mr. Colvin had not loaded the kernel pool, and were
!!        surprised to learn that no error had occurred.
!!
!!        Multiple FOUND flags and a bevy of new error messages
!!        were implemented to cope with this unfortunate oversight.
!!
!!     SPICELIB functions
!!
!!     Local parameters
!!
!!     Local variables
!!
!!     Saved variables
!!
!!     Initial values
!!
!!     Standard SPICE error handling.
!!
      use pepper_kinds, only: ip, wp
      use pepper_strings, only: ucase
      use pepper_containers, only: grow
      implicit none
      real(wp) :: eb_1(1)  !! one-element copy of eb (array argument)
      real(wp) :: k_1(1)  !! one-element copy of k (array argument)
      real(wp) :: dta_1(1)  !! one-element copy of dta (array argument)
      character(4)          type
      character(1)          dtype
      real(wp)              aet
      real(wp)              dta
      real(wp)              ea
      real(wp)              eb
      real(wp)              et
      real(wp)              ettai
      real(wp)              k
      real(wp)              leaps
      real(wp)              m    ( 0:1 )
      real(wp)              ma
      integer(ip)           i
      integer(ip)           n
      integer(ip)           nleap
      logical               found    ( 5 )
      if ( .not. allocated ( ctx%deltet_state ) ) allocate ( ctx%deltet_state )
      associate ( missed => ctx%deltet_state%missed )
      if ( ctx%failed() ) return

!
!     Convert the epoch type to uppercase, to simplify comparisons.
!
      call ucase ( eptype, type )

!
!     Extract the necessary constants from the kernel pool.
!     Leap seconds and their epochs are interleaved in DELTA_AT.
!
!     DLEAP(1,i) is the number of leap seconds at DLEAP(2,i) UTC
!     seconds past J2000.
!
      call gdpool ( ctx, 'DELTET/DELTA_T_A', 1, 1, n, dta_1, found(1) )
      dta = dta_1(1)
      call gdpool ( ctx, 'DELTET/K', 1, 1, n, k_1, found(2) )
      k = k_1(1)
      call gdpool ( ctx, 'DELTET/EB', 1, 1, n, eb_1, found(3) )
      eb = eb_1(1)
      call gdpool ( ctx, 'DELTET/M',         1, 2, n, m,   found(4) )

!
!     PEPPER: the buffer grows to hold the leapseconds (SPICELIB: 200,
!     SPICE(BUFFERTOOSMALL)).
!
      call dtpool ( ctx, 'DELTET/DELTA_AT', found(5), nleap, dtype )

      if ( .not. found(5) ) nleap = 0

      call grow ( ctx%deltet_state%dleap, max ( (nleap + 1) / 2, 1_ip ), 2_ip )

      associate ( dleap => ctx%deltet_state%dleap )

      call gdpool ( ctx, 'DELTET/DELTA_AT',  1,  size( dleap, kind=ip ), &
                     nleap,             dleap(1,1),   found(5) )

      nleap = nleap / 2

      if ( .not. (       found(1) &
                   .and. found(2) &
                   .and. found(3) &
                   .and. found(4) &
                   .and. found(5) ) ) then

         call ctx%setmsg ( 'The following, needed to compute '      // &
                       'Delta ET (ET - UTC), could not be '     // &
                       'found in the kernel pool: #'               )

         do i = 1, 5
            if ( .not. found(i) ) then
               call ctx%errch ( '#', missed(i) )
            end if
         end do

         call ctx%errch  ( ', #', '.' )
         call ctx%sigerr ( 'SPICE(KERNELVARNOTFOUND)' )

         return

      end if

!
!     There are two separate quantities to be determined. First,
!     the appropriate number of leap seconds. Second, the size of
!     the periodic term ET-TAI.
!

!
!     For epochs before the first leap second, return Delta ET at
!     the epoch of the leap second minus one second.
!
      leaps = dleap(1,1) - 1

!
!     When counting leap seconds for UTC epochs, we can compare
!     directly against the values in DLEAP.
!
      if ( type == 'UTC' ) then

         do i = 1, nleap
            if ( epoch >= dleap(2,i) ) then
               leaps = dleap(1,i)
            end if
         end do

!
!     For ET epochs, things are a little tougher. In order to compare
!     the input epoch against the epochs of the leap seconds, we need
!     to compute ET-TAI at each of the leap epochs. To make sure that
!     the computation is reversible, it is always done at the nearest
!     ET second (the "approximate ET", or AET).
!
!     There must be a hundred ways to do this more efficiently.
!     For now, we'll settle for one that works.
!
      else if ( type == 'ET' ) then

         do i = 1, nleap
            if ( epoch > dleap(2,i) ) then

               aet   = dnint ( dleap(2,i) + dta + dleap(1,i) )

               ma    = m(0) + m(1) * aet
               ea    = ma   + eb   * sin ( ma )
               ettai =        k    * sin ( ea )

               et    = dleap(2,i) + dta + dleap(1,i) + ettai

               if ( epoch >= et ) then
                  leaps = dleap(1,i)
               end if

            end if
         end do

!
!     Uh, those are the only choices.
!
      else

         call ctx%setmsg ( 'Epoch type was #' )
         call ctx%errch  ( '#', type )
         call ctx%sigerr ( 'SPICE(INVALIDEPOCH)' )

         return

      end if

!
!     Add the constant offset, leap seconds, and the relativistic term
!     (as before, computed at the nearest ET second).
!
      if ( type == 'ET' ) then
         aet = dnint ( epoch )

      else if ( type == 'UTC' ) then
         aet = dnint ( epoch + dta + leaps )
      end if

      ma    = m(0) + m(1) * aet
      ea    = ma   + eb   * sin ( ma )
      ettai =        k    * sin ( ea )

      delta = dta + leaps + ettai

      return
      end associate
      end associate
      end procedure deltet

      module procedure et2utc
!! ## Abstract
!!
!!     Convert an input time from ephemeris seconds past J2000
!!     to Calendar, Day-of-Year, or Julian Date format, UTC.
!!
!! ## Required Reading
!!
!!     TIME
!!
!! ## Keywords
!!
!!     TIME
!!
!! ## Declarations
!!
!! ## Detailed Input
!!
!!     ET       is the input epoch, ephemeris seconds past J2000.
!!
!!     FORMAT   is the format of the output time string. It may be
!!              any of the following:
!!
!!                 'C'      Calendar format, UTC.
!!
!!                 'D'      Day-of-Year format, UTC.
!!
!!                 'J'      Julian Date format, UTC.
!!
!!                 'ISOC'   ISO Calendar format, UTC.
!!
!!                 'ISOD'   ISO Day-of-Year format, UTC.
!!
!!     PREC     is the number of digits of precision to which
!!              fractional seconds (for Calendar and Day-of-Year
!!              formats) or days (for Julian Date format) are to
!!              be computed. If PREC is zero or smaller, no decimal
!!              point is appended to the output string. If PREC is
!!              greater than 14, it is treated as 14.
!!
!! ## Detailed Output
!!
!!     UTCSTR   is the output time string equivalent to the input
!!              epoch, in the specified format. Some examples are
!!              shown below.
!!
!!                 'C'      '1986 APR 12 16:31:09.814'
!!                 'D'      '1986-102 // 16:31:12.814'
!!                 'J'      'JD 2446533.18834276'
!!                 'ISOC'   '1987-04-12T16:31:12.814'
!!                 'ISOD'   '1987-102T16:31:12.814'
!!
!!              If an error occurs, UTCSTR is not changed.
!!
!!              Fractional seconds, or for Julian dates, fractional
!!              days, are rounded to the precision level specified
!!              by the input argument PREC.
!!
!!              UTCSTR should be declared to be at least
!!              20 + PREC characters in length to ensure
!!              sufficient room to hold calendar strings
!!              for modern epochs. For epochs prior to
!!              1000 A.D. at least 24 + PREC characters in
!!              length are required to hold the output
!!              calendar string.
!!
!!              For epochs prior to 1000 A.D. Jan 1 calendar
!!              and day of year formats are returned with the
!!              era (A.D. or B.C.) attached to the year. For
!!              example
!!
!!                 '877 A.D. MAR 17 13:29:11.829'
!!                 '471 B.C. Jan 01 12:00:00.000'
!!                 '471 B.C. 001 // 12:00:00.000'
!!
!!              ISO formats do not support the inclusion of an era.
!!              For years prior to 1 A.D. an error will be signaled
!!              if ISO format has been requested.
!!
!! ## Parameters
!!
!!     None.
!!
!! ## Exceptions
!!
!!     1)  If the format for the output string is not recognized, the
!!         error SPICE(INVALIDTIMEFORMAT) is signaled.
!!
!!     2)  If PREC is less than or equal to zero, it is treated as
!!         zero. If PREC is greater than 14, it is treated as 14.
!!
!!     3)  If one of the ISO formats is specified (ISOC or ISOD) but the
!!         year corresponding to ET is prior to 1 A.D. on the Gregorian
!!         Calendar, the error SPICE(YEAROUTOFRANGE) is signaled.
!!
!!     4)  Epochs prior to 15 Oct, 1582 on the Gregorian calendar (the
!!         calendar commonly used in western societies) are returned in
!!         the "extended" Gregorian Calendar. To convert epochs to the
!!         Julian calendar see the SPICELIB routine GR2JUL.
!!
!!     5)  This routine does not attempt to account for variations
!!         in the length of the second that were in effect prior
!!         to Jan 1, 1972. For days prior to that date, we assume
!!         there are exactly 86400 ephemeris seconds. Consequently
!!         the UTC Gregorian calendar strings produced for epochs
!!         prior to Jan 1, 1972 differ from the corresponding
!!         TDB calendar strings by approximately 41.18 seconds.
!!         (TDB Gregorian calendar strings are produced by the
!!         routine ETCAL).
!!
!!     6)  If a leapseconds kernel has not been loaded prior to calling
!!         this routine, an error is signaled by a routine in the
!!         call tree of this routine.
!!
!! ## Files
!!
!!     A leapseconds kernel must be loaded via FURNSH prior to calling
!!     this routine. The kernel need be loaded only once during a program
!!     run.
!!
!! ## Particulars
!!
!!     This routine handles the task of converting a double precision
!!     representation of an epoch to a character string suitable for
!!     human consumption. The more general routine TIMOUT may also be
!!     used to convert ET to time strings.
!!
!! ## Examples
!!
!!     Let the value of ET be -527644192.5403653 ephemeris seconds
!!     past J2000. Assuming that the nominal values in the kernel pool
!!     have not been altered, the following calls
!!
!!        CALL ET2UTC ( ET, 'C', 0, UTCSTR )
!!        CALL ET2UTC ( ET, 'C', 3, UTCSTR )
!!        CALL ET2UTC ( ET, 'D', 5, UTCSTR )
!!        CALL ET2UTC ( ET, 'J', 7, UTCSTR )
!!
!!     produce the following output strings
!!
!!        '1983 APR 13 12:09:14'
!!        '1983 APR 13 12:09:14.274'
!!        '1983-103 // 12:09:14.27400'
!!        'JD 2445438.0064152'
!!
!!     respectively.
!!
!! ## Restrictions
!!
!!     None.
!!
!! ## Literature References
!!
!!     [1]  J. Jespersen and J. Fitz-Randolph, "From Sundials to Atomic
!!          Clocks, Understanding Time and Frequency," Dover
!!          Publications, Inc. New York, 1982.
!!
!! ## Author and Institution
!!
!!     C.H. Acton         (JPL)
!!     N.J. Bachman       (JPL)
!!     J. Diaz del Rio    (ODC Space)
!!     K.R. Gehringer     (JPL)
!!     J.M. Lynch         (JPL)
!!     W.M. Owen          (JPL)
!!     B.V. Semenov       (JPL)
!!     W.L. Taber         (JPL)
!!     I.M. Underwood     (JPL)
!!     E.D. Wright        (JPL)
!!
!! ## Version
!!
!!-    SPICELIB Version 3.0.5, 24-AUG-2021 (JDR)
!!
!!        Edited the header to comply with NAIF standard. Extended $Files
!!        section.
!!
!!-    SPICELIB Version 3.0.4, 06-APR-2009 (NJB)
!!
!!        Header was updated to state that fractional
!!        seconds or days are rounded in the output
!!        string.
!!
!!-    SPICELIB Version 3.0.3, 28-JAN-2008 (BVS)
!!
!!        Fixed typo in the ISOC example string in $Detailed_Output.
!!
!!-    SPICELIB Version 3.0.2, 29-JUL-2003 (NJB) (CHA)
!!
!!        Various header changes were made to improve clarity and
!!        more fully explain the routine's functionality.
!!
!!-    SPICELIB Version 3.0.1, 14-SEP-2000 (EDW)
!!
!!        Added FAILED check after TTRANS call during the calendar "C"
!!        format processing to catch failure signal from TTRANS.
!!        Lack of this check caused CSPICE based programs to core dump
!!        if ET2UTC was called without a leapseconds kernel while
!!        error action was set to RETURN.
!!
!!-    SPICELIB Version 3.0.0, 13-MAR-1996 (WLT)
!!
!!        The construction of the numerical components of the
!!        output string are now handled by the SPICELIB routine
!!        TTRANS.
!!
!!        In addition the routine now supports the ISO formats and
!!        the era associated with an epoch (B.C. or A.D.) in non
!!        ISO formats.
!!
!!-    SPICELIB Version 2.1.0, 11-JUL-1995 (KRG)
!!
!!        Removed some potential compile warnings that could be caused
!!        by truncation of double precision values to integers through
!!        a direct assignment. The direct assignment has been replaced
!!        with a call to the intrinsic function IDINT.
!!
!!-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
!!
!!        Comment section for permuted index source lines was added
!!        following the header.
!!
!!-    SPICELIB Version 2.0.0, 21-MAR-1991 (NJB) (JML)
!!
!!        Two bugs involving rounding errors were corrected. One of
!!        the bugs caused conversion errors of magnitude as large as
!!        1 second. See $Revisions for details.
!!
!!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) (IMU)
!!
!! ## Index Entries
!!
!!     ephemeris time to utc
!!
!! ## Revisions
!!
!!-    SPICELIB Version 3.0.0, 13-MAR-1996 (WLT)
!!
!!        The construction of the numerical components of the
!!        output string are now handled by the SPICELIB routine
!!        TTRANS.
!!
!!        In addition the routine now supports the era associated
!!        with an epoch (B.C. or A.D.)
!!
!!-    SPICELIB Version 2.1.0, 11-JUL-1995 (KRG)
!!
!!        Removed some potential compile warnings that could be
!!        caused by truncation of double precision values to
!!        integers through a direct assignment. The direct
!!        assignment has been replaced with a call to the
!!        intrinsic function IDINT.
!!
!!-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
!!
!!        Comment section for permuted index source lines was
!!        added following the header.
!!
!!-    SPICELIB Version 2.0.0, 21-MAR-1991 (NJB) (JML)
!!
!!        1) In the previous version of this routine, the algorithm
!!        that was used permitted inconsistent conversion of the
!!        integer and fractional parts of the UTC value
!!        corresponding to the input ET value.
!!
!!           In the case where rounding the double precision UTC
!!           time corresponding to the input ET value to PREC
!!           decimal places resulted in a carry (to the integer
!!           portion of the UTC value), the integer portion of the
!!           UTC value was treated correctly, but the fraction was
!!           not always rounded correctly. The specific case where
!!           the problem occurred was when the input ET value
!!           mapped to a UTC time having a fractional part that
!!           rounded up to 1.0 when rounded PREC decimal places,
!!           but that did not round up to 1.0 when rounded to the
!!           nearest PREC+1 decimal places. The set of such
!!           fractions can be represented as
!!
!!             { 1 - EPSILON :      EPSILON  <  5 * ( 10 **
!!             -(PREC+1) ) - and
!!
!!                                  EPSILON  >  5 * ( 10 **
!!                                  -(PREC+2) )
!!
!!                                                                     }
!!
!!           For example, if the input ET mapped to the UTC time
!!
!!              2 JAN 1991 00:34:12.99994,
!!
!!           then a call to this routine with PREC set to 3 would
!!           result in the output
!!
!!              2 JAN 1991 00:34:13.999
!!
!!           instead of the correct value
!!
!!              2 JAN 1991 00:34:13.000
!!
!!           On the other hand, if the input ET mapped to the UTC
!!           time
!!
!!              2 JAN 1991 00:34:12.99996,
!!
!!           then a call to this routine with PREC set to 3 would
!!           result in the correct output.
!!
!!           This error was apparently difficult to generate: it
!!           has never been reported by any SPICELIB users, and was
!!           eventually discovered by NAIF staff.
!!
!!        2) The second bug is somewhat less severe, as far as the
!!        magnitude of the error is concerned. However, it's
!!        easier to generate this error. Namely, in some cases,
!!        the fractional part of the input ET value is rounded to
!!        PREC SIGNIFICANT DIGITS, rather than to PREC decimal
!!        places. The effect of this is that the fraction is
!!        occasionally truncated rather than rounded. For example,
!!        the ET value equivalent to the UTC string
!!
!!              1991 JAN 2 00:34:12.0009
!!
!!           would be converted to
!!
!!              1991 JAN 2 00:34:12.000
!!
!!           instead of the correct value
!!
!!              1991 JAN 2 00:34:12.001
!!
!!           when the input argument PREC was set equal to 3.
!!
!!        The modifications made to solve these problems are as
!!        follows:
!!
!!           1)  The input ET value, after conversion to `UTC
!!               seconds past 2000', is broken up into the sum of a
!!               whole number of seconds and a non-negative,
!!               fractional number of seconds. The fact that the
!!               fractional part is non-negative simplifies the
!!               conversion of the fraction.
!!
!!           2)  The fraction is rounded to PREC decimal places---
!!               that is, to the nearest integer multiple of
!!               10**(-PREC). If the rounding results in a carry,
!!               the whole number portion of the time value is
!!               incremented by 1 second. After this step, the
!!               whole number of seconds correctly accounts for
!!               any necessary rounding of the fraction.
!!
!!           3)  The whole number portion of the time value is
!!           passed through the inverse Muller-Wimberly algorithm
!!           to obtain years, months, days, hours, minutes, and
!!           whole seconds. A small fraction is added to the whole
!!           number to prevent round-off error from occurring when
!!           divisions are performed.
!!
!!           4)  The fraction is converted to a string using the
!!               SPICELIB routine DPSTRF. To ensure that DPSTRF
!!               produces an output string containing PREC decimal
!!               places, an integer is added to the fraction value
!!               before supplying it to DPSTRF. This integer
!!               `anchors' the first significant digit of the input
!!               value in the units place.
!!
!!     SPICELIB functions
!!
!!     Local Functions (Statement Functions)
!!
!!     Local variables
!!
!!     Save everything between calls
!!
!!     Initial values
!!
!!     The function NDIGIT gives the number of digits required to
!!     display a non-negative integer that is less than 10000
!!
!!     Standard SPICE error handling.
!!
      use pepper_kinds, only: ip, wp
      use pepper_strings, only: intstr, prefix, suffix, ucase
      implicit none
      integer(ip)           ndigit
      integer(ip)           day
      character(3), parameter :: mthnam   ( 12 ) = &
         [ character(3) :: 'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', &
           'SEP', 'OCT', 'NOV', 'DEC' ]
      ndigit(day) = min(1,day/1000) + min(1,day/100) + min(1,day/10) + 1
      integer(ip) :: myprec
      integer(ip) :: bsc
      integer(ip) :: emonth
      integer(ip) :: eday
      integer(ip) :: bhr
      real(wp) :: scale
      integer(ip) :: emn
      integer(ip) :: esc
      real(wp) :: tai
      integer(ip) :: i
      real(wp) :: frcsec
      real(wp) :: whlsec
      integer(ip) :: bmonth
      character(80) :: fract
      integer(ip) :: minute
      integer(ip) :: bday
      integer(ip) :: hour
      integer(ip) :: month
      integer(ip) :: second
      integer(ip) :: bmn
      character(80) :: endstr
      integer(ip) :: year
      character(4) :: fmt
      integer(ip) :: ehr
      if ( .not. allocated ( ctx%et2utc_state ) ) allocate ( ctx%et2utc_state )
      associate ( str => ctx%et2utc_state%str, &
                  tvec => ctx%et2utc_state%tvec )
      if ( ctx%failed() ) return

!
!     Convert FORMAT to uppercase for ease of comparison. Make sure it's
!     one of the recognized formats.
!
      call ucase ( format, fmt )

      if (       fmt /= 'J' &
           .and. fmt /= 'C' &
           .and. fmt /= 'D' &
           .and. fmt /= 'ISOD' &
           .and. fmt /= 'ISOC' ) then

         call ctx%setmsg ( 'ET2UTC: Format specification for output ' &
         //            'time string is not recognized. Valid ' &
         //            'specifications are: ''C'', ' &
         //            '''D'', ''J'', ''ISOC'', or ''ISOD''. ' &
         //            'The supplied format was ''#''. ' )

         call ctx%errch  ( '#', format )
         call ctx%sigerr ( 'SPICE(INVALIDTIMEFORMAT)' )
         return

      end if
!
!     Force PREC into an acceptable range
!
      myprec = max ( 0 , min ( 14, prec ))

!
!     If the output is Julian Date, we're ready to go. Remember that
!     the day part of Julian Date already has seven digits built in.
!
      if ( fmt == 'J' ) then

         tvec(1) = et

         call ttrans ( ctx, 'TDB',  'JDUTC',        tvec )

         if ( ctx%failed() ) return

         call dpstrf ( ctx, tvec(1), myprec+7, 'F', str  )
         call prefix ( 'JD', 0,                str  )

         utcstr = str

         return

      end if
!
!     If we've dropped past the IF-THEN block above, we need
!     to construct a calendar format string. First thing to
!     do is convert from ET to TAI.
!

      tai = unitim ( ctx, et, 'TDB', 'TAI' )

!
!     We're going to break up TAI into an integer and a
!     fractional part.  The integer will be the greatest
!     integer less than or equal to TAI, and the fraction
!     will be the difference between TAI and the integer
!     part.  The fraction will always be in the interval
!
!        [0, 1)
!
!     After making this decomposition, we'll adjust the integer
!     and fraction to take rounding into account.  The result
!     of the adjustment is that the fraction will be an integer
!     number of time units of length 10**(-MYPREC) seconds, where
!     the integer is in the range [0, (10**MYPREC)-1].  If the
!     fraction rounds up to 1, the fraction will be set to zero,
!     and the whole number portion of TAI will be incremented.
!
!     Since the integers involved may be too large to represent
!     using the INTEGER data type, we'll represent them with
!     double precision numbers.  We'll use the intrinsic ANINT
!     function to keep round-off from creeping into these d.p.
!     numbers representing integers.
!
!     Find the greatest integer less than or equal to TAI.
!     Recall that INT truncates toward the origin.  If TAI
!     is negative and is not already an integer, the result we
!     want is one less than AINT(TAI).
!
      whlsec = aint ( tai )

      if (        ( tai < 0.0_wp  ) &
            .and. ( tai /= whlsec )  )  then

         whlsec = whlsec - 1.0_wp

      end if

!
!     The fractional part of TAI must be rounded to the
!     nearest multiple of 10**(-MYPREC).  Fractions that are
!     equidistant from two multiples of 10**(-MYPREC) are
!     rounded up.
!
!     To accomplish the rounding, we scale the fraction by
!     10**MYPREC.
!
      scale  = anint (  10.0_wp ** myprec )
      frcsec = anint (  scale * ( tai - whlsec )  )

!
!     If a carry occurred, the fraction becomes zero, and
!     we must increment WHLSEC.
!
      if (  frcsec  ==  scale  ) then

         whlsec = whlsec + 1.0_wp
         frcsec = 0.0_wp

      end if

      frcsec = frcsec / scale

!
!     Now, we let TTRANS handle the transformation to
!     the desired components for output.
!
!     FRCSEC will be converted to a string containing MYPREC digits.
!     This will be done later on when the output string is
!     assembled.
!
      tvec(1) = whlsec

      if ( fmt == 'C' .or. fmt == 'ISOC' ) then

         call ttrans ( ctx, 'TAI', 'YMD', tvec )

         if ( ctx%failed() ) return

         year        = nint( tvec(1) )
         month       = nint( tvec(2) )
         day         = nint( tvec(3) )
         hour        = nint( tvec(4) )
         minute      = nint( tvec(5) )
         second      = nint( tvec(6) )
!
!        The beginning of the string is going to be the year.
!        Depending upon the size of the year, it may or
!        may not have an era label.  However the end of the
!        string has a fixed size.  We set up that portion of the
!        string now.  First fill in the month...
!
         if ( fmt == 'C' ) then
            endstr      = ' MMM 00 00:00:00'
            endstr(2:4) = mthnam(month)
!
!           ... and then fill in the day portion of the string.
!
            eday = 7
            bday = eday - ndigit(day) + 1

            call intstr ( day, endstr(bday:eday) )
            ehr  = 10
            emn  = 13
            esc  = 16

         else

            endstr = '-0M-00T00:00:00'
            eday   =  6
            bday   =  eday - ndigit(day) + 1

            emonth = 3
            bmonth = emonth - ndigit(month) + 1
            call intstr ( month, endstr(bmonth:emonth) )
            call intstr ( day,   endstr(bday:eday)     )
            ehr  =  9
            emn  = 12
            esc  = 15

         end if

      else
!
!        We must have day of year format.  Convert TAI to that
!        format.
!

         call ttrans ( ctx, 'TAI', 'YD', tvec )

         if ( ctx%failed() ) return

         year   = nint( tvec(1) )
         month  = 1
         day    = nint( tvec(2) )
         hour   = nint( tvec(3) )
         minute = nint( tvec(4) )
         second = nint( tvec(5) )
!
!        As in the previous case, the end of the output string will
!        have a fixed size.  We fill in the day portion of the string
!        now.  Note that we have set things up so that the hour,
!        minutes and seconds appear in the same location in both
!        day of year and calendar format of strings.
!
         if ( fmt == 'D' ) then
            endstr = '-000 // 00:00:00'
            eday   = 4
            bday   = eday - ndigit(day) + 1

            call intstr ( day, endstr(bday:eday) )
            ehr  = 10
            emn  = 13
            esc  = 16
         else
            endstr = '-000T00:00:00'
            eday   = 4
            bday   = eday - ndigit(day) + 1

            call intstr ( day, endstr(bday:eday) )
            ehr  = 7
            emn  = 10
            esc  = 13
         end if
      end if
!
!     Fill out the hours, minutes and integer portion of
!     seconds in the output string.
!
      bhr  = ehr - ndigit(hour) + 1
      bmn  = emn - ndigit(minute) + 1
      bsc  = esc - ndigit(second) + 1

      call intstr ( hour,   endstr(bhr:ehr) )
      call intstr ( minute, endstr(bmn:emn) )
      call intstr ( second, endstr(bsc:esc) )
!
!     Append the fractional part of the seconds component.
!
      if ( myprec > 0 ) then

!
!        DPSTRF gives MYPREC significant digits in the output,
!        not necessarily MYPREC digits to the right of the
!        decimal point.  We will add a one-digit integer to
!        FRCSEC to `anchor' the first significant digit of
!        FRCSEC in a known place.  That way, we can get DPSTRF
!        to give us a known number of digits after the decimal
!        point.
!
!        The integer part of FRCSEC will not affect the output
!        string.
!
         frcsec = frcsec + 1.0_wp

         call dpstrf ( ctx, frcsec, myprec+1, 'F', fract )

         i = index ( fract, '.' )
         endstr(esc+1: ) = fract(i:i+myprec)

      end if
!
!     The end of the time string is now complete.  We need to
!     construct the year portion of the string. We are going to
!     append an era if the year is before 1000 A.D. Note that
!     we make sure the first character in the ending string
!     is a blank (' ') if the era is to be attached.  Otherwise
!     we'd get confusing day of year formats like
!     999 A.D.-019 // 12:13:18.
!
      if ( year >= 1000 ) then

         call   intstr( year, str )

      else if ( year > 0 ) then

         call intstr ( year,      str )

         if ( fmt == 'C' .or. fmt == 'D' ) then
            call suffix ( 'A.D.', 1, str )
            endstr(1:1) = ' '
         end if

      else if ( year <= 0 ) then

         if ( fmt == 'C' .or. fmt == 'D' ) then
            year = -year + 1

            call intstr ( year,      str )
            call suffix ( 'B.C.', 1, str )

            endstr(1:1) = ' '
         else

            year = -year + 1
            call ctx%setmsg ( 'The year of the ET epoch supplied is ' &
            //            '# B.C.  Years in this era are not ' &
            //            'supported in ISO format. ' )
            call ctx%errint ( '#', year )
            call ctx%sigerr ( 'SPICE(YEAROUTOFRANGE)'  )
            return

         end if

      end if

!
!     Finally append the ENDSTR to STR to get the fully formatted
!     string.
!
      call suffix ( endstr, 0, str )

      utcstr = str

      return
      end associate
      end procedure et2utc

!$Procedure SCANIT ( Scan a character string )
!
!     PEPPER: SPICELIB's umbrella routine SCANIT is not a procedure any more:
!     its entry points are the module procedures below, and its saved
!     variables are in pepper_state_scanit. Its documentation follows.
!


! $ Abstract
!
!     This routine serves as an umbrella routine for routines
!     that are used to scan a string for recognized and unrecognized
!     substrings.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     PARSE
!     SEARCH
!
! $ Declarations



! $ Detailed_Input
!
!     STRING   is any character string that is to be scanned
!              to locate recognized and unrecognized substrings.
!
!     ROOM     is the amount of space available for storing the
!              results of scanning the string.
!
!     NMARKS   is the number of marks that will be
!              recognized substrings of STRING.
!
!     MARKS    is an array of marks that will be recognized
!              by the scanning routine. The array must be
!              processed by a call to SCANPR before it can
!              be used by SCAN. Further details are given
!              in documentation for the individual entry points.
!
!     MRKLEN   is an auxiliary array populated by SCANPR
!              for use by SCAN. It should be declared with
!              length equal to the length of MARKS.
!
!     PNTERS   is an auxiliary array populated by SCANPR for
!              use by SCAN. It should be declared in the
!              calling program as
!
!                 INTEGER  PNTERS ( RCHARS )
!
!              RCHARS is given by the expression
!
!                MAX - MIN + 5
!
!              where
!
!              MAX is the maximum value of ICHAR(MARKS(I)(1:1))
!                  over the range I = 1, NMARKS
!
!              MIN is the minimum value of ICHAR(MARKS(I)(1:1))
!                  over the range I = 1, NMARKS
!
!               Further details are provided in the entry point
!               SCANPR.
!
!     START    is the position in the STRING from which scanning
!              should commence.
!
! $ Detailed_Output
!
!     NMARKS   is the number of marks in the array MARKS after it
!              has been prepared for SCANPR.
!
!     MARKS    is an array of recognizable substrings that has
!              been prepared for SCAN by SCANPR. Note that MARKS
!              will be sorted in increasing order.
!
!     MRKLEN   is an auxiliary array, populated by SCANPR for
!              use by SCAN.
!
!     PNTERS   is an auxiliary array, populated by a call to
!              SCANPR and is intended for use by SCAN.
!
!     START    is the position from which scanning should continue
!              in order to fully scan STRING (if sufficient memory was
!              not provided in BEG, END, and IDENT on the current
!              call to SCAN).
!
!     NTOKNS   is the number of substrings identified in the current
!              scan of STRING.
!
!     BEG      beginnings of scanned substrings.
!              This should be declared so that it is at least
!              as large as ROOM.
!
!     END      endings of scanned substrings.
!              This should be declared so that it is at least
!              as large as ROOM.
!
!     IDENT    positions of scanned substring within array MARKS.
!              If the substring STRING(BEG(I):END(I)) is not in the
!              list of MARKS then IDENT(I) will have the value 0.
!              This should be declared so that it is at least
!              as large as ROOM.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If this routine is called directly, the error
!         SPICE(BOGUSENTRY) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine serves as an umbrella routine for the two entry
!     points SCANPR and SCAN. It can be used to locate keywords
!     or delimited substrings within a string.
!
!     The process of breaking a string into those substrings that
!     have recognizable meaning, is called "scanning." The substrings
!     identified by the scanning process are called "tokens."
!
!     Scanning has many applications including:
!
!     -- the parsing of algebraic expressions
!
!     -- parsing calendar dates
!
!     -- processing text with embedded directions for displaying
!        the text.
!
!     -- interpretation of command languages
!
!     -- compilation of programming languages
!
!     This routine simplifies the process of scanning a string for
!     its tokens.
!
! $ Examples
!
!     Example 1.
!     ----------
!
!     Suppose you need to identify all of the words within a string
!     and wish to ignore punctuation marks such as ',', ':', ';', ' ',
!     '---'.
!
!     The first step is to load the array of marks as shown here:
!
!        The minimum ASCII code for the first character of a marker is
!        32 ( for ' ').
!
!        INTEGER               FCHAR
!        PARAMETER           ( FCHAR = 32 )
!
!        The maximum ASCII code for the first character of a marker is
!        59 (for ';' )
!
!        INTEGER               LCHAR
!        PARAMETER           ( LCHAR = 59 )
!
!        INTEGER               RCHAR
!        PARAMETER           ( RCHAR = LCHAR - FCHAR + 5 )
!
!        LOGICAL               FIRST
!        CHARACTER*(3)         MARKS
!        INTEGER               NMARKS ( 5     )
!        INTEGER               MRKLEN ( 5     )
!        INTEGER               PNTERS ( RCHAR )
!
!        INTEGER               ROOM
!        PARAMETER           ( ROOM = 50 )
!
!        INTEGER               BEG    ( ROOM  )
!        INTEGER               END    ( ROOM  )
!        INTEGER               IDENT  ( ROOM  )
!
!        SAVE                  FIRST
!        SAVE                  MARKS
!        SAVE                  MRKLEN
!        SAVE                  PNTERS
!
!        IF ( FIRST ) THEN
!
!           FIRST    = .FALSE.
!
!           MARKS(1) = ' '
!           MARKS(2) = '---'
!           MARKS(3) = ':'
!           MARKS(4) = ','
!           MARKS(5) = ';'
!
!           NMARKS   = 5
!
!           CALL SCANPR ( NMARKS, MARKS, MRKLEN, PNTERS )
!
!        END IF
!
!     Notice that the call to SCANPR is nested inside an
!     IF ( FIRST ) THEN ... END IF block. In this and many applications
!     the marks that will be used in the scan are fixed. Since the
!     marks are not changing, you need to process MARKS and set up
!     the auxiliary arrays MRKLEN and PNTERS only once (assuming that
!     you SAVE the appropriate variables as has been done above).
!     In this way if the code is executed many times, there is only
!     a small overhead required for preparing the data so that it
!     can be used efficiently in scanning.
!
!     To identify the substrings that represent words we scan the
!     string using the prepared MARKS, MRKLEN and PNTERS.
!
!        CALL SCAN ( STRING, MARKS,  MRKLEN, PNTERS, ROOM,
!       .            START,  NTOKNS, IDENT,  BEG,    END   )
!
!     To isolate only the words of the string, we examine the
!     array IDENT and keep only those Begin and Ends for which
!     the corresponding identity is non-positive.
!
!        KEPT = 0
!
!        DO I = 1, NTOKNS
!
!           IF ( IDENT(I) .LE. 0 ) THEN
!
!              KEPT      = KEPT + 1
!              BEG(KEPT) = BEG(I)
!              END(KEPT) = END(I)
!
!           END IF
!
!        END DO
!
!     Example 2.
!     ----------
!
!     To parse an algebraic expression such as
!
!        ( X + Y ) * ( 2*Z + SIN(W) ) ** 2
!
!     You would select '**', '*', '+', '-', '(', ')' and ' '
!     to be the markers. Note that all of these begin with one
!     of the characters in the string ' !"#$%&''()*+,-./'
!     so that we can declare PNTERS to have length 20.
!
!     Prepare the MARKS, MRKLEN, and PNTERS.
!
!        LOGICAL               FIRST
!        CHARACTER*(4)         MARKS
!        INTEGER               NMARKS ( 8  )
!        INTEGER               MRKLEN ( 8  )
!        INTEGER               PNTERS ( 20 )
!
!        SAVE                  FIRST
!        SAVE                  MARKS
!        SAVE                  MRKLEN
!        SAVE                  PNTERS
!
!        IF ( FIRST ) THEN
!
!           MARKS(1) = '('
!           MARKS(2) = ')'
!           MARKS(3) = '+'
!           MARKS(4) = '-'
!           MARKS(5) = '*'
!           MARKS(6) = '/'
!           MARKS(7) = '**'
!           MARKS(8) = ' '
!
!           NMARKS   = 8
!
!           CALL SCANPR ( NMARKS, MARKS, MRKLEN, PNTERS )
!
!           Locate the blank character in MARKS once it has
!           been prepared.
!
!           BLANK = BSRCHC ( ' ', NMARKS, MARKS )
!
!        END IF
!
!     Once all of the initializations are out of the way,
!     we can scan an input string.
!
!        CALL SCAN ( STRING, MARKS,  MRKLEN, PNTERS, ROOM,
!       .            START,  NTOKNS, IDENT,  BEG,    END   )
!
!     Next eliminate any white space that was returned in the
!     list of tokens.
!
!     KEPT = 0
!
!     DO I = 1, NTOKNS
!
!        IF ( IDENT(I) .NE. BLANK ) THEN
!           KEPT        = KEPT + 1
!           BEG  (KEPT) = BEG   (I)
!           END  (KEPT) = END   (I)
!           IDENT(KEPT) = IDENT (I)
!        END IF
!
!     END DO
!
!     Now all of the substrings remaining point to grouping symbols,
!     operators, functions, or variables. Given that the individual
!     "words" of the expression are now in hand, the meaning of the
!     expression is much easier to determine.
!
!     The rest of the routine is left as a non-trivial exercise
!     for the reader.
!
! $ Restrictions
!
!     1)  The array of MARKS, MRKLEN, and PNTERS must be properly
!         formatted prior to calling SCAN. This is accomplished by
!         calling SCANPR.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 26-JUL-1996 (WLT)
!
!-&

! $ Index_Entries
!
!     Scan a string for recognized and unrecognized tokens
!     Parse a string
!
!-&

!
!     SPICELIB functions
!

!
!     Local variables

      module procedure scanpr
!! ## Abstract
!!
!!     Prepare recognized markers and auxiliary arrays for the
!!     routine SCAN.
!!
!! ## Required Reading
!!
!!     None.
!!
!! ## Keywords
!!
!!     PARSING
!!     UTILITY
!!
!! ## Declarations
!!
!!     INTEGER               NMARKS
!!     CHARACTER*(*)         MARKS   ( * )
!!     INTEGER               MRKLEN  ( * )
!!     INTEGER               PNTERS  ( * )
!!
!! ## Detailed Input
!!
!!     NMARKS   is the number of recognized marks that will be
!!              recognized substrings of STRING.
!!
!!     MARKS    is an array of marks that will be recognized
!!              by the scanning routine. Leading and trailing
!!              blanks are not significant. (Except for the
!!              blank character ' ', itself.  After all, some
!!              part of it must be significant.)  Case of the
!!              entries in MARKS is significant. The MARKS
!!              'XX' and 'xx' are regarded as different MARKS.
!!
!! ## Detailed Output
!!
!!     NMARKS   is the number of marks in the array MARKS after it
!!              has been prepared for SCAN.
!!
!!     MARKS    is an array of recognizable substrings.
!!              It has been prepared for use by SCAN
!!              so as to be compatible with the other arrays.
!!              It will be sorted in ascending order, left
!!              justified and contain no duplicate entries.
!!
!!     MRKLEN   is an auxiliary array populated by SCANPR
!!              for use by SCAN that describes MARKS.
!!
!!     PNTERS   is an auxiliary array populated by SCANPR for
!!              use by SCAN. It should be declared in the
!!              calling program as
!!
!!                 INTEGER   PNTERS ( RCHARS )
!!
!!              RCHARS is given by the expression
!!
!!                MAX - MIN + 5
!!
!!              where
!!
!!              MAX is the maximum value of ICHAR(MARKS(I)(1:1))
!!                  over the range I = 1, NMARKS
!!
!!              MIN is the minimum value of ICHAR(MARKS(I)(1:1))
!!                  over the range I = 1, NMARKS
!!
!!              Here are some typical values that may help you avoid
!!              going through the computations above. (This assumes
!!              that ICHAR returns the ASCII code for a character.)
!!
!!              Scanning Situation           RCHAR
!!              ------------------          -------------------
!!              If NMARKS = 1
!!              or all MARKS                   5
!!              begin with the same
!!              character.
!!
!!              All MARKS begin with
!!              one of the characters          20
!!              in the string
!!              ' !"#$%&''()*+,-./'
!!
!!              All MARKS begin with
!!              one of the characters          11
!!              in the string
!!              ':;<=>?@'
!!
!!              All MARKS begin with
!!              one of the characters          37
!!              in the string
!!              ' !"#$%&''()*+,-./:;<=>?@'
!!
!!              All MARKS begin with
!!              an upper case English letter   30
!!
!!              All MARKS begin with a
!!              decimal digit                  14
!!
!!              All Marks begin with a
!!              lower case English letter      30
!!
!!              All Marks begin with
!!              a digit or upper case          47
!!              character.
!!
!!              All Marks begin with a
!!              printing character or          100
!!              a blank.
!!
!!              Anything might be a mark       132
!!
!!              Finally, so you won't have to look it up elsewhere
!!              here are the ASCII codes for the printing
!!              characters and blanks.
!!
!!              (Common Punctuations) Character     ASCII Code
!!                                    -----------   ----------
!!                                    ' ' (space)     32
!!                                    '!'             33
!!                                    '"'             34
!!                                    '#'             35
!!                                    '$'             36
!!                                    '%'             37
!!                                    '&'             38
!!                                    ''''            39
!!                                    '('             40
!!                                    ')'             41
!!                                    '*'             42
!!                                    '+'             43
!!                                    ','             44
!!                                    '-'             45
!!                                    '.'             46
!!                                    '/'             47
!!
!!              (Decimal Digits)      Character     ASCII Code
!!                                    -----------   ----------
!!                                    '0'             48
!!                                    '1'             49
!!                                    '2'             50
!!                                    '3'             51
!!                                    '4'             52
!!                                    '5'             53
!!                                    '6'             54
!!                                    '7'             55
!!                                    '8'             56
!!                                    '9'             57
!!
!!              (More punctuation)    Character     ASCII Code
!!                                    -----------   ----------
!!                                    ':'             58
!!                                    ';'             59
!!                                    '<'             60
!!                                    '='             61
!!                                    '>'             62
!!                                    '?'             63
!!                                    '@'             64
!!
!!              (Uppercase characters)  Character     ASCII Code
!!                                    -----------   ----------
!!                                    'A'             65
!!                                    'B'             66
!!                                    'C'             67
!!                                    'D'             68
!!                                    'E'             69
!!                                    'F'             70
!!                                    'G'             71
!!                                    'H'             72
!!                                    'I'             73
!!                                    'J'             74
!!                                    'K'             75
!!                                    'L'             76
!!                                    'M'             77
!!                                    'N'             78
!!                                    'O'             79
!!                                    'P'             80
!!                                    'Q'             81
!!                                    'R'             82
!!                                    'S'             83
!!                                    'T'             84
!!                                    'U'             85
!!                                    'V'             86
!!                                    'W'             87
!!                                    'X'             88
!!                                    'Y'             89
!!                                    'Z'             90
!!
!!              (More punctuation)    Character     ASCII Code
!!                                    -----------   ----------
!!                                    '['             91
!!                                    '\'             92
!!                                    ']'             93
!!                                    '^'             94
!!                                    '_'             95
!!                                    '`'             96
!!
!!              (Lowercase characters)  Character     ASCII Code
!!                                    -----------   ----------
!!                                    'a'             97
!!                                    'b'             98
!!                                    'c'             99
!!                                    'd'            100
!!                                    'e'            101
!!                                    'f'            102
!!                                    'g'            103
!!                                    'h'            104
!!                                    'i'            105
!!                                    'j'            106
!!                                    'k'            107
!!                                    'l'            108
!!                                    'm'            109
!!                                    'n'            110
!!                                    'o'            111
!!                                    'p'            112
!!                                    'q'            113
!!                                    'r'            114
!!                                    's'            115
!!                                    't'            116
!!                                    'u'            117
!!                                    'v'            118
!!                                    'w'            119
!!                                    'x'            120
!!                                    'y'            121
!!                                    'z'            122
!!
!!              (More punctuation)      Character     ASCII Code
!!                                    -----------   ----------
!!                                    '{'            123
!!                                    '|'            124
!!                                    '}'            125
!!                                    '~'            126
!!
!! ## Parameters
!!
!!     None.
!!
!! ## Exceptions
!!
!!     Error free.
!!
!!     1)  A space is regarded as a special mark. If MARKS(I) = ' ',
!!         then MARKS(I) will match any consecutive sequence of blanks.
!!
!!     2)  If NMARKS is less than or equal to zero, SCAN will always
!!         find a single token, namely the entire string to be scanned.
!!
!! ## Files
!!
!!     None.
!!
!! ## Particulars
!!
!!     This routine prepares the arrays MARKS, MRKLEN and PNTERS
!!     so that they are suitable for input to the routine SCAN.
!!
!!     It is expected that users will need to scan many strings
!!     and that from the programming point of view it is
!!     easiest to simply supply a list of MARKS to a "formatting"
!!     routine such as this so that the strings can then
!!     be efficiently scanned by the routine SCAN. This formatting
!!     is the function of this routine.
!!
!! ## Examples
!!
!!     Suppose you need to identify all of the words within a string
!!     and wish to ignore punctuation marks such as ' ', ',', ':', ';'
!!     '---'.  Then the first step is to load the array of marks as
!!     shown here:
!!
!!        The minimum ASCII code for the first character of a marker is
!!        32 (for ' ').
!!
!!        INTEGER               FCHAR
!!        PARAMETER           ( FCHAR = 32 )
!!
!!        The maximum ASCII code for the first character of a marker is
!!        59 (for ';').
!!
!!        INTEGER               LCHAR
!!        PARAMETER           ( LCHAR = 59 )
!!
!!        The proper size to declare PNTERS is given by the parameter
!!        RCHAR defined in terms of LCHAR and FCHAR.
!!
!!        INTEGER               RCHAR
!!        PARAMETER           ( RCHAR = LCHAR - FCHAR + 5 )
!!
!!        LOGICAL               FIRST
!!        CHARACTER*(4)         MARKS
!!        INTEGER               NMARKS ( 5     )
!!        INTEGER               MRKLEN ( 5     )
!!        INTEGER               PNTERS ( RCHAR )
!!
!!        SAVE                  FIRST
!!        SAVE                  MARKS
!!        SAVE                  MRKLEN
!!        SAVE                  PNTERS
!!
!!        IF ( FIRST ) THEN
!!
!!           FIRST    = .FALSE.
!!
!!           MARKS(1) = ' '
!!           MARKS(2) = '---'
!!           MARKS(3) = ':'
!!           MARKS(4) = ','
!!           MARKS(5) = ';'
!!
!!           NMARKS   = 5
!!
!!           CALL SCANPR ( NMARKS, MARKS, MRKLEN, PNTERS )
!!
!!        END IF
!!
!!     Notice that the call to SCANPR is nested inside an
!!     IF ( FIRST ) THEN ... END IF block. In this and many applications
!!     the marks that will used in the scan are fixed. Since the marks
!!     are not changing, you need to process MARKS and set up
!!     the auxiliary arrays MRKLEN and PNTERS only once (assuming that
!!     you SAVE the appropriate variables as has been done above).
!!     In this way if the code is executed many times, there is only
!!     a small overhead required for preparing the data so that it
!!     can be used efficiently in scanning.
!!
!! ## Restrictions
!!
!!     1)  MRKLEN and PNTERS must be declared to be at least as large
!!         as indicated above. If not, this routine will write
!!         past the ends of these arrays. Much unpleasantness may
!!         ensue in the attempt to debug such problems.
!!
!! ## Literature References
!!
!!     None.
!!
!! ## Author and Institution
!!
!!     J. Diaz del Rio    (ODC Space)
!!     W.L. Taber         (JPL)
!!
!! ## Version
!!
!!-    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR)
!!
!!        Added IMPLICIT NONE statement.
!!
!!        Edited the header to comply with NAIF standard.
!!
!!-    SPICELIB Version 1.0.0, 26-JUL-1996 (WLT)
!!
!! ## Index Entries
!!
!!     Prepare for scanning strings
!!     Prepare for parsing strings
!!
!!     We handle the case where NMARKS is non-positive separately.
!!
      use pepper_kinds, only: ip
      use pepper_arrays, only: rmdupc
      use pepper_strings, only: ljust, rtrim
      implicit none
      logical               return
      character(1)          letter
      integer(ip)           backup
      integer(ip)           eblock
      integer(ip)           fchar
      integer(ip)           finish
      integer(ip)           i
      integer(ip)           intval
      integer(ip)           j
      integer(ip)           jump
      integer(ip)           l
      integer(ip)           last
      integer(ip)           last1
      integer(ip)           lbound
      integer(ip)           lchar
      integer(ip)           n
      integer(ip)           offset
      integer(ip)           slot
      integer(ip)           stop
      integer(ip)           test
      integer(ip)           this1
      integer(ip)           ubound
      logical               equal
      logical               known
      if ( nmarks <= 0 ) then

         pnters(1) = 0
         pnters(2) = 0
         pnters(3) = 0
         pnters(4) = 0
         pnters(5) = 0

         return
      end if
!
!     First left justify MARKS and remove duplicates.
!
      do i = 1, nmarks
         call ljust ( marks(i), marks(i) )
      end do

      n = nmarks

!
!     Sort and remove duplicates from the array MARKS.
!
      call rmdupc ( n, marks )

!
!     All of the MARKS have the same declared length.
!     However, since all of your marks may not have
!     the same intended length (for example '*' and
!     '**') it is desirable to be able to specify
!     how much of MARKS(I) should actually be used
!     when examining STRING for a substring match.
!     This is done with the array MRKLEN.
!     MARKS(I)(1:MRKLEN(I)) will be used when
!     scanning STRING.
!
!     Here is the expected structure of PNTERS.
!
!             PNTERS(1) = MIN ( ICHAR(MARKS(I)(1:1)  ), I=1,NMARKS )
!             PNTERS(2) = MAX ( ICHAR(MARKS(I)(1:1)  ), I=1,NMARKS )
!
!     For ease of further discussion let
!     MYCHAR(I) represent the characters from PNTERS(1)
!     to PNTERS(2), and assume that legitimate values of
!     I are from 1 to M.
!
!             PNTERS(3)   = 0
!             PNTERS(4)   = index of the last entry of MARKS
!                           that begins with the character
!                           MYCHAR(1).
!
!             PNTERS(5)   = index of the last entry of MARKS
!                           that begins with the character
!                           MYCHAR(2), if there is no such element
!                           of MARKS let PNTERS(5) = PNTERS(4)
!                .
!                .
!                .
!
!             PNTERS(3+K) = index of the last entry of MARKS
!                           that begins with the character
!                           MYCHAR(K), if there is no such element
!                           of MARKS, let PNTERS(3+K) =
!                           PNTERS(3+K-1)
!                .
!                .
!                .
!
!             PNTERS(3+M) = index of the last entry of MARKS
!                           that begins with the character
!                           MYCHAR(M).
!
!             PNTERS(4+M) = PNTERS(3+M)
!

!
!     Next determine the minimum and maximum ASCII values
!     of the first characters of the MARKS.
!
      fchar     = ichar ( marks(1)(1:1) )
      lchar     = ichar ( marks(n)(1:1) )

      pnters(1) = fchar
      pnters(2) = lchar

!
!     For the purposes of getting started, we will say the last
!     character that started a MARK was one before FCHAR.  We
!     will record the end of its block in slot 3 of PNTERS.
!
      last1     = fchar - 1
      slot      = 3

      do  i = 1, n

         mrklen(i) = rtrim( marks(i)      )
         this1     = ichar( marks(i)(1:1) )

         if ( this1 /= last1 ) then

!
!           We need to record the address of the end of the last
!           block of MARKS that began with the same character.
!           This is of course one before the current value of I.
!
!           While we are at it, we might as well determine how
!           many possible first letters were "jumped" over in
!           going from the last first character to the current
!           first character.
!
            eblock = i     - 1
            jump   = this1 - last1

!
!           The end of the block for all of the MARKS having
!           first character between the last one and this one
!           is the same.
!
            do j = slot, slot + jump - 1
               pnters(j) = eblock
            end do

            slot  = slot  + jump
            last1 = this1

         end if

      end do

      pnters(slot  ) = n
      pnters(slot+1) = n
      nmarks         = n

      return
      end procedure scanpr

      module procedure scan
!! ## Abstract
!!
!!     Scan a string and return the beginnings and ends of recognized
!!     and unrecognized substrings. The full collection of these
!!     substrings partitions the string.
!!
!! ## Required Reading
!!
!!     None.
!!
!! ## Keywords
!!
!!     PARSING
!!
!! ## Declarations
!!
!!     CHARACTER*(*)         STRING
!!     CHARACTER*(*)         MARKS   ( * )
!!     INTEGER               MRKLEN  ( * )
!!     INTEGER               PNTERS  ( * )
!!     INTEGER               ROOM
!!     INTEGER               START
!!     INTEGER               NTOKNS
!!     INTEGER               BEG     ( * )
!!     INTEGER               END     ( * )
!!     INTEGER               IDENT   ( * )
!!
!! ## Detailed Input
!!
!!     STRING   is any character string that is to be scanned
!!              to locate recognized and unrecognized substrings.
!!
!!     MARKS    is an array of marks that will be recognized
!!              by the scanning routine. This array must be prepared
!!              by calling the routine SCANPR.
!!
!!              Note that the blank string is interpreted
!!              in a special way by SCAN. If the blank character,
!!              ' ', is one of the MARKS, it will match any unbroken
!!              sequence of blanks in string.  Thus if ' ' is the only
!!              marks supplied and STRING is
!!
!!                 'A   lot of      space '
!!                  ......................
!!
!!              Then scan will locate the following substrings
!!
!!              'A'          STRING(1:1)    (unrecognized)
!!              '   '        STRING(2:4)    (recognized --- all blanks)
!!              'lot'        STRING(5:7)    (unrecognized)
!!              ' '          STRING(8:8)    (recognized --- a blank)
!!              'of'         STRING(9:10)   (unrecognized)
!!              '      '     STRING(11:16)  (recognized --- all blanks)
!!              'space'      STRING(17:21)  (unrecognized)
!!              ' '          STRING(22:22)  (recognized --- a blank)
!!
!!     MRKLEN   is an auxiliary array populated by SCANPR
!!              for use by SCAN. It should be declared with
!!              length equal to the length of MARKS. It must
!!              be prepared for use by the routine SCANPR.
!!
!!     PNTERS   is a specially structured array of integers that
!!              describes the array MARKS. It is must be filled
!!              in by the routine SCANPR. It should be declared
!!              by the calling program as shown here:
!!
!!                 INTEGER  PNTERS ( RCHARS )
!!
!!              RCHARS is given by the expression
!!
!!                MAX - MIN + 5
!!
!!              where
!!
!!              MAX is the maximum value of ICHAR(MARKS(I)(1:1))
!!                  over the range I = 1, NMARKS
!!
!!              MIN is the minimum value of ICHAR(MARKS(I)(1:1))
!!                  over the range I = 1, NMARKS
!!
!!              See SCANPR for a more detailed description of the
!!              declaration of PNTERS.
!!
!!     ROOM     is the amount of space available for storing the
!!              results of scanning the string.
!!
!!     START    is the position from which scanning should commence.
!!              Values of START less than 1 are treated as 1.
!!
!! ## Detailed Output
!!
!!     START    is the position from which scanning should continue
!!              in order to fully scan STRING (if sufficient memory was
!!              not provided in BEG, END, and IDENT on the current
!!              call to SCAN).
!!
!!     NTOKNS   is the number of substrings identified in the current
!!              scan of STRING.
!!
!!     BEG      beginnings of scanned substrings. This should be
!!              declared so that it is at least as large as ROOM.
!!
!!     END      endings of scanned substrings. This should be declared
!!              so that it is at least as large as ROOM.
!!
!!     IDENT    positions of scanned substring within array MARKS.
!!              If the substring STRING(BEG(I):END(I)) is in the array
!!              MARKS, then MARKS(IDENT(I)) will equal
!!              STRING(BEG(I):END(I)).
!!
!!              If the substring STRING(BEG(I):END(I)) is not in the
!!              list of MARKS then IDENT(I) will have the value 0.
!!
!!              IDENT should be declared so that it can contain at least
!!              ROOM integers.
!!
!! ## Parameters
!!
!!     None.
!!
!! ## Exceptions
!!
!!     Error free.
!!
!!     1)  A space is regarded as a special mark. If MARKS(I) = ' ',
!!         then MARKS(I) will match any consecutive sequence of blanks.
!!
!!     2)  If START is less than 1 on input, it will be treated as
!!         if it were 1.
!!
!!     3)  If START is greater than the length of the string, no
!!         tokens will be found and the value of START will return
!!         unchanged.
!!
!! ## Files
!!
!!     None.
!!
!! ## Particulars
!!
!!     This routine allows you to scan a string and partition it into
!!     recognized and unrecognized substrings.
!!
!!     For some applications the recognized substrings serve only as
!!     delimiters between the portions of the string
!!     that are of interest to your application. For other
!!     applications the recognized substrings are equally important as
!!     they may indicate operations that are to be performed on the
!!     unrecognized portions of the string. However, the techniques
!!     required to scan the string are the same in both instances. The
!!     examples below illustrate some common situations.
!!
!! ## Examples
!!
!!     Example 1.
!!     ----------
!!
!!     Suppose you wished to write a routine that would return the words
!!     of a string. The following routine shows how SCANPR and SCAN can
!!     be used to accomplish this task.
!!
!!        SUBROUTINE GETWDS ( STRING, WDROOM, NWORDS, WORDS )
!!
!!        CHARACTER*(*)      STRING
!!        INTEGER            WDROOM
!!        INTEGER            NWORDS
!!        CHARACTER*(*)      WORDS  ( * )
!!
!!        CHARACTER*(1)      MARKS  ( 1 )
!!        INTEGER            MRKLEN ( 1 )
!!        INTEGER            PNTERS ( 5 )
!!
!!        INTEGER            ROOM
!!        PARAMETER        ( ROOM = 50 )
!!
!!        INTEGER            BEG   ( ROOM )
!!        INTEGER            END   ( ROOM )
!!        INTEGER            I
!!        INTEGER            IDENT ( ROOM )
!!        INTEGER            NMARKS
!!        INTEGER            NTOKNS
!!        INTEGER            START
!!
!!        LOGICAL            FIRST
!!        SAVE               FIRST
!!        DATA               FIRST  / .TRUE. /
!!
!!        On the first time through the routine, set up the MARKS
!!        MRKLEN, and PNTERS arrays.
!!
!!        IF( FIRST ) THEN
!!
!!           FIRST    = .FALSE.
!!           MARKS(1) = ' '
!!           NMARKS   = 1
!!
!!           CALL SCANPR ( NMARKS, MARKS, MRKLEN, PNTERS )
!!
!!        END IF
!!
!!        Now simply scan the input string for words until we have
!!        them all or until we run out of room.
!!
!!        START  = 1
!!        NWORDS = 0
!!
!!        CALL SCAN ( STRING,
!!                    MARKS,  MRKLEN, PNTERS, ROOM, START,
!!                    NTOKNS, IDENT,  BEG,    END          )
!!
!!        If we found something in our scan, copy the substrings into the
!!        words array.
!!
!!        DO WHILE (       ( NWORDS .LT. WDROOM )
!!       .           .AND. ( NTOKNS .GT. 0      ) )
!!
!!           Step through the scanned substrings, looking for those
!!           that are not blank ...
!!
!!           I = 1
!!
!!           DO WHILE (       ( NWORDS .LT. WDROOM )
!!          .           .AND. ( I      .LE. NTOKNS ) )
!!
!!              Copy the non-blank substrings (those unidentified by
!!              SCAN) into WORDS.
!!
!!              IF ( IDENT(I) .EQ. 0 ) THEN
!!                 NWORDS        = NWORDS + 1
!!                 WORDS(NWORDS) = STRING(BEG(I):END(I))
!!              END IF
!!
!!              I      = I      + 1
!!
!!           END DO
!!
!!           Scan the STRING again for any substrings that might
!!           remain. Note that START is already pointing at the
!!           point in the string from which to resume scanning.
!!
!!           CALL SCAN ( STRING,
!!                       MARKS,  MRKLEN, PNTERS, ROOM, START,
!!                       NTOKNS, IDENT,  BEG,    END          )
!!        END DO
!!
!!        That's all, we've got all the substrings there were (or
!!        that we had room for).
!!
!!        RETURN
!!
!!     Example 2.
!!     ----------
!!
!!     To parse an algebraic expression such as
!!
!!        ( X + Y ) * ( 2*Z + SIN(W) ) ** 2
!!
!!     You would select '**', '*', '+', '-', '(', ')' and ' '
!!     to be the markers. Note that all of these begin with one
!!     of the characters in the string ' !"#$%&''()*+,-./'
!!     so that we can declare PNTERS to have length 20.
!!
!!     Prepare the MARKS, MRKLEN, and PNTERS.
!!
!!        CHARACTER*(4)         MARKS
!!        INTEGER               NMARKS ( 8  )
!!        INTEGER               MRKLEN ( 8  )
!!        INTEGER               PNTERS ( 20 )
!!
!!        INTEGER               ROOM
!!        PARAMETER           ( ROOM = 20 )
!!
!!        INTEGER               NTOKNS
!!        INTEGER               BEG    ( ROOM )
!!        INTEGER               END    ( ROOM )
!!        INTEGER               IDENT  ( ROOM )
!!
!!        LOGICAL               FIRST
!!        SAVE                  FIRST
!!        SAVE                  MARKS
!!        SAVE                  MRKLEN
!!        SAVE                  PNTERS
!!
!!        DATA                  FIRST  / .TRUE. /
!!
!!        IF ( FIRST ) THEN
!!
!!           MARKS(1) = '('
!!           MARKS(2) = ')'
!!           MARKS(3) = '+'
!!           MARKS(4) = '-'
!!           MARKS(5) = '*'
!!           MARKS(6) = '/'
!!           MARKS(7) = '**'
!!           MARKS(8) = ' '
!!
!!           NMARKS   = 8
!!
!!           CALL SCANPR ( NMARKS, MARKS, MRKLEN, PNTERS )
!!
!!           BLANK = BSRCHC ( ' ', NMARKS, MARKS )
!!
!!        END IF
!!
!!        Once all of the initializations are out of the way,
!!        we can scan an input string.
!!
!!        CALL SCAN ( STRING, MARKS,  MRKLEN, PNTERS, ROOM,
!!       .            START,  NTOKNS, IDENT,  BEG,    END  )
!!
!!        Next eliminate any white space that was returned in the
!!        list of tokens.
!!
!!        KEPT = 0
!!
!!        DO I = 1, NTOKNS
!!
!!           IF ( IDENT(I) .NE. BLANK ) THEN
!!
!!              KEPT        = KEPT + 1
!!              BEG  (KEPT) = BEG(I)
!!              END  (KEPT) = END(I)
!!              IDENT(KEPT) = IDENT(I)
!!
!!           END IF
!!
!!        END DO
!!
!!        Now all of the substrings remaining point to grouping symbols,
!!        operators, functions, or variables. Given that the individual
!!        "words" of the expression are now in hand, the meaning of the
!!        expression is much easier to determine.
!!
!!        The rest of the routine is left as a non-trivial exercise
!!        for the reader.
!!
!! ## Restrictions
!!
!!     1)  The arrays MARKS, MRKLEN, and PNTERS must be prepared by the
!!         routine SCANPR prior to supplying them for use by SCAN.
!!
!! ## Literature References
!!
!!     None.
!!
!! ## Author and Institution
!!
!!     J. Diaz del Rio    (ODC Space)
!!     W.L. Taber         (JPL)
!!
!! ## Version
!!
!!-    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR)
!!
!!        Added IMPLICIT NONE statement.
!!
!!        Edited the header to comply with NAIF standard.
!!
!!-    SPICELIB Version 1.0.0, 26-JUL-1996 (WLT)
!!
!! ## Index Entries
!!
!!     Scan a string for recognized and unrecognized tokens
!!     Parse a string
!!
!!     All of the MARKS have the same declared length.
!!     However, since all of your marks may not have
!!     the same intended length (for example '*' and
!!     '**') it is desirable to be able to specify
!!     how much of MARKS(I) should actually be used
!!     when examining STRING for a substring match.
!!     This is done with the array MRKLEN.
!!     MARKS(I)(1:MRKLEN(I)) will be used when
!!     scanning STRING.
!!
!!     Here is the expected structure of PNTERS.
!!
!!             PNTERS(1) = MIN ( ICHAR(MARKS(I)(1:1)  )
!!             PNTERS(2) = MAX ( ICHAR(MARKS(I)(1:1)  )
!!
!!     where I ranges from 1 to the number of MARKS stored
!!     in MARKS.  For ease of further discussion let
!!     MYCHAR(I) represent the characters from PNTERS(1)
!!     to PNTERS(2), and assume that legitimate values of
!!     I are from 1 to N.
!!
!!             PNTERS(3)   = 0
!!             PNTERS(4)   = index of the last entry of MARKS
!!                           that begins with the character
!!                           MYCHAR(1).
!!
!!             PNTERS(5)   = index of the last entry of MARKS
!!                           that begins with the character
!!                           MYCHAR(2), if there is no such element
!!                           of MARKS let PNTERS(5) = PNTERS(4)
!!                .
!!                .
!!                .
!!
!!             PNTERS(3+K) = index of the last entry of MARKS
!!                           that begins with the character
!!                           MYCHAR(K), if there is no such element
!!                           of MARKS, let PNTERS(3+K) =
!!                           PNTERS(3+K-1)
!!                .
!!                .
!!                .
!!
!!             PNTERS(3+N) = index of the last entry of MARKS
!!                           that begins with the character
!!                           MYCHAR(N).
!!
!!             PNTERS(4+N) = PNTERS(3+N)
!!
!!     Get the information concerning the range of the
!!     marks from the PNTERS array.
!!
      use pepper_kinds, only: ip
      use pepper_strings, only: ncpos
      implicit none
      logical               return
      character(1)          letter
      integer(ip)           backup
      integer(ip)           eblock
      integer(ip)           fchar
      integer(ip)           finish
      integer(ip)           i
      integer(ip)           intval
      integer(ip)           j
      integer(ip)           jump
      integer(ip)           l
      integer(ip)           last
      integer(ip)           last1
      integer(ip)           lbound
      integer(ip)           lchar
      integer(ip)           n
      integer(ip)           offset
      integer(ip)           slot
      integer(ip)           stop
      integer(ip)           test
      integer(ip)           this1
      integer(ip)           ubound
      logical               equal
      logical               known
      offset = pnters(1) - 4
      lbound = pnters(1) - 1
      ubound = pnters(2) + 1

      last   =  len ( string )
      ntokns =  0
      backup =  start - 1
      known  = .true.
      start  =  max ( 1, start )

      do while ( start <= last )

!
!        Get the numeric code for this letter, and look up
!        the range of markers that begin with this letter.
!
         letter = string(start:start)

         intval = max( lbound, min(ichar(letter), ubound) )

         test   = pnters( intval - offset     )
         finish = pnters( intval - offset - 1 )

         equal  = .false.
!
!        If TEST is greater than FINISH, then there is a range of
!        markers that start with this letter.
!
         do while ( test > finish )

!
!           Look up the length of the next marker to test for
!           and compute where it would end in STRING if there
!           is a match.
!
            l    = mrklen ( test )
            stop = backup + l

!
!           Make sure that we are not going to violate any substring
!           references when we compare the current candidate mark with
!           the substring having the same length and starting at START.
!
            if ( stop > last ) then

               test = test - 1

            else
!
!              OK. The substring reference STRING(START:STOP) is
!              legal.  See if it is equal to the current test mark.
!
               equal = marks(test)(1:l) == string(start:stop)

!
!              If it isn't equal, just set up to test the next mark.
!
               if ( .not. equal ) then

                  test = test - 1

               else

!
!                 If we were in the middle of an unrecognized string
!                 then, we need to check whether or not we have room
!                 to identify another token. If we don't we must return
!                 now.
!
                  if (       (        .not. known ) .and. ( ntokns ==  room  ) ) return

!
!                 A space is a special kind of mark.  All white space
!                 is regarded as being the same.  If the current mark
!                 is a space, we need to collect all of the consecutive
!                 blanks beginning with the one at the START position.
!
                  if ( marks(test) == ' ' ) then

                     stop = ncpos(string, ' ', start) - 1

                     if ( stop < 0 ) then
                        stop = last
                     end if

                  end if

!
!                 Ok. We have a new known token.
!
!                 1)  Record its begin, end, and identity.
!
!                 2)  Set TEST to FINISH so that the loop will end.
!
!                 3)  Set START to the current STOP so that later when
!                     we add 1, START will point to the beginning
!                     of the remainder of the string that needs to be
!                     scanned.
!
                  known            = .true.
                  ntokns           = ntokns + 1
                  beg   ( ntokns ) = start
                  end   ( ntokns ) = stop
                  ident ( ntokns ) = test
                  test             = finish
                  start            = stop

!
!                 If we have just used up all available room,
!                 position START so that we will be ready
!                 to continue scanning on a subsequent call
!                 and return.
!
                  if ( ntokns == room ) then
                     start = start + 1
                     return
                  end if

               end if

            end if

         end do

!
!        If none of the markers matched a substring starting at
!        the current position, we are beginning or continuing
!        an unrecognized substring.
!
         if ( .not. equal ) then

!
!           If we are already in the middle of an unrecognized
!           substring, just extend our current unrecognized string.
!
            if ( .not. known ) then

               end(ntokns)   = start

!
!           Otherwise, start up a new unrecognized substring.
!
            else

               ntokns        =  ntokns + 1
               beg  (ntokns) =  start
               end  (ntokns) =  start
               ident(ntokns) =  0
               known         = .false.

            end if

         end if

         backup = start
         start  = start + 1

      end do

      return
      end procedure scan

!$Procedure TPICTR ( Create a Time Format Picture )

      module procedure tpictr
!! ## Abstract
!!
!!     Create a time format picture suitable for use by the routine
!!     TIMOUT from a given sample time string.
!!
!! ## Required Reading
!!
!!     None.
!!
!! ## Keywords
!!
!!     TIME
!!
!! ## Declarations
!!
      use pepper_kinds, only: ip, wp
      implicit none


! $ Detailed_Input
!
!     SAMPLE   is a representative time string that to use
!              as a model to format time strings.
!
! $ Detailed_Output
!
!     PICTUR   is a format picture suitable for use with the SPICE
!              routine TIMOUT. This picture when used to format
!              the appropriate  epoch via TIMOUT will yield the same
!              time components in the same order as the components
!              in SAMPLE.
!
!              Picture should be declared to be at least 80 characters
!              in length. If Picture is not sufficiently large
!              to contain the format picture, the picture will
!              be truncated on the right.
!
!     OK       is a logical flag. If all of the components of SAMPLE
!              are recognizable, OK will be returned with the value
!              .TRUE. If some part of PICTUR cannot be parsed,
!              OK will be returned with the value .FALSE.
!
!     ERRMSG   is a diagnostic message  that indicates what part of
!              SAMPLE was not recognizable. If SAMPLE can be
!              successfully parsed, OK will be .TRUE. and ERRMSG will
!              be returned as a blank string. If ERRMSG does not
!              have sufficient room (up to 400 characters) to
!              contain the full message, the message will be truncated
!              on the right.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  All problems with the inputs are reported via OK and ERRMSG.
!
!     2)  If a format picture can not be created from the sample
!         time string, PICTUR is returned as a blank string.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Although the routine TIMOUT provides SPICE users with a great
!     deal of flexibility in formatting time strings, users must
!     master the means by which a time picture is constructed
!     suitable for use by TIMOUT.
!
!     This routine allows SPICE users to supply a sample time string
!     from which a corresponding time format picture can be created,
!     freeing users from the task of mastering the intricacies of
!     the routine TIMOUT.
!
!     Note that TIMOUT can produce many time strings whose patterns
!     can not be discerned by this routine. When such outputs are
!     called for, the user must consult TIMOUT and construct the
!     appropriate format picture "by hand." However, these exceptional
!     formats are not widely used and are not generally recognizable
!     to an uninitiated reader.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as input,
!     the compiler and supporting libraries, and the machine specific
!     arithmetic implementation.
!
!     1) Given a sample with the format of the UNIX date string
!        local to California, create a SPICE time picture for use
!        in TIMOUT.
!
!        Using that SPICE time picture, convert a series of ephemeris
!        times to that picture format.
!
!        Use the LSK kernel below to load the leap seconds and time
!        constants required for the conversions.
!
!           naif0012.tls
!
!        Example code begins here.
!
!              PROGRAM TPICTR_EX1
!              IMPLICIT NONE
!
!        C
!        C     Local parameters.
!        C
!              INTEGER               ERRLEN
!              PARAMETER           ( ERRLEN  = 400 )
!
!              INTEGER               TIMLEN
!              PARAMETER           ( TIMLEN  = 64  )
!
!        C
!        C     Local variables
!        C
!              CHARACTER*(ERRLEN)    ERR
!              CHARACTER*(TIMLEN)    PICTUR
!              CHARACTER*(TIMLEN)    SAMPLE
!              CHARACTER*(TIMLEN)    TIMSTR
!              CHARACTER*(TIMLEN)    UTCSTR
!
!              DOUBLE PRECISION      ET
!
!              LOGICAL               OK
!
!        C
!        C     Load LSK file.
!        C
!              CALL FURNSH ( 'naif0012.tls' )
!
!        C
!        C     Create the required time picture.
!        C
!              SAMPLE = 'Thu Oct 01 11:11:11 PDT 1111'
!
!              CALL TPICTR ( SAMPLE, PICTUR, OK, ERR )
!
!              IF ( .NOT. OK ) THEN
!
!                 WRITE(*,*) 'Invalid time picture.'
!                 WRITE(*,*) ERR
!
!              ELSE
!
!        C
!        C        Convert the input UTC time to ephemeris time.
!        C
!                 UTCSTR = '24 Mar 2018  16:23:00 UTC'
!                 CALL STR2ET ( UTCSTR, ET )
!
!        C
!        C         Now convert ET to the desired output format.
!        C
!                  CALL TIMOUT ( ET, PICTUR, TIMSTR )
!                  WRITE (*,*) 'Sample format: ', SAMPLE
!                  WRITE (*,*) 'Time picture : ', PICTUR
!                  WRITE (*,*)
!                  WRITE (*,*) 'Input UTC    : ', UTCSTR
!                  WRITE (*,*) 'Output       : ', TIMSTR
!
!              END IF
!
!              END
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!         Sample format: Thu Oct 01 11:11:11 PDT 1111
!         Time picture : Wkd Mon DD HR:MN:SC PDT YYYY ::UTC-7
!
!         Input UTC    : 24 Mar 2018  16:23:00 UTC
!         Output       : Sat Mar 24 09:23:00 PDT 2018
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
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 25-AUG-2021 (JDR)
!
!        Changed output argument name ERROR to ERRMSG for consistency
!        with other routines.
!
!        Edited the header to comply with NAIF standard.
!        Converted the existing code fragments into complete example
!        and added reference to required LSK.
!
!-    SPICELIB Version 1.0.1, 16-MAR-1999 (WLT)
!
!        Corrected a minor spelling error in the header comments.
!
!-    SPICELIB Version 1.0.0, 10-AUG-1996 (WLT)
!
!-&

! $ Index_Entries
!
!     Use a sample time string to produce a time format picture
!
!-&

!
!     Local variables
!
      character(5)          type
      character(8)          modify   ( 5 )

      real(wp)              tvec     ( 10 )

      integer(ip)           ntvec

      logical               mods
      logical               succes
      logical               yabbrv

!
!     This routine is really just a front for one aspect of
!     the routine TPARTV.
!
      errmsg  = ' '

      call tpartv ( ctx, sample, &
                    tvec,   ntvec, type, &
                    modify, mods,  yabbrv, succes, &
                    pictur, errmsg )

      if ( pictur == ' ' ) then
         ok = .false.
      else
         ok    = .true.
         errmsg = ' '
      end if

      return
      end procedure tpictr

!$Procedure UTC2ET ( UTC to Ephemeris Time )

      module procedure utc2et
!! ## Abstract
!!
!!     Convert an input time from Calendar or Julian Date format, UTC,
!!     to ephemeris seconds past J2000.
!!
!! ## Required Reading
!!
!!     KERNEL
!!     TIME
!!
!! ## Keywords
!!
!!     TIME
!!
!! ## Declarations
!!
      use pepper_kinds, only: ip, wp
      use pepper_strings, only: repmc
      implicit none


! $ Detailed_Input
!
!     UTCSTR   is an input time string, containing a Calendar or Julian
!              Date, UTC. Any input string acceptable to the SPICELIB
!              routine TPARTV are acceptable to UTC2ET. The length of
!              UTCSTR should not exceed 80 characters.
!
! $ Detailed_Output
!
!     ET       is the equivalent of UTCSTR, expressed in ephemeris
!              seconds past J2000. If an error occurs, or if the
!              input string is ambiguous, ET is not changed.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input time string is ambiguous, the error
!         SPICE(INVALIDTIMESTRING) is signaled.
!
!     2)  This routine does not attempt to account for variations
!         in the length of the second that were in effect prior
!         to Jan 1, 1972. For days prior to that date, we assume
!         there are exactly 86400 ephemeris seconds.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Note: NAIF recommends the use of STR2ET instead of UTC2ET.
!
!     This routine handles that task of converting strings
!     representing epochs in the UTC system to ephemeris seconds
!     (TDB) past the epoch of the J2000 frame.
!
!     Although this routine is intended primarily for the
!     conversion of epochs during the "space age" it may also
!     be used to convert much earlier epochs. However, before
!     using this routine to convert epochs prior to 1972
!     you must be sure that the assumptions made by in the
!     implementation are consistent with the accuracy of
!     the input calendar string.
!
!     As noted in the $Exceptions section, this routine does not attempt
!     to account for variations in the length of the second that were
!     used prior to Jan 1, 1972. Instead each "UTC" day prior to
!     Jan 1, 1972 is assumed to have exactly 86400 TDT seconds.
!
!     Ancient Epochs
!     --------------
!
!     The calendar used today, the Gregorian calendar, has its
!     initial epoch on 15 October, 1582. Prior to that epoch the
!     Julian calendar was used for the recording of epochs.
!     October 15, 1582 (Gregorian) corresponds to
!     October 05, 1582 (Julian). From the point of view of the
!     implementation of this routine, all epochs belong to the
!     Gregorian calendar extended indefinitely backward in time.
!     If you need to obtain ephemeris seconds past the J2000 epoch
!     from Julian Calendar strings, we suggest that
!     you make use of the SPICELIB routine STR2ET.
!
! $ Examples
!
!     The numerical results shown for these examples may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Below is a sampling of some of the time formats that
!        are acceptable as inputs to UTC2ET. A complete discussion
!        of permissible formats is given in the SPICELIB routine
!        TPARTV as well as the User's reference file time.req
!        located in the "doc" directory of the toolkit.
!
!        ISO (T) Formats (with or without trailing "Z").
!
!        String                        Year Mon  DOY DOM  HR Min Sec
!        ----------------------------  ---- ---  --- ---  -- --- ------
!        1996-12-18T12:28:28           1996 Dec   na  18  12  28 28
!        1986-01-18T12                 1986 Jan   na  18  12  00 00
!        1986-01-18T12:19              1986 Jan   na  18  12  19 00
!        1986-01-18T12:19:52.18Z       1986 Jan   na  18  12  19 52.18
!        1995-08T18:28:12Z             1995  na  008  na  18  28 12
!        1995-18T                      1995  na  018  na  00  00 00
!        0000-01-01T                   1 BC Jan   na  01  00  00 00
!
!        Calendar Formats.
!
!        String                        Year   Mon DOM  HR Min  Sec
!        ----------------------------  ----   --- ---  -- ---  ------
!        Tue Aug  6 11:10:57  1996     1996   Aug  06  11  10  57
!        1 DEC 1997 12:28:29.192       1997   Dec  01  12  28  29.192
!        2/3/1996 17:18:12.002         1996   Feb  03  17  18  12.002
!        Mar 2 12:18:17.287 1993       1993   Mar  02  12  18  17.287
!        1992 11:18:28  3 Jul          1992   Jul  03  11  18  28
!        June 12, 1989 01:21           1989   Jun  12  01  21  00
!        1978/3/12 23:28:59.29         1978   Mar  12  23  28  59.29
!        17JUN1982 18:28:28            1982   Jun  17  18  28  28
!        13:28:28.128 1992 27 Jun      1992   Jun  27  13  28  28.128
!        1972 27 jun 12:29             1972   Jun  27  12  29  00
!        '93 Jan 23 12:29:47.289       1993*  Jan  23  12  29  47.289
!        27 Jan 3, 19:12:28.182        2027*  Jan  03  19  12  28.182
!        23 A.D. APR 4, 18:28:29.29    0023** Apr  04  18  28  29.29
!        18 B.C. Jun 3, 12:29:28.291   -017** Jun  03  12  29  28.291
!        29 Jun  30 12:29:29.298       2029+  Jun  30  12  29  29.298
!        29 Jun '30 12:29:29.298       2030*  Jun  29  12  29  29.298
!
!        Day of Year Formats
!
!        String                        Year  DOY HR Min Sec
!        ----------------------------  ----  --- -- --- ------
!        1997-162::12:18:28.827        1997  162 12  18 28.827
!        162-1996/12:28:28.287         1996  162 12  28 28.287
!        1993-321/12:28:28.287         1993  231 12  28 28.287
!        1992 183// 12 18 19           1992  183 12  18 19
!        17:28:01.287 1992-272//       1992  272 17  28 01.287
!        17:28:01.282 272-1994//       1994  272 17  28 01.282
!        '92-271/ 12:28:30.291         1992* 271 12  28 30.291
!        92-182/ 18:28:28.281          1992* 182 18  28 28.281
!        182-92/ 12:29:29.192          0182+ 092 12  29 29.192
!        182-'92/ 12:28:29.182         1992  182 12  28 29.182
!
!        Julian Date Strings
!
!        jd 28272.291                  Julian Date   28272.291
!        2451515.2981 (JD)             Julian Date 2451515.2981
!        2451515.2981 JD               Julian Date 2451515.2981
!
!                                      Abbreviations Used in Tables
!
!                                         na    --- Not Applicable
!                                         Mon   --- Month
!                                         DOY   --- Day of Year
!                                         DOM   --- Day of Month
!                                         Wkday --- Weekday
!                                         Hr    --- Hour
!                                         Min   --- Minutes
!                                         Sec   --- Seconds
!
!        *  The default interpretation of a year that has been
!           abbreviated with a leading quote as in 'xy (such as '92) is
!           to treat the year as 19xy if xy > 68 and to treat it is
!           20xy otherwise. Thus '69 is interpreted as 1969 and '68 is
!           treated as 2068. However, you may change the "split point"
!           and centuries through use of the SPICELIB routine TSETYR
!           which is an entry point in the SPICELIB module TEXPYR. See
!           that routine for a discussion of how you may reset the
!           split point.
!
!        ** All epochs are regarded as belonging to the Gregorian
!           calendar. We formally extend the Gregorian calendar
!           backward and forward in time for all epochs. If you have
!           epochs belonging to the Julian Calendar, consult the
!           SPICELIB routines TPARTV and JUL2GR for a discussion
!           concerning conversions to the Gregorian calendar and ET.
!
!        +  When a day of year format or calendar format string is
!           input and neither of the integer components of the date is
!           greater than 1000, the first integer is regarded as being
!           the year.
!
!     2) Convert a calendar date in UTC string format to Julian
!        Ephemeris date.
!
!        Use the LSK kernel below to load the leap seconds and time
!        constants required for the conversions.
!
!           naif0012.tls
!
!        Example code begins here.
!
!              PROGRAM UTC2ET_EX1
!              IMPLICIT NONE
!
!        C
!        C     SPICELIB functions
!        C
!              DOUBLE PRECISION      UNITIM
!
!        C
!        C     Local constants.
!        C
!              CHARACTER*(*)         UTCSTR
!              PARAMETER           ( UTCSTR = 'Dec 19 2003  16:48:00' )
!
!        C
!        C     Local variables.
!        C
!              DOUBLE PRECISION      ET
!              DOUBLE PRECISION      JED
!
!        C
!        C     Load the LSK file.
!        C
!              CALL FURNSH ( 'naif0012.tls' )
!
!        C
!        C     Convert input UTC string to Ephemeris Time.
!        C
!              CALL UTC2ET ( UTCSTR, ET )
!              WRITE(*,'(2A)') 'UTC time             : ', UTCSTR
!              WRITE(*,'(A,F22.9)') 'Ephemeris time       :', ET
!
!        C
!        C     Convert the Ephemeris Time to Julian ephemeris date,
!        C     i.e. Julian date relative to TDB time scale.
!        C
!              JED = UNITIM( ET, 'ET', 'JED' )
!              WRITE(*,'(A,F22.9)') 'Julian Ephemeris Date:', JED
!
!              END
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!        UTC time             : Dec 19 2003  16:48:00
!        Ephemeris time       :   125124544.183560610
!        Julian Ephemeris Date:     2452993.200742865
!
! $ Restrictions
!
!     1)  The conversion between ET and UTC depends on the values in the
!         input kernel pool. The kernel pool should be loaded prior to
!         calling this routine.
!
!     2)  Before using this routine for epochs prior to Jan 1, 1972
!         be sure to check the $Particulars section to make sure
!         that the assumptions made in this routine are consistent
!         with the accuracy you require for your application.
!
! $ Literature_References
!
!     [1]  J. Jespersen and J. Fitz-Randolph, "From Sundials to Atomic
!          Clocks, Understanding Time and Frequency," Dover
!          Publications, Inc. New York, 1982.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     W.M. Owen          (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.1.1, 13-AUG-2021 (JDR) (EDW)
!
!        Header edits to expand description of ISO format.
!
!        Edited the header to comply with NAIF standard. Added complete
!        code example.
!
!        Added note to $Particulars section.
!
!-    SPICELIB Version 2.1.0, 05-JAN-1998 (WLT)
!
!        Comments concerning the default century for abbreviated
!        years were updated to reflect changes to TEXPYR.
!
!-    SPICELIB Version 2.0.0, 20-NOV-1996 (WLT)
!
!        About the only thing that is the same in this routine
!        from the previous editions, is that the interface is
!        unchanged. Nearly everything else has been modified.
!        The routine was modified to make use of TPARTV
!        and TTRANS to handle the task of parsing and
!        computing seconds past 2000 TDB. This version
!        now handles leap seconds correctly.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) (IMU)
!
!-&

! $ Index_Entries
!
!     UTC to ephemeris time
!
!-&

!
!     SPICELIB functions
!

!
!     Parameters
!
      integer(ip), parameter :: era = 1

      integer(ip), parameter :: wday = era    + 1

      integer(ip), parameter :: zone = wday   + 1

      integer(ip), parameter :: ampm = zone   + 1

      integer(ip), parameter :: system = ampm   + 1

!
!     Local variables
!

      integer(ip), parameter :: longln = 480

      integer(ip), parameter :: lnsize = 80

      character(lnsize)     pictur
      character(longln)     error

      integer(ip), parameter :: smwdsz = 8

      character(smwdsz)     type
      character(smwdsz)     modify ( 5 )

      real(wp)              tvec   ( 10 )
      integer(ip)           ntvec
      integer(ip)           year

      logical               succes
      logical               ok
      logical               mods
      logical               yabbrv

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     So far we have no errors, the type of input is unknown.
!
      error  = ' '
      type   = ' '

!
!     First parse the string and perform the various tests on
!     the validity of its components.
!
      call tpartv ( ctx, utcstr, &
                    tvec,   ntvec, type, &
                    modify, mods,  yabbrv, succes, &
                    pictur, error )

      if ( .not. succes ) then
         call ctx%setmsg ( error )
         call ctx%sigerr ( 'SPICE(INVALIDTIMESTRING)' )
         return
      end if

!
!     We are not going to allow most of the modifiers in strings.
!
      if ( mods ) then

         if (       modify(system) /= ' ' &
              .and. modify(system) /= 'UTC' ) then

            error = 'UTC2ET does not support the specification ' &
            //      'of a time system in a string.  The time ' &
            //      'system # was specified. Try the routine STR2ET.'
            call repmc  ( error, '#', modify(system), error )

            call ctx%setmsg ( error )
            call ctx%sigerr ( 'SPICE(INVALIDTIMESTRING)' )
            return

         else if ( modify(zone) /= ' ' ) then

            error = 'UTC2ET does not support the specification ' &
            //      'of a time zone in a time string.  The time ' &
            //      'zone ''#'' was specified. Try the routine STR2ET.'
            call repmc  ( error, '#', modify(zone), error )
            call ctx%setmsg ( error )
            call ctx%sigerr ( 'SPICE(INVALIDTIMESTRING)' )
            return

         else if ( modify(ampm) /= ' ' ) then

            error = 'UTC2ET does not support the AM/PM ' &
            //      'conventions for time strings. Try the ' &
            //      'routine STR2ET.'
            call ctx%setmsg ( error )
            call ctx%sigerr ( 'SPICE(INVALIDTIMESTRING)' )
            return

         end if

      end if

!
!     If parsing the time string went well, we let TTRANS handle
!     the problem of transforming the time vector to TDB.
!
      if ( type == 'YMD' .or. type == 'YD') then
!
!        Check the components of the time vector for reasonableness.
!
         call tcheck ( ctx, tvec, type, mods, modify, ok, error )

         if ( .not. ok ) then
            call ctx%setmsg ( error )
            call ctx%sigerr ( 'SPICE(INVALIDTIMESTRING)' )
         end if

!
!        Fix up the year as needed.
!
         year   = nint( tvec(1) )

         if ( modify(era) == 'B.C.' ) then

            year = 1 - year

         else if ( modify(era) == 'A.D.' ) then
!
!           Do nothing.
!
         else if ( year < 100 ) then

            call texpyr ( ctx, year )

         end if

         tvec(1) = real(year, wp)
!
!        We are ready for launch, convert the time vector.
!
         call ttrans ( ctx, type, 'TDB', tvec )
         et = tvec(1)

      else if ( type == 'JD' ) then

         call ttrans ( ctx, 'JDUTC', 'TDB', tvec )
         et = tvec(1)

      else
!
!        The only way to get here is if we got some unexpected
!        type of time string. Signal an error.
!
         call ctx%setmsg ( '# time strings are not handled by ' &
         //            'UTC2ET. ' )
         call ctx%errch  ( '#', type )
         call ctx%sigerr ( 'SPICE(INVALIDTIMESTRING)' )
         return

      end if

      return

      end procedure utc2et

      module procedure timout
!! ## Abstract
!!
!!     Convert an input epoch represented in TDB seconds past the TDB
!!     epoch of J2000 to a character string formatted to the
!!     specifications of a user's format picture.
!!
!! ## Required Reading
!!
!!     TIME
!!
!! ## Keywords
!!
!!     TIME
!!
!! ## Declarations
!!
!! ## Detailed Input
!!
!!     ET       is a double precision representation of time in seconds
!!              past the ephemeris epoch J2000.
!!
!!     PICTUR   is a string that specifies how the output should be
!!              presented. The string is made up of various markers
!!              that stand for various components associated with
!!              a time.
!!
!!              There are five types of markers that may appear in a
!!              format picture. These are String Markers, Numeric
!!              Markers, Meta markers, Modifier Markers and Literal
!!              Markers.
!!
!!              The PICTUR string is examined and the various markers
!!              are identified. The output time string is constructed
!!              by replacing each of the identified markers with
!!              an appropriate time component.
!!
!!              The various markers and their meanings are discussed
!!              in the $Particulars section below.
!!
!!              Note that leading and trailing blanks in PICTUR are
!!              ignored.
!!
!! ## Detailed Output
!!
!!     OUTPUT   is a time string equivalent to the input epoch ET,
!!              matching the format specified by PICTUR.
!!
!! ## Parameters
!!
!!     None.
!!
!! ## Exceptions
!!
!!     1)  A leapseconds kernel must be loaded via the routine FURNSH
!!         before calling this routine. If a leapsecond kernel has not
!!         been loaded, an error is signaled by a routine in the call
!!         tree of this routine.
!!
!!     2)  If PICTUR contains the numeric marker 'YYYY' and the
!!         magnitude of year is too large to be displayed as a four-digit
!!         integer, TIMOUT will replace it by '****'.
!!
!!     3)  If the requested precision is higher than 12 decimal places,
!!         TIMOUT will truncate the decimal part down to 12, and OUTPUT
!!         will have all the remaining digits in the decimal part set to
!!         zero.
!!
!!     4)  Double colon (::), when is not part of one of the supported
!!         markers, has no effect and will be presented as is on the
!!         output string.
!!
!! ## Files
!!
!!     A leapseconds kernel must be "loaded" via the routine FURNSH
!!     prior to calling TIMOUT.
!!
!! ## Particulars
!!
!!     A format picture is simply a string of letters that lets
!!     TIMOUT know where various components of a time representation
!!     should be placed during creation of the time string.
!!     Here's an example of such a picture:
!!
!!        MON DD,YYYY  HR:MN:SC.#### (TDB) ::TDB
!!
!!     Here is a sample of times that would be created by using this
!!     format.
!!
!!        JAN 12,1992  12:28:18.2772 (TDB)
!!        FEB 13,1994  23:18:25.2882 (TDB)
!!        AUG 21,1995  00:02:00.1881 (TDB)
!!
!!     As you can see from the samples above, the format picture
!!     specifies that every time string created should begin with a
!!     three-letter abbreviation for the month, followed by a space and
!!     the day of the month. The day of month is followed immediately by
!!     a comma and the year. The year component is followed by two
!!     spaces. The next outputs are hours represented as a two digit
!!     integer, a colon, minutes represented as a two digit integer,
!!     another colon, and seconds truncated to 4 decimal places and
!!     having a two digit integer part (rounding can be commanded; see
!!     the discussion of truncation and rounding below). This is
!!     followed by a space and the string '(TDB)'. The special marker
!!     '::TDB' in the time picture is an ``invisible'' marker. It is
!!     used to specify the time system that should be used in creating
!!     the time string (in this case Barycentric Dynamical Time).
!!
!!     TIMOUT does not recognize all of the parts of the time format
!!     picture in the example above. The list of recognized parts and
!!     unrecognized parts is shown in the table below.
!!
!!       Recognized       Unrecognized
!!       ----------       ------------
!!       'MON'            ' '
!!       'DD'             ','
!!       'YYYY'           '  '
!!       'HR'             ':'
!!       'MN'             '(TDB)'
!!       'SC'
!!       '.####'
!!       '::TDB'
!!
!!     The unrecognized parts are called literal markers. They are
!!     copied exactly as they appear in PICTUR into the output string.
!!     The recognized parts of the picture are replaced by a
!!     component of time or, as in the case of '::TDB' are used
!!     as instructions about the overall properties of the time
!!     string.
!!
!!     The full list of recognized markers, their classification
!!     and meaning are given below.
!!
!!     MARKER       CLASS     MEANING
!!     -----------  --------  -----------------------------------------
!!     '.##...'     modifier  represents a numeric component that
!!                            immediately precedes this in a decimal
!!                            format. Number of decimal places
!!                            equals the number of '#' characters
!!     '::GCAL'     meta      dates are reported in Gregorian calendar
!!     '::JCAL'     meta      dates are reported in Julian calendar
!!     '::MCAL'     meta      dates after 15 October, 1582 are reported
!!                            in Gregorian calendar; before that
!!                            dates are reported in Julian calendar
!!
!!     '::RND'      meta      round output to places specified by
!!                            least significant component
!!
!!     '::TDB'      meta      all components should be TDB
!!
!!     '::TDT'      meta      all components should be TT (TDT)
!!
!!     '::TT'       meta      all components should be TT (TDT)
!!
!!     '::TRNC'     meta      truncate all output components (default)
!!     '::UTC'      meta      all components should be UTC (default)
!!     '::UTC+h:m'  meta      all components in UTC offset by +h (hours)
!!                            and +m (minutes) so as to allow time zones.
!!     '::UTC-h:m'  meta      all components in UTC offset by -h (hours)
!!                            and -m (minutes) so as to allow time zones.
!!     'AMPM'       string    String (either 'A.M.' or 'P.M.')
!!                            indicating whether hours are before
!!                            or after noon.
!!     'ampm'       string    String (either 'a.m.' or 'p.m.')
!!                            indicating whether hours are before
!!                            or after noon.
!!     'AP'         numeric   AM/PM equivalents of the hour component
!!                            of a time.
!!     'DD'         numeric   Day of month
!!     'DOY'        numeric   Day of year
!!     'ERA'        string    String (either 'B.C.' or 'A.D.') giving
!!                            era associated with an epoch.
!!     '?ERA?'      string    String: either ' B.C. ' or ' A.D. ' if the
!!                            year is before 1000 A.D. otherwise a
!!                            blank: ' '.
!!     'era'        string    String (either 'b.c.' or 'a.d.') giving
!!                            era associated with an epoch.
!!     '?era?'      string    String: either ' b.c. ' or ' a.d. ' if the
!!                            year is before 1000 A.D. otherwise a
!!                            blank: ' '.
!!     'HR'         numeric   hour component of time
!!     'JULIAND'    numeric   Julian date component of time
!!     'MM'         numeric   numeric representation of month component
!!     'MN'         numeric   minute component of time
!!     'MON'        string    upper case three letter abbreviation for
!!                            month
!!     'Mon'        string    capitalized three letter abbreviation for
!!                            month
!!     'mon'        string    lower case three letter abbreviation for
!!                            month
!!     'MONTH'      string    upper case full name of month
!!     'Month'      string    capitalized full name of month
!!     'month'      string    lower case full name of month
!!     'SC'         numeric   seconds component of time
!!     'SP1950'     numeric   seconds past 1950 component of time
!!     'SP2000'     numeric   seconds past 2000 component of time
!!     'YR'         numeric   last two digits of year component of time
!!     'YYYY'       numeric   year component of time
!!     'WEEKDAY'    string    upper case day of week
!!     'Weekday'    string    capitalized day of week
!!     'weekday'    string    lower case day of week
!!     'WKD'        string    upper case three letter abbreviation for
!!                            day of week.
!!     'Wkd'        string    capitalized three letter abbreviation for
!!                            day of week.
!!     'wkd'        string    lower case three letter abbreviation for
!!                            day of week.
!!
!!     String Markers
!!
!!        String markers are portions of the format picture that will
!!        be replaced with a character string that represents the
!!        corresponding component of a time.
!!
!!     Numeric Markers
!!
!!        Numeric markers are portions of the format picture that will
!!        be replaced with a decimal string that represents the
!!        corresponding component of a time.
!!
!!     Meta Markers
!!
!!        Meta markers (listed under the class ``meta'' in the
!!        table above) are used to indicate "global" properties of
!!        your time string. You may specify time scale and how
!!        rounding should be performed on the components of time
!!        in your output string. Meta markers may be placed anywhere
!!        in your format picture. They do not contribute to placement
!!        of characters in output time strings. Also there are no
!!        restrictions on how many meta markers you may place in
!!        the format picture. However, if you supply conflicting
!!        `meta' markers (for example '::TDT' and '::TDB') in your
!!        picture the first marker listed (in left to right order)
!!        overrules the conflicting marker that appears later in
!!        the picture.
!!
!!     Default Meta Markers
!!
!!        If you do not specify a time system, calendar, or time
!!        zone through the use of a Meta Marker, TIMOUT uses the
!!        values returned by the SPICE routine TIMDEF. The default
!!        time system, calendar returned by TIMDEF are UTC and
!!        the Gregorian calendar. The default time zone returned
!!        by TIMDEF is a blank indicating that no time zone offset
!!        should be used.
!!
!!        See the header for the routine TIMDEF for a more complete
!!        discussion of setting and retrieving default values.
!!
!!     Modifier Markers
!!
!!        The numeric markers listed in the table above stand
!!        for integers unless they are modified through use of a
!!        modifier marker. The strings
!!
!!           .#
!!           .##
!!           .###
!!           .####
!!
!!        are used to this end. When a numeric marker is followed
!!        immediately by one of these modifiers, the corresponding time
!!        component will be written with the number of decimal places
!!        indicated by the number of successive occurrences of the
!!        character '#'. Any numeric token may be modified.
!!
!!     Rounding vs. Truncation
!!
!!        The meta markers ::TRNC and ::RND allow you to control
!!        how the output time picture is rounded. If you specify
!!        ::TRNC all components of time are simply truncated to
!!        the precision specified by the marker and any modifier.
!!        If you specify ::RND the output time is rounded to the
!!        least significant component of the format picture. The
!!        default action is truncation.
!!
!!        Whether an output time string should be rounded or
!!        truncated depends upon what you plan to do with the
!!        string. For example suppose you simply want to get the
!!        calendar date associated with a time and not the time of
!!        day. Then you probably do not want to round your output.
!!        Rounding 1992 Dec 31, 13:12:00 to the nearest day
!!        produces 1993 Jan 1. Thus in this case rounding is probably
!!        not appropriate.
!!
!!        However, if you are producing output for plotting using
!!        Julian Date, seconds past 1950 or seconds past 2000, you will
!!        probably want your output rounded so as to produce a smoother
!!        plot.
!!
!!     Time Systems
!!
!!        TIMOUT can produce output strings for epochs relative to
!!        any of the systems UTC, TT or TDT, or TDB. If you do not
!!        explicitly specify a time system, TIMOUT will produce strings
!!        relative to the time system returned by the SPICE routine
!!        TIMDEF. Unless you call TIMDEF and change it, the default time
!!        system is UTC. However, by using one of the Meta Markers
!!        ::UTC, ::TT, ::TDT, or ::TDB you may specify that TIMOUT
!!        produce time strings relative to the UTC, TT or TDT, or TDB
!!        system respectively.
!!
!!     Time Zones
!!
!!        The meta markers ::UTC+h:m  and ::UTC-h:m  allow you to
!!        offset UTC times so that you may represent times in a time
!!        zone other than GMT. For example you can output times in
!!        Pacific Standard time by placing the meta-marker ::UTC-8 in
!!        your format picture.
!!
!!        For instance, if you use the picture
!!
!!           YYYY Mon DD, HR:MN:SC ::UTC
!!
!!        you will get output strings such as:
!!
!!           1995 Jan 03, 12:00:00
!!
!!        If you use the picture
!!
!!           YYYY Mon DD, HR:MN:SC ::UTC-8
!!
!!        you will get output strings such as:
!!
!!           1995 Jan 03, 04:00:00
!!
!!        Finally, if you use the picture
!!
!!           YYYY Mon DD, HR:MN:SC ::UTC-8:15
!!
!!        you will get output string
!!
!!           1995 Jan 03, 03:45:00
!!
!!        Note that the minutes are always added or subtracted based on
!!        the sign present in the time zone specifier. In the case of
!!        ::UTC+h:m, minutes are added. In the case ::UTC-h:m, minutes
!!        are subtracted.
!!
!!        The unsigned part of the hours component can be no more than
!!        12. The unsigned part of the minutes component can be no
!!        more than 59.
!!
!!     Calendars
!!
!!        The calendar currently used by western countries is the
!!        Gregorian calendar. This calendar begins on Oct 15, 1582.
!!        Prior to Gregorian calendar the Julian calendar was used. The
!!        last Julian calendar date prior to the beginning of the
!!        Gregorian calendar is Oct 5, 1582.
!!
!!        The primary difference between the Julian and Gregorian
!!        calendars is in the determination of leap years. Nevertheless,
!!        both can be formally extended backward and forward in time
!!        indefinitely.
!!
!!        By default TIMOUT uses the default calendar returned by
!!        TIMDEF. Under most circumstances this will be the Gregorian
!!        calendar (::GCAL). However you may specify that TIMOUT use a
!!        specific calendar through use of one of the calendar Meta
!!        Markers. You may specify that TIMOUT use the Julian calendar
!!        (::JCAL), the Gregorian calendar (::GCAL)  or a mixture of
!!        both (::MCAL).
!!
!!        If you specify ::MCAL, epochs that occur after the beginning
!!        of the Gregorian calendar will be represented using the
!!        Gregorian calendar, and epochs prior to the beginning of the
!!        Gregorian calendar will be represented using the Julian
!!        calendar.
!!
!!     Getting Software to Construct Pictures for You.
!!
!!        Although it is not difficult to construct time format
!!        pictures, you do need to be aware of the various markers that
!!        may appear in a format picture.
!!
!!        There is an alternative means for getting a format picture.
!!        The routine TPICTR constructs format pictures from a sample
!!        time string. For example, suppose you would like your time
!!        strings to look like the basic pattern of the string below.
!!
!!           'Fri Jul 26 12:22:09 PDT 1996'
!!
!!        You can call TPICTR with this string, and it will create the
!!        appropriate PICTUR for use with TIMOUT.
!!
!!           CALL TPICTR ( 'Fri Jul 26 12:22:09 PDT 1996',
!!          .              PICTUR, OK, ERRMSG             )
!!
!!        The result will be:
!!
!!           'Wkd Mon DD HR:MN:SC (PDT) ::UTC-7'
!!
!!        Note: not every date that you can read is interpretable by
!!        TPICTR. For example, you might be able to understand that
!!        19960212121116 is Feb 2 1996, 12:11:16. However, TPICTR
!!        cannot recognize this string. Thus it is important to check
!!        the logical output OK to make sure that TPICTR was able to
!!        understand the time picture you provided.
!!
!!        Even thought TPICTR can not recognize every time pattern that
!!        has been used by various people, it does recognize nearly all
!!        patterns that you use when you want to communicate outside
!!        your particular circle of colleagues.
!!
!! ## Examples
!!
!!     The numerical results shown for these examples may differ across
!!     platforms. The results depend on the SPICE kernels used as input,
!!     the compiler and supporting libraries, and the machine specific
!!     arithmetic implementation.
!!
!!     1) Given a sample with the format of the UNIX date string
!!        local to California, create a SPICE time picture for use
!!        in TIMOUT.
!!
!!        Using that SPICE time picture, convert a series of ephemeris
!!        times to that picture format.
!!
!!        Use the LSK kernel below to load the leap seconds and time
!!        constants required for the conversions.
!!
!!           naif0012.tls
!!
!!        Example code begins here.
!!
!!              PROGRAM TIMOUT_EX1
!!              IMPLICIT NONE
!!
!!        C
!!        C     Local parameters.
!!        C
!!              INTEGER               ERRLEN
!!              PARAMETER           ( ERRLEN  = 400 )
!!
!!              INTEGER               TIMLEN
!!              PARAMETER           ( TIMLEN  = 64  )
!!
!!        C
!!        C     Local variables
!!        C
!!              CHARACTER*(ERRLEN)    ERR
!!              CHARACTER*(TIMLEN)    PICTUR
!!              CHARACTER*(TIMLEN)    SAMPLE
!!              CHARACTER*(TIMLEN)    TIMSTR
!!              CHARACTER*(TIMLEN)    UTCSTR
!!
!!              DOUBLE PRECISION      ET
!!
!!              LOGICAL               OK
!!
!!        C
!!        C     Load LSK file.
!!        C
!!              CALL FURNSH ( 'naif0012.tls' )
!!
!!        C
!!        C     Create the required time picture.
!!        C
!!              SAMPLE = 'Thu Oct 01 11:11:11 PDT 1111'
!!
!!              CALL TPICTR ( SAMPLE, PICTUR, OK, ERR )
!!
!!              IF ( .NOT. OK ) THEN
!!
!!                 WRITE(*,*) 'Invalid time picture.'
!!                 WRITE(*,*) ERR
!!
!!              ELSE
!!
!!        C
!!        C        Convert the input UTC time to ephemeris time.
!!        C
!!                 UTCSTR = '26 Nov 2018  23:23:00 UTC'
!!                 CALL STR2ET ( UTCSTR, ET )
!!
!!        C
!!        C         Now convert ET to the desired output format.
!!        C
!!                  CALL TIMOUT ( ET, PICTUR, TIMSTR )
!!                  WRITE (*,*) 'Sample format: ', SAMPLE
!!                  WRITE (*,*) 'Time picture : ', PICTUR
!!                  WRITE (*,*)
!!                  WRITE (*,*) 'Input UTC    : ', UTCSTR
!!                  WRITE (*,*) 'Output       : ', TIMSTR
!!
!!              END IF
!!
!!              END
!!
!!        When this program was executed on a Mac/Intel/gfortran/64-bit
!!        platform, the output was:
!!
!!         Sample format: Thu Oct 01 11:11:11 PDT 1111
!!         Time picture : Wkd Mon DD HR:MN:SC PDT YYYY ::UTC-7
!!
!!         Input UTC    : 26 Nov 2018  23:23:00 UTC
!!         Output       : Mon Nov 26 16:23:00 PDT 2018
!!
!!     2) Convert a UTC time to a string that contains both the
!!        calendar representations of the date as well as the Julian
!!        date; for example a string of the form:
!!
!!           "Thu Aug 01 09:47:16 PDT 1996 (2450297.1994 JDUTC)"
!!
!!        Use the LSK kernel below to load the leap seconds and time
!!        constants required for the conversions.
!!
!!           naif0012.tls
!!
!!        Example code begins here.
!!
!!              PROGRAM TIMOUT_EX2
!!              IMPLICIT NONE
!!
!!        C
!!        C     Local parameters.
!!        C
!!              INTEGER               TIMLEN
!!              PARAMETER           ( TIMLEN  = 80 )
!!
!!        C
!!        C     Local variables
!!        C
!!              CHARACTER*(TIMLEN)    PICTUR
!!              CHARACTER*(TIMLEN)    TIMSTR
!!              CHARACTER*(TIMLEN)    UTCSTR
!!
!!              DOUBLE PRECISION      ET
!!
!!        C
!!        C     Load LSK file.
!!        C
!!              CALL FURNSH ( 'naif0012.tls' )
!!
!!        C
!!        C     Convert the input UTC time to ephemeris time.
!!        C
!!              UTCSTR = '26 Nov 2018  16:23:00 UTC'
!!              CALL STR2ET ( UTCSTR, ET )
!!
!!        C
!!        C     Create the required time picture. This could be done
!!        C     using TPICTR.
!!        C
!!              PICTUR = 'Wkd Mon DD HR:MN ::UTC-7 YYYY '
!!             .      // '(JULIAND.#### JDUTC)'
!!
!!        C
!!        C      Now convert ET to the desired output format.
!!        C
!!               CALL TIMOUT ( ET, PICTUR, TIMSTR )
!!               WRITE (*,*) 'Input UTC: ', UTCSTR
!!               WRITE (*,*) 'Output   : ', TIMSTR
!!
!!               END
!!
!!        When this program was executed on a Mac/Intel/gfortran/64-bit
!!        platform, the output was:
!!
!!         Input UTC: 26 Nov 2018  16:23:00 UTC
!!         Output   : Mon Nov 26 09:23  2018 (2458449.1826 JDUTC)
!!
!! ## Restrictions
!!
!!     None.
!!
!! ## Literature References
!!
!!     None.
!!
!! ## Author and Institution
!!
!!     N.J. Bachman       (JPL)
!!     J. Diaz del Rio    (ODC Space)
!!     M.J. Spencer       (JPL)
!!     W.L. Taber         (JPL)
!!     E.D. Wright        (JPL)
!!
!! ## Version
!!
!!-    SPICELIB Version 3.4.0, 07-AUG-2021 (EDW) (JDR) (NJB)
!!
!!        Corrected typo preventing correct calculation of decimal
!!        values for HR.###... and MN.###... markers with ::UTC+N:M
!!        and ::UTC-N:M meta tags.
!!
!!        Added "::TT" as a time system meta marker equivalent-to/
!!        alias-for "::TDT". No change to functionality.
!!
!!        Corrected OUTPUT argument name in $Brief_I/O section (it was
!!        STRING) and improved its description in $Detailed_Output.
!!        Fixed call to TPICTR in $Particulars.
!!
!!        Edited the header to comply with NAIF standard. Removed
!!        unnecessary entries in $Revisions section. Converted the
!!        existing code fragments into complete examples and added
!!        references to required LSKs.
!!
!!        Updated $Exceptions section, rewording the existing entry
!!        and adding three additional cases.
!!
!!-    SPICELIB Version 3.3.1, 31-JAN-2017 (NJB)
!!
!!        Updated header comments to draw attention to the fact that
!!        rounding can be commanded.
!!
!!-    SPICELIB Version 3.3.0, 23-OCT-2005 (NJB)
!!
!!        Updated to remove non-standard use of duplicate arguments
!!        in RMAIND call. Replaced header references to LDPOOL with
!!        references to FURNSH.
!!
!!-    SPICELIB Version 3.2.0, 09-APR-2002 (WLT)
!!
!!        Added code to bracket the fractional part of a time component
!!        so that it cannot become negative due to inability to invert
!!        arithmetic operations with double precision arithmetic.
!!
!!-    SPICELIB Version 3.1.0, 21-JUN-2001 (WLT)
!!
!!        Added the format picture components ?ERA? and ?era? which
!!        vanish for years after 999 A.D.
!!
!!-    SPICELIB Version 3.0.2, 10-APR-2000 (WLT)
!!
!!        Declared SCAN to be external.
!!
!!-    SPICELIB Version 3.0.1, 22-JUN-1998 (WLT)
!!
!!        A number of typographical and grammatical errors
!!        were corrected in the header.
!!
!!-    SPICELIB Version 3.0.0, 30-DEC-1997 (WLT)
!!
!!        The previous version of this routine did not output
!!        fractional components for epochs prior to 1 A.D.
!!
!!        In addition, the default time system, calendar and time zone
!!        are obtained from TIMDEF.
!!
!!-    SPICELIB Version 2.0.0, 01-APR-1997 (WLT)
!!
!!        In the event that the format picture requested 'YR' as
!!        the first component of a time string, the previous edition
!!        of this routine used the year value corresponding to the
!!        last call to this routine (or whatever happened to be in
!!        memory on the first call). This error has been corrected.
!!
!!-    SPICELIB Version 1.0.0, 26-JUL-1996 (WLT) (MJS) (NJB)
!!
!! ## Index Entries
!!
!!     Convert and format d.p. seconds past J2000 as a string
!!
!! ## Revisions
!!
!!-    SPICELIB Version 3.0.0, 30-DEC-1997 (WLT)
!!
!!        The previous version of this routine did not output
!!        fractional components for epochs prior to 1 A.D.
!!
!!        This error was due to overuse of the original year
!!        component returned from TTRANS. The original year
!!        component is now saved for use in computing the fractional
!!        component. The modified year (used in printing B.C. epochs)
!!        is stored in a separate variable.
!!
!!-    SPICELIB Version 2.0.0, 01-APR-1997 (WLT)
!!
!!        In the event that the format picture requested 'YR' as
!!        the first component of a time string, the previous edition
!!        of this routine used the year value corresponding to the
!!        last call to this routine (or whatever happened to be in
!!        memory on the first call). This error has been corrected.
!!
!!        The error was fixed by recoding the following IF THEN statement
!!
!!              IF (       TYPE .EQ. YEAR
!!    .               .OR. TYPE .EQ. MONTH
!!    .               .OR. TYPE .EQ. MON
!!    .               .OR. TYPE .EQ. DAY
!!    .               .OR. TYPE .EQ. DOY
!!    .               .OR. TYPE .EQ. NOON
!!    .               .OR. TYPE .EQ. HOUR
!!    .               .OR. TYPE .EQ. ERA
!!    .               .OR. TYPE .EQ. AMPM
!!    .               .OR. TYPE .EQ. MINUTE
!!    .               .OR. TYPE .EQ. SEC   ) THEN
!!
!!        as
!!
!!              IF (       TYPE .EQ. YEAR
!!    .               .OR. TYPE .EQ. YR
!!    .               .OR. TYPE .EQ. MONTH
!!    .               .OR. TYPE .EQ. MON
!!    .               .OR. TYPE .EQ. DAY
!!    .               .OR. TYPE .EQ. DOY
!!    .               .OR. TYPE .EQ. NOON
!!    .               .OR. TYPE .EQ. HOUR
!!    .               .OR. TYPE .EQ. ERA
!!    .               .OR. TYPE .EQ. AMPM
!!    .               .OR. TYPE .EQ. MINUTE
!!    .               .OR. TYPE .EQ. SEC   ) THEN
!!
!!-    Beta Version 2.1.0, 17-MAR-1994 (MJS) (NJB)
!!
!!        Integer argument to BRCKTD changed from 0 to 0.0D0.
!!
!!     SPICELIB functions
!!
!!     Local parameters
!!
!!     The parameters below act essentially as an enumeration
!!     of the various kinds of components we will be looking at in the
!!     input time string.
!!
!!     The following parameters serve as an enumeration of the various
!!     time formats that are recognized.
!!
!!     The parameters below are used to declare the space needed for
!!     scanning the input format string.
!!
!!     The length of the local string that we will use for copying
!!     the format string.
!!
!!     Local variables
!!
!!     The next set of variables holds the marks and auxiliary
!!     arrays used for scanning the format string.
!!
!!     The variables below are used to hold, base formats, values of
!!     time vector components, adjustments to use when rounding,
!!     the lengths of the format pictures and whether or not various
!!     components have already been computed.
!!
!!     The array below contains the indexes of the various values
!!     associated with the three different times of time vectors
!!     that we will be using YMD, YD, CONTIN.
!!
!!     We will be making a local copy of the input format string
!!     and the input time.
!!
!!     The integers below are used to mark substring boundaries.
!!
!!     Times come in three flavors: TT or TDT, TDB, UTC.  The one used
!!     on this particular invocation of TIMOUT is stored in TIMTYP.
!!     The routine TTRANS needs to have input and output time vector
!!     types.  The one used based upon the input PICTUR is stored
!!     in BASTYP.
!!
!!     Loop counters and delimiters
!!
!!     Utility double precision numbers
!!
!!     The array power is used to assist in the truncation of double
!!     precision values.
!!
!!     calendar variables.
!!
!!     Character string representations for months and week days.
!!
!!     Save everything.
!!
!!     Initial values
!!
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: j1950, j2000, spd
      use pepper_arrays, only: bsrchc, isrchi
      use pepper_math, only: brcktd, brckti
      use pepper_strings, only: lcase, ljust, prefix, rtrim, scanrj, ucase
      implicit none
      integer(ip), parameter :: currnt = 1
      integer(ip), parameter :: begin = currnt
      integer(ip), parameter :: noname = currnt  + 1
      integer(ip), parameter :: year = noname + 1
      integer(ip), parameter :: yr = year   + 1
      integer(ip), parameter :: umon = yr     + 1
      integer(ip), parameter :: mmon = umon   + 1
      integer(ip), parameter :: lmon = mmon   + 1
      integer(ip), parameter :: umnth = lmon   + 1
      integer(ip), parameter :: mmnth = umnth  + 1
      integer(ip), parameter :: lmnth = mmnth  + 1
      integer(ip), parameter :: month = lmnth  + 1
      integer(ip), parameter :: doy = month  + 1
      integer(ip), parameter :: uwkd = doy    + 1
      integer(ip), parameter :: mwkd = uwkd   + 1
      integer(ip), parameter :: lwkd = mwkd   + 1
      integer(ip), parameter :: uweekd = lwkd   + 1
      integer(ip), parameter :: mweekd = uweekd + 1
      integer(ip), parameter :: lweekd = mweekd + 1
      integer(ip), parameter :: day = lweekd + 1
      integer(ip), parameter :: minute = day    + 1
      integer(ip), parameter :: hour = minute + 1
      integer(ip), parameter :: sec = hour   + 1
      integer(ip), parameter :: point = sec    + 1
      integer(ip), parameter :: place = point  + 1
      integer(ip), parameter :: julian = place  + 1
      integer(ip), parameter :: utc = julian + 1
      integer(ip), parameter :: tdb = utc    + 1
      integer(ip), parameter :: tdt = tdb    + 1
      integer(ip), parameter :: tt = tdt    + 1
      integer(ip), parameter :: sp2000 = tt     + 1
      integer(ip), parameter :: sp1950 = sp2000 + 1
      integer(ip), parameter :: round = sp1950 + 1
      integer(ip), parameter :: trunc = round  + 1
      integer(ip), parameter :: uera = trunc  + 1
      integer(ip), parameter :: lera = uera   + 1
      integer(ip), parameter :: uerax = lera + 1
      integer(ip), parameter :: lerax = uerax + 1
      integer(ip), parameter :: uampm = lerax   + 1
      integer(ip), parameter :: lampm = uampm  + 1
      integer(ip), parameter :: utcp = lampm  + 1
      integer(ip), parameter :: utcm = utcp   + 1
      integer(ip), parameter :: jcal = utcm   + 1
      integer(ip), parameter :: gcal = jcal   + 1
      integer(ip), parameter :: mcal = gcal   + 1
      integer(ip), parameter :: timsys = mcal   + 1
      integer(ip), parameter :: calndr = timsys + 1
      integer(ip), parameter :: ampm = calndr + 1
      integer(ip), parameter :: mon = ampm   + 1
      integer(ip), parameter :: wkday = mon    + 1
      integer(ip), parameter :: era = wkday  + 1
      integer(ip), parameter :: noon = era    + 1
      integer(ip), parameter :: rlyear = noon   + 1
      integer(ip), parameter :: finish = rlyear
      integer(ip), parameter :: ymd = 1
      integer(ip), parameter :: contin = ymd + 1
      integer(ip), parameter :: maxmrk = 43
      integer(ip), parameter :: room = 100
      integer(ip), parameter :: maxlen = 256
      integer(ip), parameter :: mrksiz = 8
      integer(ip), parameter :: loclen = 32
      real(wp), parameter :: power  ( 0 : 14 ) = &
         [ 1.0_wp, 1.0e1_wp, 1.0e2_wp, 1.0e3_wp, 1.0e4_wp, 1.0e5_wp, &
           1.0e6_wp, 1.0e7_wp, 1.0e8_wp, 1.0e9_wp, 1.0e10_wp, 1.0e11_wp, &
           1.0e12_wp, 1.0e13_wp, 1.0e14_wp ]
      character(9), parameter :: months ( 12 ) = &
         [ character(9) :: 'January', 'February', 'March', 'April', 'May', 'June', &
           'July', 'August', 'September', 'October', 'November', &
           'December' ]
      integer(ip), parameter :: mlen   ( 12 ) = &
         [ 7, 8, 5, 5, 3, 4, 4, 6, 9, 7, 8, 8 ]
      character(9), parameter :: wkdays (  7 ) = &
         [ character(9) :: 'Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', &
           'Friday', 'Saturday' ]
      integer(ip), parameter :: wklen  (  7 ) = &
         [ 6, 6, 7, 9, 8, 6, 8 ]
      real(wp) :: timpad
      integer(ip) :: ident  (  room   )
      integer(ip) :: start
      integer(ip) :: end    (  room   )
      logical :: doera
      character(9) :: mywkd
      logical :: unknwn
      integer(ip) :: ntokns
      logical :: ok
      real(wp) :: incr
      character(maxlen) :: mystr
      real(wp) :: x
      integer(ip) :: part
      integer(ip) :: type
      character(8) :: ymdfmt
      real(wp) :: factor
      integer(ip) :: jmonth
      character(9) :: mymon
      integer(ip) :: jday
      integer(ip) :: j
      real(wp) :: moff
      integer(ip) :: indx
      integer(ip) :: gmonth
      integer(ip) :: jdoy
      integer(ip) :: montyp
      integer(ip) :: caltyp
      real(wp) :: myet
      integer(ip) :: beg    (  room   )
      integer(ip) :: gyear
      integer(ip) :: gday
      integer(ip) :: i
      integer(ip) :: jyear
      integer(ip) :: b
      logical :: making
      integer(ip) :: stopat
      real(wp) :: intmed
      integer(ip) :: e
      logical :: pumpup
      character(8) :: ywfmt
      real(wp) :: hoff
      integer(ip) :: nmarks
      logical :: vanish
      integer(ip) :: appnd
      real(wp) :: delta
      real(wp) :: frac
      integer(ip) :: timtyp
      integer(ip) :: trncat
      integer(ip) :: gdoy
      character(maxlen) :: substr
      integer(ip) :: wktyp
      integer(ip) :: last
      logical :: go2jul
      logical :: dozone
      real(wp) :: tempd
      character(16) :: bastyp
      if ( .not. allocated ( ctx%timout_state ) ) allocate ( ctx%timout_state )
      associate ( first => ctx%timout_state%first, &
                  marks => ctx%timout_state%marks, &
                  class => ctx%timout_state%class, &
                  mrklen => ctx%timout_state%mrklen, &
                  pntrs => ctx%timout_state%pntrs, &
                  dump => ctx%timout_state%dump, &
                  ndump => ctx%timout_state%ndump, &
                  timfmt => ctx%timout_state%timfmt, &
                  numtyp => ctx%timout_state%numtyp, &
                  orignl => ctx%timout_state%orignl, &
                  values => ctx%timout_state%values, &
                  pad => ctx%timout_state%pad, &
                  length => ctx%timout_state%length, &
                  id => ctx%timout_state%id, &
                  have => ctx%timout_state%have, &
                  compnt => ctx%timout_state%compnt, &
                  string => ctx%timout_state%string, &
                  fmt => ctx%timout_state%fmt, &
                  width => ctx%timout_state%width, &
                  intyp => ctx%timout_state%intyp, &
                  tsys => ctx%timout_state%tsys, &
                  cal => ctx%timout_state%cal, &
                  zon => ctx%timout_state%zon, &
                  ntvec => ctx%timout_state%ntvec, &
                  ptvec => ctx%timout_state%ptvec, &
                  tvec => ctx%timout_state%tvec, &
                  value => ctx%timout_state%value, &
                  mylen => ctx%timout_state%mylen )
      if ( ctx%failed() ) return

!
!     Chapter 1. Initializations.
!     =================================================================
!
!     On the first pass, we need to set up the recognized tokens
!     that will be used for scanning, the classes of these tokens
!     and the array of ID's for time systems.
!
      if ( first ) then

         first    = .false.

         marks (  1 ) = 'YYYY'
         marks (  2 ) = 'YR'
         marks (  3 ) = 'MON'
         marks (  4 ) = 'Mon'
         marks (  5 ) = 'mon'
         marks (  6 ) = 'MONTH'
         marks (  7 ) = 'Month'
         marks (  8 ) = 'month'
         marks (  9 ) = 'MM'
         marks ( 10 ) = 'DOY'
         marks ( 11 ) = 'WKD'
         marks ( 12 ) = 'Wkd'
         marks ( 13 ) = 'wkd'
         marks ( 14 ) = 'WEEKDAY'
         marks ( 15 ) = 'Weekday'
         marks ( 16 ) = 'weekday'
         marks ( 17 ) = 'DD'
         marks ( 18 ) = 'MN'
         marks ( 19 ) = 'HR'
         marks ( 20 ) = 'SC'
         marks ( 21 ) = '.#'
         marks ( 22 ) = '#'
         marks ( 23 ) = 'JULIAND'
         marks ( 24 ) = '::UTC'
         marks ( 25 ) = '::TDB'
         marks ( 26 ) = '::TDT'
         marks ( 27 ) = '::TT'
         marks ( 28 ) = 'SP2000'
         marks ( 29 ) = 'SP1950'
         marks ( 30 ) = '::RND'
         marks ( 31 ) = '::TRNC'
         marks ( 32 ) = 'ERA'
         marks ( 33 ) = 'era'
         marks ( 34 ) = 'AMPM'
         marks ( 35 ) = 'ampm'
         marks ( 36 ) = '::UTC+'
         marks ( 37 ) = '::UTC-'
         marks ( 38 ) = '::JCAL'
         marks ( 39 ) = '::GCAL'
         marks ( 40 ) = '::MCAL'
         marks ( 41 ) = 'AP'
         marks ( 42 ) = '?ERA?'
         marks ( 43 ) = '?era?'

         nmarks       =  43

         call scanpr ( ctx, nmarks, marks, mrklen, pntrs )

!
!        Now that we've prepared our recognized substrings and
!        auxiliary arrays for scanning, collect the id's of the
!        various marks and classify the various marks.
!        substrings.
!
         id ( noname ) = 0
         id ( year   ) = bsrchc ( 'YYYY',    nmarks, marks )
         id ( yr     ) = bsrchc ( 'YR',      nmarks, marks )
         id ( umon   ) = bsrchc ( 'MON',     nmarks, marks )
         id ( mmon   ) = bsrchc ( 'Mon',     nmarks, marks )
         id ( lmon   ) = bsrchc ( 'mon',     nmarks, marks )
         id ( umnth  ) = bsrchc ( 'MONTH',   nmarks, marks )
         id ( mmnth  ) = bsrchc ( 'Month',   nmarks, marks )
         id ( lmnth  ) = bsrchc ( 'month',   nmarks, marks )
         id ( month  ) = bsrchc ( 'MM',      nmarks, marks )
         id ( doy    ) = bsrchc ( 'DOY',     nmarks, marks )
         id ( uwkd   ) = bsrchc ( 'WKD',     nmarks, marks )
         id ( mwkd   ) = bsrchc ( 'Wkd',     nmarks, marks )
         id ( lwkd   ) = bsrchc ( 'wkd',     nmarks, marks )
         id ( uweekd ) = bsrchc ( 'WEEKDAY', nmarks, marks )
         id ( mweekd ) = bsrchc ( 'Weekday', nmarks, marks )
         id ( lweekd ) = bsrchc ( 'weekday', nmarks, marks )
         id ( day    ) = bsrchc ( 'DD',      nmarks, marks )
         id ( minute ) = bsrchc ( 'MN',      nmarks, marks )
         id ( hour   ) = bsrchc ( 'HR',      nmarks, marks )
         id ( sec    ) = bsrchc ( 'SC',      nmarks, marks )
         id ( point  ) = bsrchc ( '.#',      nmarks, marks )
         id ( place  ) = bsrchc ( '#',       nmarks, marks )
         id ( julian ) = bsrchc ( 'JULIAND', nmarks, marks )
         id ( utc    ) = bsrchc ( '::UTC',   nmarks, marks )
         id ( tdb    ) = bsrchc ( '::TDB',   nmarks, marks )
         id ( tdt    ) = bsrchc ( '::TDT',   nmarks, marks )
         id ( tt     ) = bsrchc ( '::TT',    nmarks, marks )
         id ( sp2000 ) = bsrchc ( 'SP2000',  nmarks, marks )
         id ( sp1950 ) = bsrchc ( 'SP1950',  nmarks, marks )
         id ( round  ) = bsrchc ( '::RND',   nmarks, marks )
         id ( trunc  ) = bsrchc ( '::TRNC',  nmarks, marks )
         id ( uera   ) = bsrchc ( 'ERA',     nmarks, marks )
         id ( lera   ) = bsrchc ( 'era',     nmarks, marks )
         id ( uerax  ) = bsrchc ( '?ERA?',   nmarks, marks )
         id ( lerax  ) = bsrchc ( '?era?',   nmarks, marks )
         id ( uampm  ) = bsrchc ( 'AMPM',    nmarks, marks )
         id ( lampm  ) = bsrchc ( 'ampm',    nmarks, marks )
         id ( utcp   ) = bsrchc ( '::UTC+',  nmarks, marks )
         id ( utcm   ) = bsrchc ( '::UTC-',  nmarks, marks )
         id ( jcal   ) = bsrchc ( '::JCAL',  nmarks, marks )
         id ( gcal   ) = bsrchc ( '::GCAL',  nmarks, marks )
         id ( mcal   ) = bsrchc ( '::MCAL',  nmarks, marks )
         id ( ampm   ) = bsrchc ( 'AP',      nmarks, marks )

         class ( id(noname)  ) =  noname
         class ( id(year)    ) =  year
         class ( id(yr)      ) =  yr
         class ( id(umon)    ) =  mon
         class ( id(mmon)    ) =  mon
         class ( id(lmon)    ) =  mon
         class ( id(umnth)   ) =  mon
         class ( id(mmnth)   ) =  mon
         class ( id(lmnth)   ) =  mon
         class ( id(month)   ) =  month
         class ( id(doy)     ) =  doy
         class ( id(uwkd)    ) =  wkday
         class ( id(mwkd)    ) =  wkday
         class ( id(lwkd)    ) =  wkday
         class ( id(uweekd)  ) =  wkday
         class ( id(mweekd)  ) =  wkday
         class ( id(lweekd)  ) =  wkday
         class ( id(day)     ) =  day
         class ( id(minute)  ) =  minute
         class ( id(hour)    ) =  hour
         class ( id(sec)     ) =  sec
         class ( id(point)   ) =  point
         class ( id(place)   ) =  place
         class ( id(julian)  ) =  julian
         class ( id(utc)     ) =  timsys
         class ( id(tdb)     ) =  timsys
         class ( id(tdt)     ) =  timsys
         class ( id(tt)      ) =  timsys
         class ( id(sp2000)  ) =  sp2000
         class ( id(sp1950)  ) =  sp1950
         class ( id(round)   ) =  round
         class ( id(trunc)   ) =  trunc
         class ( id(uera)    ) =  era
         class ( id(lera)    ) =  era
         class ( id(uerax)   ) =  era
         class ( id(lerax)   ) =  era
         class ( id(uampm)   ) =  noon
         class ( id(lampm)   ) =  noon
         class ( id(utcp)    ) =  timsys
         class ( id(utcm)    ) =  timsys
         class ( id(jcal)    ) =  calndr
         class ( id(gcal)    ) =  calndr
         class ( id(mcal)    ) =  calndr
         class ( id(ampm)    ) =  ampm

         do i = begin, finish
            pad (i) = 0.0_wp
         end do

         pad  ( sec    )     =   0.5_wp
         pad  ( minute )     =  60.0_wp * pad(sec)
         pad  ( hour   )     =  60.0_wp * pad(minute)
         pad  ( day    )     =  24.0_wp * pad(hour)
         pad  ( month  )     =  30.0_wp * pad(day)
         pad  ( doy    )     =           pad(day)
         pad  ( mon    )     =           pad(month)
         pad  ( year   )     = 365.0_wp * pad(day)
         pad  ( yr     )     = 365.0_wp * pad(day)
         pad  ( julian )     =           pad(day)
         pad  ( sp2000 )     =           pad(sec)
         pad  ( sp1950 )     =           pad(sec)
         pad  ( ampm   )     =           pad(hour)

!
!        After we've made the initial scan for tokens and determined
!        the time system requested, we will want to get rid of the
!        time system tokens.
!
         dump (  1 ) = id(utc)
         dump (  2 ) = id(tdt)
         dump (  3 ) = id(tt)
         dump (  4 ) = id(tdb)
         dump (  5 ) = id(round)
         dump (  6 ) = id(trunc)
         dump (  7 ) = id(utcm)
         dump (  8 ) = id(utcp)
         dump (  9 ) = id(jcal)
         dump ( 10 ) = id(gcal)
         dump ( 11 ) = id(mcal)

         ndump      = 11

!
!        Set up the default formats for the various time components
!
         orignl(year)   = 'YYYY'
         length(year)   =  4

         orignl(yr)     = '0Y'
         length(yr)     =  2

         orignl(doy)    = '0DD'
         length(doy)    =  3

         orignl(day)    = '0D'
         length(day)    =  2

         orignl(month)  = '0M'
         length(month)  =  2

         orignl(hour)   = '0H'
         length(hour)   =  2

         orignl(ampm)   = '0H'
         length(ampm)   =  2

         orignl(minute) = '0M'
         length(minute) =  2

         orignl(sec)    = '0S'
         length(sec)    =  2

         orignl(julian) = 'XXXXXXX'
         length(julian) =  7

         orignl(sp2000) = 'XXXXXXXXXXX'
         length(sp2000) =  11

         orignl(sp1950) = 'XXXXXXXXXXX'
         length(sp1950) =  11

!
!        Finally set up the component pointers...
!
         compnt(1,ymd)    = rlyear
         compnt(2,ymd)    = month
         compnt(3,ymd)    = day
         compnt(4,ymd)    = hour
         compnt(5,ymd)    = minute
         compnt(6,ymd)    = sec

         compnt(1,contin) = currnt

      end if

!
!     Chapter 2.  Parsing the input picture.
!     ==============================================================
!
!     First let's copy the input picture into local storage
!     (left justified) and get just past the end of the
!     significant portion (this way the loop that constructs the
!     output string will terminate with no unfinished business
!     left to resolve).
!
      mystr = ' '
      call    ljust ( pictur, mystr(1:maxlen-1) )
      e     = rtrim (         mystr             ) + 1
      start = 1

!
!     Scan the input string.
!
      call scan ( ctx, mystr(1:e), &
                  marks,      mrklen, pntrs,  room,  start, &
                  ntokns,     ident,  beg,    end           )

!
!     Locate the time system that will be used.  This must
!     be one of the following: UTC, TDB, TT, TDT
!
      unknwn = .true.
      go2jul = .false.
      dozone = .false.
      i      = 1
      hoff   = 0.0_wp
      moff   = 0.0_wp

!
!     Get the default time type from TIMDEF
!
      call timdef ( ctx, 'GET', 'SYSTEM', tsys )

      if ( tsys == 'UTC' ) then
         timtyp = id(utc)
      else if ( tsys == 'TDB' ) then
         timtyp = id(tdb)
      else if ( (tsys == 'TDT') .or. (tsys == 'TT') ) then
         timtyp = id(tdt)
      else
         timtyp = id(utcp)

         call timdef ( ctx, 'GET', 'ZONE', zon )

         call prefix ( '::', 0, zon )
         call zzutcpm( ctx, zon, 1, hoff, moff, last, ok )

         dozone = ok

!
!        The routine TIMDEF uses ZZUTCPM to determine whether
!        or not a time zone is legitimate before it stores it
!        to be "GOTTEN."  As a result the value of OK should
!        always be .TRUE.  However, just in case TIMDEF should
!        someday use something other that ZZUTCPM for checking
!        we put in the unneeded check below.
!
         if ( .not. ok ) then
            timtyp = id(utc)
         end if

      end if

      do while ( unknwn .and. i <= ntokns )

         if ( class(ident(i)) == timsys ) then
            timtyp = ident(i)
            unknwn = .false.
            dozone = .false.

            if(      ident(i) == id(utcp) &
                .or. ident(i) == id(utcm) ) then

!
!              We've got a time zone specification. Parse it and
!              store the offsets from UTC.
!
               call zzutcpm ( ctx, mystr, beg(i), hoff, moff, last, ok )

               if ( ok ) then

                  dozone = .true.
                  timtyp = id(utcp)

!
!                 If we ran all the way up to the end of the next
!                 token, we simply reset the identity of the next
!                 token to be a zone type and increment  I.
!
!                 This way we never see the next token in this loop
!                 and it gets removed later when time systems and
!                 other meta markers from  our copy of the time
!                 format string.
!
                  if ( last == end(i+1) ) then
                     ident(i+1) = ident(i)
                     i          = i + 1
                  else
                     end(i)     = last
                     beg(i+1)   = last + 1
                  end if

               end if

            end if

         end if

         i = i + 1

      end do

!
!     Determine whether we should use the Julian or gregorian (default)
!     calendar
!
      unknwn = .true.
      i      = 1

!
!     Get the default calendar from TIMDEF.
!
      call timdef ( ctx, 'GET', 'CALENDAR', cal )

      if      ( cal == 'GREGORIAN' ) then
         caltyp = id(gcal)
      else if ( cal == 'JULIAN'    ) then
         caltyp = id(jcal)
      else
         caltyp = id(mcal)
      end if

      do while ( unknwn .and. i <= ntokns )

         if ( class(ident(i)) == calndr ) then
            caltyp = ident(i)
            unknwn = .false.
         end if

         i = i + 1
      end do

!
!     Next determine whether or not we shall be performing rounding
!     on output.
!
      pumpup =      isrchi ( id(round), ntokns, ident ) /= 0

!
!     Determine if we have an Era specification
!
      doera  =      isrchi ( id(lera),  ntokns, ident ) /= 0 &
               .or. isrchi ( id(uera),  ntokns, ident ) /= 0 &
               .or. isrchi ( id(uerax), ntokns, ident ) /= 0 &
               .or. isrchi ( id(lerax), ntokns, ident ) /= 0

!
!     Until we've examined the year, we assume that the era is not
!     supposed to vanish.
!
      vanish = .false.
!
!     Next remove all of the time system dudes from the list of
!     tokens.
!
      call scanrj ( dump, ndump, ntokns, ident, beg, end )

!
!     If the user wants to round the output, we need to pump up ET
!     by the smallest significant part of the input picture.  But
!     in either case we are going to pad the input time.  For now
!     we pad it by zero.
!
      timpad = 0.0_wp

      if ( pumpup ) then

!
!        We need to determine the amount to pad ET by.  So we need
!        to look at the string and find the least significant component
!        that has been requested.  Keep in mind that the last token
!        is of type NONAME (its a blank) by construction.
!
         i = 1

         do while ( i <= ntokns )

            type = class ( ident(i) )

            if   (      ( type == noname ) &
                   .or. ( type == point  ) &
                   .or. ( type == place  ) &
                   .or. ( type == noon   ) &
                   .or. ( type == era    ) &
                   .or. ( type == mon    ) &
                   .or. ( type == wkday  ) ) then

!
!              Don't do anything, just go on to the next token.
!
               i = i + 1

            else

!
!              Look up the amount we should pad our time by.
!
               factor = 1.0_wp
               incr   = pad ( type )

!
!              Examine the next token.  If it's not a decimal point
!              and marker, we have the least significant part of
!              this component.
!
               i      = i + 1
               type   = class(ident(i))

               if ( type == point ) then

                  factor = factor * 0.1_wp
                  i      = i + 1

!
!                 Now just look for the end of the string of place
!                 holders
!
                  do while ( ident(i) == id(place) )
                     factor = factor * 0.1_wp
                     i      = i + 1
                  end do

               end if

!
!              Now compute the time pad for this component of the
!              time string.
!
               incr = incr * factor

               if ( timpad /= 0.0_wp ) then
                  timpad = min ( timpad, incr )
               else
                  timpad = incr
               end if

            end if

         end do

      end if

!
!     Right now we don't have any components of the time format
!     and we don't need any of them so far.
!
      do part = begin, finish
         have(part) = .false.
      end do

!
!     Set up the input time format and the output time format that will
!     be used later.
!
!     The input time format is used to convert the basic ET we have now
!     to one of the various time formats that are supported by the
!     routine TTRANS.  If we are going to construct a string in one of
!     the dynamical time systems we will call the input time a formal
!     time in seconds past a formal calendar epoch of J2000.  If on the
!     other hand we are going to construct a UTC based string, we will
!     convert our ET to an earth based epoch (TT, TDT) and use this as
!     our base input system.
!
      myet = et

      if      ( timtyp == id(tdb) ) then

!
!        Since we are likely to need SP2000, SP1950 or JD, we
!        compute them now.
!
         myet           = myet + timpad
         values(sp2000) = myet
         values(julian) = unitim( ctx, myet,   'TDB',  'JDTDB'  )
         values(sp1950) = values(sp2000) + spd*( j2000 - j1950 )

         bastyp         = 'FORMAL'
         ymdfmt         = 'YMDF'
         ywfmt          = 'YMWDF'
         have  (sp2000) = .true.
         have  (sp1950) = .true.
         have  (julian) = .true.

      else if ( (timtyp == id(tdt)) .or. &
                (timtyp == id(tt)) ) then

         myet           = unitim( ctx, myet,   'TDB',  'TDT'    )  + timpad
         values(sp2000) = myet
         values(julian) = unitim( ctx, myet,   'TDT',  'JDTDT'  )
         values(sp1950) = values(sp2000) + spd*( j2000 - j1950 )

         bastyp         = 'FORMAL'
         ymdfmt         = 'YMDF'
         ywfmt          = 'YMWDF'
         have  (sp2000) = .true.
         have  (sp1950) = .true.
         have  (julian) = .true.

      else

!
!        In this case we convert to an earth based frame for our
!        working epoch.  This rounds properly when it's time to get
!        fractional components.
!
         myet   =  unitim ( ctx, myet, 'TDB', 'TDT' ) + timpad
         bastyp = 'TDT'
         ymdfmt = 'YMD'
         ywfmt  = 'YMWD'

      end if

!
!     Chapter 3.  Building the Output String
!     ==================================================================
!

!
!     Now we are ready to go, we need to fetch the tokens
!     and construct the output string.  We will
!     put the next portion of the output at APPND
!
      appnd  =  1
      making = .false.

      do i = 1, ntokns

         type    = class( ident(i) )
         tvec(1) = myet

!
!        If the next marker is not one we use as a place holder
!        in the fractional part of decimal formats AND we
!        are in the process of building a format, then the format
!        building is done.  We can construct the component and
!        append it to the string we are building.
!
         if ( (type /= place) .and.  making ) then

!
!           We also need to be sure this isn't a decimal point
!           marker before we add on to the output string.
!
            if ( (type /= point) .or. have(point) ) then

!
!              We are going to truncate the number to the number of
!              places requested NOT round.
!
               trncat = brckti( width - length(numtyp) - 1, 0, 14 )
               frac   = value - dint(value )

               if ( frac < 0 ) then
                  value = value - 1.0_wp
                  frac  = frac  + 1.0_wp
               end if

               intmed = ( dint( frac * power(trncat) ) - 0.125_wp ) &
                                      / power(trncat)

               frac   = brcktd( intmed,      0.0_wp,      1.0_wp )

               value  = dint(value) + frac

               call dpfmt ( ctx, value, fmt, substr )

               string(appnd:) = substr
               appnd          = appnd + width
               have  (point)  =.false.
               making         =.false.

            end if

         end if

!
!        If the token isn't recognized we can just
!        append it to the string we are constructing and
!        adjust the point at which the next substring is
!        to be appended.
!
         if ( type == noname ) then

            string(appnd:) = mystr ( beg(i) : end(i) )
            appnd          = appnd - beg(i) + end(i) + 1

!
!        If the token is a place holder, we either just append it
!        or tack it on to a format string we are creating..
!
         else if ( type == place ) then

            if ( making ) then
               b           = width + 1
               e           = b     - beg(i) + end(i)
               fmt(b:e)    = mystr ( beg(i) : end(i) )
               width       = width - beg(i) + end(i) + 1
            else
               string(appnd:appnd) = mystr ( beg(i) : end(i) )
               appnd               = appnd - beg(i) + end(i) + 1
            end if

!
!        If the token is the decimal point plus place holder
!        AND we are making a format, we append it to the current
!        format and determine the fractional part of the current
!        quantity.
!
         else if ( type == point ) then

            if ( .not. making ) then

               b           =  appnd
               e           =  appnd - beg(i) + end(i)
               string(b:e) =  mystr ( beg(i) : end(i) )
               appnd       =  e + 1
               have(point) = .false.

            else if ( timfmt == contin ) then

               b           = width + 1
               e           = b     - beg(i) + end(i)
               fmt(b:e)    = mystr ( beg(i) : end(i) )
               width       = e
               have(point) =.true.

            else

               b           = width + 1
               e           = b     - beg(i) + end(i)
               fmt(b:e)    = mystr ( beg(i) : end(i) )
               width       = e
               have(point) =.true.

!
!              Since we obviously are going to be needing
!              the fractional part of this component we fetch it
!              now and add it to whatever the integer part of the
!              current value is.  Here's how we do this.
!              If we truncated the input time to this component
!              we'd have a value on an "integer" portion of the
!              time scale.
!              .
!              .               current
!              .               time
!              .               truncated    .---MYET
!                                   |       |
!                                   v       v
!              time scale: ---------+-------X-----------+-----
!                                                       ^
!                                                       |
!              .                               truncated time
!              .                               plus 1 in this
!              .                               component
!              .
!              Add one to the truncated component to get the
!              next integer component.  Finally we convert these
!              two constructed stings to seconds so that we can
!              get the "fractional part" of the current component.
!              Fortunately, when we computed the integer value
!              for this component we constructed the time
!              vectors we need, so we don't have to go to a lot
!              of trouble now.
!

               call ttrans( ctx, intyp, bastyp, ptvec  )
               call ttrans( ctx, intyp, bastyp, ntvec  )

               delta = max(1.0_wp, ntvec(1) - ptvec(1) )
               frac  = brcktd( 0.0_wp, 1.0_wp, (myet - ptvec(1)) / delta )

               value = value + frac

            end if

         else

!
!        If we get to this point we have an honest time
!        string component to fetch.  We might already have
!        this guy.  If so we can just collect him from the
!        values buffer (although this collection is performed
!        after the next long IF-THEN block that gets the value
!        if we don't already have it).
!
            making      =.true.
            have(point) =.false.
            fmt         = orignl( type )
            width       = length( type )
            numtyp      =         type

            if ( .not. have(type) )  then

               tvec(1) = myet

!
!              Most components are handled in the next block.
!
               if (       (type == year) &
                     .or. (type == yr) &
                     .or. (type == month) &
                     .or. (type == mon) &
                     .or. (type == day) &
                     .or. (type == doy) &
                     .or. (type == noon) &
                     .or. (type == hour) &
                     .or. (type == era) &
                     .or. (type == ampm) &
                     .or. (type == minute) &
                     .or. (type == sec)   ) then

                  call ttrans ( ctx, bastyp, ymdfmt, tvec )

!
!                 The seconds component is finished.  Regardless
!                 of any zone or calendar modifications, we just
!                 don't have to deal with this component any more.
!
                  values(sec)    = tvec(6)

!
!                 If we need to deal with time zones, this is
!                 the time to do it.
!
                  if ( timtyp == id(utcp) ) then

                     tvec(4) = tvec(4) + hoff
                     tvec(5) = tvec(5) + moff
                     tvec(6) = 0.0_wp

                     call ttrans ( ctx, 'YMDF', 'YMDF', tvec )

                  end if

!
!                 One way or the other the hours and minutes components
!                 are finished.  Record their values.
!
                  values(hour)   = tvec(4)
                  values(minute) = tvec(5)

                  if ( values(hour) == 0.0_wp ) then
                     values(ampm) =  12.0_wp
                  else if ( values(hour) > 12.0_wp ) then
                     values(ampm) = values(hour) - 12.0_wp
                  else
                     values(ampm) = values(hour)
                  end if

!
!                 Finally, if we need to change the calendar to
!                 Julian this is the place to handle it.
!
                  jyear  = nint( tvec(1) )
                  jmonth = nint( tvec(2) )
                  jday   = nint( tvec(3) )

                  call gr2jul ( ctx, jyear, jmonth, jday, jdoy )

                  gyear  = jyear
                  gmonth = jmonth
                  gday   = jday

                  call jul2gr ( ctx, gyear, gmonth, gday, gdoy )

                  if      ( caltyp == id(gcal) ) then

                     values(year)  = real(gyear, wp)
                     values(month) = real(gmonth, wp)
                     values(day)   = real(gday, wp)
                     values(doy)   = real(gdoy, wp)
                     go2jul        = .false.

                  else if ( caltyp == id(jcal) ) then

                     values(year)  = real(jyear, wp)
                     values(month) = real(jmonth, wp)
                     values(day)   = real(jday, wp)
                     values(doy)   = real(jdoy, wp)
                     go2jul        = .true.

                  else if ( caltyp == id(mcal) ) then

                     if      ( gyear < 1582 ) then
                        go2jul = .true.
                     else if ( gyear > 1582 ) then
                        go2jul = .false.
                     else if ( gmonth < 10 ) then
                        go2jul = .true.
                     else if ( gmonth > 10 ) then
                        go2jul = .false.
                     else if ( gday   >= 15 ) then
                        go2jul = .false.
                     else
                        go2jul = .true.
                     end if

                     if ( go2jul ) then
                        values(year)  = real(jyear, wp)
                        values(month) = real(jmonth, wp)
                        values(day)   = real(jday, wp)
                        values(doy)   = real(jdoy, wp)
                     else
                        values(year)  = real(gyear, wp)
                        values(month) = real(gmonth, wp)
                        values(day)   = real(gday, wp)
                        values(doy)   = real(gdoy, wp)
                     end if

                  end if

!
!                 Determine the era associated with the epoch.  Also
!                 if the year component is negative, we handle  that
!                 now.
!
!                 We store the actual value of the year so that
!                 it can be used when determining rounding of
!                 other components.
!
                  values(rlyear) = values(year)

                  if ( doera ) then

                     if ( values(year) < 1 ) then
                        values(year) = 1.0_wp - values(year)
                        values(era)  = 1.0_wp
                     else
                        values(era)  = 2.0_wp
                     end if

                     vanish = values(year) >= 1000.0_wp

                  end if

!
!                 Fetch the last two digits of the year.
!
                  call rmaind ( ctx, values(year), 100.0_wp, x, tempd )
                  values(yr) = tempd

                  have  (year)   = .true.
                  have  (yr)     = .true.
                  have  (doy)    = .true.
                  have  (month)  = .true.
                  have  (mon)    = .true.
                  have  (day)    = .true.
                  have  (hour)   = .true.
                  have  (minute) = .true.
                  have  (sec)    = .true.
                  have  (ampm)   = .true.
                  have  (era)    = .true.

               else if ( type == wkday ) then

                  tvec(1) = myet

                  call ttrans ( ctx, bastyp, ywfmt, tvec )

!
!                 Weekday. If we need to deal with time zones, this is
!                 the time to do it.
!
                  if ( timtyp == id(utcp) ) then

                     tvec(5) = tvec(5) + hoff
                     tvec(6) = tvec(6) + moff
                     tvec(7) = 0.0_wp

                     call ttrans ( ctx, 'YMWDF', 'YMWDF', tvec )

                  end if

                  values(wkday)  = tvec(4)
                  have  (wkday)  =.true.

               else if (      ( type == sp1950 ) &
                         .or. ( type == sp2000 ) ) then

!
!                 The only way to get here is if the output time
!                 type is UTC or a time zone (otherwise we'd
!                 already HAVE SP2000 and SP1950).
!
                  tvec(1) = myet

                  call ttrans ( ctx, bastyp, 'FORMAL', tvec )

                  values(sp2000) = tvec(1)
                  values(sp1950) = values(sp2000) &
                                 + spd * ( j2000 - j1950 )

                  have  (sp2000) = .true.
                  have  (sp1950) = .true.

               else if ( type == julian ) then

!
!                 The same tale can be told here as in the last
!                 case.  We can only get here if this is UTC
!                 output.
!
                  tvec(1) = myet

                  call ttrans ( ctx, bastyp, 'JDUTC', tvec )

                  values(julian) = tvec(1)
                  have  (julian) =.true.

               end if

            end if

!
!           O.K. whatever thing we are about to construct, we now
!           have it's numeric value.  It's time to construct its
!           string  value.
!
!           We need to treat character months, weekdays, eras, a.m.'s
!           and p.m.'s specially.
!
            if ( type == mon ) then

               indx           = nint  ( values(month) )
               mymon          = months( indx )
               montyp         = ident ( i )

!
!              There is no ELSE case in the block below because all of
!              the possible MONTYP values are checked explicitly.
!
               if      ( montyp == id(umon)  ) then
                  call ucase ( mymon, mymon )
                  mymon(4:) = ' '
                  mylen     = 3
               else if ( montyp == id(mmon)   ) then
                  mymon(4:) = ' '
                  mylen     = 3
               else if ( montyp == id(lmon)   ) then
                  call lcase ( mymon, mymon )
                  mymon(4:) = ' '
                  mylen     = 3
               else if ( montyp == id(mmnth) ) then
                  mylen     = mlen(indx)
               else if ( montyp == id(umnth) ) then
                  call ucase ( mymon, mymon )
                  mylen     = mlen(indx)
               else if ( montyp == id(lmnth) ) then
                  call lcase ( mymon, mymon )
                  mylen     = mlen(indx)
               end if

               string(appnd:) = mymon
               appnd          = appnd + mylen
               making         =.false.

            else if ( type == wkday ) then

               indx           = nint  ( values(wkday) )
               mywkd          = wkdays( indx )
               wktyp          = ident ( i )

!
!              There is no ELSE case in the block below because all of
!              the possible WKTYP values are checked explicitly.
!
               if      ( wktyp == id(uwkd)   ) then
                  call ucase ( mywkd, mywkd )
                  mywkd(4:) = ' '
                  mylen     = 3
               else if ( wktyp == id(mwkd)   ) then
                  mywkd(4:) = ' '
                  mylen     = 3
               else if ( wktyp == id(lwkd)   ) then
                  call lcase ( mywkd, mywkd )
                  mywkd(4:) = ' '
                  mylen     = 3
               else if ( wktyp == id(mweekd) ) then
                  mylen     = wklen(indx)
               else if ( wktyp == id(uweekd) ) then
                  call ucase ( mywkd, mywkd )
                  mylen     = wklen(indx)
               else if ( wktyp == id(lweekd) ) then
                  call lcase ( mywkd, mywkd )
                  mylen     = wklen(indx)
               end if

               string(appnd:) = mywkd
               appnd          = appnd + mylen
               making         =.false.

            else if ( type == era ) then

               if      (         values(era)    == 2.0_wp &
                         .and. (      ident(i) == id(uera) &
                                 .or. ident(i) == id(uerax) ) ) then

                  string(appnd:) = ' A.D.'

               else if (         values(era)   == 2.0_wp &
                         .and. (      ident(i) == id(lera) &
                                 .or. ident(i) == id(lerax) ) ) then
                   string(appnd:) = ' a.d.'

               else if  (         values(era)  == 1.0_wp &
                         .and. (      ident(i) == id(uera) &
                                 .or. ident(i) == id(uerax) ) ) then

                  string(appnd:) = ' B.C.'

               else

                  string(appnd:) = ' b.c.'

               end if
!
!              If we have the vanishing kind of era, and we've
!              determined that it needs to vanish, then blank out the
!              portion of the string we just filled in. and don't
!              increment the place holder.
!
               if (     ident(i) == id(uerax) &
                   .or. ident(i) == id(lerax)) then

                  if ( vanish ) then
                     string(appnd:) = ' '
                     appnd = appnd + 1
                  else
                     appnd = appnd + 6
                  end if

               else
                  call ljust ( string(appnd:), string(appnd:) )
                  appnd          = appnd + 4
               end if

               making         =.false.

            else if ( type == noon ) then

               if      (       ident(i)   == id(uampm) &
                         .and. values(hour) >= 12.0_wp    ) then

                  string(appnd:) = 'P.M.'

               else if (       ident(i)    == id(uampm) &
                         .and. values(hour) < 12.0_wp    ) then

                  string(appnd:) = 'A.M.'

               else if (       ident(i)    == id(lampm) &
                         .and. values(hour) >= 12.0_wp    ) then

                  string(appnd:) = 'p.m.'

               else

                  string(appnd:) = 'a.m.'

               end if

               appnd          = appnd + 4
               making         =.false.

            else

               value = values( type )

            end if

!
!           If we are now creating a format string, we should
!           construct the previous time representation and
!           the next for this component (just in case we need it
!           later).
!
            if ( making ) then

!
!              We store the value of our current type in the
!              CURRNT slot of the values array.  This value
!              is used by the single numeric types, JD, SP2000,
!              and SP1950.
!
               values(currnt) = values(type)

!
!              Here's how this works:  We will copy all of
!              the components of the time representation up to
!              the current one.  This is the truncated representation
!              of our epoch.  We then copy these same components into
!              another time vector, but add an increment to the
!              component corresponding to the one we are dealing with
!              now.  We use an increment of 0 for those components that
!              already contain their fractional part. We use an
!              increment of 1 for the components that typically have
!              integer representations.
!
!              Zero out the previous and next time vectors so we won't
!              have to do it when we are filling in the truncated
!              portions.
!
               do j = 1, 7
                  ptvec(j) = 0.0_wp
                  ntvec(j) = 0.0_wp
               end do

               if      ( type == year  .or. type == yr ) then

                  stopat = 1
                  timfmt = ymd
                  intyp  = ymdfmt
                  incr   = 1.0_wp

               else if ( type == month) then

                  stopat = 2
                  timfmt = ymd
                  intyp  = ymdfmt
                  incr   = 1.0_wp

               else if ( type == day .or. type == doy ) then

                  stopat = 3
                  timfmt = ymd
                  intyp  = ymdfmt
                  incr   = 1.0_wp

               else if ( type == hour .or. type == ampm ) then

!
!                 Note that in this case (and the next 2) that if we
!                 an HOUR component, we had to get it either from
!                 a Day of Year format or from a Year Month Day
!                 format. Thus we have all of the more significant
!                 components for this format.
!
                  stopat = 4
                  timfmt = ymd
                  intyp  = ymdfmt
                  incr   = 1.0_wp

               else if ( type == minute  ) then

                  stopat = 5
                  timfmt = ymd
                  intyp  = ymdfmt

                  incr   = 1.0_wp

               else if ( type == sec  ) then

                  stopat = 6
                  timfmt = ymd
                  intyp  = ymdfmt
                  incr   = 0.0_wp

               else if ( type == julian ) then

                  stopat = 1
                  timfmt = contin
                  incr   = 0.0_wp

                  if      ( (timtyp == id(tdt)) .or. &
                            (timtyp == id(tt))) then
                     intyp = 'JDTDT'
                  else if ( timtyp == id(tdb) ) then
                     intyp = 'JDTDB'
                  else if (      timtyp == id(utc) &
                            .or. timtyp == id(utcp) ) then
                     intyp = 'JDUTC'
                  end if

               else

!
!                 The only types left are the continuous (numeric)
!                 types.
!
                  stopat = 1
                  timfmt = contin
                  incr   = 0.0_wp
                  intyp  = bastyp

               end if

!
!              Ok.  We are now ready to construct the previous
!              and next time vectors.
!
               do j = 1, stopat
                  ptvec(j) = values(compnt(j,timfmt))
                  ntvec(j) = ptvec (j)
               end do

               ntvec(stopat) = ntvec(stopat) + incr

!
!              If the type is a year or month, then we need to set
!              the month to 1, so that we will be working with
!              beginnings of years not beginning of last months of
!              the previous year.
!
               if ( type == year  .or. type == yr ) then
                  ptvec(2) = 1.0_wp
                  ntvec(2) = 1.0_wp
                  ptvec(3) = 1.0_wp
                  ntvec(3) = 1.0_wp
               else if ( type == month ) then
                  ptvec(3) = 1.0_wp
                  ntvec(3) = 1.0_wp
               end if

               if ( go2jul .and. timfmt /= contin ) then

!
!                 Convert both PTVEC and NTVEC to the gregorian
!                 calendar
!
                  jyear  = nint(ptvec(1))
                  jmonth = nint(ptvec(2))
                  jday   = nint(ptvec(3))

                  call jul2gr ( ctx, jyear, jmonth, jday, jdoy )

                  ptvec(1) = real(jyear, wp)
                  ptvec(2) = real(jmonth, wp)
                  ptvec(3) = real(jday, wp)

                  jyear  = nint(ntvec(1))
                  jmonth = nint(ntvec(2))
                  jday   = nint(ntvec(3))

                  call jul2gr ( ctx, jyear, jmonth, jday, jdoy )

                  ntvec(1) = real(jyear, wp)
                  ntvec(2) = real(jmonth, wp)
                  ntvec(3) = real(jday, wp)

               end if

!
!              Handle the +/- time zone shifts.
!
               if ( dozone .and. (timfmt /= contin) ) then

                  ptvec(4) = ptvec(4) - hoff
                  ntvec(4) = ntvec(4) - hoff

                  ptvec(5) = ptvec(5) - moff
                  ntvec(5) = ntvec(5) - moff

                  ptvec(6) = 0.0_wp
                  ntvec(6) = 0.0_wp

                  call ttrans ( ctx, 'YMDF', 'YMDF', ptvec )
                  call ttrans ( ctx, 'YMDF', 'YMDF', ntvec )

                  if ( type == sec ) then
                     ptvec(6) = values(sec)
                     ntvec(6) = values(sec)
                  end if

               end if

            end if

         end if

      end do

!
!     All that's left to do is to copy the constructed string
!     to the output string.
!
      output = string

      return
      end associate
      end procedure timout

end submodule pepper_time_api
