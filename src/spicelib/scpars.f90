!$Procedure SCPARS ( Parse a spacecraft clock string )

      subroutine scpars ( sc, sclkch, error, msg, sclkdp )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Parse a character representation of spacecraft clock time and
!     encode it as a double precision number.
!
! $ Required_Reading
!
!     SCLK
!
! $ Keywords
!
!     CONVERSION
!     TIME
!
! $ Declarations

      implicit none

      include               'sclk.inc'

      integer(ip)           sc
      character(*)          sclkch
      logical               error
      character(*)          msg
      real(wp)              sclkdp

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     SC         I   NAIF spacecraft identification code.
!     SCLKCH     I   Character representation of a spacecraft clock.
!     ERROR      O   Flag to indicate if string parsed correctly.
!     MSG        O   Error message if string did not parse.
!     SCLKDP     O   Encoded representation of the clock count.
!     MXPART     P   Maximum number of spacecraft clock partitions.
!
! $ Detailed_Input
!
!     SC       is the standard NAIF ID of the spacecraft whose clock's
!              time is being encoded.
!
!     SCLKCH   is the character representation of some spacecraft's
!              clock count.
!
!              SCLKCH will have the following general format:
!
!                           'pp/sclk_string', or just
!                              'sclk_string'
!
!              'pp' is an integer greater than or equal to one
!              and is called the partition number.
!
!              Each mission is divided into some number of partitions.
!              A new partition starts when the spacecraft clock
!              resets, either to zero, or to some other
!              value. Thus, the first partition for any mission
!              starts with launch, and ends with the first clock
!              reset. The second partition starts immediately when
!              the first stopped, and so on.
!
!              In order to be completely unambiguous about a
!              particular time, you need to specify a partition number
!              along with the standard clock string.
!
!              Information about when partitions occur for different
!              missions is contained in a spacecraft clock kernel
!              file, which needs to be loaded into the kernel pool,
!              using the routines CLPOOL and FURNSH.
!
!              The routine SCPART is used to read the partition
!              start and stop times, in encoded units of SCLK (called
!              "ticks" -- see SCLKDP below) from the kernel file.
!
!              If the partition number is included, it must be
!              separated from the rest of the string by a '/'.
!              Any number of spaces may separate the partition number,
!              the '/', and the rest of the clock string.
!
!
!              If the partition number is omitted, a default partition
!              will be assumed. The default partition is the lowest-
!              numbered partition that contains the given clock time.
!              If the clock time does not fall in any of the
!              partition boundaries then an error is signaled.
!
!
!              'sclk_string' is a spacecraft specific clock string.
!              Using Galileo as an example, the full format is
!
!                             wwwwwwww:xx:y:z
!
!              where z is a mod-8 counter (values 0-7) which
!              increments approximately once every 8 1/3 ms., y is a
!              mod-10 counter (values 0-9) which increments once
!              every time z turns over, i.e., approximately once every
!              66 2/3 ms., xx is a mod-91 (values 0-90) counter
!              which increments once every time y turns over, i.e.,
!              once every 2/3 seconds. wwwwwwww is the Real-Time Image
!              Count (RIM), which increments once every time xx turns
!              over, i.e., once every 60 2/3 seconds. The roll-over
!              expression for the RIM is 16777215, which corresponds
!              to approximately 32 years.
!
!              wwwwwwww, xx, y, and z are referred to interchangeably
!              as the fields or components of the spacecraft clock.
!              SCLK components may be separated by any of these
!              five characters: ' '  ':'  ','  '-'  '.'
!              Any number of spaces can separate the components and
!              the delimiters. The presence of the RIM component
!              is required. Successive components may be omitted, and
!              in such cases are assumed to represent zero values.
!
!              Values for the individual components may exceed the
!              maximum expected values. For instance, '0:0:0:9' is
!              an acceptable Galileo clock string, and will convert
!              to the same number of ticks as '0:0:1:1'.
!
!              Consecutive delimiters containing no intervening digits
!              are treated as if they delimit zero components.
!
!              Trailing zeros should always be included to match the
!              length of the counter.  For example, a Galileo clock
!              count of '25684.90' should not be represented as
!              '25684.9'.
!
!              Some spacecraft clock components have offset, or
!              starting, values different from zero. For example,
!              with an offset value of 1, a mod 20 counter would
!              cycle from 1 to 20 instead of from 0 to 19.
!
!              See the SCLK required reading for a detailed
!              description of the Voyager and Mars Observer clock
!              formats.
!
! $ Detailed_Output
!
!     ERROR    is .TRUE. if an error occurred parsing the input clock
!              string and converting it to ticks.
!
!     MSG      is the message generated if an error occurred parsing
!              the input clock string.
!
!     SCLKDP   is the double precision encoding of SCLKCH.
!
!              The encoding is such that order and proximity will be
!              preserved. That is, if t1, t2, and t3 are spacecraft
!              clock times, and t1*, t2*, and t3* are their encodings,
!              then if
!
!                            t1 < t2 < t3, and
!
!              t2 is closer to t1 than to t3, you will have the result
!              that
!
!                           t1* < t2* < t3*, and
!
!              t2* is closer to t1* than to t3*.
!
!              The units of encoded SCLK are "ticks since the start of
!              the mission", where a "tick" is defined to be the
!              shortest time increment expressible by a particular
!              spacecraft's clock.
!
!              Each clock string without partition number represents
!              a certain number of ticks, but you need to include
!              partition information to determine the relative
!              position of that time in relation to the start of the
!              mission.
!
!              Since the end time of one partition is coincident
!              with the begin time of the next, there are two
!              different representations for this instant, and they
!              will both yield the same encoding.
!
!              For example, if partition 1 has an end time of t1, and
!              partition 2 has a begin time of t2, then if we did
!
!                 CALL SCENCD ( '1/t1', SC, X ) and
!                 CALL SCENCD ( '2/t2', SC, Y ), then
!
!                                X = Y.
!
!              The individual routines TIKSnn, where nn is the
!              clock type code, contain more detailed information
!              on the conversion process.
!
! $ Parameters
!
!     MXPART   is the maximum number of spacecraft clock partitions
!              expected in the kernel file for any one spacecraft.
!              See the INCLUDE file sclk.inc for this parameter's
!              value.
!
! $ Exceptions
!
!     This routine uses both the normal SPICELIB error handling and
!     an ERROR flag and message. Errors that deal with kernel pool
!     data that are missing or invalid are treated in the usual way.
!     Errors that arise solely from parsing the input clock string
!     do not signal SPICELIB errors, but instead use the ERROR flag
!     and MSG string.
!
!     In the case of any SPICELIB error occurring, ERROR is initialized
!     to .TRUE. and MSG to 'SPICELIB error detected.'.
!
!     1)  If the number of partitions in the kernel file for spacecraft
!         SC exceeds the parameter MXPART, the error
!         SPICE(TOOMANYPARTS) is signaled.
!
!     2)  If the data type of the clock for the specified spacecraft is
!         of a data type not recognized by this routine, the error
!         SPICE(NOTSUPPORTED) is signaled.
!
!     If a partition number is included in the SCLK string, the
!     following errors may occur:
!
!     3)  The partition number cannot be parsed as an integer.
!
!     4)  The partition number is not in the range of the number of
!         partitions found in the kernel pool.
!
!     5)  The clock count does not fall in the boundaries of the
!         specified partition.
!
!     If a partition number is not included in the SCLK string, the
!     following exception may occur:
!
!     6)  The clock count does not fall in the boundaries of any
!         partition found in the kernel pool.
!
!     The actual parsing of the remainder of the clock string is
!     performed by data type specific routines. The following exceptions
!     may occur:
!
!     7)  The input spacecraft clock string is blank.
!
!     8)  The remainder clock string cannot be parsed by the SCLK data
!         type specific routine. The precise issue detected while
!         parsing the clock string is provided in the MSG string.
!
! $ Files
!
!     A kernel file containing spacecraft clock partition information
!     for the desired spacecraft must be loaded, using the routines
!     CLPOOL and FURNSH, before calling this routine.
!
! $ Particulars
!
!     In general, it is difficult to compare spacecraft clock counts
!     numerically since there are too many clock components for a
!     single comparison. This routine provides a method of assigning a
!     single double precision number to a spacecraft's clock count,
!     given one of its character representations.
!
!     The routine SCDECD performs the inverse operation to SCENCD,
!     converting an encoded double precision number to character format.
!
!     To convert the string to ticks since the start of the mission,
!     SCENCD
!
!        1) Converts the non-partition portion of the string to
!           ticks, using the routine SCTIKS.
!
!        2) Determines the partition number for the clock time,
!           either by getting it directly from the input string, or
!           determining the default partition if none was specified.
!
!        3) Includes partition start and stop times, which are also
!           measured in ticks, to compute the number of ticks
!           since the beginning of the mission of the clock time.
!
! $ Examples
!
!     Double precision encodings of spacecraft clock counts are used to
!     tag pointing data in the C-kernel.
!
!     In the following example, pointing for a sequence of images from
!     the Voyager 2 narrow angle camera is requested from the C-kernel
!     using an array of character spacecraft clock counts as input.
!     The clock counts attached to the output are then decoded to
!     character and compared with the input strings.
!
!           CHARACTER*(25)     SCLKIN   ( 4 )
!           CHARACTER*(25)     SCLKOUT
!           CHARACTER*(25)     CLKTOL
!
!           DOUBLE PRECISION   TIMEIN
!           DOUBLE PRECISION   TIMOUT
!           DOUBLE PRECISION   CMAT     ( 3, 3 )
!
!           INTEGER            NPICS
!           INTEGER            SC
!
!           DATA  NPICS     /  4                   /
!
!           DATA  SCLKIN    / '2 / 20538:39:768',
!          .                  '2 / 20543:21:768',
!          .                  '2 / 20550:37',
!          .                  '2 / 20561:59'       /
!
!           DATA  CLKTOL   /  '      0:01:000'     /
!
!     C
!     C     The instrument we want pointing for is the Voyager 2
!     C     narrow angle camera. The reference frame we want is
!     C     J2000. The spacecraft is Voyager 2.
!     C
!           INST = -32001
!           REF  = 'J2000'
!           SC   = -32
!
!     C
!     C     Load the appropriate files. We need
!     C
!     C     1) CK file containing pointing data.
!     C     2) Spacecraft clock kernel file, for SCENCD and SCDECD.
!     C
!           CALL CKLPF  ( 'VGR2NA.CK' )
!           CALL CLPOOL
!           CALL FURNSH ( 'SCLK.KER'  )
!
!     C
!     C     Convert the tolerance string to ticks.
!     C
!           CALL SCTIKS ( SC, CLKTOL, TOL )
!
!           DO I = 1, NPICS
!
!              CALL SCENCD ( SC, SCLKIN( I ), TIMEIN )
!
!              CALL CKGP   ( INST, TIMEIN, TOL, REF, CMAT, TIMOUT,
!          .                 FOUND )
!
!              CALL SCDECD ( SC, TIMOUT, SCLKOUT )
!
!              WRITE (*,*)
!              WRITE (*,*) 'Input  s/c clock count: ', SCLKIN( I )
!              WRITE (*,*) 'Output s/c clock count: ', SCLKOUT
!              WRITE (*,*) 'Output C-Matrix:        ', CMAT
!              WRITE (*,*)
!
!           END DO
!
!     The output from such a program might look like:
!
!
!           Input  s/c clock count:  2 / 20538:39:768
!           Output s/c clock count:  2/20538:39:768
!           Output C-Matrix:  'first C-matrix'
!
!           Input  s/c clock count:  2 / 20543:21:768
!           Output s/c clock count:  2/20543:22:768
!           Output C-Matrix:  'second C-matrix'
!
!           Input  s/c clock count:  2 / 20550:37
!           Output s/c clock count:  2/20550:36:768
!           Output C-Matrix:  'third C-matrix'
!
!           Input  s/c clock count:  2 / 20561:59
!           Output s/c clock count:  2/20561:58:768
!           Output C-Matrix:  'fourth C-matrix'
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
!     J.M. Lynch         (JPL)
!     W.L. Taber         (JPL)
!     R.E. Thurman       (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.3.0, 22-NOV-2021 (JDR) (NJB)
!
!        Bug fix: out-of-range character positions of SCLKCH are
!        no longer referenced.
!
!        Edited the header to comply with NAIF standard.
!
!        Added entries #7 and #8 to $Exceptions section.
!
!-    SPICELIB Version 1.2.0, 05-FEB-2008 (NJB)
!
!        The values of parameter MXPART and is now
!        provided by the INCLUDE file sclk.inc.
!
!-    SPICELIB Version 1.1.1, 22-AUG-2006 (EDW)
!
!        Replaced references to LDPOOL with references
!        to FURNSH.
!
!-    SPICELIB Version 1.1.0, 18-JUN-1999 (WLT)
!
!        Make CHKIN and CHKOUT arguments consistent.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 03-SEP-1990 (JML) (RET)
!
!-&


! $ Index_Entries
!
!     encode spacecraft_clock
!
!-&


!
!     SPICELIB functions
!
      integer(ip)           cpos
      integer(ip)           sctype

      logical               failed
      logical               return


!
!     Local variables
!
      character(255)        psmsg
      character(255)        strerr

      real(wp)              pstart   ( mxpart )
      real(wp)              pstop    ( mxpart )
      real(wp)              ptotls   ( mxpart )
      real(wp)              ticks

      integer(ip)           dtype
      integer(ip)           i
      integer(ip)           nparts
      integer(ip)           part
      integer(ip)           pos
      integer(ip)           pnter

      logical               pserr

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

      call chkin ( 'SCPARS' )

!
!     This routine handles errors in two different ways.
!
!     1) Errors relating to parsing the input clock string
!        will not use the normal SPICELIB error handling.
!        Instead they will use the ERROR and MSG arguments
!        to this routine.
!
!     2) Errors relating to missing or invalid data in the
!        kernel pool will use the normal SPICELIB error
!        handling.
!
!     In the event that a SPICE error occurs somewhere, ERROR
!     and MSG will be initialized to the following values:
!
      error = .true.

      msg   = 'SPICELIB error detected.'

      dtype = sctype ( sc )

      if ( failed() ) then
         call chkout ( 'SCPARS' )
         return
      end if

      if ( dtype /= 1 )  then

         call setmsg ( 'Clock type # is not supported.' )
         call errint ( '#', dtype                       )
         call sigerr ( 'SPICE(NOTSUPPORTED)'            )
         call chkout ( 'SCPARS'                         )
         return

      end if

!
!     Read the partition start and stop times (in ticks) for this
!     mission. Error if there are too many of them.
!
      call scpart ( sc, nparts, pstart, pstop )

      if ( failed() ) then
!
!        This code should be unreachable but is provided for safety.
!        Missing partition data should be caught by the earlier SCTYPE
!        call, which accesses the type 1 SCLK database.
!
         call chkout ( 'SCPARS' )
         return

      end if

      if ( nparts > mxpart ) then
!
!        This code should be unreachable but is provided for safety.
!
         call setmsg ( 'The number of partitions, #, for spacecraft ' // &
                       '# exceeds the value for parameter MXPART, #.' )
         call errint ( '#', nparts           )
         call errint ( '#', sc               )
         call errint ( '#', mxpart           )
         call sigerr ( 'SPICE(TOOMANYPARTS)' )
         call chkout ( 'SCPARS'              )
         return

      end if

!
!     First check if the string is blank.
!
      if ( sclkch == ' ' ) then

         error = .true.

         msg   = 'Input spacecraft clock string is blank.'

         call chkout ( 'SCPARS' )
         return

      end if

!
!     Convert the non-partition clock string to a tick value.
!     This conversion depends on the data type of the clock.
!
      pos = cpos ( sclkch, '/', 1 )

      if ( pos == 1 ) then
!
!        The slash character is first character in the string which
!        means that the partition number is not there.
!
         msg = 'Unable to parse the partition number from SCLK ' // &
               'string #.'

         call repmc ( msg, '#', sclkch, msg )

         call chkout ( 'SCPARS' )
         return

      end if

      if ( pos == len(sclkch) ) then

         msg = 'SCLK string ends with slash.'

         call chkout ( 'SCPARS' )
         return

      end if

!
!     Parse the portion of the clock string following the slash,
!     if any, or the whole string if the slash is absent.
!
      call scps01 ( sc, sclkch( pos+1 : ), pserr, psmsg, ticks )

      if ( failed() ) then
!
!        This code should be unreachable but is provided for safety.
!        Missing SCLK data should be caught by the earlier SCTYPE call,
!        which accesses the type 1 SCLK database.
!
         call chkout ( 'SCPARS' )
         return
      end if

!
!     Check if the SCPSxx routine encountered a problem.
!
      if ( pserr ) then

         error = .true.
         msg   =  psmsg
         call chkout ( 'SCPARS' )
         return

      end if

!
!     Find the partition that this clock time falls in.
!
!     For each partition, compute the total number of ticks in that
!     partition plus all preceding partitions.
!
      ptotls( 1 ) = anint ( pstop( 1 ) - pstart( 1 ) )

      do i = 2, nparts
         ptotls( i ) = anint ( ptotls( i-1 ) + pstop( i ) - pstart( i ))
      end do

!
!     Determine the partition number for the input clock string:
!
!        If it was included in the string make sure it's valid for
!        this mission.
!
!           Error if
!
!           1) The partition number can't be parsed.
!
!           2) The partition number is not in the range 1 to the number
!              of partitions.
!
!           3) The clock count does not fall in the boundaries of the
!              specified partition.
!
!        If it wasn't included, determine the default partition for
!        this clock count.
!
!           Error if
!
!           1) The clock count does not fall in the boundaries of any
!              of the partitions.
!
!
      if ( pos > 1 ) then
!
!        Try to parse the partition number.
!
         part = 0

         call nparsi ( sclkch( : pos-1 ), part, strerr, pnter )
!
!        Make sure that the number parsed is correct.
!
         if ( strerr /= ' ' ) then
!
!          Was not able to parse a number.
!

            msg = 'Unable to parse the partition number from SCLK ' // &
                  'string #.'

            call repmc ( msg, '#', sclkch, msg )

            call chkout ( 'SCPARS' )
            return

         else if ( part <= 0 .or. part > nparts ) then

!
!           The parsed number does not fall in the range of valid
!           numbers.
!
            msg = 'Partition number # taken from SCLK string ' // &
                  '# is not in acceptable range 1 to #.'

            call repmi ( msg, '#', part,   msg )

            call repmc ( msg, '#', sclkch, msg )

            call repmi ( msg, '#', nparts, msg )

            call chkout ( 'SCPARS' )
            return

         else if ( ticks < pstart( part )   .or. &
                   ticks > pstop ( part ) )  then

!
!           The TICKS value does not fall in the range of valid
!           values for the partition number parsed from the input
!           clock string.
!
            msg = 'SCLK count from # does not fall in the ' // &
                  'boundaries of partition number #.'

            call repmc ( msg, '#', sclkch( pos+1 : ), msg )

            call repmi ( msg, '#', part, msg )

            call chkout ( 'SCPARS' )
            return

         end if

      else

!
!        The partition number was not included in the string.
!        Determine the partition from the TICKS value that the
!        clock string converted to.
!
         part = 1

         do while (   part  <= nparts             .and. &
                    ( ticks < pstart( part )     .or. &
                      ticks > pstop ( part ) )        )

            part = part + 1

         end do

         if ( part > nparts ) then

            msg = 'SCLK count # does not fall in the boundaries ' // &
                  'of any of the partitions for spacecraft #.'

            call repmc ( msg, '#', sclkch( pos+1 : ), msg )

            call repmi ( msg, '#', sc, msg )

            call chkout ( 'SCPARS' )
            return

         end if

      end if

!
!     Now we have a valid partition number, and the number of ticks for
!     the clock string. To convert to ticks since the start of the
!     mission, add in the total number of ticks in preceding partitions
!     and subtract off the starting ticks value for this partition.
!
      if ( part > 1 ) then
         sclkdp = ticks - pstart( part ) + ptotls( part - 1 )
      else
         sclkdp = ticks - pstart( part )
      end if

      error = .false.

      msg   = ' '

      call chkout ( 'SCPARS' )
      return

      end
