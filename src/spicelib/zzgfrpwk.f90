!$Procedure    ZZGFRPWK ( Geometry finder report work done on a task )

      subroutine zzgfrpwk ( unit,   total, wait, &
                            tcheck, begin, end, incr )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     The entry points under this routine allows one to easily monitor
!     the status of job in progress.
!
! $ Required_Reading
!
!     GF
!
! $ Keywords
!
!     UTILITY
!     REPORT
!     WORK
!
! $ Declarations

      implicit none

      include               'zzgf.inc'

      integer(ip)           unit
      real(wp)              total
      real(wp)              wait
      integer(ip)           tcheck
      character*(*)         begin
      character*(*)         end
      real(wp)              incr

! $ Brief_I/O
!
!     VARIABLE  I/O  Entry points
!     --------  ---  --------------------------------------------------
!     UNIT      I-O  ZZGFWKUN, ZZGFWKMO
!     TOTAL     I-O  ZZGFTSWK, ZZGFWKAD, ZZGFWKMO
!     WAIT      I-O  ZZGFTSWK, ZZGFWKAD, ZZGFWKMO
!     TCHECK    I-O  ZZGFTSWK, ZZGFWKAD, ZZGFWKMO
!     BEGIN     I-O  ZZGFTSWK, ZZGFWKAD, ZZGFWKMO
!     END       I-O  ZZGFTSWK, ZZGFWKAD, ZZGFWKMO
!     INCR      I-O  ZZGFWKIN, ZZGFWKMO
!
! $ Detailed_Input
!
!     See the headers of the entry points.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     MXBEGM,
!     MXENDM,
!     MXMSG     are, respectively, the maximum lengths of the progress
!               message prefix, progress message suffix, and the
!               complete message.
!
! $ Exceptions
!
!     If this routine is called directly, the error SPICE(BOGUSENTRY)
!     is signaled.
!
!     See the entry points for descriptions of exceptions they detect.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The entry points under this routine are designed to allow one to
!     easily build into his/her application a monitoring facility
!     that reports how work on a particular task is proceeding.
!
!     There are five entry points: ZZGFTSWK, ZZGFWKIN, ZZGFWKAD,
!     ZZGFWKUN, and ZZGFWKMO.
!
!     The first entry point ZZGFTSWK is used to initialize the reporter.
!     It is used to tell the reporter "I have some work to do.  This is
!     how much, and this is how often I want you to report on the
!     progress of the task."
!
!     The second entry point ZZGFWKIN is used to tell the reporter "I've
!     just finished some of the task I told you about with ZZGFTSWK.
!     This is how much I've just done."  (As in real life, the amount
!     of work you've just done can be negative.)  The reporter uses
!     this information together with the information input in ZZGFTSWK
!     to decide whether and how much work to report as finished.  The
!     reports will be sent to the current output device.
!
!     The third entry point, ZZGFWKAD, adjusts the frequency with which
!     work progress is reported.
!
!     The fourth entry point ZZGFWKUN also is used for testing. It is
!     used to send the output to the file connected to a specified
!     logical unit.
!
!     The fifth entry point ZZGFWKMO is used for testing. It returns
!     the saved search parameters.
!
!     A more detailed description of each entry point is provided in its
!     associated header.
!
! $ Examples
!
!     A typical use of ZZGFRPWK might be as follows.
!
!
!     C
!     C     Compute how much work is to be done and put it in TOTAL
!     C
!
!           code
!           computing
!           how
!           much
!           work
!           to
!           do
!            .
!            .
!            .
!           TOTAL     = <the amount of work to do>
!
!     C
!     C     Tell the work reporter to report work completed every
!     C     3 seconds. (The third argument in ZZGFTSWK is explained
!     C     in the header for ZZGFTSWK.)
!     C
!           WAIT   = 3.0D0
!           BEGIN  = 'Current work status: '
!           END    = 'completed. '
!
!           CALL ZZGFTSWK ( TOTAL, WAIT, 1, BEGIN, END )
!
!           DO WHILE ( THERE_IS_MORE_WORK_TO_DO )
!
!              code that
!              performs
!              the work to
!              be done
!
!              AMOUNT = amount of work done in this loop pass
!
!              CALL ZZGFWKIN ( AMOUNT )
!
!           END DO
!
!
! $ Restrictions
!
!      You can use this routine to report progress on only one task at
!      a time.  The work reporter must be initialized using ZZGFTSWK
!      before calling ZZGFWKIN.  Failure to do this may lead to
!      unexpected results.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!     L.S. Elson     (JPL)
!     W.L. Taber     (JPL)
!     I.M. Underwood (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0 05-NOV-2021 (NJB)
!
!        Changed name of argument FREQ to WAIT. Updated entry points
!        affected by this change. Deleted description of argument
!        UNIT from header of entry point ZZGFTSWK.
!
!-    SPICELIB Version 1.0.0 17-FEB-2009 (NJB) (LSE) (WLT) (IMU)
!
!-&

! $ Index_Entries
!
!     GF low-level progress report umbrella
!
!-&

!
!     SPICELIB Functions
!
      real(wp)              brcktd
      integer(ip)           rtrim
      logical               return

!
!     Local variables
!
      character*10          prcent
      character*(mxendm)    finish
      character*(mxbegm)    start
      character*(mxmsg)     messge

      real(wp)              cursec
      real(wp)              done
      real(wp)              entire
      real(wp)              fractn
      real(wp)              svincr
      real(wp)              lstsec
      real(wp)              step
      real(wp)              tvec ( 6 )

      integer(ip)           calls
      integer(ip)           check
      integer(ip)           ls
      integer(ip)           stdout
      integer(ip)           svunit

      logical               first

!
!     Saved variables
!
      save                  calls
      save                  check
      save                  done
      save                  entire
      save                  finish
      save                  first
      save                  ls
      save                  lstsec
      save                  start
      save                  stdout
      save                  step
      save                  svincr
      save                  svunit

!
!     Initial values
!
      data                  calls  / 0       /
      data                  check  / 1       /
      data                  done   / 0.0_wp    /
      data                  entire / 0.0_wp    /
      data                  finish / ' '     /
      data                  first  / .true.  /
      data                  ls     / 1       /
      data                  lstsec / 0.0_wp    /
      data                  start  / ' '     /
      data                  stdout / 6       /
      data                  step   / 0.0_wp    /
      data                  svincr / 0.0_wp    /
      data                  svunit / 6       /


      call chkin  ( 'ZZGFRPWK'          )
      call sigerr ( 'SPICE(BOGUSENTRY)' )
      call chkout ( 'ZZGFRPWK'          )
      return





!$Procedure ZZGFTSWK ( Geometry finder total sum of work to be done. )

      entry zzgftswk ( total, wait, tcheck, begin, end )

! $ Abstract
!
!     Initialize the work progress utility. This is required prior to
!     use of the routine that performs the actual reporting.
!
! $ Required_Reading
!
!     GF
!
! $ Keywords
!
!     UTILITY
!     REPORT
!     WORK
!
! $ Declarations
!
!     DOUBLE PRECISION      TOTAL
!     DOUBLE PRECISION      WAIT
!     INTEGER               TCHECK
!     CHARACTER*(*)         BEGIN
!     CHARACTER*(*)         END
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     TOTAL      I   A measure of the total amount of work to be done.
!     WAIT       I   Minimum wait time between reports.
!     TCHECK     I   How often to sample the system clock.
!     BEGIN      I   First part of the output message.
!     END        I   Last part of the output message.
!
! $ Detailed_Input
!
!     TOTAL      is a measure of the total amount of work to be done
!                by the routine(s) that will be using this facility.
!                It is expected (but not required) that TOTAL is a
!                positive number.
!
!     WAIT       is the minimum duration between reports in seconds. If
!                WAIT = 5 then a work progress report will be sent to
!                the output device no more frequently than every 5
!                seconds.  Since writing to the output device takes
!                time, the smaller WAIT is, the greater the overhead
!                taken up by the work reporter will be. (A value of 2
!                or greater should not burden your application
!                appreciably.)
!
!                Regardless of the value of WAIT, the number of calls
!                to ZZGFWKIN between reports will not be less than that
!                specified by the argument TCHECK. See the description
!                below.
!
!     TCHECK     is an integer used to the tell the reporter how often
!                to sample the system clock; TCHECK specifies the
!                number of calls to ZZGFWKIN between system clock
!                samples. If TCHECK = 7, then on every seventh call to
!                ZZGFWKIN, the system clock will be sampled to
!                determine if WAIT seconds have elapsed since the last
!                report time.  Sampling the system clock takes time.
!                Not a lot of time, but it does take time. If ZZGFWKIN
!                is being called from a loop that does not take a lot
!                of time for each pass, the sampling of the system
!                clock can become a significant overhead cost in
!                itself.  On the VAX the sampling of the system clock
!                used here takes about 37 double precision multiplies.
!                If thousands of multiplies take place between calls to
!                ZZGFWKIN, the sampling time is insignificant.  On the
!                other hand, if only a hundred or so multiplies occur
!                between calls to ZZGFWKIN, the sampling of the system
!                clock can become a significant fraction of your
!                overhead.  TCHECK allows you to tailor the work
!                reporter to your application.
!
!                If a non-positive value for TCHECK is entered, a value
!                of 1 will be used instead of the input value.
!

!      BEGIN     Is the first part of the output message that will be
!                constructed for shipment to the output device. This
!                message will have the form:
!
!                BEGIN // xx.x% // END
!
!                where xx.x  is the percentage of the job completed when
!                the output message is sent to the output device.
!
!      END       is the second part of the output message that will be
!                constructed and sent to the output device (see above).
!
!
! $ Detailed_Output
!
!      None.
!
! $ Parameters
!
!      None.
!
! $ Exceptions
!
!     Standard SPICE error handling.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This entry point is used to initialize parameters that will
!     be used by ZZGFWKIN.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     See the header for this module
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!     L.S. Elson     (JPL)
!     W.L. Taber     (JPL)
!     I.M. Underwood (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0 05-NOV-2021 (NJB)
!
!        Changed name of argument FREQ to WAIT. Updated header to
!        clarify roles of WAIT and TCHECK. Deleted description of
!        UNIT from $Detailed_Input header section.
!
!-    SPICELIB Version 1.0.0 17-FEB-2009 (NJB) (LSE) (WLT) (IMU)
!
!-&

! $ Index_Entries
!
!     GF low-level initialize progress report
!
!-&

      if ( return() ) then
         return
      end if

      call chkin  ( 'ZZGFTSWK' )

!
!     On the first pass, obtain the logical unit for
!     standard output.
!
      if ( first ) then

         call stdio ( 'STDOUT', stdout )

!
!        The output unit is STDOUT unless the caller
!        sets it to something else.
!
         svunit = stdout

         first  = .false.

      end if

!
!     Save the inputs and set the amount of work done to 0
!
      entire = total
      step   = min ( 3600.0_wp, max( 0.0_wp, wait ) )
      check  = max ( 1, tcheck )

      start  = begin
      finish = end

      done   = 0.0_wp

!
!     Set the timer.
!
      call zzcputim ( tvec )

      lstsec = tvec(4)*3600.0_wp + tvec(5)*60.0_wp + tvec(6)

!
!     Set the increment counter
!
      calls  = 0

!
!     Compose the output message.
!
      ls            = rtrim ( start  )
      messge        = start ( 1 : ls )  // &
                      ' '               // &
                      '  0.00%'         // &
                      ' '               // &
                      finish

!
!     Display a blank line, make sure we don't overwrite anything
!     at the bottom of the screen. The display the message.
!
      if ( svunit .eq. stdout ) then

         call zzgfdsps ( 1, messge, 'A', 0 )

      else
!
!        Write the message without special carriage control.
!
         call writln ( ' ',    svunit )
         call writln ( ' ',    svunit )
         call writln ( messge, svunit )

      end if

      call chkout  ( 'ZZGFTSWK' )
      return



!$Procedure ZZGFWKIN ( Geometry finder work finished increment )

      entry zzgfwkin ( incr )

! $ Abstract
!
!     Let the work reporter know that an increment of work has just
!     been completed.
!
! $ Required_Reading
!
!     GF
!
! $ Keywords
!
!     UTILITY
!     REPORT
!     WORK
!
! $ Declarations
!
!     DOUBLE PRECISION      INCR
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     INCR       I   An amount of work just completed.
!
! $ Detailed_Input
!
!     INCR       is some amount of work that has been completed since
!                the last call to ZZGFWKIN.
!
! $ Detailed_Output
!
!      None.
!
! $ Parameters
!
!      None.
!
! $ Exceptions
!
!     Standard SPICE error handling.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This entry point is used to report work that has been done since
!     initialization was performed using ZZGFTSWK or since the last
!     call to ZZGFWKIN.  The work reporter uses this information
!     together with samples of the system clock to report how much of
!     the total job has been completed.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     See the header for this module
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!     L.S. Elson     (JPL)
!     W.L. Taber     (JPL)
!     I.M. Underwood (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0 17-FEB-2009 (NJB) (LSE) (WLT) (IMU)
!
!-&

! $ Index_Entries
!
!     ZZGF low-level progress report increment
!
!-&

      if ( return() ) then
         return
      end if

      call chkin  ( 'ZZGFWKIN' )

      svincr = incr
      done   = done  + incr
      calls  = calls + 1

      if ( entire .eq. 0 ) then
         call chkout  ( 'ZZGFWKIN' )
         return
      end if

      if ( calls .ge. check ) then

         calls  = 0

         call zzcputim ( tvec )

         cursec = tvec(4)*3600.0_wp + tvec(5)*60.0_wp + tvec(6)


         if ( dabs( cursec - lstsec ) .ge. step ) then

            lstsec = cursec
!
!           Report how much work has been done.
!
            fractn = brcktd ( ( done/entire )*100.0_wp,  0.0_wp,  100.0_wp )

            call dpfmt ( fractn, 'xxx.xx', prcent )

            prcent(7:7) = '%'

            messge        = start  (1:ls)  // &
                            ' '            // &
                            prcent (1:7)   // &
                            ' '            // &
                            finish(1:rtrim(finish))


            if ( svunit .eq. stdout ) then

               call zzgfdsps ( 0, messge, 'A', 0 )

            else
!
!              Write the message without special carriage control.
!
               call writln ( messge, svunit )

            end if

         end if

      end if

      call chkout  ( 'ZZGFWKIN' )
      return



!$Procedure ZZGFWKAD ( Geometry finder work reporting adjustment )

      entry zzgfwkad ( wait, tcheck, begin, end )

! $ Abstract
!
!     Adjust the frequency with which work progress is reported.
!
! $ Required_Reading
!
!     GF
!
! $ Keywords
!
!     UTILITY
!     REPORT
!     WORK
!
! $ Declarations
!
!     DOUBLE PRECISION      WAIT
!     INTEGER               TCHECK
!     CHARACTER*(*)         BEGIN
!     CHARACTER*(*)         END
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     TOTAL      I   A measure of the total amount of work to be done.
!     WAIT       I   Minimum wait time between reports.
!     BEGIN      I   First part of the output message.
!     END        I   Last part of the output message.
!
! $ Detailed_Input
!
!     WAIT       is the minimum duration between reports in seconds. If
!                WAIT = 5 then a work progress report will be sent to
!                the output device no more frequently than every 5
!                seconds.  Since writing to the output device takes
!                time, the smaller WAIT is, the greater the overhead
!                taken up by the work reporter will be. (A value of 2
!                or greater should not burden your application
!                appreciably.)
!
!                Regardless of the value of WAIT, the number of calls
!                to ZZGFWKIN between reports will not be less than that
!                specified by the argument TCHECK. See the description
!                below.
!
!     TCHECK     is an integer used to the tell the reporter how often
!                to sample the system clock; TCHECK specifies the
!                number of calls to ZZGFWKIN between system clock
!                samples. If TCHECK = 7, then on every seventh call to
!                ZZGFWKIN, the system clock will be sampled to
!                determine if WAIT seconds have elapsed since the last
!                report time.  Sampling the system clock takes time.
!                Not a lot of time, but it does take time. If ZZGFWKIN
!                is being called from a loop that does not take a lot
!                of time for each pass, the sampling of the system
!                clock can become a significant overhead cost in
!                itself.  On the VAX the sampling of the system clock
!                used here takes about 37 double precision multiplies.
!                If thousands of multiplies take place between calls to
!                ZZGFWKIN, the sampling time is insignificant.  On the
!                other hand, if only a hundred or so multiplies occur
!                between calls to ZZGFWKIN, the sampling of the system
!                clock can become a significant fraction of your
!                overhead.  TCHECK allows you to tailor the work
!                reporter to your application.
!
!                If a non-positive value for TCHECK is entered, a value
!                of 1 will be used instead of the input value.
!
!
!     BEGIN      Is the first part of the output message that will be
!                constructed for shipment to the output device. This
!                message will have the form:
!
!                BEGIN // xx.x% // END
!
!                where xx.x  is the percentage of the job completed when
!                the output message is sent to the output device.
!
!     END        is the second part of the output message that will be
!                constructed and sent to the output device (see above).
!
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1) If TCHECK is less than 1, the value 1 is stored.
!
!     2) If WAIT is less than 0.D0, the value 0.D0 is stored.
!        If WAIT is greater than 3600, the value 3600 is stored.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This entry point exists to modify the reporting frequency set
!     up by an initial call to ZZGFTSWK.  In this way one can override
!     how often reporting of work increments is performed, without
!     causing the screen to be modified (which happens if a new
!     call to  ZZGFTSWK is made.)
!
!     It exists primarily as a back door to existing code
!     that calls ZZGFTSWK in a rigid way.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     See the header for this module.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!     W.L. Taber     (JPL)
!     I.M. Underwood (JPL)
!     L.S. Elson     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0 21-FEB-2021 (NJB)
!
!        Changed name of argument FREQ to WAIT. Updated header to
!        clarify roles of WAIT and TCHECK. Corrected Exceptions
!        section's description of handling of WAIT values less than
!        zero.
!
!-    SPICELIB Version 1.0.0 17-FEB-2009 (NJB) (LSE) (WLT) (IMU)
!
!-&


! $ Index_Entries
!
!     GF low-level progress report adjust frequency
!
!-&

      step   = min  ( 3600.0_wp,  max( 0.0_wp, wait ) )
      check  = max  ( 1, tcheck )
      start  = begin
      finish = end

      return






!$Procedure ZZGFWUN ( Geometry finder set work report output unit )

      entry zzgfwkun ( unit )

! $ Abstract
!
!     Set the output unit for the progress report.
!
! $ Required_Reading
!
!     GF
!
! $ Keywords
!
!     UTILITY
!     REPORT
!     WORK
!
! $ Declarations
!
!     INTEGER               UNIT
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     UNIT       I   Output logical unit.
!
! $ Detailed_Input
!
!     UNIT           Logical unit of a text file open for write access.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
! $ Files
!
!     The file designated by UNIT should be a text file opened by the
!     calling application.
!
! $ Particulars
!
!     This routine can be called before ZZGFTSWK to set the output
!     logical unit to that of a text file.
!
!     This entry point exists to support testing of the higher-level
!     GF progress reporting routines
!
!        GFREPI
!        GFREPU
!        GFREPF
!
!     This routine enables TSPICE to send the output report to
!     a specified file.
!
! $ Examples
!
!     None.
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
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0 17-FEB-2009 (NJB)
!
!-&


! $ Index_Entries
!
!     GF low-level progress report output select unit
!
!-&


!
!     On the first pass, obtain the logical unit for
!     standard output.
!
      if ( first ) then

         call stdio ( 'STDOUT', stdout )

         first  = .false.

      end if

      svunit = unit

      return





!$Procedure ZZGFWKMO ( Geometry finder work reporting monitor )

      entry zzgfwkmo ( unit, total, wait, tcheck, begin, end, incr )

! $ Abstract
!
!     Return saved progress report parameters.
!
! $ Required_Reading
!
!     GF
!
! $ Keywords
!
!     UTILITY
!     REPORT
!     WORK
!
! $ Declarations
!
!     INTEGER               UNIT
!     DOUBLE PRECISION      TOTAL
!     DOUBLE PRECISION      WAIT
!     INTEGER               TCHECK
!     CHARACTER*(*)         BEGIN
!     CHARACTER*(*)         END
!     DOUBLE PRECISION      INCR
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     UNIT       O   Output logical unit.
!     TOTAL      O   A measure of the total amount of work to be done.
!     WAIT       O   Minimum wait time between reports.
!     TCHECK     O   Number of calls between system time check.
!     BEGIN      O   First part of the output message.
!     END        O   Last part of the output message.
!     INCR       O   Last progress increment.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     UNIT,
!     TOTAL,
!     WAIT,
!     TCHECK,
!     BEGIN,
!     END,
!     INCR           are the most recent values of these
!                    variables passed in via calls to ZZGFTSWK,
!                    ZZGFWKIN, or ZZGFWKAD.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This entry point exists to support testing of the higher-level
!     GF progress reporting routines
!
!        GFREPI
!        GFREPU
!        GFREPF
!
!     This routine enables TSPICE to determine the values passed
!     in to entry points of this package by those routines.
!
! $ Examples
!
!     None.
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
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0 21-FEB-2021 (NJB)
!
!        Changed name of argument FREQ to WAIT. Updated Brief_I/O
!        description of this argument.
!
!-    SPICELIB Version 1.0.0 17-FEB-2009 (NJB)
!
!-&


! $ Index_Entries
!
!     GF low-level progress report monitor
!
!-&

      unit   = svunit
      total  = entire
      wait   = step
      tcheck = check
      begin  = start
      end    = finish
      incr   = svincr

      return
      end
