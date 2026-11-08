!$Procedure GFRPRT ( GF, progress reporting package )

      subroutine gfrprt ( window, begmss, endmss, ivbeg, ivend, time )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     The entry points contained under this routine provide users
!     information regarding the status of a GF search in progress.
!
! $ Required_Reading
!
!     GF
!
! $ Keywords
!
!     SEARCH
!     UTILITY
!
! $ Declarations

      implicit none

      include               'zzgf.inc'

      integer(ip)           lbcell
      parameter           ( lbcell = -5 )

      real(wp)              window ( lbcell : * )
      character*(*)         begmss
      character*(*)         endmss
      real(wp)              ivbeg
      real(wp)              ivend
      real(wp)              time

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     LBCELL     P   The SPICE cell lower bound.
!     MXBEGM     P   Maximum progress report message prefix length.
!     MXENDM     P   Maximum progress report message suffix length.
!     WINDOW     I   A window over which a job is to be performed.
!     BEGMSS     I   Beginning of the text portion of the output message
!     ENDMSS     I   End of the text portion of the output message
!     IVBEG      I   Current confinement window interval start time.
!     IVEND      I   Current confinement window interval stop time.
!     TIME       I   Input to the reporting routine.
!
! $ Detailed_Input
!
!     See the individual entry points.
!
! $ Detailed_Output
!
!     See the individual entry points.
!
! $ Parameters
!
!     LBCELL   is the SPICE cell lower bound.
!
!     MXBEGM,
!     MXENDM   are, respectively, the maximum lengths of the progress
!              report message prefix and suffix.
!
! $ Exceptions
!
!     1)  See the individual entry points.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This umbrella routine contains default progress reporting entry
!     points that display a report via console I/O. These routines may
!     be used by SPICE-based applications as inputs to mid-level GF
!     search routines. These routines may be useful even when progress
!     reporting is not desired, since the mid-level search routines
!     provide some capabilities that aren't supported by the top-level
!     GF routines.
!
!     Developers wishing to use their own GF progress reporting
!     routines must design them with the same interfaces and should
!     assign them the same progress reporting roles as the entry points
!     of these routines.
!
!     The entry points contained in this routine are written to
!     make reporting of work (such as searching for a geometric event)
!     over a particular window easy. This is an important feature for
!     interactive programs that may "go away" from the user's control
!     for a considerable length of time. It allows the user to see that
!     something is still going on (although maybe not too quickly).
!
!     The three entry points contained under this module are:
!
!        GFREPI  used to set up the reporting mechanism. It lets GFRPRT
!                know that some task is about to begin that involves
!                interaction with some window of times. It is used
!                only to set up and store the constants associated with
!                the reporting of the job in progress.
!
!        GFREPU  is used to notify the reporter that work has
!                progressed to a given point with respect to the start
!                of the confinement window.
!
!        GFREPF  is used to "finish" the reporting of work (set the
!                completion value to 100%.
!
!     The progress reporting utilities are called by GF search routines
!     as follows:
!
!        1) Given a window over which some work is to be performed,
!           CALL GFREPI with the appropriate inputs, to let the routine
!           know the intervals over which some work is to be done.
!
!        2) Each time some "good" amount of work has been done, call
!           GFREPU so that the total amount of work done can be updated
!           and can be reported.
!
!        3) When work is complete call GFREPF to "clean up" the end of
!           the progress report.
!
! $ Examples
!
!     The numerical results shown for these examples may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) This example shows how to call a mid-level GF search API that
!        requires as input progress reporting routines.
!
!        If custom progress reporting routines are available, they
!        can replace GFREPI, GFREPU, and GFREPF in any GF API calls.
!
!        The code example below is the first example in the header of
!        GFOCCE.
!
!
!        Conduct a search using the default GF progress reporting
!        capability.
!
!        The program will use console I/O to display a simple
!        ASCII-based progress report.
!
!        The program will find occultations of the Sun by the Moon as
!        seen from the center of the Earth over the month December,
!        2001.
!
!        We use light time corrections to model apparent positions of
!        Sun and Moon. Stellar aberration corrections are not specified
!        because they don't affect occultation computations.
!
!        Use the meta-kernel shown below to load the required SPICE
!        kernels.
!
!
!           KPL/MK
!
!           File name: gfrprt_ex1.tm
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
!              File name                     Contents
!              ---------                     --------
!              de421.bsp                     Planetary ephemeris
!              pck00008.tpc                  Planet orientation and
!                                            radii
!              naif0009.tls                  Leapseconds
!
!
!           \begindata
!
!              KERNELS_TO_LOAD = ( 'de421.bsp',
!                                  'pck00008.tpc',
!                                  'naif0009.tls'  )
!
!           \begintext
!
!           End of meta-kernel
!
!
!        Example code begins here.
!
!
!              PROGRAM GFRPRT_EX1
!              IMPLICIT NONE
!
!        C
!        C     SPICELIB functions
!        C
!              INTEGER               WNCARD
!
!        C
!        C     SPICELIB default functions for
!        C
!        C        - Interrupt handling (no-op function):   GFBAIL
!        C        - Search refinement:                     GFREFN
!        C        - Progress report termination:           GFREPF
!        C        - Progress report initialization:        GFREPI
!        C        - Progress report update:                GFREPU
!        C        - Search step size "get" function:       GFSTEP
!        C
!              LOGICAL               GFBAIL
!              EXTERNAL              GFBAIL
!
!              EXTERNAL              GFREFN
!              EXTERNAL              GFREPI
!              EXTERNAL              GFREPU
!              EXTERNAL              GFREPF
!              EXTERNAL              GFSTEP
!
!        C
!        C     Local parameters
!        C
!              CHARACTER*(*)         TIMFMT
!              PARAMETER           ( TIMFMT =
!             .   'YYYY MON DD HR:MN:SC.###### ::TDB (TDB)' )
!
!              DOUBLE PRECISION      CNVTOL
!              PARAMETER           ( CNVTOL = 1.D-6 )
!
!              INTEGER               MAXWIN
!              PARAMETER           ( MAXWIN = 2 * 100 )
!
!              INTEGER               TIMLEN
!              PARAMETER           ( TIMLEN = 40 )
!
!              INTEGER               LBCELL
!              PARAMETER           ( LBCELL = -5 )
!
!        C
!        C     Local variables
!        C
!              CHARACTER*(TIMLEN)    WIN0
!              CHARACTER*(TIMLEN)    WIN1
!              CHARACTER*(TIMLEN)    BEGSTR
!              CHARACTER*(TIMLEN)    ENDSTR
!
!              DOUBLE PRECISION      CNFINE ( LBCELL : 2 )
!              DOUBLE PRECISION      ET0
!              DOUBLE PRECISION      ET1
!              DOUBLE PRECISION      LEFT
!              DOUBLE PRECISION      RESULT ( LBCELL : MAXWIN )
!              DOUBLE PRECISION      RIGHT
!
!              INTEGER               I
!
!              LOGICAL               BAIL
!              LOGICAL               RPT
!
!        C
!        C     Saved variables
!        C
!        C     The confinement and result windows CNFINE and RESULT are
!        C     saved because this practice helps to prevent stack
!        C     overflow.
!        C
!              SAVE                  CNFINE
!              SAVE                  RESULT
!
!        C
!        C     Load kernels.
!        C
!              CALL FURNSH ( 'gfrprt_ex1.tm' )
!
!        C
!        C     Initialize the confinement and result windows.
!        C
!              CALL SSIZED ( 2,      CNFINE )
!              CALL SSIZED ( MAXWIN, RESULT )
!
!        C
!        C     Obtain the TDB time bounds of the confinement
!        C     window, which is a single interval in this case.
!        C
!              WIN0 = '2001 DEC 01 00:00:00 TDB'
!              WIN1 = '2002 JAN 01 00:00:00 TDB'
!
!              CALL STR2ET ( WIN0, ET0 )
!              CALL STR2ET ( WIN1, ET1 )
!
!        C
!        C     Insert the time bounds into the confinement
!        C     window.
!        C
!              CALL WNINSD ( ET0, ET1, CNFINE )
!
!        C
!        C     Select a 20 second step. We'll ignore any occultations
!        C     lasting less than 20 seconds.
!        C
!              CALL GFSSTP ( 20.D0 )
!
!        C
!        C     Turn on progress reporting; turn off interrupt
!        C     handling.
!        C
!              RPT  = .TRUE.
!              BAIL = .FALSE.
!
!        C
!        C     Perform the search.
!        C
!              CALL GFOCCE ( 'ANY',
!             .              'MOON',   'ellipsoid',  'IAU_MOON',
!             .              'SUN',    'ellipsoid',  'IAU_SUN',
!             .              'LT',     'EARTH',      CNVTOL,
!             .              GFSTEP,   GFREFN,       RPT,
!             .              GFREPI,   GFREPU,       GFREPF,
!             .              BAIL,     GFBAIL,       CNFINE,  RESULT )
!
!
!              IF ( WNCARD(RESULT) .EQ. 0 ) THEN
!
!                 WRITE (*,*) 'No occultation was found.'
!
!              ELSE
!
!                 DO I = 1, WNCARD(RESULT)
!
!        C
!        C           Fetch and display each occultation interval.
!        C
!                    CALL WNFETD ( RESULT, I, LEFT, RIGHT )
!
!                    CALL TIMOUT ( LEFT,  TIMFMT, BEGSTR )
!                    CALL TIMOUT ( RIGHT, TIMFMT, ENDSTR )
!
!                    WRITE (*,*) 'Interval ', I
!                    WRITE (*,*) '   Start time: '//BEGSTR
!                    WRITE (*,*) '   Stop time:  '//ENDSTR
!
!                 END DO
!
!              END IF
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        Occultation/transit search 100.00% done.
!
!         Interval            1
!            Start time: 2001 DEC 14 20:10:14.195952  (TDB)
!            Stop time:  2001 DEC 14 21:35:50.317994  (TDB)
!
!
!        Note that the progress report has the format shown below:
!
!           Occultation/transit search   6.02% done.
!
!        The completion percentage was updated approximately once per
!        second.
!
!
!     2) The following piece of code provides a more concrete example
!        of how these routines might be used. It is part of code that
!        performs a search for the time of an occultation of one body
!        by another. It is intended only for illustration and is not
!        recommended for use in code that has to do real work.
!
!        C
!        C     Prepare the progress reporter if appropriate.
!        C
!              IF ( RPT ) THEN
!                 CALL UDREPI ( CNFINE, 'Occultation/transit search ',
!             .                         'done.'                      )
!              END IF
!
!        C
!        C     Cycle over the intervals in the confining window.
!        C
!              COUNT = WNCARD(CNFINE)
!
!              DO I = 1, COUNT
!        C
!        C        Retrieve the bounds for the Ith interval of the
!        C        confinement window. Search this interval for
!        C        occultation events. Union the result with the
!        C        contents of the RESULT window.
!        C
!                 CALL WNFETD ( CNFINE, I, START, FINISH  )
!
!                 CALL ZZGFSOLV ( ZZGFOCST, UDSTEP, UDREFN, BAIL,
!             .                   UDBAIL,   CSTEP,  STEP,   START,
!             .                   FINISH,   TOL,    RPT,    UDREPU,
!             .                   RESULT                          )
!
!
!                 IF (  FAILED()  ) THEN
!                    CALL CHKOUT ( 'GFOCCE'  )
!                    RETURN
!                 END IF
!
!                 IF ( BAIL ) THEN
!        C
!        C           Interrupt handling is enabled.
!        C
!                    IF ( UDBAIL () ) THEN
!        C
!        C              An interrupt has been issued. Return now
!        C              regardless of whether the search has been
!        C              completed.
!        C
!                       CALL CHKOUT ( 'GFOCCE' )
!                       RETURN
!
!                    END IF
!
!                 END IF
!
!              END DO
!
!        C
!        C     End the progress report.
!        C
!              IF ( RPT ) THEN
!                 CALL UDREPF
!              END IF
!
!
!     3) For more concrete examples of how these routines are used in
!        SPICELIB, please refer to the actual code of any of the GF API
!        calls.
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
!     L.S. Elson         (JPL)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.2, 27-AUG-2021 (JDR)
!
!        Edited the header of all entry points and GFRPRT to comply with
!        NAIF standard.
!
!        Added complete example code to GFRPRT.
!
!-    SPICELIB Version 1.0.1, 10-FEB-2014 (BVS)
!
!        Added declarations of IVBEG and IVEND to the $Declarations
!        section of the GFREPU header.
!
!        Corrected declaration of WINDOW in the $Declarations
!        section and added descriptions of LBCELL to the GFREPI
!        header.
!
!-    SPICELIB Version 1.0.0, 06-MAR-2009 (NJB) (LSE) (WLT) (IMU)
!
!-&


! $ Index_Entries
!
!     GF progress report umbrella
!
!-&


!
!     SPICELIB functions
!
      integer(ip)           cardd
      integer(ip)           lastnb

      logical               failed
      logical               return

!
!     Local parameters
!
      integer(ip)           fprint
      parameter           ( fprint  =  32 )

      integer(ip)           lprint
      parameter           ( lprint  = 126 )

!
!     Local variables
!

      character*(mxbegm)    begin
      character*(mxbegm)    copyb
      character*(mxendm)    copye
      character*(mxendm)    end

      real(wp)              ave
      real(wp)              freq
      real(wp)              incr
      real(wp)              measur
      real(wp)              remain
      real(wp)              stddev
      real(wp)              t0
      real(wp)              total

      integer(ip)           chrcod
      integer(ip)           i
      integer(ip)           long
      integer(ip)           short
      integer(ip)           stdout
      integer(ip)           tcheck
      integer(ip)           unit

      save                  copyb
      save                  copye
      save                  t0
      save                  remain


      call chkin  ( 'GFRPRT'            )
      call sigerr ( 'SPICE(BOGUSENTRY)' )
      call chkout ( 'GFRPRT'            )
      return



!$Procedure GFREPI ( GF, progress report initialization )

      entry gfrepi ( window, begmss, endmss )

! $ Abstract
!
!     Initialize a search progress report.
!
! $ Required_Reading
!
!     GF
!
! $ Keywords
!
!     SEARCH
!     UTILITY
!
! $ Declarations
!
!     DOUBLE PRECISION      WINDOW ( LBCELL : * )
!     CHARACTER*(*)         BEGMSS
!     CHARACTER*(*)         ENDMSS
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     LBCELL     P   The SPICE cell lower bound.
!     MXBEGM     P   Maximum progress report message prefix length.
!     MXENDM     P   Maximum progress report message suffix length.
!     WINDOW     I   A window over which a job is to be performed.
!     BEGMSS     I   Beginning of the text portion of output message.
!     ENDMSS     I   End of the text portion of output message.
!
! $ Detailed_Input
!
!     WINDOW   is the name of a constraint window. This is the window
!              associated with some root finding activity. It is
!              used to determine how much total time is being searched
!              in order to find the events of interest.
!
!     BEGMSS   is the beginning of the progress report message written
!              to standard output by the GF subsystem. This output
!              message has the form
!
!                 BEGMSS(1:LASTNB(BEGMSS)) // ' xxx.xx% ' // ENDMSS
!
!              The total length of BEGMSS must be less than MXBEGM
!              characters. All characters of BEGMSS must be printable.
!
!              For example, the progress report message created by the
!              SPICELIB routine GFOCCE at the completion of a search is
!
!                 Occultation/transit search 100.00% done.
!
!              In this message, BEGMSS is
!
!                 'Occultation/transit search'
!
!     ENDMSS   is the last portion of the output message written to
!              standard output by the GF subsystem.
!
!              The total length of ENDMSS must be less than MXENDM
!              characters. All characters of ENDMSS must be printable.
!
!              In the progress report message created by GFOCCE at the
!              completion of a search, ENDMSS is
!
!                 'done.'
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     LBCELL   is the SPICE cell lower bound.
!
!     MXBEGM,
!     MXENDM   are, respectively, the maximum lengths of the progress
!              report message prefix and suffix. See the INCLUDE file
!              zzgf.inc for details.
!
! $ Exceptions
!
!     1)  If BEGMSS has length greater than MXBEGM characters, or if
!         ENDMSS has length greater than MXENDM characters, the error
!         SPICE(MESSAGETOOLONG) is signaled.
!
!     2)  If either BEGMSS or ENDMSS contains non-printing characters,
!         the error SPICE(NOTPRINTABLECHARS) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This entry point initializes the GF progress reporting system. It
!     is called by the GF root finding utilities once at the start of
!     each search pass. See the $Particulars section of the main
!     subroutine header for further details of its function.
!
! $ Examples
!
!     See $Examples in GFRPRT.
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
!     L.S. Elson         (JPL)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.2, 27-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!        Extended description of BEGMSS and ENDMSS arguments.
!
!-    SPICELIB Version 1.0.1, 10-FEB-2014 (BVS)
!
!        Corrected declaration of WINDOW in the $Declarations
!        section. Added description of LBCELL to the $Declarations,
!        $Brief_I/O, and $Parameters sections.
!
!-    SPICELIB Version 1.0.0, 21-FEB-2009 (NJB) (LSE) (WLT) (IMU)
!
!-&


! $ Index_Entries
!
!     GF initialize a progress report
!
!-&


!
!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

      call chkin  ( 'GFREPI' )

!
!     Check to see if either the message prefix or suffix
!     is too long.
!
      if ( lastnb(begmss) .gt. mxbegm ) then

         call setmsg ( 'Progress report prefix message contains ' &
         //            '# characters; limit is #.'                 )
         call errint ( '#', lastnb(begmss)                         )
         call errint ( '#', mxbegm                                 )
         call sigerr ( 'SPICE(MESSAGETOOLONG)'                     )
         call chkout ( 'GFREPI'                                    )
         return

      end if

      if ( lastnb(endmss) .gt. mxendm ) then

         call setmsg ( 'Progress report suffix message contains ' &
         //            '# characters; limit is #.'                 )
         call errint ( '#', lastnb(endmss)                         )
         call errint ( '#', mxendm                                 )
         call sigerr ( 'SPICE(MESSAGETOOLONG)'                     )
         call chkout ( 'GFREPI'                                    )
         return

      end if

!
!     Now check that all the characters in the message prefix and
!     suffix can be printed.
!
      do i = 1, lastnb(begmss)

         chrcod = ichar( begmss(i:i) )

         if ( ( chrcod .lt. fprint ) .or. ( chrcod .gt. lprint ) ) then

            call setmsg ( 'The progress report message prefix ' &
            //            'contains a nonprintable character; ' &
            //            'ASCII code is #.'                    )
            call errint ( '#', chrcod                           )
            call sigerr ( 'SPICE(NONPRINTABLECHARS)'            )
            call chkout ( 'GFREPI'                              )
            return

         end if

      end do

      do i = 1, lastnb(endmss)

         chrcod = ichar( endmss(i:i) )

         if ( ( chrcod .lt. fprint ) .or. ( chrcod .gt. lprint ) ) then

            call setmsg ( 'The progress report message suffix ' &
            //            'contains a nonprintable character; ' &
            //            'ASCII code is #.'                    )
            call errint ( '#', chrcod                           )
            call sigerr ( 'SPICE(NONPRINTABLECHARS)'            )
            call chkout ( 'GFREPI'                              )
            return

         end if

      end do


      copyb = begmss
      copye = endmss

!
!     Find the length of the window. Use that to initialize the work
!     reporter.
!
      call wnsumd   ( window, measur, ave, stddev, short, long )
      call zzgftswk ( measur, 1.0_wp,  4,   begmss, endmss )

      if ( failed() ) then
         call chkout ( 'GFREPI' )
         return
      end if


!
!     Initialize the time to the start of the confinement window.
!     The remaining amount of work in the current interval is
!     the measure of the interval.
!
      if ( cardd(window) .ge. 2 ) then

         t0     = window(1)
         remain = window(2) - t0

      else

         remain = 0.0_wp

      end if



      call chkout  ( 'GFREPI' )
      return





!$Procedure GFREPU ( GF, progress report update )

      entry gfrepu ( ivbeg, ivend, time )

! $ Abstract
!
!     Tell the progress reporting system how far a search has
!     progressed.
!
! $ Required_Reading
!
!     GF
!
! $ Keywords
!
!     SEARCH
!     UTILITY
!
! $ Declarations
!
!     DOUBLE PRECISION      IVBEG
!     DOUBLE PRECISION      IVEND
!     DOUBLE PRECISION      TIME
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     IVBEG      I   Start time of work interval.
!     IVEND      I   End time of work interval.
!     TIME       I   Current time being examined in the search process
!
! $ Detailed_Input
!
!     IVBEG,
!     IVEND    are the bounds of an interval that is contained in some
!              interval belonging to the confinement window. The
!              confinement window is associated with some root finding
!              activity. It is used to determine how much total time is
!              being searched in order to find the events of interest.
!
!              In order for a meaningful progress report to be
!              displayed, IVBEG and IVEND must satisfy the following
!              constraints:
!
!                 - IVBEG must be less than or equal to IVEND.
!
!                 - The interval [ IVBEG, IVEND ] must be contained in
!                   some interval of the confinement window. It can be
!                   a proper subset of the containing interval; that
!                   is, it can be smaller than the interval of the
!                   confinement window that contains it.
!
!                 - Over a search pass, the sum of the differences
!
!                      IVEND - IVBEG
!
!                   for all calls to this routine made during the pass
!                   must equal the measure of the confinement window.
!
!
!     TIME     is the current time reached in the search for an event.
!              TIME must lie in the interval
!
!                 IVBEG : IVEND
!
!              inclusive. The input values of TIME for a given interval
!              need not form an increasing sequence.
!
! $ Detailed_Output
!
!     None. This routine does perform console I/O when progress
!     reporting is enabled.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If IVBEG and IVEND are in decreasing order, the error
!         SPICE(BADENDPOINTS) is signaled.
!
!     2)  If TIME is not in the closed interval [IVBEG, IVEND], the
!         error SPICE(VALUEOUTOFRANGE) is signaled.
!
!     3)  If an I/O error results from writing to standard output, the
!         error is signaled by a routine in the call tree of this
!         routine.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This entry point is used to indicate the current progress of a
!     search. Using information recorded through the initialization
!     entry point of this routine, the progress reporting system
!     determines how much work has been completed and whether or not to
!     report it on the users screen.
!
! $ Examples
!
!     See $Examples in GFRPRT.
!
! $ Restrictions
!
!     1)  This routine has no way of enforcing that the input values of
!         IVBEG and IVEND are compatible with the input window passed to
!         GFREPI. Callers of this routine are responsible for ensuring
!         that this requirement is obeyed.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     L.S. Elson         (JPL)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.2, 27-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.1, 10-FEB-2014 (BVS)
!
!        Added declarations of IVBEG and IVEND to the $Declarations
!        section.
!
!-    SPICELIB Version 1.0.0, 21-FEB-2009 (NJB) (LSE) (WLT) (IMU)
!
!-&


! $ Index_Entries
!
!     GF update a progress report
!
!-&


      if ( return () ) then
         return
      end if

      call chkin  ( 'GFREPU' )

!
!     Do a few error checks before getting started.
!
!     We expect the endpoints of the current window to be in order.
!
      if ( ivend .lt. ivbeg ) then

         call setmsg ( 'Interval endpoints are #:#; endpoints ' &
         //            'must be in increasing order.'             )
         call errdp  ( '#',  ivbeg                                )
         call errdp  ( '#',  ivend                                )
         call sigerr ( 'SPICE(BADENDPOINTS)'                      )
         call chkout ( 'GFREPU'                                   )
         return
      end if

!
!     We expect TIME to be in the current interval of the confinement
!     window.
!
      if (  ( time .lt. ivbeg ) .or. ( time .gt. ivend )  ) then

         call setmsg ( 'TIME should be in interval #:# but ' &
         //            'is #.'                               )
         call errdp  ( '#',  time                            )
         call errdp  ( '#',  ivbeg                           )
         call errdp  ( '#',  ivend                           )
         call sigerr ( 'SPICE(VALUEOUTOFRANGE)'              )
         call chkout ( 'GFREPU'                              )
         return
      end if

!
!     The amount of work done is the difference between the current
!     time and the previous time T0, presuming both times are in
!     the current interval.  Note this work amount may be negative.
!
      if (  ( t0 .ge. ivbeg ) .and. ( t0 .le. ivend )  ) then

         incr = time - t0

      else
!
!        T0 is in the previous interval.  The amount of work
!        done to complete processing of that interval is REMAIN.
!        The amount of work done in the current interval is
!        the difference of TIME and the left endpoint of the
!        interval.
!
         incr = remain + time - ivbeg

      end if

!
!     The remaining work is the distance from TIME to the right
!     endpoint of the current interval.
!
      remain = ivend - time

!
!     Record the current time as T0.
!
      t0  =  time

!
!     Report the work increment.
!
      call zzgfwkin ( incr )

      call chkout  ( 'GFREPU' )
      return


!$Procedure GFREPF ( GF, progress report finalization )

      entry gfrepf

! $ Abstract
!
!     Finish a progress report.
!
! $ Required_Reading
!
!     GF
!
! $ Keywords
!
!     SEARCH
!     UTILITY
!
! $ Declarations
!
!     None.
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     None.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     None. This routine does perform console I/O when progress
!     reporting is enabled.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If an I/O error results from writing to standard output, the
!         error is signaled by a routine in the call tree of this
!         routine.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This entry point "finishes" a progress report, i.e. updates the
!     report to indicate the underlying task is 100% complete.
!
! $ Examples
!
!     See $Examples in GFRPRT.
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
!     L.S. Elson         (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 07-APR-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 21-FEB-2009 (NJB) (LSE) (WLT) (IMU)
!
!-&


! $ Index_Entries
!
!     GF finish a progress report
!
!-&


      if ( return () ) then
         return
      end if

      call chkin  ( 'GFREPF' )

      call zzgfwkad ( 0.0_wp, 1, copyb, copye )
      call zzgfwkin ( 0.0_wp )

!
!     Determine whether progress report output is currently
!     being sent to standard output. Fetch the output unit.
!
      call zzgfwkmo ( unit, total, freq, tcheck, begin, end, incr )

      call stdio ( 'STDOUT', stdout )

      if ( unit .ne. stdout ) then
!
!        We're not currently writing to standard output, so we're
!        done.
!
         call chkout ( 'GFREPF' )
         return

      end if

!
!     Emit a final blank line by moving the cursor down two
!     spaces.
!
!     The set of actual arguments passed here is rather funky
!     and deserves some explanation:
!
!        The first argument, calling for a leading blank line, moves
!        the cursor down so that the next blank line written won't
!        overwrite the final status message. That blank line is
!        followed with a cursor repositioning command that moves the
!        cursor to the beginning of the line that was just written. The
!        last argument, calling for another blank line, moves the
!        cursor down again. The total cursor movement is down 2 lines.
!        This results in one skipped line.
!
!     We could accomplish the same results more simply if were
!     were to use I/O statements in this routine; however, in the
!     interest of minimizing the number of places where I/O is
!     performed, we rely on ZZGFDSPS to do that job.
!
      call zzgfdsps  ( 1, ' ', 'A', 1 )


      call chkout  ( 'GFREPF' )
      return
      end
