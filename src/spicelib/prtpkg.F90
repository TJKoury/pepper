!$Procedure PRTPKG ( Declare Arguments for Error Message Routines )

      logical function prtpkg ( short, long, expl, trace, dfault, type )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Declare the arguments for the error message selection entry
!     points.  DO NOT CALL THIS ROUTINE.
!
! $ Required_Reading
!
!     ERROR
!
! $ Keywords
!
!     ERROR
!
! $ Declarations

      implicit none

      logical               short
      logical               expl
      logical               long
      logical               trace
      logical               dfault
      character(*)          type

       integer(ip)           filen
#if defined(SPICE_OS_WINDOWS) && defined(SPICE_COMPILER_INTEL)
       parameter           ( filen = 128 )
#else
       parameter           ( filen = 255 )
#endif

! $ Brief_I/O
!
!     VARIABLE  I/O  ENTRY POINTS
!     --------  ---  --------------------------------------------------
!     SHORT      I   SETPRT
!     EXPL       I   SETPRT
!     LONG       I   SETPRT
!     TRACE      I   SETPRT
!     DFAULT     I   SETPRT
!     TYPE       I   MSGSEL
!     FILEN      P   MSGSEL
!
! $ Detailed_Input
!
!     See the ENTRY points for discussions of their arguments.
!
! $ Detailed_Output
!
!     See the ENTRY points for discussions of their arguments.
!
! $ Parameters
!
!     See the ENTRY points for discussions of their parameters.
!
! $ Exceptions
!
!     1)  If PRTPKG is called directly, the error SPICE(BOGUSENTRY) is
!         signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     DO NOT CALL THIS ROUTINE.
!
!     The entry points declared in this routine are:
!
!        SETPRT
!        MSGSEL
!
!     There is no reason to call this subroutine. The purpose of this
!     subroutine is to make the declarations required by the various
!     entry points. This routine has no run-time function.
!
! $ Examples
!
!     None.  DO NOT CALL THIS ROUTINE.
!
! $ Restrictions
!
!     1)  DO NOT CALL THIS ROUTINE.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     H.A. Neilan        (JPL)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 3.27.0, 28-NOV-2021 (BVS)
!
!        Updated for MAC-OSX-M1-64BIT-CLANG_C.
!
!-    SPICELIB Version 3.26.0, 17-JUN-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header of the PRTPKG umbrella routine and all its
!        entry entry points to comply with NAIF standard.
!
!-    SPICELIB Version 3.25.0, 10-MAR-2014 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-INTEL.
!
!-    SPICELIB Version 3.24.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-LINUX-64BIT-IFORT.
!
!-    SPICELIB Version 3.23.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-GFORTRAN.
!
!-    SPICELIB Version 3.22.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GFORTRAN.
!
!-    SPICELIB Version 3.21.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GCC_C.
!
!-    SPICELIB Version 3.20.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL.
!
!-    SPICELIB Version 3.19.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL-CC_C.
!
!-    SPICELIB Version 3.18.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
!
!-    SPICELIB Version 3.17.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
!
!-    SPICELIB Version 3.16.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-WINDOWS-64BIT-IFORT.
!
!-    SPICELIB Version 3.15.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-LINUX-64BIT-GFORTRAN.
!
!-    SPICELIB Version 3.14.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-64BIT-MS_C.
!
!-    SPICELIB Version 3.13.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-INTEL_C.
!
!-    SPICELIB Version 3.12.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-IFORT.
!
!-    SPICELIB Version 3.11.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-GFORTRAN.
!
!-    SPICELIB Version 3.10.0, 18-MAR-2009 (BVS)
!
!        Updated for PC-LINUX-GFORTRAN.
!
!-    SPICELIB Version 3.9.0, 18-MAR-2009 (BVS)
!
!        Updated for MAC-OSX-GFORTRAN.
!
!-    SPICELIB Version 3.8.0, 19-FEB-2008 (BVS)
!
!        Updated for PC-LINUX-IFORT.
!
!-    SPICELIB Version 3.7.0, 14-NOV-2006 (BVS)
!
!        Updated for PC-LINUX-64BIT-GCC_C.
!
!-    SPICELIB Version 3.6.0, 14-NOV-2006 (BVS)
!
!        Updated for MAC-OSX-INTEL_C.
!
!-    SPICELIB Version 3.5.0, 14-NOV-2006 (BVS)
!
!        Updated for MAC-OSX-IFORT.
!
!-    SPICELIB Version 3.4.0, 14-NOV-2006 (BVS)
!
!        Updated for PC-WINDOWS-IFORT.
!
!-    SPICELIB Version 3.3.0, 26-OCT-2005 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-GCC_C.
!
!-    SPICELIB Version 3.2.0, 03-JAN-2005 (BVS)
!
!        Updated for PC-CYGWIN_C.
!
!-    SPICELIB Version 3.1.0, 03-JAN-2005 (BVS)
!
!        Updated for PC-CYGWIN.
!
!-    SPICELIB Version 3.0.5, 17-JUL-2002 (BVS)
!
!        Added MAC-OSX environments.
!
!-    SPICELIB Version 3.0.4, 08-OCT-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are WIN-NT
!
!-    SPICELIB Version 3.0.3, 24-SEP-1999 (NJB)
!
!        CSPICE environments were added. Some typos were corrected.
!
!-    SPICELIB Version 3.0.2, 28-JUL-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
!
!-    SPICELIB Version 3.0.1, 18-MAR-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. Previously,
!        environments such as SUN-SUNOS and SUN-SOLARIS were implied
!        by the environment label SUN.
!
!-    SPICELIB Version 3.0.0, 07-APR-1998 (NJB)
!
!        Module was updated for the PC-LINUX platform.
!
!-    SPICELIB Version 2.0.0, 09-NOV-1993 (HAN)
!
!         Module was updated to include the value for FILEN
!         for the Silicon Graphics, DEC Alpha-OSF/1, and
!         NeXT platforms. Also, the previous value of 256 for
!         Unix platforms was changed to 255.
!
!-    SPICELIB Version 1.1.0, 12-OCT-1992 (HAN)
!
!         Updated module for multiple environments.
!
!         The code was also reformatted so that a utility program can
!         create the source file for a specific environment given a
!         master source file.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!         Comment section for permuted index source lines was added
!         following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (NJB)
!
!-&


! $ Index_Entries
!
!     None.
!
!-&


! $ Revisions
!
!-    SPICELIB Version 3.0.0, 07-APR-1998 (NJB)
!
!        Module was updated for the PC-LINUX platform.
!
!-     SPICELIB Version 2.0.0, 9-NOV-1993 (HAN)
!
!         Module was updated to include the value for FILEN
!         for the Silicon Graphics, DEC Alpha-OSF/1, and
!         NeXT platforms. Also, the previous value of 256 for
!         Unix platforms was changed to 255.
!
!-     SPICELIB Version 1.1.0, 12-OCT-1992 (HAN)
!
!         Updated module for multiple environments.
!
!         The code was also reformatted so that a utility program can
!         create the source file for a specific environment given a
!         master source file.
!
!-     Beta Version 1.1.0, 13-DEC-1989 (NJB)
!
!         PRTPKG, though it performs no run-time function, must
!         still return a value, in order to comply with the Fortran
!         standard. So, now it does.
!
!-     Beta Version 1.0.1, 08-FEB-1989 (NJB)
!
!         Warnings added to discourage use of this routine.
!         Parameter declarations moved to "Declarations" section.
!         Two local declarations moved to the correct location.
!
!-&


!
!     SPICELIB functions
!

      logical               setprt
      logical               msgsel

!
!     Local variables:
!
      character(filen)      device

      character(10)         ltype
      character(10)         loctyp

!
!     Saved variables:
!
      logical               svshrt
      logical               svexpl
      logical               svlong
      logical               svtrac
      logical               svdflt

      save                  svshrt
      save                  svexpl
      save                  svlong
      save                  svtrac
      save                  svdflt

!
!     Initial values:
!
      data     svshrt   / .true. /
      data     svexpl   / .true. /
      data     svlong   / .true. /
      data     svtrac   / .true. /
      data     svdflt   / .true. /

!
!     Executable Code:
!

      call getdev ( device )

      call wrline ( device, &
                    'PRTPKG:  You have called an entry point which' // &
                    ' has no run-time function; this may indicate'  // &
                    ' a program bug.  Please check the PRTPKG'      // &
                    ' documentation.  ' )

      call wrline ( device, 'SPICE(BOGUSENTRY)' )

      prtpkg = .false.

      return






!$Procedure SETPRT ( Store Error Message Types to be Output )

      entry setprt ( short, expl, long, trace, dfault )

! $ Abstract
!
!     Store (a representation of) the selection of types of error
!     messages to be output.  DO NOT CALL THIS ROUTINE.
!
! $ Required_Reading
!
!     ERROR
!
! $ Keywords
!
!     ERROR
!
! $ Declarations
!
!     LOGICAL               SHORT
!     LOGICAL               EXPL
!     LOGICAL               LONG
!     LOGICAL               TRACE
!     LOGICAL               DFAULT
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     SHORT      I   Select output of short error message?
!     EXPL       I   Select output of explanation of short message?
!     LONG       I   Select output of long error message?
!     TRACE      I   Select output of traceback?
!     DFAULT     I   Select output of default message?
!
! $ Detailed_Input
!
!     SHORT    indicates whether the short error message is selected
!              as one of the error messages to be output when an error
!              is detected. A value of .TRUE. indicates that the
!              short error message IS selected.
!
!     EXPL     indicates whether the explanatory text for the short
!              error message is selected as one of the error messages
!              to be output when an error is detected. A value of
!              .TRUE. indicates that the explanatory text for the
!              short error message IS selected.
!
!     LONG     indicates whether the long error message is selected
!              as one of the error messages to be output when an error
!              is detected. A value of .TRUE. indicates that the
!              long error message IS selected.
!
!     TRACE    indicates whether the traceback is selected
!              as one of the error messages to be output when an error
!              is detected. A value of .TRUE. indicates that the
!              traceback IS selected.
!
!     DFAULT   indicates whether the default message is selected
!              as one of the error messages to be output when an error
!              is detected. A value of .TRUE. indicates that the
!              default message IS selected.
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
!     None.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     DO NOT CALL THIS ROUTINE.
!
!     The effect of this routine is an ENVIRONMENTAL one. This
!     routine performs no output;  it stores the error message
!     selection provided as input.
!
!     Note that the actual output of error messages depends not
!     only on the selection made using this routine, but also
!     on the selection of the error output device (see ERRDEV)
!     and the choice of error response action (see ERRACT). If
!     the action is not 'IGNORE' (possible choices are
!     'IGNORE', 'ABORT', 'DEFAULT', 'REPORT', and 'RETURN'),
!     the selected error messages will be written to the chosen
!     output device when an error is detected.
!
! $ Examples
!
!     1. In this example, the short and long messages are selected.
!
!     C
!     C     Select short and long error messages for output
!     C     (We don't examine the status returned because no
!     C     errors are detected by SETPRT):
!     C
!
!           STATUS = SETPRT ( .TRUE., .FALSE., .TRUE., .FALSE.,
!          .                  .FALSE.                          )
!
! $ Restrictions
!
!     1)  DO NOT CALL THIS ROUTINE.
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
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 3.1.0, 17-JUN-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 3.0.5, 17-JUL-2002 (BVS)
!
!        Added MAC-OSX environments.
!
!-    SPICELIB Version 3.0.4, 08-OCT-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are WIN-NT
!
!-    SPICELIB Version 3.0.2, 28-JUL-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
!
!-    SPICELIB Version 3.0.1, 18-MAR-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. Previously,
!        environments such as SUN-SUNOS and SUN-SOLARIS were implied
!        by the environment label SUN.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!         Comment section for permuted index source lines was added
!         following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (NJB)
!
!-&


! $ Index_Entries
!
!     None.
!
!-&


! $ Revisions
!
!-     Beta Version 1.0.1, 08-FEB-1989 (NJB)
!
!         Warnings added to discourage use of this routine in
!         non-error-handling code. $Parameters section added.
!
!-&


!
!     Executable Code:
!


      if   (  short  )    then
         svshrt = .true.
      else
         svshrt = .false.
      end if



      if   (  expl   )    then
         svexpl = .true.
      else
         svexpl = .false.
      end if



      if   (  long   )    then
         svlong = .true.
      else
         svlong = .false.
      end if



      if   (  trace  )    then
         svtrac = .true.
      else
         svtrac = .false.
      end if

      if   (  dfault  )    then
         svdflt = .true.
      else
         svdflt = .false.
      end if


!
!     We assign a value to SETPRT, but this value is
!     not meaningful...
!
      setprt = .true.


      return




!$Procedure MSGSEL  ( Is This Message Type Selected for Output? )

      entry  msgsel ( type )

! $ Abstract
!
!     Indicate whether the specified message type has been selected
!     for output.
!
! $ Required_Reading
!
!     ERROR
!
! $ Keywords
!
!     ERROR
!
! $ Declarations
!
!     CHARACTER*(*)         TYPE
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     TYPE       I   Type of message whose selection status is queried.
!     FILEN      P   Maximum length of a file name.
!
!     The function returns the value .TRUE. if the message type
!     indicated by TYPE has been selected for output to the error output
!     device.
!
! $ Detailed_Input
!
!     TYPE     is the type of error message.
!
!              Possible values of TYPE are 'SHORT', 'EXPLAIN', 'LONG',
!              'DEFAULT', and 'TRACEBACK'.
!
! $ Detailed_Output
!
!     The function returns the value .TRUE. if the message type
!     indicated by TYPE has been selected for output to the error
!     output device.
!
! $ Parameters
!
!     FILEN    is the maximum length of a file name.
!
! $ Exceptions
!
!     1)  If an invalid value of TYPE is detected, the error
!         SPICE(INVALIDMSGTYPE) is signaled. The handling of this error
!         is a special case; to avoid recursion problems, SIGERR is not
!         called when the error is detected. Instead, the short and long
!         error messages are output directly.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine is part of the SPICELIB error handling mechanism.
!
!     Note that even though a given type of message may have been
!     selected for output, the output device and error response
!     action must also have been selected appropriately.
!     Use ERRDEV to choose the output device for error messages.
!     Use ERRACT to choose the error response action. Any action
!     other than 'IGNORE' will result in error messages being
!     written to the error output device when errors are detected.
!     See ERRACT for details.
!
! $ Examples
!
!     1. We want to know if the short message has been selected
!         for output:
!
!         C
!         C     Test whether the short message has been selected:
!         C
!
!               SELECT = MSGSEL ( 'SHORT' )
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
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 3.1.0, 17-JUN-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Corrected
!        type declaration in $Declarations section.
!
!-    SPICELIB Version 3.0.5, 17-JUL-2002 (BVS)
!
!        Added MAC-OSX environments.
!
!-    SPICELIB Version 3.0.4, 08-OCT-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are WIN-NT
!
!-    SPICELIB Version 3.0.2, 28-JUL-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
!
!-    SPICELIB Version 3.0.1, 18-MAR-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. Previously,
!        environments such as SUN-SUNOS and SUN-SOLARIS were implied
!        by the environment label SUN.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!         Comment section for permuted index source lines was added
!         following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (NJB)
!
!-&


! $ Index_Entries
!
!     None.
!
!-&


! $ Revisions
!
!-     Beta Version 1.0.1, 08-FEB-1989 (NJB)
!
!         $Parameters section added; parameter declaration added
!         to brief I/O section as well.
!
!-&


!
!     Executable Code:
!

      call ljust ( type,  ltype )
      call ucase ( ltype, ltype )


      if ( ltype  == 'SHORT' ) then

         msgsel = svshrt

      else if ( ltype  == 'EXPLAIN' ) then

         msgsel = svexpl

      else if ( ltype  == 'LONG' ) then

         msgsel = svlong

      else if ( ltype  == 'TRACEBACK' ) then

         msgsel = svtrac

      else if ( ltype  == 'DEFAULT' ) then

         msgsel = svdflt

      else

!
!        Bad value of type!  We have a special case here; to
!        avoid recursion, we output the messages directly,
!        rather than call SIGERR.
!

         call getdev ( device )

         call wrline ( device,  'SPICE(INVALIDMSGTYPE)' )

         call wrline ( device, ' ' )

         loctyp = type

!
!        Note:  What looks like a typo below isn't; there's
!        a line break after the substring 'specified' of
!        the "word" 'specifiedwas'.
!

         call wrline ( device, &
         'MSGSEL:  An invalid error message type was supplied as' // &
         ' input; the type specifiedwas:  ' // loctyp &
                      )


      end if

      end
