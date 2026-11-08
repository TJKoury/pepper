!$Procedure BYEBYE ( Exit a program indicating an error status )

      subroutine byebye ( status )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Exit an executing program returning a success or failure status
!     to the operating system, if this capability is supported, or
!     simply exit the program.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     UTILITY
!
! $ Declarations

      implicit none

      character(*)          status

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     STATUS     I   A string indicating the exit status of a program.
!
! $ Detailed_Input
!
!     STATUS   is a character string which indicates the status
!              to use when exiting a program. The two status values
!              currently supported are 'SUCCESS' and 'FAILURE', which
!              have their obvious meanings. The case of the input is
!              not important, i.e., 'Success' or 'failure' are accepted.
!
!              If STATUS has a value of 'SUCCESS', then the calling
!              program will be terminated with a status that indicates
!              success.
!
!              If STATUS has a value of 'FAILURE', then the calling
!              program will be terminated with a status that indicates
!              failure.
!
!              If STATUS has a value that is not recognized, the calling
!              program will be terminated with a status that indicates
!              failure.
!
!              For environments which do not support the passage of a
!              status indicator to the operating system the value of
!              STATUS is not significant; an executing program will be
!              terminated using the Fortran STOP statement.
!
!              See the value of the parameter STLEN declared in the
!              Local $Parameters section for the maximum significant
!              length of an exit status string.
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
!     None.
!
! $ Particulars
!
!     This subroutine is called by SIGERR to exit a program
!     returning a success or failure indication to the operating
!     system when this is possible. If returning a status
!     indication to the operating system is not possible, this
!     subroutine simply executes a Fortran STOP statement.
#if defined(SPICE_OS_WINDOWS) && defined(SPICE_COMPILER_INTEL)
!
!     In the case of Microsoft FORTRAN, stopping with an exit status
!     works only if the status is given as a literal value. A constant
!     parameter will not do the job. For example is you want to
!     issue a STOP with the status value 1, you cannot use a variable
!     or parameter as in
!
!            INTEGER               FAILUR
!            PARAMETER           ( FAILUR = 1 )
!
!            STOP FAILUR
!
!     instead you must code the STOP command as:
!
!            STOP 1
!
#else
#endif
!
! $ Examples
!
!     To exit a program indicating success:
!
!        CALL BYEBYE ( 'SUCCESS' )
!
!     To exit a program indicating failure:
!
!        CALL BYEBYE ( 'FAILURE' )
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
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.16.0, 13-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Removed
!        unnecessary $Revisions section.
!
!-    SPICELIB Version 2.15.0, 10-MAR-2014 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-INTEL.
!
!-    SPICELIB Version 2.14.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-LINUX-64BIT-IFORT.
!
!-    SPICELIB Version 2.13.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-GFORTRAN.
!
!-    SPICELIB Version 2.12.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GFORTRAN.
!
!-    SPICELIB Version 2.11.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL.
!
!-    SPICELIB Version 2.10.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-WINDOWS-64BIT-IFORT.
!
!-    SPICELIB Version 2.9.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-LINUX-64BIT-GFORTRAN.
!
!-    SPICELIB Version 2.8.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-IFORT.
!
!-    SPICELIB Version 2.7.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-GFORTRAN.
!
!-    SPICELIB Version 2.6.0, 18-MAR-2009 (BVS)
!
!        Updated for PC-LINUX-GFORTRAN.
!
!-    SPICELIB Version 2.5.0, 18-MAR-2009 (BVS)
!
!        Updated for MAC-OSX-GFORTRAN.
!
!-    SPICELIB Version 2.4.0, 19-FEB-2008 (BVS)
!
!        Updated for PC-LINUX-IFORT.
!
!-    SPICELIB Version 2.3.0, 14-NOV-2006 (BVS)
!
!        Updated for MAC-OSX-IFORT.
!
!-    SPICELIB Version 2.2.0, 14-NOV-2006 (BVS)
!
!        Updated for PC-WINDOWS-IFORT.
!
!-    SPICELIB Version 2.1.0, 03-JAN-2005 (BVS)
!
!        Updated for PC-CYGWIN.
!
!-    SPICELIB Version 2.0.5, 17-JUL-2002 (BVS)
!
!        Added MAC-OSX environments.
!
!-    SPICELIB Version 2.0.4, 28-AUG-2000 (EDW)
!
!        Included PROMPT call for MAC Absoft environment.
!        Without something to pause execution, the output window
!        collapses at program end.
!
!-    SPICELIB Version 2.0.3, 08-OCT-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are WIN-NT
!
!-    SPICELIB Version 2.0.2, 28-JUL-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
!
!        The success flag for SGI-N32 is '0', failure '1'. These
!        were simply 0 and 1 respectively when the environment was
!        just SGI.
!
!-    SPICELIB Version 2.0.1, 18-MAR-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. Previously,
!        environments such as SUN-SUNOS and SUN-SOLARIS were implied
!        by the environment label SUN.
!
!-    SPICELIB Version 2.0.0, 05-APR-1998 (NJB)
!
!        Added the PC-LINUX environment.
!
!-    SPICELIB Version 1.0.0, 25-APR-1996 (KRG)
!
!-&


! $ Index_Entries
!
!     gracefully exit a program
!
!-&


!
!     Local Parameters
!
!     Define the maximum length for the usable portion of a status
!     string.
!
      integer(ip)           stlen
      parameter           ( stlen = 7 )
!
!     Define the notion of 'SUCCESS' and 'FAILURE'.
!
#if defined(SPICE_OS_WINDOWS) && defined(SPICE_COMPILER_INTEL)
#else
      integer(ip)           failur
      parameter           ( failur = 1 )

      integer(ip)           succes
      parameter           ( succes = 0 )
#endif


      character(stlen)      chstat

      logical               mystat

      call ljust ( status, chstat )
      call ucase ( chstat, chstat )

      if ( chstat == 'SUCCESS' ) then
         mystat = .true.
      else
         mystat = .false.
      end if

      if ( mystat ) then
#if defined(SPICE_OS_WINDOWS) && defined(SPICE_COMPILER_INTEL)
         stop 0
#else
         call exit ( succes )
#endif
      else
#if defined(SPICE_OS_WINDOWS) && defined(SPICE_COMPILER_INTEL)
         stop 1
#else
         call exit ( failur )
#endif
      end if

!
!     We never really get here, but for pedantic reasons we must
!     have a RETURN statement in a subroutine.
!
      return

      end
