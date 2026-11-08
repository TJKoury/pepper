!$Procedure     EDTCMD ( Edit a file using a specified text editor )

      SUBROUTINE EDTCMD ( CMD, FILE )

! $ Abstract
!
!     Edit a file using a specified editor.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     SYSTEM
!     UTILITY
!
! $ Declarations

      CHARACTER*(*)         CMD
      CHARACTER*(*)         FILE

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     CMD        I   Command string used to invoke editor.
!     FILE       I   Name of file to edit.
!
! $ Detailed_Input
!
!     CMD            is a character string containing the command
!                    used to invoke a text editor available on the
!                    system under which the calling program is running.
!                    This routine will invoke the specified editor
!                    using FILE as the target file to edit.  The name
!                    of the file to be edited is not included in the
!                    command; this name is input as a separate argument.
!
!                    Case sensitivity of CMD varies with the system on
!                    which the calling program is run.
!
!                    Trailing white space in CMD is not significant.
!
!
!     FILE           is the name of a file that is to be edited.  FILE
!                    need not exist at the time this routine is called.
!
!                    Case sensitivity of FILE varies with the system on
!                    which the calling program is run.
!
!                    Trailing white space in FILE is not significant.
!
! $ Detailed_Output
!
!     None.  See $Particulars for further information on the action of
!     this routine.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the specified edit command fails, the error will be
!         diagnosed by routines called by this routine.
!
!     2)  If the editing session started by this routine is terminated
!         abnormally, the effect on the operation of the calling program
!         is unspecified.
!
! $ Files
!
!     See $Particulars.
!
! $ Particulars
!
!     This routine should be used with caution; calls to this routine
!     may have unintended side effects on the operation of the calling
!     program.  A solid understanding of the global operation of the
!     calling program is a prerequisite for programmers wishing to
!     use this routine.
!
!     The input argument FILE should unambiguously designate a file
!     that can be edited by the specified editor on the system under
!     which the calling program is being run.  The calling program
!     should have read or write privileges consistent with the intended
!     mode of access to FILE.
!
!     This routine may fail to recover in a predictable fashion from
!     abnormal termination of an editing session.
!
! $ Examples
!
!     1)   On a VAX/VMS system, the EDT editor could be invoked by
!          the calls
!
!             CALL EDTCMD ( 'EDIT/EDT',  FILE  )
!
!          or
!
!             CALL EDTCMD ( 'EDIT/EDT/COMMAND = <command file>',  FILE )
!
!
!     2)   On a Unix system, the emacs editor could be invoked
!          (normally) by the calls
!
!              CALL EDTCMD ( 'emacs', FILE )
!
!          or
!
!              CALL EDTCMD ( '/usr/bin/emacs', FILE )
!
!
! $ Restrictions
!
!     1)   The means by which this routine invokes an editor are system-
!          dependent; invoking the editor may have side effects that
!          affect the operation of the calling program.  For example,
!          on Unix systems, this routine may start a new shell in which
!          to run the editor; starting a new shell may interfere with
!          any sequential file I/O in progress at the time the shell is
!          started.
!
!          See the code for implementation details.
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
!-    Beta Version 2.28.0, 28-NOV-2021 (BVS)
!
!        Updated for MAC-OSX-M1-64BIT-CLANG_C.
!
!-    Beta Version 2.27.0, 10-MAR-2014 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-INTEL.
!
!-    Beta Version 2.26.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-LINUX-64BIT-IFORT.
!
!-    Beta Version 2.25.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-GFORTRAN.
!
!-    Beta Version 2.24.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GFORTRAN.
!
!-    Beta Version 2.23.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GCC_C.
!
!-    Beta Version 2.22.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL.
!
!-    Beta Version 2.21.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL-CC_C.
!
!-    Beta Version 2.20.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
!
!-    Beta Version 2.19.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
!
!-    Beta Version 2.18.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-WINDOWS-64BIT-IFORT.
!
!-    Beta Version 2.17.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-LINUX-64BIT-GFORTRAN.
!
!-    Beta Version 2.16.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-64BIT-MS_C.
!
!-    Beta Version 2.15.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-INTEL_C.
!
!-    Beta Version 2.14.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-IFORT.
!
!-    Beta Version 2.13.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-GFORTRAN.
!
!-    Beta Version 2.12.0, 18-MAR-2009 (BVS)
!
!        Updated for PC-LINUX-GFORTRAN.
!
!-    Beta Version 2.11.0, 18-MAR-2009 (BVS)
!
!        Updated for MAC-OSX-GFORTRAN.
!
!-    Beta Version 2.10.0, 19-FEB-2008 (BVS)
!
!        Updated for PC-LINUX-IFORT.
!
!-    Beta Version 2.9.0, 14-NOV-2006 (BVS)
!
!        Updated for PC-LINUX-64BIT-GCC_C.
!
!-    Beta Version 2.8.0, 14-NOV-2006 (BVS)
!
!        Updated for MAC-OSX-INTEL_C.
!
!-    Beta Version 2.7.0, 14-NOV-2006 (BVS)
!
!        Updated for MAC-OSX-IFORT.
!
!-    Beta Version 2.6.0, 14-NOV-2006 (BVS)
!
!        Updated for PC-WINDOWS-IFORT.
!
!-    Beta Version 2.5.0, 26-OCT-2005 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-GCC_C.
!
!-    Beta Version 2.4.0, 03-JAN-2005 (BVS)
!
!        Updated for PC-CYGWIN_C.
!
!-    Beta Version 2.3.0, 03-JAN-2005 (BVS)
!
!        Updated for PC-CYGWIN.
!
!-    Beta Version 2.2.5, 17-JUL-2002 (BVS)
!
!        Added MAC-OSX environments.
!
!-    Beta Version 2.2.4, 08-OCT-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given.  New
!        environments are WIN-NT
!
!-    Beta Version 2.2.3, 20-SEP-1999 (NJB)
!
!        CSPICE and PC-LINUX environment lines were added.  Some
!        typos were corrected.
!
!-    Beta Version 2.2.2, 28-JUL-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given.  New
!        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
!
!-    Beta Version 2.2.1, 18-MAR-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given.  Previously,
!        environments such as SUN-SUNOS and SUN-SOLARIS were implied
!        by the environment label SUN.
!
!-    Beta Version 2.2.0, 12-AUG-1996 (WLT)
!
!        Added DEC-OSF1 to the list of supported environments
!
!-    Beta Version 2.1.0, 10-JAN-1996 (WLT)
!
!        Added PC-LAHEY to the list of supported environments.
!
!-    Beta Version 2.0.0, 16-JUN-1995 (WLT)(HAN)
!
!        Created master file from collection of machine dependent
!        routines.  Copyright notice added.
!
!-    Beta Version 1.0.0, 16-AUG-1994 (NJB)
!
!-&

! $ Index_Entries
!
!     invoke a text editor within a program
!
!-&

!
!     SPICELIB functions
!
      INTEGER               RTRIM
      LOGICAL               RETURN

!
!     Local parameters
!
      INTEGER               CMDLEN
      PARAMETER           ( CMDLEN = 255 )

!
!     Local variables
!
      CHARACTER*(CMDLEN)    LOCCMD


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'EDTCMD' )
      END IF

#if defined(SPICE_COMPILER_INTEL)
!     VAX:
!
!        Computer:         VAX 11/780
!        Operating System: VAX VMS 5.3
!        Fortran:          VAX FORTRAN 5.5

#else
#endif
!
!
#if defined(SPICE_COMPILER_INTEL)
!     PC-MS:
#else
!     SUN:
#endif
!
#if defined(SPICE_COMPILER_INTEL)
!        Computer:         PC
!        Operating System: Microsoft DOS 5.00
!        Fortran:          Microsoft Powerstation Fortran V1.0
#else
!        Computer:         Sun SPARCstation 2
!        Operating System: Sun OS 4.1.2
!        Fortran:          Sun FORTRAN 1.3.1
!
!     HP:
!
!        Computer:         HP 715/50
!        Operating System: HP-UX 9.01
!        Fortran:          HP-UX.09.00.24
!                          HP-UX FORTRAN/9000
!                             Series 700 B2408A.09.00
!                             Series 800 B2409B.09.00
!
!     NEXT:
!
!        Computer:         NeXT
!        Operating System: NeXtStep 3.0, 3.2
!        Fortran:          Absoft Fortran V3.2
!        NEXT (NeXT 3.0, Absoft Fortran 3.2):
!
!        Computer:         Alpha/OSF1
!        Operating System: OSF1 V3.2
!        Fortran:        : DEC Fortran Compiler Driver V3.5-053
#endif
!
!
!     Build the edit command to be passed to the system.
!
      LOCCMD = CMD
      CALL SUFFIX ( FILE, 1, LOCCMD )
#if defined(SPICE_COMPILER_INTEL)
#else

!
!     For safety, append a null to the command.  If the user has
!     linked to a C version of the "system" function, this may
!     save us much grief.
!
      CALL SUFFIX ( CHAR(0), 0, LOCCMD )
#endif

!
!     Invoke the editor.
!
      CALL EXESYS (   LOCCMD( :RTRIM(LOCCMD) )   )

      CALL CHKOUT ( 'EDTCMD' )
      RETURN
      END
