!$Procedure TRCPKG ( Trace package )

      SUBROUTINE TRCPKG ( DEPTH, INDEX, MODULE, TRACE, NAME )

! $ Abstract
!
!     Maintain a trace of subroutine calls for error messages.
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
!     ERROR
!
! $ Keywords
!
!     ERROR
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               DEPTH
      INTEGER               INDEX
      CHARACTER*(*)         MODULE
      CHARACTER*(*)         TRACE
      CHARACTER*(*)         NAME

      INTEGER               FILEN
#if defined(SPICE_OS_WINDOWS) && defined(SPICE_COMPILER_INTEL)
      PARAMETER           ( FILEN  = 128 )
#else
      PARAMETER           ( FILEN  = 255 )
#endif

      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN =  32 )

      INTEGER               MAXMOD
      PARAMETER           ( MAXMOD = 100 )

! $ Brief_I/O
!
!     VARIABLE  I/O  ENTRY POINTS
!     --------  ---  --------------------------------------------------
!     DEPTH      O   TRCDEP
!     DEPTH      O   TRCMXD
!     INDEX      I   TRCNAM
!     NAME       O   TRCNAM
!     MODULE     I   CHKIN, CHKOUT
!     TRACE      O   QCKTRC
!     FILEN      P
!     NAMLEN     P
!     MAXMOD     P
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
!     FILEN    is the maximum length of a file name.
!
!     NAMLEN   is the maximum length of the significant
!              portion of a module name.
!
!     MAXMOD   is the maximum storage depth for names in the
!              traceback stack.
!
! $ Exceptions
!
!     1)  If TRCPKG is called directly, the error SPICE(BOGUSENTRY) is
!         signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The entry points declared in this routine are:
!
!       CHKIN
!       CHKOUT
!       TRCDEP
!       TRCMXD
!       TRCNAM
!       QCKTRC
!       FREEZE
!       TRCOFF
!
!     This routine serves as an umbrella that allows the entry
!     points to share data. TRCPKG should never be called directly.
!
!     See the subroutine ERRACT for descriptions of the error actions
!     and codes.
!
! $ Examples
!
!     See the entry points CHKIN, CHKOUT, TRCDEP, TRCMXD, TRCNAM,
!     QCKTRC, FREEZE, and TRCOFF for examples.
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
!     H.A. Neilan        (JPL)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 4.28.0, 28-NOV-2021 (BVS)
!
!        Updated for MAC-OSX-M1-64BIT-CLANG_C.
!
!-    SPICELIB Version 4.27.0, 03-JUN-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Added entry in
!        $Index_Entries.
!
!-    SPICELIB Version 4.26.0, 10-MAR-2014 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-INTEL.
!
!-    SPICELIB Version 4.25.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-LINUX-64BIT-IFORT.
!
!-    SPICELIB Version 4.24.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-GFORTRAN.
!
!-    SPICELIB Version 4.23.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GFORTRAN.
!
!-    SPICELIB Version 4.22.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GCC_C.
!
!-    SPICELIB Version 4.21.0, 29-JUL-2013 (BVS)
!
!        Changed logic in the CHKIN and CHKOUT entries to check if the
!        first character of the input value is not a space and, if so,
!        bypass the call to FRSTNB. This change speeds up the execution
!        by ~20%.
!
!-    SPICELIB Version 4.20.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL.
!
!-    SPICELIB Version 4.19.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL-CC_C.
!
!-    SPICELIB Version 4.18.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
!
!-    SPICELIB Version 4.17.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
!
!-    SPICELIB Version 4.16.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-WINDOWS-64BIT-IFORT.
!
!-    SPICELIB Version 4.15.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-LINUX-64BIT-GFORTRAN.
!
!-    SPICELIB Version 4.14.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-64BIT-MS_C.
!
!-    SPICELIB Version 4.13.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-INTEL_C.
!
!-    SPICELIB Version 4.12.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-IFORT.
!
!-    SPICELIB Version 4.11.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-GFORTRAN.
!
!-    SPICELIB Version 4.10.0, 18-MAR-2009 (BVS)
!
!        Updated for PC-LINUX-GFORTRAN.
!
!-    SPICELIB Version 4.9.0, 18-MAR-2009 (BVS)
!
!        Updated for MAC-OSX-GFORTRAN.
!
!-    SPICELIB Version 4.8.0, 19-FEB-2008 (BVS)
!
!        Updated for PC-LINUX-IFORT.
!
!-    SPICELIB Version 4.7.0, 14-NOV-2006 (BVS)
!
!        Updated for PC-LINUX-64BIT-GCC_C.
!
!-    SPICELIB Version 4.6.0, 14-NOV-2006 (BVS)
!
!        Updated for MAC-OSX-INTEL_C.
!
!-    SPICELIB Version 4.5.0, 14-NOV-2006 (BVS)
!
!        Updated for MAC-OSX-IFORT.
!
!-    SPICELIB Version 4.4.0, 14-NOV-2006 (BVS)
!
!        Updated for PC-WINDOWS-IFORT.
!
!-    SPICELIB Version 4.3.0, 26-OCT-2005 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-GCC_C.
!
!-    SPICELIB Version 4.2.0, 03-JAN-2005 (BVS)
!
!        Updated for PC-CYGWIN_C.
!
!-    SPICELIB Version 4.1.0, 03-JAN-2005 (BVS)
!
!        Updated for PC-CYGWIN.
!
!-    SPICELIB Version 4.0.5, 17-JUL-2002 (BVS)
!
!        Added MAC-OSX environments.
!
!-    SPICELIB Version 4.0.4, 08-OCT-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are WIN-NT
!
!-    SPICELIB Version 4.0.3, 24-SEP-1999 (NJB)
!
!        CSPICE environments were added. Some typos were corrected.
!
!-    SPICELIB Version 4.0.2, 28-JUL-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
!
!-    SPICELIB Version 4.0.1, 18-MAR-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. Previously,
!        environments such as SUN-SUNOS and SUN-SOLARIS were implied
!        by the environment label SUN.
!
!-    SPICELIB Version 4.0.0, 07-APR-1998 (NJB)
!
!        Module was updated for the PC-LINUX platform.
!
!        Bug fix: the previous version of entry point CHKOUT failed to
!        make a correct module name comparison when the input name
!        exceeded NAMLEN characters in length. Now only the initial
!        NAMLEN non-blank characters (at most) of the input name are
!        used in the comparison.
!
!-    SPICELIB Version 3.0.0, 12-MAR-1996 (KRG)
!
!        The structure of this routine has completely changed. A stack,
!        implemented as an array of character strings, is now used to
!        store subroutine names that use the CHKIN and CHKOUT entry
!        points. This change simplified the individual entry points as
!        well as speeding up the process of checking in and checking
!        out.
!
!        The error action mechanism has been changed as well. GETACT
!        now uses an integer code rather than a short character
!        string to represent the error action. The entry points affected
!        by this change are: TRCDEP, TRCNAM, QCKTRC.
!
!-    SPICELIB Version 2.0.0, 11-NOV-1993 (HAN)
!
!        Module was updated to include the values for FILEN and
!        NAMLEN for the Silicon Graphics, DEC Alpha-OSF/1, and
!        NeXT platforms. The previous value of 256 for Unix
!        platforms was changed to 255.
!
!-    SPICELIB Version 1.3.0, 23-OCT-1992 (NJB)
!
!        Bug fix made to routine QCKTRC:  a section of code which
!        itself is exercised only if a bug is present inserted the
!        wrong variable into an error message.
!
!-    SPICELIB Version 1.2.0, 12-OCT-1992 (HAN)
!
!        Module was updated to include the values of the parameters
!        for the Hewlett Packard UX 9000/750 environment.
!
!-    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.1.0, 18-JUN-1990 (NJB)
!
!        Added declarations for trace disabling. Re-organized
!        declarations. Updated comments.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (NJB)
!
!-&


! $ Index_Entries
!
!     error trace subsystem
!
!-&


! $ Revisions
!
!-    SPICELIB Version 4.0.0, 07-APR-1998 (NJB)
!
!        Module was updated for the PC-LINUX platform.
!
!        Bug fix: the previous version of entry point CHKOUT failed to
!        make a correct module name comparison when the input name
!        exceeded NAMLEN characters in length. Now only the initial
!        NAMLEN non-blank characters (at most) of the input name are
!        used in the comparison.
!
!-    SPICELIB Version 3.0.0, 12-MAR-1996 (KRG)
!
!        The structure of this routine has completely changed. A stack,
!        implemented as an array of character strings, is now used to
!        store subroutine names that use the CHKIN and CHKOUT entry
!        points. This change simplified the individual entry points as
!        well as speeding up the process of checking in and checking
!        out.
!
!        The error action mechanism has been changed as well. GETACT
!        now uses an integer code rather than a short character
!        string to represent the error action. The entry points affected
!        by this change are: TRCDEP, TRCNAM, QCKTRC.
!
!-    SPICELIB Version 2.0.0, 11-NOV-1993 (HAN)
!
!        Module was updated to include the values for FILEN and
!        NAMLEN for the Silicon Graphics, DEC Alpha-OSF/1, and
!        NeXT platforms. The previous value of 256 for Unix
!        platforms was changed to 255.
!
!-    SPICELIB Version 1.3.0, 23-OCT-1992 (NJB)
!
!        Bug fix made to routine QCKTRC:  a section of code which
!        itself is exercised only if a bug is present inserted the
!        wrong variable into an error message.
!
!-     SPICELIB Version 1.2.0, 12-OCT-1992 (HAN)
!
!        Module was updated to include the values of the parameters
!        for the Hewlett Packard UX 9000/750 environment.
!
!        The code was also reformatted so that a utility program can
!        create the source file for a specific environment given a
!        master source file.
!
!-    SPICELIB Version 1.1.0, 18-JUN-1990 (NJB)
!
!        Added declarations for trace disabling. Re-organized
!        declarations. Updated comments to reflect inclusion
!        of the new entry point TRCOFF. Also updated the header
!        to make the style more parallel to other SPICELIB
!        umbrella routines. Updated the description line and
!        abstract, in particular.
!
!-    Beta Version 1.0.1, 08-FEB-1989 (NJB)
!
!        Warnings added to discourage use of this routine.
!
!-&


!
!     SPICELIB functions:
!
      INTEGER               FRSTNB
      INTEGER               RTRIM

      LOGICAL               FAILED
!
!     Local parameters
!
!     This is the length for a local temporary string used to help
!     format error messages. It and the character string are only
!     present to avoid real or potential problems with pedantic
!     FORTRAN compilers. 80 characters should be more than sufficient
!     to contain a module name.
!
      INTEGER               TMPLEN
      PARAMETER           ( TMPLEN = 80 )
!
!     The integer mnemonic for the RETURN error action.
!
      INTEGER               IRETRN
      PARAMETER           ( IRETRN = 3 )

!
!     Local Variables:
!
      CHARACTER*(FILEN)     DEVICE
      CHARACTER*(11)        STRING
      CHARACTER*(TMPLEN)    TMPNAM

      INTEGER               ACTION
      INTEGER               FIRST
      INTEGER               I
      INTEGER               L

!
!     Saved variables:
!
      CHARACTER*(NAMLEN)    STACK (MAXMOD)
      CHARACTER*(NAMLEN)    FROZEN(MAXMOD)

      INTEGER               FRZCNT
      INTEGER               FRZOVR
      INTEGER               MAXDEP
      INTEGER               MODCNT
      INTEGER               OVRFLW

      LOGICAL               NOTRC

      SAVE                  NOTRC
      SAVE                  FROZEN
      SAVE                  FRZCNT
      SAVE                  FRZOVR
      SAVE                  MAXDEP
      SAVE                  MODCNT
      SAVE                  OVRFLW
      SAVE                  STACK

!
!     Initial values:
!
      DATA    NOTRC    / .FALSE. /
      DATA    FRZCNT   /  0     /
      DATA    FRZOVR   /  0     /
      DATA    MAXDEP   /  0     /
      DATA    MODCNT   /  0     /
      DATA    OVRFLW   /  0     /

!
!     Executable Code:
!
      CALL WRLINE ( 'SCREEN', 'SPICE(BOGUSENTRY)'                  )
      CALL WRLINE ( 'SCREEN', 'TRCPKG: You have called an entry ' &
      //            'that performs no run-time function. '         )

      RETURN


!$Procedure CHKIN ( Module Check In )

      ENTRY CHKIN ( MODULE )

! $ Abstract
!
!     Inform the SPICELIB error handling mechanism of entry into a
!     routine.
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
!     ERROR
!
! $ Keywords
!
!     ERROR
!
! $ Declarations
!
!     IMPLICIT NONE
!
!     CHARACTER*(*)          MODULE
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  ---------------------------------------------------
!     MODULE     I   The name of the calling routine.
!     FILEN      P   Maximum length of file name.
!
! $ Detailed_Input
!
!     MODULE   is the name of the routine calling CHKIN. The
!              named routine is supposed to be `checking in'
!              when it calls CHKIN; that is, the call should be
!              the first executable statement following the
!              reference to the function RETURN (which should be
!              the first executable statement).
!
!              Only the first NAMLEN non-blank characters in
!              a module name are stored for use in a traceback
!              by this subroutine.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     FILEN    is the maximum file name length that can be
!              accommodated by this routine.
!
! $ Exceptions
!
!     CHKIN does not signal errors; rather it writes error messages,
!     so as to avoid recursion.
!
!     1)  If the traceback storage area overflows, the short error
!         message SPICE(TRACEBACKOVERFLOW) is written to the error
!         output device.
!
!     2)  If the input argument MODULE is blank, the short error message
!         SPICE(BLANKMODULENAME) is written to the error output device.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine is part of the SPICELIB error handling mechanism.
!
!     Conceptually, the effect of this routine is to `push' the
!     supplied module name onto a stack. The routine CHKOUT performs
!     the inverse, or `pop', operation.
!
!     Every routine that participates in the traceback scheme should
!     have a call to CHKIN as the second executable statement. The
!     first executable statements should be:
!
!        IF ( RETURN() ) THEN
!           RETURN
!        ELSE
!           CALL CHKIN ( module )
!        END IF
!
!     Here module is the name of the routine in which this code appears.
!
!     The line of code preceding the END or any RETURN statement should
!     be
!
!         CALL CHKOUT ( module )
!
!
!     All SPICELIB routines should call CHKIN and CHKOUT, unless they
!     are classified as `error free'. Programs linked with SPICELIB
!     may also use CHKIN and CHKOUT.
!
!     Routines that don't call CHKIN and CHKOUT won't appear in the
!     traceback.
!
!     All routines that call CHKIN must also call CHKOUT, or else the
!     trace mechanism will become very confused.
!
!     It is possible to disable check-ins (and check-outs) by calling
!     the entry point TRCOFF. CHKIN and CHKOUT will return immediately
!     upon entry after TRCOFF has been called. It is not possible to
!     re-enable check-ins and check-outs after calling TRCOFF. Routines
!     that don't call CHKIN and CHKOUT won't appear in the traceback.
!
! $ Examples
!
!     See `Particulars' for an example of how to call this routine.
!
! $ Restrictions
!
!     1)  Routines that call this routine must call CHKOUT immediately
!         prior to any RETURN or END statement.
!
!     2)  Module names are assumed to have no embedded blanks.
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
!
! $ Version
!
!-    SPICELIB Version 4.2.0, 03-JUN-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 4.1.0, 29-JUL-2013 (BVS)
!
!        Changed logic to check if the first character of the input
!        value is not a space and, if so, bypass the call to FRSTNB.
!        This change speeds up the execution by ~20%.
!
!-    SPICELIB Version 4.0.5, 17-JUL-2002 (BVS)
!
!        Added MAC-OSX environments.
!
!-    SPICELIB Version 4.0.4, 08-OCT-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are WIN-NT
!
!-    SPICELIB Version 4.0.3, 24-SEP-1999 (NJB)
!
!        CSPICE environments were added. Some typos were corrected.
!
!-    SPICELIB Version 4.0.2, 28-JUL-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
!
!-    SPICELIB Version 4.0.1, 18-MAR-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. Previously,
!        environments such as SUN-SUNOS and SUN-SOLARIS were implied
!        by the environment label SUN.
!
!-    SPICELIB Version 4.0.0, 07-APR-1998 (NJB)
!
!        Module was updated for the PC-LINUX platform.
!
!        Bug fix: the previous version of entry point CHKOUT failed to
!        make a correct module name comparison when the input name
!        exceeded NAMLEN characters in length. Now only the initial
!        NAMLEN non-blank characters (at most) of the input name are
!        used in the comparison.
!
!-    SPICELIB Version 3.0.0, 12-MAR-1996 (KRG)
!
!        The structure of this routine has completely changed. A stack,
!        implemented as an array of character strings, is now used to
!        store subroutine names that use the CHKIN and CHKOUT entry
!        points. This change simplified the individual entry points as
!        well as speeding up the process of checking in and checking
!        out.
!
!        The short error dealing with embedded blanks has been removed,
!        since the new implementation is not hampered by Embedded
!        blanks.
!
!-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 2.0.0, 15-JUN-1990 (NJB)
!
!        Disabling of check-ins implemented. Many parts of the
!        header have be re-written. Weird spacing ameliorated.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (NJB)
!
!-&


! $ Index_Entries
!
!     module check in
!
!-&


! $ Revisions
!
!-    SPICELIB Version 4.0.0, 07-APR-1998 (NJB)
!
!        Module was updated for the PC-LINUX platform.
!
!        Bug fix: the previous version of entry point CHKOUT failed to
!        make a correct module name comparison when the input name
!        exceeded NAMLEN characters in length. Now only the initial
!        NAMLEN non-blank characters (at most) of the input name are
!        used in the comparison.
!
!-    SPICELIB Version 3.0.0, 12-MAR-1996 (KRG)
!
!        The structure of this routine has completely changed. A stack,
!        implemented as an array of character strings, is now used to
!        store subroutine names that use the CHKIN and CHKOUT entry
!        points. This change simplified the individual entry points as
!        well as speeding up the process of checking in and checking
!        out.
!
!        The short error dealing with embedded blanks has been removed,
!        since the new implementation is not hampered by Embedded
!        blanks.
!
!-    SPICELIB Version 2.0.0, 15-JUN-1990  (NJB)
!
!        Disabling of check-ins implemented. Many parts of the
!        header have be re-written. Weird spacing ameliorated.
!
!-    Beta Version 1.1.1, 10-FEB-1988  (NJB)
!
!        Parameter declarations documented. $Parameters section added,
!        and parameter declarations listed in `Brief I/O'.
!
!-    Beta Version 1.1.0, 27-OCT-1988  (NJB)
!
!        Cosmetic improvement to code. Condensed a continued
!        statement into one line.
!
!-&


!
!     Get out immediately if tracing is disabled.
!
      IF ( NOTRC ) THEN
         RETURN
      END IF
!
!     Get the position of the first and last non-blank characters in
!     input module name, and set the length of the module name.
!
      IF ( MODULE(1:1) .NE. ' ' ) THEN
         FIRST  =  1
      ELSE
         FIRST  =  FRSTNB ( MODULE )
      END IF

!
!     Check to see if the module name is blank.
!
      IF ( FIRST .GT. 0  ) THEN
!
!        If there is room for the name, place it at the top of the
!        stack. If not, increment the overflow counter and signal an
!        error.
!
         IF ( MODCNT .LT. MAXMOD ) THEN

            MODCNT        = MODCNT + 1
            STACK(MODCNT) = MODULE ( FIRST: )

         ELSE

            OVRFLW = OVRFLW + 1

            CALL GETDEV ( DEVICE )

            CALL WRLINE ( DEVICE,  'SPICE(TRACEBACKOVERFLOW)' )
            CALL WRLINE ( DEVICE,  'CHKIN:  The trace storage is ' &
            //            'completely full.  No further module ' &
            //            'names can be added.' )

         END IF
!
!        Keep track of the maximum depth encountered.
!
         IF ( ( MODCNT + OVRFLW ) .GT. MAXDEP ) THEN
            MAXDEP = MODCNT + OVRFLW
         END IF

      ELSE

         CALL GETDEV (  DEVICE  )

         CALL WRLINE (  DEVICE,  'SPICE(BLANKMODULENAME)'         )
         CALL WRLINE (  DEVICE,  'CHKIN:  An attempt to check' &
         //                      ' in was made without supplying' &
         //                      ' a module name.'                )

      END IF
!
!     We're done now, so return.
!
      RETURN


!$Procedure CHKOUT ( Module Check Out )

      ENTRY CHKOUT ( MODULE )

! $ Abstract
!
!     Inform the SPICELIB error handling mechanism of exit from a
!     routine.
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
!     ERROR
!
! $ Keywords
!
!     ERROR
!
! $ Declarations
!
!     IMPLICIT NONE
!
!     CHARACTER*(*)        MODULE
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     MODULE     I   The name of the calling routine.
!     NAMLEN     P   Maximum module name length.
!     FILEN      P   Maximum file name length.
!
! $ Detailed_Input
!
!     MODULE   is the name of the routine calling CHKOUT. The
!              named routine is supposed to be `checking out'
!              when it calls CHKOUT; that is, the call should be
!              the last executable statement preceding any exit
!              from the routine.
!
!              Only the first NAMLEN non-blank characters in
!              a module name are used when checking out.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     FILEN    is the maximum file name length that can be
!              accommodated by this routine.
!
!     NAMLEN   is the maximum module name length that can be
!              accommodated by this routine.
!
! $ Exceptions
!
!     CHKOUT does not signal errors; rather it writes error messages,
!     so as to avoid recursion.
!
!     1)  If the input module name MODULE does not match the name popped
!         from the trace stack, the short error message
!         SPICE(NAMESDONOTMATCH) is written to the error output device.
!
!     2)  If the trace stack is empty, the short error message
!         SPICE(TRACESTACKEMPTY) is written to the error output device.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine is part of the SPICELIB error handling mechanism.
!
!     Conceptually, the effect of this routine is to `pop' a module
!     name from a stack. The routine CHKIN performs the inverse, or
!     `push' operation.
!
!     Every routine that participates in the traceback scheme should
!     have a call to CHKIN as the second executable statement.
!     The first executable statements should be:
!
!        IF ( RETURN() ) THEN
!           RETURN
!        ELSE
!           CALL CHKIN ( module )
!        END IF
!
!     Here module is the name of the routine in which this code appears.
!
!     The line of code preceding the END or any RETURN statement
!     should be
!
!        CALL CHKOUT  ( module )
!
!     All SPICELIB routines should call CHKIN and CHKOUT, unless they
!     are classified as `error free'. Programs linked with SPICELIB
!     may also use CHKIN and CHKOUT.
!
!     Routines that don't call CHKIN and CHKOUT won't appear in the
!     traceback.
!
!     All routines that call CHKIN must also call CHKOUT, or else the
!     trace mechanism will become very confused.
!
!     It is possible to disable check-ins (and check-outs) by calling
!     the entry point TRCOFF. CHKIN and CHKOUT will return immediately
!     upon entry after TRCOFF has been called. It is not possible to
!     re-enable check-ins and check-outs after calling TRCOFF. Routines
!     that don't call CHKIN and CHKOUT won't appear in the traceback.
!
! $ Examples
!
!     1)  Call CHKOUT before a RETURN statement:
!
!            IF ( FAILED() ) THEN
!               CALL CHKOUT ( module )
!               RETURN
!            END IF
!
!
!     2)  Call CHKOUT before an END statement:
!
!            CALL CHKOUT ( module )
!            END
!
!
!     3)  Only ONE call to CHKOUT is needed here:
!
!            CALL CHKOUT ( module )
!            RETURN
!            END
!
! $ Restrictions
!
!     1)  Routines that call this routine must call CHKIN as the second
!         executable statement. (The first is a call to RETURN).
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
!
! $ Version
!
!-    SPICELIB Version 4.2.0, 03-JUN-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 4.1.0, 29-JUL-2013 (BVS)
!
!        Changed logic to check if the first character of the input
!        value is not a space and, if so, bypass the call to FRSTNB.
!        This change speeds up the execution by ~20%.
!
!-    SPICELIB Version 4.0.5, 17-JUL-2002 (BVS)
!
!        Added MAC-OSX environments.
!
!-    SPICELIB Version 4.0.4, 08-OCT-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are WIN-NT
!
!-    SPICELIB Version 4.0.3, 24-SEP-1999 (NJB)
!
!        CSPICE environments were added. Some typos were corrected.
!
!-    SPICELIB Version 4.0.2, 28-JUL-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
!
!-    SPICELIB Version 4.0.1, 18-MAR-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. Previously,
!        environments such as SUN-SUNOS and SUN-SOLARIS were implied
!        by the environment label SUN.
!
!-    SPICELIB Version 4.0.0, 30-OCT-1997 (NJB)
!
!        Module was updated for the PC-LINUX platform.
!
!        Bug fix: The previous version failed to make a correct
!        module name comparison when the input name exceeded NAMLEN
!        characters in length. Now only the initial NAMLEN non-blank
!        characters (at most) of the input name are used in the
!        comparison.
!
!-    SPICELIB Version 3.0.0, 12-MAR-1996 (KRG)
!
!        The structure of this routine has completely changed. A stack,
!        implemented as an array of character strings, is now used to
!        store subroutine names that use the CHKIN and CHKOUT entry
!        points. This change simplified the individual entry points as
!        well as speeding up the process of checking in and checking
!        out.
!
!-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 2.0.0, 15-JUN-1990 (NJB)
!
!        Disabling of check-ins implemented. Many parts of the
!        header have be re-written. Weird spacing ameliorated.
!        Removed a bug check. Short error messages made more
!        specific.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (NJB)
!
!-&


! $ Index_Entries
!
!     module check out
!
!-&


! $ Revisions
!
!-    SPICELIB Version 4.0.0, 30-OCT-1997 (NJB)
!
!        Module was updated for the PC-LINUX platform.
!
!        Bug fix: The previous version failed to make a correct
!        module name comparison when the input name exceeded NAMLEN
!        characters in length. Now only the initial NAMLEN non-blank
!        characters (at most) of the input name are used in the
!        comparison.
!
!-    SPICELIB Version 3.0.0, 12-MAR-1996 (KRG)
!
!        The structure of this routine has completely changed. A stack,
!        implemented as an array of character strings, is now used to
!        store subroutine names that use the CHKIN and CHKOUT entry
!        points. This change simplified the individual entry points as
!        well as speeding up the process of checking in and checking
!        out.
!
!-    SPICELIB Version 2.0.0, 15-JUN-1990 (NJB)
!
!        Disabling of check-ins implemented. Many parts of the
!        header have be re-written. Weird spacing ameliorated.
!        Removed a bug check. Short error messages changed from
!        SPICE(INVALIDCHECKOUT) to SPICE(NAMESDONOTMATCH) and
!        SPICE(TRACESTACKEMPTY).
!
!-    Beta Version 1.1.1, 10-FEB-1988 (NJB)
!
!        Parameter declarations documented. $Parameters section added,
!        and parameter declarations listed in `Brief I/O'.
!
!-    Beta Version 1.1.0, 27-OCT-1988 (NJB)
!
!        Cosmetic improvement to code. Removed a blank line
!        separating the first line of a statement from the next
!        continuation line, and condensed and re-organized
!        the statement. Note:  the precompiler failed to properly
!        convert the original statement into standard FORTRAN.
!
!-&


!
!     Get out immediately if tracing is disabled.
!
      IF ( NOTRC ) THEN
         RETURN
      END IF
!
!     Check to be sure we can remove a module name from the stack,
!     i.e., that we have not overflowed.
!
      IF ( OVRFLW .EQ. 0 ) THEN
!
!        We are not in overflow mode, compare the module name on
!        the top of the stack with the module name passed to us. If
!        they differ, it's an error. Regardless, we decrement the
!        module count.
!

         IF ( MODCNT .GT. 0 ) THEN
!
!           Make the comparison using at most NAMLEN characters of the
!           initial non-blank sub-string of MODULE.
!
            IF ( MODULE(1:1) .NE. ' ' ) THEN
               FIRST  =  1
            ELSE
               FIRST  =  FRSTNB ( MODULE )
            END IF

            L      =  MIN ( LEN(MODULE), FIRST+NAMLEN-1 )

            IF (  STACK(MODCNT)  .NE.  MODULE( FIRST : L )  ) THEN

               TMPNAM = MODULE( FIRST: )
               CALL GETDEV ( DEVICE )
               CALL WRLINE ( DEVICE, 'SPICE(NAMESDONOTMATCH)'    )
               CALL WRLINE ( DEVICE, 'CHKOUT:  Caller is ' &
               //            TMPNAM(:RTRIM(TMPNAM)) &
               //            '; popped name is ' &
               //            STACK(MODCNT)(:RTRIM(STACK(MODCNT)))//'.' )

            END IF

            MODCNT = MODCNT - 1

         ELSE

            CALL GETDEV ( DEVICE )

            CALL WRLINE ( DEVICE, 'SPICE(TRACESTACKEMPTY)' )
            CALL WRLINE ( DEVICE, 'CHKOUT: An attempt to check' &
            //                    ' out was made when no modules' &
            //                    ' were checked in.' )

         END IF

      ELSE
!
!        Overflow case: just decrement the overflow count.
!
         OVRFLW = OVRFLW - 1

      END IF
!
!     Return to the caller.
!
      RETURN


!$Procedure TRCDEP ( Traceback depth )

      ENTRY TRCDEP ( DEPTH )

! $ Abstract
!
!     Return the number of modules in the traceback representation.
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
!     ERROR
!
! $ Keywords
!
!     ERROR
!
! $ Declarations
!
!     IMPLICIT NONE
!
!     INTEGER               DEPTH
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  ---------------------------------------------------
!     DEPTH      O   The number of modules in the traceback.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     DEPTH    is the number of module names in the traceback
!              representation.
!
!              The module names represent modules in a call chain,
!              with the first name being the top-level module,
!              and the name with index DEPTH being the lowest
!              level module.
!
!              The meaning of the traceback depends on the state
!              of the error handling mechanism. There are two
!              cases:
!
!                 1. In 'RETURN' mode, when an error is
!                     signaled, the traceback at that point is
!                     saved.  TRCDEP, TRCNAM, and QCKTRC will
!                     return values pertaining to the saved
!                     traceback.
!
!                 2. In all other modes, the traceback represents
!                     the CURRENT call chain.  TRCDEP, TRCNAM,
!                     and QCKTRC will return values pertaining to
!                     the current trace representation.
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
!     This routine is part of the SPICELIB error handling mechanism.
!
! $ Examples
!
!     1)  You can use this routine, together with TRCNAM, to create a
!         traceback report. We might wish to create such a report when
!         we have detected an error condition (see FAILED).
!
!         In this example, we assume that the error has already been
!         detected, and that we wish to create a traceback report. We
!         assume the existence of two user-supplied routines:
!
!            USER_TRACE_FORMAT   --   creates a traceback report in the
!                                     format preferred by the user
!
!            USER_TRACE_INIT     --   indicates that a traceback report
!                                     is to be created; it also
!                                     indicates how many module names
!                                     will be in the report
!
!            C
!            C     Get the trace depth, and retrieve that number of
!            C     module names from the traceback representation.
!            C     Call USER_TRACE_INIT to indicate that a traceback
!            C     report is to be created containing `DEPTH'
!            C     number of module names. Input each of these names,
!            C     as they are retrieved, to USER_TRACE_FORMAT.
!            C
!
!                 CALL TRCDEP           ( DEPTH )
!
!                 CALL USER_TRACE_INIT  ( DEPTH )
!
!
!                 DO INDEX = 1, DEPTH
!
!                    CALL TRCNAM     ( INDEX, MODULE )
!
!                    CALL USER_TRACE_FORMAT ( MODULE )
!
!                 END DO
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
!
! $ Version
!
!-    SPICELIB Version 4.1.0, 03-JUN-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 4.0.5, 17-JUL-2002 (BVS)
!
!        Added MAC-OSX environments.
!
!-    SPICELIB Version 4.0.4, 08-OCT-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are WIN-NT
!
!-    SPICELIB Version 4.0.3, 24-SEP-1999 (NJB)
!
!        CSPICE environments were added. Some typos were corrected.
!
!-    SPICELIB Version 4.0.2, 28-JUL-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
!
!-    SPICELIB Version 4.0.1, 18-MAR-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. Previously,
!        environments such as SUN-SUNOS and SUN-SOLARIS were implied
!        by the environment label SUN.
!
!-    SPICELIB Version 2.0.0, 12-MAR-1996 (KRG)
!
!        The structure of this routine has completely changed. A stack,
!        implemented as an array of character strings, is now used to
!        store subroutine names that use the CHKIN and CHKOUT entry
!        points. This change simplified the individual entry points as
!        well as speeding up the process of checking in and checking
!        out.
!
!        The error action mechanism has been changed as well. GETACT
!        now uses an integer code rather than a short character
!        string to represent the error action. The entry points affected
!        by this change are: TRCDEP, TRCNAM, QCKTRC.
!
!-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.1, 15-JUN-1990 (NJB)
!
!        Some comments updated. Some cosmetic changes too.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (NJB)
!
!-&


! $ Index_Entries
!
!     traceback depth
!
!-&


! $ Revisions
!
!-    SPICELIB Version 2.0.0, 12-MAR-1996 (KRG)
!
!        The structure of this routine has completely changed. A stack,
!        implemented as an array of character strings, is now used to
!        store subroutine names that use the CHKIN and CHKOUT entry
!        points. This change simplified the individual entry points as
!        well as speeding up the process of checking in and checking
!        out.
!
!        The error action mechanism has been changed as well. GETACT
!        now uses an integer code rather than a short character
!        string to represent the error action. The entry points affected
!        by this change are: TRCDEP, TRCNAM, QCKTRC.
!
!-    SPICELIB Version 1.0.1, 15-JUN-1990  (NJB)
!
!        Some comments updated. Some cosmetic changes too.
!
!-&


!
!     Find the error handling mode.
!
      CALL GETACT ( ACTION )
!
!     If we're in 'RETURN' mode, and an error has occurred, we want to
!     use the frozen version of the traceback.  Otherwise, we want to
!     get the use the current module stack depth.
!
      IF  (   ( ACTION .EQ. IRETRN )  .AND.   FAILED()   )   THEN

         DEPTH = FRZCNT + FRZOVR

      ELSE

         DEPTH = MODCNT + OVRFLW

      END IF
!
!     Return to the caller.
!
      RETURN


!$Procedure TRCMXD ( Maximum traceback depth encountered. )

      ENTRY TRCMXD ( DEPTH )

! $ Abstract
!
!     Return the maximum number of modules encountered in the
!     traceback so far.
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
!     ERROR
!
! $ Keywords
!
!     ERROR
!
! $ Declarations
!
!     IMPLICIT NONE
!
!     INTEGER               DEPTH
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  ---------------------------------------------------
!     DEPTH      O   The maximum number of modules encountered.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     DEPTH    is the maximum number of module names encountered in the
!              traceback stack. This would be the longest call chain
!              that occurred during the run of a program.
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
!     This routine is part of the SPICELIB error handling mechanism.
!
! $ Examples
!
!     1)  You can use this routine to determine the length of the
!         longest sequence of subroutine calls in a program. Suppose
!         that you have a program, PROGRAM, that uses the SPICELIB
!         error handling with CHKIN and CHKOUT, and has three
!         subroutines, SUB_A, SUB_B, and SUB_C. The program and
!         subroutines have the following relationships:
!
!             PROGRAM calls SUB_A and SUB_C
!             SUB_C   calls SUB_B
!
!         If at the end of the program you were to call TRCMXD,
!
!            CALL TRCMXD ( MAXDEP )
!
!         to obtain the maximum depth reached, MAXDEP would have a
!         value of three (3), because the program checked in, SUB_C
!         checked in, and SUB_B checked in during the longest call
!         chain in the program.
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
!
! $ Version
!
!-    SPICELIB Version 4.1.0, 03-JUN-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 4.0.5, 17-JUL-2002 (BVS)
!
!        Added MAC-OSX environments.
!
!-    SPICELIB Version 4.0.4, 08-OCT-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are WIN-NT
!
!-    SPICELIB Version 4.0.3, 24-SEP-1999 (NJB)
!
!        CSPICE environments were added. Some typos were corrected.
!
!-    SPICELIB Version 4.0.2, 28-JUL-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
!
!-    SPICELIB Version 4.0.1, 18-MAR-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. Previously,
!        environments such as SUN-SUNOS and SUN-SOLARIS were implied
!        by the environment label SUN.
!
!-    SPICELIB Version 1.0.0, 12-MAR-1996 (KRG)
!
!-&


! $ Index_Entries
!
!     traceback maximum depth
!
!-&


!
!     It doesn't get any easier than this, simply set the maximum
!     depth and return.
!
      DEPTH = MAXDEP

      RETURN


!$Procedure TRCNAM ( Get Module Name from Traceback )

      ENTRY TRCNAM ( INDEX, NAME )

! $ Abstract
!
!     Return the name of the module having the specified position in
!     the trace representation. The first module to check in is at
!     position 1.
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
!     ERROR
!
! $ Keywords
!
!     ERROR
!
! $ Declarations
!
!     IMPLICIT NONE
!
!     INTEGER               INDEX
!     CHARACTER*(*)         NAME
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     INDEX      I   The position of the requested module name.
!     NAME       O   The name in the #INDEX position in the traceback.
!     FILEN      P   Maximum file name length.
!
! $ Detailed_Input
!
!     INDEX    is the position in the traceback of the requested
!              module name. The first module to check in is in
!              the first position; the last to check in the
!              position indicated by the argument, DEPTH,
!              returned by TRCDEP. Note that the first module to
!              check in is at the top of the traced call chain.
!
! $ Detailed_Output
!
!     NAME     is the name of the module in the position within
!              the traceback indicated by INDEX.
!
!              The meaning of the traceback depends on the state
!              of the error handling mechanism. There are two
!              cases:
!
!                 1. In 'RETURN' mode, when an error is
!                     signaled, the traceback at that point is
!                     saved.  TRCDEP, TRCNAM, and QCKTRC will
!                     return values pertaining to the saved
!                     traceback.
!
!                 2. In all other modes, the traceback represents
!                     the CURRENT call chain.  TRCDEP, TRCNAM,
!                     and QCKTRC will return values pertaining to
!                     the current trace representation.
!
! $ Parameters
!
!     FILEN    is the maximum file name length that can be
!              accommodated by this routine.
!
! $ Exceptions
!
!     Because this routine is below SIGERR in the calling hierarchy,
!     this routine can not call SIGERR in the event of an error.
!     Therefore, this routine outputs error messages, rather than
!     signaling errors.
!
!     1)  This routine detects the condition of INDEX being out of
!         range. The short error message set in that case is
!         SPICE(INVALIDINDEX).
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine is part of the SPICELIB error handling mechanism.
!
! $ Examples
!
!     1)  You can use this routine, together with TRCNAM, to create a
!         traceback report. We might wish to create such a report when
!         we have detected an error condition (see FAILED).
!
!         In this example, we assume that the error has already been
!         detected, and that we wish to create a traceback report. We
!         assume the existence of two user-supplied routines:
!
!            USER_TRACE_FORMAT   --   creates a traceback report in the
!                                     format preferred by the user
!
!            USER_TRACE_INIT     --   indicates that a traceback report
!                                     is to be created; it also
!                                     indicates how many module names
!                                     will be in the report
!
!            C
!            C     Get the trace depth, and retrieve that number of
!            C     module names from the traceback representation.
!            C     Call USER_TRACE_INIT to indicate that a traceback
!            C     report is to be created containing `DEPTH'
!            C     number of module names. Input each of these names,
!            C     as they are retrieved, to USER_TRACE_FORMAT.
!            C
!
!                 CALL TRCDEP           ( DEPTH )
!
!                 CALL USER_TRACE_INIT  ( DEPTH )
!
!
!                 DO INDEX = 1, DEPTH
!
!                    CALL TRCNAM     ( INDEX, MODULE )
!
!                    CALL USER_TRACE_FORMAT ( MODULE )
!
!                 END DO
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
!
! $ Version
!
!-    SPICELIB Version 4.1.0, 03-JUN-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 4.0.5, 17-JUL-2002 (BVS)
!
!        Added MAC-OSX environments.
!
!-    SPICELIB Version 4.0.4, 08-OCT-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are WIN-NT
!
!-    SPICELIB Version 4.0.3, 24-SEP-1999 (NJB)
!
!        CSPICE environments were added. Some typos were corrected.
!
!-    SPICELIB Version 4.0.2, 28-JUL-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
!
!-    SPICELIB Version 4.0.1, 18-MAR-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. Previously,
!        environments such as SUN-SUNOS and SUN-SOLARIS were implied
!        by the environment label SUN.
!
!-    SPICELIB Version 2.0.0, 12-MAR-1996 (KRG)
!
!        The structure of this routine has completely changed. A stack,
!        implemented as an array of character strings, is now used to
!        store subroutine names that use the CHKIN and CHKOUT entry
!        points. This change simplified the individual entry points as
!        well as speeding up the process of checking in and checking
!        out.
!
!        The exception:
!
!           2)  If INDEX is in range, but no module name is found
!               at the indicated location in the trace representation,
!               the error message SPICE(INVALIDINDEX) is set.
!
!        has been removed. The only way in which a module name cannot
!        be found for a specified index is if we have overflowed the
!        stack storage for module names, and in this case we return the
!        message '<Name Not Available>'.
!
!        The error action mechanism has been changed as well. GETACT
!        now uses an integer code rather than a short character
!        string to represent the error action. The entry points affected
!        by this change are: TRCDEP, TRCNAM, QCKTRC.
!
!-    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.1.0, 15-JUN-1990 (NJB)
!
!        Error messages streamlined. Some comments updated.
!        Some cosmetic changes too.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (NJB)
!
!-&


! $ Index_Entries
!
!     get module name from traceback
!
!-&


! $ Revisions
!
!-    SPICELIB Version 2.0.0, 12-MAR-1996 (KRG)
!
!        The structure of this routine has completely changed. A stack,
!        implemented as an array of character strings, is now used to
!        store subroutine names that use the CHKIN and CHKOUT entry
!        points. This change simplified the individual entry points as
!        well as speeding up the process of checking in and checking
!        out.
!
!        The exception:
!
!           2)  If INDEX is in range, but no module name is found
!               at the indicated location in the trace representation,
!               the error message SPICE(INVALIDINDEX) is set.
!
!        has been removed. The only way in which a module name cannot
!        be found for a specified index is if we have overflowed the
!        stack storage for module names, and in this case we return the
!        message '<Name Not Available>'.
!
!        The error action mechanism has been changed as well. GETACT
!        now uses an integer code rather than a short character
!        string to represent the error action. The entry points affected
!        by this change are: TRCDEP, TRCNAM, QCKTRC.
!
!-    SPICELIB Version 1.1.0, 15-JUN-1990  (NJB)
!
!        Error messages streamlined. Some comments updated.
!        Some cosmetic changes too.
!
!-    Beta Version 1.1.1, 10-FEB-1988  (NJB)
!
!        Parameter declarations documented. $Parameters section added,
!        and parameter declarations listed in `Brief I/O'.
!
!-    Beta Version 1.1.0, 27-OCT-1988  (NJB)
!
!        Added test for failure to remove name from trace
!        representation. If LOC equals 0 on return from
!        NTHWD, the error SPICE(INVALIDINDEX) is reported.
!        SIGERR is not called; that would be overly recursive.
!
!        Cosmetic changes to header and code were made. Indentation
!        of some header items was changed, and some blank lines
!        were removed from the code.
!
!-&


!
!     Get the error handling mode.
!
      CALL GETACT ( ACTION )
!
!     If we're in 'RETURN' mode, and an error has occurred, we want to
!     use the frozen version of the traceback.  Otherwise, we want to
!     get the module name from the current traceback.
!
      IF ( ( ACTION .EQ. IRETRN ) .AND. FAILED() ) THEN
!
!        Check the input index. It must be positive and less than the
!        current stack depth.
!
         IF ( ( INDEX .LE. 0 ) .OR. ( INDEX .GT. FRZCNT+FRZOVR ) ) THEN
!
!           Invalid index...we output the error messages directly
!           in this case:
!
            CALL  GETDEV ( DEVICE )
            CALL  WRLINE ( DEVICE,  'SPICE(INVALIDINDEX)' )
            CALL  INTSTR ( INDEX, STRING )
            CALL  WRLINE ( DEVICE, 'TRCNAM: An invalid index was '    // &
                           'input.  The value was: '                  // &
                           STRING( :RTRIM(STRING) )//'.'  )
            RETURN

         END IF
!
!        We're OK, so get the name or not available.
!
         IF ( INDEX .LE. MAXMOD )  THEN
            NAME = FROZEN(INDEX)
         ELSE
            NAME = '<Overflow No Name Available>'
         END IF

      ELSE
!
!        Otherwise, use current traceback:
!
!        Check the input index. It must be positive and less than the
!        current stack depth.
!
         IF ( ( INDEX .LE. 0 ) .OR. ( INDEX .GT. MODCNT+OVRFLW ) ) THEN
!
!           Invalid index...we output the error messages directly
!           in this case:
!
            CALL  GETDEV ( DEVICE )
            CALL  WRLINE ( DEVICE,  'SPICE(INVALIDINDEX)' )
            CALL  INTSTR ( INDEX, STRING )
            CALL  WRLINE ( DEVICE, 'TRCNAM: An invalid index was '    // &
                           'input.  The value was: '                  // &
                           STRING( :RTRIM(STRING) )//'.'  )
            RETURN

         END IF
!
!        We're OK, so get the name or name not available.
!
         IF ( INDEX .LE. MAXMOD )  THEN
            NAME = STACK(INDEX)
         ELSE
            NAME = '<Overflow No Name Available>'
         END IF

      END IF

      RETURN


!$Procedure QCKTRC ( Get Quick Traceback )

      ENTRY QCKTRC ( TRACE )

! $ Abstract
!
!     Return a string containing a traceback.
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
!     ERROR
!
! $ Keywords
!
!     ERROR
!
! $ Declarations
!
!     IMPLICIT NONE
!
!     CHARACTER*(*)         TRACE
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     TRACE      O   A traceback report.
!     NAMLEN     P   Maximum module name length.
!     FILEN      P   Maximum file name length.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     TRACE    is a list of module names, delimited by the
!              string, ' -->'.  An example would be
!
!                 'SPUD -->SPAM -->FOOBAR'.
!
!              In general, the meaning of the trace is as
!              follows:
!
!              The first name in the list is the name of the first
!              module to check in (that hasn't yet checked out).
!              The last name is the name of the module at the end
!              of the call chain; this is the last module that
!              checked in.
!
!              The meaning of the traceback depends on the state
!              of the error handling mechanism.  There are two
!              cases:
!
!                 1.  In 'RETURN' mode, when an error is
!                     signaled, the traceback at that point is
!                     saved.  TRCDEP, TRCNAM, and QCKTRC will
!                     return values pertaining to the saved
!                     traceback.
!
!                 2. In all other modes, the traceback represents
!                     the CURRENT call chain.  TRCDEP, TRCNAM,
!                     and QCKTRC will return values pertaining to
!                     the current trace representation.
!
!              Any module names exceeding NAMLEN characters in
!              length are truncated on the right.
!
! $ Parameters
!
!     FILEN    is the maximum file name length that can be
!              accommodated by this routine.
!
!     NAMLEN   is the maximum module name length that can be
!              accommodated by this routine.
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
!     This routine is part of the SPICELIB error handling mechanism.
!
! $ Examples
!
!     1)  Here's an example of how to use this routine:
!
!           C
!           C     We call RDTEXT and test for an error condition.
!           C     If an error occurred, we get the traceback and
!           C     long error message and output them using the
!           C     user-defined routine, USER_ERROR.
!           C
!
!                 CALL RDTEXT ( FILE, LINE, EOF )
!
!                 IF ( FAILED() ) THEN
!
!                    CALL QCKTRC     ( TRACE )
!                    CALL USER_ERROR ( TRACE )
!
!                    CALL GETMSG     ( 'LONG', MSG )
!                    CALL USER_ERROR (         MSG )
!
!                 END IF
!
! $ Restrictions
!
!     1)  It is assumed no module names exceed NAMLEN characters in
!         length.
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
!
! $ Version
!
!-    SPICELIB Version 4.1.0, 03-JUN-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 4.0.5, 17-JUL-2002 (BVS)
!
!        Added MAC-OSX environments.
!
!-    SPICELIB Version 4.0.4, 08-OCT-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are WIN-NT
!
!-    SPICELIB Version 4.0.3, 24-SEP-1999 (NJB)
!
!        CSPICE environments were added. Some typos were corrected.
!
!-    SPICELIB Version 4.0.2, 28-JUL-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
!
!-    SPICELIB Version 4.0.1, 18-MAR-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. Previously,
!        environments such as SUN-SUNOS and SUN-SOLARIS were implied
!        by the environment label SUN.
!
!-    SPICELIB Version 2.0.0, 12-MAR-1996 (KRG)
!
!        The structure of this routine has completely changed. A stack,
!        implemented as an array of character strings, is now used to
!        store subroutine names that use the CHKIN and CHKOUT entry
!        points. This change simplified the individual entry points as
!        well as speeding up the process of checking in and checking
!        out.
!
!        The error action mechanism has been changed as well. GETACT
!        now uses an integer code rather than a short character
!        string to represent the error action. The entry points affected
!        by this change are: TRCDEP, TRCNAM, QCKTRC.
!
!-    SPICELIB Version 1.2.0, 23-OCT-1992 (NJB)
!
!        Bug fix made to routine QCKTRC:  a section of code which
!        itself is exercised only if a bug is present inserted the
!        wrong variable into an error message.
!
!-    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.1.0, 15-JUN-1990 (NJB)
!
!        Error messages streamlined. Some comments updated.
!        Some cosmetic changes too.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (NJB)
!
!-&


! $ Index_Entries
!
!     get quick traceback
!
!-&


! $ Revisions
!
!-    SPICELIB Version 2.0.0, 12-MAR-1996 (KRG)
!
!        The structure of this routine has completely changed. A stack,
!        implemented as an array of character strings, is now used to
!        store subroutine names that use the CHKIN and CHKOUT entry
!        points. This change simplified the individual entry points as
!        well as speeding up the process of checking in and checking
!        out.
!
!        The error action mechanism has been changed as well. GETACT
!        now uses an integer code rather than a short character
!        string to represent the error action. The entry points affected
!        by this change are: TRCDEP, TRCNAM, QCKTRC.
!
!-    SPICELIB Version 1.2.0, 23-OCT-1992 (NJB)
!
!        Bug fix made to routine QCKTRC:  a section of code which
!        itself is exercised only if a bug is present inserted the
!        wrong variable into an error message. The variable in
!        question was the input argument INDEX; the correct variable
!        to insert in the message is the local variable POS.
!
!-    SPICELIB Version 1.1.0, 15-JUN-1990  (NJB)
!
!        Error messages streamlined. Some comments updated.
!        Some cosmetic changes too. Use of SUFFIX made more
!        rational.
!
!-    Beta Version 1.1.1, 10-FEB-1988  (NJB)
!
!        Parameter declarations documented. $Parameters section added,
!        and parameter declarations listed in `Brief I/O'.
!
!-    Beta Version 1.1.0, 06-OCT-1988  (NJB)
!
!        Added test for failure to remove name from trace
!        representation. If LOC equals 0 on return from
!        NTHWD, the error SPICE(INVALIDINDEX) is reported.
!        SIGERR is not called; that would be overly recursive.
!
!        Also, some cosmetic changes to code were made. Some
!        unnecessary continuation lines were removed.
!
!-&


!
!     Be sure that the output string is empty.
!
      TRACE = ' '
!
!     Get the error handling mode.
!
      CALL GETACT ( ACTION )
!
!     If we're in 'RETURN' mode, and an error has occurred, we want to
!     use the frozen version of the traceback.  Otherwise, we want to
!     use the current traceback.
!
      IF  (  ( ACTION .EQ. IRETRN )  .AND.  FAILED()  ) THEN

         DO I = 1, FRZCNT

            IF ( I .GT. 1 ) THEN
               CALL SUFFIX ( '-->',  1, TRACE )
               CALL SUFFIX ( FROZEN(I), 1, TRACE )
            ELSE
               CALL SUFFIX ( FROZEN(I), 0, TRACE )
            END IF

         END DO

         IF ( FRZOVR .GT. 0 ) THEN

            CALL SUFFIX ( '-->',   1, TRACE )
            IF ( FRZOVR .GT. 1 ) THEN
               CALL INTSTR ( FRZOVR, STRING )
               CALL SUFFIX ( '<', 1, TRACE )
               CALL SUFFIX ( STRING,  0, TRACE )
               CALL SUFFIX ( 'Names Overflowed>', 1, TRACE )
            ELSE
               CALL SUFFIX ( '<One Name Overflowed>', 1, TRACE )
            END IF

         END IF

      ELSE

         DO I = 1, MODCNT

            IF ( I .GT. 1 ) THEN
               CALL SUFFIX ( '-->',  1, TRACE )
               CALL SUFFIX ( STACK(I), 1, TRACE )
            ELSE
               CALL SUFFIX ( STACK(I), 0, TRACE )
            END IF

         END DO

         IF ( OVRFLW .GT. 0 ) THEN

            CALL SUFFIX ( '-->',  1, TRACE )
            IF ( OVRFLW .GT. 1 ) THEN
               CALL INTSTR ( OVRFLW, STRING )
               CALL SUFFIX ( '<', 1, TRACE )
               CALL SUFFIX ( STRING,  0, TRACE )
               CALL SUFFIX ( 'Names Overflowed>', 1, TRACE )
            ELSE
               CALL SUFFIX ( '<One Name Overflowed>', 1, TRACE )
            END IF

         END IF

      END IF

      RETURN


!$Procedure FREEZE ( Get frozen copy of traceback )

      ENTRY FREEZE

! $ Abstract
!
!     Make a copy of the current traceback. This copy is frozen, i.e.
!     unchanged, until the next call to FREEZE. DO NOT CALL THIS
!     ROUTINE.
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
!     ERROR
!
! $ Keywords
!
!     ERROR
!
! $ Declarations
!
!     IMPLICIT NONE
!
! $ Brief_I/O
!
!     None.
!
! $ Detailed_Input
!
!     None.
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
!     DO NOT CALL THIS ROUTINE.
!
!     When the error response action is 'RETURN', and an error is
!     signaled, a copy of the traceback is saved for later retrieval
!     by the application program.  This is called the `frozen' version
!     of the traceback. FREEZE is used to create this frozen version.
!
!     This routine is called by the SPICELIB routines SIGERR and RESET.
!
! $ Examples
!
!     1)
!         C
!         C     Create a frozen traceback:
!         C
!               CALL FREEZE
!
! $ Restrictions
!
!     1)  For SPICELIB error handling only.
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
!
! $ Version
!
!-    SPICELIB Version 4.1.0, 03-JUN-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Added entry to
!        $Index_Entries.
!
!-    SPICELIB Version 4.0.5, 17-JUL-2002 (BVS)
!
!        Added MAC-OSX environments.
!
!-    SPICELIB Version 4.0.4, 08-OCT-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are WIN-NT
!
!-    SPICELIB Version 4.0.3, 24-SEP-1999 (NJB)
!
!        CSPICE environments were added. Some typos were corrected.
!
!-    SPICELIB Version 4.0.2, 28-JUL-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
!
!-    SPICELIB Version 4.0.1, 18-MAR-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. Previously,
!        environments such as SUN-SUNOS and SUN-SOLARIS were implied
!        by the environment label SUN.
!
!-    SPICELIB Version 2.0.0, 12-MAR-1996 (KRG)
!
!        The structure of this routine has completely changed. A stack,
!        implemented as an array of character strings, is now used to
!        store subroutine names that use the CHKIN and CHKOUT entry
!        points. This change simplified the individual entry points as
!        well as speeding up the process of checking in and checking
!        out.
!
!-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.1, 15-JUN-1990 (NJB)
!
!        Some comments changed. Cosmetic changes too.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (NJB)
!
!-&


! $ Index_Entries
!
!     get frozen copy of traceback
!
!-&


! $ Revisions
!
!-    SPICELIB Version 2.0.0, 12-MAR-1996 (KRG)
!
!        The structure of this routine has completely changed. A stack,
!        implemented as an array of character strings, is now used to
!        store subroutine names that use the CHKIN and CHKOUT entry
!        points. This change simplified the individual entry points as
!        well as speeding up the process of checking in and checking
!        out.
!
!-    SPICELIB Version 1.0.1, 15-JUN-1990  (NJB)
!
!       Some comments changed. Cosmetic changes too.
!
!-    Beta Version 1.0.1, 08-FEB-1989 (NJB)
!
!        Warnings added to discourage use of this routine in
!        non-error-handling code.
!
!-&


!
!     Create a frozen version of the traceback. To do this, we move
!     the current traceback state into the freezer..
!

      FRZCNT = MODCNT
      FRZOVR = OVRFLW

      DO I = 1, MODCNT
         FROZEN(I) = STACK(I)
      END DO

      RETURN


!$Procedure TRCOFF ( Turn tracing off )

      ENTRY TRCOFF

! $ Abstract
!
!     Disable tracing.
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
!     ERROR
!
! $ Keywords
!
!     ERROR
!
! $ Declarations
!
!     IMPLICIT NONE
!
! $ Brief_I/O
!
!     None.
!
! $ Detailed_Input
!
!     None.
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
!     This routine disables tracing. Checking in or out does not modify
!     the current traceback any further after TRCOFF is called. The
!     routines TRCNAM, TRCDEP, and QCKTRC will return information
!     based on the traceback at the point where TRCOFF is called.
!
!     Once tracing has been disabled, it cannot be re-enabled.
!
!     Additionally, TRCOFF blanks out the existing trace, since the
!     trace will usually be invalid at the time an error is signaled.
!     The frozen copy of the trace, if there is one, is not modified.
!
! $ Examples
!
!     1)    C
!           C     Program initialization:
!           C
!                      .
!                      .
!                      .
!           C
!           C     We disable tracing to enhance speed:
!           C
!                 CALL TRCOFF
!           C
!           C     More initialization code:
!           C
!                      .
!                      .
!                      .
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
!
! $ Version
!
!-    SPICELIB Version 4.1.0, 03-JUN-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 4.0.5, 17-JUL-2002 (BVS)
!
!        Added MAC-OSX environments.
!
!-    SPICELIB Version 4.0.4, 08-OCT-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are WIN-NT
!
!-    SPICELIB Version 4.0.3, 24-SEP-1999 (NJB)
!
!        CSPICE environments were added. Some typos were corrected.
!
!-    SPICELIB Version 4.0.2, 28-JUL-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
!
!-    SPICELIB Version 4.0.1, 18-MAR-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. Previously,
!        environments such as SUN-SUNOS and SUN-SOLARIS were implied
!        by the environment label SUN.
!
!-    SPICELIB Version 2.0.0, 12-MAR-1996 (KRG)
!
!        The structure of this routine has completely changed. A stack,
!        implemented as an array of character strings, is now used to
!        store subroutine names that use the CHKIN and CHKOUT entry
!        points. This change simplified the individual entry points as
!        well as speeding up the process of checking in and checking
!        out.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 11-JUL-1990 (NJB)
!
!-&


! $ Index_Entries
!
!     turn tracing off
!
!-&


!
!     Indicate that tracing is disabled:
!
      NOTRC = .TRUE.

!
!     The stack depth becomes 0 (it will be referenced if TRCDEP is
!     called). The overflow count set to 0 as well, for consistency;
!     it will not be referenced again after this code is executed.
!
      MODCNT = 0
      OVRFLW = 0

      RETURN

      END
