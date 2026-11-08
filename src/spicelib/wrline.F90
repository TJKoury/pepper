!$Procedure WRLINE ( Write Output Line to a Device )

      subroutine wrline ( device, line )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Write a character string to an output device.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ERROR
!     FILES
!     TEXT
!
! $ Declarations

      implicit none

      character(*)           device
      character(*)           line

      integer(ip)            filen
#if defined(SPICE_OS_WINDOWS) && defined(SPICE_COMPILER_INTEL)
      parameter            ( filen = 128 )
#else
      parameter            ( filen = 255 )
#endif

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     DEVICE     I   A string specifying an output device.
!     LINE       I   A line of text to be output.
!     FILEN      P   Maximum length of a file name.
!
! $ Detailed_Input
!
!     LINE     is a line of text to be written to the output
!              device specified by DEVICE.
!
!     DEVICE   is the output device to which the line of text
!              will be written.
!
!              Possible values and meanings of DEVICE are:
!
!                 a device name   This may be the name of a
!                                 file, or any other name that
!                                 is valid in a FORTRAN OPEN
!                                 statement. For example, on a
!                                 VAX, a logical name may be
!                                 used.
!
!                                 The device name must not
!                                 be any of the reserved strings
!                                 below.
!
!                 'SCREEN'        The output will go to the
!                                 terminal screen.
!
!                 'NULL'          The data will not be output.
!
!              'SCREEN' and 'NULL' can be written in mixed
!              case. For example, the following call will work:
!
!                 CALL WRLINE ( 'screEn', LINE )
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     FILEN    is the maximum length of a file name.
!
! $ Exceptions
!
!     This routine is a special case as far as error handling
!     is concerned because it is called to output error
!     messages resulting from errors detected by other routines.
!     In such a case, calling SIGERR would constitute recursion.
!     Therefore, this routine prints error messages rather
!     than signaling errors via SIGERR and setting the long
!     error message via SETMSG.
!
!     The following exceptional cases are treated as errors:
!
!     1)  SPICE(NOFREELOGICALUNIT) -- No logical unit number
!         is available to refer to the device.
!
!     2)  SPICE(FILEOPENFAILED) -- General file open error.
!
!     3)  SPICE(FILEWRITEFAILED) -- General file write error.
!
!     4)  SPICE(INQUIREFAILED) -- INQUIRE statement failed.
!
!     5)  Leading blanks in (non-blank) file names are not
!         significant. The file names
!
!             'MYFILE.DAT'
!             '   MYFILE.DAT'
!
!         are considered to name the same file.
!
!     6)  If different names that indicate the same file are supplied
!         to this routine on different calls, all output associated
!         with these calls WILL be written to the file. For example,
!         on a system where logical file names are supported, if
!         ALIAS is a logical name pointing to MYFILE, then the calls
!
!             CALL WRLINE ( 'MYFILE', 'This is the first line'  )
!             CALL WRLINE ( 'ALIAS',  'This is the second line' )
!
!         will place the lines of text
!
!              'This is the first line'
!              'This is the second line'
!
!         in MYFILE. See $Restrictions for more information on use
!         of logical names on VAX systems.
!
! $ Files
!
!     If DEVICE specifies a device other than 'SCREEN' or 'NULL',
!     that device is opened (if it's not already open) as a NEW,
!     SEQUENTIAL, FORMATTED file. The logical unit used is
!     determined at run time.
!
! $ Particulars
!
!     If the output device is a file that is not open, the file will
!     be opened (if possible) as a NEW, sequential, formatted file,
!     and the line of text will be written to the file. If the file
!     is already opened as a sequential, formatted file, the line of
!     text will be written to the file.
!
!     Use the entry point CLLINE to close files opened by WRLINE.
!
! $ Examples
!
!     1)  Write a message to the screen:
!
!                CALL WRLINE ( 'SCREEN', 'Here''s a message.' )
!
!         The text
!
!                Here's a message.
!
!         will be written to the screen.
!
!     2)  Write out all of the elements of a character string array
!         to a file.
!
!                CHARACTER*(80)          STRING ( ASIZE )
!                             .
!                             .
!                             .
!                DO I = 1, ASIZE
!                   CALL WRLINE ( FILE, STRING(I) )
!                END DO
!
!     3)  Set DEVICE to NULL to suppress output:
!
!             C
!             C     Ask the user whether verbose program output is
!             C     desired. Set the output device accordingly.
!             C
!                   WRITE (*,*) 'Do you want to see test results '    //
!                  .            'on the screen?'
!                   READ  (*,FMT='(A)') VERBOS
!
!                   CALL LJUST ( VERBOS, VERBOS )
!                   CALL UCASE ( VERBOS, VERBOS )
!
!                   IF ( VERBOS(1:1) .EQ. 'Y' ) THEN
!                      DEVICE = 'SCREEN'
!                   ELSE
!                      DEVICE = 'NULL'
!                   ENDIF
!                             .
!                             .
!                             .
!             C
!             C     Output test results.
!             C
!                   CALL WRLINE ( DEVICE, STRING )
!                             .
!                             .
!                             .
!
! $ Restrictions
!
!     1)  File names must not exceed FILEN characters.
!
!     2)  On VAX systems, caution should be exercised when using
!         multiple logical names to point to the same file. Logical
!         name translation supporting execution of the Fortran
!         INQUIRE statement does not appear to work reliably in all
!         cases, which may lead this routine to believe that different
!         logical names indicate different files. The specific problem
!         that has been observed is that logical names that include
!         disk specifications are not always recognized as pointing
!         to the file they actually name.
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
!-    SPICELIB Version 4.27.0, 28-NOV-2021 (BVS)
!
!        Updated for MAC-OSX-M1-64BIT-CLANG_C.
!
!-    SPICELIB Version 4.26.0, 13-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 4.25.0, 10-MAR-2014 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-INTEL.
!
!-    SPICELIB Version 4.24.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-LINUX-64BIT-IFORT.
!
!-    SPICELIB Version 4.23.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-GFORTRAN.
!
!-    SPICELIB Version 4.22.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GFORTRAN.
!
!-    SPICELIB Version 4.21.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GCC_C.
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
!-    SPICELIB Version 4.0.3, 16-SEP-1999 (NJB)
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
!        References to the PC-LINUX environment were added. The
!        write format for the case where the output device is the
!        screen has been made system-dependent; list-directed output
!        format is now used for systems that require a leading carriage
!        control character; other systems use character format. The
!        write format for the case where the output device is a file
!        has been changed from list-directed to character.
!
!-    SPICELIB Version 3.0.0, 11-NOV-1993 (HAN)
!
!        Module was updated to include the value for FILEN
!        and the appropriate OPEN statement for the Silicon
!        Graphics, DEC Alpha-OSF/1, and NeXT platforms. The previous
!        value of 256 for Unix platforms was changed to 255.
!
!-    SPICELIB Version 2.1.0, 13-OCT-1992 (HAN)
!
!       Module was updated to include the value of FILEN for the
!       Hewlett Packard UX 9000/750 environment.
!
!       The code was also reformatted so that a utility program can
!       create the source file for a specific environment given a
!       master source file.
!
!-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
!
!       Comment section for permuted index source lines was added
!       following the header.
!
!-    SPICELIB Version 2.0.0, 26-MAR-1991 (NJB)
!
!       This routine now can write to files that have been opened
!       by other routines.
!
!       The limit imposed by this routine on the number of files it
!       can open has been removed.
!
!       The output file is now opened as a normal text file on
!       VAX systems.
!
!       Improper treatment of the case where DEVICE is blank was
!       remedied.
!
!       Unneeded variable declarations and references were removed.
!
!       Initialization of SAVED variables was added.
!
!       All occurrences of "PRINT *" have been replaced by
!       "WRITE (*,*)".
!
!       Calls to UCASE and LJUST replace in-line code that performed
!       these operations.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (NJB)
!
!-&

! $ Index_Entries
!
!     write output line to a device
!
!-&

! $ Revisions
!
!-    SPICELIB Version 4.0.0, 07-APR-1998 (NJB)
!
!        References to the PC-LINUX environment were added.
!
!        The write format for the case where the output device is the
!        screen has been made system-dependent; list-directed output
!        format is now used for systems that require a leading carriage
!        control character; other systems use character format. The
!        write format for the case where the output device is a file
!        has been changed from list-directed to character.
!
!-    SPICELIB Version 3.0.0, 11-NOV-1993 (HAN)
!
!        Module was updated to include the value for FILEN
!        and the appropriate OPEN statement for the Silicon
!        Graphics, DEC Alpha-OSF/1, and NeXT platforms. The previous
!        value of 256 for Unix platforms was changed to 255.
!
!-     SPICELIB Version 2.1.0, 13-OCT-1992 (HAN)
!
!        Module was updated to include the value of FILEN for the
!        Hewlett Packard UX 9000/750 environment.
!
!        The code was also reformatted so that a utility program can
!        create the source file for a specific environment given a
!        master source file.
!
!-    SPICELIB Version 2.0.0, 26-MAR-1991 (NJB)
!
!        1)  This routine now can write to files that have been opened
!            by other routines. WRLINE uses an INQUIRE statement to
!            determine whether the file indicated by DEVICE is open,
!            and if it is, WRLINE does not attempt to open it. This
!            allows use of WRLINE to feed error output into a log file
!            opened by another routine.
!
!            The header has been updated accordingly.
!
!            This fix also fixes a bug wherein this routine would treat
!            different character strings naming the same file as though
!            they indicated different files.
!
!        2)  The limit imposed by this routine on the number of files it
!            can open has been removed. The file database used in
!            previous versions of this routine is no longer used.
!
!        3)  On VAX systems, this routine now opens the output file
!            (when required to do so) as a normal text file.
!
!        4)  Improper treatment of the case where DEVICE is blank was
!            remedied. Any value of DEVICE that is not equal to
!            'SCREEN' or 'NULL' after being left-justified and
!            converted to upper case is considered to be a file name.
!
!        5)  Unneeded variable declarations and references were removed.
!            The arrays called STATUS and FILES are not needed.
!
!        6)  All instances if "PRINT *" have been replaced by
!            "WRITE (*,*)" because Language Systems Fortran on the
!            Macintosh interprets "PRINT *" in a non-standard manner.
!
!        7)  Use of the EXIST specifier was added to the INQUIRE
!            statement used to determine whether the file named by
!            DEVICE is open. This is a work-around for a rather
!            peculiar behavior of at least one version of Sun Fortran:
!            files that don't exist may be considered to be open, as
!            indicated by the OPENED specifier of the INQUIRE statement.
!
!        8)  One other thing: now that LJUST and UCASE are error-free,
!            WRLINE uses them; this simplifies the code.
!
!-    Beta Version 1.2.0, 27-FEB-1989 (NJB)
!
!        Call to GETLUN replaced by call to FNDLUN, which is error-free.
!        Call to IOERR replaced with in-line code to construct long
!        error message indicating file open failure. Arrangement of
!        declarations changed. Keywords added. FILEN declaration
!        moved to "declarations" section. $Parameters section added.
!
!-    Beta Version 1.1.0, 06-OCT-1988 (NJB)
!
!        Upper bound of written substring changed to prevent use of
!        invalid substring bound. Specifically, LASTNB ( LINE ) was
!        replaced by  MAX ( 1, LASTNB (LINE) ).  This upper bound
!        now used in the PRINT statement as well.
!
!-&

!
!     SPICELIB functions
!
      integer(ip)           ltrim
      integer(ip)           rtrim

!
!     Local variables
!
      character(240)        error
      character(11)         errstr
      character(filen)      tmpnam

      integer(ip)           iostat
      integer(ip)           unit

      logical               exists
      logical               opened

!
!     Executable Code:
!
      call ljust ( device, tmpnam )
      call ucase ( tmpnam, tmpnam )

!
!     TMPNAM is now left justified and is in upper case.
!
      if ( tmpnam == 'NULL' ) then

         return

      else if ( tmpnam == 'SCREEN' ) then

#if defined(SPICE_OS_WINDOWS) && defined(SPICE_COMPILER_INTEL)
         write ( *, *, iostat=iostat ) line(:rtrim(line))
#else
         write ( *, fmt='(A)', iostat=iostat ) line(:rtrim(line))
#endif
         return

      end if

!
!     Find out whether we'll need to open the file.
!
!     We use the EXIST inquiry specifier because files that don't exist
!     may be (possibly due to a Sun compiler bug) deemed to be OPEN by
!     Sun Fortran.
!
      inquire ( file         = device( ltrim(device): ), &
                opened       = opened, &
                exist        = exists, &
                number       = unit, &
                iostat       = iostat   )

      if ( iostat /= 0 ) then
!
!        This is weird.  How can an INQUIRE statement fail,
!        if the syntax is correct?  But just in case...
!
         write (*,*) 'SPICE(INQUIREFAILED)'
         write (*,*) 'WRLINE: File = ', device, 'IOSTAT = ', iostat
         return

      end if

      if ( .not. ( opened .and. exists )  ) then
!
!        We will need a free logical unit.  There is always the chance
!        that no units are available.
!
         call fndlun ( unit )

         if ( unit < 1 ) then

            write (*,*) 'SPICE(NOFREELOGICALUNIT)'
            write (*,*) ' '
            write (*,*) 'WRLINE: Maximum number of logical units '    // &
                        'that can be allocated by SPICELIB has '      // &
                        'already been reached'
            return

         end if

!
!        Okay, we have a unit. Open the file, and hope nothing
!        goes awry. (On the VAX, the qualifier
!
!           CARRIAGECONTROL = 'LIST'
!
!        may be inserted into the OPEN statement.)
!

         open ( file            = device( ltrim(device): ), &
                unit            = unit, &
                status          = 'NEW', &
                iostat          = iostat )

         if ( iostat /= 0 ) then

            write (*,*) 'SPICE(FILEOPENFAILED)'
            write (*,*) ' '

            error = 'WRLINE: An error occurred while attempting to open'

            call suffix ( device, 1, error )
            call suffix ( '.',    0, error )
            call suffix ( 'The value of IOSTAT returned was', 2, &
                          error  )
            call suffix ( ':',    0, error  )
            call intstr ( iostat,    errstr )
            call suffix ( errstr, 1, error  )
            call suffix ( '.',    0, error  )

            write (*,*) error
            return

         end if
!
!        Whew! We're ready to write to this file.
!
      end if

!
!     At this point, either we opened the file, or it was already
!     opened by somebody else.
!
!     This is the easy part. Write the next line to the file.
!
      write ( unit, fmt='(A)', iostat=iostat ) line(:rtrim(line))

!
!     Well, what happened? Any non-zero value for IOSTAT indicates
!     an error.
!
      if ( iostat /= 0 ) then

         error = 'WRLINE: An error occurred while attempting to '     // &
                 'WRITE to '

         call suffix ( device, 1, error  )
         call suffix ( '.',    0, error  )
         call suffix ( 'The value of IOSTAT returned was', 2, &
                       error  )
         call suffix ( ':',    0, error  )
         call intstr ( iostat,    errstr )
         call suffix ( errstr, 1, error  )
         call suffix ( '.',    0, error  )

         write (*,*) error

         return

      end if

      return

!$Procedure CLLINE ( Close a device )

      entry clline ( device )

! $ Abstract
!
!     Close a device.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ERROR
!     FILES
!     TEXT
!
! $ Declarations
!
!     IMPLICIT NONE
!
!     CHARACTER*(*)        DEVICE
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     DEVICE     I   Device to be closed.
!
! $ Detailed_Input
!
!     DEVICE   is the name of a device which is currently
!              opened for reading or writing.
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
!     This routine is called by SPICELIB error handling routines, so
!     it cannot use the normal SPICELIB error signaling mechanism.
!     Instead, it writes error messages to the screen if necessary.
!
!     1)  If the device indicated by DEVICE was not opened by WRLINE,
!         this routine closes it anyway.
!
!     2)  If the INQUIRE performed by this routine fails, an error
!         diagnosis is printed to the screen.
!
! $ Files
!
!     See argument DEVICE.
!
! $ Particulars
!
!     CLLINE closes a device that is currently open.
!
! $ Examples
!
!     1)  Write two lines to the file, SPUD.DAT (VAX file name
!         syntax), and then close the file.
!
!         CALL WRLINE ( 'SPUD.DAT', ' This is line 1 ' )
!         CALL WRLINE ( 'SPUD.DAT', ' This is line 2 ' )
!         CALL CLLINE ( 'SPUD.DAT' )
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
!-    SPICELIB Version 4.1.0, 03-JUN-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Fixed $Files
!        section and added entry to $Index_Entries.
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
!-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 2.0.0, 26-MAR-1991 (NJB)
!
!        All occurrences of "PRINT *" have been replaced by
!        "WRITE (*,*)".
!
!        Also, this routine now closes the device named by DEVICE
!        whether or not the device was opened by WRLINE.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (NJB)
!
!-&

! $ Index_Entries
!
!     close a device
!
!-&

! $ Revisions
!
!-    SPICELIB Version 2.0.0, 26-MAR-1991 (NJB)
!
!        All instances if "PRINT *" have been replaced by "WRITE (*,*)"
!        because Language Systems Fortran on the Macintosh interprets
!        "PRINT *" in a non-standard manner.
!
!        This routine no longer has to maintain the file database, since
!        WRLINE does not use it any more.
!
!        Also, this routine now closes the device named by DEVICE,
!        whether or not the device was opened by WRLINE.
!
!-    Beta Version 1.0.1, 08-NOV-1988 (NJB)
!
!        $Keywords added.
!
!-&

!
!     Find the unit connected to DEVICE.
!
      inquire ( file         = device( ltrim(device): ), &
                number       = unit, &
                iostat       = iostat   )

      if ( iostat /= 0 ) then
!
!        This is weird.  How can an INQUIRE statement fail,
!        if the syntax is correct?  But just in case...
!
         write (*,*) 'SPICE(INQUIREFAILED)'
         write (*,*) 'CLLINE:  File = ', device, 'IOSTAT = ', iostat
         return

      end if

      close ( unit )

      return
      end
