!$Procedure ZZGETFAT ( Get file architecture, type, and unit )

      SUBROUTINE ZZGETFAT ( FILE, ARCH, TYPE, NUMBER )

! $ Abstract
!
!     Determine the file architecture and file type of most SPICE kernel
!     files.
!
!     NOTE: This routine is currently for use ONLY with the SPACIT
!           and TOBIN utility programs. Use it at your own risk.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     KERNEL
!     UTILITY
!
! $ Declarations

      IMPLICIT NONE

      CHARACTER*(*)         FILE
      CHARACTER*(*)         ARCH
      CHARACTER*(*)         TYPE
      INTEGER               NUMBER

!
!     The record length should be big enough to hold 128 double
!     precision numbers.
!
!     For some environments, record length is measured in longwords,
!     since our records are unformatted, with two longwords per double
!     precision number. The value of RECL is 256.
!
!     Environment: VAX/VMS, VAX FORTRAN
!     Source:      Programming in VAX Fortran
!
!     Environment: Silicon Graphics IRIX OS, SGI FORTRAN 77
!     Source:      NAIF Program
!
!     Environment: DEC Alpha 3000/4000, OSF/1, DEC FORTRAN-77
!     Source:      NAIF Program
!
!     For the following environments, record length is measured in
!     characters (bytes) with eight characters per double precision
!     number. The value of RECL is 1024.
!
!     Environment: Sun, Sun FORTRAN
!     Source:      Sun Fortran Programmer's Guide
!
!     Environment: PC, MS FORTRAN
!     Source:      Microsoft Fortran Optimizing Compiler User's Guide
!
!     Environment: Macintosh, Language Systems FORTRAN
!     Source:      Language Systems FORTRAN Reference Manual,
!                  Version 1.2, page 12-7
!
!     Environment: PC, Lahey F77 EM/32 Version 4.0
!     Source:      Lahey F77 EM/32 Language Reference Manual,
!                  page 144
!
!     Environment: HP-UX 9000/750, FORTRAN/9000 Series 700 computers
!     Source:      FORTRAN/9000 Reference-Series 700 Computers,
!                  page 5-110
!
!     Environment: NeXT/Mach OS, Absoft Fortran
!     Source:      NAIF Program
!

      INTEGER               RECL
      PARAMETER           ( RECL   = 1024 )

! $ Brief_I/O
!
!      VARIABLE  I/O  DESCRIPTION
!      --------  ---  --------------------------------------------------
!      FILE       I   The name of a file to be examined.
!      ARCH       O   The architecture of the kernel file.
!      TYPE       O   The type of the kernel file.
!      NUMBER     O   The logical unit number for the open file FILE.
!
! $ Detailed_Input
!
!     FILE        is the name of a SPICE kernel file whose architecture
!                 and type are desired. This file must be closed when
!                 this routine is called.
!
! $ Detailed_Output
!
!     ARCH        is the file architecture of the SPICE kernel file
!                 specified be FILE. If the architecture cannot be
!                 determined or is not recognized the value '?' is
!                 returned.
!
!                 Architectures currently recognized are:
!
!                    DAF - The file is based on the DAF architecture.
!                    DAS - The file is based on the DAS architecture.
!                    XFR - The file is in a SPICE transfer file format.
!                    DEC - The file is an old SPICE decimal text file.
!                    ASC -- An ASCII text file.
!                    KPL -- Kernel Pool File (i.e., a text kernel)
!                    TXT -- An ASCII text file.
!                    TE1 -- Text E-Kernel type 1.
!                     ?  - The architecture could not be determined.
!
!                 This variable must be at least 3 characters long.
!
!     TYPE        is the type of the SPICE kernel file. If the type
!                 can not be determined the value '?' is returned.
!
!                 Kernel file types may be any sequence of at most four
!                 printing characters. NAIF has reserved for its use
!                 types which contain all upper case letters.
!
!                 A file type of 'PRE' means that the file is a
!                 pre-release file.
!
!                 This variable may be at most 4 characters long.
!
!     NUMBER      The logical unit number assigned to the file FILE
!                 when opened.  An integer, returned to the calling
!                 routine.
!
! $ Parameters
!
!     RECL        is the record length of a binary kernel file. Each
!                 record must be large enough to hold 128 double
!                 precision numbers. The units in which the record
!                 length must be specified vary from environment to
!                 environment. For example, VAX Fortran requires
!                 record lengths to be specified in longwords,
!                 where two longwords equal one double precision
!                 number.
!
! $ Exceptions
!
!      1)  If the inquire on the filename specified by FILE fails for
!          some reason, the error SPICE(INQUIREERROR) will be signaled.
!
!      2)  If the file specified by FILE is already open, the error
!          SPICE(FILECURRENTLYOPEN) will be signaled.
!
!      3)  If the file specified by FILE does not exist, the error
!          SPICE(NOSUCHFILE) will be signaled.
!
!      4)  If the attempt to open the file specified by FILE fails, the
!          error SPICE(FILEOPENFAILED) will be signaled.
!
!      5)  If all attempts to open the file specified by FILE fail, the
!          error SPICE(FILEOPENFAILED) will be signaled.
!
!      6)  If all attempts to read from the file specified be FILE
!          fail, the error SPICE(FILEREADFAILED) will be signaled.
!
! $ Files
!
!     The SPICE kernel file specified by FILE is opened and then
!     closed by this routine to determine its file architecture and
!     type. Names of open files should not be passed to this routine.
!
! $ Particulars
!
!     This subroutine is a support utility routine that determines the
!     architecture and type of a SPICE kernel file.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     This routine should only be called as part of spacit or tobin
!     by spat2b.
!
!     The file to be examined must be closed when this routine is
!     invoked.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     K.R. Gehringer  (JPL)
!     H.A. Neilan     (JPL)
!     W.L. Taber      (JPL)
!     E.D. Wright     (JPL)
!
! $ Version
!
!-    Beta Version 1.26.0, 28-NOV-2021 (BVS)
!
!        Updated for MAC-OSX-M1-64BIT-CLANG_C.
!
!-    Beta Version 1.25.0, 10-MAR-2014 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-INTEL.
!
!-    Beta Version 1.24.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-LINUX-64BIT-IFORT.
!
!-    Beta Version 1.23.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-GFORTRAN.
!
!-    Beta Version 1.22.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GFORTRAN.
!
!-    Beta Version 1.21.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GCC_C.
!
!-    Beta Version 1.20.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL.
!
!-    Beta Version 1.19.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL-CC_C.
!
!-    Beta Version 1.18.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
!
!-    Beta Version 1.17.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
!
!-    Beta Version 1.16.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-WINDOWS-64BIT-IFORT.
!
!-    Beta Version 1.15.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-LINUX-64BIT-GFORTRAN.
!
!-    Beta Version 1.14.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-64BIT-MS_C.
!
!-    Beta Version 1.13.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-INTEL_C.
!
!-    Beta Version 1.12.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-IFORT.
!
!-    Beta Version 1.11.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-GFORTRAN.
!
!-    Beta Version 1.10.0, 18-MAR-2009 (BVS)
!
!        Updated for PC-LINUX-GFORTRAN.
!
!-    Beta Version 1.9.0, 18-MAR-2009 (BVS)
!
!        Updated for MAC-OSX-GFORTRAN.
!
!-    Beta Version 1.8.0, 19-FEB-2008 (BVS)
!
!        Updated for PC-LINUX-IFORT.
!
!-    Beta Version 1.7.0, 14-NOV-2006 (BVS)
!
!        Updated for PC-LINUX-64BIT-GCC_C.
!
!-    Beta Version 1.6.0, 14-NOV-2006 (BVS)
!
!        Updated for MAC-OSX-INTEL_C.
!
!-    Beta Version 1.5.0, 14-NOV-2006 (BVS)
!
!        Updated for MAC-OSX-IFORT.
!
!-    Beta Version 1.4.0, 14-NOV-2006 (BVS)
!
!        Updated for PC-WINDOWS-IFORT.
!
!-    Beta Version 1.3.0, 26-OCT-2005 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-GCC_C.
!
!-    Beta Version 1.2.0, 03-JAN-2005 (BVS)
!
!        Updated for PC-CYGWIN_C.
!
!-    Beta Version 1.1.0, 03-JAN-2005 (BVS)
!
!        Updated for PC-CYGWIN.
!
!-    Beta Version 1.0.3, 17-JUL-2002 (BVS)
!
!        Added MAC-OSX environments.
!
!-    Beta Version 1.0.2, 08-OCT-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given.  New
!        environments are WIN-NT
!
!-    Beta Version 1.0.1, 21-SEP-1999 (NJB)
!
!        CSPICE environments were added.  Some typos were corrected.
!
!-    Beta Version 1.0.0, 19-MAR-1999 (EDW)
!
!        This routine is a modification of the GETFAT routine.
!        Both have the same basic functionality, but this routine
!        will ignore all data until a known NAIF file identifier
!        is found.  The derivation of file type and architecture
!        proceeds as in GETFAT.  Note:  the file is not closed
!        on exit.
!
!        The logic for the case architecture = DAF, type = unknown, '?',
!        has been removed.
!
!-&

!
! $ Index_Entries
!
!     determine the architecture and type of a kernel file
!
!-&

!
!     SPICELIB functions
!
      LOGICAL               RETURN

!
!     Local parameters
!
!
!     The following parameters point to the various slots in the
!     integer portion of the DAF descriptor where the values are
!     located.
!
      INTEGER               BODID
      PARAMETER           ( BODID  =  1 )

      INTEGER               CENID
      PARAMETER           ( CENID  = BODID  + 1 )

      INTEGER               SPKFRM
      PARAMETER           ( SPKFRM = CENID  + 1 )

      INTEGER               SPKTYP
      PARAMETER           ( SPKTYP = SPKFRM + 1 )

      INTEGER               START
      PARAMETER           ( START  = SPKTYP + 1 )

      INTEGER               FINISH
      PARAMETER           ( FINISH = START  + 1 )

      INTEGER               CKRATE
      PARAMETER           ( CKRATE = SPKTYP     )

!
!     These parameters give the number of integer and double precision
!     components of the descriptor for SPK and CK files.
!
      INTEGER               ND
      PARAMETER           ( ND = 2 )

      INTEGER               NI
      PARAMETER           ( NI = 6 )
!
!     The size of a summary.
!
      INTEGER               SUMSIZ
      PARAMETER           ( SUMSIZ = ND + ((NI+1)/2) )

!
!     Set the length of a SPICE kernel file ID word.
!
      INTEGER               IDLEN
      PARAMETER           ( IDLEN = 12 )
!
!     Set minimum and maximum values for the range of ASCII printing
!     characters.
!
      INTEGER               MINPCH
      PARAMETER           ( MINPCH = 32 )

      INTEGER               MAXPCH
      PARAMETER           ( MAXPCH = 126 )
!
!     Local Variables
!
      CHARACTER*(IDLEN)     IDWORD
      CHARACTER*(IDLEN)     TMPWRD


      INTEGER               I
      INTEGER               IOSTAT

      LOGICAL               DIROPN
      LOGICAL               SEQOPN
      LOGICAL               CHECK
      LOGICAL               EXIST
      LOGICAL               OPENED

!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN  ( 'ZZGETFAT' )
      END IF

!
!     Initialize the temporary storage variables that we use.
!
      IDWORD = ' '
      SEQOPN = .FALSE.
      CHECK  = .TRUE.


!
!     If the filename we got is blank, signal an error and return.
!
      IF ( FILE .EQ. ' ' ) THEN

         CALL SETMSG ( 'The file name is blank.' )
         CALL SIGERR ( 'SPICE(BLANKFILENAME)'    )
         CALL CHKOUT ( 'ZZGETFAT'                )
         RETURN

      END IF

!
!     We'll do a bit of inquiring before we try opening anything.
!
      INQUIRE ( FILE   = FILE, &
                EXIST  = EXIST, &
                IOSTAT = IOSTAT, &
                OPENED = OPENED )

!
!     Not too likely, but if the INQUIRE statement fails...
!
      IF ( IOSTAT .NE. 0 ) THEN

         CALL SETMSG ( 'IOSTAT error in INQUIRE statement. IOSTAT '   // &
                       '= #.'                                          )
         CALL ERRINT ( '#', IOSTAT                                     )
         CALL SIGERR ( 'SPICE(INQUIREERROR)'                           )
         CALL CHKOUT ( 'ZZGETFAT'                                      )
         RETURN

      END IF
!
!     Note: the following two tests MUST be performed in the order in
!           which they appear, since in some environments files that do
!           not exist are considered to be open.
!
!     By calling this routine, the user implies that the file exists.
!
      IF ( .NOT. EXIST ) THEN

         CALL SETMSG ( 'The kernel file ''#'' does not exist.'         )
         CALL ERRCH  ( '#', FILE                                       )
         CALL SIGERR ( 'SPICE(NOSUCHFILE)'                             )
         CALL CHKOUT ( 'ZZGETFAT'                                      )
         RETURN

      END IF

!
!     This routine should not be called if the file is already open.
!
      IF ( OPENED ) THEN

         CALL SETMSG ( 'The kernel file ''#'' is already open.'        )
         CALL ERRCH  ( '#', FILE                                       )
         CALL SIGERR ( 'SPICE(FILECURRENTLYOPEN)'                      )
         CALL CHKOUT ( 'ZZGETFAT'                                      )
         RETURN

      END IF

!
!     Open the file with a record length of RECL (the length of the
!     DAF and DAS records). We assume, for now, that opening the file as
!     a direct access file will work.
!

      DIROPN = .TRUE.

      CALL GETLUN ( NUMBER )

      OPEN ( UNIT               = NUMBER, &
             FILE               = FILE, &
             ACCESS             = 'DIRECT', &
             RECL               = RECL, &
             STATUS             = 'OLD', &
             IOSTAT             = IOSTAT    )

!
!     If we had trouble opening the file, try opening it as a sequential
!     file.
!
      IF ( IOSTAT .NE. 0 ) THEN

         DIROPN = .FALSE.

         OPEN ( UNIT            = NUMBER, &
                FILE            = FILE, &
                ACCESS          = 'SEQUENTIAL', &
                STATUS          = 'OLD', &
                IOSTAT          = IOSTAT    )
!
!        If we still have problems opening the file, we don't have a
!        clue about the file architecture and type.
!
         IF ( IOSTAT .NE. 0 ) THEN

            ARCH = '?'
            TYPE = '?'
            CALL SETMSG ( 'Attempt to open the file ''#'' failed.'  // &
                       ' IOSTAT = #.'                                )
            CALL ERRCH  ( '#', FILE                                  )
            CALL ERRINT ( '#', IOSTAT                                )
            CALL SIGERR ( 'SPICE(FILEOPENFAILED)'                    )
            CALL CHKOUT ( 'ZZGETFAT'                                 )
            RETURN

         END IF

      END IF

!
!     We opened the file successfully, so let's try to read from the
!     file. We need to be sure to use the correct form of the read
!     statement, depending on whether the file was opened with direct
!     access or sequential access.
!
      IF ( DIROPN ) THEN

         READ ( NUMBER, REC=1, IOSTAT=IOSTAT ) TMPWRD
!
!        If we couldn't read from the file as a direct access file with
!        a fixed record length, then try to open the file as a
!        sequential file and read from it.
!
         IF ( IOSTAT .EQ. 0 ) THEN

            SEQOPN = .TRUE.
            DIROPN = .FALSE.

            CLOSE ( NUMBER )

            OPEN ( UNIT   = NUMBER, &
                   FILE   = FILE, &
                   ACCESS = 'SEQUENTIAL', &
                   STATUS = 'OLD', &
                   IOSTAT = IOSTAT    )
!
!           If we could not open the file, we don't have a clue about
!           the file architecture and type.
!
            IF ( IOSTAT .NE. 0 ) THEN

               ARCH = '?'
               TYPE = '?'
               CALL SETMSG ( 'Attempt to open the file ''#''' // &
                             ' failed. IOSTAT = #.'            )
               CALL ERRCH  ( '#', FILE                         )
               CALL ERRINT ( '#', IOSTAT                       )
               CALL SIGERR ( 'SPICE(FILEOPENFAILED)'           )
               CALL CHKOUT ( 'ZZGETFAT'                        )
               RETURN

            END IF
!
!           Try to read from the file.
!
            READ ( NUMBER, FMT='(A)', IOSTAT=IOSTAT ) TMPWRD

         END IF

      ELSE

         SEQOPN = .TRUE.
         READ ( NUMBER, FMT='(A)', IOSTAT=IOSTAT ) TMPWRD

      END IF

!
!     If we had an error while reading, we don't recognize this file.
!
      IF ( IOSTAT .NE. 0 ) THEN

         ARCH = '?'
         TYPE = '?'
         CLOSE       ( NUMBER                                      )
         CALL SETMSG ( 'Attempt to read from file ''#'' failed.'  // &
                       ' IOSTAT = #.'                              )
         CALL ERRCH  ( '#', FILE                                   )
         CALL ERRINT ( '#', IOSTAT                                 )
         CALL SIGERR ( 'SPICE(FILEREADFAILED)'                     )
         CALL CHKOUT ( 'ZZGETFAT'                                  )
         RETURN

      END IF


!
!     Loop until a known NAIF file ID word is found.
!
      DO WHILE ( CHECK )

!
!        At this point, we have a candidate for an ID word. To avoid
!        difficulties with Fortran I/O and other things, we will now
!        replace any non printing ASCII characters with blanks.
!
         DO I = 1, IDLEN
            IF (      ( ICHAR(TMPWRD(I:I)) .LT. MINPCH ) &
                 .OR. ( ICHAR(TMPWRD(I:I)) .GT. MAXPCH ) ) THEN
               TMPWRD(I:I) = ' '
            END IF
         END DO

!
!        Identify the architecture and type, if we can.
!
         CALL LJUST  ( TMPWRD, TMPWRD         )
         CALL UCASE  ( TMPWRD, TMPWRD         )
         CALL NEXTWD ( TMPWRD, IDWORD, TMPWRD )

         IF ( IDWORD .EQ. 'DAFETF' ) THEN
!
!           We have a DAF encoded transfer file.
!
            ARCH = 'XFR'
            TYPE = 'DAF'
            CHECK = .FALSE.

         ELSE IF ( IDWORD .EQ. 'DASETF') THEN
!
!           We have a DAS encoded transfer file.
!
            ARCH = 'XFR'
            TYPE = 'DAS'
            CHECK = .FALSE.

         ELSE IF ( IDWORD(1:10) .EQ. '''NAIF/DAF''' ) THEN
!
!           We have an old DAF decimal text file.
!
            ARCH = 'DEC'
            TYPE = 'DAF'
            CHECK = .FALSE.

         ELSE IF ( IDWORD(1:8) .EQ. 'NAIF/DAS' ) THEN
!
!           We have a pre release DAS binary file.
!
            ARCH = 'DAS'
            TYPE = 'PRE'
            CHECK = .FALSE.

         ELSE
!
!           Get the architecture and type from the ID word, if we can.
!
            CALL IDW2AT ( IDWORD(1:8), ARCH, TYPE )

            IF ( ( ARCH .EQ. 'DAF' ) .AND. ( TYPE .EQ. '?' ) ) THEN

               CHECK = .FALSE.

            ELSE

!
!              No identification on line.  Read another line.
!
               IF ( SEQOPN ) THEN
                  READ ( NUMBER, FMT='(A)', IOSTAT=IOSTAT ) TMPWRD
               ELSE
                  READ ( NUMBER, REC=1, IOSTAT=IOSTAT ) TMPWRD
               END IF

!
!              If IOSTAT is a negative value, we probably hit an
!              end-of-file.  Error out.
!
               IF ( IOSTAT .LT. 0 ) THEN

                  ARCH = '?'
                  TYPE = '?'
                  CLOSE       ( NUMBER )
                  CALL SETMSG ( 'Encountered end-of-file of # before ' &
                                // ' finding known SPICE ID word.'    )
                  CALL ERRCH  ( '#', FILE                             )
                  CALL SIGERR ( 'SPICE(ENDOFFILE)'               )
                  CALL CHKOUT ( 'ZZGETFAT'                            )
                  RETURN

               END IF


            END IF


         END IF

      END DO


      CALL CHKOUT ( 'ZZGETFAT' )
      RETURN

      END

