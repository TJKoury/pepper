!$Procedure  F_ZZASC2 ( Family of tests for ZZASCII )

      SUBROUTINE F_ZZASC2 ( OK )

      IMPLICIT NONE

! $ Abstract
!
!     Another test family for the ZZASCII routine. The tests confirm
!     correct responses when scanning differently terminated text files
!     by ZZASCII and ZZLDKER.
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
!     TBD.
!
! $ Keywords
!
!     TBD.
!
! $ Declarations
!
!     TBD.
!
! $ Brief_I/O
!
!     TBD.
!
! $ Detailed_Input
!
!     TBD.
!
! $ Detailed_Output
!
!     TBD.
!
! $ Parameters
!
!     TBD.
!
! $ Exceptions
!
!     TBD.
!
! $ Files
!
!     TBD.
!
! $ Particulars
!
!     TBD.
!
! $ Examples
!
!     TBD.
!
! $ Restrictions
!
!     TBD.
!
! $ Literature_References
!
!     TBD.
!
! $ Author_and_Institution
!
!     E.D. Wright      (JPL)
!     B.V. Semenov     (JPL)
!
! $ Version
!
!-    TSPICE Version 1.27.0, 28-NOV-2021 (BVS)
!
!        Updated for MAC-OSX-M1-64BIT-CLANG_C.
!
!-    TSPICE Version 1.26.0, 02-APR-2014 (BVS)
!
!        Changed PC-CYGWIN-GFORTRAN and PC-CYGWIN-64BIT-GFORTRAN
!        to be like LINUX GFORTRAN environments.
!
!-    TSPICE Version 1.25.0, 10-MAR-2014 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-INTEL.
!
!-    TSPICE Version 1.24.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-LINUX-64BIT-IFORT.
!
!-    TSPICE Version 1.23.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-GFORTRAN.
!
!-    TSPICE Version 1.22.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GFORTRAN.
!
!-    TSPICE Version 1.21.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GCC_C.
!
!-    TSPICE Version 1.20.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL.
!
!-    TSPICE Version 1.19.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL-CC_C.
!
!-    TSPICE Version 1.18.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
!
!-    TSPICE Version 1.17.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
!
!-    TSPICE Version 1.16.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-WINDOWS-64BIT-IFORT.
!
!-    TSPICE Version 1.15.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-LINUX-64BIT-GFORTRAN.
!
!-    TSPICE Version 1.14.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-64BIT-MS_C.
!
!-    TSPICE Version 1.13.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-INTEL_C.
!
!-    TSPICE Version 1.12.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-IFORT.
!
!-    TSPICE Version 1.11.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-GFORTRAN.
!
!-    TSPICE Version 1.10.0, 01-APR-2009 (BVS)(EDW)
!
!        Update for gfortran with tags:
!
!        PC-LINUX-GFORTRAN
!        MAC-OSX-GFORTRAN
!
!-    TSPICE Version 1.9.0, 19-FEB-2008 (BVS)
!
!        Updated for PC-LINUX-IFORT.
!
!-    TSPICE Version 1.8.0, 20-NOV-2006 (BVS)
!
!        Updated for PC-LINUX-64BIT-GCC_C.
!
!-    TSPICE Version 1.7.0, 14-NOV-2006 (BVS)
!
!        Updated for MAC-OSX-INTEL_C.
!
!-    TSPICE Version 1.6.0, 14-NOV-2006 (BVS)
!
!        Updated for MAC-OSX-IFORT.
!
!-    TSPICE Version 1.5.0, 14-NOV-2006 (BVS)
!
!        Updated for PC-WINDOWS-IFORT.
!
!-    TSPICE Version 1.4.0, 26-OCT-2005 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-GCC_C.
!
!-    TSPICE Version 1.3.0, 03-OCT-2005 (EDW)
!
!        Updated to block-out all code when processed for C
!        environments. Due to the non-native text kernel capability
!        in CSPICE, tspice_c should not perform zzascii tests - so
!        always return a success.
!
!-    TSPICE Version 1.2.0, 03-JAN-2005 (BVS)
!
!        Updated for PC-CYGWIN_C.
!
!-    TSPICE Version 1.1.0, 03-JAN-2005 (BVS)
!
!        Updated for PC-CYGWIN.
!
!-    TSPICE Version 1.0.0, 17-FEB-2004 (EDW) (BVS)
!
!-&

      LOGICAL               OK





!
!     Local parameters
!
      INTEGER               LNGLEN
      PARAMETER           ( LNGLEN = 128 )


      INTEGER               MIDLEN
      PARAMETER           ( MIDLEN = LNGLEN/2 )




      INTEGER               RECLEN
      PARAMETER           ( RECLEN = LNGLEN/2 )





      INTEGER               NFILES
      PARAMETER           ( NFILES = 12 )

      INTEGER               EOLTSZ
      PARAMETER           ( EOLTSZ = 5 )

!
!     Local Variables.
!
      INTEGER               IOSTAT
      INTEGER               NUMBER
      INTEGER               REC
      INTEGER               I
      INTEGER               J

      CHARACTER*(MIDLEN)    LINE   ( 3 )
      CHARACTER*(2)         TERMIN (3)
      CHARACTER*(MIDLEN)    HLINE

      CHARACTER*(12)        FILES ( NFILES )
      CHARACTER*(12)        MYFILE

      CHARACTER*(EOLTSZ)    OUTTRM
      CHARACTER*(LNGLEN)    WRKLIN
      CHARACTER*(2)         XSHRTL

      LOGICAL               MYZZEF( NFILES )
      CHARACTER*(32)        MYZZEM( NFILES )
      CHARACTER*(EOLTSZ)    MYTERM( NFILES )
      LOGICAL               MYFNEF( NFILES )
      CHARACTER*(32)        MYFNEM( NFILES )

!
!     SPICELIB functions.
!
      LOGICAL               EXISTS

!
!     Open the test family.
!
      CALL TOPEN ( 'F_ZZASC2' )

!
!     Define the data to write to test files. ID the file as an LSK
!     kernel, with a begin text block - thus we don't risk damaging any
!     kernel pool variable assignments. The third line contains the
!     data to write.
!
      LINE (1) = 'KPL/LSK                          ' // &
                 '                               '
      LINE (2) = '\begintext                       ' // &
                 '                               '
      LINE (3) = '123456789012345678901234567890123' // &
                 '4567890123456789012345678901234'

!
!     Initialize the file names. The first three files will be smaller
!     than the buffer that will be used to read them. The second three
!     will have exactly same size as the buffer. The third three will
!     be larger than the buffer. The last three will be larger than the
!     buffer but will contain mixed terminators.
!
      FILES(1)  = 'macs.ker'
      FILES(2)  = 'unxs.ker'
      FILES(3)  = 'doss.ker'
      FILES(4)  = 'macm.ker'
      FILES(5)  = 'unxm.ker'
      FILES(6)  = 'dosm.ker'
      FILES(7)  = 'macl.ker'
      FILES(8)  = 'unxl.ker'
      FILES(9)  = 'dosl.ker'
      FILES(10) = 'mxd1.ker'
      FILES(11) = 'mxd2.ker'
      FILES(12) = 'mxd3.ker'

!
!     Initialize terminators and tags.
!
      TERMIN(1) = CHAR(13)//' '
      TERMIN(2) = CHAR(10)//' '
      TERMIN(3) = CHAR(13)//CHAR(10)

!
!     Generate test files. The first loop ("I" loop) controls how many
!     records will be written to the test files (up to three, the 4th
!     is for the mixed files.)
!
      DO I = 1, 4

!
!        The second loop ("J" loop) goes over triplets of files and
!        corresponding terminators.
!
         DO J = 1, 3

!
!           If the test file exists, delete it.
!
            MYFILE =  FILES((I-1)*3+J)

            IF ( EXISTS( MYFILE ) ) THEN
               CALL KILFIL( MYFILE )
            END IF

!
!           Find a free logical unit and open the file as direct access.
!
            CALL GETLUN ( NUMBER )

            OPEN ( UNIT               = NUMBER, &
                   FILE               = MYFILE, &
                   ACCESS             = 'DIRECT', &
                   STATUS             = 'NEW'   , &
                   RECL               = RECLEN  , &
                   IOSTAT             = IOSTAT  )

!
!           The third loop ("REC" loop) pads "I" lines with the right
!           terminators (for the first three file triplets) or with
!           mixed terminators (for the last triplet) and writes them to
!           the file.
!
            DO REC = 1, I

!
!              Copy line to temporary buffer, add the terminator and
!              write it to the file. If REC is 4 or greater, use
!              incorrect terminator to get all flavors of
!              "contaminated" files.
!
               IF ( REC .LE. 3 ) THEN

                  HLINE = LINE(REC)
                  HLINE( MIDLEN-1:MIDLEN ) = TERMIN(J)
                  WRITE( NUMBER, REC=REC ) HLINE(1:MIDLEN)

               ELSE

                  HLINE = LINE(1)
                  IF      ( J .EQ. 1 ) THEN
                     HLINE( MIDLEN-1:MIDLEN ) = TERMIN(2)
                  ELSE IF ( J .EQ. 2 ) THEN
                     HLINE( MIDLEN-1:MIDLEN ) = TERMIN(3)
                  ELSE IF ( J .EQ. 3 ) THEN
                     HLINE( MIDLEN-1:MIDLEN ) = TERMIN(1)
                  END IF
                  WRITE( NUMBER, REC=1   ) HLINE(1:MIDLEN)

               END IF

            END DO

!
!           Close the file.
!
            CLOSE( NUMBER )

         END DO

      END DO




!
!     Ensure ZZASCII signals an error for work line lengths below the 3
!     character threshold.
!

      CALL TCASE   ( 'Check for error when using tiny buffer.' )
      CALL ZZASCII ( FILES(1), XSHRTL, .FALSE., OUTTRM )
      CALL CHCKXC  ( .TRUE., 'SPICE(STRINGTOOSHORT)', OK )


!
!     Ensure ZZASCII signals an error for a file that cannot be opened.
!

      CALL TCASE   ( 'Check for error when cannot open the file.' )
      CALL ZZASCII ( 'hakuna-matata', WRKLIN, .FALSE., OUTTRM )
      CALL CHCKXC  ( .TRUE., 'SPICE(FILEOPENFAIL)', OK )





!
!     Set ZZASCII exception flag and exception message for each file.
!     Since all files exist and buffer is big enough there should not
!     be any exceptions on any of the platforms.
!
      DO I = 1, NFILES
         MYZZEF( I ) = .FALSE.
         MYZZEM( I ) = ' '
      END DO

!
!     Set ZZASCII output terminator for each file. These are a
!     different story. We assign them as they should be detected
!     everywhere, but reassign some/all for the platforms that don't
!     "behave."
!
      MYTERM(  1 ) = '?'
      MYTERM(  2 ) = '?'
      MYTERM(  3 ) = '?'
      MYTERM(  4 ) = 'CR'
      MYTERM(  5 ) = 'LF'
      MYTERM(  6 ) = 'CR-LF'
      MYTERM(  7 ) = 'CR'
      MYTERM(  8 ) = 'LF'
      MYTERM(  9 ) = 'CR-LF'
      MYTERM( 10 ) = '?'
      MYTERM( 11 ) = '?'
      MYTERM( 12 ) = '?'




!
!     The IFORT, GFORTRAN, DIGITAL, and LAHEY seem to
!     generate IOSTAT=0 when read past the end of a file and,
!     therefore, proceed and detect terminators in short files.
!

      MYTERM(  1 ) = 'CR'
      MYTERM(  2 ) = 'LF'
      MYTERM(  3 ) = 'CR-LF'






!
!     Set FURNSH exception flag, exception message and loaded flag for
!     each file as if FURNSH wouldn't fail and then reset some of them
!     depending on the platform.
!
      DO I = 1, NFILES
         MYFNEF( I ) = .FALSE.
         MYFNEM( I ) = ' '
      END DO



!
#if defined(SPICE_COMPILER_INTEL)
!     FORTRANs on MS PCs can detect terminators in small and medium
!     files and, therefore, complain about them same way as for the
!     large files.
#else
!     IFORT on Macs and Linux and GFORTRAN everywhere can detect
!     terminators in small and medium files and, therefore, complain
!     about them same way as for the large files.
#endif
!
      MYFNEF(  1 ) = .TRUE.
#if defined(SPICE_COMPILER_INTEL)
      MYFNEF(  2 ) = .TRUE.
#else
      MYFNEF(  3 ) = .TRUE.
#endif
      MYFNEF(  4 ) = .TRUE.
#if defined(SPICE_COMPILER_INTEL)
      MYFNEF(  5 ) = .TRUE.
#else
      MYFNEF(  6 ) = .TRUE.
#endif
      MYFNEF(  7 ) = .TRUE.
#if defined(SPICE_COMPILER_INTEL)
      MYFNEF(  8 ) = .TRUE.
#else
      MYFNEF(  9 ) = .TRUE.
#endif
      MYFNEM(  1 ) = 'SPICE(INCOMPATIBLEEOL)'
#if defined(SPICE_COMPILER_INTEL)
      MYFNEM(  2 ) = 'SPICE(INCOMPATIBLEEOL)'
#else
      MYFNEM(  3 ) = 'SPICE(INCOMPATIBLEEOL)'
#endif
      MYFNEM(  4 ) = 'SPICE(INCOMPATIBLEEOL)'
#if defined(SPICE_COMPILER_INTEL)
      MYFNEM(  5 ) = 'SPICE(INCOMPATIBLEEOL)'
#else
      MYFNEM(  6 ) = 'SPICE(INCOMPATIBLEEOL)'
#endif
      MYFNEM(  7 ) = 'SPICE(INCOMPATIBLEEOL)'
#if defined(SPICE_COMPILER_INTEL)
      MYFNEM(  8 ) = 'SPICE(INCOMPATIBLEEOL)'
#else
      MYFNEM(  9 ) = 'SPICE(INCOMPATIBLEEOL)'
#endif


!
!     Now go over the files and call ZZASCII and FURNSH on each of them.
!
      DO I = 1, NFILES

!
!        Direct check by calling ZZASCII.
!
         CALL TCASE   ( 'Check for EOL terminator in ' // FILES( I ) )
         CALL ZZASCII ( FILES( I ), WRKLIN, .FALSE., OUTTRM )
         CALL CHCKXC  ( MYZZEF( I ), MYZZEM( I ), OK )
         CALL CHCKSC  ( 'Terminator', OUTTRM, '=', MYTERM( I ), OK )

!
!        Indirect check by calling FURNSH.
!
         CALL TCASE  ( 'Check what FURNSH does with ' // FILES( I ) )

         CALL FURNSH ( FILES( I ) )
         CALL CHCKXC ( MYFNEF( I ), MYFNEM( I ), OK )

         IF ( .NOT. MYFNEF( I ) ) THEN
            CALL UNLOAD ( FILES( I ) )
         END IF

      END DO

!
!     Clean-up: delete test files.
!
      DO I = 1, 12
         CALL KILFIL( FILES(I) )
      END DO

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )


      RETURN
      END
