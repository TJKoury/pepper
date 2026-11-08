!$Procedure      TSTCK3 (Create at test CK of type 3 and SCLK file)

      SUBROUTINE TSTCK3 ( CKNM,   SCLKNM, LOADCK, &
                          LOADSC, KEEPSC, HANDLE )

! $ Abstract
!
!     Create and if appropriate load a test type 03 C-kernel and
!     associated S-clock kernel file.
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
!      None.
!
! $ Keywords
!
!       TESTING
!
! $ Declarations

      IMPLICIT NONE
      CHARACTER*(*)         CKNM
      CHARACTER*(*)         SCLKNM
      LOGICAL               LOADCK
      LOGICAL               LOADSC
      LOGICAL               KEEPSC
      INTEGER               HANDLE


! $ Brief_I/O
!
!      VARIABLE  I/O  DESCRIPTION
!      --------  ---  -------------------------------------------------
!      CKNM       I   The name of the C-kernel to create
!      SCLKNM     I   The name of the S-clock kernel to create.
!      LOADCK     I   Load the C-kernel if TRUE
!      LOADSC     I   Load the S-clock kernel if TRUE
!      KEEPSC     I   Keep the S-clock kernel if TRUE, else delete it.
!      HANDLE     O   Handle of the c-kernel if it is loaded.
!
! $ Detailed_Input
!
!     CKNM        is the name of a C-kernel to create and load if
!                 LOADCK is set to TRUE.  If a C-kernel of the same
!                 name already exists it is deleted.
!
!     SCLKNM      is the name of an S-clock Kernel to create and load
!                 if LOADSC is set to TRUE.  If an S-clock kernel of
!                 the same name already exists, delete the existing
!                 kernel before creating this one.
!
!     LOADCK      is a logical that indicates whether or not the CK
!                 file should be loaded after it is created.  If it
!                 has the value TRUE the C-kernel is loaded after
!                 it is created.  Otherwise it is left un-opened.
!
!     LOADSC      is a logical that indicates whether or not the SCLK
!                 file should be loaded into the kernel pool.  If it
!                 has the value TRUE the SCLK file is loaded, otherwise
!                 it is left un-opened.
!
!     KEEPSC      is a logical that indicates whether or not the SCLK
!                 file should be deleted after it is loaded.  If KEEPSC
!                 is TRUE the file is not deleted.  If KEEPSC is FALSE
!                 the file is deleted after it is loaded.  Note that
!                 unless LOADSC is TRUE, the SCLK file is not deleted
!                 by this routine.  This routine deletes the SCLK
!                 kernel only if it LOADSC is TRUE and KEEPSC is FALSE.

! $ Detailed_Output
!
!     HANDLE      is the handle attached to the created C-kernel if
!                 the kernel is loaded because LOADCK has a value of
!                 TRUE.  Otherwise the value of HANDLE has no meaning.
!
! $ Parameters
!
!      None.
!
! $ Exceptions
!
!     None.
!
! $ Files
!
!      This routine creates two files a C-kernel with a three type 03
!      segments and an associated SCLK kernel that contains all of the
!      connection information about the CK file and its associated
!      ephemeris and S-clock.  See Particulars for more details.
!
! $ Particulars
!
!     This routine creates two files.
!
!     1) A C-kernel for the fictional objects with ID codes -9999,
!       -10000, and -10001
!
!     2) A SCLK kernel to be associated with the C-kernel.
!        In addition to coefficients for clock -9, this SCLK kernel
!        contains leapseconds kernel data and CK frame specifications
!        for three frames corresponding to the three objects. The
!        CK frame names and corresponding frame class IDs are
!
!           CK_-9999      -9999000
!           CK_-10000    -10000000
!           CK_-10001    -10000001
!
!     The C-kernel contains a single segment for each of the
!     fictional objects.  These segments give continuous attitude
!     over the time interval
!     from 1980 JAN 1, 00:00:00.000 (ET)
!     to   2011 SEP 9, 01:46:40.000 (ET)
!     (a span of exactly 1 billion seconds).
!
!
!     The base frames of the objects are
!
!     Object    Frame
!     -------   --------
!     -9999     Galactic
!     -10000    FK4
!     -10001    J2000
!
!     All three objects rotate  at a rate of 1 radian per 10 million
!     seconds. The axis of rotation changes every 100 million seconds.
!
!     At various epochs the axes of the objects are exactly aligned
!     with their associated reference frame.
!
!     Object     Aligned with reference frame at epoch
!     ------     -------------------------------------
!     -9999      Epoch of the J2000 frame
!     -10000     Epoch of J2000
!     -10001     Epoch of J2000
!
!     At the moment when the frames are aligned. The are rotating
!     around the direction (2, 1, 3) in their associated frames.
!
!     The C-kernel contains 606 attitude instances.
!
!     The attitude and angular velocity produced by the CK software
!     should very nearly duplicate the results returned by the test
!     routine TSTATD.
!
!     More specifically suppose we set up the arrays:
!
!        ID(1)     = -9999
!        ID(2)     = -10000
!        ID(3)     = -10001
!
!
!        FRAME(1)  = 'GALACTIC'
!        FRAME(2)  = 'FK4'
!        FRAME(3)  = 'J2000'
!
!
!     Then the two methods of getting ROT and AV below should
!     produce results that agree to nearly roundoff.
!
!     Method 1.
!
!        CALL SCE2T ( -9, ET, TICK )
!        CALL CKGPAV ( ID(I), TICK, 0.D0, FRAME(I), ROT, AV, OUT, FND )
!
!     Method 2.
!
!        CALL TSTATD ( ET , ROT, AV )
!
!
! $ Examples
!
!     This is intended to be used in those instances when you
!     need a well defined C-kernel whose attitude can be accurately
!     predicted in advance.
!
!     The routine TSTATD returns the continuous attitude and angular
!     velocity of the C-kernel for all time.  As such it provides
!     a convenient method for testing the CK software for an individual
!     segment.
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!      None.
!
! $ Author_and_Institution
!
!      W.L. Taber      (JPL)
!
! $ Version
!
!-    Test Utilities 2.0.0, 10-SEP-2020 (NJB)
!
!        This routine now adds CK frame specifications to the
!        SCLK kernel it creates.
!
!        Typo correction: changed FK5 to FK4.
!
!        Reformatted a few lines that extended into column 72.
!
!        Left Bill Taber's JPL phone number, which is still valid at
!        the date of this revision, in the source code.
!
!-    Test Utilities 1.1.1, 18-MAY-2010 (BVS)
!
!        Removed "C$" marker from text in the header.
!
!-    Test Utilities 1.1.0, 28-JUL-1999 (WLT)
!
!        Added code so that files are "registered" with FILREG.  In
!        this way they will automatically be deleted when a test
!        family is done.
!
!-    Test Utilities 1.0.0, 24-JAN-1995 (WLT)
!
!
!-&

! $ Index_Entries
!
!     Create test CK and SCLK files.
!
!-&

!
!     SPICELIB Functions
!
      INTEGER               RTRIM

!
!     Test Utility Functions
!
      CHARACTER*(16)        BEGDAT
      CHARACTER*(16)        BEGTXT

!
!     Inline functions.
!
      DOUBLE PRECISION      TICKS
      DOUBLE PRECISION      SECNDS
!
!     Local Variables.
!

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               NLINES
      PARAMETER           ( NLINES = 126 )

      INTEGER               MAXTIK
      PARAMETER           ( MAXTIK = 205 )

      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )

      CHARACTER*(WDSIZE)    REF
      CHARACTER*(WDSIZE)    SEGID

      CHARACTER*(LNSIZE)    SCLKBF ( NLINES )
      CHARACTER*(LNSIZE)    ERROR

      DOUBLE PRECISION      BEGTIM
      DOUBLE PRECISION      ENDTIM
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      TK
      DOUBLE PRECISION      ZEROPT
      DOUBLE PRECISION      MAXET

      DOUBLE PRECISION      ROT    ( 3, 3 )
      DOUBLE PRECISION      STARTS ( 1         )
      DOUBLE PRECISION      TICK   (    MAXTIK )
      DOUBLE PRECISION      QUATRN ( 4, MAXTIK )
      DOUBLE PRECISION      ANGVEL ( 3, MAXTIK )

      INTEGER               I
      INTEGER               INST
      INTEGER               J
      INTEGER               NINTS
      INTEGER               NTICKS
      INTEGER               UNIT
      INTEGER               R

      LOGICAL               AVFLG

!
!     Definitions of inline functions.
!
      TICKS ( ET ) =  ( ET - ZEROPT )*10000.0D0
      SECNDS( TK ) =    ZEROPT + (TK/10000.0D0)

!
!     The first order of business is to wipe out any existing
!     files with the same name.
!
      CALL KILFIL ( CKNM   )
      CALL KILFIL ( SCLKNM )
!
!     Fill up the buffer SCLKBF with the text that will make up
!     our test SCLK file.
!

      SCLKBF(  1) = 'TEST SPICE SCLK Kernel '
      SCLKBF(  2) = '------------------------------------------- ' &
      //            '------------- '
      SCLKBF(  3) = ' '
      SCLKBF(  4) = 'This file contains the data necessary for ' &
      //            'converting from '
      SCLKBF(  5) = 'ET to ticks for the fictional spacecraft ' &
      //            '-9999.  It is produced '
      SCLKBF(  6) = 'by the Test Utility routine TSTCK3. '
      SCLKBF(  7) = ' '
      SCLKBF(  8) = 'This SCLK kernel is intended to be used ' &
      //            'with the test CK file '
      SCLKBF(  9) = 'produced by that same routine.  The ' &
      //            'internal file name of the '
      SCLKBF( 10) = 'test C-Kernel is ZZ-TEST-CK-ZZ.  It ' &
      //            'contains a single type 03 '
      SCLKBF( 11) = 'segment. '
      SCLKBF( 12) = ' '
      SCLKBF( 13) = 'This fictional clock begins a 1 JAN 1980 ' &
      //            '00:00:00 ET and continues '
      SCLKBF( 14) = 'for 1 billion seconds.  (i.e. until 2011 ' &
      //            'SEP 9, 01:46:40.000 ET ). '
      SCLKBF( 15) = ' '
      SCLKBF( 16) = 'This is intended for test purposes only and ' &
      //            'can be easily rebuilt '
      SCLKBF( 17) = 'by calling the routine TSTCK3. '
      SCLKBF( 18) = ' '
      SCLKBF( 19) = 'If you have any questions about this file ' &
      //            'that these comments don''t '
      SCLKBF( 20) = 'answer, contact Bill Taber at NAIF. '
      SCLKBF( 21) = ' '
      SCLKBF( 22) = '(818) 354-4279 '
      SCLKBF( 23) = 'btaber@spice.jpl.nasa.gov '
      SCLKBF( 24) = ' '
      SCLKBF( 25) = ' '
      SCLKBF( 26) = ' '
      SCLKBF( 27) = 'Implementation notes '
      SCLKBF( 28) = '-------------------------------------------' &
      //            '------------- '
      SCLKBF( 29) = ' '
      SCLKBF( 30) = 'This SCLK file is constructed so that the ' &
      //            'valid SCLK strings '
      SCLKBF( 31) = 'are simply the number of TDB seconds that ' &
      //            'have passed '
      SCLKBF( 32) = 'since the Ephemeris epoch 1 Jan 1980 ' &
      //            '00:00:00 '
      SCLKBF( 33) = ' '
      SCLKBF( 34) = 'So that 1/ 288929292.82017  simply ' &
      //            'represents the epoch that occurs '
      SCLKBF( 35) = '288929292.82017 TDB seconds past the ET ' &
      //            'epoch 1 Jan 1980. '
      SCLKBF( 36) = ' '
      SCLKBF( 37) = 'For all time, the clock runs at the same ' &
      //            'rate as TDB. There is only '
      SCLKBF( 38) = 'one partition for this clock. '
      SCLKBF( 39) = ' '
      SCLKBF( 40) = 'You must load this file into the kernel ' &
      //            'pool before using any of the '
      SCLKBF( 41) = 'SPICELIB SCLK routines. The code fragment '
      SCLKBF( 42) = ' '
      SCLKBF( 43) = 'CALL LDPOOL ( < name of this file > ) '
      SCLKBF( 44) = ' '
      SCLKBF( 45) = 'performs this task. To convert between ET ' &
      //            'and UTC, you will also need '
      SCLKBF( 46) = 'to load a leapseconds kernel. The ' &
      //            'additional call to LDPOOL, '
      SCLKBF( 47) = ' '
      SCLKBF( 48) = 'CALL LDPOOL ( < name of your leapsecond ' &
      //            'file > ) '
      SCLKBF( 49) = ' '
      SCLKBF( 50) = 'will accomplish this. Note that you must ' &
      //            'supply the actual names of '
      SCLKBF( 51) = 'the files used on your system as arguments ' &
      //            'to LDPOOL. Because the file '
      SCLKBF( 52) = 'names are system dependent, we do not list ' &
      //            'them here. '
      SCLKBF( 53) = ' '
      SCLKBF( 54) = 'For more information, consult your SPICELIB ' &
      //            'required reading files. '
      SCLKBF( 55) = 'The following areas are covered: '
      SCLKBF( 56) = ' '
      SCLKBF( 57) = 'SCLK system                     SCLK ' &
      //            'required reading '
      SCLKBF( 58) = 'Time systems and conversion     TIME ' &
      //            'required reading '
      SCLKBF( 59) = 'Kernel pool                     KERNEL ' &
      //            'required reading '
      SCLKBF( 60) = ' '
      SCLKBF( 61) = ' '
      SCLKBF( 62) = 'Kernel data '
      SCLKBF( 63) = '-------------------------------------------' &
      //            '------------- '
      SCLKBF( 64) = ' '
      SCLKBF( 65) = ' '
      SCLKBF( 66) = BEGDAT()
      SCLKBF( 67) = ' '
      SCLKBF( 68) = ' '
      SCLKBF( 69) = 'SCLK_KERNEL_ID                = ( ' &
      //            '@28-OCT-1994        ) '
      SCLKBF( 70) = ' '
      SCLKBF( 71) = 'SCLK_DATA_TYPE_9              = ( ' &
      //            '1 ) '
      SCLKBF( 72) = ' '
      SCLKBF( 73) = 'SCLK01_TIME_SYSTEM_9          = ( ' &
      //            '1 ) '
      SCLKBF( 74) = 'SCLK01_N_FIELDS_9             = ( ' &
      //            '2 ) '
      SCLKBF( 75) = 'SCLK01_MODULI_9               = ( ' &
      //            '1000000000     10000 ) '
      SCLKBF( 76) = 'SCLK01_OFFSETS_9              = ( ' &
      //            '0         0 ) '
      SCLKBF( 77) = 'SCLK01_OUTPUT_DELIM_9         = ( ' &
      //            '1 ) '
      SCLKBF( 78) = ' '
      SCLKBF( 79) = 'SCLK_PARTITION_START_9        = ( ' &
      //            '0.0000000000000E+00 ) '
      SCLKBF( 80) = 'SCLK_PARTITION_END_9          = ( ' &
      //            '1.00000000E+14      ) '
      SCLKBF( 81) = 'SCLK01_COEFFICIENTS_9         = ( ' &
      //            '0.00000000E+00 '
      SCLKBF( 82) = '@01-JAN-1980-00:00:00.000 '
      SCLKBF( 83) = '1  ) '
      SCLKBF( 84) = ' '
      SCLKBF( 85) = ' '
      SCLKBF( 86) = 'DELTET/DELTA_T_A       =   32.184 '
      SCLKBF( 87) = 'DELTET/K               =    1.657D-3 '
      SCLKBF( 88) = 'DELTET/EB              =    1.671D-2 '
      SCLKBF( 89) = 'DELTET/M               = (  6.239996D0 ' &
      //            '1.99096871D-7 ) '
      SCLKBF( 90) = ' '
      SCLKBF( 91) = 'CK_-9999_SCLK          =   -9 '
      SCLKBF( 92) = 'CK_-9999_SPK           =   -9 '
      SCLKBF( 93) = ' '
      SCLKBF( 94) = 'CK_-10000_SCLK         =   -9 '
      SCLKBF( 95) = 'CK_-10000_SPK          =   -9 '
      SCLKBF( 96) = ' '
      SCLKBF( 97) = 'CK_-10001_SCLK         =   -9 '
      SCLKBF( 98) = 'CK_-10001_SPK          =   -9 '
      SCLKBF( 99 ) = ' '
      SCLKBF( 100 ) = BEGTXT()
      SCLKBF( 101 ) = ' '
      SCLKBF( 102 ) = '      Following are specifications of ' &
      //              'CK frames for each instrument.'
      SCLKBF( 103 ) = ' '
      SCLKBF( 104 ) = BEGDAT()

      SCLKBF( 105 ) = ' '
      SCLKBF( 106 ) = '      FRAME_CK_-9999          = -99990' &
      //              '00'
      SCLKBF( 107 ) = '      FRAME_-9999000_NAME     = ''CK_-' &
      //              '9999'''
      SCLKBF( 108 ) = '      FRAME_-9999000_CLASS    = 3'
      SCLKBF( 109 ) = '      FRAME_-9999000_CLASS_ID = -9999'
      SCLKBF( 110 ) = '      FRAME_-9999000_CENTER   = -9'
      SCLKBF( 111 ) = ' '
      SCLKBF( 112 ) = ' '
      SCLKBF( 113 ) = '      FRAME_CK_-10000          = -1000' &
      //              '0000'
      SCLKBF( 114 ) = '      FRAME_-10000000_NAME     = ''CK_' &
      //              '-10000'''
      SCLKBF( 115 ) = '      FRAME_-10000000_CLASS    = 3'
      SCLKBF( 116 ) = '      FRAME_-10000000_CLASS_ID = -1000' &
      //              '0'
      SCLKBF( 117 ) = '      FRAME_-10000000_CENTER   = -9'
      SCLKBF( 118 ) = ' '
      SCLKBF( 119 ) = ' '
      SCLKBF( 120 ) = '      FRAME_CK_-10001          = -1000' &
      //              '1000'
      SCLKBF( 121 ) = '      FRAME_-10001000_NAME     = ''CK_' &
      //              '-10001'''
      SCLKBF( 122 ) = '      FRAME_-10001000_CLASS    = 3'
      SCLKBF( 123 ) = '      FRAME_-10001000_CLASS_ID = -1000' &
      //              '1'
      SCLKBF( 124 ) = '      FRAME_-10001000_CENTER   = -9'
      SCLKBF( 125 ) = ' '
      SCLKBF( 126 ) = BEGTXT()


!     Create the SCLK kernel.
!
      CALL TXTOPN ( SCLKNM, UNIT )

      DO I = 1, NLINES

         R =     RTRIM ( SCLKBF(I) )
         WRITE (UNIT,*)  SCLKBF(I)(1:R)

      END DO

      CLOSE ( UNIT )

!
!     Next create the C-kernel. Recall the relationship between
!     ET and encoded SCLK ticks.  There are 10000 ticks/second.
!     The zero point of the clock is 1 Jan 1980 TDB.
!
      CALL TPARSE ( '1 Jan 1980', ZEROPT, ERROR )

      MAXET = ZEROPT + 1000000000.0D0
      ET    =  0.0D0
      J     = 0

      DO WHILE ( ET .GT. ZEROPT )

         J       = J + 1
         TICK(J) = TICKS(ET)

         J       = J + 1
         TICK(J) = TICK(J-1) - 1.0D0

         ET      = ET - 10000000.0D0

      END DO

      J        = J + 1
      TICK(J)  = 0.0D0


      ET = 10000000.0D0

      DO WHILE ( ET .LT. MAXET )

         J       = J + 1
         TICK(J) = TICKS(ET)

         J       = J + 1
         TICK(J) = TICK(J-1) - 1.0D0

         ET      = ET + 10000000.0D0

      END DO

      J       = J + 1
      TICK(J) = TICKS(MAXET)

      NTICKS = J
!
!     Sort the ticks.
!
      CALL SHELLD ( NTICKS, TICK )

      BEGTIM = TICK(1)
      ENDTIM = TICK(NTICKS)

      INST   = -9999
      REF    = 'GALACTIC'
      AVFLG  = .TRUE.
      SEGID  = 'Test Segment for object -9999'


      DO I = 1, NTICKS
         TK = TICK(I)
         ET = SECNDS ( TK )
         CALL TSTATD ( ET, ROT, ANGVEL(1,I) )
         CALL M2Q    (     ROT, QUATRN(1,I) )
      END DO


      NINTS     = 1
      STARTS(1) = 0.0D0

      CALL SPCOPN ( CKNM, 'Test C-kernel',  HANDLE )
      CALL CKW03  ( HANDLE, &
                    BEGTIM, ENDTIM, INST, REF, AVFLG, &
                    SEGID,  NTICKS, TICK, QUATRN, ANGVEL, &
                    NINTS,  STARTS )
!
!     Now create a second segment by simply taking that attitude
!     10 million seconds later than those for body -9999
!
      BEGTIM = TICK(1)
      ENDTIM = TICK(NTICKS)

      INST   = -10000
      REF    = 'FK4'
      AVFLG  = .TRUE.
      SEGID  = 'Object -10000'


      DO I = 1, NTICKS
         TK = TICK(I)
         ET = SECNDS ( TK )
         CALL TSTATD ( ET, ROT, ANGVEL(1,I) )
         CALL M2Q    (     ROT, QUATRN(1,I) )
      END DO


      NINTS     = 1
      STARTS(1) = 0.0D0

      CALL CKW03  ( HANDLE, &
                    BEGTIM, ENDTIM, INST, REF, AVFLG, &
                    SEGID,  NTICKS, TICK, QUATRN, ANGVEL, &
                    NINTS,  STARTS )

!
!     Finally for the third segment take take the same attitudes
!     but 100 million seconds later than those for object -9999
!
      BEGTIM = TICK(1)
      ENDTIM = TICK(NTICKS)

      INST   = -10001
      REF    = 'J2000'
      AVFLG  = .TRUE.
      SEGID  = 'Test Segment for object -10001'


      DO I = 1, NTICKS
         TK = TICK(I)
         ET = SECNDS ( TK )
         CALL TSTATD ( ET, ROT, ANGVEL(1,I) )
         CALL M2Q    (     ROT, QUATRN(1,I) )
      END DO


      NINTS     = 1
      STARTS(1) = 0.0D0

      CALL CKW03  ( HANDLE, &
                    BEGTIM, ENDTIM, INST, REF, AVFLG, &
                    SEGID,  NTICKS, TICK, QUATRN, ANGVEL, &
                    NINTS,  STARTS )



      CALL DAFCLS ( HANDLE )
!
!     Now take care of loading the test kernels if they are needed.
!
      IF ( LOADSC ) THEN

         CALL LDPOOL ( SCLKNM )


         IF ( .NOT. KEEPSC ) THEN

            CALL KILFIL ( SCLKNM )

         END IF

      END IF

      IF ( LOADCK ) THEN

         CALL CKLPF ( CKNM, HANDLE )


      END IF

!
!     Register these files with FILREG so that they will automatically
!     get removed when they are no longer needed.
!
      CALL TFILES ( SCLKNM )
      CALL TFILES ( CKNM   )

      RETURN
      END
