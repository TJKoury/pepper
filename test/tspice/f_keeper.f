C$Procedure      F_KEEPER ( Test the entry points of KEEPER )
 
      SUBROUTINE F_KEEPER ( OK )
  
C$ Abstract
C
C     Perform a collection of rudimentary tests on the
C     KEEPER collection of entry points.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     None.
C
C$ Keywords
C
C     TEST FAMILY
C
C$ Declarations

      IMPLICIT NONE
      LOGICAL               OK
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     OK         O   logical indicating test status.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     OK         is a logical that indicates the test status to the
C                caller.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     This routine does not generate any errors. Routines in its
C     call tree may generate errors that are either intentional and
C     trapped or unintentional and need reporting.  The test family
C     utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine tests the higher-level SPICELIB 
C     geometry routine SINCPT.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman     (JPL)
C
C$ Version
C
C-    TSPICE Version 3.0.0 01-APR-2009 (NJB)
C
C        Updated routine to test rejection of long file and path names,
C        and to test handling of continued path names.
C
C-    TSPICE Version 2.0.0 15-NOV-2006 (NJB)
C
C        Added two test cases to validate updating of source
C        pointers in KEEPER data base when files are unloaded.
C
C        Added a test case for the KCLEAR routine.
C
C-&
 
 
C
C     Test Utility Functions
C
      CHARACTER*(16)        BEGDAT
 
C
C     SPICELIB functions
C
      INTEGER               CARDI
 
C
C     Local parameters
C 
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )
 
      INTEGER               NUFILS
      PARAMETER           ( NUFILS = 5 )

      INTEGER               MAXTST
      PARAMETER           ( MAXTST = 100 )

      INTEGER               BIGSIZ
      PARAMETER           ( BIGSIZ = 1000 )
      
C
C     Local variables
C
      CHARACTER*(LNSIZE)    CVALS  ( 1  )
      CHARACTER*(BIGSIZ)    BIGNAM
      CHARACTER*(LNSIZE)    EXPFIL ( 20 )
      CHARACTER*(LNSIZE)    EXPSRC ( 20 )
      CHARACTER*(LNSIZE)    FILE
      CHARACTER*(LNSIZE)    SOURCE
      CHARACTER*(LNSIZE)    UFILES ( NUFILS )
      CHARACTER*(LNSIZE)    ZZLEAP ( 29 )
      CHARACTER*(LNSIZE)    ZZTST1 (  3 )
      CHARACTER*(LNSIZE)    ZZTST2 (  3 )
      CHARACTER*(LNSIZE)    ZZTST3 (  3 )
      CHARACTER*(LNSIZE)    ZZTST4 (  9 )
      CHARACTER*(LNSIZE)    ZZTST6 ( MAXTST )
 
      CHARACTER*(WDSIZE)    CMP    ( 20 )
      CHARACTER*(WDSIZE)    DTTYPE
      CHARACTER*(WDSIZE)    EXPTYP ( 20 )
      CHARACTER*(WDSIZE)    FILTYP
 
      INTEGER               B
      INTEGER               COUNT
      INTEGER               E
      INTEGER               HANDLE
      INTEGER               HANSET ( LBCELL:10 )
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               N
      INTEGER               NMETA
      INTEGER               EXPCNT

      LOGICAL               FOUND
 
C
C     Saved variables
C
      SAVE




C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_KEEPER' )

      CALL TCASE ( 'KEEPER test setup' )
 
C
C     Set up the text for the various text kernels we shall
C     be loading via FURNSH.
C
      ZZLEAP(  1 ) =  BEGDAT()
      ZZLEAP(  2 ) = 'DELTET/DELTA_T_A =   32.184'
      ZZLEAP(  3 ) = 'DELTET/K         =    1.657D-3'
      ZZLEAP(  4 ) = 'DELTET/EB        =    1.671D-2'
      ZZLEAP(  5 ) = 'DELTET/M         = (  6.239996D0   1.99'
     .//             '096871D-7 )'
      ZZLEAP(  6 ) = ' '
      ZZLEAP(  7 ) = 'DELTET/DELTA_AT  = ( 10,   @1972-JAN-1'
      ZZLEAP(  8 ) = '                     11,   @1972-JUL-1'
      ZZLEAP(  9 ) = '                     12,   @1973-JAN-1'
      ZZLEAP( 10 ) = '                     13,   @1974-JAN-1'
      ZZLEAP( 11 ) = '                     14,   @1975-JAN-1'
      ZZLEAP( 12 ) = '                     15,   @1976-JAN-1'
      ZZLEAP( 13 ) = '                     16,   @1977-JAN-1'
      ZZLEAP( 14 ) = '                     17,   @1978-JAN-1'
      ZZLEAP( 15 ) = '                     18,   @1979-JAN-1'
      ZZLEAP( 16 ) = '                     19,   @1980-JAN-1'
      ZZLEAP( 17 ) = '                     20,   @1981-JUL-1'
      ZZLEAP( 18 ) = '                     21,   @1982-JUL-1'
      ZZLEAP( 19 ) = '                     22,   @1983-JUL-1'
      ZZLEAP( 20 ) = '                     23,   @1985-JUL-1'
      ZZLEAP( 21 ) = '                     24,   @1988-JAN-1'
      ZZLEAP( 22 ) = '                     25,   @1990-JAN-1'
      ZZLEAP( 23 ) = '                     26,   @1991-JAN-1'
      ZZLEAP( 24 ) = '                     27,   @1992-JUL-1'
      ZZLEAP( 25 ) = '                     28,   @1993-JUL-1'
      ZZLEAP( 26 ) = '                     29,   @1994-JUL-1'
      ZZLEAP( 27 ) = '                     30,   @1996-JAN-1'
      ZZLEAP( 28 ) = '                     31,   @1997-JUL-1'
      ZZLEAP( 29 ) = '                     32,   @1999-JAN-1 )'
 
 
      ZZTST1(  1 ) =  BEGDAT()
      ZZTST1(  2 ) = 'ZZTST1_NUMBER += 1'
      ZZTST1(  3 ) = 'ZZTST1_STRING = ''1'''
 
      ZZTST2(  1 ) =  BEGDAT()
      ZZTST2(  2 ) = 'ZZTST1_NUMBER += 2'
      ZZTST2(  3 ) = 'ZZTST2_STRING = ''2'''
 
      ZZTST3(  1 ) =  BEGDAT()
      ZZTST3(  2 ) = 'ZZTST1_NUMBER += 3'
      ZZTST3(  3 ) = 'ZZTST3_STRING  = ''3'''
C
C     The first meta-text kernel.
C
      ZZTST4(  1 ) =  BEGDAT()
      ZZTST4(  2 ) = 'KERNELS_TO_LOAD = ( ''zz3spk$SPK'','
      ZZTST4(  3 ) = '                    ''zzck2$CK'','
      ZZTST4(  4 ) = '                    ''zzsclk2.ker'','
      ZZTST4(  5 ) = '                    ''zztst3$TXT'' )'
      ZZTST4(  6 ) = 'PATH_SYMBOLS = ( ''SPK'',  ''CK'',   '''
     .//             'TXT'' )'
      ZZTST4(  7 ) = 'PATH_VALUES  = ( ''.bsp'', ''.bc'', ''.'
     .//             'txt'' )'
 
C
C     Wipe out any existing test kernels.  (There shouldn't
C     be any, but just in case.)
C
      CALL KILFIL ( 'zz1spk.bsp'  )
      CALL KILFIL ( 'zz2spk.bsp'  )
      CALL KILFIL ( 'zz3spk.bsp'  )
      CALL KILFIL ( 'zzck1.bc'    )
      CALL KILFIL ( 'zzck2.bc'    )
      CALL KILFIL ( 'zzsclk1.ker' )
      CALL KILFIL ( 'zzsclk2.ker' )
      CALL KILFIL ( 'zzleaps.ker' )
      CALL KILFIL ( 'zztst1.txt' )
      CALL KILFIL ( 'zztst2.txt' )
      CALL KILFIL ( 'zztst3.txt' )
      CALL KILFIL ( 'zztst4.txt' )
      CALL KILFIL ( 'zztst5.txt' )
 
      CALL KILFIL ( 'zztstek.be'  )
 
C
C     Create all of the test kernels we shall need.
C
      CALL TSTSPK ( 'zz1spk.bsp', .FALSE., HANDLE )
      CALL TSTSPK ( 'zz2spk.bsp', .FALSE., HANDLE )
      CALL TSTSPK ( 'zz3spk.bsp', .FALSE., HANDLE )
      CALL TSTCK3 ( 'zzck1.bc', 'zzsclk1.ker',
     .              .FALSE.,    .FALSE., .TRUE., HANDLE )
      CALL TSTCK3 ( 'zzck2.bc', 'zzsclk2.ker',
     .              .FALSE.,    .FALSE., .TRUE.,  HANDLE )
      CALL TSTSPK ( 'zztstek.be',   .FALSE., HANDLE )
 
      CALL TSTTXT ( 'zzleaps.ker', ZZLEAP, 29, .FALSE., .TRUE. )
      CALL TSTTXT ( 'zztst1.txt', ZZTST1,  3, .FALSE., .TRUE. )
      CALL TSTTXT ( 'zztst2.txt', ZZTST2,  3, .FALSE., .TRUE. )
      CALL TSTTXT ( 'zztst3.txt', ZZTST3,  3, .FALSE., .TRUE. )
      CALL TSTTXT ( 'zztst4.txt', ZZTST4,  7, .FALSE., .TRUE. )
      CALL TSTTXT ( 'zztst5.txt', ZZTST4,  7, .FALSE., .TRUE. )
 
      CALL CLPOOL
      CALL CHCKXC ( .FALSE., ' ',  OK )
 
 
      CALL TCASE ( 'Initialization Check.' )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
 


 
      CALL TCASE ( 'Error: input file name too long.' )

         BIGNAM = 'BIG_FILE_NAME'

         DO I = 14, BIGSIZ-1

            BIGNAM(I:I) = 'X'

         END DO
      
         BIGNAM(BIGSIZ:BIGSIZ) = '@'

         CALL FURNSH ( BIGNAM )
         CALL CHCKXC ( .TRUE., 'SPICE(FILENAMETOOLONG)',  OK )




      CALL TCASE ( 'Error: KERNELS_TO_LOAD file name too long.' )

         ZZTST6(  1 ) =  BEGDAT()
         ZZTST6(  2 ) = 'KERNELS_TO_LOAD = ('
         
C
C        Put 50 characters of BIGNAM on each of 20 lines in
C        the meta-kernel.
C
         B = 1
         E = 50

         DO I = 1, 19
            
            ZZTST6( 2+I ) = ''''//BIGNAM(B:E)//'+'''

            B = B + 50
            E = E + 50

         END DO

C
C        The last line gets a closing parenthesis.
C
         ZZTST6( 22 ) = ''''//BIGNAM(B:E)//''' )'


         CALL TSTTXT ( 'zztst6.txt', ZZTST6, 22, .FALSE., .TRUE. )
         CALL CHCKXC ( .FALSE., ' ',  OK )


         CALL FURNSH ( 'zztst6.txt'  )
         CALL CHCKXC ( .TRUE., 'SPICE(FILENAMETOOLONG)',  OK )

C
C        The meta-kernel is still loaded; unload it so the 
C        subsequent tests are done on a clean system.
C
         CALL UNLOAD ( 'zztst6.txt'  )
         CALL CHCKXC ( .FALSE., ' ',  OK )




      CALL TCASE ( 'Error: path name too long.' )


         CALL CLEARC ( MAXTST, ZZTST6 )
         CALL CHCKXC ( .FALSE., ' ',  OK )


         ZZTST6(  1 ) =  BEGDAT()
         ZZTST6(  2 ) = 'PATH_VALUES = ( ''Z'' '
         
C
C        Put 50 characters of BIGNAM on each of 20 lines in
C        the meta-kernel.
C
         B = 1
         E = 50

         DO I = 1, 19
            
            ZZTST6( 2+I ) = ''''//BIGNAM(B:E)//'+'''

            B = B + 50
            E = E + 50

         END DO

C 
C        The last line gets a closing parenthesis.
C
         ZZTST6( 22 ) = ''''//BIGNAM(B:E)//''' )'

C
C        Fill in the rest of the meta-kernel.
C
         ZZTST6( 23 ) = 'PATH_SYMBOLS    = ( ''B''  ''A'' )'

         ZZTST6( 24 ) = 'KERNELS_TO_LOAD = ''$A/spud.txt'' '


C         DO I = 1, 24
C            CALL TOSTDO ( ZZTST6(I) )
C         END DO


         CALL TSTTXT ( 'zztst6.txt', ZZTST6, 24, .FALSE., .TRUE. )
         CALL CHCKXC ( .FALSE., ' ',  OK )


         CALL FURNSH ( 'zztst6.txt'  )
         CALL CHCKXC ( .TRUE., 'SPICE(PATHTOOLONG)',  OK )

C
C        The meta-kernel is still loaded; unload it so the 
C        subsequent tests are done on a clean system.
C
         CALL UNLOAD ( 'zztst6.txt'  )
         CALL CHCKXC ( .FALSE., ' ',  OK )




      CALL TCASE ( 'Error: combined path and file name too long.' )


         CALL CLEARC ( MAXTST, ZZTST6 )
         CALL CHCKXC ( .FALSE., ' ',  OK )


         ZZTST6(  1 ) =  BEGDAT()
         ZZTST6(  2 ) = 'PATH_VALUES = ('
         
C
C        Put 50 characters of BIGNAM on each of 20 lines in
C        the meta-kernel.
C
         B = 1
         E = 50

         DO I = 1, 4
            
            ZZTST6( 2+I ) = ''''//BIGNAM(B:E)//'+'''

            B = B + 50
            E = E + 50

         END DO

C 
C        The last line gets a closing parenthesis.
C
         ZZTST6( 7 ) = ''''//BIGNAM(B:E)//''' )'

C
C        Fill in the rest of the meta-kernel.
C
         ZZTST6( 8 ) = 'PATH_SYMBOLS    = ''A'' '

         ZZTST6( 9 ) = 'KERNELS_TO_LOAD = ''$A/spud.txt'' '


C         DO I = 1, 9
C            CALL TOSTDO ( ZZTST6(I) )
C         END DO


         CALL TSTTXT ( 'zztst6.txt', ZZTST6, 9, .FALSE., .TRUE. )
         CALL CHCKXC ( .FALSE., ' ',  OK )


         CALL FURNSH ( 'zztst6.txt'  )
         CALL CHCKXC ( .TRUE., 'SPICE(FILENAMETOOLONG)',  OK )

C
C        The meta-kernel is still loaded; unload it so the 
C        subsequent tests are done on a clean system.
C
         CALL UNLOAD ( 'zztst6.txt'  )
         CALL CHCKXC ( .FALSE., ' ',  OK )




      CALL TCASE ( 'Load file with multiple symbol substitutions.' )

         CALL CLEARC ( MAXTST, ZZTST6 )
         CALL CHCKXC ( .FALSE., ' ',  OK )


         ZZTST6(  1 ) =  BEGDAT()
         ZZTST6(  2 ) = 'PATH_SYMBOLS    = ( ''ABC''  ''WXYZ'' )'
         ZZTST6(  3 ) = 'PATH_VALUES     = ( ''zz''   ''spk''  )'
         ZZTST6(  4 ) = 'KERNELS_TO_LOAD = ''$ABC1$WXYZ.bsp'' '

         CALL TSTTXT ( 'zztst6.txt', ZZTST6, 4, .FALSE., .TRUE. )
         CALL CHCKXC ( .FALSE., ' ',  OK )


C         DO I = 1, 4
C            CALL TOSTDO ( ZZTST6(I) )
C         END DO

         CALL FURNSH ( 'zztst6.txt'  )
         CALL CHCKXC ( .FALSE., ' ',  OK )

C
C        Check loaded file counts.
C
         CALL KTOTAL ( 'ALL',   COUNT )
         CALL CHCKSI ( 'ALLCOUNT', COUNT, '=', 2, 0, OK )
         CALL CHCKXC ( .FALSE., ' ',  OK )
 
         CALL KTOTAL ( 'SPK',   COUNT )
         CALL CHCKSI ( 'SPKCOUNT', COUNT, '=', 1, 0, OK )
         CALL CHCKXC ( .FALSE., ' ',  OK )


         CALL UNLOAD ( 'zztst6.txt'  )
         CALL CHCKXC ( .FALSE., ' ',  OK )

C
C        Check loaded file counts again.
C
         CALL KTOTAL ( 'ALL',   COUNT )
         CALL CHCKSI ( 'ALLCOUNT', COUNT, '=', 0, 0, OK )
         CALL CHCKXC ( .FALSE., ' ',  OK )
 
         CALL KTOTAL ( 'SPK',   COUNT )
         CALL CHCKSI ( 'SPKCOUNT', COUNT, '=', 0, 0, OK )
         CALL CHCKXC ( .FALSE., ' ',  OK )



      CALL TCASE ( 'Load file with continued path value.' )

         CALL CLEARC ( MAXTST, ZZTST6 )
         CALL CHCKXC ( .FALSE., ' ',  OK )


         ZZTST6(  1 ) =  BEGDAT()
         ZZTST6(  2 ) = 'PATH_SYMBOLS    = ( ''ABC'' )'
         ZZTST6(  3 ) = 'PATH_VALUES     = ( ''zz+'' ''2+''  ''spk''  )'
         ZZTST6(  4 ) = 'KERNELS_TO_LOAD = ''$ABC.bsp'' '

         CALL TSTTXT ( 'zztst6.txt', ZZTST6, 4, .FALSE., .TRUE. )
         CALL CHCKXC ( .FALSE., ' ',  OK )




C         DO I = 1, 4
C            CALL TOSTDO ( ZZTST6(I) )
C         END DO


         CALL FURNSH ( 'zztst6.txt'  )
         CALL CHCKXC ( .FALSE., ' ',  OK )

C
C        Check loaded file counts.
C
         CALL KTOTAL ( 'ALL',   COUNT )
         CALL CHCKSI ( 'ALLCOUNT', COUNT, '=', 2, 0, OK )
         CALL CHCKXC ( .FALSE., ' ',  OK )
 
         CALL KTOTAL ( 'SPK',   COUNT )
         CALL CHCKSI ( 'SPKCOUNT', COUNT, '=', 1, 0, OK )
         CALL CHCKXC ( .FALSE., ' ',  OK )


         CALL UNLOAD ( 'zztst6.txt'  )
         CALL CHCKXC ( .FALSE., ' ',  OK )

C
C        Check loaded file counts again.
C
         CALL KTOTAL ( 'ALL',   COUNT )
         CALL CHCKSI ( 'ALLCOUNT', COUNT, '=', 0, 0, OK )
         CALL CHCKXC ( .FALSE., ' ',  OK )
 
         CALL KTOTAL ( 'SPK',   COUNT )
         CALL CHCKSI ( 'SPKCOUNT', COUNT, '=', 0, 0, OK )
         CALL CHCKXC ( .FALSE., ' ',  OK )







      CALL TCASE ( 'Check Initial Values.' )
 
         CALL KTOTAL ( 'ALL',   COUNT )
         CALL CHCKSI ( 'ALLCOUNT', COUNT, '=', 0, 0, OK )
         CALL CHCKXC ( .FALSE., ' ',  OK )
 
         CALL KTOTAL ( 'SPK',   COUNT )
         CALL CHCKSI ( 'SPKCOUNT', COUNT, '=', 0, 0, OK )
         CALL CHCKXC ( .FALSE., ' ',  OK )
 
         CALL KTOTAL ( 'CK',   COUNT )
         CALL CHCKSI ( 'CKCOUNT', COUNT, '=', 0, 0, OK )
         CALL CHCKXC ( .FALSE., ' ',  OK )
 
         CALL KTOTAL ( 'TEXT',   COUNT )
         CALL CHCKSI ( 'TEXTCOUNT', COUNT, '=', 0, 0, OK )
         CALL CHCKXC ( .FALSE., ' ',  OK )
 
         CALL KTOTAL ( 'PCK',   COUNT )
         CALL CHCKSI ( 'PCKCOUNT', COUNT, '=', 0, 0, OK )
         CALL CHCKXC ( .FALSE., ' ',  OK )
 
         CALL KTOTAL ( 'EK',   COUNT )
         CALL CHCKSI ( 'EKCOUNT', COUNT, '=', 0, 0, OK )
         CALL CHCKXC ( .FALSE., ' ',  OK )
 
         CALL KTOTAL ( 'SPK CK PCK',   COUNT )
         CALL CHCKSI ( '3COUNT', COUNT, '=', 0, 0, OK )
         CALL CHCKXC ( .FALSE., ' ',  OK )
 
 
      CALL TCASE ( 'Check that we can furnish all kernels directly.' )
 
         CALL FURNSH ( 'zz1spk.bsp'  )
         CALL CHCKXC ( .FALSE., ' ',  OK )
 
         CALL KTOTAL ( 'ALL',   COUNT )
         CALL CHCKSI ( 'ALLCOUNT', COUNT, '=', 1, 0, OK )
         CALL KTOTAL ( 'SPK',   COUNT )
         CALL CHCKSI ( 'SPKCOUNT', COUNT, '=', 1, 0, OK )
         CALL KTOTAL ( 'CK',   COUNT )
         CALL CHCKSI ( 'CKCOUNT', COUNT, '=', 0, 0, OK )
         CALL KTOTAL ( 'CK PCK SPK',   COUNT )
         CALL CHCKSI ( '3COUNT', COUNT, '=', 1, 0, OK )
         CALL KTOTAL ( 'TEXT',   COUNT )
         CALL CHCKSI ( 'TEXTCOUNT', COUNT, '=', 0, 0, OK )
         CALL KTOTAL ( 'EK',   COUNT )
         CALL CHCKSI ( 'EKCOUNT', COUNT, '=', 0, 0, OK )
 
         CALL FURNSH ( 'zz2spk.bsp'  )
         CALL KTOTAL ( 'ALL',   COUNT )
         CALL CHCKSI ( 'ALLCOUNT', COUNT, '=', 2, 0, OK )
         CALL KTOTAL ( 'SPK',   COUNT )
         CALL CHCKSI ( 'SPKCOUNT', COUNT, '=', 2, 0, OK )
         CALL KTOTAL ( 'CK',    COUNT )
         CALL CHCKSI ( 'CKCOUNT', COUNT, '=', 0, 0, OK )
         CALL KTOTAL ( 'CK PCK SPK',   COUNT )
         CALL CHCKSI ( '3COUNT', COUNT, '=', 2, 0, OK )
         CALL KTOTAL ( 'TEXT',  COUNT )
         CALL CHCKSI ( 'TESTCOUNT', COUNT, '=', 0, 0, OK )
         CALL KTOTAL ( 'EK',    COUNT )
         CALL CHCKSI ( 'EKCOUNT', COUNT, '=', 0, 0, OK )
 
         CALL FURNSH ( 'zzck1.bc'    )
         CALL KTOTAL ( 'ALL',   COUNT )
         CALL CHCKSI ( 'ALLCOUNT', COUNT, '=', 3, 0, OK )
         CALL KTOTAL ( 'SPK',   COUNT )
         CALL CHCKSI ( 'APKCOUNT', COUNT, '=', 2, 0, OK )
         CALL KTOTAL ( 'CK',    COUNT )
         CALL CHCKSI ( 'CKCOUNT', COUNT, '=', 1, 0, OK )
         CALL KTOTAL ( 'CK PCK SPK',   COUNT )
         CALL CHCKSI ( '3COUNT', COUNT, '=', 3, 0, OK )
         CALL KTOTAL ( 'TEXT',  COUNT )
         CALL CHCKSI ( 'TEXTCOUNT', COUNT, '=', 0, 0, OK )
         CALL KTOTAL ( 'EK',    COUNT )
         CALL CHCKSI ( 'EKCOUNT', COUNT, '=', 0, 0, OK )
 
 
         CALL SSIZEI ( 10, HANSET )
         CALL DAFHOF ( HANSET )
 
         COUNT = CARDI ( HANSET )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'DAFCOUNT', COUNT, '=', 3, 0, OK )
 
 
 
         CALL FURNSH ( 'zzsclk1.ker' )
         CALL KTOTAL ( 'ALL',   COUNT )
         CALL CHCKSI ( 'ALLCOUNT', COUNT, '=', 4, 0, OK )
         CALL KTOTAL ( 'SPK',   COUNT )
         CALL CHCKSI ( 'SPKCOUNT', COUNT, '=', 2, 0, OK )
         CALL KTOTAL ( 'CK',    COUNT )
         CALL CHCKSI ( 'CKCOUNT', COUNT, '=', 1, 0, OK )
         CALL KTOTAL ( 'CK PCK SPK',   COUNT )
         CALL CHCKSI ( '3COUNT', COUNT, '=', 3, 0, OK )
         CALL KTOTAL ( 'TEXT',  COUNT )
         CALL CHCKSI ( 'TEXTCOUNT', COUNT, '=', 1, 0, OK )
         CALL KTOTAL ( 'EK',    COUNT )
         CALL CHCKSI ( 'EKCOUNT', COUNT, '=', 0, 0, OK )
 
         CALL FURNSH ( 'zztstek.be'  )
         CALL KTOTAL ( 'ALL',   COUNT )
         CALL CHCKSI ( 'ALLCOUNT', COUNT, '=', 5, 0, OK )
         CALL KTOTAL ( 'SPK',   COUNT )
         CALL CHCKSI ( 'SPKCOUNT', COUNT, '=', 3, 0, OK )
         CALL KTOTAL ( 'CK',    COUNT )
         CALL CHCKSI ( 'CKCOUNT', COUNT, '=', 1, 0, OK )
         CALL KTOTAL ( 'CK PCK SPK',   COUNT )
         CALL CHCKSI ( '3COUNT', COUNT, '=', 4, 0, OK )
         CALL KTOTAL ( 'TEXT',  COUNT )
         CALL CHCKSI ( 'TEXTCOUNT', COUNT, '=', 1, 0, OK )
         CALL KTOTAL ( 'EK',    COUNT )
         CALL CHCKSI ( 'EKCOUNT', COUNT, '=', 0, 0, OK )
 
 
         CALL SSIZEI ( 10, HANSET )
         CALL DAFHOF ( HANSET )
 
         COUNT = CARDI ( HANSET )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'DASCOUNT', COUNT, '=', 4, 0, OK )
 
 
 
C
C        Now fetch files to see if we can get the correct
C        information about them.
C
         EXPFIL ( 1 ) = 'zz1spk.bsp'
         EXPFIL ( 2 ) = 'zz2spk.bsp'
         EXPFIL ( 3 ) = 'zzck1.bc'
         EXPFIL ( 4 ) = 'zzsclk1.ker'
         EXPFIL ( 5 ) = 'zztstek.be'
 
         EXPTYP ( 1 ) = 'SPK'
         EXPTYP ( 2 ) = 'SPK'
         EXPTYP ( 3 ) = 'CK'
         EXPTYP ( 4 ) = 'TEXT'
         EXPTYP ( 5 ) = 'SPK'
 
 
         CMP ( 1 ) = '!='
         CMP ( 2 ) = '!='
         CMP ( 3 ) = '!='
         CMP ( 4 ) = '='
         CMP ( 5 ) = '!='
 
         DO I = 1, 5
 
            CALL KDATA ( I, 'ALL', FILE, FILTYP, SOURCE, HANDLE, FOUND )
 
            CALL CHCKXC ( .FALSE., ' ',  OK )
            CALL CHCKSC ( 'FILE', FILE,     '=', EXPFIL(I), OK )
            CALL CHCKSC ( 'FILTYP', FILTYP, '=', EXPTYP(I), OK )
            CALL CHCKSC ( 'SOURCE', SOURCE, '=', ' ',       OK )
 
            CALL CHCKSI ( 'HANDLE', HANDLE, CMP(I), 0, 0,   OK )
            CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
 
         END DO
 
 
      CALL TCASE ( 'Check that Meta-Kernels load successfully.' )
 
         CALL FURNSH ( 'zztst4.txt' )
         CALL CHCKXC ( .FALSE., ' ',  OK )
 
         CALL KTOTAL ( 'ALL',   COUNT )
         CALL CHCKSI ( 'ALLCOUNT', COUNT, '=', 10, 0, OK )
         CALL KTOTAL ( 'SPK',   COUNT )
         CALL CHCKSI ( 'SPKCOUNT', COUNT, '=',  4, 0, OK )
         CALL KTOTAL ( 'CK',    COUNT )
         CALL CHCKSI ( 'CKCOUNT', COUNT, '=',  2, 0, OK )
         CALL KTOTAL ( 'CK PCK SPK',   COUNT )
         CALL CHCKSI ( '3COUNT', COUNT, '=',  6, 0, OK )
         CALL KTOTAL ( 'TEXT',  COUNT )
         CALL CHCKSI ( 'TEXTCOUNT', COUNT, '=',  3, 0, OK )
         CALL KTOTAL ( 'META',  COUNT )
         CALL CHCKSI ( 'METACOUNT', COUNT, '=',  1, 0, OK )
         CALL KTOTAL ( 'EK',    COUNT )
         CALL CHCKSI ( 'EKCOUNT', COUNT, '=',  0, 0, OK )
 
 
         EXPFIL (  1 ) = 'zz1spk.bsp'
         EXPFIL (  2 ) = 'zz2spk.bsp'
         EXPFIL (  3 ) = 'zzck1.bc'
         EXPFIL (  4 ) = 'zzsclk1.ker'
         EXPFIL (  5 ) = 'zztstek.be'
         EXPFIL (  6 ) = 'zztst4.txt'
         EXPFIL (  7 ) = 'zz3spk.bsp'
         EXPFIL (  8 ) = 'zzck2.bc'
         EXPFIL (  9 ) = 'zzsclk2.ker'
         EXPFIL ( 10 ) = 'zztst3.txt'
 
         EXPTYP ( 1 )  = 'SPK'
         EXPTYP ( 2 )  = 'SPK'
         EXPTYP ( 3 )  = 'CK'
         EXPTYP ( 4 )  = 'TEXT'
         EXPTYP ( 5 )  = 'SPK'
         EXPTYP ( 6 )  = 'META'
         EXPTYP ( 7 )  = 'SPK'
         EXPTYP ( 8 )  = 'CK'
         EXPTYP ( 9 )  = 'TEXT'
         EXPTYP ( 10 ) = 'TEXT'
 
 
         CMP ( 1 )  = '!='
         CMP ( 2 )  = '!='
         CMP ( 3 )  = '!='
         CMP ( 4 )  = '='
         CMP ( 5 )  = '!='
         CMP ( 6)  = '='
         CMP ( 7)  = '!='
         CMP ( 8)  = '!='
         CMP ( 9)  = '='
         CMP ( 10) = '='
 
         EXPSRC( 1 )  = ' '
         EXPSRC( 2 )  = ' '
         EXPSRC( 3 )  = ' '
         EXPSRC( 4 )  = ' '
         EXPSRC( 5 )  = ' '
         EXPSRC( 6 )  = ' '
         EXPSRC( 7 )  = 'zztst4.txt'
         EXPSRC( 8 )  = 'zztst4.txt'
         EXPSRC( 9 )  = 'zztst4.txt'
         EXPSRC( 10 ) = 'zztst4.txt'
 
         DO I = 1, 10
 
            CALL KDATA ( I, 'ALL', FILE, FILTYP, SOURCE, HANDLE, FOUND )
 
            CALL CHCKXC ( .FALSE., ' ',  OK )
            CALL CHCKSC ( 'FILE', FILE,     '=', EXPFIL(I), OK )
            CALL CHCKSC ( 'FILTYP', FILTYP, '=', EXPTYP(I), OK )
            CALL CHCKSC ( 'SOURCE', SOURCE, '=', EXPSRC(I), OK )
 
            CALL CHCKSI ( 'HANDLE', HANDLE, CMP(I), 0, 0,      OK )
            CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
 
         END DO
 
         CALL KDATA ( 4,    'SPK CK', FILE, FILTYP, SOURCE,
     .                HANDLE, FOUND )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'FILE', FILE,     '=', EXPFIL(5), OK )
         CALL CHCKSC ( 'FILTYP', FILTYP, '=', EXPTYP(5), OK )
         CALL CHCKSC ( 'SOURCE', SOURCE, '=', EXPSRC(5), OK )
         CALL CHCKSI ( 'HANDLE', HANDLE, CMP(5), 0, 0,   OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
 
         CALL KDATA ( 2,     'TEXT', FILE, FILTYP, SOURCE,
     .                HANDLE, FOUND )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'FILE', FILE,     '=', EXPFIL(9), OK )
         CALL CHCKSC ( 'FILTYP', FILTYP, '=', EXPTYP(9), OK )
         CALL CHCKSC ( 'SOURCE', SOURCE, '=', EXPSRC(9), OK )
         CALL CHCKSI ( 'HANDLE', HANDLE, CMP(9), 0, 0,   OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
 
 
 
      CALL TCASE ( 'Check that files loaded after a meta-kernel '
     .//           'are loaded correctly.' )
 
 
         CALL FURNSH ( 'zzleaps.ker' )
         CALL CHCKXC ( .FALSE., ' ',  OK )
 
         CALL FURNSH ( 'zztst1.txt' )
         CALL CHCKXC ( .FALSE., ' ',  OK )
 
         CALL FURNSH ( 'zztst2.txt' )
         CALL CHCKXC ( .FALSE., ' ',  OK )
 
 
         EXPFIL ( 11 ) = 'zzleaps.ker'
         EXPFIL ( 12 ) = 'zztst1.txt'
         EXPFIL ( 13 ) = 'zztst2.txt'
 
 
         EXPTYP ( 11 ) = 'TEXT'
         EXPTYP ( 12 ) = 'TEXT'
         EXPTYP ( 13 ) = 'TEXT'
 
         CMP ( 11)     = '='
         CMP ( 12)     = '='
         CMP ( 13)     = '='
 
         EXPSRC( 11 )  = ' '
         EXPSRC( 12 )  = ' '
         EXPSRC( 13 )  = ' '
 
         CALL KTOTAL ( 'ALL',   COUNT )
         CALL CHCKSI ( 'ALLCOUNT', COUNT, '=', 13, 0, OK )
         CALL KTOTAL ( 'SPK',   COUNT )
         CALL CHCKSI ( 'SPKCOUNT', COUNT, '=',  4, 0, OK )
         CALL KTOTAL ( 'CK',    COUNT )
         CALL CHCKSI ( 'CKCOUNT', COUNT, '=',  2, 0, OK )
         CALL KTOTAL ( 'CK PCK SPK',   COUNT )
         CALL CHCKSI ( '3COUNT', COUNT, '=',  6, 0, OK )
         CALL KTOTAL ( 'TEXT',  COUNT )
         CALL CHCKSI ( 'TEXTCOUNT', COUNT, '=',  6, 0, OK )
         CALL KTOTAL ( 'META',  COUNT )
         CALL CHCKSI ( 'METACOUNT', COUNT, '=',  1, 0, OK )
         CALL KTOTAL ( 'EK',    COUNT )
         CALL CHCKSI ( 'EKCOUNT', COUNT, '=',  0, 0, OK )
 
         CALL SSIZEI ( 10, HANSET )
         CALL DAFHOF ( HANSET )
 
         COUNT = CARDI ( HANSET )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'DAFCOUNT', COUNT, '=', 6, 0, OK )
 
         CALL SSIZEI ( 10, HANSET )
         CALL DASHOF ( HANSET )
 
         COUNT = CARDI ( HANSET )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'DASCOUNT', COUNT, '=', 0, 0, OK )
 
 
 
         DO I = 1, 13
 
            CALL KDATA ( I, 'ALL', FILE, FILTYP, SOURCE, HANDLE, FOUND )
 
            CALL CHCKXC ( .FALSE., ' ',  OK )
            CALL CHCKSC ( 'FILE', FILE,     '=', EXPFIL(I), OK )
            CALL CHCKSC ( 'FILTYP', FILTYP, '=', EXPTYP(I), OK )
            CALL CHCKSC ( 'SOURCE', SOURCE, '=', EXPSRC(I), OK )
 
            CALL CHCKSI ( 'HANDLE', HANDLE, CMP(I), 0, 0,      OK )
            CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
 
         END DO
 
         CALL KDATA ( 4,    'SPK CK', FILE, FILTYP, SOURCE,
     .                HANDLE, FOUND )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'FILE', FILE,     '=', EXPFIL(5), OK )
         CALL CHCKSC ( 'FILTYP', FILTYP, '=', EXPTYP(5), OK )
         CALL CHCKSC ( 'SOURCE', SOURCE, '=', EXPSRC(5), OK )
         CALL CHCKSI ( 'HANDLE', HANDLE, CMP(5), 0, 0,      OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
 
         CALL KDATA ( 2,     'TEXT', FILE, FILTYP, SOURCE,
     .                HANDLE, FOUND )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'FILE', FILE,     '=', EXPFIL(9), OK )
         CALL CHCKSC ( 'FILTYP', FILTYP, '=', EXPTYP(9), OK )
         CALL CHCKSC ( 'SOURCE', SOURCE, '=', EXPSRC(9), OK )
         CALL CHCKSI ( 'HANDLE', HANDLE, CMP(9), 0, 0,      OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
 
 
      CALL TCASE ( 'See if data is actually present in the '
     .//           'kernel pool. ' )
 
         CALL DTPOOL ( 'ZZTST1_NUMBER', FOUND, N, DTTYPE )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
         CALL CHCKSI ( 'N', N, '=', 3, 0, OK )
         CALL CHCKSC ( 'DTTYPE', DTTYPE, '=', 'N', OK )
 
         CALL DTPOOL ( 'ZZTST1_STRING', FOUND, N, DTTYPE )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
         CALL CHCKSI ( 'N', N, '=', 1, 0, OK )
         CALL CHCKSC ( 'DTTYPE', DTTYPE, '=', 'C', OK )
 
         CALL DTPOOL ( 'KERNELS_TO_LOAD', FOUND, N, DTTYPE )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,  OK )
 
         CALL DTPOOL ( 'PATH_SYMBOLS', FOUND, N, DTTYPE )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,  OK )
 
         CALL DTPOOL ( 'PATH_VALUES', FOUND, N, DTTYPE )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,  OK )
 
      CALL TCASE ( 'Make sure that we can fetch information '
     .//           'about files by name. ' )
 
         DO I = 1, 13
            CALL KINFO ( EXPFIL(I), FILTYP, SOURCE, HANDLE, FOUND )
 
            CALL CHCKXC ( .FALSE., ' ',  OK )
 
            CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
            CALL CHCKSC ( 'FILTYP', FILTYP, '=', EXPTYP(I), OK )
            CALL CHCKSC ( 'SOURCE', SOURCE, '=', EXPSRC(I), OK )
 
            CALL CHCKSI ( 'HANDLE', HANDLE, CMP(I), 0, 0,      OK )
         END DO
 
         CALL KINFO  ( 'SPUD', FILTYP, SOURCE, HANDLE, FOUND )
         CALL CHCKXC ( .FALSE., ' ',             OK )
         CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,  OK )
 
 
      CALL TCASE ( 'See if we can successfully unload a kernel.' )
 
         CALL UNLOAD ( 'zztst2.txt' )
 
         CALL DTPOOL ( 'ZZTST1_NUMBER', FOUND, N, DTTYPE )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
         CALL CHCKSI ( 'N', N, '=', 2, 0, OK )
         CALL CHCKSC ( 'DTTYPE', DTTYPE, '=', 'N', OK )
 
         CALL DTPOOL ( 'ZZTST3_STRING', FOUND, N, DTTYPE )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
         CALL CHCKSI ( 'N', N, '=', 1, 0, OK )
         CALL CHCKSC ( 'DTTYPE', DTTYPE, '=', 'C', OK )
 
         CALL DTPOOL ( 'KERNELS_TO_LOAD', FOUND, N, DTTYPE )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,  OK )
 
         CALL DTPOOL ( 'PATH_SYMBOLS', FOUND, N, DTTYPE )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,  OK )
 
         CALL DTPOOL ( 'PATH_VALUES', FOUND, N, DTTYPE )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,  OK )
 
         CALL KTOTAL ( 'ALL', COUNT )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'ALLCOUNT', COUNT, '=', 12, 0, OK )
 
         CALL KTOTAL ( 'SPK',   COUNT )
         CALL CHCKSI ( 'SPKCOUNT', COUNT, '=',  4, 0, OK )
         CALL KTOTAL ( 'CK',    COUNT )
         CALL CHCKSI ( 'CKCOUNT', COUNT, '=',  2, 0, OK )
         CALL KTOTAL ( 'CK PCK SPK',   COUNT )
         CALL CHCKSI ( '3COUNT', COUNT, '=',  6, 0, OK )
         CALL KTOTAL ( 'TEXT',  COUNT )
         CALL CHCKSI ( 'TEXTCOUNT', COUNT, '=',  5, 0, OK )
         CALL KTOTAL ( 'META',  COUNT )
         CALL CHCKSI ( 'METACOUNT', COUNT, '=',  1, 0, OK )
         CALL KTOTAL ( 'EK',    COUNT )
         CALL CHCKSI ( 'EKCOUNT', COUNT, '=',  0, 0, OK )
 
 
 
      CALL TCASE ( 'See if we can successfully unload a meta-kernel.' )
 
         CALL UNLOAD ( 'zztst4.txt' )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL KTOTAL ( 'ALL', COUNT )
         CALL CHCKSI ( 'ALLCOUNT', COUNT, '=',  7, 0, OK )
         CALL KTOTAL ( 'SPK',   COUNT )
         CALL CHCKSI ( 'SPKCOUNT', COUNT, '=',  3, 0, OK )
         CALL KTOTAL ( 'CK',    COUNT )
         CALL CHCKSI ( 'CKCOUNT', COUNT, '=',  1, 0, OK )
         CALL KTOTAL ( 'CK PCK SPK',   COUNT )
         CALL CHCKSI ( '3COUNT', COUNT, '=',  4, 0, OK )
         CALL KTOTAL ( 'TEXT',  COUNT )
         CALL CHCKSI ( 'TEXTCOUNT', COUNT, '=',  3, 0, OK )
         CALL KTOTAL ( 'META',  COUNT )
         CALL CHCKSI ( 'METACOUNT', COUNT, '=',  0, 0, OK )
         CALL KTOTAL ( 'EK',    COUNT )
         CALL CHCKSI ( 'EKCOUNT', COUNT, '=',  0, 0, OK )
 
 
         CALL DTPOOL ( 'ZZTST1_NUMBER', FOUND, N, DTTYPE )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
         CALL CHCKSI ( 'N', N, '=', 1, 0, OK )
         CALL CHCKSC ( 'DTTYPE', DTTYPE, '=', 'N', OK )
 
         CALL DTPOOL ( 'ZZTST3_STRING', FOUND, N, DTTYPE )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,  OK )
 
         CALL SSIZEI ( 10, HANSET )
         CALL DAFHOF ( HANSET )
 
         COUNT = CARDI ( HANSET )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'DAFCOUNT', COUNT, '=', 4, 0, OK )
 
 
         CALL SSIZEI ( 10, HANSET )
         CALL DASHOF ( HANSET )
 
         COUNT = CARDI ( HANSET )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'DASCOUNT', COUNT, '=', 0, 0, OK )
 
         CALL DTPOOL ( 'KERNELS_TO_LOAD', FOUND, N, DTTYPE )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,  OK )
 
         CALL DTPOOL ( 'PATH_SYMBOLS', FOUND, N, DTTYPE )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,  OK )
 
         CALL DTPOOL ( 'PATH_VALUES', FOUND, N, DTTYPE )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,  OK )
 
 
 
 
      CALL TCASE ( 'Unload all of the remaining files' )
 
         CALL UNLOAD ( 'zz1spk.bsp'  )
         CALL UNLOAD ( 'zz2spk.bsp'  )
         CALL UNLOAD ( 'zzck1.bc'    )
         CALL UNLOAD ( 'zztstek.be'  )
         CALL UNLOAD ( 'zzsclk1.ker' )
         CALL UNLOAD ( 'zzleaps.ker' )
         CALL UNLOAD ( 'zztst1.txt' )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL KTOTAL ( 'ALL', COUNT )
         CALL CHCKSI ( 'ALLCOUNT', COUNT, '=',  0, 0, OK )
 
         CALL DTPOOL ( 'ZZTST1_NUMBER', FOUND, N, DTTYPE )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,  OK )
 
         CALL DTPOOL ( 'ZZTST3_STRING', FOUND, N, DTTYPE )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,  OK )
 
 
         CALL SSIZEI ( 10, HANSET )
         CALL DAFHOF ( HANSET )
 
         COUNT = CARDI ( HANSET )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'DAFCOUNT', COUNT, '=', 0, 0, OK )
 
 
         CALL SSIZEI ( 10, HANSET )
         CALL DASHOF ( HANSET )
 
         COUNT = CARDI ( HANSET )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'DASCOUNT', COUNT, '=', 0, 0, OK )


C
C     Test proper updating of source pointers when kernels are
C     deleted.
C
      CALL TCASE ( 'Load meta-kernel, then individual files, '
     .//           'then another meta-kernel.  Unload the '
     .//           'individual files loaded initially.  Verify entries '
     .//           'for files referenced by the meta-kernel.'          )
     
C
C        Here's the first meta-kernel:
C
         CALL FURNSH ( 'zztst4.txt' )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         UFILES(1) = 'zz1spk.bsp'
         UFILES(2) = 'zz2spk.bsp'
         UFILES(3) = 'zzck1.bc'
         UFILES(4) = 'zztstek.be'
         UFILES(5) = 'zzleaps.ker'

         DO I = 1, NUFILS
            CALL FURNSH ( UFILES(I) )
            CALL CHCKXC ( .FALSE., ' ',  OK )
         END DO

C
C        Here's the second meta-kernel:
C
         CALL FURNSH ( 'zztst5.txt' )
         CALL CHCKXC ( .FALSE., ' ',  OK )

C
C        Create a local database of information
C        on loaded files.
C
         CALL KTOTAL ( 'ALL',   EXPCNT )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         DO I = 1, EXPCNT

            CALL KDATA  ( I,         'ALL',  EXPFIL(I), EXPTYP(I), 
     .                    EXPSRC(I), HANDLE, FOUND                )

            CALL CHCKXC ( .FALSE., ' ',            OK )
            CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )

         END DO

C
C        Let NMETA be the number of database entries corresponding
C        to the first meta-kernel.
C
         NMETA = 5

         DO I = 1, NUFILS
C
C           Unload the Ith file preceding the second meta-kernel.
C
            CALL UNLOAD ( UFILES(I) )
            CALL CHCKXC ( .FALSE., ' ',  OK )

C
C           Check the database.
C
            CALL KTOTAL ( 'ALL',   COUNT )
            CALL CHCKXC ( .FALSE., ' ',  OK )

            CALL CHCKSI ( 'COUNT', COUNT, '=', EXPCNT-I, 0, OK )

            DO J = 1, COUNT
          
               CALL KDATA  ( J,      'ALL',  FILE, FILTYP, 
     .                       SOURCE, HANDLE, FOUND        )
            
               CALL CHCKXC ( .FALSE., ' ',            OK )
               CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )

C
C              Compute K, the index in the original database
C              of the data for the Jth file.  The first
C              meta-kernel and entries for files it references
C              are unaffected by the UNLOADs.  Later files
C              have their data shifted as the unloads occur.
C

               IF ( J .LE. NMETA ) THEN
                  K = J
               ELSE
                  K = J + I
               END IF

               CALL CHCKSC ( 'FILE',   FILE,   '=', EXPFIL(K), OK )
               CALL CHCKSC ( 'FILTYP', FILTYP, '=', EXPTYP(K), OK )
               CALL CHCKSC ( 'SOURCE', SOURCE, '=', EXPSRC(K), OK )

            END DO

         END DO



      CALL TCASE ( 'Continue the previous test, this time ' 
     .//           'unloading the first meta-kernel.'       )

C
C        Unload the first meta-kernel.
C
         CALL UNLOAD ( 'zztst4.txt' )
         CALL CHCKXC ( .FALSE., ' ',  OK )

C
C        Check the database.  At this point all that
C        should be left are entries for the second
C        meta-kernel and its references.
C
         CALL KTOTAL ( 'ALL',   COUNT )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         CALL CHCKSI ( 'COUNT', COUNT, '=', EXPCNT-NMETA-NUFILS, 0, OK )

         DO J = 1, COUNT
          
            CALL KDATA  ( J,      'ALL',  FILE, FILTYP, 
     .                    SOURCE, HANDLE, FOUND        )
            
            CALL CHCKXC ( .FALSE., ' ',            OK )
            CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )

C
C           Compute K, the index in the original database
C           of the data for the Jth file.  The first
C           meta-kernel and entries for files it references
C           are unaffected by the UNLOADs.  Later files
C           have their data shifted as the unloads occur.
C
            K = J + NMETA + NUFILS

            CALL CHCKSC ( 'FILE',   FILE,   '=', EXPFIL(K), OK )
            CALL CHCKSC ( 'FILTYP', FILTYP, '=', EXPTYP(K), OK )
            CALL CHCKSC ( 'SOURCE', SOURCE, '=', EXPSRC(K), OK )

         END DO


C
C     Test KCLEAR.
C
      CALL TCASE ( 'Load meta-kernel, then individual files, '
     .//           'then another meta-kernel.  Unload all '
     .//           'files using KCLEAR.'                     )
     
C
C        Here's the first meta-kernel:
C
         CALL FURNSH ( 'zztst4.txt' )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         UFILES(1) = 'zz1spk.bsp'
         UFILES(2) = 'zz2spk.bsp'
         UFILES(3) = 'zzck1.bc'
         UFILES(4) = 'zztstek.be'
         UFILES(5) = 'zzleaps.ker'

         DO I = 1, NUFILS
            CALL FURNSH ( UFILES(I) )
            CALL CHCKXC ( .FALSE., ' ',  OK )
         END DO

C
C        Here's the second meta-kernel:
C
         CALL FURNSH ( 'zztst5.txt' )
         CALL CHCKXC ( .FALSE., ' ',  OK )

C
C        Clear out the keeper system:
C
         CALL KCLEAR
         CALL CHCKXC ( .FALSE., ' ',  OK )

C
C        Check the database entry count:  the database should
C        be empty.
C
         CALL KTOTAL ( 'ALL',   COUNT )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         CALL CHCKSI ( 'COUNT', COUNT, '=', 0, 0, OK )

C
C        Make sure the kernel pool is empty.
C        
         CALL GNPOOL ( '*', 1, 1, N, CVALS, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,  OK )

C
C        This is the end of the KCLEAR test.
C

       
C
C     Cleanup any debris left around from our test files.
C
 
      CALL CLPOOL ()
 
      CALL KILFIL ( 'zz1spk.bsp'  )
      CALL KILFIL ( 'zz2spk.bsp'  )
      CALL KILFIL ( 'zz3spk.bsp'  )
      CALL KILFIL ( 'zzck1.bc'    )
      CALL KILFIL ( 'zzck2.bc'    )
      CALL KILFIL ( 'zzsclk1.ker' )
      CALL KILFIL ( 'zzsclk2.ker' )
      CALL KILFIL ( 'zzleaps.ker' )
      CALL KILFIL ( 'zztst1.txt' )
      CALL KILFIL ( 'zztst2.txt' )
      CALL KILFIL ( 'zztst3.txt' )
      CALL KILFIL ( 'zztst4.txt' )
      CALL KILFIL ( 'zztst5.txt' )
      CALL KILFIL ( 'zztst6.txt' )
      CALL KILFIL ( 'zztstek.be'  )
 
 
      CALL T_SUCCESS ( OK )
      RETURN
      END
