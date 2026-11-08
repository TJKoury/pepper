C$Procedure F_TKFRAM ( Family Test of TKFRAM )

      SUBROUTINE F_TKFRAM ( OK )

C$ Abstract
C
C     Test the routine TKFRAM.
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
C     OK       is a logical that indicates the test status to the
C              caller.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  This routine does not generate any errors. Routines in its
C         call tree may generate errors that are either intentional and
C         trapped or unintentional and need reporting. The test family
C         utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine tests the frame system utility routine TKFRAM.
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
C     N.J. Bachman       (JPL)
C     J. Diaz del Rio    (ODC Space)
C     B.V. Semenov       (JPL)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    TSPICE Version 3.2.0, 20-AUG-2021 (BVS) (JDR) (NJB)
C
C        Added failure test cases competing keywords.
C
C        Updated misspelled short error message to
C        SPICE(INCOMPLETEFRAME).
C
C        Updated header and comments to comply with NAIF standard.
C        Added missing $Index_Entries.
C
C-    TSPICE Version 3.1.0, 08-JAN-2014 (BVS)
C
C        Added failure test case for a frame specified
C        relative to itself.
C
C        Renamed TKBFSZ to BUFSIZ and increased it from 20 to 200 to be
C        consistent with TKFRAM.
C
C        Changed to fetch MAXVAR via SZPOOL.
C
C        Replaced in-line numbers with BUFSIZ where applicable.
C
C-    TSPICE Version 3.0.0, 24-APR-2009 (NJB)
C
C        Added test cases to exercise watchers, in
C        particular to ensure that watchers are not
C        deleted improperly.
C
C-    TSPICE Version 2.0.0, 19-MAR-2009 (NJB)
C
C        Added test case to check watcher deletion.
C
C-    TSPICE Version 1.1.0, 20-OCT-1999 (WLT)
C
C        Declared PI to be an EXTERNAL Function.
C
C-&


C$ Index_Entries
C
C     test TKFRAM routine
C
C-&


C
C     Test Utility Functions
C
      CHARACTER*(16)        BEGTXT
      CHARACTER*(16)        BEGDAT

C
C     SPICELIB functions
C
      EXTERNAL              PI
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      RPD

C
C     Local parameters
C
      INTEGER               LBFSIZ
      PARAMETER           ( LBFSIZ = 300 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 132 )

      INTEGER               SMLSIZ
      PARAMETER           ( SMLSIZ =  30 )

C
C     BUFSIZ must be set to BUFSIZ used in TKFRAM.
C
      INTEGER               BUFSIZ
      PARAMETER           ( BUFSIZ = 200 )

C
C     Maximum size of angle, matrix lists.
C
      INTEGER               MAXSIZ
      PARAMETER           ( MAXSIZ = BUFSIZ * 2 + 5 )

C
C     Local variables
C
      CHARACTER*(LNSIZE)    BUFFER( SMLSIZ )
      CHARACTER*(LNSIZE)    LINES ( LBFSIZ )
      CHARACTER*(LNSIZE)    QNAME

      DOUBLE PRECISION      ANGIDN ( 3 )
      DOUBLE PRECISION      ANGLE1 ( MAXSIZ )
      DOUBLE PRECISION      ANGLE2 ( MAXSIZ )
      DOUBLE PRECISION      ANGLE3 ( MAXSIZ )
      DOUBLE PRECISION      EROT   ( 9 )
      DOUBLE PRECISION      QIDN   ( 4 )
      DOUBLE PRECISION      R      ( 3, 3, MAXSIZ )
      DOUBLE PRECISION      ROT    ( 3, 3 )
      DOUBLE PRECISION      ROTIDN ( 3, 3 )
      DOUBLE PRECISION      XROT   ( 3, 3 )

      INTEGER               AXES   ( 3 )
      INTEGER               EFRAME
      INTEGER               FRAME
      INTEGER               I
      INTEGER               ID
      INTEGER               J
      INTEGER               MAXVAR
      INTEGER               NLINES

      LOGICAL               FOUND

C
C     Saved variables
C
      SAVE


C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_TKFRAM' )
C
C     Clear the kernel pool so that we don't have to worry
C     about previous test cases contaminating the kernel pool.
C
      CALL CLPOOL

C
C     Get rid of any existing frame kernels.
C
      CALL KILFIL ( 'phoenix.tk'  )
      CALL KILFIL ( 'phoenix.prt' )

C
C     Create an I-kernel that we can load later on.
C
      LINES(  1 ) = 'This is a test TK Frame kernel for the f'
     .//            'ictional instrument TST_PHOENIX'
      LINES(  2 ) = 'on board the fictional spacecraft PHOENI'
     .//            'X.  A C-kernel for'
      LINES(  3 ) = 'the platform on which TST_PHOENIX is mou'
     .//            'nted can be generated'
      LINES(  4 ) = 'by calling the test utility TSTCK3.'
      LINES(  5 ) = ' '
      LINES(  6 ) = 'This kernel describes only the orientati'
     .//            'on attributes of the'
      LINES(  7 ) = 'TST_PHOENIX instrument.'
      LINES(  8 ) = ' '
      LINES(  9 ) = 'This kernel is intended only for test pu'
     .//            'rposes.  It is primarily'
      LINES( 10 ) = 'useful for testing the TKFRAM data fetch'
     .//            'ing routine.'
      LINES( 11 ) = ' '
      LINES( 12 ) = BEGDAT()
      LINES( 13 ) = ' '
      LINES( 14 ) = 'TKFRAME_-111111_SPEC      = ''MATRIX'''
      LINES( 15 ) = 'TKFRAME_-111111_RELATIVE  = ''PHOENIX'''
      LINES( 16 ) = ' '
      LINES( 17 ) = 'TKFRAME_-111111_MATRIX    = ( 0.48'
      LINES( 18 ) = ' 0.60'
      LINES( 19 ) = ' 0.64'
      LINES( 20 ) = '-0.8'
      LINES( 21 ) = ' 0.0'
      LINES( 22 ) = ' 0.6'
      LINES( 23 ) = ' 0.36'
      LINES( 24 ) = '-0.80'
      LINES( 25 ) = ' 0.48 )'
      LINES( 26 ) = ' '
      LINES( 27 ) = ' '
      LINES( 28 ) = 'TKFRAME_TST2-PHOENIX_SPEC      = ''ROTATION'''
      LINES( 29 ) = 'TKFRAME_TST2-PHOENIX_RELATIVE  = ''PHOENIX'''
      LINES( 30 ) = ' '
      LINES( 31 ) = 'TKFRAME_-111112_ROTATION  = ( 0.48'
      LINES( 32 ) = ' 0.60'
      LINES( 33 ) = ' 0.64'
      LINES( 34 ) = '-0.8'
      LINES( 35 ) = ' 0.0'
      LINES( 36 ) = ' 0.6'
      LINES( 37 ) = ' 0.36'
      LINES( 38 ) = '-0.80'
      LINES( 39 ) = ' 0.48 )'
      LINES( 40 ) = ' '
      LINES( 41 ) = ' '
      LINES( 42 ) = BEGTXT()
      LINES( 43 ) = ' '
      LINES( 44 ) = 'Next we need to supply the various bits '
     .//            'of frame identification for'
      LINES( 45 ) = 'this instrument.'
      LINES( 46 ) = ' '
      LINES( 47 ) = BEGDAT()
      LINES( 48 ) = ' '
      LINES( 49 ) = 'FRAME_-111111_CLASS    =  4'
      LINES( 50 ) = 'FRAME_-111111_CENTER   = -9'
      LINES( 51 ) = 'FRAME_-111111_CLASS_ID = -111111'
      LINES( 52 ) = 'FRAME_-111111_NAME     = ''TST-PHOENIX'''
      LINES( 53 ) = 'FRAME_TST-PHOENIX      = -111111'
      LINES( 54 ) = ' '
      LINES( 55 ) = 'FRAME_-9999_CLASS      =  3'
      LINES( 56 ) = 'FRAME_-9999_CENTER     = -9'
      LINES( 57 ) = 'FRAME_-9999_CLASS_ID   = -9999'
      LINES( 58 ) = 'FRAME_-9999_NAME       = ''PHOENIX'''
      LINES( 59 ) = 'FRAME_PHOENIX          = -9999'
      LINES( 60 ) = ' '
      LINES( 61 ) = 'FRAME_-111112_CLASS    =  4'
      LINES( 62 ) = 'FRAME_-111112_CENTER   = -9'
      LINES( 63 ) = 'FRAME_-111112_CLASS_ID = -111112'
      LINES( 64 ) = 'FRAME_-111112_NAME     = ''TST2-PHOENIX'''
      LINES( 65 ) = 'FRAME_TST2-PHOENIX     = -111112'
      LINES( 66 ) = ' '
      LINES( 67 ) = ' '
      LINES( 68 ) = ' '
      LINES( 69 ) = 'CK_-9999_SCLK          =  -9'
      LINES( 70 ) = 'CK_-9999_SPK           =  -9'
      LINES( 71 ) = ' '
      LINES( 72 ) = BEGTXT()
      LINES( 73 ) = ' '
      LINES( 74 ) = 'This frame is defined relative to itself. A no-no.'
      LINES( 75 ) = ' '
      LINES( 76 ) = BEGDAT()
      LINES( 77 ) = ' '
      LINES( 78 ) = 'FRAME_-111113_NAME        = ''TST3-PHOENIX'''
      LINES( 79 ) = 'FRAME_TST3-PHOENIX        = -111113'
      LINES( 80 ) = 'FRAME_-111113_CLASS       =  4'
      LINES( 81 ) = 'FRAME_-111113_CENTER      = -9'
      LINES( 82 ) = 'FRAME_-111113_CLASS_ID    = -111113'
      LINES( 83 ) = 'TKFRAME_-111113_SPEC      = ''MATRIX'''
      LINES( 84 ) = 'TKFRAME_-111113_RELATIVE  = ''TST3-PHOENIX'''
      LINES( 85 ) = 'TKFRAME_-111113_ROTATION  = ('
      LINES( 86 ) = '0.48 0.60 0.64 -0.8 0.0 0.6 0.36 -0.80 0.48 )'


      CALL IDENT  ( ROTIDN )
      CALL M2Q    ( ROTIDN, QIDN )
      CALL VPACK  ( 0.D0, 0.D0, 0.D0, ANGIDN )
      AXES(1) = 1
      AXES(2) = 2
      AXES(3) = 3

C
C     *****************************************************************
C
C        Error cases
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Check that a zero instrument ID causes '
     .//           'an error to be signaled. ' )


      ID = 0
      CALL TKFRAM  (  ID, ROT,  FRAME, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(ZEROFRAMEID)', OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Check that a proper error is signaled '
     .//           'if there is no frame data for the requested '
     .//           'frame. ' )

      ID = -10
      CALL TKFRAM  (  ID, ROT,  FRAME, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(INCOMPLETEFRAME)', OK    )
      CALL CHCKAD ( 'ROT', ROT, '=', ROTIDN, 9, 0.D0, OK )
      CALL CHCKSI ( 'FRAME', FRAME, '=', 0, 0, OK )
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Load a TK frame that is missing the '
     .//           'orientation information and check that the '
     .//           'deficiency is properly diagnosed.' )

      ID = -111111

      CALL CLPOOL
      CALL TSTTXT ( 'phoenix.prt', LINES, 53, .TRUE., .TRUE. )

      CALL TKFRAM  (  ID, ROT,  FRAME, FOUND )
      CALL CHCKXC  ( .TRUE., 'SPICE(BADFRAMESPEC)', OK    )
      CALL CHCKAD ( 'ROT', ROT, '=', ROTIDN, 9, 0.D0, OK )
      CALL CHCKSI ( 'FRAME', FRAME, '=', 0, 0, OK )
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Make sure that the proper error is signaled '
     .//           'if the frame ''SPEC'' is not recognized. ' )

      ID = -111112
      CALL CLPOOL
      CALL TSTTXT ( 'phoenix.tk',  LINES, 86, .TRUE., .TRUE. )
      CALL TKFRAM (  ID, ROT,  FRAME, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNFRAMESPEC)', OK )
      CALL CHCKAD ( 'ROT', ROT, '=', ROTIDN, 9, 0.D0, OK )
      CALL CHCKSI ( 'FRAME', FRAME, '=', 0, 0, OK )
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Make sure that the proper error is signaled '
     .//           'if the frame is defined relative to itself. ' )

      ID = -111113
      CALL TKFRAM (  ID, ROT,  FRAME, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(BADFRAMESPEC2)', OK )
      CALL CHCKAD ( 'ROT', ROT, '=', ROTIDN, 9, 0.D0, OK )
      CALL CHCKSI ( 'FRAME', FRAME, '=', 0, 0, OK )
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Make sure that the proper error is signaled '
     .//           'if there are competing _SPEC keywords' )

      CALL PCPOOL ( 'TKFRAME_TST3-PHOENIX_SPEC', 1, 'MATRIX' )

      ID = -111113
      CALL TKFRAM (  ID, ROT,  FRAME, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(COMPETINGFRAMESPEC)', OK )
      CALL CHCKAD ( 'ROT', ROT, '=', ROTIDN, 9, 0.D0, OK )
      CALL CHCKSI ( 'FRAME', FRAME, '=', 0, 0, OK )
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

      CALL DVPOOL ( 'TKFRAME_TST3-PHOENIX_SPEC' )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Make sure that the proper error is signaled '
     .//           'if there are competing _RELATIVE keywords' )

      CALL PCPOOL ( 'TKFRAME_-111113_RELATIVE', 1, 'J2000' )
      CALL PCPOOL ( 'TKFRAME_TST3-PHOENIX_RELATIVE', 1, 'J2000' )

      ID = -111113
      CALL TKFRAM (  ID, ROT,  FRAME, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(COMPETINGFRAMESPEC)', OK )
      CALL CHCKAD ( 'ROT', ROT, '=', ROTIDN, 9, 0.D0, OK )
      CALL CHCKSI ( 'FRAME', FRAME, '=', 0, 0, OK )
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

      CALL DVPOOL ( 'TKFRAME_TST3-PHOENIX_RELATIVE' )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Make sure that the proper error is signaled '
     .//           'if there are competing _MATRIX keywords' )

      CALL PCPOOL ( 'TKFRAME_-111113_SPEC', 1, 'MATRIX' )

      CALL PDPOOL ( 'TKFRAME_-111113_MATRIX', 9, ROTIDN )
      CALL PDPOOL ( 'TKFRAME_TST3-PHOENIX_MATRIX', 9, ROTIDN )

      ID = -111113
      CALL TKFRAM (  ID, ROT,  FRAME, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(COMPETINGFRAMESPEC)', OK )
      CALL CHCKAD ( 'ROT', ROT, '=', ROTIDN, 9, 0.D0, OK )
      CALL CHCKSI ( 'FRAME', FRAME, '=', 0, 0, OK )
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

      CALL DVPOOL ( 'TKFRAME_-111113_MATRIX' )
      CALL DVPOOL ( 'TKFRAME_TST3-PHOENIX_MATRIX' )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Make sure that the proper error is signaled '
     .//           'if there are competing _Q keywords' )

      CALL PCPOOL ( 'TKFRAME_-111113_SPEC', 1, 'QUATERNION' )

      CALL PDPOOL ( 'TKFRAME_-111113_Q', 4, QIDN )
      CALL PDPOOL ( 'TKFRAME_TST3-PHOENIX_Q', 4, QIDN )

      ID = -111113
      CALL TKFRAM (  ID, ROT,  FRAME, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(COMPETINGFRAMESPEC)', OK )
      CALL CHCKAD ( 'ROT', ROT, '=', ROTIDN, 9, 0.D0, OK )
      CALL CHCKSI ( 'FRAME', FRAME, '=', 0, 0, OK )
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

      CALL DVPOOL ( 'TKFRAME_-111113_Q' )
      CALL DVPOOL ( 'TKFRAME_TST3-PHOENIX_Q' )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Make sure that the proper error is signaled '
     .//           'if there are competing _ANGLES keywords' )

      CALL PCPOOL ( 'TKFRAME_-111113_SPEC', 1, 'ANGLES' )

      CALL PDPOOL ( 'TKFRAME_-111113_ANGLES', 3, ANGIDN  )
      CALL PDPOOL ( 'TKFRAME_TST3-PHOENIX_ANGLES', 3, ANGIDN  )
      CALL PIPOOL ( 'TKFRAME_-111113_AXES', 3, AXES )
      CALL PCPOOL ( 'TKFRAME_-111113_UNITS', 1, 'DEGREES' )

      ID = -111113
      CALL TKFRAM (  ID, ROT,  FRAME, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(COMPETINGFRAMESPEC)', OK )
      CALL CHCKAD ( 'ROT', ROT, '=', ROTIDN, 9, 0.D0, OK )
      CALL CHCKSI ( 'FRAME', FRAME, '=', 0, 0, OK )
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

      CALL DVPOOL ( 'TKFRAME_TST3-PHOENIX_ANGLES' )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Make sure that the proper error is signaled '
     .//           'if there are competing _AXES keywords' )

      CALL PCPOOL ( 'TKFRAME_-111113_SPEC', 1, 'ANGLES' )

      CALL PDPOOL ( 'TKFRAME_-111113_ANGLES', 3, ANGIDN  )
      CALL PIPOOL ( 'TKFRAME_-111113_AXES', 3, AXES )
      CALL PIPOOL ( 'TKFRAME_TST3-PHOENIX_AXES', 3, AXES )
      CALL PCPOOL ( 'TKFRAME_-111113_UNITS', 1, 'DEGREES' )

      ID = -111113
      CALL TKFRAM (  ID, ROT,  FRAME, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(COMPETINGFRAMESPEC)', OK )
      CALL CHCKAD ( 'ROT', ROT, '=', ROTIDN, 9, 0.D0, OK )
      CALL CHCKSI ( 'FRAME', FRAME, '=', 0, 0, OK )
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

      CALL DVPOOL ( 'TKFRAME_TST3-PHOENIX_AXES' )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Make sure that the proper error is signaled '
     .//           'if there are competing _UNITS keywords' )

      CALL PCPOOL ( 'TKFRAME_-111113_SPEC', 1, 'ANGLES' )

      CALL PDPOOL ( 'TKFRAME_-111113_ANGLES', 3, ANGIDN  )
      CALL PIPOOL ( 'TKFRAME_-111113_AXES', 3, AXES )
      CALL PCPOOL ( 'TKFRAME_-111113_UNITS', 1, 'DEGREES' )
      CALL PCPOOL ( 'TKFRAME_TST3-PHOENIX_UNITS', 1, 'DEGREES' )

      ID = -111113
      CALL TKFRAM (  ID, ROT,  FRAME, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(COMPETINGFRAMESPEC)', OK )
      CALL CHCKAD ( 'ROT', ROT, '=', ROTIDN, 9, 0.D0, OK )
      CALL CHCKSI ( 'FRAME', FRAME, '=', 0, 0, OK )
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

      CALL DVPOOL ( 'TKFRAME_TST3-PHOENIX_UNITS' )


C
C     *****************************************************************
C
C        Normal cases
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Determine the orientation of the instrument '
     .//           'frame relative to its C-kernel frame. ' )


      ID = -111111

      CALL TKFRAM (  ID, ROT,  FRAME, FOUND )
      CALL CHCKXC ( .FALSE.,         ' ',       OK    )

      EROT(1) =  0.48D0
      EROT(2) =  0.60D0
      EROT(3) =  0.64D0
      EROT(4) = -0.80D0
      EROT(5) =  0.00D0
      EROT(6) =  0.60D0
      EROT(7) =  0.36D0
      EROT(8) = -0.80D0
      EROT(9) =  0.48D0

      EFRAME  = -9999

      CALL CHCKAD ( 'ROT', ROT, '~', EROT, 9,  1.0D-14, OK )
      CALL CHCKSI ( 'FRAME', FRAME, '=', EFRAME, 0,     OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Make sure that we can successfully retrieve '
     .//           'rotations for 25 realistic frames.' )


      LINES(  1 ) = ' '
      LINES(  2 ) = 'This PCK file contains definitions for T'
     .   //            'OPOGRAPHIC reference'
      LINES(  3 ) = 'frames at 25 different observatories aro'
     .   //            'und the world.  Note'
      LINES(  4 ) = 'that the definition of these frames is a'
     .   //            'pproximate and that'
      LINES(  5 ) = 'they are accurate to only about 0.1 degr'
     .   //            'ees.'
      LINES(  6 ) = ' '
      LINES(  7 ) = BEGDAT()
      LINES(  8 ) = ' '
      LINES(  9 ) = 'FRAME_LAPLATA_TOPO     = 1000001'
      LINES( 10 ) = 'FRAME_CANBERRA_TOPO    = 1000002'
      LINES( 11 ) = 'FRAME_URANIA_TOPO      = 1000003'
      LINES( 12 ) = 'FRAME_VALONGO_TOPO     = 1000004'
      LINES( 13 ) = 'FRAME_LASCAMPANAS_TOPO = 1000005'
      LINES( 14 ) = 'FRAME_YUNNAN_TOPO      = 1000006'
      LINES( 15 ) = 'FRAME_QUITO_TOPO       = 1000007'
      LINES( 16 ) = 'FRAME_TUORLA_TOPO      = 1000008'
      LINES( 17 ) = 'FRAME_GRENOBLE_TOPO    = 1000009'
      LINES( 18 ) = 'FRAME_HAMBURG_TOPO     = 1000010'
      LINES( 19 ) = 'FRAME_MUNICH_TOPO      = 1000011'
      LINES( 20 ) = 'FRAME_KODAIKANAL_TOPO  = 1000012'
      LINES( 21 ) = 'FRAME_DUNSINK_TOPO     = 1000013'
      LINES( 22 ) = 'FRAME_TURIN_TOPO       = 1000014'
      LINES( 23 ) = 'FRAME_HIDA_TOPO        = 1000015'
      LINES( 24 ) = 'FRAME_ARECIBO_TOPO     = 1000016'
      LINES( 25 ) = 'FRAME_PULKOVO_TOPO     = 1000017'
      LINES( 26 ) = 'FRAME_BOYDEN_TOPO      = 1000018'
      LINES( 27 ) = 'FRAME_MADRID_TOPO      = 1000019'
      LINES( 28 ) = 'FRAME_AROSA_TOPO       = 1000020'
      LINES( 29 ) = 'FRAME_GREENWICH_TOPO   = 1000021'
      LINES( 30 ) = 'FRAME_KITTPEAK_TOPO    = 1000022'
      LINES( 31 ) = 'FRAME_GOLDSTONE_TOPO   = 1000023'
      LINES( 32 ) = 'FRAME_PALOMAR_TOPO     = 1000024'
      LINES( 33 ) = 'FRAME_USNO_TOPO        = 1000025'
      LINES( 34 ) = ' '
      LINES( 35 ) = ' '
      LINES( 36 ) = ' '
      LINES( 37 ) = 'FRAME_1000001_CLASS    =  4'
      LINES( 38 ) = 'FRAME_1000002_CLASS    =  4'
      LINES( 39 ) = 'FRAME_1000003_CLASS    =  4'
      LINES( 40 ) = 'FRAME_1000004_CLASS    =  4'
      LINES( 41 ) = 'FRAME_1000005_CLASS    =  4'
      LINES( 42 ) = 'FRAME_1000006_CLASS    =  4'
      LINES( 43 ) = 'FRAME_1000007_CLASS    =  4'
      LINES( 44 ) = 'FRAME_1000008_CLASS    =  4'
      LINES( 45 ) = 'FRAME_1000009_CLASS    =  4'
      LINES( 46 ) = 'FRAME_1000010_CLASS    =  4'
      LINES( 47 ) = 'FRAME_1000011_CLASS    =  4'
      LINES( 48 ) = 'FRAME_1000012_CLASS    =  4'
      LINES( 49 ) = 'FRAME_1000013_CLASS    =  4'
      LINES( 50 ) = 'FRAME_1000014_CLASS    =  4'
      LINES( 51 ) = 'FRAME_1000015_CLASS    =  4'
      LINES( 52 ) = 'FRAME_1000016_CLASS    =  4'
      LINES( 53 ) = 'FRAME_1000017_CLASS    =  4'
      LINES( 54 ) = 'FRAME_1000018_CLASS    =  4'
      LINES( 55 ) = 'FRAME_1000019_CLASS    =  4'
      LINES( 56 ) = 'FRAME_1000020_CLASS    =  4'
      LINES( 57 ) = 'FRAME_1000021_CLASS    =  4'
      LINES( 58 ) = 'FRAME_1000022_CLASS    =  4'
      LINES( 59 ) = 'FRAME_1000023_CLASS    =  4'
      LINES( 60 ) = 'FRAME_1000024_CLASS    =  4'
      LINES( 61 ) = 'FRAME_1000025_CLASS    =  4'
      LINES( 62 ) = ' '
      LINES( 63 ) = ' '
      LINES( 64 ) = 'FRAME_1000001_CENTER   =  399'
      LINES( 65 ) = 'FRAME_1000002_CENTER   =  399'
      LINES( 66 ) = 'FRAME_1000003_CENTER   =  399'
      LINES( 67 ) = 'FRAME_1000004_CENTER   =  399'
      LINES( 68 ) = 'FRAME_1000005_CENTER   =  399'
      LINES( 69 ) = 'FRAME_1000006_CENTER   =  399'
      LINES( 70 ) = 'FRAME_1000007_CENTER   =  399'
      LINES( 71 ) = 'FRAME_1000008_CENTER   =  399'
      LINES( 72 ) = 'FRAME_1000009_CENTER   =  399'
      LINES( 73 ) = 'FRAME_1000010_CENTER   =  399'
      LINES( 74 ) = 'FRAME_1000011_CENTER   =  399'
      LINES( 75 ) = 'FRAME_1000012_CENTER   =  399'
      LINES( 76 ) = 'FRAME_1000013_CENTER   =  399'
      LINES( 77 ) = 'FRAME_1000014_CENTER   =  399'
      LINES( 78 ) = 'FRAME_1000015_CENTER   =  399'
      LINES( 79 ) = 'FRAME_1000016_CENTER   =  399'
      LINES( 80 ) = 'FRAME_1000017_CENTER   =  399'
      LINES( 81 ) = 'FRAME_1000018_CENTER   =  399'
      LINES( 82 ) = 'FRAME_1000019_CENTER   =  399'
      LINES( 83 ) = 'FRAME_1000020_CENTER   =  399'
      LINES( 84 ) = 'FRAME_1000021_CENTER   =  399'
      LINES( 85 ) = 'FRAME_1000022_CENTER   =  399'
      LINES( 86 ) = 'FRAME_1000023_CENTER   =  399'
      LINES( 87 ) = 'FRAME_1000024_CENTER   =  399'
      LINES( 88 ) = 'FRAME_1000025_CENTER   =  399'
      LINES( 89 ) = ' '
      LINES( 90 ) = ' '
      LINES( 91 ) = 'FRAME_1000001_CLASS_ID = 1000001'
      LINES( 92 ) = 'FRAME_1000002_CLASS_ID = 1000002'
      LINES( 93 ) = 'FRAME_1000003_CLASS_ID = 1000003'
      LINES( 94 ) = 'FRAME_1000004_CLASS_ID = 1000004'
      LINES( 95 ) = 'FRAME_1000005_CLASS_ID = 1000005'
      LINES( 96 ) = 'FRAME_1000006_CLASS_ID = 1000006'
      LINES( 97 ) = 'FRAME_1000007_CLASS_ID = 1000007'
      LINES( 98 ) = 'FRAME_1000008_CLASS_ID = 1000008'
      LINES( 99 ) = 'FRAME_1000009_CLASS_ID = 1000009'
      LINES( 100 ) = 'FRAME_1000010_CLASS_ID = 1000010'
      LINES( 101 ) = 'FRAME_1000011_CLASS_ID = 1000011'
      LINES( 102 ) = 'FRAME_1000012_CLASS_ID = 1000012'
      LINES( 103 ) = 'FRAME_1000013_CLASS_ID = 1000013'
      LINES( 104 ) = 'FRAME_1000014_CLASS_ID = 1000014'
      LINES( 105 ) = 'FRAME_1000015_CLASS_ID = 1000015'
      LINES( 106 ) = 'FRAME_1000016_CLASS_ID = 1000016'
      LINES( 107 ) = 'FRAME_1000017_CLASS_ID = 1000017'
      LINES( 108 ) = 'FRAME_1000018_CLASS_ID = 1000018'
      LINES( 109 ) = 'FRAME_1000019_CLASS_ID = 1000019'
      LINES( 110 ) = 'FRAME_1000020_CLASS_ID = 1000020'
      LINES( 111 ) = 'FRAME_1000021_CLASS_ID = 1000021'
      LINES( 112 ) = 'FRAME_1000022_CLASS_ID = 1000022'
      LINES( 113 ) = 'FRAME_1000023_CLASS_ID = 1000023'
      LINES( 114 ) = 'FRAME_1000024_CLASS_ID = 1000024'
      LINES( 115 ) = 'FRAME_1000025_CLASS_ID = 1000025'
      LINES( 116 ) = ' '
      LINES( 117 ) = ' '
      LINES( 118 ) = 'FRAME_1000001_NAME     = ''LAPLATA_TOPO'
     .   //          ''''
      LINES( 119 ) = 'FRAME_1000002_NAME     = ''CANBERRA_TOP'
     .   //          'O'''
      LINES( 120 ) = 'FRAME_1000003_NAME     = ''URANIA_TOPO'''
      LINES( 121 ) = 'FRAME_1000004_NAME     = ''VALONGO_TOPO'
     .   //          ''''
      LINES( 122 ) = 'FRAME_1000005_NAME     = ''LASCAMPANAS_'
     .   //          'TOPO'''
      LINES( 123 ) = 'FRAME_1000006_NAME     = ''YUNNAN_TOPO'''
      LINES( 124 ) = 'FRAME_1000007_NAME     = ''QUITO_TOPO'''
      LINES( 125 ) = 'FRAME_1000008_NAME     = ''TUORLA_TOPO'''
      LINES( 126 ) = 'FRAME_1000009_NAME     = ''GRENOBLE_TOP'
     .   //           'O'''
      LINES( 127 ) = 'FRAME_1000010_NAME     = ''HAMBURG_TOPO'
     .   //          ''''
      LINES( 128 ) = 'FRAME_1000011_NAME     = ''MUNICH_TOPO'''
      LINES( 129 ) = 'FRAME_1000012_NAME     = ''KODAIKANAL_T'
     .   //          'OPO'''
      LINES( 130 ) = 'FRAME_1000013_NAME     = ''DUNSINK_TOPO'
     .   //          ''''
      LINES( 131 ) = 'FRAME_1000014_NAME     = ''TURIN_TOPO'''
      LINES( 132 ) = 'FRAME_1000015_NAME     = ''HIDA_TOPO'''
      LINES( 133 ) = 'FRAME_1000016_NAME     = ''ARECIBO_TOPO'
     .   //          ''''
      LINES( 134 ) = 'FRAME_1000017_NAME     = ''PULKOVO_TOPO'
     .   //          ''''
      LINES( 135 ) = 'FRAME_1000018_NAME     = ''BOYDEN_TOPO'''
      LINES( 136 ) = 'FRAME_1000019_NAME     = ''MADRID_TOPO'''
      LINES( 137 ) = 'FRAME_1000020_NAME     = ''AROSA_TOPO'''
      LINES( 138 ) = 'FRAME_1000021_NAME     = ''GREENWICH_TO'
     .   //          'PO'''
      LINES( 139 ) = 'FRAME_1000022_NAME     = ''KITTPEAK_TOP'
     .   //          'O'''
      LINES( 140 ) = 'FRAME_1000023_NAME     = ''GOLDSTONE_TO'
     .   //          'PO'''
      LINES( 141 ) = 'FRAME_1000024_NAME     = ''PALOMAR_TOPO'
     .   //          ''''
      LINES( 142 ) = 'FRAME_1000025_NAME     = ''USNO_TOPO'''
      LINES( 143 ) = ' '
      LINES( 144 ) = ' '
      LINES( 145 ) = 'TKFRAME_1000001_SPEC     = ''ANGLES'''
      LINES( 146 ) = 'TKFRAME_1000002_SPEC     = ''ANGLES'''
      LINES( 147 ) = 'TKFRAME_1000003_SPEC     = ''ANGLES'''
      LINES( 148 ) = 'TKFRAME_1000004_SPEC     = ''ANGLES'''
      LINES( 149 ) = 'TKFRAME_1000005_SPEC     = ''ANGLES'''
      LINES( 150 ) = 'TKFRAME_1000006_SPEC     = ''ANGLES'''
      LINES( 151 ) = 'TKFRAME_1000007_SPEC     = ''ANGLES'''
      LINES( 152 ) = 'TKFRAME_1000008_SPEC     = ''ANGLES'''
      LINES( 153 ) = 'TKFRAME_1000009_SPEC     = ''ANGLES'''
      LINES( 154 ) = 'TKFRAME_1000010_SPEC     = ''ANGLES'''
      LINES( 155 ) = 'TKFRAME_1000011_SPEC     = ''ANGLES'''
      LINES( 156 ) = 'TKFRAME_1000012_SPEC     = ''ANGLES'''
      LINES( 157 ) = 'TKFRAME_1000013_SPEC     = ''ANGLES'''
      LINES( 158 ) = 'TKFRAME_1000014_SPEC     = ''ANGLES'''
      LINES( 159 ) = 'TKFRAME_1000015_SPEC     = ''ANGLES'''
      LINES( 160 ) = 'TKFRAME_1000016_SPEC     = ''ANGLES'''
      LINES( 161 ) = 'TKFRAME_1000017_SPEC     = ''ANGLES'''
      LINES( 162 ) = 'TKFRAME_1000018_SPEC     = ''ANGLES'''
      LINES( 163 ) = 'TKFRAME_1000019_SPEC     = ''ANGLES'''
      LINES( 164 ) = 'TKFRAME_AROSA_TOPO_SPEC  = ''ANGLES'''
      LINES( 165 ) = 'TKFRAME_1000021_SPEC     = ''ANGLES'''
      LINES( 166 ) = 'TKFRAME_1000022_SPEC     = ''ANGLES'''
      LINES( 167 ) = 'TKFRAME_1000023_SPEC     = ''ANGLES'''
      LINES( 168 ) = 'TKFRAME_1000024_SPEC     = ''ANGLES'''
      LINES( 169 ) = 'TKFRAME_1000025_SPEC     = ''ANGLES'''
      LINES( 170 ) = ' '
      LINES( 171 ) = ' '
      LINES( 172 ) = 'TKFRAME_1000001_RELATIVE = ''IAU_EARTH'''
      LINES( 173 ) = 'TKFRAME_1000002_RELATIVE = ''IAU_EARTH'''
      LINES( 174 ) = 'TKFRAME_1000003_RELATIVE = ''IAU_EARTH'''
      LINES( 175 ) = 'TKFRAME_1000004_RELATIVE = ''IAU_EARTH'''
      LINES( 176 ) = 'TKFRAME_1000005_RELATIVE = ''IAU_EARTH'''
      LINES( 177 ) = 'TKFRAME_1000006_RELATIVE = ''IAU_EARTH'''
      LINES( 178 ) = 'TKFRAME_1000007_RELATIVE = ''IAU_EARTH'''
      LINES( 179 ) = 'TKFRAME_1000008_RELATIVE = ''IAU_EARTH'''
      LINES( 180 ) = 'TKFRAME_1000009_RELATIVE = ''IAU_EARTH'''
      LINES( 181 ) = 'TKFRAME_1000010_RELATIVE = ''IAU_EARTH'''
      LINES( 182 ) = 'TKFRAME_1000011_RELATIVE = ''IAU_EARTH'''
      LINES( 183 ) = 'TKFRAME_1000012_RELATIVE = ''IAU_EARTH'''
      LINES( 184 ) = 'TKFRAME_1000013_RELATIVE = ''IAU_EARTH'''
      LINES( 185 ) = 'TKFRAME_1000014_RELATIVE = ''IAU_EARTH'''
      LINES( 186 ) = 'TKFRAME_1000015_RELATIVE = ''IAU_EARTH'''
      LINES( 187 ) = 'TKFRAME_1000016_RELATIVE = ''IAU_EARTH'''
      LINES( 188 ) = 'TKFRAME_1000017_RELATIVE = ''IAU_EARTH'''
      LINES( 189 ) = 'TKFRAME_1000018_RELATIVE = ''IAU_EARTH'''
      LINES( 190 ) = 'TKFRAME_1000019_RELATIVE = ''IAU_EARTH'''
      LINES( 191 ) = 'TKFRAME_AROSA_TOPO_RELATIVE = ''IAU_EARTH'''
      LINES( 192 ) = 'TKFRAME_1000021_RELATIVE = ''IAU_EARTH'''
      LINES( 193 ) = 'TKFRAME_1000022_RELATIVE = ''IAU_EARTH'''
      LINES( 194 ) = 'TKFRAME_1000023_RELATIVE = ''IAU_EARTH'''
      LINES( 195 ) = 'TKFRAME_1000024_RELATIVE = ''IAU_EARTH'''
      LINES( 196 ) = 'TKFRAME_1000025_RELATIVE = ''IAU_EARTH'''
      LINES( 197 ) = ' '
      LINES( 198 ) = ' '
      LINES( 199 ) = 'TKFRAME_1000001_AXES     = (       3,  '
     .   //             '      2,   3 )'
      LINES( 200 ) = 'TKFRAME_1000002_AXES     = (       3,  '
     .   //             '      2,   3 )'
      LINES( 201 ) = 'TKFRAME_1000003_AXES     = (       3,  '
     .   //             '      2,   3 )'
      LINES( 202 ) = 'TKFRAME_1000004_AXES     = (       3,  '
     .   //             '      2,   3 )'
      LINES( 203 ) = 'TKFRAME_1000005_AXES     = (       3,  '
     .   //             '      2,   3 )'
      LINES( 204 ) = 'TKFRAME_1000006_AXES     = (       3,  '
     .   //             '      2,   3 )'
      LINES( 205 ) = 'TKFRAME_1000007_AXES     = (       3,  '
     .   //             '      2,   3 )'
      LINES( 206 ) = 'TKFRAME_1000008_AXES     = (       3,  '
     .   //             '      2,   3 )'
      LINES( 207 ) = 'TKFRAME_1000009_AXES     = (       3,  '
     .   //             '      2,   3 )'
      LINES( 208 ) = 'TKFRAME_1000010_AXES     = (       3,  '
     .   //             '      2,   3 )'
      LINES( 209 ) = 'TKFRAME_1000011_AXES     = (       3,  '
     .   //             '      2,   3 )'
      LINES( 210 ) = 'TKFRAME_1000012_AXES     = (       3,  '
     .   //             '      2,   3 )'
      LINES( 211 ) = 'TKFRAME_1000013_AXES     = (       3,  '
     .   //             '      2,   3 )'
      LINES( 212 ) = 'TKFRAME_1000014_AXES     = (       3,  '
     .   //             '      2,   3 )'
      LINES( 213 ) = 'TKFRAME_1000015_AXES     = (       3,  '
     .   //             '      2,   3 )'
      LINES( 214 ) = 'TKFRAME_1000016_AXES     = (       3,  '
     .   //             '      2,   3 )'
      LINES( 215 ) = 'TKFRAME_1000017_AXES =     (       3,  '
     .   //             '      2,   3 )'
      LINES( 216 ) = 'TKFRAME_BOYDEN_TOPO_AXES = (       3,  '
     .   //             '      2,   3 )'
      LINES( 217 ) = 'TKFRAME_1000019_AXES     = (       3,  '
     .   //             '      2,   3 )'
      LINES( 218 ) = 'TKFRAME_1000020_AXES     = (       3,  '
     .   //             '      2,   3 )'
      LINES( 219 ) = 'TKFRAME_1000021_AXES     = (       3,  '
     .   //             '      2,   3 )'
      LINES( 220 ) = 'TKFRAME_1000022_AXES     = (       3,  '
     .   //             '      2,   3 )'
      LINES( 221 ) = 'TKFRAME_1000023_AXES     = (       3,  '
     .   //             '      2,   3 )'
      LINES( 222 ) = 'TKFRAME_1000024_AXES     = (       3,  '
     .   //             '      2,   3 )'
      LINES( 223 ) = 'TKFRAME_1000025_AXES     = (       3,  '
     .   //             '      2,   3 )'
      LINES( 224 ) = ' '
      LINES( 225 ) = ' '
      LINES( 226 ) = 'TKFRAME_1000001_UNITS    = ''DEGREES'''
      LINES( 227 ) = 'TKFRAME_1000002_UNITS    = ''DEGREES'''
      LINES( 228 ) = 'TKFRAME_1000003_UNITS    = ''DEGREES'''
      LINES( 229 ) = 'TKFRAME_1000004_UNITS    = ''DEGREES'''
      LINES( 230 ) = 'TKFRAME_1000005_UNITS    = ''DEGREES'''
      LINES( 231 ) = 'TKFRAME_1000006_UNITS    = ''DEGREES'''
      LINES( 232 ) = 'TKFRAME_1000007_UNITS    = ''DEGREES'''
      LINES( 233 ) = 'TKFRAME_1000008_UNITS    = ''DEGREES'''
      LINES( 234 ) = 'TKFRAME_1000009_UNITS    = ''DEGREES'''
      LINES( 235 ) = 'TKFRAME_1000010_UNITS    = ''DEGREES'''
      LINES( 236 ) = 'TKFRAME_1000011_UNITS    = ''DEGREES'''
      LINES( 237 ) = 'TKFRAME_1000012_UNITS    = ''DEGREES'''
      LINES( 238 ) = 'TKFRAME_1000013_UNITS    = ''DEGREES'''
      LINES( 239 ) = 'TKFRAME_1000014_UNITS    = ''DEGREES'''
      LINES( 240 ) = 'TKFRAME_1000015_UNITS    = ''DEGREES'''
      LINES( 241 ) = 'TKFRAME_1000016_UNITS    = ''DEGREES'''
      LINES( 242 ) = 'TKFRAME_1000017_UNITS    = ''DEGREES'''
      LINES( 243 ) = 'TKFRAME_BOYDEN_TOPO_UNITS = ''DEGREES'''
      LINES( 244 ) = 'TKFRAME_1000019_UNITS    = ''DEGREES'''
      LINES( 245 ) = 'TKFRAME_1000020_UNITS    = ''DEGREES'''
      LINES( 246 ) = 'TKFRAME_1000021_UNITS    = ''DEGREES'''
      LINES( 247 ) = 'TKFRAME_1000022_UNITS    = ''DEGREES'''
      LINES( 248 ) = 'TKFRAME_1000023_UNITS    = ''DEGREES'''
      LINES( 249 ) = 'TKFRAME_1000024_UNITS    = ''DEGREES'''
      LINES( 250 ) = 'TKFRAME_1000025_UNITS    = ''DEGREES'''
      LINES( 251 ) = ' '
      LINES( 252 ) = ' '
      LINES( 253 ) = 'TKFRAME_1000001_ANGLES   = ( 57.9, -124'
     .   //             '.9, 180 )'
      LINES( 254 ) = 'TKFRAME_1000002_ANGLES   = ( -149.0, -1'
     .   //             '25.3, 180 )'
      LINES( 255 ) = 'TKFRAME_1000003_ANGLES   = ( -16.4, -41'
     .   //             '.8, 180 )'
      LINES( 256 ) = 'TKFRAME_1000004_ANGLES   = ( 43.2, -112'
     .   //             '.9, 180 )'
      LINES( 257 ) = 'TKFRAME_1000005_ANGLES   = ( 70.7, -123'
     .   //             '.5, 180 )'
      LINES( 258 ) = 'TKFRAME_1000006_ANGLES   = ( -102.8, -6'
     .   //             '5.0, 180 )'
      LINES( 259 ) = 'TKFRAME_1000007_ANGLES   = ( 78.5, -90.'
     .   //             '2, 180 )'
      LINES( 260 ) = 'TKFRAME_1000008_ANGLES   = ( -22.4, -29'
     .   //             '.6, 180 )'
      LINES( 261 ) = 'TKFRAME_1000009_ANGLES   = ( -05.9, -45'
     .   //             '.4, 180 )'
      LINES( 262 ) = 'TKFRAME_1000010_ANGLES   = ( -10.2, -36'
     .   //             '.5, 180 )'
      LINES( 263 ) = 'TKFRAME_1000011_ANGLES   = ( -11.6, -41'
     .   //             '.9, 180 )'
      LINES( 264 ) = 'TKFRAME_1000012_ANGLES   = ( -77.5, -79'
     .   //             '.8, 180 )'
      LINES( 265 ) = 'TKFRAME_1000013_ANGLES   = ( 06.3, -36.'
     .   //             '6, 180 )'
      LINES( 266 ) = 'TKFRAME_1000014_ANGLES   = ( -07.8, -45'
     .   //             '.0, 180 )'
      LINES( 267 ) = 'TKFRAME_1000015_ANGLES   = ( -137.6, -5'
     .   //             '3.7, 180 )'
      LINES( 268 ) = 'TKFRAME_1000016_ANGLES   = ( 66.8, -71.'
     .   //             '6, 180 )'
      LINES( 269 ) = 'TKFRAME_1000017_ANGLES   = ( -42.5, -46'
     .   //             '.3, 180 )'
      LINES( 270 ) = 'TKFRAME_1000018_ANGLES   = ( -26.6, -11'
     .   //             '9.0, 180 )'
      LINES( 271 ) = 'TKFRAME_1000019_ANGLES   = ( 03.7, -49.'
     .   //             '6, 180 )'
      LINES( 272 ) = 'TKFRAME_1000020_ANGLES   = ( -09.7, -43'
     .   //             '.2, 180 )'
      LINES( 273 ) = 'TKFRAME_1000021_ANGLES   = ( -00.1, -37'
     .   //             '.8, 180 )'
      LINES( 274 ) = 'TKFRAME_1000022_ANGLES   = ( 111.6, -58'
     .   //             '.0, 180 )'
      LINES( 275 ) = 'TKFRAME_1000023_ANGLES   = ( 116.8, -54'
     .   //             '.6, 180 )'
      LINES( 276 ) = 'TKFRAME_1000024_ANGLES   = ( 116.8, -56'
     .   //             '.7, 180 )'
      LINES( 277 ) = 'TKFRAME_1000025_ANGLES   = ( 77.1, -51.'
     .   //             '1, 180 )'

      CALL KILFIL ( 'topocentrc.frm' )
      CALL TSTTXT ( 'topocentrc.frm', LINES, 277, .TRUE., .TRUE. )

      CALL CHCKXC ( .FALSE., ' ',  OK )

      ANGLE1(  1 ) = 57.9D0 * RPD()
      ANGLE1(  2 ) = -149.0D0 * RPD()
      ANGLE1(  3 ) = -16.4D0 * RPD()
      ANGLE1(  4 ) = 43.2D0 * RPD()
      ANGLE1(  5 ) = 70.7D0 * RPD()
      ANGLE1(  6 ) = -102.8D0 * RPD()
      ANGLE1(  7 ) = 78.5D0 * RPD()
      ANGLE1(  8 ) = -22.4D0 * RPD()
      ANGLE1(  9 ) = -05.9D0 * RPD()
      ANGLE1( 10 ) = -10.2D0 * RPD()
      ANGLE1( 11 ) = -11.6D0 * RPD()
      ANGLE1( 12 ) = -77.5D0 * RPD()
      ANGLE1( 13 ) = 06.3D0 * RPD()
      ANGLE1( 14 ) = -07.8D0 * RPD()
      ANGLE1( 15 ) = -137.6D0 * RPD()
      ANGLE1( 16 ) = 66.8D0 * RPD()
      ANGLE1( 17 ) = -42.5D0 * RPD()
      ANGLE1( 18 ) = -26.6D0 * RPD()
      ANGLE1( 19 ) = 03.7D0 * RPD()
      ANGLE1( 20 ) = -09.7D0 * RPD()
      ANGLE1( 21 ) = -00.1D0 * RPD()
      ANGLE1( 22 ) = 111.6D0 * RPD()
      ANGLE1( 23 ) = 116.8D0 * RPD()
      ANGLE1( 24 ) = 116.8D0 * RPD()
      ANGLE1( 25 ) = 77.1D0 * RPD()


      ANGLE2(  1 ) = -124.9D0 * RPD()
      ANGLE2(  2 ) = -125.3D0 * RPD()
      ANGLE2(  3 ) = -41.8D0 * RPD()
      ANGLE2(  4 ) = -112.9D0 * RPD()
      ANGLE2(  5 ) = -123.5D0 * RPD()
      ANGLE2(  6 ) = -65.0D0 * RPD()
      ANGLE2(  7 ) = -90.2D0 * RPD()
      ANGLE2(  8 ) = -29.6D0 * RPD()
      ANGLE2(  9 ) = -45.4D0 * RPD()
      ANGLE2( 10 ) = -36.5D0 * RPD()
      ANGLE2( 11 ) = -41.9D0 * RPD()
      ANGLE2( 12 ) = -79.8D0 * RPD()
      ANGLE2( 13 ) = -36.6D0 * RPD()
      ANGLE2( 14 ) = -45.0D0 * RPD()
      ANGLE2( 15 ) = -53.7D0 * RPD()
      ANGLE2( 16 ) = -71.6D0 * RPD()
      ANGLE2( 17 ) = -46.3D0 * RPD()
      ANGLE2( 18 ) = -119.0D0 * RPD()
      ANGLE2( 19 ) = -49.6D0 * RPD()
      ANGLE2( 20 ) = -43.2D0 * RPD()
      ANGLE2( 21 ) = -37.8D0 * RPD()
      ANGLE2( 22 ) = -58.0D0 * RPD()
      ANGLE2( 23 ) = -54.6D0 * RPD()
      ANGLE2( 24 ) = -56.7D0 * RPD()
      ANGLE2( 25 ) = -51.1D0 * RPD()

      DO I = 1, 25
         ANGLE3(I) = PI()
      END DO


      DO I = 1, 25
         CALL EUL2M ( ANGLE1(I), ANGLE2(I), ANGLE3(I),
     .                        3,         2,         3, R(1,1,I) )
      END DO

      DO J = 1, 3
         DO I = 1, 25

            ID = 1000000 + I
            CALL TKFRAM (  ID, ROT,  FRAME, FOUND )

            CALL CHCKXC ( .FALSE., ' ',  OK )
            CALL CHCKSI ( 'FRAME', FRAME, '=', 10013, 0, OK )
            CALL CHCKAD ( 'ROT', ROT, '~', R(1,1,I), 9, 1.0D-13, OK )

         END DO
      END DO


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Make sure we can get the right answer if we '
     .//           'ask for the same frame several times in a '
     .//           'row. ' )


      DO I = 1, 3
         CALL TKFRAM ( 1000003, ROT, FRAME, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'FRAME', FRAME, '=', 10013, 0, OK )
         CALL CHCKAD ( 'ROT', ROT, '~', R(1,1,3), 9, 1.0D-13, OK )
      END DO


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Make sure we don''t overflow the watcher '
     .//           'system if we define a very large number '
     .//           'of frames.'                              )

C
C     Fetch the number of POOL variables.
C
      CALL SZPOOL ( 'MAXVAR', MAXVAR, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'SZPOOL FOUND', FOUND, .TRUE., OK )

      NLINES = 17

      LINES(  1 ) = 'FRAME_#_NAME        = ''TKF_#'''
      LINES(  2 ) = 'FRAME_TKF_#         = #'
      LINES(  3 ) = 'FRAME_#_CLASS       = 4'
      LINES(  4 ) = 'FRAME_#_CENTER      = 9'
      LINES(  5 ) = 'FRAME_#_CLASS_ID    = #'
      LINES(  6 ) = 'TKFRAME_#_SPEC      = ''MATRIX'''
      LINES(  7 ) = 'TKFRAME_#_RELATIVE  = ''J2000'''
      LINES(  8 ) = ' '
      LINES(  9 ) = 'TKFRAME_#_MATRIX    = ( 0.48'
      LINES( 10 ) = ' 0.60'
      LINES( 11 ) = ' 0.64'
      LINES( 12 ) = '-0.8'
      LINES( 13 ) = ' 0.0'
      LINES( 14 ) = ' 0.6'
      LINES( 15 ) = ' 0.36'
      LINES( 16 ) = '-0.80'
      LINES( 17 ) = ' 0.48 )'

C
C     We can fit up to MAXVAR/8 TK frames in the POOL before
C     we run out POOL variables.
C
      DO I = 1, MAXVAR / 8

         ID = 5000000 + I
C
C        Create a frame definition by filling the frame
C        ID into the template.
C
         DO J = 1, NLINES

            CALL REPMI  ( LINES(J), '#', ID, BUFFER(J) )
            CALL CHCKXC ( .FALSE., ' ',  OK )

            CALL REPMI  ( BUFFER(J), '#', ID, BUFFER(J) )
            CALL CHCKXC ( .FALSE., ' ',  OK )

         END DO

C
C        Load the frame specification into the kernel pool.
C
         CALL LMPOOL ( BUFFER, NLINES )
         CALL CHCKXC ( .FALSE., ' ',  OK )

C
C        Look up frame data for the current frame.
C
         CALL TKFRAM (  ID, ROT, FRAME, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         IF ( MOD( I, BUFSIZ ) .EQ. 0 ) THEN
C
C           Clear the kernel pool so we don't run out of
C           room for kernel variables before we run out
C           of watchers.
C
            CALL CLPOOL
            CALL CHCKXC ( .FALSE., ' ',  OK )

         END IF

      END DO


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Verify that buffered frame data, '
     .//           'for the number of frames equal to the '
     .//           'TKFRAM buffer size, are updated when watchers '
     .//           'indicate a kernel pool update.'  )


C
C     Clear the kernel pool, then insert definitions
C     of BUFSIZ TK frames into the pool.
C
      CALL CLPOOL
      CALL CHCKXC ( .FALSE., ' ',  OK )

      NLINES = 11

      LINES(  1 ) = 'FRAME_#_NAME        = ''TKF_#'''
      LINES(  2 ) = 'FRAME_TKF_#         = #'
      LINES(  3 ) = 'FRAME_#_CLASS       = 4'
      LINES(  4 ) = 'FRAME_#_CENTER      = 9'
      LINES(  5 ) = 'FRAME_#_CLASS_ID    = #'
      LINES(  6 ) = 'TKFRAME_#_SPEC      = ''ANGLES'''
      LINES(  7 ) = 'TKFRAME_#_RELATIVE  = ''J2000'''
      LINES(  8 ) = ' '
      LINES(  9 ) = 'TKFRAME_#_AXES   = ( 3, 1, 3 )'
      LINES( 10 ) = 'TKFRAME_#_UNITS  = ''DEGREES'''
      LINES( 11 ) = 'TKFRAME_#_ANGLES = ( @ @ @ )'

      DO I = 1, BUFSIZ

         ID = 5000000 + I
C
C        Create a frame definition by filling the frame
C        ID into the template.
C
         DO J = 1, NLINES

            CALL REPMI  ( LINES(J), '#', ID, BUFFER(J) )
            CALL CHCKXC ( .FALSE., ' ',  OK )

            CALL REPMI  ( BUFFER(J), '#', ID, BUFFER(J) )
            CALL CHCKXC ( .FALSE., ' ',  OK )

         END DO

C
C        Give each frame a different orientation.
C
         ANGLE1(I) = ( 100 + I ) / 100.D0
         ANGLE2(I) = ( 200 + I ) / 100.D0
         ANGLE3(I) = ( 300 + I ) / 100.D0

         J = 11
         CALL REPMD ( BUFFER(J), '@', ANGLE1(I), 14, BUFFER(J) )
         CALL REPMD ( BUFFER(J), '@', ANGLE2(I), 14, BUFFER(J) )
         CALL REPMD ( BUFFER(J), '@', ANGLE3(I), 14, BUFFER(J) )
C
C        Load the frame specification into the kernel pool.
C
         CALL LMPOOL ( BUFFER, NLINES )
         CALL CHCKXC ( .FALSE., ' ',  OK )

      END DO

C
C     Verify that the expected frame definitions are
C     available.
C
      DO I = 1, BUFSIZ

         DO J = 1, 2
C
C           Look up frame data for the current frame.
C
            ID = 5000000 + I
            CALL TKFRAM (  ID, ROT, FRAME, FOUND )
            CALL CHCKXC ( .FALSE., ' ',  OK )

C
C           Create the expected matrix.
C
            CALL EUL2M ( ANGLE1(I)*RPD(),
     .                   ANGLE2(I)*RPD(),
     .                   ANGLE3(I)*RPD(), 3, 1, 3, XROT )

            CALL CHCKXC ( .FALSE., ' ',  OK )

            QNAME = 'ROT(#,#) (pass 1)'
            CALL REPMI  ( QNAME, '#', I, QNAME )
            CALL REPMI  ( QNAME, '#', J, QNAME )
            CALL CHCKXC ( .FALSE., ' ',  OK )

            CALL CHCKAD ( QNAME, ROT, '~', XROT, 9, 1.D-12, OK )

         END DO

      END DO

C
C     Now update all of the frame definitions.
C
      DO I = 1, BUFSIZ

         ID = 5000000 + I
C
C        Create a frame definition by filling the frame
C        ID into the template.
C
         DO J = 1, NLINES

            CALL REPMI  ( LINES(J), '#', ID, BUFFER(J) )
            CALL CHCKXC ( .FALSE., ' ',  OK )

            CALL REPMI  ( BUFFER(J), '#', ID, BUFFER(J) )
            CALL CHCKXC ( .FALSE., ' ',  OK )

         END DO

C
C        Give each frame a different orientation.
C
         ANGLE1(I) = ( 100 + I ) / 100.D0
         ANGLE2(I) = ( 400 + I ) / 100.D0
         ANGLE3(I) = ( 800 + I ) / 100.D0

         J = 11
         CALL REPMD ( BUFFER(J), '@', ANGLE1(I), 14, BUFFER(J) )
         CALL REPMD ( BUFFER(J), '@', ANGLE2(I), 14, BUFFER(J) )
         CALL REPMD ( BUFFER(J), '@', ANGLE3(I), 14, BUFFER(J) )
C
C        Load the frame specification into the kernel pool.
C
         CALL LMPOOL ( BUFFER, NLINES )
         CALL CHCKXC ( .FALSE., ' ',  OK )

      END DO


C
C     Verify that the expected frame definitions are
C     available. At this point, we're relying on the
C     watchers set in TKFRAM to indicate the need to
C     fetch new data from the kernel pool.
C
      DO I = 1, BUFSIZ

         DO J = 1, 2
C
C           Look up frame data for the current frame.
C
            ID = 5000000 + I
            CALL TKFRAM (  ID, ROT, FRAME, FOUND )
            CALL CHCKXC ( .FALSE., ' ',  OK )

C
C           Create the expected matrix.
C
            CALL EUL2M ( ANGLE1(I)*RPD(),
     .                   ANGLE2(I)*RPD(),
     .                   ANGLE3(I)*RPD(), 3, 1, 3, XROT )

            CALL CHCKXC ( .FALSE., ' ',  OK )

            QNAME = 'ROT(#,#) (pass 2)'
            CALL REPMI  ( QNAME, '#', I, QNAME )
            CALL REPMI  ( QNAME, '#', J, QNAME )
            CALL CHCKXC ( .FALSE., ' ',  OK )

            CALL CHCKAD ( QNAME, ROT, '~', XROT, 9, 1.D-12, OK )

         END DO

      END DO


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Repeat watcher test '
     .//           'for the number of frames greater than the '
     .//           'TKFRAM buffer size' )
C
C     Clear the kernel pool, then insert definitions
C     of MAXSIZ TK frames into the pool.
C
      CALL CLPOOL
      CALL CHCKXC ( .FALSE., ' ',  OK )

      NLINES = 11

      LINES(  1 ) = 'FRAME_#_NAME        = ''TKF_#'''
      LINES(  2 ) = 'FRAME_TKF_#         = #'
      LINES(  3 ) = 'FRAME_#_CLASS       = 4'
      LINES(  4 ) = 'FRAME_#_CENTER      = 9'
      LINES(  5 ) = 'FRAME_#_CLASS_ID    = #'
      LINES(  6 ) = 'TKFRAME_#_SPEC      = ''ANGLES'''
      LINES(  7 ) = 'TKFRAME_#_RELATIVE  = ''J2000'''
      LINES(  8 ) = ' '
      LINES(  9 ) = 'TKFRAME_#_AXES   = ( 3, 1, 3 )'
      LINES( 10 ) = 'TKFRAME_#_UNITS  = ''DEGREES'''
      LINES( 11 ) = 'TKFRAME_#_ANGLES = ( @ @ @ )'

      DO I = 1, MAXSIZ

         ID = 5000000 + I
C
C        Create a frame definition by filling the frame
C        ID into the template.
C
         DO J = 1, NLINES

            CALL REPMI  ( LINES(J), '#', ID, BUFFER(J) )
            CALL CHCKXC ( .FALSE., ' ',  OK )

            CALL REPMI  ( BUFFER(J), '#', ID, BUFFER(J) )
            CALL CHCKXC ( .FALSE., ' ',  OK )

         END DO

C
C        Give each frame a different orientation.
C
         ANGLE1(I) = ( 100 + I ) / 100.D0
         ANGLE2(I) = ( 200 + I ) / 100.D0
         ANGLE3(I) = ( 300 + I ) / 100.D0

         J = 11
         CALL REPMD ( BUFFER(J), '@', ANGLE1(I), 14, BUFFER(J) )
         CALL REPMD ( BUFFER(J), '@', ANGLE2(I), 14, BUFFER(J) )
         CALL REPMD ( BUFFER(J), '@', ANGLE3(I), 14, BUFFER(J) )
C
C        Load the frame specification into the kernel pool.
C
         CALL LMPOOL ( BUFFER, NLINES )
         CALL CHCKXC ( .FALSE., ' ',  OK )

      END DO

C
C     Verify that the expected frame definitions are
C     available.
C
      DO I = 1, MAXSIZ

         DO J = 1, 2
C
C           Look up frame data for the current frame.
C
            ID = 5000000 + I
            CALL TKFRAM (  ID, ROT, FRAME, FOUND )
            CALL CHCKXC ( .FALSE., ' ',  OK )

            QNAME = 'ROT(#,#) (pass 1)'
            CALL REPMI  ( QNAME, '#', I, QNAME )
            CALL REPMI  ( QNAME, '#', J, QNAME )
            CALL CHCKXC ( .FALSE., ' ',  OK )

            CALL CHCKSL ( QNAME//' FOUND', FOUND, .TRUE., OK )

            IF ( OK ) THEN
C
C              Create the expected matrix.
C
               CALL EUL2M ( ANGLE1(I)*RPD(),
     .                      ANGLE2(I)*RPD(),
     .                      ANGLE3(I)*RPD(), 3, 1, 3, XROT )

               CALL CHCKXC ( .FALSE., ' ',  OK )

               CALL CHCKAD ( QNAME, ROT, '~', XROT, 9, 1.D-12, OK )

            END IF

         END DO

      END DO

C
C     Now update all of the frame definitions.
C
      DO I = 1, MAXSIZ

         ID = 5000000 + I
C
C        Create a frame definition by filling the frame
C        ID into the template.
C
         DO J = 1, NLINES

            CALL REPMI  ( LINES(J), '#', ID, BUFFER(J) )
            CALL CHCKXC ( .FALSE., ' ',  OK )

            CALL REPMI  ( BUFFER(J), '#', ID, BUFFER(J) )
            CALL CHCKXC ( .FALSE., ' ',  OK )

         END DO

C
C        Give each frame a different orientation.
C
         ANGLE1(I) = ( 100 + I ) / 100.D0
         ANGLE2(I) = ( 400 + I ) / 100.D0
         ANGLE3(I) = ( 800 + I ) / 100.D0

         J = 11
         CALL REPMD ( BUFFER(J), '@', ANGLE1(I), 14, BUFFER(J) )
         CALL REPMD ( BUFFER(J), '@', ANGLE2(I), 14, BUFFER(J) )
         CALL REPMD ( BUFFER(J), '@', ANGLE3(I), 14, BUFFER(J) )
C
C        Load the frame specification into the kernel pool.
C
         CALL LMPOOL ( BUFFER, NLINES )
         CALL CHCKXC ( .FALSE., ' ',  OK )

      END DO


C
C     Verify that the expected frame definitions are
C     available. At this point, we're relying on the
C     watchers set in TKFRAM to indicate the need to
C     fetch new data from the kernel pool.
C
      DO I = 1, MAXSIZ

         DO J = 1, 2
C
C           Look up frame data for the current frame.
C
            ID = 5000000 + I
            CALL TKFRAM (  ID, ROT, FRAME, FOUND )
            CALL CHCKXC ( .FALSE., ' ',  OK )

C
C           Create the expected matrix.
C
            CALL EUL2M ( ANGLE1(I)*RPD(),
     .                   ANGLE2(I)*RPD(),
     .                   ANGLE3(I)*RPD(), 3, 1, 3, XROT )

            CALL CHCKXC ( .FALSE., ' ',  OK )

            QNAME = 'ROT(#,#) (pass 2)'
            CALL REPMI  ( QNAME, '#', I, QNAME )
            CALL REPMI  ( QNAME, '#', J, QNAME )
            CALL CHCKXC ( .FALSE., ' ',  OK )

            CALL CHCKAD ( QNAME, ROT, '~', XROT, 9, 1.D-12, OK )

         END DO

      END DO


C
C     That's all.  Clean up the kernel files we created.
C
      CALL KILFIL ( 'phoenix.tk'  )
      CALL KILFIL ( 'phoenix.prt' )
      CALL KILFIL ( 'topocentrc.frm' )

      CALL T_SUCCESS ( OK )
      RETURN
      END
