C$Procedure      F_ROTGET ( Family of tests for ROTGET )
 
      SUBROUTINE F_ROTGET ( OK )
      IMPLICIT NONE
 
      LOGICAL               OK
 
C$ Abstract
C
C     This routine tests the primary frame utility ROTGET.
C     It makes sure that all 4 frame classes are recognized
C     and that the routine retrieves the correct frame information
C     for each of them.
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
C-&
 
 
C
C     Test Utility Functions
C
      CHARACTER*(16)        BEGDAT
      CHARACTER*(16)        BEGTXT
 
C
C     SPICELIB Functions
C
 
C
C     Local Variables
C
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )
 
      INTEGER               NLINES
      PARAMETER           ( NLINES = 66 )
 
      INTEGER               DEFSIZ
      PARAMETER           ( DEFSIZ = 15 )

      CHARACTER*(LNSIZE)    DEFTXT ( DEFSIZ )
      CHARACTER*(LNSIZE)    ERROR
      CHARACTER*(LNSIZE)    LINES  ( NLINES )
      
      DOUBLE PRECISION      AV     ( 3    )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ROT    ( 3, 3 )
      DOUBLE PRECISION      TIPM   ( 3, 3 )
      DOUBLE PRECISION      TPMI   ( 3, 3 )
      DOUBLE PRECISION      TMPROT ( 3, 3 )
      DOUBLE PRECISION      ROTAT  ( 3, 3 )
 
      INTEGER               CENT
      INTEGER               CLASS
      INTEGER               CLSSID
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               ID
      INTEGER               J
      INTEGER               NTEXT
      INTEGER               OUTFRM
      INTEGER               XPCTD ( 3 )
      INTEGER               XCENT
      INTEGER               XCLASS
      INTEGER               XCLSID
      LOGICAL               FOUND
 
 
 
C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_ROTGET' )
 
 
      LINES(  1 ) = 'This is a test I-kernel for the fictional '
     .//            'instrument TST_PHEONIX '
      LINES(  2 ) = 'on board the fictional spacecraft PHEONIX. '
     .//            'A C-kernel for '
      LINES(  3 ) = 'the platform on which TST_PHEONIX is '
     .//            'mounted can be generated '
      LINES(  4 ) = 'by calling the test utility TSTCKN. '
      LINES(  5 ) = ' '
      LINES(  6 ) = 'This kernel describes only the mode '
     .//            'independent attributes of the '
      LINES(  7 ) = 'TST_PHOENIX instrument. '
      LINES(  8 ) = ' '
      LINES(  9 ) = 'This kernel is intended only for test '
     .//            'purposes.  It is primarily '
      LINES( 10 ) = 'useful for testing the I-kernel data '
     .//            'fetching routines '
      LINES( 11 ) = ' '
      LINES( 12 ) =  BEGDAT()
      LINES( 13 ) = 'TKFRAME_-111111_SPEC = '
     .//            '''MATRIX'' '
      LINES( 14 ) = ' '
      LINES( 15 ) = ' '
      LINES( 16 ) = '  '
      LINES( 17 ) = ' '
      LINES( 18 ) = ' '
      LINES( 19 ) = ' '
      LINES( 20 ) = ' '
      LINES( 21 ) = ' '
      LINES( 22 ) = 'TKFRAME_-111111_RELATIVE     = ''PHOENIX'' '
      LINES( 23 ) = ' '
      LINES( 24 ) = ' '
      LINES( 25 ) = ' '
      LINES( 26 ) = ' '
      LINES( 27 ) = ' '
      LINES( 28 ) = ' '
      LINES( 29 ) = 'TKFRAME_-111111_MATRIX    = ( 0.48 '
      LINES( 30 ) = '0.60 '
      LINES( 31 ) = '0.64 '
      LINES( 32 ) = '-0.8 '
      LINES( 33 ) = '0.0 '
      LINES( 34 ) = '0.6 '
      LINES( 35 ) = '0.36 '
      LINES( 36 ) = '-0.80 '
      LINES( 37 ) = '0.48 ) '
      LINES( 38 ) = ' '
      LINES( 39 ) = ' '
      LINES( 40 ) = ' '
      LINES( 41 ) =  BEGTXT()
      LINES( 42 ) = ' '
      LINES( 43 ) = 'Next we need to supply the various bits of '
     .//            'frame identification for '
      LINES( 44 ) = 'this instrument. '
      LINES( 45 ) = ' '
      LINES( 46 ) =  BEGDAT()
      LINES( 47 ) = 'FRAME_-222222_CLASS    =  7 '
      LINES( 48 ) = 'FRAME_-222222_CENTER   = -9 '
      LINES( 49 ) = 'FRAME_-222222_CLASS_ID = -1 '
      LINES( 50 ) = 'FRAME_-222222_NAME     = ''UNSUPPORTED'' '
      LINES( 51 ) = 'FRAME_UNSUPPORTED      = -222222 '
      LINES( 52 ) = ' '
      LINES( 53 ) = 'FRAME_-111111_CLASS    =  4 '
      LINES( 54 ) = 'FRAME_-111111_CENTER   = -9 '
      LINES( 55 ) = 'FRAME_-111111_CLASS_ID = -111111 '
      LINES( 56 ) = 'FRAME_-111111_NAME     = ''TST-PHOENIX'' '
      LINES( 57 ) = 'FRAME_TST-PHOENIX      = -111111 '
      LINES( 58 ) = ' '
      LINES( 59 ) = 'FRAME_-9999_CLASS      =  3 '
      LINES( 60 ) = 'FRAME_-9999_CENTER     = -9 '
      LINES( 61 ) = 'FRAME_-9999_CLASS_ID   = -9999 '
      LINES( 62 ) = 'FRAME_-9999_NAME       = ''PHOENIX'' '
      LINES( 63 ) = 'FRAME_PHOENIX          = -9999 '
      LINES( 64 ) = ' '
      LINES( 65 ) = 'CK_-9999_SCLK          =  -9 '
      LINES( 66 ) = 'CK_-9999_SPK           =  -9 '
 
C
C     Clean out the kernel pool. Just in case we have something
C     already loaded there.
C
      CALL CLPOOL
C
C     Create and load the test kernels that will be needed during
C     this test.
C
      CALL TCASE ( 'Make sure we can load everything.' )
 
         CALL TSTPCK ( 'test.pck',                   .TRUE., .FALSE. )
         CALL TSTCKN ( 'phoenix.bc',  'phoenix.tsc', .FALSE.,
     .                                               .TRUE., .FALSE.,
     .                                                HANDLE )
         CALL TSTTXT ( 'phoenix2.ik', LINES, NLINES, .TRUE., .FALSE. )
         CALL CHCKXC ( .FALSE., ' ', OK )
 
         CALL CKLPF  ( 'phoenix.bc',   HANDLE         )
         CALL TPARSE ( '1 Jan 1995',   ET,     ERROR  )
 
 
      CALL TCASE ( 'Determine that a frame of unrecognized class '
     .//           'causes the correct exception to be '
     .//           'signalled. ' )
 
 
         CALL ROTGET (  -222222, ET, ROTAT, OUTFRM, FOUND )
         CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNFRAMETYPE)', OK )
 
 
      CALL TCASE ( 'Verify that all inertial frames are '
     .//           'recognized and that they produce the '
     .//           'expected transformation. ' )
 
         DO I = 1, 18
 
            CALL ROTGET ( I, ET, ROTAT, OUTFRM, FOUND )
            CALL IRFROT ( I, 1,  ROT                  )
 
 
            J = I
            CALL TSTMSG ( '#', 'Subcase. Frame ID = # ' )
            CALL TSTMSI (  J  )
 
            CALL CHCKXC ( .FALSE.,  ' ',                            OK )
            CALL CHCKSL ( 'FOUND',  FOUND,      .TRUE.,             OK )
            CALL CHCKSI ( 'OUTFRM', OUTFRM, '=', 1,          0,     OK )
            CALL CHCKAD ( 'ROTAT',  ROTAT,  '=', ROT,     9, 0.0D0, OK )
 
         END DO
C
C        Clear the extra message.
C
         CALL TSTMSG ( '#', ' ' )
 
      CALL TCASE ( 'Verify that inertial class frames that are '
     .//           'not yet supported are not found by ROTGET. ' )
 
 
         CALL ROTGET ( 99, ET, ROTAT, OUTFRM, FOUND )
 
         CALL CHCKXC ( .FALSE., ' ',             OK )
         CALL CHCKSL ( 'FOUND',  FOUND, .FALSE., OK )
 
      CALL TCASE ( 'Verify that the planetary frames are '
     .//           'recognized. ' )
 
         DO I = 10010, 10027
 
            J = I
            CALL TSTMSG ( '#', 'Subcase: Frame ID = # ' )
            CALL TSTMSI (  J                            )
 
 
            CALL FRINFO (  J, CENT, CLASS,  CLSSID, FOUND )
            CALL TIPBOD ( 'J2000',  CLSSID, ET,     TIPM  )
            CALL XPOSE  (  TIPM,                    TPMI  )
            CALL ROTGET (  I, ET,   ROTAT,  OUTFRM, FOUND )
 
            CALL CHCKXC ( .FALSE.,  ' ',                           OK )
            CALL CHCKSL ( 'FOUND',  FOUND, .TRUE.,                 OK )
            CALL CHCKSI ( 'OUTFRM', OUTFRM, '=',         1, 0,     OK )
            CALL CHCKAD ( 'ROTAT',  ROTAT,  '=', TPMI,  9, 0.0D0, OK )
 
         END DO
 
         CALL TSTMSG ( '#', ' ' )
 
      CALL TCASE ( 'Verify that loaded C-kernels are recognized.' )

         CALL TSTATD ( ET,     TMPROT,  AV      )
         CALL XPOSE  ( TMPROT, ROT              )
 
         CALL NAMFRM ( 'J2000',    XPCTD(1)  )
         CALL NAMFRM ( 'FK4',      XPCTD(2)  )
         CALL NAMFRM ( 'GALACTIC', XPCTD(3)  )

         DO I = 1, 3
 
            ID = -10002 + I
 
            CALL FRINFO ( XPCTD(I), XCENT, XCLASS, XCLSID, FOUND )
            CALL CHCKXC ( .FALSE., ' ', OK )  
            CALL CHCKSL ( 'Expected frame info',  FOUND, .TRUE., OK )

C
C           Define a CK frame associated with instrument having ID code
C           "ID"; the pointing for this instrument is given by the CK
C           created by the test utility TSTCK3.
C
            DEFTXT(1) = 
     .      'FRAME_CK_INSTID                  =  INSTID'
            DEFTXT(2) = 
     .      'FRAME_INSTID_NAME                = ''CK_INSTID'''
            DEFTXT(3) = 
     .      'FRAME_INSTID_CLASS               =  3'
            DEFTXT(4) = 
     .      'FRAME_INSTID_CLASS_ID            =  INSTID'
            DEFTXT(5) = 
     .      'FRAME_INSTID_CENTER              =  XCENT'
            DEFTXT(6) = 
     .      'FRAME_INSTID_RELATIVE            =  XPCT'
  
            NTEXT = 6

            DO J = 1, NTEXT
C
C              Do two replacements for the ID code; if the second
C              replacement is unnecessary, no harm's done.
C
               CALL REPMI  ( DEFTXT(J), 'INSTID', ID, DEFTXT(J) )
               CALL REPMI  ( DEFTXT(J), 'INSTID', ID, DEFTXT(J) )
               CALL CHCKXC ( .FALSE., ' ', OK )  
C
C              Replace the center ID when it comes up.
C
               CALL REPMI  ( DEFTXT(J), 'XCENT', XCENT, DEFTXT(J) )
               CALL CHCKXC ( .FALSE., ' ', OK )  

            END DO
C
C           Insert the base frame ID.
C
            CALL REPMI ( DEFTXT(NTEXT), 'XPCT', 
     .                   XPCTD(I),      DEFTXT(NTEXT) )
            CALL CHCKXC( .FALSE., ' ', OK )  
            
C
C           Load the CK_-10001 frame definition.
C
            CALL LMPOOL ( DEFTXT, 6 )
            CALL CHCKXC ( .FALSE., ' ', OK )  


            CALL TSTMSG (  '#', 'Subcase: Frame ID = # ' )
            CALL TSTMSI (  ID                            )
            CALL ROTGET (  ID, ET, ROTAT, OUTFRM, FOUND  )
 
            CALL CHCKXC ( .FALSE.,  ' ',                             OK)
            CALL CHCKSL ( 'FOUND',  FOUND, .TRUE.,                   OK)
            CALL CHCKSI ( 'OUTFRM', OUTFRM, '=', XPCTD(I),  0,       OK)
            CALL CHCKAD ( 'ROTAT',  ROTAT,  '~', ROT,    9, 1.0D-12, OK)
 
         END DO
 
 
      CALL TCASE ( 'Verify that I-kernel frames are recognized. ' )
 
         CALL ROTGET ( -111111, ET, ROTAT, OUTFRM, FOUND )
C
C        Construct the expected output state transformation matrix.
C
 
 
         ROT( 1, 1 ) =  0.48D0
         ROT( 2, 1 ) =  0.60D0
         ROT( 3, 1 ) =  0.64D0
 
         ROT( 1, 2 ) = -0.8D0
         ROT( 2, 2 ) =  0.0D0
         ROT( 3, 2 ) =  0.6D0
 
         ROT( 1, 3 ) =  0.36D0
         ROT( 2, 3 ) = -0.80D0
         ROT( 3, 3 ) =  0.48D0
 
 
         CALL CHCKXC ( .FALSE.,  ' ',                              OK)
         CALL CHCKSL ( 'FOUND',  FOUND, .TRUE.,                    OK)
         CALL CHCKSI ( 'OUTFRM', OUTFRM, '=', -9999,      0,       OK)
         CALL CHCKAD ( 'ROTAT',  ROTAT,  '~', ROT,  9, 1.0D-14, OK)
 
C
C     Finally clean up the loaded data.
C
      CALL CKUPF     ( HANDLE )
      CALL CLPOOL
      CALL KILFIL    ( 'phoenix.bc' )
 
      CALL T_SUCCESS ( OK     )
      RETURN
      END
