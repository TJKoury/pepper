!$Procedure      F_FRMGET ( Family of tests for FRMGET )

      SUBROUTINE F_FRMGET ( OK )
      IMPLICIT NONE

      LOGICAL               OK

! $ Abstract
!
!     This routine tests the primary frame utility FRMGET.
!     It makes sure that all 4 frame classes are recognized
!     and that the routine retrieves the correct frame information
!     for each of them.
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
!-&


!
!     Test Utility Functions
!
      CHARACTER*(16)        BEGDAT
      CHARACTER*(16)        BEGTXT

!
!     SPICELIB Functions
!

!
!     Local Variables
!
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               NLINES
      PARAMETER           ( NLINES = 66 )

      INTEGER               DEFSIZ
      PARAMETER           ( DEFSIZ = 15 )

      CHARACTER*(LNSIZE)    DEFTXT ( DEFSIZ )
      CHARACTER*(LNSIZE)    ERROR
      CHARACTER*(LNSIZE)    LINES ( NLINES )

      DOUBLE PRECISION      AV     ( 3    )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ROT    ( 3, 3 )
      DOUBLE PRECISION      TSIPM  ( 6, 6 )
      DOUBLE PRECISION      TSPMI  ( 6, 6 )
      DOUBLE PRECISION      TMPXFM ( 6, 6 )
      DOUBLE PRECISION      TXFORM ( 6, 6 )
      DOUBLE PRECISION      XFORM  ( 6, 6 )

      INTEGER               CENT
      INTEGER               CLASS
      INTEGER               CLSSID
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               ID
      INTEGER               J
      INTEGER               K
      INTEGER               NTEXT
      INTEGER               OUTFRM
      INTEGER               XPCTD ( 3 )
      INTEGER               XCENT
      INTEGER               XCLASS
      INTEGER               XCLSID

      LOGICAL               FOUND



!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_FRMGET' )


      LINES(  1 ) = 'This is a test I-kernel for the fictional ' &
      //            'instrument TST_PHEONIX '
      LINES(  2 ) = 'on board the fictional spacecraft PHEONIX. ' &
      //            'A C-kernel for '
      LINES(  3 ) = 'the platform on which TST_PHEONIX is ' &
      //            'mounted can be generated '
      LINES(  4 ) = 'by calling the test utility TSTCK3. '
      LINES(  5 ) = ' '
      LINES(  6 ) = 'This kernel describes only the mode ' &
      //            'independent attributes of the '
      LINES(  7 ) = 'TST_PHOENIX instrument. '
      LINES(  8 ) = ' '
      LINES(  9 ) = 'This kernel is intended only for test ' &
      //            'purposes.  It is primarily '
      LINES( 10 ) = 'useful for testing the I-kernel data ' &
      //            'fetching routines '
      LINES( 11 ) = ' '
      LINES( 12 ) =  BEGDAT()
      LINES( 13 ) = 'TKFRAME_-111111_SPEC = ' &
      //            '''MATRIX'' '
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
      LINES( 43 ) = 'Next we need to supply the various bits of ' &
      //            'frame identification for '
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

!
!     Clean out the kernel pool. Just in case we have something
!     already loaded there.
!
      CALL CLPOOL
!
!     Create and load the test kernels that will be needed during
!     this test.
!
      CALL TCASE ( 'Make sure we can load everything.' )

         CALL TSTPCK ( 'test.pck',                   .TRUE., .FALSE. )
         CALL TSTCK3 ( 'phoenix.bc',  'phoenix.tsc', .FALSE., &
                                                     .TRUE., .FALSE., &
                                                      HANDLE )
         CALL TSTTXT ( 'phoenix2.ik', LINES, NLINES, .TRUE., .FALSE. )
         CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CKLPF  ( 'phoenix.bc',   HANDLE         )
      CALL TPARSE ( '1 Jan 1995',   ET,     ERROR  )


      CALL TCASE ( 'Determine that a frame of unrecognized class ' &
      //           'causes the correct exception to be ' &
      //           'signalled. ' )


         CALL FRMGET (  -222222, ET, XFORM, OUTFRM, FOUND )
         CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNFRAMETYPE)', OK )


      CALL TCASE ( 'Verify that all inertial frames are ' &
      //           'recognized and that they produce the ' &
      //           'expected transformation. ' )

         DO I = 1, 18

            CALL FRMGET ( I, ET, XFORM, OUTFRM, FOUND )
            CALL IRFROT ( I, 1,  ROT                  )

            DO K = 4,6
               DO J = 1,3
                  TXFORM (K,J) = 0.0D0
                  TXFORM (J,K) = 0.0D0
               END DO
            END DO

            DO K = 1,3
               DO J = 1,3
                  TXFORM(K,  J  ) = ROT(K,J)
                  TXFORM(K+3,J+3) = ROT(K,J)
               END DO
            END DO

            J = I
            CALL TSTMSG ( '#', 'Subcase. Frame ID = # ' )
            CALL TSTMSI (  J  )

            CALL CHCKXC ( .FALSE.,  ' ',                            OK )
            CALL CHCKSL ( 'FOUND',  FOUND,      .TRUE.,             OK )
            CALL CHCKSI ( 'OUTFRM', OUTFRM, '=', 1,          0,     OK )
            CALL CHCKAD ( 'XFORM',  XFORM,  '=', TXFORM, 36, 0.0D0, OK )

         END DO
!
!        Clear the extra message.
!
         CALL TSTMSG ( '#', ' ' )

      CALL TCASE ( 'Verify that inertial class frames that are ' &
      //           'not yet supported are not found by FRMGET. ' )


         CALL FRMGET ( 99, ET, XFORM, OUTFRM, FOUND )

         CALL CHCKXC ( .FALSE., ' ',             OK )
         CALL CHCKSL ( 'FOUND',  FOUND, .FALSE., OK )

      CALL TCASE ( 'Verify that the planetary frames are ' &
      //           'recognized. ' )

         DO I = 10010, 10027

            J = I
            CALL TSTMSG ( '#', 'Subcase: Frame ID = # ' )
            CALL TSTMSI (  J                            )


            CALL FRINFO (  J, CENT, CLASS,  CLSSID, FOUND )
            CALL TISBOD ( 'J2000',  CLSSID, ET,     TSIPM )
            CALL INVSTM (  TSIPM,                   TSPMI )
            CALL FRMGET (  I, ET,   XFORM,  OUTFRM, FOUND )

            CALL CHCKXC ( .FALSE.,  ' ',                           OK )
            CALL CHCKSL ( 'FOUND',  FOUND, .TRUE.,                 OK )
            CALL CHCKSI ( 'OUTFRM', OUTFRM, '=',         1, 0,     OK )
            CALL CHCKAD ( 'XFORM',  XFORM,  '=', TSPMI, 36, 0.0D0, OK )

         END DO

         CALL TSTMSG ( '#', ' ' )

      CALL TCASE ( 'Verify that loaded C-kernels are recognized.' )

         CALL TSTATD ( ET,   ROT,  AV     )
         CALL RAV2XF ( ROT,  AV,   TMPXFM )
         CALL INVSTM ( TMPXFM,     TXFORM )

         CALL NAMFRM ( 'J2000',    XPCTD(1)  )
         CALL NAMFRM ( 'FK4',      XPCTD(2)  )
         CALL NAMFRM ( 'GALACTIC', XPCTD(3)  )

         DO I = 1, 3

            ID = -10002 + I

            CALL FRINFO ( XPCTD(I), XCENT, XCLASS, XCLSID, FOUND )
            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL CHCKSL ( 'Expected frame info',  FOUND, .TRUE., OK )

!
!           Define a CK frame associated with instrument having ID code
!           "ID"; the pointing for this instrument is given by the CK
!           created by the test utility TSTCK3.
!
            DEFTXT(1) = &
            'FRAME_CK_INSTID                  =  INSTID'
            DEFTXT(2) = &
            'FRAME_INSTID_NAME                = ''CK_INSTID'''
            DEFTXT(3) = &
            'FRAME_INSTID_CLASS               =  3'
            DEFTXT(4) = &
            'FRAME_INSTID_CLASS_ID            =  INSTID'
            DEFTXT(5) = &
            'FRAME_INSTID_CENTER              =  XCENT'
            DEFTXT(6) = &
            'FRAME_INSTID_RELATIVE            =  XPCT'

            NTEXT = 6

            DO J = 1, NTEXT
!
!              Do two replacements for the ID code; if the second
!              replacement is unnecessary, no harm's done.
!
               CALL REPMI  ( DEFTXT(J), 'INSTID', ID, DEFTXT(J) )
               CALL REPMI  ( DEFTXT(J), 'INSTID', ID, DEFTXT(J) )
               CALL CHCKXC ( .FALSE., ' ', OK )
!
!              Replace the center ID when it comes up.
!
               CALL REPMI  ( DEFTXT(J), 'XCENT', XCENT, DEFTXT(J) )
               CALL CHCKXC ( .FALSE., ' ', OK )

            END DO
!
!           Insert the base frame ID.
!
            CALL REPMI ( DEFTXT(NTEXT), 'XPCT', &
                         XPCTD(I),      DEFTXT(NTEXT) )
            CALL CHCKXC( .FALSE., ' ', OK )

!
!           Load the CK_-10001 frame definition.
!
            CALL LMPOOL ( DEFTXT, 6 )
            CALL CHCKXC ( .FALSE., ' ', OK  )


            CALL TSTMSG (  '#', 'Subcase: Frame ID = # ' )
            CALL TSTMSI (  ID                            )
            CALL FRMGET (  ID, ET, XFORM, OUTFRM, FOUND  )

            CALL CHCKXC ( .FALSE.,  ' ',                             OK)
            CALL CHCKSL ( 'FOUND',  FOUND, .TRUE.,                   OK)
            CALL CHCKSI ( 'OUTFRM', OUTFRM, '=', XPCTD(I),  0,       OK)
            CALL CHCKAD ( 'XFORM',  XFORM,  '~', TXFORM,36, 1.0D-12, OK)

         END DO


      CALL TCASE ( 'Verify that I-kernel frames are recognized. ' )

         CALL FRMGET ( -111111, ET, XFORM, OUTFRM, FOUND )
!
!        Construct the expected output state transformation matrix.
!
         DO I = 1, 6
            DO J = 1,6
               TXFORM(I,J) = 0.0D0
            END DO
         END DO


         TXFORM( 1, 1 ) =  0.48D0
         TXFORM( 2, 1 ) =  0.60D0
         TXFORM( 3, 1 ) =  0.64D0

         TXFORM( 1, 2 ) = -0.8D0
         TXFORM( 2, 2 ) =  0.0D0
         TXFORM( 3, 2 ) =  0.6D0

         TXFORM( 1, 3 ) =  0.36D0
         TXFORM( 2, 3 ) = -0.80D0
         TXFORM( 3, 3 ) =  0.48D0

         TXFORM( 4, 4 ) =  0.48D0
         TXFORM( 5, 4 ) =  0.60D0
         TXFORM( 6, 4 ) =  0.64D0

         TXFORM( 4, 5 ) = -0.8D0
         TXFORM( 5, 5 ) =  0.0D0
         TXFORM( 6, 5 ) =  0.6D0

         TXFORM( 4, 6 ) =  0.36D0
         TXFORM( 5, 6 ) = -0.80D0
         TXFORM( 6, 6 ) =  0.48D0

         CALL CHCKXC ( .FALSE.,  ' ',                              OK)
         CALL CHCKSL ( 'FOUND',  FOUND, .TRUE.,                    OK)
         CALL CHCKSI ( 'OUTFRM', OUTFRM, '=', -9999,      0,       OK)
         CALL CHCKAD ( 'XFORM',  XFORM,  '~', TXFORM, 36, 1.0D-14, OK)

!
!     Finally clean up the loaded data.
!
      CALL CKUPF     ( HANDLE )
      CALL CLPOOL
      CALL KILFIL    ( 'phoenix.bc' )

      CALL T_SUCCESS ( OK     )
      RETURN
      END
