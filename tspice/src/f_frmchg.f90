!$Procedure      F_FRMCHG ( Family of tests for FRMCHG )

      SUBROUTINE F_FRMCHG ( OK )
      use pepper_legacy, pepper_legacy_frame => frame
      IMPLICIT NONE

      LOGICAL               OK

! $ Abstract
!
!     This routine tests the user interface level frame transformation
!     software FRMGHG. FRMCHG.
!
! $ Version
!
!-    Version 2.0.0  14-JAN-1998  (WLT)
!
!     The equality tests were relaxed to equal within 1.0D-13
!     so that comparisons will pass on the PC which for some reason
!     isn't able to achieve the same performance as the UNIX machines.
!
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
      PARAMETER           ( NLINES = 83 )

      INTEGER               DEFSIZ
      PARAMETER           ( DEFSIZ = 15 )


      CHARACTER*(LNSIZE)    DEFTXT ( DEFSIZ )
      CHARACTER*(LNSIZE)    ERROR
      CHARACTER*(LNSIZE)    LINES ( NLINES )

      DOUBLE PRECISION      AV     ( 3    )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ROT    ( 3, 3 )
      DOUBLE PRECISION      ROTT   ( 3, 3 )
      DOUBLE PRECISION      TSIPM  ( 6, 6 )
      DOUBLE PRECISION      TSPMI  ( 6, 6 )
      DOUBLE PRECISION      TXFORM ( 6, 6 )
      DOUBLE PRECISION      XFORM  ( 6, 6 )
      DOUBLE PRECISION      MODIFY ( 6, 6 )
      DOUBLE PRECISION      EXPECT ( 6, 6 )
      DOUBLE PRECISION      TMPXFM ( 6, 6 )
      DOUBLE PRECISION      TK2CK  ( 6, 6 )
      DOUBLE PRECISION      TK2IN  ( 6, 6 )
      DOUBLE PRECISION      TK2J2  ( 6, 6 )
      DOUBLE PRECISION      CK2IN  ( 6, 6 )

      INTEGER               EARTH

      INTEGER               CENT
      INTEGER               CLASS
      INTEGER               CLSSID
      INTEGER               FRAME
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               ID
      INTEGER               J
      INTEGER               K
      INTEGER               M
      INTEGER               NTEXT
      INTEGER               XPCTD ( 3 )
      INTEGER               XCENT
      INTEGER               XCLASS
      INTEGER               XCLSID

      LOGICAL               FOUND



!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_FRMCHG' )

      LINES(  1 ) = 'This is a test frame kernel for the fict' &
      //            'ional instrument TST_PHEONIX'
      LINES(  2 ) = 'on board the fictional spacecraft PHEONI' &
      //            'X. A C-kernel for'
      LINES(  3 ) = 'the platform on which TST_PHEONIX is mou' &
      //            'nted can be generated'
      LINES(  4 ) = 'by calling the test utility TSTCK3.'
      LINES(  5 ) = ' '
      LINES(  6 ) = 'This kernel describes only the orientati' &
      //            'on attributes of the'
      LINES(  7 ) = 'TST_PHOENIX instrument.'
      LINES(  8 ) = ' '
      LINES(  9 ) = 'This kernel is intended only for test pu' &
      //            'rposes.  It is primarily'
      LINES( 10 ) = 'useful for testing frame routines'
      LINES( 11 ) = ' '
      LINES( 12 ) = ' '
      LINES( 13 ) = 'Next we need to supply the various bits ' &
      //            'of frame identification for'
      LINES( 14 ) = 'this instrument.'
      LINES( 15 ) = ' '
      LINES( 16 ) = BEGDAT()
      LINES( 17 ) = 'FRAME_-399999_CLASS    =  4'
      LINES( 18 ) = 'FRAME_-399999_CENTER   =  399'
      LINES( 19 ) = 'FRAME_-399999_CLASS_ID = -399999'
      LINES( 20 ) = 'FRAME_-399999_NAME     = ''TOPOCENTRIC'''
      LINES( 21 ) = 'FRAME_TOPOCENTRIC      = -399999'
      LINES( 22 ) = ' '
      LINES( 23 ) = 'FRAME_-111111_CLASS    =  4'
      LINES( 24 ) = 'FRAME_-111111_CENTER   = -9'
      LINES( 25 ) = 'FRAME_-111111_CLASS_ID = -111111'
      LINES( 26 ) = 'FRAME_-111111_NAME     = ''TST-PHOENIX'''
      LINES( 27 ) = 'FRAME_TST-PHOENIX      = -111111'
      LINES( 28 ) = ' '
      LINES( 29 ) = 'FRAME_-9999_CLASS      =  3'
      LINES( 30 ) = 'FRAME_-9999_CENTER     = -9'
      LINES( 31 ) = 'FRAME_-9999_CLASS_ID   = -9999'
      LINES( 32 ) = 'FRAME_-9999_NAME       = ''PHOENIX'''
      LINES( 33 ) = 'FRAME_PHOENIX          = -9999'
      LINES( 34 ) = ' '
      LINES( 35 ) = 'CK_-9999_SCLK          =  -9'
      LINES( 36 ) = 'CK_-9999_SPK           =  -9'
      LINES( 37 ) = ' '
      LINES( 38 ) = 'TKFRAME_-111111_SPEC      = ''MATRIX'''
      LINES( 39 ) = 'TKFRAME_-111111_RELATIVE  = ''PHOENIX'''
      LINES( 40 ) = 'TKFRAME_-111111_MATRIX    =  ( 0.48'
      LINES( 41 ) = '                               0.60'
      LINES( 42 ) = '                               0.64'
      LINES( 43 ) = '                              -0.8'
      LINES( 44 ) = '                               0.0'
      LINES( 45 ) = '                               0.6'
      LINES( 46 ) = '                               0.36'
      LINES( 47 ) = '                              -0.80'
      LINES( 48 ) = '                               0.48 )'
      LINES( 49 ) = ' '
      LINES( 50 ) = 'TKFRAME_-399999_SPEC       = ''ANGLES'''
      LINES( 51 ) = 'TKFRAME_-399999_RELATIVE   = ''IAU_EARTH' &
      //            ''''
      LINES( 52 ) = 'TKFRAME_-399999_ANGLES     = ( 90, 56.18' &
      //            '29, -118.0 )'
      LINES( 53 ) = 'TKFRAME_-399999_AXES       = ( 3, 2, 3 )'
      LINES( 54 ) = 'TKFRAME_-399999_UNITS      = ''DEGREES'''
      LINES( 55 ) = ' '
      LINES( 56 ) = BEGTXT()
      LINES( 57 ) = ' '
      LINES( 58 ) = ' ' &
      //            ' '
      LINES( 59 ) = ' ' &
      //            ' '
      LINES( 60 ) = ' '
      LINES( 61 ) = ' '
      LINES( 62 ) = ' '
      LINES( 63 ) = BEGDAT()
      LINES( 64 ) = ' '
      LINES( 65 ) = 'FRAME_-222222_CLASS    =  4'
      LINES( 66 ) = 'FRAME_-222222_CENTER   = -9'
      LINES( 67 ) = 'FRAME_-222222_CLASS_ID = -222222'
      LINES( 68 ) = 'FRAME_-222222_NAME     = ''TST_PHOENIX2'''
      LINES( 69 ) = 'FRAME_TST_PHOENIX2     = -222222'
      LINES( 70 ) = ' '
      LINES( 71 ) = ' '
      LINES( 72 ) = 'TKFRAME_-222222_RELATIVE   =  ''PHOENIX'''
      LINES( 73 ) = 'TKFRAME_-222222_SPEC       =  ''MATRIX'''
      LINES( 74 ) = 'TKFRAME_-222222_MATRIX     = ( 0.48'
      LINES( 75 ) = '                               0.60'
      LINES( 76 ) = '                               0.64'
      LINES( 77 ) = '                              -0.8'
      LINES( 78 ) = '                               0.0'
      LINES( 79 ) = '                               0.6'
      LINES( 80 ) = '                               0.36'
      LINES( 81 ) = '                              -0.80'
      LINES( 82 ) = '                               0.48 )'
      LINES( 83 ) = ' '

!
!     Clean out the kernel pool. Just in case we have something
!     already loaded there.
!
      CALL CLPOOL
!
!     Create and load the test kernels that will be needed during
!     this test.
!
      CALL TSTTXT ( 'phoenix2.tk', LINES, NLINES, .TRUE., .TRUE. )
      CALL TSTPCK ( 'test.pck',                   .TRUE., .FALSE. )
      CALL TSTCK3 ( 'phoenix.bc',  'phoenix.tsc', .FALSE., &
                                                  .TRUE., .FALSE., &
                                                   HANDLE )

      CALL CKLPF  ( 'phoenix.bc',   HANDLE         )
      CALL TPARSE ( '1 Jan 2195',   ET,     ERROR  )


      CALL TCASE ( 'Exercise the exception handling for the case ' &
      //           'in which information is not provided that is ' &
      //           'sufficient to transform from one frame to ' &
      //           'another. ' )

         CALL FRMCHG ( -222222, 1, ET, XFORM )
         CALL CHCKXC ( .TRUE., 'SPICE(NOFRAMECONNECT)', OK )


      CALL TCASE ( 'Verify that if a frame is not recognized, ' &
      //           'that an error is signalled that says so. ' )


         CALL TPARSE ( '1 Jan 1995',   ET,     ERROR  )
         CALL FRMCHG ( 27687628, -1987291, ET, XFORM )
         CALL CHCKXC ( .TRUE.,    'SPICE(UNKNOWNFRAME)', OK )

      CALL TCASE ( 'Verify that all inertial frames are ' &
      //           'recognized and that they produce the ' &
      //           'expected transformation. ' )

         DO I = 1, 18
            DO J = 1, 18

               CALL FRMCHG ( I, J, ET, XFORM  )
               CALL IRFROT ( I, J, ROT    )

               DO K = 4,6
                  DO M = 1,3
                     TXFORM (K,M) = 0.0D0
                     TXFORM (M,K) = 0.0D0
                  END DO
               END DO

               DO K = 1,3
                  DO M = 1,3
                     TXFORM(K,  M  ) = ROT(K,M)
                     TXFORM(K+3,M+3) = ROT(K,M)
                  END DO
               END DO

               CALL TSTMSG ( '#', 'Subcase. Frame 1 = #, Frame 2 = #.' )
               CALL TSTMSI (  I  )
               CALL TSTMSI (  J  )

               CALL CHCKXC ( .FALSE.,  ' ',     OK )
               CALL CHCKAD ( 'XFORM',  XFORM,  '~', TXFORM, &
                              36,      1.0D-13, OK )
            END DO
         END DO
!
!        Clear the extra message.
!
         CALL TSTMSG ( '#', ' ' )


      CALL TCASE ( 'Verify that the transformations from ' &
      //           'inertial to bodyfixed frames behave as ' &
      //           'expected. ' )


         DO I = 10010, 10027

            J = I
            CALL TSTMSG ( '#', 'Subcase: Frame ID = # ' )
            CALL TSTMSI (  J                            )


            CALL FRINFO (  J, CENT, CLASS,  CLSSID, FOUND )
            CALL TISBOD ( 'J2000',  CLSSID, ET,     TSIPM )
            CALL FRMCHG (  1, J,            ET,     XFORM )

            CALL CHCKXC ( .FALSE.,  ' ',                            OK )
            CALL CHCKAD ( 'XFORM',  XFORM,  '=', TSIPM , 36, 0.0D0, OK )

         END DO

         CALL TSTMSG ( '#', ' ' )

      CALL TCASE ( 'Verify that the transformations from ' &
      //           'bodyfixed to inertial frames behave as ' &
      //           'expected. ' )


         DO I = 10010, 10027

            J = I
            CALL TSTMSG ( '#', 'Subcase: Frame ID = # ' )
            CALL TSTMSI (  J                            )


            CALL FRINFO (  J, CENT, CLASS,  CLSSID, FOUND )
            CALL TISBOD ( 'J2000',  CLSSID, ET,     TSIPM )
            CALL INVSTM (  TSIPM,                   TSPMI )
            CALL FRMCHG (  J,  1,           ET,     XFORM )

            CALL CHCKXC ( .FALSE.,  ' ',                           OK )
            CALL CHCKAD ( 'XFORM',  XFORM,  '=', TSPMI, 36, 0.0D0, OK )

         END DO

         CALL TSTMSG ( '#', ' ' )

      CALL TCASE ( 'Verify that C-kernel frames to inertial ' &
      //           'frame transformations are carried out ' &
      //           'correctly. ' )
!
!        First construct the transformation from the C-kernel
!        frame to its native frame.  And look up the native
!        frame for each of the segments in the C-kernel.
!
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
            CALL FRMCHG (  ID, 1, ET, XFORM )
!
!           Construct the expected transfromation from the C-kernel
!           native frame to J2000. And then construct the transformation
!           from the C-kernel frame to J2000.
!
            CALL FRMCHG ( XPCTD(I), 1, ET, MODIFY )
            CALL MXMG   ( MODIFY, TXFORM, 6, 6, 6, EXPECT )

            CALL CHCKXC ( .FALSE.,  ' ',                             OK)
            CALL CHCKAD ( 'XFORM',  XFORM,  '~', EXPECT,36, 1.0D-12, OK)

         END DO

         CALL TSTMSG ( '#', ' ' )

      CALL TCASE ( 'Verify that we can perform a complete ' &
      //           'transformation from an instrument frame to a ' &
      //           'body fixed frame. TK -> CK ->INERTIAL ->PCK ' )


         CALL NAMFRM ( 'IAU_EARTH', EARTH )
         CALL FRMCHG ( -111111,     EARTH, ET, XFORM )

!
!        Construct the expected output state transformation matrix.
!
         DO I = 1, 6
            DO J = 1,6
               TK2CK(I,J) = 0.0D0
            END DO
         END DO


         TK2CK( 1, 1 ) =  0.48D0
         TK2CK( 2, 1 ) =  0.60D0
         TK2CK( 3, 1 ) =  0.64D0

         TK2CK( 1, 2 ) = -0.8D0
         TK2CK( 2, 2 ) =  0.0D0
         TK2CK( 3, 2 ) =  0.6D0

         TK2CK( 1, 3 ) =  0.36D0
         TK2CK( 2, 3 ) = -0.80D0
         TK2CK( 3, 3 ) =  0.48D0

         TK2CK( 4, 4 ) =  0.48D0
         TK2CK( 5, 4 ) =  0.60D0
         TK2CK( 6, 4 ) =  0.64D0

         TK2CK( 4, 5 ) = -0.8D0
         TK2CK( 5, 5 ) =  0.0D0
         TK2CK( 6, 5 ) =  0.6D0

         TK2CK( 4, 6 ) =  0.36D0
         TK2CK( 5, 6 ) = -0.80D0
         TK2CK( 6, 6 ) =  0.48D0

         CALL FRMCHG ( XPCTD(3), 1, ET, MODIFY )

!
!        TSTATD can reproduce the attitude in the C-kernel so we
!        use that instead of CKGPAV.
!
         CALL TSTATD ( ET,   ROT,  AV     )
         CALL RAV2XF ( ROT,  AV,   TMPXFM )
         CALL INVSTM ( TMPXFM,     CK2IN  )

         CALL TISBOD ( 'J2000',  399, ET, TSIPM )

         CALL MXMG ( CK2IN,  TK2CK, 6, 6, 6, TK2IN  )
         CALL MXMG ( MODIFY, TK2IN, 6, 6, 6, TK2J2 )
         CALL MXMG ( TSIPM,  TK2J2, 6, 6, 6, TXFORM )

         CALL CHCKXC ( .FALSE.,  ' ',                              OK)
         CALL CHCKAD ( 'XFORM',  XFORM,  '~', TXFORM, 36, 1.0D-13, OK)


      CALL TCASE ( 'Perform test a long chain of transformations ' &
      //           'on both ends. TK -> CK -> INERTIAL -> PCK -> ' &
      //           'TK '  )

!
!        Get the transformation from frame -399999 to the
!        underlying bodyfixed frame.
!
         CALL TKFRAM ( -399999, ROT, FRAME, FOUND )
         CALL XPOSE  (  ROT,    ROTT           )

         DO I = 1, 3
            DO J = 1, 3
               MODIFY(I,  J  ) = ROTT(I,J)
               MODIFY(I+3,J  ) = 0.0D0
               MODIFY(I,  J+3) = 0.0D0
               MODIFY(I+3,J+3) = ROTT(I,J)
            END DO
         END DO

         CALL MXMG ( MODIFY, TXFORM, 6, 6, 6, EXPECT )

         CALL FRMCHG ( -111111, -399999, ET, XFORM )
         CALL CHCKXC ( .FALSE.,  ' ',                 OK )
         CALL CHCKSI ( 'FRAME', FRAME, '=', EARTH, 0, OK )

         CALL CHCKAD ( 'XFORM',  XFORM,  '~', EXPECT, 36, 1.0D-13, OK)

!
!     Finally clean up the loaded data.
!
      CALL CKUPF     (  HANDLE      )
      CALL KILFIL    ( 'phoenix.bc' )
      CALL KILFIL    ( 'phoenix2.tk' )
      CALL CLPOOL
      CALL T_SUCCESS ( OK     )
      RETURN
      END
