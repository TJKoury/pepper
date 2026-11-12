!$Procedure      F_SPKGPS ( Family of tests for SPKGPS )

      SUBROUTINE F_SPKGPS ( OK )
      use pepper_legacy
      IMPLICIT NONE

      LOGICAL               OK

! $ Abstract
!
!     This routine tests the behaviour of the SPICE routine SPKGPS
!     with the addition of non-inertial frames.
!
! $ Version
!
!-    TSPICE Version 2.0.0, 10-NOV-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in VADDG calls.
!
!-&

      CHARACTER*(40)        BEGTXT
      CHARACTER*(40)        BEGDAT

!
!     Spicelib Routines.
!


!
!     Local Variables
!
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )

      INTEGER               LWSIZE
      PARAMETER           ( LWSIZE = 40 )

      INTEGER               NLINES
      PARAMETER           ( NLINES = 46 )


      CHARACTER*(LNSIZE)    ERROR
      CHARACTER*(LNSIZE)    LINES  ( NLINES )
      CHARACTER*(LWSIZE)    SEGID
      CHARACTER*(WDSIZE)    REF


      INTEGER               CENTER
      INTEGER               CKHAN
      INTEGER               IAUE
      INTEGER               IDCODE ( 46 )
      INTEGER               J
      INTEGER               K
      INTEGER               OBS
      INTEGER               REF1
      INTEGER               REF2
      INTEGER               REF3
      INTEGER               REF4
      INTEGER               REF5
      INTEGER               REF6
      INTEGER               REF7
      INTEGER               REFID
      INTEGER               SPKHAN
      INTEGER               TARG



      DOUBLE PRECISION      ET
      DOUBLE PRECISION      GM
      DOUBLE PRECISION      IAUST1  ( 6 )
      DOUBLE PRECISION      IAUST2  ( 6 )
      DOUBLE PRECISION      IAUST3  ( 6 )
      DOUBLE PRECISION      IAUST4  ( 6 )
      DOUBLE PRECISION      IAUST5  ( 6 )
      DOUBLE PRECISION      IAUST6  ( 6 )
      DOUBLE PRECISION      IAUST7  ( 6 )
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      LT2
      DOUBLE PRECISION      STATE   ( 6 )
      DOUBLE PRECISION      STATE1  ( 6 )
      DOUBLE PRECISION      STATE2  ( 6 )
      DOUBLE PRECISION      STATE3  ( 6 )
      DOUBLE PRECISION      STATE4  ( 6 )
      DOUBLE PRECISION      STATE5  ( 6 )
      DOUBLE PRECISION      STATE6  ( 6 )
      DOUBLE PRECISION      STATE7  ( 6 )
      DOUBLE PRECISION      TSIPM   ( 3, 3 )
      DOUBLE PRECISION      TSTATE  ( 6 )
      DOUBLE PRECISION      XFORM   ( 3, 3 )
      DOUBLE PRECISION      XFORM1  ( 3, 3 )
      DOUBLE PRECISION      XFORM2  ( 3, 3 )
      DOUBLE PRECISION      XFORM3  ( 3, 3 )
      DOUBLE PRECISION      XFORM4  ( 3, 3 )
      DOUBLE PRECISION      XFORM5  ( 3, 3 )
      DOUBLE PRECISION      XFORM6  ( 3, 3 )
      DOUBLE PRECISION      XFORM7  ( 3, 3 )
      DOUBLE PRECISION      STXFRM  ( 3, 3 )

      LINES(  1 ) = BEGTXT()
      LINES(  2 ) = 'This PCK file contains definitions for T' &
      //            'OPOGRAPHIC reference'
      LINES(  3 ) = 'frames at 3 different observatories arou' &
      //            'nd the world.  Note'
      LINES(  4 ) = 'that the definition of these frames is a' &
      //            'pproximate and that'
      LINES(  5 ) = 'they are accurate to only about 0.1 degr' &
      //            'ees.'
      LINES(  6 ) = ' '
      LINES(  7 ) = BEGDAT()
      LINES(  8 ) = ' '
      LINES(  9 ) = 'FRAME_CANBERRA_TOPO    = 1000002'
      LINES( 10 ) = 'FRAME_MADRID_TOPO      = 1000019'
      LINES( 11 ) = 'FRAME_GOLDSTONE_TOPO   = 1000023'
      LINES( 12 ) = ' '
      LINES( 13 ) = ' '
      LINES( 14 ) = 'FRAME_1000002_CENTER   =  399'
      LINES( 15 ) = 'FRAME_1000002_CLASS    =  4'
      LINES( 16 ) = 'FRAME_1000002_CLASS_ID = 1000002'
      LINES( 17 ) = 'FRAME_1000002_NAME     = ''CANBERRA_TOPO' &
      //            ''''
      LINES( 18 ) = ' '
      LINES( 19 ) = 'FRAME_1000019_CENTER   =  399'
      LINES( 20 ) = 'FRAME_1000019_CLASS    =  4'
      LINES( 21 ) = 'FRAME_1000019_CLASS_ID = 1000019'
      LINES( 22 ) = 'FRAME_1000019_NAME     = ''MADRID_TOPO'''
      LINES( 23 ) = ' '
      LINES( 24 ) = 'FRAME_1000023_CENTER   =  399'
      LINES( 25 ) = 'FRAME_1000023_CLASS    =  4'
      LINES( 26 ) = 'FRAME_1000023_CLASS_ID = 1000023'
      LINES( 27 ) = 'FRAME_1000023_NAME     = ''GOLDSTONE_TOP' &
      //            'O'''
      LINES( 28 ) = ' '
      LINES( 29 ) = ' '
      LINES( 30 ) = 'TKFRAME_1000002_ANGLES   = ( -149.0, -12' &
      //            '5.3, 180 )'
      LINES( 31 ) = 'TKFRAME_1000002_AXES     = (       3,   ' &
      //            '     2,   3 )'
      LINES( 32 ) = 'TKFRAME_1000002_RELATIVE = ''IAU_EARTH'''
      LINES( 33 ) = 'TKFRAME_1000002_SPEC     = ''ANGLES'''
      LINES( 34 ) = 'TKFRAME_1000002_UNITS    = ''DEGREES'''
      LINES( 35 ) = ' '
      LINES( 36 ) = 'TKFRAME_1000019_ANGLES   = ( 03.7, -49.6' &
      //            ', 180 )'
      LINES( 37 ) = 'TKFRAME_1000019_AXES     = (       3,   ' &
      //            '     2,   3 )'
      LINES( 38 ) = 'TKFRAME_1000019_RELATIVE = ''IAU_EARTH'''
      LINES( 39 ) = 'TKFRAME_1000019_SPEC     = ''ANGLES'''
      LINES( 40 ) = 'TKFRAME_1000019_UNITS    = ''DEGREES'''
      LINES( 41 ) = ' '
      LINES( 42 ) = 'TKFRAME_1000023_ANGLES   = ( 116.8, -54.' &
      //            '6, 180 )'
      LINES( 43 ) = 'TKFRAME_1000023_AXES     = (       3,   ' &
      //            '     2,   3 )'
      LINES( 44 ) = 'TKFRAME_1000023_RELATIVE = ''IAU_EARTH'''
      LINES( 45 ) = 'TKFRAME_1000023_SPEC     = ''ANGLES'''
      LINES( 46 ) = 'TKFRAME_1000023_UNITS    = ''DEGREES'''


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_SPKGPS' )

!
!     Set up the data needed for testing.
!

      CALL KILFIL ( 'test_pck.ker' )
      CALL TSTPCK ( 'test_pck.ker', .TRUE.,  .FALSE.         )
      CALL TSTCK3 ( 'phoenix.bc', &
                    'phoenix.tsc',  .FALSE., .TRUE., .FALSE., &
                     CKHAN )
      CALL CKLPF  ( 'phoenix.bc',                     CKHAN  )
      CALL TSTSPK ( 'test_spk.bsp', .TRUE.,           SPKHAN )


      IDCODE(  1 ) = 1
      IDCODE(  2 ) = 2
      IDCODE(  3 ) = 3
      IDCODE(  4 ) = 4
      IDCODE(  5 ) = 5
      IDCODE(  6 ) = 6
      IDCODE(  7 ) = 7
      IDCODE(  8 ) = 8
      IDCODE(  9 ) = 9
      IDCODE( 10 ) = 301
      IDCODE( 11 ) = 401
      IDCODE( 12 ) = 402
      IDCODE( 13 ) = 501
      IDCODE( 14 ) = 502
      IDCODE( 15 ) = 503
      IDCODE( 16 ) = 504
      IDCODE( 17 ) = 603
      IDCODE( 18 ) = 604
      IDCODE( 19 ) = 605
      IDCODE( 20 ) = 606
      IDCODE( 21 ) = 607
      IDCODE( 22 ) = 608
      IDCODE( 23 ) = 701
      IDCODE( 24 ) = 702
      IDCODE( 25 ) = 703
      IDCODE( 26 ) = 704
      IDCODE( 27 ) = 705
      IDCODE( 28 ) = 801
      IDCODE( 29 ) = 802
      IDCODE( 30 ) = 901
      IDCODE( 31 ) = 199
      IDCODE( 32 ) = 299
      IDCODE( 33 ) = 399
      IDCODE( 34 ) = 499
      IDCODE( 35 ) = 599
      IDCODE( 36 ) = 699
      IDCODE( 37 ) = 799
      IDCODE( 38 ) = 899
      IDCODE( 39 ) = 999
      IDCODE( 40 ) = 10
      IDCODE( 41 ) = 399001
      IDCODE( 42 ) = 399002
      IDCODE( 43 ) = 399003
      IDCODE( 44 ) = -9
      IDCODE( 45 ) = 401001
      IDCODE( 46 ) = 301001

      ET = 0.0D0

      CALL TCASE ( 'Check that exceptions behave as expected.' )

         CALL SPKGPS ( -10, ET, 'J2000', 399, STATE,   LT )
         CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

      CALL TCASE ( 'Get the state of the moon in the earth ' &
      //           'bodyfixed frame and compare it to the old ' &
      //           'fashioned way of computing this. ' )


         CALL SPKGPS  (  301, ET, 'IAU_EARTH', 399, STATE,  LT  )
         CALL SPKGEO  (  301, ET, 'J2000',     399, STATE1, LT2 )
         CALL TIPBOD  ( 'J2000', 399,    ET,        TSIPM      )
         CALL MXV     (  TSIPM,  STATE1,            STATE2     )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKAD ( 'STATE', STATE, '~/', STATE2, 3, 1.0D-10, OK )
         CALL CHCKSD ( 'LT',    LT,    '~/', LT2,       1.0D-10, OK )


      CALL TCASE ( 'Get the state of the moon relative to the ' &
      //           'earth in the mars bodyfixed frame and ' &
      //           'compare it to the old fashioned way of ' &
      //           'computing this. ' )



         CALL SPKGPS  (  301, ET, 'IAU_MARS',  399, STATE,  LT   )
         CALL SPKGEO  (  301, ET, 'J2000',     399, STATE1, LT2 )
         CALL TIPBOD  ( 'J2000', 499,    ET, XFORM  )
         CALL MXV     (  XFORM,  STATE1, STATE2 )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKAD ( 'STATE', STATE, '~/', STATE2, 3, 1.0D-10, OK )
         CALL CHCKSD ( 'LT',    LT,    '~/', LT2,       1.0D-10, OK )


      CALL TCASE ( 'Compute the state of body -9 relative to 401 ' &
      //           'in the IAU_EARTH frame using SPKGPS and ' &
      //           'computed via TSTST. ' )


         CALL SPKGPS ( -9, ET, 'IAU_EARTH', 401, STATE,  LT  )

         CALL NAMFRM ( 'IAU_EARTH', IAUE )

         CALL TSTST  ( -9,     ET, SEGID, REF1, STATE1, CENTER, GM )
         CALL TSTST  ( 301,    ET, SEGID, REF2, STATE2, CENTER, GM )
         CALL TSTST  ( 399,    ET, SEGID, REF3, STATE3, CENTER, GM )
         CALL TSTST  (   3,    ET, SEGID, REF4, STATE4, CENTER, GM )
         CALL TSTST  ( 401,    ET, SEGID, REF5, STATE5, CENTER, GM )
         CALL TSTST  ( 499,    ET, SEGID, REF6, STATE6, CENTER, GM )
         CALL TSTST  (   4,    ET, SEGID, REF7, STATE7, CENTER, GM )

         CALL REFCHG ( REF1, IAUE, ET, XFORM1 )
         CALL REFCHG ( REF2, IAUE, ET, XFORM2 )
         CALL REFCHG ( REF3, IAUE, ET, XFORM3 )
         CALL REFCHG ( REF4, IAUE, ET, XFORM4 )
         CALL REFCHG ( REF5, IAUE, ET, XFORM5 )
         CALL REFCHG ( REF6, IAUE, ET, XFORM6 )
         CALL REFCHG ( REF7, IAUE, ET, XFORM7 )

         CALL MXV    ( XFORM1, STATE1, IAUST1 )
         CALL MXV    ( XFORM2, STATE2, IAUST2 )
         CALL MXV    ( XFORM3, STATE3, IAUST3 )
         CALL MXV    ( XFORM4, STATE4, IAUST4 )
         CALL MXV    ( XFORM5, STATE5, IAUST5 )
         CALL MXV    ( XFORM6, STATE6, IAUST6 )
         CALL MXV    ( XFORM7, STATE7, IAUST7 )

         CALL CLEARD ( 3,      STATE1 )
         CALL CLEARD ( 3,      STATE2 )

         CALL VADDG  ( STATE1, IAUST1, 6,    TSTATE )
         CALL VADDG  ( TSTATE, IAUST2, 6,    STATE1 )
         CALL VADDG  ( STATE1, IAUST3, 6,    TSTATE )
         CALL VADDG  ( TSTATE, IAUST4, 6,    STATE1 )

         CALL VADDG  ( STATE2, IAUST5, 6,    TSTATE )
         CALL VADDG  ( TSTATE, IAUST6, 6,    STATE2 )
         CALL VADDG  ( STATE2, IAUST7, 6,    TSTATE )
         CALL MOVED  ( TSTATE,         6,    STATE2 )

         CALL VSUB   ( STATE1, STATE2, STATE3 )

         LT2 = VNORM( STATE3 ) / CLIGHT()

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKAD ( 'STATE', STATE, '~/', STATE3, 3, 1.0D-10, OK )
         CALL CHCKSD ( 'LT',    LT,    '~/', LT2,       1.0D-10, OK )


      CALL TCASE ( 'Compare states as generated with the old ' &
      //           'version of SPKGEO with SPKGPS. ' &
      //           'This case uses a long ephemeris and a large ' &
      //           'variety of observer-target pairs.' )
!
!        This routine tests the states of every object
!        in a pair of SPK files with every other object in every
!        available inertial reference frame.
!
!        This test is probably overkill, but is intended to demonstrate
!        that under normal circumstances, nothing has changed in the
!        latest upgrade of the SPK system.
!

         CALL TPARSE ( '1 JAN 1995', ET, ERROR )


         DO REFID = 1, 18, 5

            CALL FRMNAM ( REFID, REF )

            DO J = 1, 46, 10

               DO K = 46, 1, -3

                  CALL TSTMSG ( '#', 'ET: #, Frame: #, ' &
                  //                 'Target: #, Observer: ' &
                  //                 '#. ' )

                  TARG = IDCODE( J )
                  OBS  = IDCODE( K )

                  CALL TSTMSD ( ET   )
                  CALL TSTMSC ( REF  )
                  CALL TSTMSI ( TARG )
                  CALL TSTMSI ( OBS  )


                  CALL SPKGPS  ( TARG, ET, REF, OBS, STATE,  LT  )
                  CALL SPKGEO  ( TARG, ET, REF, OBS, STATE2, LT2 )

                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL CHCKAD ( 'STATE', STATE, '~/', &
                                         STATE2, 3, 1.0D-10, OK )
                  CALL CHCKSD ( 'LT',    LT, '~/', &
                                         LT2,       1.0D-10, OK )

               END DO

            END DO

         END DO

         CALL TSTMSG ( '#', ' ' )


      CALL TCASE ( 'Check to make sure that the logic that ' &
      //           'handles long chains of target center pairs ' &
      //           'functions as expected. ' )

         CALL SPKGPS ( 401001, ET, 'IAU_EARTH', 301001, STATE,  LT  )
         CALL SPKGEO ( 401001, ET, 'IAU_EARTH', 301001, STATE2, LT2 )


         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKAD ( 'STATE', STATE, '~/',  STATE2, 3, 1.0D-10, OK )
         CALL CHCKSD ( 'LT',    LT,    '~/',  LT2,       1.0D-10, OK )



      CALL TCASE ( 'Make sure we can get the state of an object ' &
      //           'relative to a topocentric frame. ' )

         CALL KILFIL ( 'topo.txt'     )
         CALL TSTTXT ( 'topo.txt',  LINES, NLINES, .TRUE., .TRUE. )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL SPKGPS ( 401001, ET, 'IAU_EARTH',  301001,  STATE,  LT  )
         CALL REFCHG ( IAUE,   1000023, ET, STXFRM )
         CALL MXV    ( STXFRM, STATE,   STATE2 )

         CALL SPKGPS ( 401001, ET, 'GOLDSTONE_TOPO', 301001, STATE, LT)
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKAD ( 'STATE', STATE, '~/',  STATE2, 3, 1.0D-10, OK )
!
!     That's all folks.
!
      CALL SPKUEF ( SPKHAN )
      CALL CKUPF  ( CKHAN  )

      CALL KILFIL ( 'phoenix.bc'    )
      CALL KILFIL ( 'test_spk.bsp'  )
      CALL KILFIL ( 'topo.txt'     )

      CALL T_SUCCESS ( OK )
      RETURN
      END
