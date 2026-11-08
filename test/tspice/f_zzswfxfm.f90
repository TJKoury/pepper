!$Procedure F_ZZSWFXFM ( ZZSWFXFM tests )

      SUBROUTINE F_ZZSWFXFM ( OK )

! $ Abstract
!
!     Exercise the SPICELIB routines ZZSWFXFM and ZZSWFINI.
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
!     None.
!
! $ Keywords
!
!     TEST FAMILY
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'errhnd.inc'

      LOGICAL               OK

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     OK         O   logical indicating test status.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     OK         is a logical that indicates the test status to the
!                caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     This routine does not generate any errors. Routines in its
!     call tree may generate errors that are either intentional and
!     trapped or unintentional and need reporting.  The test family
!     utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine tests the SPICELIB routines ZZSWFXFM and, mostly
!     indirectly, ZZSWFINI.
!
! $ Examples
!
!     None.
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
!     N.J. Bachman     (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 11-DEC-2021 (NJB)
!
!-&

!
!     Test utility functions
!
      CHARACTER*(10)        BEGDAT
      CHARACTER*(10)        BEGTXT

!
!     SPICELIB functions
!
      LOGICAL               EXISTS
      LOGICAL               ODD

!
!     Local Parameters
!
      CHARACTER*(*)         CK0
      PARAMETER           ( CK0    = 'ZZSWFXFM_test0.bc' )

      CHARACTER*(*)         CK1
      PARAMETER           ( CK1    = 'ZZSWFXFM_test1.bc' )

      CHARACTER*(*)         FK0
      PARAMETER           ( FK0    = 'ZZSWFXFM_test0.tf' )

      CHARACTER*(*)         PCK0
      PARAMETER           ( PCK0   = 'ZZSWFXFM_test0.tpc' )

      CHARACTER*(*)         SCLK0
      PARAMETER           ( SCLK0  = 'ZZSWFXFM_test0.tsc' )

      CHARACTER*(*)         SPK0
      PARAMETER           ( SPK0   = 'ZZSWFXFM_test0.bsp' )


      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-14  )

!
!     Lower bound of control area of singly linked list:
!
      INTEGER               LBSNGL
      PARAMETER           ( LBSNGL = -5 )

      INTEGER               MAXBAS
      PARAMETER           ( MAXBAS = 15000 )

      INTEGER               MAXFRM
      PARAMETER           ( MAXFRM = 1013 )

      INTEGER               BUFSIZ
      PARAMETER           ( BUFSIZ = 300 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               KVNMLN
      PARAMETER           ( KVNMLN = 32 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               TIMLEN
      PARAMETER           ( TIMLEN = 50 )

      INTEGER               TXTSIZ
      PARAMETER           ( TXTSIZ = 300 )

      INTEGER               NCKS
      PARAMETER           ( NCKS   = 3 )

      INTEGER               NTICKS
      PARAMETER           ( NTICKS = 1 )

      INTEGER               J2CODE
      PARAMETER           ( J2CODE = 1 )

      INTEGER               NSTART
      PARAMETER           ( NSTART = 1 )

      INTEGER               SIDLEN
      PARAMETER           ( SIDLEN = 40 )

!
!     Local Variables
!
      CHARACTER*(FRNMLN)    BASBUF ( BUFSIZ )
      CHARACTER*(FRNMLN)    BASNAM
      CHARACTER*(FRNMLN)    CKBASE ( NCKS )
      CHARACTER*(FRNMLN)    CKFRAM ( NCKS )
      CHARACTER*(FRNMLN)    FRNAME
      CHARACTER*(KVNMLN)    KVNAME
      CHARACTER*(LNSIZE)    LABEL
      CHARACTER*(SIDLEN)    SEGID
      CHARACTER*(SMSGLN)    SHRTMS
      CHARACTER*(TIMLEN)    STPBUF ( BUFSIZ )
      CHARACTER*(TIMLEN)    STRBUF ( BUFSIZ )
      CHARACTER*(LNSIZE)    TXTBUF ( TXTSIZ )

      DOUBLE PRECISION      AVVS   ( 3, NTICKS )
      DOUBLE PRECISION      BEGTIM
      DOUBLE PRECISION      DSTPBF ( MAXBAS + 1 )
      DOUBLE PRECISION      DSTRBF ( MAXBAS + 1 )
      DOUBLE PRECISION      ENDTIM
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ET0
      DOUBLE PRECISION      ET1
      DOUBLE PRECISION      IDNT33 ( 3, 3 )
      DOUBLE PRECISION      IDNT66 ( 6, 6 )
      DOUBLE PRECISION      QUATS  ( 0 : 3, NTICKS )
      DOUBLE PRECISION      RMAT   ( 3, 3 )
      DOUBLE PRECISION      STARTS ( NSTART )
      DOUBLE PRECISION      TICKS  ( NTICKS )
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      XFORM  ( 6, 6 )
      DOUBLE PRECISION      XRMAT  ( 3, 3 )
      DOUBLE PRECISION      XXFORM ( 6, 6 )

      INTEGER               BASEID
      INTEGER               CKHAN
      INTEGER               CKHAN1
      INTEGER               CKID
      INTEGER               FIDLST ( MAXFRM )
      INTEGER               FRAMID
      INTEGER               FREE
      INTEGER               FRPOOL ( LBSNGL : MAXFRM )
      INTEGER               HANDLE
      INTEGER               HDBASE ( MAXFRM )
      INTEGER               HDFRAM ( MAXFRM )
      INTEGER               I
      INTEGER               IBASBF ( MAXBAS + 1 )
      INTEGER               J
      INTEGER               N
      INTEGER               NBASES
      INTEGER               NFRAMS
      INTEGER               NI
      INTEGER               NLINES
      INTEGER               NREP
      INTEGER               OUTFRM
      INTEGER               PRVAT
      INTEGER               PRVFRM
      INTEGER               REPS
      INTEGER               SCLKID
      INTEGER               ROOM
      INTEGER               XFRMID

      LOGICAL               FOUND
      LOGICAL               SAMFRM

!
!     Saved values
!
      SAVE

!
!     Initial values
!
      DATA                  CKFRAM / 'CK_-9999', 'CK_-10000', &
                                     'CK_-10001' /

      DATA                  CKBASE / 'GALACTIC', 'FK4', 'J2000' /

!
!     Open the test family.
!
      CALL TOPEN ( 'F_ZZSWFXFM' )


!***********************************************************************
!
!     Normal cases
!
!***********************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Setup: create and load CK0, SCLK0, FK0, ' &
      //           'PCK0, SPK0, and LSK file.' )

!
!     Create a text kernel containing the following frame
!     specifications:
!
!     FRAME_SWITCH0               =  1400000
!     FRAME_1400000_NAME          =  'SWITCH0'
!     FRAME_1400000_CLASS         =  6
!     FRAME_1400000_CLASS_ID      =  1400000
!     FRAME_1400000_CENTER        =  0
!
!     FRAME_1400000_ALIGNED_WITH  =  ( 'GALACTIC',
!                                      'B1950',
!                                      'ECLIPJ2000',
!                                      'J2000',
!                                      'GALACTIC',
!                                      'B1950',
!                                      'ECLIPJ2000',
!                                      'J2000'      )
!
!     FRAME_1400000_START         = ( '2001 AUG 1 00:00:00 TDB',
!                                     '2001 SEP 1 00:00:00 TDB',
!                                     '2001 OCT 1 00:00:00 TDB',
!                                     '2001 NOV 1 00:00:00 TDB',
!                                     '2000 APR 1 00:00:00 TDB',
!                                     '2000 MAR 1 00:00:00 TDB',
!                                     '2000 FEB 1 00:00:00 TDB',
!                                     '2000 JAN 1 00:00:00 TDB' )
!
!     FRAME_1400000_STOP          = ( '2001 SEP 1 00:00:00 TDB',
!                                     '2001 OCT 1 00:00:00 TDB',
!                                     '2001 NOV 1 00:00:00 TDB',
!                                     '2001 DEC 1 00:00:00 TDB',
!                                     '2000 MAY 1 00:00:00 TDB',
!                                     '2000 APR 1 00:00:00 TDB',
!                                     '2000 MAR 1 00:00:00 TDB',
!                                     '2000 FEB 1 00:00:00 TDB' )
!
!     FRAME_SWITCH1               =  1400001
!     FRAME_1400001_NAME          =  'SWITCH1'
!     FRAME_1400001_CLASS         =  6
!     FRAME_1400001_CLASS_ID      =  1400001
!     FRAME_1400001_CENTER        =  0
!
!     FRAME_1400001_ALIGNED_WITH  =  ( 'J2000',
!                                      'CK_-10001',
!                                      'CK_-10000',
!                                      'CK_-9999',
!                                      'CK_-10001',
!                                      'CK_-10000',
!                                      'CK_-9999'  )
!
!     FRAME_1400001_START         = ( '1990 JAN 1 00:00:00 TDB',
!                                     '2001 JAN 1 00:00:00 TDB',
!                                     '2001 JAN 1 00:00:00 TDB',
!                                     '2001 JAN 1 00:00:00 TDB',
!                                     '2000 JAN 1 00:00:00 TDB',
!                                     '2000 JAN 1 00:00:00 TDB',
!                                     '2000 JAN 1 00:00:00 TDB' )
!
!     FRAME_1400001_STOP          = ( '2010 JAN 1 00:00:00 TDB',
!                                     '2001 APR 1 00:00:00 TDB',
!                                     '2001 MAR 1 00:00:00 TDB',
!                                     '2001 FEB 1 00:00:00 TDB',
!                                     '2000 APR 1 00:00:00 TDB',
!                                     '2000 MAR 1 00:00:00 TDB',
!                                     '2000 FEB 1 00:00:00 TDB' )
!
!     FRAME_SWITCH2               =  1400002
!     FRAME_1400002_NAME          =  'SWITCH2'
!     FRAME_1400002_CLASS         =  6
!     FRAME_1400002_CLASS_ID      =  1400002
!     FRAME_1400002_CENTER        =  0
!     FRAME_1400002_ALIGNED_WITH  =  ( 'J2000',
!                                      'CK_-10000',
!                                      'CK_-9999'  )
!
!     FRAME_SWITCH3               =  1400003
!     FRAME_1400003_NAME          =  'SWITCH3'
!     FRAME_1400003_CLASS         =  6
!     FRAME_1400003_CLASS_ID      =  1400003
!     FRAME_1400003_CENTER        =  0
!     FRAME_1400003_ALIGNED_WITH  =  ( 'SWITCH0' )
!
!     FRAME_SWITCH4               =  1400004
!     FRAME_1400004_NAME          =  'SWITCH4'
!     FRAME_1400004_CLASS         =  6
!     FRAME_1400004_CLASS_ID      =  1400004
!     FRAME_1400004_CENTER        =  0
!     FRAME_1400004_ALIGNED_WITH  =  ( 'IAU_EARTH',
!                                      'TK_0',
!                                      'SWITCH2',
!                                      'GSE_SWITCH_0' )
!
!     FRAME_1400004_START         = ( '2000 JAN 1 00:00:00 TDB',
!                                     '2000 FEB 1 00:00:00 TDB',
!                                     '2000 MAR 1 00:00:00 TDB',
!                                     '2000 APR 1 00:00:00 TDB' )
!
!     FRAME_1400004_STOP          = ( '2000 FEB 1 00:00:00 TDB',
!                                     '2000 MAR 1 00:00:00 TDB',
!                                     '2000 APR 1 00:00:00 TDB',
!                                     '2000 MAY 1 00:00:00 TDB'  )
!
!     FRAME_GSE_SWITCH_0              =  1400005
!     FRAME_1400005_NAME              =  'GSE_SWITCH_0'
!     FRAME_1400005_CLASS             =  5
!     FRAME_1400005_CLASS_ID          =  1400005
!     FRAME_1400005_CENTER            =  'EARTH'
!     FRAME_1400005_RELATIVE          =  'J2000'
!     FRAME_1400005_DEF_STYLE         =  'PARAMETERIZED'
!     FRAME_1400005_FAMILY            =  'TWO-VECTOR'
!     FRAME_1400005_PRI_AXIS          =  'Z'
!     FRAME_1400005_PRI_VECTOR_DEF    =  'CONSTANT'
!     FRAME_1400005_PRI_FRAME         =  'MEAN_ECL_EQ_SWITCH_0'
!     FRAME_1400005_PRI_SPEC          =  'RECTANGULAR'
!     FRAME_1400005_PRI_VECTOR        =  ( 0.0 0.0 1.0 )
!     FRAME_1400005_SEC_AXIS          =  'Y'
!     FRAME_1400005_SEC_VECTOR_DEF    =  'OBSERVER_TARGET_VELOCITY'
!     FRAME_1400005_SEC_OBSERVER      =  'EARTH'
!     FRAME_1400005_SEC_TARGET        =  'SUN'
!     FRAME_1400005_SEC_ABCORR        =  'NONE'
!     FRAME_1400005_SEC_FRAME         =  'SWITCH0'
!
!
!     FRAME_MEAN_ECL_EQ_SWITCH_0  = 1400006
!     FRAME_1400006_NAME          = 'MEAN_ECL_EQ_SWITCH_0'
!     FRAME_1400006_CLASS         = 5
!     FRAME_1400006_CLASS_ID      = 1400006
!     FRAME_1400006_CENTER        = 'EARTH'
!     FRAME_1400006_RELATIVE      = 'SWITCH0'
!     FRAME_1400006_DEF_STYLE     = 'PARAMETERIZED'
!     FRAME_1400006_FAMILY        = 'MEAN_ECLIPTIC_AND_EQUINOX_OF_DATE'
!     FRAME_1400006_PREC_MODEL    = 'EARTH_IAU_1976'
!     FRAME_1400006_OBLIQ_MODEL   = 'EARTH_IAU_1980'
!     FRAME_1400006_ROTATION_STATE = 'ROTATING'
!
!
!     FRAME_TK_0                  = 1400007
!     FRAME_1400007_NAME          = 'TK_0'
!     FRAME_1400007_CLASS         = 4
!     FRAME_1400007_CLASS_ID      = 1400007
!     FRAME_1400007_CENTER        = 499
!     TKFRAME_1400007_RELATIVE    = 'SWITCH0'
!     TKFRAME_1400007_SPEC        = 'MATRIX'
!     TKFRAME_1400007_MATRIX      = ( -1.0   0.0   0.0
!                                      0.0  -1.0   0.0
!                                      0.0   0.0   1.0 )
!
!     FRAME_BOGUS_INERTIAL        =  1400008
!     FRAME_1400008_NAME          =  'BOGUS_INERTIAL'
!     FRAME_1400008_CLASS         =  1
!     FRAME_1400008_CLASS_ID      =  -77
!     FRAME_1400008_CENTER        =  0
!
!     FRAME_SWITCH_BOGUS_INERTIAL =  1400009
!     FRAME_1400009_NAME          =  'SWITCH_BOGUS_INERTIAL'
!     FRAME_1400009_CLASS         =  6
!     FRAME_1400009_CLASS_ID      =  1400009
!     FRAME_1400009_CENTER        =  0
!     FRAME_1400009_ALIGNED_WITH  =  ( 1400008 )
!
!     FRAME_SWITCH_SINGLE_BASE    =  1400050
!     FRAME_1400050_NAME          =  'SWITCH_SINGLE_BASE'
!     FRAME_1400050_CLASS         =  6
!     FRAME_1400050_CLASS_ID      =  1400050
!     FRAME_1400050_CENTER        =  -9
!     FRAME_1400050_ALIGNED_WITH  =  ( 'CK_-9999' )
!     FRAME_1400050_START         = '2000 JAN 1 00:00:00 TDB'
!     FRAME_1400050_STOP          = '2000 JAN 2 00:00:00 TDB'
!
!     FRAME_SWITCH_BINARY_GAP     =  1400051
!     FRAME_1400051_NAME          =  'SWITCH_BINARY_GAP'
!     FRAME_1400051_CLASS         =  6
!     FRAME_1400051_CLASS_ID      =  1400051
!     FRAME_1400051_CENTER        =  -9
!     FRAME_1400051_ALIGNED_WITH  =  ( 'CK_-9999',
!                                      'CK_-8888' )
!     FRAME_1400051_START         = ( '1980 JAN 1 00:00:00 TDB',
!                                     '2011 SEP 9 01:46:40 TDB' )
!     FRAME_1400051_STOP          = ( '2011 SEP 9 01:46:40 TDB',
!                                     '2012 JAN 1 00:00:00 TDB' )
!
!     FRAME_CK_-8888              = -8888000
!     FRAME_-8888000_NAME         = 'CK_-8888'
!     FRAME_-8888000_CLASS        = 3
!     FRAME_-8888000_CLASS_ID     = -8888
!     FRAME_-8888000_CENTER       = -9
!     CK_-8888_SCLK               = -9
!
!
      TXTBUF(  1 ) = BEGDAT()

      TXTBUF(  2 ) = '      FRAME_SWITCH0               =  14' &
      //             '00000'
      TXTBUF(  3 ) = '      FRAME_1400000_NAME          =  ''' &
      //             'SWITCH0'''
      TXTBUF(  4 ) = '      FRAME_1400000_CLASS         =  6'
      TXTBUF(  5 ) = '      FRAME_1400000_CLASS_ID      =  14' &
      //             '00000'
      TXTBUF(  6 ) = '      FRAME_1400000_CENTER        =  0'
      TXTBUF(  7 ) = ' '
      TXTBUF(  8 ) = '      FRAME_1400000_ALIGNED_WITH  =  ( ' &
      //             '''GALACTIC'','
      TXTBUF(  9 ) = '                                       ' &
      //             '''B1950'','
      TXTBUF( 10 ) = '                                       ' &
      //             '''ECLIPJ2000'','
      TXTBUF( 11 ) = '                                       ' &
      //             '''J2000'','
      TXTBUF( 12 ) = '                                       ' &
      //             '''GALACTIC'','
      TXTBUF( 13 ) = '                                       ' &
      //             '''B1950'','
      TXTBUF( 14 ) = '                                       ' &
      //             '''ECLIPJ2000'','
      TXTBUF( 15 ) = '                                       ' &
      //             '''J2000''      )'
      TXTBUF( 16 ) = ' '
      TXTBUF( 17 ) = '      FRAME_1400000_START         = ( ''' &
      //             '2001 AUG 1 00:00:00 TDB'','
      TXTBUF( 18 ) = '                                      ''' &
      //             '2001 SEP 1 00:00:00 TDB'','
      TXTBUF( 19 ) = '                                      ''' &
      //             '2001 OCT 1 00:00:00 TDB'','
      TXTBUF( 20 ) = '                                      ''' &
      //             '2001 NOV 1 00:00:00 TDB'','
      TXTBUF( 21 ) = '                                      ''' &
      //             '2000 APR 1 00:00:00 TDB'','
      TXTBUF( 22 ) = '                                      ''' &
      //             '2000 MAR 1 00:00:00 TDB'','
      TXTBUF( 23 ) = '                                      ''' &
      //             '2000 FEB 1 00:00:00 TDB'','
      TXTBUF( 24 ) = '                                      ''' &
      //             '2000 JAN 1 00:00:00 TDB'' )'
      TXTBUF( 25 ) = ' '
      TXTBUF( 26 ) = '      FRAME_1400000_STOP          = ( ''' &
      //             '2001 SEP 1 00:00:00 TDB'','
      TXTBUF( 27 ) = '                                      ''' &
      //             '2001 OCT 1 00:00:00 TDB'','
      TXTBUF( 28 ) = '                                      ''' &
      //             '2001 NOV 1 00:00:00 TDB'','
      TXTBUF( 29 ) = '                                      ''' &
      //             '2001 DEC 1 00:00:00 TDB'','
      TXTBUF( 30 ) = '                                      ''' &
      //             '2000 MAY 1 00:00:00 TDB'','
      TXTBUF( 31 ) = '                                      ''' &
      //             '2000 APR 1 00:00:00 TDB'','
      TXTBUF( 32 ) = '                                      ''' &
      //             '2000 MAR 1 00:00:00 TDB'','
      TXTBUF( 33 ) = '                                      ''' &
      //             '2000 FEB 1 00:00:00 TDB'' )'
      TXTBUF( 34 ) = ' '
      TXTBUF( 35 ) = '      FRAME_SWITCH1               =  14' &
      //             '00001'
      TXTBUF( 36 ) = '      FRAME_1400001_NAME          =  ''' &
      //             'SWITCH1'''
      TXTBUF( 37 ) = '      FRAME_1400001_CLASS         =  6'
      TXTBUF( 38 ) = '      FRAME_1400001_CLASS_ID      =  14' &
      //             '00001'
      TXTBUF( 39 ) = '      FRAME_1400001_CENTER        =  0'
      TXTBUF( 40 ) = ' '
      TXTBUF( 41 ) = '      FRAME_1400001_ALIGNED_WITH  =  ( ' &
      //             '''J2000'','
      TXTBUF( 42 ) = '                                       ' &
      //             '''CK_-10001'','
      TXTBUF( 43 ) = '                                       ' &
      //             '''CK_-10000'','
      TXTBUF( 44 ) = '                                       ' &
      //             '''CK_-9999'','
      TXTBUF( 45 ) = '                                       ' &
      //             '''CK_-10001'','
      TXTBUF( 46 ) = '                                       ' &
      //             '''CK_-10000'','
      TXTBUF( 47 ) = '                                       ' &
      //             '''CK_-9999''  )'
      TXTBUF( 48 ) = ' '
      TXTBUF( 49 ) = '      FRAME_1400001_START         = ( ''' &
      //             '1990 JAN 1 00:00:00 TDB'','
      TXTBUF( 50 ) = '                                      ''' &
      //             '2001 JAN 1 00:00:00 TDB'','
      TXTBUF( 51 ) = '                                      ''' &
      //             '2001 JAN 1 00:00:00 TDB'','
      TXTBUF( 52 ) = '                                      ''' &
      //             '2001 JAN 1 00:00:00 TDB'','
      TXTBUF( 53 ) = '                                      ''' &
      //             '2000 JAN 1 00:00:00 TDB'','
      TXTBUF( 54 ) = '                                      ''' &
      //             '2000 JAN 1 00:00:00 TDB'','
      TXTBUF( 55 ) = '                                      ''' &
      //             '2000 JAN 1 00:00:00 TDB'' )'
      TXTBUF( 56 ) = ' '
      TXTBUF( 57 ) = '      FRAME_1400001_STOP          = ( ''' &
      //             '2010 JAN 1 00:00:00 TDB'','
      TXTBUF( 58 ) = '                                      ''' &
      //             '2001 APR 1 00:00:00 TDB'','
      TXTBUF( 59 ) = '                                      ''' &
      //             '2001 MAR 1 00:00:00 TDB'','
      TXTBUF( 60 ) = '                                      ''' &
      //             '2001 FEB 1 00:00:00 TDB'','
      TXTBUF( 61 ) = '                                      ''' &
      //             '2000 APR 1 00:00:00 TDB'','
      TXTBUF( 62 ) = '                                      ''' &
      //             '2000 MAR 1 00:00:00 TDB'','
      TXTBUF( 63 ) = '                                      ''' &
      //             '2000 FEB 1 00:00:00 TDB'' )'
      TXTBUF( 64 ) = ' '
      TXTBUF( 65 ) = '      FRAME_SWITCH2               =  14' &
      //             '00002'
      TXTBUF( 66 ) = '      FRAME_1400002_NAME          =  ''' &
      //             'SWITCH2'''
      TXTBUF( 67 ) = '      FRAME_1400002_CLASS         =  6'
      TXTBUF( 68 ) = '      FRAME_1400002_CLASS_ID      =  14' &
      //             '00002'
      TXTBUF( 69 ) = '      FRAME_1400002_CENTER        =  0'
      TXTBUF( 70 ) = '      FRAME_1400002_ALIGNED_WITH  =  ( ' &
      //             '''J2000'','
      TXTBUF( 71 ) = '                                       ' &
      //             '''CK_-10000'','
      TXTBUF( 72 ) = '                                       ' &
      //             '''CK_-9999''  )'
      TXTBUF( 73 ) = ' '
      TXTBUF( 74 ) = '      FRAME_SWITCH3               =  14' &
      //             '00003'
      TXTBUF( 75 ) = '      FRAME_1400003_NAME          =  ''' &
      //             'SWITCH3'''
      TXTBUF( 76 ) = '      FRAME_1400003_CLASS         =  6'
      TXTBUF( 77 ) = '      FRAME_1400003_CLASS_ID      =  14' &
      //             '00003'
      TXTBUF( 78 ) = '      FRAME_1400003_CENTER        =  0'
      TXTBUF( 79 ) = '      FRAME_1400003_ALIGNED_WITH  =  ( ' &
      //             '''SWITCH0'' )'
      TXTBUF( 80 ) = ' '
      TXTBUF( 81 ) = '      FRAME_SWITCH4               =  14' &
      //             '00004'
      TXTBUF( 82 ) = '      FRAME_1400004_NAME          =  ''' &
      //             'SWITCH4'''
      TXTBUF( 83 ) = '      FRAME_1400004_CLASS         =  6'
      TXTBUF( 84 ) = '      FRAME_1400004_CLASS_ID      =  14' &
      //             '00004'
      TXTBUF( 85 ) = '      FRAME_1400004_CENTER        =  0'
      TXTBUF( 86 ) = '      FRAME_1400004_ALIGNED_WITH  =  ( ' &
      //             '''IAU_EARTH'','
      TXTBUF( 87 ) = '                                       ' &
      //             '''TK_0'','
      TXTBUF( 88 ) = '                                       ' &
      //             '''SWITCH2'','
      TXTBUF( 89 ) = '                                       ' &
      //             '''GSE_SWITCH_0'' )'
      TXTBUF( 90 ) = ' '
      TXTBUF( 91 ) = '      FRAME_1400004_START         = ( ''' &
      //             '2000 JAN 1 00:00:00 TDB'','
      TXTBUF( 92 ) = '                                      ''' &
      //             '2000 FEB 1 00:00:00 TDB'','
      TXTBUF( 93 ) = '                                      ''' &
      //             '2000 MAR 1 00:00:00 TDB'','
      TXTBUF( 94 ) = '                                      ''' &
      //             '2000 APR 1 00:00:00 TDB'' )'
      TXTBUF( 95 ) = ' '
      TXTBUF( 96 ) = '      FRAME_1400004_STOP          = ( ''' &
      //             '2000 FEB 1 00:00:00 TDB'','
      TXTBUF( 97 ) = '                                      ''' &
      //             '2000 MAR 1 00:00:00 TDB'','
      TXTBUF( 98 ) = '                                      ''' &
      //             '2000 APR 1 00:00:00 TDB'','
      TXTBUF( 99 ) = '                                      ''' &
      //             '2000 MAY 1 00:00:00 TDB''  )'
      TXTBUF( 100 ) = ' '
      TXTBUF( 101 ) = '      FRAME_GSE_SWITCH_0              ' &
      //              '=  1400005'
      TXTBUF( 102 ) = '      FRAME_1400005_NAME              ' &
      //              '=  ''GSE_SWITCH_0'''
      TXTBUF( 103 ) = '      FRAME_1400005_CLASS             ' &
      //              '=  5'
      TXTBUF( 104 ) = '      FRAME_1400005_CLASS_ID          ' &
      //              '=  1400005'
      TXTBUF( 105 ) = '      FRAME_1400005_CENTER            ' &
      //              '=  ''EARTH'''
      TXTBUF( 106 ) = '      FRAME_1400005_RELATIVE          ' &
      //              '=  ''J2000'''
      TXTBUF( 107 ) = '      FRAME_1400005_DEF_STYLE         ' &
      //              '=  ''PARAMETERIZED'''
      TXTBUF( 108 ) = '      FRAME_1400005_FAMILY            ' &
      //              '=  ''TWO-VECTOR'''
      TXTBUF( 109 ) = '      FRAME_1400005_PRI_AXIS          ' &
      //              '=  ''Z'''
      TXTBUF( 110 ) = '      FRAME_1400005_PRI_VECTOR_DEF    ' &
      //              '=  ''CONSTANT'''
      TXTBUF( 111 ) = '      FRAME_1400005_PRI_FRAME         ' &
      //              '=  ''MEAN_ECL_EQ_SWITCH_0'''
      TXTBUF( 112 ) = '      FRAME_1400005_PRI_SPEC          ' &
      //              '=  ''RECTANGULAR'''
      TXTBUF( 113 ) = '      FRAME_1400005_PRI_VECTOR        ' &
      //              '=  ( 0.0 0.0 1.0 )'
      TXTBUF( 114 ) = '      FRAME_1400005_SEC_AXIS          ' &
      //              '=  ''Y'''
      TXTBUF( 115 ) = '      FRAME_1400005_SEC_VECTOR_DEF    ' &
      //              '=  ''OBSERVER_TARGET_VELOCITY'''
      TXTBUF( 116 ) = '      FRAME_1400005_SEC_OBSERVER      ' &
      //              '=  ''EARTH'''
      TXTBUF( 117 ) = '      FRAME_1400005_SEC_TARGET        ' &
      //              '=  ''SUN'''
      TXTBUF( 118 ) = '      FRAME_1400005_SEC_ABCORR        ' &
      //              '=  ''NONE'''
      TXTBUF( 119 ) = '      FRAME_1400005_SEC_FRAME         ' &
      //              '=  ''SWITCH0'''
      TXTBUF( 120 ) = ' '
      TXTBUF( 121 ) = ' '
      TXTBUF( 122 ) = '      FRAME_MEAN_ECL_EQ_SWITCH_0  = 14' &
      //              '00006'
      TXTBUF( 123 ) = '      FRAME_1400006_NAME          = ''' &
      //              'MEAN_ECL_EQ_SWITCH_0'''
      TXTBUF( 124 ) = '      FRAME_1400006_CLASS         = 5'
      TXTBUF( 125 ) = '      FRAME_1400006_CLASS_ID      = 14' &
      //              '00006'
      TXTBUF( 126 ) = '      FRAME_1400006_CENTER        = ''' &
      //              'EARTH'''
      TXTBUF( 127 ) = '      FRAME_1400006_RELATIVE      = ''' &
      //              'SWITCH0'''
      TXTBUF( 128 ) = '      FRAME_1400006_DEF_STYLE     = ''' &
      //              'PARAMETERIZED'''
      TXTBUF( 129 ) = '      FRAME_1400006_FAMILY        = ''' &
      //              'MEAN_ECLIPTIC_AND_EQUINOX_OF_DATE'''
      TXTBUF( 130 ) = '      FRAME_1400006_PREC_MODEL    = ''' &
      //              'EARTH_IAU_1976'''
      TXTBUF( 131 ) = '      FRAME_1400006_OBLIQ_MODEL   = ''' &
      //              'EARTH_IAU_1980'''
      TXTBUF( 132 ) = '      FRAME_1400006_ROTATION_STATE = ''' &
      //              'ROTATING'''
      TXTBUF( 133 ) = ' '
      TXTBUF( 134 ) = ' '
      TXTBUF( 135 ) = '      FRAME_TK_0                  = 14' &
      //              '00007'
      TXTBUF( 136 ) = '      FRAME_1400007_NAME          = ''' &
      //              'TK_0'''
      TXTBUF( 137 ) = '      FRAME_1400007_CLASS         = 4'
      TXTBUF( 138 ) = '      FRAME_1400007_CLASS_ID      = 14' &
      //              '00007'
      TXTBUF( 139 ) = '      FRAME_1400007_CENTER        = 49' &
      //              '9'
      TXTBUF( 140 ) = '      TKFRAME_1400007_RELATIVE    = ''' &
      //              'SWITCH0'''
      TXTBUF( 141 ) = '      TKFRAME_1400007_SPEC        = ''' &
      //              'MATRIX'''
      TXTBUF( 142 ) = '      TKFRAME_1400007_MATRIX      = ( ' &
      //              '-1.0   0.0   0.0'
      TXTBUF( 143 ) = '                                      ' &
      //              ' 0.0  -1.0   0.0'
      TXTBUF( 144 ) = '                                      ' &
      //              ' 0.0   0.0   1.0 )'
      TXTBUF( 145 ) = ' '
      TXTBUF( 146 ) = '      FRAME_BOGUS_INERTIAL        =  1' &
      //              '400008'
      TXTBUF( 147 ) = '      FRAME_1400008_NAME          =  ''' &
      //              'BOGUS_INERTIAL'''
      TXTBUF( 148 ) = '      FRAME_1400008_CLASS         =  1'
      TXTBUF( 149 ) = '      FRAME_1400008_CLASS_ID      =  -' &
      //              '77'
      TXTBUF( 150 ) = '      FRAME_1400008_CENTER        =  0'
      TXTBUF( 151 ) = ' '
      TXTBUF( 152 ) = '      FRAME_SWITCH_BOGUS_INERTIAL =  1' &
      //              '400009'
      TXTBUF( 153 ) = '      FRAME_1400009_NAME          =  ''' &
      //              'SWITCH_BOGUS_INERTIAL'''
      TXTBUF( 154 ) = '      FRAME_1400009_CLASS         =  6'
      TXTBUF( 155 ) = '      FRAME_1400009_CLASS_ID      =  1' &
      //              '400009'
      TXTBUF( 156 ) = '      FRAME_1400009_CENTER        =  0'
      TXTBUF( 157 ) = '      FRAME_1400009_ALIGNED_WITH  =  (' &
      //              ' 1400008 )'
      TXTBUF( 158 ) = ' '
      TXTBUF( 159 ) = '      FRAME_SWITCH_SINGLE_BASE    =  1' &
      //              '400050'
      TXTBUF( 160 ) = '      FRAME_1400050_NAME          =  ''' &
      //              'SWITCH_SINGLE_BASE'''
      TXTBUF( 161 ) = '      FRAME_1400050_CLASS         =  6'
      TXTBUF( 162 ) = '      FRAME_1400050_CLASS_ID      =  1' &
      //              '400050'
      TXTBUF( 163 ) = '      FRAME_1400050_CENTER        =  -' &
      //              '9'
      TXTBUF( 164 ) = '      FRAME_1400050_ALIGNED_WITH  =  (' &
      //              ' ''CK_-9999'' )'
      TXTBUF( 165 ) = '      FRAME_1400050_START         = ''' &
      //              '2000 JAN 1 00:00:00 TDB'''
      TXTBUF( 166 ) = '      FRAME_1400050_STOP          = ''' &
      //              '2000 JAN 2 00:00:00 TDB'''
      TXTBUF( 167 ) = ' '
      TXTBUF( 168 ) = '      FRAME_SWITCH_BINARY_GAP     =  1' &
      //              '400051'
      TXTBUF( 169 ) = '      FRAME_1400051_NAME          =  ''' &
      //              'SWITCH_BINARY_GAP'''
      TXTBUF( 170 ) = '      FRAME_1400051_CLASS         =  6'
      TXTBUF( 171 ) = '      FRAME_1400051_CLASS_ID      =  1' &
      //              '400051'
      TXTBUF( 172 ) = '      FRAME_1400051_CENTER        =  -' &
      //              '9'
      TXTBUF( 173 ) = '      FRAME_1400051_ALIGNED_WITH  =  (' &
      //              ' ''CK_-9999'','
      TXTBUF( 174 ) = '                                      ' &
      //              ' ''CK_-8888''     )'
      TXTBUF( 175 ) = '      FRAME_1400051_START         = ( ' &
      //              '''1980 JAN 1 00:00:00 TDB'','
      TXTBUF( 176 ) = '                                      ' &
      //              '''2011 SEP 9 01:46:40 TDB'' )'
      TXTBUF( 177 ) = '      FRAME_1400051_STOP          = ( ' &
      //              '''2011 SEP 9 01:46:40 TDB'','
      TXTBUF( 178 ) = '                                      ' &
      //              '''2012 JAN 1 00:00:00 TDB'' )'
      TXTBUF( 179 ) = ' '

      TXTBUF( 180 ) = '      FRAME_CK_-8888              = -88880' &
      //              '00'
      TXTBUF( 181 ) = '      FRAME_-8888000_NAME         = ''CK_-' &
      //              '8888'''
      TXTBUF( 182 ) = '      FRAME_-8888000_CLASS        = 3'
      TXTBUF( 183 ) = '      FRAME_-8888000_CLASS_ID     = -8888'
      TXTBUF( 184 ) = '      FRAME_-8888000_CENTER       = -9'
      TXTBUF( 185 ) = '      CK_-8888_SCLK               = -9'

      TXTBUF( 186 ) = BEGTXT()
      TXTBUF( 187 ) = ' '

      IF ( EXISTS(FK0) ) THEN
         CALL KILFIL( FK0 )
      END IF

      CALL KCLEAR

      NLINES = 187
      CALL TSTTXT ( FK0, TXTBUF, NLINES, .FALSE., .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH( FK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      IF ( EXISTS(PCK0) ) THEN
         CALL KILFIL( PCK0 )
      END IF

      CALL TSTPCK ( PCK0, .FALSE., .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH ( PCK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL TSTSPK ( SPK0, .FALSE., HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL FURNSH ( SPK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create and load CK and SCLK kernel. The SCLK kernel includes
!     CK frame specifications.
!
      IF ( EXISTS(CK0) ) THEN
         CALL KILFIL( CK0 )
      END IF

      IF ( EXISTS(SCLK0) ) THEN
         CALL KILFIL( SCLK0 )
      END IF

      CALL TSTCK3 ( CK0, SCLK0, .FALSE., .FALSE., .TRUE., CKHAN )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH ( CK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH ( SCLK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Also load LSK to support time conversions.
!
      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create and load CK with gaps. This CK uses the same SCLK
!     as CK0.
!
      IF ( EXISTS(CK1) ) THEN
         CALL KILFIL( CK1 )
      END IF

      CALL CKOPN ( CK1, CK1, 0, CKHAN1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CKID   = -8888
      SEGID  = 'Type 3 segment with gap for -8888'
      SCLKID = -9

!
!     The start epoch is the end time of the segments of CK0.
!
      CALL STR2ET ( '2011 JAN 01 01:46:40 TDB', ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL SCE2C  ( SCLKID, ET0, BEGTIM )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2015 JAN 01 00:00:00 TDB', ET1 )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL SCE2C  ( SCLKID, ET1, ENDTIM )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TICKS(1)  = ( BEGTIM + ENDTIM ) / 2
      STARTS(1) = TICKS(1)

      CALL VPACK  ( 0.D0, 0.D0, 1.D0, AVVS(1,1) )

      CALL CLEARD ( 3, QUATS(1,1) )
      QUATS(0,1) = 1.D0


      CALL CKW03  ( CKHAN1, &
                    BEGTIM, ENDTIM, CKID, 'J2000', .TRUE., &
                    SEGID,  NTICKS, TICKS, QUATS,  AVVS, &
                    NSTART, STARTS )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFCLS ( CKHAN1 )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Create identity matrices.
!
      CALL IDENT ( IDNT33 )

      CALL CLEARD ( 36, IDNT66 )

      DO I = 1, 6
         IDNT66(I,I) = 1.D0
      END DO

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Perform state transformation lookups for ' &
      //           'each base frame of frame SWITCH0' )

      NI = 8

      CALL GCPOOL ( 'FRAME_1400000_ALIGNED_WITH', &
                                           1, NI, N, BASBUF, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'BASBUF FOUND', FOUND, .TRUE., OK )

      CALL GCPOOL ( 'FRAME_1400000_START', 1, NI, N, STRBUF, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'STRBUF FOUND', FOUND, .TRUE., OK )

      CALL GCPOOL ( 'FRAME_1400000_STOP',  1, NI, N, STPBUF, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'STPBUF FOUND', FOUND, .TRUE., OK )

!
!     We expect to get the identity matrix for each call.
!
      DO I = 1, NI

         CALL STR2ET ( STRBUF(I), ET0 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL STR2ET ( STPBUF(I), ET1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Use as the lookup time the midpoint of the Ith interval.
!
         ET = ( ET0 + ET1 ) / 2

!
!        First try a direct call to ZZSWFXFM, then call SXFORM.
!
         CALL NAMFRM ( 'SWITCH0', FRAMID )
         CALL CHCKXC ( .FALSE., ' ', OK  )

         CALL ZZSWFXFM ( FRAMID, ET, 6, XFORM, OUTFRM, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!        The output frame is J2000 for inertial base frames.
!
         XFRMID = J2CODE
         CALL CHCKSI ( 'OUTFRM', OUTFRM, '=', XFRMID, 0, OK )

         BASNAM = BASBUF(I)
         CALL CHCKSC ( 'BASNAM', BASNAM, '!=', ' ', OK  )

         CALL SXFORM ( BASNAM, 'J2000', ET, XXFORM )
         CALL CHCKXC ( .FALSE., ' ', OK )

         TOL = VTIGHT

         CALL CHCKAD( 'SWITCH0 to J2000', XFORM, '~', &
                      XXFORM, 36, TOL, OK )

!
!        Use SXFORM for the remaining transformations.
!
!        Repeat the previous transformation, to test use of saved
!        values.
!
         CALL SXFORM ( BASNAM, 'J2000', ET, XXFORM )
         CALL CHCKXC ( .FALSE., ' ', OK )

         TOL = VTIGHT

         CALL CHCKAD( 'SWITCH0 to J2000', XFORM, '~', &
                      XXFORM, 36, TOL, OK )

!
!        Verify that the switch frame is aligned with its base.
!
         CALL SXFORM( 'SWITCH0', BASNAM, ET, XFORM )
         CALL CHCKXC ( .FALSE., ' ', OK )

         TOL = VTIGHT

         CALL CHCKAD( 'SWITCH0 to '//BASNAM, XFORM, '~', &
                      IDNT66, 36, TOL, OK )


         CALL SXFORM( BASNAM, 'SWITCH0', ET, XFORM )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD( 'SWITCH0 from '//BASNAM, XFORM, '~', &
                      IDNT66, 36, TOL, OK )

      END DO

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Perform state transformation lookups for ' &
      //           'frame SWITCH0, using times outside of the ' &
      //           'coverage intervals.' )
!
!     ET is less than earliest start.
!
      CALL STR2ET ( STRBUF(NI), ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET = ET0 - 1.D0

      CALL ZZSWFXFM ( FRAMID, ET, 6, XFORM, OUTFRM, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND (1)', FOUND, .FALSE., OK )

!
!     ET is greater than 5th stop and less than 1st start.
!
      CALL STR2ET ( STPBUF(5), ET1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET = ET1 + 1.D0

      CALL ZZSWFXFM ( FRAMID, ET, 6, XFORM, OUTFRM, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND (2)', FOUND, .FALSE., OK )

!
!     ET is past last stop time, which belongs to the 4th interval.
!
      CALL STR2ET ( STPBUF(4), ET1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET = ET1 + 1.D0

      CALL ZZSWFXFM ( FRAMID, ET, 6, XFORM, OUTFRM, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND (3)', FOUND, .FALSE., OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Perform state transformation lookups for ' &
      //           'frame SWITCH0, using the exact interval time ' &
      //           'bounds as request times.' )

!
!     Because the interval endpoint times are defined by strings,
!     expect to be able to duplicate the exact stored time bounds.
!     We'll use those bounds as the input times. We can't expect
!     transformation matrices to match expected values, however.
!
!     Use the buffered data from the previous test case.
!
      DO I = 1, NI

         CALL STR2ET ( STRBUF(I), ET0 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL STR2ET ( STPBUF(I), ET1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL NAMFRM ( 'SWITCH0', FRAMID )
         CALL CHCKXC ( .FALSE., ' ', OK  )

         BASNAM = BASBUF(I)
         CALL CHCKSC ( '(ET0) BASNAM', BASNAM, '!=', ' ', OK  )

         TOL = VTIGHT

!
!        For the first 4 intervals, which have increasing time order,
!        and for the last one, which is the last of a set having
!        decreasing time order, the left endpoint of each interval is
!        not masked by a higher-priority interval.
!
         IF (  ( I .LE. 4 ) .OR. ( I .EQ. NI ) ) THEN
!
!           Get state transformation to J2000 at the left endpoint.
!
            CALL SXFORM ( 'SWITCH0', 'J2000', ET0, XFORM )
            CALL CHCKXC ( .FALSE., ' ', OK )
!
!           The expected result maps from the Ith base to J2000.
!
            CALL SXFORM ( BASNAM,   'J2000', ET0, XXFORM )
            CALL CHCKXC ( .FALSE., ' ', OK )

            TOL = VTIGHT

            LABEL = 'XFORM: SWITCH0 to J2000 (#)'
            CALL REPMI( LABEL, '#', I, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD( LABEL, XFORM, '~', XXFORM, 36, TOL, OK )

!
!           Repeat the tests for position transformations.
!
            CALL PXFORM ( 'SWITCH0', 'J2000', ET0, RMAT )
            CALL CHCKXC ( .FALSE., ' ', OK )
!
!           The expected result maps from the Ith base to J2000.
!
            CALL PXFORM ( BASNAM,  'J2000', ET0, XRMAT )
            CALL CHCKXC ( .FALSE., ' ', OK )

            TOL = VTIGHT

            LABEL = 'RMAT: SWITCH0 to J2000 (#)'
            CALL REPMI( LABEL, '#', I, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD( LABEL, RMAT, '~', XRMAT, 9, TOL, OK )

         END IF

         IF ( I .GE. 4 ) THEN
!
!           For the last 4 intervals, which are in decreasing time
!           order, and for the 4th one, which is the last of a set in
!           increasing time order, the right endpoint of each interval
!           is not masked by a higher-priority interval.

!           Get state transformation to J2000 at the right endpoint.
!
            CALL SXFORM ( 'SWITCH0', 'J2000', ET1, XFORM )
            CALL CHCKXC ( .FALSE., ' ', OK )
!
!           The expected result maps from the Ith base to J2000.
!
            CALL SXFORM ( BASNAM, 'J2000', ET1, XXFORM )
            CALL CHCKXC ( .FALSE., ' ', OK )

            TOL = VTIGHT

            LABEL = 'XFORM: SWITCH0 to J2000 (#)'
            CALL REPMI( LABEL, '#', I, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD( LABEL, XFORM, '~', XXFORM, 36, TOL, OK )
!
!           Repeat the tests for position transformations.
!
            CALL PXFORM ( 'SWITCH0', 'J2000', ET1, RMAT )
            CALL CHCKXC ( .FALSE., ' ', OK )
!
!           The expected result maps from the Ith base to J2000.
!
            CALL PXFORM ( BASNAM, 'J2000', ET1, XRMAT )
            CALL CHCKXC ( .FALSE., ' ', OK )

            TOL = VTIGHT

            LABEL = 'RMAT: SWITCH0 to J2000 (#)'
            CALL REPMI( LABEL, '#', I, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD( LABEL, RMAT, '~', XRMAT, 9, TOL, OK )

         END IF

      END DO

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Touch the kernel pool, then perform position ' &
      //           'transformation lookups for each base frame of ' &
      //           'frame SWITCH0.' )

      CALL PDPOOL ( 'X', 1, 0.D0 )

!
!     We expect to get the identity matrix for each call.
!
      DO I = 1, NI

         CALL STR2ET ( STRBUF(I), ET0 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL STR2ET ( STPBUF(I), ET1 )
         CALL CHCKXC ( .FALSE., ' ', OK )


         ET = ( ET0 + ET1 ) / 2

         BASNAM = BASBUF(I)
         CALL CHCKSC ( 'BASNAM', BASNAM, '!=', ' ', OK  )

         CALL PXFORM( 'SWITCH0', BASNAM, ET, RMAT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD( 'SWITCH0 to' // BASNAM, RMAT, &
                      '~', IDNT33, 9, VTIGHT, OK )

         CALL PXFORM( BASNAM, 'SWITCH0', ET, RMAT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD( 'SWITCH0 from' // BASNAM, RMAT, &
                      '~', IDNT33, 9, VTIGHT, OK )
      END DO

      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Perform position transformation lookups for ' &
      //           'each base frame of frame SWITCH0' )

!
!     We expect to get the identity matrix for each call.
!
      DO I = 1, NI

         CALL STR2ET ( STRBUF(I), ET0 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL STR2ET ( STPBUF(I), ET1 )
         CALL CHCKXC ( .FALSE., ' ', OK )


         ET = ( ET0 + ET1 ) / 2

         BASNAM = BASBUF(I)
         CALL CHCKSC ( 'BASNAM', BASNAM, '!=', ' ', OK  )

         CALL PXFORM( 'SWITCH0', BASNAM, ET, RMAT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD( 'SWITCH0 to' // BASNAM, RMAT, &
                      '~', IDNT33, 9, VTIGHT, OK )

         CALL PXFORM( BASNAM, 'SWITCH0', ET, RMAT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD( 'SWITCH0 from' // BASNAM, RMAT, &
                      '~', IDNT33, 9, VTIGHT, OK )
      END DO


      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Perform state and position transformation ' &
      //           'lookups for each base frame of frame SWITCH1' )

      ROOM = BUFSIZ

      CALL GCPOOL ( 'FRAME_1400001_ALIGNED_WITH', &
                                           1, ROOM, N, BASBUF, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'BASBUF FOUND', FOUND, .TRUE., OK )

      CALL CHCKSI ( 'N (number of bases)', N, '=', 7, 0, OK )

      CALL GCPOOL ( 'FRAME_1400001_START', 1, ROOM, N, STRBUF, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'STRBUF FOUND', FOUND, .TRUE., OK )

      CALL GCPOOL ( 'FRAME_1400001_STOP',  1, ROOM, N, STPBUF, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'STPBUF FOUND', FOUND, .TRUE., OK )

!
!     Interval for last base:
!
      CALL STR2ET ( STRBUF(7), ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( STPBUF(7), ET1 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      ET  = ( ET0 + ET1 ) / 2

!
!     The last base is a CK frame having the base frame GALACTIC.
!
      FRNAME = CKFRAM(1)
      BASNAM = CKBASE(1)

      CALL SXFORM ( 'SWITCH1', BASNAM, ET, XFORM  )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SXFORM ( FRNAME,    BASNAM, ET, XXFORM )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD( '(7) XFORM', XFORM, '=', XXFORM, 36, 0.D0, OK )


      CALL PXFORM ( 'SWITCH1',  BASNAM, ET, RMAT  )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PXFORM ( FRNAME,     BASNAM, ET, XRMAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD( '(7) RMAT', RMAT, '=', XRMAT, 9, 0.D0, OK )

!
!     Interval for sixth base. Avoid the 7th interval.
!
      CALL STR2ET ( STPBUF(7), ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( STPBUF(6), ET1 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      ET  = ( ET0 + ET1 ) / 2

!
!     The sixth base is a CK frame having the base frame FK4.
!
      FRNAME = CKFRAM(2)
      BASNAM = CKBASE(2)

      CALL SXFORM ( 'SWITCH1', BASNAM, ET, XFORM  )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SXFORM ( FRNAME,    BASNAM, ET, XXFORM )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD( '(6) XFORM', XFORM, '=', XXFORM, 36, 0.D0, OK )


      CALL PXFORM ( 'SWITCH1',  BASNAM, ET, RMAT  )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PXFORM ( FRNAME,     BASNAM, ET, XRMAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD( '(6) RMAT', RMAT, '=', XRMAT, 9, 0.D0, OK )


!
!     Interval for 5th base. Skip over the 6th interval:
!
      CALL STR2ET ( STPBUF(6), ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( STPBUF(5), ET1 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      ET  = ( ET0 + ET1 ) / 2

!
!     The 5th base is a CK frame having the base frame J2000.
!
      FRNAME = CKFRAM(3)
      BASNAM = CKBASE(3)

      CALL SXFORM ( 'SWITCH1', BASNAM, ET, XFORM  )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SXFORM ( FRNAME,    BASNAM, ET, XXFORM )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD( '(5) XFORM', XFORM, '=', XXFORM, 36, 0.D0, OK )


      CALL PXFORM ( 'SWITCH1',  BASNAM, ET, RMAT  )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PXFORM ( FRNAME,     BASNAM, ET, XRMAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD( '(5) RMAT', RMAT, '=', XRMAT, 9, 0.D0, OK )


!
!     Interval for fourth base.
!
      CALL STR2ET ( STRBUF(4), ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( STPBUF(4), ET1 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      ET  = ( ET0 + ET1 ) / 2

!
!     The fourth base is a CK frame having the base frame GALACTIC.
!
      FRNAME = CKFRAM(1)
      BASNAM = CKBASE(1)

      CALL SXFORM ( 'SWITCH1', BASNAM, ET, XFORM  )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SXFORM ( FRNAME,    BASNAM, ET, XXFORM )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD( '(4) XFORM', XFORM, '=', XXFORM, 36, 0.D0, OK )


      CALL PXFORM ( 'SWITCH1',  BASNAM, ET, RMAT  )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PXFORM ( FRNAME,     BASNAM, ET, XRMAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD( '(4) RMAT', RMAT, '=', XRMAT, 9, 0.D0, OK )

!
!     Interval for the third base. Skip over the interval for the
!     fourth base.

      CALL STR2ET ( STPBUF(4), ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( STPBUF(3), ET1 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      ET  = ( ET0 + ET1 ) / 2

!
!     The third base is a CK frame having the base frame FK4.
!
      FRNAME = CKFRAM(2)
      BASNAM = CKBASE(2)

      CALL SXFORM ( 'SWITCH1', BASNAM, ET, XFORM  )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SXFORM ( FRNAME,    BASNAM, ET, XXFORM )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD( '(3) XFORM', XFORM, '=', XXFORM, 36, 0.D0, OK )


      CALL PXFORM ( 'SWITCH1',  BASNAM, ET, RMAT  )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PXFORM ( FRNAME,     BASNAM, ET, XRMAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD( '(3) RMAT', RMAT, '=', XRMAT, 9, 0.D0, OK )

!
!     Interval for the second base. Skip over the interval for the
!     third base.

      CALL STR2ET ( STPBUF(3), ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( STPBUF(2), ET1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET  = ( ET0 + ET1 ) / 2

!
!     The second base is a CK frame having the base frame J2000.
!
      FRNAME = CKFRAM(3)
      BASNAM = CKBASE(3)

      CALL SXFORM ( 'SWITCH1', BASNAM, ET, XFORM  )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SXFORM ( FRNAME,    BASNAM, ET, XXFORM )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD( '(2) XFORM', XFORM, '=', XXFORM, 36, 0.D0, OK )


      CALL PXFORM ( 'SWITCH1',  BASNAM, ET, RMAT  )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PXFORM ( FRNAME,     BASNAM, ET, XRMAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD( '(2) RMAT', RMAT, '=', XRMAT, 9, 0.D0, OK )


!
!     First interval for the first base. Use as a stop time the
!     common start time of the other intervals.
!
      CALL STR2ET ( STRBUF(1), ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( STRBUF(7), ET1 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      ET  = ( ET0 + ET1 ) / 2

!
!     The first base is J2000.
!
      CALL SXFORM ( 'SWITCH1',  'J2000', ET, XFORM  )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD( '(1A) XFORM', XFORM, '=', IDNT66, 36, 0.D0, OK )


      CALL PXFORM ( 'SWITCH1',  'J2000', ET, RMAT  )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD( '(1A) RMAT', RMAT, '=', IDNT33, 9, 0.D0, OK )

!
!     Second interval for the first base. Use as a start time the
!     latest stop time of intervals ending in 2000. Use as a stop time
!     the earliest start time of intervals starting in 2001.
!
      CALL STR2ET ( STPBUF(5), ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( STPBUF(2), ET1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET  = ( ET0 + ET1 ) / 2

      CALL SXFORM ( 'SWITCH1',  'J2000', ET, XFORM  )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD( '(1B) XFORM', XFORM, '=', IDNT66, 36, 0.D0, OK )


      CALL PXFORM ( 'SWITCH1',  'J2000', ET, RMAT  )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD( '(1B) RMAT', RMAT, '=', IDNT33, 9, 0.D0, OK )

!
!     Third interval for the first base. Use as a start time the
!     latest stop time of the other intervals.
!
      CALL STR2ET ( STPBUF(2), ET0 )
      CALL STR2ET ( STPBUF(1), ET1 )

      ET  = ( ET0 + ET1 ) / 2

      CALL SXFORM ( 'SWITCH1',  'J2000', ET, XFORM  )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD( '(1C) XFORM', XFORM, '=', IDNT66, 36, 0.D0, OK )


      CALL PXFORM ( 'SWITCH1',  'J2000', ET, RMAT  )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD( '(1C) RMAT', RMAT, '=', IDNT33, 9, 0.D0, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Perform state and position transformation ' &
      //           'lookups for a switch frame that doesn''t have ' &
      //           'time intervals associated with its bases.' )

      NREP = 4

      DO I = 1, NREP
!
!        Set ET to a value covered by the first base frame, which is a
!        CK frame.
!
         ET = 0.D0

         CALL SXFORM ( 'SWITCH2',   'GALACTIC', ET, XFORM  )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL SXFORM ( 'CK_-9999',  'GALACTIC', ET, XXFORM  )
         CALL CHCKXC ( .FALSE., ' ', OK )

         TOL = VTIGHT
         CALL CHCKAD( '(1A) XFORM', XFORM, '~', XXFORM, 36, TOL, OK )


         CALL PXFORM ( 'SWITCH2',  'GALACTIC', ET, RMAT  )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL PXFORM ( 'CK_-9999', 'GALACTIC', ET, XRMAT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD( '(1A) RMAT', RMAT, '~', XRMAT, 9, TOL, OK )

!
!        Set ET to a value not covered by a CK.
!
         CALL STR2ET ( '2050 JAN 1', ET )
         CALL CHCKXC ( .FALSE., ' ', OK )


         CALL SXFORM ( 'SWITCH2',   'J2000', ET, XFORM  )
         CALL CHCKXC ( .FALSE., ' ', OK )

         TOL = VTIGHT
         CALL CHCKAD( '(1B) XFORM', XFORM, '~', IDNT66, 36, TOL, OK )


         CALL PXFORM ( 'SWITCH2',  'J2000', ET, RMAT  )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD( '(1B) RMAT', RMAT, '~', IDNT33, 9, TOL, OK )

         IF ( I .EQ. 2 ) THEN
!
!           Touch the kernel pool by re-loading FK0.
!
            CALL FURNSH( FK0 )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF

      END DO


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Perform state and position transformation ' &
      //           'lookups for a switch frame that has a switch frame ' &
      //           'as its base. This frame has no associated ' &
      //           'time intervals.' )


      NREP = 4

      DO I = 1, NREP
!
!        Set ET to a value covered by the first base frame, which is a
!        CK frame.
!
         ET = 0.D0

         CALL SXFORM ( 'SWITCH3',  'SWITCH0', ET, XFORM  )
         CALL CHCKXC ( .FALSE., ' ', OK )

         TOL = VTIGHT
         CALL CHCKAD( '(1A) XFORM', XFORM, '~', IDNT66, 36, TOL, OK )


         CALL PXFORM ( 'SWITCH3',  'SWITCH0', ET, RMAT  )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD( '(1A) RMAT', RMAT, '~', IDNT33, 9, TOL, OK )

         IF ( I .EQ. 2 ) THEN
!
!           Touch the kernel pool by re-loading FK0.
!
            CALL FURNSH( FK0 )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF

      END DO

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Perform state and position transformation ' &
      //           'lookups for a switch frame that has as its ' &
      //           'bases a PCK frame, a TK frame, a switch frame, ' &
      //           'and a dynamic frame. ' &
      //           'This frame has associated time intervals.' )

      NREP = 4

!
!     Set ET to a value covered by the first base frame, which is a
!     PCK frame.
!
      CALL STR2ET ( '2000 JAN 2 00:00:00 TDB', ET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, NREP

         CALL SXFORM ( 'SWITCH4',   'J2000', ET, XFORM  )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL SXFORM ( 'IAU_EARTH', 'J2000', ET, XXFORM  )
         CALL CHCKXC ( .FALSE., ' ', OK )

         TOL = VTIGHT
         CALL CHCKAD( '(A) XFORM', XFORM, '~', XXFORM, 36, TOL, OK )


         CALL PXFORM ( 'SWITCH4',   'J2000', ET, RMAT  )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL PXFORM ( 'IAU_EARTH', 'J2000', ET, XRMAT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD( '(A) RMAT', RMAT, '~', XRMAT, 9, TOL, OK )

         IF ( I .EQ. 2 ) THEN
!
!           Touch the kernel pool by re-loading FK0.
!
            CALL FURNSH( FK0 )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF

      END DO

!
!     Set ET to a value covered by the second base frame, which is a
!     TK frame.
!
      CALL STR2ET ( '2000 FEB 2 00:00:00 TDB', ET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, NREP

         CALL SXFORM ( 'SWITCH4',   'J2000', ET, XFORM  )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL SXFORM ( 'TK_0', 'J2000', ET, XXFORM  )
         CALL CHCKXC ( .FALSE., ' ', OK )

         TOL = VTIGHT
         CALL CHCKAD( '(B) XFORM', XFORM, '~', XXFORM, 36, TOL, OK )


         CALL PXFORM ( 'SWITCH4',   'J2000', ET, RMAT  )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL PXFORM ( 'TK_0', 'J2000', ET, XRMAT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD( '(B) RMAT', RMAT, '~', XRMAT, 9, TOL, OK )

         IF ( I .EQ. 2 ) THEN
!
!           Touch the kernel pool by re-loading FK0.
!
            CALL FURNSH( FK0 )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF

      END DO

!
!     Set ET to a value covered by the third base frame, which is a
!     switch frame.
!
      CALL STR2ET ( '2000 MAR 2 00:00:00 TDB', ET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, NREP

         CALL SXFORM ( 'SWITCH4',   'SWITCH2', ET, XFORM  )
         CALL CHCKXC ( .FALSE., ' ', OK )

         TOL = VTIGHT
         CALL CHCKAD( '(1C) XFORM', XFORM, '~', IDNT66, 36, TOL, OK )


         CALL SXFORM ( 'SWITCH4',   'GALACTIC', ET, XFORM  )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL SXFORM ( 'CK_-9999',  'GALACTIC', ET, XXFORM  )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD( '(2C) XFORM', XFORM, '~', XXFORM, 36, TOL, OK )


         CALL SXFORM ( 'SWITCH4',   'SWITCH2', ET, XFORM  )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD( '(3C) XFORM', XFORM, '~', IDNT66, 36, TOL, OK )


         CALL PXFORM ( 'SWITCH4',   'SWITCH2', ET, RMAT  )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD( '(1C) RMAT', RMAT, '~', IDNT33, 9, TOL, OK )


         CALL PXFORM ( 'SWITCH4',   'GALACTIC', ET, RMAT  )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL PXFORM ( 'CK_-9999',  'GALACTIC', ET, XRMAT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD( '(2C) RMAT', RMAT, '~', XRMAT, 9, TOL, OK )

         IF ( I .EQ. 2 ) THEN
!
!           Touch the kernel pool by re-loading FK0.
!
            CALL FURNSH( FK0 )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF

      END DO

!
!     Set ET to a value covered by the fourth base frame, which is a
!     dynamic frame.
!
      CALL STR2ET ( '2000 APR 2 00:00:00 TDB', ET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, NREP

         CALL SXFORM ( 'SWITCH4',   'GSE_SWITCH_0', ET, XFORM  )
         CALL CHCKXC ( .FALSE., ' ', OK )

         TOL = VTIGHT
         CALL CHCKAD( '(1C) XFORM', XFORM, '~', IDNT66, 36, TOL, OK )

         CALL SXFORM ( 'SWITCH4',   'J2000', ET, XFORM  )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL SXFORM ( 'GSE_SWITCH_0', 'J2000', ET, XXFORM  )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD( '(2C) XFORM', XFORM, '~', XXFORM, 36, TOL, OK )


         CALL PXFORM ( 'SWITCH4',   'GSE_SWITCH_0', ET, RMAT  )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD( '(1C) RMAT', RMAT, '~', IDNT33, 9, TOL, OK )


         CALL PXFORM ( 'SWITCH4',   'J2000', ET, RMAT  )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL PXFORM ( 'GSE_SWITCH_0', 'J2000', ET, XRMAT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD( '(2C) RMAT', RMAT, '~', XRMAT, 9, TOL, OK )

         IF ( I .EQ. 2 ) THEN
!
!           Touch the kernel pool by re-loading FK0.
!
            CALL FURNSH( FK0 )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF

      END DO


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Create a switch frame having the maximum ' &
      //           'number of base frame intervals.' )

!
!     Don't clear the kernel pool: force the switch frame subsystem
!     to make room.
!
      NBASES = 15000

      CALL PIPOOL ( 'FRAME_SWITCH_MAXBASE',   1, 1400010          )
      CALL PCPOOL ( 'FRAME_1400010_NAME',     1, 'SWITCH_MAXBASE' )
      CALL PIPOOL ( 'FRAME_1400010_CLASS',    1, 6                )
      CALL PIPOOL ( 'FRAME_1400010_CLASS_ID', 1, 1400010          )
      CALL PIPOOL ( 'FRAME_1400010_CENTER',   1, 0                )
      CALL CHCKXC ( .FALSE., ' ', OK )
!
!     17 is the ID code of ECLIPJ2000.
!
      CALL FILLI  ( 17, NBASES, IBASBF )

      CALL PIPOOL ( 'FRAME_1400010_ALIGNED_WITH', NBASES, IBASBF )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create a set of disjoint intervals.
!
      DO I = 1, NBASES
         DSTRBF(I) = 2.D0 * DBLE(I-1)
         DSTPBF(I) = DSTRBF(I) + 0.9D0
      END DO

      CALL PDPOOL ( 'FRAME_1400010_START', NBASES, DSTRBF )
      CALL PDPOOL ( 'FRAME_1400010_STOP',  NBASES, DSTPBF )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Look up state transformations at times covered by the
!     base frame intervals.
!
      CALL SXFORM ( 'J2000', 'ECLIPJ2000', ET, XXFORM  )
      CALL CHCKXC ( .FALSE., ' ', OK )

      NREP = ( 100000 / NBASES ) / 3

      DO REPS = 1, NREP

         DO I = 1, NBASES
!
!           Calls used for timing tests---do not delete.
!
!            CALL SXFORM ( 'IAU_EARTH', 'J2000', ET, XFORM  )
!            CALL SXFORM ( 'J2000', 'IAU_EARTH', ET, XFORM  )
!            CALL SXFORM ( 'ECLIPJ2000', 'J2000', ET, XFORM  )
!            CALL SXFORM ( 'J2000', 'ECLIPJ2000', ET, XFORM  )
!            CALL SXFORM ( 'CK_-9999', 'J2000', ET, XFORM  )
!            CALL SXFORM ( 'J2000', 'CK_-9999', ET, XFORM  )
!            BASE = IAU_EARTH
!            BASE = ECLIPJ2000
!            BASE = CK_-9999

            DO J = 1, 3
!
!              Perform lookups at each interval endpoint and midpoint.
!
               IF ( J .EQ. 1 ) THEN

                  ET = DSTRBF(I)

               ELSE IF ( J .EQ. 2 ) THEN

                  ET = ( DSTRBF(I) + DSTPBF(I) ) / 2.D0

               ELSE
                  ET = DSTPBF(I)
               END IF

               CALL SXFORM ( 'J2000', 'SWITCH_MAXBASE', ET, XFORM )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKAD ( 'XFORM', XFORM, '~', XXFORM, 36, TOL, OK )

            END DO

         END DO

      END DO


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Fill the system with the maximum ' &
      //           'number of switch frames.' )

!
!     Don't clear the kernel pool: force the switch frame subsystem
!     to make room.
!
      NFRAMS = MAXFRM

      DO I = 1, NFRAMS

         FRNAME = 'SWITCH_MAXFRM_#'
         CALL REPMI ( FRNAME, '#', I, FRNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         FRAMID = 1500000 + I

         CALL PIPOOL ( 'FRAME_'//FRNAME, 1, FRAMID )
         CALL CHCKXC ( .FALSE., ' ', OK )

         KVNAME = 'FRAME_#_NAME'
         CALL REPMI  ( KVNAME, '#', FRAMID, KVNAME )
         CALL PCPOOL ( KVNAME,      1, FRNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         KVNAME = 'FRAME_#_CLASS'
         CALL REPMI  ( KVNAME, '#', FRAMID, KVNAME )
         CALL PIPOOL ( KVNAME,      1, 6      )
         CALL CHCKXC ( .FALSE., ' ', OK )

         KVNAME = 'FRAME_#_CLASS_ID'
         CALL REPMI  ( KVNAME, '#', FRAMID, KVNAME )
         CALL PIPOOL ( KVNAME,      1, FRAMID )
         CALL CHCKXC ( .FALSE., ' ', OK )

         KVNAME = 'FRAME_#_CENTER'
         CALL REPMI  ( KVNAME, '#', FRAMID, KVNAME )
         CALL PIPOOL ( KVNAME,      1, 0      )
         CALL CHCKXC ( .FALSE., ' ', OK )

         KVNAME = 'FRAME_#_ALIGNED_WITH'
         CALL REPMI  ( KVNAME, '#', FRAMID, KVNAME     )
         CALL PCPOOL ( KVNAME,      1, 'CK_-9999' )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

!
!     Look up state transformations for each frame.
!
      CALL NAMFRM ( 'GALACTIC', BASEID )
      CALL CHCKXC ( .FALSE., ' ', OK )


      DO I = 1, NFRAMS

         FRAMID = 1500000 + I

         ET = 0.D0

         CALL ZZSWFXFM ( FRAMID, ET, 6, XFORM, OUTFRM, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
         CALL CHCKSI ( 'OUTFRM', OUTFRM,'=', BASEID, 0, OK )

         CALL SXFORM ( 'CK_-9999', 'GALACTIC', ET, XXFORM )
         CALL CHCKXC ( .FALSE., ' ', OK )

         TOL = VTIGHT
         CALL CHCKAD ( 'XFORM', XFORM, '~', XXFORM, 36, TOL, OK )


         CALL ZZSWFXFM ( FRAMID, ET, 3, RMAT, OUTFRM, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
         CALL CHCKSI ( 'OUTFRM', OUTFRM,'=', BASEID, 0, OK )

         CALL PXFORM ( 'CK_-9999', 'GALACTIC', ET, XRMAT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         TOL = VTIGHT
         CALL CHCKAD ( 'RMAT', RMAT, '~', XRMAT, 9, TOL, OK )

      END DO

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Create more than the maximum number of switch ' &
      //           'frames that can be buffered. It should still ' &
      //           'be possible to access all of these frames.' )

!
!     Don't clear the kernel pool: force the switch frame subsystem
!     to make room.
!
      NFRAMS = MAXFRM + 100

      DO I = 1, NFRAMS

         FRNAME = 'SWITCH_MAXFRM_#'
         CALL REPMI ( FRNAME, '#', I, FRNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         FRAMID = 1600000 + I

         CALL PIPOOL ( 'FRAME_'//FRNAME, 1, FRAMID )
         CALL CHCKXC ( .FALSE., ' ', OK )

         KVNAME = 'FRAME_#_NAME'
         CALL REPMI  ( KVNAME, '#', FRAMID, KVNAME )
         CALL PCPOOL ( KVNAME,      1, FRNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         KVNAME = 'FRAME_#_CLASS'
         CALL REPMI  ( KVNAME, '#', FRAMID, KVNAME )
         CALL PIPOOL ( KVNAME,      1, 6      )
         CALL CHCKXC ( .FALSE., ' ', OK )

         KVNAME = 'FRAME_#_CLASS_ID'
         CALL REPMI  ( KVNAME, '#', FRAMID, KVNAME )
         CALL PIPOOL ( KVNAME,      1, FRAMID )
         CALL CHCKXC ( .FALSE., ' ', OK )

         KVNAME = 'FRAME_#_CENTER'
         CALL REPMI  ( KVNAME, '#', FRAMID, KVNAME )
         CALL PIPOOL ( KVNAME,      1, 0      )
         CALL CHCKXC ( .FALSE., ' ', OK )

         KVNAME = 'FRAME_#_ALIGNED_WITH'
         CALL REPMI  ( KVNAME, '#', FRAMID, KVNAME     )
         CALL PCPOOL ( KVNAME,      1, 'CK_-9999' )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

!
!     Look up state transformations for each frame.
!
      CALL NAMFRM ( 'GALACTIC', BASEID )
      CALL CHCKXC ( .FALSE., ' ', OK )


      DO I = 1, NFRAMS

         FRAMID = 1600000 + I

         ET = 0.D0

         CALL ZZSWFXFM ( FRAMID, ET, 6, XFORM, OUTFRM, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
         CALL CHCKSI ( 'OUTFRM', OUTFRM,'=', BASEID, 0, OK )

         CALL SXFORM ( 'CK_-9999', 'GALACTIC', ET, XXFORM )
         CALL CHCKXC ( .FALSE., ' ', OK )

         TOL = VTIGHT
         CALL CHCKAD ( 'XFORM', XFORM, '~', XXFORM, 36, TOL, OK )


         CALL ZZSWFXFM ( FRAMID, ET, 3, RMAT, OUTFRM, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
         CALL CHCKSI ( 'OUTFRM', OUTFRM,'=', BASEID, 0, OK )

         CALL PXFORM ( 'CK_-9999', 'GALACTIC', ET, XRMAT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         TOL = VTIGHT
         CALL CHCKAD ( 'RMAT', RMAT, '~', XRMAT, 9, TOL, OK )

      END DO


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Binary search test: create sequence of ' &
      //           'intervals with increasing start times and ' &
      //           'singleton overlap. Odd-indexed intervals ' &
      //           'map to J2000 and even-indexed intervals map to ' &
      //           'the frame GALACTIC.' )

!
!     Load an FK that defines switch frames, and use some of
!     these frames, so our new specification doesn't start at node 1.
!     Use of the frames must follow kernel pool updates.
!
      CALL FURNSH ( FK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PIPOOL ( 'FRAME_SWITCH_BINARY',    1, 1700000         )
      CALL PCPOOL ( 'FRAME_1700000_NAME',     1, 'SWITCH_BINARY' )
      CALL PIPOOL ( 'FRAME_1700000_CLASS',    1, 6               )
      CALL PIPOOL ( 'FRAME_1700000_CLASS_ID', 1, 1700000         )
      CALL PIPOOL ( 'FRAME_1700000_CENTER',   1, 0               )
      CALL CHCKXC ( .FALSE., ' ', OK )


      NBASES = 250

      DO I = 1, NBASES

         IF ( ODD(I) ) THEN
            IBASBF(I) = 1
         ELSE
            IBASBF(I) = 13
         END IF

      END DO


      CALL PIPOOL ( 'FRAME_1700000_ALIGNED_WITH', NBASES, IBASBF )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create a set of ordered intervals with singleton overlap.
!
      DO I = 1, NBASES
         DSTRBF(I) = DBLE(I-1)
         DSTPBF(I) = DSTRBF(I) + 1.0D0
      END DO

      CALL PDPOOL ( 'FRAME_1700000_START', NBASES, DSTRBF )
      CALL PDPOOL ( 'FRAME_1700000_STOP',  NBASES, DSTPBF )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Use other switch frames. This will populate the switch frame
!     database of ZZSWFXFM.
!
      CALL SXFORM ( 'SWITCH0', 'SWITCH1', 0.D0, XFORM )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SXFORM ( 'SWITCH2', 'SWITCH4', 0.D0, XFORM )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SXFORM ( 'SWITCH2', 'GSE_SWITCH_0', 0.D0, XFORM )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Look up state transformations at times covered by the
!     base frame intervals.
!
      NREP = 4

      DO REPS = 1, NREP

         DO I = 1, NBASES

            DO J = 1, 3
!
!              Perform lookups at each interval endpoint and midpoint.
!
               IF ( J .EQ. 1 ) THEN

                  ET = DSTRBF(I)

               ELSE IF ( J .EQ. 2 ) THEN

                  ET = ( DSTRBF(I) + DSTPBF(I) ) / 2.D0
               ELSE
                  ET = DSTPBF(I)
               END IF

               CALL SXFORM ( 'J2000', 'SWITCH_BINARY', ET, XFORM )
               CALL CHCKXC ( .FALSE., ' ', OK )

               IF ( I .EQ. NBASES ) THEN
!
!                 This is the highest-priority interval. We should get
!                 the same result for all three times.
!
                  CALL SXFORM ( 'J2000',   'GALACTIC', &
                                ET,        XXFORM     )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL CHCKAD ( 'a XFORM', XFORM, '~', &
                                 XXFORM,   36,    TOL, OK )
              ELSE
!
!                 The right endpoint of this interval is the left
!                 endpoint of the following, higher-priority
!                 interval.
!
                  IF ( J .EQ. 3 ) THEN

                     IF ( ODD(I) ) THEN

                        CALL SXFORM ( 'J2000',   'GALACTIC', &
                                      ET,         XXFORM )
                        CALL CHCKXC ( .FALSE., ' ', OK )

                        CALL CHCKAD ( 'a XFORM', XFORM, '~', &
                                                 XXFORM, 36, TOL, OK )
                     ELSE

                        CALL CHCKAD ( 'b XFORM', XFORM, '~', &
                                                 IDNT66, 36, TOL, OK )
                     END IF

                  ELSE

                     IF ( ODD(I) ) THEN

                        CALL CHCKAD ( 'c XFORM', XFORM, '~', &
                                                 IDNT66, 36, TOL, OK )
                     ELSE

                        CALL SXFORM ( 'J2000', 'GALACTIC', &
                                      ET,         XXFORM    )
                        CALL CHCKXC ( .FALSE., ' ', OK )

                        CALL CHCKAD ( 'd XFORM', XFORM, '~', &
                                                 XXFORM, 36, TOL, OK )
                     END IF

                  END IF
!
!                 All 3 values of J have been handled.
!
               END IF
!
!              End of the IF block for I = NBASES and I != NBASES.
!
            END DO
!
!           End of the time loop.
!
         END DO

!
!        End of the loop over all bases.
!
         IF ( REPS .EQ. 2 ) THEN
!
!           Update the kernel pool. This will force storage of the
!           frame parameters at new locations.
!
            CALL PIPOOL ( 'XYZ', 1, 3 )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF

      END DO
!
!     End of the repetition loop.
!

!
!     Try computations for times not covered by the frame's intervals.
!
      ET = DSTRBF(1) - 1.D0

      CALL SXFORM ( 'J2000', 'SWITCH_BINARY', ET, XFORM )
      CALL CHCKXC ( .TRUE., 'SPICE(NOFRAMECONNECT)', OK )

      ET = DSTPBF(NBASES) + 1.D0

      CALL SXFORM ( 'J2000', 'SWITCH_BINARY', ET, XFORM )
      CALL CHCKXC ( .TRUE., 'SPICE(NOFRAMECONNECT)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Binary search test: create sequence of disjoint ' &
      //           'intervals with increasing start times. ' &
      //           'Odd-indexed intervals map to CK_-10000 and ' &
      //           'even-indexed intervals map to the frame ' &
      //           'CK_-9999.' )

!
!     Load an FK that defines switch frames, and use some of
!     these frames, so our new specification doesn't start at node 1.
!     Use of the frames must follow kernel pool updates.
!
      CALL FURNSH ( FK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL PIPOOL ( 'FRAME_SWITCH_BINARY2',   1, 1800000         )
      CALL PCPOOL ( 'FRAME_1800000_NAME',     1, 'SWITCH_BINARY2' )
      CALL PIPOOL ( 'FRAME_1800000_CLASS',    1, 6               )
      CALL PIPOOL ( 'FRAME_1800000_CLASS_ID', 1, 1800000         )
      CALL PIPOOL ( 'FRAME_1800000_CENTER',   1, 0               )
      CALL CHCKXC ( .FALSE., ' ', OK )


      NBASES = 250

      DO I = 1, NBASES

         IF ( ODD(I) ) THEN
            IBASBF(I) = -10000000
         ELSE
            IBASBF(I) =  -9999000
         END IF

      END DO


      CALL PIPOOL ( 'FRAME_1800000_ALIGNED_WITH', NBASES, IBASBF )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create a set of ordered intervals separated by gaps.
!
      DO I = 1, NBASES
         DSTRBF(I) = 2*DBLE(I-1)
         DSTPBF(I) = DSTRBF(I) + 1.0D0
      END DO

      CALL PDPOOL ( 'FRAME_1800000_START', NBASES, DSTRBF )
      CALL PDPOOL ( 'FRAME_1800000_STOP',  NBASES, DSTPBF )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Use other switch frames. This will populate the switch frame
!     database of ZZSWFXFM.
!
      CALL SXFORM ( 'SWITCH0', 'SWITCH1', 0.D0, XFORM )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SXFORM ( 'SWITCH2', 'SWITCH4', 0.D0, XFORM )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SXFORM ( 'SWITCH2', 'GSE_SWITCH_0', 0.D0, XFORM )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Look up state transformations at times covered by the
!     base frame intervals.
!
      NREP = 4

      DO REPS = 1, NREP

         DO I = 1, NBASES

            DO J = 1, 3
!
!              Perform lookups at each interval endpoint and midpoint.
!              Try a lookup in the gap preceding each interval.
!
               IF ( J .EQ. 1 ) THEN

                  ET = DSTRBF(I)

               ELSE IF ( J .EQ. 2 ) THEN

                  ET = ( DSTRBF(I) + DSTPBF(I) ) / 2.D0

               ELSE IF ( J .EQ. 3 ) THEN

                  ET = DSTPBF(I)
               ELSE
                  ET = DSTRBF(I) - 0.5D0
               END IF

               CALL SXFORM ( 'J2000', 'SWITCH_BINARY2', ET, XFORM )

               IF ( J .EQ. 4 ) THEN

                  CALL CHCKXC ( .TRUE., 'SPICE(NOFRAMECONNECT)', OK )

               ELSE

                  CALL CHCKXC ( .FALSE., ' ', OK )

                  IF ( ODD(I) ) THEN

                     CALL SXFORM ( 'J2000', 'CK_-10000', ET, XXFORM )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     CALL CHCKAD ( 'a XFORM', XFORM, '~', &
                                              XXFORM, 36, TOL, OK )
                  ELSE

                     CALL SXFORM ( 'J2000', 'CK_-9999', ET, XXFORM )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     CALL CHCKAD ( 'b XFORM', XFORM, '~', &
                                              XXFORM, 36, TOL, OK )
                  END IF

               END IF

            END DO
!
!           End of the time loop.
!
         END DO
!
!        End of the loop over all bases.
!
         IF ( REPS .EQ. 2 ) THEN
!
!           Update the kernel pool. This will force storage of the
!           frame parameters at new locations.
!
            CALL PIPOOL ( 'XYZ', 1, 3 )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF

      END DO
!
!     End of the repetition loop.
!

!
!     Try a computation for the rightmost gap.
!
      ET = DSTPBF(NBASES) + 1.D0

      CALL SXFORM ( 'J2000', 'SWITCH_BINARY2', ET, XFORM )
      CALL CHCKXC ( .TRUE., 'SPICE(NOFRAMECONNECT)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error case: try to load frame that has too ' &
      //           'many bases to be buffered.' )


      CALL PIPOOL ( 'FRAME_TOOBIG1',          1, 1400011   )
      CALL PCPOOL ( 'FRAME_1400011_NAME',     1, 'TOOBIG1' )
      CALL PIPOOL ( 'FRAME_1400011_CLASS',    1, 6         )
      CALL PIPOOL ( 'FRAME_1400011_CLASS_ID', 1, 1400011   )
      CALL PIPOOL ( 'FRAME_1400011_CENTER',   1, 0         )
      CALL CHCKXC ( .FALSE., ' ', OK )

      NBASES = MAXBAS + 1

      CALL FILLI  ( 17, NBASES, IBASBF )

      CALL PIPOOL ( 'FRAME_1400011_ALIGNED_WITH', NBASES, IBASBF )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SXFORM ( 'TOOBIG1', 'J2000',  ET, XFORM  )
      CALL CHCKXC ( .TRUE., 'SPICE(TOOMANYBASEFRAMES)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error case: bad dimension' )


      CALL ZZSWFXFM ( 1, 0.D0, 2, XFORM, OUTFRM, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(BADDIMENSION)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error case: first stop time of BADSTOP1 ' &
      //           'is invalid' )


      CALL PIPOOL ( 'FRAME_BADSTOP1',         1, 1400012    )
      CALL PCPOOL ( 'FRAME_1400012_NAME',     1, 'BADSTOP1' )
      CALL PIPOOL ( 'FRAME_1400012_CLASS',    1, 6                )
      CALL PIPOOL ( 'FRAME_1400012_CLASS_ID', 1, 1400012          )
      CALL PIPOOL ( 'FRAME_1400012_CENTER',   1, 0                )
      CALL CHCKXC ( .FALSE., ' ', OK )

      NBASES = 10

      CALL FILLI  ( 17, NBASES, IBASBF )

      CALL PIPOOL ( 'FRAME_1400012_ALIGNED_WITH', NBASES, IBASBF )
      CALL CHCKXC ( .FALSE., ' ', OK )


      DO I = 1, NBASES
         DSTRBF(I) = 2.D0 * DBLE(I-1)
         DSTPBF(I) = DSTRBF(I) + 1.D0
      END DO

      DSTPBF(1) = DSTRBF(1)

      CALL PDPOOL ( 'FRAME_1400012_START', NBASES, DSTRBF )
      CALL PDPOOL ( 'FRAME_1400012_STOP',  NBASES, DSTPBF )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZSWFXFM ( 1400012, 0.D0, 6, XFORM, OUTFRM, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(BADTIMEBOUNDS)', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

!
!     Make sure the system recovers after the time bounds are
!     corrected.
!
      DSTPBF(1) = DSTRBF(1) + 1.D0
      CALL PDPOOL ( 'FRAME_1400012_STOP',  NBASES, DSTPBF )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZSWFXFM ( 1400012, 0.D0, 6, XFORM, OUTFRM, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )
      CALL CHCKSI ( 'OUTFRM', OUTFRM, '=', 1, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error case: stop time of BADSTOP1 ' &
      //           'at index 200 is invalid.' )


      CALL PIPOOL ( 'FRAME_BADSTOP1',         1, 1400012    )
      CALL PCPOOL ( 'FRAME_1400012_NAME',     1, 'BADSTOP1' )
      CALL PIPOOL ( 'FRAME_1400012_CLASS',    1, 6                )
      CALL PIPOOL ( 'FRAME_1400012_CLASS_ID', 1, 1400012          )
      CALL PIPOOL ( 'FRAME_1400012_CENTER',   1, 0                )
      CALL CHCKXC ( .FALSE., ' ', OK )

      NBASES = 200

      CALL FILLI  ( 17, NBASES, IBASBF )

      CALL PIPOOL ( 'FRAME_1400012_ALIGNED_WITH', NBASES, IBASBF )
      CALL CHCKXC ( .FALSE., ' ', OK )


      DO I = 1, NBASES
         DSTRBF(I) = 2.D0 * DBLE(I-1)
         DSTPBF(I) = DSTRBF(I) + 1.D0
      END DO

      DSTPBF(NBASES) = DSTRBF(NBASES)

      CALL PDPOOL ( 'FRAME_1400012_START', NBASES, DSTRBF )
      CALL PDPOOL ( 'FRAME_1400012_STOP',  NBASES, DSTPBF )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZSWFXFM ( 1400012, 0.D0, 6, XFORM, OUTFRM, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(BADTIMEBOUNDS)', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

!
!     Make sure the system recovers after the time bounds are
!     corrected.
!
      DSTPBF(NBASES) = DSTRBF(NBASES) + 1.D0
      CALL PDPOOL ( 'FRAME_1400012_STOP',  NBASES, DSTPBF )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZSWFXFM ( 1400012, 0.D0, 6, XFORM, OUTFRM, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )
      CALL CHCKSI ( 'OUTFRM', OUTFRM, '=', 1, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error case: first base frame has incomplete ' &
      //           'specification: center is missing.' )


      CALL PIPOOL ( 'FRAME_BADBASE1',         1, 1400013    )
      CALL PCPOOL ( 'FRAME_1400013_NAME',     1, 'BADBASE1' )
      CALL PIPOOL ( 'FRAME_1400013_CLASS',    1, 6          )
      CALL PIPOOL ( 'FRAME_1400013_CLASS_ID', 1, 1400013    )
      CALL PIPOOL ( 'FRAME_1400013_CENTER',   1, 0          )
      CALL CHCKXC ( .FALSE., ' ', OK )

      NBASES = 10

!
!     Make the first base SWITCH0; make the rest ECLIPJ2000.
!
      IBASBF(1) = 1400000
      CALL FILLI  ( 17, NBASES-1, IBASBF(2) )

      CALL PIPOOL ( 'FRAME_1400013_ALIGNED_WITH', NBASES, IBASBF )
      CALL CHCKXC ( .FALSE., ' ', OK )


      DO I = 1, NBASES
         DSTRBF(I) = 2.D0 * DBLE(I-1)
         DSTPBF(I) = DSTRBF(I) + 1.D0
      END DO

      CALL PDPOOL ( 'FRAME_1400013_START', NBASES, DSTRBF )
      CALL PDPOOL ( 'FRAME_1400013_STOP',  NBASES, DSTPBF )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Remove the center from SWITCH0.
!
      CALL DVPOOL ( 'FRAME_1400000_CENTER' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZSWFXFM ( 1400013, 0.D0, 6, XFORM, OUTFRM, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

!
!     Make sure the system recovers after the center is restored.
!
      CALL PIPOOL ( 'FRAME_1400000_CENTER',  1, 0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZSWFXFM ( 1400013, 0.D0, 6, XFORM, OUTFRM, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )
      CALL CHCKSI ( 'OUTFRM', OUTFRM, '=', 1400000, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error case: base frame has incomplete ' &
      //           'specification at index 200: center is missing.' )

      NBASES = 200

!
!     Make the last base SWITCH0; make the rest ECLIPJ2000.
!
      IBASBF(NBASES) = 1400000
      CALL FILLI  ( 17, NBASES-1, IBASBF )

      CALL PIPOOL ( 'FRAME_1400013_ALIGNED_WITH', NBASES, IBASBF )
      CALL CHCKXC ( .FALSE., ' ', OK )


      DO I = 1, NBASES
         DSTRBF(I) = 2.D0 * DBLE(I-1)
         DSTPBF(I) = DSTRBF(I) + 1.D0
      END DO

      CALL PDPOOL ( 'FRAME_1400013_START', NBASES, DSTRBF )
      CALL PDPOOL ( 'FRAME_1400013_STOP',  NBASES, DSTPBF )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Remove the center from SWITCH0.
!
      CALL DVPOOL ( 'FRAME_1400000_CENTER' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZSWFXFM ( 1400013, 0.D0, 6, XFORM, OUTFRM, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

!
!     Make sure the system recovers after the center is restored.
!
      CALL PIPOOL ( 'FRAME_1400000_CENTER',  1, 0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET1 = DSTPBF(NBASES)

      CALL ZZSWFXFM ( 1400013, ET1, 6, XFORM, OUTFRM, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )
      CALL CHCKSI ( 'OUTFRM', OUTFRM, '=', 1400000, 0, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error case: return on entry' )

      SHRTMS = 'SPICE(TESTERROR)'

      CALL SIGERR ( SHRTMS )

      CALL ZZSWFXFM ( 1400013, ET1, 6, XFORM, OUTFRM, FOUND )

      CALL CHCKXC ( .TRUE., SHRTMS, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error case: CKFXFM failure' )

!
!     Try a lookup using a time that cannot be converted to the
!     SCLK associated with the CK frame for instrument -9999.
!
!     Use frame SWITCH2.
!
      FRAMID = 1400002
      ET     = -3.D9

      CALL ZZSWFXFM ( FRAMID, ET, 6, XFORM, OUTFRM, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error case: CKFROT failure' )

!
!     Try a lookup using a time that cannot be converted to the
!     SCLK associated with the CK frame for instrument -9999.
!
!     Use frame SWITCH2.
!
      FRAMID = 1400002
      ET     = -3.D9

      CALL ZZSWFXFM ( FRAMID, ET, 3, XFORM, OUTFRM, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error case: TISBOD failure' )

!
!     The top priority base frame of frame SWITCH4, over the
!     time interval
!
!        2000 JAN 1 00:00:00 TDB
!        2000 FEB 1 00:00:00 TDB
!
!     is a PCK frame. We'll get rid of necessary PCK data and use
!     a request time that will cause the frame subsystem to attempt
!     to compute the orientation of this frame.
!
      CALL STR2ET ( '2000 JAN 2 00:00:00 TDB', ET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DVPOOL ( 'BODY399_POLE_RA' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      FRAMID = 1400004
      CALL ZZSWFXFM ( FRAMID, ET, 6, XFORM, OUTFRM, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

!
!     Restore the PCK data.
!
      CALL FURNSH ( PCK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error case: TIPBOD failure' )

!
!     Use the same technique as was used for TISBOD to generate the
!     desired error.
!
      CALL STR2ET ( '2000 JAN 2 00:00:00 TDB', ET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DVPOOL ( 'BODY399_POLE_RA' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      FRAMID = 1400004
      CALL ZZSWFXFM ( FRAMID, ET, 3, XFORM, OUTFRM, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

!
!     Restore the PCK data.
!
      CALL FURNSH ( PCK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error case: TKFRAM failure' )

!
!     Again we'll delete data needed by frame SWITCH4.
!
      CALL STR2ET ( '2000 FEB 2 00:00:00 TDB', ET )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The frame ID 1400007 is that of frame TK_0.
!
      CALL DVPOOL ( 'TKFRAME_1400007_RELATIVE' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      FRAMID = 1400004
      CALL ZZSWFXFM ( FRAMID, ET, 3, XFORM, OUTFRM, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(VARIABLENOTFOUND)', OK )

!
!     Restore the frame data.
!
      CALL FURNSH ( FK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error case: IRFROT failure' )

!
!     We'll use a frame that has no time intervals and
!     one bogus inertial base frame.
!
      FRAMID = 1400009
      ET     = 0.D0
      CALL ZZSWFXFM ( FRAMID, ET, 3, XFORM, OUTFRM, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(IRFNOTREC)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Non-error exception case: in binary search ' &
      //           'mode, CKFXFM cannot find data.' )

!
!     Use the frame SWITCH_BINARY_GAP. The request time is
!     covered by this frame's second interval, but there's
!     no CK data available
!
      FRAMID = 1400051
      CALL STR2ET ( '2012 JAN 1 00:00:00 TDB', ET )

      CALL ZZSWFXFM ( FRAMID, ET, 6, XFORM, OUTFRM, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

      CALL SXFORM ( 'SWITCH_BINARY_GAP', 'J2000', ET, XFORM )
      CALL CHCKXC ( .TRUE., 'SPICE(NOFRAMECONNECT)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Non-error exception case: in binary search ' &
      //           'mode, CKFROT cannot find data.' )

!
!     Use the frame SWITCH_BINARY_GAP.
!
      FRAMID = 1400051
      CALL STR2ET ( '2012 JAN 1 00:00:00 TDB', ET )

      CALL ZZSWFXFM ( FRAMID, ET, 3, XFORM, OUTFRM, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

      CALL PXFORM ( 'SWITCH_BINARY_GAP', 'J2000', ET, RMAT )
      CALL CHCKXC ( .TRUE., 'SPICE(NOFRAMECONNECT)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Non-error exception case: in binary search ' &
      //           'mode, CKFXFM cannot find data at start time of ' &
      //           'interval, but data for the preceding interval ' &
      //           'with common endpoint are available.' )

!
!     Use the frame SWITCH_BINARY_GAP.
!
      FRAMID = 1400051

!
!     The request time is the stop time of the first interval.
!
      CALL STR2ET ( '2011 SEP 9 01:46:40 TDB', ET )

      CALL ZZSWFXFM ( FRAMID, ET, 6, XFORM, OUTFRM, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!     The expected base frame is the base of CK_-9999: GALACTIC
!
      CALL CHCKSI ( 'OUTFRM', OUTFRM, '=', 13, 0, OK )

      CALL SXFORM ( 'SWITCH_BINARY_GAP', 'J2000', ET, XFORM )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Verify that a slightly later time lies in the gap.
!
      ET = ET + 1.D-5

      CALL ZZSWFXFM ( FRAMID, ET, 6, XFORM, OUTFRM, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Non-error exception case: in binary search ' &
      //           'mode, CKFFOT cannot find data at stop time of ' &
      //           'interval, but data for the next interval with ' &
      //           'common endpoint are available.' )

!
!     Use the frame SWITCH_BINARY_GAP.
!
      FRAMID = 1400051

!
!     The request time is the stop time of the first interval.
!
      CALL STR2ET ( '2011 SEP 9 01:46:40 TDB', ET )

      CALL ZZSWFXFM ( FRAMID, ET, 3, RMAT, OUTFRM, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!     The expected base frame is the base of CK_-9999: GALACTIC
!
      CALL CHCKSI ( 'OUTFRM', OUTFRM, '=', 13, 0, OK )

      CALL PXFORM ( 'SWITCH_BINARY_GAP', 'J2000', ET, RMAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Verify that a slightly later time lies in the gap.
!
      ET = ET + 1.D-5

      CALL ZZSWFXFM ( FRAMID, ET, 3, RMAT, OUTFRM, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Non-error exception case: binary search ' &
      //           'on a single interval, result found, 6x6 case.' )

!
!     Use frame SWITCH_SINGLE_BASE.
!
      FRAMID = 1400050

      CALL STR2ET ( '2000 JAN 1 18:00:00 TDB', ET )

      CALL ZZSWFXFM ( FRAMID, ET, 6, XFORM, OUTFRM, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Non-error exception case: binary search ' &
      //           'on a single interval, result not found, ' &
      //           '6x6 case.' )

!
!     Use frame SWITCH_SINGLE_BASE.
!
      FRAMID = 1400050

      CALL STR2ET ( '2000 JAN 2 00:00:00.001 TDB', ET )

      CALL ZZSWFXFM ( FRAMID, ET, 6, XFORM, OUTFRM, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

      CALL STR2ET ( '1999 DEC 31 23:59:59.999 TDB', ET )

      CALL ZZSWFXFM ( FRAMID, ET, 6, XFORM, OUTFRM, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Non-error exception case: binary search ' &
      //           'on a single interval, result found, 3x3 case.' )

!
!     Use frame SWITCH_SINGLE_BASE.
!
      FRAMID = 1400050

      CALL STR2ET ( '2000 JAN 1 18:00:00 TDB', ET )

      CALL ZZSWFXFM ( FRAMID, ET, 3, RMAT, OUTFRM, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Non-error exception case: binary search ' &
      //           'on a single interval, result not found, ' &
      //           '3x3 case.' )

!
!     Use frame SWITCH_SINGLE_BASE.
!
      FRAMID = 1400050

      CALL STR2ET ( '2000 JAN 2 00:00:00.001 TDB', ET )

      CALL ZZSWFXFM ( FRAMID, ET, 3, RMAT, OUTFRM, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

      CALL STR2ET ( '1999 DEC 31 23:59:59.999 TDB', ET )

      CALL ZZSWFXFM ( FRAMID, ET, 3, XFORM, OUTFRM, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZSWFINI Error case: return on entry' )

      SHRTMS = 'SPICE(TESTERROR)'

      CALL SIGERR ( SHRTMS )

      CALL ZZSWFINI ( HDFRAM, FRPOOL, FIDLST, HDBASE, &
                      FREE,   PRVAT,  PRVFRM, SAMFRM  )
      CALL CHCKXC ( .TRUE., SHRTMS, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Clean up.' )

      CALL KCLEAR
      CALL KILFIL ( FK0   )
      CALL KILFIL ( CK0   )
      CALL KILFIL ( CK1   )
      CALL KILFIL ( PCK0  )
      CALL KILFIL ( SCLK0 )
      CALL KILFIL ( SPK0  )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END
