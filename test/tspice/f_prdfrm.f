C$Procedure F_PRDFRM ( Product frame tests )

      SUBROUTINE F_PRDFRM ( OK )

C$ Abstract
C
C     Exercise the product frame computation capabilities of
C     the SPICELIB routines ZZDYNFRM, ZZDYNROT, and their shadow
C     routines.
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
C     This test family exercises the product frame computation
C     capabilities of the SPICELIB routines ZZDYNFRM, ZZDYNROT, and
C     their shadow routines.
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
C-    TSPICE Version 1.0.0, 22-SEP-2020 (NJB)
C
C-&

C
C     Test utility functions
C
      CHARACTER*(10)        BEGDAT
      CHARACTER*(10)        BEGTXT

C
C     SPICELIB functions
C
      LOGICAL               EXISTS

C
C     Local Parameters
C
      CHARACTER*(*)         FK0
      PARAMETER           ( FK0    = 'PRDFRM_test0.tf' )

      CHARACTER*(*)         PCK0
      PARAMETER           ( PCK0   = 'PRDFRM_test0.tpc' )

      CHARACTER*(*)         SPK0
      PARAMETER           ( SPK0   = 'PRDFRM_test0.bsp' )


      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-14  )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               TXTSIZ
      PARAMETER           ( TXTSIZ = 300 )

C
C     Local Variables
C
      CHARACTER*(LNSIZE)    TXTBUF ( TXTSIZ )

      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ET0
      DOUBLE PRECISION      ET1
      DOUBLE PRECISION      IDNT33 ( 3, 3 )
      DOUBLE PRECISION      IDNT66 ( 6, 6 )
      DOUBLE PRECISION      RMAT   ( 3, 3 )
      DOUBLE PRECISION      STEP
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      XFORM  ( 6, 6 )
      DOUBLE PRECISION      XRMAT  ( 3, 3 )
      DOUBLE PRECISION      XXFORM ( 6, 6 )

      INTEGER               HANDLE
      INTEGER               I
      INTEGER               N
      INTEGER               NLINES

C
C     Saved values
C
      SAVE

C
C     Open the test family.
C
      CALL TOPEN ( 'F_PRDFRM' )


C***********************************************************************
C
C     Normal cases
C
C***********************************************************************

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Setup: create and load CK0, SCLK0, FK0, '
     .//           'PCK0, SPK0, and LSK file.' )

C
C     Create a text kernel containing the following frame
C     specifications:
C
C     FRAME_SINGLETON_PRODUCT     =  1400000
C     FRAME_1400000_NAME          = 'SINGLETON_PRODUCT'
C     FRAME_1400000_CLASS         =  5
C     FRAME_1400000_CLASS_ID      =  1400000
C     FRAME_1400000_CENTER        =  499
C     FRAME_1400000_RELATIVE      = 'J2000'
C     FRAME_1400000_DEF_STYLE     = 'PARAMETERIZED'
C     FRAME_1400000_FAMILY        = 'PRODUCT'
C     FRAME_1400000_FROM_FRAMES   = 'IAU_EARTH'
C     FRAME_1400000_TO_FRAMES     = 'IAU_JUPITER'
C
C     FRAME_IAU_MARS_PRODUCT      =  1400499
C     FRAME_1400499_NAME          = 'IAU_MARS_PRODUCT'
C     FRAME_1400499_CLASS         =  5
C     FRAME_1400499_CLASS_ID      =  1400499
C     FRAME_1400499_CENTER        =  499
C     FRAME_1400499_RELATIVE      = 'J2000'
C     FRAME_1400499_DEF_STYLE     = 'PARAMETERIZED'
C     FRAME_1400499_FAMILY        = 'PRODUCT'
C     FRAME_1400499_FROM_FRAMES   = ( 'IAU_EARTH'
C                                     'IAU_JUPITER'
C                                     'J2000' )
C     FRAME_1400499_TO_FRAMES     = ( 'IAU_MARS'
C                                     'IAU_EARTH'
C                                     'IAU_JUPITER')
C
C     FRAME_MARS_EQUATOR_SOLAR_PROD   =  1400001
C     FRAME_1400001_NAME              =  'MARS_EQUATOR_SOLAR_PROD'
C     FRAME_1400001_CLASS             =  5
C     FRAME_1400001_CLASS_ID          =  1400001
C     FRAME_1400001_CENTER            =  'EARTH'
C     FRAME_1400001_RELATIVE          =  'J2000'
C     FRAME_1400001_DEF_STYLE         =  'PARAMETERIZED'
C     FRAME_1400001_FAMILY            =  'TWO-VECTOR'
C     FRAME_1400001_PRI_AXIS          =  'Z'
C     FRAME_1400001_PRI_VECTOR_DEF    =  'CONSTANT'
C     FRAME_1400001_PRI_FRAME         =  'IAU_MARS_PRODUCT'
C     FRAME_1400001_PRI_SPEC          =  'RECTANGULAR'
C     FRAME_1400001_PRI_VECTOR        =  ( 0.0 0.0 1.0 )
C     FRAME_1400001_SEC_AXIS          =  'Y'
C     FRAME_1400001_SEC_VECTOR_DEF    =  'OBSERVER_TARGET_VELOCITY'
C     FRAME_1400001_SEC_OBSERVER      =  'MARS'
C     FRAME_1400001_SEC_TARGET        =  'SUN'
C     FRAME_1400001_SEC_ABCORR        =  'NONE'
C     FRAME_1400001_SEC_FRAME         =  'J2000'
C
C     FRAME_MARS_EQUATOR_SOLAR        =  1400002
C     FRAME_1400002_NAME              =  'MARS_EQUATOR_SOLAR'
C     FRAME_1400002_CLASS             =  5
C     FRAME_1400002_CLASS_ID          =  1400002
C     FRAME_1400002_CENTER            =  'EARTH'
C     FRAME_1400002_RELATIVE          =  'J2000'
C     FRAME_1400002_DEF_STYLE         =  'PARAMETERIZED'
C     FRAME_1400002_FAMILY            =  'TWO-VECTOR'
C     FRAME_1400002_PRI_AXIS          =  'Z'
C     FRAME_1400002_PRI_VECTOR_DEF    =  'CONSTANT'
C     FRAME_1400002_PRI_FRAME         =  'IAU_MARS'
C     FRAME_1400002_PRI_SPEC          =  'RECTANGULAR'
C     FRAME_1400002_PRI_VECTOR        =  ( 0.0 0.0 1.0 )
C     FRAME_1400002_SEC_AXIS          =  'Y'
C     FRAME_1400002_SEC_VECTOR_DEF    =  'OBSERVER_TARGET_VELOCITY'
C     FRAME_1400002_SEC_OBSERVER      =  'MARS'
C     FRAME_1400002_SEC_TARGET        =  'SUN'
C     FRAME_1400002_SEC_ABCORR        =  'NONE'
C     FRAME_1400002_SEC_FRAME         =  'J2000'
C
C
C
C
C
      TXTBUF(  1 ) = BEGDAT()
      TXTBUF(  2 ) = ' '
      TXTBUF(  3 ) = ' '
      TXTBUF(  4 ) = '      FRAME_SINGLETON_PRODUCT     =  14'
     .//             '00000'
      TXTBUF(  5 ) = '      FRAME_1400000_NAME          = ''S'
     .//             'INGLETON_PRODUCT'''
      TXTBUF(  6 ) = '      FRAME_1400000_CLASS         =  5'
      TXTBUF(  7 ) = '      FRAME_1400000_CLASS_ID      =  14'
     .//             '00000'
      TXTBUF(  8 ) = '      FRAME_1400000_CENTER        =  49'
     .//             '9'
      TXTBUF(  9 ) = '      FRAME_1400000_RELATIVE      = ''J'
     .//             '2000'''
      TXTBUF( 10 ) = '      FRAME_1400000_DEF_STYLE     = ''P'
     .//             'ARAMETERIZED'''
      TXTBUF( 11 ) = '      FRAME_1400000_FAMILY        = ''P'
     .//             'RODUCT'''
      TXTBUF( 12 ) = '      FRAME_1400000_FROM_FRAMES   = ''I'
     .//             'AU_EARTH'''
      TXTBUF( 13 ) = '      FRAME_1400000_TO_FRAMES     = ''I'
     .//             'AU_JUPITER'''
      TXTBUF( 14 ) = ' '
      TXTBUF( 15 ) = '      FRAME_IAU_MARS_PRODUCT      =  14'
     .//             '00499'
      TXTBUF( 16 ) = '      FRAME_1400499_NAME          = ''I'
     .//             'AU_MARS_PRODUCT'''
      TXTBUF( 17 ) = '      FRAME_1400499_CLASS         =  5'
      TXTBUF( 18 ) = '      FRAME_1400499_CLASS_ID      =  14'
     .//             '00499'
      TXTBUF( 19 ) = '      FRAME_1400499_CENTER        =  49'
     .//             '9'
      TXTBUF( 20 ) = '      FRAME_1400499_RELATIVE      = ''J'
     .//             '2000'''
      TXTBUF( 21 ) = '      FRAME_1400499_DEF_STYLE     = ''P'
     .//             'ARAMETERIZED'''
      TXTBUF( 22 ) = '      FRAME_1400499_FAMILY        = ''P'
     .//             'RODUCT'''
      TXTBUF( 23 ) = '      FRAME_1400499_FROM_FRAMES   = ( '''
     .//             'IAU_EARTH'''
      TXTBUF( 24 ) = '                                      '''
     .//             'IAU_JUPITER'''
      TXTBUF( 25 ) = '                                      '''
     .//             'J2000'' )'
      TXTBUF( 26 ) = '      FRAME_1400499_TO_FRAMES     = ( '''
     .//             'IAU_MARS'''
      TXTBUF( 27 ) = '                                      '''
     .//             'IAU_EARTH'''
      TXTBUF( 28 ) = '                                      '''
     .//             'IAU_JUPITER'')'
      TXTBUF( 29 ) = ' '
      TXTBUF( 30 ) = '      FRAME_MARS_EQUATOR_SOLAR_PROD   ='
     .//             '  1400001'
      TXTBUF( 31 ) = '      FRAME_1400001_NAME              ='
     .//             '  ''MARS_EQUATOR_SOLAR_PROD'''
      TXTBUF( 32 ) = '      FRAME_1400001_CLASS             ='
     .//             '  5'
      TXTBUF( 33 ) = '      FRAME_1400001_CLASS_ID          ='
     .//             '  1400001'
      TXTBUF( 34 ) = '      FRAME_1400001_CENTER            ='
     .//             '  ''EARTH'''
      TXTBUF( 35 ) = '      FRAME_1400001_RELATIVE          ='
     .//             '  ''J2000'''
      TXTBUF( 36 ) = '      FRAME_1400001_DEF_STYLE         ='
     .//             '  ''PARAMETERIZED'''
      TXTBUF( 37 ) = '      FRAME_1400001_FAMILY            ='
     .//             '  ''TWO-VECTOR'''
      TXTBUF( 38 ) = '      FRAME_1400001_PRI_AXIS          ='
     .//             '  ''Z'''
      TXTBUF( 39 ) = '      FRAME_1400001_PRI_VECTOR_DEF    ='
     .//             '  ''CONSTANT'''
      TXTBUF( 40 ) = '      FRAME_1400001_PRI_FRAME         ='
     .//             '  ''IAU_MARS_PRODUCT'''
      TXTBUF( 41 ) = '      FRAME_1400001_PRI_SPEC          ='
     .//             '  ''RECTANGULAR'''
      TXTBUF( 42 ) = '      FRAME_1400001_PRI_VECTOR        ='
     .//             '  ( 0.0 0.0 1.0 )'
      TXTBUF( 43 ) = '      FRAME_1400001_SEC_AXIS          ='
     .//             '  ''Y'''
      TXTBUF( 44 ) = '      FRAME_1400001_SEC_VECTOR_DEF    ='
     .//             '  ''OBSERVER_TARGET_VELOCITY'''
      TXTBUF( 45 ) = '      FRAME_1400001_SEC_OBSERVER      ='
     .//             '  ''MARS'''
      TXTBUF( 46 ) = '      FRAME_1400001_SEC_TARGET        ='
     .//             '  ''SUN'''
      TXTBUF( 47 ) = '      FRAME_1400001_SEC_ABCORR        ='
     .//             '  ''NONE'''
      TXTBUF( 48 ) = '      FRAME_1400001_SEC_FRAME         ='
     .//             '  ''J2000'''
      TXTBUF( 49 ) = ' '
      TXTBUF( 50 ) = '      FRAME_MARS_EQUATOR_SOLAR        ='
     .//             '  1400002'
      TXTBUF( 51 ) = '      FRAME_1400002_NAME              ='
     .//             '  ''MARS_EQUATOR_SOLAR'''
      TXTBUF( 52 ) = '      FRAME_1400002_CLASS             ='
     .//             '  5'
      TXTBUF( 53 ) = '      FRAME_1400002_CLASS_ID          ='
     .//             '  1400002'
      TXTBUF( 54 ) = '      FRAME_1400002_CENTER            ='
     .//             '  ''EARTH'''
      TXTBUF( 55 ) = '      FRAME_1400002_RELATIVE          ='
     .//             '  ''J2000'''
      TXTBUF( 56 ) = '      FRAME_1400002_DEF_STYLE         ='
     .//             '  ''PARAMETERIZED'''
      TXTBUF( 57 ) = '      FRAME_1400002_FAMILY            ='
     .//             '  ''TWO-VECTOR'''
      TXTBUF( 58 ) = '      FRAME_1400002_PRI_AXIS          ='
     .//             '  ''Z'''
      TXTBUF( 59 ) = '      FRAME_1400002_PRI_VECTOR_DEF    ='
     .//             '  ''CONSTANT'''
      TXTBUF( 60 ) = '      FRAME_1400002_PRI_FRAME         ='
     .//             '  ''IAU_MARS'''
      TXTBUF( 61 ) = '      FRAME_1400002_PRI_SPEC          ='
     .//             '  ''RECTANGULAR'''
      TXTBUF( 62 ) = '      FRAME_1400002_PRI_VECTOR        ='
     .//             '  ( 0.0 0.0 1.0 )'
      TXTBUF( 63 ) = '      FRAME_1400002_SEC_AXIS          ='
     .//             '  ''Y'''
      TXTBUF( 64 ) = '      FRAME_1400002_SEC_VECTOR_DEF    ='
     .//             '  ''OBSERVER_TARGET_VELOCITY'''
      TXTBUF( 65 ) = '      FRAME_1400002_SEC_OBSERVER      ='
     .//             '  ''MARS'''
      TXTBUF( 66 ) = '      FRAME_1400002_SEC_TARGET        ='
     .//             '  ''SUN'''
      TXTBUF( 67 ) = '      FRAME_1400002_SEC_ABCORR        ='
     .//             '  ''NONE'''
      TXTBUF( 68 ) = '      FRAME_1400002_SEC_FRAME         ='
     .//             '  ''J2000'''
      TXTBUF( 69 ) = ' '
      TXTBUF( 70 ) = BEGTXT()
      TXTBUF( 71 ) = ' '

      IF ( EXISTS(FK0) ) THEN
         CALL KILFIL( FK0 )
      END IF

      NLINES = 71
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

C
C     Also load LSK to support time conversions.
C
      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Create identity matrices.
C
      CALL IDENT ( IDNT33 )

      CALL CLEARD ( 36, IDNT66 )

      DO I = 1, 6
         IDNT66(I,I) = 1.D0
      END DO

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Get the state transformation from J2000 to '
     .//           'SINGLETON_PRODUCT. This should match the '
     .//           'transformation from IAU_EARTH to IAU_JUPITER '
     .//           'at the same epoch.' )

      CALL STR2ET ( '2020 OCT 1', ET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SXFORM ( 'J2000', 'SINGLETON_PRODUCT', ET, XFORM )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SXFORM ( 'IAU_EARTH', 'IAU_JUPITER', ET, XXFORM )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKAD ( 'XFORM a', XFORM, '~', XXFORM, 36, TOL, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Get the position transformation from J2000 to '
     .//           'SINGLETON_PRODUCT. This should match the '
     .//           'transformation from IAU_EARTH to IAU_JUPITER '
     .//           'at the same epoch.' )

      CALL STR2ET ( '2020 OCT 1', ET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PXFORM ( 'J2000', 'SINGLETON_PRODUCT', ET, RMAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PXFORM ( 'IAU_EARTH', 'IAU_JUPITER', ET, XRMAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKAD ( 'RMAT', RMAT, '~', XRMAT, 9, TOL, OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Get the state transformation from J2000 to '
     .//           'IAU_MARS_PRODUCT. This should match the '
     .//           'transformation from J2000 to IAU_MARS '
     .//           'at the same epoch.' )

      CALL STR2ET ( '2020 OCT 1', ET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SXFORM ( 'J2000', 'IAU_MARS_PRODUCT', ET, XFORM )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SXFORM ( 'J2000', 'IAU_MARS', ET, XXFORM )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKAD ( 'XFORM a', XFORM, '~', XXFORM, 36, TOL, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Get the position transformation from J2000 to '
     .//           'IAU_MARS_PRODUCT. This should match the '
     .//           'transformation from J2000 to IAU_MARS '
     .//           'at the same epoch.' )

      CALL STR2ET ( '2020 OCT 1', ET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PXFORM ( 'J2000', 'IAU_MARS_PRODUCT', ET, RMAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PXFORM ( 'J2000', 'IAU_MARS', ET, XRMAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKAD ( 'RMAT', RMAT, '~', XRMAT, 9, TOL, OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Get the state transformation from J2000 to '
     .//           'MARS_EQUATOR_SOLAR_PROD. This should match the '
     .//           'transformation from J2000 to MARS_EQUATOR_SOLAR '
     .//           'at the same epoch. Compute transformations '
     .//           'for a sequence of times.' )

      CALL STR2ET ( '2005 OCT 1', ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK  )

      CALL STR2ET ( '2005 OCT 30', ET1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      N    = 100
      STEP = ( ET1 - ET0 ) / ( N - 1 )

      TOL = VTIGHT

      DO I = 1, N

         ET = ET0 + (I-1)*STEP

         CALL SXFORM ( 'J2000', 'MARS_EQUATOR_SOLAR_PROD', ET, XFORM )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL SXFORM ( 'J2000', 'MARS_EQUATOR_SOLAR', ET, XXFORM )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( 'XFORM', XFORM, '~', XXFORM, 36, TOL, OK )

      END DO

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Get the position transformation from J2000 to '
     .//           'MARS_EQUATOR_SOLAR_PROD. This should match the '
     .//           'transformation from J2000 to MARS_EQUATOR_SOLAR '
     .//           'at the same epoch. Compute transformations '
     .//           'for a sequence of times.' )

      CALL STR2ET ( '2005 OCT 1', ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK  )

      CALL STR2ET ( '2005 OCT 30', ET1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      N    = 100
      STEP = ( ET1 - ET0 ) / ( N - 1 )

      TOL = VTIGHT

      DO I = 1, N

         ET = ET0 + (I-1)*STEP

         CALL PXFORM ( 'J2000', 'MARS_EQUATOR_SOLAR_PROD', ET, RMAT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL PXFORM ( 'J2000', 'MARS_EQUATOR_SOLAR', ET, XRMAT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( 'RMAT', RMAT, '~', XRMAT, 9, TOL, OK )

      END DO
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Clean up.' )

      CALL KCLEAR
      CALL KILFIL ( FK0   )
      CALL KILFIL ( PCK0  )
      CALL KILFIL ( SPK0  )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN
      END
