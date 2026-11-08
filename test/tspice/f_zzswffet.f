C$Procedure F_ZZSWFFET ( ZZSWFFET tests )

      SUBROUTINE F_ZZSWFFET ( OK )

C$ Abstract
C
C     Exercise the SPICELIB routine ZZSWFFET.
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

      INCLUDE 'errhnd.inc'
      INCLUDE 'frmtyp.inc'
      INCLUDE 'zzswtchf.inc'

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
C     This routine tests the SPICELIB routine ZZSWFFET.
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
C-    TSPICE Version 1.0.0, 04-DEC-2021 (NJB)
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
      PARAMETER           ( FK0    = 'zzswffet_test0.tf' )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               TXTSIZ
      PARAMETER           ( TXTSIZ = 300 )

C
C     Local Variables
C
      CHARACTER*(FRNMLN)    BASNAM
      CHARACTER*(SMSGLN)    SHRTMS
      CHARACTER*(LNSIZE)    TXTBUF ( TXTSIZ )
      CHARACTER*(FRNMLN)    XBASE  ( MAXBAS )

      DOUBLE PRECISION      ET
      DOUBLE PRECISION      STARTS ( MAXBAS )
      DOUBLE PRECISION      STOPS  ( MAXBAS )
      DOUBLE PRECISION      XFORM  ( 6, 6 )
      DOUBLE PRECISION      XSTART ( MAXBAS )
      DOUBLE PRECISION      XSTOP  ( MAXBAS )

      INTEGER               BASBEG ( MAXFRM )
      INTEGER               BASCNT ( MAXFRM )
      INTEGER               BASLST ( MAXBAS )
      INTEGER               CLSSES ( MAXBAS )
      INTEGER               CLSIDS ( MAXBAS )
      INTEGER               FIDLST ( MAXBAS )
      INTEGER               FRAMAT
      INTEGER               FRAMID
      INTEGER               FRPOOL ( LBSNGL : MAXFRM )
      INTEGER               FREE
      INTEGER               HDFRAM ( MAXFRM )
      INTEGER               I
      INTEGER               IBASBF ( MAXBAS )
      INTEGER               J
      INTEGER               NBASES
      INTEGER               NLINES
      INTEGER               NWHDBS ( MAXFRM )
      INTEGER               NWHDFR ( MAXFRM )
      INTEGER               NWPOOL ( LBSNGL : MAXFRM )
      INTEGER               XCLASS ( MAXBAS )
      INTEGER               XCLSID ( MAXBAS )

      LOGICAL               BINARY ( MAXFRM )
      LOGICAL               HAVTIM
      LOGICAL               USETIM ( MAXFRM )

C
C     Saved values
C
      SAVE

C
C     Initial values
C

C
C     Open the test family.
C
      CALL TOPEN ( 'F_ZZSWFFET' )


C***********************************************************************
C
C     Normal cases
C
C***********************************************************************

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Create and load FK0.' )

C
C     Create a text kernel containing the following frame
C     specifications:
C
C
C     BEGDAT()
C     FRAME_SWITCH0               =  1400000
C     FRAME_1400000_NAME          =  'SWITCH0'
C     FRAME_1400000_CLASS         =  6
C     FRAME_1400000_CLASS_ID      =  1400000
C     FRAME_1400000_CENTER        =  0
C     FRAME_1400000_ALIGNED_WITH  =  ( 'J2000',
C                                      'ECLIPJ2000',
C                                      'B1950',
C                                      'GALACTIC' )
C
C     FRAME_1400000_START         = ( '2000 JAN 1 00:00:00 TDB',
C                                     '2000 FEB 1 00:00:00 TDB',
C                                     '2000 MAR 1 00:00:00 TDB',
C                                     '2000 APR 1 00:00:00 TDB' )
C
C     FRAME_1400000_STOP          = ( '2000 FEB 1 00:00:00 TDB',
C                                     '2000 MAR 1 00:00:00 TDB',
C                                     '2000 APR 1 00:00:00 TDB',
C                                     '2000 MAY 1 00:00:00 TDB'  )
C
C     FRAME_SWITCH1               =  1400001
C     FRAME_1400001_NAME          =  'SWITCH1'
C     FRAME_1400001_CLASS         =  6
C     FRAME_1400001_CLASS_ID      =  1400001
C     FRAME_SWITCH1_CENTER        =  0
C     FRAME_1400001_ALIGNED_WITH  =  ( 'J2000',
C                                      'ECLIPJ2000',
C                                      'B1950',
C                                      'GALACTIC' )
C
C     FRAME_1400001_START         = ( @2000-JAN-1/00:00:00,
C                                     @2000-FEB-1/00:00:00,
C                                     @2000-MAR-1/00:00:00,
C                                     @2000-APR-1/00:00:00  )
C
C     FRAME_1400001_STOP          = ( @2000-FEB-1/00:00:00,
C                                     @2000-MAR-1/00:00:00,
C                                     @2000-APR-1/00:00:00,
C                                     @2000-MAY-1/00:00:00  )
C
C     FRAME_SWITCH2               =  1400002
C     FRAME_1400002_NAME          =  'SWITCH2'
C     FRAME_1400002_CLASS         =  6
C     FRAME_1400002_CLASS_ID      =  1400002
C     FRAME_1400002_CENTER        =  'SOLAR SYSTEM BARYCENTER'
C     FRAME_1400002_ALIGNED_WITH  =  ( 1,
C                                      17,
C                                      2,
C                                      13 )
C
C     FRAME_1400002_START         = ( '2000 JAN 1 00:00:00 TDB',
C                                     '2000 FEB 1 00:00:00 TDB',
C                                     '2000 MAR 1 00:00:00 TDB',
C                                     '2000 APR 1 00:00:00 TDB' )
C
C     FRAME_1400002_STOP          = ( @2000-FEB-1/00:00:00,
C                                     @2000-MAR-1/00:00:00,
C                                     @2000-APR-2/00:00:00,
C                                     @2000-MAY-1/00:00:00  )
C
C     FRAME_SWITCH3               =  1400003
C     FRAME_1400003_NAME          =  'SWITCH3'
C     FRAME_1400003_CLASS         =  6
C     FRAME_1400003_CLASS_ID      =  1400003
C     FRAME_1400003_CENTER        =  'SOLAR SYSTEM BARYCENTER'
C     FRAME_1400003_ALIGNED_WITH  =  ( 1,
C                                      17,
C                                      2,
C                                      13 )
C     BEGTXT()
C

      TXTBUF(  1 ) = BEGDAT()
      TXTBUF(  2 ) = '      FRAME_SWITCH0               =  14'
     .//             '00000'
      TXTBUF(  3 ) = '      FRAME_1400000_NAME          =  '''
     .//             'SWITCH0'''
      TXTBUF(  4 ) = '      FRAME_1400000_CLASS         =  6'
      TXTBUF(  5 ) = '      FRAME_1400000_CLASS_ID      =  14'
     .//             '00000'
      TXTBUF(  6 ) = '      FRAME_1400000_CENTER        =  0'
      TXTBUF(  7 ) = '      FRAME_1400000_ALIGNED_WITH  =  ( '
     .//             '''J2000'','
      TXTBUF(  8 ) = '                                       '
     .//             '''ECLIPJ2000'','
      TXTBUF(  9 ) = '                                       '
     .//             '''B1950'','
      TXTBUF( 10 ) = '                                       '
     .//             '''GALACTIC'' )'
      TXTBUF( 11 ) = ' '
      TXTBUF( 12 ) = '      FRAME_1400000_START         = ( '''
     .//             '2000 JAN 1 00:00:00 TDB'','
      TXTBUF( 13 ) = '                                      '''
     .//             '2000 FEB 1 00:00:00 TDB'','
      TXTBUF( 14 ) = '                                      '''
     .//             '2000 MAR 1 00:00:00 TDB'','
      TXTBUF( 15 ) = '                                      '''
     .//             '2000 APR 1 00:00:00 TDB'' )'
      TXTBUF( 16 ) = ' '
      TXTBUF( 17 ) = '      FRAME_1400000_STOP          = ( '''
     .//             '2000 FEB 1 00:00:00 TDB'','
      TXTBUF( 18 ) = '                                      '''
     .//             '2000 MAR 1 00:00:00 TDB'','
      TXTBUF( 19 ) = '                                      '''
     .//             '2000 APR 1 00:00:00 TDB'','
      TXTBUF( 20 ) = '                                      '''
     .//             '2000 MAY 1 00:00:00 TDB''  )'
      TXTBUF( 21 ) = ' '
      TXTBUF( 22 ) = '      FRAME_SWITCH1               =  14'
     .//             '00001'
      TXTBUF( 23 ) = '      FRAME_1400001_NAME          =  '''
     .//             'SWITCH1'''
      TXTBUF( 24 ) = '      FRAME_1400001_CLASS         =  6'
      TXTBUF( 25 ) = '      FRAME_1400001_CLASS_ID      =  14'
     .//             '00001'
      TXTBUF( 26 ) = '      FRAME_SWITCH1_CENTER        =  0'
      TXTBUF( 27 ) = '      FRAME_1400001_ALIGNED_WITH  =  ( '
     .//             '''J2000'','
      TXTBUF( 28 ) = '                                       '
     .//             '''ECLIPJ2000'','
      TXTBUF( 29 ) = '                                       '
     .//             '''B1950'','
      TXTBUF( 30 ) = '                                       '
     .//             '''GALACTIC'' )'
      TXTBUF( 31 ) = ' '
      TXTBUF( 32 ) = '      FRAME_1400001_START         = ( @'
     .//             '2000-JAN-1/00:00:00,'
      TXTBUF( 33 ) = '                                      @'
     .//             '2000-FEB-1/00:00:00,'
      TXTBUF( 34 ) = '                                      @'
     .//             '2000-MAR-1/00:00:00,'
      TXTBUF( 35 ) = '                                      @'
     .//             '2000-APR-1/00:00:00  )'
      TXTBUF( 36 ) = ' '
      TXTBUF( 37 ) = '      FRAME_1400001_STOP          = ( @'
     .//             '2000-FEB-1/00:00:00,'
      TXTBUF( 38 ) = '                                      @'
     .//             '2000-MAR-1/00:00:00,'
      TXTBUF( 39 ) = '                                      @'
     .//             '2000-APR-1/00:00:00,'
      TXTBUF( 40 ) = '                                      @'
     .//             '2000-MAY-1/00:00:00  )'
      TXTBUF( 41 ) = ' '
      TXTBUF( 42 ) = '      FRAME_SWITCH2               =  14'
     .//             '00002'
      TXTBUF( 43 ) = '      FRAME_1400002_NAME          =  '''
     .//             'SWITCH2'''
      TXTBUF( 44 ) = '      FRAME_1400002_CLASS         =  6'
      TXTBUF( 45 ) = '      FRAME_1400002_CLASS_ID      =  14'
     .//             '00002'
      TXTBUF( 46 ) = '      FRAME_1400002_CENTER        =  '''
     .//             'SOLAR SYSTEM BARYCENTER'''
      TXTBUF( 47 ) = '      FRAME_1400002_ALIGNED_WITH  =  ( '
     .//             '1,'
      TXTBUF( 48 ) = '                                       '
     .//             '17,'
      TXTBUF( 49 ) = '                                       '
     .//             '2,'
      TXTBUF( 50 ) = '                                       '
     .//             '13 )'
      TXTBUF( 51 ) = ' '
      TXTBUF( 52 ) = '      FRAME_1400002_START         = ( '''
     .//             '2000 JAN 1 00:00:00 TDB'','
      TXTBUF( 53 ) = '                                      '''
     .//             '2000 FEB 1 00:00:00 TDB'','
      TXTBUF( 54 ) = '                                      '''
     .//             '2000 MAR 1 00:00:00 TDB'','
      TXTBUF( 55 ) = '                                      '''
     .//             '2000 APR 1 00:00:00 TDB'' )'
      TXTBUF( 56 ) = ' '
      TXTBUF( 57 ) = '      FRAME_1400002_STOP          = ( @'
     .//             '2000-FEB-1/00:00:00,'
      TXTBUF( 58 ) = '                                      @'
     .//             '2000-MAR-1/00:00:00,'
      TXTBUF( 59 ) = '                                      @'
     .//             '2000-APR-2/00:00:00,'
      TXTBUF( 60 ) = '                                      @'
     .//             '2000-MAY-1/00:00:00  )'
      TXTBUF( 61 ) = ' '
      TXTBUF( 62 ) = '      FRAME_SWITCH3               =  14'
     .//             '00003'
      TXTBUF( 63 ) = '      FRAME_1400003_NAME          =  '''
     .//             'SWITCH3'''
      TXTBUF( 64 ) = '      FRAME_1400003_CLASS         =  6'
      TXTBUF( 65 ) = '      FRAME_1400003_CLASS_ID      =  14'
     .//             '00003'
      TXTBUF( 66 ) = '      FRAME_1400003_CENTER        =  '''
     .//             'SOLAR SYSTEM BARYCENTER'''
      TXTBUF( 67 ) = '      FRAME_1400003_ALIGNED_WITH  =  ( '
     .//             '1,'
      TXTBUF( 68 ) = '                                       '
     .//             '17,'
      TXTBUF( 69 ) = '                                       '
     .//             '2,'
      TXTBUF( 70 ) = '                                       '
     .//             '13 )'
      TXTBUF( 71 ) = BEGTXT()



      IF ( EXISTS(FK0) ) THEN
         CALL KILFIL( FK0 )
      END IF

      NLINES = 71
      CALL TSTTXT ( FK0, TXTBUF, NLINES, .TRUE., .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Also load LSK to support time conversions.
C
      CALL TSTLSK

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Fetch data for frame SWITCH0 in FK0, using free '
     .//           '= 1.' )


      XBASE(1)  = 'J2000'
      XBASE(2)  = 'ECLIPJ2000'
      XBASE(3)  = 'B1950'
      XBASE(4)  = 'GALACTIC'

      XCLASS(1) = INERTL
      XCLASS(2) = INERTL
      XCLASS(3) = INERTL
      XCLASS(4) = INERTL

      XCLSID(1) = 1
      XCLSID(2) = 17
      XCLSID(3) = 2
      XCLSID(4) = 13

      CALL STR2ET( '2000 JAN 1 00:00:00 TDB', XSTART(1) )
      CALL STR2ET( '2000 FEB 1 00:00:00 TDB', XSTART(2) )
      CALL STR2ET( '2000 MAR 1 00:00:00 TDB', XSTART(3) )
      CALL STR2ET( '2000 APR 1 00:00:00 TDB', XSTART(4) )

      XSTOP(1) = XSTART(2)
      XSTOP(2) = XSTART(3)
      XSTOP(3) = XSTART(4)

      CALL STR2ET( '2000 MAY 1 00:00:00 TDB', XSTOP(4) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      FRAMID = 1400000

C
C     Start with clean database.
C
      CALL ZZHSIINI ( MAXFRM, HDFRAM, FRPOOL )
      CALL CLEARI   ( MAXFRM, FIDLST         )
      CALL CLEARI   ( MAXFRM, BASBEG         )
      FREE = 1

      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG,
     .                FREE,   BASCNT, USETIM, BINARY, CLSSES,
     .                CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'FRAMAT', FRAMAT, '!=', 0, 0, OK )

      IF ( FRAMAT .NE. 0 ) THEN

         HAVTIM = USETIM(FRAMAT)
         CALL CHCKSL ( 'HAVTIM', HAVTIM, .TRUE., OK )

         CALL CHCKSL ( 'BINARY', BINARY(FRAMAT), .TRUE., OK )

         NBASES = BASCNT(FRAMAT)
         CALL CHCKSI ( 'NBASES', NBASES, '=', 4, 0, OK )

         DO I = 1, NBASES

            J = BASBEG(FRAMAT) - 1 + I

            CALL FRMNAM ( BASLST(J), BASNAM )

            CALL CHCKSC( 'BASNAM', BASNAM,    '=', XBASE(I),        OK)
            CALL CHCKSD( 'STARTS', STARTS(J), '=', XSTART(I), 0.D0, OK)
            CALL CHCKSD( 'STOPS',  STOPS(J),  '=', XSTOP(I),  0.D0, OK)
            CALL CHCKSI( 'CLSSES', CLSSES(J), '=', XCLASS(I), 0,    OK)
            CALL CHCKSI( 'CLSIDS', CLSIDS(J), '=', XCLSID(I), 0,    OK)

         END DO

      END IF

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Fetch data for frame SWITCH0 in FK0, using '
     .//           'a value of FREE greater than 1.' )

      FRAMID = 1400000

C
C     Start with clean database.
C
      CALL ZZHSIINI ( MAXFRM, HDFRAM, FRPOOL )
      CALL CLEARI   ( MAXFRM, FIDLST         )
      CALL CLEARI   ( MAXFRM, BASBEG         )
      FREE = 100


      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG,
     .                FREE,   BASCNT, USETIM, BINARY, CLSSES,
     .                CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'FRAMAT', FRAMAT, '!=', 0, 0, OK )

      IF ( FRAMAT .NE. 0 ) THEN

         HAVTIM = USETIM(FRAMAT)
         CALL CHCKSL ( 'HAVTIM', HAVTIM, .TRUE., OK )

         CALL CHCKSL ( 'BINARY', BINARY(FRAMAT), .TRUE., OK )

         NBASES = BASCNT(FRAMAT)
         CALL CHCKSI ( 'NBASES', NBASES, '=', 4, 0, OK )

         DO I = 1, NBASES

            J = BASBEG(FRAMAT) - 1 + I

            CALL FRMNAM ( BASLST(J), BASNAM )

            CALL CHCKSC( 'BASNAM', BASNAM,    '=',
     .                               XBASE(I),  OK  )

            CALL CHCKSD( 'STARTS', STARTS(J), '=',
     .                             XSTART(I), 0.D0, OK )

            CALL CHCKSD( 'STOPS', STOPS(J), '=',
     .                            XSTOP(I),  0.D0, OK )

            CALL CHCKSI( 'CLSSES', CLSSES(J), '=', XCLASS(I), 0, OK)
            CALL CHCKSI( 'CLSIDS', CLSIDS(J), '=', XCLSID(I), 0, OK)

         END DO

      END IF

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Fetch data for frame SWITCH1 in FK0.' )

C
C     Frame SWITCH1 differs from SWITCH0 only in the time interval
C     bound data type.
C
      FRAMID = 1400001

      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG,
     .                FREE,   BASCNT, USETIM, BINARY, CLSSES,
     .                CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'FRAMAT', FRAMAT, '!=', 0, 0, OK )

      IF ( FRAMAT .NE. 0 ) THEN

         HAVTIM = USETIM(FRAMAT)
         CALL CHCKSL ( 'HAVTIM', HAVTIM, .TRUE., OK )

         CALL CHCKSL ( 'BINARY', BINARY(FRAMAT), .TRUE., OK )

         NBASES = BASCNT(FRAMAT)
         CALL CHCKSI ( 'NBASES', NBASES, '=', 4, 0, OK )

         DO I = 1, NBASES

            J = BASBEG(FRAMAT) - 1 + I

            CALL FRMNAM ( BASLST(J), BASNAM )

            CALL CHCKSC( 'BASNAM', BASNAM,    '=', XBASE(I),        OK)
            CALL CHCKSD( 'STARTS', STARTS(J), '=', XSTART(I), 0.D0, OK)
            CALL CHCKSD( 'STOPS',  STOPS(J),  '=', XSTOP(I),  0.D0, OK)
            CALL CHCKSI( 'CLSSES', CLSSES(J), '=', XCLASS(I), 0,    OK)
            CALL CHCKSI( 'CLSIDS', CLSIDS(J), '=', XCLSID(I), 0,    OK)

         END DO

      END IF

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Fetch data for frame SWITCH2 in FK0.' )

C
C     Frame SWITCH2 differs from SWITCH0 only in the base frame
C     data type and base frame stop times.
C
      CALL STR2ET( '2000 FEB 1 00:00:00 TDB', XSTOP(1) )
      CALL STR2ET( '2000 MAR 1 00:00:00 TDB', XSTOP(2) )
      CALL STR2ET( '2000 APR 2 00:00:00 TDB', XSTOP(3) )
      CALL STR2ET( '2000 MAY 1 00:00:00 TDB', XSTOP(4) )

      FRAMID = 1400002

      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG,
     .                FREE,   BASCNT, USETIM, BINARY, CLSSES,
     .                CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'FRAMAT', FRAMAT, '!=', 0, 0, OK )

      IF ( FRAMAT .NE. 0 ) THEN

         HAVTIM = USETIM(FRAMAT)

         CALL CHCKSL ( 'HAVTIM', HAVTIM, .TRUE., OK )

         CALL CHCKSL ( 'BINARY', BINARY(FRAMAT), .FALSE., OK )

         NBASES = BASCNT(FRAMAT)
         CALL CHCKSI ( 'NBASES', NBASES, '=', 4, 0, OK )

         DO I = 1, NBASES

            J = BASBEG(FRAMAT) - 1 + I

            CALL FRMNAM ( BASLST(J), BASNAM )

            CALL CHCKSC( 'BASNAM', BASNAM,    '=', XBASE(I),        OK)
            CALL CHCKSD( 'STARTS', STARTS(J), '=', XSTART(I), 0.D0, OK)
            CALL CHCKSD( 'STOPS', STOPS(J),   '=', XSTOP(I),  0.D0, OK)
            CALL CHCKSI( 'CLSSES', CLSSES(J), '=', XCLASS(I), 0,    OK)
            CALL CHCKSI( 'CLSIDS', CLSIDS(J), '=', XCLSID(I), 0,    OK)

         END DO

      END IF


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Fetch data for frame SWITCH3 in FK0.' )

C
C     Frame SWITCH3 has no time intervals.
C
      FRAMID = 1400003

      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG,
     .                FREE,   BASCNT, USETIM, BINARY, CLSSES,
     .                CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'FRAMAT', FRAMAT, '!=', 0, 0, OK )

      IF ( FRAMAT .NE. 0 ) THEN

         HAVTIM = USETIM(FRAMAT)
         CALL CHCKSL ( 'HAVTIM', HAVTIM, .FALSE., OK )

         CALL CHCKSL ( 'BINARY', BINARY(FRAMAT), .FALSE., OK )

         NBASES = BASCNT(FRAMAT)
         CALL CHCKSI ( 'NBASES', NBASES, '=', 4, 0, OK )

         DO I = 1, NBASES

            J = BASBEG(FRAMAT) - 1 + I

            CALL FRMNAM ( BASLST(J), BASNAM )

            CALL CHCKSC( 'BASNAM', BASNAM,    '=', XBASE(I),     OK )
            CALL CHCKSI( 'CLSSES', CLSSES(J), '=', XCLASS(I), 0, OK )
            CALL CHCKSI( 'CLSIDS', CLSIDS(J), '=', XCLSID(I), 0, OK )

         END DO

      END IF

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Create and use a switch frame having half the '
     .//           'maximum number of base frame intervals.' )

C
C     Don't clear the kernel pool: force the switch frame subsystem
C     to make room.
C
      NBASES = MAXBAS/2

      CALL PIPOOL ( 'FRAME_SWITCH_MAXBASE',   1, 1400010          )
      CALL PCPOOL ( 'FRAME_1400010_NAME',     1, 'SWITCH_MAXBASE' )
      CALL PIPOOL ( 'FRAME_1400010_CLASS',    1, 6                )
      CALL PIPOOL ( 'FRAME_1400010_CLASS_ID', 1, 1400010          )
      CALL PIPOOL ( 'FRAME_1400010_CENTER',   1, 0                )
      CALL CHCKXC ( .FALSE., ' ', OK )
C
C     17 is the ID code of ECLIPJ2000.
C
      CALL FILLI  ( 17, NBASES, IBASBF )
      CALL PIPOOL ( 'FRAME_1400010_ALIGNED_WITH', NBASES, IBASBF )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FILLI  ( INERTL, NBASES, XCLASS )
      CALL FILLI  ( 17,     NBASES, XCLSID )

C
C     Create a set of disjoint intervals.
C
      DO I = 1, NBASES
         XSTART(I) = 2.D0 * DBLE(I-1)
         XSTOP(I) = XSTART(I) + 0.9D0
      END DO

      CALL PDPOOL ( 'FRAME_1400010_START', NBASES, XSTART )
      CALL PDPOOL ( 'FRAME_1400010_STOP',  NBASES, XSTOP )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Do a frame transformation to populate the switch frame database.
C
      ET = XSTART( NBASES/2 ) + 0.5D0
      CALL SXFORM ( 'SWITCH_MAXBASE', 'J2000', ET, XFORM )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Fetch data for the frame.
C
      FRAMID = 1400010

      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG,
     .                FREE,   BASCNT, USETIM, BINARY, CLSSES,
     .                CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )

      CALL CHCKXC ( .FALSE., ' ', OK )
C
C     This test depends on the previous test cases.
C
      CALL CHCKSI ( 'FREE', FREE, '=', 7616, 0, OK )

      CALL CHCKSI ( 'FRAMAT', FRAMAT, '!=', 0, 0, OK )

      IF ( FRAMAT .NE. 0 ) THEN
C
C       This test depends on the previous test cases.
C
         CALL CHCKSI ( 'BASBEG(FRAMAT)',
     .                  BASBEG(FRAMAT), '=', 116, 0, OK )

         HAVTIM = USETIM(FRAMAT)

         CALL CHCKSL ( 'HAVTIM', HAVTIM, .TRUE., OK )

         CALL CHCKSL ( 'BINARY', BINARY(FRAMAT), .TRUE., OK )

         CALL CHCKSI ( 'NBASES', BASCNT(FRAMAT), '=', NBASES, 0, OK )

         DO I = 1, NBASES

            J = BASBEG(FRAMAT) - 1 + I

            CALL CHCKSI( 'BASEID', BASLST(J), '=', IBASBF(I), 0,    OK)
            CALL CHCKSD( 'STARTS', STARTS(J), '=', XSTART(I), 0.D0, OK)
            CALL CHCKSD( 'STOPS',  STOPS(J),  '=', XSTOP(I),  0.D0, OK)
            CALL CHCKSI( 'CLSSES', CLSSES(J), '=', XCLASS(I), 0,    OK)
            CALL CHCKSI( 'CLSIDS', CLSIDS(J), '=', XCLSID(I), 0,    OK)

         END DO

      END IF

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Create and use a switch frame having the maximum '
     .//           'number of base frame intervals.' )

C
C     Don't clear the kernel pool: force the switch frame subsystem
C     to make room.
C
      NBASES = MAXBAS

      CALL PIPOOL ( 'FRAME_SWITCH_MAXBASE',   1, 1400011          )
      CALL PCPOOL ( 'FRAME_1400011_NAME',     1, 'SWITCH_MAXBASE' )
      CALL PIPOOL ( 'FRAME_1400011_CLASS',    1, 6                )
      CALL PIPOOL ( 'FRAME_1400011_CLASS_ID', 1, 1400011          )
      CALL PIPOOL ( 'FRAME_1400011_CENTER',   1, 0                )
      CALL CHCKXC ( .FALSE., ' ', OK )
C
C     17 is the ID code of ECLIPJ2000.
C
      CALL FILLI  ( 17, NBASES, IBASBF )
      CALL PIPOOL ( 'FRAME_1400011_ALIGNED_WITH', NBASES, IBASBF )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FILLI  ( INERTL, NBASES, XCLASS )
      CALL FILLI  ( 17,     NBASES, XCLSID )

C
C     Create a set of disjoint intervals.
C
      DO I = 1, NBASES
         XSTART(I) = 2.D0 * DBLE(I-1)
         XSTOP(I) = XSTART(I) + 0.9D0
      END DO

      CALL PDPOOL ( 'FRAME_1400011_START', NBASES, XSTART )
      CALL PDPOOL ( 'FRAME_1400011_STOP',  NBASES, XSTOP )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Do a frame transformation to populate the switch frame database.
C
      ET = XSTART( NBASES/2 ) + 0.5D0
      CALL SXFORM ( 'SWITCH_MAXBASE', 'J2000', ET, XFORM )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Fetch data for the frame.
C
      FRAMID = 1400011

      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG,
     .                FREE,   BASCNT, USETIM, BINARY, CLSSES,
     .                CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     This test depends on the previous test cases.
C
      CALL CHCKSI ( 'FREE', FREE, '=', MAXBAS+1, 0, OK )

      CALL CHCKSI ( 'FRAMAT', FRAMAT, '!=', 0, 0, OK )

      IF ( FRAMAT .NE. 0 ) THEN
C
C       This test depends on the previous test cases.
C
         CALL CHCKSI ( 'BASBEG(FRAMAT)',
     .                  BASBEG(FRAMAT), '=', 1, 0, OK )

         HAVTIM = USETIM(FRAMAT)

         CALL CHCKSL ( 'HAVTIM', HAVTIM, .TRUE., OK )

         CALL CHCKSL ( 'BINARY', BINARY(FRAMAT), .TRUE., OK )

         CALL CHCKSI ( 'NBASES', BASCNT(FRAMAT), '=', NBASES, 0, OK )

         DO I = 1, NBASES

            J = BASBEG(FRAMAT) - 1 + I

            CALL CHCKSI( 'BASEID', BASLST(J), '=', IBASBF(I), 0,    OK)
            CALL CHCKSD( 'STARTS', STARTS(J), '=', XSTART(I), 0.D0, OK)
            CALL CHCKSD( 'STOPS',  STOPS(J),  '=', XSTOP(I),  0.D0, OK)
            CALL CHCKSI( 'CLSSES', CLSSES(J), '=', XCLASS(I), 0,    OK)
            CALL CHCKSI( 'CLSIDS', CLSIDS(J), '=', XCLSID(I), 0,    OK)

         END DO

      END IF

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Look up a frame that doesn''t exist.' )

C
C     Prepare initialized data structures to compare against.
C
      CALL ZZHSIINI ( MAXFRM, NWHDFR, NWPOOL )
      CALL CLEARI   ( MAXFRM, NWHDBS )
      CALL CHCKXC ( .FALSE., ' ', OK )


      FRAMID = -1

      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG,
     .                FREE,   BASCNT, USETIM, BINARY, CLSSES,
     .                CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )


      CALL CHCKXC ( .TRUE., 'SPICE(MISSINGFRAMEVAR)', OK )

      CALL CHCKSI ( 'FRAMAT', FRAMAT, '=', 0, 0, OK )

      CALL CHCKAI ( 'FRPOOL', FRPOOL, '=', NWPOOL, MAXFRM+6, OK )
      CALL CHCKAI ( 'HDFRAM', HDFRAM, '=', NWHDFR, MAXFRM,   OK )
      CALL CHCKAI ( 'BASBEG', BASBEG, '=', NWHDBS, MAXFRM,   OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Missing switch frame name' )

      FRAMID = 1500000

      CALL PIPOOL ( 'FRAME_SWITCH_ERROR',         1, FRAMID )
C
C     Don't set the frame name.
C
      CALL PIPOOL ( 'FRAME_1500000_CLASS',        1, 6      )
      CALL PIPOOL ( 'FRAME_1500000_CLASS_ID',     1, FRAMID )
      CALL PIPOOL ( 'FRAME_1500000_CENTER',       1, 0      )
      CALL PIPOOL ( 'FRAME_1500000_ALIGNED_WITH', 1, 1      )

      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG,
     .                FREE,   BASCNT, USETIM, BINARY, CLSSES,
     .                CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )


      CALL CHCKXC ( .TRUE., 'SPICE(MISSINGFRAMEVAR)', OK )

      CALL CHCKSI ( 'FRAMAT', FRAMAT, '=', 0, 0, OK )

      CALL CHCKAI ( 'FRPOOL', FRPOOL, '=', NWPOOL, MAXFRM+6, OK )
      CALL CHCKAI ( 'HDFRAM', HDFRAM, '=', NWHDFR, MAXFRM,   OK )
      CALL CHCKAI ( 'BASBEG', BASBEG, '=', NWHDBS, MAXFRM,   OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Missing switch frame ID' )

      FRAMID = 1500000

C
C     Don't set the frame ID in the kernel pool.
C
      CALL DVPOOL ( 'FRAME_SWITCH_ERROR' )

      CALL PCPOOL ( 'FRAME_1500000_NAME',         1, 'SWITCH_ERROR' )
      CALL PIPOOL ( 'FRAME_1500000_CLASS',        1, 6      )
      CALL PIPOOL ( 'FRAME_1500000_CLASS_ID',     1, FRAMID )
      CALL PIPOOL ( 'FRAME_1500000_CENTER',       1, 0      )
      CALL PIPOOL ( 'FRAME_1500000_ALIGNED_WITH', 1, 1      )

      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG,
     .                FREE,   BASCNT, USETIM, BINARY, CLSSES,
     .                CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )


      CALL CHCKXC ( .TRUE., 'SPICE(MISSINGFRAMEVAR)', OK )

      CALL CHCKSI ( 'FRAMAT', FRAMAT, '=', 0, 0, OK )

      CALL CHCKAI ( 'FRPOOL', FRPOOL, '=', NWPOOL, MAXFRM+6, OK )
      CALL CHCKAI ( 'HDFRAM', HDFRAM, '=', NWHDFR, MAXFRM,   OK )
      CALL CHCKAI ( 'BASBEG', BASBEG, '=', NWHDBS, MAXFRM,   OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Switch frame ID triggers GIPOOL error' )

      FRAMID = 1500000

      CALL PDPOOL ( 'FRAME_SWITCH_ERROR',         1, 1.D20 )

      CALL PCPOOL ( 'FRAME_1500000_NAME',         1, 'SWITCH_ERROR' )
      CALL PIPOOL ( 'FRAME_1500000_CLASS',        1, 6      )
      CALL PIPOOL ( 'FRAME_1500000_CLASS_ID',     1, FRAMID )
      CALL PIPOOL ( 'FRAME_1500000_CENTER',       1, 0      )
      CALL PIPOOL ( 'FRAME_1500000_ALIGNED_WITH', 1, 1      )

      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG,
     .                FREE,   BASCNT, USETIM, BINARY, CLSSES,
     .                CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )


      CALL CHCKXC ( .TRUE., 'SPICE(INTOUTOFRANGE)', OK )

      CALL CHCKSI ( 'FRAMAT', FRAMAT, '=', 0, 0, OK )

      CALL CHCKAI ( 'FRPOOL', FRPOOL, '=', NWPOOL, MAXFRM+6, OK )
      CALL CHCKAI ( 'HDFRAM', HDFRAM, '=', NWHDFR, MAXFRM,   OK )
      CALL CHCKAI ( 'BASBEG', BASBEG, '=', NWHDBS, MAXFRM,   OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Frame class ID triggers GIPOOL error' )

      FRAMID = 1500000
C
C     Don't set the frame class ID in the kernel pool.
C
      CALL DVPOOL ( 'FRAME_1500000_CLASS_ID' )

      CALL PIPOOL ( 'FRAME_SWITCH_ERROR',         1, FRAMID )
      CALL PCPOOL ( 'FRAME_1500000_NAME',         1, 'SWITCH_ERROR' )
      CALL PIPOOL ( 'FRAME_1500000_CLASS',        1, 6      )
      CALL PIPOOL ( 'FRAME_1500000_CENTER',       1, 0      )
      CALL PIPOOL ( 'FRAME_1500000_ALIGNED_WITH', 1, 1      )

      CALL PDPOOL ( 'FRAME_1500000_CLASS_ID',     1, 1.D20  )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG,
     .                FREE,   BASCNT, USETIM, BINARY, CLSSES,
     .                CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )

      CALL CHCKXC ( .TRUE., 'SPICE(INTOUTOFRANGE)', OK )

      CALL CHCKSI ( 'FRAMAT', FRAMAT, '=', 0, 0, OK )

      CALL CHCKAI ( 'FRPOOL', FRPOOL, '=', NWPOOL, MAXFRM+6, OK )
      CALL CHCKAI ( 'HDFRAM', HDFRAM, '=', NWHDFR, MAXFRM,   OK )
      CALL CHCKAI ( 'BASBEG', BASBEG, '=', NWHDBS, MAXFRM,   OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Mismatched switch frame ID: ID associated with '
     .//           'name doesn''t match ID in associated frame '
     .//           'specification kernel variables.' )

      FRAMID = 1500000

      CALL PIPOOL ( 'FRAME_SWITCH_ERROR', 1, 1499999 )

      CALL PCPOOL ( 'FRAME_1500000_NAME',         1, 'SWITCH_ERROR' )
      CALL PIPOOL ( 'FRAME_1500000_CLASS',        1, 6      )
      CALL PIPOOL ( 'FRAME_1500000_CLASS_ID',     1, FRAMID )
      CALL PIPOOL ( 'FRAME_1500000_CENTER',       1, 0      )
      CALL PIPOOL ( 'FRAME_1500000_ALIGNED_WITH', 1, 1      )

      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG,
     .                FREE,   BASCNT, USETIM, BINARY, CLSSES,
     .                CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )


      CALL CHCKXC ( .TRUE., 'SPICE(BADFRAMESPEC)', OK )

      CALL CHCKSI ( 'FRAMAT', FRAMAT, '=', 0, 0, OK )

      CALL CHCKAI ( 'FRPOOL', FRPOOL, '=', NWPOOL, MAXFRM+6, OK )
      CALL CHCKAI ( 'HDFRAM', HDFRAM, '=', NWHDFR, MAXFRM,   OK )
      CALL CHCKAI ( 'BASBEG', BASBEG, '=', NWHDBS, MAXFRM,   OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Missing frame class' )

      FRAMID = 1500000

C
C     Don't set the frame class in the kernel pool.
C
      CALL DVPOOL ( 'FRAME_1500000_CLASS' )

      CALL PIPOOL ( 'FRAME_SWITCH_ERROR',         1, FRAMID )
      CALL PCPOOL ( 'FRAME_1500000_NAME',         1, 'SWITCH_ERROR' )
      CALL PIPOOL ( 'FRAME_1500000_CLASS_ID',     1, FRAMID )
      CALL PIPOOL ( 'FRAME_1500000_CENTER',       1, 0      )
      CALL PIPOOL ( 'FRAME_1500000_ALIGNED_WITH', 1, 1      )

      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG,
     .                FREE,   BASCNT, USETIM, BINARY, CLSSES,
     .                CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )

      CALL CHCKXC ( .TRUE., 'SPICE(MISSINGFRAMEVAR)', OK )

      CALL CHCKSI ( 'FRAMAT', FRAMAT, '=', 0, 0, OK )

      CALL CHCKAI ( 'FRPOOL', FRPOOL, '=', NWPOOL, MAXFRM+6, OK )
      CALL CHCKAI ( 'HDFRAM', HDFRAM, '=', NWHDFR, MAXFRM,   OK )
      CALL CHCKAI ( 'BASBEG', BASBEG, '=', NWHDBS, MAXFRM,   OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Missing frame class ID' )

      FRAMID = 1500000
C
C     Don't set the frame class ID in the kernel pool.
C
      CALL DVPOOL ( 'FRAME_1500000_CLASS_ID' )

      CALL PIPOOL ( 'FRAME_SWITCH_ERROR',         1, FRAMID )
      CALL PCPOOL ( 'FRAME_1500000_NAME',         1, 'SWITCH_ERROR' )
      CALL PIPOOL ( 'FRAME_1500000_CLASS',        1, 6      )
      CALL PIPOOL ( 'FRAME_1500000_CENTER',       1, 0      )
      CALL PIPOOL ( 'FRAME_1500000_ALIGNED_WITH', 1, 1      )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG,
     .                FREE,   BASCNT, USETIM, BINARY, CLSSES,
     .                CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )

      CALL CHCKXC ( .TRUE., 'SPICE(MISSINGFRAMEVAR)', OK )

      CALL CHCKSI ( 'FRAMAT', FRAMAT, '=', 0, 0, OK )

      CALL CHCKAI ( 'FRPOOL', FRPOOL, '=', NWPOOL, MAXFRM+6, OK )
      CALL CHCKAI ( 'HDFRAM', HDFRAM, '=', NWHDFR, MAXFRM,   OK )
      CALL CHCKAI ( 'BASBEG', BASBEG, '=', NWHDBS, MAXFRM,   OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Frame class ID triggers GIPOOL error' )

      FRAMID = 1500000
C
C     Don't set the frame class ID in the kernel pool.
C
      CALL DVPOOL ( 'FRAME_1500000_CLASS_ID' )

      CALL PIPOOL ( 'FRAME_SWITCH_ERROR',         1, FRAMID )
      CALL PCPOOL ( 'FRAME_1500000_NAME',         1, 'SWITCH_ERROR' )
      CALL PIPOOL ( 'FRAME_1500000_CLASS',        1, 6      )
      CALL PIPOOL ( 'FRAME_1500000_CENTER',       1, 0      )
      CALL PIPOOL ( 'FRAME_1500000_ALIGNED_WITH', 1, 1      )

      CALL PDPOOL ( 'FRAME_1500000_CLASS_ID',     1, 1.D20  )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG,
     .                FREE,   BASCNT, USETIM, BINARY, CLSSES,
     .                CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )

      CALL CHCKXC ( .TRUE., 'SPICE(INTOUTOFRANGE)', OK )

      CALL CHCKSI ( 'FRAMAT', FRAMAT, '=', 0, 0, OK )

      CALL CHCKAI ( 'FRPOOL', FRPOOL, '=', NWPOOL, MAXFRM+6, OK )
      CALL CHCKAI ( 'HDFRAM', HDFRAM, '=', NWHDFR, MAXFRM,   OK )
      CALL CHCKAI ( 'BASBEG', BASBEG, '=', NWHDBS, MAXFRM,   OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Missing frame center' )

      FRAMID = 1500000

C
C     Prepare initialized data structures to compare against.
C
      CALL ZZHSIINI ( MAXFRM, NWHDFR, NWPOOL )
      CALL CLEARI   ( MAXFRM, NWHDBS )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Don't set the frame center in the kernel pool.
C
      CALL DVPOOL ( 'FRAME_1500000_CENTER' )

      CALL PIPOOL ( 'FRAME_SWITCH_ERROR',         1, FRAMID )
      CALL PCPOOL ( 'FRAME_1500000_NAME',         1, 'SWITCH_ERROR' )
      CALL PIPOOL ( 'FRAME_1500000_CLASS',        1, 6      )
      CALL PIPOOL ( 'FRAME_1500000_CLASS_ID',     1, FRAMID )
      CALL PIPOOL ( 'FRAME_1500000_ALIGNED_WITH', 1, 1      )

      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG,
     .                FREE,   BASCNT, USETIM, BINARY, CLSSES,
     .                CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )

      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

      CALL CHCKSI ( 'FRAMAT', FRAMAT, '=', 0, 0, OK )

      CALL CHCKAI ( 'FRPOOL', FRPOOL, '=', NWPOOL, MAXFRM+6, OK )
      CALL CHCKAI ( 'HDFRAM', HDFRAM, '=', NWHDFR, MAXFRM,   OK )
      CALL CHCKAI ( 'BASBEG', BASBEG, '=', NWHDBS, MAXFRM,   OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Missing base frame list' )

      FRAMID = 1500000
C
C     Don't set the base frame list in the kernel pool.
C
      CALL DVPOOL ( 'FRAME_1500000_ALIGNED_WITH' )

      CALL PIPOOL ( 'FRAME_SWITCH_ERROR',         1, FRAMID )
      CALL PCPOOL ( 'FRAME_1500000_NAME',         1, 'SWITCH_ERROR' )
      CALL PIPOOL ( 'FRAME_1500000_CLASS',        1, 6      )
      CALL PIPOOL ( 'FRAME_1500000_CLASS_ID',     1, FRAMID )
      CALL PIPOOL ( 'FRAME_1500000_CENTER',       1, 0      )

      CALL DVPOOL ( 'FRAME_1500000_ALIGNED_WITH' )

      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG,
     .                FREE,   BASCNT, USETIM, BINARY, CLSSES,
     .                CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )

      CALL CHCKXC ( .TRUE., 'SPICE(MISSINGFRAMEVAR)', OK )

      CALL CHCKSI ( 'FRAMAT', FRAMAT, '=', 0, 0, OK )

      CALL CHCKAI ( 'FRPOOL', FRPOOL, '=', NWPOOL, MAXFRM+6, OK )
      CALL CHCKAI ( 'HDFRAM', HDFRAM, '=', NWHDFR, MAXFRM,   OK )
      CALL CHCKAI ( 'BASBEG', BASBEG, '=', NWHDBS, MAXFRM,   OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Unrecognized base frame name' )

      FRAMID = 1500000

      CALL PIPOOL ( 'FRAME_SWITCH_ERROR',         1, FRAMID )
      CALL PCPOOL ( 'FRAME_1500000_NAME',         1, 'SWITCH_ERROR' )
      CALL PIPOOL ( 'FRAME_1500000_CLASS',        1, 6      )
      CALL PIPOOL ( 'FRAME_1500000_CLASS_ID',     1, FRAMID )
      CALL PIPOOL ( 'FRAME_1500000_CENTER',       1, 0      )

      CALL DVPOOL ( 'FRAME_1500000_ALIGNED_WITH' )
      CALL PCPOOL ( 'FRAME_1500000_ALIGNED_WITH', 1, 'XYZ'  )

      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG,
     .                FREE,   BASCNT, USETIM, BINARY, CLSSES,
     .                CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )

      CALL CHCKXC ( .TRUE., 'SPICE(FRAMENAMENOTFOUND)', OK )

      CALL CHCKSI ( 'FRAMAT', FRAMAT, '=', 0, 0, OK )

      CALL CHCKAI ( 'FRPOOL', FRPOOL, '=', NWPOOL, MAXFRM+6, OK )
      CALL CHCKAI ( 'HDFRAM', HDFRAM, '=', NWHDFR, MAXFRM,   OK )
      CALL CHCKAI ( 'BASBEG', BASBEG, '=', NWHDBS, MAXFRM,   OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Base frame triggers FRINFO error' )

      FRAMID = 1500000

      CALL PIPOOL ( 'FRAME_SWITCH_ERROR',         1, FRAMID )
      CALL PCPOOL ( 'FRAME_1500000_NAME',         1, 'SWITCH_ERROR' )
      CALL PIPOOL ( 'FRAME_1500000_CLASS',        1, 6      )
      CALL PIPOOL ( 'FRAME_1500000_CLASS_ID',     1, FRAMID )
      CALL PIPOOL ( 'FRAME_1500000_CENTER',       1, 0      )

      CALL PIPOOL ( 'FRAME_1500000_ALIGNED_WITH', 1, -777  )

      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG,
     .                FREE,   BASCNT, USETIM, BINARY, CLSSES,
     .                CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )

      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEINFONOTFOUND)', OK )

      CALL CHCKSI ( 'FRAMAT', FRAMAT, '=', 0, 0, OK )

      CALL CHCKAI ( 'FRPOOL', FRPOOL, '=', NWPOOL, MAXFRM+6, OK )
      CALL CHCKAI ( 'HDFRAM', HDFRAM, '=', NWHDFR, MAXFRM,   OK )
      CALL CHCKAI ( 'BASBEG', BASBEG, '=', NWHDBS, MAXFRM,   OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Base frame ID triggers GIPOOL error' )

      FRAMID = 1500000

      CALL PIPOOL ( 'FRAME_SWITCH_ERROR',         1, FRAMID )
      CALL PCPOOL ( 'FRAME_1500000_NAME',         1, 'SWITCH_ERROR' )
      CALL PIPOOL ( 'FRAME_1500000_CLASS',        1, 6      )
      CALL PIPOOL ( 'FRAME_1500000_CLASS_ID',     1, FRAMID )
      CALL PIPOOL ( 'FRAME_1500000_CENTER',       1, 0      )

      CALL PDPOOL ( 'FRAME_1500000_ALIGNED_WITH', 1, 1.D20  )

      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG,
     .                FREE,   BASCNT, USETIM, BINARY, CLSSES,
     .                CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )

      CALL CHCKXC ( .TRUE., 'SPICE(INTOUTOFRANGE)', OK )

      CALL CHCKSI ( 'FRAMAT', FRAMAT, '=', 0, 0, OK )

      CALL CHCKAI ( 'FRPOOL', FRPOOL, '=', NWPOOL, MAXFRM+6, OK )
      CALL CHCKAI ( 'HDFRAM', HDFRAM, '=', NWHDFR, MAXFRM,   OK )
      CALL CHCKAI ( 'BASBEG', BASBEG, '=', NWHDBS, MAXFRM,   OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Start time provided without stop time' )

      FRAMID = 1500000

      CALL PIPOOL ( 'FRAME_SWITCH_ERROR',         1, FRAMID )
      CALL PCPOOL ( 'FRAME_1500000_NAME',         1, 'SWITCH_ERROR' )
      CALL PIPOOL ( 'FRAME_1500000_CLASS',        1, 6      )
      CALL PIPOOL ( 'FRAME_1500000_CLASS_ID',     1, FRAMID )
      CALL PIPOOL ( 'FRAME_1500000_CENTER',       1, 0      )

      CALL DVPOOL ( 'FRAME_1500000_ALIGNED_WITH' )
      CALL PIPOOL ( 'FRAME_1500000_ALIGNED_WITH', 1, 1      )

      CALL PDPOOL ( 'FRAME_1500000_START',        1, 0.0D0  )
      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG,
     .                FREE,   BASCNT, USETIM, BINARY, CLSSES,
     .                CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )


      CALL CHCKXC ( .TRUE., 'SPICE(PARTIALFRAMESPEC)', OK )

      CALL CHCKSI ( 'FRAMAT', FRAMAT, '=', 0, 0, OK )

      CALL CHCKAI ( 'FRPOOL', FRPOOL, '=', NWPOOL, MAXFRM+6, OK )
      CALL CHCKAI ( 'HDFRAM', HDFRAM, '=', NWHDFR, MAXFRM,   OK )
      CALL CHCKAI ( 'BASBEG', BASBEG, '=', NWHDBS, MAXFRM,   OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Stop time provided without start time' )

      FRAMID = 1500000

      CALL PIPOOL ( 'FRAME_SWITCH_ERROR',         1, FRAMID )
      CALL PCPOOL ( 'FRAME_1500000_NAME',         1, 'SWITCH_ERROR' )
      CALL PIPOOL ( 'FRAME_1500000_CLASS',        1, 6      )
      CALL PIPOOL ( 'FRAME_1500000_CLASS_ID',     1, FRAMID )
      CALL PIPOOL ( 'FRAME_1500000_CENTER',       1, 0      )
      CALL PIPOOL ( 'FRAME_1500000_ALIGNED_WITH', 1, 1      )

      CALL DVPOOL ( 'FRAME_1500000_START' )

      CALL PDPOOL ( 'FRAME_1500000_STOP',         1, 0.0D0  )

      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG,
     .                FREE,   BASCNT, USETIM, BINARY, CLSSES,
     .                CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )

      CALL CHCKXC ( .TRUE., 'SPICE(PARTIALFRAMESPEC)', OK )

      CALL CHCKSI ( 'FRAMAT', FRAMAT, '=', 0, 0, OK )

      CALL CHCKAI ( 'FRPOOL', FRPOOL, '=', NWPOOL, MAXFRM+6, OK )
      CALL CHCKAI ( 'HDFRAM', HDFRAM, '=', NWHDFR, MAXFRM,   OK )
      CALL CHCKAI ( 'BASBEG', BASBEG, '=', NWHDBS, MAXFRM,   OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Start time count doesn''t match base frame '
     .//           'count.' )

      FRAMID = 1500000

      CALL PIPOOL ( 'FRAME_SWITCH_ERROR',         1, FRAMID )
      CALL PCPOOL ( 'FRAME_1500000_NAME',         1, 'SWITCH_ERROR' )
      CALL PIPOOL ( 'FRAME_1500000_CLASS',        1, 6      )
      CALL PIPOOL ( 'FRAME_1500000_CLASS_ID',     1, FRAMID )
      CALL PIPOOL ( 'FRAME_1500000_CENTER',       1, 0      )
      CALL PIPOOL ( 'FRAME_1500000_ALIGNED_WITH', 1, 1      )

      STARTS(1) = 0.0D0
      STARTS(2) = 1.0D0

      STOPS(1) = 1.0D0
      STOPS(2) = 2.0D0

      CALL PDPOOL ( 'FRAME_1500000_START',        2, STARTS )
      CALL PDPOOL ( 'FRAME_1500000_STOP',         1, STOPS )
      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG,
     .                FREE,   BASCNT, USETIM, BINARY, CLSSES,
     .                CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )


      CALL CHCKXC ( .TRUE., 'SPICE(COUNTMISMATCH)', OK )

      CALL CHCKSI ( 'FRAMAT', FRAMAT, '=', 0, 0, OK )

      CALL CHCKAI ( 'FRPOOL', FRPOOL, '=', NWPOOL, MAXFRM+6, OK )
      CALL CHCKAI ( 'HDFRAM', HDFRAM, '=', NWHDFR, MAXFRM,   OK )
      CALL CHCKAI ( 'BASBEG', BASBEG, '=', NWHDBS, MAXFRM,   OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Stop time count doesn''t match base frame '
     .//           'count.' )

      FRAMID = 1500000

      CALL PIPOOL ( 'FRAME_SWITCH_ERROR',         1, FRAMID )
      CALL PCPOOL ( 'FRAME_1500000_NAME',         1, 'SWITCH_ERROR' )
      CALL PIPOOL ( 'FRAME_1500000_CLASS',        1, 6      )
      CALL PIPOOL ( 'FRAME_1500000_CLASS_ID',     1, FRAMID )
      CALL PIPOOL ( 'FRAME_1500000_CENTER',       1, 0      )
      CALL PIPOOL ( 'FRAME_1500000_ALIGNED_WITH', 1, 1      )

      STARTS(1) = 0.0D0
      STARTS(2) = 1.0D0

      STOPS(1) = 1.0D0
      STOPS(2) = 2.0D0

      CALL PDPOOL ( 'FRAME_1500000_START',        1, STARTS )
      CALL PDPOOL ( 'FRAME_1500000_STOP',         2, STOPS )
      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG,
     .                FREE,   BASCNT, USETIM, BINARY, CLSSES,
     .                CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )

      CALL CHCKXC ( .TRUE., 'SPICE(COUNTMISMATCH)', OK )

      CALL CHCKSI ( 'FRAMAT', FRAMAT, '=', 0, 0, OK )

      CALL CHCKAI ( 'FRPOOL', FRPOOL, '=', NWPOOL, MAXFRM+6, OK )
      CALL CHCKAI ( 'HDFRAM', HDFRAM, '=', NWHDFR, MAXFRM,   OK )
      CALL CHCKAI ( 'BASBEG', BASBEG, '=', NWHDBS, MAXFRM,   OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Start and stop time counts are equal but '
     .//           'don''t match base frame count.' )

      FRAMID = 1500000

      CALL PIPOOL ( 'FRAME_SWITCH_ERROR',         1, FRAMID )
      CALL PCPOOL ( 'FRAME_1500000_NAME',         1, 'SWITCH_ERROR' )
      CALL PIPOOL ( 'FRAME_1500000_CLASS',        1, 6      )
      CALL PIPOOL ( 'FRAME_1500000_CLASS_ID',     1, FRAMID )
      CALL PIPOOL ( 'FRAME_1500000_CENTER',       1, 0      )
      CALL PIPOOL ( 'FRAME_1500000_ALIGNED_WITH', 1, 1      )

      STARTS(1) = 0.0D0
      STARTS(2) = 1.0D0

      STOPS(1) = 1.0D0
      STOPS(2) = 2.0D0

      CALL PDPOOL ( 'FRAME_1500000_START',        2, STARTS )
      CALL PDPOOL ( 'FRAME_1500000_STOP',         2, STOPS  )

      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG,
     .                FREE,   BASCNT, USETIM, BINARY, CLSSES,
     .                CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )


      CALL CHCKXC ( .TRUE., 'SPICE(COUNTMISMATCH)', OK )

      CALL CHCKSI ( 'FRAMAT', FRAMAT, '=', 0, 0, OK )

      CALL CHCKAI ( 'FRPOOL', FRPOOL, '=', NWPOOL, MAXFRM+6, OK )
      CALL CHCKAI ( 'HDFRAM', HDFRAM, '=', NWHDFR, MAXFRM,   OK )
      CALL CHCKAI ( 'BASBEG', BASBEG, '=', NWHDBS, MAXFRM,   OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Bad string start time.' )

      FRAMID = 1500000

      CALL PIPOOL ( 'FRAME_SWITCH_ERROR',         1, FRAMID )
      CALL PCPOOL ( 'FRAME_1500000_NAME',         1, 'SWITCH_ERROR' )
      CALL PIPOOL ( 'FRAME_1500000_CLASS',        1, 6      )
      CALL PIPOOL ( 'FRAME_1500000_CLASS_ID',     1, FRAMID )
      CALL PIPOOL ( 'FRAME_1500000_CENTER',       1, 0      )
      CALL PIPOOL ( 'FRAME_1500000_ALIGNED_WITH', 1, 1      )


      CALL DVPOOL ( 'FRAME_1500000_START' )
      CALL DVPOOL ( 'FRAME_1500000_STOP'  )

      CALL PCPOOL ( 'FRAME_1500000_START', 1, 'XYZ'        )
      CALL PCPOOL ( 'FRAME_1500000_STOP',  1, '2000 JAN 1' )

      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG,
     .                FREE,   BASCNT, USETIM, BINARY, CLSSES,
     .                CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )

      CALL CHCKXC ( .TRUE., 'SPICE(UNPARSEDTIME)', OK )

      CALL CHCKSI ( 'FRAMAT', FRAMAT, '=', 0, 0, OK )

      CALL CHCKAI ( 'FRPOOL', FRPOOL, '=', NWPOOL, MAXFRM+6, OK )
      CALL CHCKAI ( 'HDFRAM', HDFRAM, '=', NWHDFR, MAXFRM,   OK )
      CALL CHCKAI ( 'BASBEG', BASBEG, '=', NWHDBS, MAXFRM,   OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Bad string stop time.' )

      FRAMID = 1500000

      CALL PIPOOL ( 'FRAME_SWITCH_ERROR',         1, FRAMID )
      CALL PCPOOL ( 'FRAME_1500000_NAME',         1, 'SWITCH_ERROR' )
      CALL PIPOOL ( 'FRAME_1500000_CLASS',        1, 6      )
      CALL PIPOOL ( 'FRAME_1500000_CLASS_ID',     1, FRAMID )
      CALL PIPOOL ( 'FRAME_1500000_CENTER',       1, 0      )
      CALL PIPOOL ( 'FRAME_1500000_ALIGNED_WITH', 1, 1      )


      CALL DVPOOL ( 'FRAME_1500000_START' )
      CALL DVPOOL ( 'FRAME_1500000_STOP'  )

      CALL PCPOOL ( 'FRAME_1500000_START', 1, '2000 JAN 1' )
      CALL PCPOOL ( 'FRAME_1500000_STOP',  1, 'XYZ'        )

      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG,
     .                FREE,   BASCNT, USETIM, BINARY, CLSSES,
     .                CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )


      CALL CHCKXC ( .TRUE., 'SPICE(UNPARSEDTIME)', OK )

      CALL CHCKSI ( 'FRAMAT', FRAMAT, '=', 0, 0, OK )

      CALL CHCKAI ( 'FRPOOL', FRPOOL, '=', NWPOOL, MAXFRM+6, OK )
      CALL CHCKAI ( 'HDFRAM', HDFRAM, '=', NWHDFR, MAXFRM,   OK )
      CALL CHCKAI ( 'BASBEG', BASBEG, '=', NWHDBS, MAXFRM,   OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Create and use a switch frame having half the '
     .//           'maximum number of base frame intervals. Post '
     .//           'error test case version.' )

C
C     Don't clear the kernel pool: force the switch frame subsystem
C     to make room.
C
      NBASES = MAXBAS/2

      CALL PIPOOL ( 'FRAME_SWITCH_MAXBASE',   1, 1400010          )
      CALL PCPOOL ( 'FRAME_1400010_NAME',     1, 'SWITCH_MAXBASE' )
      CALL PIPOOL ( 'FRAME_1400010_CLASS',    1, 6                )
      CALL PIPOOL ( 'FRAME_1400010_CLASS_ID', 1, 1400010          )
      CALL PIPOOL ( 'FRAME_1400010_CENTER',   1, 0                )
      CALL CHCKXC ( .FALSE., ' ', OK )
C
C     17 is the ID code of ECLIPJ2000.
C
      CALL FILLI  ( 17, NBASES, IBASBF )
      CALL PIPOOL ( 'FRAME_1400010_ALIGNED_WITH', NBASES, IBASBF )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FILLI  ( INERTL, NBASES, XCLASS )
      CALL FILLI  ( 17,     NBASES, XCLSID )

C
C     Create a set of disjoint intervals.
C
      DO I = 1, NBASES
         XSTART(I) = 2.D0 * DBLE(I-1)
         XSTOP(I) = XSTART(I) + 0.9D0
      END DO

      CALL PDPOOL ( 'FRAME_1400010_START', NBASES, XSTART )
      CALL PDPOOL ( 'FRAME_1400010_STOP',  NBASES, XSTOP )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Do a frame transformation to populate the switch frame database.
C
      ET = XSTART( NBASES/2 ) + 0.5D0
      CALL SXFORM ( 'SWITCH_MAXBASE', 'J2000', ET, XFORM )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Fetch data for the frame.
C
      FRAMID = 1400010

      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG,
     .                FREE,   BASCNT, USETIM, BINARY, CLSSES,
     .                CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     This test depends on the previous test cases.
C
      CALL CHCKSI ( 'FREE', FREE, '=', NBASES+1, 0, OK )

      CALL CHCKSI ( 'FRAMAT', FRAMAT, '!=', 0, 0, OK )

      IF ( FRAMAT .NE. 0 ) THEN
C
C       This test depends on the previous test cases.
C
         CALL CHCKSI ( 'BASBEG(FRAMAT)',
     .                  BASBEG(FRAMAT), '=', 1, 0, OK )

         HAVTIM = USETIM(FRAMAT)

         CALL CHCKSL ( 'HAVTIM', HAVTIM, .TRUE., OK )

         CALL CHCKSL ( 'BINARY', BINARY(FRAMAT), .TRUE., OK )

         CALL CHCKSI ( 'NBASES', BASCNT(FRAMAT), '=', NBASES, 0, OK )

         DO I = 1, NBASES

            J = BASBEG(FRAMAT) - 1 + I

            CALL CHCKSI( 'BASEID', BASLST(J), '=', IBASBF(I), 0,    OK)
            CALL CHCKSD( 'STARTS', STARTS(J), '=', XSTART(I), 0.D0, OK)
            CALL CHCKSD( 'STOPS',  STOPS(J),  '=', XSTOP(I),  0.D0, OK)
            CALL CHCKSI( 'CLSSES', CLSSES(J), '=', XCLASS(I), 0,    OK)
            CALL CHCKSI( 'CLSIDS', CLSIDS(J), '=', XCLSID(I), 0,    OK)

         END DO

      END IF

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Create and use a switch frame having the maximum '
     .//           'number of base frame intervals. Post error test '
     .//           'case version.' )

C
C     Don't clear the kernel pool: force the switch frame subsystem
C     to make room.
C
      NBASES = MAXBAS

      CALL PIPOOL ( 'FRAME_SWITCH_MAXBASE',   1, 1400011          )
      CALL PCPOOL ( 'FRAME_1400011_NAME',     1, 'SWITCH_MAXBASE' )
      CALL PIPOOL ( 'FRAME_1400011_CLASS',    1, 6                )
      CALL PIPOOL ( 'FRAME_1400011_CLASS_ID', 1, 1400011          )
      CALL PIPOOL ( 'FRAME_1400011_CENTER',   1, 0                )
      CALL CHCKXC ( .FALSE., ' ', OK )
C
C     17 is the ID code of ECLIPJ2000.
C
      CALL FILLI  ( 17, NBASES, IBASBF )
      CALL PIPOOL ( 'FRAME_1400011_ALIGNED_WITH', NBASES, IBASBF )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FILLI  ( INERTL, NBASES, XCLASS )
      CALL FILLI  ( 17,     NBASES, XCLSID )

C
C     Create a set of disjoint intervals.
C
      DO I = 1, NBASES
         XSTART(I) = 2.D0 * DBLE(I-1)
         XSTOP(I) = XSTART(I) + 0.9D0
      END DO

      CALL PDPOOL ( 'FRAME_1400011_START', NBASES, XSTART )
      CALL PDPOOL ( 'FRAME_1400011_STOP',  NBASES, XSTOP )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Do a frame transformation to populate the switch frame database.
C
      ET = XSTART( NBASES/2 ) + 0.5D0
      CALL SXFORM ( 'SWITCH_MAXBASE', 'J2000', ET, XFORM )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Fetch data for the frame.
C
      FRAMID = 1400011

      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG,
     .                FREE,   BASCNT, USETIM, BINARY, CLSSES,
     .                CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )

      CALL CHCKXC ( .FALSE., ' ', OK )
C
C     This test depends on the previous test cases.
C
      CALL CHCKSI ( 'FREE', FREE, '=', MAXBAS+1, 0, OK )


      CALL CHCKSI ( 'FRAMAT', FRAMAT, '!=', 0, 0, OK )

      IF ( FRAMAT .NE. 0 ) THEN
C
C       This test depends on the previous test cases.
C
         CALL CHCKSI ( 'BASBEG(FRAMAT)',
     .                  BASBEG(FRAMAT), '=', 1, 0, OK )

         HAVTIM = USETIM(FRAMAT)

         CALL CHCKSL ( 'HAVTIM', HAVTIM, .TRUE., OK )

         CALL CHCKSL ( 'BINARY', BINARY(FRAMAT), .TRUE., OK )

         CALL CHCKSI ( 'NBASES', BASCNT(FRAMAT), '=', NBASES, 0, OK )

         DO I = 1, NBASES

            J = BASBEG(FRAMAT) - 1 + I

            CALL CHCKSI( 'BASEID', BASLST(J), '=', IBASBF(I), 0,    OK)
            CALL CHCKSD( 'STARTS', STARTS(J), '=', XSTART(I), 0.D0, OK)
            CALL CHCKSD( 'STOPS',  STOPS(J),  '=', XSTOP(I),  0.D0, OK)
            CALL CHCKSI( 'CLSSES', CLSSES(J), '=', XCLASS(I), 0,    OK)
            CALL CHCKSI( 'CLSIDS', CLSIDS(J), '=', XCLSID(I), 0,    OK)

         END DO

      END IF


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error case: return on entry' )

      SHRTMS = 'SPICE(TESTERROR)'
      CALL SIGERR ( SHRTMS )

      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG,
     .                FREE,   BASCNT, USETIM, BINARY, CLSSES,
     .                CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )

      CALL CHCKXC ( .TRUE., SHRTMS, OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Clean up.' )

      CALL KILFIL ( FK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN
      END
