!$Procedure F_ZZSWFFET ( ZZSWFFET tests )

      SUBROUTINE F_ZZSWFFET ( OK )

! $ Abstract
!
!     Exercise the SPICELIB routine ZZSWFFET.
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
      INCLUDE 'frmtyp.inc'
      INCLUDE 'zzswtchf.inc'

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
!     This routine tests the SPICELIB routine ZZSWFFET.
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
!-    TSPICE Version 1.0.0, 04-DEC-2021 (NJB)
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

!
!     Local Parameters
!
      CHARACTER*(*)         FK0
      PARAMETER           ( FK0    = 'zzswffet_test0.tf' )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               TXTSIZ
      PARAMETER           ( TXTSIZ = 300 )

!
!     Local Variables
!
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

!
!     Saved values
!
      SAVE

!
!     Initial values
!

!
!     Open the test family.
!
      CALL TOPEN ( 'F_ZZSWFFET' )


!***********************************************************************
!
!     Normal cases
!
!***********************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Create and load FK0.' )

!
!     Create a text kernel containing the following frame
!     specifications:
!
!
!     BEGDAT()
!     FRAME_SWITCH0               =  1400000
!     FRAME_1400000_NAME          =  'SWITCH0'
!     FRAME_1400000_CLASS         =  6
!     FRAME_1400000_CLASS_ID      =  1400000
!     FRAME_1400000_CENTER        =  0
!     FRAME_1400000_ALIGNED_WITH  =  ( 'J2000',
!                                      'ECLIPJ2000',
!                                      'B1950',
!                                      'GALACTIC' )
!
!     FRAME_1400000_START         = ( '2000 JAN 1 00:00:00 TDB',
!                                     '2000 FEB 1 00:00:00 TDB',
!                                     '2000 MAR 1 00:00:00 TDB',
!                                     '2000 APR 1 00:00:00 TDB' )
!
!     FRAME_1400000_STOP          = ( '2000 FEB 1 00:00:00 TDB',
!                                     '2000 MAR 1 00:00:00 TDB',
!                                     '2000 APR 1 00:00:00 TDB',
!                                     '2000 MAY 1 00:00:00 TDB'  )
!
!     FRAME_SWITCH1               =  1400001
!     FRAME_1400001_NAME          =  'SWITCH1'
!     FRAME_1400001_CLASS         =  6
!     FRAME_1400001_CLASS_ID      =  1400001
!     FRAME_SWITCH1_CENTER        =  0
!     FRAME_1400001_ALIGNED_WITH  =  ( 'J2000',
!                                      'ECLIPJ2000',
!                                      'B1950',
!                                      'GALACTIC' )
!
!     FRAME_1400001_START         = ( @2000-JAN-1/00:00:00,
!                                     @2000-FEB-1/00:00:00,
!                                     @2000-MAR-1/00:00:00,
!                                     @2000-APR-1/00:00:00  )
!
!     FRAME_1400001_STOP          = ( @2000-FEB-1/00:00:00,
!                                     @2000-MAR-1/00:00:00,
!                                     @2000-APR-1/00:00:00,
!                                     @2000-MAY-1/00:00:00  )
!
!     FRAME_SWITCH2               =  1400002
!     FRAME_1400002_NAME          =  'SWITCH2'
!     FRAME_1400002_CLASS         =  6
!     FRAME_1400002_CLASS_ID      =  1400002
!     FRAME_1400002_CENTER        =  'SOLAR SYSTEM BARYCENTER'
!     FRAME_1400002_ALIGNED_WITH  =  ( 1,
!                                      17,
!                                      2,
!                                      13 )
!
!     FRAME_1400002_START         = ( '2000 JAN 1 00:00:00 TDB',
!                                     '2000 FEB 1 00:00:00 TDB',
!                                     '2000 MAR 1 00:00:00 TDB',
!                                     '2000 APR 1 00:00:00 TDB' )
!
!     FRAME_1400002_STOP          = ( @2000-FEB-1/00:00:00,
!                                     @2000-MAR-1/00:00:00,
!                                     @2000-APR-2/00:00:00,
!                                     @2000-MAY-1/00:00:00  )
!
!     FRAME_SWITCH3               =  1400003
!     FRAME_1400003_NAME          =  'SWITCH3'
!     FRAME_1400003_CLASS         =  6
!     FRAME_1400003_CLASS_ID      =  1400003
!     FRAME_1400003_CENTER        =  'SOLAR SYSTEM BARYCENTER'
!     FRAME_1400003_ALIGNED_WITH  =  ( 1,
!                                      17,
!                                      2,
!                                      13 )
!     BEGTXT()
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
      TXTBUF(  7 ) = '      FRAME_1400000_ALIGNED_WITH  =  ( ' &
      //             '''J2000'','
      TXTBUF(  8 ) = '                                       ' &
      //             '''ECLIPJ2000'','
      TXTBUF(  9 ) = '                                       ' &
      //             '''B1950'','
      TXTBUF( 10 ) = '                                       ' &
      //             '''GALACTIC'' )'
      TXTBUF( 11 ) = ' '
      TXTBUF( 12 ) = '      FRAME_1400000_START         = ( ''' &
      //             '2000 JAN 1 00:00:00 TDB'','
      TXTBUF( 13 ) = '                                      ''' &
      //             '2000 FEB 1 00:00:00 TDB'','
      TXTBUF( 14 ) = '                                      ''' &
      //             '2000 MAR 1 00:00:00 TDB'','
      TXTBUF( 15 ) = '                                      ''' &
      //             '2000 APR 1 00:00:00 TDB'' )'
      TXTBUF( 16 ) = ' '
      TXTBUF( 17 ) = '      FRAME_1400000_STOP          = ( ''' &
      //             '2000 FEB 1 00:00:00 TDB'','
      TXTBUF( 18 ) = '                                      ''' &
      //             '2000 MAR 1 00:00:00 TDB'','
      TXTBUF( 19 ) = '                                      ''' &
      //             '2000 APR 1 00:00:00 TDB'','
      TXTBUF( 20 ) = '                                      ''' &
      //             '2000 MAY 1 00:00:00 TDB''  )'
      TXTBUF( 21 ) = ' '
      TXTBUF( 22 ) = '      FRAME_SWITCH1               =  14' &
      //             '00001'
      TXTBUF( 23 ) = '      FRAME_1400001_NAME          =  ''' &
      //             'SWITCH1'''
      TXTBUF( 24 ) = '      FRAME_1400001_CLASS         =  6'
      TXTBUF( 25 ) = '      FRAME_1400001_CLASS_ID      =  14' &
      //             '00001'
      TXTBUF( 26 ) = '      FRAME_SWITCH1_CENTER        =  0'
      TXTBUF( 27 ) = '      FRAME_1400001_ALIGNED_WITH  =  ( ' &
      //             '''J2000'','
      TXTBUF( 28 ) = '                                       ' &
      //             '''ECLIPJ2000'','
      TXTBUF( 29 ) = '                                       ' &
      //             '''B1950'','
      TXTBUF( 30 ) = '                                       ' &
      //             '''GALACTIC'' )'
      TXTBUF( 31 ) = ' '
      TXTBUF( 32 ) = '      FRAME_1400001_START         = ( @' &
      //             '2000-JAN-1/00:00:00,'
      TXTBUF( 33 ) = '                                      @' &
      //             '2000-FEB-1/00:00:00,'
      TXTBUF( 34 ) = '                                      @' &
      //             '2000-MAR-1/00:00:00,'
      TXTBUF( 35 ) = '                                      @' &
      //             '2000-APR-1/00:00:00  )'
      TXTBUF( 36 ) = ' '
      TXTBUF( 37 ) = '      FRAME_1400001_STOP          = ( @' &
      //             '2000-FEB-1/00:00:00,'
      TXTBUF( 38 ) = '                                      @' &
      //             '2000-MAR-1/00:00:00,'
      TXTBUF( 39 ) = '                                      @' &
      //             '2000-APR-1/00:00:00,'
      TXTBUF( 40 ) = '                                      @' &
      //             '2000-MAY-1/00:00:00  )'
      TXTBUF( 41 ) = ' '
      TXTBUF( 42 ) = '      FRAME_SWITCH2               =  14' &
      //             '00002'
      TXTBUF( 43 ) = '      FRAME_1400002_NAME          =  ''' &
      //             'SWITCH2'''
      TXTBUF( 44 ) = '      FRAME_1400002_CLASS         =  6'
      TXTBUF( 45 ) = '      FRAME_1400002_CLASS_ID      =  14' &
      //             '00002'
      TXTBUF( 46 ) = '      FRAME_1400002_CENTER        =  ''' &
      //             'SOLAR SYSTEM BARYCENTER'''
      TXTBUF( 47 ) = '      FRAME_1400002_ALIGNED_WITH  =  ( ' &
      //             '1,'
      TXTBUF( 48 ) = '                                       ' &
      //             '17,'
      TXTBUF( 49 ) = '                                       ' &
      //             '2,'
      TXTBUF( 50 ) = '                                       ' &
      //             '13 )'
      TXTBUF( 51 ) = ' '
      TXTBUF( 52 ) = '      FRAME_1400002_START         = ( ''' &
      //             '2000 JAN 1 00:00:00 TDB'','
      TXTBUF( 53 ) = '                                      ''' &
      //             '2000 FEB 1 00:00:00 TDB'','
      TXTBUF( 54 ) = '                                      ''' &
      //             '2000 MAR 1 00:00:00 TDB'','
      TXTBUF( 55 ) = '                                      ''' &
      //             '2000 APR 1 00:00:00 TDB'' )'
      TXTBUF( 56 ) = ' '
      TXTBUF( 57 ) = '      FRAME_1400002_STOP          = ( @' &
      //             '2000-FEB-1/00:00:00,'
      TXTBUF( 58 ) = '                                      @' &
      //             '2000-MAR-1/00:00:00,'
      TXTBUF( 59 ) = '                                      @' &
      //             '2000-APR-2/00:00:00,'
      TXTBUF( 60 ) = '                                      @' &
      //             '2000-MAY-1/00:00:00  )'
      TXTBUF( 61 ) = ' '
      TXTBUF( 62 ) = '      FRAME_SWITCH3               =  14' &
      //             '00003'
      TXTBUF( 63 ) = '      FRAME_1400003_NAME          =  ''' &
      //             'SWITCH3'''
      TXTBUF( 64 ) = '      FRAME_1400003_CLASS         =  6'
      TXTBUF( 65 ) = '      FRAME_1400003_CLASS_ID      =  14' &
      //             '00003'
      TXTBUF( 66 ) = '      FRAME_1400003_CENTER        =  ''' &
      //             'SOLAR SYSTEM BARYCENTER'''
      TXTBUF( 67 ) = '      FRAME_1400003_ALIGNED_WITH  =  ( ' &
      //             '1,'
      TXTBUF( 68 ) = '                                       ' &
      //             '17,'
      TXTBUF( 69 ) = '                                       ' &
      //             '2,'
      TXTBUF( 70 ) = '                                       ' &
      //             '13 )'
      TXTBUF( 71 ) = BEGTXT()



      IF ( EXISTS(FK0) ) THEN
         CALL KILFIL( FK0 )
      END IF

      NLINES = 71
      CALL TSTTXT ( FK0, TXTBUF, NLINES, .TRUE., .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Also load LSK to support time conversions.
!
      CALL TSTLSK

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Fetch data for frame SWITCH0 in FK0, using free ' &
      //           '= 1.' )


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

!
!     Start with clean database.
!
      CALL ZZHSIINI ( MAXFRM, HDFRAM, FRPOOL )
      CALL CLEARI   ( MAXFRM, FIDLST         )
      CALL CLEARI   ( MAXFRM, BASBEG         )
      FREE = 1

      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG, &
                      FREE,   BASCNT, USETIM, BINARY, CLSSES, &
                      CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )

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

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Fetch data for frame SWITCH0 in FK0, using ' &
      //           'a value of FREE greater than 1.' )

      FRAMID = 1400000

!
!     Start with clean database.
!
      CALL ZZHSIINI ( MAXFRM, HDFRAM, FRPOOL )
      CALL CLEARI   ( MAXFRM, FIDLST         )
      CALL CLEARI   ( MAXFRM, BASBEG         )
      FREE = 100


      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG, &
                      FREE,   BASCNT, USETIM, BINARY, CLSSES, &
                      CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )

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

            CALL CHCKSC( 'BASNAM', BASNAM,    '=', &
                                     XBASE(I),  OK  )

            CALL CHCKSD( 'STARTS', STARTS(J), '=', &
                                   XSTART(I), 0.D0, OK )

            CALL CHCKSD( 'STOPS', STOPS(J), '=', &
                                  XSTOP(I),  0.D0, OK )

            CALL CHCKSI( 'CLSSES', CLSSES(J), '=', XCLASS(I), 0, OK)
            CALL CHCKSI( 'CLSIDS', CLSIDS(J), '=', XCLSID(I), 0, OK)

         END DO

      END IF

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Fetch data for frame SWITCH1 in FK0.' )

!
!     Frame SWITCH1 differs from SWITCH0 only in the time interval
!     bound data type.
!
      FRAMID = 1400001

      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG, &
                      FREE,   BASCNT, USETIM, BINARY, CLSSES, &
                      CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )

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

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Fetch data for frame SWITCH2 in FK0.' )

!
!     Frame SWITCH2 differs from SWITCH0 only in the base frame
!     data type and base frame stop times.
!
      CALL STR2ET( '2000 FEB 1 00:00:00 TDB', XSTOP(1) )
      CALL STR2ET( '2000 MAR 1 00:00:00 TDB', XSTOP(2) )
      CALL STR2ET( '2000 APR 2 00:00:00 TDB', XSTOP(3) )
      CALL STR2ET( '2000 MAY 1 00:00:00 TDB', XSTOP(4) )

      FRAMID = 1400002

      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG, &
                      FREE,   BASCNT, USETIM, BINARY, CLSSES, &
                      CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )

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


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Fetch data for frame SWITCH3 in FK0.' )

!
!     Frame SWITCH3 has no time intervals.
!
      FRAMID = 1400003

      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG, &
                      FREE,   BASCNT, USETIM, BINARY, CLSSES, &
                      CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )

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

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Create and use a switch frame having half the ' &
      //           'maximum number of base frame intervals.' )

!
!     Don't clear the kernel pool: force the switch frame subsystem
!     to make room.
!
      NBASES = MAXBAS/2

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

      CALL FILLI  ( INERTL, NBASES, XCLASS )
      CALL FILLI  ( 17,     NBASES, XCLSID )

!
!     Create a set of disjoint intervals.
!
      DO I = 1, NBASES
         XSTART(I) = 2.D0 * DBLE(I-1)
         XSTOP(I) = XSTART(I) + 0.9D0
      END DO

      CALL PDPOOL ( 'FRAME_1400010_START', NBASES, XSTART )
      CALL PDPOOL ( 'FRAME_1400010_STOP',  NBASES, XSTOP )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Do a frame transformation to populate the switch frame database.
!
      ET = XSTART( NBASES/2 ) + 0.5D0
      CALL SXFORM ( 'SWITCH_MAXBASE', 'J2000', ET, XFORM )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Fetch data for the frame.
!
      FRAMID = 1400010

      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG, &
                      FREE,   BASCNT, USETIM, BINARY, CLSSES, &
                      CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )

      CALL CHCKXC ( .FALSE., ' ', OK )
!
!     This test depends on the previous test cases.
!
      CALL CHCKSI ( 'FREE', FREE, '=', 7616, 0, OK )

      CALL CHCKSI ( 'FRAMAT', FRAMAT, '!=', 0, 0, OK )

      IF ( FRAMAT .NE. 0 ) THEN
!
!       This test depends on the previous test cases.
!
         CALL CHCKSI ( 'BASBEG(FRAMAT)', &
                        BASBEG(FRAMAT), '=', 116, 0, OK )

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

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Create and use a switch frame having the maximum ' &
      //           'number of base frame intervals.' )

!
!     Don't clear the kernel pool: force the switch frame subsystem
!     to make room.
!
      NBASES = MAXBAS

      CALL PIPOOL ( 'FRAME_SWITCH_MAXBASE',   1, 1400011          )
      CALL PCPOOL ( 'FRAME_1400011_NAME',     1, 'SWITCH_MAXBASE' )
      CALL PIPOOL ( 'FRAME_1400011_CLASS',    1, 6                )
      CALL PIPOOL ( 'FRAME_1400011_CLASS_ID', 1, 1400011          )
      CALL PIPOOL ( 'FRAME_1400011_CENTER',   1, 0                )
      CALL CHCKXC ( .FALSE., ' ', OK )
!
!     17 is the ID code of ECLIPJ2000.
!
      CALL FILLI  ( 17, NBASES, IBASBF )
      CALL PIPOOL ( 'FRAME_1400011_ALIGNED_WITH', NBASES, IBASBF )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FILLI  ( INERTL, NBASES, XCLASS )
      CALL FILLI  ( 17,     NBASES, XCLSID )

!
!     Create a set of disjoint intervals.
!
      DO I = 1, NBASES
         XSTART(I) = 2.D0 * DBLE(I-1)
         XSTOP(I) = XSTART(I) + 0.9D0
      END DO

      CALL PDPOOL ( 'FRAME_1400011_START', NBASES, XSTART )
      CALL PDPOOL ( 'FRAME_1400011_STOP',  NBASES, XSTOP )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Do a frame transformation to populate the switch frame database.
!
      ET = XSTART( NBASES/2 ) + 0.5D0
      CALL SXFORM ( 'SWITCH_MAXBASE', 'J2000', ET, XFORM )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Fetch data for the frame.
!
      FRAMID = 1400011

      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG, &
                      FREE,   BASCNT, USETIM, BINARY, CLSSES, &
                      CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     This test depends on the previous test cases.
!
      CALL CHCKSI ( 'FREE', FREE, '=', MAXBAS+1, 0, OK )

      CALL CHCKSI ( 'FRAMAT', FRAMAT, '!=', 0, 0, OK )

      IF ( FRAMAT .NE. 0 ) THEN
!
!       This test depends on the previous test cases.
!
         CALL CHCKSI ( 'BASBEG(FRAMAT)', &
                        BASBEG(FRAMAT), '=', 1, 0, OK )

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

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Look up a frame that doesn''t exist.' )

!
!     Prepare initialized data structures to compare against.
!
      CALL ZZHSIINI ( MAXFRM, NWHDFR, NWPOOL )
      CALL CLEARI   ( MAXFRM, NWHDBS )
      CALL CHCKXC ( .FALSE., ' ', OK )


      FRAMID = -1

      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG, &
                      FREE,   BASCNT, USETIM, BINARY, CLSSES, &
                      CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )


      CALL CHCKXC ( .TRUE., 'SPICE(MISSINGFRAMEVAR)', OK )

      CALL CHCKSI ( 'FRAMAT', FRAMAT, '=', 0, 0, OK )

      CALL CHCKAI ( 'FRPOOL', FRPOOL, '=', NWPOOL, MAXFRM+6, OK )
      CALL CHCKAI ( 'HDFRAM', HDFRAM, '=', NWHDFR, MAXFRM,   OK )
      CALL CHCKAI ( 'BASBEG', BASBEG, '=', NWHDBS, MAXFRM,   OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Missing switch frame name' )

      FRAMID = 1500000

      CALL PIPOOL ( 'FRAME_SWITCH_ERROR',         1, FRAMID )
!
!     Don't set the frame name.
!
      CALL PIPOOL ( 'FRAME_1500000_CLASS',        1, 6      )
      CALL PIPOOL ( 'FRAME_1500000_CLASS_ID',     1, FRAMID )
      CALL PIPOOL ( 'FRAME_1500000_CENTER',       1, 0      )
      CALL PIPOOL ( 'FRAME_1500000_ALIGNED_WITH', 1, 1      )

      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG, &
                      FREE,   BASCNT, USETIM, BINARY, CLSSES, &
                      CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )


      CALL CHCKXC ( .TRUE., 'SPICE(MISSINGFRAMEVAR)', OK )

      CALL CHCKSI ( 'FRAMAT', FRAMAT, '=', 0, 0, OK )

      CALL CHCKAI ( 'FRPOOL', FRPOOL, '=', NWPOOL, MAXFRM+6, OK )
      CALL CHCKAI ( 'HDFRAM', HDFRAM, '=', NWHDFR, MAXFRM,   OK )
      CALL CHCKAI ( 'BASBEG', BASBEG, '=', NWHDBS, MAXFRM,   OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Missing switch frame ID' )

      FRAMID = 1500000

!
!     Don't set the frame ID in the kernel pool.
!
      CALL DVPOOL ( 'FRAME_SWITCH_ERROR' )

      CALL PCPOOL ( 'FRAME_1500000_NAME',         1, 'SWITCH_ERROR' )
      CALL PIPOOL ( 'FRAME_1500000_CLASS',        1, 6      )
      CALL PIPOOL ( 'FRAME_1500000_CLASS_ID',     1, FRAMID )
      CALL PIPOOL ( 'FRAME_1500000_CENTER',       1, 0      )
      CALL PIPOOL ( 'FRAME_1500000_ALIGNED_WITH', 1, 1      )

      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG, &
                      FREE,   BASCNT, USETIM, BINARY, CLSSES, &
                      CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )


      CALL CHCKXC ( .TRUE., 'SPICE(MISSINGFRAMEVAR)', OK )

      CALL CHCKSI ( 'FRAMAT', FRAMAT, '=', 0, 0, OK )

      CALL CHCKAI ( 'FRPOOL', FRPOOL, '=', NWPOOL, MAXFRM+6, OK )
      CALL CHCKAI ( 'HDFRAM', HDFRAM, '=', NWHDFR, MAXFRM,   OK )
      CALL CHCKAI ( 'BASBEG', BASBEG, '=', NWHDBS, MAXFRM,   OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Switch frame ID triggers GIPOOL error' )

      FRAMID = 1500000

      CALL PDPOOL ( 'FRAME_SWITCH_ERROR',         1, 1.D20 )

      CALL PCPOOL ( 'FRAME_1500000_NAME',         1, 'SWITCH_ERROR' )
      CALL PIPOOL ( 'FRAME_1500000_CLASS',        1, 6      )
      CALL PIPOOL ( 'FRAME_1500000_CLASS_ID',     1, FRAMID )
      CALL PIPOOL ( 'FRAME_1500000_CENTER',       1, 0      )
      CALL PIPOOL ( 'FRAME_1500000_ALIGNED_WITH', 1, 1      )

      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG, &
                      FREE,   BASCNT, USETIM, BINARY, CLSSES, &
                      CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )


      CALL CHCKXC ( .TRUE., 'SPICE(INTOUTOFRANGE)', OK )

      CALL CHCKSI ( 'FRAMAT', FRAMAT, '=', 0, 0, OK )

      CALL CHCKAI ( 'FRPOOL', FRPOOL, '=', NWPOOL, MAXFRM+6, OK )
      CALL CHCKAI ( 'HDFRAM', HDFRAM, '=', NWHDFR, MAXFRM,   OK )
      CALL CHCKAI ( 'BASBEG', BASBEG, '=', NWHDBS, MAXFRM,   OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Frame class ID triggers GIPOOL error' )

      FRAMID = 1500000
!
!     Don't set the frame class ID in the kernel pool.
!
      CALL DVPOOL ( 'FRAME_1500000_CLASS_ID' )

      CALL PIPOOL ( 'FRAME_SWITCH_ERROR',         1, FRAMID )
      CALL PCPOOL ( 'FRAME_1500000_NAME',         1, 'SWITCH_ERROR' )
      CALL PIPOOL ( 'FRAME_1500000_CLASS',        1, 6      )
      CALL PIPOOL ( 'FRAME_1500000_CENTER',       1, 0      )
      CALL PIPOOL ( 'FRAME_1500000_ALIGNED_WITH', 1, 1      )

      CALL PDPOOL ( 'FRAME_1500000_CLASS_ID',     1, 1.D20  )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG, &
                      FREE,   BASCNT, USETIM, BINARY, CLSSES, &
                      CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )

      CALL CHCKXC ( .TRUE., 'SPICE(INTOUTOFRANGE)', OK )

      CALL CHCKSI ( 'FRAMAT', FRAMAT, '=', 0, 0, OK )

      CALL CHCKAI ( 'FRPOOL', FRPOOL, '=', NWPOOL, MAXFRM+6, OK )
      CALL CHCKAI ( 'HDFRAM', HDFRAM, '=', NWHDFR, MAXFRM,   OK )
      CALL CHCKAI ( 'BASBEG', BASBEG, '=', NWHDBS, MAXFRM,   OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Mismatched switch frame ID: ID associated with ' &
      //           'name doesn''t match ID in associated frame ' &
      //           'specification kernel variables.' )

      FRAMID = 1500000

      CALL PIPOOL ( 'FRAME_SWITCH_ERROR', 1, 1499999 )

      CALL PCPOOL ( 'FRAME_1500000_NAME',         1, 'SWITCH_ERROR' )
      CALL PIPOOL ( 'FRAME_1500000_CLASS',        1, 6      )
      CALL PIPOOL ( 'FRAME_1500000_CLASS_ID',     1, FRAMID )
      CALL PIPOOL ( 'FRAME_1500000_CENTER',       1, 0      )
      CALL PIPOOL ( 'FRAME_1500000_ALIGNED_WITH', 1, 1      )

      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG, &
                      FREE,   BASCNT, USETIM, BINARY, CLSSES, &
                      CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )


      CALL CHCKXC ( .TRUE., 'SPICE(BADFRAMESPEC)', OK )

      CALL CHCKSI ( 'FRAMAT', FRAMAT, '=', 0, 0, OK )

      CALL CHCKAI ( 'FRPOOL', FRPOOL, '=', NWPOOL, MAXFRM+6, OK )
      CALL CHCKAI ( 'HDFRAM', HDFRAM, '=', NWHDFR, MAXFRM,   OK )
      CALL CHCKAI ( 'BASBEG', BASBEG, '=', NWHDBS, MAXFRM,   OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Missing frame class' )

      FRAMID = 1500000

!
!     Don't set the frame class in the kernel pool.
!
      CALL DVPOOL ( 'FRAME_1500000_CLASS' )

      CALL PIPOOL ( 'FRAME_SWITCH_ERROR',         1, FRAMID )
      CALL PCPOOL ( 'FRAME_1500000_NAME',         1, 'SWITCH_ERROR' )
      CALL PIPOOL ( 'FRAME_1500000_CLASS_ID',     1, FRAMID )
      CALL PIPOOL ( 'FRAME_1500000_CENTER',       1, 0      )
      CALL PIPOOL ( 'FRAME_1500000_ALIGNED_WITH', 1, 1      )

      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG, &
                      FREE,   BASCNT, USETIM, BINARY, CLSSES, &
                      CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )

      CALL CHCKXC ( .TRUE., 'SPICE(MISSINGFRAMEVAR)', OK )

      CALL CHCKSI ( 'FRAMAT', FRAMAT, '=', 0, 0, OK )

      CALL CHCKAI ( 'FRPOOL', FRPOOL, '=', NWPOOL, MAXFRM+6, OK )
      CALL CHCKAI ( 'HDFRAM', HDFRAM, '=', NWHDFR, MAXFRM,   OK )
      CALL CHCKAI ( 'BASBEG', BASBEG, '=', NWHDBS, MAXFRM,   OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Missing frame class ID' )

      FRAMID = 1500000
!
!     Don't set the frame class ID in the kernel pool.
!
      CALL DVPOOL ( 'FRAME_1500000_CLASS_ID' )

      CALL PIPOOL ( 'FRAME_SWITCH_ERROR',         1, FRAMID )
      CALL PCPOOL ( 'FRAME_1500000_NAME',         1, 'SWITCH_ERROR' )
      CALL PIPOOL ( 'FRAME_1500000_CLASS',        1, 6      )
      CALL PIPOOL ( 'FRAME_1500000_CENTER',       1, 0      )
      CALL PIPOOL ( 'FRAME_1500000_ALIGNED_WITH', 1, 1      )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG, &
                      FREE,   BASCNT, USETIM, BINARY, CLSSES, &
                      CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )

      CALL CHCKXC ( .TRUE., 'SPICE(MISSINGFRAMEVAR)', OK )

      CALL CHCKSI ( 'FRAMAT', FRAMAT, '=', 0, 0, OK )

      CALL CHCKAI ( 'FRPOOL', FRPOOL, '=', NWPOOL, MAXFRM+6, OK )
      CALL CHCKAI ( 'HDFRAM', HDFRAM, '=', NWHDFR, MAXFRM,   OK )
      CALL CHCKAI ( 'BASBEG', BASBEG, '=', NWHDBS, MAXFRM,   OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Frame class ID triggers GIPOOL error' )

      FRAMID = 1500000
!
!     Don't set the frame class ID in the kernel pool.
!
      CALL DVPOOL ( 'FRAME_1500000_CLASS_ID' )

      CALL PIPOOL ( 'FRAME_SWITCH_ERROR',         1, FRAMID )
      CALL PCPOOL ( 'FRAME_1500000_NAME',         1, 'SWITCH_ERROR' )
      CALL PIPOOL ( 'FRAME_1500000_CLASS',        1, 6      )
      CALL PIPOOL ( 'FRAME_1500000_CENTER',       1, 0      )
      CALL PIPOOL ( 'FRAME_1500000_ALIGNED_WITH', 1, 1      )

      CALL PDPOOL ( 'FRAME_1500000_CLASS_ID',     1, 1.D20  )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG, &
                      FREE,   BASCNT, USETIM, BINARY, CLSSES, &
                      CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )

      CALL CHCKXC ( .TRUE., 'SPICE(INTOUTOFRANGE)', OK )

      CALL CHCKSI ( 'FRAMAT', FRAMAT, '=', 0, 0, OK )

      CALL CHCKAI ( 'FRPOOL', FRPOOL, '=', NWPOOL, MAXFRM+6, OK )
      CALL CHCKAI ( 'HDFRAM', HDFRAM, '=', NWHDFR, MAXFRM,   OK )
      CALL CHCKAI ( 'BASBEG', BASBEG, '=', NWHDBS, MAXFRM,   OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Missing frame center' )

      FRAMID = 1500000

!
!     Prepare initialized data structures to compare against.
!
      CALL ZZHSIINI ( MAXFRM, NWHDFR, NWPOOL )
      CALL CLEARI   ( MAXFRM, NWHDBS )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Don't set the frame center in the kernel pool.
!
      CALL DVPOOL ( 'FRAME_1500000_CENTER' )

      CALL PIPOOL ( 'FRAME_SWITCH_ERROR',         1, FRAMID )
      CALL PCPOOL ( 'FRAME_1500000_NAME',         1, 'SWITCH_ERROR' )
      CALL PIPOOL ( 'FRAME_1500000_CLASS',        1, 6      )
      CALL PIPOOL ( 'FRAME_1500000_CLASS_ID',     1, FRAMID )
      CALL PIPOOL ( 'FRAME_1500000_ALIGNED_WITH', 1, 1      )

      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG, &
                      FREE,   BASCNT, USETIM, BINARY, CLSSES, &
                      CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )

      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

      CALL CHCKSI ( 'FRAMAT', FRAMAT, '=', 0, 0, OK )

      CALL CHCKAI ( 'FRPOOL', FRPOOL, '=', NWPOOL, MAXFRM+6, OK )
      CALL CHCKAI ( 'HDFRAM', HDFRAM, '=', NWHDFR, MAXFRM,   OK )
      CALL CHCKAI ( 'BASBEG', BASBEG, '=', NWHDBS, MAXFRM,   OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Missing base frame list' )

      FRAMID = 1500000
!
!     Don't set the base frame list in the kernel pool.
!
      CALL DVPOOL ( 'FRAME_1500000_ALIGNED_WITH' )

      CALL PIPOOL ( 'FRAME_SWITCH_ERROR',         1, FRAMID )
      CALL PCPOOL ( 'FRAME_1500000_NAME',         1, 'SWITCH_ERROR' )
      CALL PIPOOL ( 'FRAME_1500000_CLASS',        1, 6      )
      CALL PIPOOL ( 'FRAME_1500000_CLASS_ID',     1, FRAMID )
      CALL PIPOOL ( 'FRAME_1500000_CENTER',       1, 0      )

      CALL DVPOOL ( 'FRAME_1500000_ALIGNED_WITH' )

      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG, &
                      FREE,   BASCNT, USETIM, BINARY, CLSSES, &
                      CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )

      CALL CHCKXC ( .TRUE., 'SPICE(MISSINGFRAMEVAR)', OK )

      CALL CHCKSI ( 'FRAMAT', FRAMAT, '=', 0, 0, OK )

      CALL CHCKAI ( 'FRPOOL', FRPOOL, '=', NWPOOL, MAXFRM+6, OK )
      CALL CHCKAI ( 'HDFRAM', HDFRAM, '=', NWHDFR, MAXFRM,   OK )
      CALL CHCKAI ( 'BASBEG', BASBEG, '=', NWHDBS, MAXFRM,   OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Unrecognized base frame name' )

      FRAMID = 1500000

      CALL PIPOOL ( 'FRAME_SWITCH_ERROR',         1, FRAMID )
      CALL PCPOOL ( 'FRAME_1500000_NAME',         1, 'SWITCH_ERROR' )
      CALL PIPOOL ( 'FRAME_1500000_CLASS',        1, 6      )
      CALL PIPOOL ( 'FRAME_1500000_CLASS_ID',     1, FRAMID )
      CALL PIPOOL ( 'FRAME_1500000_CENTER',       1, 0      )

      CALL DVPOOL ( 'FRAME_1500000_ALIGNED_WITH' )
      CALL PCPOOL ( 'FRAME_1500000_ALIGNED_WITH', 1, 'XYZ'  )

      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG, &
                      FREE,   BASCNT, USETIM, BINARY, CLSSES, &
                      CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )

      CALL CHCKXC ( .TRUE., 'SPICE(FRAMENAMENOTFOUND)', OK )

      CALL CHCKSI ( 'FRAMAT', FRAMAT, '=', 0, 0, OK )

      CALL CHCKAI ( 'FRPOOL', FRPOOL, '=', NWPOOL, MAXFRM+6, OK )
      CALL CHCKAI ( 'HDFRAM', HDFRAM, '=', NWHDFR, MAXFRM,   OK )
      CALL CHCKAI ( 'BASBEG', BASBEG, '=', NWHDBS, MAXFRM,   OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Base frame triggers FRINFO error' )

      FRAMID = 1500000

      CALL PIPOOL ( 'FRAME_SWITCH_ERROR',         1, FRAMID )
      CALL PCPOOL ( 'FRAME_1500000_NAME',         1, 'SWITCH_ERROR' )
      CALL PIPOOL ( 'FRAME_1500000_CLASS',        1, 6      )
      CALL PIPOOL ( 'FRAME_1500000_CLASS_ID',     1, FRAMID )
      CALL PIPOOL ( 'FRAME_1500000_CENTER',       1, 0      )

      CALL PIPOOL ( 'FRAME_1500000_ALIGNED_WITH', 1, -777  )

      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG, &
                      FREE,   BASCNT, USETIM, BINARY, CLSSES, &
                      CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )

      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEINFONOTFOUND)', OK )

      CALL CHCKSI ( 'FRAMAT', FRAMAT, '=', 0, 0, OK )

      CALL CHCKAI ( 'FRPOOL', FRPOOL, '=', NWPOOL, MAXFRM+6, OK )
      CALL CHCKAI ( 'HDFRAM', HDFRAM, '=', NWHDFR, MAXFRM,   OK )
      CALL CHCKAI ( 'BASBEG', BASBEG, '=', NWHDBS, MAXFRM,   OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Base frame ID triggers GIPOOL error' )

      FRAMID = 1500000

      CALL PIPOOL ( 'FRAME_SWITCH_ERROR',         1, FRAMID )
      CALL PCPOOL ( 'FRAME_1500000_NAME',         1, 'SWITCH_ERROR' )
      CALL PIPOOL ( 'FRAME_1500000_CLASS',        1, 6      )
      CALL PIPOOL ( 'FRAME_1500000_CLASS_ID',     1, FRAMID )
      CALL PIPOOL ( 'FRAME_1500000_CENTER',       1, 0      )

      CALL PDPOOL ( 'FRAME_1500000_ALIGNED_WITH', 1, 1.D20  )

      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG, &
                      FREE,   BASCNT, USETIM, BINARY, CLSSES, &
                      CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )

      CALL CHCKXC ( .TRUE., 'SPICE(INTOUTOFRANGE)', OK )

      CALL CHCKSI ( 'FRAMAT', FRAMAT, '=', 0, 0, OK )

      CALL CHCKAI ( 'FRPOOL', FRPOOL, '=', NWPOOL, MAXFRM+6, OK )
      CALL CHCKAI ( 'HDFRAM', HDFRAM, '=', NWHDFR, MAXFRM,   OK )
      CALL CHCKAI ( 'BASBEG', BASBEG, '=', NWHDBS, MAXFRM,   OK )


!
! --- Case: ------------------------------------------------------
!
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
      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG, &
                      FREE,   BASCNT, USETIM, BINARY, CLSSES, &
                      CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )


      CALL CHCKXC ( .TRUE., 'SPICE(PARTIALFRAMESPEC)', OK )

      CALL CHCKSI ( 'FRAMAT', FRAMAT, '=', 0, 0, OK )

      CALL CHCKAI ( 'FRPOOL', FRPOOL, '=', NWPOOL, MAXFRM+6, OK )
      CALL CHCKAI ( 'HDFRAM', HDFRAM, '=', NWHDFR, MAXFRM,   OK )
      CALL CHCKAI ( 'BASBEG', BASBEG, '=', NWHDBS, MAXFRM,   OK )


!
! --- Case: ------------------------------------------------------
!
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

      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG, &
                      FREE,   BASCNT, USETIM, BINARY, CLSSES, &
                      CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )

      CALL CHCKXC ( .TRUE., 'SPICE(PARTIALFRAMESPEC)', OK )

      CALL CHCKSI ( 'FRAMAT', FRAMAT, '=', 0, 0, OK )

      CALL CHCKAI ( 'FRPOOL', FRPOOL, '=', NWPOOL, MAXFRM+6, OK )
      CALL CHCKAI ( 'HDFRAM', HDFRAM, '=', NWHDFR, MAXFRM,   OK )
      CALL CHCKAI ( 'BASBEG', BASBEG, '=', NWHDBS, MAXFRM,   OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Start time count doesn''t match base frame ' &
      //           'count.' )

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
      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG, &
                      FREE,   BASCNT, USETIM, BINARY, CLSSES, &
                      CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )


      CALL CHCKXC ( .TRUE., 'SPICE(COUNTMISMATCH)', OK )

      CALL CHCKSI ( 'FRAMAT', FRAMAT, '=', 0, 0, OK )

      CALL CHCKAI ( 'FRPOOL', FRPOOL, '=', NWPOOL, MAXFRM+6, OK )
      CALL CHCKAI ( 'HDFRAM', HDFRAM, '=', NWHDFR, MAXFRM,   OK )
      CALL CHCKAI ( 'BASBEG', BASBEG, '=', NWHDBS, MAXFRM,   OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Stop time count doesn''t match base frame ' &
      //           'count.' )

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
      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG, &
                      FREE,   BASCNT, USETIM, BINARY, CLSSES, &
                      CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )

      CALL CHCKXC ( .TRUE., 'SPICE(COUNTMISMATCH)', OK )

      CALL CHCKSI ( 'FRAMAT', FRAMAT, '=', 0, 0, OK )

      CALL CHCKAI ( 'FRPOOL', FRPOOL, '=', NWPOOL, MAXFRM+6, OK )
      CALL CHCKAI ( 'HDFRAM', HDFRAM, '=', NWHDFR, MAXFRM,   OK )
      CALL CHCKAI ( 'BASBEG', BASBEG, '=', NWHDBS, MAXFRM,   OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Start and stop time counts are equal but ' &
      //           'don''t match base frame count.' )

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

      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG, &
                      FREE,   BASCNT, USETIM, BINARY, CLSSES, &
                      CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )


      CALL CHCKXC ( .TRUE., 'SPICE(COUNTMISMATCH)', OK )

      CALL CHCKSI ( 'FRAMAT', FRAMAT, '=', 0, 0, OK )

      CALL CHCKAI ( 'FRPOOL', FRPOOL, '=', NWPOOL, MAXFRM+6, OK )
      CALL CHCKAI ( 'HDFRAM', HDFRAM, '=', NWHDFR, MAXFRM,   OK )
      CALL CHCKAI ( 'BASBEG', BASBEG, '=', NWHDBS, MAXFRM,   OK )


!
! --- Case: ------------------------------------------------------
!
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

      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG, &
                      FREE,   BASCNT, USETIM, BINARY, CLSSES, &
                      CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )

      CALL CHCKXC ( .TRUE., 'SPICE(UNPARSEDTIME)', OK )

      CALL CHCKSI ( 'FRAMAT', FRAMAT, '=', 0, 0, OK )

      CALL CHCKAI ( 'FRPOOL', FRPOOL, '=', NWPOOL, MAXFRM+6, OK )
      CALL CHCKAI ( 'HDFRAM', HDFRAM, '=', NWHDFR, MAXFRM,   OK )
      CALL CHCKAI ( 'BASBEG', BASBEG, '=', NWHDBS, MAXFRM,   OK )


!
! --- Case: ------------------------------------------------------
!
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

      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG, &
                      FREE,   BASCNT, USETIM, BINARY, CLSSES, &
                      CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )


      CALL CHCKXC ( .TRUE., 'SPICE(UNPARSEDTIME)', OK )

      CALL CHCKSI ( 'FRAMAT', FRAMAT, '=', 0, 0, OK )

      CALL CHCKAI ( 'FRPOOL', FRPOOL, '=', NWPOOL, MAXFRM+6, OK )
      CALL CHCKAI ( 'HDFRAM', HDFRAM, '=', NWHDFR, MAXFRM,   OK )
      CALL CHCKAI ( 'BASBEG', BASBEG, '=', NWHDBS, MAXFRM,   OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Create and use a switch frame having half the ' &
      //           'maximum number of base frame intervals. Post ' &
      //           'error test case version.' )

!
!     Don't clear the kernel pool: force the switch frame subsystem
!     to make room.
!
      NBASES = MAXBAS/2

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

      CALL FILLI  ( INERTL, NBASES, XCLASS )
      CALL FILLI  ( 17,     NBASES, XCLSID )

!
!     Create a set of disjoint intervals.
!
      DO I = 1, NBASES
         XSTART(I) = 2.D0 * DBLE(I-1)
         XSTOP(I) = XSTART(I) + 0.9D0
      END DO

      CALL PDPOOL ( 'FRAME_1400010_START', NBASES, XSTART )
      CALL PDPOOL ( 'FRAME_1400010_STOP',  NBASES, XSTOP )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Do a frame transformation to populate the switch frame database.
!
      ET = XSTART( NBASES/2 ) + 0.5D0
      CALL SXFORM ( 'SWITCH_MAXBASE', 'J2000', ET, XFORM )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Fetch data for the frame.
!
      FRAMID = 1400010

      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG, &
                      FREE,   BASCNT, USETIM, BINARY, CLSSES, &
                      CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     This test depends on the previous test cases.
!
      CALL CHCKSI ( 'FREE', FREE, '=', NBASES+1, 0, OK )

      CALL CHCKSI ( 'FRAMAT', FRAMAT, '!=', 0, 0, OK )

      IF ( FRAMAT .NE. 0 ) THEN
!
!       This test depends on the previous test cases.
!
         CALL CHCKSI ( 'BASBEG(FRAMAT)', &
                        BASBEG(FRAMAT), '=', 1, 0, OK )

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

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Create and use a switch frame having the maximum ' &
      //           'number of base frame intervals. Post error test ' &
      //           'case version.' )

!
!     Don't clear the kernel pool: force the switch frame subsystem
!     to make room.
!
      NBASES = MAXBAS

      CALL PIPOOL ( 'FRAME_SWITCH_MAXBASE',   1, 1400011          )
      CALL PCPOOL ( 'FRAME_1400011_NAME',     1, 'SWITCH_MAXBASE' )
      CALL PIPOOL ( 'FRAME_1400011_CLASS',    1, 6                )
      CALL PIPOOL ( 'FRAME_1400011_CLASS_ID', 1, 1400011          )
      CALL PIPOOL ( 'FRAME_1400011_CENTER',   1, 0                )
      CALL CHCKXC ( .FALSE., ' ', OK )
!
!     17 is the ID code of ECLIPJ2000.
!
      CALL FILLI  ( 17, NBASES, IBASBF )
      CALL PIPOOL ( 'FRAME_1400011_ALIGNED_WITH', NBASES, IBASBF )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FILLI  ( INERTL, NBASES, XCLASS )
      CALL FILLI  ( 17,     NBASES, XCLSID )

!
!     Create a set of disjoint intervals.
!
      DO I = 1, NBASES
         XSTART(I) = 2.D0 * DBLE(I-1)
         XSTOP(I) = XSTART(I) + 0.9D0
      END DO

      CALL PDPOOL ( 'FRAME_1400011_START', NBASES, XSTART )
      CALL PDPOOL ( 'FRAME_1400011_STOP',  NBASES, XSTOP )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Do a frame transformation to populate the switch frame database.
!
      ET = XSTART( NBASES/2 ) + 0.5D0
      CALL SXFORM ( 'SWITCH_MAXBASE', 'J2000', ET, XFORM )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Fetch data for the frame.
!
      FRAMID = 1400011

      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG, &
                      FREE,   BASCNT, USETIM, BINARY, CLSSES, &
                      CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )

      CALL CHCKXC ( .FALSE., ' ', OK )
!
!     This test depends on the previous test cases.
!
      CALL CHCKSI ( 'FREE', FREE, '=', MAXBAS+1, 0, OK )


      CALL CHCKSI ( 'FRAMAT', FRAMAT, '!=', 0, 0, OK )

      IF ( FRAMAT .NE. 0 ) THEN
!
!       This test depends on the previous test cases.
!
         CALL CHCKSI ( 'BASBEG(FRAMAT)', &
                        BASBEG(FRAMAT), '=', 1, 0, OK )

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


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error case: return on entry' )

      SHRTMS = 'SPICE(TESTERROR)'
      CALL SIGERR ( SHRTMS )

      CALL ZZSWFFET ( FRAMID, HDFRAM, FRPOOL, FIDLST, BASBEG, &
                      FREE,   BASCNT, USETIM, BINARY, CLSSES, &
                      CLSIDS, BASLST, STARTS, STOPS,  FRAMAT )

      CALL CHCKXC ( .TRUE., SHRTMS, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Clean up.' )

      CALL KILFIL ( FK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END
