!$Procedure      F_CCIFRM ( Family of tests for CCIFRM )

      SUBROUTINE F_CCIFRM ( OK )

      IMPLICIT NONE

! $ Abstract
!
!     This test family checks out the entry point CCIFRM belonging
!     to the frame expert utility FRAMEX.
!
! $ Required_Reading
!
!   None
!
! $ Keywords
!
!   None
!
! $ Declarations

      INCLUDE              'ninert.inc'
      INCLUDE              'nninrt.inc'
      INCLUDE              'frmtyp.inc'

      LOGICAL               OK

! $ Brief_I/O
!
!   None
!
! $ Detailed_Input
!
!   None
!
! $ Detailed_Output
!
!   None
!
! $ Parameters
!
!   None
!
! $ Exceptions
!
!   None
!
! $ Files
!
!   None
!
! $ Particulars
!
!   None
!
! $ Examples
!
!   None
!
! $ Restrictions
!
!   None
!
! $ Literature_References
!
!   None
!
! $ Author_and_Institution
!
!   None
!
! $ Version
!
!-    TSPICE Version 1.1.0, 09-AUG-2013 (BVS)
!
!        Updated for changed ZZFDAT calling sequence. Changed error
!        cases dealing with missing keywords to work with class 3
!        rather than class 1 frame (to accomodate changed look-up
!        priority with built-in examined first.
!
!-    TSPICE Version 1.0.0, 07-NOV-2007 (NJB)
!
!-&


!
!     Test Utility Functions
!

!
!     SPICELIB Functions
!


!
!     Local Parameters
!
      INTEGER               NON
      PARAMETER           ( NON    = NNINRT )

      INTEGER               NCOUNT
      PARAMETER           ( NCOUNT = NINERT + NON )


      INTEGER               KVNMLN
      PARAMETER           ( KVNMLN = 32 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               TXTSIZ
      PARAMETER           ( TXTSIZ = 1222 )

      INTEGER               NFRAME
      PARAMETER           ( NFRAME = 89 )

      INTEGER               BADSIZ
      PARAMETER           ( BADSIZ = 100 )


!
!     Local Variables
!
      CHARACTER*(LNSIZE)    BADTXT ( BADSIZ )
      CHARACTER*(FRNMLN)    FRNAME
      CHARACTER*(FRNMLN)    XNAME
      CHARACTER*(KVNMLN)    KVNAME
      CHARACTER*(KVNMLN)    NAME   ( NCOUNT )
      CHARACTER*(LNSIZE)    FRTEXT ( TXTSIZ )
      CHARACTER*(LNSIZE)    TITLE

      INTEGER               CENT
      INTEGER               CENTER ( NCOUNT )
      INTEGER               CENTRD ( NCOUNT )
      INTEGER               CLASS
      INTEGER               CLSSID
      INTEGER               FRCODE
      INTEGER               I
      INTEGER               IDCODE ( NCOUNT )
      INTEGER               N
      INTEGER               NVALS
      INTEGER               TYPE   ( NCOUNT )
      INTEGER               TYPEID ( NCOUNT )
      INTEGER               XCENT
      INTEGER               XCODE

      LOGICAL               FOUND

!
!     Built-in frame hashes returned by ZZFDAT.
!
      INTEGER               LBPOOL
      PARAMETER           ( LBPOOL = -5 )

      INTEGER               MAXBFR
      PARAMETER           ( MAXBFR = NCOUNT+1 )

      INTEGER               BNMLST  (          MAXBFR )
      INTEGER               BNMPOL  ( LBPOOL : MAXBFR )
      CHARACTER*(FRNMLN)    BNMNMS  (          MAXBFR )
      INTEGER               BNMIDX  (          MAXBFR )

      INTEGER               BIDLST  (          MAXBFR )
      INTEGER               BIDPOL  ( LBPOOL : MAXBFR )
      INTEGER               BIDIDS  (          MAXBFR )
      INTEGER               BIDIDX  (          MAXBFR )


!
!     Begin every test family with an open call.
!
      CALL TOPEN  ( 'F_CCIFRM' )


      FRTEXT(  1 ) = ' FRAME_CASSINI_LGA1               =    ' &
      //             ' -0.82102000000000000E+05'
      FRTEXT(  2 ) = ' FRAME_-82730_NAME                =    ' &
      //             '''CASSINI_RPWS'''
      FRTEXT(  3 ) = ' TKFRAME_DSS-64_TOPO_UNITS        =    ' &
      //             '''DEGREES'''
      FRTEXT(  4 ) = ' TKFRAME_DSS-17_TOPO_ANGLES       = (  ' &
      //             ' -0.24312649410913031E+03,'
      FRTEXT(  5 ) = '                                       ' &
      //             ' -0.54657823408081398E+02,'
      FRTEXT(  6 ) = '                                       ' &
      //             '  0.18000000000000000E+03 )'
      FRTEXT(  7 ) = ' OBJECT_399025_FRAME              =    ' &
      //             '''DSS-25_TOPO'''
      FRTEXT(  8 ) = ' FRAME_-82763_NAME                =    ' &
      //             '''CASSINI_MIMI_LEMMS2'''
      FRTEXT(  9 ) = ' FRAME_-82764_CENTER              =    ' &
      //             ' -0.82000000000000000E+02'
      FRTEXT( 10 ) = ' TKFRAME_DSS-64_TOPO_AXES         = (  ' &
      //             '  0.30000000000000000E+01,'
      FRTEXT( 11 ) = '                                       ' &
      //             '  0.20000000000000000E+01,'
      FRTEXT( 12 ) = '                                       ' &
      //             '  0.30000000000000000E+01 )'
      FRTEXT( 13 ) = ' FRAME_-82103_CENTER              =    ' &
      //             ' -0.82000000000000000E+02'
      FRTEXT( 14 ) = ' FRAME_CASSINI_RADAR_5            =    ' &
      //             ' -0.82814000000000000E+05'
      FRTEXT( 15 ) = ' OBJECT_399054_FRAME              =    ' &
      //             '''DSS-54_TOPO'''
      FRTEXT( 16 ) = ' TKFRAME_-82378_UNITS             =    ' &
      //             '''DEGREES'''
      FRTEXT( 17 ) = ' FRAME_-82812_CLASS_ID            =    ' &
      //             ' -0.82812000000000000E+05'
      FRTEXT( 18 ) = ' TKFRAME_-82740_SPEC              =    ' &
      //             '''ANGLES'''
      FRTEXT( 19 ) = ' FRAME_CASSINI_MAG_MINUS          =    ' &
      //             ' -0.82351000000000000E+05'
      FRTEXT( 20 ) = ' FRAME_DSS-28_TOPO                =    ' &
      //             '  0.13990280000000000E+07'
      FRTEXT( 21 ) = ' FRAME_CASSINI_LGA2               =    ' &
      //             ' -0.82103000000000000E+05'
      FRTEXT( 22 ) = ' TKFRAME_DSS-54_TOPO_UNITS        =    ' &
      //             '''DEGREES'''
      FRTEXT( 23 ) = ' TKFRAME_DSS-16_TOPO_ANGLES       = (  ' &
      //             ' -0.24312634972224771E+03,'
      FRTEXT( 24 ) = '                                       ' &
      //             ' -0.54658460594124101E+02,'
      FRTEXT( 25 ) = '                                       ' &
      //             '  0.18000000000000000E+03 )'
      FRTEXT( 26 ) = ' TKFRAME_-82370_RELATIVE          =    ' &
      //             '''CASSINI_SC_COORD'''
      FRTEXT( 27 ) = ' FRAME_DSS-64_TOPO                =    ' &
      //             '  0.13990640000000000E+07'
      FRTEXT( 28 ) = ' TKFRAME_31000_MATRIX             = (  ' &
      //             '  0.10000000000000000E+01,'
      FRTEXT( 29 ) = '                                       ' &
      //             '  0.00000000000000000E+00,'
      FRTEXT( 30 ) = '                                       ' &
      //             '  0.00000000000000000E+00,'
      FRTEXT( 31 ) = '                                       ' &
      //             '  0.00000000000000000E+00,'
      FRTEXT( 32 ) = '                                       ' &
      //             '  0.10000000000000000E+01,'
      FRTEXT( 33 ) = '                                       ' &
      //             '  0.00000000000000000E+00,'
      FRTEXT( 34 ) = '                                       ' &
      //             '  0.00000000000000000E+00,'
      FRTEXT( 35 ) = '                                       ' &
      //             '  0.00000000000000000E+00,'
      FRTEXT( 36 ) = '                                       ' &
      //             '  0.10000000000000000E+01 )'
      FRTEXT( 37 ) = ' FRAME_1399053_CENTER             =    ' &
      //             '  0.39905300000000000E+06'
      FRTEXT( 38 ) = ' FRAME_-82898_NAME                =    ' &
      //             '''CASSINI_CIRS_RAD'''
      FRTEXT( 39 ) = ' FRAME_31000_CLASS                =    ' &
      //             '  0.40000000000000000E+01'
      FRTEXT( 40 ) = ' TKFRAME_-82842_SPEC              =    ' &
      //             '''ANGLES'''
      FRTEXT( 41 ) = ' TKFRAME_-82733_RELATIVE          =    ' &
      //             '''CASSINI_SC_COORD'''
      FRTEXT( 42 ) = ' TKFRAME_DSS-33_TOPO_RELATIVE     =    ' &
      //             '''EARTH_FIXED'''
      FRTEXT( 43 ) = ' FRAME_1399033_NAME               =    ' &
      //             '''DSS-33_TOPO'''
      FRTEXT( 44 ) = ' TKFRAME_DSS-15_TOPO_ANGLES       = (  ' &
      //             ' -0.24311280436637821E+03,'
      FRTEXT( 45 ) = '                                       ' &
      //             ' -0.54578146708818899E+02,'
      FRTEXT( 46 ) = '                                       ' &
      //             '  0.18000000000000000E+03 )'
      FRTEXT( 47 ) = ' TKFRAME_-82107_SPEC              =    ' &
      //             '''ANGLES'''
      FRTEXT( 48 ) = ' FRAME_-82102_CLASS_ID            =    ' &
      //             ' -0.82102000000000000E+05'
      FRTEXT( 49 ) = ' FRAME_-82891_CENTER              =    ' &
      //             ' -0.82000000000000000E+02'
      FRTEXT( 50 ) = ' FRAME_1399066_NAME               =    ' &
      //             '''DSS-66_TOPO'''
      FRTEXT( 51 ) = ' FRAME_-82762_CLASS_ID            =    ' &
      //             ' -0.82762000000000000E+05'
      FRTEXT( 52 ) = ' TKFRAME_DSS-24_TOPO_SPEC         =    ' &
      //             '''ANGLES'''
      FRTEXT( 53 ) = ' NAIF_BODY_CODE                   =    ' &
      //             '  0.39906400000000000E+06'
      FRTEXT( 54 ) = ' CK_-82763_SCLK                   =    ' &
      //             ' -0.82000000000000000E+02'
      FRTEXT( 55 ) = ' TKFRAME_-82814_RELATIVE          =    ' &
      //             '''CASSINI_SC_COORD'''
      FRTEXT( 56 ) = ' FRAME_1399061_CLASS_ID           =    ' &
      //             '  0.13990610000000000E+07'
      FRTEXT( 57 ) = ' TKFRAME_DSS-25_TOPO_RELATIVE     =    ' &
      //             '''EARTH_FIXED'''
      FRTEXT( 58 ) = ' FRAME_-82378_CLASS_ID            =    ' &
      //             ' -0.82378000000000000E+05'
      FRTEXT( 59 ) = ' OBJECT_399049_FRAME              =    ' &
      //             '''DSS-49_TOPO'''
      FRTEXT( 60 ) = ' TKFRAME_DSS-34_TOPO_UNITS        =    ' &
      //             '''DEGREES'''
      FRTEXT( 61 ) = ' FRAME_-82843_CLASS_ID            =    ' &
      //             ' -0.82843000000000000E+05'
      FRTEXT( 62 ) = ' TKFRAME_DSS-14_TOPO_ANGLES       = (  ' &
      //             ' -0.24311046126072219E+03,'
      FRTEXT( 63 ) = '                                       ' &
      //             ' -0.54574099118225000E+02,'
      FRTEXT( 64 ) = '                                       ' &
      //             '  0.18000000000000000E+03 )'
      FRTEXT( 65 ) = ' FRAME_-82372_CLASS               =    ' &
      //             '  0.40000000000000000E+01'
      FRTEXT( 66 ) = ' TKFRAME_-82840_RELATIVE          =    ' &
      //             '''CASSINI_SC_COORD'''
      FRTEXT( 67 ) = ' FRAME_1399014_CLASS              =    ' &
      //             '  0.40000000000000000E+01'
      FRTEXT( 68 ) = ' TKFRAME_-82733_AXES              = (  ' &
      //             '  0.30000000000000000E+01,'
      FRTEXT( 69 ) = '                                       ' &
      //             '  0.20000000000000000E+01,'
      FRTEXT( 70 ) = '                                       ' &
      //             '  0.10000000000000000E+01 )'
      FRTEXT( 71 ) = ' FRAME_1399043_CLASS              =    ' &
      //             '  0.40000000000000000E+01'
      FRTEXT( 72 ) = ' TKFRAME_DSS-17_TOPO_RELATIVE     =    ' &
      //             '''EARTH_FIXED'''
      FRTEXT( 73 ) = ' FRAME_31001_NAME                 =    ' &
      //             '''MOON_ME'''
      FRTEXT( 74 ) = ' TKFRAME_DSS-24_TOPO_UNITS        =    ' &
      //             '''DEGREES'''
      FRTEXT( 75 ) = ' FRAME_DSS-14_TOPO                =    ' &
      //             '  0.13990140000000000E+07'
      FRTEXT( 76 ) = ' TKFRAME_-82813_ANGLES            = (  ' &
      //             '  0.18084999999999999E+03,'
      FRTEXT( 77 ) = '                                       ' &
      //             ' -0.12000000000000000E+01,'
      FRTEXT( 78 ) = '                                       ' &
      //             '  0.00000000000000000E+00 )'
      FRTEXT( 79 ) = ' TKFRAME_DSS-13_TOPO_ANGLES       = (  ' &
      //             ' -0.24320554047636159E+03,'
      FRTEXT( 80 ) = '                                       ' &
      //             ' -0.54752835732536603E+02,'
      FRTEXT( 81 ) = '                                       ' &
      //             '  0.18000000000000000E+03 )'
      FRTEXT( 82 ) = ' FRAME_-82107_CLASS_ID            =    ' &
      //             ' -0.82107000000000000E+05'
      FRTEXT( 83 ) = ' CK_-82763_SPK                    =    ' &
      //             ' -0.82000000000000000E+02'
      FRTEXT( 84 ) = ' TKFRAME_DSS-12_TOPO_SPEC         =    ' &
      //             '''ANGLES'''
      FRTEXT( 85 ) = ' FRAME_-82360_CLASS_ID            =    ' &
      //             ' -0.82360000000000000E+05'
      FRTEXT( 86 ) = ' FRAME_-82730_CENTER              =    ' &
      //             ' -0.82000000000000000E+02'
      FRTEXT( 87 ) = ' FRAME_-82008_CLASS               =    ' &
      //             '  0.40000000000000000E+01'
      FRTEXT( 88 ) = ' TKFRAME_-82104_RELATIVE          =    ' &
      //             '''CASSINI_KABAND'''
      FRTEXT( 89 ) = ' TKFRAME_-82792_SPEC              =    ' &
      //             '''ANGLES'''
      FRTEXT( 90 ) = ' TKFRAME_-82009_UNITS             =    ' &
      //             '''DEGREES'''
      FRTEXT( 91 ) = ' TKFRAME_-82001_AXES              = (  ' &
      //             '  0.30000000000000000E+01,'
      FRTEXT( 92 ) = '                                       ' &
      //             '  0.20000000000000000E+01,'
      FRTEXT( 93 ) = '                                       ' &
      //             '  0.10000000000000000E+01 )'
      FRTEXT( 94 ) = ' FRAME_MOON_PA                    =    ' &
      //             '  0.31000000000000000E+05'
      FRTEXT( 95 ) = ' FRAME_1399066_CLASS_ID           =    ' &
      //             '  0.13990660000000000E+07'
      FRTEXT( 96 ) = ' FRAME_-82008_CENTER              =    ' &
      //             ' -0.82000000000000000E+02'
      FRTEXT( 97 ) = ' FRAME_CASSINI_UVIS_EUV           =    ' &
      //             ' -0.82842000000000000E+05'
      FRTEXT( 98 ) = ' FRAME_-82814_CLASS               =    ' &
      //             '  0.40000000000000000E+01'
      FRTEXT( 99 ) = ' FRAME_1399016_NAME               =    ' &
      //             '''DSS-16_TOPO'''
      FRTEXT( 100 ) = ' TKFRAME_31001_RELATIVE           =   ' &
      //              ' ''MOON_ME_DE403'''
      FRTEXT( 101 ) = ' TKFRAME_DSS-14_TOPO_UNITS        =   ' &
      //              ' ''DEGREES'''
      FRTEXT( 102 ) = ' FRAME_1399049_NAME               =   ' &
      //              ' ''DSS-49_TOPO'''
      FRTEXT( 103 ) = ' FRAME_-82840_CENTER              =   ' &
      //              '  -0.82000000000000000E+02'
      FRTEXT( 104 ) = ' TKFRAME_DSS-12_TOPO_ANGLES       = ( ' &
      //              '  -0.24319451024426459E+03,'
      FRTEXT( 105 ) = '                                      ' &
      //              '  -0.54700062904314699E+02,'
      FRTEXT( 106 ) = '                                      ' &
      //              '   0.18000000000000000E+03 )'
      FRTEXT( 107 ) = ' FRAME_-82843_CLASS               =   ' &
      //              '   0.40000000000000000E+01'
      FRTEXT( 108 ) = ' TKFRAME_-82845_RELATIVE          =   ' &
      //              ' ''CASSINI_SC_COORD'''
      FRTEXT( 109 ) = ' TKFRAME_-82844_UNITS             =   ' &
      //              ' ''DEGREES'''
      FRTEXT( 110 ) = ' OBJECT_399015_FRAME              =   ' &
      //              ' ''DSS-15_TOPO'''
      FRTEXT( 111 ) = ' FRAME_-82763_CENTER              =   ' &
      //              '  -0.82000000000000000E+02'
      FRTEXT( 112 ) = ' TKFRAME_-82350_SPEC              =   ' &
      //              ' ''ANGLES'''
      FRTEXT( 113 ) = ' FRAME_-82102_CENTER              =   ' &
      //              '  -0.82000000000000000E+02'
      FRTEXT( 114 ) = ' TKFRAME_-82103_AXES              = ( ' &
      //              '   0.30000000000000000E+01,'
      FRTEXT( 115 ) = '                                      ' &
      //              '   0.20000000000000000E+01,'
      FRTEXT( 116 ) = '                                      ' &
      //              '   0.10000000000000000E+01 )'
      FRTEXT( 117 ) = ' TKFRAME_-82360_AXES              = ( ' &
      //              '   0.10000000000000000E+01,'
      FRTEXT( 118 ) = '                                      ' &
      //              '   0.20000000000000000E+01,'
      FRTEXT( 119 ) = '                                      ' &
      //              '   0.30000000000000000E+01 )'
      FRTEXT( 120 ) = ' TKFRAME_-82108_ANGLES            = ( ' &
      //              '  -0.10982989689352000E+03,'
      FRTEXT( 121 ) = '                                      ' &
      //              '  -0.17997052693372001E+03,'
      FRTEXT( 122 ) = '                                      ' &
      //              '   0.00000000000000000E+00 )'
      FRTEXT( 123 ) = ' TKFRAME_-82368_UNITS             =   ' &
      //              ' ''DEGREES'''
      FRTEXT( 124 ) = ' TKFRAME_DSS-66_TOPO_SPEC         =   ' &
      //              ' ''ANGLES'''
      FRTEXT( 125 ) = ' FRAME_-82105_CLASS               =   ' &
      //              '   0.40000000000000000E+01'
      FRTEXT( 126 ) = ' FRAME_-82732_NAME                =   ' &
      //              ' ''CASSINI_RPWS_EXMINUS'''
      FRTEXT( 127 ) = ' TKFRAME_-82106_UNITS             =   ' &
      //              ' ''DEGREES'''
      FRTEXT( 128 ) = ' FRAME_CASSINI_RPWS_EZPLUS        =   ' &
      //              '  -0.82733000000000000E+05'
      FRTEXT( 129 ) = ' FRAME_-82765_NAME                =   ' &
      //              ' ''CASSINI_MIMI_LEMMS_BASE'''
      FRTEXT( 130 ) = ' FRAME_31000_CLASS_ID             =   ' &
      //              '   0.31000000000000000E+05'
      FRTEXT( 131 ) = ' FRAME_CASSINI_ISS_NAC_RAD        =   ' &
      //              '  -0.82368000000000000E+05'
      FRTEXT( 132 ) = ' FRAME_-82000_NAME                =   ' &
      //              ' ''CASSINI_SC_COORD'''
      FRTEXT( 133 ) = ' FRAME_CASSINI_SC_COORD           =   ' &
      //              '  -0.82000000000000000E+05'
      FRTEXT( 134 ) = ' FRAME_1399027_CLASS_ID           =   ' &
      //              '   0.13990270000000000E+07'
      FRTEXT( 135 ) = ' FRAME_-82890_CENTER              =   ' &
      //              '  -0.82000000000000000E+02'
      FRTEXT( 136 ) = ' TKFRAME_-82810_UNITS             =   ' &
      //              ' ''DEGREES'''
      FRTEXT( 137 ) = ' TKFRAME_DSS-23_TOPO_AXES         = ( ' &
      //              '   0.30000000000000000E+01,'
      FRTEXT( 138 ) = '                                      ' &
      //              '   0.20000000000000000E+01,'
      FRTEXT( 139 ) = '                                      ' &
      //              '   0.30000000000000000E+01 )'
      FRTEXT( 140 ) = ' FRAME_DSS-33_TOPO                =   ' &
      //              '   0.13990330000000000E+07'
      FRTEXT( 141 ) = ' TKFRAME_PARKES_TOPO_UNITS        =   ' &
      //              ' ''DEGREES'''
      FRTEXT( 142 ) = ' FRAME_DSS-66_TOPO                =   ' &
      //              '   0.13990660000000000E+07'
      FRTEXT( 143 ) = ' TKFRAME_-82811_SPEC              =   ' &
      //              ' ''ANGLES'''
      FRTEXT( 144 ) = ' FRAME_1399053_CLASS_ID           =   ' &
      //              '   0.13990530000000000E+07'
      FRTEXT( 145 ) = ' TKFRAME_DSS-54_TOPO_SPEC         =   ' &
      //              ' ''ANGLES'''
      FRTEXT( 146 ) = ' TKFRAME_-82844_SPEC              =   ' &
      //              ' ''ANGLES'''
      FRTEXT( 147 ) = ' TKFRAME_DSS-28_TOPO_ANGLES       = ( ' &
      //              '  -0.24322110829953050E+03,'
      FRTEXT( 148 ) = '                                      ' &
      //              '  -0.54761727965969001E+02,'
      FRTEXT( 149 ) = '                                      ' &
      //              '   0.18000000000000000E+03 )'
      FRTEXT( 150 ) = ' FRAME_MOON_PA_DE403              =   ' &
      //              '   0.31002000000000000E+05'
      FRTEXT( 151 ) = ' FRAME_-82102_NAME                =   ' &
      //              ' ''CASSINI_LGA1'''
      FRTEXT( 152 ) = ' FRAME_-82765_CLASS               =   ' &
      //              '   0.40000000000000000E+01'
      FRTEXT( 153 ) = ' TKFRAME_DSS-24_TOPO_RELATIVE     =   ' &
      //              ' ''EARTH_FIXED'''
      FRTEXT( 154 ) = ' TKFRAME_-82890_AXES              = ( ' &
      //              '   0.30000000000000000E+01,'
      FRTEXT( 155 ) = '                                      ' &
      //              '   0.10000000000000000E+01,'
      FRTEXT( 156 ) = '                                      ' &
      //              '   0.20000000000000000E+01 )'
      FRTEXT( 157 ) = ' FRAME_1399033_CLASS              =   ' &
      //              '   0.40000000000000000E+01'
      FRTEXT( 158 ) = ' TKFRAME_-82101_UNITS             =   ' &
      //              ' ''DEGREES'''
      FRTEXT( 159 ) = ' TKFRAME_DSS-27_TOPO_ANGLES       = ( ' &
      //              '  -0.24322334902235940E+03,'
      FRTEXT( 160 ) = '                                      ' &
      //              '  -0.54761728205681997E+02,'
      FRTEXT( 161 ) = '                                      ' &
      //              '   0.18000000000000000E+03 )'
      FRTEXT( 162 ) = ' FRAME_-82822_CENTER              =   ' &
      //              '  -0.82000000000000000E+02'
      FRTEXT( 163 ) = ' TKFRAME_31003_UNITS              =   ' &
      //              ' ''ARCSECONDS'''
      FRTEXT( 164 ) = ' TKFRAME_-82812_ANGLES            = ( ' &
      //              '   0.18000000000000000E+03,'
      FRTEXT( 165 ) = '                                      ' &
      //              '   0.00000000000000000E+00,'
      FRTEXT( 166 ) = '                                      ' &
      //              '   0.00000000000000000E+00 )'
      FRTEXT( 167 ) = ' FRAME_-82372_CENTER              =   ' &
      //              '  -0.82000000000000000E+02'
      FRTEXT( 168 ) = ' CK_-82000_SCLK                   =   ' &
      //              '  -0.82000000000000000E+02'
      FRTEXT( 169 ) = ' FRAME_1399017_CENTER             =   ' &
      //              '   0.39901700000000000E+06'
      FRTEXT( 170 ) = ' TKFRAME_DSS-27_TOPO_UNITS        =   ' &
      //              ' ''DEGREES'''
      FRTEXT( 171 ) = ' TKFRAME_DSS-49_TOPO_SPEC         =   ' &
      //              ' ''ANGLES'''
      FRTEXT( 172 ) = ' TKFRAME_-82378_ANGLES            = ( ' &
      //              '  -0.85500000000000000E+02,'
      FRTEXT( 173 ) = '                                      ' &
      //              '  -0.90000000000000000E+02,'
      FRTEXT( 174 ) = '                                      ' &
      //              '   0.00000000000000000E+00 )'
      FRTEXT( 175 ) = ' TKFRAME_DSS-16_TOPO_RELATIVE     =   ' &
      //              ' ''EARTH_FIXED'''
      FRTEXT( 176 ) = ' FRAME_-82000_CLASS_ID            =   ' &
      //              '  -0.82000000000000000E+05'
      FRTEXT( 177 ) = ' TKFRAME_DSS-42_TOPO_SPEC         =   ' &
      //              ' ''ANGLES'''
      FRTEXT( 178 ) = ' TKFRAME_DSS-26_TOPO_ANGLES       = ( ' &
      //              '  -0.24312698297645241E+03,'
      FRTEXT( 179 ) = '                                      ' &
      //              '  -0.54664310768860098E+02,'
      FRTEXT( 180 ) = '                                      ' &
      //              '   0.18000000000000000E+03 )'
      FRTEXT( 181 ) = ' FRAME_31003_NAME                 =   ' &
      //              ' ''MOON_ME_DE403'''
      FRTEXT( 182 ) = ' FRAME_DSS-16_TOPO                =   ' &
      //              '   0.13990160000000000E+07'
      FRTEXT( 183 ) = ' TKFRAME_-82761_SPEC              =   ' &
      //              ' ''ANGLES'''
      FRTEXT( 184 ) = ' TKFRAME_-82845_ANGLES            = ( ' &
      //              '   0.00000000000000000E+00,'
      FRTEXT( 185 ) = '                                      ' &
      //              '   0.00000000000000000E+00,'
      FRTEXT( 186 ) = '                                      ' &
      //              '  -0.90000000000000000E+02 )'
      FRTEXT( 187 ) = ' OBJECT_399005_FRAME              =   ' &
      //              ' ''PARKES_TOPO'''
      FRTEXT( 188 ) = ' FRAME_DSS-49_TOPO                =   ' &
      //              '   0.13990490000000000E+07'
      FRTEXT( 189 ) = ' FRAME_-82762_CENTER              =   ' &
      //              '  -0.82000000000000000E+02'
      FRTEXT( 190 ) = ' FRAME_1399014_CLASS_ID           =   ' &
      //              '   0.13990140000000000E+07'
      FRTEXT( 191 ) = ' TKFRAME_DSS-17_TOPO_UNITS        =   ' &
      //              ' ''DEGREES'''
      FRTEXT( 192 ) = ' FRAME_-82820_NAME                =   ' &
      //              ' ''CASSINI_CAPS'''
      FRTEXT( 193 ) = ' FRAME_-82101_CENTER              =   ' &
      //              '  -0.82000000000000000E+02'
      FRTEXT( 194 ) = ' FRAME_1399034_CENTER             =   ' &
      //              '   0.39903400000000000E+06'
      FRTEXT( 195 ) = ' FRAME_CASSINI_CIRS_RAD           =   ' &
      //              '  -0.82898000000000000E+05'
      FRTEXT( 196 ) = ' OBJECT_399034_FRAME              =   ' &
      //              ' ''DSS-34_TOPO'''
      FRTEXT( 197 ) = ' TKFRAME_-82107_ANGLES            = ( ' &
      //              '   0.00000000000000000E+00,'
      FRTEXT( 198 ) = '                                      ' &
      //              '   0.00000000000000000E+00,'
      FRTEXT( 199 ) = '                                      ' &
      //              '   0.18000000000000000E+03 )'
      FRTEXT( 200 ) = ' FRAME_-82731_CLASS               =   ' &
      //              '   0.40000000000000000E+01'
      FRTEXT( 201 ) = ' TKFRAME_-82732_UNITS             =   ' &
      //              ' ''DEGREES'''
      FRTEXT( 202 ) = ' FRAME_-82891_CLASS               =   ' &
      //              '   0.40000000000000000E+01'
      FRTEXT( 203 ) = ' TKFRAME_-82892_UNITS             =   ' &
      //              ' ''DEGREES'''
      FRTEXT( 204 ) = ' TKFRAME_DSS-25_TOPO_ANGLES       = ( ' &
      //              '  -0.24312463626565020E+03,'
      FRTEXT( 205 ) = '                                      ' &
      //              '  -0.54662388023518702E+02,'
      FRTEXT( 206 ) = '                                      ' &
      //              '   0.18000000000000000E+03 )'
      FRTEXT( 207 ) = ' OBJECT_399063_FRAME              =   ' &
      //              ' ''DSS-63_TOPO'''
      FRTEXT( 208 ) = ' FRAME_-82760_CLASS               =   ' &
      //              '   0.40000000000000000E+01'
      FRTEXT( 209 ) = ' TKFRAME_DSS-65_TOPO_AXES         = ( ' &
      //              '   0.30000000000000000E+01,'
      FRTEXT( 210 ) = '                                      ' &
      //              '   0.20000000000000000E+01,'
      FRTEXT( 211 ) = '                                      ' &
      //              '   0.30000000000000000E+01 )'
      FRTEXT( 212 ) = ' TKFRAME_-82761_UNITS             =   ' &
      //              ' ''DEGREES'''
      FRTEXT( 213 ) = ' FRAME_-82892_CLASS_ID            =   ' &
      //              '  -0.82892000000000000E+05'
      FRTEXT( 214 ) = ' FRAME_1399028_CLASS              =   ' &
      //              '   0.40000000000000000E+01'
      FRTEXT( 215 ) = ' TKFRAME_-82840_AXES              = ( ' &
      //              '   0.10000000000000000E+01,'
      FRTEXT( 216 ) = '                                      ' &
      //              '   0.20000000000000000E+01,'
      FRTEXT( 217 ) = '                                      ' &
      //              '   0.10000000000000000E+01 )'
      FRTEXT( 218 ) = ' FRAME_-82822_CLASS_ID            =   ' &
      //              '  -0.82822000000000000E+05'
      FRTEXT( 219 ) = ' FRAME_CASSINI_VIMS_RAD           =   ' &
      //              '  -0.82378000000000000E+05'
      FRTEXT( 220 ) = ' FRAME_1399054_NAME               =   ' &
      //              ' ''DSS-54_TOPO'''
      FRTEXT( 221 ) = ' TKFRAME_-82002_RELATIVE          =   ' &
      //              ' ''CASSINI_SC_COORD'''
      FRTEXT( 222 ) = ' TKFRAME_-82105_AXES              = ( ' &
      //              '   0.30000000000000000E+01,'
      FRTEXT( 223 ) = '                                      ' &
      //              '   0.10000000000000000E+01,'
      FRTEXT( 224 ) = '                                      ' &
      //              '   0.30000000000000000E+01 )'
      FRTEXT( 225 ) = ' TKFRAME_DSS-24_TOPO_ANGLES       = ( ' &
      //              '  -0.24312520504708351E+03,'
      FRTEXT( 226 ) = '                                      ' &
      //              '  -0.54660107163981301E+02,'
      FRTEXT( 227 ) = '                                      ' &
      //              '   0.18000000000000000E+03 )'
      FRTEXT( 228 ) = ' FRAME_-82734_NAME                =   ' &
      //              ' ''CASSINI_RPWS_LP'''
      FRTEXT( 229 ) = ' TKFRAME_DSS-23_TOPO_ANGLES       = ( ' &
      //              '  -0.24312713644946820E+03,'
      FRTEXT( 230 ) = '                                      ' &
      //              '  -0.54660449632925499E+02,'
      FRTEXT( 231 ) = '                                      ' &
      //              '   0.18000000000000000E+03 )'
      FRTEXT( 232 ) = ' CK_-82820_SCLK                   =   ' &
      //              '  -0.82000000000000000E+02'
      FRTEXT( 233 ) = ' FRAME_1399045_CLASS_ID           =   ' &
      //              '   0.13990450000000000E+07'
      FRTEXT( 234 ) = ' TKFRAME_DSS-53_TOPO_AXES         = ( ' &
      //              '   0.30000000000000000E+01,'
      FRTEXT( 235 ) = '                                      ' &
      //              '   0.20000000000000000E+01,'
      FRTEXT( 236 ) = '                                      ' &
      //              '   0.30000000000000000E+01 )'
      FRTEXT( 237 ) = ' TKFRAME_DSS-25_TOPO_SPEC         =   ' &
      //              ' ''ANGLES'''
      FRTEXT( 238 ) = ' FRAME_-82002_NAME                =   ' &
      //              ' ''CASSINI_SRU-B'''
      FRTEXT( 239 ) = ' TKFRAME_-82009_SPEC              =   ' &
      //              ' ''ANGLES'''
      FRTEXT( 240 ) = ' FRAME_1399023_CLASS              =   ' &
      //              '   0.40000000000000000E+01'
      FRTEXT( 241 ) = ' TKFRAME_-82813_SPEC              =   ' &
      //              ' ''ANGLES'''
      FRTEXT( 242 ) = ' FRAME_-82821_CENTER              =   ' &
      //              '  -0.82000000000000000E+02'
      FRTEXT( 243 ) = ' TKFRAME_-82811_ANGLES            = ( ' &
      //              '   0.17915000000000001E+03,'
      FRTEXT( 244 ) = '                                      ' &
      //              '  -0.12000000000000000E+01,'
      FRTEXT( 245 ) = '                                      ' &
      //              '   0.00000000000000000E+00 )'
      FRTEXT( 246 ) = ' TKFRAME_DSS-23_TOPO_RELATIVE     =   ' &
      //              ' ''EARTH_FIXED'''
      FRTEXT( 247 ) = ' FRAME_-82104_NAME                =   ' &
      //              ' ''CASSINI_XBAND'''
      FRTEXT( 248 ) = ' FRAME_-82361_NAME                =   ' &
      //              ' ''CASSINI_ISS_WAC'''
      FRTEXT( 249 ) = ' FRAME_-82371_CENTER              =   ' &
      //              '  -0.82000000000000000E+02'
      FRTEXT( 250 ) = ' TKFRAME_-82734_ANGLES            = ( ' &
      //              '   0.18000000000000000E+03,'
      FRTEXT( 251 ) = '                                      ' &
      //              '  -0.90000000000000000E+02,'
      FRTEXT( 252 ) = '                                      ' &
      //              '   0.00000000000000000E+00 )'
      FRTEXT( 253 ) = ' FRAME_1399016_CENTER             =   ' &
      //              '   0.39901600000000000E+06'
      FRTEXT( 254 ) = ' TKFRAME_-82361_ANGLES            = ( ' &
      //              '  -0.89985421000000002E+02,'
      FRTEXT( 255 ) = '                                      ' &
      //              '  -0.69806000000000007E-01,'
      FRTEXT( 256 ) = '                                      ' &
      //              '   0.89973600000000005E+02 )'
      FRTEXT( 257 ) = ' TKFRAME_-82368_SPEC              =   ' &
      //              ' ''ANGLES'''
      FRTEXT( 258 ) = ' TKFRAME_-82378_AXES              = ( ' &
      //              '   0.30000000000000000E+01,'
      FRTEXT( 259 ) = '                                      ' &
      //              '   0.10000000000000000E+01,'
      FRTEXT( 260 ) = '                                      ' &
      //              '   0.30000000000000000E+01 )'
      FRTEXT( 261 ) = ' TKFRAME_DSS-13_TOPO_SPEC         =   ' &
      //              ' ''ANGLES'''
      FRTEXT( 262 ) = ' TKFRAME_-82892_AXES              = ( ' &
      //              '   0.30000000000000000E+01,'
      FRTEXT( 263 ) = '                                      ' &
      //              '   0.10000000000000000E+01,'
      FRTEXT( 264 ) = '                                      ' &
      //              '   0.20000000000000000E+01 )'
      FRTEXT( 265 ) = ' FRAME_-82370_CLASS_ID            =   ' &
      //              '  -0.82370000000000000E+05'
      FRTEXT( 266 ) = ' TKFRAME_-82371_SPEC              =   ' &
      //              ' ''ANGLES'''
      FRTEXT( 267 ) = ' FRAME_CASSINI_XBAND              =   ' &
      //              '  -0.82104000000000000E+05'
      FRTEXT( 268 ) = ' TKFRAME_DSS-15_TOPO_RELATIVE     =   ' &
      //              ' ''EARTH_FIXED'''
      FRTEXT( 269 ) = ' TKFRAME_-82844_ANGLES            = ( ' &
      //              '  -0.90000000000000000E+02,'
      FRTEXT( 270 ) = '                                      ' &
      //              '   0.17993125072403001E+03,'
      FRTEXT( 271 ) = '                                      ' &
      //              '  -0.17993125077351999E+03 )'
      FRTEXT( 272 ) = ' FRAME_-82761_CENTER              =   ' &
      //              '  -0.82000000000000000E+02'
      FRTEXT( 273 ) = ' FRAME_-82733_CLASS_ID            =   ' &
      //              '  -0.82733000000000000E+05'
      FRTEXT( 274 ) = ' FRAME_1399033_CENTER             =   ' &
      //              '   0.39903300000000000E+06'
      FRTEXT( 275 ) = ' FRAME_1399049_CENTER             =   ' &
      //              '   0.39904900000000000E+06'
      FRTEXT( 276 ) = ' OBJECT_399024_FRAME              =   ' &
      //              ' ''DSS-24_TOPO'''
      FRTEXT( 277 ) = ' TKFRAME_-82730_RELATIVE          =   ' &
      //              ' ''CASSINI_SC_COORD'''
      FRTEXT( 278 ) = ' TKFRAME_EARTH_FIXED_RELATIVE     =   ' &
      //              ' ''ITRF93'''
      FRTEXT( 279 ) = ' TKFRAME_-82106_ANGLES            = ( ' &
      //              '   0.00000000000000000E+00,'
      FRTEXT( 280 ) = '                                      ' &
      //              '   0.00000000000000000E+00,'
      FRTEXT( 281 ) = '                                      ' &
      //              '   0.18000000000000000E+03 )'
      FRTEXT( 282 ) = ' OBJECT_399053_FRAME              =   ' &
      //              ' ''DSS-53_TOPO'''
      FRTEXT( 283 ) = ' TKFRAME_-82730_SPEC              =   ' &
      //              ' ''ANGLES'''
      FRTEXT( 284 ) = ' TKFRAME_EARTH_FIXED_SPEC         =   ' &
      //              ' ''MATRIX'''
      FRTEXT( 285 ) = ' FRAME_CASSINI_CAPS_ART           =   ' &
      //              '  -0.82821000000000000E+05'
      FRTEXT( 286 ) = ' CK_-82000_SPK                    =   ' &
      //              '  -0.82000000000000000E+02'
      FRTEXT( 287 ) = ' TKFRAME_-82740_AXES              = ( ' &
      //              '   0.10000000000000000E+01,'
      FRTEXT( 288 ) = '                                      ' &
      //              '   0.20000000000000000E+01,'
      FRTEXT( 289 ) = '                                      ' &
      //              '   0.30000000000000000E+01 )'
      FRTEXT( 290 ) = ' FRAME_-82814_CLASS_ID            =   ' &
      //              '  -0.82814000000000000E+05'
      FRTEXT( 291 ) = ' TKFRAME_-82811_RELATIVE          =   ' &
      //              ' ''CASSINI_SC_COORD'''
      FRTEXT( 292 ) = ' FRAME_-82822_NAME                =   ' &
      //              ' ''CASSINI_CAPS_BASE'''
      FRTEXT( 293 ) = ' FRAME_1399066_CENTER             =   ' &
      //              '   0.39906600000000000E+06'
      FRTEXT( 294 ) = ' TKFRAME_PARKES_TOPO_SPEC         =   ' &
      //              ' ''ANGLES'''
      FRTEXT( 295 ) = ' TKFRAME_-82372_RELATIVE          =   ' &
      //              ' ''CASSINI_SC_COORD'''
      FRTEXT( 296 ) = ' FRAME_-82840_CLASS_ID            =   ' &
      //              '  -0.82840000000000000E+05'
      FRTEXT( 297 ) = ' FRAME_DSS-54_TOPO                =   ' &
      //              '   0.13990540000000000E+07'
      FRTEXT( 298 ) = ' FRAME_CASSINI_ISS_WAC_RAD        =   ' &
      //              '  -0.82369000000000000E+05'
      FRTEXT( 299 ) = ' TKFRAME_-82842_AXES              = ( ' &
      //              '   0.10000000000000000E+01,'
      FRTEXT( 300 ) = '                                      ' &
      //              '   0.20000000000000000E+01,'
      FRTEXT( 301 ) = '                                      ' &
      //              '   0.10000000000000000E+01 )'
      FRTEXT( 302 ) = ' FRAME_-82891_NAME                =   ' &
      //              ' ''CASSINI_CIRS_FP3'''
      FRTEXT( 303 ) = ' FRAME_1399023_NAME               =   ' &
      //              ' ''DSS-23_TOPO'''
      FRTEXT( 304 ) = ' TKFRAME_-82898_SPEC              =   ' &
      //              ' ''ANGLES'''
      FRTEXT( 305 ) = ' TKFRAME_-82107_AXES              = ( ' &
      //              '   0.30000000000000000E+01,'
      FRTEXT( 306 ) = '                                      ' &
      //              '   0.20000000000000000E+01,'
      FRTEXT( 307 ) = '                                      ' &
      //              '   0.10000000000000000E+01 )'
      FRTEXT( 308 ) = ' FRAME_-82104_CLASS_ID            =   ' &
      //              '  -0.82104000000000000E+05'
      FRTEXT( 309 ) = ' FRAME_-82764_CLASS_ID            =   ' &
      //              '  -0.82764000000000000E+05'
      FRTEXT( 310 ) = ' TKFRAME_-82101_RELATIVE          =   ' &
      //              ' ''CASSINI_SC_COORD'''
      FRTEXT( 311 ) = ' TKFRAME_DSS-24_TOPO_AXES         = ( ' &
      //              '   0.30000000000000000E+01,'
      FRTEXT( 312 ) = '                                      ' &
      //              '   0.20000000000000000E+01,'
      FRTEXT( 313 ) = '                                      ' &
      //              '   0.30000000000000000E+01 )'
      FRTEXT( 314 ) = ' TKFRAME_-82761_RELATIVE          =   ' &
      //              ' ''CASSINI_SC_COORD'''
      FRTEXT( 315 ) = ' FRAME_-82369_CENTER              =   ' &
      //              '  -0.82000000000000000E+02'
      FRTEXT( 316 ) = ' FRAME_CASSINI_UVIS_SOLAR         =   ' &
      //              '  -0.82843000000000000E+05'
      FRTEXT( 317 ) = ' FRAME_1399063_CLASS_ID           =   ' &
      //              '   0.13990630000000000E+07'
      FRTEXT( 318 ) = ' TKFRAME_DSS-55_TOPO_SPEC         =   ' &
      //              ' ''ANGLES'''
      FRTEXT( 319 ) = ' FRAME_-82790_CLASS_ID            =   ' &
      //              '  -0.82790000000000000E+05'
      FRTEXT( 320 ) = ' FRAME_-82371_CLASS               =   ' &
      //              '   0.40000000000000000E+01'
      FRTEXT( 321 ) = ' TKFRAME_-82372_UNITS             =   ' &
      //              ' ''DEGREES'''
      FRTEXT( 322 ) = ' FRAME_-82845_CLASS_ID            =   ' &
      //              '  -0.82845000000000000E+05'
      FRTEXT( 323 ) = ' FRAME_1399013_CLASS              =   ' &
      //              '   0.40000000000000000E+01'
      FRTEXT( 324 ) = ' TKFRAME_-82842_RELATIVE          =   ' &
      //              ' ''CASSINI_SC_COORD'''
      FRTEXT( 325 ) = ' FRAME_-82820_CENTER              =   ' &
      //              '  -0.82000000000000000E+02'
      FRTEXT( 326 ) = ' TKFRAME_-82810_ANGLES            = ( ' &
      //              '   0.17780000000000001E+03,'
      FRTEXT( 327 ) = '                                      ' &
      //              '   0.00000000000000000E+00,'
      FRTEXT( 328 ) = '                                      ' &
      //              '   0.00000000000000000E+00 )'
      FRTEXT( 329 ) = ' FRAME_1399042_CLASS              =   ' &
      //              '   0.40000000000000000E+01'
      FRTEXT( 330 ) = ' FRAME_CASSINI_MIMI_INCA          =   ' &
      //              '  -0.82761000000000000E+05'
      FRTEXT( 331 ) = ' FRAME_-82370_CENTER              =   ' &
      //              '  -0.82000000000000000E+02'
      FRTEXT( 332 ) = ' TKFRAME_-82733_ANGLES            = ( ' &
      //              '  -0.91200000000000003E+02,'
      FRTEXT( 333 ) = '                                      ' &
      //              '  -0.31399999999999999E+02,'
      FRTEXT( 334 ) = '                                      ' &
      //              '   0.00000000000000000E+00 )'
      FRTEXT( 335 ) = ' FRAME_1399015_CENTER             =   ' &
      //              '   0.39901500000000000E+06'
      FRTEXT( 336 ) = ' TKFRAME_-82360_ANGLES            = ( ' &
      //              '  -0.90009795999999994E+02,'
      FRTEXT( 337 ) = '                                      ' &
      //              '  -0.33000000000000002E-01,'
      FRTEXT( 338 ) = '                                      ' &
      //              '   0.89914800000000000E+02 )'
      FRTEXT( 339 ) = ' TKFRAME_-82008_UNITS             =   ' &
      //              ' ''DEGREES'''
      FRTEXT( 340 ) = ' FRAME_CASSINI_INMS               =   ' &
      //              '  -0.82740000000000000E+05'
      FRTEXT( 341 ) = ' TKFRAME_DSS-12_TOPO_AXES         = ( ' &
      //              '   0.30000000000000000E+01,'
      FRTEXT( 342 ) = '                                      ' &
      //              '   0.20000000000000000E+01,'
      FRTEXT( 343 ) = '                                      ' &
      //              '   0.30000000000000000E+01 )'
      FRTEXT( 344 ) = ' FRAME_CASSINI_SRU-B_RAD          =   ' &
      //              '  -0.82009000000000000E+05'
      FRTEXT( 345 ) = ' TKFRAME_-82792_AXES              = ( ' &
      //              '   0.30000000000000000E+01,'
      FRTEXT( 346 ) = '                                      ' &
      //              '   0.20000000000000000E+01,'
      FRTEXT( 347 ) = '                                      ' &
      //              '   0.10000000000000000E+01 )'
      FRTEXT( 348 ) = ' TKFRAME_-82106_RELATIVE          =   ' &
      //              ' ''CASSINI_SC_COORD'''
      FRTEXT( 349 ) = ' FRAME_-82813_CLASS               =   ' &
      //              '   0.40000000000000000E+01'
      FRTEXT( 350 ) = ' TKFRAME_-82814_UNITS             =   ' &
      //              ' ''DEGREES'''
      FRTEXT( 351 ) = ' TKFRAME_DSS-43_TOPO_SPEC         =   ' &
      //              ' ''ANGLES'''
      FRTEXT( 352 ) = ' TKFRAME_-82843_ANGLES            = ( ' &
      //              '   0.00000000000000000E+00,'
      FRTEXT( 353 ) = '                                      ' &
      //              '   0.00000000000000000E+00,'
      FRTEXT( 354 ) = '                                      ' &
      //              '  -0.11000000000000000E+03 )'
      FRTEXT( 355 ) = ' FRAME_-82106_NAME                =   ' &
      //              ' ''CASSINI_KUBAND'''
      FRTEXT( 356 ) = ' FRAME_-82760_CENTER              =   ' &
      //              '  -0.82000000000000000E+02'
      FRTEXT( 357 ) = ' TKFRAME_DSS-34_TOPO_ANGLES       = ( ' &
      //              '  -0.14898196500211100E+03,'
      FRTEXT( 358 ) = '                                      ' &
      //              '  -0.12539847787565520E+03,'
      FRTEXT( 359 ) = '                                      ' &
      //              '   0.18000000000000000E+03 )'
      FRTEXT( 360 ) = ' TKFRAME_31003_RELATIVE           =   ' &
      //              ' ''MOON_PA_DE403'''
      FRTEXT( 361 ) = ' TKFRAME_-82792_RELATIVE          =   ' &
      //              ' ''CASSINI_SC_COORD'''
      FRTEXT( 362 ) = ' FRAME_-82842_CLASS               =   ' &
      //              '   0.40000000000000000E+01'
      FRTEXT( 363 ) = ' TKFRAME_-82843_UNITS             =   ' &
      //              ' ''DEGREES'''
      FRTEXT( 364 ) = ' OBJECT_399014_FRAME              =   ' &
      //              ' ''DSS-14_TOPO'''
      FRTEXT( 365 ) = ' TKFRAME_-82105_ANGLES            = ( ' &
      //              '  -0.13256335175319001E+03,'
      FRTEXT( 366 ) = '                                      ' &
      //              '  -0.17996188215321999E+03,'
      FRTEXT( 367 ) = '                                      ' &
      //              '   0.00000000000000000E+00 )'
      FRTEXT( 368 ) = ' TKFRAME_DSS-14_TOPO_RELATIVE     =   ' &
      //              ' ''EARTH_FIXED'''
      FRTEXT( 369 ) = ' FRAME_1399042_NAME               =   ' &
      //              ' ''DSS-42_TOPO'''
      FRTEXT( 370 ) = ' OBJECT_399043_FRAME              =   ' &
      //              ' ''DSS-43_TOPO'''
      FRTEXT( 371 ) = ' TKFRAME_-82350_AXES              = ( ' &
      //              '   0.10000000000000000E+01,'
      FRTEXT( 372 ) = '                                      ' &
      //              '   0.20000000000000000E+01,'
      FRTEXT( 373 ) = '                                      ' &
      //              '   0.30000000000000000E+01 )'
      FRTEXT( 374 ) = ' FRAME_1399024_CLASS_ID           =   ' &
      //              '   0.13990240000000000E+07'
      FRTEXT( 375 ) = ' FRAME_-82740_CLASS               =   ' &
      //              '   0.40000000000000000E+01'
      FRTEXT( 376 ) = ' TKFRAME_DSS-33_TOPO_ANGLES       = ( ' &
      //              '  -0.14898309235951129E+03,'
      FRTEXT( 377 ) = '                                      ' &
      //              '  -0.12540048377056090E+03,'
      FRTEXT( 378 ) = '                                      ' &
      //              '   0.18000000000000000E+03 )'
      FRTEXT( 379 ) = ' FRAME_-82104_CLASS               =   ' &
      //              '   0.40000000000000000E+01'
      FRTEXT( 380 ) = ' TKFRAME_-82105_UNITS             =   ' &
      //              ' ''DEGREES'''
      FRTEXT( 381 ) = ' TKFRAME_DSS-66_TOPO_AXES         = ( ' &
      //              '   0.30000000000000000E+01,'
      FRTEXT( 382 ) = '                                      ' &
      //              '   0.20000000000000000E+01,'
      FRTEXT( 383 ) = '                                      ' &
      //              '   0.30000000000000000E+01 )'
      FRTEXT( 384 ) = ' FRAME_1399065_CENTER             =   ' &
      //              '   0.39906500000000000E+06'
      FRTEXT( 385 ) = ' NAIF_BODY_NAME                   =   ' &
      //              ' ''DSS-64'''
      FRTEXT( 386 ) = ' FRAME_1399066_CLASS              =   ' &
      //              '   0.40000000000000000E+01'
      FRTEXT( 387 ) = ' FRAME_31002_CLASS_ID             =   ' &
      //              '   0.31002000000000000E+05'
      FRTEXT( 388 ) = ' FRAME_-82002_CLASS               =   ' &
      //              '   0.40000000000000000E+01'
      FRTEXT( 389 ) = ' TKFRAME_-82732_SPEC              =   ' &
      //              ' ''ANGLES'''
      FRTEXT( 390 ) = ' TKFRAME_-82765_SPEC              =   ' &
      //              ' ''ANGLES'''
      FRTEXT( 391 ) = ' FRAME_-82791_NAME                =   ' &
      //              ' ''CASSINI_CDA_ART'''
      FRTEXT( 392 ) = ' TKFRAME_-82893_ANGLES            = ( ' &
      //              '   0.00000000000000000E+00,'
      FRTEXT( 393 ) = '                                      ' &
      //              '  -0.90002291831180528E+02,'
      FRTEXT( 394 ) = '                                      ' &
      //              '  -0.97402825172239985E-01 )'
      FRTEXT( 395 ) = ' TKFRAME_DSS-66_TOPO_RELATIVE     =   ' &
      //              ' ''EARTH_FIXED'''
      FRTEXT( 396 ) = ' FRAME_DSS-23_TOPO                =   ' &
      //              '   0.13990230000000000E+07'
      FRTEXT( 397 ) = ' TKFRAME_DSS-63_TOPO_UNITS        =   ' &
      //              ' ''DEGREES'''
      FRTEXT( 398 ) = ' TKFRAME_-82811_AXES              = ( ' &
      //              '   0.10000000000000000E+01,'
      FRTEXT( 399 ) = '                                      ' &
      //              '   0.20000000000000000E+01,'
      FRTEXT( 400 ) = '                                      ' &
      //              '   0.30000000000000000E+01 )'
      FRTEXT( 401 ) = ' FRAME_-82368_CENTER              =   ' &
      //              '  -0.82000000000000000E+02'
      FRTEXT( 402 ) = ' TKFRAME_DSS-54_TOPO_AXES         = ( ' &
      //              '   0.30000000000000000E+01,'
      FRTEXT( 403 ) = '                                      ' &
      //              '   0.20000000000000000E+01,'
      FRTEXT( 404 ) = '                                      ' &
      //              '   0.30000000000000000E+01 )'
      FRTEXT( 405 ) = ' FRAME_1399055_CLASS_ID           =   ' &
      //              '   0.13990550000000000E+07'
      FRTEXT( 406 ) = ' TKFRAME_-82844_AXES              = ( ' &
      //              '   0.10000000000000000E+01,'
      FRTEXT( 407 ) = '                                      ' &
      //              '   0.20000000000000000E+01,'
      FRTEXT( 408 ) = '                                      ' &
      //              '   0.10000000000000000E+01 )'
      FRTEXT( 409 ) = ' FRAME_-82893_NAME                =   ' &
      //              ' ''CASSINI_CIRS_FPB'''
      FRTEXT( 410 ) = ' FRAME_1399025_NAME               =   ' &
      //              ' ''DSS-25_TOPO'''
      FRTEXT( 411 ) = ' TKFRAME_DSS-26_TOPO_SPEC         =   ' &
      //              ' ''ANGLES'''
      FRTEXT( 412 ) = ' FRAME_-82361_CLASS               =   ' &
      //              '   0.40000000000000000E+01'
      FRTEXT( 413 ) = ' TKFRAME_-82369_RELATIVE          =   ' &
      //              ' ''CASSINI_SC_COORD'''
      FRTEXT( 414 ) = ' FRAME_-82764_CLASS               =   ' &
      //              '   0.30000000000000000E+01'
      FRTEXT( 415 ) = ' TKFRAME_-82765_UNITS             =   ' &
      //              ' ''DEGREES'''
      FRTEXT( 416 ) = ' TKFRAME_DSS-53_TOPO_UNITS        =   ' &
      //              ' ''DEGREES'''
      FRTEXT( 417 ) = ' TKFRAME_-82102_SPEC              =   ' &
      //              ' ''ANGLES'''
      FRTEXT( 418 ) = ' FRAME_1399061_NAME               =   ' &
      //              ' ''DSS-61_TOPO'''
      FRTEXT( 419 ) = ' TKFRAME_DSS-49_TOPO_ANGLES       = ( ' &
      //              '  -0.14826351615289471E+03,'
      FRTEXT( 420 ) = '                                      ' &
      //              '  -0.12299839804233260E+03,'
      FRTEXT( 421 ) = '                                      ' &
      //              '   0.18000000000000000E+03 )'
      FRTEXT( 422 ) = ' TKFRAME_-82732_ANGLES            = ( ' &
      //              '  -0.16309999999999999E+03,'
      FRTEXT( 423 ) = '                                      ' &
      //              '  -0.10759999999999999E+03,'
      FRTEXT( 424 ) = '                                      ' &
      //              '   0.00000000000000000E+00 )'
      FRTEXT( 425 ) = ' FRAME_1399061_CLASS              =   ' &
      //              '   0.40000000000000000E+01'
      FRTEXT( 426 ) = ' FRAME_1399014_CENTER             =   ' &
      //              '   0.39901400000000000E+06'
      FRTEXT( 427 ) = ' CK_-82791_SCLK                   =   ' &
      //              '  -0.82000000000000000E+02'
      FRTEXT( 428 ) = ' TKFRAME_-82351_RELATIVE          =   ' &
      //              ' ''CASSINI_SC_COORD'''
      FRTEXT( 429 ) = ' TKFRAME_DSS-49_TOPO_AXES         = ( ' &
      //              '   0.30000000000000000E+01,'
      FRTEXT( 430 ) = '                                      ' &
      //              '   0.20000000000000000E+01,'
      FRTEXT( 431 ) = '                                      ' &
      //              '   0.30000000000000000E+01 )'
      FRTEXT( 432 ) = ' TKFRAME_DSS-43_TOPO_UNITS        =   ' &
      //              ' ''DEGREES'''
      FRTEXT( 433 ) = ' FRAME_-82002_CLASS_ID            =   ' &
      //              '  -0.82002000000000000E+05'
      FRTEXT( 434 ) = ' TKFRAME_DSS-42_TOPO_AXES         = ( ' &
      //              '   0.30000000000000000E+01,'
      FRTEXT( 435 ) = '                                      ' &
      //              '   0.20000000000000000E+01,'
      FRTEXT( 436 ) = '                                      ' &
      //              '   0.30000000000000000E+01 )'
      FRTEXT( 437 ) = ' TKFRAME_-82842_ANGLES            = ( ' &
      //              '  -0.90000000000000000E+02,'
      FRTEXT( 438 ) = '                                      ' &
      //              '   0.17993125071716000E+03,'
      FRTEXT( 439 ) = '                                      ' &
      //              '   0.17992552119261001E+03 )'
      FRTEXT( 440 ) = ' TKFRAME_DSS-14_TOPO_SPEC         =   ' &
      //              ' ''ANGLES'''
      FRTEXT( 441 ) = ' FRAME_MOON_ME                    =   ' &
      //              '   0.31001000000000000E+05'
      FRTEXT( 442 ) = ' TKFRAME_-82765_ANGLES            = ( ' &
      //              '   0.18000000000000000E+03,'
      FRTEXT( 443 ) = '                                      ' &
      //              '   0.00000000000000000E+00,'
      FRTEXT( 444 ) = '                                      ' &
      //              '  -0.90000000000000000E+02 )'
      FRTEXT( 445 ) = ' TKFRAME_-82104_ANGLES            = ( ' &
      //              '   0.00000000000000000E+00,'
      FRTEXT( 446 ) = '                                      ' &
      //              '   0.00000000000000000E+00,'
      FRTEXT( 447 ) = '                                      ' &
      //              '   0.00000000000000000E+00 )'
      FRTEXT( 448 ) = ' TKFRAME_-82761_AXES              = ( ' &
      //              '   0.10000000000000000E+01,'
      FRTEXT( 449 ) = '                                      ' &
      //              '   0.20000000000000000E+01,'
      FRTEXT( 450 ) = '                                      ' &
      //              '   0.30000000000000000E+01 )'
      FRTEXT( 451 ) = ' FRAME_-82810_NAME                =   ' &
      //              ' ''CASSINI_RADAR_1'''
      FRTEXT( 452 ) = ' OBJECT_399033_FRAME              =   ' &
      //              ' ''DSS-33_TOPO'''
      FRTEXT( 453 ) = ' FRAME_1399016_CLASS_ID           =   ' &
      //              '   0.13990160000000000E+07'
      FRTEXT( 454 ) = ' TKFRAME_DSS-33_TOPO_UNITS        =   ' &
      //              ' ''DEGREES'''
      FRTEXT( 455 ) = ' FRAME_CASSINI_MAG_PLUS           =   ' &
      //              '  -0.82350000000000000E+05'
      FRTEXT( 456 ) = ' FRAME_-82843_NAME                =   ' &
      //              ' ''CASSINI_UVIS_SOLAR'''
      FRTEXT( 457 ) = ' FRAME_-82730_CLASS               =   ' &
      //              '   0.40000000000000000E+01'
      FRTEXT( 458 ) = ' TKFRAME_-82731_UNITS             =   ' &
      //              ' ''DEGREES'''
      FRTEXT( 459 ) = ' FRAME_-82890_CLASS               =   ' &
      //              '   0.40000000000000000E+01'
      FRTEXT( 460 ) = ' FRAME_DSS-42_TOPO                =   ' &
      //              '   0.13990420000000000E+07'
      FRTEXT( 461 ) = ' TKFRAME_-82891_UNITS             =   ' &
      //              ' ''DEGREES'''
      FRTEXT( 462 ) = ' TKFRAME_-82740_RELATIVE          =   ' &
      //              ' ''CASSINI_SC_COORD'''
      FRTEXT( 463 ) = ' TKFRAME_-82760_UNITS             =   ' &
      //              ' ''DEGREES'''
      FRTEXT( 464 ) = ' TKFRAME_DSS-13_TOPO_RELATIVE     =   ' &
      //              ' ''EARTH_FIXED'''
      FRTEXT( 465 ) = ' FRAME_-82108_NAME                =   ' &
      //              ' ''CASSINI_XBAND_TRUE'''
      FRTEXT( 466 ) = ' FRAME_1399042_CLASS_ID           =   ' &
      //              '   0.13990420000000000E+07'
      FRTEXT( 467 ) = ' FRAME_-82792_CENTER              =   ' &
      //              '  -0.82000000000000000E+02'
      FRTEXT( 468 ) = ' FRAME_1399027_CLASS              =   ' &
      //              '   0.40000000000000000E+01'
      FRTEXT( 469 ) = ' FRAME_1399064_CENTER             =   ' &
      //              '   0.39906400000000000E+06'
      FRTEXT( 470 ) = ' TKFRAME_-82891_RELATIVE          =   ' &
      //              ' ''CASSINI_CIRS_FPB'''
      FRTEXT( 471 ) = ' TKFRAME_DSS-23_TOPO_UNITS        =   ' &
      //              ' ''DEGREES'''
      FRTEXT( 472 ) = ' TKFRAME_31001_SPEC               =   ' &
      //              ' ''MATRIX'''
      FRTEXT( 473 ) = ' FRAME_CASSINI_CAPS_BASE          =   ' &
      //              '  -0.82822000000000000E+05'
      FRTEXT( 474 ) = ' TKFRAME_DSS-46_TOPO_ANGLES       = ( ' &
      //              '  -0.14898308226653509E+03,'
      FRTEXT( 475 ) = '                                      ' &
      //              '  -0.12540500967083020E+03,'
      FRTEXT( 476 ) = '                                      ' &
      //              '   0.18000000000000000E+03 )'
      FRTEXT( 477 ) = ' TKFRAME_DSS-61_TOPO_SPEC         =   ' &
      //              ' ''ANGLES'''
      FRTEXT( 478 ) = ' TKFRAME_-82892_ANGLES            = ( ' &
      //              '   0.00000000000000000E+00,'
      FRTEXT( 479 ) = '                                      ' &
      //              '   0.00000000000000000E+00,'
      FRTEXT( 480 ) = '                                      ' &
      //              '  -0.26929016371149000E-01 )'
      FRTEXT( 481 ) = ' TKFRAME_DSS-13_TOPO_UNITS        =   ' &
      //              ' ''DEGREES'''
      FRTEXT( 482 ) = ' TKFRAME_DSS-45_TOPO_ANGLES       = ( ' &
      //              '  -0.14897768621480211E+03,'
      FRTEXT( 483 ) = '                                      ' &
      //              '  -0.12539845671876880E+03,'
      FRTEXT( 484 ) = '                                      ' &
      //              '   0.18000000000000000E+03 )'
      FRTEXT( 485 ) = ' TKFRAME_DSS-65_TOPO_RELATIVE     =   ' &
      //              ' ''EARTH_FIXED'''
      FRTEXT( 486 ) = ' TKFRAME_-82734_SPEC              =   ' &
      //              ' ''ANGLES'''
      FRTEXT( 487 ) = ' FRAME_-82351_CENTER              =   ' &
      //              '  -0.82000000000000000E+02'
      FRTEXT( 488 ) = ' FRAME_-82760_NAME                =   ' &
      //              ' ''CASSINI_MIMI_CHEMS'''
      FRTEXT( 489 ) = ' FRAME_CASSINI_ISS_WAC            =   ' &
      //              '  -0.82361000000000000E+05'
      FRTEXT( 490 ) = ' OBJECT_399028_FRAME              =   ' &
      //              ' ''DSS-28_TOPO'''
      FRTEXT( 491 ) = ' TKFRAME_-82101_BORESIGHT         = ( ' &
      //              '   0.00000000000000000E+00,'
      FRTEXT( 492 ) = '                                      ' &
      //              '   0.00000000000000000E+00,'
      FRTEXT( 493 ) = '                                      ' &
      //              '   0.10000000000000000E+01 )'
      FRTEXT( 494 ) = ' TKFRAME_DSS-66_TOPO_UNITS        =   ' &
      //              ' ''DEGREES'''
      FRTEXT( 495 ) = ' TKFRAME_DSS-25_TOPO_AXES         = ( ' &
      //              '   0.30000000000000000E+01,'
      FRTEXT( 496 ) = '                                      ' &
      //              '   0.20000000000000000E+01,'
      FRTEXT( 497 ) = '                                      ' &
      //              '   0.30000000000000000E+01 )'
      FRTEXT( 498 ) = ' FRAME_-82351_CLASS               =   ' &
      //              '   0.40000000000000000E+01'
      FRTEXT( 499 ) = ' FRAME_DSS-25_TOPO                =   ' &
      //              '   0.13990250000000000E+07'
      FRTEXT( 500 ) = ' TKFRAME_-82009_AXES              = ( ' &
      //              '   0.30000000000000000E+01,'
      FRTEXT( 501 ) = '                                      ' &
      //              '   0.10000000000000000E+01,'
      FRTEXT( 502 ) = '                                      ' &
      //              '   0.30000000000000000E+01 )'
      FRTEXT( 503 ) = ' TKFRAME_-82009_RELATIVE          =   ' &
      //              ' ''CASSINI_SC_COORD'''
      FRTEXT( 504 ) = ' TKFRAME_-82002_SPEC              =   ' &
      //              ' ''ANGLES'''
      FRTEXT( 505 ) = ' FRAME_CASSINI_RPWS_EXPLUS        =   ' &
      //              '  -0.82731000000000000E+05'
      FRTEXT( 506 ) = ' FRAME_-82730_CLASS_ID            =   ' &
      //              '  -0.82730000000000000E+05'
      FRTEXT( 507 ) = ' TKFRAME_-82813_AXES              = ( ' &
      //              '   0.10000000000000000E+01,'
      FRTEXT( 508 ) = '                                      ' &
      //              '   0.20000000000000000E+01,'
      FRTEXT( 509 ) = '                                      ' &
      //              '   0.30000000000000000E+01 )'
      FRTEXT( 510 ) = ' TKFRAME_-82731_ANGLES            = ( ' &
      //              '  -0.16899999999999999E+02,'
      FRTEXT( 511 ) = '                                      ' &
      //              '  -0.10759999999999999E+03,'
      FRTEXT( 512 ) = '                                      ' &
      //              '   0.00000000000000000E+00 )'
      FRTEXT( 513 ) = ' FRAME_1399013_CENTER             =   ' &
      //              '   0.39901300000000000E+06'
      FRTEXT( 514 ) = ' FRAME_DSS-61_TOPO                =   ' &
      //              '   0.13990610000000000E+07'
      FRTEXT( 515 ) = ' FRAME_1399027_NAME               =   ' &
      //              ' ''DSS-27_TOPO'''
      FRTEXT( 516 ) = ' FRAME_-82351_NAME                =   ' &
      //              ' ''CASSINI_MAG_MINUS'''
      FRTEXT( 517 ) = ' CK_-82764_SPK                    =   ' &
      //              '  -0.82000000000000000E+02'
      FRTEXT( 518 ) = ' TKFRAME_-82009_ANGLES            = ( ' &
      //              '   0.18000000000000000E+03,'
      FRTEXT( 519 ) = '                                      ' &
      //              '  -0.90000000000000000E+02,'
      FRTEXT( 520 ) = '                                      ' &
      //              '   0.00000000000000000E+00 )'
      FRTEXT( 521 ) = ' FRAME_31003_CLASS                =   ' &
      //              '   0.40000000000000000E+01'
      FRTEXT( 522 ) = ' TKFRAME_-82368_AXES              = ( ' &
      //              '   0.30000000000000000E+01,'
      FRTEXT( 523 ) = '                                      ' &
      //              '   0.10000000000000000E+01,'
      FRTEXT( 524 ) = '                                      ' &
      //              '   0.30000000000000000E+01 )'
      FRTEXT( 525 ) = ' FRAME_-82811_CLASS_ID            =   ' &
      //              '  -0.82811000000000000E+05'
      FRTEXT( 526 ) = ' TKFRAME_DSS-43_TOPO_ANGLES       = ( ' &
      //              '  -0.14898126789071159E+03,'
      FRTEXT( 527 ) = '                                      ' &
      //              '  -0.12540242326663780E+03,'
      FRTEXT( 528 ) = '                                      ' &
      //              '   0.18000000000000000E+03 )'
      FRTEXT( 529 ) = ' TKFRAME_DSS-49_TOPO_RELATIVE     =   ' &
      //              ' ''EARTH_FIXED'''
      FRTEXT( 530 ) = ' TKFRAME_-82104_SPEC              =   ' &
      //              ' ''ANGLES'''
      FRTEXT( 531 ) = ' TKFRAME_-82361_SPEC              =   ' &
      //              ' ''ANGLES'''
      FRTEXT( 532 ) = ' FRAME_1399063_NAME               =   ' &
      //              ' ''DSS-63_TOPO'''
      FRTEXT( 533 ) = ' TKFRAME_DSS-13_TOPO_AXES         = ( ' &
      //              '   0.30000000000000000E+01,'
      FRTEXT( 534 ) = '                                      ' &
      //              '   0.20000000000000000E+01,'
      FRTEXT( 535 ) = '                                      ' &
      //              '   0.30000000000000000E+01 )'
      FRTEXT( 536 ) = ' FRAME_-82372_CLASS_ID            =   ' &
      //              '  -0.82372000000000000E+05'
      FRTEXT( 537 ) = ' TKFRAME_-82371_AXES              = ( ' &
      //              '   0.30000000000000000E+01,'
      FRTEXT( 538 ) = '                                      ' &
      //              '   0.10000000000000000E+01,'
      FRTEXT( 539 ) = '                                      ' &
      //              '   0.30000000000000000E+01 )'
      FRTEXT( 540 ) = ' TKFRAME_DSS-46_TOPO_UNITS        =   ' &
      //              ' ''DEGREES'''
      FRTEXT( 541 ) = ' FRAME_-82822_CLASS               =   ' &
      //              '   0.40000000000000000E+01'
      FRTEXT( 542 ) = ' FRAME_1399046_CENTER             =   ' &
      //              '   0.39904600000000000E+06'
      FRTEXT( 543 ) = ' TKFRAME_-82103_ANGLES            = ( ' &
      //              '   0.00000000000000000E+00,'
      FRTEXT( 544 ) = '                                      ' &
      //              '   0.90000000000000000E+02,'
      FRTEXT( 545 ) = '                                      ' &
      //              '   0.00000000000000000E+00 )'
      FRTEXT( 546 ) = ' OBJECT_399023_FRAME              =   ' &
      //              ' ''DSS-23_TOPO'''
      FRTEXT( 547 ) = ' TKFRAME_DSS-42_TOPO_ANGLES       = ( ' &
      //              '  -0.14898126797965500E+03,'
      FRTEXT( 548 ) = '                                      ' &
      //              '  -0.12540067366658261E+03,'
      FRTEXT( 549 ) = '                                      ' &
      //              '   0.18000000000000000E+03 )'
      FRTEXT( 550 ) = ' TKFRAME_-82732_RELATIVE          =   ' &
      //              ' ''CASSINI_SC_COORD'''
      FRTEXT( 551 ) = ' FRAME_-82008_NAME                =   ' &
      //              ' ''CASSINI_SRU-A_RAD'''
      FRTEXT( 552 ) = ' FRAME_-82101_CLASS_ID            =   ' &
      //              '  -0.82101000000000000E+05'
      FRTEXT( 553 ) = ' FRAME_1399034_CLASS_ID           =   ' &
      //              '   0.13990340000000000E+07'
      FRTEXT( 554 ) = ' FRAME_-82791_CENTER              =   ' &
      //              '  -0.82000000000000000E+02'
      FRTEXT( 555 ) = ' FRAME_1399017_CLASS              =   ' &
      //              '   0.40000000000000000E+01'
      FRTEXT( 556 ) = ' FRAME_-82761_CLASS_ID            =   ' &
      //              '  -0.82761000000000000E+05'
      FRTEXT( 557 ) = ' TKFRAME_-82730_AXES              = ( ' &
      //              '   0.30000000000000000E+01,'
      FRTEXT( 558 ) = '                                      ' &
      //              '   0.10000000000000000E+01,'
      FRTEXT( 559 ) = '                                      ' &
      //              '   0.30000000000000000E+01 )'
      FRTEXT( 560 ) = ' FRAME_1399063_CENTER             =   ' &
      //              '   0.39906300000000000E+06'
      FRTEXT( 561 ) = ' FRAME_1399046_CLASS              =   ' &
      //              '   0.40000000000000000E+01'
      FRTEXT( 562 ) = ' FRAME_-82812_NAME                =   ' &
      //              ' ''CASSINI_RADAR_3'''
      FRTEXT( 563 ) = ' TKFRAME_-82813_RELATIVE          =   ' &
      //              ' ''CASSINI_SC_COORD'''
      FRTEXT( 564 ) = ' TKFRAME_DSS-12_TOPO_RELATIVE     =   ' &
      //              ' ''EARTH_FIXED'''
      FRTEXT( 565 ) = ' FRAME_-82845_NAME                =   ' &
      //              ' ''CASSINI_UVIS_HDAC'''
      FRTEXT( 566 ) = ' TKFRAME_PARKES_TOPO_AXES         = ( ' &
      //              '   0.30000000000000000E+01,'
      FRTEXT( 567 ) = '                                      ' &
      //              '   0.20000000000000000E+01,'
      FRTEXT( 568 ) = '                                      ' &
      //              '   0.30000000000000000E+01 )'
      FRTEXT( 569 ) = ' TKFRAME_PARKES_TOPO_ANGLES       = ( ' &
      //              '  -0.14826351615289471E+03,'
      FRTEXT( 570 ) = '                                      ' &
      //              '  -0.12299839804233260E+03,'
      FRTEXT( 571 ) = '                                      ' &
      //              '   0.18000000000000000E+03 )'
      FRTEXT( 572 ) = ' TKFRAME_DSS-26_TOPO_UNITS        =   ' &
      //              ' ''DEGREES'''
      FRTEXT( 573 ) = ' FRAME_-82842_CLASS_ID            =   ' &
      //              '  -0.82842000000000000E+05'
      FRTEXT( 574 ) = ' TKFRAME_-82891_ANGLES            = ( ' &
      //              '   0.00000000000000000E+00,'
      FRTEXT( 575 ) = '                                      ' &
      //              '   0.00000000000000000E+00,'
      FRTEXT( 576 ) = '                                      ' &
      //              '   0.26929016371149000E-01 )'
      FRTEXT( 577 ) = ' TKFRAME_-82822_SPEC              =   ' &
      //              ' ''ANGLES'''
      FRTEXT( 578 ) = ' FRAME_1399013_NAME               =   ' &
      //              ' ''DSS-13_TOPO'''
      FRTEXT( 579 ) = ' FRAME_1399046_NAME               =   ' &
      //              ' ''DSS-46_TOPO'''
      FRTEXT( 580 ) = ' FRAME_-82370_NAME                =   ' &
      //              ' ''CASSINI_VIMS_V'''
      FRTEXT( 581 ) = ' TKFRAME_31003_SPEC               =   ' &
      //              ' ''ANGLES'''
      FRTEXT( 582 ) = ' TKFRAME_-82898_AXES              = ( ' &
      //              '   0.30000000000000000E+01,'
      FRTEXT( 583 ) = '                                      ' &
      //              '   0.10000000000000000E+01,'
      FRTEXT( 584 ) = '                                      ' &
      //              '   0.30000000000000000E+01 )'
      FRTEXT( 585 ) = ' TKFRAME_-82891_SPEC              =   ' &
      //              ' ''ANGLES'''
      FRTEXT( 586 ) = ' FRAME_-82350_CENTER              =   ' &
      //              '  -0.82000000000000000E+02'
      FRTEXT( 587 ) = ' TKFRAME_DSS-16_TOPO_UNITS        =   ' &
      //              ' ''DEGREES'''
      FRTEXT( 588 ) = ' FRAME_-82106_CLASS_ID            =   ' &
      //              '  -0.82106000000000000E+05'
      FRTEXT( 589 ) = ' TKFRAME_-82103_RELATIVE          =   ' &
      //              ' ''CASSINI_SC_COORD'''
      FRTEXT( 590 ) = ' TKFRAME_DSS-64_TOPO_RELATIVE     =   ' &
      //              ' ''EARTH_FIXED'''
      FRTEXT( 591 ) = ' FRAME_-82370_CLASS               =   ' &
      //              '   0.40000000000000000E+01'
      FRTEXT( 592 ) = ' TKFRAME_-82371_UNITS             =   ' &
      //              ' ''DEGREES'''
      FRTEXT( 593 ) = ' TKFRAME_DSS-55_TOPO_AXES         = ( ' &
      //              '   0.30000000000000000E+01,'
      FRTEXT( 594 ) = '                                      ' &
      //              '   0.20000000000000000E+01,'
      FRTEXT( 595 ) = '                                      ' &
      //              '   0.30000000000000000E+01 )'
      FRTEXT( 596 ) = ' FRAME_1399065_CLASS_ID           =   ' &
      //              '   0.13990650000000000E+07'
      FRTEXT( 597 ) = ' FRAME_-82792_CLASS_ID            =   ' &
      //              '  -0.82792000000000000E+05'
      FRTEXT( 598 ) = ' TKFRAME_31000_RELATIVE           =   ' &
      //              ' ''MOON_PA_DE403'''
      FRTEXT( 599 ) = ' FRAME_1399012_CLASS              =   ' &
      //              '   0.40000000000000000E+01'
      FRTEXT( 600 ) = ' TKFRAME_DSS-27_TOPO_SPEC         =   ' &
      //              ' ''ANGLES'''
      FRTEXT( 601 ) = ' FRAME_-82740_CENTER              =   ' &
      //              '  -0.82000000000000000E+02'
      FRTEXT( 602 ) = ' FRAME_-82108_CLASS               =   ' &
      //              '   0.40000000000000000E+01'
      FRTEXT( 603 ) = ' FRAME_-82762_NAME                =   ' &
      //              ' ''CASSINI_MIMI_LEMMS1'''
      FRTEXT( 604 ) = ' TKFRAME_-82844_RELATIVE          =   ' &
      //              ' ''CASSINI_SC_COORD'''
      FRTEXT( 605 ) = ' TKFRAME_-82730_ANGLES            = ( ' &
      //              '   0.18000000000000000E+03,'
      FRTEXT( 606 ) = '                                      ' &
      //              '  -0.90000000000000000E+02,'
      FRTEXT( 607 ) = '                                      ' &
      //              '   0.00000000000000000E+00 )'
      FRTEXT( 608 ) = ' FRAME_1399012_CENTER             =   ' &
      //              '   0.39901200000000000E+06'
      FRTEXT( 609 ) = ' FRAME_1399028_CENTER             =   ' &
      //              '   0.39902800000000000E+06'
      FRTEXT( 610 ) = ' FRAME_-82002_CENTER              =   ' &
      //              '  -0.82000000000000000E+02'
      FRTEXT( 611 ) = ' FRAME_CASSINI_RPWS_LP            =   ' &
      //              '  -0.82734000000000000E+05'
      FRTEXT( 612 ) = ' TKFRAME_-82008_ANGLES            = ( ' &
      //              '   0.18000000000000000E+03,'
      FRTEXT( 613 ) = '                                      ' &
      //              '  -0.90000000000000000E+02,'
      FRTEXT( 614 ) = '                                      ' &
      //              '   0.00000000000000000E+00 )'
      FRTEXT( 615 ) = ' FRAME_DSS-27_TOPO                =   ' &
      //              '   0.13990270000000000E+07'
      FRTEXT( 616 ) = ' FRAME_CASSINI_SBAND              =   ' &
      //              '  -0.82107000000000000E+05'
      FRTEXT( 617 ) = ' TKFRAME_-82840_ANGLES            = ( ' &
      //              '  -0.90000000000000000E+02,'
      FRTEXT( 618 ) = '                                      ' &
      //              '   0.17998854084310000E+03,'
      FRTEXT( 619 ) = '                                      ' &
      //              '   0.17999427042161000E+03 )'
      FRTEXT( 620 ) = ' TKFRAME_-82108_RELATIVE          =   ' &
      //              ' ''CASSINI_SC_COORD'''
      FRTEXT( 621 ) = ' FRAME_-82812_CLASS               =   ' &
      //              '   0.40000000000000000E+01'
      FRTEXT( 622 ) = ' TKFRAME_-82813_UNITS             =   ' &
      //              ' ''DEGREES'''
      FRTEXT( 623 ) = ' TKFRAME_-82361_RELATIVE          =   ' &
      //              ' ''CASSINI_SC_COORD'''
      FRTEXT( 624 ) = ' FRAME_DSS-63_TOPO                =   ' &
      //              '   0.13990630000000000E+07'
      FRTEXT( 625 ) = ' FRAME_1399045_CENTER             =   ' &
      //              '   0.39904500000000000E+06'
      FRTEXT( 626 ) = ' TKFRAME_-82102_ANGLES            = ( ' &
      //              '   0.00000000000000000E+00,'
      FRTEXT( 627 ) = '                                      ' &
      //              '   0.18000000000000000E+03,'
      FRTEXT( 628 ) = '                                      ' &
      //              '   0.00000000000000000E+00 )'
      FRTEXT( 629 ) = ' TKFRAME_DSS-43_TOPO_AXES         = ( ' &
      //              '   0.30000000000000000E+01,'
      FRTEXT( 630 ) = '                                      ' &
      //              '   0.20000000000000000E+01,'
      FRTEXT( 631 ) = '                                      ' &
      //              '   0.30000000000000000E+01 )'
      FRTEXT( 632 ) = ' TKFRAME_DSS-49_TOPO_UNITS        =   ' &
      //              ' ''DEGREES'''
      FRTEXT( 633 ) = ' TKFRAME_-82842_UNITS             =   ' &
      //              ' ''DEGREES'''
      FRTEXT( 634 ) = ' FRAME_MOON_ME_DE403              =   ' &
      //              '   0.31003000000000000E+05'
      FRTEXT( 635 ) = ' TKFRAME_DSS-15_TOPO_SPEC         =   ' &
      //              ' ''ANGLES'''
      FRTEXT( 636 ) = ' OBJECT_399013_FRAME              =   ' &
      //              ' ''DSS-13_TOPO'''
      FRTEXT( 637 ) = ' OBJECT_399042_FRAME              =   ' &
      //              ' ''DSS-42_TOPO'''
      FRTEXT( 638 ) = ' TKFRAME_-82106_SPEC              =   ' &
      //              ' ''ANGLES'''
      FRTEXT( 639 ) = ' FRAME_1399065_NAME               =   ' &
      //              ' ''DSS-65_TOPO'''
      FRTEXT( 640 ) = ' TKFRAME_-82740_UNITS             =   ' &
      //              ' ''DEGREES'''
      FRTEXT( 641 ) = ' FRAME_1399026_CLASS_ID           =   ' &
      //              '   0.13990260000000000E+07'
      FRTEXT( 642 ) = ' FRAME_-82790_CENTER              =   ' &
      //              '  -0.82000000000000000E+02'
      FRTEXT( 643 ) = ' CK_-82762_SCLK                   =   ' &
      //              '  -0.82000000000000000E+02'
      FRTEXT( 644 ) = ' TKFRAME_DSS-55_TOPO_ANGLES       = ( ' &
      //              '  -0.35574736739940482E+03,'
      FRTEXT( 645 ) = '                                      ' &
      //              '  -0.49575703528880503E+02,'
      FRTEXT( 646 ) = '                                      ' &
      //              '   0.18000000000000000E+03 )'
      FRTEXT( 647 ) = ' FRAME_-82103_CLASS               =   ' &
      //              '   0.40000000000000000E+01'
      FRTEXT( 648 ) = ' TKFRAME_-82104_UNITS             =   ' &
      //              ' ''DEGREES'''
      FRTEXT( 649 ) = ' FRAME_1399065_CLASS              =   ' &
      //              '   0.40000000000000000E+01'
      FRTEXT( 650 ) = ' FRAME_-82369_CLASS_ID            =   ' &
      //              '  -0.82369000000000000E+05'
      FRTEXT( 651 ) = ' TKFRAME_-82890_ANGLES            = ( ' &
      //              '   0.00000000000000000E+00,'
      FRTEXT( 652 ) = '                                      ' &
      //              '   0.40107045659157998E-02,'
      FRTEXT( 653 ) = '                                      ' &
      //              '  -0.22803720246206999E+00 )'
      FRTEXT( 654 ) = ' FRAME_-82001_CLASS               =   ' &
      //              '   0.40000000000000000E+01'
      FRTEXT( 655 ) = ' TKFRAME_-82002_UNITS             =   ' &
      //              ' ''DEGREES'''
      FRTEXT( 656 ) = ' TKFRAME_-82732_AXES              = ( ' &
      //              '   0.30000000000000000E+01,'
      FRTEXT( 657 ) = '                                      ' &
      //              '   0.20000000000000000E+01,'
      FRTEXT( 658 ) = '                                      ' &
      //              '   0.10000000000000000E+01 )'
      FRTEXT( 659 ) = ' TKFRAME_DSS-54_TOPO_ANGLES       = ( ' &
      //              '  -0.35574590387093929E+03,'
      FRTEXT( 660 ) = '                                      ' &
      //              '  -0.49574377750704997E+02,'
      FRTEXT( 661 ) = '                                      ' &
      //              '   0.18000000000000000E+03 )'
      FRTEXT( 662 ) = ' TKFRAME_-82765_AXES              = ( ' &
      //              '   0.30000000000000000E+01,'
      FRTEXT( 663 ) = '                                      ' &
      //              '   0.20000000000000000E+01,'
      FRTEXT( 664 ) = '                                      ' &
      //              '   0.10000000000000000E+01 )'
      FRTEXT( 665 ) = ' FRAME_31000_NAME                 =   ' &
      //              ' ''MOON_PA'''
      FRTEXT( 666 ) = ' FRAME_-82814_NAME                =   ' &
      //              ' ''CASSINI_RADAR_5'''
      FRTEXT( 667 ) = ' FRAME_DSS-13_TOPO                =   ' &
      //              '   0.13990130000000000E+07'
      FRTEXT( 668 ) = ' FRAME_DSS-46_TOPO                =   ' &
      //              '   0.13990460000000000E+07'
      FRTEXT( 669 ) = ' FRAME_-82351_CLASS_ID            =   ' &
      //              '  -0.82351000000000000E+05'
      FRTEXT( 670 ) = ' FRAME_-82369_NAME                =   ' &
      //              ' ''CASSINI_ISS_WAC_RAD'''
      FRTEXT( 671 ) = ' TKFRAME_DSS-53_TOPO_ANGLES       = ( ' &
      //              '  -0.35575034853158411E+03,'
      FRTEXT( 672 ) = '                                      ' &
      //              '  -0.49572642141262399E+02,'
      FRTEXT( 673 ) = '                                      ' &
      //              '   0.18000000000000000E+03 )'
      FRTEXT( 674 ) = ' FRAME_-82734_CLASS               =   ' &
      //              '   0.40000000000000000E+01'
      FRTEXT( 675 ) = ' FRAME_1399015_NAME               =   ' &
      //              ' ''DSS-15_TOPO'''
      FRTEXT( 676 ) = ' FRAME_-82360_CLASS               =   ' &
      //              '   0.40000000000000000E+01'
      FRTEXT( 677 ) = ' TKFRAME_-82361_UNITS             =   ' &
      //              ' ''DEGREES'''
      FRTEXT( 678 ) = ' TKFRAME_-82102_BORESIGHT         = ( ' &
      //              '   0.00000000000000000E+00,'
      FRTEXT( 679 ) = '                                      ' &
      //              '   0.00000000000000000E+00,'
      FRTEXT( 680 ) = '                                      ' &
      //              '   0.10000000000000000E+01 )'
      FRTEXT( 681 ) = ' OBJECT_399066_FRAME              =   ' &
      //              ' ''DSS-66_TOPO'''
      FRTEXT( 682 ) = ' FRAME_-82372_NAME                =   ' &
      //              ' ''CASSINI_VIMS_IR_SOL'''
      FRTEXT( 683 ) = ' TKFRAME_DSS-26_TOPO_AXES         = ( ' &
      //              '   0.30000000000000000E+01,'
      FRTEXT( 684 ) = '                                      ' &
      //              '   0.20000000000000000E+01,'
      FRTEXT( 685 ) = '                                      ' &
      //              '   0.30000000000000000E+01 )'
      FRTEXT( 686 ) = ' FRAME_-82763_CLASS               =   ' &
      //              '   0.30000000000000000E+01'
      FRTEXT( 687 ) = ' TKFRAME_-82822_ANGLES            = ( ' &
      //              '   0.00000000000000000E+00,'
      FRTEXT( 688 ) = '                                      ' &
      //              '   0.00000000000000000E+00,'
      FRTEXT( 689 ) = '                                      ' &
      //              '   0.00000000000000000E+00 )'
      FRTEXT( 690 ) = ' TKFRAME_-82893_SPEC              =   ' &
      //              ' ''ANGLES'''
      FRTEXT( 691 ) = ' TKFRAME_DSS-63_TOPO_RELATIVE     =   ' &
      //              ' ''EARTH_FIXED'''
      FRTEXT( 692 ) = ' FRAME_-82792_CLASS               =   ' &
      //              '   0.40000000000000000E+01'
      FRTEXT( 693 ) = ' FRAME_1399027_CENTER             =   ' &
      //              '   0.39902700000000000E+06'
      FRTEXT( 694 ) = ' TKFRAME_-82372_ANGLES            = ( ' &
      //              '   0.85943668669984000E-01,'
      FRTEXT( 695 ) = '                                      ' &
      //              '   0.00000000000000000E+00,'
      FRTEXT( 696 ) = '                                      ' &
      //              '  -0.11063025357166001E+03 )'
      FRTEXT( 697 ) = ' TKFRAME_-82102_AXES              = ( ' &
      //              '   0.30000000000000000E+01,'
      FRTEXT( 698 ) = '                                      ' &
      //              '   0.20000000000000000E+01,'
      FRTEXT( 699 ) = '                                      ' &
      //              '   0.10000000000000000E+01 )'
      FRTEXT( 700 ) = ' FRAME_1399013_CLASS_ID           =   ' &
      //              '   0.13990130000000000E+07'
      FRTEXT( 701 ) = ' FRAME_-82740_CLASS_ID            =   ' &
      //              '  -0.82740000000000000E+05'
      FRTEXT( 702 ) = ' FRAME_-82001_CENTER              =   ' &
      //              '  -0.82000000000000000E+02'
      FRTEXT( 703 ) = ' FRAME_-82731_NAME                =   ' &
      //              ' ''CASSINI_RPWS_EXPLUS'''
      FRTEXT( 704 ) = ' TKFRAME_DSS-55_TOPO_RELATIVE     =   ' &
      //              ' ''EARTH_FIXED'''
      FRTEXT( 705 ) = ' FRAME_-82891_CLASS_ID            =   ' &
      //              '  -0.82891000000000000E+05'
      FRTEXT( 706 ) = ' FRAME_-82764_NAME                =   ' &
      //              ' ''CASSINI_MIMI_LEMMS_ART'''
      FRTEXT( 707 ) = ' FRAME_-82821_CLASS_ID            =   ' &
      //              '  -0.82821000000000000E+05'
      FRTEXT( 708 ) = ' FRAME_CASSINI_CDA_ART            =   ' &
      //              '  -0.82791000000000000E+05'
      FRTEXT( 709 ) = ' TKFRAME_-82101_ANGLES            = ( ' &
      //              '   0.00000000000000000E+00,'
      FRTEXT( 710 ) = '                                      ' &
      //              '   0.18000000000000000E+03,'
      FRTEXT( 711 ) = '                                      ' &
      //              '   0.00000000000000000E+00 )'
      FRTEXT( 712 ) = ' TKFRAME_-82001_RELATIVE          =   ' &
      //              ' ''CASSINI_SC_COORD'''
      FRTEXT( 713 ) = ' TKFRAME_DSS-14_TOPO_AXES         = ( ' &
      //              '   0.30000000000000000E+01,'
      FRTEXT( 714 ) = '                                      ' &
      //              '   0.20000000000000000E+01,'
      FRTEXT( 715 ) = '                                      ' &
      //              '   0.30000000000000000E+01 )'
      FRTEXT( 716 ) = ' TKFRAME_EARTH_FIXED_MATRIX       = ( ' &
      //              '   0.10000000000000000E+01,'
      FRTEXT( 717 ) = '                                      ' &
      //              '   0.00000000000000000E+00,'
      FRTEXT( 718 ) = '                                      ' &
      //              '   0.00000000000000000E+00,'
      FRTEXT( 719 ) = '                                      ' &
      //              '   0.00000000000000000E+00,'
      FRTEXT( 720 ) = '                                      ' &
      //              '   0.10000000000000000E+01,'
      FRTEXT( 721 ) = '                                      ' &
      //              '   0.00000000000000000E+00,'
      FRTEXT( 722 ) = '                                      ' &
      //              '   0.00000000000000000E+00,'
      FRTEXT( 723 ) = '                                      ' &
      //              '   0.00000000000000000E+00,'
      FRTEXT( 724 ) = '                                      ' &
      //              '   0.10000000000000000E+01 )'
      FRTEXT( 725 ) = ' TKFRAME_DSS-45_TOPO_SPEC         =   ' &
      //              ' ''ANGLES'''
      FRTEXT( 726 ) = ' FRAME_-82898_CENTER              =   ' &
      //              '  -0.82000000000000000E+02'
      FRTEXT( 727 ) = ' TKFRAME_-82730_UNITS             =   ' &
      //              ' ''DEGREES'''
      FRTEXT( 728 ) = ' TKFRAME_-82890_UNITS             =   ' &
      //              ' ''DEGREES'''
      FRTEXT( 729 ) = ' OBJECT_399061_FRAME              =   ' &
      //              ' ''DSS-61_TOPO'''
      FRTEXT( 730 ) = ' FRAME_DSS-65_TOPO                =   ' &
      //              '   0.13990650000000000E+07'
      FRTEXT( 731 ) = ' FRAME_1399061_CENTER             =   ' &
      //              '   0.39906100000000000E+06'
      FRTEXT( 732 ) = ' TKFRAME_-82810_SPEC              =   ' &
      //              ' ''ANGLES'''
      FRTEXT( 733 ) = ' FRAME_1399026_CLASS              =   ' &
      //              '   0.40000000000000000E+01'
      FRTEXT( 734 ) = ' TKFRAME_-82843_SPEC              =   ' &
      //              ' ''ANGLES'''
      FRTEXT( 735 ) = ' FRAME_-82101_NAME                =   ' &
      //              ' ''CASSINI_HGA'''
      FRTEXT( 736 ) = ' FRAME_1399034_NAME               =   ' &
      //              ' ''DSS-34_TOPO'''
      FRTEXT( 737 ) = ' FRAME_1399055_CLASS              =   ' &
      //              '   0.40000000000000000E+01'
      FRTEXT( 738 ) = ' TKFRAME_-82108_SPEC              =   ' &
      //              ' ''ANGLES'''
      FRTEXT( 739 ) = ' FRAME_CASSINI_KUBAND             =   ' &
      //              '  -0.82106000000000000E+05'
      FRTEXT( 740 ) = ' TKFRAME_-82893_RELATIVE          =   ' &
      //              ' ''CASSINI_SC_COORD'''
      FRTEXT( 741 ) = ' FRAME_-82009_CLASS_ID            =   ' &
      //              '  -0.82009000000000000E+05'
      FRTEXT( 742 ) = ' FRAME_CASSINI_MIMI_CHEMS         =   ' &
      //              '  -0.82760000000000000E+05'
      FRTEXT( 743 ) = ' CK_-82764_SCLK                   =   ' &
      //              '  -0.82000000000000000E+02'
      FRTEXT( 744 ) = ' TKFRAME_DSS-61_TOPO_AXES         = ( ' &
      //              '   0.30000000000000000E+01,'
      FRTEXT( 745 ) = '                                      ' &
      //              '   0.20000000000000000E+01,'
      FRTEXT( 746 ) = '                                      ' &
      //              '   0.30000000000000000E+01 )'
      FRTEXT( 747 ) = ' FRAME_-82814_CENTER              =   ' &
      //              '  -0.82000000000000000E+02'
      FRTEXT( 748 ) = ' TKFRAME_DSS-33_TOPO_SPEC         =   ' &
      //              ' ''ANGLES'''
      FRTEXT( 749 ) = ' FRAME_CASSINI_VIMS_IR_SOL        =   ' &
      //              '  -0.82372000000000000E+05'
      FRTEXT( 750 ) = ' OBJECT_399027_FRAME              =   ' &
      //              ' ''DSS-27_TOPO'''
      FRTEXT( 751 ) = ' TKFRAME_-82734_AXES              = ( ' &
      //              '   0.30000000000000000E+01,'
      FRTEXT( 752 ) = '                                      ' &
      //              '   0.20000000000000000E+01,'
      FRTEXT( 753 ) = '                                      ' &
      //              '   0.10000000000000000E+01 )'
      FRTEXT( 754 ) = ' FRAME_-82350_CLASS               =   ' &
      //              '   0.40000000000000000E+01'
      FRTEXT( 755 ) = ' TKFRAME_-82351_UNITS             =   ' &
      //              ' ''DEGREES'''
      FRTEXT( 756 ) = ' FRAME_1399049_CLASS_ID           =   ' &
      //              '   0.13990490000000000E+07'
      FRTEXT( 757 ) = ' FRAME_31002_NAME                 =   ' &
      //              ' ''MOON_PA_DE403'''
      FRTEXT( 758 ) = ' FRAME_DSS-15_TOPO                =   ' &
      //              '   0.13990150000000000E+07'
      FRTEXT( 759 ) = ' TKFRAME_-82760_SPEC              =   ' &
      //              ' ''ANGLES'''
      FRTEXT( 760 ) = ' TKFRAME_-82898_RELATIVE          =   ' &
      //              ' ''CASSINI_SC_COORD'''
      FRTEXT( 761 ) = ' TKFRAME_DSS-66_TOPO_ANGLES       = ( ' &
      //              '  -0.35574858307052830E+03,'
      FRTEXT( 762 ) = '                                      ' &
      //              '  -0.49570024558335497E+02,'
      FRTEXT( 763 ) = '                                      ' &
      //              '   0.18000000000000000E+03 )'
      FRTEXT( 764 ) = ' FRAME_1399026_CENTER             =   ' &
      //              '   0.39902600000000000E+06'
      FRTEXT( 765 ) = ' TKFRAME_-82371_ANGLES            = ( ' &
      //              '  -0.36012175939433001E+03,'
      FRTEXT( 766 ) = '                                      ' &
      //              '  -0.90048672769633001E+02,'
      FRTEXT( 767 ) = '                                      ' &
      //              '   0.00000000000000000E+00 )'
      FRTEXT( 768 ) = ' TEXT_KERNEL_ID                   =   ' &
      //              ' ''CASSINI_FRAMES V3.9.0 14-OCT-2004 F' &
      //              'K'''
      FRTEXT( 769 ) = ' TKFRAME_-82002_AXES              = ( ' &
      //              '   0.30000000000000000E+01,'
      FRTEXT( 770 ) = '                                      ' &
      //              '   0.20000000000000000E+01,'
      FRTEXT( 771 ) = '                                      ' &
      //              '   0.10000000000000000E+01 )'
      FRTEXT( 772 ) = ' FRAME_1399005_CLASS_ID           =   ' &
      //              '   0.13990050000000000E+07'
      FRTEXT( 773 ) = ' TKFRAME_DSS-28_TOPO_SPEC         =   ' &
      //              ' ''ANGLES'''
      FRTEXT( 774 ) = ' FRAME_CASSINI_XBAND_TRUE         =   ' &
      //              '  -0.82108000000000000E+05'
      FRTEXT( 775 ) = ' FRAME_-82732_CLASS_ID            =   ' &
      //              '  -0.82732000000000000E+05'
      FRTEXT( 776 ) = ' FRAME_-82000_CENTER              =   ' &
      //              '  -0.82000000000000000E+02'
      FRTEXT( 777 ) = ' FRAME_31003_CENTER               =   ' &
      //              '   0.30100000000000000E+03'
      FRTEXT( 778 ) = ' FRAME_CASSINI_RPWS               =   ' &
      //              '  -0.82730000000000000E+05'
      FRTEXT( 779 ) = ' FRAME_1399017_NAME               =   ' &
      //              ' ''DSS-17_TOPO'''
      FRTEXT( 780 ) = ' FRAME_31002_CLASS                =   ' &
      //              '   0.20000000000000000E+01'
      FRTEXT( 781 ) = ' TKFRAME_DSS-65_TOPO_ANGLES       = ( ' &
      //              '  -0.35574930190246852E+03,'
      FRTEXT( 782 ) = '                                      ' &
      //              '  -0.49572793062938800E+02,'
      FRTEXT( 783 ) = '                                      ' &
      //              '   0.18000000000000000E+03 )'
      FRTEXT( 784 ) = ' TKFRAME_-82351_SPEC              =   ' &
      //              ' ''ANGLES'''
      FRTEXT( 785 ) = ' FRAME_1399053_NAME               =   ' &
      //              ' ''DSS-53_TOPO'''
      FRTEXT( 786 ) = ' FRAME_-82813_CLASS_ID            =   ' &
      //              '  -0.82813000000000000E+05'
      FRTEXT( 787 ) = ' TKFRAME_-82761_ANGLES            = ( ' &
      //              '  -0.90000000000000000E+02,'
      FRTEXT( 788 ) = '                                      ' &
      //              '  -0.95000000000000000E+01,'
      FRTEXT( 789 ) = '                                      ' &
      //              '   0.00000000000000000E+00 )'
      FRTEXT( 790 ) = ' FRAME_1399043_CENTER             =   ' &
      //              '   0.39904300000000000E+06'
      FRTEXT( 791 ) = ' TKFRAME_-82104_AXES              = ( ' &
      //              '   0.30000000000000000E+01,'
      FRTEXT( 792 ) = '                                      ' &
      //              '   0.10000000000000000E+01,'
      FRTEXT( 793 ) = '                                      ' &
      //              '   0.20000000000000000E+01 )'
      FRTEXT( 794 ) = ' TKFRAME_-82810_RELATIVE          =   ' &
      //              ' ''CASSINI_SC_COORD'''
      FRTEXT( 795 ) = ' TKFRAME_-82361_AXES              = ( ' &
      //              '   0.10000000000000000E+01,'
      FRTEXT( 796 ) = '                                      ' &
      //              '   0.20000000000000000E+01,'
      FRTEXT( 797 ) = '                                      ' &
      //              '   0.30000000000000000E+01 )'
      FRTEXT( 798 ) = ' FRAME_-82821_CLASS               =   ' &
      //              '   0.30000000000000000E+01'
      FRTEXT( 799 ) = ' TKFRAME_-82822_UNITS             =   ' &
      //              ' ''DEGREES'''
      FRTEXT( 800 ) = ' TKFRAME_DSS-54_TOPO_RELATIVE     =   ' &
      //              ' ''EARTH_FIXED'''
      FRTEXT( 801 ) = ' TKFRAME_-82371_RELATIVE          =   ' &
      //              ' ''CASSINI_SC_COORD'''
      FRTEXT( 802 ) = ' TKFRAME_DSS-64_TOPO_ANGLES       = ( ' &
      //              '  -0.35574930190246852E+03,'
      FRTEXT( 803 ) = '                                      ' &
      //              '  -0.49572793062938800E+02,'
      FRTEXT( 804 ) = '                                      ' &
      //              '   0.18000000000000000E+03 )'
      FRTEXT( 805 ) = ' FRAME_-82733_NAME                =   ' &
      //              ' ''CASSINI_RPWS_EZPLUS'''
      FRTEXT( 806 ) = ' TKFRAME_DSS-16_TOPO_SPEC         =   ' &
      //              ' ''ANGLES'''
      FRTEXT( 807 ) = ' TKFRAME_-82734_RELATIVE          =   ' &
      //              ' ''CASSINI_SC_COORD'''
      FRTEXT( 808 ) = ' FRAME_1399016_CLASS              =   ' &
      //              '   0.40000000000000000E+01'
      FRTEXT( 809 ) = ' TKFRAME_DSS-46_TOPO_RELATIVE     =   ' &
      //              ' ''EARTH_FIXED'''
      FRTEXT( 810 ) = ' FRAME_-82103_CLASS_ID            =   ' &
      //              '  -0.82103000000000000E+05'
      FRTEXT( 811 ) = ' FRAME_-82763_CLASS_ID            =   ' &
      //              '  -0.82763000000000000E+05'
      FRTEXT( 812 ) = ' FRAME_-82001_NAME                =   ' &
      //              ' ''CASSINI_SRU-A'''
      FRTEXT( 813 ) = ' FRAME_1399045_CLASS              =   ' &
      //              '   0.40000000000000000E+01'
      FRTEXT( 814 ) = ' TKFRAME_DSS-63_TOPO_ANGLES       = ( ' &
      //              '  -0.35575199215642402E+03,'
      FRTEXT( 815 ) = '                                      ' &
      //              '  -0.49568789683094202E+02,'
      FRTEXT( 816 ) = '                                      ' &
      //              '   0.18000000000000000E+03 )'
      FRTEXT( 817 ) = ' TKFRAME_-82760_RELATIVE          =   ' &
      //              ' ''CASSINI_SC_COORD'''
      FRTEXT( 818 ) = ' TKFRAME_-82008_SPEC              =   ' &
      //              ' ''ANGLES'''
      FRTEXT( 819 ) = ' CK_-82790_SPK                    =   ' &
      //              '  -0.82000000000000000E+02'
      FRTEXT( 820 ) = ' FRAME_DSS-34_TOPO                =   ' &
      //              '   0.13990340000000000E+07'
      FRTEXT( 821 ) = ' TKFRAME_-82812_SPEC              =   ' &
      //              ' ''ANGLES'''
      FRTEXT( 822 ) = ' CK_-82820_SPK                    =   ' &
      //              '  -0.82000000000000000E+02'
      FRTEXT( 823 ) = ' TKFRAME_DSS-42_TOPO_UNITS        =   ' &
      //              ' ''DEGREES'''
      FRTEXT( 824 ) = ' FRAME_CASSINI_VIMS_IR            =   ' &
      //              '  -0.82371000000000000E+05'
      FRTEXT( 825 ) = ' FRAME_-82844_CLASS_ID            =   ' &
      //              '  -0.82844000000000000E+05'
      FRTEXT( 826 ) = ' TKFRAME_-82845_SPEC              =   ' &
      //              ' ''ANGLES'''
      FRTEXT( 827 ) = ' TKFRAME_-82822_AXES              = ( ' &
      //              '   0.30000000000000000E+01,'
      FRTEXT( 828 ) = '                                      ' &
      //              '   0.20000000000000000E+01,'
      FRTEXT( 829 ) = '                                      ' &
      //              '   0.10000000000000000E+01 )'
      FRTEXT( 830 ) = ' FRAME_-82813_CENTER              =   ' &
      //              '  -0.82000000000000000E+02'
      FRTEXT( 831 ) = ' FRAME_CASSINI_KABAND             =   ' &
      //              '  -0.82105000000000000E+05'
      FRTEXT( 832 ) = ' TKFRAME_PARKES_TOPO_RELATIVE     =   ' &
      //              ' ''EARTH_FIXED'''
      FRTEXT( 833 ) = ' FRAME_CASSINI_VIMS_V             =   ' &
      //              '  -0.82370000000000000E+05'
      FRTEXT( 834 ) = ' FRAME_-82103_NAME                =   ' &
      //              ' ''CASSINI_LGA2'''
      FRTEXT( 835 ) = ' FRAME_-82360_NAME                =   ' &
      //              ' ''CASSINI_ISS_NAC'''
      FRTEXT( 836 ) = ' TKFRAME_-82369_ANGLES            = ( ' &
      //              '  -0.90000000000000000E+02,'
      FRTEXT( 837 ) = '                                      ' &
      //              '  -0.90000000000000000E+02,'
      FRTEXT( 838 ) = '                                      ' &
      //              '   0.00000000000000000E+00 )'
      FRTEXT( 839 ) = ' TKFRAME_DSS-63_TOPO_SPEC         =   ' &
      //              ' ''ANGLES'''
      FRTEXT( 840 ) = ' TKFRAME_31003_AXES               = ( ' &
      //              '   0.30000000000000000E+01,'
      FRTEXT( 841 ) = '                                      ' &
      //              '   0.20000000000000000E+01,'
      FRTEXT( 842 ) = '                                      ' &
      //              '   0.10000000000000000E+01 )'
      FRTEXT( 843 ) = ' FRAME_-82845_CLASS               =   ' &
      //              '   0.40000000000000000E+01'
      FRTEXT( 844 ) = ' OBJECT_399017_FRAME              =   ' &
      //              ' ''DSS-17_TOPO'''
      FRTEXT( 845 ) = ' FRAME_CASSINI_CIRS_FP1           =   ' &
      //              '  -0.82890000000000000E+05'
      FRTEXT( 846 ) = ' TKFRAME_-82891_AXES              = ( ' &
      //              '   0.30000000000000000E+01,'
      FRTEXT( 847 ) = '                                      ' &
      //              '   0.10000000000000000E+01,'
      FRTEXT( 848 ) = '                                      ' &
      //              '   0.20000000000000000E+01 )'
      FRTEXT( 849 ) = ' TKFRAME_-82370_SPEC              =   ' &
      //              ' ''ANGLES'''
      FRTEXT( 850 ) = ' FRAME_-82108_CLASS_ID            =   ' &
      //              '  -0.82108000000000000E+05'
      FRTEXT( 851 ) = ' OBJECT_399046_FRAME              =   ' &
      //              ' ''DSS-46_TOPO'''
      FRTEXT( 852 ) = ' FRAME_-82361_CLASS_ID            =   ' &
      //              '  -0.82361000000000000E+05'
      FRTEXT( 853 ) = ' TKFRAME_-82105_RELATIVE          =   ' &
      //              ' ''CASSINI_SC_COORD'''
      FRTEXT( 854 ) = ' FRAME_-82369_CLASS               =   ' &
      //              '   0.40000000000000000E+01'
      FRTEXT( 855 ) = ' TKFRAME_-82765_RELATIVE          =   ' &
      //              ' ''CASSINI_SC_COORD'''
      FRTEXT( 856 ) = ' TKFRAME_DSS-61_TOPO_ANGLES       = ( ' &
      //              '  -0.35575097846087601E+03,'
      FRTEXT( 857 ) = '                                      ' &
      //              '  -0.49571260262846202E+02,'
      FRTEXT( 858 ) = '                                      ' &
      //              '   0.18000000000000000E+03 )'
      FRTEXT( 859 ) = ' TKFRAME_-82370_UNITS             =   ' &
      //              ' ''DEGREES'''
      FRTEXT( 860 ) = ' TKFRAME_-82103_BORESIGHT         = ( ' &
      //              '   0.00000000000000000E+00,'
      FRTEXT( 861 ) = '                                      ' &
      //              '   0.00000000000000000E+00,'
      FRTEXT( 862 ) = '                                      ' &
      //              '   0.10000000000000000E+01 )'
      FRTEXT( 863 ) = ' FRAME_1399025_CENTER             =   ' &
      //              '   0.39902500000000000E+06'
      FRTEXT( 864 ) = ' TKFRAME_-82370_ANGLES            = ( ' &
      //              '   0.00000000000000000E+00,'
      FRTEXT( 865 ) = '                                      ' &
      //              '   0.00000000000000000E+00,'
      FRTEXT( 866 ) = '                                      ' &
      //              '  -0.90000000000000000E+02 )'
      FRTEXT( 867 ) = ' FRAME_-82108_CENTER              =   ' &
      //              '  -0.82000000000000000E+02'
      FRTEXT( 868 ) = ' TKFRAME_DSS-27_TOPO_AXES         = ( ' &
      //              '   0.30000000000000000E+01,'
      FRTEXT( 869 ) = '                                      ' &
      //              '   0.20000000000000000E+01,'
      FRTEXT( 870 ) = '                                      ' &
      //              '   0.30000000000000000E+01 )'
      FRTEXT( 871 ) = ' FRAME_-82107_CLASS               =   ' &
      //              '   0.40000000000000000E+01'
      FRTEXT( 872 ) = ' TKFRAME_-82108_UNITS             =   ' &
      //              ' ''DEGREES'''
      FRTEXT( 873 ) = ' FRAME_31002_CENTER               =   ' &
      //              '   0.30100000000000000E+03'
      FRTEXT( 874 ) = ' FRAME_1399023_CLASS_ID           =   ' &
      //              '   0.13990230000000000E+07'
      FRTEXT( 875 ) = ' FRAME_DSS-17_TOPO                =   ' &
      //              '   0.13990170000000000E+07'
      FRTEXT( 876 ) = ' FRAME_CASSINI_UVIS_HSP           =   ' &
      //              '  -0.82844000000000000E+05'
      FRTEXT( 877 ) = ' FRAME_-82821_NAME                =   ' &
      //              ' ''CASSINI_CAPS_ART'''
      FRTEXT( 878 ) = ' TKFRAME_-82760_ANGLES            = ( ' &
      //              '   0.00000000000000000E+00,'
      FRTEXT( 879 ) = '                                      ' &
      //              '   0.90000000000000000E+02,'
      FRTEXT( 880 ) = '                                      ' &
      //              '   0.90000000000000000E+02 )'
      FRTEXT( 881 ) = ' FRAME_1399042_CENTER             =   ' &
      //              '   0.39904200000000000E+06'
      FRTEXT( 882 ) = ' TKFRAME_DSS-61_TOPO_RELATIVE     =   ' &
      //              ' ''EARTH_FIXED'''
      FRTEXT( 883 ) = ' TKFRAME_DSS-12_TOPO_UNITS        =   ' &
      //              ' ''DEGREES'''
      FRTEXT( 884 ) = ' FRAME_CASSINI_CIRS_FP3           =   ' &
      //              '  -0.82891000000000000E+05'
      FRTEXT( 885 ) = ' FRAME_-82811_CLASS               =   ' &
      //              '   0.40000000000000000E+01'
      FRTEXT( 886 ) = ' TKFRAME_-82812_UNITS             =   ' &
      //              ' ''DEGREES'''
      FRTEXT( 887 ) = ' FRAME_DSS-53_TOPO                =   ' &
      //              '   0.13990530000000000E+07'
      FRTEXT( 888 ) = ' FRAME_-82840_CLASS               =   ' &
      //              '   0.40000000000000000E+01'
      FRTEXT( 889 ) = ' OBJECT_399012_FRAME              =   ' &
      //              ' ''DSS-12_TOPO'''
      FRTEXT( 890 ) = ' FRAME_31001_CLASS_ID             =   ' &
      //              '   0.31001000000000000E+05'
      FRTEXT( 891 ) = ' FRAME_-82890_NAME                =   ' &
      //              ' ''CASSINI_CIRS_FP1'''
      FRTEXT( 892 ) = ' TKFRAME_DSS-65_TOPO_UNITS        =   ' &
      //              ' ''DEGREES'''
      FRTEXT( 893 ) = ' TKFRAME_DSS-15_TOPO_AXES         = ( ' &
      //              '   0.30000000000000000E+01,'
      FRTEXT( 894 ) = '                                      ' &
      //              '   0.20000000000000000E+01,'
      FRTEXT( 895 ) = '                                      ' &
      //              '   0.30000000000000000E+01 )'
      FRTEXT( 896 ) = ' TKFRAME_DSS-53_TOPO_RELATIVE     =   ' &
      //              ' ''EARTH_FIXED'''
      FRTEXT( 897 ) = ' FRAME_1399055_NAME               =   ' &
      //              ' ''DSS-55_TOPO'''
      FRTEXT( 898 ) = ' FRAME_-82898_CLASS               =   ' &
      //              '   0.40000000000000000E+01'
      FRTEXT( 899 ) = ' TKFRAME_-82106_AXES              = ( ' &
      //              '   0.30000000000000000E+01,'
      FRTEXT( 900 ) = '                                      ' &
      //              '   0.20000000000000000E+01,'
      FRTEXT( 901 ) = '                                      ' &
      //              '   0.10000000000000000E+01 )'
      FRTEXT( 902 ) = ' FRAME_CASSINI_CIRS_FP4           =   ' &
      //              '  -0.82892000000000000E+05'
      FRTEXT( 903 ) = ' TKFRAME_DSS-46_TOPO_SPEC         =   ' &
      //              ' ''ANGLES'''
      FRTEXT( 904 ) = ' FRAME_1399028_CLASS_ID           =   ' &
      //              '   0.13990280000000000E+07'
      FRTEXT( 905 ) = ' FRAME_-82102_CLASS               =   ' &
      //              '   0.40000000000000000E+01'
      FRTEXT( 906 ) = ' TKFRAME_-82103_UNITS             =   ' &
      //              ' ''DEGREES'''
      FRTEXT( 907 ) = ' TKFRAME_DSS-55_TOPO_UNITS        =   ' &
      //              ' ''DEGREES'''
      FRTEXT( 908 ) = ' FRAME_1399064_CLASS              =   ' &
      //              '   0.40000000000000000E+01'
      FRTEXT( 909 ) = ' FRAME_CASSINI_MIMI_LEMMS_ART     =   ' &
      //              '  -0.82764000000000000E+05'
      FRTEXT( 910 ) = ' TKFRAME_DSS-45_TOPO_RELATIVE     =   ' &
      //              ' ''EARTH_FIXED'''
      FRTEXT( 911 ) = ' CK_-82762_SPK                    =   ' &
      //              '  -0.82000000000000000E+02'
      FRTEXT( 912 ) = ' FRAME_1399054_CLASS_ID           =   ' &
      //              '   0.13990540000000000E+07'
      FRTEXT( 913 ) = ' FRAME_-82000_CLASS               =   ' &
      //              '   0.30000000000000000E+01'
      FRTEXT( 914 ) = ' TKFRAME_-82001_UNITS             =   ' &
      //              ' ''DEGREES'''
      FRTEXT( 915 ) = ' CK_-82821_SCLK                   =   ' &
      //              '  -0.82000000000000000E+02'
      FRTEXT( 916 ) = ' TKFRAME_-82368_RELATIVE          =   ' &
      //              ' ''CASSINI_SC_COORD'''
      FRTEXT( 917 ) = ' FRAME_-82812_CENTER              =   ' &
      //              '  -0.82000000000000000E+02'
      FRTEXT( 918 ) = ' TKFRAME_DSS-45_TOPO_UNITS        =   ' &
      //              ' ''DEGREES'''
      FRTEXT( 919 ) = ' FRAME_-82378_CENTER              =   ' &
      //              '  -0.82000000000000000E+02'
      FRTEXT( 920 ) = ' TKFRAME_-82368_ANGLES            = ( ' &
      //              '  -0.90000000000000000E+02,'
      FRTEXT( 921 ) = '                                      ' &
      //              '  -0.90000000000000000E+02,'
      FRTEXT( 922 ) = '                                      ' &
      //              '   0.00000000000000000E+00 )'
      FRTEXT( 923 ) = ' TKFRAME_DSS-34_TOPO_SPEC         =   ' &
      //              ' ''ANGLES'''
      FRTEXT( 924 ) = ' TKFRAME_31003_ANGLES             = ( ' &
      //              '   0.63898600000000002E+02,'
      FRTEXT( 925 ) = '                                      ' &
      //              '   0.79076800000000006E+02,'
      FRTEXT( 926 ) = '                                      ' &
      //              '   0.14620000000000000E+00 )'
      FRTEXT( 927 ) = ' TKFRAME_-82814_SPEC              =   ' &
      //              ' ''ANGLES'''
      FRTEXT( 928 ) = ' FRAME_-82840_NAME                =   ' &
      //              ' ''CASSINI_UVIS_FUV'''
      FRTEXT( 929 ) = ' FRAME_-82845_CENTER              =   ' &
      //              '  -0.82000000000000000E+02'
      FRTEXT( 930 ) = ' FRAME_-82733_CLASS               =   ' &
      //              '   0.40000000000000000E+01'
      FRTEXT( 931 ) = ' FRAME_1399005_NAME               =   ' &
      //              ' ''PARKES_TOPO'''
      FRTEXT( 932 ) = ' TKFRAME_-82734_UNITS             =   ' &
      //              ' ''DEGREES'''
      FRTEXT( 933 ) = ' FRAME_-82893_CLASS               =   ' &
      //              '   0.40000000000000000E+01'
      FRTEXT( 934 ) = ' TKFRAME_-82350_RELATIVE          =   ' &
      //              ' ''CASSINI_SC_COORD'''
      FRTEXT( 935 ) = ' TKFRAME_-82360_UNITS             =   ' &
      //              ' ''DEGREES'''
      FRTEXT( 936 ) = ' OBJECT_399065_FRAME              =   ' &
      //              ' ''DSS-65_TOPO'''
      FRTEXT( 937 ) = ' FRAME_-82105_NAME                =   ' &
      //              ' ''CASSINI_KABAND'''
      FRTEXT( 938 ) = ' FRAME_-82001_CLASS_ID            =   ' &
      //              '  -0.82001000000000000E+05'
      FRTEXT( 939 ) = ' FRAME_-82762_CLASS               =   ' &
      //              '   0.30000000000000000E+01'
      FRTEXT( 940 ) = ' FRAME_1399024_CENTER             =   ' &
      //              '   0.39902400000000000E+06'
      FRTEXT( 941 ) = ' TKFRAME_-82369_SPEC              =   ' &
      //              ' ''ANGLES'''
      FRTEXT( 942 ) = ' FRAME_-82107_CENTER              =   ' &
      //              '  -0.82000000000000000E+02'
      FRTEXT( 943 ) = ' FRAME_-82791_CLASS               =   ' &
      //              '   0.30000000000000000E+01'
      FRTEXT( 944 ) = ' TKFRAME_-82792_UNITS             =   ' &
      //              ' ''DEGREES'''
      FRTEXT( 945 ) = ' FRAME_31001_CENTER               =   ' &
      //              '   0.30100000000000000E+03'
      FRTEXT( 946 ) = ' FRAME_CASSINI_CAPS               =   ' &
      //              '  -0.82820000000000000E+05'
      FRTEXT( 947 ) = ' TKFRAME_-82893_AXES              = ( ' &
      //              '   0.30000000000000000E+01,'
      FRTEXT( 948 ) = '                                      ' &
      //              '   0.10000000000000000E+01,'
      FRTEXT( 949 ) = '                                      ' &
      //              '   0.20000000000000000E+01 )'
      FRTEXT( 950 ) = ' TKFRAME_-82372_SPEC              =   ' &
      //              ' ''ANGLES'''
      FRTEXT( 951 ) = ' FRAME_CASSINI_ISS_NAC            =   ' &
      //              '  -0.82360000000000000E+05'
      FRTEXT( 952 ) = ' FRAME_1399015_CLASS_ID           =   ' &
      //              '   0.13990150000000000E+07'
      FRTEXT( 953 ) = ' TKFRAME_DSS-25_TOPO_UNITS        =   ' &
      //              ' ''DEGREES'''
      FRTEXT( 954 ) = ' FRAME_-82893_CLASS_ID            =   ' &
      //              '  -0.82893000000000000E+05'
      FRTEXT( 955 ) = ' TKFRAME_-82890_RELATIVE          =   ' &
      //              ' ''CASSINI_CIRS_FPB'''
      FRTEXT( 956 ) = ' TKFRAME_-82731_SPEC              =   ' &
      //              ' ''ANGLES'''
      FRTEXT( 957 ) = ' TKFRAME_DSS-15_TOPO_UNITS        =   ' &
      //              ' ''DEGREES'''
      FRTEXT( 958 ) = ' FRAME_-82790_NAME                =   ' &
      //              ' ''CASSINI_CDA'''
      FRTEXT( 959 ) = ' TKFRAME_-82792_ANGLES            = ( ' &
      //              '   0.15000000000000000E+03,'
      FRTEXT( 960 ) = '                                      ' &
      //              '   0.00000000000000000E+00,'
      FRTEXT( 961 ) = '                                      ' &
      //              '   0.10500000000000000E+03 )'
      FRTEXT( 962 ) = ' TKFRAME_DSS-45_TOPO_AXES         = ( ' &
      //              '   0.30000000000000000E+01,'
      FRTEXT( 963 ) = '                                      ' &
      //              '   0.20000000000000000E+01,'
      FRTEXT( 964 ) = '                                      ' &
      //              '   0.30000000000000000E+01 )'
      FRTEXT( 965 ) = ' FRAME_DSS-55_TOPO                =   ' &
      //              '   0.13990550000000000E+07'
      FRTEXT( 966 ) = ' TKFRAME_DSS-17_TOPO_SPEC         =   ' &
      //              ' ''ANGLES'''
      FRTEXT( 967 ) = ' FRAME_CASSINI_MIMI_LEMMS1        =   ' &
      //              '  -0.82762000000000000E+05'
      FRTEXT( 968 ) = ' FRAME_1399025_CLASS              =   ' &
      //              '   0.40000000000000000E+01'
      FRTEXT( 969 ) = ' TKFRAME_-82810_AXES              = ( ' &
      //              '   0.10000000000000000E+01,'
      FRTEXT( 970 ) = '                                      ' &
      //              '   0.20000000000000000E+01,'
      FRTEXT( 971 ) = '                                      ' &
      //              '   0.30000000000000000E+01 )'
      FRTEXT( 972 ) = ' FRAME_-82378_NAME                =   ' &
      //              ' ''CASSINI_VIMS_RAD'''
      FRTEXT( 973 ) = ' TKFRAME_-82843_AXES              = ( ' &
      //              '   0.30000000000000000E+01,'
      FRTEXT( 974 ) = '                                      ' &
      //              '   0.20000000000000000E+01,'
      FRTEXT( 975 ) = '                                      ' &
      //              '   0.10000000000000000E+01 )'
      FRTEXT( 976 ) = ' FRAME_-82892_NAME                =   ' &
      //              ' ''CASSINI_CIRS_FP4'''
      FRTEXT( 977 ) = ' FRAME_1399024_NAME               =   ' &
      //              ' ''DSS-24_TOPO'''
      FRTEXT( 978 ) = ' FRAME_1399054_CLASS              =   ' &
      //              '   0.40000000000000000E+01'
      FRTEXT( 979 ) = ' FRAME_1399046_CLASS_ID           =   ' &
      //              '   0.13990460000000000E+07'
      FRTEXT( 980 ) = ' FRAME_-82898_CLASS_ID            =   ' &
      //              '  -0.82898000000000000E+05'
      FRTEXT( 981 ) = ' TKFRAME_-82108_AXES              = ( ' &
      //              '   0.30000000000000000E+01,'
      FRTEXT( 982 ) = '                                      ' &
      //              '   0.10000000000000000E+01,'
      FRTEXT( 983 ) = '                                      ' &
      //              '   0.30000000000000000E+01 )'
      FRTEXT( 984 ) = ' FRAME_-82811_CENTER              =   ' &
      //              '  -0.82000000000000000E+02'
      FRTEXT( 985 ) = ' TKFRAME_-82101_SPEC              =   ' &
      //              ' ''ANGLES'''
      FRTEXT( 986 ) = ' FRAME_CASSINI_MIMI_LEMMS2        =   ' &
      //              '  -0.82763000000000000E+05'
      FRTEXT( 987 ) = ' TKFRAME_-82008_RELATIVE          =   ' &
      //              ' ''CASSINI_SC_COORD'''
      FRTEXT( 988 ) = ' FRAME_-82734_CENTER              =   ' &
      //              '  -0.82000000000000000E+02'
      FRTEXT( 989 ) = ' FRAME_-82361_CENTER              =   ' &
      //              '  -0.82000000000000000E+02'
      FRTEXT( 990 ) = ' TKFRAME_-82351_ANGLES            = ( ' &
      //              '   0.00000000000000000E+00,'
      FRTEXT( 991 ) = '                                      ' &
      //              '   0.90000000000000000E+02,'
      FRTEXT( 992 ) = '                                      ' &
      //              '   0.00000000000000000E+00 )'
      FRTEXT( 993 ) = ' TKFRAME_DSS-33_TOPO_AXES         = ( ' &
      //              '   0.30000000000000000E+01,'
      FRTEXT( 994 ) = '                                      ' &
      //              '   0.20000000000000000E+01,'
      FRTEXT( 995 ) = '                                      ' &
      //              '   0.30000000000000000E+01 )'
      FRTEXT( 996 ) = ' CK_-82790_SCLK                   =   ' &
      //              '  -0.82000000000000000E+02'
      FRTEXT( 997 ) = ' FRAME_CASSINI_CIRS_FPB           =   ' &
      //              '  -0.82893000000000000E+05'
      FRTEXT( 998 ) = ' FRAME_-82740_NAME                =   ' &
      //              ' ''CASSINI_INMS'''
      FRTEXT( 999 ) = ' OBJECT_399026_FRAME              =   ' &
      //              ' ''DSS-26_TOPO'''
      FRTEXT( 1000 ) = ' FRAME_CASSINI_SRU-A              =  ' &
      //               '   -0.82001000000000000E+05'
      FRTEXT( 1001 ) = ' TKFRAME_DSS-64_TOPO_SPEC         =  ' &
      //               '  ''ANGLES'''
      FRTEXT( 1002 ) = ' FRAME_-82844_CENTER              =  ' &
      //               '   -0.82000000000000000E+02'
      FRTEXT( 1003 ) = ' FRAME_-82810_CLASS_ID            =  ' &
      //               '   -0.82810000000000000E+05'
      FRTEXT( 1004 ) = ' TKFRAME_-82350_UNITS             =  ' &
      //               '  ''DEGREES'''
      FRTEXT( 1005 ) = ' OBJECT_399055_FRAME              =  ' &
      //               '  ''DSS-55_TOPO'''
      FRTEXT( 1006 ) = ' FRAME_-82378_CLASS               =  ' &
      //               '    0.40000000000000000E+01'
      FRTEXT( 1007 ) = ' FRAME_1399023_CENTER             =  ' &
      //               '    0.39902300000000000E+06'
      FRTEXT( 1008 ) = ' FRAME_-82106_CENTER              =  ' &
      //               '   -0.82000000000000000E+02'
      FRTEXT( 1009 ) = ' FRAME_-82371_CLASS_ID            =  ' &
      //               '   -0.82371000000000000E+05'
      FRTEXT( 1010 ) = ' TKFRAME_-82760_AXES              = (' &
      //               '    0.10000000000000000E+01,'
      FRTEXT( 1011 ) = '                                     ' &
      //               '    0.20000000000000000E+01,'
      FRTEXT( 1012 ) = '                                     ' &
      //               '    0.30000000000000000E+01 )'
      FRTEXT( 1013 ) = ' FRAME_31000_CENTER               =  ' &
      //               '    0.30100000000000000E+03'
      FRTEXT( 1014 ) = ' FRAME_1399049_CLASS              =  ' &
      //               '    0.40000000000000000E+01'
      FRTEXT( 1015 ) = ' FRAME_-82842_NAME                =  ' &
      //               '  ''CASSINI_UVIS_EUV'''
      FRTEXT( 1016 ) = ' FRAME_CASSINI_SRU-B              =  ' &
      //               '   -0.82002000000000000E+05'
      FRTEXT( 1017 ) = ' TKFRAME_DSS-28_TOPO_RELATIVE     =  ' &
      //               '  ''EARTH_FIXED'''
      FRTEXT( 1018 ) = ' TKFRAME_DSS-28_TOPO_AXES         = (' &
      //               '    0.30000000000000000E+01,'
      FRTEXT( 1019 ) = '                                     ' &
      //               '    0.20000000000000000E+01,'
      FRTEXT( 1020 ) = '                                     ' &
      //               '    0.30000000000000000E+01 )'
      FRTEXT( 1021 ) = ' FRAME_-82734_CLASS_ID            =  ' &
      //               '   -0.82734000000000000E+05'
      FRTEXT( 1022 ) = ' FRAME_-82107_NAME                =  ' &
      //               '  ''CASSINI_SBAND'''
      FRTEXT( 1023 ) = ' TKFRAME_-82731_RELATIVE          =  ' &
      //               '  ''CASSINI_SC_COORD'''
      FRTEXT( 1024 ) = ' FRAME_31001_CLASS                =  ' &
      //               '    0.40000000000000000E+01'
      FRTEXT( 1025 ) = ' TKFRAME_DSS-28_TOPO_UNITS        =  ' &
      //               '  ''DEGREES'''
      FRTEXT( 1026 ) = ' FRAME_1399033_CLASS_ID           =  ' &
      //               '    0.13990330000000000E+07'
      FRTEXT( 1027 ) = ' FRAME_-82760_CLASS_ID            =  ' &
      //               '   -0.82760000000000000E+05'
      FRTEXT( 1028 ) = ' FRAME_1399043_NAME               =  ' &
      //               '  ''DSS-43_TOPO'''
      FRTEXT( 1029 ) = ' TKFRAME_31000_SPEC               =  ' &
      //               '  ''MATRIX'''
      FRTEXT( 1030 ) = ' TKFRAME_-82351_AXES              = (' &
      //               '    0.10000000000000000E+01,'
      FRTEXT( 1031 ) = '                                     ' &
      //               '    0.20000000000000000E+01,'
      FRTEXT( 1032 ) = '                                     ' &
      //               '    0.30000000000000000E+01 )'
      FRTEXT( 1033 ) = ' FRAME_-82820_CLASS               =  ' &
      //               '    0.30000000000000000E+01'
      FRTEXT( 1034 ) = ' FRAME_CASSINI_MIMI_LEMMS_BASE    =  ' &
      //               '   -0.82765000000000000E+05'
      FRTEXT( 1035 ) = ' TKFRAME_-82812_RELATIVE          =  ' &
      //               '  ''CASSINI_SC_COORD'''
      FRTEXT( 1036 ) = ' FRAME_CASSINI_CDA_BASE           =  ' &
      //               '   -0.82792000000000000E+05'
      FRTEXT( 1037 ) = ' TKFRAME_DSS-16_TOPO_AXES         = (' &
      //               '    0.30000000000000000E+01,'
      FRTEXT( 1038 ) = '                                     ' &
      //               '    0.20000000000000000E+01,'
      FRTEXT( 1039 ) = '                                     ' &
      //               '    0.30000000000000000E+01 )'
      FRTEXT( 1040 ) = ' FRAME_1399015_CLASS              =  ' &
      //               '    0.40000000000000000E+01'
      FRTEXT( 1041 ) = ' TKFRAME_-82733_SPEC              =  ' &
      //               '  ''ANGLES'''
      FRTEXT( 1042 ) = ' FRAME_-82792_NAME                =  ' &
      //               '  ''CASSINI_CDA_BASE'''
      FRTEXT( 1043 ) = ' FRAME_-82105_CLASS_ID            =  ' &
      //               '   -0.82105000000000000E+05'
      FRTEXT( 1044 ) = ' FRAME_PARKES_TOPO                =  ' &
      //               '    0.13990050000000000E+07'
      FRTEXT( 1045 ) = ' FRAME_-82765_CLASS_ID            =  ' &
      //               '   -0.82765000000000000E+05'
      FRTEXT( 1046 ) = ' TKFRAME_-82102_RELATIVE          =  ' &
      //               '  ''CASSINI_SC_COORD'''
      FRTEXT( 1047 ) = ' FRAME_DSS-24_TOPO                =  ' &
      //               '    0.13990240000000000E+07'
      FRTEXT( 1048 ) = ' TKFRAME_-82008_AXES              = (' &
      //               '    0.30000000000000000E+01,'
      FRTEXT( 1049 ) = '                                     ' &
      //               '    0.10000000000000000E+01,'
      FRTEXT( 1050 ) = '                                     ' &
      //               '    0.30000000000000000E+01 )'
      FRTEXT( 1051 ) = ' FRAME_CASSINI_UVIS_HDAC          =  ' &
      //               '   -0.82845000000000000E+05'
      FRTEXT( 1052 ) = ' FRAME_-82810_CENTER              =  ' &
      //               '   -0.82000000000000000E+02'
      FRTEXT( 1053 ) = ' FRAME_-82009_CLASS               =  ' &
      //               '    0.40000000000000000E+01'
      FRTEXT( 1054 ) = ' TKFRAME_-82001_SPEC              =  ' &
      //               '  ''ANGLES'''
      FRTEXT( 1055 ) = ' FRAME_1399064_CLASS_ID           =  ' &
      //               '    0.13990640000000000E+07'
      FRTEXT( 1056 ) = ' FRAME_-82791_CLASS_ID            =  ' &
      //               '   -0.82791000000000000E+05'
      FRTEXT( 1057 ) = ' TKFRAME_-82812_AXES              = (' &
      //               '    0.10000000000000000E+01,'
      FRTEXT( 1058 ) = '                                     ' &
      //               '    0.20000000000000000E+01,'
      FRTEXT( 1059 ) = '                                     ' &
      //               '    0.30000000000000000E+01 )'
      FRTEXT( 1060 ) = ' FRAME_-82733_CENTER              =  ' &
      //               '   -0.82000000000000000E+02'
      FRTEXT( 1061 ) = ' FRAME_-82360_CENTER              =  ' &
      //               '   -0.82000000000000000E+02'
      FRTEXT( 1062 ) = ' FRAME_1399005_CENTER             =  ' &
      //               '    0.39900500000000000E+06'
      FRTEXT( 1063 ) = ' TKFRAME_-82350_ANGLES            = (' &
      //               '    0.00000000000000000E+00,'
      FRTEXT( 1064 ) = '                                     ' &
      //               '   -0.90000000000000000E+02,'
      FRTEXT( 1065 ) = '                                     ' &
      //               '    0.00000000000000000E+00 )'
      FRTEXT( 1066 ) = ' TKFRAME_-82845_AXES              = (' &
      //               '    0.30000000000000000E+01,'
      FRTEXT( 1067 ) = '                                     ' &
      //               '    0.20000000000000000E+01,'
      FRTEXT( 1068 ) = '                                     ' &
      //               '    0.10000000000000000E+01 )'
      FRTEXT( 1069 ) = ' TKFRAME_-82378_RELATIVE          =  ' &
      //               '  ''CASSINI_SC_COORD'''
      FRTEXT( 1070 ) = ' FRAME_1399026_NAME               =  ' &
      //               '  ''DSS-26_TOPO'''
      FRTEXT( 1071 ) = ' FRAME_-82350_NAME                =  ' &
      //               '  ''CASSINI_MAG_PLUS'''
      FRTEXT( 1072 ) = ' TKFRAME_-82843_RELATIVE          =  ' &
      //               '  ''CASSINI_SC_COORD'''
      FRTEXT( 1073 ) = ' TKFRAME_DSS-43_TOPO_RELATIVE     =  ' &
      //               '  ''EARTH_FIXED'''
      FRTEXT( 1074 ) = ' FRAME_-82844_CLASS               =  ' &
      //               '    0.40000000000000000E+01'
      FRTEXT( 1075 ) = ' TKFRAME_-82845_UNITS             =  ' &
      //               '  ''DEGREES'''
      FRTEXT( 1076 ) = ' TKFRAME_DSS-63_TOPO_AXES         = (' &
      //               '    0.30000000000000000E+01,'
      FRTEXT( 1077 ) = '                                     ' &
      //               '    0.20000000000000000E+01,'
      FRTEXT( 1078 ) = '                                     ' &
      //               '    0.30000000000000000E+01 )'
      FRTEXT( 1079 ) = ' OBJECT_399016_FRAME              =  ' &
      //               '  ''DSS-16_TOPO'''
      FRTEXT( 1080 ) = ' FRAME_-82843_CENTER              =  ' &
      //               '   -0.82000000000000000E+02'
      FRTEXT( 1081 ) = ' TKFRAME_-82103_SPEC              =  ' &
      //               '  ''ANGLES'''
      FRTEXT( 1082 ) = ' TKFRAME_-82360_SPEC              =  ' &
      //               '  ''ANGLES'''
      FRTEXT( 1083 ) = ' OBJECT_399045_FRAME              =  ' &
      //               '  ''DSS-45_TOPO'''
      FRTEXT( 1084 ) = ' TKFRAME_-82370_AXES              = (' &
      //               '    0.30000000000000000E+01,'
      FRTEXT( 1085 ) = '                                     ' &
      //               '    0.20000000000000000E+01,'
      FRTEXT( 1086 ) = '                                     ' &
      //               '    0.10000000000000000E+01 )'
      FRTEXT( 1087 ) = ' FRAME_-82368_CLASS               =  ' &
      //               '    0.40000000000000000E+01'
      FRTEXT( 1088 ) = ' TKFRAME_-82369_UNITS             =  ' &
      //               '  ''DEGREES'''
      FRTEXT( 1089 ) = ' TKFRAME_-82740_ANGLES            = (' &
      //               '    0.00000000000000000E+00,'
      FRTEXT( 1090 ) = '                                     ' &
      //               '    0.90000000000000000E+02,'
      FRTEXT( 1091 ) = '                                     ' &
      //               '    0.00000000000000000E+00 )'
      FRTEXT( 1092 ) = ' FRAME_-82105_CENTER              =  ' &
      //               '   -0.82000000000000000E+02'
      FRTEXT( 1093 ) = ' TKFRAME_-82107_RELATIVE          =  ' &
      //               '  ''CASSINI_SC_COORD'''
      FRTEXT( 1094 ) = ' TKFRAME_-82360_RELATIVE          =  ' &
      //               '  ''CASSINI_SC_COORD'''
      FRTEXT( 1095 ) = ' TKFRAME_-82002_ANGLES            = (' &
      //               '    0.00000000000000000E+00,'
      FRTEXT( 1096 ) = '                                     ' &
      //               '   -0.90000000000000000E+02,'
      FRTEXT( 1097 ) = '                                     ' &
      //               '    0.00000000000000000E+00 )'
      FRTEXT( 1098 ) = ' FRAME_-82106_CLASS               =  ' &
      //               '    0.40000000000000000E+01'
      FRTEXT( 1099 ) = ' TKFRAME_-82107_UNITS             =  ' &
      //               '  ''DEGREES'''
      FRTEXT( 1100 ) = ' FRAME_1399055_CENTER             =  ' &
      //               '    0.39905500000000000E+06'
      FRTEXT( 1101 ) = ' TKFRAME_DSS-27_TOPO_RELATIVE     =  ' &
      //               '  ''EARTH_FIXED'''
      FRTEXT( 1102 ) = ' FRAME_1399025_CLASS_ID           =  ' &
      //               '    0.13990250000000000E+07'
      FRTEXT( 1103 ) = ' FRAME_-82811_NAME                =  ' &
      //               '  ''CASSINI_RADAR_2'''
      FRTEXT( 1104 ) = ' TKFRAME_DSS-23_TOPO_SPEC         =  ' &
      //               '  ''ANGLES'''
      FRTEXT( 1105 ) = ' FRAME_-82844_NAME                =  ' &
      //               '  ''CASSINI_UVIS_HSP'''
      FRTEXT( 1106 ) = ' FRAME_-82810_CLASS               =  ' &
      //               '    0.40000000000000000E+01'
      FRTEXT( 1107 ) = ' TKFRAME_-82811_UNITS             =  ' &
      //               '  ''DEGREES'''
      FRTEXT( 1108 ) = ' FRAME_DSS-43_TOPO                =  ' &
      //               '    0.13990430000000000E+07'
      FRTEXT( 1109 ) = ' FRAME_-82893_CENTER              =  ' &
      //               '   -0.82000000000000000E+02'
      FRTEXT( 1110 ) = ' TKFRAME_-82840_UNITS             =  ' &
      //               '  ''DEGREES'''
      FRTEXT( 1111 ) = ' FRAME_-82368_CLASS_ID            =  ' &
      //               '   -0.82368000000000000E+05'
      FRTEXT( 1112 ) = ' FRAME_1399012_NAME               =  ' &
      //               '  ''DSS-12_TOPO'''
      FRTEXT( 1113 ) = ' FRAME_31003_CLASS_ID             =  ' &
      //               '    0.31003000000000000E+05'
      FRTEXT( 1114 ) = ' FRAME_1399045_NAME               =  ' &
      //               '  ''DSS-45_TOPO'''
      FRTEXT( 1115 ) = ' TKFRAME_-82898_UNITS             =  ' &
      //               '  ''DEGREES'''
      FRTEXT( 1116 ) = ' FRAME_CASSINI_RPWS_EXMINUS       =  ' &
      //               '   -0.82732000000000000E+05'
      FRTEXT( 1117 ) = ' FRAME_1399005_CLASS              =  ' &
      //               '    0.40000000000000000E+01'
      FRTEXT( 1118 ) = ' TKFRAME_-82890_SPEC              =  ' &
      //               '  ''ANGLES'''
      FRTEXT( 1119 ) = ' TKFRAME_DSS-46_TOPO_AXES         = (' &
      //               '    0.30000000000000000E+01,'
      FRTEXT( 1120 ) = '                                     ' &
      //               '    0.20000000000000000E+01,'
      FRTEXT( 1121 ) = '                                     ' &
      //               '    0.30000000000000000E+01 )'
      FRTEXT( 1122 ) = ' FRAME_-82101_CLASS               =  ' &
      //               '    0.40000000000000000E+01'
      FRTEXT( 1123 ) = ' FRAME_1399034_CLASS              =  ' &
      //               '    0.40000000000000000E+01'
      FRTEXT( 1124 ) = ' TKFRAME_-82102_UNITS             =  ' &
      //               '  ''DEGREES'''
      FRTEXT( 1125 ) = ' FRAME_CASSINI_UVIS_FUV           =  ' &
      //               '   -0.82840000000000000E+05'
      FRTEXT( 1126 ) = ' FRAME_-82350_CLASS_ID            =  ' &
      //               '   -0.82350000000000000E+05'
      FRTEXT( 1127 ) = ' FRAME_1399063_CLASS              =  ' &
      //               '    0.40000000000000000E+01'
      FRTEXT( 1128 ) = ' FRAME_CASSINI_SRU-A_RAD          =  ' &
      //               '   -0.82008000000000000E+05'
      FRTEXT( 1129 ) = ' FRAME_-82732_CENTER              =  ' &
      //               '   -0.82000000000000000E+02'
      FRTEXT( 1130 ) = ' FRAME_-82761_NAME                =  ' &
      //               '  ''CASSINI_MIMI_INCA'''
      FRTEXT( 1131 ) = ' TKFRAME_DSS-61_TOPO_UNITS        =  ' &
      //               '  ''DEGREES'''
      FRTEXT( 1132 ) = ' FRAME_1399012_CLASS_ID           =  ' &
      //               '    0.13990120000000000E+07'
      FRTEXT( 1133 ) = ' FRAME_DSS-26_TOPO                =  ' &
      //               '    0.13990260000000000E+07'
      FRTEXT( 1134 ) = ' FRAME_-82842_CENTER              =  ' &
      //               '   -0.82000000000000000E+02'
      FRTEXT( 1135 ) = ' TKFRAME_DSS-34_TOPO_AXES         = (' &
      //               '    0.30000000000000000E+01,'
      FRTEXT( 1136 ) = '                                     ' &
      //               '    0.20000000000000000E+01,'
      FRTEXT( 1137 ) = '                                     ' &
      //               '    0.30000000000000000E+01 )'
      FRTEXT( 1138 ) = ' TKFRAME_-82814_AXES              = (' &
      //               '    0.10000000000000000E+01,'
      FRTEXT( 1139 ) = '                                     ' &
      //               '    0.20000000000000000E+01,'
      FRTEXT( 1140 ) = '                                     ' &
      //               '    0.30000000000000000E+01 )'
      FRTEXT( 1141 ) = ' FRAME_-82765_CENTER              =  ' &
      //               '   -0.82000000000000000E+02'
      FRTEXT( 1142 ) = ' TKFRAME_DSS-42_TOPO_RELATIVE     =  ' &
      //               '  ''EARTH_FIXED'''
      FRTEXT( 1143 ) = ' FRAME_-82732_CLASS               =  ' &
      //               '    0.40000000000000000E+01'
      FRTEXT( 1144 ) = ' TKFRAME_-82733_UNITS             =  ' &
      //               '  ''DEGREES'''
      FRTEXT( 1145 ) = ' FRAME_-82892_CLASS               =  ' &
      //               '    0.40000000000000000E+01'
      FRTEXT( 1146 ) = ' TKFRAME_-82893_UNITS             =  ' &
      //               '  ''DEGREES'''
      FRTEXT( 1147 ) = ' FRAME_-82104_CENTER              =  ' &
      //               '   -0.82000000000000000E+02'
      FRTEXT( 1148 ) = ' TKFRAME_DSS-65_TOPO_SPEC         =  ' &
      //               '  ''ANGLES'''
      FRTEXT( 1149 ) = ' FRAME_-82890_CLASS_ID            =  ' &
      //               '   -0.82890000000000000E+05'
      FRTEXT( 1150 ) = ' OBJECT_399064_FRAME              =  ' &
      //               '  ''DSS-64_TOPO'''
      FRTEXT( 1151 ) = ' FRAME_1399028_NAME               =  ' &
      //               '  ''DSS-28_TOPO'''
      FRTEXT( 1152 ) = ' FRAME_-82761_CLASS               =  ' &
      //               '    0.40000000000000000E+01'
      FRTEXT( 1153 ) = ' FRAME_-82820_CLASS_ID            =  ' &
      //               '   -0.82820000000000000E+05'
      FRTEXT( 1154 ) = ' TKFRAME_-82840_SPEC              =  ' &
      //               '  ''ANGLES'''
      FRTEXT( 1155 ) = ' TKFRAME_-82001_ANGLES            = (' &
      //               '    0.00000000000000000E+00,'
      FRTEXT( 1156 ) = '                                     ' &
      //               '   -0.90000000000000000E+02,'
      FRTEXT( 1157 ) = '                                     ' &
      //               '    0.00000000000000000E+00 )'
      FRTEXT( 1158 ) = ' TKFRAME_-82369_AXES              = (' &
      //               '    0.30000000000000000E+01,'
      FRTEXT( 1159 ) = '                                     ' &
      //               '    0.10000000000000000E+01,'
      FRTEXT( 1160 ) = '                                     ' &
      //               '    0.30000000000000000E+01 )'
      FRTEXT( 1161 ) = ' FRAME_-82790_CLASS               =  ' &
      //               '    0.30000000000000000E+01'
      FRTEXT( 1162 ) = ' TKFRAME_-82105_SPEC              =  ' &
      //               '  ''ANGLES'''
      FRTEXT( 1163 ) = ' FRAME_1399064_NAME               =  ' &
      //               '  ''DSS-64_TOPO'''
      FRTEXT( 1164 ) = ' TKFRAME_DSS-34_TOPO_RELATIVE     =  ' &
      //               '  ''EARTH_FIXED'''
      FRTEXT( 1165 ) = ' TKFRAME_-82372_AXES              = (' &
      //               '    0.30000000000000000E+01,'
      FRTEXT( 1166 ) = '                                     ' &
      //               '    0.20000000000000000E+01,'
      FRTEXT( 1167 ) = '                                     ' &
      //               '    0.10000000000000000E+01 )'
      FRTEXT( 1168 ) = ' FRAME_CASSINI_RADAR_1            =  ' &
      //               '   -0.82810000000000000E+05'
      FRTEXT( 1169 ) = ' TKFRAME_31001_MATRIX             = (' &
      //               '    0.10000000000000000E+01,'
      FRTEXT( 1170 ) = '                                     ' &
      //               '    0.00000000000000000E+00,'
      FRTEXT( 1171 ) = '                                     ' &
      //               '    0.00000000000000000E+00,'
      FRTEXT( 1172 ) = '                                     ' &
      //               '    0.00000000000000000E+00,'
      FRTEXT( 1173 ) = '                                     ' &
      //               '    0.10000000000000000E+01,'
      FRTEXT( 1174 ) = '                                     ' &
      //               '    0.00000000000000000E+00,'
      FRTEXT( 1175 ) = '                                     ' &
      //               '    0.00000000000000000E+00,'
      FRTEXT( 1176 ) = '                                     ' &
      //               '    0.00000000000000000E+00,'
      FRTEXT( 1177 ) = '                                     ' &
      //               '    0.10000000000000000E+01 )'
      FRTEXT( 1178 ) = ' FRAME_1399054_CENTER             =  ' &
      //               '    0.39905400000000000E+06'
      FRTEXT( 1179 ) = ' FRAME_1399017_CLASS_ID           =  ' &
      //               '    0.13990170000000000E+07'
      FRTEXT( 1180 ) = ' FRAME_1399043_CLASS_ID           =  ' &
      //               '    0.13990430000000000E+07'
      FRTEXT( 1181 ) = ' FRAME_-82892_CENTER              =  ' &
      //               '   -0.82000000000000000E+02'
      FRTEXT( 1182 ) = ' TKFRAME_DSS-26_TOPO_RELATIVE     =  ' &
      //               '  ''EARTH_FIXED'''
      FRTEXT( 1183 ) = ' TKFRAME_DSS-53_TOPO_SPEC         =  ' &
      //               '  ''ANGLES'''
      FRTEXT( 1184 ) = ' TKFRAME_-82898_ANGLES            = (' &
      //               '   -0.90000000000000000E+02,'
      FRTEXT( 1185 ) = '                                     ' &
      //               '   -0.90000000000000000E+02,'
      FRTEXT( 1186 ) = '                                     ' &
      //               '    0.00000000000000000E+00 )'
      FRTEXT( 1187 ) = ' FRAME_-82009_NAME                =  ' &
      //               '  ''CASSINI_SRU-B_RAD'''
      FRTEXT( 1188 ) = ' FRAME_CASSINI_RADAR_2            =  ' &
      //               '   -0.82811000000000000E+05'
      FRTEXT( 1189 ) = ' TKFRAME_-82892_RELATIVE          =  ' &
      //               '  ''CASSINI_CIRS_FPB'''
      FRTEXT( 1190 ) = ' TKFRAME_-82731_AXES              = (' &
      //               '    0.30000000000000000E+01,'
      FRTEXT( 1191 ) = '                                     ' &
      //               '    0.20000000000000000E+01,'
      FRTEXT( 1192 ) = '                                     ' &
      //               '    0.10000000000000000E+01 )'
      FRTEXT( 1193 ) = ' TKFRAME_-82822_RELATIVE          =  ' &
      //               '  ''CASSINI_SC_COORD'''
      FRTEXT( 1194 ) = ' FRAME_-82008_CLASS_ID            =  ' &
      //               '   -0.82008000000000000E+05'
      FRTEXT( 1195 ) = ' FRAME_CASSINI_CDA                =  ' &
      //               '   -0.82790000000000000E+05'
      FRTEXT( 1196 ) = ' FRAME_-82813_NAME                =  ' &
      //               '  ''CASSINI_RADAR_4'''
      FRTEXT( 1197 ) = ' FRAME_DSS-12_TOPO                =  ' &
      //               '    0.13990120000000000E+07'
      FRTEXT( 1198 ) = ' FRAME_DSS-45_TOPO                =  ' &
      //               '    0.13990450000000000E+07'
      FRTEXT( 1199 ) = ' TKFRAME_DSS-17_TOPO_AXES         = (' &
      //               '    0.30000000000000000E+01,'
      FRTEXT( 1200 ) = '                                     ' &
      //               '    0.20000000000000000E+01,'
      FRTEXT( 1201 ) = '                                     ' &
      //               '    0.30000000000000000E+01 )'
      FRTEXT( 1202 ) = ' FRAME_1399024_CLASS              =  ' &
      //               '    0.40000000000000000E+01'
      FRTEXT( 1203 ) = ' FRAME_CASSINI_RADAR_3            =  ' &
      //               '   -0.82812000000000000E+05'
      FRTEXT( 1204 ) = ' FRAME_-82368_NAME                =  ' &
      //               '  ''CASSINI_ISS_NAC_RAD'''
      FRTEXT( 1205 ) = ' FRAME_1399014_NAME               =  ' &
      //               '  ''DSS-14_TOPO'''
      FRTEXT( 1206 ) = ' FRAME_1399053_CLASS              =  ' &
      //               '    0.40000000000000000E+01'
      FRTEXT( 1207 ) = ' FRAME_-82371_NAME                =  ' &
      //               '  ''CASSINI_VIMS_IR'''
      FRTEXT( 1208 ) = ' FRAME_CASSINI_HGA                =  ' &
      //               '   -0.82101000000000000E+05'
      FRTEXT( 1209 ) = ' CK_-82791_SPK                    =  ' &
      //               '   -0.82000000000000000E+02'
      FRTEXT( 1210 ) = ' TKFRAME_-82814_ANGLES            = (' &
      //               '    0.18219999999999999E+03,'
      FRTEXT( 1211 ) = '                                     ' &
      //               '    0.00000000000000000E+00,'
      FRTEXT( 1212 ) = '                                     ' &
      //               '    0.00000000000000000E+00 )'
      FRTEXT( 1213 ) = ' TKFRAME_-82378_SPEC              =  ' &
      //               '  ''ANGLES'''
      FRTEXT( 1214 ) = ' FRAME_-82731_CENTER              =  ' &
      //               '   -0.82000000000000000E+02'
      FRTEXT( 1215 ) = ' TKFRAME_-82892_SPEC              =  ' &
      //               '  ''ANGLES'''
      FRTEXT( 1216 ) = ' CK_-82821_SPK                    =  ' &
      //               '   -0.82000000000000000E+02'
      FRTEXT( 1217 ) = ' TKFRAME_-82101_AXES              = (' &
      //               '    0.30000000000000000E+01,'
      FRTEXT( 1218 ) = '                                     ' &
      //               '    0.20000000000000000E+01,'
      FRTEXT( 1219 ) = '                                     ' &
      //               '    0.10000000000000000E+01 )'
      FRTEXT( 1220 ) = ' FRAME_CASSINI_RADAR_4            =  ' &
      //               '   -0.82813000000000000E+05'
      FRTEXT( 1221 ) = ' FRAME_-82009_CENTER              =  ' &
      //               '   -0.82000000000000000E+02'
      FRTEXT( 1222 ) = ' FRAME_-82731_CLASS_ID            =  ' &
      //               '   -0.82731000000000000E+05'


!
!     Start by checking the built-in frames.
!
      CALL ZZFDAT ( NCOUNT, MAXBFR, NAME,   IDCODE, &
                    CENTER, TYPE,   TYPEID, CENTRD, &
                    BNMLST, BNMPOL, BNMNMS, BNMIDX, &
                    BIDLST, BIDPOL, BIDIDS, BIDIDX )

      DO I = 1, NCOUNT
!
!------- Case ----------------------------------------------------
!
         XNAME = NAME(I)
         TITLE = 'FK case #: checking frame '//XNAME
         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL TCASE ( TITLE )

         CALL NAMFRM ( XNAME, XCODE )

         CALL CHCKSI ( 'FRCODE', XCODE, '!=', 0, 0, OK )

         IF ( OK ) THEN

            CLSSID = -999
            CALL FRINFO ( XCODE, XCENT, CLASS, CLSSID, FOUND )


            CALL CHCKXC ( .FALSE., ' ', OK )

            IF ( OK ) THEN

               CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

               IF ( FOUND ) THEN

                  CALL CCIFRM ( CLASS,  CLSSID, FRCODE, &
                                FRNAME, CENT,   FOUND  )

                  CALL CHCKXC ( .FALSE., ' ', OK )
                  CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

                  CALL CHCKSC ( 'FRNAME', FRNAME, '=', XNAME,    OK )
                  CALL CHCKSI ( 'FRCODE', FRCODE, '=', XCODE,  0, OK )
                  CALL CHCKSI ( 'CENT',   CENT,   '=', XCENT,  0, OK )

               END IF

            END IF

         END IF

      END DO



!
!     Load our frame kernel data into the kernel pool.
!
      CALL LMPOOL ( FRTEXT, TXTSIZ )
      CALL CHCKXC ( .FALSE., ' ', OK )

      I = 1

      CALL GNPOOL ( 'FRAME_*_NAME', I, 1, N, KVNAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      DO WHILE ( N .GT. 0 )
!
!------- Case ----------------------------------------------------
!
         TITLE = 'FK case #: checking frame '//KVNAME
         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL TCASE ( TITLE )

         CALL GCPOOL ( KVNAME, 1, 1, NVALS, XNAME, FOUND )


         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

         CALL NAMFRM ( XNAME, XCODE )

         CALL CHCKSI ( 'FRCODE', XCODE, '!=', 0, 0, OK )

         IF ( OK ) THEN

            CLSSID = -999
            CALL FRINFO ( XCODE, XCENT, CLASS, CLSSID, FOUND )


            CALL CHCKXC ( .FALSE., ' ', OK )

            IF ( OK ) THEN

               CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

               IF ( FOUND ) THEN

                  CALL CCIFRM ( CLASS,  CLSSID, FRCODE, &
                                FRNAME, CENT,   FOUND  )

                  CALL CHCKXC ( .FALSE., ' ', OK )
                  CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

                  CALL CHCKSC ( 'FRNAME', FRNAME, '=', XNAME,    OK )
                  CALL CHCKSI ( 'FRCODE', FRCODE, '=', XCODE,  0, OK )
                  CALL CHCKSI ( 'CENT',   CENT,   '=', XCENT,  0, OK )

               END IF

            END IF

         END IF

         I = I + 1
         CALL GNPOOL ( 'FRAME_*_NAME', I, 1, N, KVNAME, FOUND )

      END DO



!
!     Re-check the built-in frames. Now the kernel pool contains
!     a lot of frame specifications, so we'll speed things up a
!     bit by sampling rather than looking up each frame.
!
      DO I = 1, NCOUNT, 10
!
!------- Case ----------------------------------------------------
!
         XNAME = NAME(I)
         TITLE = 'FK case #: re-checking frame '//XNAME
         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL TCASE ( TITLE )

         CALL NAMFRM ( XNAME, XCODE )

         CALL CHCKSI ( 'FRCODE', XCODE, '!=', 0, 0, OK )

         IF ( OK ) THEN

            CLSSID = -999
            CALL FRINFO ( XCODE, XCENT, CLASS, CLSSID, FOUND )


            CALL CHCKXC ( .FALSE., ' ', OK )

            IF ( OK ) THEN

               CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

               IF ( FOUND ) THEN

                  CALL CCIFRM ( CLASS,  CLSSID, FRCODE, &
                                FRNAME, CENT,   FOUND  )

                  CALL CHCKXC ( .FALSE., ' ', OK )
                  CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

                  CALL CHCKSC ( 'FRNAME', FRNAME, '=', XNAME,    OK )
                  CALL CHCKSI ( 'FRCODE', FRCODE, '=', XCODE,  0, OK )
                  CALL CHCKSI ( 'CENT',   CENT,   '=', XCENT,  0, OK )

               END IF

            END IF

         END IF

      END DO

!
!     Error cases:
!
!
!------- Case ----------------------------------------------------
!
      CALL TCASE ( 'Error: frame class variable has character type' )

      CALL KCLEAR
      CALL CHCKXC ( .FALSE., ' ', OK )

      BADTXT(1) = 'FRAME_-999_CLASS = ''X'' '

      CALL LMPOOL ( BADTXT, 1 )

      CALL CCIFRM ( 3, -1, FRCODE, FRNAME, CENT, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAMEDEF)', OK )


!
!------- Case ----------------------------------------------------
!
      CALL TCASE ( 'Error: frame class ID variable is missing' )

      CALL KCLEAR
      CALL CHCKXC ( .FALSE., ' ', OK )

      BADTXT(1) = 'FRAME_-999_CLASS = 3 '

      CALL LMPOOL ( BADTXT, 1 )

      CALL CCIFRM ( 3, -1, FRCODE, FRNAME, CENT, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAMEDEF)', OK )

!
!------- Case ----------------------------------------------------
!
      CALL TCASE ( 'Error: frame name variable is missing' )

      CALL KCLEAR
      CALL CHCKXC ( .FALSE., ' ', OK )

      BADTXT(1) = 'FRAME_-999_CLASS    = 3 '
      BADTXT(2) = 'FRAME_-999_CLASS_ID = -1 '

      CALL LMPOOL ( BADTXT, 2 )

      CALL CCIFRM ( 3, -1, FRCODE, FRNAME, CENT, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAMEDEF)', OK )

!
!------- Case ----------------------------------------------------
!
      CALL TCASE ( 'Error: frame ID variable is missing' )

      CALL KCLEAR
      CALL CHCKXC ( .FALSE., ' ', OK )

      BADTXT(1) = 'FRAME_-999_CLASS    = 3 '
      BADTXT(2) = 'FRAME_-999_CLASS_ID = -1 '
      BADTXT(3) = 'FRAME_-999_NAME     = ''BADFRAME'' '

      CALL LMPOOL ( BADTXT, 3 )

      CALL CCIFRM ( 3, -1, FRCODE, FRNAME, CENT, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAMEDEF)', OK )


!
!------- Case ----------------------------------------------------
!
      CALL TCASE ( 'Error: center ID variable is missing' )

      CALL KCLEAR
      CALL CHCKXC ( .FALSE., ' ', OK )

      BADTXT(1) = 'FRAME_-999_CLASS    = 3 '
      BADTXT(2) = 'FRAME_-999_CLASS_ID = -1 '
      BADTXT(3) = 'FRAME_-999_NAME     = ''BADFRAME'' '
      BADTXT(4) = 'FRAME_BADFRAME      = -999 '

      CALL LMPOOL ( BADTXT, 4 )


      CALL CCIFRM ( 3, -1, FRCODE, FRNAME, CENT, FOUND )

!
!     The short error message we're looking for is signaled
!     by ZZDYNBID, which is called directly from CCIFRM. If
!     the implementation of CCIFRM changes, this check may
!     need to be changed as well.
!
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )


!
!------- Case ----------------------------------------------------
!
      CALL TCASE ( 'Check that built-in has higher priority' )

      CALL KCLEAR
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Try to override IAU_EARTH that has CLASS = 2 and CLASS_ID = 399.
!
      BADTXT(1) = 'FRAME_BADFRAME      = -999 '
      BADTXT(2) = 'FRAME_-999_NAME     = ''BADFRAME'' '
      BADTXT(3) = 'FRAME_-999_CLASS    = 2 '
      BADTXT(4) = 'FRAME_-999_CLASS_ID = 399 '
      BADTXT(5) = 'FRAME_-999_CENTER   = -999 '

      CALL LMPOOL ( BADTXT, 5 )

      CALL CCIFRM ( 2, 399, FRCODE, FRNAME, CENT, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )
      CALL CHCKSC ( 'FRNAME', FRNAME, '=', 'IAU_EARTH',    OK )
      CALL CHCKSI ( 'FRCODE', FRCODE, '=', 10013,  0, OK )
      CALL CHCKSI ( 'CENT',   CENT,   '=', 399,  0, OK )


      CALL KCLEAR
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL T_SUCCESS( OK )
      RETURN
      END

