!$Procedure F_GETFV2 ( GETFOV Test Family )

      SUBROUTINE F_GETFV2 ( OK )
      use pepper_legacy, pepper_legacy_frame => frame

! $ Abstract
!
!     Test family to exercise the logic and code in the GETFOV routine.
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
! $ Files
!
!     None.
!
! $ Exceptions
!
!     This routine does not generate any errors. Routines in its
!     call tree may generate erros that are either intentional and
!     trapped or unintentional and need reporting.  The test family
!     utilities manage this.
!
! $ Particulars
!
!     This routine exercises GETFOV's logic.  This module exists,
!     because it was written unknowingly after a F_GETFOV had already
!     been delievered to CM.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     F.S. Turner     (JPL)
!     B.V. Semenov    (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    TSPICE Version 1.1.0, 08-MAR-2005 (BVS)
!
!        Added tests with non-unit REFVEC and non-normal REFVEC and
!        BSIGHT.
!
!-    TSPICE Version 1.0.0, 19-APR-2001 (FST)
!
!
!-&

!
!     SPICELIB Functions
!


!
!     Local Parameters
!
      INTEGER               LINLEN
      PARAMETER           ( LINLEN = 81 )

      INTEGER               NLINES
      PARAMETER           ( NLINES = 478 )

      INTEGER               MXBNDS
      PARAMETER           ( MXBNDS = 10 )

!
!     Local Variables
!
      CHARACTER*(LINLEN)    BNDNAM
      CHARACTER*(LINLEN)    CKFRAM
      CHARACTER*(LINLEN)    CKSHAP
      CHARACTER*(LINLEN)    FRAME
      CHARACTER*(LINLEN)    KBPOOL ( NLINES )
      CHARACTER*(LINLEN)    SHAPE

      DOUBLE PRECISION      BOUNDS ( 3, MXBNDS )
      DOUBLE PRECISION      BSIGHT ( 3 )
      DOUBLE PRECISION      CKBNDS ( 3, MXBNDS )
      DOUBLE PRECISION      CKBSGT ( 3 )

      INTEGER               CKN
      INTEGER               I
      INTEGER               N

!
!     Start the test family with an open call.
!
      CALL TOPEN ( 'F_GETFV2' )

!
!     Build the string buffer with the FOV definitions.
!
      KBPOOL(  1 ) = ' '
      KBPOOL(  2 ) = 'INS-10000_FOV_SHAPE    = ''CIRCLE'''
      KBPOOL(  3 ) = 'INS-10000_BORESIGHT    = ( 1.0, 0.0, 0.' &
      //             '0 )'
      KBPOOL(  4 ) = 'INS-10000_FOV_BOUNDARY = ( 0.0, 1.0, 0.' &
      //             '0 )'
      KBPOOL(  5 ) = ' '
      KBPOOL(  6 ) = 'INS-11000_FOV_FRAME    = ''11000-FRAME'''
      KBPOOL(  7 ) = 'INS-11000_FOV_SHAPE    = ''SPUD-SHAPED'''
      KBPOOL(  8 ) = 'INS-11000_BORESIGHT    = ( 1.0, 0.0, 0.' &
      //             '0 )'
      KBPOOL(  9 ) = 'INS-11000_FOV_BOUNDARY = ( 0.0, 1.0, 0.' &
      //             '0 )'
      KBPOOL( 10 ) = ' '
      KBPOOL( 11 ) = 'INS-12000_FOV_FRAME    = ''12000-FRAME'''
      KBPOOL( 12 ) = 'INS-12000_BORESIGHT    = ( 1.0, 0.0, 0.' &
      //             '0 )'
      KBPOOL( 13 ) = 'INS-12000_FOV_BOUNDARY = ( 0.0, 1.0, 0.' &
      //             '0 )'
      KBPOOL( 14 ) = ' '
      KBPOOL( 15 ) = 'INS-12500_FOV_FRAME    = ''12500-FRAME'''
      KBPOOL( 16 ) = 'INS-12500_FOV_SHAPE    = ''CIRCLE'''
      KBPOOL( 17 ) = 'INS-12500_FOV_BOUNDARY = ( 0.0, 1.0, 0.' &
      //             '0 )'
      KBPOOL( 18 ) = ' '
      KBPOOL( 19 ) = 'INS-13000_FOV_FRAME    = ''13000-FRAME'''
      KBPOOL( 20 ) = 'INS-13000_FOV_SHAPE    = ''CIRCLE'''
      KBPOOL( 21 ) = 'INS-13000_BORESIGHT    = ( 1.0, 0.0 )'
      KBPOOL( 22 ) = 'INS-13000_FOV_BOUNDARY = ( 0.0, 1.0, 0.' &
      //             '0 )'
      KBPOOL( 23 ) = ' '
      KBPOOL( 24 ) = 'INS-14000_FOV_FRAME    = ''14000-FRAME'''
      KBPOOL( 25 ) = 'INS-14000_FOV_SHAPE    = ''CIRCLE'''
      KBPOOL( 26 ) = 'INS-14000_BORESIGHT    = ( ''A'', ''B''' &
      //             ', ''C'' )'
      KBPOOL( 27 ) = 'INS-14000_FOV_BOUNDARY = ( 0.0, 1.0, 0.' &
      //             '0 )'
      KBPOOL( 28 ) = ' '
      KBPOOL( 29 ) = 'INS-15000_FOV_FRAME    = ''15000-FRAME'''
      KBPOOL( 30 ) = 'INS-15000_FOV_SHAPE    = ''CIRCLE'''
      KBPOOL( 31 ) = 'INS-15000_BORESIGHT    = ( 1.0, 0.0, 0.' &
      //             '0 )'
      KBPOOL( 32 ) = ' '
      KBPOOL( 33 ) = 'INS-16000_FOV_FRAME    = ''16000-FRAME'''
      KBPOOL( 34 ) = 'INS-16000_FOV_SHAPE    = ''POLYGON'''
      KBPOOL( 35 ) = 'INS-16000_BORESIGHT    = ( 1.0, 0.0, 0.' &
      //             '0 )'
      KBPOOL( 36 ) = 'INS-16000_FOV_BOUNDARY = ('
      KBPOOL( 37 ) = '                           0.0,  1.0,  ' &
      //             '0.0'
      KBPOOL( 38 ) = '                           0.0, -1.0,  ' &
      //             '0.0'
      KBPOOL( 39 ) = '                           0.0,  0.0,  ' &
      //             '1.0'
      KBPOOL( 40 ) = '                           0.0,  0.0, -' &
      //             '1.0'
      KBPOOL( 41 ) = '                           1.0,  1.0,  ' &
      //             '1.0'
      KBPOOL( 42 ) = '                         )'
      KBPOOL( 43 ) = ' '
      KBPOOL( 44 ) = 'INS-17000_FOV_FRAME    = ''17000-FRAME'''
      KBPOOL( 45 ) = 'INS-17000_FOV_SHAPE    = ''CIRCLE'''
      KBPOOL( 46 ) = 'INS-17000_BORESIGHT    = ( 1.0, 0.0, 0.' &
      //             '0 )'
      KBPOOL( 47 ) = 'INS-17000_FOV_BOUNDARY = ( 0.0, 1.0 )'
      KBPOOL( 48 ) = ' '
      KBPOOL( 49 ) = 'INS-18100_FOV_FRAME    = ''18100-FRAME'''
      KBPOOL( 50 ) = 'INS-18100_FOV_SHAPE    = ''CIRCLE'''
      KBPOOL( 51 ) = 'INS-18100_BORESIGHT    = ( 0.0, 0.0, 1.' &
      //             '0 )'
      KBPOOL( 52 ) = 'INS-18100_FOV_BOUNDARY = ('
      KBPOOL( 53 ) = '                           1.0, 0.0, 1.' &
      //             '0'
      KBPOOL( 54 ) = '                          -1.0, 0.0, 1.' &
      //             '0'
      KBPOOL( 55 ) = '                         )'
      KBPOOL( 56 ) = ' '
      KBPOOL( 57 ) = 'INS-18200_FOV_FRAME    = ''18200-FRAME'''
      KBPOOL( 58 ) = 'INS-18200_FOV_SHAPE    = ''ELLIPSE'''
      KBPOOL( 59 ) = 'INS-18200_BORESIGHT    = ( 0.0, 0.0, 1.' &
      //             '0 )'
      KBPOOL( 60 ) = 'INS-18200_FOV_BOUNDARY = ('
      KBPOOL( 61 ) = '                           1.0, 0.0, 1.' &
      //             '0'
      KBPOOL( 62 ) = '                         )'
      KBPOOL( 63 ) = ' '
      KBPOOL( 64 ) = 'INS-18300_FOV_FRAME    = ''18300-FRAME'''
      KBPOOL( 65 ) = 'INS-18300_FOV_SHAPE    = ''RECTANGLE'''
      KBPOOL( 66 ) = 'INS-18300_BORESIGHT    = ( 0.0, 0.0, 1.' &
      //             '0 )'
      KBPOOL( 67 ) = 'INS-18300_FOV_BOUNDARY = ('
      KBPOOL( 68 ) = '                           1.0, 0.0, 1.' &
      //             '0'
      KBPOOL( 69 ) = '                         )'
      KBPOOL( 70 ) = ' '
      KBPOOL( 71 ) = 'INS-18400_FOV_FRAME    = ''18400-FRAME'''
      KBPOOL( 72 ) = 'INS-18400_FOV_SHAPE    = ''POLYGON'''
      KBPOOL( 73 ) = 'INS-18400_BORESIGHT    = ( 0.0, 0.0, 1.' &
      //             '0 )'
      KBPOOL( 74 ) = 'INS-18400_FOV_BOUNDARY = ('
      KBPOOL( 75 ) = '                           1.0, 0.0, 1.' &
      //             '0'
      KBPOOL( 76 ) = '                         )'
      KBPOOL( 77 ) = ' '
      KBPOOL( 78 ) = 'INS-20000_FOV_FRAME    = ''20000-FRAME'''
      KBPOOL( 79 ) = 'INS-20000_FOV_SHAPE    = ''CIRCLE'''
      KBPOOL( 80 ) = 'INS-20000_BORESIGHT    = ( 0.0, 0.0, 1.' &
      //             '0 )'
      KBPOOL( 81 ) = 'INS-20000_FOV_BOUNDARY = ('
      KBPOOL( 82 ) = '                           1.0, 0.0, 1.' &
      //             '0'
      KBPOOL( 83 ) = '                         )'
      KBPOOL( 84 ) = ' '
      KBPOOL( 85 ) = 'INS-20100_FOV_FRAME            = ''2010' &
      //             '0-FRAME'''
      KBPOOL( 86 ) = 'INS-20100_FOV_SHAPE            = ''CIRC' &
      //             'LE'''
      KBPOOL( 87 ) = 'INS-20100_BORESIGHT            = ( 0.0,' &
      //             ' 0.0, 1.0 )'
      KBPOOL( 88 ) = 'INS-20100_FOV_BOUNDARY_CORNERS = ('
      KBPOOL( 89 ) = '                                   1.0,' &
      //             ' 0.0, 1.0'
      KBPOOL( 90 ) = '                                 )'
      KBPOOL( 91 ) = ' '
      KBPOOL( 92 ) = 'INS-20200_FOV_FRAME            = ''2020' &
      //             '0-FRAME'''
      KBPOOL( 93 ) = 'INS-20200_FOV_SHAPE            = ''CIRC' &
      //             'LE'''
      KBPOOL( 94 ) = 'INS-20200_BORESIGHT            = ( 0.0,' &
      //             ' 0.0, 1.0 )'
      KBPOOL( 95 ) = 'INS-20200_FOV_BOUNDARY_CORNERS = ('
      KBPOOL( 96 ) = '                                   1.0,' &
      //             ' 0.0, 1.0'
      KBPOOL( 97 ) = '                                 )'
      KBPOOL( 98 ) = 'INS-20200_FOV_BOUNDARY         = ('
      KBPOOL( 99 ) = '                                   2.0,' &
      //             ' 0.0, 1.0'
      KBPOOL( 100 ) = '                                 )'
      KBPOOL( 101 ) = ' '
      KBPOOL( 102 ) = 'INS-21000_FOV_FRAME    = ''21000-FRAME' &
      //              ''''
      KBPOOL( 103 ) = 'INS-21000_FOV_SHAPE    = ''ELLIPSE'''
      KBPOOL( 104 ) = 'INS-21000_BORESIGHT    = ( 0.0, 0.0, 1' &
      //              '.0 )'
      KBPOOL( 105 ) = 'INS-21000_FOV_BOUNDARY = ('
      KBPOOL( 106 ) = '                           1.0, 0.0, 1' &
      //              '.0'
      KBPOOL( 107 ) = '                           0.0, 1.0, 1' &
      //              '.0'
      KBPOOL( 108 ) = '                         )'
      KBPOOL( 109 ) = ' '
      KBPOOL( 110 ) = 'INS-21100_FOV_FRAME            = ''211' &
      //              '00-FRAME'''
      KBPOOL( 111 ) = 'INS-21100_FOV_SHAPE            = ''ELL' &
      //              'IPSE'''
      KBPOOL( 112 ) = 'INS-21100_BORESIGHT            = ( 0.0' &
      //              ', 0.0, 1.0 )'
      KBPOOL( 113 ) = 'INS-21100_FOV_BOUNDARY_CORNERS = ('
      KBPOOL( 114 ) = '                                   1.0' &
      //              ', 0.0, 1.0'
      KBPOOL( 115 ) = '                                   0.0' &
      //              ', 1.0, 1.0'
      KBPOOL( 116 ) = '                                 )'
      KBPOOL( 117 ) = ' '
      KBPOOL( 118 ) = 'INS-21200_FOV_FRAME            = ''212' &
      //              '00-FRAME'''
      KBPOOL( 119 ) = 'INS-21200_FOV_SHAPE            = ''ELL' &
      //              'IPSE'''
      KBPOOL( 120 ) = 'INS-21200_BORESIGHT            = ( 0.0' &
      //              ', 0.0, 1.0 )'
      KBPOOL( 121 ) = 'INS-21200_FOV_BOUNDARY_CORNERS = ('
      KBPOOL( 122 ) = '                                   1.0' &
      //              ', 0.0, 1.0'
      KBPOOL( 123 ) = '                                   0.0' &
      //              ', 1.0, 1.0'
      KBPOOL( 124 ) = '                                 )'
      KBPOOL( 125 ) = 'INS-21200_FOV_BOUNDARY         = ('
      KBPOOL( 126 ) = '                                   2.0' &
      //              ', 0.0, 1.0'
      KBPOOL( 127 ) = '                                   0.0' &
      //              ', 2.0, 1.0'
      KBPOOL( 128 ) = '                                 )'
      KBPOOL( 129 ) = ' '
      KBPOOL( 130 ) = 'INS-22000_FOV_FRAME    = ''22000-FRAME' &
      //              ''''
      KBPOOL( 131 ) = 'INS-22000_FOV_SHAPE    = ''RECTANGLE'''
      KBPOOL( 132 ) = 'INS-22000_BORESIGHT    = ( 0.0, 0.0, 1' &
      //              '.0 )'
      KBPOOL( 133 ) = 'INS-22000_FOV_BOUNDARY = ('
      KBPOOL( 134 ) = '                            1.0,  1.0,' &
      //              ' 1.0'
      KBPOOL( 135 ) = '                            1.0, -1.0,' &
      //              ' 1.0'
      KBPOOL( 136 ) = '                           -1.0, -1.0,' &
      //              ' 1.0'
      KBPOOL( 137 ) = '                           -1.0,  1.0,' &
      //              ' 1.0'
      KBPOOL( 138 ) = '                         )'
      KBPOOL( 139 ) = ' '
      KBPOOL( 140 ) = 'INS-22100_FOV_FRAME            = ''221' &
      //              '00-FRAME'''
      KBPOOL( 141 ) = 'INS-22100_FOV_SHAPE            = ''REC' &
      //              'TANGLE'''
      KBPOOL( 142 ) = 'INS-22100_BORESIGHT            = ( 0.0' &
      //              ', 0.0, 1.0 )'
      KBPOOL( 143 ) = 'INS-22100_FOV_BOUNDARY_CORNERS = ('
      KBPOOL( 144 ) = '                                    1.' &
      //              '0,  1.0, 1.0'
      KBPOOL( 145 ) = '                                    1.' &
      //              '0, -1.0, 1.0'
      KBPOOL( 146 ) = '                                   -1.' &
      //              '0, -1.0, 1.0'
      KBPOOL( 147 ) = '                                   -1.' &
      //              '0,  1.0, 1.0'
      KBPOOL( 148 ) = '                                 )'
      KBPOOL( 149 ) = ' '
      KBPOOL( 150 ) = 'INS-22200_FOV_FRAME            = ''222' &
      //              '00-FRAME'''
      KBPOOL( 151 ) = 'INS-22200_FOV_SHAPE            = ''REC' &
      //              'TANGLE'''
      KBPOOL( 152 ) = 'INS-22200_BORESIGHT            = ( 0.0' &
      //              ', 0.0, 1.0 )'
      KBPOOL( 153 ) = 'INS-22200_FOV_BOUNDARY_CORNERS = ('
      KBPOOL( 154 ) = '                                    1.' &
      //              '0,  1.0, 1.0'
      KBPOOL( 155 ) = '                                    1.' &
      //              '0, -1.0, 1.0'
      KBPOOL( 156 ) = '                                   -1.' &
      //              '0, -1.0, 1.0'
      KBPOOL( 157 ) = '                                   -1.' &
      //              '0,  1.0, 1.0'
      KBPOOL( 158 ) = '                                 )'
      KBPOOL( 159 ) = 'INS-22200_FOV_BOUNDARY         = ('
      KBPOOL( 160 ) = '                                    2.' &
      //              '0,  2.0, 1.0'
      KBPOOL( 161 ) = '                                    2.' &
      //              '0, -2.0, 1.0'
      KBPOOL( 162 ) = '                                   -2.' &
      //              '0, -2.0, 1.0'
      KBPOOL( 163 ) = '                                   -2.' &
      //              '0,  2.0, 1.0'
      KBPOOL( 164 ) = '                                 )'
      KBPOOL( 165 ) = ' '
      KBPOOL( 166 ) = 'INS-23000_FOV_FRAME    = ''23000-FRAME' &
      //              ''''
      KBPOOL( 167 ) = 'INS-23000_FOV_SHAPE    = ''POLYGON'''
      KBPOOL( 168 ) = 'INS-23000_BORESIGHT    = ( 0.0, 0.0, 1' &
      //              '.0 )'
      KBPOOL( 169 ) = 'INS-23000_FOV_BOUNDARY = ('
      KBPOOL( 170 ) = '                            1.0,  1.0,' &
      //              ' 1.0'
      KBPOOL( 171 ) = '                            1.0, -1.0,' &
      //              ' 1.0'
      KBPOOL( 172 ) = '                           -1.0, -1.0,' &
      //              ' 1.0'
      KBPOOL( 173 ) = '                           -1.0,  1.0,' &
      //              ' 1.0'
      KBPOOL( 174 ) = '                         )'
      KBPOOL( 175 ) = ' '
      KBPOOL( 176 ) = 'INS-23100_FOV_FRAME            = ''231' &
      //              '00-FRAME'''
      KBPOOL( 177 ) = 'INS-23100_FOV_SHAPE            = ''POL' &
      //              'YGON'''
      KBPOOL( 178 ) = 'INS-23100_BORESIGHT            = ( 0.0' &
      //              ', 0.0, 1.0 )'
      KBPOOL( 179 ) = 'INS-23100_FOV_BOUNDARY_CORNERS = ('
      KBPOOL( 180 ) = '                                    1.' &
      //              '0,  1.0, 1.0'
      KBPOOL( 181 ) = '                                    1.' &
      //              '0, -1.0, 1.0'
      KBPOOL( 182 ) = '                                   -1.' &
      //              '0, -1.0, 1.0'
      KBPOOL( 183 ) = '                                   -1.' &
      //              '0,  1.0, 1.0'
      KBPOOL( 184 ) = '                                 )'
      KBPOOL( 185 ) = ' '
      KBPOOL( 186 ) = 'INS-23200_FOV_FRAME            = ''232' &
      //              '00-FRAME'''
      KBPOOL( 187 ) = 'INS-23200_FOV_SHAPE            = ''POL' &
      //              'YGON'''
      KBPOOL( 188 ) = 'INS-23200_BORESIGHT            = ( 0.0' &
      //              ', 0.0, 1.0 )'
      KBPOOL( 189 ) = 'INS-23200_FOV_BOUNDARY_CORNERS = ('
      KBPOOL( 190 ) = '                                    1.' &
      //              '0,  1.0, 1.0'
      KBPOOL( 191 ) = '                                    1.' &
      //              '0, -1.0, 1.0'
      KBPOOL( 192 ) = '                                   -1.' &
      //              '0, -1.0, 1.0'
      KBPOOL( 193 ) = '                                   -1.' &
      //              '0,  1.0, 1.0'
      KBPOOL( 194 ) = '                                 )'
      KBPOOL( 195 ) = 'INS-23200_FOV_BOUNDARY         = ('
      KBPOOL( 196 ) = '                                    2.' &
      //              '0,  2.0, 1.0'
      KBPOOL( 197 ) = '                                    2.' &
      //              '0, -2.0, 1.0'
      KBPOOL( 198 ) = '                                   -2.' &
      //              '0, -2.0, 1.0'
      KBPOOL( 199 ) = '                                   -2.' &
      //              '0,  2.0, 1.0'
      KBPOOL( 200 ) = '                                 )'
      KBPOOL( 201 ) = ' '
      KBPOOL( 202 ) = 'INS-30000_FOV_FRAME    = ''30000-FRAME' &
      //              ''''
      KBPOOL( 203 ) = 'INS-30000_FOV_SHAPE    = ''CIRCLE'''
      KBPOOL( 204 ) = 'INS-30000_BORESIGHT    = ( 0.0, 0.0, 1' &
      //              '.0 )'
      KBPOOL( 205 ) = 'INS-30000_FOV_CLASS_SPEC       = ''COR' &
      //              'NERS'''
      KBPOOL( 206 ) = 'INS-30000_FOV_BOUNDARY = ('
      KBPOOL( 207 ) = '                           1.0, 0.0, 1' &
      //              '.0'
      KBPOOL( 208 ) = '                         )'
      KBPOOL( 209 ) = ' '
      KBPOOL( 210 ) = 'INS-30100_FOV_FRAME            = ''301' &
      //              '00-FRAME'''
      KBPOOL( 211 ) = 'INS-30100_FOV_SHAPE            = ''CIR' &
      //              'CLE'''
      KBPOOL( 212 ) = 'INS-30100_BORESIGHT            = ( 0.0' &
      //              ', 0.0, 1.0 )'
      KBPOOL( 213 ) = 'INS-30100_FOV_CLASS_SPEC       = ''COR' &
      //              'NERS'''
      KBPOOL( 214 ) = 'INS-30100_FOV_BOUNDARY_CORNERS = ('
      KBPOOL( 215 ) = '                                   1.0' &
      //              ', 0.0, 1.0'
      KBPOOL( 216 ) = '                                 )'
      KBPOOL( 217 ) = ' '
      KBPOOL( 218 ) = 'INS-30200_FOV_FRAME            = ''302' &
      //              '00-FRAME'''
      KBPOOL( 219 ) = 'INS-30200_FOV_SHAPE            = ''CIR' &
      //              'CLE'''
      KBPOOL( 220 ) = 'INS-30200_BORESIGHT            = ( 0.0' &
      //              ', 0.0, 1.0 )'
      KBPOOL( 221 ) = 'INS-30200_FOV_CLASS_SPEC       = ''COR' &
      //              'NERS'''
      KBPOOL( 222 ) = 'INS-30200_FOV_BOUNDARY_CORNERS = ('
      KBPOOL( 223 ) = '                                   1.0' &
      //              ', 0.0, 1.0'
      KBPOOL( 224 ) = '                                 )'
      KBPOOL( 225 ) = 'INS-30200_FOV_BOUNDARY         = ('
      KBPOOL( 226 ) = '                                   2.0' &
      //              ', 0.0, 1.0'
      KBPOOL( 227 ) = '                                 )'
      KBPOOL( 228 ) = ' '
      KBPOOL( 229 ) = 'INS-31000_FOV_FRAME    = ''31000-FRAME' &
      //              ''''
      KBPOOL( 230 ) = 'INS-31000_FOV_SHAPE    = ''ELLIPSE'''
      KBPOOL( 231 ) = 'INS-31000_BORESIGHT    = ( 0.0, 0.0, 1' &
      //              '.0 )'
      KBPOOL( 232 ) = 'INS-31000_FOV_CLASS_SPEC       = ''COR' &
      //              'NERS'''
      KBPOOL( 233 ) = 'INS-31000_FOV_BOUNDARY = ('
      KBPOOL( 234 ) = '                           1.0, 0.0, 1' &
      //              '.0'
      KBPOOL( 235 ) = '                           0.0, 1.0, 1' &
      //              '.0'
      KBPOOL( 236 ) = '                         )'
      KBPOOL( 237 ) = ' '
      KBPOOL( 238 ) = 'INS-31100_FOV_FRAME            = ''311' &
      //              '00-FRAME'''
      KBPOOL( 239 ) = 'INS-31100_FOV_SHAPE            = ''ELL' &
      //              'IPSE'''
      KBPOOL( 240 ) = 'INS-31100_BORESIGHT            = ( 0.0' &
      //              ', 0.0, 1.0 )'
      KBPOOL( 241 ) = 'INS-31100_FOV_CLASS_SPEC       = ''COR' &
      //              'NERS'''
      KBPOOL( 242 ) = 'INS-31100_FOV_BOUNDARY_CORNERS = ('
      KBPOOL( 243 ) = '                                   1.0' &
      //              ', 0.0, 1.0'
      KBPOOL( 244 ) = '                                   0.0' &
      //              ', 1.0, 1.0'
      KBPOOL( 245 ) = '                                 )'
      KBPOOL( 246 ) = ' '
      KBPOOL( 247 ) = 'INS-31200_FOV_FRAME            = ''312' &
      //              '00-FRAME'''
      KBPOOL( 248 ) = 'INS-31200_FOV_SHAPE            = ''ELL' &
      //              'IPSE'''
      KBPOOL( 249 ) = 'INS-31200_BORESIGHT            = ( 0.0' &
      //              ', 0.0, 1.0 )'
      KBPOOL( 250 ) = 'INS-31200_FOV_CLASS_SPEC       = ''COR' &
      //              'NERS'''
      KBPOOL( 251 ) = 'INS-31200_FOV_BOUNDARY_CORNERS = ('
      KBPOOL( 252 ) = '                                   1.0' &
      //              ', 0.0, 1.0'
      KBPOOL( 253 ) = '                                   0.0' &
      //              ', 1.0, 1.0'
      KBPOOL( 254 ) = '                                 )'
      KBPOOL( 255 ) = 'INS-31200_FOV_BOUNDARY         = ('
      KBPOOL( 256 ) = '                                   2.0' &
      //              ', 0.0, 1.0'
      KBPOOL( 257 ) = '                                   0.0' &
      //              ', 2.0, 1.0'
      KBPOOL( 258 ) = '                                 )'
      KBPOOL( 259 ) = ' '
      KBPOOL( 260 ) = 'INS-32000_FOV_FRAME    = ''32000-FRAME' &
      //              ''''
      KBPOOL( 261 ) = 'INS-32000_FOV_SHAPE    = ''RECTANGLE'''
      KBPOOL( 262 ) = 'INS-32000_BORESIGHT    = ( 0.0, 0.0, 1' &
      //              '.0 )'
      KBPOOL( 263 ) = 'INS-32000_FOV_CLASS_SPEC       = ''COR' &
      //              'NERS'''
      KBPOOL( 264 ) = 'INS-32000_FOV_BOUNDARY = ('
      KBPOOL( 265 ) = '                            1.0,  1.0,' &
      //              ' 1.0'
      KBPOOL( 266 ) = '                            1.0, -1.0,' &
      //              ' 1.0'
      KBPOOL( 267 ) = '                           -1.0, -1.0,' &
      //              ' 1.0'
      KBPOOL( 268 ) = '                           -1.0,  1.0,' &
      //              ' 1.0'
      KBPOOL( 269 ) = '                         )'
      KBPOOL( 270 ) = ' '
      KBPOOL( 271 ) = 'INS-32100_FOV_FRAME            = ''321' &
      //              '00-FRAME'''
      KBPOOL( 272 ) = 'INS-32100_FOV_SHAPE            = ''REC' &
      //              'TANGLE'''
      KBPOOL( 273 ) = 'INS-32100_BORESIGHT            = ( 0.0' &
      //              ', 0.0, 1.0 )'
      KBPOOL( 274 ) = 'INS-32100_FOV_CLASS_SPEC       = ''COR' &
      //              'NERS'''
      KBPOOL( 275 ) = 'INS-32100_FOV_BOUNDARY_CORNERS = ('
      KBPOOL( 276 ) = '                                    1.' &
      //              '0,  1.0, 1.0'
      KBPOOL( 277 ) = '                                    1.' &
      //              '0, -1.0, 1.0'
      KBPOOL( 278 ) = '                                   -1.' &
      //              '0, -1.0, 1.0'
      KBPOOL( 279 ) = '                                   -1.' &
      //              '0,  1.0, 1.0'
      KBPOOL( 280 ) = '                                 )'
      KBPOOL( 281 ) = ' '
      KBPOOL( 282 ) = 'INS-32200_FOV_FRAME            = ''322' &
      //              '00-FRAME'''
      KBPOOL( 283 ) = 'INS-32200_FOV_SHAPE            = ''REC' &
      //              'TANGLE'''
      KBPOOL( 284 ) = 'INS-32200_BORESIGHT            = ( 0.0' &
      //              ', 0.0, 1.0 )'
      KBPOOL( 285 ) = 'INS-32200_FOV_CLASS_SPEC       = ''COR' &
      //              'NERS'''
      KBPOOL( 286 ) = 'INS-32200_FOV_BOUNDARY_CORNERS = ('
      KBPOOL( 287 ) = '                                    1.' &
      //              '0,  1.0, 1.0'
      KBPOOL( 288 ) = '                                    1.' &
      //              '0, -1.0, 1.0'
      KBPOOL( 289 ) = '                                   -1.' &
      //              '0, -1.0, 1.0'
      KBPOOL( 290 ) = '                                   -1.' &
      //              '0,  1.0, 1.0'
      KBPOOL( 291 ) = '                                 )'
      KBPOOL( 292 ) = 'INS-32200_FOV_BOUNDARY         = ('
      KBPOOL( 293 ) = '                                    2.' &
      //              '0,  2.0, 1.0'
      KBPOOL( 294 ) = '                                    2.' &
      //              '0, -2.0, 1.0'
      KBPOOL( 295 ) = '                                   -2.' &
      //              '0, -2.0, 1.0'
      KBPOOL( 296 ) = '                                   -2.' &
      //              '0,  2.0, 1.0'
      KBPOOL( 297 ) = '                                 )'
      KBPOOL( 298 ) = ' '
      KBPOOL( 299 ) = 'INS-33000_FOV_FRAME    = ''33000-FRAME' &
      //              ''''
      KBPOOL( 300 ) = 'INS-33000_FOV_SHAPE    = ''POLYGON'''
      KBPOOL( 301 ) = 'INS-33000_BORESIGHT    = ( 0.0, 0.0, 1' &
      //              '.0 )'
      KBPOOL( 302 ) = 'INS-33000_FOV_CLASS_SPEC       = ''COR' &
      //              'NERS'''
      KBPOOL( 303 ) = 'INS-33000_FOV_BOUNDARY = ('
      KBPOOL( 304 ) = '                            1.0,  1.0,' &
      //              ' 1.0'
      KBPOOL( 305 ) = '                            1.0, -1.0,' &
      //              ' 1.0'
      KBPOOL( 306 ) = '                           -1.0, -1.0,' &
      //              ' 1.0'
      KBPOOL( 307 ) = '                           -1.0,  1.0,' &
      //              ' 1.0'
      KBPOOL( 308 ) = '                         )'
      KBPOOL( 309 ) = ' '
      KBPOOL( 310 ) = 'INS-33100_FOV_FRAME            = ''331' &
      //              '00-FRAME'''
      KBPOOL( 311 ) = 'INS-33100_FOV_SHAPE            = ''POL' &
      //              'YGON'''
      KBPOOL( 312 ) = 'INS-33100_BORESIGHT            = ( 0.0' &
      //              ', 0.0, 1.0 )'
      KBPOOL( 313 ) = 'INS-33100_FOV_CLASS_SPEC       = ''COR' &
      //              'NERS'''
      KBPOOL( 314 ) = 'INS-33100_FOV_BOUNDARY_CORNERS = ('
      KBPOOL( 315 ) = '                                    1.' &
      //              '0,  1.0, 1.0'
      KBPOOL( 316 ) = '                                    1.' &
      //              '0, -1.0, 1.0'
      KBPOOL( 317 ) = '                                   -1.' &
      //              '0, -1.0, 1.0'
      KBPOOL( 318 ) = '                                   -1.' &
      //              '0,  1.0, 1.0'
      KBPOOL( 319 ) = '                                 )'
      KBPOOL( 320 ) = ' '
      KBPOOL( 321 ) = 'INS-33200_FOV_FRAME            = ''332' &
      //              '00-FRAME'''
      KBPOOL( 322 ) = 'INS-33200_FOV_SHAPE            = ''POL' &
      //              'YGON'''
      KBPOOL( 323 ) = 'INS-33200_BORESIGHT            = ( 0.0' &
      //              ', 0.0, 1.0 )'
      KBPOOL( 324 ) = 'INS-33200_FOV_CLASS_SPEC       = ''COR' &
      //              'NERS'''
      KBPOOL( 325 ) = 'INS-33200_FOV_BOUNDARY_CORNERS = ('
      KBPOOL( 326 ) = '                                    1.' &
      //              '0,  1.0, 1.0'
      KBPOOL( 327 ) = '                                    1.' &
      //              '0, -1.0, 1.0'
      KBPOOL( 328 ) = '                                   -1.' &
      //              '0, -1.0, 1.0'
      KBPOOL( 329 ) = '                                   -1.' &
      //              '0,  1.0, 1.0'
      KBPOOL( 330 ) = '                                 )'
      KBPOOL( 331 ) = 'INS-33200_FOV_BOUNDARY         = ('
      KBPOOL( 332 ) = '                                    2.' &
      //              '0,  2.0, 1.0'
      KBPOOL( 333 ) = '                                    2.' &
      //              '0, -2.0, 1.0'
      KBPOOL( 334 ) = '                                   -2.' &
      //              '0, -2.0, 1.0'
      KBPOOL( 335 ) = '                                   -2.' &
      //              '0,  2.0, 1.0'
      KBPOOL( 336 ) = '                                 )'
      KBPOOL( 337 ) = ' '
      KBPOOL( 338 ) = 'INS-40000_FOV_FRAME            = ''400' &
      //              '00-FRAME'''
      KBPOOL( 339 ) = 'INS-40000_FOV_SHAPE            = ''BOG' &
      //              'US-SHAPE'''
      KBPOOL( 340 ) = 'INS-40000_BORESIGHT            = ( 0.0' &
      //              ', 0.0, 1.0 )'
      KBPOOL( 341 ) = 'INS-40000_FOV_CLASS_SPEC       = ''ANG' &
      //              'LES'''
      KBPOOL( 342 ) = 'INS-40000_FOV_REF_VECTOR       = ( 1.0' &
      //              ', 0.0, 0.0 )'
      KBPOOL( 343 ) = 'INS-40000_FOV_REF_ANGLE        = ( 20.' &
      //              '0 )'
      KBPOOL( 344 ) = 'INS-40000_FOV_CROSS_ANGLE      = ( 40.' &
      //              '0 )'
      KBPOOL( 345 ) = 'INS-40000_FOV_ANGLE_UNITS      = ''DEG' &
      //              'REES'''
      KBPOOL( 346 ) = ' '
      KBPOOL( 347 ) = 'INS-40100_FOV_FRAME            = ''401' &
      //              '00-FRAME'''
      KBPOOL( 348 ) = 'INS-40100_FOV_SHAPE            = ''CIR' &
      //              'CLE'''
      KBPOOL( 349 ) = 'INS-40100_BORESIGHT            = ( 0.0' &
      //              ', 0.0, 1.0 )'
      KBPOOL( 350 ) = 'INS-40100_FOV_CLASS_SPEC       = ''BOG' &
      //              'US-SPEC'''
      KBPOOL( 351 ) = 'INS-40100_FOV_REF_VECTOR       = ( 1.0' &
      //              ', 0.0, 0.0 )'
      KBPOOL( 352 ) = 'INS-40100_FOV_REF_ANGLE        = ( 20.' &
      //              '0 )'
      KBPOOL( 353 ) = 'INS-40100_FOV_CROSS_ANGLE      = ( 40.' &
      //              '0 )'
      KBPOOL( 354 ) = 'INS-40100_FOV_ANGLE_UNITS      = ''DEG' &
      //              'REES'''
      KBPOOL( 355 ) = ' '
      KBPOOL( 356 ) = 'INS-41000_FOV_FRAME            = ''410' &
      //              '00-FRAME'''
      KBPOOL( 357 ) = 'INS-41000_FOV_SHAPE            = ''ELL' &
      //              'IPSE'''
      KBPOOL( 358 ) = 'INS-41000_BORESIGHT            = ( 0.0' &
      //              ', 0.0, 1.0 )'
      KBPOOL( 359 ) = 'INS-41000_FOV_CLASS_SPEC       = ''ANG' &
      //              'LES'''
      KBPOOL( 360 ) = 'INS-41000_FOV_REF_ANGLE        = ( 20.' &
      //              '0 )'
      KBPOOL( 361 ) = 'INS-41000_FOV_CROSS_ANGLE      = ( 40.' &
      //              '0 )'
      KBPOOL( 362 ) = 'INS-41000_FOV_ANGLE_UNITS      = ''DEG' &
      //              'REES'''
      KBPOOL( 363 ) = ' '
      KBPOOL( 364 ) = 'INS-41100_FOV_FRAME            = ''411' &
      //              '00-FRAME'''
      KBPOOL( 365 ) = 'INS-41100_FOV_SHAPE            = ''ELL' &
      //              'IPSE'''
      KBPOOL( 366 ) = 'INS-41100_BORESIGHT            = ( 0.0' &
      //              ', 0.0, 1.0 )'
      KBPOOL( 367 ) = 'INS-41100_FOV_CLASS_SPEC       = ''ANG' &
      //              'LES'''
      KBPOOL( 368 ) = 'INS-41100_FOV_REF_VECTOR       = ( 0.0' &
      //              ', 0.1 )'
      KBPOOL( 369 ) = 'INS-41100_FOV_REF_ANGLE        = ( 20.' &
      //              '0 )'
      KBPOOL( 370 ) = 'INS-41100_FOV_CROSS_ANGLE      = ( 40.' &
      //              '0 )'
      KBPOOL( 371 ) = 'INS-41100_FOV_ANGLE_UNITS      = ''DEG' &
      //              'REES'''
      KBPOOL( 372 ) = ' '
      KBPOOL( 373 ) = 'INS-41200_FOV_FRAME            = ''412' &
      //              '00-FRAME'''
      KBPOOL( 374 ) = 'INS-41200_FOV_SHAPE            = ''ELL' &
      //              'IPSE'''
      KBPOOL( 375 ) = 'INS-41200_BORESIGHT            = ( 0.0' &
      //              ', 0.0, 1.0 )'
      KBPOOL( 376 ) = 'INS-41200_FOV_CLASS_SPEC       = ''ANG' &
      //              'LES'''
      KBPOOL( 377 ) = 'INS-41200_FOV_REF_VECTOR       = ( ''A' &
      //              ''', ''B'', ''C'' )'
      KBPOOL( 378 ) = 'INS-41200_FOV_REF_ANGLE        = ( 20.' &
      //              '0 )'
      KBPOOL( 379 ) = 'INS-41200_FOV_CROSS_ANGLE      = ( 40.' &
      //              '0 )'
      KBPOOL( 380 ) = 'INS-41200_FOV_ANGLE_UNITS      = ''DEG' &
      //              'REES'''
      KBPOOL( 381 ) = ' '
      KBPOOL( 382 ) = 'INS-41300_FOV_FRAME            = ''413' &
      //              '00-FRAME'''
      KBPOOL( 383 ) = 'INS-41300_FOV_SHAPE            = ''ELL' &
      //              'IPSE'''
      KBPOOL( 384 ) = 'INS-41300_BORESIGHT            = ( 0.0' &
      //              ', 0.0, 1.0 )'
      KBPOOL( 385 ) = 'INS-41300_FOV_CLASS_SPEC       = ''ANG' &
      //              'LES'''
      KBPOOL( 386 ) = 'INS-41300_FOV_REF_VECTOR       = ( 0.0' &
      //              ', 0.0, 1.0 )'
      KBPOOL( 387 ) = 'INS-41300_FOV_REF_ANGLE        = ( 20.' &
      //              '0 )'
      KBPOOL( 388 ) = 'INS-41300_FOV_CROSS_ANGLE      = ( 40.' &
      //              '0 )'
      KBPOOL( 389 ) = 'INS-41300_FOV_ANGLE_UNITS      = ''DEG' &
      //              'REES'''
      KBPOOL( 390 ) = ' '
      KBPOOL( 391 ) = 'INS-42000_FOV_FRAME            = ''420' &
      //              '00-FRAME'''
      KBPOOL( 392 ) = 'INS-42000_FOV_SHAPE            = ''ELL' &
      //              'IPSE'''
      KBPOOL( 393 ) = 'INS-42000_BORESIGHT            = ( 0.0' &
      //              ', 0.0, 1.0 )'
      KBPOOL( 394 ) = 'INS-42000_FOV_CLASS_SPEC       = ''ANG' &
      //              'LES'''
      KBPOOL( 395 ) = 'INS-42000_FOV_REF_VECTOR       = ( 0.0' &
      //              ', 1.0, 0.0 )'
      KBPOOL( 396 ) = 'INS-42000_FOV_CROSS_ANGLE      = ( 40.' &
      //              '0 )'
      KBPOOL( 397 ) = 'INS-42000_FOV_ANGLE_UNITS      = ''DEG' &
      //              'REES'''
      KBPOOL( 398 ) = ' '
      KBPOOL( 399 ) = 'INS-43000_FOV_FRAME            = ''430' &
      //              '00-FRAME'''
      KBPOOL( 400 ) = 'INS-43000_FOV_SHAPE            = ''ELL' &
      //              'IPSE'''
      KBPOOL( 401 ) = 'INS-43000_BORESIGHT            = ( 0.0' &
      //              ', 0.0, 1.0 )'
      KBPOOL( 402 ) = 'INS-43000_FOV_CLASS_SPEC       = ''ANG' &
      //              'LES'''
      KBPOOL( 403 ) = 'INS-43000_FOV_REF_ANGLE        = ( 20.' &
      //              '0 )'
      KBPOOL( 404 ) = 'INS-43000_FOV_REF_VECTOR       = ( 0.0' &
      //              ', 1.0, 0.0 )'
      KBPOOL( 405 ) = 'INS-43000_FOV_CROSS_ANGLE      = ( 40.' &
      //              '0 )'
      KBPOOL( 406 ) = ' '
      KBPOOL( 407 ) = 'INS-44000_FOV_FRAME            = ''440' &
      //              '00-FRAME'''
      KBPOOL( 408 ) = 'INS-44000_FOV_SHAPE            = ''ELL' &
      //              'IPSE'''
      KBPOOL( 409 ) = 'INS-44000_BORESIGHT            = ( 0.0' &
      //              ', 0.0, 1.0 )'
      KBPOOL( 410 ) = 'INS-44000_FOV_CLASS_SPEC       = ''ANG' &
      //              'LES'''
      KBPOOL( 411 ) = 'INS-44000_FOV_REF_VECTOR       = ( 0.0' &
      //              ', 1.0, 0.0 )'
      KBPOOL( 412 ) = 'INS-44000_FOV_REF_ANGLE        = ( 20.' &
      //              '0 )'
      KBPOOL( 413 ) = 'INS-44000_FOV_ANGLE_UNITS      = ''DEG' &
      //              'REES'''
      KBPOOL( 414 ) = ' '
      KBPOOL( 415 ) = 'INS-45000_FOV_FRAME            = ''450' &
      //              '00-FRAME'''
      KBPOOL( 416 ) = 'INS-45000_FOV_SHAPE            = ''REC' &
      //              'TANGLE'''
      KBPOOL( 417 ) = 'INS-45000_BORESIGHT            = ( 0.0' &
      //              ', 0.0, 1.0 )'
      KBPOOL( 418 ) = 'INS-45000_FOV_CLASS_SPEC       = ''ANG' &
      //              'LES'''
      KBPOOL( 419 ) = 'INS-45000_FOV_REF_VECTOR       = ( 0.0' &
      //              ', 1.0, 0.0 )'
      KBPOOL( 420 ) = 'INS-45000_FOV_REF_ANGLE        = ( 20.' &
      //              '0 )'
      KBPOOL( 421 ) = 'INS-45000_FOV_ANGLE_UNITS      = ''DEG' &
      //              'REES'''
      KBPOOL( 422 ) = ' '
      KBPOOL( 423 ) = 'INS-45100_FOV_FRAME            = ''451' &
      //              '00-FRAME'''
      KBPOOL( 424 ) = 'INS-45100_FOV_SHAPE            = ''REC' &
      //              'TANGLE'''
      KBPOOL( 425 ) = 'INS-45100_BORESIGHT            = ( 0.0' &
      //              ', 0.0, 1.0 )'
      KBPOOL( 426 ) = 'INS-45100_FOV_CLASS_SPEC       = ''ANG' &
      //              'LES'''
      KBPOOL( 427 ) = 'INS-45100_FOV_REF_VECTOR       = ( 0.0' &
      //              ', 1.0, 0.0 )'
      KBPOOL( 428 ) = 'INS-45100_FOV_REF_ANGLE        = ( 90.' &
      //              '0 )'
      KBPOOL( 429 ) = 'INS-45100_FOV_ANGLE_UNITS      = ''DEG' &
      //              'REES'''
      KBPOOL( 430 ) = 'INS-45100_FOV_CROSS_ANGLE      = ( 90.' &
      //              '0 )'
      KBPOOL( 431 ) = ' '
      KBPOOL( 432 ) = 'INS-46000_FOV_FRAME            = ''460' &
      //              '00-FRAME'''
      KBPOOL( 433 ) = 'INS-46000_FOV_SHAPE            = ''CIR' &
      //              'CLE'''
      KBPOOL( 434 ) = 'INS-46000_BORESIGHT            = ( 0.0' &
      //              ', 0.0, 1.0 )'
      KBPOOL( 435 ) = 'INS-46000_FOV_CLASS_SPEC       = ''ANG' &
      //              'LES'''
      KBPOOL( 436 ) = 'INS-46000_FOV_REF_VECTOR       = ( 0.0' &
      //              ', 1.0, 0.0 )'
      KBPOOL( 437 ) = 'INS-46000_FOV_REF_ANGLE        = ( 45.' &
      //              '0 )'
      KBPOOL( 438 ) = 'INS-46000_FOV_ANGLE_UNITS      = ''DEG' &
      //              'REES'''
      KBPOOL( 439 ) = ' '
      KBPOOL( 440 ) = 'INS-46100_FOV_FRAME            = ''461' &
      //              '00-FRAME'''
      KBPOOL( 441 ) = 'INS-46100_FOV_SHAPE            = ''ELL' &
      //              'IPSE'''
      KBPOOL( 442 ) = 'INS-46100_BORESIGHT            = ( 0.0' &
      //              ', 0.0, 1.0 )'
      KBPOOL( 443 ) = 'INS-46100_FOV_CLASS_SPEC       = ''ANG' &
      //              'LES'''
      KBPOOL( 444 ) = 'INS-46100_FOV_REF_VECTOR       = ( 0.0' &
      //              ', 1.0, 0.0 )'
      KBPOOL( 445 ) = 'INS-46100_FOV_REF_ANGLE        = ( 45.' &
      //              '0 )'
      KBPOOL( 446 ) = 'INS-46100_FOV_CROSS_ANGLE      = ( 30.' &
      //              '0 )'
      KBPOOL( 447 ) = 'INS-46100_FOV_ANGLE_UNITS      = ''DEG' &
      //              'REES'''
      KBPOOL( 448 ) = ' '
      KBPOOL( 449 ) = 'INS-46200_FOV_FRAME            = ''462' &
      //              '00-FRAME'''
      KBPOOL( 450 ) = 'INS-46200_FOV_SHAPE            = ''REC' &
      //              'TANGLE'''
      KBPOOL( 451 ) = 'INS-46200_BORESIGHT            = ( 0.0' &
      //              ', 0.0, 1.0 )'
      KBPOOL( 452 ) = 'INS-46200_FOV_CLASS_SPEC       = ''ANG' &
      //              'LES'''
      KBPOOL( 453 ) = 'INS-46200_FOV_REF_VECTOR       = ( 0.0' &
      //              ', 1.0, 0.0 )'
      KBPOOL( 454 ) = 'INS-46200_FOV_REF_ANGLE        = ( 45.' &
      //              '0 )'
      KBPOOL( 455 ) = 'INS-46200_FOV_CROSS_ANGLE      = ( 60.' &
      //              '0 )'
      KBPOOL( 456 ) = 'INS-46200_FOV_ANGLE_UNITS      = ''DEG' &
      //              'REES'''
      KBPOOL( 457 ) = ' '
      KBPOOL( 458 ) = ' '
      KBPOOL( 459 ) = 'INS-46201_FOV_FRAME            = ''462' &
      //              '01-FRAME'''
      KBPOOL( 460 ) = 'INS-46201_FOV_SHAPE            = ''REC' &
      //              'TANGLE'''
      KBPOOL( 461 ) = 'INS-46201_BORESIGHT            = ( 0.0' &
      //              ', 0.0, 1.0 )'
      KBPOOL( 462 ) = 'INS-46201_FOV_CLASS_SPEC       = ''ANG' &
      //              'LES'''
      KBPOOL( 463 ) = 'INS-46201_FOV_REF_VECTOR       = ( 0.0' &
      //              ', 0.5, 0.0 )'
      KBPOOL( 464 ) = 'INS-46201_FOV_REF_ANGLE        = ( 45.' &
      //              '0 )'
      KBPOOL( 465 ) = 'INS-46201_FOV_CROSS_ANGLE      = ( 60.' &
      //              '0 )'
      KBPOOL( 466 ) = 'INS-46201_FOV_ANGLE_UNITS      = ''DEG' &
      //              'REES'''
      KBPOOL( 467 ) = ' '
      KBPOOL( 468 ) = ' '
      KBPOOL( 469 ) = 'INS-46202_FOV_FRAME            = ''462' &
      //              '02-FRAME'''
      KBPOOL( 470 ) = 'INS-46202_FOV_SHAPE            = ''REC' &
      //              'TANGLE'''
      KBPOOL( 471 ) = 'INS-46202_BORESIGHT            = ( 0.0' &
      //              ', 0.0, 1.0 )'
      KBPOOL( 472 ) = 'INS-46202_FOV_CLASS_SPEC       = ''ANG' &
      //              'LES'''
      KBPOOL( 473 ) = 'INS-46202_FOV_REF_VECTOR       = ( 0.0' &
      //              ', 0.7071067811865476, 0.7071067811865476 )'
      KBPOOL( 474 ) = 'INS-46202_FOV_REF_ANGLE        = ( 45.' &
      //              '0 )'
      KBPOOL( 475 ) = 'INS-46202_FOV_CROSS_ANGLE      = ( 60.' &
      //              '0 )'
      KBPOOL( 476 ) = 'INS-46202_FOV_ANGLE_UNITS      = ''DEG' &
      //              'REES'''
      KBPOOL( 477 ) = ' '
      KBPOOL( 478 ) = ' '

!
!     Clear the kernel pool.
!
      CALL CLPOOL ( )

!
!     Now load the character buffer with the test
!     scenario definitions into the kernel pool.
!
      CALL LMPOOL ( KBPOOL, NLINES )

!
! --- Case #1: ------------------------------------------------------
!
      CALL TCASE ( 'No Frame Defined Exception' )

!
!     Frame with ID Code -10000 is missing it's FOV_FRAME keyword.
!
      CALL GETFOV ( -10000, MXBNDS, SHAPE, FRAME, BSIGHT, N, BOUNDS )

      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEMISSING)', OK )

!
! --- Case #2: ------------------------------------------------------
!
      CALL TCASE ( 'Unsupported Shape Exception' )

!
!     The -11000 FOV Shape is 'SPUD-SHAPED', an unsupported GETFOV
!     shape.
!
      CALL GETFOV ( -11000, MXBNDS, SHAPE, FRAME, BSIGHT, N, BOUNDS )

      CALL CHCKXC ( .TRUE., 'SPICE(SHAPENOTSUPPORTED)', OK )

!
! --- Case #3: ------------------------------------------------------
!
      CALL TCASE ( 'Missing Shape Definition Exception' )

!
!     The -12000 FOV Shape keyword is missing.
!
      CALL GETFOV ( -12000, MXBNDS, SHAPE, FRAME, BSIGHT, N, BOUNDS )

      CALL CHCKXC ( .TRUE., 'SPICE(SHAPEMISSING)', OK )

!
! --- Case #4: ------------------------------------------------------
!
      CALL TCASE ( 'Missing Boresight Definition Exception' )

!
!     The -12500 Boresight keyword is missing.
!
      CALL GETFOV ( -12500, MXBNDS, SHAPE, FRAME, BSIGHT, N, BOUNDS )

      CALL CHCKXC ( .TRUE., 'SPICE(BORESIGHTMISSING)', OK )

!
! --- Case #5: ------------------------------------------------------
!
      CALL TCASE ( 'Improperly Defined Boresight Vector' )

!
!     The -13000 boresight has only 2 components.
!
      CALL GETFOV ( -13000, MXBNDS, SHAPE, FRAME, BSIGHT, N, BOUNDS )

      CALL CHCKXC ( .TRUE., 'SPICE(BADBORESIGHTSPEC)', OK )

!
!     The -14000 boresight has 3 character components.
!
      CALL GETFOV ( -14000, MXBNDS, SHAPE, FRAME, BSIGHT, N, BOUNDS )

      CALL CHCKXC ( .TRUE., 'SPICE(BADBORESIGHTSPEC)', OK )

!
! --- Case #6: ------------------------------------------------------
!
      CALL TCASE ( 'Missing FOV Boundary Definition Exception' )

!
!     The -15000 FOV boundary keyword is missing.
!
      CALL GETFOV ( -15000, MXBNDS, SHAPE, FRAME, BSIGHT, N, BOUNDS )

      CALL CHCKXC ( .TRUE., 'SPICE(BOUNDARYMISSING)', OK )

!
! --- Case #7: ------------------------------------------------------
!
      CALL TCASE ( 'Not Enough ROOM Exception' )

!
!     The -16000 FOV boundary keyword contains 5 vectors, specify
!     that ROOM is only 4 and check for the exception.
!
      CALL GETFOV ( -16000, 4, SHAPE, FRAME, BSIGHT, N, BOUNDS )

      CALL CHCKXC ( .TRUE., 'SPICE(BOUNDARYTOOBIG)', OK )

!
! --- Case #8: ------------------------------------------------------
!
      CALL TCASE ( 'Boundary Vectors are 3 Vectors' )

!
!     The -17000 FOV boundary keyword contains exactly 2 DP entries.
!
      CALL GETFOV ( -17000, MXBNDS, SHAPE, FRAME, BSIGHT, N, BOUNDS )

      CALL CHCKXC ( .TRUE., 'SPICE(BADBOUNDARY)', OK )

!
! --- Case #9: ------------------------------------------------------
!
      CALL TCASE ( 'Improper Number of Boundary Corner Vectors' )

!
!     The -18100 FOV boundary keyword contains 2 boundary corner
!     vectors, but the shape is circular.
!
      CALL GETFOV ( -18100, MXBNDS, SHAPE, FRAME, BSIGHT, N, BOUNDS )

      CALL CHCKXC ( .TRUE., 'SPICE(BADBOUNDARY)', OK )

!
!     The -18200 FOV boundary keyword contains 1 boundary corner
!     vector, but the shape is elliptical.
!
      CALL GETFOV ( -18200, MXBNDS, SHAPE, FRAME, BSIGHT, N, BOUNDS )

      CALL CHCKXC ( .TRUE., 'SPICE(BADBOUNDARY)', OK )

!
!     The -18300 FOV boundary keyword contains 1 boundary corner
!     vector, but the shape is rectangular.
!
      CALL GETFOV ( -18300, MXBNDS, SHAPE, FRAME, BSIGHT, N, BOUNDS )

      CALL CHCKXC ( .TRUE., 'SPICE(BADBOUNDARY)', OK )

!
!     The -18400 FOV boundary keyword contains 1 boundary corner
!     vector, but the shape is polygonal.
!
      CALL GETFOV ( -18400, MXBNDS, SHAPE, FRAME, BSIGHT, N, BOUNDS )

      CALL CHCKXC ( .TRUE., 'SPICE(BADBOUNDARY)', OK )

!
! --- Case #10: -----------------------------------------------------
!
      CALL TCASE ( 'Nominal CIRCLE CORNERS Specification' )

!
!     The -20000 ID Code contains the definition of a circle with
!     the following characteristics:
!
      CKSHAP          = 'CIRCLE'
      CKFRAM          = '20000-FRAME'

      CKBSGT ( 1 )    = 0.0D0
      CKBSGT ( 2 )    = 0.0D0
      CKBSGT ( 3 )    = 1.0D0

      CKN             = 1

      CKBNDS ( 1, 1 ) = 1.0D0
      CKBNDS ( 2, 1 ) = 0.0D0
      CKBNDS ( 3, 1 ) = 1.0D0

!
!     Fetch the FOV definition.
!
      CALL GETFOV ( -20000, MXBNDS, SHAPE, FRAME, BSIGHT, N, BOUNDS )

!
!     Check that no exception was generated.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the parameters returned.
!
      CALL CHCKSC ( 'SHAPE', SHAPE, '=', CKSHAP, OK )
      CALL CHCKSC ( 'FRAME', FRAME, '=', CKFRAM, OK )
      CALL CHCKAD ( 'BORESIGHT', BSIGHT, '~', CKBSGT, 3, 1.0D-15, OK )
      CALL CHCKSI ( 'N', N, '=', CKN, 0, OK )

      IF ( CKN .EQ. N ) THEN

!
!        Loop over all of the boundary vectors returned and
!        check them for any errors.
!
         DO I = 1, CKN

            BNDNAM = 'BOUNDS[#]'
            CALL REPMI  ( BNDNAM, '#', I, BNDNAM )
            CALL CHCKAD ( BNDNAM, &
                          BOUNDS(1,I), &
                          '~', &
                          CKBNDS(1,I), &
                          3, &
                          1.0D-15, &
                          OK           )

         END DO

      END IF

!
! --- Case #11: -----------------------------------------------------
!
      CALL TCASE ( 'Old-Style CIRCLE CORNERS Specification' )

!
!     The -20100 ID Code contains the definition of a circle with
!     the following characteristics:
!
      CKSHAP          = 'CIRCLE'
      CKFRAM          = '20100-FRAME'

      CKBSGT ( 1 )    = 0.0D0
      CKBSGT ( 2 )    = 0.0D0
      CKBSGT ( 3 )    = 1.0D0

      CKN             = 1

      CKBNDS ( 1, 1 ) = 1.0D0
      CKBNDS ( 2, 1 ) = 0.0D0
      CKBNDS ( 3, 1 ) = 1.0D0

!
!     Fetch the FOV definition.
!
      CALL GETFOV ( -20100, MXBNDS, SHAPE, FRAME, BSIGHT, N, BOUNDS )

!
!     Check that no exception was generated.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the parameters returned.
!
      CALL CHCKSC ( 'SHAPE', SHAPE, '=', CKSHAP, OK )
      CALL CHCKSC ( 'FRAME', FRAME, '=', CKFRAM, OK )
      CALL CHCKAD ( 'BORESIGHT', BSIGHT, '~', CKBSGT, 3, 1.0D-15, OK )
      CALL CHCKSI ( 'N', N, '=', CKN, 0, OK )

      IF ( CKN .EQ. N ) THEN

!
!        Loop over all of the boundary vectors returned and
!        check them for any errors.
!
         DO I = 1, CKN

            BNDNAM = 'BOUNDS[#]'
            CALL REPMI  ( BNDNAM, '#', I, BNDNAM )
            CALL CHCKAD ( BNDNAM, &
                          BOUNDS(1,I), &
                          '~', &
                          CKBNDS(1,I), &
                          3, &
                          1.0D-15, &
                          OK           )

         END DO

      END IF

!
! --- Case #12: -----------------------------------------------------
!
      CALL TCASE ( 'Conflict CIRCLE CORNERS Specification' )

!
!     The -20200 ID Code contains the definition of a circle with
!     the following characteristics:
!
      CKSHAP          = 'CIRCLE'
      CKFRAM          = '20200-FRAME'

      CKBSGT ( 1 )    = 0.0D0
      CKBSGT ( 2 )    = 0.0D0
      CKBSGT ( 3 )    = 1.0D0

      CKN             = 1

      CKBNDS ( 1, 1 ) = 2.0D0
      CKBNDS ( 2, 1 ) = 0.0D0
      CKBNDS ( 3, 1 ) = 1.0D0

!
!     Fetch the FOV definition.
!
      CALL GETFOV ( -20200, MXBNDS, SHAPE, FRAME, BSIGHT, N, BOUNDS )

!
!     Check that no exception was generated.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the parameters returned.
!
      CALL CHCKSC ( 'SHAPE', SHAPE, '=', CKSHAP, OK )
      CALL CHCKSC ( 'FRAME', FRAME, '=', CKFRAM, OK )
      CALL CHCKAD ( 'BORESIGHT', BSIGHT, '~', CKBSGT, 3, 1.0D-15, OK )
      CALL CHCKSI ( 'N', N, '=', CKN, 0, OK )

      IF ( CKN .EQ. N ) THEN

!
!        Loop over all of the boundary vectors returned and
!        check them for any errors.
!
         DO I = 1, CKN

            BNDNAM = 'BOUNDS[#]'
            CALL REPMI  ( BNDNAM, '#', I, BNDNAM )
            CALL CHCKAD ( BNDNAM, &
                          BOUNDS(1,I), &
                          '~', &
                          CKBNDS(1,I), &
                          3, &
                          1.0D-15, &
                          OK           )

         END DO

      END IF

!
! --- Case #13: -----------------------------------------------------
!
      CALL TCASE ( 'Nominal ELLIPSE CORNERS Specification' )

!
!     The -21000 ID Code contains the definition of an ellipse with
!     the following characteristics:
!
      CKSHAP          = 'ELLIPSE'
      CKFRAM          = '21000-FRAME'

      CKBSGT ( 1 )    = 0.0D0
      CKBSGT ( 2 )    = 0.0D0
      CKBSGT ( 3 )    = 1.0D0

      CKN             = 2

      CKBNDS ( 1, 1 ) = 1.0D0
      CKBNDS ( 2, 1 ) = 0.0D0
      CKBNDS ( 3, 1 ) = 1.0D0

      CKBNDS ( 1, 2 ) = 0.0D0
      CKBNDS ( 2, 2 ) = 1.0D0
      CKBNDS ( 3, 2 ) = 1.0D0

!
!     Fetch the FOV definition.
!
      CALL GETFOV ( -21000, MXBNDS, SHAPE, FRAME, BSIGHT, N, BOUNDS )

!
!     Check that no exception was generated.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the parameters returned.
!
      CALL CHCKSC ( 'SHAPE', SHAPE, '=', CKSHAP, OK )
      CALL CHCKSC ( 'FRAME', FRAME, '=', CKFRAM, OK )
      CALL CHCKAD ( 'BORESIGHT', BSIGHT, '~', CKBSGT, 3, 1.0D-15, OK )
      CALL CHCKSI ( 'N', N, '=', CKN, 0, OK )

      IF ( CKN .EQ. N ) THEN

!
!        Loop over all of the boundary vectors returned and
!        check them for any errors.
!
         DO I = 1, CKN

            BNDNAM = 'BOUNDS[#]'
            CALL REPMI  ( BNDNAM, '#', I, BNDNAM )
            CALL CHCKAD ( BNDNAM, &
                          BOUNDS(1,I), &
                          '~', &
                          CKBNDS(1,I), &
                          3, &
                          1.0D-15, &
                          OK           )

         END DO

      END IF

!
! --- Case #14: -----------------------------------------------------
!
      CALL TCASE ( 'Old-Style ELLIPSE CORNERS Specification' )

!
!     The -21100 ID Code contains the definition of an ellipse with
!     the following characteristics:
!
      CKSHAP          = 'ELLIPSE'
      CKFRAM          = '21100-FRAME'

      CKBSGT ( 1 )    = 0.0D0
      CKBSGT ( 2 )    = 0.0D0
      CKBSGT ( 3 )    = 1.0D0

      CKN             = 2

      CKBNDS ( 1, 1 ) = 1.0D0
      CKBNDS ( 2, 1 ) = 0.0D0
      CKBNDS ( 3, 1 ) = 1.0D0

      CKBNDS ( 1, 2 ) = 0.0D0
      CKBNDS ( 2, 2 ) = 1.0D0
      CKBNDS ( 3, 2 ) = 1.0D0

!
!     Fetch the FOV definition.
!
      CALL GETFOV ( -21100, MXBNDS, SHAPE, FRAME, BSIGHT, N, BOUNDS )

!
!     Check that no exception was generated.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the parameters returned.
!
      CALL CHCKSC ( 'SHAPE', SHAPE, '=', CKSHAP, OK )
      CALL CHCKSC ( 'FRAME', FRAME, '=', CKFRAM, OK )
      CALL CHCKAD ( 'BORESIGHT', BSIGHT, '~', CKBSGT, 3, 1.0D-15, OK )
      CALL CHCKSI ( 'N', N, '=', CKN, 0, OK )

      IF ( CKN .EQ. N ) THEN

!
!        Loop over all of the boundary vectors returned and
!        check them for any errors.
!
         DO I = 1, CKN

            BNDNAM = 'BOUNDS[#]'
            CALL REPMI  ( BNDNAM, '#', I, BNDNAM )
            CALL CHCKAD ( BNDNAM, &
                          BOUNDS(1,I), &
                          '~', &
                          CKBNDS(1,I), &
                          3, &
                          1.0D-15, &
                          OK           )

         END DO

      END IF

!
! --- Case #15: -----------------------------------------------------
!
      CALL TCASE ( 'Conflict ELLIPSE CORNERS Specification' )

!
!     The -21200 ID Code contains the definition of an ellipse with
!     the following characteristics:
!
      CKSHAP          = 'ELLIPSE'
      CKFRAM          = '21200-FRAME'

      CKBSGT ( 1 )    = 0.0D0
      CKBSGT ( 2 )    = 0.0D0
      CKBSGT ( 3 )    = 1.0D0

      CKN             = 2

      CKBNDS ( 1, 1 ) = 2.0D0
      CKBNDS ( 2, 1 ) = 0.0D0
      CKBNDS ( 3, 1 ) = 1.0D0

      CKBNDS ( 1, 2 ) = 0.0D0
      CKBNDS ( 2, 2 ) = 2.0D0
      CKBNDS ( 3, 2 ) = 1.0D0

!
!     Fetch the FOV definition.
!
      CALL GETFOV ( -21200, MXBNDS, SHAPE, FRAME, BSIGHT, N, BOUNDS )

!
!     Check that no exception was generated.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the parameters returned.
!
      CALL CHCKSC ( 'SHAPE', SHAPE, '=', CKSHAP, OK )
      CALL CHCKSC ( 'FRAME', FRAME, '=', CKFRAM, OK )
      CALL CHCKAD ( 'BORESIGHT', BSIGHT, '~', CKBSGT, 3, 1.0D-15, OK )
      CALL CHCKSI ( 'N', N, '=', CKN, 0, OK )

      IF ( CKN .EQ. N ) THEN

!
!        Loop over all of the boundary vectors returned and
!        check them for any errors.
!
         DO I = 1, CKN

            BNDNAM = 'BOUNDS[#]'
            CALL REPMI  ( BNDNAM, '#', I, BNDNAM )
            CALL CHCKAD ( BNDNAM, &
                          BOUNDS(1,I), &
                          '~', &
                          CKBNDS(1,I), &
                          3, &
                          1.0D-15, &
                          OK           )

         END DO

      END IF

!
! --- Case #16: -----------------------------------------------------
!
      CALL TCASE ( 'Nominal RECTANGLE CORNERS Specification' )

!
!     The -22000 ID Code contains the definition of a rectangle with
!     the following characteristics:
!
      CKSHAP          = 'RECTANGLE'
      CKFRAM          = '22000-FRAME'

      CKBSGT ( 1 )    = 0.0D0
      CKBSGT ( 2 )    = 0.0D0
      CKBSGT ( 3 )    = 1.0D0

      CKN             = 4

      CKBNDS ( 1, 1 ) =  1.0D0
      CKBNDS ( 2, 1 ) =  1.0D0
      CKBNDS ( 3, 1 ) =  1.0D0

      CKBNDS ( 1, 2 ) =  1.0D0
      CKBNDS ( 2, 2 ) = -1.0D0
      CKBNDS ( 3, 2 ) =  1.0D0

      CKBNDS ( 1, 3 ) = -1.0D0
      CKBNDS ( 2, 3 ) = -1.0D0
      CKBNDS ( 3, 3 ) =  1.0D0

      CKBNDS ( 1, 4 ) = -1.0D0
      CKBNDS ( 2, 4 ) =  1.0D0
      CKBNDS ( 3, 4 ) =  1.0D0

!
!     Fetch the FOV definition.
!
      CALL GETFOV ( -22000, MXBNDS, SHAPE, FRAME, BSIGHT, N, BOUNDS )

!
!     Check that no exception was generated.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the parameters returned.
!
      CALL CHCKSC ( 'SHAPE', SHAPE, '=', CKSHAP, OK )
      CALL CHCKSC ( 'FRAME', FRAME, '=', CKFRAM, OK )
      CALL CHCKAD ( 'BORESIGHT', BSIGHT, '~', CKBSGT, 3, 1.0D-15, OK )
      CALL CHCKSI ( 'N', N, '=', CKN, 0, OK )

      IF ( CKN .EQ. N ) THEN

!
!        Loop over all of the boundary vectors returned and
!        check them for any errors.
!
         DO I = 1, CKN

            BNDNAM = 'BOUNDS[#]'
            CALL REPMI  ( BNDNAM, '#', I, BNDNAM )
            CALL CHCKAD ( BNDNAM, &
                          BOUNDS(1,I), &
                          '~', &
                          CKBNDS(1,I), &
                          3, &
                          1.0D-15, &
                          OK           )

         END DO

      END IF

!
! --- Case #17: -----------------------------------------------------
!
      CALL TCASE ( 'Old-Style RECTANGLE CORNERS Specification' )

!
!     The -22100 ID Code contains the definition of a rectangle with
!     the following characteristics:
!
      CKSHAP          = 'RECTANGLE'
      CKFRAM          = '22100-FRAME'

      CKBSGT ( 1 )    = 0.0D0
      CKBSGT ( 2 )    = 0.0D0
      CKBSGT ( 3 )    = 1.0D0

      CKN             = 4

      CKBNDS ( 1, 1 ) =  1.0D0
      CKBNDS ( 2, 1 ) =  1.0D0
      CKBNDS ( 3, 1 ) =  1.0D0

      CKBNDS ( 1, 2 ) =  1.0D0
      CKBNDS ( 2, 2 ) = -1.0D0
      CKBNDS ( 3, 2 ) =  1.0D0

      CKBNDS ( 1, 3 ) = -1.0D0
      CKBNDS ( 2, 3 ) = -1.0D0
      CKBNDS ( 3, 3 ) =  1.0D0

      CKBNDS ( 1, 4 ) = -1.0D0
      CKBNDS ( 2, 4 ) =  1.0D0
      CKBNDS ( 3, 4 ) =  1.0D0

!
!     Fetch the FOV definition.
!
      CALL GETFOV ( -22100, MXBNDS, SHAPE, FRAME, BSIGHT, N, BOUNDS )

!
!     Check that no exception was generated.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the parameters returned.
!
      CALL CHCKSC ( 'SHAPE', SHAPE, '=', CKSHAP, OK )
      CALL CHCKSC ( 'FRAME', FRAME, '=', CKFRAM, OK )
      CALL CHCKAD ( 'BORESIGHT', BSIGHT, '~', CKBSGT, 3, 1.0D-15, OK )
      CALL CHCKSI ( 'N', N, '=', CKN, 0, OK )

      IF ( CKN .EQ. N ) THEN

!
!        Loop over all of the boundary vectors returned and
!        check them for any errors.
!
         DO I = 1, CKN

            BNDNAM = 'BOUNDS[#]'
            CALL REPMI  ( BNDNAM, '#', I, BNDNAM )
            CALL CHCKAD ( BNDNAM, &
                          BOUNDS(1,I), &
                          '~', &
                          CKBNDS(1,I), &
                          3, &
                          1.0D-15, &
                          OK           )

         END DO

      END IF

!
! --- Case #18: -----------------------------------------------------
!
      CALL TCASE ( 'Conflict RECTANGLE CORNERS Specification' )

!
!     The -22200 ID Code contains the definition of a rectangle with
!     the following characteristics:
!
      CKSHAP          = 'RECTANGLE'
      CKFRAM          = '22200-FRAME'

      CKBSGT ( 1 )    = 0.0D0
      CKBSGT ( 2 )    = 0.0D0
      CKBSGT ( 3 )    = 1.0D0

      CKN             = 4

      CKBNDS ( 1, 1 ) =  2.0D0
      CKBNDS ( 2, 1 ) =  2.0D0
      CKBNDS ( 3, 1 ) =  1.0D0

      CKBNDS ( 1, 2 ) =  2.0D0
      CKBNDS ( 2, 2 ) = -2.0D0
      CKBNDS ( 3, 2 ) =  1.0D0

      CKBNDS ( 1, 3 ) = -2.0D0
      CKBNDS ( 2, 3 ) = -2.0D0
      CKBNDS ( 3, 3 ) =  1.0D0

      CKBNDS ( 1, 4 ) = -2.0D0
      CKBNDS ( 2, 4 ) =  2.0D0
      CKBNDS ( 3, 4 ) =  1.0D0

!
!     Fetch the FOV definition.
!
      CALL GETFOV ( -22200, MXBNDS, SHAPE, FRAME, BSIGHT, N, BOUNDS )

!
!     Check that no exception was generated.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the parameters returned.
!
      CALL CHCKSC ( 'SHAPE', SHAPE, '=', CKSHAP, OK )
      CALL CHCKSC ( 'FRAME', FRAME, '=', CKFRAM, OK )
      CALL CHCKAD ( 'BORESIGHT', BSIGHT, '~', CKBSGT, 3, 1.0D-15, OK )
      CALL CHCKSI ( 'N', N, '=', CKN, 0, OK )

      IF ( CKN .EQ. N ) THEN

!
!        Loop over all of the boundary vectors returned and
!        check them for any errors.
!
         DO I = 1, CKN

            BNDNAM = 'BOUNDS[#]'
            CALL REPMI  ( BNDNAM, '#', I, BNDNAM )
            CALL CHCKAD ( BNDNAM, &
                          BOUNDS(1,I), &
                          '~', &
                          CKBNDS(1,I), &
                          3, &
                          1.0D-15, &
                          OK           )

         END DO

      END IF

!
! --- Case #19: -----------------------------------------------------
!
      CALL TCASE ( 'Nominal POLYGON CORNERS Specification' )

!
!     The -23000 ID Code contains the definition of a polygon with
!     the following characteristics:
!
      CKSHAP          = 'POLYGON'
      CKFRAM          = '23000-FRAME'

      CKBSGT ( 1 )    = 0.0D0
      CKBSGT ( 2 )    = 0.0D0
      CKBSGT ( 3 )    = 1.0D0

      CKN             = 4

      CKBNDS ( 1, 1 ) =  1.0D0
      CKBNDS ( 2, 1 ) =  1.0D0
      CKBNDS ( 3, 1 ) =  1.0D0

      CKBNDS ( 1, 2 ) =  1.0D0
      CKBNDS ( 2, 2 ) = -1.0D0
      CKBNDS ( 3, 2 ) =  1.0D0

      CKBNDS ( 1, 3 ) = -1.0D0
      CKBNDS ( 2, 3 ) = -1.0D0
      CKBNDS ( 3, 3 ) =  1.0D0

      CKBNDS ( 1, 4 ) = -1.0D0
      CKBNDS ( 2, 4 ) =  1.0D0
      CKBNDS ( 3, 4 ) =  1.0D0

!
!     Fetch the FOV definition.
!
      CALL GETFOV ( -23000, MXBNDS, SHAPE, FRAME, BSIGHT, N, BOUNDS )

!
!     Check that no exception was generated.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the parameters returned.
!
      CALL CHCKSC ( 'SHAPE', SHAPE, '=', CKSHAP, OK )
      CALL CHCKSC ( 'FRAME', FRAME, '=', CKFRAM, OK )
      CALL CHCKAD ( 'BORESIGHT', BSIGHT, '~', CKBSGT, 3, 1.0D-15, OK )
      CALL CHCKSI ( 'N', N, '=', CKN, 0, OK )

      IF ( CKN .EQ. N ) THEN

!
!        Loop over all of the boundary vectors returned and
!        check them for any errors.
!
         DO I = 1, CKN

            BNDNAM = 'BOUNDS[#]'
            CALL REPMI  ( BNDNAM, '#', I, BNDNAM )
            CALL CHCKAD ( BNDNAM, &
                          BOUNDS(1,I), &
                          '~', &
                          CKBNDS(1,I), &
                          3, &
                          1.0D-15, &
                          OK           )

         END DO

      END IF

!
! --- Case #20: -----------------------------------------------------
!
      CALL TCASE ( 'Old-Style POLYGON CORNERS Specification' )

!
!     The -23100 ID Code contains the definition of a polygon with
!     the following characteristics:
!
      CKSHAP          = 'POLYGON'
      CKFRAM          = '23100-FRAME'

      CKBSGT ( 1 )    = 0.0D0
      CKBSGT ( 2 )    = 0.0D0
      CKBSGT ( 3 )    = 1.0D0

      CKN             = 4

      CKBNDS ( 1, 1 ) =  1.0D0
      CKBNDS ( 2, 1 ) =  1.0D0
      CKBNDS ( 3, 1 ) =  1.0D0

      CKBNDS ( 1, 2 ) =  1.0D0
      CKBNDS ( 2, 2 ) = -1.0D0
      CKBNDS ( 3, 2 ) =  1.0D0

      CKBNDS ( 1, 3 ) = -1.0D0
      CKBNDS ( 2, 3 ) = -1.0D0
      CKBNDS ( 3, 3 ) =  1.0D0

      CKBNDS ( 1, 4 ) = -1.0D0
      CKBNDS ( 2, 4 ) =  1.0D0
      CKBNDS ( 3, 4 ) =  1.0D0

!
!     Fetch the FOV definition.
!
      CALL GETFOV ( -23100, MXBNDS, SHAPE, FRAME, BSIGHT, N, BOUNDS )

!
!     Check that no exception was generated.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the parameters returned.
!
      CALL CHCKSC ( 'SHAPE', SHAPE, '=', CKSHAP, OK )
      CALL CHCKSC ( 'FRAME', FRAME, '=', CKFRAM, OK )
      CALL CHCKAD ( 'BORESIGHT', BSIGHT, '~', CKBSGT, 3, 1.0D-15, OK )
      CALL CHCKSI ( 'N', N, '=', CKN, 0, OK )

      IF ( CKN .EQ. N ) THEN

!
!        Loop over all of the boundary vectors returned and
!        check them for any errors.
!
         DO I = 1, CKN

            BNDNAM = 'BOUNDS[#]'
            CALL REPMI  ( BNDNAM, '#', I, BNDNAM )
            CALL CHCKAD ( BNDNAM, &
                          BOUNDS(1,I), &
                          '~', &
                          CKBNDS(1,I), &
                          3, &
                          1.0D-15, &
                          OK           )

         END DO

      END IF


!
! --- Case #21: -----------------------------------------------------
!
      CALL TCASE ( 'Conflict POLYGON CORNERS Specification' )

!
!     The -23200 ID Code contains the definition of a polygon with
!     the following characteristics:
!
      CKSHAP          = 'POLYGON'
      CKFRAM          = '23200-FRAME'

      CKBSGT ( 1 )    = 0.0D0
      CKBSGT ( 2 )    = 0.0D0
      CKBSGT ( 3 )    = 1.0D0

      CKN             = 4

      CKBNDS ( 1, 1 ) =  2.0D0
      CKBNDS ( 2, 1 ) =  2.0D0
      CKBNDS ( 3, 1 ) =  1.0D0

      CKBNDS ( 1, 2 ) =  2.0D0
      CKBNDS ( 2, 2 ) = -2.0D0
      CKBNDS ( 3, 2 ) =  1.0D0

      CKBNDS ( 1, 3 ) = -2.0D0
      CKBNDS ( 2, 3 ) = -2.0D0
      CKBNDS ( 3, 3 ) =  1.0D0

      CKBNDS ( 1, 4 ) = -2.0D0
      CKBNDS ( 2, 4 ) =  2.0D0
      CKBNDS ( 3, 4 ) =  1.0D0

!
!     Fetch the FOV definition.
!
      CALL GETFOV ( -23200, MXBNDS, SHAPE, FRAME, BSIGHT, N, BOUNDS )

!
!     Check that no exception was generated.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the parameters returned.
!
      CALL CHCKSC ( 'SHAPE', SHAPE, '=', CKSHAP, OK )
      CALL CHCKSC ( 'FRAME', FRAME, '=', CKFRAM, OK )
      CALL CHCKAD ( 'BORESIGHT', BSIGHT, '~', CKBSGT, 3, 1.0D-15, OK )
      CALL CHCKSI ( 'N', N, '=', CKN, 0, OK )

      IF ( CKN .EQ. N ) THEN

!
!        Loop over all of the boundary vectors returned and
!        check them for any errors.
!
         DO I = 1, CKN

            BNDNAM = 'BOUNDS[#]'
            CALL REPMI  ( BNDNAM, '#', I, BNDNAM )
            CALL CHCKAD ( BNDNAM, &
                          BOUNDS(1,I), &
                          '~', &
                          CKBNDS(1,I), &
                          3, &
                          1.0D-15, &
                          OK           )

         END DO

      END IF

!
! --- Case #22: -----------------------------------------------------
!
      CALL TCASE ( 'Nominal CIRCLE CORNERS-SET Specification' )

!
!     The -30000 ID Code contains the definition of a circle with
!     the following characteristics:
!
      CKSHAP          = 'CIRCLE'
      CKFRAM          = '30000-FRAME'

      CKBSGT ( 1 )    = 0.0D0
      CKBSGT ( 2 )    = 0.0D0
      CKBSGT ( 3 )    = 1.0D0

      CKN             = 1

      CKBNDS ( 1, 1 ) = 1.0D0
      CKBNDS ( 2, 1 ) = 0.0D0
      CKBNDS ( 3, 1 ) = 1.0D0

!
!     Fetch the FOV definition.
!
      CALL GETFOV ( -30000, MXBNDS, SHAPE, FRAME, BSIGHT, N, BOUNDS )

!
!     Check that no exception was generated.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the parameters returned.
!
      CALL CHCKSC ( 'SHAPE', SHAPE, '=', CKSHAP, OK )
      CALL CHCKSC ( 'FRAME', FRAME, '=', CKFRAM, OK )
      CALL CHCKAD ( 'BORESIGHT', BSIGHT, '~', CKBSGT, 3, 1.0D-15, OK )
      CALL CHCKSI ( 'N', N, '=', CKN, 0, OK )

      IF ( CKN .EQ. N ) THEN

!
!        Loop over all of the boundary vectors returned and
!        check them for any errors.
!
         DO I = 1, CKN

            BNDNAM = 'BOUNDS[#]'
            CALL REPMI  ( BNDNAM, '#', I, BNDNAM )
            CALL CHCKAD ( BNDNAM, &
                          BOUNDS(1,I), &
                          '~', &
                          CKBNDS(1,I), &
                          3, &
                          1.0D-15, &
                          OK           )

         END DO

      END IF

!
! --- Case #23: -----------------------------------------------------
!
      CALL TCASE ( 'Old-Style CIRCLE CORNERS-SET Specification' )

!
!     The -30100 ID Code contains the definition of a circle with
!     the following characteristics:
!
      CKSHAP          = 'CIRCLE'
      CKFRAM          = '30100-FRAME'

      CKBSGT ( 1 )    = 0.0D0
      CKBSGT ( 2 )    = 0.0D0
      CKBSGT ( 3 )    = 1.0D0

      CKN             = 1

      CKBNDS ( 1, 1 ) = 1.0D0
      CKBNDS ( 2, 1 ) = 0.0D0
      CKBNDS ( 3, 1 ) = 1.0D0

!
!     Fetch the FOV definition.
!
      CALL GETFOV ( -30100, MXBNDS, SHAPE, FRAME, BSIGHT, N, BOUNDS )

!
!     Check that no exception was generated.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the parameters returned.
!
      CALL CHCKSC ( 'SHAPE', SHAPE, '=', CKSHAP, OK )
      CALL CHCKSC ( 'FRAME', FRAME, '=', CKFRAM, OK )
      CALL CHCKAD ( 'BORESIGHT', BSIGHT, '~', CKBSGT, 3, 1.0D-15, OK )
      CALL CHCKSI ( 'N', N, '=', CKN, 0, OK )

      IF ( CKN .EQ. N ) THEN

!
!        Loop over all of the boundary vectors returned and
!        check them for any errors.
!
         DO I = 1, CKN

            BNDNAM = 'BOUNDS[#]'
            CALL REPMI  ( BNDNAM, '#', I, BNDNAM )
            CALL CHCKAD ( BNDNAM, &
                          BOUNDS(1,I), &
                          '~', &
                          CKBNDS(1,I), &
                          3, &
                          1.0D-15, &
                          OK           )

         END DO

      END IF

!
! --- Case #24: -----------------------------------------------------
!
      CALL TCASE ( 'Conflict CIRCLE CORNERS-SET Specification' )

!
!     The -30200 ID Code contains the definition of a circle with
!     the following characteristics:
!
      CKSHAP          = 'CIRCLE'
      CKFRAM          = '30200-FRAME'

      CKBSGT ( 1 )    = 0.0D0
      CKBSGT ( 2 )    = 0.0D0
      CKBSGT ( 3 )    = 1.0D0

      CKN             = 1

      CKBNDS ( 1, 1 ) = 2.0D0
      CKBNDS ( 2, 1 ) = 0.0D0
      CKBNDS ( 3, 1 ) = 1.0D0

!
!     Fetch the FOV definition.
!
      CALL GETFOV ( -30200, MXBNDS, SHAPE, FRAME, BSIGHT, N, BOUNDS )

!
!     Check that no exception was generated.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the parameters returned.
!
      CALL CHCKSC ( 'SHAPE', SHAPE, '=', CKSHAP, OK )
      CALL CHCKSC ( 'FRAME', FRAME, '=', CKFRAM, OK )
      CALL CHCKAD ( 'BORESIGHT', BSIGHT, '~', CKBSGT, 3, 1.0D-15, OK )
      CALL CHCKSI ( 'N', N, '=', CKN, 0, OK )

      IF ( CKN .EQ. N ) THEN

!
!        Loop over all of the boundary vectors returned and
!        check them for any errors.
!
         DO I = 1, CKN

            BNDNAM = 'BOUNDS[#]'
            CALL REPMI  ( BNDNAM, '#', I, BNDNAM )
            CALL CHCKAD ( BNDNAM, &
                          BOUNDS(1,I), &
                          '~', &
                          CKBNDS(1,I), &
                          3, &
                          1.0D-15, &
                          OK           )

         END DO

      END IF

!
! --- Case #25: -----------------------------------------------------
!
      CALL TCASE ( 'Nominal ELLIPSE CORNERS-SET Specification' )

!
!     The -31000 ID Code contains the definition of an ellipse with
!     the following characteristics:
!
      CKSHAP          = 'ELLIPSE'
      CKFRAM          = '31000-FRAME'

      CKBSGT ( 1 )    = 0.0D0
      CKBSGT ( 2 )    = 0.0D0
      CKBSGT ( 3 )    = 1.0D0

      CKN             = 2

      CKBNDS ( 1, 1 ) = 1.0D0
      CKBNDS ( 2, 1 ) = 0.0D0
      CKBNDS ( 3, 1 ) = 1.0D0

      CKBNDS ( 1, 2 ) = 0.0D0
      CKBNDS ( 2, 2 ) = 1.0D0
      CKBNDS ( 3, 2 ) = 1.0D0

!
!     Fetch the FOV definition.
!
      CALL GETFOV ( -31000, MXBNDS, SHAPE, FRAME, BSIGHT, N, BOUNDS )

!
!     Check that no exception was generated.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the parameters returned.
!
      CALL CHCKSC ( 'SHAPE', SHAPE, '=', CKSHAP, OK )
      CALL CHCKSC ( 'FRAME', FRAME, '=', CKFRAM, OK )
      CALL CHCKAD ( 'BORESIGHT', BSIGHT, '~', CKBSGT, 3, 1.0D-15, OK )
      CALL CHCKSI ( 'N', N, '=', CKN, 0, OK )

      IF ( CKN .EQ. N ) THEN

!
!        Loop over all of the boundary vectors returned and
!        check them for any errors.
!
         DO I = 1, CKN

            BNDNAM = 'BOUNDS[#]'
            CALL REPMI  ( BNDNAM, '#', I, BNDNAM )
            CALL CHCKAD ( BNDNAM, &
                          BOUNDS(1,I), &
                          '~', &
                          CKBNDS(1,I), &
                          3, &
                          1.0D-15, &
                          OK           )

         END DO

      END IF

!
! --- Case #26: -----------------------------------------------------
!
      CALL TCASE ( 'Old-Style ELLIPSE CORNERS-SET Specification' )

!
!     The -31100 ID Code contains the definition of an ellipse with
!     the following characteristics:
!
      CKSHAP          = 'ELLIPSE'
      CKFRAM          = '31100-FRAME'

      CKBSGT ( 1 )    = 0.0D0
      CKBSGT ( 2 )    = 0.0D0
      CKBSGT ( 3 )    = 1.0D0

      CKN             = 2

      CKBNDS ( 1, 1 ) = 1.0D0
      CKBNDS ( 2, 1 ) = 0.0D0
      CKBNDS ( 3, 1 ) = 1.0D0

      CKBNDS ( 1, 2 ) = 0.0D0
      CKBNDS ( 2, 2 ) = 1.0D0
      CKBNDS ( 3, 2 ) = 1.0D0

!
!     Fetch the FOV definition.
!
      CALL GETFOV ( -31100, MXBNDS, SHAPE, FRAME, BSIGHT, N, BOUNDS )

!
!     Check that no exception was generated.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the parameters returned.
!
      CALL CHCKSC ( 'SHAPE', SHAPE, '=', CKSHAP, OK )
      CALL CHCKSC ( 'FRAME', FRAME, '=', CKFRAM, OK )
      CALL CHCKAD ( 'BORESIGHT', BSIGHT, '~', CKBSGT, 3, 1.0D-15, OK )
      CALL CHCKSI ( 'N', N, '=', CKN, 0, OK )

      IF ( CKN .EQ. N ) THEN

!
!        Loop over all of the boundary vectors returned and
!        check them for any errors.
!
         DO I = 1, CKN

            BNDNAM = 'BOUNDS[#]'
            CALL REPMI  ( BNDNAM, '#', I, BNDNAM )
            CALL CHCKAD ( BNDNAM, &
                          BOUNDS(1,I), &
                          '~', &
                          CKBNDS(1,I), &
                          3, &
                          1.0D-15, &
                          OK           )

         END DO

      END IF

!
! --- Case #27: -----------------------------------------------------
!
      CALL TCASE ( 'Conflict ELLIPSE CORNERS-SET Specification' )

!
!     The -31200 ID Code contains the definition of an ellipse with
!     the following characteristics:
!
      CKSHAP          = 'ELLIPSE'
      CKFRAM          = '31200-FRAME'

      CKBSGT ( 1 )    = 0.0D0
      CKBSGT ( 2 )    = 0.0D0
      CKBSGT ( 3 )    = 1.0D0

      CKN             = 2

      CKBNDS ( 1, 1 ) = 2.0D0
      CKBNDS ( 2, 1 ) = 0.0D0
      CKBNDS ( 3, 1 ) = 1.0D0

      CKBNDS ( 1, 2 ) = 0.0D0
      CKBNDS ( 2, 2 ) = 2.0D0
      CKBNDS ( 3, 2 ) = 1.0D0

!
!     Fetch the FOV definition.
!
      CALL GETFOV ( -31200, MXBNDS, SHAPE, FRAME, BSIGHT, N, BOUNDS )

!
!     Check that no exception was generated.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the parameters returned.
!
      CALL CHCKSC ( 'SHAPE', SHAPE, '=', CKSHAP, OK )
      CALL CHCKSC ( 'FRAME', FRAME, '=', CKFRAM, OK )
      CALL CHCKAD ( 'BORESIGHT', BSIGHT, '~', CKBSGT, 3, 1.0D-15, OK )
      CALL CHCKSI ( 'N', N, '=', CKN, 0, OK )

      IF ( CKN .EQ. N ) THEN

!
!        Loop over all of the boundary vectors returned and
!        check them for any errors.
!
         DO I = 1, CKN

            BNDNAM = 'BOUNDS[#]'
            CALL REPMI  ( BNDNAM, '#', I, BNDNAM )
            CALL CHCKAD ( BNDNAM, &
                          BOUNDS(1,I), &
                          '~', &
                          CKBNDS(1,I), &
                          3, &
                          1.0D-15, &
                          OK           )

         END DO

      END IF

!
! --- Case #28: -----------------------------------------------------
!
      CALL TCASE ( 'Nominal RECTANGLE CORNERS-SET Specification' )

!
!     The -32000 ID Code contains the definition of a rectangle with
!     the following characteristics:
!
      CKSHAP          = 'RECTANGLE'
      CKFRAM          = '32000-FRAME'

      CKBSGT ( 1 )    = 0.0D0
      CKBSGT ( 2 )    = 0.0D0
      CKBSGT ( 3 )    = 1.0D0

      CKN             = 4

      CKBNDS ( 1, 1 ) =  1.0D0
      CKBNDS ( 2, 1 ) =  1.0D0
      CKBNDS ( 3, 1 ) =  1.0D0

      CKBNDS ( 1, 2 ) =  1.0D0
      CKBNDS ( 2, 2 ) = -1.0D0
      CKBNDS ( 3, 2 ) =  1.0D0

      CKBNDS ( 1, 3 ) = -1.0D0
      CKBNDS ( 2, 3 ) = -1.0D0
      CKBNDS ( 3, 3 ) =  1.0D0

      CKBNDS ( 1, 4 ) = -1.0D0
      CKBNDS ( 2, 4 ) =  1.0D0
      CKBNDS ( 3, 4 ) =  1.0D0

!
!     Fetch the FOV definition.
!
      CALL GETFOV ( -32000, MXBNDS, SHAPE, FRAME, BSIGHT, N, BOUNDS )

!
!     Check that no exception was generated.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the parameters returned.
!
      CALL CHCKSC ( 'SHAPE', SHAPE, '=', CKSHAP, OK )
      CALL CHCKSC ( 'FRAME', FRAME, '=', CKFRAM, OK )
      CALL CHCKAD ( 'BORESIGHT', BSIGHT, '~', CKBSGT, 3, 1.0D-15, OK )
      CALL CHCKSI ( 'N', N, '=', CKN, 0, OK )

      IF ( CKN .EQ. N ) THEN

!
!        Loop over all of the boundary vectors returned and
!        check them for any errors.
!
         DO I = 1, CKN

            BNDNAM = 'BOUNDS[#]'
            CALL REPMI  ( BNDNAM, '#', I, BNDNAM )
            CALL CHCKAD ( BNDNAM, &
                          BOUNDS(1,I), &
                          '~', &
                          CKBNDS(1,I), &
                          3, &
                          1.0D-15, &
                          OK           )

         END DO

      END IF

!
! --- Case #29: -----------------------------------------------------
!
      CALL TCASE ( 'Old-Style RECTANGLE CORNERS-SET Specification' )

!
!     The -32100 ID Code contains the definition of a rectangle with
!     the following characteristics:
!
      CKSHAP          = 'RECTANGLE'
      CKFRAM          = '32100-FRAME'

      CKBSGT ( 1 )    = 0.0D0
      CKBSGT ( 2 )    = 0.0D0
      CKBSGT ( 3 )    = 1.0D0

      CKN             = 4

      CKBNDS ( 1, 1 ) =  1.0D0
      CKBNDS ( 2, 1 ) =  1.0D0
      CKBNDS ( 3, 1 ) =  1.0D0

      CKBNDS ( 1, 2 ) =  1.0D0
      CKBNDS ( 2, 2 ) = -1.0D0
      CKBNDS ( 3, 2 ) =  1.0D0

      CKBNDS ( 1, 3 ) = -1.0D0
      CKBNDS ( 2, 3 ) = -1.0D0
      CKBNDS ( 3, 3 ) =  1.0D0

      CKBNDS ( 1, 4 ) = -1.0D0
      CKBNDS ( 2, 4 ) =  1.0D0
      CKBNDS ( 3, 4 ) =  1.0D0

!
!     Fetch the FOV definition.
!
      CALL GETFOV ( -32100, MXBNDS, SHAPE, FRAME, BSIGHT, N, BOUNDS )

!
!     Check that no exception was generated.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the parameters returned.
!
      CALL CHCKSC ( 'SHAPE', SHAPE, '=', CKSHAP, OK )
      CALL CHCKSC ( 'FRAME', FRAME, '=', CKFRAM, OK )
      CALL CHCKAD ( 'BORESIGHT', BSIGHT, '~', CKBSGT, 3, 1.0D-15, OK )
      CALL CHCKSI ( 'N', N, '=', CKN, 0, OK )

      IF ( CKN .EQ. N ) THEN

!
!        Loop over all of the boundary vectors returned and
!        check them for any errors.
!
         DO I = 1, CKN

            BNDNAM = 'BOUNDS[#]'
            CALL REPMI  ( BNDNAM, '#', I, BNDNAM )
            CALL CHCKAD ( BNDNAM, &
                          BOUNDS(1,I), &
                          '~', &
                          CKBNDS(1,I), &
                          3, &
                          1.0D-15, &
                          OK           )

         END DO

      END IF

!
! --- Case #30: -----------------------------------------------------
!
      CALL TCASE ( 'Conflict RECTANGLE CORNERS-SET Specification' )

!
!     The -32200 ID Code contains the definition of a rectangle with
!     the following characteristics:
!
      CKSHAP          = 'RECTANGLE'
      CKFRAM          = '32200-FRAME'

      CKBSGT ( 1 )    = 0.0D0
      CKBSGT ( 2 )    = 0.0D0
      CKBSGT ( 3 )    = 1.0D0

      CKN             = 4

      CKBNDS ( 1, 1 ) =  2.0D0
      CKBNDS ( 2, 1 ) =  2.0D0
      CKBNDS ( 3, 1 ) =  1.0D0

      CKBNDS ( 1, 2 ) =  2.0D0
      CKBNDS ( 2, 2 ) = -2.0D0
      CKBNDS ( 3, 2 ) =  1.0D0

      CKBNDS ( 1, 3 ) = -2.0D0
      CKBNDS ( 2, 3 ) = -2.0D0
      CKBNDS ( 3, 3 ) =  1.0D0

      CKBNDS ( 1, 4 ) = -2.0D0
      CKBNDS ( 2, 4 ) =  2.0D0
      CKBNDS ( 3, 4 ) =  1.0D0

!
!     Fetch the FOV definition.
!
      CALL GETFOV ( -32200, MXBNDS, SHAPE, FRAME, BSIGHT, N, BOUNDS )

!
!     Check that no exception was generated.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the parameters returned.
!
      CALL CHCKSC ( 'SHAPE', SHAPE, '=', CKSHAP, OK )
      CALL CHCKSC ( 'FRAME', FRAME, '=', CKFRAM, OK )
      CALL CHCKAD ( 'BORESIGHT', BSIGHT, '~', CKBSGT, 3, 1.0D-15, OK )
      CALL CHCKSI ( 'N', N, '=', CKN, 0, OK )

      IF ( CKN .EQ. N ) THEN

!
!        Loop over all of the boundary vectors returned and
!        check them for any errors.
!
         DO I = 1, CKN

            BNDNAM = 'BOUNDS[#]'
            CALL REPMI  ( BNDNAM, '#', I, BNDNAM )
            CALL CHCKAD ( BNDNAM, &
                          BOUNDS(1,I), &
                          '~', &
                          CKBNDS(1,I), &
                          3, &
                          1.0D-15, &
                          OK           )

         END DO

      END IF

!
! --- Case #31: -----------------------------------------------------
!
      CALL TCASE ( 'Nominal POLYGON CORNERS-SET Specification' )

!
!     The -33000 ID Code contains the definition of a polygon with
!     the following characteristics:
!
      CKSHAP          = 'POLYGON'
      CKFRAM          = '33000-FRAME'

      CKBSGT ( 1 )    = 0.0D0
      CKBSGT ( 2 )    = 0.0D0
      CKBSGT ( 3 )    = 1.0D0

      CKN             = 4

      CKBNDS ( 1, 1 ) =  1.0D0
      CKBNDS ( 2, 1 ) =  1.0D0
      CKBNDS ( 3, 1 ) =  1.0D0

      CKBNDS ( 1, 2 ) =  1.0D0
      CKBNDS ( 2, 2 ) = -1.0D0
      CKBNDS ( 3, 2 ) =  1.0D0

      CKBNDS ( 1, 3 ) = -1.0D0
      CKBNDS ( 2, 3 ) = -1.0D0
      CKBNDS ( 3, 3 ) =  1.0D0

      CKBNDS ( 1, 4 ) = -1.0D0
      CKBNDS ( 2, 4 ) =  1.0D0
      CKBNDS ( 3, 4 ) =  1.0D0

!
!     Fetch the FOV definition.
!
      CALL GETFOV ( -33000, MXBNDS, SHAPE, FRAME, BSIGHT, N, BOUNDS )

!
!     Check that no exception was generated.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the parameters returned.
!
      CALL CHCKSC ( 'SHAPE', SHAPE, '=', CKSHAP, OK )
      CALL CHCKSC ( 'FRAME', FRAME, '=', CKFRAM, OK )
      CALL CHCKAD ( 'BORESIGHT', BSIGHT, '~', CKBSGT, 3, 1.0D-15, OK )
      CALL CHCKSI ( 'N', N, '=', CKN, 0, OK )

      IF ( CKN .EQ. N ) THEN

!
!        Loop over all of the boundary vectors returned and
!        check them for any errors.
!
         DO I = 1, CKN

            BNDNAM = 'BOUNDS[#]'
            CALL REPMI  ( BNDNAM, '#', I, BNDNAM )
            CALL CHCKAD ( BNDNAM, &
                          BOUNDS(1,I), &
                          '~', &
                          CKBNDS(1,I), &
                          3, &
                          1.0D-15, &
                          OK           )

         END DO

      END IF

!
! --- Case #32: -----------------------------------------------------
!
      CALL TCASE ( 'Old-Style POLYGON CORNERS-SET Specification' )

!
!     The -33100 ID Code contains the definition of a polygon with
!     the following characteristics:
!
      CKSHAP          = 'POLYGON'
      CKFRAM          = '33100-FRAME'

      CKBSGT ( 1 )    = 0.0D0
      CKBSGT ( 2 )    = 0.0D0
      CKBSGT ( 3 )    = 1.0D0

      CKN             = 4

      CKBNDS ( 1, 1 ) =  1.0D0
      CKBNDS ( 2, 1 ) =  1.0D0
      CKBNDS ( 3, 1 ) =  1.0D0

      CKBNDS ( 1, 2 ) =  1.0D0
      CKBNDS ( 2, 2 ) = -1.0D0
      CKBNDS ( 3, 2 ) =  1.0D0

      CKBNDS ( 1, 3 ) = -1.0D0
      CKBNDS ( 2, 3 ) = -1.0D0
      CKBNDS ( 3, 3 ) =  1.0D0

      CKBNDS ( 1, 4 ) = -1.0D0
      CKBNDS ( 2, 4 ) =  1.0D0
      CKBNDS ( 3, 4 ) =  1.0D0

!
!     Fetch the FOV definition.
!
      CALL GETFOV ( -33100, MXBNDS, SHAPE, FRAME, BSIGHT, N, BOUNDS )

!
!     Check that no exception was generated.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the parameters returned.
!
      CALL CHCKSC ( 'SHAPE', SHAPE, '=', CKSHAP, OK )
      CALL CHCKSC ( 'FRAME', FRAME, '=', CKFRAM, OK )
      CALL CHCKAD ( 'BORESIGHT', BSIGHT, '~', CKBSGT, 3, 1.0D-15, OK )
      CALL CHCKSI ( 'N', N, '=', CKN, 0, OK )

      IF ( CKN .EQ. N ) THEN

!
!        Loop over all of the boundary vectors returned and
!        check them for any errors.
!
         DO I = 1, CKN

            BNDNAM = 'BOUNDS[#]'
            CALL REPMI  ( BNDNAM, '#', I, BNDNAM )
            CALL CHCKAD ( BNDNAM, &
                          BOUNDS(1,I), &
                          '~', &
                          CKBNDS(1,I), &
                          3, &
                          1.0D-15, &
                          OK           )

         END DO

      END IF


!
! --- Case #33: -----------------------------------------------------
!
      CALL TCASE ( 'Conflict POLYGON CORNERS-SET Specification' )

!
!     The -33200 ID Code contains the definition of a polygon with
!     the following characteristics:
!
      CKSHAP          = 'POLYGON'
      CKFRAM          = '33200-FRAME'

      CKBSGT ( 1 )    = 0.0D0
      CKBSGT ( 2 )    = 0.0D0
      CKBSGT ( 3 )    = 1.0D0

      CKN             = 4

      CKBNDS ( 1, 1 ) =  2.0D0
      CKBNDS ( 2, 1 ) =  2.0D0
      CKBNDS ( 3, 1 ) =  1.0D0

      CKBNDS ( 1, 2 ) =  2.0D0
      CKBNDS ( 2, 2 ) = -2.0D0
      CKBNDS ( 3, 2 ) =  1.0D0

      CKBNDS ( 1, 3 ) = -2.0D0
      CKBNDS ( 2, 3 ) = -2.0D0
      CKBNDS ( 3, 3 ) =  1.0D0

      CKBNDS ( 1, 4 ) = -2.0D0
      CKBNDS ( 2, 4 ) =  2.0D0
      CKBNDS ( 3, 4 ) =  1.0D0

!
!     Fetch the FOV definition.
!
      CALL GETFOV ( -33200, MXBNDS, SHAPE, FRAME, BSIGHT, N, BOUNDS )

!
!     Check that no exception was generated.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the parameters returned.
!
      CALL CHCKSC ( 'SHAPE', SHAPE, '=', CKSHAP, OK )
      CALL CHCKSC ( 'FRAME', FRAME, '=', CKFRAM, OK )
      CALL CHCKAD ( 'BORESIGHT', BSIGHT, '~', CKBSGT, 3, 1.0D-15, OK )
      CALL CHCKSI ( 'N', N, '=', CKN, 0, OK )

      IF ( CKN .EQ. N ) THEN

!
!        Loop over all of the boundary vectors returned and
!        check them for any errors.
!
         DO I = 1, CKN

            BNDNAM = 'BOUNDS[#]'
            CALL REPMI  ( BNDNAM, '#', I, BNDNAM )
            CALL CHCKAD ( BNDNAM, &
                          BOUNDS(1,I), &
                          '~', &
                          CKBNDS(1,I), &
                          3, &
                          1.0D-15, &
                          OK           )

         END DO

      END IF

!
! --- Case #34: -----------------------------------------------------
!
      CALL TCASE ( 'Bad Angles Shape Specification' )

!
!     Fetch the FOV definition with ID code -40000 since it contains
!     a the shape 'BOGUS-SHAPE' and the ANGLES class spec.
!
      CALL GETFOV ( -40000, MXBNDS, SHAPE, FRAME, BSIGHT, N, BOUNDS )

!
!     Check for the exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(SHAPENOTSUPPORTED)', OK )

!
! --- Case #35: -----------------------------------------------------
!
      CALL TCASE ( 'Unsupported FOV Class Specification' )

!
!     Fetch the FOV definition with ID code -40100 since it contains
!     the class spec 'BOGUS-SPEC'.
!
      CALL GETFOV ( -40100, MXBNDS, SHAPE, FRAME, BSIGHT, N, BOUNDS )

!
!     Check for the exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(UNSUPPORTEDSPEC)', OK )

!
! --- Case #36: -----------------------------------------------------
!
      CALL TCASE ( 'Missing FOV Reference Vector Exception' )

!
!     Fetch the FOV definition with ID code -41000 since it is
!     missing the FOV_REF_VECTOR keyword.
!
      CALL GETFOV ( -41000, MXBNDS, SHAPE, FRAME, BSIGHT, N, BOUNDS )

!
!     Check for the exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(REFVECTORMISSING)', OK )

!
! --- Case #37: -----------------------------------------------------
!
      CALL TCASE ( 'Improperly Defined Reference Vector' )

!
!     Fetch the FOV definition with ID code -41100 since it contains
!     a FOV_REF_VECTOR keyword with only 2 DP components.
!
      CALL GETFOV ( -41100, MXBNDS, SHAPE, FRAME, BSIGHT, N, BOUNDS )

!
!     Check for the exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(BADREFVECTORSPEC)', OK )

!
!     Fetch the FOV definition with ID code -41200 since it contains
!     a FOV_REF_VECTOR keyword with 3 character components.
!
      CALL GETFOV ( -41200, MXBNDS, SHAPE, FRAME, BSIGHT, N, BOUNDS )

!
!     Check for the exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(BADREFVECTORSPEC)', OK )

!
!     Fetch the FOV definition with ID code -41300 since it contains
!     a FOV_REF_VECTOR that is parallel to the BORESIGHT.
!
      CALL GETFOV ( -41300, MXBNDS, SHAPE, FRAME, BSIGHT, N, BOUNDS )

!
!     Check for the exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(BADREFVECTORSPEC)', OK )

!
! --- Case #38: -----------------------------------------------------
!
      CALL TCASE ( 'Missing Reference Angle Exception' )

!
!     Fetch the FOV definition with ID code -42000 since it is missing
!     the FOV_REF_ANGLE keyword.
!
      CALL GETFOV ( -42000, MXBNDS, SHAPE, FRAME, BSIGHT, N, BOUNDS )

!
!     Check for the exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(REFANGLEMISSING)', OK )

!
! --- Case #39: -----------------------------------------------------
!
      CALL TCASE ( 'Missing Angle Units Exception' )

!
!     Fetch the FOV definition with ID code -43000 since it is missing
!     the FOV_ANGLE_UNITS keyword.
!
      CALL GETFOV ( -43000, MXBNDS, SHAPE, FRAME, BSIGHT, N, BOUNDS )

!
!     Check for the exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(UNITSMISSING)', OK )

!
! --- Case #40: -----------------------------------------------------
!
      CALL TCASE ( 'Ellipse ANGLE missing Cross Angle Exception' )

!
!     Fetch the FOV definition with ID code -44000 since it refers
!     to the shape ELLIPSE which requires FOV_CROSS_ANGLE, which is
!     missing.
!
      CALL GETFOV ( -44000, MXBNDS, SHAPE, FRAME, BSIGHT, N, BOUNDS )

!
!     Check for the exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(CROSSANGLEMISSING)', OK )

!
! --- Case #41: -----------------------------------------------------
!
      CALL TCASE ( 'Rectangle ANGLE missing Cross Angle Exception' )

!
!     Fetch the FOV definition with ID code -45000 since it refers
!     to the shape RECTANGLE which requires FOV_CROSS_ANGLE, which
!     is absent.
!
      CALL GETFOV ( -45000, MXBNDS, SHAPE, FRAME, BSIGHT, N, BOUNDS )

!
!     Check for the exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(CROSSANGLEMISSING)', OK )

!
! --- Case #42: -----------------------------------------------------
!
      CALL TCASE ( 'Rectangle ANGLE Degenerate Boundary Exception' )

!
!     Fetch the FOV definition with ID code -45100 since it contains
!     reference and cross angles near 90.0 degrees.
!
      CALL GETFOV ( -45100, MXBNDS, SHAPE, FRAME, BSIGHT, N, BOUNDS )

!
!     Check for the exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(BADBOUNDARY)', OK )

!
! --- Case #43: -----------------------------------------------------
!
      CALL TCASE ( 'Circle ANGLE Room Failure Boundary Exception' )

!
!     Fetch the FOV definition for -46000.  It is circular, so
!     since we are testing the ROOM exception, report we have
!     only ROOM for 0 boundary vectors.
!
      CALL GETFOV ( -46000, 0, SHAPE, FRAME, BSIGHT, N, BOUNDS )

!
!     Check for the exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(BOUNDARYTOOBIG)', OK )

!
! --- Case #44: -----------------------------------------------------
!
      CALL TCASE ( 'Ellipse ANGLE Room Failure Boundary Exception' )

!
!     Fetch the FOV definition with ID code -46100.  It is elliptical,
!     so since we are testing the ROOM exception, report we have only
!     ROOM for 0 boundary vectors.
!
      CALL GETFOV ( -46100, 0, SHAPE, FRAME, BSIGHT, N, BOUNDS )

!
!     Check for the exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(BOUNDARYTOOBIG)', OK )

!
! --- Case #45: -----------------------------------------------------
!
      CALL TCASE ( 'Rectangle ANGLE Room Failure Boundary Exception' )

!
!     Fetch the FOV definition with ID code -46200. It is rectangular
!     so since we are testing the ROOM exception, report we have only
!     ROOM for 0 boundary vectors.
!
      CALL GETFOV ( -46200, 0, SHAPE, FRAME, BSIGHT, N, BOUNDS )

!
!     Check for the exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(BOUNDARYTOOBIG)', OK )

!
! --- Case #46: -----------------------------------------------------
!
      CALL TCASE ( 'Nominal CIRCLE ANGLES Specification' )

!
!     The -46000 ID Code contains the definition of a circle with
!     the following characteristics:
!
      CKSHAP          = 'CIRCLE'
      CKFRAM          = '46000-FRAME'

      CKBSGT ( 1 )    = 0.0D0
      CKBSGT ( 2 )    = 0.0D0
      CKBSGT ( 3 )    = 1.0D0

      CKN             = 1

      CKBNDS ( 1, 1 ) = 0.0D0
      CKBNDS ( 2, 1 ) = DSQRT(2.0D0)/2.0D0
      CKBNDS ( 3, 1 ) = DSQRT(2.0D0)/2.0D0

!
!     Fetch the FOV definition.
!
      CALL GETFOV ( -46000, MXBNDS, SHAPE, FRAME, BSIGHT, N, BOUNDS )

!
!     Check that no exception was generated.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the parameters returned.
!
      CALL CHCKSC ( 'SHAPE', SHAPE, '=', CKSHAP, OK )
      CALL CHCKSC ( 'FRAME', FRAME, '=', CKFRAM, OK )
      CALL CHCKAD ( 'BORESIGHT', BSIGHT, '~', CKBSGT, 3, 1.0D-15, OK )
      CALL CHCKSI ( 'N', N, '=', CKN, 0, OK )

      IF ( CKN .EQ. N ) THEN

!
!        Loop over all of the boundary vectors returned and
!        check them for any errors.
!
         DO I = 1, CKN

            BNDNAM = 'BOUNDS[#]'
            CALL REPMI  ( BNDNAM, '#', I, BNDNAM )
            CALL CHCKAD ( BNDNAM, &
                          BOUNDS(1,I), &
                          '~', &
                          CKBNDS(1,I), &
                          3, &
                          1.0D-15, &
                          OK           )

         END DO

      END IF

!
! --- Case #47: -----------------------------------------------------
!
      CALL TCASE ( 'Nominal ELLIPSE ANGLES Specification' )

!
!     The -46100 ID Code contains the definition of an ellipse with
!     the following characteristics:
!
      CKSHAP          = 'ELLIPSE'
      CKFRAM          = '46100-FRAME'

      CKBSGT ( 1 )    = 0.0D0
      CKBSGT ( 2 )    = 0.0D0
      CKBSGT ( 3 )    = 1.0D0

      CKN             = 2

      CKBNDS ( 1, 1 ) = 0.0D0
      CKBNDS ( 2, 1 ) = DSQRT(2.0D0)/2.0D0
      CKBNDS ( 3, 1 ) = DSQRT(2.0D0)/2.0D0

      CKBNDS ( 1, 2 ) = -0.5D0
      CKBNDS ( 2, 2 ) = 0.0D0
      CKBNDS ( 3, 2 ) = DSQRT(3.0D0)/2.0D0

!
!     Fetch the FOV definition.
!
      CALL GETFOV ( -46100, MXBNDS, SHAPE, FRAME, BSIGHT, N, BOUNDS )

!
!     Check that no exception was generated.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the parameters returned.
!
      CALL CHCKSC ( 'SHAPE', SHAPE, '=', CKSHAP, OK )
      CALL CHCKSC ( 'FRAME', FRAME, '=', CKFRAM, OK )
      CALL CHCKAD ( 'BORESIGHT', BSIGHT, '~', CKBSGT, 3, 1.0D-15, OK )
      CALL CHCKSI ( 'N', N, '=', CKN, 0, OK )

      IF ( CKN .EQ. N ) THEN

!
!        Loop over all of the boundary vectors returned and
!        check them for any errors.
!
         DO I = 1, CKN

            BNDNAM = 'BOUNDS[#]'
            CALL REPMI  ( BNDNAM, '#', I, BNDNAM )
            CALL CHCKAD ( BNDNAM, &
                          BOUNDS(1,I), &
                          '~', &
                          CKBNDS(1,I), &
                          3, &
                          1.0D-15, &
                          OK           )

         END DO

      END IF

!
! --- Case #16: -----------------------------------------------------
!
      CALL TCASE ( 'Nominal RECTANGLE CORNERS Specification' )

!
!     The -46200 ID Code contains the definition of a rectangle with
!     the following characteristics:
!
      CKSHAP          = 'RECTANGLE'
      CKFRAM          = '46200-FRAME'

      CKBSGT ( 1 )    = 0.0D0
      CKBSGT ( 2 )    = 0.0D0
      CKBSGT ( 3 )    = 1.0D0

      CKN             = 4

      CKBNDS ( 1, 1 ) = -DSQRT(3.0D0)/DSQRT(5.0D0)
      CKBNDS ( 2, 1 ) =  1.0D0/DSQRT(5.0D0)
      CKBNDS ( 3, 1 ) =  1.0D0/DSQRT(5.0D0)

      CKBNDS ( 1, 2 ) = -DSQRT(3.0D0)/DSQRT(5.0D0)
      CKBNDS ( 2, 2 ) = -1.0D0/DSQRT(5.0D0)
      CKBNDS ( 3, 2 ) =  1.0D0/DSQRT(5.0D0)

      CKBNDS ( 1, 3 ) =  DSQRT(3.0D0)/DSQRT(5.0D0)
      CKBNDS ( 2, 3 ) = -1.0D0/DSQRT(5.0D0)
      CKBNDS ( 3, 3 ) =  1.0D0/DSQRT(5.0D0)

      CKBNDS ( 1, 4 ) =  DSQRT(3.0D0)/DSQRT(5.0D0)
      CKBNDS ( 2, 4 ) =  1.0D0/DSQRT(5.0D0)
      CKBNDS ( 3, 4 ) =  1.0D0/DSQRT(5.0D0)

!
!     Fetch the FOV definition.
!
      CALL GETFOV ( -46200, MXBNDS, SHAPE, FRAME, BSIGHT, N, BOUNDS )

!
!     Check that no exception was generated.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the parameters returned.
!
      CALL CHCKSC ( 'SHAPE', SHAPE, '=', CKSHAP, OK )
      CALL CHCKSC ( 'FRAME', FRAME, '=', CKFRAM, OK )
      CALL CHCKAD ( 'BORESIGHT', BSIGHT, '~', CKBSGT, 3, 1.0D-15, OK )
      CALL CHCKSI ( 'N', N, '=', CKN, 0, OK )

      IF ( CKN .EQ. N ) THEN

!
!        Loop over all of the boundary vectors returned and
!        check them for any errors.
!
         DO I = 1, CKN

            BNDNAM = 'BOUNDS[#]'
            CALL REPMI  ( BNDNAM, '#', I, BNDNAM )
            CALL CHCKAD ( BNDNAM, &
                          BOUNDS(1,I), &
                          '~', &
                          CKBNDS(1,I), &
                          3, &
                          1.0D-15, &
                          OK           )

         END DO

      END IF

!
! --- Case #17: -----------------------------------------------------
!
      CALL TCASE ( 'RECTANGLE CORNERS Specification: non-unit REFVEC' )

!
!     The -46201 ID Code contains the definition of a rectangle with
!     the following characteristics:
!
      CKSHAP          = 'RECTANGLE'
      CKFRAM          = '46201-FRAME'

      CKBSGT ( 1 )    = 0.0D0
      CKBSGT ( 2 )    = 0.0D0
      CKBSGT ( 3 )    = 1.0D0

      CKN             = 4

      CKBNDS ( 1, 1 ) = -DSQRT(3.0D0)/DSQRT(5.0D0)
      CKBNDS ( 2, 1 ) =  1.0D0/DSQRT(5.0D0)
      CKBNDS ( 3, 1 ) =  1.0D0/DSQRT(5.0D0)

      CKBNDS ( 1, 2 ) = -DSQRT(3.0D0)/DSQRT(5.0D0)
      CKBNDS ( 2, 2 ) = -1.0D0/DSQRT(5.0D0)
      CKBNDS ( 3, 2 ) =  1.0D0/DSQRT(5.0D0)

      CKBNDS ( 1, 3 ) =  DSQRT(3.0D0)/DSQRT(5.0D0)
      CKBNDS ( 2, 3 ) = -1.0D0/DSQRT(5.0D0)
      CKBNDS ( 3, 3 ) =  1.0D0/DSQRT(5.0D0)

      CKBNDS ( 1, 4 ) =  DSQRT(3.0D0)/DSQRT(5.0D0)
      CKBNDS ( 2, 4 ) =  1.0D0/DSQRT(5.0D0)
      CKBNDS ( 3, 4 ) =  1.0D0/DSQRT(5.0D0)

!
!     Fetch the FOV definition.
!
      CALL GETFOV ( -46201, MXBNDS, SHAPE, FRAME, BSIGHT, N, BOUNDS )

!
!     Check that no exception was generated.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the parameters returned.
!
      CALL CHCKSC ( 'SHAPE', SHAPE, '=', CKSHAP, OK )
      CALL CHCKSC ( 'FRAME', FRAME, '=', CKFRAM, OK )
      CALL CHCKAD ( 'BORESIGHT', BSIGHT, '~', CKBSGT, 3, 1.0D-15, OK )
      CALL CHCKSI ( 'N', N, '=', CKN, 0, OK )

      IF ( CKN .EQ. N ) THEN

!
!        Loop over all of the boundary vectors returned and
!        check them for any errors.
!
         DO I = 1, CKN

            BNDNAM = 'BOUNDS[#]'
            CALL REPMI  ( BNDNAM, '#', I, BNDNAM )
            CALL CHCKAD ( BNDNAM, &
                          BOUNDS(1,I), &
                          '~', &
                          CKBNDS(1,I), &
                          3, &
                          1.0D-15, &
                          OK           )

         END DO

      END IF

!
! --- Case #18: -----------------------------------------------------
!
      CALL TCASE ( 'RECTANGLE CORNERS: non-normal REF and BSIGHT')

!
!     The -46201 ID Code contains the definition of a rectangle with
!     the following characteristics:
!
      CKSHAP          = 'RECTANGLE'
      CKFRAM          = '46202-FRAME'

      CKBSGT ( 1 )    = 0.0D0
      CKBSGT ( 2 )    = 0.0D0
      CKBSGT ( 3 )    = 1.0D0

      CKN             = 4

      CKBNDS ( 1, 1 ) = -DSQRT(3.0D0)/DSQRT(5.0D0)
      CKBNDS ( 2, 1 ) =  1.0D0/DSQRT(5.0D0)
      CKBNDS ( 3, 1 ) =  1.0D0/DSQRT(5.0D0)

      CKBNDS ( 1, 2 ) = -DSQRT(3.0D0)/DSQRT(5.0D0)
      CKBNDS ( 2, 2 ) = -1.0D0/DSQRT(5.0D0)
      CKBNDS ( 3, 2 ) =  1.0D0/DSQRT(5.0D0)

      CKBNDS ( 1, 3 ) =  DSQRT(3.0D0)/DSQRT(5.0D0)
      CKBNDS ( 2, 3 ) = -1.0D0/DSQRT(5.0D0)
      CKBNDS ( 3, 3 ) =  1.0D0/DSQRT(5.0D0)

      CKBNDS ( 1, 4 ) =  DSQRT(3.0D0)/DSQRT(5.0D0)
      CKBNDS ( 2, 4 ) =  1.0D0/DSQRT(5.0D0)
      CKBNDS ( 3, 4 ) =  1.0D0/DSQRT(5.0D0)

!
!     Fetch the FOV definition.
!
      CALL GETFOV ( -46202, MXBNDS, SHAPE, FRAME, BSIGHT, N, BOUNDS )

!
!     Check that no exception was generated.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the parameters returned.
!
      CALL CHCKSC ( 'SHAPE', SHAPE, '=', CKSHAP, OK )
      CALL CHCKSC ( 'FRAME', FRAME, '=', CKFRAM, OK )
      CALL CHCKAD ( 'BORESIGHT', BSIGHT, '~', CKBSGT, 3, 1.0D-15, OK )
      CALL CHCKSI ( 'N', N, '=', CKN, 0, OK )

      IF ( CKN .EQ. N ) THEN

!
!        Loop over all of the boundary vectors returned and
!        check them for any errors.
!
         DO I = 1, CKN

            BNDNAM = 'BOUNDS[#]'
            CALL REPMI  ( BNDNAM, '#', I, BNDNAM )
            CALL CHCKAD ( BNDNAM, &
                          BOUNDS(1,I), &
                          '~', &
                          CKBNDS(1,I), &
                          3, &
                          1.0D-15, &
                          OK           )

         END DO

      END IF

!
!     Clear the kernel pool.
!
      CALL CLPOOL ( )

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN

      END
