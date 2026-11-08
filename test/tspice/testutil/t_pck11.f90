!$Procedure T_PCK11 (Create a test text PCK based on pck00011.tpc )

      SUBROUTINE T_PCK11 ( NAMEPC, LOADPC, KEEPPC )

! $ Abstract
!
!     Create and if appropriate load a test PCK kernel. The kernel
!     created by this routine is meant to contain data consistent with
!     the equations implemented in the test utility routine t_pckeq.
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
!     TESTING
!
! $ Declarations

      IMPLICIT NONE

      CHARACTER*(*)         NAMEPC
      LOGICAL               LOADPC
      LOGICAL               KEEPPC

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NAMEPC     I   The name of the PC-kernel to create
!     LOADPC     I   Load the PC-kernel if .TRUE.
!     KEEPPC     I   Keep the PC-kernel if .TRUE., else delete it.
!
! $ Detailed_Input
!
!     NAMEPC   is the name of a PC-kernel to create and load if
!              LOADPC is set to .TRUE. If a PC-kernel of the same
!              name already exists it is deleted.
!
!
!     LOADPC   is a logical that indicates whether or not the PCK
!              file should be loaded after it is created. If it
!              has the value .TRUE. the PC-kernel is loaded after
!              it is created. Otherwise it is left un-opened.
!
!
!     KEEPPC   is a logical that indicates whether or not the PCK
!              file should be deleted after it is loaded. If KEEPPC
!              is .TRUE. the file is not deleted. If KEEPPC is .FALSE.
!              the file is deleted after it is loaded. NOTE that
!              unless LOADSC is .TRUE., the PCK file is not deleted
!              by this routine. This routine deletes the PC-kernel
!              only if it LOADSC is .TRUE. and KEEPPC is .FALSE.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     None.
!
! $ Files
!
!     This routine creates a text PCK based on the current
!     IAU report [1].
!
! $ Particulars
!
!     This test utility supports validation of the current NAIF
!     text PCK.
!
! $ Examples
!
!     See F_RDPCK.
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     [1] B. A. Archinal et al., "Report of the IAU Working Group on
!         Cartographic Coordinates and Rotational Elements: 2015,"
!         Celest Mech Dyn Astr (2018) 130:22
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    Test Utilities Version 3.0.0, 09-DEC-2021 (NJB) (JDR)
!
!        Updated to reflect the 2015 IAU WGCCRE report.
!
!        Edited the header to comply with NAIF standard.
!
!        Local variable PCK is now saved.
!
!-    Test Utilities Version 2.0.0, 21-OCT-2011 (NJB)
!
!        Updated to reflect changes for Pluto, Charon, and Ida.
!
!-    Test Utilities Version 1.0.0, 29-JAN-2011 (WLT) (NJB)
!
!-&


! $ Index_Entries
!
!     Create current NAIF text PCK file.
!
!-&


!
!     SPICELIB Functions
!
      INTEGER               RTRIM

!
!     Test Utility Functions
!
      CHARACTER*(10)        BEGDAT
      CHARACTER*(10)        BEGTXT

!
!     Local Variables.
!
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               NLINES
      PARAMETER           ( NLINES = 3430 )

      CHARACTER*(LNSIZE)    PCK  ( NLINES )

      INTEGER               I
      INTEGER               R
      INTEGER               S
      INTEGER               UNIT

!
!     Saved variables
!
      SAVE                  PCK


      CALL KILFIL ( NAMEPC )

      CALL CLEARC ( NLINES, PCK )


!
!     Create the PCK kernel.
!
      PCK(  1 ) = 'KPL/PCK'
      PCK(  2 ) = ' '
      PCK(  3 ) = ' '
      PCK(  4 ) = 'Orientation Constants for the Sun and Plan' &
      //          'ets'
      PCK(  5 ) = '------------------------------------------' &
      //          '--------------'
      PCK(  6 ) = ' '
      PCK(  7 ) = ' '
      PCK(  8 ) = 'Sun'
      PCK(  9 ) = ' '
      PCK( 10 ) = '     Old values:'
      PCK( 11 ) = ' '
      PCK( 12 ) = '        Values are unchanged in the 2015 I' &
      //          'AU report.'
      PCK( 13 ) = ' '
      PCK( 14 ) = '     Current values:'
      PCK( 15 ) = ' '
      PCK( 16 ) = BEGDAT()
      PCK( 17 ) = ' '
      PCK( 18 ) = '        BODY10_POLE_RA         = (  286.13' &
      //          '       0.          0. )'
      PCK( 19 ) = '        BODY10_POLE_DEC        = (   63.87' &
      //          '       0.          0. )'
      PCK( 20 ) = '        BODY10_PM              = (   84.17' &
      //          '6     14.18440     0. )'
      PCK( 21 ) = '        BODY10_LONG_AXIS       = (    0.  ' &
      //          '                      )'
      PCK( 22 ) = ' '
      PCK( 23 ) = BEGTXT()
      PCK( 24 ) = ' '
      PCK( 25 ) = 'Mercury'
      PCK( 26 ) = ' '
      PCK( 27 ) = '     Old values:'
      PCK( 28 ) = ' '
      PCK( 29 ) = '        Values are from the 2009 IAU repor' &
      //          't.'
      PCK( 30 ) = ' '
      PCK( 31 ) = ' '
      PCK( 32 ) = '        body199_pole_ra          = (  281.' &
      //          '0097   -0.0328     0. )'
      PCK( 33 ) = '        body199_pole_dec         = (   61.' &
      //          '4143   -0.0049     0. )'
      PCK( 34 ) = '        body199_pm               = (  329.' &
      //          '5469    6.1385025  0. )'
      PCK( 35 ) = ' '
      PCK( 36 ) = '        body199_long_axis        = (    0.' &
      //          '                        )'
      PCK( 37 ) = ' '
      PCK( 38 ) = '        body199_nut_prec_ra  = ( 0. 0. 0. ' &
      //          '0. 0. )'
      PCK( 39 ) = ' '
      PCK( 40 ) = '        body199_nut_prec_dec = ( 0. 0. 0. ' &
      //          '0. 0. )'
      PCK( 41 ) = ' '
      PCK( 42 ) = '        body199_nut_prec_pm  = (    0.0099' &
      //          '3822'
      PCK( 43 ) = '                                   -0.0010' &
      //          '4581'
      PCK( 44 ) = '                                   -0.0001' &
      //          '0280'
      PCK( 45 ) = '                                   -0.0000' &
      //          '2364'
      PCK( 46 ) = '                                   -0.0000' &
      //          '0532  )'
      PCK( 47 ) = ' '
      PCK( 48 ) = ' '
      PCK( 49 ) = '     Current values:'
      PCK( 50 ) = ' '
      PCK( 51 ) = BEGDAT()
      PCK( 52 ) = ' '
      PCK( 53 ) = '        BODY199_POLE_RA          = (  281.' &
      //          '0103   -0.0328     0. )'
      PCK( 54 ) = '        BODY199_POLE_DEC         = (   61.' &
      //          '4155   -0.0049     0. )'
      PCK( 55 ) = '        BODY199_PM               = (  329.' &
      //          '5988    6.1385108  0. )'
      PCK( 56 ) = ' '
      PCK( 57 ) = '        BODY199_LONG_AXIS        = (    0.' &
      //          '                        )'
      PCK( 58 ) = ' '
      PCK( 59 ) = '        BODY199_NUT_PREC_RA  = ( 0. 0. 0. ' &
      //          '0. 0. )'
      PCK( 60 ) = ' '
      PCK( 61 ) = '        BODY199_NUT_PREC_DEC = ( 0. 0. 0. ' &
      //          '0. 0. )'
      PCK( 62 ) = ' '
      PCK( 63 ) = '        BODY199_NUT_PREC_PM  = (    0.0106' &
      //          '7257'
      PCK( 64 ) = '                                   -0.0011' &
      //          '2309'
      PCK( 65 ) = '                                   -0.0001' &
      //          '1040'
      PCK( 66 ) = '                                   -0.0000' &
      //          '2539'
      PCK( 67 ) = '                                   -0.0000' &
      //          '0571  )'
      PCK( 68 ) = BEGTXT()
      PCK( 69 ) = ' '
      PCK( 70 ) = '           The linear coefficients have be' &
      //          'en scaled up from degrees/day'
      PCK( 71 ) = '           to degrees/century, because the' &
      //          ' SPICELIB PCK reader expects'
      PCK( 72 ) = '           these units.  The original cons' &
      //          'tants were:'
      PCK( 73 ) = ' '
      PCK( 74 ) = '                                    174.79' &
      //          '10857      4.092335'
      PCK( 75 ) = '                                    349.58' &
      //          '21714      8.184670'
      PCK( 76 ) = '                                    164.37' &
      //          '32571     12.277005'
      PCK( 77 ) = '                                    339.16' &
      //          '43429     16.369340'
      PCK( 78 ) = '                                    153.95' &
      //          '54286     20.461675'
      PCK( 79 ) = ' '
      PCK( 80 ) = ' '
      PCK( 81 ) = BEGDAT()
      PCK( 82 ) = ' '
      PCK( 83 ) = '        BODY1_NUT_PREC_ANGLES  = ( 174.791' &
      //          '0857  0.14947253587500003E+06'
      PCK( 84 ) = '                                   349.582' &
      //          '1714  0.29894507175000006E+06'
      PCK( 85 ) = '                                   164.373' &
      //          '2571  0.44841760762500006E+06'
      PCK( 86 ) = '                                   339.164' &
      //          '3429  0.59789014350000012E+06'
      PCK( 87 ) = '                                   153.955' &
      //          '4286  0.74736267937499995E+06 )'
      PCK( 88 ) = ' '
      PCK( 89 ) = ' '
      PCK( 90 ) = ' '
      PCK( 91 ) = ' '
      PCK( 92 ) = BEGTXT()
      PCK( 93 ) = ' '
      PCK( 94 ) = ' '
      PCK( 95 ) = 'Venus'
      PCK( 96 ) = ' '
      PCK( 97 ) = '     Old values:'
      PCK( 98 ) = ' '
      PCK( 99 ) = '        Values are unchanged in the 2015 I' &
      //          'AU report.'
      PCK( 100 ) = ' '
      PCK( 101 ) = '     Current values:'
      PCK( 102 ) = ' '
      PCK( 103 ) = BEGDAT()
      PCK( 104 ) = ' '
      PCK( 105 ) = '        BODY299_POLE_RA          = (  272' &
      //           '.76       0.          0. )'
      PCK( 106 ) = '        BODY299_POLE_DEC         = (   67' &
      //           '.16       0.          0. )'
      PCK( 107 ) = '        BODY299_PM               = (  160' &
      //           '.20      -1.4813688   0. )'
      PCK( 108 ) = ' '
      PCK( 109 ) = '        BODY299_LONG_AXIS        = (    0' &
      //           '.                        )'
      PCK( 110 ) = ' '
      PCK( 111 ) = BEGTXT()
      PCK( 112 ) = ' '
      PCK( 113 ) = ' '
      PCK( 114 ) = 'Earth'
      PCK( 115 ) = ' '
      PCK( 116 ) = '     Old values:'
      PCK( 117 ) = ' '
      PCK( 118 ) = '        The values shown below are those ' &
      //           'from the 2009 report. The 2015'
      PCK( 119 ) = '        report does not provide orientati' &
      //           'on data for the Earth or Moon.'
      PCK( 120 ) = ' '
      PCK( 121 ) = BEGDAT()
      PCK( 122 ) = ' '
      PCK( 123 ) = '        BODY399_POLE_RA        = (    0. ' &
      //           '     -0.641         0. )'
      PCK( 124 ) = '        BODY399_POLE_DEC       = (   90. ' &
      //           '     -0.557         0. )'
      PCK( 125 ) = '        BODY399_PM             = (  190.1' &
      //           '47  360.9856235     0. )'
      PCK( 126 ) = '        BODY399_LONG_AXIS      = (    0. ' &
      //           '                       )'
      PCK( 127 ) = ' '
      PCK( 128 ) = BEGTXT()
      PCK( 129 ) = ' '
      PCK( 130 ) = ' '
      PCK( 131 ) = '        Nutation precession angles for th' &
      //           'e Earth-Moon system:'
      PCK( 132 ) = ' '
      PCK( 133 ) = '           The linear coefficients have b' &
      //           'een scaled up from degrees/day'
      PCK( 134 ) = '           to degrees/century, because th' &
      //           'e SPICELIB PCK reader expects'
      PCK( 135 ) = '           these units.  The original con' &
      //           'stants were:'
      PCK( 136 ) = ' '
      PCK( 137 ) = '                                    125.0' &
      //           '45D0   -0.0529921D0'
      PCK( 138 ) = '                                    250.0' &
      //           '89D0   -0.1059842D0'
      PCK( 139 ) = '                                    260.0' &
      //           '08D0   13.0120009D0'
      PCK( 140 ) = '                                    176.6' &
      //           '25D0   13.3407154D0'
      PCK( 141 ) = '                                    357.5' &
      //           '29D0    0.9856003D0'
      PCK( 142 ) = '                                    311.5' &
      //           '89D0   26.4057084D0'
      PCK( 143 ) = '                                    134.9' &
      //           '63D0   13.0649930D0'
      PCK( 144 ) = '                                    276.6' &
      //           '17D0    0.3287146D0'
      PCK( 145 ) = '                                     34.2' &
      //           '26D0    1.7484877D0'
      PCK( 146 ) = '                                     15.1' &
      //           '34D0   -0.1589763D0'
      PCK( 147 ) = '                                    119.7' &
      //           '43D0    0.0036096D0'
      PCK( 148 ) = '                                    239.9' &
      //           '61D0    0.1643573D0'
      PCK( 149 ) = '                                     25.0' &
      //           '53D0   12.9590088D0'
      PCK( 150 ) = ' '
      PCK( 151 ) = ' '
      PCK( 152 ) = BEGDAT()
      PCK( 153 ) = ' '
      PCK( 154 ) = ' '
      PCK( 155 ) = '        BODY3_NUT_PREC_ANGLES  = (  125.0' &
      //           '45         -1935.5364525000'
      PCK( 156 ) = '                                    250.0' &
      //           '89         -3871.0729050000'
      PCK( 157 ) = '                                    260.0' &
      //           '08        475263.3328725000'
      PCK( 158 ) = '                                    176.6' &
      //           '25        487269.6299850000'
      PCK( 159 ) = '                                    357.5' &
      //           '29         35999.0509575000'
      PCK( 160 ) = '                                    311.5' &
      //           '89        964468.4993100000'
      PCK( 161 ) = '                                    134.9' &
      //           '63        477198.8693250000'
      PCK( 162 ) = '                                    276.6' &
      //           '17         12006.3007650000'
      PCK( 163 ) = '                                     34.2' &
      //           '26         63863.5132425000'
      PCK( 164 ) = '                                     15.1' &
      //           '34         -5806.6093575000'
      PCK( 165 ) = '                                    119.7' &
      //           '43           131.8406400000'
      PCK( 166 ) = '                                    239.9' &
      //           '61          6003.1503825000'
      PCK( 167 ) = '                                     25.0' &
      //           '53        473327.7964200000 )'
      PCK( 168 ) = ' '
      PCK( 169 ) = ' '
      PCK( 170 ) = BEGTXT()
      PCK( 171 ) = ' '
      PCK( 172 ) = ' '
      PCK( 173 ) = '     Earth north geomagnetic centered dip' &
      //           'ole:'
      PCK( 174 ) = ' '
      PCK( 175 ) = '           The north dipole location is t' &
      //           'ime-varying.  The values shown'
      PCK( 176 ) = '           below, taken from [8], represe' &
      //           'nt a discrete sampling of the'
      PCK( 177 ) = '           north dipole location from 194' &
      //           '5 to 2000. The terms DGRF and'
      PCK( 178 ) = '           IGRF refer to, respectively, "' &
      //           'Definitive Geomagnetic'
      PCK( 179 ) = '           Reference Field" and "Internat' &
      //           'ional Geomagnetic Reference'
      PCK( 180 ) = '           Field."  See references [6], [' &
      //           '8], and [9] for details.'
      PCK( 181 ) = ' '
      PCK( 182 ) = '           Coordinates are planetocentric' &
      //           '.'
      PCK( 183 ) = ' '
      PCK( 184 ) = '             Data source    Lat      Lon'
      PCK( 185 ) = '             -----------   -----    -----' &
      //           '-'
      PCK( 186 ) = '              DGRF 1945    78.47    291.4' &
      //           '7'
      PCK( 187 ) = '              DGRF 1950    78.47    291.1' &
      //           '5'
      PCK( 188 ) = '              DGRF 1955    78.46    290.8' &
      //           '4'
      PCK( 189 ) = '              DGRF 1960    78.51    290.5' &
      //           '3'
      PCK( 190 ) = '              DGRF 1965    78.53    290.1' &
      //           '5'
      PCK( 191 ) = '              DGRF 1970    78.59    289.8' &
      //           '2'
      PCK( 192 ) = '              DGRF 1975    78.69    289.5' &
      //           '3'
      PCK( 193 ) = '              DGRF 1980    78.81    289.2' &
      //           '4'
      PCK( 194 ) = '              DGRF 1985    78.97    289.1' &
      //           '0'
      PCK( 195 ) = '              DGRF 1990    79.13    288.8' &
      //           '9'
      PCK( 196 ) = '              IGRF 1995    79.30    288.5' &
      //           '9'
      PCK( 197 ) = '              IGRF 2000    79.54    288.4' &
      //           '3'
      PCK( 198 ) = ' '
      PCK( 199 ) = '        Original values:'
      PCK( 200 ) = ' '
      PCK( 201 ) = '           Values are from [7].  Note the' &
      //           ' year of publication was 1971.'
      PCK( 202 ) = ' '
      PCK( 203 ) = '           body399_mag_north_pole_lon  = ' &
      //           ' ( -69.761 )'
      PCK( 204 ) = '           body399_mag_north_pole_lat  = ' &
      //           ' (  78.565 )'
      PCK( 205 ) = ' '
      PCK( 206 ) = '        Previous values:'
      PCK( 207 ) = ' '
      PCK( 208 ) = '           body399_n_geomag_ctr_dipole_lo' &
      //           'n  =  ( 288.43 )'
      PCK( 209 ) = '           body399_n_geomag_ctr_dipole_la' &
      //           't  =  (  79.54 )'
      PCK( 210 ) = ' '
      PCK( 211 ) = ' '
      PCK( 212 ) = '        Current values:'
      PCK( 213 ) = ' '
      PCK( 214 ) = '           Values are given for the epoch' &
      //           ' 2012.0 and were derived'
      PCK( 215 ) = '           by Nat Bachman from constants ' &
      //           'provided by [11].'
      PCK( 216 ) = ' '
      PCK( 217 ) = BEGDAT()
      PCK( 218 ) = ' '
      PCK( 219 ) = '        BODY399_N_GEOMAG_CTR_DIPOLE_LON  ' &
      //           '=  ( 287.62 )'
      PCK( 220 ) = '        BODY399_N_GEOMAG_CTR_DIPOLE_LAT  ' &
      //           '=  (  80.13 )'
      PCK( 221 ) = ' '
      PCK( 222 ) = BEGTXT()
      PCK( 223 ) = ' '
      PCK( 224 ) = ' '
      PCK( 225 ) = ' '
      PCK( 226 ) = ' '
      PCK( 227 ) = 'Mars'
      PCK( 228 ) = ' '
      PCK( 229 ) = '     Old values:'
      PCK( 230 ) = ' '
      PCK( 231 ) = '        Values are from the 2009 IAU repo' &
      //           'rt.'
      PCK( 232 ) = ' '
      PCK( 233 ) = '        body499_pole_ra          = (  317' &
      //           '.68143   -0.1061      0.  )'
      PCK( 234 ) = '        body499_pole_dec         = (   52' &
      //           '.88650   -0.0609      0.  )'
      PCK( 235 ) = '        body499_pm               = (  176' &
      //           '.630    350.89198226  0.  )'
      PCK( 236 ) = ' '
      PCK( 237 ) = '        Source [5] specifies the followin' &
      //           'g value for the lambda_a term'
      PCK( 238 ) = '        (BODY499_LONG_AXIS ) for Mars. Th' &
      //           'is term is the POSITIVE EAST'
      PCK( 239 ) = '        LONGITUDE, measured from the prim' &
      //           'e meridian, of the meridian'
      PCK( 240 ) = '        containing the longest axis of th' &
      //           'e reference ellipsoid.'
      PCK( 241 ) = '        (CAUTION: previous values were PO' &
      //           'SITIVE WEST.)'
      PCK( 242 ) = ' '
      PCK( 243 ) = '           body499_long_axis        = (  ' &
      //           '252.  )'
      PCK( 244 ) = ' '
      PCK( 245 ) = '        We list this lambda_a value for c' &
      //           'ompleteness. The IAU report'
      PCK( 246 ) = '        [1] gives equal values for both e' &
      //           'quatorial radii, so the'
      PCK( 247 ) = '        lambda_a offset does not apply to' &
      //           ' the IAU model.'
      PCK( 248 ) = ' '
      PCK( 249 ) = '        The 2003 IAU report defines M2, t' &
      //           'he second nutation precession angle,'
      PCK( 250 ) = '        by:'
      PCK( 251 ) = ' '
      PCK( 252 ) = '                                         ' &
      //           '       2'
      PCK( 253 ) = '           192.93  +  1128.4096700 d  +  ' &
      //           '8.864 T'
      PCK( 254 ) = ' '
      PCK( 255 ) = '        We truncate the M2 series to a li' &
      //           'near expression, because the PCK'
      PCK( 256 ) = '        software cannot handle the quadra' &
      //           'tic term.'
      PCK( 257 ) = ' '
      PCK( 258 ) = '        Again, the linear terms are scale' &
      //           'd by 36525.0:'
      PCK( 259 ) = ' '
      PCK( 260 ) = '            -0.4357640000000000       -->' &
      //           '     -15916.28010000000'
      PCK( 261 ) = '          1128.409670000000           -->' &
      //           '   41215163.19675000'
      PCK( 262 ) = '            -1.8151000000000000E-02   -->' &
      //           '       -662.9652750000000'
      PCK( 263 ) = ' '
      PCK( 264 ) = '        We also introduce a fourth nutati' &
      //           'on precession angle, which'
      PCK( 265 ) = '        is the pi/2-complement of the thi' &
      //           'rd angle.  This angle is used'
      PCK( 266 ) = '        in computing the prime meridian l' &
      //           'ocation for Deimos.  See the'
      PCK( 267 ) = '        discussion of this angle below in' &
      //           ' the section containing orientation'
      PCK( 268 ) = '        constants for Deimos.'
      PCK( 269 ) = ' '
      PCK( 270 ) = '        body4_nut_prec_angles  = (  169.5' &
      //           '1     -15916.2801'
      PCK( 271 ) = '                                    192.9' &
      //           '3   41215163.19675'
      PCK( 272 ) = '                                     53.4' &
      //           '7       -662.965275'
      PCK( 273 ) = '                                     36.5' &
      //           '3        662.965275  )'
      PCK( 274 ) = ' '
      PCK( 275 ) = ' '
      PCK( 276 ) = '     Current values:'
      PCK( 277 ) = ' '
      PCK( 278 ) = BEGDAT()
      PCK( 279 ) = ' '
      PCK( 280 ) = '        BODY499_POLE_RA          = (  317' &
      //           '.269202  -0.10927547        0.  )'
      PCK( 281 ) = '        BODY499_POLE_DEC         = (   54' &
      //           '.432516  -0.05827105        0.  )'
      PCK( 282 ) = '        BODY499_PM               = (  176' &
      //           '.049863  +350.891982443297  0.  )'
      PCK( 283 ) = ' '
      PCK( 284 ) = '        BODY499_NUT_PREC_RA      = (  0  ' &
      //           '   0     0     0     0'
      PCK( 285 ) = '                                      0  ' &
      //           '   0     0     0     0'
      PCK( 286 ) = '                                      0  ' &
      //           '   0     0     0     0'
      PCK( 287 ) = '                                      0.0' &
      //           '00068'
      PCK( 288 ) = '                                      0.0' &
      //           '00238'
      PCK( 289 ) = '                                      0.0' &
      //           '00052'
      PCK( 290 ) = '                                      0.0' &
      //           '00009'
      PCK( 291 ) = '                                      0.4' &
      //           '19057                  )'
      PCK( 292 ) = ' '
      PCK( 293 ) = ' '
      PCK( 294 ) = '        BODY499_NUT_PREC_DEC     = (  0  ' &
      //           '   0     0     0     0'
      PCK( 295 ) = '                                      0  ' &
      //           '   0     0     0     0'
      PCK( 296 ) = '                                      0  ' &
      //           '   0     0     0     0'
      PCK( 297 ) = '                                      0  ' &
      //           '   0     0     0     0'
      PCK( 298 ) = '                                      0.0' &
      //           '00051'
      PCK( 299 ) = '                                      0.0' &
      //           '00141'
      PCK( 300 ) = '                                      0.0' &
      //           '00031'
      PCK( 301 ) = '                                      0.0' &
      //           '00005'
      PCK( 302 ) = '                                      1.5' &
      //           '91274                  )'
      PCK( 303 ) = ' '
      PCK( 304 ) = ' '
      PCK( 305 ) = '        BODY499_NUT_PREC_PM      = (  0  ' &
      //           '   0     0     0     0'
      PCK( 306 ) = '                                      0  ' &
      //           '   0     0     0     0'
      PCK( 307 ) = '                                      0  ' &
      //           '   0     0     0     0'
      PCK( 308 ) = '                                      0  ' &
      //           '   0     0     0     0'
      PCK( 309 ) = '                                      0  ' &
      //           '   0     0     0     0'
      PCK( 310 ) = '                                      0.0' &
      //           '00145'
      PCK( 311 ) = '                                      0.0' &
      //           '00157'
      PCK( 312 ) = '                                      0.0' &
      //           '00040'
      PCK( 313 ) = '                                      0.0' &
      //           '00001'
      PCK( 314 ) = '                                      0.0' &
      //           '00001'
      PCK( 315 ) = '                                      0.5' &
      //           '84542                  )'
      PCK( 316 ) = ' '
      PCK( 317 ) = ' '
      PCK( 318 ) = BEGTXT()
      PCK( 319 ) = ' '
      PCK( 320 ) = '            Missing from M5: 12.71192322 ' &
      //           'T**2'
      PCK( 321 ) = '            Missing from M6: 25.42412173 ' &
      //           'T**2'
      PCK( 322 ) = '            Missing from M7: 38.13293168 ' &
      //           'T**2'
      PCK( 323 ) = ' '
      PCK( 324 ) = BEGDAT()
      PCK( 325 ) = ' '
      PCK( 326 ) = ' '
      PCK( 327 ) = '        BODY4_NUT_PREC_ANGLES  = ( 190.72' &
      //           '646643      15917.10818695'
      PCK( 328 ) = '                                    21.46' &
      //           '892470      31834.27934054'
      PCK( 329 ) = '                                   332.86' &
      //           '082793      19139.89694742'
      PCK( 330 ) = '                                   394.93' &
      //           '256437      38280.79631835'
      PCK( 331 ) = '                                   189.63' &
      //           '271560   41215158.18420050'
      PCK( 332 ) = ' '
      PCK( 333 ) = '                                    19.26' &
      //           '538605   82430316.36864280'
      PCK( 334 ) = '                                   208.89' &
      //           '882434  123645474.54466790'
      PCK( 335 ) = '                                   121.46' &
      //           '893664        660.22803474'
      PCK( 336 ) = '                                   231.05' &
      //           '028581        660.99123540'
      PCK( 337 ) = '                                   251.37' &
      //           '314025       1320.50145245'
      PCK( 338 ) = ' '
      PCK( 339 ) = '                                   217.98' &
      //           '635955      38279.96125550'
      PCK( 340 ) = '                                   196.19' &
      //           '729402      19139.83628608'
      PCK( 341 ) = '                                    10.80' &
      //           '071482   10414879.22849759'
      PCK( 342 ) = '                                   345.99' &
      //           '306351    4801583.39793913'
      PCK( 343 ) = '                                   303.28' &
      //           '024985   10415546.40050500'
      PCK( 344 ) = ' '
      PCK( 345 ) = '                                   198.99' &
      //           '1226        19139.4819985'
      PCK( 346 ) = '                                   226.29' &
      //           '2679        38280.8511281'
      PCK( 347 ) = '                                   249.66' &
      //           '3391        57420.7251593'
      PCK( 348 ) = '                                   266.18' &
      //           '3510        76560.6367950'
      PCK( 349 ) = '                                    79.39' &
      //           '8797            0.5042615'
      PCK( 350 ) = ' '
      PCK( 351 ) = '                                   122.43' &
      //           '3576        19139.9407476'
      PCK( 352 ) = '                                    43.05' &
      //           '8401        38280.8753272'
      PCK( 353 ) = '                                    57.66' &
      //           '3379        57420.7517205'
      PCK( 354 ) = '                                    79.47' &
      //           '6401        76560.6495004'
      PCK( 355 ) = '                                   166.32' &
      //           '5722            0.5042615'
      PCK( 356 ) = ' '
      PCK( 357 ) = '                                   129.07' &
      //           '1773        19140.0328244'
      PCK( 358 ) = '                                    36.35' &
      //           '2167        38281.0473591'
      PCK( 359 ) = '                                    56.66' &
      //           '8646        57420.9295360'
      PCK( 360 ) = '                                    67.36' &
      //           '4003        76560.2552215'
      PCK( 361 ) = '                                   104.79' &
      //           '2680        95700.4387578'
      PCK( 362 ) = '                                    95.39' &
      //           '1654            0.5042615  )'
      PCK( 363 ) = ' '
      PCK( 364 ) = BEGTXT()
      PCK( 365 ) = ' '
      PCK( 366 ) = ' '
      PCK( 367 ) = 'Jupiter'
      PCK( 368 ) = ' '
      PCK( 369 ) = '     Old values:'
      PCK( 370 ) = ' '
      PCK( 371 ) = '        The rotation rate is from the 200' &
      //           '6 IAU report; all other'
      PCK( 372 ) = '        values are unchanged in the 2009 ' &
      //           'report.'
      PCK( 373 ) = ' '
      PCK( 374 ) = '           body599_pm             = (   2' &
      //           '84.95        870.5366420      0. )'
      PCK( 375 ) = ' '
      PCK( 376 ) = ' '
      PCK( 377 ) = '     Current values:'
      PCK( 378 ) = ' '
      PCK( 379 ) = '        The number of nutation precession' &
      //           ' angles is 15. The ninth and'
      PCK( 380 ) = '        tenth are twice the first and sec' &
      //           'ond, respectively. The'
      PCK( 381 ) = '        eleventh through fifteenth corres' &
      //           'pond to angles JA-JE in'
      PCK( 382 ) = '        the 2006 IAU report; angles JA-JE' &
      //           ' were not used prior to that'
      PCK( 383 ) = '        report.'
      PCK( 384 ) = ' '
      PCK( 385 ) = BEGDAT()
      PCK( 386 ) = ' '
      PCK( 387 ) = ' '
      PCK( 388 ) = '        BODY599_POLE_RA        = (   268.' &
      //           '056595     -0.006499       0. )'
      PCK( 389 ) = '        BODY599_POLE_DEC       = (    64.' &
      //           '495303      0.002413       0. )'
      PCK( 390 ) = '        BODY599_PM             = (   284.' &
      //           '95        870.5360000      0. )'
      PCK( 391 ) = '        BODY599_LONG_AXIS      = (     0.' &
      //           '                        )'
      PCK( 392 ) = ' '
      PCK( 393 ) = '        BODY599_NUT_PREC_RA  = ( 0. 0. 0.' &
      //           ' 0. 0. 0. 0. 0. 0. 0.  0.000117'
      PCK( 394 ) = '                                         ' &
      //           '                       0.000938'
      PCK( 395 ) = '                                         ' &
      //           '                       0.001432'
      PCK( 396 ) = '                                         ' &
      //           '                       0.000030'
      PCK( 397 ) = '                                         ' &
      //           '                       0.002150 )'
      PCK( 398 ) = ' '
      PCK( 399 ) = '        BODY599_NUT_PREC_DEC = ( 0. 0. 0.' &
      //           ' 0. 0. 0. 0. 0. 0. 0.  0.000050'
      PCK( 400 ) = '                                         ' &
      //           '                       0.000404'
      PCK( 401 ) = '                                         ' &
      //           '                       0.000617'
      PCK( 402 ) = '                                         ' &
      //           '                      -0.000013'
      PCK( 403 ) = '                                         ' &
      //           '                       0.000926 )'
      PCK( 404 ) = ' '
      PCK( 405 ) = '        BODY599_NUT_PREC_PM  = ( 0. 0. 0.' &
      //           ' 0. 0. 0. 0. 0. 0. 0.  0.0'
      PCK( 406 ) = '                                         ' &
      //           '                       0.0'
      PCK( 407 ) = '                                         ' &
      //           '                       0.0'
      PCK( 408 ) = '                                         ' &
      //           '                       0.0'
      PCK( 409 ) = '                                         ' &
      //           '                       0.0  )'
      PCK( 410 ) = ' '
      PCK( 411 ) = ' '
      PCK( 412 ) = '        BODY5_NUT_PREC_ANGLES  = (    73.' &
      //           '32      91472.9'
      PCK( 413 ) = '                                      24.' &
      //           '62      45137.2'
      PCK( 414 ) = '                                     283.' &
      //           '90       4850.7'
      PCK( 415 ) = '                                     355.' &
      //           '80       1191.3'
      PCK( 416 ) = '                                     119.' &
      //           '90        262.1'
      PCK( 417 ) = '                                     229.' &
      //           '80         64.3'
      PCK( 418 ) = '                                     352.' &
      //           '25       2382.6'
      PCK( 419 ) = '                                     113.' &
      //           '35       6070.0'
      PCK( 420 ) = '                                     146.' &
      //           '64     182945.8'
      PCK( 421 ) = '                                      49.' &
      //           '24      90274.4'
      PCK( 422 ) = '                                      99.' &
      //           '360714   4850.4046'
      PCK( 423 ) = '                                     175.' &
      //           '895369   1191.9605'
      PCK( 424 ) = '                                     300.' &
      //           '323162    262.5475'
      PCK( 425 ) = '                                     114.' &
      //           '012305   6070.2476'
      PCK( 426 ) = '                                      49.' &
      //           '511251     64.3000  )'
      PCK( 427 ) = BEGTXT()
      PCK( 428 ) = ' '
      PCK( 429 ) = ' '
      PCK( 430 ) = 'Saturn'
      PCK( 431 ) = ' '
      PCK( 432 ) = '     Old values:'
      PCK( 433 ) = ' '
      PCK( 434 ) = '        Values are from the 2006 IAU repo' &
      //           'rt.'
      PCK( 435 ) = ' '
      PCK( 436 ) = ' '
      PCK( 437 ) = '        body699_pole_ra        = (    40.' &
      //           '589    -0.036      0.  )'
      PCK( 438 ) = '        body699_pole_dec       = (    83.' &
      //           '537    -0.004      0.  )'
      PCK( 439 ) = '        body699_pm             = (    38.' &
      //           '90    810.7939024  0.  )'
      PCK( 440 ) = '        body699_long_axis      = (     0.' &
      //           '                       )'
      PCK( 441 ) = ' '
      PCK( 442 ) = ' '
      PCK( 443 ) = '        The first seven angles given here' &
      //           ' are the angles S1'
      PCK( 444 ) = '        through S7 from the 2000 report; ' &
      //           'the eighth and'
      PCK( 445 ) = '        ninth angles are 2*S1 and 2*S2, r' &
      //           'espectively.'
      PCK( 446 ) = ' '
      PCK( 447 ) = ' '
      PCK( 448 ) = '        body6_nut_prec_angles  = (  353.3' &
      //           '2   75706.7'
      PCK( 449 ) = '                                     28.7' &
      //           '2   75706.7'
      PCK( 450 ) = '                                    177.4' &
      //           '0  -36505.5'
      PCK( 451 ) = '                                    300.0' &
      //           '0   -7225.9'
      PCK( 452 ) = '                                    316.4' &
      //           '5     506.2'
      PCK( 453 ) = '                                    345.2' &
      //           '0   -1016.3'
      PCK( 454 ) = '                                     29.8' &
      //           '0     -52.1'
      PCK( 455 ) = '                                    706.6' &
      //           '4  151413.4'
      PCK( 456 ) = '                                     57.4' &
      //           '4  151413.4  )'
      PCK( 457 ) = ' '
      PCK( 458 ) = ' '
      PCK( 459 ) = '     Current values:'
      PCK( 460 ) = ' '
      PCK( 461 ) = ' '
      PCK( 462 ) = '        The change from the previous set ' &
      //           'of values is the'
      PCK( 463 ) = '        removal of S7. This causes BODY6_' &
      //           'NUT_PREC_ANGLES'
      PCK( 464 ) = '        elements that formerly correspond' &
      //           'ed to 2*S1 and 2*S1'
      PCK( 465 ) = '        to be shifted toward the start of' &
      //           ' the array.'
      PCK( 466 ) = ' '
      PCK( 467 ) = BEGDAT()
      PCK( 468 ) = ' '
      PCK( 469 ) = '        BODY699_POLE_RA        = (    40.' &
      //           '589    -0.036      0.  )'
      PCK( 470 ) = '        BODY699_POLE_DEC       = (    83.' &
      //           '537    -0.004      0.  )'
      PCK( 471 ) = '        BODY699_PM             = (    38.' &
      //           '90    810.7939024  0.  )'
      PCK( 472 ) = '        BODY699_LONG_AXIS      = (     0.' &
      //           '                       )'
      PCK( 473 ) = ' '
      PCK( 474 ) = BEGTXT()
      PCK( 475 ) = ' '
      PCK( 476 ) = '        The first six angles given here a' &
      //           're the angles S1'
      PCK( 477 ) = '        through S6 from the 2009 report; ' &
      //           'the seventh and'
      PCK( 478 ) = '        eigth angles are 2*S1 and 2*S2, r' &
      //           'espectively.'
      PCK( 479 ) = ' '
      PCK( 480 ) = ' '
      PCK( 481 ) = BEGDAT()
      PCK( 482 ) = ' '
      PCK( 483 ) = '        BODY6_NUT_PREC_ANGLES  = (  353.3' &
      //           '2   75706.7'
      PCK( 484 ) = '                                     28.7' &
      //           '2   75706.7'
      PCK( 485 ) = '                                    177.4' &
      //           '0  -36505.5'
      PCK( 486 ) = '                                    300.0' &
      //           '0   -7225.9'
      PCK( 487 ) = '                                    316.4' &
      //           '5     506.2'
      PCK( 488 ) = '                                    345.2' &
      //           '0   -1016.3'
      PCK( 489 ) = '                                    706.6' &
      //           '4  151413.4'
      PCK( 490 ) = '                                     57.4' &
      //           '4  151413.4  )'
      PCK( 491 ) = BEGTXT()
      PCK( 492 ) = ' '
      PCK( 493 ) = ' '
      PCK( 494 ) = 'Uranus'
      PCK( 495 ) = ' '
      PCK( 496 ) = '     Old values:'
      PCK( 497 ) = ' '
      PCK( 498 ) = '        Values are unchanged in the 2009 ' &
      //           'IAU report.'
      PCK( 499 ) = ' '
      PCK( 500 ) = '     Current values:'
      PCK( 501 ) = ' '
      PCK( 502 ) = BEGDAT()
      PCK( 503 ) = ' '
      PCK( 504 ) = '        BODY799_POLE_RA        = (  257.3' &
      //           '11     0.         0.  )'
      PCK( 505 ) = '        BODY799_POLE_DEC       = (  -15.1' &
      //           '75     0.         0.  )'
      PCK( 506 ) = '        BODY799_PM             = (  203.8' &
      //           '1   -501.1600928  0.  )'
      PCK( 507 ) = '        BODY799_LONG_AXIS      = (    0. ' &
      //           '                      )'
      PCK( 508 ) = ' '
      PCK( 509 ) = BEGTXT()
      PCK( 510 ) = ' '
      PCK( 511 ) = '        The first 16 angles given here ar' &
      //           'e the angles U1'
      PCK( 512 ) = '        through U16 from the 2000 report;' &
      //           ' the 17th and'
      PCK( 513 ) = '        18th angles are 2*U11 and 2*U12, ' &
      //           'respectively.'
      PCK( 514 ) = ' '
      PCK( 515 ) = BEGDAT()
      PCK( 516 ) = ' '
      PCK( 517 ) = '        BODY7_NUT_PREC_ANGLES  = (  115.7' &
      //           '5   54991.87'
      PCK( 518 ) = '                                    141.6' &
      //           '9   41887.66'
      PCK( 519 ) = '                                    135.0' &
      //           '3   29927.35'
      PCK( 520 ) = '                                     61.7' &
      //           '7   25733.59'
      PCK( 521 ) = '                                    249.3' &
      //           '2   24471.46'
      PCK( 522 ) = '                                     43.8' &
      //           '6   22278.41'
      PCK( 523 ) = '                                     77.6' &
      //           '6   20289.42'
      PCK( 524 ) = '                                    157.3' &
      //           '6   16652.76'
      PCK( 525 ) = '                                    101.8' &
      //           '1   12872.63'
      PCK( 526 ) = '                                    138.6' &
      //           '4    8061.81'
      PCK( 527 ) = '                                    102.2' &
      //           '3   -2024.22'
      PCK( 528 ) = '                                    316.4' &
      //           '1    2863.96'
      PCK( 529 ) = '                                    304.0' &
      //           '1     -51.94'
      PCK( 530 ) = '                                    308.7' &
      //           '1     -93.17'
      PCK( 531 ) = '                                    340.8' &
      //           '2     -75.32'
      PCK( 532 ) = '                                    259.1' &
      //           '4    -504.81'
      PCK( 533 ) = '                                    204.4' &
      //           '6   -4048.44'
      PCK( 534 ) = '                                    632.8' &
      //           '2    5727.92     )'
      PCK( 535 ) = ' '
      PCK( 536 ) = BEGTXT()
      PCK( 537 ) = ' '
      PCK( 538 ) = ' '
      PCK( 539 ) = ' '
      PCK( 540 ) = 'Neptune'
      PCK( 541 ) = ' '
      PCK( 542 ) = '     Old values are from the 2000 report:'
      PCK( 543 ) = ' '
      PCK( 544 ) = ' '
      PCK( 545 ) = '           body899_pole_ra        = (  29' &
      //           '9.36     0.         0. )'
      PCK( 546 ) = '           body899_pole_dec       = (   4' &
      //           '3.46     0.         0. )'
      PCK( 547 ) = '           body899_pm             = (  25' &
      //           '3.18   536.3128492  0. )'
      PCK( 548 ) = '           body899_long_axis      = (    ' &
      //           '0.                     )'
      PCK( 549 ) = ' '
      PCK( 550 ) = ' '
      PCK( 551 ) = '           body899_nut_prec_ra    = (  0.' &
      //           '70 0. 0. 0. 0. 0. 0. 0. )'
      PCK( 552 ) = '           body899_nut_prec_dec   = ( -0.' &
      //           '51 0. 0. 0. 0. 0. 0. 0. )'
      PCK( 553 ) = '           body899_nut_prec_pm    = ( -0.' &
      //           '48 0. 0. 0. 0. 0. 0. 0. )'
      PCK( 554 ) = ' '
      PCK( 555 ) = ' '
      PCK( 556 ) = '     Current values:'
      PCK( 557 ) = ' '
      PCK( 558 ) = BEGDAT()
      PCK( 559 ) = ' '
      PCK( 560 ) = '           BODY899_POLE_RA        = (  29' &
      //           '9.36     0.         0. )'
      PCK( 561 ) = '           BODY899_POLE_DEC       = (   4' &
      //           '3.46     0.         0. )'
      PCK( 562 ) = '           BODY899_PM             = (  28' &
      //           '5.946  541.1397757  0. )'
      PCK( 563 ) = '           BODY899_LONG_AXIS      = (    ' &
      //           '0.                     )'
      PCK( 564 ) = ' '
      PCK( 565 ) = ' '
      PCK( 566 ) = '           BODY899_NUT_PREC_RA    = (  0.' &
      //           '70 0. 0. 0. 0. 0. 0. 0. )'
      PCK( 567 ) = '           BODY899_NUT_PREC_DEC   = ( -0.' &
      //           '51 0. 0. 0. 0. 0. 0. 0. )'
      PCK( 568 ) = '           BODY899_NUT_PREC_PM    = ( -0.' &
      //           '48 0. 0. 0. 0. 0. 0. 0. )'
      PCK( 569 ) = ' '
      PCK( 570 ) = BEGTXT()
      PCK( 571 ) = ' '
      PCK( 572 ) = '           The 2000 report defines the nu' &
      //           'tation precession angles'
      PCK( 573 ) = ' '
      PCK( 574 ) = '              N, N1, N2, ... , N7'
      PCK( 575 ) = ' '
      PCK( 576 ) = '           and also uses the multiples of' &
      //           ' N1 and N7'
      PCK( 577 ) = ' '
      PCK( 578 ) = '              2*N1'
      PCK( 579 ) = ' '
      PCK( 580 ) = '           and'
      PCK( 581 ) = ' '
      PCK( 582 ) = '              2*N7, 3*N7, ..., 9*N7'
      PCK( 583 ) = ' '
      PCK( 584 ) = '           In this file, we treat the ang' &
      //           'les and their multiples as'
      PCK( 585 ) = '           separate angles.  In the kerne' &
      //           'l variable'
      PCK( 586 ) = ' '
      PCK( 587 ) = '              BODY8_NUT_PREC_ANGLES'
      PCK( 588 ) = ' '
      PCK( 589 ) = '           the order of the angles is'
      PCK( 590 ) = ' '
      PCK( 591 ) = '              N, N1, N2, ... , N7, 2*N1, ' &
      //           '2*N7, 3*N7, ..., 9*N7'
      PCK( 592 ) = ' '
      PCK( 593 ) = '           Each angle is defined by a lin' &
      //           'ear polynomial, so two'
      PCK( 594 ) = '           consecutive array elements are' &
      //           ' allocated for each'
      PCK( 595 ) = '           angle.  The first term of each' &
      //           ' pair is the constant term,'
      PCK( 596 ) = '           the second is the linear term.'
      PCK( 597 ) = ' '
      PCK( 598 ) = BEGDAT()
      PCK( 599 ) = ' '
      PCK( 600 ) = '              BODY8_NUT_PREC_ANGLES = (  ' &
      //           ' 357.85         52.316'
      PCK( 601 ) = '                                         ' &
      //           ' 323.92      62606.6'
      PCK( 602 ) = '                                         ' &
      //           ' 220.51      55064.2'
      PCK( 603 ) = '                                         ' &
      //           ' 354.27      46564.5'
      PCK( 604 ) = '                                         ' &
      //           '  75.31      26109.4'
      PCK( 605 ) = '                                         ' &
      //           '  35.36      14325.4'
      PCK( 606 ) = '                                         ' &
      //           ' 142.61       2824.6'
      PCK( 607 ) = '                                         ' &
      //           ' 177.85         52.316'
      PCK( 608 ) = '                                         ' &
      //           ' 647.840    125213.200'
      PCK( 609 ) = '                                         ' &
      //           ' 355.700       104.632'
      PCK( 610 ) = '                                         ' &
      //           ' 533.550       156.948'
      PCK( 611 ) = '                                         ' &
      //           ' 711.400       209.264'
      PCK( 612 ) = '                                         ' &
      //           ' 889.250       261.580'
      PCK( 613 ) = '                                         ' &
      //           '1067.100       313.896'
      PCK( 614 ) = '                                         ' &
      //           '1244.950       366.212'
      PCK( 615 ) = '                                         ' &
      //           '1422.800       418.528'
      PCK( 616 ) = '                                         ' &
      //           '1600.650       470.844   )'
      PCK( 617 ) = ' '
      PCK( 618 ) = BEGTXT()
      PCK( 619 ) = ' '
      PCK( 620 ) = ' '
      PCK( 621 ) = ' '
      PCK( 622 ) = ' '
      PCK( 623 ) = 'Orientation Constants for the Dwarf Plane' &
      //           't Pluto'
      PCK( 624 ) = '-----------------------------------------' &
      //           '---------------'
      PCK( 625 ) = ' '
      PCK( 626 ) = 'Pluto'
      PCK( 627 ) = ' '
      PCK( 628 ) = '     Old values:'
      PCK( 629 ) = ' '
      PCK( 630 ) = '        Values are from the 2009 IAU repo' &
      //           'rt.'
      PCK( 631 ) = ' '
      PCK( 632 ) = '        body999_pole_ra        = (  132.9' &
      //           '93   0.          0. )'
      PCK( 633 ) = '        body999_pole_dec       = (   -6.1' &
      //           '63   0.          0. )'
      PCK( 634 ) = '        body999_pm             = (  302.6' &
      //           '95   56.3625225  0. )'
      PCK( 635 ) = '        body999_long_axis      = (    0. ' &
      //           '                    )'
      PCK( 636 ) = ' '
      PCK( 637 ) = ' '
      PCK( 638 ) = '     Current values:'
      PCK( 639 ) = ' '
      PCK( 640 ) = BEGDAT()
      PCK( 641 ) = ' '
      PCK( 642 ) = '        BODY999_POLE_RA        = (  132.9' &
      //           '93   0.          0. )'
      PCK( 643 ) = '        BODY999_POLE_DEC       = (   -6.1' &
      //           '63   0.          0. )'
      PCK( 644 ) = '        BODY999_PM             = (  302.6' &
      //           '95   56.3625225  0. )'
      PCK( 645 ) = '        BODY999_LONG_AXIS      = (    0. ' &
      //           '                    )'
      PCK( 646 ) = ' '
      PCK( 647 ) = BEGTXT()
      PCK( 648 ) = ' '
      PCK( 649 ) = ' '
      PCK( 650 ) = ' '
      PCK( 651 ) = ' '
      PCK( 652 ) = 'Orientation constants for the satellites'
      PCK( 653 ) = '-----------------------------------------' &
      //           '---------------'
      PCK( 654 ) = ' '
      PCK( 655 ) = ' '
      PCK( 656 ) = 'Satellites of Earth'
      PCK( 657 ) = ' '
      PCK( 658 ) = '     Old values:'
      PCK( 659 ) = ' '
      PCK( 660 ) = '        The values shown below are those ' &
      //           'from the 2009 report. The 2015'
      PCK( 661 ) = '        report does not provide orientati' &
      //           'on data for the Earth or Moon.'
      PCK( 662 ) = ' '
      PCK( 663 ) = BEGDAT()
      PCK( 664 ) = ' '
      PCK( 665 ) = ' '
      PCK( 666 ) = '        BODY301_POLE_RA      = (  269.994' &
      //           '9        0.0031        0.      )'
      PCK( 667 ) = '        BODY301_POLE_DEC     = (   66.539' &
      //           '2        0.0130        0.      )'
      PCK( 668 ) = '        BODY301_PM           = (   38.321' &
      //           '3       13.17635815   -1.4D-12 )'
      PCK( 669 ) = '        BODY301_LONG_AXIS    = (    0.   ' &
      //           '                               )'
      PCK( 670 ) = ' '
      PCK( 671 ) = '        BODY301_NUT_PREC_RA  = (   -3.878' &
      //           '7   -0.1204   0.0700   -0.0172'
      PCK( 672 ) = '                                    0.0  ' &
      //           '     0.0072   0.0       0.0'
      PCK( 673 ) = '                                    0.0  ' &
      //           '    -0.0052   0.0       0.0'
      PCK( 674 ) = '                                    0.004' &
      //           '3                              )'
      PCK( 675 ) = ' '
      PCK( 676 ) = '        BODY301_NUT_PREC_DEC = (   1.5419' &
      //           '     0.0239  -0.0278    0.0068'
      PCK( 677 ) = '                                   0.0   ' &
      //           '    -0.0029   0.0009    0.0'
      PCK( 678 ) = '                                   0.0   ' &
      //           '     0.0008   0.0       0.0'
      PCK( 679 ) = '                                  -0.0009' &
      //           '                               )'
      PCK( 680 ) = ' '
      PCK( 681 ) = '        BODY301_NUT_PREC_PM  = (   3.5610' &
      //           '     0.1208  -0.0642    0.0158'
      PCK( 682 ) = '                                   0.0252' &
      //           '    -0.0066  -0.0047   -0.0046'
      PCK( 683 ) = '                                   0.0028' &
      //           '     0.0052   0.0040    0.0019'
      PCK( 684 ) = '                                  -0.0044' &
      //           '                               )'
      PCK( 685 ) = BEGTXT()
      PCK( 686 ) = ' '
      PCK( 687 ) = ' '
      PCK( 688 ) = ' '
      PCK( 689 ) = 'Satellites of Mars'
      PCK( 690 ) = ' '
      PCK( 691 ) = ' '
      PCK( 692 ) = '     Phobos'
      PCK( 693 ) = ' '
      PCK( 694 ) = '          Old values are from the 2009 re' &
      //           'port.'
      PCK( 695 ) = ' '
      PCK( 696 ) = ' '
      PCK( 697 ) = '          body401_pole_ra  = ( 317.68    ' &
      //           '-0.108     0.                     )'
      PCK( 698 ) = '          body401_pole_dec = (  52.90    ' &
      //           '-0.061     0.                     )'
      PCK( 699 ) = '          body401_pm       = (  35.06  11' &
      //           '28.8445850 6.6443009930565219e-09 )'
      PCK( 700 ) = ' '
      PCK( 701 ) = '          body401_long_axis     = (    0.' &
      //           '   )'
      PCK( 702 ) = ' '
      PCK( 703 ) = '          body401_nut_prec_ra   = (   1.7' &
      //           '9    0.    0.   0. )'
      PCK( 704 ) = '          body401_nut_prec_dec  = (  -1.0' &
      //           '8    0.    0.   0. )'
      PCK( 705 ) = '          body401_nut_prec_pm   = (  -1.4' &
      //           '2   -0.78  0.   0. )'
      PCK( 706 ) = ' '
      PCK( 707 ) = '            The quadratic prime meridian ' &
      //           'term is scaled by 1/36525**2:'
      PCK( 708 ) = ' '
      PCK( 709 ) = '               8.864000000000000   --->  ' &
      //           ' 6.6443009930565219E-09'
      PCK( 710 ) = ' '
      PCK( 711 ) = ' '
      PCK( 712 ) = '          Current values:'
      PCK( 713 ) = ' '
      PCK( 714 ) = '            The quadratic prime meridian ' &
      //           'term is scaled by 1/36525**2:'
      PCK( 715 ) = ' '
      PCK( 716 ) = '               12.72192797000000000000   ' &
      //           '--->  9.536137031212154e-09'
      PCK( 717 ) = ' '
      PCK( 718 ) = ' '
      PCK( 719 ) = ' '
      PCK( 720 ) = BEGDAT()
      PCK( 721 ) = ' '
      PCK( 722 ) = '          BODY401_POLE_RA  = ( 317.670716' &
      //           '57    -0.10844326  0. )'
      PCK( 723 ) = '          BODY401_POLE_DEC = (  52.886272' &
      //           '66    -0.06134706  0. )'
      PCK( 724 ) = '          BODY401_PM       = (  35.187744' &
      //           '40  1128.84475928'
      PCK( 725 ) = '                                9.5361370' &
      //           '31212154e-09 )'
      PCK( 726 ) = ' '
      PCK( 727 ) = '          BODY401_LONG_AXIS     = (    0.' &
      //           '   )'
      PCK( 728 ) = ' '
      PCK( 729 ) = '          BODY401_NUT_PREC_RA   = (  -1.7' &
      //           '8428399'
      PCK( 730 ) = '                                      0.0' &
      //           '2212824'
      PCK( 731 ) = '                                     -0.0' &
      //           '1028251'
      PCK( 732 ) = '                                     -0.0' &
      //           '0475595  )'
      PCK( 733 ) = ' '
      PCK( 734 ) = ' '
      PCK( 735 ) = ' '
      PCK( 736 ) = '          BODY401_NUT_PREC_DEC  = (  -1.0' &
      //           '7516537'
      PCK( 737 ) = '                                      0.0' &
      //           '0668626'
      PCK( 738 ) = '                                     -0.0' &
      //           '0648740'
      PCK( 739 ) = '                                      0.0' &
      //           '0281576  )'
      PCK( 740 ) = ' '
      PCK( 741 ) = ' '
      PCK( 742 ) = '          BODY401_NUT_PREC_PM   = (   1.4' &
      //           '2421769'
      PCK( 743 ) = '                                     -0.0' &
      //           '2273783'
      PCK( 744 ) = '                                      0.0' &
      //           '0410711'
      PCK( 745 ) = '                                      0.0' &
      //           '0631964'
      PCK( 746 ) = '                                      1.7' &
      //           '3203319'
      PCK( 747 ) = '                                      0.0' &
      //           '1635407'
      PCK( 748 ) = '                                      0.0' &
      //           '0021426  )'
      PCK( 749 ) = ' '
      PCK( 750 ) = ' '
      PCK( 751 ) = ' '
      PCK( 752 ) = ' '
      PCK( 753 ) = BEGTXT()
      PCK( 754 ) = ' '
      PCK( 755 ) = ' '
      PCK( 756 ) = '     Deimos'
      PCK( 757 ) = ' '
      PCK( 758 ) = '        Old values:'
      PCK( 759 ) = ' '
      PCK( 760 ) = '           Values are from the 2009 IAU r' &
      //           'eport.'
      PCK( 761 ) = ' '
      PCK( 762 ) = ' '
      PCK( 763 ) = '           The Deimos prime meridian expr' &
      //           'ession is:'
      PCK( 764 ) = ' '
      PCK( 765 ) = ' '
      PCK( 766 ) = '                                         ' &
      //           '            2'
      PCK( 767 ) = '              W = 79.41  +  285.1618970 d' &
      //           '  -  0.520 T  -  2.58 sin M'
      PCK( 768 ) = '                                         ' &
      //           '                           3'
      PCK( 769 ) = ' '
      PCK( 770 ) = '                                         ' &
      //           '              +  0.19 cos M .'
      PCK( 771 ) = '                                         ' &
      //           '                           3'
      PCK( 772 ) = ' '
      PCK( 773 ) = ' '
      PCK( 774 ) = '           At the present time, the PCK k' &
      //           'ernel software (the routine'
      PCK( 775 ) = '           BODEUL in particular) cannot h' &
      //           'andle the cosine term directly,'
      PCK( 776 ) = '           but we can represent it as'
      PCK( 777 ) = ' '
      PCK( 778 ) = '              0.19 sin M'
      PCK( 779 ) = '                        4'
      PCK( 780 ) = ' '
      PCK( 781 ) = '           where'
      PCK( 782 ) = ' '
      PCK( 783 ) = '              M   =  90.D0 - M'
      PCK( 784 ) = '               4              3'
      PCK( 785 ) = ' '
      PCK( 786 ) = '           Therefore, the nutation preces' &
      //           'sion angle assignments for Phobos'
      PCK( 787 ) = '           and Deimos contain four coeffi' &
      //           'cients rather than three.'
      PCK( 788 ) = ' '
      PCK( 789 ) = '           The quadratic prime meridian t' &
      //           'erm is scaled by 1/36525**2:'
      PCK( 790 ) = ' '
      PCK( 791 ) = '              -0.5200000000000000  --->  ' &
      //           ' -3.8978300049519307E-10'
      PCK( 792 ) = ' '
      PCK( 793 ) = ' '
      PCK( 794 ) = '           body402_pole_ra       = (  316' &
      //           '.65     -0.108       0.           )'
      PCK( 795 ) = '           body402_pole_dec      = (   53' &
      //           '.52     -0.061       0.           )'
      PCK( 796 ) = '           body402_pm            = (   79' &
      //           '.41    285.1618970  -3.897830d-10 )'
      PCK( 797 ) = '           body402_long_axis     = (    0' &
      //           '.                                 )'
      PCK( 798 ) = ' '
      PCK( 799 ) = '           body402_nut_prec_ra   = (    0' &
      //           '.   0.   2.98    0.   )'
      PCK( 800 ) = '           body402_nut_prec_dec  = (    0' &
      //           '.   0.  -1.78    0.   )'
      PCK( 801 ) = '           body402_nut_prec_pm   = (    0' &
      //           '.   0.  -2.58    0.19 )'
      PCK( 802 ) = ' '
      PCK( 803 ) = ' '
      PCK( 804 ) = ' '
      PCK( 805 ) = '        New values:'
      PCK( 806 ) = ' '
      PCK( 807 ) = ' '
      PCK( 808 ) = BEGDAT()
      PCK( 809 ) = ' '
      PCK( 810 ) = '           BODY402_POLE_RA       = (  316' &
      //           '.65705808     -0.10518014      0. )'
      PCK( 811 ) = '           BODY402_POLE_DEC      = (   53' &
      //           '.50992033     -0.05979094      0. )'
      PCK( 812 ) = ' '
      PCK( 813 ) = '           BODY402_PM            = (   79' &
      //           '.39932954    285.16188899      0. )'
      PCK( 814 ) = '           BODY402_LONG_AXIS     = (    0' &
      //           '.                                 )'
      PCK( 815 ) = ' '
      PCK( 816 ) = '           BODY402_NUT_PREC_RA   = (    0' &
      //           '    0    0    0'
      PCK( 817 ) = '                                        0' &
      //           '    0    0'
      PCK( 818 ) = '                                        3' &
      //           '.09217726'
      PCK( 819 ) = '                                        0' &
      //           '.22980637'
      PCK( 820 ) = '                                        0' &
      //           '.06418655'
      PCK( 821 ) = '                                        0' &
      //           '.02533537'
      PCK( 822 ) = '                                        0' &
      //           '.00778695       )'
      PCK( 823 ) = ' '
      PCK( 824 ) = ' '
      PCK( 825 ) = '           BODY402_NUT_PREC_DEC  = (    0' &
      //           '    0    0    0'
      PCK( 826 ) = '                                        0' &
      //           '    0    0'
      PCK( 827 ) = '                                        1' &
      //           '.83936004'
      PCK( 828 ) = '                                        0' &
      //           '.14325320'
      PCK( 829 ) = '                                        0' &
      //           '.01911409'
      PCK( 830 ) = '                                       -0' &
      //           '.01482590'
      PCK( 831 ) = '                                        0' &
      //           '.00192430       )'
      PCK( 832 ) = ' '
      PCK( 833 ) = ' '
      PCK( 834 ) = '           BODY402_NUT_PREC_PM   = (    0' &
      //           '    0    0    0'
      PCK( 835 ) = '                                        0' &
      //           '    0    0'
      PCK( 836 ) = '                                       -2' &
      //           '.73954829'
      PCK( 837 ) = '                                       -0' &
      //           '.39968606'
      PCK( 838 ) = '                                       -0' &
      //           '.06563259'
      PCK( 839 ) = '                                       -0' &
      //           '.02912940'
      PCK( 840 ) = '                                        0' &
      //           '.01699160'
      PCK( 841 ) = '                                        0' &
      //           '.03080596'
      PCK( 842 ) = '                                        0' &
      //           '.01248044'
      PCK( 843 ) = '                                       -0' &
      //           '.00437509       )'
      PCK( 844 ) = ' '
      PCK( 845 ) = ' '
      PCK( 846 ) = BEGTXT()
      PCK( 847 ) = ' '
      PCK( 848 ) = ' '
      PCK( 849 ) = ' '
      PCK( 850 ) = ' '
      PCK( 851 ) = 'Satellites of Jupiter'
      PCK( 852 ) = ' '
      PCK( 853 ) = ' '
      PCK( 854 ) = '     Io'
      PCK( 855 ) = ' '
      PCK( 856 ) = '          Old values:'
      PCK( 857 ) = ' '
      PCK( 858 ) = '             Values are unchanged in the ' &
      //           '2009 IAU report.'
      PCK( 859 ) = ' '
      PCK( 860 ) = '          Current values:'
      PCK( 861 ) = ' '
      PCK( 862 ) = BEGDAT()
      PCK( 863 ) = ' '
      PCK( 864 ) = '        BODY501_POLE_RA       = (  268.05' &
      //           '          -0.009      0. )'
      PCK( 865 ) = '        BODY501_POLE_DEC      = (   64.50' &
      //           '           0.003      0. )'
      PCK( 866 ) = '        BODY501_PM            = (  200.39' &
      //           '         203.4889538  0. )'
      PCK( 867 ) = '        BODY501_LONG_AXIS     = (    0.  ' &
      //           '                         )'
      PCK( 868 ) = ' '
      PCK( 869 ) = '        BODY501_NUT_PREC_RA   = (    0.  ' &
      //           ' 0.     0.094    0.024   )'
      PCK( 870 ) = '        BODY501_NUT_PREC_DEC  = (    0.  ' &
      //           ' 0.     0.040    0.011   )'
      PCK( 871 ) = '        BODY501_NUT_PREC_PM   = (    0.  ' &
      //           ' 0.    -0.085   -0.022   )'
      PCK( 872 ) = ' '
      PCK( 873 ) = BEGTXT()
      PCK( 874 ) = ' '
      PCK( 875 ) = ' '
      PCK( 876 ) = ' '
      PCK( 877 ) = '     Europa'
      PCK( 878 ) = ' '
      PCK( 879 ) = ' '
      PCK( 880 ) = '        Old values:'
      PCK( 881 ) = ' '
      PCK( 882 ) = '           Values are unchanged in the 20' &
      //           '09 IAU report.'
      PCK( 883 ) = ' '
      PCK( 884 ) = ' '
      PCK( 885 ) = '        Current values:'
      PCK( 886 ) = ' '
      PCK( 887 ) = BEGDAT()
      PCK( 888 ) = ' '
      PCK( 889 ) = '        BODY502_POLE_RA       = (  268.08' &
      //           '          -0.009      0.   )'
      PCK( 890 ) = '        BODY502_POLE_DEC      = (   64.51' &
      //           '           0.003      0.   )'
      PCK( 891 ) = '        BODY502_PM            = (   36.02' &
      //           '2        101.3747235  0.   )'
      PCK( 892 ) = '        BODY502_LONG_AXIS     = (    0.  ' &
      //           '                           )'
      PCK( 893 ) = ' '
      PCK( 894 ) = '        BODY502_NUT_PREC_RA   = ( 0. 0. 0' &
      //           '.   1.086   0.060   0.015   0.009 )'
      PCK( 895 ) = '        BODY502_NUT_PREC_DEC  = ( 0. 0. 0' &
      //           '.   0.468   0.026   0.007   0.002 )'
      PCK( 896 ) = '        BODY502_NUT_PREC_PM   = ( 0. 0. 0' &
      //           '.  -0.980  -0.054  -0.014  -0.008 )'
      PCK( 897 ) = ' '
      PCK( 898 ) = BEGTXT()
      PCK( 899 ) = ' '
      PCK( 900 ) = ' '
      PCK( 901 ) = '     Ganymede'
      PCK( 902 ) = ' '
      PCK( 903 ) = '        Old values:'
      PCK( 904 ) = ' '
      PCK( 905 ) = '             Values are unchanged in the ' &
      //           '2009 IAU report.'
      PCK( 906 ) = ' '
      PCK( 907 ) = '        Current values:'
      PCK( 908 ) = ' '
      PCK( 909 ) = BEGDAT()
      PCK( 910 ) = ' '
      PCK( 911 ) = '        BODY503_POLE_RA       = (  268.20' &
      //           '         -0.009       0.  )'
      PCK( 912 ) = '        BODY503_POLE_DEC      = (   64.57' &
      //           '          0.003       0.  )'
      PCK( 913 ) = '        BODY503_PM            = (   44.06' &
      //           '4        50.3176081   0.  )'
      PCK( 914 ) = '        BODY503_LONG_AXIS     = (    0.  ' &
      //           '                          )'
      PCK( 915 ) = ' '
      PCK( 916 ) = '        BODY503_NUT_PREC_RA   = ( 0. 0. 0' &
      //           '.  -0.037   0.431   0.091   )'
      PCK( 917 ) = '        BODY503_NUT_PREC_DEC  = ( 0. 0. 0' &
      //           '.  -0.016   0.186   0.039   )'
      PCK( 918 ) = '        BODY503_NUT_PREC_PM   = ( 0. 0. 0' &
      //           '.   0.033  -0.389  -0.082   )'
      PCK( 919 ) = ' '
      PCK( 920 ) = BEGTXT()
      PCK( 921 ) = ' '
      PCK( 922 ) = ' '
      PCK( 923 ) = '     Callisto'
      PCK( 924 ) = ' '
      PCK( 925 ) = '        Old values:'
      PCK( 926 ) = ' '
      PCK( 927 ) = '             Values are unchanged in the ' &
      //           '2009 IAU report.'
      PCK( 928 ) = ' '
      PCK( 929 ) = '        Current values:'
      PCK( 930 ) = ' '
      PCK( 931 ) = ' '
      PCK( 932 ) = BEGDAT()
      PCK( 933 ) = ' '
      PCK( 934 ) = '        BODY504_POLE_RA       = (   268.7' &
      //           '2    -0.009       0.  )'
      PCK( 935 ) = '        BODY504_POLE_DEC      = (    64.8' &
      //           '3     0.003       0.  )'
      PCK( 936 ) = '        BODY504_PM            = (   259.5' &
      //           '1    21.5710715   0.  )'
      PCK( 937 ) = '        BODY504_LONG_AXIS     = (     0. ' &
      //           '                      )'
      PCK( 938 ) = ' '
      PCK( 939 ) = '        BODY504_NUT_PREC_RA   = ( 0. 0. 0' &
      //           '. 0.  -0.068   0.590  0.   0.010 )'
      PCK( 940 ) = '        BODY504_NUT_PREC_DEC  = ( 0. 0. 0' &
      //           '. 0.  -0.029   0.254  0.  -0.004 )'
      PCK( 941 ) = '        BODY504_NUT_PREC_PM   = ( 0. 0. 0' &
      //           '. 0.   0.061  -0.533  0.  -0.009 )'
      PCK( 942 ) = ' '
      PCK( 943 ) = BEGTXT()
      PCK( 944 ) = ' '
      PCK( 945 ) = ' '
      PCK( 946 ) = '     Amalthea'
      PCK( 947 ) = ' '
      PCK( 948 ) = ' '
      PCK( 949 ) = '        Old values:'
      PCK( 950 ) = ' '
      PCK( 951 ) = '           Values are unchanged in the 20' &
      //           '09 IAU report.'
      PCK( 952 ) = ' '
      PCK( 953 ) = '        Current values:'
      PCK( 954 ) = ' '
      PCK( 955 ) = BEGDAT()
      PCK( 956 ) = ' '
      PCK( 957 ) = '        BODY505_POLE_RA       = (   268.0' &
      //           '5    -0.009      0.  )'
      PCK( 958 ) = '        BODY505_POLE_DEC      = (    64.4' &
      //           '9     0.003      0.  )'
      PCK( 959 ) = '        BODY505_PM            = (   231.6' &
      //           '7   722.6314560  0.  )'
      PCK( 960 ) = '        BODY505_LONG_AXIS     = (     0. ' &
      //           '                     )'
      PCK( 961 ) = ' '
      PCK( 962 ) = '        BODY505_NUT_PREC_RA  = ( -0.84  0' &
      //           '. 0. 0. 0. 0. 0. 0.   0.01  0. )'
      PCK( 963 ) = '        BODY505_NUT_PREC_DEC = ( -0.36  0' &
      //           '. 0. 0. 0. 0. 0. 0.   0.    0. )'
      PCK( 964 ) = '        BODY505_NUT_PREC_PM  = (  0.76  0' &
      //           '. 0. 0. 0. 0. 0. 0.  -0.01  0. )'
      PCK( 965 ) = ' '
      PCK( 966 ) = BEGTXT()
      PCK( 967 ) = ' '
      PCK( 968 ) = ' '
      PCK( 969 ) = '     Thebe'
      PCK( 970 ) = ' '
      PCK( 971 ) = ' '
      PCK( 972 ) = '        Old values:'
      PCK( 973 ) = ' '
      PCK( 974 ) = '           Values are unchanged in the 20' &
      //           '09 IAU report.'
      PCK( 975 ) = ' '
      PCK( 976 ) = '        Current values:'
      PCK( 977 ) = ' '
      PCK( 978 ) = BEGDAT()
      PCK( 979 ) = ' '
      PCK( 980 ) = '        BODY514_POLE_RA       = (  268.05' &
      //           '     -0.009       0.  )'
      PCK( 981 ) = '        BODY514_POLE_DEC      = (   64.49' &
      //           '      0.003       0.  )'
      PCK( 982 ) = '        BODY514_PM            = (    8.56' &
      //           '    533.7004100   0.  )'
      PCK( 983 ) = '        BODY514_LONG_AXIS     = (    0.  ' &
      //           '                      )'
      PCK( 984 ) = ' '
      PCK( 985 ) = '        BODY514_NUT_PREC_RA  = ( 0.  -2.1' &
      //           '1  0. 0. 0. 0. 0. 0. 0.  0.04 )'
      PCK( 986 ) = '        BODY514_NUT_PREC_DEC = ( 0.  -0.9' &
      //           '1  0. 0. 0. 0. 0. 0. 0.  0.01 )'
      PCK( 987 ) = '        BODY514_NUT_PREC_PM  = ( 0.   1.9' &
      //           '1  0. 0. 0. 0. 0. 0. 0. -0.04 )'
      PCK( 988 ) = ' '
      PCK( 989 ) = BEGTXT()
      PCK( 990 ) = ' '
      PCK( 991 ) = ' '
      PCK( 992 ) = '     Adrastea'
      PCK( 993 ) = ' '
      PCK( 994 ) = '        Old values:'
      PCK( 995 ) = ' '
      PCK( 996 ) = '           Values are unchanged in the 20' &
      //           '09 IAU report.'
      PCK( 997 ) = ' '
      PCK( 998 ) = '        Current values:'
      PCK( 999 ) = ' '
      PCK( 1000 ) = BEGDAT()
      PCK( 1001 ) = ' '
      PCK( 1002 ) = '        BODY515_POLE_RA       = (  268.0' &
      //            '5     -0.009       0.  )'
      PCK( 1003 ) = '        BODY515_POLE_DEC      = (   64.4' &
      //            '9      0.003       0.  )'
      PCK( 1004 ) = '        BODY515_PM            = (   33.2' &
      //            '9   1206.9986602   0.  )'
      PCK( 1005 ) = '        BODY515_LONG_AXIS     = (    0. ' &
      //            '                       )'
      PCK( 1006 ) = ' '
      PCK( 1007 ) = BEGTXT()
      PCK( 1008 ) = ' '
      PCK( 1009 ) = ' '
      PCK( 1010 ) = '     Metis'
      PCK( 1011 ) = ' '
      PCK( 1012 ) = '        Old values:'
      PCK( 1013 ) = ' '
      PCK( 1014 ) = '           Values are unchanged in the 2' &
      //            '009 IAU report.'
      PCK( 1015 ) = ' '
      PCK( 1016 ) = '        Current values:'
      PCK( 1017 ) = ' '
      PCK( 1018 ) = BEGDAT()
      PCK( 1019 ) = ' '
      PCK( 1020 ) = '        BODY516_POLE_RA       = (  268.0' &
      //            '5     -0.009       0.  )'
      PCK( 1021 ) = '        BODY516_POLE_DEC      = (   64.4' &
      //            '9      0.003       0.  )'
      PCK( 1022 ) = '        BODY516_PM            = (  346.0' &
      //            '9   1221.2547301   0.  )'
      PCK( 1023 ) = '        BODY516_LONG_AXIS     = (    0. ' &
      //            '                       )'
      PCK( 1024 ) = ' '
      PCK( 1025 ) = BEGTXT()
      PCK( 1026 ) = ' '
      PCK( 1027 ) = ' '
      PCK( 1028 ) = ' '
      PCK( 1029 ) = 'Satellites of Saturn'
      PCK( 1030 ) = ' '
      PCK( 1031 ) = ' '
      PCK( 1032 ) = '     Mimas'
      PCK( 1033 ) = ' '
      PCK( 1034 ) = '        Old values:'
      PCK( 1035 ) = ' '
      PCK( 1036 ) = '           Values are from the 2006 IAU ' &
      //            'report.'
      PCK( 1037 ) = ' '
      PCK( 1038 ) = '           body601_pole_ra       = (   4' &
      //            '0.66     -0.036      0.  )'
      PCK( 1039 ) = '           body601_pole_dec      = (   8' &
      //            '3.52     -0.004      0.  )'
      PCK( 1040 ) = '           body601_pm            = (  33' &
      //            '7.46    381.9945550  0.  )'
      PCK( 1041 ) = '           body601_long_axis     = (    ' &
      //            ' 0.                      )'
      PCK( 1042 ) = ' '
      PCK( 1043 ) = '           body601_nut_prec_ra   = ( 0. ' &
      //            '0.   13.56  0.    0.    0. 0. 0. 0. )'
      PCK( 1044 ) = '           body601_nut_prec_dec  = ( 0. ' &
      //            '0.   -1.53  0.    0.    0. 0. 0. 0. )'
      PCK( 1045 ) = '           body601_nut_prec_pm   = ( 0. ' &
      //            '0.  -13.48  0.  -44.85  0. 0. 0. 0. )'
      PCK( 1046 ) = ' '
      PCK( 1047 ) = ' '
      PCK( 1048 ) = '        Current values:'
      PCK( 1049 ) = ' '
      PCK( 1050 ) = BEGDAT()
      PCK( 1051 ) = ' '
      PCK( 1052 ) = '           BODY601_POLE_RA       = (   4' &
      //            '0.66     -0.036      0.  )'
      PCK( 1053 ) = '           BODY601_POLE_DEC      = (   8' &
      //            '3.52     -0.004      0.  )'
      PCK( 1054 ) = '           BODY601_PM            = (  33' &
      //            '3.46    381.9945550  0.  )'
      PCK( 1055 ) = '           BODY601_LONG_AXIS     = (    ' &
      //            ' 0.                      )'
      PCK( 1056 ) = ' '
      PCK( 1057 ) = '           BODY601_NUT_PREC_RA   = ( 0. ' &
      //            '0.   13.56  0.    0.    0. 0. 0.  )'
      PCK( 1058 ) = '           BODY601_NUT_PREC_DEC  = ( 0. ' &
      //            '0.   -1.53  0.    0.    0. 0. 0.  )'
      PCK( 1059 ) = '           BODY601_NUT_PREC_PM   = ( 0. ' &
      //            '0.  -13.48  0.  -44.85  0. 0. 0.  )'
      PCK( 1060 ) = ' '
      PCK( 1061 ) = BEGTXT()
      PCK( 1062 ) = ' '
      PCK( 1063 ) = ' '
      PCK( 1064 ) = '     Enceladus'
      PCK( 1065 ) = ' '
      PCK( 1066 ) = ' '
      PCK( 1067 ) = '        Old values:'
      PCK( 1068 ) = ' '
      PCK( 1069 ) = '           Values are from the 2006 IAU ' &
      //            'report.'
      PCK( 1070 ) = ' '
      PCK( 1071 ) = '           body602_pole_ra       = (   4' &
      //            '0.66    -0.036       0. )'
      PCK( 1072 ) = '           body602_pole_dec      = (   8' &
      //            '3.52    -0.004       0. )'
      PCK( 1073 ) = '           body602_pm            = (    ' &
      //            '2.82   262.7318996   0. )'
      PCK( 1074 ) = '           body602_long_axis     = (    ' &
      //            '0.                      )'
      PCK( 1075 ) = ' '
      PCK( 1076 ) = ' '
      PCK( 1077 ) = '        Current values:'
      PCK( 1078 ) = ' '
      PCK( 1079 ) = BEGDAT()
      PCK( 1080 ) = ' '
      PCK( 1081 ) = '           BODY602_POLE_RA       = (   4' &
      //            '0.66    -0.036       0. )'
      PCK( 1082 ) = '           BODY602_POLE_DEC      = (   8' &
      //            '3.52    -0.004       0. )'
      PCK( 1083 ) = '           BODY602_PM            = (    ' &
      //            '6.32   262.7318996   0. )'
      PCK( 1084 ) = '           BODY602_LONG_AXIS     = (    ' &
      //            '0.                      )'
      PCK( 1085 ) = ' '
      PCK( 1086 ) = BEGTXT()
      PCK( 1087 ) = ' '
      PCK( 1088 ) = ' '
      PCK( 1089 ) = ' '
      PCK( 1090 ) = '     Tethys'
      PCK( 1091 ) = ' '
      PCK( 1092 ) = ' '
      PCK( 1093 ) = '        Old values:'
      PCK( 1094 ) = ' '
      PCK( 1095 ) = '           Values are from the 2006 IAU ' &
      //            'report.'
      PCK( 1096 ) = ' '
      PCK( 1097 ) = '           body603_pole_ra       = (   4' &
      //            '0.66    -0.036       0. )'
      PCK( 1098 ) = '           body603_pole_dec      = (   8' &
      //            '3.52    -0.004       0. )'
      PCK( 1099 ) = '           body603_pm            = (   1' &
      //            '0.45   190.6979085   0. )'
      PCK( 1100 ) = '           body603_long_axis     = (    ' &
      //            '0.                      )'
      PCK( 1101 ) = ' '
      PCK( 1102 ) = '           body603_nut_prec_ra   = ( 0. ' &
      //            '0. 0.   9.66   0.    0.  0.  0.  0. )'
      PCK( 1103 ) = '           body603_nut_prec_dec  = ( 0. ' &
      //            '0. 0.  -1.09   0.    0.  0.  0.  0. )'
      PCK( 1104 ) = '           body603_nut_prec_pm   = ( 0. ' &
      //            '0. 0.  -9.60   2.23  0.  0.  0.  0. )'
      PCK( 1105 ) = ' '
      PCK( 1106 ) = ' '
      PCK( 1107 ) = '        Current values:'
      PCK( 1108 ) = ' '
      PCK( 1109 ) = BEGDAT()
      PCK( 1110 ) = ' '
      PCK( 1111 ) = '           BODY603_POLE_RA       = (   4' &
      //            '0.66    -0.036       0. )'
      PCK( 1112 ) = '           BODY603_POLE_DEC      = (   8' &
      //            '3.52    -0.004       0. )'
      PCK( 1113 ) = '           BODY603_PM            = (    ' &
      //            '8.95   190.6979085   0. )'
      PCK( 1114 ) = '           BODY603_LONG_AXIS     = (    ' &
      //            '0.                      )'
      PCK( 1115 ) = ' '
      PCK( 1116 ) = '           BODY603_NUT_PREC_RA   = ( 0. ' &
      //            '0. 0.   9.66   0.    0.  0.  0. )'
      PCK( 1117 ) = '           BODY603_NUT_PREC_DEC  = ( 0. ' &
      //            '0. 0.  -1.09   0.    0.  0.  0. )'
      PCK( 1118 ) = '           BODY603_NUT_PREC_PM   = ( 0. ' &
      //            '0. 0.  -9.60   2.23  0.  0.  0. )'
      PCK( 1119 ) = ' '
      PCK( 1120 ) = BEGTXT()
      PCK( 1121 ) = ' '
      PCK( 1122 ) = ' '
      PCK( 1123 ) = '     Dione'
      PCK( 1124 ) = ' '
      PCK( 1125 ) = ' '
      PCK( 1126 ) = '        Old values:'
      PCK( 1127 ) = ' '
      PCK( 1128 ) = '           Values are from the 2006 IAU ' &
      //            'report.'
      PCK( 1129 ) = ' '
      PCK( 1130 ) = '           body604_pole_ra       = (  40' &
      //            '.66      -0.036      0.  )'
      PCK( 1131 ) = '           body604_pole_dec      = (  83' &
      //            '.52      -0.004      0.  )'
      PCK( 1132 ) = '           body604_pm            = (  35' &
      //            '7.00    131.5349316  0.  )'
      PCK( 1133 ) = '           body604_long_axis     = (    ' &
      //            '0.                       )'
      PCK( 1134 ) = ' '
      PCK( 1135 ) = ' '
      PCK( 1136 ) = '        Current values:'
      PCK( 1137 ) = ' '
      PCK( 1138 ) = BEGDAT()
      PCK( 1139 ) = ' '
      PCK( 1140 ) = '           BODY604_POLE_RA       = (  40' &
      //            '.66      -0.036      0.  )'
      PCK( 1141 ) = '           BODY604_POLE_DEC      = (  83' &
      //            '.52      -0.004      0.  )'
      PCK( 1142 ) = '           BODY604_PM            = (  35' &
      //            '7.6     131.5349316  0.  )'
      PCK( 1143 ) = '           BODY604_LONG_AXIS     = (    ' &
      //            '0.                       )'
      PCK( 1144 ) = ' '
      PCK( 1145 ) = BEGTXT()
      PCK( 1146 ) = ' '
      PCK( 1147 ) = ' '
      PCK( 1148 ) = ' '
      PCK( 1149 ) = '     Rhea'
      PCK( 1150 ) = ' '
      PCK( 1151 ) = ' '
      PCK( 1152 ) = '        Old values:'
      PCK( 1153 ) = ' '
      PCK( 1154 ) = '           Values are from the 2009 IAU ' &
      //            'report.'
      PCK( 1155 ) = ' '
      PCK( 1156 ) = '           body605_pole_ra       = (   4' &
      //            '0.38   -0.036       0. )'
      PCK( 1157 ) = '           body605_pole_dec      = (   8' &
      //            '3.55   -0.004       0. )'
      PCK( 1158 ) = '           body605_pm            = (  23' &
      //            '5.16   79.6900478   0. )'
      PCK( 1159 ) = '           body605_long_axis     = (    ' &
      //            '0.                     )'
      PCK( 1160 ) = ' '
      PCK( 1161 ) = '           body605_nut_prec_ra   = ( 0. ' &
      //            '0. 0. 0. 0.   3.10   0. 0. 0. )'
      PCK( 1162 ) = '           body605_nut_prec_dec  = ( 0. ' &
      //            '0. 0. 0. 0.  -0.35   0. 0. 0. )'
      PCK( 1163 ) = '           body605_nut_prec_pm   = ( 0. ' &
      //            '0. 0. 0. 0.  -3.08   0. 0. 0. )'
      PCK( 1164 ) = ' '
      PCK( 1165 ) = ' '
      PCK( 1166 ) = '        Current values:'
      PCK( 1167 ) = ' '
      PCK( 1168 ) = '           Data values are unchanged in ' &
      //            'the 2009 IAU report. However'
      PCK( 1169 ) = '           the kernel variable contents ' &
      //            'have changed due to removal of'
      PCK( 1170 ) = '           the angle S7.'
      PCK( 1171 ) = ' '
      PCK( 1172 ) = BEGDAT()
      PCK( 1173 ) = ' '
      PCK( 1174 ) = '           BODY605_POLE_RA       = (   4' &
      //            '0.38   -0.036       0. )'
      PCK( 1175 ) = '           BODY605_POLE_DEC      = (   8' &
      //            '3.55   -0.004       0. )'
      PCK( 1176 ) = '           BODY605_PM            = (  23' &
      //            '5.16   79.6900478   0. )'
      PCK( 1177 ) = '           BODY605_LONG_AXIS     = (    ' &
      //            '0.                     )'
      PCK( 1178 ) = ' '
      PCK( 1179 ) = '           BODY605_NUT_PREC_RA   = ( 0. ' &
      //            '0. 0. 0. 0.   3.10   0. 0. )'
      PCK( 1180 ) = '           BODY605_NUT_PREC_DEC  = ( 0. ' &
      //            '0. 0. 0. 0.  -0.35   0. 0. )'
      PCK( 1181 ) = '           BODY605_NUT_PREC_PM   = ( 0. ' &
      //            '0. 0. 0. 0.  -3.08   0. 0. )'
      PCK( 1182 ) = ' '
      PCK( 1183 ) = BEGTXT()
      PCK( 1184 ) = ' '
      PCK( 1185 ) = ' '
      PCK( 1186 ) = ' '
      PCK( 1187 ) = '     Titan'
      PCK( 1188 ) = ' '
      PCK( 1189 ) = ' '
      PCK( 1190 ) = '        Old values:'
      PCK( 1191 ) = ' '
      PCK( 1192 ) = '           Values are from the 2006 IAU ' &
      //            'report.'
      PCK( 1193 ) = ' '
      PCK( 1194 ) = '           BODY606_POLE_RA       = (    ' &
      //            '36.41   -0.036      0. )'
      PCK( 1195 ) = '           BODY606_POLE_DEC      = (    ' &
      //            '83.94   -0.004      0. )'
      PCK( 1196 ) = '           BODY606_PM            = (   1' &
      //            '89.64   22.5769768  0. )'
      PCK( 1197 ) = '           BODY606_LONG_AXIS     = (    ' &
      //            ' 0.                    )'
      PCK( 1198 ) = ' '
      PCK( 1199 ) = '           BODY606_NUT_PREC_RA   = ( 0. ' &
      //            '0. 0. 0. 0. 0.  2.66  0. 0 )'
      PCK( 1200 ) = '           BODY606_NUT_PREC_DEC  = ( 0. ' &
      //            '0. 0. 0. 0. 0. -0.30  0. 0 )'
      PCK( 1201 ) = '           BODY606_NUT_PREC_PM   = ( 0. ' &
      //            '0. 0. 0. 0. 0. -2.64  0. 0 )'
      PCK( 1202 ) = ' '
      PCK( 1203 ) = ' '
      PCK( 1204 ) = '        Current values:'
      PCK( 1205 ) = ' '
      PCK( 1206 ) = '              Note removal of dependence' &
      //            ' on the nutation precession'
      PCK( 1207 ) = '              angles.'
      PCK( 1208 ) = ' '
      PCK( 1209 ) = BEGDAT()
      PCK( 1210 ) = ' '
      PCK( 1211 ) = '           BODY606_POLE_RA       = (    ' &
      //            '39.4827    0.         0. )'
      PCK( 1212 ) = '           BODY606_POLE_DEC      = (    ' &
      //            '83.4279    0.         0. )'
      PCK( 1213 ) = '           BODY606_PM            = (   1' &
      //            '86.5855   22.5769768  0. )'
      PCK( 1214 ) = '           BODY606_LONG_AXIS     = (    ' &
      //            ' 0.                      )'
      PCK( 1215 ) = ' '
      PCK( 1216 ) = '           BODY606_NUT_PREC_RA   = ( 0. ' &
      //            '0. 0. 0. 0. 0. 0. 0 )'
      PCK( 1217 ) = '           BODY606_NUT_PREC_DEC  = ( 0. ' &
      //            '0. 0. 0. 0. 0. 0. 0 )'
      PCK( 1218 ) = '           BODY606_NUT_PREC_PM   = ( 0. ' &
      //            '0. 0. 0. 0. 0. 0. 0 )'
      PCK( 1219 ) = ' '
      PCK( 1220 ) = BEGTXT()
      PCK( 1221 ) = ' '
      PCK( 1222 ) = ' '
      PCK( 1223 ) = ' '
      PCK( 1224 ) = '     Hyperion'
      PCK( 1225 ) = ' '
      PCK( 1226 ) = '         The IAU report does not give an' &
      //            ' orientation model for Hyperion.'
      PCK( 1227 ) = '         Hyperion''s rotation is in chao' &
      //            'tic and is not predictable for'
      PCK( 1228 ) = '         long periods.'
      PCK( 1229 ) = ' '
      PCK( 1230 ) = ' '
      PCK( 1231 ) = '     Iapetus'
      PCK( 1232 ) = ' '
      PCK( 1233 ) = ' '
      PCK( 1234 ) = '        Old values:'
      PCK( 1235 ) = ' '
      PCK( 1236 ) = '           Values are from the 2006 IAU ' &
      //            'report.'
      PCK( 1237 ) = ' '
      PCK( 1238 ) = '           body608_pole_ra       = (   3' &
      //            '18.16  -3.949      0.  )'
      PCK( 1239 ) = '           body608_pole_dec      = (    ' &
      //            '75.03  -1.143      0.  )'
      PCK( 1240 ) = '           body608_pm            = (   3' &
      //            '50.20   4.5379572  0.  )'
      PCK( 1241 ) = '           body608_long_axis     = (    ' &
      //            ' 0.                    )'
      PCK( 1242 ) = ' '
      PCK( 1243 ) = ' '
      PCK( 1244 ) = '        Current values:'
      PCK( 1245 ) = ' '
      PCK( 1246 ) = BEGDAT()
      PCK( 1247 ) = ' '
      PCK( 1248 ) = '           BODY608_POLE_RA       = (   3' &
      //            '18.16  -3.949      0.  )'
      PCK( 1249 ) = '           BODY608_POLE_DEC      = (    ' &
      //            '75.03  -1.143      0.  )'
      PCK( 1250 ) = '           BODY608_PM            = (   3' &
      //            '55.2    4.5379572  0.  )'
      PCK( 1251 ) = '           BODY608_LONG_AXIS     = (    ' &
      //            ' 0.                    )'
      PCK( 1252 ) = ' '
      PCK( 1253 ) = BEGTXT()
      PCK( 1254 ) = ' '
      PCK( 1255 ) = ' '
      PCK( 1256 ) = ' '
      PCK( 1257 ) = '     Phoebe'
      PCK( 1258 ) = ' '
      PCK( 1259 ) = ' '
      PCK( 1260 ) = '        Old values:'
      PCK( 1261 ) = ' '
      PCK( 1262 ) = '           Values are unchanged in the 2' &
      //            '009 IAU report.'
      PCK( 1263 ) = ' '
      PCK( 1264 ) = '        Current values:'
      PCK( 1265 ) = ' '
      PCK( 1266 ) = BEGDAT()
      PCK( 1267 ) = ' '
      PCK( 1268 ) = '           BODY609_POLE_RA       = ( 356' &
      //            '.90       0.         0.  )'
      PCK( 1269 ) = '           BODY609_POLE_DEC      = (  77' &
      //            '.80       0.         0.  )'
      PCK( 1270 ) = '           BODY609_PM            = ( 178' &
      //            '.58     931.639      0.  )'
      PCK( 1271 ) = '           BODY609_LONG_AXIS     = (    ' &
      //            '0.                       )'
      PCK( 1272 ) = ' '
      PCK( 1273 ) = BEGTXT()
      PCK( 1274 ) = ' '
      PCK( 1275 ) = ' '
      PCK( 1276 ) = '     Janus'
      PCK( 1277 ) = ' '
      PCK( 1278 ) = ' '
      PCK( 1279 ) = '        Old values:'
      PCK( 1280 ) = ' '
      PCK( 1281 ) = '           Values are unchanged in the 2' &
      //            '009 IAU report.'
      PCK( 1282 ) = ' '
      PCK( 1283 ) = ' '
      PCK( 1284 ) = '        Current values:'
      PCK( 1285 ) = ' '
      PCK( 1286 ) = '           Data values are unchanged in ' &
      //            'the 2009 IAU report. However'
      PCK( 1287 ) = '           the kernel variable contents ' &
      //            'have changed due to removal of'
      PCK( 1288 ) = '           the angle S7.'
      PCK( 1289 ) = ' '
      PCK( 1290 ) = BEGDAT()
      PCK( 1291 ) = ' '
      PCK( 1292 ) = '           BODY610_POLE_RA       = (  40' &
      //            '.58    -0.036       0. )'
      PCK( 1293 ) = '           BODY610_POLE_DEC      = (  83' &
      //            '.52    -0.004       0. )'
      PCK( 1294 ) = '           BODY610_PM            = (  58' &
      //            '.83   518.2359876   0. )'
      PCK( 1295 ) = '           BODY610_LONG_AXIS     = (   0' &
      //            '.                      )'
      PCK( 1296 ) = ' '
      PCK( 1297 ) = '           BODY610_NUT_PREC_RA   = ( 0. ' &
      //            '-1.623  0. 0. 0. 0. 0.  0.023 )'
      PCK( 1298 ) = '           BODY610_NUT_PREC_DEC  = ( 0. ' &
      //            '-0.183  0. 0. 0. 0. 0.  0.001 )'
      PCK( 1299 ) = '           BODY610_NUT_PREC_PM   = ( 0. ' &
      //            ' 1.613  0. 0. 0. 0. 0. -0.023 )'
      PCK( 1300 ) = ' '
      PCK( 1301 ) = BEGTXT()
      PCK( 1302 ) = ' '
      PCK( 1303 ) = ' '
      PCK( 1304 ) = ' '
      PCK( 1305 ) = '     Epimetheus'
      PCK( 1306 ) = ' '
      PCK( 1307 ) = ' '
      PCK( 1308 ) = '        Old values:'
      PCK( 1309 ) = ' '
      PCK( 1310 ) = '           Values are unchanged in the 2' &
      //            '009 IAU report.'
      PCK( 1311 ) = ' '
      PCK( 1312 ) = '        Current values:'
      PCK( 1313 ) = ' '
      PCK( 1314 ) = '           Data values are unchanged in ' &
      //            'the 2009 IAU report. However'
      PCK( 1315 ) = '           the kernel variable contents ' &
      //            'have changed due to removal of'
      PCK( 1316 ) = '           the angle S7.'
      PCK( 1317 ) = ' '
      PCK( 1318 ) = BEGDAT()
      PCK( 1319 ) = ' '
      PCK( 1320 ) = '           BODY611_POLE_RA       = (  40' &
      //            '.58    -0.036        0. )'
      PCK( 1321 ) = '           BODY611_POLE_DEC      = (  83' &
      //            '.52    -0.004        0. )'
      PCK( 1322 ) = '           BODY611_PM            = ( 293' &
      //            '.87   518.4907239    0. )'
      PCK( 1323 ) = '           BODY611_LONG_AXIS     = (   0' &
      //            '.                       )'
      PCK( 1324 ) = ' '
      PCK( 1325 ) = '           BODY611_NUT_PREC_RA   = ( -3.' &
      //            '153   0. 0. 0. 0. 0.   0.086  0. )'
      PCK( 1326 ) = '           BODY611_NUT_PREC_DEC  = ( -0.' &
      //            '356   0. 0. 0. 0. 0.   0.005  0. )'
      PCK( 1327 ) = '           BODY611_NUT_PREC_PM   = (  3.' &
      //            '133   0. 0. 0. 0. 0.  -0.086  0. )'
      PCK( 1328 ) = ' '
      PCK( 1329 ) = BEGTXT()
      PCK( 1330 ) = ' '
      PCK( 1331 ) = ' '
      PCK( 1332 ) = ' '
      PCK( 1333 ) = '     Helene'
      PCK( 1334 ) = ' '
      PCK( 1335 ) = ' '
      PCK( 1336 ) = '        Old values:'
      PCK( 1337 ) = ' '
      PCK( 1338 ) = '           Values are unchanged in the 2' &
      //            '009 IAU report.'
      PCK( 1339 ) = ' '
      PCK( 1340 ) = '        Current values:'
      PCK( 1341 ) = ' '
      PCK( 1342 ) = BEGDAT()
      PCK( 1343 ) = ' '
      PCK( 1344 ) = '           BODY612_POLE_RA       = (  40' &
      //            '.85     -0.036        0. )'
      PCK( 1345 ) = '           BODY612_POLE_DEC      = (  83' &
      //            '.34     -0.004        0. )'
      PCK( 1346 ) = '           BODY612_PM            = ( 245' &
      //            '.12    131.6174056    0. )'
      PCK( 1347 ) = '           BODY612_LONG_AXIS     = (   0' &
      //            '.                        )'
      PCK( 1348 ) = ' '
      PCK( 1349 ) = BEGTXT()
      PCK( 1350 ) = ' '
      PCK( 1351 ) = ' '
      PCK( 1352 ) = ' '
      PCK( 1353 ) = '     Telesto'
      PCK( 1354 ) = ' '
      PCK( 1355 ) = ' '
      PCK( 1356 ) = '        Old values:'
      PCK( 1357 ) = ' '
      PCK( 1358 ) = '           Values are unchanged in the 2' &
      //            '009 IAU report.'
      PCK( 1359 ) = ' '
      PCK( 1360 ) = '        Current values:'
      PCK( 1361 ) = ' '
      PCK( 1362 ) = BEGDAT()
      PCK( 1363 ) = ' '
      PCK( 1364 ) = '           BODY613_POLE_RA       = ( 50.' &
      //            '51    -0.036      0.  )'
      PCK( 1365 ) = '           BODY613_POLE_DEC      = ( 84.' &
      //            '06    -0.004      0.  )'
      PCK( 1366 ) = '           BODY613_PM            = ( 56.' &
      //            '88   190.6979332  0.  )'
      PCK( 1367 ) = '           BODY613_LONG_AXIS     = (  0.' &
      //            '                      )'
      PCK( 1368 ) = ' '
      PCK( 1369 ) = BEGTXT()
      PCK( 1370 ) = ' '
      PCK( 1371 ) = ' '
      PCK( 1372 ) = ' '
      PCK( 1373 ) = '     Calypso'
      PCK( 1374 ) = ' '
      PCK( 1375 ) = ' '
      PCK( 1376 ) = '        Old values:'
      PCK( 1377 ) = ' '
      PCK( 1378 ) = '           Values are unchanged in the 2' &
      //            '009 IAU report.'
      PCK( 1379 ) = ' '
      PCK( 1380 ) = '        Current values:'
      PCK( 1381 ) = ' '
      PCK( 1382 ) = BEGDAT()
      PCK( 1383 ) = ' '
      PCK( 1384 ) = '           BODY614_POLE_RA       = (   3' &
      //            '6.41    -0.036        0.  )'
      PCK( 1385 ) = '           BODY614_POLE_DEC      = (   8' &
      //            '5.04    -0.004        0.  )'
      PCK( 1386 ) = '           BODY614_PM            = (  15' &
      //            '3.51   190.6742373    0.  )'
      PCK( 1387 ) = '           BODY614_LONG_AXIS     = (    ' &
      //            '0.                        )'
      PCK( 1388 ) = ' '
      PCK( 1389 ) = BEGTXT()
      PCK( 1390 ) = ' '
      PCK( 1391 ) = ' '
      PCK( 1392 ) = ' '
      PCK( 1393 ) = '     Atlas'
      PCK( 1394 ) = ' '
      PCK( 1395 ) = ' '
      PCK( 1396 ) = '        Old values:'
      PCK( 1397 ) = ' '
      PCK( 1398 ) = '           Values are unchanged in the 2' &
      //            '009 IAU report.'
      PCK( 1399 ) = ' '
      PCK( 1400 ) = '        Current values:'
      PCK( 1401 ) = ' '
      PCK( 1402 ) = BEGDAT()
      PCK( 1403 ) = ' '
      PCK( 1404 ) = '           BODY615_POLE_RA       = (   4' &
      //            '0.58     -0.036      0. )'
      PCK( 1405 ) = '           BODY615_POLE_DEC      = (   8' &
      //            '3.53     -0.004      0. )'
      PCK( 1406 ) = '           BODY615_PM            = (  13' &
      //            '7.88    598.3060000  0. )'
      PCK( 1407 ) = '           BODY615_LONG_AXIS     = (    ' &
      //            '0.                      )'
      PCK( 1408 ) = ' '
      PCK( 1409 ) = BEGTXT()
      PCK( 1410 ) = ' '
      PCK( 1411 ) = ' '
      PCK( 1412 ) = ' '
      PCK( 1413 ) = '     Prometheus'
      PCK( 1414 ) = ' '
      PCK( 1415 ) = ' '
      PCK( 1416 ) = '        Old values:'
      PCK( 1417 ) = ' '
      PCK( 1418 ) = '           Values are unchanged in the 2' &
      //            '009 IAU report.'
      PCK( 1419 ) = ' '
      PCK( 1420 ) = '        Current values:'
      PCK( 1421 ) = ' '
      PCK( 1422 ) = BEGDAT()
      PCK( 1423 ) = ' '
      PCK( 1424 ) = '           BODY616_POLE_RA       = (  40' &
      //            '.58      -0.036    )'
      PCK( 1425 ) = '           BODY616_POLE_DEC      = (  83' &
      //            '.53      -0.004    )'
      PCK( 1426 ) = '           BODY616_PM            = ( 296' &
      //            '.14     587.289000 )'
      PCK( 1427 ) = '           BODY616_LONG_AXIS     = (   0' &
      //            '.                  )'
      PCK( 1428 ) = ' '
      PCK( 1429 ) = BEGTXT()
      PCK( 1430 ) = ' '
      PCK( 1431 ) = ' '
      PCK( 1432 ) = ' '
      PCK( 1433 ) = '     Pandora'
      PCK( 1434 ) = ' '
      PCK( 1435 ) = ' '
      PCK( 1436 ) = '        Old values:'
      PCK( 1437 ) = ' '
      PCK( 1438 ) = '           Values are unchanged in the 2' &
      //            '009 IAU report.'
      PCK( 1439 ) = ' '
      PCK( 1440 ) = '        Current values:'
      PCK( 1441 ) = ' '
      PCK( 1442 ) = BEGDAT()
      PCK( 1443 ) = ' '
      PCK( 1444 ) = '           BODY617_POLE_RA       = (   4' &
      //            '0.58     -0.036      0.  )'
      PCK( 1445 ) = '           BODY617_POLE_DEC      = (   8' &
      //            '3.53     -0.004      0.  )'
      PCK( 1446 ) = '           BODY617_PM            = (  16' &
      //            '2.92    572.7891000  0.  )'
      PCK( 1447 ) = '           BODY617_LONG_AXIS     = (    ' &
      //            ' 0.                      )'
      PCK( 1448 ) = ' '
      PCK( 1449 ) = BEGTXT()
      PCK( 1450 ) = ' '
      PCK( 1451 ) = ' '
      PCK( 1452 ) = ' '
      PCK( 1453 ) = '     Pan'
      PCK( 1454 ) = ' '
      PCK( 1455 ) = ' '
      PCK( 1456 ) = '        Old values:'
      PCK( 1457 ) = ' '
      PCK( 1458 ) = '           Values are unchanged in the 2' &
      //            '009 IAU report.'
      PCK( 1459 ) = ' '
      PCK( 1460 ) = '        Current values:'
      PCK( 1461 ) = ' '
      PCK( 1462 ) = BEGDAT()
      PCK( 1463 ) = ' '
      PCK( 1464 ) = '           BODY618_POLE_RA       = (   4' &
      //            '0.6     -0.036       0. )'
      PCK( 1465 ) = '           BODY618_POLE_DEC      = (   8' &
      //            '3.5     -0.004       0. )'
      PCK( 1466 ) = '           BODY618_PM            = (   4' &
      //            '8.8    626.0440000   0. )'
      PCK( 1467 ) = '           BODY618_LONG_AXIS     = (    ' &
      //            '0.                      )'
      PCK( 1468 ) = ' '
      PCK( 1469 ) = BEGTXT()
      PCK( 1470 ) = ' '
      PCK( 1471 ) = ' '
      PCK( 1472 ) = ' '
      PCK( 1473 ) = ' '
      PCK( 1474 ) = ' '
      PCK( 1475 ) = 'Satellites of Uranus'
      PCK( 1476 ) = ' '
      PCK( 1477 ) = ' '
      PCK( 1478 ) = ' '
      PCK( 1479 ) = '     Ariel'
      PCK( 1480 ) = ' '
      PCK( 1481 ) = '        Old values:'
      PCK( 1482 ) = ' '
      PCK( 1483 ) = '           Values are unchanged in the 2' &
      //            '009 IAU report.'
      PCK( 1484 ) = ' '
      PCK( 1485 ) = '        Current values:'
      PCK( 1486 ) = ' '
      PCK( 1487 ) = BEGDAT()
      PCK( 1488 ) = ' '
      PCK( 1489 ) = '           BODY701_POLE_RA       = ( 257' &
      //            '.43     0.          0. )'
      PCK( 1490 ) = '           BODY701_POLE_DEC      = ( -15' &
      //            '.10     0.          0. )'
      PCK( 1491 ) = '           BODY701_PM            = ( 156' &
      //            '.22  -142.8356681   0. )'
      PCK( 1492 ) = '           BODY701_LONG_AXIS     = (   0' &
      //            '.                      )'
      PCK( 1493 ) = ' '
      PCK( 1494 ) = '           BODY701_NUT_PREC_RA   = (  0.' &
      //            ' 0. 0. 0. 0.'
      PCK( 1495 ) = '                                      0.' &
      //            ' 0. 0. 0. 0.  0.    0.    0.29 )'
      PCK( 1496 ) = ' '
      PCK( 1497 ) = '           BODY701_NUT_PREC_DEC  = (  0.' &
      //            ' 0. 0. 0. 0.'
      PCK( 1498 ) = '                                      0.' &
      //            ' 0. 0. 0. 0.  0.    0.    0.28 )'
      PCK( 1499 ) = ' '
      PCK( 1500 ) = '           BODY701_NUT_PREC_PM   = (  0.' &
      //            ' 0. 0. 0. 0.'
      PCK( 1501 ) = '                                      0.' &
      //            ' 0. 0. 0. 0.  0.   0.05   0.08 )'
      PCK( 1502 ) = BEGTXT()
      PCK( 1503 ) = ' '
      PCK( 1504 ) = ' '
      PCK( 1505 ) = ' '
      PCK( 1506 ) = '     Umbriel'
      PCK( 1507 ) = ' '
      PCK( 1508 ) = '        Old values:'
      PCK( 1509 ) = ' '
      PCK( 1510 ) = '           Values are unchanged in the 2' &
      //            '009 IAU report.'
      PCK( 1511 ) = ' '
      PCK( 1512 ) = '        Current values:'
      PCK( 1513 ) = ' '
      PCK( 1514 ) = BEGDAT()
      PCK( 1515 ) = ' '
      PCK( 1516 ) = '           BODY702_POLE_RA       = (  25' &
      //            '7.43     0.          0. )'
      PCK( 1517 ) = '           BODY702_POLE_DEC      = (  -1' &
      //            '5.10     0.          0. )'
      PCK( 1518 ) = '           BODY702_PM            = (  10' &
      //            '8.05   -86.8688923   0. )'
      PCK( 1519 ) = '           BODY702_LONG_AXIS     = (    ' &
      //            '0.                      )'
      PCK( 1520 ) = ' '
      PCK( 1521 ) = '           BODY702_NUT_PREC_RA   = ( 0. ' &
      //            '0. 0. 0. 0.'
      PCK( 1522 ) = '                                     0. ' &
      //            '0. 0. 0. 0.   0.   0.    0.   0.21 )'
      PCK( 1523 ) = ' '
      PCK( 1524 ) = '           BODY702_NUT_PREC_DEC  = ( 0. ' &
      //            '0. 0. 0. 0.'
      PCK( 1525 ) = '                                     0. ' &
      //            '0. 0. 0. 0.   0.   0.    0.   0.20 )'
      PCK( 1526 ) = ' '
      PCK( 1527 ) = '           BODY702_NUT_PREC_PM   = ( 0. ' &
      //            '0. 0. 0. 0.'
      PCK( 1528 ) = '                                     0. ' &
      //            '0. 0. 0. 0.   0.  -0.09  0.   0.06 )'
      PCK( 1529 ) = ' '
      PCK( 1530 ) = BEGTXT()
      PCK( 1531 ) = ' '
      PCK( 1532 ) = ' '
      PCK( 1533 ) = ' '
      PCK( 1534 ) = '     Titania'
      PCK( 1535 ) = ' '
      PCK( 1536 ) = '        Old values:'
      PCK( 1537 ) = ' '
      PCK( 1538 ) = '           Values are unchanged in the 2' &
      //            '009 IAU report.'
      PCK( 1539 ) = ' '
      PCK( 1540 ) = '        Current values:'
      PCK( 1541 ) = ' '
      PCK( 1542 ) = BEGDAT()
      PCK( 1543 ) = ' '
      PCK( 1544 ) = '           BODY703_POLE_RA       = (  25' &
      //            '7.43    0.          0. )'
      PCK( 1545 ) = '           BODY703_POLE_DEC      = (  -1' &
      //            '5.10    0.          0. )'
      PCK( 1546 ) = '           BODY703_PM            = (   7' &
      //            '7.74  -41.3514316   0. )'
      PCK( 1547 ) = '           BODY703_LONG_AXIS     = (    ' &
      //            '0.                     )'
      PCK( 1548 ) = ' '
      PCK( 1549 ) = '           BODY703_NUT_PREC_RA   = ( 0. ' &
      //            '0. 0. 0. 0.'
      PCK( 1550 ) = '                                     0. ' &
      //            '0. 0. 0. 0.   0. 0. 0. 0.   0.29 )'
      PCK( 1551 ) = ' '
      PCK( 1552 ) = '           BODY703_NUT_PREC_DEC  = ( 0. ' &
      //            '0. 0. 0. 0.'
      PCK( 1553 ) = '                                     0. ' &
      //            '0. 0. 0. 0.   0. 0. 0. 0.   0.28 )'
      PCK( 1554 ) = ' '
      PCK( 1555 ) = '           BODY703_NUT_PREC_PM   = ( 0. ' &
      //            '0. 0. 0. 0.'
      PCK( 1556 ) = '                                     0. ' &
      //            '0. 0. 0. 0.   0. 0. 0. 0.   0.08 )'
      PCK( 1557 ) = BEGTXT()
      PCK( 1558 ) = ' '
      PCK( 1559 ) = ' '
      PCK( 1560 ) = ' '
      PCK( 1561 ) = '     Oberon'
      PCK( 1562 ) = ' '
      PCK( 1563 ) = '        Old values:'
      PCK( 1564 ) = ' '
      PCK( 1565 ) = '           Values are unchanged in the 2' &
      //            '009 IAU report.'
      PCK( 1566 ) = ' '
      PCK( 1567 ) = '        Current values:'
      PCK( 1568 ) = ' '
      PCK( 1569 ) = BEGDAT()
      PCK( 1570 ) = ' '
      PCK( 1571 ) = '           BODY704_POLE_RA       = (  25' &
      //            '7.43    0.          0. )'
      PCK( 1572 ) = '           BODY704_POLE_DEC      = (  -1' &
      //            '5.10    0.          0. )'
      PCK( 1573 ) = '           BODY704_PM            = (    ' &
      //            '6.77  -26.7394932   0. )'
      PCK( 1574 ) = '           BODY704_LONG_AXIS     = (    ' &
      //            '0.                     )'
      PCK( 1575 ) = ' '
      PCK( 1576 ) = ' '
      PCK( 1577 ) = '           BODY704_NUT_PREC_RA   = ( 0. ' &
      //            '0. 0. 0. 0.'
      PCK( 1578 ) = '                                     0. ' &
      //            '0. 0. 0. 0.'
      PCK( 1579 ) = '                                     0. ' &
      //            '0. 0. 0. 0.   0.16 )'
      PCK( 1580 ) = ' '
      PCK( 1581 ) = '           BODY704_NUT_PREC_DEC  = ( 0. ' &
      //            '0. 0. 0. 0.'
      PCK( 1582 ) = '                                     0. ' &
      //            '0. 0. 0. 0.'
      PCK( 1583 ) = '                                     0. ' &
      //            '0. 0. 0. 0.   0.16 )'
      PCK( 1584 ) = ' '
      PCK( 1585 ) = '           BODY704_NUT_PREC_PM   = ( 0. ' &
      //            '0. 0. 0. 0.'
      PCK( 1586 ) = '                                     0. ' &
      //            '0. 0. 0. 0.'
      PCK( 1587 ) = '                                     0. ' &
      //            '0. 0. 0. 0.   0.04 )'
      PCK( 1588 ) = BEGTXT()
      PCK( 1589 ) = ' '
      PCK( 1590 ) = ' '
      PCK( 1591 ) = ' '
      PCK( 1592 ) = '     Miranda'
      PCK( 1593 ) = ' '
      PCK( 1594 ) = '        Old values:'
      PCK( 1595 ) = ' '
      PCK( 1596 ) = '           Values are unchanged in the 2' &
      //            '009 IAU report.'
      PCK( 1597 ) = ' '
      PCK( 1598 ) = '        Current values:'
      PCK( 1599 ) = ' '
      PCK( 1600 ) = BEGDAT()
      PCK( 1601 ) = ' '
      PCK( 1602 ) = '           BODY705_POLE_RA      = (  257' &
      //            '.43     0.         0. )'
      PCK( 1603 ) = '           BODY705_POLE_DEC     = (  -15' &
      //            '.08     0.         0. )'
      PCK( 1604 ) = '           BODY705_PM           = (   30' &
      //            '.70  -254.6906892  0. )'
      PCK( 1605 ) = '           BODY705_LONG_AXIS    = (    0' &
      //            '.                     )'
      PCK( 1606 ) = ' '
      PCK( 1607 ) = '           BODY705_NUT_PREC_RA  = ( 0.  ' &
      //            '   0.     0.    0.    0.'
      PCK( 1608 ) = '                                    0.  ' &
      //            '   0.     0.    0.    0.'
      PCK( 1609 ) = '                                    4.41' &
      //            '   0.     0.    0.    0.'
      PCK( 1610 ) = '                                    0.  ' &
      //            '  -0.04   0.             )'
      PCK( 1611 ) = ' '
      PCK( 1612 ) = '           BODY705_NUT_PREC_DEC = ( 0.  ' &
      //            '   0.     0.    0.    0.'
      PCK( 1613 ) = '                                    0.  ' &
      //            '   0.     0.    0.    0.'
      PCK( 1614 ) = '                                    4.25' &
      //            '   0.     0.    0.    0.'
      PCK( 1615 ) = '                                    0.  ' &
      //            '  -0.02   0.             )'
      PCK( 1616 ) = ' '
      PCK( 1617 ) = '           BODY705_NUT_PREC_PM  = ( 0.  ' &
      //            '   0.     0.    0.    0.'
      PCK( 1618 ) = '                                    0.  ' &
      //            '   0.     0.    0.    0.'
      PCK( 1619 ) = '                                    1.15' &
      //            '  -1.27   0.    0.    0.'
      PCK( 1620 ) = '                                    0.  ' &
      //            '  -0.09   0.15           )'
      PCK( 1621 ) = BEGTXT()
      PCK( 1622 ) = ' '
      PCK( 1623 ) = ' '
      PCK( 1624 ) = ' '
      PCK( 1625 ) = '     Cordelia'
      PCK( 1626 ) = ' '
      PCK( 1627 ) = '        Old values:'
      PCK( 1628 ) = ' '
      PCK( 1629 ) = '           Values are unchanged in the 2' &
      //            '009 IAU report.'
      PCK( 1630 ) = ' '
      PCK( 1631 ) = '        Current values:'
      PCK( 1632 ) = ' '
      PCK( 1633 ) = BEGDAT()
      PCK( 1634 ) = ' '
      PCK( 1635 ) = '           BODY706_POLE_RA      = (   25' &
      //            '7.31      0.         0.  )'
      PCK( 1636 ) = '           BODY706_POLE_DEC     = (   -1' &
      //            '5.18      0.         0.  )'
      PCK( 1637 ) = '           BODY706_PM           = (   12' &
      //            '7.69  -1074.5205730  0.  )'
      PCK( 1638 ) = '           BODY706_LONG_AXIS    = (     ' &
      //            '0.                       )'
      PCK( 1639 ) = ' '
      PCK( 1640 ) = '           BODY706_NUT_PREC_RA  = (   -0' &
      //            '.15    0.     0.    0.    0.'
      PCK( 1641 ) = '                                       0' &
      //            '.      0.     0.    0.    0.'
      PCK( 1642 ) = '                                       0' &
      //            '.      0.     0.    0.    0.'
      PCK( 1643 ) = '                                       0' &
      //            '.      0.     0.             )'
      PCK( 1644 ) = ' '
      PCK( 1645 ) = '           BODY706_NUT_PREC_DEC = (    0' &
      //            '.14    0.     0.    0.    0.'
      PCK( 1646 ) = '                                       0' &
      //            '.      0.     0.    0.    0.'
      PCK( 1647 ) = '                                       0' &
      //            '.      0.     0.    0.    0.'
      PCK( 1648 ) = '                                       0' &
      //            '.      0.     0.             )'
      PCK( 1649 ) = ' '
      PCK( 1650 ) = '           BODY706_NUT_PREC_PM  = (   -0' &
      //            '.04    0.     0.    0.    0.'
      PCK( 1651 ) = '                                       0' &
      //            '.      0.     0.    0.    0.'
      PCK( 1652 ) = '                                       0' &
      //            '.      0.     0.    0.    0.'
      PCK( 1653 ) = '                                       0' &
      //            '.      0.     0.             )'
      PCK( 1654 ) = ' '
      PCK( 1655 ) = BEGTXT()
      PCK( 1656 ) = ' '
      PCK( 1657 ) = ' '
      PCK( 1658 ) = ' '
      PCK( 1659 ) = '     Ophelia'
      PCK( 1660 ) = ' '
      PCK( 1661 ) = ' '
      PCK( 1662 ) = '        Old values:'
      PCK( 1663 ) = ' '
      PCK( 1664 ) = '           Values are unchanged in the 2' &
      //            '009 IAU report.'
      PCK( 1665 ) = ' '
      PCK( 1666 ) = '        Current values:'
      PCK( 1667 ) = ' '
      PCK( 1668 ) = BEGDAT()
      PCK( 1669 ) = ' '
      PCK( 1670 ) = '           BODY707_POLE_RA      = (  257' &
      //            '.31     0.         0. )'
      PCK( 1671 ) = '           BODY707_POLE_DEC     = (  -15' &
      //            '.18     0.         0. )'
      PCK( 1672 ) = '           BODY707_PM           = (  130' &
      //            '.35  -956.4068150  0. )'
      PCK( 1673 ) = '           BODY707_LONG_AXIS    = (    0' &
      //            '.                     )'
      PCK( 1674 ) = ' '
      PCK( 1675 ) = '           BODY707_NUT_PREC_RA  = (    0' &
      //            '.     -0.09   0.    0.    0.'
      PCK( 1676 ) = '                                       0' &
      //            '.      0.     0.    0.    0.'
      PCK( 1677 ) = '                                       0' &
      //            '.      0.     0.    0.    0.'
      PCK( 1678 ) = '                                       0' &
      //            '.      0.     0.             )'
      PCK( 1679 ) = ' '
      PCK( 1680 ) = '           BODY707_NUT_PREC_DEC = (    0' &
      //            '.      0.09   0.    0.    0.'
      PCK( 1681 ) = '                                       0' &
      //            '.      0.     0.    0.    0.'
      PCK( 1682 ) = '                                       0' &
      //            '.      0.     0.    0.    0.'
      PCK( 1683 ) = '                                       0' &
      //            '.      0.     0.             )'
      PCK( 1684 ) = ' '
      PCK( 1685 ) = '           BODY707_NUT_PREC_PM  = (    0' &
      //            '.     -0.03   0.    0.    0.'
      PCK( 1686 ) = '                                       0' &
      //            '.      0.     0.    0.    0.'
      PCK( 1687 ) = '                                       0' &
      //            '.      0.     0.    0.    0.'
      PCK( 1688 ) = '                                       0' &
      //            '.      0.     0.             )'
      PCK( 1689 ) = ' '
      PCK( 1690 ) = BEGTXT()
      PCK( 1691 ) = ' '
      PCK( 1692 ) = ' '
      PCK( 1693 ) = ' '
      PCK( 1694 ) = '     Bianca'
      PCK( 1695 ) = ' '
      PCK( 1696 ) = '        Old values:'
      PCK( 1697 ) = ' '
      PCK( 1698 ) = '           Values are unchanged in the 2' &
      //            '009 IAU report.'
      PCK( 1699 ) = ' '
      PCK( 1700 ) = '        Current values:'
      PCK( 1701 ) = ' '
      PCK( 1702 ) = BEGDAT()
      PCK( 1703 ) = ' '
      PCK( 1704 ) = '           BODY708_POLE_RA      = (  257' &
      //            '.31     0.         0.  )'
      PCK( 1705 ) = '           BODY708_POLE_DEC     = (  -15' &
      //            '.18     0.         0.  )'
      PCK( 1706 ) = '           BODY708_PM           = (  105' &
      //            '.46  -828.3914760  0.  )'
      PCK( 1707 ) = '           BODY708_LONG_AXIS    = (    0' &
      //            '.                      )'
      PCK( 1708 ) = ' '
      PCK( 1709 ) = '           BODY708_NUT_PREC_RA  = (    0' &
      //            '.      0.    -0.16    0.    0.'
      PCK( 1710 ) = '                                       0' &
      //            '.      0.     0.      0.    0.'
      PCK( 1711 ) = '                                       0' &
      //            '.      0.     0.      0.    0.'
      PCK( 1712 ) = '                                       0' &
      //            '.      0.     0.               )'
      PCK( 1713 ) = ' '
      PCK( 1714 ) = '           BODY708_NUT_PREC_DEC = (    0' &
      //            '.      0.     0.16    0.    0.'
      PCK( 1715 ) = '                                       0' &
      //            '.      0.     0.      0.    0.'
      PCK( 1716 ) = '                                       0' &
      //            '.      0.     0.      0.    0.'
      PCK( 1717 ) = '                                       0' &
      //            '.      0.     0.               )'
      PCK( 1718 ) = ' '
      PCK( 1719 ) = '           BODY708_NUT_PREC_PM  = (    0' &
      //            '.      0.    -0.04    0.    0.'
      PCK( 1720 ) = '                                       0' &
      //            '.      0.     0.      0.    0.'
      PCK( 1721 ) = '                                       0' &
      //            '.      0.     0.      0.    0.'
      PCK( 1722 ) = '                                       0' &
      //            '.      0.     0.               )'
      PCK( 1723 ) = ' '
      PCK( 1724 ) = BEGTXT()
      PCK( 1725 ) = ' '
      PCK( 1726 ) = ' '
      PCK( 1727 ) = ' '
      PCK( 1728 ) = '     Cressida'
      PCK( 1729 ) = ' '
      PCK( 1730 ) = '        Old values:'
      PCK( 1731 ) = ' '
      PCK( 1732 ) = '           Values are unchanged in the 2' &
      //            '009 IAU report.'
      PCK( 1733 ) = ' '
      PCK( 1734 ) = '        Current values:'
      PCK( 1735 ) = ' '
      PCK( 1736 ) = BEGDAT()
      PCK( 1737 ) = ' '
      PCK( 1738 ) = ' '
      PCK( 1739 ) = '           BODY709_POLE_RA      = (  257' &
      //            '.31      0.          0.  )'
      PCK( 1740 ) = '           BODY709_POLE_DEC     = (  -15' &
      //            '.18      0.          0.  )'
      PCK( 1741 ) = '           BODY709_PM           = (   59' &
      //            '.16   -776.5816320   0.  )'
      PCK( 1742 ) = '           BODY709_LONG_AXIS    = (    0' &
      //            '.                        )'
      PCK( 1743 ) = ' '
      PCK( 1744 ) = ' '
      PCK( 1745 ) = '           BODY709_NUT_PREC_RA  = (    0' &
      //            '.      0.     0.     -0.04   0.'
      PCK( 1746 ) = '                                       0' &
      //            '.      0.     0.      0.     0.'
      PCK( 1747 ) = '                                       0' &
      //            '.      0.     0.      0.     0.'
      PCK( 1748 ) = '                                       0' &
      //            '.      0.     0.                )'
      PCK( 1749 ) = ' '
      PCK( 1750 ) = ' '
      PCK( 1751 ) = '           BODY709_NUT_PREC_DEC = (    0' &
      //            '.      0.     0.      0.04   0.'
      PCK( 1752 ) = '                                       0' &
      //            '.      0.     0.      0.     0.'
      PCK( 1753 ) = '                                       0' &
      //            '.      0.     0.      0.     0.'
      PCK( 1754 ) = '                                       0' &
      //            '.      0.     0.                )'
      PCK( 1755 ) = ' '
      PCK( 1756 ) = ' '
      PCK( 1757 ) = '           BODY709_NUT_PREC_PM  = (    0' &
      //            '.      0.     0.     -0.01   0.'
      PCK( 1758 ) = '                                       0' &
      //            '.      0.     0.      0.     0.'
      PCK( 1759 ) = '                                       0' &
      //            '.      0.     0.      0.     0.'
      PCK( 1760 ) = '                                       0' &
      //            '.      0.     0.                )'
      PCK( 1761 ) = ' '
      PCK( 1762 ) = ' '
      PCK( 1763 ) = BEGTXT()
      PCK( 1764 ) = ' '
      PCK( 1765 ) = ' '
      PCK( 1766 ) = ' '
      PCK( 1767 ) = '     Desdemona'
      PCK( 1768 ) = ' '
      PCK( 1769 ) = '        Old values:'
      PCK( 1770 ) = ' '
      PCK( 1771 ) = '           Values are unchanged in the 2' &
      //            '009 IAU report.'
      PCK( 1772 ) = ' '
      PCK( 1773 ) = '        Current values:'
      PCK( 1774 ) = ' '
      PCK( 1775 ) = BEGDAT()
      PCK( 1776 ) = ' '
      PCK( 1777 ) = '           BODY710_POLE_RA      = ( 257.' &
      //            '31      0.           0.  )'
      PCK( 1778 ) = '           BODY710_POLE_DEC     = ( -15.' &
      //            '18      0.           0.  )'
      PCK( 1779 ) = '           BODY710_PM           = (  95.' &
      //            '08   -760.0531690    0.  )'
      PCK( 1780 ) = '           BODY710_LONG_AXIS    = (   0.' &
      //            '                         )'
      PCK( 1781 ) = ' '
      PCK( 1782 ) = '           BODY710_NUT_PREC_RA  = (   0.' &
      //            '      0.     0.      0.    -0.17'
      PCK( 1783 ) = '                                      0.' &
      //            '      0.     0.      0.     0.'
      PCK( 1784 ) = '                                      0.' &
      //            '      0.     0.      0.     0.'
      PCK( 1785 ) = '                                      0.' &
      //            '      0.     0.                  )'
      PCK( 1786 ) = ' '
      PCK( 1787 ) = '           BODY710_NUT_PREC_DEC = (   0.' &
      //            '      0.     0.      0.     0.16'
      PCK( 1788 ) = '                                      0.' &
      //            '      0.     0.      0.     0.'
      PCK( 1789 ) = '                                      0.' &
      //            '      0.     0.      0.     0.'
      PCK( 1790 ) = '                                      0.' &
      //            '      0.     0.                  )'
      PCK( 1791 ) = ' '
      PCK( 1792 ) = '           BODY710_NUT_PREC_PM  = (   0.' &
      //            '      0.     0.      0.    -0.04'
      PCK( 1793 ) = '                                      0.' &
      //            '      0.     0.      0.     0.'
      PCK( 1794 ) = '                                      0.' &
      //            '      0.     0.      0.     0.'
      PCK( 1795 ) = '                                      0.' &
      //            '      0.     0.                 )'
      PCK( 1796 ) = ' '
      PCK( 1797 ) = BEGTXT()
      PCK( 1798 ) = ' '
      PCK( 1799 ) = ' '
      PCK( 1800 ) = ' '
      PCK( 1801 ) = '     Juliet'
      PCK( 1802 ) = ' '
      PCK( 1803 ) = '        Old values:'
      PCK( 1804 ) = ' '
      PCK( 1805 ) = '           Values are unchanged in the 2' &
      //            '009 IAU report.'
      PCK( 1806 ) = ' '
      PCK( 1807 ) = '        Current values:'
      PCK( 1808 ) = ' '
      PCK( 1809 ) = BEGDAT()
      PCK( 1810 ) = ' '
      PCK( 1811 ) = '           BODY711_POLE_RA      = (  257' &
      //            '.31     0.           0.   )'
      PCK( 1812 ) = '           BODY711_POLE_DEC     = (  -15' &
      //            '.18     0.           0.   )'
      PCK( 1813 ) = '           BODY711_PM           = (  302' &
      //            '.56  -730.1253660    0.   )'
      PCK( 1814 ) = '           BODY711_LONG_AXIS    = (    0' &
      //            '.                         )'
      PCK( 1815 ) = ' '
      PCK( 1816 ) = '           BODY711_NUT_PREC_RA  = (   0.' &
      //            '      0.     0.      0.     0.'
      PCK( 1817 ) = '                                     -0.' &
      //            '06    0.     0.      0.     0.'
      PCK( 1818 ) = '                                      0.' &
      //            '      0.     0.      0.     0.'
      PCK( 1819 ) = '                                      0.' &
      //            '      0.     0.                 )'
      PCK( 1820 ) = ' '
      PCK( 1821 ) = '           BODY711_NUT_PREC_DEC = (   0.' &
      //            '      0.     0.      0.     0.'
      PCK( 1822 ) = '                                      0.' &
      //            '06    0.     0.      0.     0.'
      PCK( 1823 ) = '                                      0.' &
      //            '      0.     0.      0.     0.'
      PCK( 1824 ) = '                                      0.' &
      //            '      0.     0.                 )'
      PCK( 1825 ) = ' '
      PCK( 1826 ) = '           BODY711_NUT_PREC_PM  = (   0.' &
      //            '      0.     0.      0.     0.'
      PCK( 1827 ) = '                                     -0.' &
      //            '02    0.     0.      0.     0.'
      PCK( 1828 ) = '                                      0.' &
      //            '      0.     0.      0.     0.'
      PCK( 1829 ) = '                                      0.' &
      //            '      0.     0.                 )'
      PCK( 1830 ) = ' '
      PCK( 1831 ) = BEGTXT()
      PCK( 1832 ) = ' '
      PCK( 1833 ) = ' '
      PCK( 1834 ) = ' '
      PCK( 1835 ) = '     Portia'
      PCK( 1836 ) = ' '
      PCK( 1837 ) = '        Old values:'
      PCK( 1838 ) = ' '
      PCK( 1839 ) = '           Values are unchanged in the 2' &
      //            '009 IAU report.'
      PCK( 1840 ) = ' '
      PCK( 1841 ) = '        Current values:'
      PCK( 1842 ) = ' '
      PCK( 1843 ) = BEGDAT()
      PCK( 1844 ) = ' '
      PCK( 1845 ) = '           BODY712_POLE_RA      = (  257' &
      //            '.31      0.           0.   )'
      PCK( 1846 ) = '           BODY712_POLE_DEC     = (  -15' &
      //            '.18      0.           0.   )'
      PCK( 1847 ) = '           BODY712_PM           = (   25' &
      //            '.03   -701.4865870    0.   )'
      PCK( 1848 ) = '           BODY712_LONG_AXIS    = (    0' &
      //            '.                          )'
      PCK( 1849 ) = ' '
      PCK( 1850 ) = '           BODY712_NUT_PREC_RA  = (   0.' &
      //            '      0.     0.      0.     0.'
      PCK( 1851 ) = '                                      0.' &
      //            '     -0.09   0.      0.     0.'
      PCK( 1852 ) = '                                      0.' &
      //            '      0.     0.      0.     0.'
      PCK( 1853 ) = '                                      0.' &
      //            '      0.     0.                )'
      PCK( 1854 ) = ' '
      PCK( 1855 ) = '           BODY712_NUT_PREC_DEC = (   0.' &
      //            '      0.     0.      0.     0.'
      PCK( 1856 ) = '                                      0.' &
      //            '      0.09   0.      0.     0.'
      PCK( 1857 ) = '                                      0.' &
      //            '      0.     0.      0.     0.'
      PCK( 1858 ) = '                                      0.' &
      //            '      0.     0.               )'
      PCK( 1859 ) = ' '
      PCK( 1860 ) = '           BODY712_NUT_PREC_PM  = (   0.' &
      //            '      0.     0.      0.     0.'
      PCK( 1861 ) = '                                      0.' &
      //            '     -0.02   0.      0.     0.'
      PCK( 1862 ) = '                                      0.' &
      //            '      0.     0.      0.     0.'
      PCK( 1863 ) = '                                      0.' &
      //            '      0.     0.               )'
      PCK( 1864 ) = ' '
      PCK( 1865 ) = BEGTXT()
      PCK( 1866 ) = ' '
      PCK( 1867 ) = ' '
      PCK( 1868 ) = ' '
      PCK( 1869 ) = '     Rosalind'
      PCK( 1870 ) = ' '
      PCK( 1871 ) = '        Old values:'
      PCK( 1872 ) = ' '
      PCK( 1873 ) = '           Values are unchanged in the 2' &
      //            '009 IAU report.'
      PCK( 1874 ) = ' '
      PCK( 1875 ) = '        Current values:'
      PCK( 1876 ) = ' '
      PCK( 1877 ) = BEGDAT()
      PCK( 1878 ) = ' '
      PCK( 1879 ) = '           BODY713_POLE_RA      = ( 257.' &
      //            '31      0.          0.  )'
      PCK( 1880 ) = '           BODY713_POLE_DEC     = ( -15.' &
      //            '18      0.          0.  )'
      PCK( 1881 ) = '           BODY713_PM           = ( 314.' &
      //            '90   -644.6311260   0.  )'
      PCK( 1882 ) = '           BODY713_LONG_AXIS    = (   0.' &
      //            '                        )'
      PCK( 1883 ) = ' '
      PCK( 1884 ) = '           BODY713_NUT_PREC_RA  = (   0.' &
      //            '      0.     0.      0.     0.'
      PCK( 1885 ) = '                                      0.' &
      //            '      0.    -0.29    0.     0.'
      PCK( 1886 ) = '                                      0.' &
      //            '      0.     0.      0.     0.'
      PCK( 1887 ) = '                                      0.' &
      //            '      0.     0.               )'
      PCK( 1888 ) = ' '
      PCK( 1889 ) = '           BODY713_NUT_PREC_DEC = (   0.' &
      //            '      0.     0.      0.     0.'
      PCK( 1890 ) = '                                      0.' &
      //            '      0.     0.28    0.     0.'
      PCK( 1891 ) = '                                      0.' &
      //            '      0.     0.      0.     0.'
      PCK( 1892 ) = '                                      0.' &
      //            '      0.     0.              )'
      PCK( 1893 ) = ' '
      PCK( 1894 ) = '           BODY713_NUT_PREC_PM  = (   0.' &
      //            '      0.     0.      0.     0.'
      PCK( 1895 ) = '                                      0.' &
      //            '      0.    -0.08    0.     0.'
      PCK( 1896 ) = '                                      0.' &
      //            '      0.     0.      0.     0.'
      PCK( 1897 ) = '                                      0.' &
      //            '      0.     0.              )'
      PCK( 1898 ) = ' '
      PCK( 1899 ) = BEGTXT()
      PCK( 1900 ) = ' '
      PCK( 1901 ) = ' '
      PCK( 1902 ) = ' '
      PCK( 1903 ) = '     Belinda'
      PCK( 1904 ) = ' '
      PCK( 1905 ) = '        Old values:'
      PCK( 1906 ) = ' '
      PCK( 1907 ) = '           Values are unchanged in the 2' &
      //            '009 IAU report.'
      PCK( 1908 ) = ' '
      PCK( 1909 ) = '        Current values:'
      PCK( 1910 ) = ' '
      PCK( 1911 ) = BEGDAT()
      PCK( 1912 ) = ' '
      PCK( 1913 ) = '           BODY714_POLE_RA      = (   25' &
      //            '7.31      0.         0. )'
      PCK( 1914 ) = '           BODY714_POLE_DEC     = (   -1' &
      //            '5.18      0.         0. )'
      PCK( 1915 ) = '           BODY714_PM           = (   29' &
      //            '7.46   -577.3628170  0. )'
      PCK( 1916 ) = '           BODY714_LONG_AXIS    = (     ' &
      //            '0.                      )'
      PCK( 1917 ) = ' '
      PCK( 1918 ) = '           BODY714_NUT_PREC_RA  = (   0.' &
      //            '      0.     0.      0.     0.'
      PCK( 1919 ) = '                                      0.' &
      //            '      0.     0.     -0.03   0.'
      PCK( 1920 ) = '                                      0.' &
      //            '      0.     0.      0.     0.'
      PCK( 1921 ) = '                                      0.' &
      //            '      0.     0.                )'
      PCK( 1922 ) = ' '
      PCK( 1923 ) = '           BODY714_NUT_PREC_DEC = (   0.' &
      //            '      0.     0.      0.     0.'
      PCK( 1924 ) = '                                      0.' &
      //            '      0.     0.      0.03   0.'
      PCK( 1925 ) = '                                      0.' &
      //            '      0.     0.      0.     0.'
      PCK( 1926 ) = '                                      0.' &
      //            '      0.     0.                )'
      PCK( 1927 ) = ' '
      PCK( 1928 ) = '           BODY714_NUT_PREC_PM  = (   0.' &
      //            '      0.     0.      0.     0.'
      PCK( 1929 ) = '                                      0.' &
      //            '      0.     0.     -0.01   0.'
      PCK( 1930 ) = '                                      0.' &
      //            '      0.     0.      0.     0.'
      PCK( 1931 ) = '                                      0.' &
      //            '      0.     0.                )'
      PCK( 1932 ) = BEGTXT()
      PCK( 1933 ) = ' '
      PCK( 1934 ) = ' '
      PCK( 1935 ) = ' '
      PCK( 1936 ) = '     Puck'
      PCK( 1937 ) = ' '
      PCK( 1938 ) = '        Old values:'
      PCK( 1939 ) = ' '
      PCK( 1940 ) = '           Values are unchanged in the 2' &
      //            '009 IAU report.'
      PCK( 1941 ) = ' '
      PCK( 1942 ) = '        Current values:'
      PCK( 1943 ) = ' '
      PCK( 1944 ) = BEGDAT()
      PCK( 1945 ) = ' '
      PCK( 1946 ) = '           BODY715_POLE_RA      = (  257' &
      //            '.31      0.         0.  )'
      PCK( 1947 ) = '           BODY715_POLE_DEC     = (  -15' &
      //            '.18      0.         0.  )'
      PCK( 1948 ) = '           BODY715_PM           = (   91' &
      //            '.24   -472.5450690  0.  )'
      PCK( 1949 ) = '           BODY715_LONG_AXIS    = (    0' &
      //            '.                       )'
      PCK( 1950 ) = ' '
      PCK( 1951 ) = '           BODY715_NUT_PREC_RA  = (   0.' &
      //            '      0.     0.      0.     0.'
      PCK( 1952 ) = '                                      0.' &
      //            '      0.     0.      0.    -0.33'
      PCK( 1953 ) = '                                      0.' &
      //            '      0.     0.      0.     0.'
      PCK( 1954 ) = '                                      0.' &
      //            '      0.     0.                  )'
      PCK( 1955 ) = ' '
      PCK( 1956 ) = '           BODY715_NUT_PREC_DEC = (   0.' &
      //            '      0.     0.      0.     0.'
      PCK( 1957 ) = '                                      0.' &
      //            '      0.     0.      0.     0.31'
      PCK( 1958 ) = '                                      0.' &
      //            '      0.     0.      0.     0.'
      PCK( 1959 ) = '                                      0.' &
      //            '      0.     0.                  )'
      PCK( 1960 ) = ' '
      PCK( 1961 ) = '           BODY715_NUT_PREC_PM  = (   0.' &
      //            '      0.     0.      0.     0.'
      PCK( 1962 ) = '                                      0.' &
      //            '      0.     0.      0.    -0.09'
      PCK( 1963 ) = '                                      0.' &
      //            '      0.     0.      0.     0.'
      PCK( 1964 ) = '                                      0.' &
      //            '      0.     0.                  )'
      PCK( 1965 ) = ' '
      PCK( 1966 ) = BEGTXT()
      PCK( 1967 ) = ' '
      PCK( 1968 ) = ' '
      PCK( 1969 ) = ' '
      PCK( 1970 ) = ' '
      PCK( 1971 ) = 'Satellites of Neptune'
      PCK( 1972 ) = ' '
      PCK( 1973 ) = ' '
      PCK( 1974 ) = '     Triton'
      PCK( 1975 ) = ' '
      PCK( 1976 ) = '        Old values:'
      PCK( 1977 ) = ' '
      PCK( 1978 ) = '           Values are unchanged in the 2' &
      //            '009 IAU report.'
      PCK( 1979 ) = ' '
      PCK( 1980 ) = '        Current values:'
      PCK( 1981 ) = ' '
      PCK( 1982 ) = BEGDAT()
      PCK( 1983 ) = ' '
      PCK( 1984 ) = '           BODY801_POLE_RA       = ( 299' &
      //            '.36     0.         0.  )'
      PCK( 1985 ) = '           BODY801_POLE_DEC      = (  41' &
      //            '.17     0.         0.  )'
      PCK( 1986 ) = '           BODY801_PM            = ( 296' &
      //            '.53   -61.2572637  0.  )'
      PCK( 1987 ) = '           BODY801_LONG_AXIS     = (   0' &
      //            '.                      )'
      PCK( 1988 ) = ' '
      PCK( 1989 ) = ' '
      PCK( 1990 ) = '           BODY801_NUT_PREC_RA   = (  0.' &
      //            '      0.      0.      0.'
      PCK( 1991 ) = '                                      0.' &
      //            '      0.      0.    -32.35'
      PCK( 1992 ) = '                                      0.' &
      //            '     -6.28   -2.08   -0.74'
      PCK( 1993 ) = '                                     -0.' &
      //            '28   -0.11   -0.07   -0.02'
      PCK( 1994 ) = '                                     -0.' &
      //            '01                         )'
      PCK( 1995 ) = ' '
      PCK( 1996 ) = ' '
      PCK( 1997 ) = '           BODY801_NUT_PREC_DEC  = (  0.' &
      //            '      0.      0.      0.'
      PCK( 1998 ) = '                                      0.' &
      //            '      0.      0.     22.55'
      PCK( 1999 ) = '                                      0.' &
      //            '      2.10    0.55    0.16'
      PCK( 2000 ) = '                                      0.' &
      //            '05    0.02    0.01    0.'
      PCK( 2001 ) = '                                      0.' &
      //            '                           )'
      PCK( 2002 ) = ' '
      PCK( 2003 ) = ' '
      PCK( 2004 ) = '           BODY801_NUT_PREC_PM   = (  0.' &
      //            '      0.      0.      0.'
      PCK( 2005 ) = '                                      0.' &
      //            '      0.      0.     22.25'
      PCK( 2006 ) = '                                      0.' &
      //            '      6.73    2.05    0.74'
      PCK( 2007 ) = '                                      0.' &
      //            '28    0.11    0.05    0.02'
      PCK( 2008 ) = '                                      0.' &
      //            '01                         )'
      PCK( 2009 ) = ' '
      PCK( 2010 ) = BEGTXT()
      PCK( 2011 ) = ' '
      PCK( 2012 ) = ' '
      PCK( 2013 ) = ' '
      PCK( 2014 ) = ' '
      PCK( 2015 ) = '     Nereid'
      PCK( 2016 ) = ' '
      PCK( 2017 ) = '        Old values:'
      PCK( 2018 ) = ' '
      PCK( 2019 ) = '           Values are from the 1988 IAU ' &
      //            'report [10].  Note that this'
      PCK( 2020 ) = '           rotation model pre-dated the ' &
      //            '1989 Voyager 2 Neptune'
      PCK( 2021 ) = '           encounter.'
      PCK( 2022 ) = ' '
      PCK( 2023 ) = ' '
      PCK( 2024 ) = '           body802_pole_ra       = (    ' &
      //            '273.48    0.        0.  )'
      PCK( 2025 ) = '           body802_pole_dec      = (    ' &
      //            ' 67.22    0.        0.  )'
      PCK( 2026 ) = '           body802_pm            = (    ' &
      //            '237.22    0.9996465 0.  )'
      PCK( 2027 ) = '           body802_long_axis     = (    ' &
      //            '  0.                    )'
      PCK( 2028 ) = ' '
      PCK( 2029 ) = ' '
      PCK( 2030 ) = '           The report seems to have a ty' &
      //            'po:  in the nut_prec_ra expression,'
      PCK( 2031 ) = '           where the report gives  -0.51' &
      //            ' sin 3N3, we use -0.51 3N2.'
      PCK( 2032 ) = ' '
      PCK( 2033 ) = '           body802_nut_prec_ra   = (  0.' &
      //            '    -17.81'
      PCK( 2034 ) = '                                      0.' &
      //            '      0.     0.      0.'
      PCK( 2035 ) = '                                      0.' &
      //            '      0.     0.'
      PCK( 2036 ) = '                                      2.' &
      //            '56   -0.51   0.11   -0.03  )'
      PCK( 2037 ) = ' '
      PCK( 2038 ) = '           body802_nut_prec_dec  = (  0.' &
      //            '     -6.67'
      PCK( 2039 ) = '                                      0.' &
      //            '      0.     0.      0.'
      PCK( 2040 ) = '                                      0.' &
      //            '      0.     0.'
      PCK( 2041 ) = '                                      0.' &
      //            '47   -0.07   0.01          )'
      PCK( 2042 ) = ' '
      PCK( 2043 ) = '           body802_nut_prec_pm   = (  0.' &
      //            '     16.48'
      PCK( 2044 ) = '                                      0.' &
      //            '      0.     0.      0.'
      PCK( 2045 ) = '                                      0.' &
      //            '      0.     0.'
      PCK( 2046 ) = '                                     -2.' &
      //            '57    0.51 -0.11    0.02  )'
      PCK( 2047 ) = ' '
      PCK( 2048 ) = ' '
      PCK( 2049 ) = ' '
      PCK( 2050 ) = '        Current values:'
      PCK( 2051 ) = ' '
      PCK( 2052 ) = '           The 2009 report [1] states th' &
      //            'at values for Nereid are not'
      PCK( 2053 ) = '           given because Nereid is not i' &
      //            'n synchronous rotation with Neptune'
      PCK( 2054 ) = '           (notes following table 2).'
      PCK( 2055 ) = ' '
      PCK( 2056 ) = ' '
      PCK( 2057 ) = ' '
      PCK( 2058 ) = '     Naiad'
      PCK( 2059 ) = ' '
      PCK( 2060 ) = '        Old values:'
      PCK( 2061 ) = ' '
      PCK( 2062 ) = '           Values are unchanged in the 2' &
      //            '009 IAU report.'
      PCK( 2063 ) = ' '
      PCK( 2064 ) = '        Current values:'
      PCK( 2065 ) = ' '
      PCK( 2066 ) = ' '
      PCK( 2067 ) = BEGDAT()
      PCK( 2068 ) = ' '
      PCK( 2069 ) = '           BODY803_POLE_RA       = (  29' &
      //            '9.36      0.          0.  )'
      PCK( 2070 ) = '           BODY803_POLE_DEC      = (   4' &
      //            '3.36      0.          0.  )'
      PCK( 2071 ) = '           BODY803_PM            = (  25' &
      //            '4.06  +1222.8441209   0.  )'
      PCK( 2072 ) = '           BODY803_LONG_AXIS     = (    ' &
      //            '0.                        )'
      PCK( 2073 ) = ' '
      PCK( 2074 ) = ' '
      PCK( 2075 ) = '           BODY803_NUT_PREC_RA   = (    ' &
      //            '0.70     -6.49     0.      0.'
      PCK( 2076 ) = '                                        ' &
      //            '0.        0.       0.      0.'
      PCK( 2077 ) = '                                        ' &
      //            '0.25      0.       0.      0.'
      PCK( 2078 ) = '                                        ' &
      //            '0.        0.       0.      0.'
      PCK( 2079 ) = '                                        ' &
      //            '0.                            )'
      PCK( 2080 ) = ' '
      PCK( 2081 ) = '           BODY803_NUT_PREC_DEC  = (   -' &
      //            '0.51     -4.75     0.      0.'
      PCK( 2082 ) = '                                        ' &
      //            '0.        0.       0.      0.'
      PCK( 2083 ) = '                                        ' &
      //            '0.09      0.       0.      0.'
      PCK( 2084 ) = '                                        ' &
      //            '0.        0.       0.      0.'
      PCK( 2085 ) = '                                        ' &
      //            '0.                            )'
      PCK( 2086 ) = ' '
      PCK( 2087 ) = '           BODY803_NUT_PREC_PM   = (   -' &
      //            '0.48      4.40     0.      0.'
      PCK( 2088 ) = '                                        ' &
      //            '0.        0.       0.      0.'
      PCK( 2089 ) = '                                       -' &
      //            '0.27      0.       0.      0.'
      PCK( 2090 ) = '                                        ' &
      //            '0.        0.       0.      0.'
      PCK( 2091 ) = '                                        ' &
      //            '0.                            )'
      PCK( 2092 ) = ' '
      PCK( 2093 ) = BEGTXT()
      PCK( 2094 ) = ' '
      PCK( 2095 ) = ' '
      PCK( 2096 ) = ' '
      PCK( 2097 ) = ' '
      PCK( 2098 ) = '     Thalassa'
      PCK( 2099 ) = ' '
      PCK( 2100 ) = ' '
      PCK( 2101 ) = '        Old values:'
      PCK( 2102 ) = ' '
      PCK( 2103 ) = '           Values are unchanged in the 2' &
      //            '009 IAU report.'
      PCK( 2104 ) = ' '
      PCK( 2105 ) = '        Current values:'
      PCK( 2106 ) = ' '
      PCK( 2107 ) = BEGDAT()
      PCK( 2108 ) = ' '
      PCK( 2109 ) = '           BODY804_POLE_RA       = (  29' &
      //            '9.36      0.          0. )'
      PCK( 2110 ) = '           BODY804_POLE_DEC      = (   4' &
      //            '3.45      0.          0. )'
      PCK( 2111 ) = '           BODY804_PM            = (  10' &
      //            '2.06   1155.7555612   0. )'
      PCK( 2112 ) = '           BODY804_LONG_AXIS     = (    ' &
      //            '0.                       )'
      PCK( 2113 ) = ' '
      PCK( 2114 ) = ' '
      PCK( 2115 ) = '           BODY804_NUT_PREC_RA   = (    ' &
      //            '0.70      0.      -0.28    0.'
      PCK( 2116 ) = '                                        ' &
      //            '0.        0.       0.      0.'
      PCK( 2117 ) = '                                        ' &
      //            '0.        0.       0.      0.'
      PCK( 2118 ) = '                                        ' &
      //            '0.        0.       0.      0.'
      PCK( 2119 ) = '                                        ' &
      //            '0.                             )'
      PCK( 2120 ) = ' '
      PCK( 2121 ) = ' '
      PCK( 2122 ) = '           BODY804_NUT_PREC_DEC  = (   -' &
      //            '0.51      0.      -0.21    0.'
      PCK( 2123 ) = '                                        ' &
      //            '0.        0.       0.      0.'
      PCK( 2124 ) = '                                        ' &
      //            '0.        0.       0.      0.'
      PCK( 2125 ) = '                                        ' &
      //            '0.        0.       0.      0.'
      PCK( 2126 ) = '                                        ' &
      //            '0.                             )'
      PCK( 2127 ) = ' '
      PCK( 2128 ) = '           BODY804_NUT_PREC_PM   = (   -' &
      //            '0.48      0.       0.19    0.'
      PCK( 2129 ) = '                                        ' &
      //            '0.        0.       0.      0.'
      PCK( 2130 ) = '                                        ' &
      //            '0.        0.       0.      0.'
      PCK( 2131 ) = '                                        ' &
      //            '0.        0.       0.      0.'
      PCK( 2132 ) = '                                        ' &
      //            '0.                             )'
      PCK( 2133 ) = ' '
      PCK( 2134 ) = BEGTXT()
      PCK( 2135 ) = ' '
      PCK( 2136 ) = ' '
      PCK( 2137 ) = ' '
      PCK( 2138 ) = '     Despina'
      PCK( 2139 ) = ' '
      PCK( 2140 ) = '        Old values:'
      PCK( 2141 ) = ' '
      PCK( 2142 ) = '           Values are unchanged in the 2' &
      //            '009 IAU report.'
      PCK( 2143 ) = ' '
      PCK( 2144 ) = '        Current values:'
      PCK( 2145 ) = ' '
      PCK( 2146 ) = ' '
      PCK( 2147 ) = BEGDAT()
      PCK( 2148 ) = ' '
      PCK( 2149 ) = '           BODY805_POLE_RA       = (  29' &
      //            '9.36      0.          0. )'
      PCK( 2150 ) = '           BODY805_POLE_DEC      = (   4' &
      //            '3.45      0.          0. )'
      PCK( 2151 ) = '           BODY805_PM            = (  30' &
      //            '6.51  +1075.7341562   0. )'
      PCK( 2152 ) = '           BODY805_LONG_AXIS     = (    ' &
      //            '0.                       )'
      PCK( 2153 ) = ' '
      PCK( 2154 ) = ' '
      PCK( 2155 ) = '           BODY805_NUT_PREC_RA   = (    ' &
      //            '0.70      0.       0.     -0.09'
      PCK( 2156 ) = '                                        ' &
      //            '0.        0.       0.      0.'
      PCK( 2157 ) = '                                        ' &
      //            '0.        0.       0.      0.'
      PCK( 2158 ) = '                                        ' &
      //            '0.        0.       0.      0.'
      PCK( 2159 ) = '                                        ' &
      //            '0.                              )'
      PCK( 2160 ) = ' '
      PCK( 2161 ) = '           BODY805_NUT_PREC_DEC  = (   -' &
      //            '0.51      0.       0.     -0.07'
      PCK( 2162 ) = '                                        ' &
      //            '0.        0.       0.      0.'
      PCK( 2163 ) = '                                        ' &
      //            '0.        0.       0.      0.'
      PCK( 2164 ) = '                                        ' &
      //            '0.        0.       0.      0.'
      PCK( 2165 ) = '                                        ' &
      //            '0.                              )'
      PCK( 2166 ) = ' '
      PCK( 2167 ) = '           BODY805_NUT_PREC_PM   = (   -' &
      //            '0.49      0.       0.      0.06'
      PCK( 2168 ) = '                                        ' &
      //            '0.        0.       0.      0.'
      PCK( 2169 ) = '                                        ' &
      //            '0.        0.       0.      0.'
      PCK( 2170 ) = '                                        ' &
      //            '0.        0.       0.      0.'
      PCK( 2171 ) = '                                        ' &
      //            '0.                              )'
      PCK( 2172 ) = BEGTXT()
      PCK( 2173 ) = ' '
      PCK( 2174 ) = ' '
      PCK( 2175 ) = ' '
      PCK( 2176 ) = '     Galatea'
      PCK( 2177 ) = ' '
      PCK( 2178 ) = ' '
      PCK( 2179 ) = '        Old values:'
      PCK( 2180 ) = ' '
      PCK( 2181 ) = '           Values are unchanged in the 2' &
      //            '009 IAU report.'
      PCK( 2182 ) = ' '
      PCK( 2183 ) = '        Current values:'
      PCK( 2184 ) = ' '
      PCK( 2185 ) = ' '
      PCK( 2186 ) = BEGDAT()
      PCK( 2187 ) = ' '
      PCK( 2188 ) = '           BODY806_POLE_RA       = (   2' &
      //            '99.36      0.          0. )'
      PCK( 2189 ) = '           BODY806_POLE_DEC      = (    ' &
      //            '43.43      0.          0. )'
      PCK( 2190 ) = '           BODY806_PM            = (   2' &
      //            '58.09    839.6597686   0. )'
      PCK( 2191 ) = '           BODY806_LONG_AXIS     = (    ' &
      //            ' 0.                       )'
      PCK( 2192 ) = ' '
      PCK( 2193 ) = ' '
      PCK( 2194 ) = '           BODY806_NUT_PREC_RA   = (    ' &
      //            '0.70      0.       0.      0.'
      PCK( 2195 ) = '                                       -' &
      //            '0.07      0.       0.      0.'
      PCK( 2196 ) = '                                        ' &
      //            '0.        0.       0.      0.'
      PCK( 2197 ) = '                                        ' &
      //            '0.        0.       0.      0.'
      PCK( 2198 ) = '                                        ' &
      //            '0.                             )'
      PCK( 2199 ) = ' '
      PCK( 2200 ) = '           BODY806_NUT_PREC_DEC  = (   -' &
      //            '0.51      0.       0.      0.'
      PCK( 2201 ) = '                                       -' &
      //            '0.05      0.       0.      0.'
      PCK( 2202 ) = '                                        ' &
      //            '0.        0.       0.      0.'
      PCK( 2203 ) = '                                        ' &
      //            '0.        0.       0.      0.'
      PCK( 2204 ) = '                                        ' &
      //            '0.                             )'
      PCK( 2205 ) = ' '
      PCK( 2206 ) = '           BODY806_NUT_PREC_PM   = (   -' &
      //            '0.48      0.       0.      0.'
      PCK( 2207 ) = '                                        ' &
      //            '0.05      0.       0.      0.'
      PCK( 2208 ) = '                                        ' &
      //            '0.        0.       0.      0.'
      PCK( 2209 ) = '                                        ' &
      //            '0.        0.       0.      0.'
      PCK( 2210 ) = '                                        ' &
      //            '0.                             )'
      PCK( 2211 ) = BEGTXT()
      PCK( 2212 ) = ' '
      PCK( 2213 ) = ' '
      PCK( 2214 ) = '     Larissa'
      PCK( 2215 ) = ' '
      PCK( 2216 ) = ' '
      PCK( 2217 ) = '        Old values:'
      PCK( 2218 ) = ' '
      PCK( 2219 ) = '           Values are unchanged in the 2' &
      //            '009 IAU report.'
      PCK( 2220 ) = ' '
      PCK( 2221 ) = '        Current values:'
      PCK( 2222 ) = ' '
      PCK( 2223 ) = BEGDAT()
      PCK( 2224 ) = ' '
      PCK( 2225 ) = '           BODY807_POLE_RA       = (   2' &
      //            '99.36     0.           0. )'
      PCK( 2226 ) = '           BODY807_POLE_DEC      = (    ' &
      //            '43.41     0.           0. )'
      PCK( 2227 ) = '           BODY807_PM            = (   1' &
      //            '79.41  +649.0534470    0. )'
      PCK( 2228 ) = '           BODY807_LONG_AXIS     = (    ' &
      //            ' 0.                       )'
      PCK( 2229 ) = ' '
      PCK( 2230 ) = ' '
      PCK( 2231 ) = '           BODY807_NUT_PREC_RA   = (    ' &
      //            '0.70      0.       0.      0.'
      PCK( 2232 ) = '                                        ' &
      //            '0.       -0.27     0.      0.'
      PCK( 2233 ) = '                                        ' &
      //            '0.        0.       0.      0.'
      PCK( 2234 ) = '                                        ' &
      //            '0.        0.       0.      0.'
      PCK( 2235 ) = '                                        ' &
      //            '0.                            )'
      PCK( 2236 ) = ' '
      PCK( 2237 ) = '           BODY807_NUT_PREC_DEC  = (   -' &
      //            '0.51      0.       0.      0.'
      PCK( 2238 ) = '                                        ' &
      //            '0.       -0.20     0.      0.'
      PCK( 2239 ) = '                                        ' &
      //            '0.        0.       0.      0.'
      PCK( 2240 ) = '                                        ' &
      //            '0.        0.       0.      0.'
      PCK( 2241 ) = '                                        ' &
      //            '0.                            )'
      PCK( 2242 ) = ' '
      PCK( 2243 ) = '           BODY807_NUT_PREC_PM   = (   -' &
      //            '0.48      0.       0.      0.'
      PCK( 2244 ) = '                                        ' &
      //            '0.        0.19     0.      0.'
      PCK( 2245 ) = '                                        ' &
      //            '0.        0.       0.      0.'
      PCK( 2246 ) = '                                        ' &
      //            '0.        0.       0.      0.'
      PCK( 2247 ) = '                                        ' &
      //            '0.                            )'
      PCK( 2248 ) = BEGTXT()
      PCK( 2249 ) = ' '
      PCK( 2250 ) = ' '
      PCK( 2251 ) = ' '
      PCK( 2252 ) = '     Proteus'
      PCK( 2253 ) = ' '
      PCK( 2254 ) = ' '
      PCK( 2255 ) = '        Old values:'
      PCK( 2256 ) = ' '
      PCK( 2257 ) = '           Values are unchanged in the 2' &
      //            '009 IAU report.'
      PCK( 2258 ) = ' '
      PCK( 2259 ) = '        Current values:'
      PCK( 2260 ) = ' '
      PCK( 2261 ) = BEGDAT()
      PCK( 2262 ) = ' '
      PCK( 2263 ) = '           BODY808_POLE_RA       = (  29' &
      //            '9.27      0.          0.  )'
      PCK( 2264 ) = '           BODY808_POLE_DEC      = (   4' &
      //            '2.91      0.          0.  )'
      PCK( 2265 ) = '           BODY808_PM            = (   9' &
      //            '3.38   +320.7654228   0.  )'
      PCK( 2266 ) = '           BODY808_LONG_AXIS     = (    ' &
      //            '0.                        )'
      PCK( 2267 ) = ' '
      PCK( 2268 ) = ' '
      PCK( 2269 ) = '           BODY808_NUT_PREC_RA   = (    ' &
      //            '0.70      0.       0.      0.'
      PCK( 2270 ) = '                                        ' &
      //            '0.        0.      -0.05    0.'
      PCK( 2271 ) = '                                        ' &
      //            '0.        0.       0.      0.'
      PCK( 2272 ) = '                                        ' &
      //            '0.        0.       0.      0.'
      PCK( 2273 ) = '                                        ' &
      //            '0.                             )'
      PCK( 2274 ) = ' '
      PCK( 2275 ) = '           BODY808_NUT_PREC_DEC  = (   -' &
      //            '0.51      0.       0.      0.'
      PCK( 2276 ) = '                                        ' &
      //            '0.        0.      -0.04    0.'
      PCK( 2277 ) = '                                        ' &
      //            '0.        0.       0.      0.'
      PCK( 2278 ) = '                                        ' &
      //            '0.        0.       0.      0.'
      PCK( 2279 ) = '                                        ' &
      //            '0.                             )'
      PCK( 2280 ) = ' '
      PCK( 2281 ) = '           BODY808_NUT_PREC_PM   = (   -' &
      //            '0.48      0.       0.      0.'
      PCK( 2282 ) = '                                        ' &
      //            '0.        0.       0.04    0.'
      PCK( 2283 ) = '                                        ' &
      //            '0.        0.       0.      0.'
      PCK( 2284 ) = '                                        ' &
      //            '0.        0.       0.      0.'
      PCK( 2285 ) = '                                        ' &
      //            '0.                             )'
      PCK( 2286 ) = ' '
      PCK( 2287 ) = BEGTXT()
      PCK( 2288 ) = ' '
      PCK( 2289 ) = ' '
      PCK( 2290 ) = ' '
      PCK( 2291 ) = ' '
      PCK( 2292 ) = ' '
      PCK( 2293 ) = 'Satellites of Pluto'
      PCK( 2294 ) = ' '
      PCK( 2295 ) = '     Charon'
      PCK( 2296 ) = ' '
      PCK( 2297 ) = '        Old values:'
      PCK( 2298 ) = ' '
      PCK( 2299 ) = '           Values are from the 2006 IAU ' &
      //            'report.'
      PCK( 2300 ) = ' '
      PCK( 2301 ) = '           body901_pole_ra       = (   3' &
      //            '12.993    0.         0. )'
      PCK( 2302 ) = '           body901_pole_dec      = (    ' &
      //            ' 6.163    0.         0. )'
      PCK( 2303 ) = '           body901_pm            = (    ' &
      //            '57.305  -56.3625225  0. )'
      PCK( 2304 ) = '           body901_long_axis     = (    ' &
      //            ' 0.                     )'
      PCK( 2305 ) = ' '
      PCK( 2306 ) = ' '
      PCK( 2307 ) = '        Current values:'
      PCK( 2308 ) = ' '
      PCK( 2309 ) = '        Due to the new definition of pla' &
      //            'netocentric coordinates'
      PCK( 2310 ) = '        for small bodies, and to the rec' &
      //            'lassification of Pluto'
      PCK( 2311 ) = '        as a dwarf planet, Charon''s nor' &
      //            'th pole direction has been'
      PCK( 2312 ) = '        inverted.'
      PCK( 2313 ) = ' '
      PCK( 2314 ) = '        The PM constant W0 is from [2].'
      PCK( 2315 ) = ' '
      PCK( 2316 ) = BEGDAT()
      PCK( 2317 ) = ' '
      PCK( 2318 ) = '           BODY901_POLE_RA       = (   1' &
      //            '32.993    0.         0. )'
      PCK( 2319 ) = '           BODY901_POLE_DEC      = (    ' &
      //            '-6.163    0.         0. )'
      PCK( 2320 ) = '           BODY901_PM            = (   1' &
      //            '22.695   56.3625225  0. )'
      PCK( 2321 ) = '           BODY901_LONG_AXIS     = (    ' &
      //            ' 0.                     )'
      PCK( 2322 ) = ' '
      PCK( 2323 ) = BEGTXT()
      PCK( 2324 ) = ' '
      PCK( 2325 ) = ' '
      PCK( 2326 ) = ' '
      PCK( 2327 ) = 'Orientation constants for Selected Comet' &
      //            's and Asteroids'
      PCK( 2328 ) = '----------------------------------------' &
      //            '----------------'
      PCK( 2329 ) = ' '
      PCK( 2330 ) = ' '
      PCK( 2331 ) = ' '
      PCK( 2332 ) = 'Ceres'
      PCK( 2333 ) = ' '
      PCK( 2334 ) = '        Old values are from the 2009 rep' &
      //            'ort.'
      PCK( 2335 ) = ' '
      PCK( 2336 ) = '           body2000001_pole_ra       = (' &
      //            '   291.       0.         0.  )'
      PCK( 2337 ) = '           body2000001_pole_dec      = (' &
      //            '    59.       0.         0.  )'
      PCK( 2338 ) = '           body2000001_pm            = (' &
      //            '   170.90   952.1532     0.  )'
      PCK( 2339 ) = '           body2000001_long_axis     = (' &
      //            '     0.                      )'
      PCK( 2340 ) = ' '
      PCK( 2341 ) = ' '
      PCK( 2342 ) = '        Current values:'
      PCK( 2343 ) = ' '
      PCK( 2344 ) = BEGDAT()
      PCK( 2345 ) = ' '
      PCK( 2346 ) = '           BODY2000001_POLE_RA       = (' &
      //            '   291.418    0.         0.  )'
      PCK( 2347 ) = '           BODY2000001_POLE_DEC      = (' &
      //            '    66.764    0.         0.  )'
      PCK( 2348 ) = '           BODY2000001_PM            = (' &
      //            '   170.650  952.1532     0.  )'
      PCK( 2349 ) = '           BODY2000001_LONG_AXIS     = (' &
      //            '     0.                      )'
      PCK( 2350 ) = ' '
      PCK( 2351 ) = BEGTXT()
      PCK( 2352 ) = ' '
      PCK( 2353 ) = ' '
      PCK( 2354 ) = ' '
      PCK( 2355 ) = 'Pallas'
      PCK( 2356 ) = ' '
      PCK( 2357 ) = '        Current values:'
      PCK( 2358 ) = ' '
      PCK( 2359 ) = BEGDAT()
      PCK( 2360 ) = ' '
      PCK( 2361 ) = '           BODY2000002_POLE_RA       = (' &
      //            '    33.       0.         0.  )'
      PCK( 2362 ) = '           BODY2000002_POLE_DEC      = (' &
      //            '    -3.       0.         0.  )'
      PCK( 2363 ) = '           BODY2000002_PM            = (' &
      //            '    38.    1105.8036     0.  )'
      PCK( 2364 ) = '           BODY2000002_LONG_AXIS     = (' &
      //            '     0.                      )'
      PCK( 2365 ) = ' '
      PCK( 2366 ) = BEGTXT()
      PCK( 2367 ) = ' '
      PCK( 2368 ) = ' '
      PCK( 2369 ) = ' '
      PCK( 2370 ) = 'Vesta'
      PCK( 2371 ) = ' '
      PCK( 2372 ) = '        Old values:'
      PCK( 2373 ) = ' '
      PCK( 2374 ) = '           Values are from the 2009 IAU ' &
      //            'report. (Errata? values in'
      PCK( 2375 ) = '           pck00010 differ from the "old' &
      //            ' values" cited and claimed to'
      PCK( 2376 ) = '           be from the 2009 report.)'
      PCK( 2377 ) = ' '
      PCK( 2378 ) = '           body2000004_pole_ra       = (' &
      //            '   305.8     0.         0.  )'
      PCK( 2379 ) = '           body2000004_pole_dec      = (' &
      //            '    41.4     0.         0.  )'
      PCK( 2380 ) = '           body2000004_pm            = (' &
      //            '   292.   1617.332776   0.  )'
      PCK( 2381 ) = '           body2000004_long_axis     = (' &
      //            '     0.                     )'
      PCK( 2382 ) = ' '
      PCK( 2383 ) = '        Current values:'
      PCK( 2384 ) = ' '
      PCK( 2385 ) = BEGDAT()
      PCK( 2386 ) = ' '
      PCK( 2387 ) = '           BODY2000004_POLE_RA       = (' &
      //            '   309.031   0.         0.  )'
      PCK( 2388 ) = '           BODY2000004_POLE_DEC      = (' &
      //            '    42.235   0.         0.  )'
      PCK( 2389 ) = '           BODY2000004_PM            = (' &
      //            '   285.39 1617.3329428  0.  )'
      PCK( 2390 ) = '           BODY2000004_LONG_AXIS     = (' &
      //            '     0.                     )'
      PCK( 2391 ) = ' '
      PCK( 2392 ) = BEGTXT()
      PCK( 2393 ) = ' '
      PCK( 2394 ) = ' '
      PCK( 2395 ) = ' '
      PCK( 2396 ) = '52 Europa (asteroid)'
      PCK( 2397 ) = ' '
      PCK( 2398 ) = ' '
      PCK( 2399 ) = '        Current values:'
      PCK( 2400 ) = ' '
      PCK( 2401 ) = BEGDAT()
      PCK( 2402 ) = ' '
      PCK( 2403 ) = '           BODY2000052_POLE_RA       = (' &
      //            '   257.0     0.         0.  )'
      PCK( 2404 ) = '           BODY2000052_POLE_DEC      = (' &
      //            '    12.0     0.         0.  )'
      PCK( 2405 ) = '           BODY2000052_PM            = (' &
      //            '    55.0  1534.6472187  0.  )'
      PCK( 2406 ) = '           BODY2000052_LONG_AXIS     = (' &
      //            '     0.                     )'
      PCK( 2407 ) = ' '
      PCK( 2408 ) = BEGTXT()
      PCK( 2409 ) = ' '
      PCK( 2410 ) = ' '
      PCK( 2411 ) = ' '
      PCK( 2412 ) = 'Lutetia'
      PCK( 2413 ) = ' '
      PCK( 2414 ) = '        Current values:'
      PCK( 2415 ) = ' '
      PCK( 2416 ) = BEGDAT()
      PCK( 2417 ) = ' '
      PCK( 2418 ) = '           BODY2000021_POLE_RA       = (' &
      //            '    52.       0.         0.  )'
      PCK( 2419 ) = '           BODY2000021_POLE_DEC      = (' &
      //            '    12.       0.         0.  )'
      PCK( 2420 ) = '           BODY2000021_PM            = (' &
      //            '    94.    1057.7515     0.  )'
      PCK( 2421 ) = '           BODY2000021_LONG_AXIS     = (' &
      //            '     0.                      )'
      PCK( 2422 ) = ' '
      PCK( 2423 ) = BEGTXT()
      PCK( 2424 ) = ' '
      PCK( 2425 ) = ' '
      PCK( 2426 ) = ' '
      PCK( 2427 ) = 'Ida'
      PCK( 2428 ) = ' '
      PCK( 2429 ) = '        Old values are from the 2009 rep' &
      //            'ort.'
      PCK( 2430 ) = ' '
      PCK( 2431 ) = '           body2431010_pole_ra       = (' &
      //            '  168.76      0.         0. )'
      PCK( 2432 ) = '           body2431010_pole_dec      = (' &
      //            '   -2.88      0.         0. )'
      PCK( 2433 ) = '           body2431010_pm            = (' &
      //            '  274.05  +1864.6280070  0. )'
      PCK( 2434 ) = '           body2431010_long_axis     = (' &
      //            '    0.                      )'
      PCK( 2435 ) = ' '
      PCK( 2436 ) = '           The PM constant W0 is from [2' &
      //            '].'
      PCK( 2437 ) = ' '
      PCK( 2438 ) = ' '
      PCK( 2439 ) = '        Current values:'
      PCK( 2440 ) = ' '
      PCK( 2441 ) = ' '
      PCK( 2442 ) = BEGDAT()
      PCK( 2443 ) = ' '
      PCK( 2444 ) = '           BODY2431010_POLE_RA       = (' &
      //            '  168.76      0.         0. )'
      PCK( 2445 ) = '           BODY2431010_POLE_DEC      = (' &
      //            '  -87.12      0.         0. )'
      PCK( 2446 ) = '           BODY2431010_PM            = (' &
      //            '  274.05  +1864.6280070  0. )'
      PCK( 2447 ) = '           BODY2431010_LONG_AXIS     = (' &
      //            '    0.                      )'
      PCK( 2448 ) = ' '
      PCK( 2449 ) = BEGTXT()
      PCK( 2450 ) = ' '
      PCK( 2451 ) = ' '
      PCK( 2452 ) = ' '
      PCK( 2453 ) = 'Eros'
      PCK( 2454 ) = ' '
      PCK( 2455 ) = '        Old values:'
      PCK( 2456 ) = ' '
      PCK( 2457 ) = '           Values are unchanged in the 2' &
      //            '009 IAU report.'
      PCK( 2458 ) = ' '
      PCK( 2459 ) = '        Current values:'
      PCK( 2460 ) = ' '
      PCK( 2461 ) = BEGDAT()
      PCK( 2462 ) = ' '
      PCK( 2463 ) = '           BODY2000433_POLE_RA       = (' &
      //            '   11.35       0.           0. )'
      PCK( 2464 ) = '           BODY2000433_POLE_DEC      = (' &
      //            '   17.22       0.           0. )'
      PCK( 2465 ) = '           BODY2000433_PM            = (' &
      //            '  326.07    1639.38864745   0. )'
      PCK( 2466 ) = '           BODY2000433_LONG_AXIS     = (' &
      //            '    0.                         )'
      PCK( 2467 ) = ' '
      PCK( 2468 ) = BEGTXT()
      PCK( 2469 ) = ' '
      PCK( 2470 ) = ' '
      PCK( 2471 ) = ' '
      PCK( 2472 ) = 'Davida'
      PCK( 2473 ) = ' '
      PCK( 2474 ) = '        Current values:'
      PCK( 2475 ) = ' '
      PCK( 2476 ) = BEGDAT()
      PCK( 2477 ) = ' '
      PCK( 2478 ) = '           BODY2000511_POLE_RA       = (' &
      //            '  297.        0.           0. )'
      PCK( 2479 ) = '           BODY2000511_POLE_DEC      = (' &
      //            '    5.        0.           0. )'
      PCK( 2480 ) = '           BODY2000511_PM            = (' &
      //            '  268.1    1684.4193549    0. )'
      PCK( 2481 ) = '           BODY2000511_LONG_AXIS     = (' &
      //            '    0.                        )'
      PCK( 2482 ) = ' '
      PCK( 2483 ) = BEGTXT()
      PCK( 2484 ) = ' '
      PCK( 2485 ) = ' '
      PCK( 2486 ) = ' '
      PCK( 2487 ) = 'Gaspra'
      PCK( 2488 ) = ' '
      PCK( 2489 ) = '        Old values:'
      PCK( 2490 ) = ' '
      PCK( 2491 ) = '           Values are unchanged in the 2' &
      //            '009 IAU report.'
      PCK( 2492 ) = ' '
      PCK( 2493 ) = '        Current values:'
      PCK( 2494 ) = ' '
      PCK( 2495 ) = BEGDAT()
      PCK( 2496 ) = ' '
      PCK( 2497 ) = '           BODY9511010_POLE_RA       = (' &
      //            '   9.47     0.         0. )'
      PCK( 2498 ) = '           BODY9511010_POLE_DEC      = (' &
      //            '  26.70     0.         0. )'
      PCK( 2499 ) = '           BODY9511010_PM            = (' &
      //            '  83.67  1226.9114850  0. )'
      PCK( 2500 ) = '           BODY9511010_LONG_AXIS     = (' &
      //            '   0.                     )'
      PCK( 2501 ) = ' '
      PCK( 2502 ) = BEGTXT()
      PCK( 2503 ) = ' '
      PCK( 2504 ) = ' '
      PCK( 2505 ) = ' '
      PCK( 2506 ) = 'Steins'
      PCK( 2507 ) = ' '
      PCK( 2508 ) = '        Old values are from the 2009 rep' &
      //            'ort.'
      PCK( 2509 ) = ' '
      PCK( 2510 ) = '           body2002867_pole_ra       = (' &
      //            '  90.        0.        0. )'
      PCK( 2511 ) = '           body2002867_pole_dec      = (' &
      //            ' -62.        0.        0. )'
      PCK( 2512 ) = '           body2002867_pm            = (' &
      //            '  93.94   1428.852332  0. )'
      PCK( 2513 ) = '           body2002867_long_axis     = (' &
      //            '   0.                     )'
      PCK( 2514 ) = ' '
      PCK( 2515 ) = '        Current values:'
      PCK( 2516 ) = ' '
      PCK( 2517 ) = BEGDAT()
      PCK( 2518 ) = ' '
      PCK( 2519 ) = '           BODY2002867_POLE_RA       = (' &
      //            '  91.        0.        0. )'
      PCK( 2520 ) = '           BODY2002867_POLE_DEC      = (' &
      //            ' -62.        0.        0. )'
      PCK( 2521 ) = '           BODY2002867_PM            = (' &
      //            ' 321.76   1428.09917   0. )'
      PCK( 2522 ) = '           BODY2002867_LONG_AXIS     = (' &
      //            '   0.                     )'
      PCK( 2523 ) = ' '
      PCK( 2524 ) = BEGTXT()
      PCK( 2525 ) = ' '
      PCK( 2526 ) = ' '
      PCK( 2527 ) = ' '
      PCK( 2528 ) = 'Itokawa'
      PCK( 2529 ) = ' '
      PCK( 2530 ) = '        Old values:'
      PCK( 2531 ) = ' '
      PCK( 2532 ) = '           Values are unchanged in the 2' &
      //            '009 IAU report.'
      PCK( 2533 ) = ' '
      PCK( 2534 ) = '        Current values:'
      PCK( 2535 ) = ' '
      PCK( 2536 ) = BEGDAT()
      PCK( 2537 ) = ' '
      PCK( 2538 ) = '           BODY2025143_POLE_RA       = (' &
      //            '   90.53       0.           0. )'
      PCK( 2539 ) = '           BODY2025143_POLE_DEC      = (' &
      //            '  -66.30       0.           0. )'
      PCK( 2540 ) = '           BODY2025143_PM            = (' &
      //            '  000.0      712.143        0. )'
      PCK( 2541 ) = '           BODY2025143_LONG_AXIS     = (' &
      //            '    0.                         )'
      PCK( 2542 ) = ' '
      PCK( 2543 ) = BEGTXT()
      PCK( 2544 ) = ' '
      PCK( 2545 ) = ' '
      PCK( 2546 ) = ' '
      PCK( 2547 ) = '9P/Tempel 1'
      PCK( 2548 ) = ' '
      PCK( 2549 ) = ' '
      PCK( 2550 ) = '        Old values are from the 2009 IAU' &
      //            ' report.'
      PCK( 2551 ) = ' '
      PCK( 2552 ) = '           body1000093_pole_ra       = (' &
      //            '   294.       0.         0.  )'
      PCK( 2553 ) = '           body1000093_pole_dec      = (' &
      //            '    73.       0.         0.  )'
      PCK( 2554 ) = '           body1000093_pm            = (' &
      //            '   252.63   212.064      0.  )'
      PCK( 2555 ) = '           body1000093_long_axis     = (' &
      //            '     0.                      )'
      PCK( 2556 ) = ' '
      PCK( 2557 ) = '        Current values:'
      PCK( 2558 ) = ' '
      PCK( 2559 ) = BEGDAT()
      PCK( 2560 ) = ' '
      PCK( 2561 ) = '           BODY1000093_POLE_RA       = (' &
      //            '   255.       0.         0.  )'
      PCK( 2562 ) = '           BODY1000093_POLE_DEC      = (' &
      //            '    64.5      0.         0.  )'
      PCK( 2563 ) = '           BODY1000093_PM            = (' &
      //            '    69.2    212.807      0.  )'
      PCK( 2564 ) = '           BODY1000093_LONG_AXIS     = (' &
      //            '     0.                      )'
      PCK( 2565 ) = ' '
      PCK( 2566 ) = BEGTXT()
      PCK( 2567 ) = ' '
      PCK( 2568 ) = ' '
      PCK( 2569 ) = ' '
      PCK( 2570 ) = '19P/Borrelly'
      PCK( 2571 ) = ' '
      PCK( 2572 ) = '        Old values are unchanged from th' &
      //            'e 2009 IAU report.'
      PCK( 2573 ) = ' '
      PCK( 2574 ) = '           body1000005_pole_ra       = (' &
      //            '   218.5      0.         0.  )'
      PCK( 2575 ) = '           body1000005_pole_dec      = (' &
      //            '   -12.5      0.         0.  )'
      PCK( 2576 ) = '           body1000005_pm            = (' &
      //            '   000.     390.0        0.  )'
      PCK( 2577 ) = '           body1000005_long_axis     = (' &
      //            '     0.                      )'
      PCK( 2578 ) = ' '
      PCK( 2579 ) = '        Current values:'
      PCK( 2580 ) = ' '
      PCK( 2581 ) = BEGDAT()
      PCK( 2582 ) = ' '
      PCK( 2583 ) = '           BODY1000005_POLE_RA       = (' &
      //            '   218.5      0.         0.  )'
      PCK( 2584 ) = '           BODY1000005_POLE_DEC      = (' &
      //            '   -12.5      0.         0.  )'
      PCK( 2585 ) = '           BODY1000005_PM            = (' &
      //            '   000.     324.3        0.  )'
      PCK( 2586 ) = '           BODY1000005_LONG_AXIS     = (' &
      //            '     0.                      )'
      PCK( 2587 ) = ' '
      PCK( 2588 ) = BEGTXT()
      PCK( 2589 ) = ' '
      PCK( 2590 ) = ' '
      PCK( 2591 ) = ' '
      PCK( 2592 ) = '67P/Churyumov-Gerasimenko'
      PCK( 2593 ) = ' '
      PCK( 2594 ) = ' '
      PCK( 2595 ) = '        Current values:'
      PCK( 2596 ) = ' '
      PCK( 2597 ) = BEGDAT()
      PCK( 2598 ) = ' '
      PCK( 2599 ) = '           BODY1000012_POLE_RA       = (' &
      //            '    69.54      0.           0.  )'
      PCK( 2600 ) = '           BODY1000012_POLE_DEC      = (' &
      //            '    64.11      0.           0.  )'
      PCK( 2601 ) = '           BODY1000012_PM            = (' &
      //            '   114.69    696.543884683  0.  )'
      PCK( 2602 ) = '           BODY1000012_LONG_AXIS     = (' &
      //            '     0.                         )'
      PCK( 2603 ) = ' '
      PCK( 2604 ) = BEGTXT()
      PCK( 2605 ) = ' '
      PCK( 2606 ) = ' '
      PCK( 2607 ) = ' '
      PCK( 2608 ) = 'Radii of Sun and Planets'
      PCK( 2609 ) = '----------------------------------------' &
      //            '----------------'
      PCK( 2610 ) = ' '
      PCK( 2611 ) = ' '
      PCK( 2612 ) = 'Sun'
      PCK( 2613 ) = ' '
      PCK( 2614 ) = '     Old values:'
      PCK( 2615 ) = ' '
      PCK( 2616 ) = '        Values are from the 2009 IAU rep' &
      //            'ort.'
      PCK( 2617 ) = ' '
      PCK( 2618 ) = '           body10_radii      = (  696000' &
      //            '.  696000.  696000.  )'
      PCK( 2619 ) = ' '
      PCK( 2620 ) = ' '
      PCK( 2621 ) = BEGDAT()
      PCK( 2622 ) = ' '
      PCK( 2623 ) = '        BODY10_RADII      = (  695700.  ' &
      //            '695700.  695700.  )'
      PCK( 2624 ) = ' '
      PCK( 2625 ) = BEGTXT()
      PCK( 2626 ) = ' '
      PCK( 2627 ) = ' '
      PCK( 2628 ) = 'Mercury'
      PCK( 2629 ) = ' '
      PCK( 2630 ) = '     Old values:'
      PCK( 2631 ) = ' '
      PCK( 2632 ) = '        Values are from the 2009 IAU rep' &
      //            'ort.'
      PCK( 2633 ) = ' '
      PCK( 2634 ) = '        body199_radii     = ( 2439.7   2' &
      //            '439.7   2439.7 )'
      PCK( 2635 ) = ' '
      PCK( 2636 ) = ' '
      PCK( 2637 ) = '     Current values:'
      PCK( 2638 ) = ' '
      PCK( 2639 ) = BEGDAT()
      PCK( 2640 ) = ' '
      PCK( 2641 ) = '        BODY199_RADII     = ( 2440.53  2' &
      //            '440.53  2438.26 )'
      PCK( 2642 ) = ' '
      PCK( 2643 ) = BEGTXT()
      PCK( 2644 ) = ' '
      PCK( 2645 ) = ' '
      PCK( 2646 ) = 'Venus'
      PCK( 2647 ) = ' '
      PCK( 2648 ) = '     Old values:'
      PCK( 2649 ) = ' '
      PCK( 2650 ) = '        Values are unchanged in the 2009' &
      //            ' IAU report.'
      PCK( 2651 ) = ' '
      PCK( 2652 ) = '     Current values:'
      PCK( 2653 ) = ' '
      PCK( 2654 ) = BEGDAT()
      PCK( 2655 ) = ' '
      PCK( 2656 ) = '        BODY299_RADII     = ( 6051.8   6' &
      //            '051.8   6051.8 )'
      PCK( 2657 ) = ' '
      PCK( 2658 ) = BEGTXT()
      PCK( 2659 ) = ' '
      PCK( 2660 ) = ' '
      PCK( 2661 ) = 'Earth'
      PCK( 2662 ) = ' '
      PCK( 2663 ) = '     Old values:'
      PCK( 2664 ) = ' '
      PCK( 2665 ) = '        Values are from the 2006 IAU rep' &
      //            'ort.'
      PCK( 2666 ) = ' '
      PCK( 2667 ) = '        body399_radii     = ( 6378.14   ' &
      //            '6378.14   6356.75 )'
      PCK( 2668 ) = ' '
      PCK( 2669 ) = ' '
      PCK( 2670 ) = '     Current values:'
      PCK( 2671 ) = ' '
      PCK( 2672 ) = ' '
      PCK( 2673 ) = BEGDAT()
      PCK( 2674 ) = ' '
      PCK( 2675 ) = '        BODY399_RADII     = ( 6378.1366 ' &
      //            '  6378.1366   6356.7519 )'
      PCK( 2676 ) = ' '
      PCK( 2677 ) = BEGTXT()
      PCK( 2678 ) = ' '
      PCK( 2679 ) = ' '
      PCK( 2680 ) = 'Mars'
      PCK( 2681 ) = ' '
      PCK( 2682 ) = ' '
      PCK( 2683 ) = '     Old values:'
      PCK( 2684 ) = ' '
      PCK( 2685 ) = '        Values are from the 2006 IAU rep' &
      //            'ort.'
      PCK( 2686 ) = ' '
      PCK( 2687 ) = '        body499_radii       = (  3397.  ' &
      //            '3397.  3375.  )'
      PCK( 2688 ) = ' '
      PCK( 2689 ) = ' '
      PCK( 2690 ) = '     Current values:'
      PCK( 2691 ) = ' '
      PCK( 2692 ) = '        The 2009 IAU report gives separa' &
      //            'te values for the north and'
      PCK( 2693 ) = '        south polar radii:'
      PCK( 2694 ) = ' '
      PCK( 2695 ) = '           north:  3373.19'
      PCK( 2696 ) = '           south:  3379.21'
      PCK( 2697 ) = ' '
      PCK( 2698 ) = '        The report provides the average ' &
      //            'of these values as well,'
      PCK( 2699 ) = '        which we use as the polar radius' &
      //            ' for the triaxial model.'
      PCK( 2700 ) = ' '
      PCK( 2701 ) = BEGDAT()
      PCK( 2702 ) = ' '
      PCK( 2703 ) = '        BODY499_RADII       = ( 3396.19 ' &
      //            '  3396.19   3376.20 )'
      PCK( 2704 ) = ' '
      PCK( 2705 ) = BEGTXT()
      PCK( 2706 ) = ' '
      PCK( 2707 ) = ' '
      PCK( 2708 ) = ' '
      PCK( 2709 ) = 'Jupiter'
      PCK( 2710 ) = ' '
      PCK( 2711 ) = '     Old values:'
      PCK( 2712 ) = ' '
      PCK( 2713 ) = '        Values are unchanged in the 2009' &
      //            ' IAU report.'
      PCK( 2714 ) = ' '
      PCK( 2715 ) = '     Current values:'
      PCK( 2716 ) = ' '
      PCK( 2717 ) = BEGDAT()
      PCK( 2718 ) = ' '
      PCK( 2719 ) = '        BODY599_RADII     = ( 71492   71' &
      //            '492   66854 )'
      PCK( 2720 ) = ' '
      PCK( 2721 ) = BEGTXT()
      PCK( 2722 ) = ' '
      PCK( 2723 ) = ' '
      PCK( 2724 ) = ' '
      PCK( 2725 ) = 'Saturn'
      PCK( 2726 ) = ' '
      PCK( 2727 ) = '     Old values:'
      PCK( 2728 ) = ' '
      PCK( 2729 ) = '        Values are unchanged in the 2009' &
      //            ' IAU report.'
      PCK( 2730 ) = ' '
      PCK( 2731 ) = '     Current values:'
      PCK( 2732 ) = ' '
      PCK( 2733 ) = BEGDAT()
      PCK( 2734 ) = ' '
      PCK( 2735 ) = '        BODY699_RADII     = ( 60268   60' &
      //            '268   54364 )'
      PCK( 2736 ) = ' '
      PCK( 2737 ) = BEGTXT()
      PCK( 2738 ) = ' '
      PCK( 2739 ) = ' '
      PCK( 2740 ) = ' '
      PCK( 2741 ) = 'Uranus'
      PCK( 2742 ) = ' '
      PCK( 2743 ) = '     Old values:'
      PCK( 2744 ) = ' '
      PCK( 2745 ) = '        Values are unchanged in the 2009' &
      //            ' IAU report.'
      PCK( 2746 ) = ' '
      PCK( 2747 ) = '     Current values:'
      PCK( 2748 ) = ' '
      PCK( 2749 ) = BEGDAT()
      PCK( 2750 ) = ' '
      PCK( 2751 ) = '        BODY799_RADII     = ( 25559   25' &
      //            '559   24973 )'
      PCK( 2752 ) = ' '
      PCK( 2753 ) = BEGTXT()
      PCK( 2754 ) = ' '
      PCK( 2755 ) = ' '
      PCK( 2756 ) = ' '
      PCK( 2757 ) = 'Neptune'
      PCK( 2758 ) = ' '
      PCK( 2759 ) = '     Old values:'
      PCK( 2760 ) = ' '
      PCK( 2761 ) = '        Values are unchanged in the 2009' &
      //            ' IAU report.'
      PCK( 2762 ) = ' '
      PCK( 2763 ) = '     Current values:'
      PCK( 2764 ) = ' '
      PCK( 2765 ) = '        (Values are for the 1 bar pressu' &
      //            're level.)'
      PCK( 2766 ) = ' '
      PCK( 2767 ) = BEGDAT()
      PCK( 2768 ) = ' '
      PCK( 2769 ) = '        BODY899_RADII     = ( 24764   24' &
      //            '764  24341 )'
      PCK( 2770 ) = ' '
      PCK( 2771 ) = BEGTXT()
      PCK( 2772 ) = ' '
      PCK( 2773 ) = ' '
      PCK( 2774 ) = ' '
      PCK( 2775 ) = 'Radii of the Dwarf Planet Pluto'
      PCK( 2776 ) = '----------------------------------------' &
      //            '----------------'
      PCK( 2777 ) = ' '
      PCK( 2778 ) = ' '
      PCK( 2779 ) = 'Pluto'
      PCK( 2780 ) = ' '
      PCK( 2781 ) = '     Old values:'
      PCK( 2782 ) = ' '
      PCK( 2783 ) = '        Values are from the 2009 IAU rep' &
      //            'ort.'
      PCK( 2784 ) = ' '
      PCK( 2785 ) = '        body999_radii     = ( 1195   119' &
      //            '5   1195 )'
      PCK( 2786 ) = ' '
      PCK( 2787 ) = '     Current values:'
      PCK( 2788 ) = ' '
      PCK( 2789 ) = BEGDAT()
      PCK( 2790 ) = ' '
      PCK( 2791 ) = '        BODY999_RADII     = ( 1188.3   1' &
      //            '188.3   1188.3 )'
      PCK( 2792 ) = ' '
      PCK( 2793 ) = BEGTXT()
      PCK( 2794 ) = ' '
      PCK( 2795 ) = ' '
      PCK( 2796 ) = ' '
      PCK( 2797 ) = ' '
      PCK( 2798 ) = 'Radii of Satellites'
      PCK( 2799 ) = '----------------------------------------' &
      //            '----------------'
      PCK( 2800 ) = ' '
      PCK( 2801 ) = ' '
      PCK( 2802 ) = 'Moon'
      PCK( 2803 ) = ' '
      PCK( 2804 ) = '     Old values:'
      PCK( 2805 ) = ' '
      PCK( 2806 ) = '        Values are unchanged in the 2009' &
      //            ' IAU report.'
      PCK( 2807 ) = ' '
      PCK( 2808 ) = '     Current values:'
      PCK( 2809 ) = ' '
      PCK( 2810 ) = BEGDAT()
      PCK( 2811 ) = ' '
      PCK( 2812 ) = '        BODY301_RADII     = ( 1737.4   1' &
      //            '737.4   1737.4 )'
      PCK( 2813 ) = ' '
      PCK( 2814 ) = BEGTXT()
      PCK( 2815 ) = ' '
      PCK( 2816 ) = ' '
      PCK( 2817 ) = ' '
      PCK( 2818 ) = 'Satellites of Mars'
      PCK( 2819 ) = ' '
      PCK( 2820 ) = '     Old values:'
      PCK( 2821 ) = ' '
      PCK( 2822 ) = '        Values are from the 2006 IAU rep' &
      //            'ort.'
      PCK( 2823 ) = ' '
      PCK( 2824 ) = '        body401_radii     = ( 13.4    11' &
      //            '.2    9.2 )'
      PCK( 2825 ) = '        body402_radii     = (  7.5     6' &
      //            '.1    5.2 )'
      PCK( 2826 ) = ' '
      PCK( 2827 ) = '     Current values:'
      PCK( 2828 ) = ' '
      PCK( 2829 ) = BEGDAT()
      PCK( 2830 ) = ' '
      PCK( 2831 ) = '        BODY401_RADII     = ( 13.0    11' &
      //            '.4    9.1 )'
      PCK( 2832 ) = '        BODY402_RADII     = (  7.8     6' &
      //            '.0    5.1 )'
      PCK( 2833 ) = ' '
      PCK( 2834 ) = BEGTXT()
      PCK( 2835 ) = ' '
      PCK( 2836 ) = ' '
      PCK( 2837 ) = ' '
      PCK( 2838 ) = 'Satellites of Jupiter'
      PCK( 2839 ) = ' '
      PCK( 2840 ) = '     Old values:'
      PCK( 2841 ) = ' '
      PCK( 2842 ) = '        Values are unchanged in the 2009' &
      //            ' IAU report,'
      PCK( 2843 ) = '        except for those of Europa, Gany' &
      //            'mede, Callisto,'
      PCK( 2844 ) = '        and Metis. For Metis, now all th' &
      //            'ree radii are'
      PCK( 2845 ) = '        provided.'
      PCK( 2846 ) = ' '
      PCK( 2847 ) = '           body502_radii     = ( 1564.13' &
      //            '  1561.23  1560.93 )'
      PCK( 2848 ) = '           body503_radii     = ( 2632.4 ' &
      //            '  2632.29  2632.35 )'
      PCK( 2849 ) = '           body504_radii     = ( 2409.4 ' &
      //            '  2409.2   2409.3  )'
      PCK( 2850 ) = ' '
      PCK( 2851 ) = '           The value for the second radi' &
      //            'us for body 516 is not given in'
      PCK( 2852 ) = '           2003 IAU report.   The values' &
      //            ' given are:'
      PCK( 2853 ) = ' '
      PCK( 2854 ) = '              body516_radii    = (  30  ' &
      //            ' ---   20   )'
      PCK( 2855 ) = ' '
      PCK( 2856 ) = '           For use within the SPICE syst' &
      //            'em, we use only the mean radius.'
      PCK( 2857 ) = ' '
      PCK( 2858 ) = '           body516_radii    = (  21.5   ' &
      //            '21.5  21.5  )'
      PCK( 2859 ) = ' '
      PCK( 2860 ) = ' '
      PCK( 2861 ) = ' '
      PCK( 2862 ) = ' '
      PCK( 2863 ) = '     Current values:'
      PCK( 2864 ) = ' '
      PCK( 2865 ) = '        Note that for Ganymede and Calli' &
      //            'sto only mean radii'
      PCK( 2866 ) = '        are provided.'
      PCK( 2867 ) = ' '
      PCK( 2868 ) = BEGDAT()
      PCK( 2869 ) = ' '
      PCK( 2870 ) = '        BODY501_RADII     = ( 1829.4   1' &
      //            '819.4   1815.7  )'
      PCK( 2871 ) = '        BODY502_RADII     = ( 1562.6  15' &
      //            '60.3    1559.5  )'
      PCK( 2872 ) = '        BODY503_RADII     = ( 2631.2  26' &
      //            '31.2    2631.2  )'
      PCK( 2873 ) = '        BODY504_RADII     = ( 2410.3  24' &
      //            '10.3    2410.3  )'
      PCK( 2874 ) = '        BODY505_RADII     = (  125      ' &
      //            ' 73       64    )'
      PCK( 2875 ) = ' '
      PCK( 2876 ) = BEGTXT()
      PCK( 2877 ) = ' '
      PCK( 2878 ) = '        Only mean radii are available in' &
      //            ' the 2003 IAU report for bodies'
      PCK( 2879 ) = '        506-513.'
      PCK( 2880 ) = ' '
      PCK( 2881 ) = BEGDAT()
      PCK( 2882 ) = ' '
      PCK( 2883 ) = '        BODY506_RADII    = (    85      ' &
      //            ' 85       85   )'
      PCK( 2884 ) = '        BODY507_RADII    = (    40      ' &
      //            ' 40       40   )'
      PCK( 2885 ) = '        BODY508_RADII    = (    18      ' &
      //            ' 18       18   )'
      PCK( 2886 ) = '        BODY509_RADII    = (    14      ' &
      //            ' 14       14   )'
      PCK( 2887 ) = '        BODY510_RADII    = (    12      ' &
      //            ' 12       12   )'
      PCK( 2888 ) = '        BODY511_RADII    = (    15      ' &
      //            ' 15       15   )'
      PCK( 2889 ) = '        BODY512_RADII    = (    10      ' &
      //            ' 10       10   )'
      PCK( 2890 ) = '        BODY513_RADII    = (     5      ' &
      //            '  5        5   )'
      PCK( 2891 ) = '        BODY514_RADII    = (    58      ' &
      //            ' 49       42   )'
      PCK( 2892 ) = '        BODY515_RADII    = (    10      ' &
      //            '  8        7   )'
      PCK( 2893 ) = '        BODY516_RADII    = (    30      ' &
      //            ' 20       17   )'
      PCK( 2894 ) = ' '
      PCK( 2895 ) = BEGTXT()
      PCK( 2896 ) = ' '
      PCK( 2897 ) = ' '
      PCK( 2898 ) = ' '
      PCK( 2899 ) = 'Satellites of Saturn'
      PCK( 2900 ) = ' '
      PCK( 2901 ) = ' '
      PCK( 2902 ) = '     Old values:'
      PCK( 2903 ) = ' '
      PCK( 2904 ) = '        Values are from the 2009 IAU rep' &
      //            'ort.'
      PCK( 2905 ) = ' '
      PCK( 2906 ) = ' '
      PCK( 2907 ) = '        body601_radii     = (  207.8    ' &
      //            ' 196.7     190.6   )'
      PCK( 2908 ) = '        body602_radii     = (  256.6    ' &
      //            ' 251.4     248.3   )'
      PCK( 2909 ) = '        body603_radii     = (  538.4    ' &
      //            ' 528.3     526.3   )'
      PCK( 2910 ) = '        body604_radii     = (  563.4    ' &
      //            ' 561.3     559.6   )'
      PCK( 2911 ) = '        body605_radii     = (  765.0    ' &
      //            ' 763.1     762.4   )'
      PCK( 2912 ) = '        body606_radii     = ( 2575.15   ' &
      //            ' 2574.78   2574.47 )'
      PCK( 2913 ) = '        body607_radii     = (  180.1    ' &
      //            '  133.0    102.7   )'
      PCK( 2914 ) = '        body608_radii     = (  745.7    ' &
      //            ' 745.7     712.1   )'
      PCK( 2915 ) = '        body609_radii     = (  109.4    ' &
      //            ' 108.5     101.8   )'
      PCK( 2916 ) = '        body610_radii     = (  101.5    ' &
      //            '  92.5      76.3   )'
      PCK( 2917 ) = '        body611_radii     = (   64.9    ' &
      //            '  57.0      53.1   )'
      PCK( 2918 ) = '        body612_radii     = (   21.7    ' &
      //            '  19.1      13.0   )'
      PCK( 2919 ) = '        body613_radii     = (   16.3    ' &
      //            '  11.8      10.0   )'
      PCK( 2920 ) = '        body614_radii     = (   15.1    ' &
      //            '  11.5       7.0   )'
      PCK( 2921 ) = '        body615_radii     = (   20.4    ' &
      //            '  17.7       9.4   )'
      PCK( 2922 ) = '        body616_radii     = (   67.8    ' &
      //            '  39.7      29.7   )'
      PCK( 2923 ) = '        body617_radii     = (   52.0    ' &
      //            '  40.5      32.0   )'
      PCK( 2924 ) = '        body618_radii     = (   17.2    ' &
      //            '  15.7      10.4   )'
      PCK( 2925 ) = ' '
      PCK( 2926 ) = '        body632_radii     = (    1.6    ' &
      //            '   1.6       1.6   )'
      PCK( 2927 ) = '        body633_radii     = (    2.9    ' &
      //            '   2.8       2.0   )'
      PCK( 2928 ) = '        body634_radii     = (    1.5    ' &
      //            '   1.2       1.0   )'
      PCK( 2929 ) = '        body635_radii     = (    4.3    ' &
      //            '   4.1       3.2   )'
      PCK( 2930 ) = '        body649_radii     = (    1      ' &
      //            '   1         1     )'
      PCK( 2931 ) = ' '
      PCK( 2932 ) = ' '
      PCK( 2933 ) = '     Current values:'
      PCK( 2934 ) = ' '
      PCK( 2935 ) = BEGDAT()
      PCK( 2936 ) = ' '
      PCK( 2937 ) = '        BODY601_RADII     = (  207.8    ' &
      //            ' 196.7     190.6   )'
      PCK( 2938 ) = '        BODY602_RADII     = (  256.6    ' &
      //            ' 251.4     248.3   )'
      PCK( 2939 ) = '        BODY603_RADII     = (  538.4    ' &
      //            ' 528.3     526.3   )'
      PCK( 2940 ) = '        BODY604_RADII     = (  563.4    ' &
      //            ' 561.3     559.6   )'
      PCK( 2941 ) = '        BODY605_RADII     = (  765.0    ' &
      //            ' 763.1     762.4   )'
      PCK( 2942 ) = '        BODY606_RADII     = ( 2575.15   ' &
      //            ' 2574.78   2574.47 )'
      PCK( 2943 ) = '        BODY607_RADII     = (  180.1    ' &
      //            '  133.0    102.7   )'
      PCK( 2944 ) = '        BODY608_RADII     = (  745.7    ' &
      //            ' 745.7     712.1   )'
      PCK( 2945 ) = '        BODY609_RADII     = (  109.4    ' &
      //            ' 108.5     101.8   )'
      PCK( 2946 ) = '        BODY610_RADII     = (  101.7    ' &
      //            '  93.0      76.3   )'
      PCK( 2947 ) = '        BODY611_RADII     = (   64.9    ' &
      //            '  57.3      53.0   )'
      PCK( 2948 ) = '        BODY612_RADII     = (   22.5    ' &
      //            '  19.6      13.3   )'
      PCK( 2949 ) = '        BODY613_RADII     = (   16.3    ' &
      //            '  11.8       9.8   )'
      PCK( 2950 ) = '        BODY614_RADII     = (   15.3    ' &
      //            '   9.3       6.3   )'
      PCK( 2951 ) = '        BODY615_RADII     = (   20.5    ' &
      //            '  17.8       9.4   )'
      PCK( 2952 ) = '        BODY616_RADII     = (   68.2    ' &
      //            '  41.6      28.2   )'
      PCK( 2953 ) = '        BODY617_RADII     = (   52.2    ' &
      //            '  40.8      31.5   )'
      PCK( 2954 ) = '        BODY618_RADII     = (   17.2    ' &
      //            '  15.4      10.4   )'
      PCK( 2955 ) = ' '
      PCK( 2956 ) = '        BODY632_RADII     = (    1.94   ' &
      //            '   1.29      1.21  )'
      PCK( 2957 ) = '        BODY633_RADII     = (    2.88   ' &
      //            '   2.08      1.8   )'
      PCK( 2958 ) = '        BODY634_RADII     = (    1.5    ' &
      //            '   1.2       1.0   )'
      PCK( 2959 ) = '        BODY635_RADII     = (    4.6    ' &
      //            '   4.5       2.8   )'
      PCK( 2960 ) = '        BODY649_RADII     = (    0.5    ' &
      //            '   0.5       0.5   )'
      PCK( 2961 ) = '        BODY653_RADII     = (    0.7    ' &
      //            '   0.25      0.2   )'
      PCK( 2962 ) = ' '
      PCK( 2963 ) = BEGTXT()
      PCK( 2964 ) = ' '
      PCK( 2965 ) = ' '
      PCK( 2966 ) = ' '
      PCK( 2967 ) = 'Satellites of Uranus'
      PCK( 2968 ) = ' '
      PCK( 2969 ) = '     Old values:'
      PCK( 2970 ) = ' '
      PCK( 2971 ) = '        Values are unchanged in the 2009' &
      //            ' IAU report.'
      PCK( 2972 ) = ' '
      PCK( 2973 ) = '     Current values:'
      PCK( 2974 ) = ' '
      PCK( 2975 ) = BEGDAT()
      PCK( 2976 ) = ' '
      PCK( 2977 ) = '        BODY701_RADII     = (  581.1   5' &
      //            '77.9   577.7 )'
      PCK( 2978 ) = '        BODY702_RADII     = (  584.7   5' &
      //            '84.7   584.7 )'
      PCK( 2979 ) = '        BODY703_RADII     = (  788.9   7' &
      //            '88.9   788.9 )'
      PCK( 2980 ) = '        BODY704_RADII     = (  761.4   7' &
      //            '61.4   761.4 )'
      PCK( 2981 ) = '        BODY705_RADII     = (  240.4   2' &
      //            '34.2   232.9 )'
      PCK( 2982 ) = ' '
      PCK( 2983 ) = BEGTXT()
      PCK( 2984 ) = ' '
      PCK( 2985 ) = '        The 2000 report gives only mean ' &
      //            'radii for satellites 706--715.'
      PCK( 2986 ) = ' '
      PCK( 2987 ) = BEGDAT()
      PCK( 2988 ) = ' '
      PCK( 2989 ) = '        BODY706_RADII     = (   13      ' &
      //            '13      13 )'
      PCK( 2990 ) = '        BODY707_RADII     = (   15      ' &
      //            '15      15 )'
      PCK( 2991 ) = '        BODY708_RADII     = (   21      ' &
      //            '21      21 )'
      PCK( 2992 ) = '        BODY709_RADII     = (   31      ' &
      //            '31      31 )'
      PCK( 2993 ) = '        BODY710_RADII     = (   27      ' &
      //            '27      27 )'
      PCK( 2994 ) = '        BODY711_RADII     = (   42      ' &
      //            '42      42 )'
      PCK( 2995 ) = '        BODY712_RADII     = (   54      ' &
      //            '54      54 )'
      PCK( 2996 ) = '        BODY713_RADII     = (   27      ' &
      //            '27      27 )'
      PCK( 2997 ) = '        BODY714_RADII     = (   33      ' &
      //            '33      33 )'
      PCK( 2998 ) = '        BODY715_RADII     = (   77      ' &
      //            '77      77 )'
      PCK( 2999 ) = ' '
      PCK( 3000 ) = BEGTXT()
      PCK( 3001 ) = ' '
      PCK( 3002 ) = ' '
      PCK( 3003 ) = ' '
      PCK( 3004 ) = ' '
      PCK( 3005 ) = 'Satellites of Neptune'
      PCK( 3006 ) = ' '
      PCK( 3007 ) = ' '
      PCK( 3008 ) = '     Old values:'
      PCK( 3009 ) = ' '
      PCK( 3010 ) = '        Values are unchanged in the 2009' &
      //            ' IAU report.'
      PCK( 3011 ) = ' '
      PCK( 3012 ) = '     Current values:'
      PCK( 3013 ) = ' '
      PCK( 3014 ) = '        The 2009 report gives mean radii' &
      //            ' only for bodies 801-806.'
      PCK( 3015 ) = ' '
      PCK( 3016 ) = BEGDAT()
      PCK( 3017 ) = ' '
      PCK( 3018 ) = '        BODY801_RADII     = ( 1352.6  13' &
      //            '52.6  1352.6 )'
      PCK( 3019 ) = '        BODY802_RADII     = (  170     1' &
      //            '70     170   )'
      PCK( 3020 ) = '        BODY803_RADII     = (   29      ' &
      //            '29     29    )'
      PCK( 3021 ) = '        BODY804_RADII     = (   40      ' &
      //            '40     40    )'
      PCK( 3022 ) = '        BODY805_RADII     = (   74      ' &
      //            '74     74    )'
      PCK( 3023 ) = '        BODY806_RADII     = (   79      ' &
      //            '79     79    )'
      PCK( 3024 ) = ' '
      PCK( 3025 ) = BEGTXT()
      PCK( 3026 ) = ' '
      PCK( 3027 ) = '        The second equatorial radius for' &
      //            ' Larissa is not given in the 2009'
      PCK( 3028 ) = '        report.  The available values ar' &
      //            'e:'
      PCK( 3029 ) = ' '
      PCK( 3030 ) = '            BODY807_RADII     = (   104 ' &
      //            '    ---     89   )'
      PCK( 3031 ) = ' '
      PCK( 3032 ) = '        For use within the SPICE system,' &
      //            ' we use only the mean radius.'
      PCK( 3033 ) = ' '
      PCK( 3034 ) = BEGDAT()
      PCK( 3035 ) = ' '
      PCK( 3036 ) = '        BODY807_RADII     = (   96      ' &
      //            '96     96   )'
      PCK( 3037 ) = '        BODY808_RADII     = (  218     2' &
      //            '08    201   )'
      PCK( 3038 ) = ' '
      PCK( 3039 ) = BEGTXT()
      PCK( 3040 ) = ' '
      PCK( 3041 ) = ' '
      PCK( 3042 ) = ' '
      PCK( 3043 ) = ' '
      PCK( 3044 ) = 'Satellites of Pluto'
      PCK( 3045 ) = ' '
      PCK( 3046 ) = ' '
      PCK( 3047 ) = '     Old values:'
      PCK( 3048 ) = ' '
      PCK( 3049 ) = '        Values are from the 2009 IAU rep' &
      //            'ort.'
      PCK( 3050 ) = ' '
      PCK( 3051 ) = '        BODY901_RADII     = (  605     6' &
      //            '05    605   )'
      PCK( 3052 ) = ' '
      PCK( 3053 ) = '     Current values:'
      PCK( 3054 ) = ' '
      PCK( 3055 ) = BEGDAT()
      PCK( 3056 ) = ' '
      PCK( 3057 ) = '        BODY901_RADII     = (  606     6' &
      //            '06    606   )'
      PCK( 3058 ) = ' '
      PCK( 3059 ) = BEGTXT()
      PCK( 3060 ) = ' '
      PCK( 3061 ) = ' '
      PCK( 3062 ) = ' '
      PCK( 3063 ) = 'Radii for Selected Comets and Asteroids'
      PCK( 3064 ) = '----------------------------------------' &
      //            '----------------'
      PCK( 3065 ) = ' '
      PCK( 3066 ) = ' '
      PCK( 3067 ) = ' '
      PCK( 3068 ) = ' '
      PCK( 3069 ) = ' '
      PCK( 3070 ) = 'Ceres'
      PCK( 3071 ) = ' '
      PCK( 3072 ) = '     Old values:'
      PCK( 3073 ) = ' '
      PCK( 3074 ) = '        Values are from the 2009 IAU rep' &
      //            'ort.'
      PCK( 3075 ) = ' '
      PCK( 3076 ) = '        body2000001_radii     = ( 487.3 ' &
      //            ' 487.3  454.7 )'
      PCK( 3077 ) = ' '
      PCK( 3078 ) = '     Current values:'
      PCK( 3079 ) = ' '
      PCK( 3080 ) = ' '
      PCK( 3081 ) = BEGDAT()
      PCK( 3082 ) = ' '
      PCK( 3083 ) = '        BODY2000001_RADII     = ( 487.3 ' &
      //            ' 487.3  446. )'
      PCK( 3084 ) = ' '
      PCK( 3085 ) = BEGTXT()
      PCK( 3086 ) = ' '
      PCK( 3087 ) = ' '
      PCK( 3088 ) = ' '
      PCK( 3089 ) = 'Vesta'
      PCK( 3090 ) = ' '
      PCK( 3091 ) = ' '
      PCK( 3092 ) = '     Old values:'
      PCK( 3093 ) = ' '
      PCK( 3094 ) = '        Values are unchanged in the 2015' &
      //            ' IAU report.'
      PCK( 3095 ) = ' '
      PCK( 3096 ) = '     Current values:'
      PCK( 3097 ) = ' '
      PCK( 3098 ) = ' '
      PCK( 3099 ) = BEGDAT()
      PCK( 3100 ) = ' '
      PCK( 3101 ) = '        BODY2000004_RADII     = ( 289.  ' &
      //            '280.  229.  )'
      PCK( 3102 ) = ' '
      PCK( 3103 ) = BEGTXT()
      PCK( 3104 ) = ' '
      PCK( 3105 ) = ' '
      PCK( 3106 ) = ' '
      PCK( 3107 ) = 'Psyche'
      PCK( 3108 ) = ' '
      PCK( 3109 ) = '     Current values:'
      PCK( 3110 ) = ' '
      PCK( 3111 ) = ' '
      PCK( 3112 ) = BEGDAT()
      PCK( 3113 ) = ' '
      PCK( 3114 ) = '        BODY2000016_RADII     = ( 139.5 ' &
      //            ' 116.  94.5  )'
      PCK( 3115 ) = ' '
      PCK( 3116 ) = BEGTXT()
      PCK( 3117 ) = ' '
      PCK( 3118 ) = ' '
      PCK( 3119 ) = ' '
      PCK( 3120 ) = 'Lutetia'
      PCK( 3121 ) = ' '
      PCK( 3122 ) = ' '
      PCK( 3123 ) = '     Current values:'
      PCK( 3124 ) = ' '
      PCK( 3125 ) = ' '
      PCK( 3126 ) = BEGDAT()
      PCK( 3127 ) = ' '
      PCK( 3128 ) = '        BODY2000021_RADII     = (  62.0 ' &
      //            '  50.5   46.5  )'
      PCK( 3129 ) = ' '
      PCK( 3130 ) = BEGTXT()
      PCK( 3131 ) = ' '
      PCK( 3132 ) = ' '
      PCK( 3133 ) = ' '
      PCK( 3134 ) = 'Europa'
      PCK( 3135 ) = ' '
      PCK( 3136 ) = ' '
      PCK( 3137 ) = '     Current values:'
      PCK( 3138 ) = ' '
      PCK( 3139 ) = ' '
      PCK( 3140 ) = BEGDAT()
      PCK( 3141 ) = ' '
      PCK( 3142 ) = '        BODY2000052_RADII     = (  189.5' &
      //            '  165.  124.5 )'
      PCK( 3143 ) = ' '
      PCK( 3144 ) = '        NAIF_BODY_NAME        += ''52 EU' &
      //            'ROPA'''
      PCK( 3145 ) = '        NAIF_BODY_CODE        += 2000052'
      PCK( 3146 ) = ' '
      PCK( 3147 ) = BEGTXT()
      PCK( 3148 ) = ' '
      PCK( 3149 ) = 'Ida'
      PCK( 3150 ) = ' '
      PCK( 3151 ) = ' '
      PCK( 3152 ) = '     Old values:'
      PCK( 3153 ) = ' '
      PCK( 3154 ) = '        Values are unchanged in the 2009' &
      //            ' IAU report.'
      PCK( 3155 ) = ' '
      PCK( 3156 ) = '     Current values:'
      PCK( 3157 ) = ' '
      PCK( 3158 ) = ' '
      PCK( 3159 ) = BEGDAT()
      PCK( 3160 ) = ' '
      PCK( 3161 ) = '        BODY2431010_RADII     = (   26.8' &
      //            '   12.0    7.6 )'
      PCK( 3162 ) = ' '
      PCK( 3163 ) = BEGTXT()
      PCK( 3164 ) = ' '
      PCK( 3165 ) = ' '
      PCK( 3166 ) = ' '
      PCK( 3167 ) = 'Mathilde'
      PCK( 3168 ) = ' '
      PCK( 3169 ) = ' '
      PCK( 3170 ) = '     Old values:'
      PCK( 3171 ) = ' '
      PCK( 3172 ) = '        Values are unchanged in the 2009' &
      //            ' IAU report.'
      PCK( 3173 ) = ' '
      PCK( 3174 ) = '     Current values:'
      PCK( 3175 ) = ' '
      PCK( 3176 ) = ' '
      PCK( 3177 ) = BEGDAT()
      PCK( 3178 ) = ' '
      PCK( 3179 ) = '        BODY2000253_RADII     = (  33.  ' &
      //            ' 24.   23.  )'
      PCK( 3180 ) = ' '
      PCK( 3181 ) = BEGTXT()
      PCK( 3182 ) = ' '
      PCK( 3183 ) = ' '
      PCK( 3184 ) = ' '
      PCK( 3185 ) = 'Eros'
      PCK( 3186 ) = ' '
      PCK( 3187 ) = ' '
      PCK( 3188 ) = '     Old values:'
      PCK( 3189 ) = ' '
      PCK( 3190 ) = '        Values are unchanged in the 2009' &
      //            ' IAU report.'
      PCK( 3191 ) = ' '
      PCK( 3192 ) = '     Current values:'
      PCK( 3193 ) = ' '
      PCK( 3194 ) = ' '
      PCK( 3195 ) = BEGDAT()
      PCK( 3196 ) = ' '
      PCK( 3197 ) = '        BODY2000433_RADII     = (  17.0 ' &
      //            '  5.5   5.5  )'
      PCK( 3198 ) = ' '
      PCK( 3199 ) = BEGTXT()
      PCK( 3200 ) = ' '
      PCK( 3201 ) = ' '
      PCK( 3202 ) = ' '
      PCK( 3203 ) = 'Davida'
      PCK( 3204 ) = ' '
      PCK( 3205 ) = ' '
      PCK( 3206 ) = '     Current values:'
      PCK( 3207 ) = ' '
      PCK( 3208 ) = ' '
      PCK( 3209 ) = BEGDAT()
      PCK( 3210 ) = ' '
      PCK( 3211 ) = '        BODY2000511_RADII     = (  180. ' &
      //            '  147.   127.  )'
      PCK( 3212 ) = ' '
      PCK( 3213 ) = BEGTXT()
      PCK( 3214 ) = ' '
      PCK( 3215 ) = ' '
      PCK( 3216 ) = ' '
      PCK( 3217 ) = 'Gaspra'
      PCK( 3218 ) = ' '
      PCK( 3219 ) = ' '
      PCK( 3220 ) = '     Old values:'
      PCK( 3221 ) = ' '
      PCK( 3222 ) = '        Values are unchanged in the 2009' &
      //            ' IAU report.'
      PCK( 3223 ) = ' '
      PCK( 3224 ) = '     Current values:'
      PCK( 3225 ) = ' '
      PCK( 3226 ) = ' '
      PCK( 3227 ) = BEGDAT()
      PCK( 3228 ) = ' '
      PCK( 3229 ) = '        BODY9511010_RADII     = (    9.1' &
      //            '    5.2    4.4 )'
      PCK( 3230 ) = ' '
      PCK( 3231 ) = BEGTXT()
      PCK( 3232 ) = ' '
      PCK( 3233 ) = ' '
      PCK( 3234 ) = ' '
      PCK( 3235 ) = 'Steins'
      PCK( 3236 ) = ' '
      PCK( 3237 ) = ' '
      PCK( 3238 ) = '     Current values:'
      PCK( 3239 ) = ' '
      PCK( 3240 ) = ' '
      PCK( 3241 ) = BEGDAT()
      PCK( 3242 ) = ' '
      PCK( 3243 ) = '        BODY2002867_RADII     = (  3.24 ' &
      //            '    2.73     2.04  )'
      PCK( 3244 ) = ' '
      PCK( 3245 ) = BEGTXT()
      PCK( 3246 ) = ' '
      PCK( 3247 ) = ' '
      PCK( 3248 ) = ' '
      PCK( 3249 ) = 'Toutatis'
      PCK( 3250 ) = ' '
      PCK( 3251 ) = ' '
      PCK( 3252 ) = '     Old values:'
      PCK( 3253 ) = ' '
      PCK( 3254 ) = '        Values are unchanged in the 2009' &
      //            ' IAU report.'
      PCK( 3255 ) = ' '
      PCK( 3256 ) = '     Current values:'
      PCK( 3257 ) = ' '
      PCK( 3258 ) = ' '
      PCK( 3259 ) = BEGDAT()
      PCK( 3260 ) = ' '
      PCK( 3261 ) = '        BODY2004179_RADII     = (  2.13 ' &
      //            ' 1.015  0.85  )'
      PCK( 3262 ) = ' '
      PCK( 3263 ) = BEGTXT()
      PCK( 3264 ) = ' '
      PCK( 3265 ) = ' '
      PCK( 3266 ) = ' '
      PCK( 3267 ) = 'Itokawa'
      PCK( 3268 ) = ' '
      PCK( 3269 ) = ' '
      PCK( 3270 ) = '     Old values:'
      PCK( 3271 ) = ' '
      PCK( 3272 ) = '        Values are from the 2009 IAU rep' &
      //            'ort. Note that the'
      PCK( 3273 ) = '        diameters rather than radii were' &
      //            ' given.'
      PCK( 3274 ) = ' '
      PCK( 3275 ) = '        body2025143_radii     = (  0.535' &
      //            '   0.294   0.209  )'
      PCK( 3276 ) = ' '
      PCK( 3277 ) = '     Current values:'
      PCK( 3278 ) = ' '
      PCK( 3279 ) = ' '
      PCK( 3280 ) = BEGDAT()
      PCK( 3281 ) = ' '
      PCK( 3282 ) = '        BODY2025143_RADII     = (  0.268' &
      //            '   0.147   0.104  )'
      PCK( 3283 ) = ' '
      PCK( 3284 ) = BEGTXT()
      PCK( 3285 ) = ' '
      PCK( 3286 ) = ' '
      PCK( 3287 ) = 'Kleopatra'
      PCK( 3288 ) = ' '
      PCK( 3289 ) = ' '
      PCK( 3290 ) = '     Old values:'
      PCK( 3291 ) = ' '
      PCK( 3292 ) = '        Values are from the 2003 report.'
      PCK( 3293 ) = ' '
      PCK( 3294 ) = ' '
      PCK( 3295 ) = '        body2000216_radii     = (   108.' &
      //            '5      47    40.5  )'
      PCK( 3296 ) = ' '
      PCK( 3297 ) = ' '
      PCK( 3298 ) = '     Current values:'
      PCK( 3299 ) = ' '
      PCK( 3300 ) = ' '
      PCK( 3301 ) = '        No values are provided in the 20' &
      //            '09 report.'
      PCK( 3302 ) = ' '
      PCK( 3303 ) = ' '
      PCK( 3304 ) = ' '
      PCK( 3305 ) = ' '
      PCK( 3306 ) = ' '
      PCK( 3307 ) = 'Halley'
      PCK( 3308 ) = ' '
      PCK( 3309 ) = ' '
      PCK( 3310 ) = '     Old values:'
      PCK( 3311 ) = ' '
      PCK( 3312 ) = '        Values are unchanged in the 2009' &
      //            ' IAU report.'
      PCK( 3313 ) = ' '
      PCK( 3314 ) = '     Current values:'
      PCK( 3315 ) = ' '
      PCK( 3316 ) = BEGDAT()
      PCK( 3317 ) = ' '
      PCK( 3318 ) = '        BODY1000036_RADII     = (  8.0  ' &
      //            ' 4.0   4.0  )'
      PCK( 3319 ) = ' '
      PCK( 3320 ) = BEGTXT()
      PCK( 3321 ) = ' '
      PCK( 3322 ) = ' '
      PCK( 3323 ) = ' '
      PCK( 3324 ) = '9P/Tempel 1'
      PCK( 3325 ) = ' '
      PCK( 3326 ) = ' '
      PCK( 3327 ) = '     Old values:'
      PCK( 3328 ) = ' '
      PCK( 3329 ) = '        The effective radius is unchange' &
      //            'd in the 2009 IAU report.'
      PCK( 3330 ) = ' '
      PCK( 3331 ) = '     Current values:'
      PCK( 3332 ) = ' '
      PCK( 3333 ) = ' '
      PCK( 3334 ) = '        The value in the data assignment' &
      //            ' below is the'
      PCK( 3335 ) = '        "effective radius."'
      PCK( 3336 ) = ' '
      PCK( 3337 ) = '        According to [1]:'
      PCK( 3338 ) = ' '
      PCK( 3339 ) = '           The maximum and minimum radii' &
      //            ' are not properly'
      PCK( 3340 ) = '           the values of the principal s' &
      //            'emi-axes, they'
      PCK( 3341 ) = '           are half the maximum and mini' &
      //            'mum values of the'
      PCK( 3342 ) = '           diameter. Due to the large de' &
      //            'viations from a'
      PCK( 3343 ) = '           simple ellipsoid, they may no' &
      //            't correspond with'
      PCK( 3344 ) = '           measurements along the princi' &
      //            'pal axes, or be'
      PCK( 3345 ) = '           orthogonal to each other.'
      PCK( 3346 ) = ' '
      PCK( 3347 ) = BEGDAT()
      PCK( 3348 ) = ' '
      PCK( 3349 ) = '        BODY1000093_RADII     = (  3.0  ' &
      //            ' 3.0   3.0  )'
      PCK( 3350 ) = ' '
      PCK( 3351 ) = BEGTXT()
      PCK( 3352 ) = ' '
      PCK( 3353 ) = ' '
      PCK( 3354 ) = '19P/Borrelly'
      PCK( 3355 ) = ' '
      PCK( 3356 ) = ' '
      PCK( 3357 ) = '     Old values:'
      PCK( 3358 ) = ' '
      PCK( 3359 ) = '        Values are unchanged in the 2015' &
      //            ' report.'
      PCK( 3360 ) = ' '
      PCK( 3361 ) = ' '
      PCK( 3362 ) = '     Current values:'
      PCK( 3363 ) = ' '
      PCK( 3364 ) = ' '
      PCK( 3365 ) = '        The value in the data assignment' &
      //            ' below is the'
      PCK( 3366 ) = '        "effective radius."'
      PCK( 3367 ) = ' '
      PCK( 3368 ) = '        The first principal axis length ' &
      //            'is'
      PCK( 3369 ) = ' '
      PCK( 3370 ) = '           3.5 km'
      PCK( 3371 ) = ' '
      PCK( 3372 ) = '        The lengths of the other semi-ax' &
      //            'es are not provided'
      PCK( 3373 ) = '        by [1].'
      PCK( 3374 ) = ' '
      PCK( 3375 ) = BEGDAT()
      PCK( 3376 ) = ' '
      PCK( 3377 ) = '        BODY1000005_RADII     = (  4.22 ' &
      //            '  4.22   4.22  )'
      PCK( 3378 ) = ' '
      PCK( 3379 ) = BEGTXT()
      PCK( 3380 ) = ' '
      PCK( 3381 ) = ' '
      PCK( 3382 ) = ' '
      PCK( 3383 ) = '81P/Wild 2'
      PCK( 3384 ) = ' '
      PCK( 3385 ) = ' '
      PCK( 3386 ) = '     Old values:'
      PCK( 3387 ) = ' '
      PCK( 3388 ) = '        Values are unchanged in the 2009' &
      //            ' report.'
      PCK( 3389 ) = ' '
      PCK( 3390 ) = '     Current values:'
      PCK( 3391 ) = ' '
      PCK( 3392 ) = ' '
      PCK( 3393 ) = BEGDAT()
      PCK( 3394 ) = ' '
      PCK( 3395 ) = '        BODY1000107_RADII     = (  2.7  ' &
      //            ' 1.9   1.5 )'
      PCK( 3396 ) = ' '
      PCK( 3397 ) = BEGTXT()
      PCK( 3398 ) = ' '
      PCK( 3399 ) = ' '
      PCK( 3400 ) = '67P/Churyumov-Gerasimenko'
      PCK( 3401 ) = ' '
      PCK( 3402 ) = ' '
      PCK( 3403 ) = BEGDAT()
      PCK( 3404 ) = ' '
      PCK( 3405 ) = '        BODY1000012_RADII     = (  2.40 ' &
      //            ' 1.55  1.20 )'
      PCK( 3406 ) = ' '
      PCK( 3407 ) = BEGTXT()
      PCK( 3408 ) = ' '
      PCK( 3409 ) = ' '
      PCK( 3410 ) = ' '
      PCK( 3411 ) = '103P/Hartley 2'
      PCK( 3412 ) = ' '
      PCK( 3413 ) = ' '
      PCK( 3414 ) = BEGDAT()
      PCK( 3415 ) = ' '
      PCK( 3416 ) = '        BODY1000041_RADII     = ( 0.58 0' &
      //            '.58 0.58 )'
      PCK( 3417 ) = ' '
      PCK( 3418 ) = BEGTXT()
      PCK( 3419 ) = ' '

!
!     Add data for "body -10."
!
      S = 3419

      PCK( S+1  ) = BEGDAT()
      PCK( S+2  ) = 'BODY-10_CONSTANTS_REF_FRAME = 2'
      PCK( S+3  ) = 'BODY-10_CONSTANTS_JED_EPOCH = 2433282.42345905D0'
      PCK( S+4  ) = ' '
      PCK( S+5  ) = 'BODY-10_POLE_RA         = (  286.13       ' &
      //          '0.          0. )'
      PCK( S+6  ) = 'BODY-10_POLE_DEC        = (   63.87       ' &
      //          '0.          0. )'
      PCK( S+7  ) = 'BODY-10_PM              = (   84.176     1' &
      //          '4.18440     0. )'
      PCK( S+8  ) = 'BODY-10_LONG_AXIS       = (  459.00       ' &
      //          '               )'
      PCK( S+9  ) = ' '
      PCK( S+10 ) = BEGTXT()
      PCK( S+11 ) = ' '





      CALL TXTOPN ( NAMEPC, UNIT )

      DO I = 1, NLINES
         R =     RTRIM ( PCK(I) )

         CALL WRITLN ( PCK(I)(1:R), UNIT )
      END DO

      CLOSE ( UNIT )


!
!     If this file needs to be loaded.  Do it now.  If not we are
!     done and can return.
!
      IF ( LOADPC ) THEN

         CALL LDPOOL ( NAMEPC )

         IF ( KEEPPC ) THEN
!
!           If we are keeping this file, we need to register it
!           with FILREG.
!
            CALL TFILES ( NAMEPC )
            RETURN
         END IF

      ELSE
!
!        We are keeping this file, so we need to register it
!        with FILREG.
!
         CALL TFILES ( NAMEPC )
         RETURN
      END IF


      CALL KILFIL ( NAMEPC )

      RETURN
      END
