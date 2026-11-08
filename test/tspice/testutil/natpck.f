C$Procedure   NATPCK (Create a text PCK for Nat's solar system)
 
      SUBROUTINE NATPCK ( NAMEPC, LOADPC, KEEPPC )
 
C$ Abstract
C
C     Create and if appropriate load a text PCK kernel for Nat's
C     solar system.
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
C     TESTING
C
C$ Declarations
 
      IMPLICIT NONE
      CHARACTER*(*)         NAMEPC
      LOGICAL               LOADPC
      LOGICAL               KEEPPC
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NAMEPC     I   The name of the PC-kernel to create.
C     LOADPC     I   Load the PC-kernel if .TRUE.
C     KEEPPC     I   Keep the PC-kernel if .TRUE., else delete it.
C
C$ Detailed_Input
C
C     NAMEPC      is the name of a PCK to create and load if LOADPC is
C                 set to .TRUE.  If a PCK of the same name already
C                 exists it is deleted.
C
C     LOADPC      is a logical that indicates whether or not the PCK
C                 file should be loaded after it is created.  If LOADPC
C                 has the value .TRUE. the PCK is loaded after it is
C                 created.  Otherwise it is left un-opened.
C
C     KEEPPC      is a logical that indicates whether or not the PCK
C                 file should be deleted after it is loaded.  If KEEPPC
C                 is .TRUE. the file is not deleted.  If KEEPPC is
C                 .FALSE. the file is deleted after it is loaded.
C
C                 Note this behavior is different from that of TSTPCK,
C                 which always keeps the file if the file is not loaded,
C                 regardless of the value of KEEPPC.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) Any I/O errors occurring in the creation of the PCK file
C        will be signaled by routines in the call tree of this
C        routine.  
C
C     Since this routine is normally used within the TSPICE system,
C     it's up the the caller to call CHCKXC to catch errors signaled by
C     this routine.
C 
C$ Files
C
C     This routine creates a PCK file meant for use with the SPK
C     file created by the test utility NATSPK.  If that routine
C     is changed, the body radii defined here may need to be
C     changed as well.  NATSPK contains commented-out lines of code
C     that generate appropriate body radii.
C
C     The PCK created by this routine contains
C
C        - Name-ID mappings for bodies ALPHA and BETA
C
C        - Body radii for bodies ALPHA and BETA
C
C        - Rotational elements for bodies ALPHA and BETA
C
C        - PCK frame definitions for the body-fixed frames 
C
C             ALPHAFIXED
C             BETAFIXED
C
C        - Associations of the bodyfixed frames with the
C          bodies ALPHA and BETA
C
C        - GM and radii for the "sun" of this solar system.  The
C          GM value is computed by NATSPK; normally the lines of
C          code that write out the value are commented out.
C
C          The smaller radii of the sun are provided because the
C          original values make the sun too large relative to the
C          orbital radii of the planets.
C
C        - View frames for bodies Alpha and Beta. These frames
C          have a primary axis that points toward the center
C          of the defining object; the secondary vector points
C          in the velocity direction.
C
C          There are two kinds of view frames for Alpha: those
C          with Alpha's orbit in the frame's X-Y plane and those
C          with Alpha's orbit in the frame's X-Z plane. The first
C          group has variants that use different aberration corrections
C          for the defining position and velocity vectors. All 
C          light time corrections are converged Newtonian.
C
C          For Beta, the only frames are based on geometric vectors.
C          
C          The view frames' names are:
C
C             'ALPHA_VIEW_XY'
C             'ALPHA_VIEW_XY_CN'
C             'ALPHA_VIEW_XY_CNS'
C             'ALPHA_VIEW_XY_XCN'
C             'ALPHA_VIEW_XY_XCNS'
C             'ALPHA_VIEW_XZ'
C             'ALPHA_VIEW_XZ_CN'
C             'ALPHA_VIEW_XZ_CNS'
C             'ALPHA_VIEW_XZ_XCN'
C             'ALPHA_VIEW_XZ_XCNS'
C             'BETA_VIEW_XY'
C             'BETA_VIEW_XZ'
C
C
C$ Particulars
C
C     This routine creates a planetary constants file for use in
C     testing.
C
C$ Examples
C
C     The normal way to use this routine is shown below.
C
C     CALL NATSPK ( 'nat.bsp', .TRUE., HANDLE  )
C     CALL NATPCK ( 'nat.tpc', .TRUE., .FALSE. )
C
C        [perform some tests and computations]
C
C     CALL SPKUEF ( HANDLE )
C     CALL KILFIL ( 'nat.bsp' )
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
C     N.J. Bachman    (JPL)
C
C$ Version
C
C-    Test Utilities 4.0.0, 17-JUL-2008 (NJB)
C     
C        Dynamic view frames for Alpha and Beta were added.
C
C-    Test Utilities 3.0.0, 14-APR-2008 (NJB)
C
C        Kernel loading is now performed using LDPOOL. 
C        Header was updated to satisfy checkit.
C
C-    Test Utilities 2.0.0, 11-NOV-2007 (NJB)
C
C        Changed polar radii of bodies Alpha and Beta to 
C        make shape of Alpha prolate and shape of Beta
C        oblate. Changed body-fixed frame of alpha so that
C        axes are permuted relative to those of Beta.
C
C        If the PCK is loaded, it is now loaded via FURNSH.
C
C-    Test Utilities 1.0.0, 29-SEP-2004 (NJB)
C
C-&
 
C$ Index_Entries
C
C     Create a "Nat's solar system" PCK file
C
C-&
 
 
C
C     Test Utility Functions
C
      CHARACTER*(16)        BEGDAT
      CHARACTER*(16)        BEGTXT
 
C
C     Local Parameters
C
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )
 
      INTEGER               NLINES
      PARAMETER           ( NLINES = 393 )
 
C
C     Local Variables
C
      CHARACTER*(LNSIZE)    PCK  ( NLINES )
 
      INTEGER               I
      INTEGER               UNIT
 


      CALL CHKIN ( 'NATPCK' )

C
C     Delete any existing file of the same name.
C
      CALL KILFIL ( NAMEPC )
 
C
C     Fill the PCK buffer with the PCK kernel variable assignments.
C
 
      PCK(  1 ) = BEGDAT()
      PCK(  2 ) = ' '
      PCK(  3 ) = '      NAIF_BODY_NAME += ( ''ALPHA'', ''BET'
     .//          'A'',  ''GAMMA'' )'
      PCK(  4 ) = '      NAIF_BODY_CODE += ( 1000,     2000, '
     .//          '  1001    )'
      PCK(  5 ) = ' '
      PCK(  6 ) = BEGTXT()
      PCK(  7 ) = ' '
      PCK(  8 ) = '   Radii for'
      PCK(  9 ) = ' '
      PCK( 10 ) = '      ALPHA'
      PCK( 11 ) = '      BETA'
      PCK( 12 ) = '      GAMMA'
      PCK( 13 ) = ' '
      PCK( 14 ) = BEGDAT()
      PCK( 15 ) = ' '
      PCK( 16 ) = '      BODY1000_RADII = ( 0.732493975338754'
     .//          '24E+05,'
      PCK( 17 ) = '                         0.366246987669377'
     .//          '12E+05,'
      PCK( 18 ) = '                         0.366246987669377'
     .//          '12E+05 )'
      PCK( 19 ) = ' '
      PCK( 20 ) = '      BODY1001_RADII = ( 1.E4, 1.E4, 1.E4 '
     .//          ')'
      PCK( 21 ) = ' '
      PCK( 22 ) = '      BODY2000_RADII = ( 0.228915262710469'
     .//          '37E+04,'
      PCK( 23 ) = '                         0.228915262710469'
     .//          '37E+04,'
      PCK( 24 ) = '                         0.1E+04 )'
      PCK( 25 ) = ' '
      PCK( 26 ) = BEGTXT()
      PCK( 27 ) = ' '
      PCK( 28 ) = '   Orientation data for'
      PCK( 29 ) = ' '
      PCK( 30 ) = '      ALPHA'
      PCK( 31 ) = '      BETA'
      PCK( 32 ) = '      GAMMA'
      PCK( 33 ) = ' '
      PCK( 34 ) = BEGDAT()
      PCK( 35 ) = ' '
      PCK( 36 ) = '      BODY1000_POLE_RA        = (    0.   '
     .//          '    0.         0. )'
      PCK( 37 ) = '      BODY1000_POLE_DEC       = (  +90.   '
     .//          '    0.         0. )'
      PCK( 38 ) = '      BODY1000_PM             = (    0.   '
     .//          '    0.         0. )'
      PCK( 39 ) = ' '
      PCK( 40 ) = '      BODY1001_POLE_RA        = ( +180.   '
     .//          '    0.         0. )'
      PCK( 41 ) = '      BODY1001_POLE_DEC       = (    0.   '
     .//          '    0.         0. )'
      PCK( 42 ) = '      BODY1001_PM             = ( +180.   '
     .//          '    360.       0. )'
      PCK( 43 ) = ' '
      PCK( 44 ) = '      BODY2000_POLE_RA        = (    0.   '
     .//          '    0.         0. )'
      PCK( 45 ) = '      BODY2000_POLE_DEC       = (  +90.   '
     .//          '    0.         0. )'
      PCK( 46 ) = '      BODY2000_PM             = (    0.   '
     .//          '    0.         0. )'
      PCK( 47 ) = ' '
      PCK( 48 ) = ' '
      PCK( 49 ) = BEGTXT()
      PCK( 50 ) = ' '
      PCK( 51 ) = '   Body-fixed frame specifications for'
      PCK( 52 ) = ' '
      PCK( 53 ) = '      ALPHA'
      PCK( 54 ) = '      BETA'
      PCK( 55 ) = '      GAMMA'
      PCK( 56 ) = ' '
      PCK( 57 ) = BEGDAT()
      PCK( 58 ) = ' '
      PCK( 59 ) = '      FRAME_ALPHAFIXED          =  1000001'
      PCK( 60 ) = '      FRAME_1000001_NAME        = ''ALPHAF'
     .//          'IXED'''
      PCK( 61 ) = '      FRAME_1000001_CLASS       =  4'
      PCK( 62 ) = '      FRAME_1000001_CLASS_ID    =  1000001'
      PCK( 63 ) = '      FRAME_1000001_CENTER      =  1000'
      PCK( 64 ) = '      TKFRAME_1000001_RELATIVE  = ''BETAFI'
     .//          'XED'''
      PCK( 65 ) = '      TKFRAME_1000001_SPEC      = ''MATRIX'
     .//          ''''
      PCK( 66 ) = '      TKFRAME_1000001_MATRIX    = ( 0, 0, '
     .//          '1,'
      PCK( 67 ) = '                                    1, 0, '
     .//          '0,'
      PCK( 68 ) = '                                    0, 1, '
     .//          '0 )'
      PCK( 69 ) = ' '
      PCK( 70 ) = '      OBJECT_1000_FRAME       = ''ALPHAFIX'
     .//          'ED'''
      PCK( 71 ) = ' '
      PCK( 72 ) = ' '
      PCK( 73 ) = ' '
      PCK( 74 ) = '      FRAME_BETAFIXED         =  1000002'
      PCK( 75 ) = '      FRAME_1000002_NAME      = ''BETAFIXE'
     .//          'D'''
      PCK( 76 ) = '      FRAME_1000002_CLASS     =  2'
      PCK( 77 ) = '      FRAME_1000002_CLASS_ID  =  2000'
      PCK( 78 ) = '      FRAME_1000002_CENTER    =  2000'
      PCK( 79 ) = ' '
      PCK( 80 ) = '      OBJECT_2000_FRAME       = ''BETAFIXE'
     .//          'D'''
      PCK( 81 ) = ' '
      PCK( 82 ) = ' '
      PCK( 83 ) = '      FRAME_GAMMAFIXED        =  1000003'
      PCK( 84 ) = '      FRAME_1000003_NAME      = ''GAMMAFIX'
     .//          'ED'''
      PCK( 85 ) = '      FRAME_1000003_CLASS     =  2'
      PCK( 86 ) = '      FRAME_1000003_CLASS_ID  =  1001'
      PCK( 87 ) = '      FRAME_1000003_CENTER    =  1001'
      PCK( 88 ) = ' '
      PCK( 89 ) = '      OBJECT_1001_FRAME       = ''GAMMAFIX'
     .//          'ED'''
      PCK( 90 ) = ' '
      PCK( 91 ) = BEGTXT()
      PCK( 92 ) = ' '
      PCK( 93 ) = '      View frames for body ALPHA relative '
     .//          'to the Sun'
      PCK( 94 ) = ' '
      PCK( 95 ) = '         ALPHA_VIEW_XY     orbital motion '
     .//          'of ALPHA lies in X-Y plane'
      PCK( 96 ) = '                           of this frame.'
      PCK( 97 ) = ' '
      PCK( 98 ) = '         ALPHA_VIEW_XZ     orbital motion '
     .//          'of ALPHA lies in X-Z plane'
      PCK( 99 ) = '                           of this frame.'
      PCK( 100 ) = ' '
      PCK( 101 ) = BEGDAT()
      PCK( 102 ) = ' '
      PCK( 103 ) = '      FRAME_ALPHA_VIEW_XY         =  1700'
     .//           '000'
      PCK( 104 ) = '      FRAME_1700000_NAME           = ''AL'
     .//           'PHA_VIEW_XY'''
      PCK( 105 ) = '      FRAME_1700000_CLASS          =  5'
      PCK( 106 ) = '      FRAME_1700000_CLASS_ID       =  170'
     .//           '0000'
      PCK( 107 ) = '      FRAME_1700000_CENTER         = ''AL'
     .//           'PHA'''
      PCK( 108 ) = '      FRAME_1700000_RELATIVE       = ''J2'
     .//           '000'''
      PCK( 109 ) = '      FRAME_1700000_DEF_STYLE      = ''PA'
     .//           'RAMETERIZED'''
      PCK( 110 ) = '      FRAME_1700000_FAMILY         = ''TW'
     .//           'O-VECTOR'''
      PCK( 111 ) = '      FRAME_1700000_PRI_AXIS       = ''X'''
      PCK( 112 ) = '      FRAME_1700000_PRI_VECTOR_DEF = ''OB'
     .//           'SERVER_TARGET_POSITION'''
      PCK( 113 ) = '      FRAME_1700000_PRI_OBSERVER   = ''SU'
     .//           'N'''
      PCK( 114 ) = '      FRAME_1700000_PRI_TARGET     = ''AL'
     .//           'PHA'''
      PCK( 115 ) = '      FRAME_1700000_PRI_ABCORR     = ''NO'
     .//           'NE'''
      PCK( 116 ) = '      FRAME_1700000_SEC_AXIS       = ''Y'''
      PCK( 117 ) = '      FRAME_1700000_SEC_VECTOR_DEF = ''OB'
     .//           'SERVER_TARGET_VELOCITY'''
      PCK( 118 ) = '      FRAME_1700000_SEC_OBSERVER   = ''SU'
     .//           'N'''
      PCK( 119 ) = '      FRAME_1700000_SEC_TARGET     = ''AL'
     .//           'PHA'''
      PCK( 120 ) = '      FRAME_1700000_SEC_ABCORR     = ''NO'
     .//           'NE'''
      PCK( 121 ) = '      FRAME_1700000_SEC_FRAME      = ''J2'
     .//           '000'''
      PCK( 122 ) = ' '
      PCK( 123 ) = ' '
      PCK( 124 ) = '      FRAME_ALPHA_VIEW_XZ         =  1700'
     .//           '001'
      PCK( 125 ) = '      FRAME_1700001_NAME           = ''AL'
     .//           'PHA_VIEW_XZ'''
      PCK( 126 ) = '      FRAME_1700001_CLASS          =  5'
      PCK( 127 ) = '      FRAME_1700001_CLASS_ID       =  170'
     .//           '0001'
      PCK( 128 ) = '      FRAME_1700001_CENTER         = ''AL'
     .//           'PHA'''
      PCK( 129 ) = '      FRAME_1700001_RELATIVE       = ''J2'
     .//           '000'''
      PCK( 130 ) = '      FRAME_1700001_DEF_STYLE      = ''PA'
     .//           'RAMETERIZED'''
      PCK( 131 ) = '      FRAME_1700001_FAMILY         = ''TW'
     .//           'O-VECTOR'''
      PCK( 132 ) = '      FRAME_1700001_PRI_AXIS       = ''X'''
      PCK( 133 ) = '      FRAME_1700001_PRI_VECTOR_DEF = ''OB'
     .//           'SERVER_TARGET_POSITION'''
      PCK( 134 ) = '      FRAME_1700001_PRI_OBSERVER   = ''SU'
     .//           'N'''
      PCK( 135 ) = '      FRAME_1700001_PRI_TARGET     = ''AL'
     .//           'PHA'''
      PCK( 136 ) = '      FRAME_1700001_PRI_ABCORR     = ''NO'
     .//           'NE'''
      PCK( 137 ) = '      FRAME_1700001_SEC_AXIS       = ''Z'''
      PCK( 138 ) = '      FRAME_1700001_SEC_VECTOR_DEF = ''OB'
     .//           'SERVER_TARGET_VELOCITY'''
      PCK( 139 ) = '      FRAME_1700001_SEC_OBSERVER   = ''SU'
     .//           'N'''
      PCK( 140 ) = '      FRAME_1700001_SEC_TARGET     = ''AL'
     .//           'PHA'''
      PCK( 141 ) = '      FRAME_1700001_SEC_ABCORR     = ''NO'
     .//           'NE'''
      PCK( 142 ) = '      FRAME_1700001_SEC_FRAME      = ''J2'
     .//           '000'''
      PCK( 143 ) = ' '
      PCK( 144 ) = BEGTXT()
      PCK( 145 ) = ' '
      PCK( 146 ) = '      Aberration corrected view frames fo'
     .//           'r body ALPHA relative to the Sun.'
      PCK( 147 ) = '      Variants of both'
      PCK( 148 ) = ' '
      PCK( 149 ) = '         ALPHA_VIEW_XY'
      PCK( 150 ) = '         ALPHA_VIEW_XZ'
      PCK( 151 ) = ' '
      PCK( 152 ) = '      are supported. All light time corre'
     .//           'ctions are converged Newtonian.'
      PCK( 153 ) = ' '
      PCK( 154 ) = BEGDAT()
      PCK( 155 ) = ' '
      PCK( 156 ) = '      FRAME_ALPHA_VIEW_XY_CN       =  170'
     .//           '0002'
      PCK( 157 ) = '      FRAME_1700002_NAME           = ''AL'
     .//           'PHA_VIEW_XY_CN'''
      PCK( 158 ) = '      FRAME_1700002_CLASS          =  5'
      PCK( 159 ) = '      FRAME_1700002_CLASS_ID       =  170'
     .//           '0002'
      PCK( 160 ) = '      FRAME_1700002_CENTER         = ''AL'
     .//           'PHA'''
      PCK( 161 ) = '      FRAME_1700002_RELATIVE       = ''J2'
     .//           '000'''
      PCK( 162 ) = '      FRAME_1700002_DEF_STYLE      = ''PA'
     .//           'RAMETERIZED'''
      PCK( 163 ) = '      FRAME_1700002_FAMILY         = ''TW'
     .//           'O-VECTOR'''
      PCK( 164 ) = '      FRAME_1700002_PRI_AXIS       = ''X'''
      PCK( 165 ) = '      FRAME_1700002_PRI_VECTOR_DEF = ''OB'
     .//           'SERVER_TARGET_POSITION'''
      PCK( 166 ) = '      FRAME_1700002_PRI_OBSERVER   = ''SU'
     .//           'N'''
      PCK( 167 ) = '      FRAME_1700002_PRI_TARGET     = ''AL'
     .//           'PHA'''
      PCK( 168 ) = '      FRAME_1700002_PRI_ABCORR     = ''CN'
     .//           ''''
      PCK( 169 ) = '      FRAME_1700002_SEC_AXIS       = ''Y'''
      PCK( 170 ) = '      FRAME_1700002_SEC_VECTOR_DEF = ''OB'
     .//           'SERVER_TARGET_VELOCITY'''
      PCK( 171 ) = '      FRAME_1700002_SEC_OBSERVER   = ''SU'
     .//           'N'''
      PCK( 172 ) = '      FRAME_1700002_SEC_TARGET     = ''AL'
     .//           'PHA'''
      PCK( 173 ) = '      FRAME_1700002_SEC_ABCORR     = ''CN'
     .//           ''''
      PCK( 174 ) = '      FRAME_1700002_SEC_FRAME      = ''J2'
     .//           '000'''
      PCK( 175 ) = ' '
      PCK( 176 ) = '      FRAME_ALPHA_VIEW_XY_CNS      =  170'
     .//           '0003'
      PCK( 177 ) = '      FRAME_1700003_NAME           = ''AL'
     .//           'PHA_VIEW_XY_CNS'''
      PCK( 178 ) = '      FRAME_1700003_CLASS          =  5'
      PCK( 179 ) = '      FRAME_1700003_CLASS_ID       =  170'
     .//           '0003'
      PCK( 180 ) = '      FRAME_1700003_CENTER         = ''AL'
     .//           'PHA'''
      PCK( 181 ) = '      FRAME_1700003_RELATIVE       = ''J2'
     .//           '000'''
      PCK( 182 ) = '      FRAME_1700003_DEF_STYLE      = ''PA'
     .//           'RAMETERIZED'''
      PCK( 183 ) = '      FRAME_1700003_FAMILY         = ''TW'
     .//           'O-VECTOR'''
      PCK( 184 ) = '      FRAME_1700003_PRI_AXIS       = ''X'''
      PCK( 185 ) = '      FRAME_1700003_PRI_VECTOR_DEF = ''OB'
     .//           'SERVER_TARGET_POSITION'''
      PCK( 186 ) = '      FRAME_1700003_PRI_OBSERVER   = ''SU'
     .//           'N'''
      PCK( 187 ) = '      FRAME_1700003_PRI_TARGET     = ''AL'
     .//           'PHA'''
      PCK( 188 ) = '      FRAME_1700003_PRI_ABCORR     = ''CN'
     .//           '+S'''
      PCK( 189 ) = '      FRAME_1700003_SEC_AXIS       = ''Y'''
      PCK( 190 ) = '      FRAME_1700003_SEC_VECTOR_DEF = ''OB'
     .//           'SERVER_TARGET_VELOCITY'''
      PCK( 191 ) = '      FRAME_1700003_SEC_OBSERVER   = ''SU'
     .//           'N'''
      PCK( 192 ) = '      FRAME_1700003_SEC_TARGET     = ''AL'
     .//           'PHA'''
      PCK( 193 ) = '      FRAME_1700003_SEC_ABCORR     = ''CN'
     .//           '+S'''
      PCK( 194 ) = '      FRAME_1700003_SEC_FRAME      = ''J2'
     .//           '000'''
      PCK( 195 ) = ' '
      PCK( 196 ) = ' '
      PCK( 197 ) = '      FRAME_ALPHA_VIEW_XY_XCN      =  170'
     .//           '0004'
      PCK( 198 ) = '      FRAME_1700004_NAME           = ''AL'
     .//           'PHA_VIEW_XY_XCN'''
      PCK( 199 ) = '      FRAME_1700004_CLASS          =  5'
      PCK( 200 ) = '      FRAME_1700004_CLASS_ID       =  170'
     .//           '0004'
      PCK( 201 ) = '      FRAME_1700004_CENTER         = ''AL'
     .//           'PHA'''
      PCK( 202 ) = '      FRAME_1700004_RELATIVE       = ''J2'
     .//           '000'''
      PCK( 203 ) = '      FRAME_1700004_DEF_STYLE      = ''PA'
     .//           'RAMETERIZED'''
      PCK( 204 ) = '      FRAME_1700004_FAMILY         = ''TW'
     .//           'O-VECTOR'''
      PCK( 205 ) = '      FRAME_1700004_PRI_AXIS       = ''X'''
      PCK( 206 ) = '      FRAME_1700004_PRI_VECTOR_DEF = ''OB'
     .//           'SERVER_TARGET_POSITION'''
      PCK( 207 ) = '      FRAME_1700004_PRI_OBSERVER   = ''SU'
     .//           'N'''
      PCK( 208 ) = '      FRAME_1700004_PRI_TARGET     = ''AL'
     .//           'PHA'''
      PCK( 209 ) = '      FRAME_1700004_PRI_ABCORR     = ''XC'
     .//           'N'''
      PCK( 210 ) = '      FRAME_1700004_SEC_AXIS       = ''Y'''
      PCK( 211 ) = '      FRAME_1700004_SEC_VECTOR_DEF = ''OB'
     .//           'SERVER_TARGET_VELOCITY'''
      PCK( 212 ) = '      FRAME_1700004_SEC_OBSERVER   = ''SU'
     .//           'N'''
      PCK( 213 ) = '      FRAME_1700004_SEC_TARGET     = ''AL'
     .//           'PHA'''
      PCK( 214 ) = '      FRAME_1700004_SEC_ABCORR     = ''XC'
     .//           'N'''
      PCK( 215 ) = '      FRAME_1700004_SEC_FRAME      = ''J2'
     .//           '000'''
      PCK( 216 ) = ' '
      PCK( 217 ) = '      FRAME_ALPHA_VIEW_XY_XCNS     =  170'
     .//           '0005'
      PCK( 218 ) = '      FRAME_1700005_NAME           = ''AL'
     .//           'PHA_VIEW_XY_XCNS'''
      PCK( 219 ) = '      FRAME_1700005_CLASS          =  5'
      PCK( 220 ) = '      FRAME_1700005_CLASS_ID       =  170'
     .//           '0005'
      PCK( 221 ) = '      FRAME_1700005_CENTER         = ''AL'
     .//           'PHA'''
      PCK( 222 ) = '      FRAME_1700005_RELATIVE       = ''J2'
     .//           '000'''
      PCK( 223 ) = '      FRAME_1700005_DEF_STYLE      = ''PA'
     .//           'RAMETERIZED'''
      PCK( 224 ) = '      FRAME_1700005_FAMILY         = ''TW'
     .//           'O-VECTOR'''
      PCK( 225 ) = '      FRAME_1700005_PRI_AXIS       = ''X'''
      PCK( 226 ) = '      FRAME_1700005_PRI_VECTOR_DEF = ''OB'
     .//           'SERVER_TARGET_POSITION'''
      PCK( 227 ) = '      FRAME_1700005_PRI_OBSERVER   = ''SU'
     .//           'N'''
      PCK( 228 ) = '      FRAME_1700005_PRI_TARGET     = ''AL'
     .//           'PHA'''
      PCK( 229 ) = '      FRAME_1700005_PRI_ABCORR     = ''XC'
     .//           'N+S'''
      PCK( 230 ) = '      FRAME_1700005_SEC_AXIS       = ''Y'''
      PCK( 231 ) = '      FRAME_1700005_SEC_VECTOR_DEF = ''OB'
     .//           'SERVER_TARGET_VELOCITY'''
      PCK( 232 ) = '      FRAME_1700005_SEC_OBSERVER   = ''SU'
     .//           'N'''
      PCK( 233 ) = '      FRAME_1700005_SEC_TARGET     = ''AL'
     .//           'PHA'''
      PCK( 234 ) = '      FRAME_1700005_SEC_ABCORR     = ''XC'
     .//           'N+S'''
      PCK( 235 ) = '      FRAME_1700005_SEC_FRAME      = ''J2'
     .//           '000'''
      PCK( 236 ) = ' '
      PCK( 237 ) = ' '
      PCK( 238 ) = '      FRAME_ALPHA_VIEW_XZ_CN       =  170'
     .//           '0006'
      PCK( 239 ) = '      FRAME_1700006_NAME           = ''AL'
     .//           'PHA_VIEW_XZ_CN'''
      PCK( 240 ) = '      FRAME_1700006_CLASS          =  5'
      PCK( 241 ) = '      FRAME_1700006_CLASS_ID       =  170'
     .//           '0006'
      PCK( 242 ) = '      FRAME_1700006_CENTER         = ''AL'
     .//           'PHA'''
      PCK( 243 ) = '      FRAME_1700006_RELATIVE       = ''J2'
     .//           '000'''
      PCK( 244 ) = '      FRAME_1700006_DEF_STYLE      = ''PA'
     .//           'RAMETERIZED'''
      PCK( 245 ) = '      FRAME_1700006_FAMILY         = ''TW'
     .//           'O-VECTOR'''
      PCK( 246 ) = '      FRAME_1700006_PRI_AXIS       = ''X'''
      PCK( 247 ) = '      FRAME_1700006_PRI_VECTOR_DEF = ''OB'
     .//           'SERVER_TARGET_POSITION'''
      PCK( 248 ) = '      FRAME_1700006_PRI_OBSERVER   = ''SU'
     .//           'N'''
      PCK( 249 ) = '      FRAME_1700006_PRI_TARGET     = ''AL'
     .//           'PHA'''
      PCK( 250 ) = '      FRAME_1700006_PRI_ABCORR     = ''CN'
     .//           ''''
      PCK( 251 ) = '      FRAME_1700006_SEC_AXIS       = ''Z'''
      PCK( 252 ) = '      FRAME_1700006_SEC_VECTOR_DEF = ''OB'
     .//           'SERVER_TARGET_VELOCITY'''
      PCK( 253 ) = '      FRAME_1700006_SEC_OBSERVER   = ''SU'
     .//           'N'''
      PCK( 254 ) = '      FRAME_1700006_SEC_TARGET     = ''AL'
     .//           'PHA'''
      PCK( 255 ) = '      FRAME_1700006_SEC_ABCORR     = ''CN'
     .//           ''''
      PCK( 256 ) = '      FRAME_1700006_SEC_FRAME      = ''J2'
     .//           '000'''
      PCK( 257 ) = ' '
      PCK( 258 ) = '      FRAME_ALPHA_VIEW_XZ_CNS      =  170'
     .//           '0007'
      PCK( 259 ) = '      FRAME_1700007_NAME           = ''AL'
     .//           'PHA_VIEW_XZ_CNS'''
      PCK( 260 ) = '      FRAME_1700007_CLASS          =  5'
      PCK( 261 ) = '      FRAME_1700007_CLASS_ID       =  170'
     .//           '0007'
      PCK( 262 ) = '      FRAME_1700007_CENTER         = ''AL'
     .//           'PHA'''
      PCK( 263 ) = '      FRAME_1700007_RELATIVE       = ''J2'
     .//           '000'''
      PCK( 264 ) = '      FRAME_1700007_DEF_STYLE      = ''PA'
     .//           'RAMETERIZED'''
      PCK( 265 ) = '      FRAME_1700007_FAMILY         = ''TW'
     .//           'O-VECTOR'''
      PCK( 266 ) = '      FRAME_1700007_PRI_AXIS       = ''X'''
      PCK( 267 ) = '      FRAME_1700007_PRI_VECTOR_DEF = ''OB'
     .//           'SERVER_TARGET_POSITION'''
      PCK( 268 ) = '      FRAME_1700007_PRI_OBSERVER   = ''SU'
     .//           'N'''
      PCK( 269 ) = '      FRAME_1700007_PRI_TARGET     = ''AL'
     .//           'PHA'''
      PCK( 270 ) = '      FRAME_1700007_PRI_ABCORR     = ''CN'
     .//           '+S'''
      PCK( 271 ) = '      FRAME_1700007_SEC_AXIS       = ''Z'''
      PCK( 272 ) = '      FRAME_1700007_SEC_VECTOR_DEF = ''OB'
     .//           'SERVER_TARGET_VELOCITY'''
      PCK( 273 ) = '      FRAME_1700007_SEC_OBSERVER   = ''SU'
     .//           'N'''
      PCK( 274 ) = '      FRAME_1700007_SEC_TARGET     = ''AL'
     .//           'PHA'''
      PCK( 275 ) = '      FRAME_1700007_SEC_ABCORR     = ''CN'
     .//           '+S'''
      PCK( 276 ) = '      FRAME_1700007_SEC_FRAME      = ''J2'
     .//           '000'''
      PCK( 277 ) = ' '
      PCK( 278 ) = ' '
      PCK( 279 ) = '      FRAME_ALPHA_VIEW_XZ_XCN      =  170'
     .//           '0008'
      PCK( 280 ) = '      FRAME_1700008_NAME           = ''AL'
     .//           'PHA_VIEW_XZ_XCN'''
      PCK( 281 ) = '      FRAME_1700008_CLASS          =  5'
      PCK( 282 ) = '      FRAME_1700008_CLASS_ID       =  170'
     .//           '0008'
      PCK( 283 ) = '      FRAME_1700008_CENTER         = ''AL'
     .//           'PHA'''
      PCK( 284 ) = '      FRAME_1700008_RELATIVE       = ''J2'
     .//           '000'''
      PCK( 285 ) = '      FRAME_1700008_DEF_STYLE      = ''PA'
     .//           'RAMETERIZED'''
      PCK( 286 ) = '      FRAME_1700008_FAMILY         = ''TW'
     .//           'O-VECTOR'''
      PCK( 287 ) = '      FRAME_1700008_PRI_AXIS       = ''X'''
      PCK( 288 ) = '      FRAME_1700008_PRI_VECTOR_DEF = ''OB'
     .//           'SERVER_TARGET_POSITION'''
      PCK( 289 ) = '      FRAME_1700008_PRI_OBSERVER   = ''SU'
     .//           'N'''
      PCK( 290 ) = '      FRAME_1700008_PRI_TARGET     = ''AL'
     .//           'PHA'''
      PCK( 291 ) = '      FRAME_1700008_PRI_ABCORR     = ''XC'
     .//           'N'''
      PCK( 292 ) = '      FRAME_1700008_SEC_AXIS       = ''Z'''
      PCK( 293 ) = '      FRAME_1700008_SEC_VECTOR_DEF = ''OB'
     .//           'SERVER_TARGET_VELOCITY'''
      PCK( 294 ) = '      FRAME_1700008_SEC_OBSERVER   = ''SU'
     .//           'N'''
      PCK( 295 ) = '      FRAME_1700008_SEC_TARGET     = ''AL'
     .//           'PHA'''
      PCK( 296 ) = '      FRAME_1700008_SEC_ABCORR     = ''XC'
     .//           'N'''
      PCK( 297 ) = '      FRAME_1700008_SEC_FRAME      = ''J2'
     .//           '000'''
      PCK( 298 ) = ' '
      PCK( 299 ) = '      FRAME_ALPHA_VIEW_XZ_XCNS     =  170'
     .//           '0009'
      PCK( 300 ) = '      FRAME_1700009_NAME           = ''AL'
     .//           'PHA_VIEW_XZ_XCNS'''
      PCK( 301 ) = '      FRAME_1700009_CLASS          =  5'
      PCK( 302 ) = '      FRAME_1700009_CLASS_ID       =  170'
     .//           '0009'
      PCK( 303 ) = '      FRAME_1700009_CENTER         = ''AL'
     .//           'PHA'''
      PCK( 304 ) = '      FRAME_1700009_RELATIVE       = ''J2'
     .//           '000'''
      PCK( 305 ) = '      FRAME_1700009_DEF_STYLE      = ''PA'
     .//           'RAMETERIZED'''
      PCK( 306 ) = '      FRAME_1700009_FAMILY         = ''TW'
     .//           'O-VECTOR'''
      PCK( 307 ) = '      FRAME_1700009_PRI_AXIS       = ''X'''
      PCK( 308 ) = '      FRAME_1700009_PRI_VECTOR_DEF = ''OB'
     .//           'SERVER_TARGET_POSITION'''
      PCK( 309 ) = '      FRAME_1700009_PRI_OBSERVER   = ''SU'
     .//           'N'''
      PCK( 310 ) = '      FRAME_1700009_PRI_TARGET     = ''AL'
     .//           'PHA'''
      PCK( 311 ) = '      FRAME_1700009_PRI_ABCORR     = ''XC'
     .//           'N+S'''
      PCK( 312 ) = '      FRAME_1700009_SEC_AXIS       = ''Z'''
      PCK( 313 ) = '      FRAME_1700009_SEC_VECTOR_DEF = ''OB'
     .//           'SERVER_TARGET_VELOCITY'''
      PCK( 314 ) = '      FRAME_1700009_SEC_OBSERVER   = ''SU'
     .//           'N'''
      PCK( 315 ) = '      FRAME_1700009_SEC_TARGET     = ''AL'
     .//           'PHA'''
      PCK( 316 ) = '      FRAME_1700009_SEC_ABCORR     = ''XC'
     .//           'N+S'''
      PCK( 317 ) = '      FRAME_1700009_SEC_FRAME      = ''J2'
     .//           '000'''
      PCK( 318 ) = ' '
      PCK( 319 ) = ' '
      PCK( 320 ) = ' '
      PCK( 321 ) = BEGTXT()
      PCK( 322 ) = ' '
      PCK( 323 ) = '      View frames for body BETA relative '
     .//           'to the Sun'
      PCK( 324 ) = ' '
      PCK( 325 ) = '         BETA_VIEW_XY      orbital motion'
     .//           ' of BETA lies in X-Y plane'
      PCK( 326 ) = '                           of this frame.'
      PCK( 327 ) = ' '
      PCK( 328 ) = '         BETA_VIEW_XZ      orbital motion'
     .//           ' of BETA lies in X-Z plane'
      PCK( 329 ) = '                           of this frame.'
      PCK( 330 ) = ' '
      PCK( 331 ) = BEGDAT()
      PCK( 332 ) = ' '
      PCK( 333 ) = '      FRAME_BETA_VIEW_XY           =  170'
     .//           '0010'
      PCK( 334 ) = '      FRAME_1700010_NAME           = ''BE'
     .//           'TA_VIEW_XY'''
      PCK( 335 ) = '      FRAME_1700010_CLASS          =  5'
      PCK( 336 ) = '      FRAME_1700010_CLASS_ID       =  170'
     .//           '0010'
      PCK( 337 ) = '      FRAME_1700010_CENTER         = ''BE'
     .//           'TA'''
      PCK( 338 ) = '      FRAME_1700010_RELATIVE       = ''J2'
     .//           '000'''
      PCK( 339 ) = '      FRAME_1700010_DEF_STYLE      = ''PA'
     .//           'RAMETERIZED'''
      PCK( 340 ) = '      FRAME_1700010_FAMILY         = ''TW'
     .//           'O-VECTOR'''
      PCK( 341 ) = '      FRAME_1700010_PRI_AXIS       = ''X'''
      PCK( 342 ) = '      FRAME_1700010_PRI_VECTOR_DEF = ''OB'
     .//           'SERVER_TARGET_POSITION'''
      PCK( 343 ) = '      FRAME_1700010_PRI_OBSERVER   = ''SU'
     .//           'N'''
      PCK( 344 ) = '      FRAME_1700010_PRI_TARGET     = ''BE'
     .//           'TA'''
      PCK( 345 ) = '      FRAME_1700010_PRI_ABCORR     = ''NO'
     .//           'NE'''
      PCK( 346 ) = '      FRAME_1700010_SEC_AXIS       = ''Y'''
      PCK( 347 ) = '      FRAME_1700010_SEC_VECTOR_DEF = ''OB'
     .//           'SERVER_TARGET_VELOCITY'''
      PCK( 348 ) = '      FRAME_1700010_SEC_OBSERVER   = ''SU'
     .//           'N'''
      PCK( 349 ) = '      FRAME_1700010_SEC_TARGET     = ''BE'
     .//           'TA'''
      PCK( 350 ) = '      FRAME_1700010_SEC_ABCORR     = ''NO'
     .//           'NE'''
      PCK( 351 ) = '      FRAME_1700010_SEC_FRAME      = ''J2'
     .//           '000'''
      PCK( 352 ) = ' '
      PCK( 353 ) = ' '
      PCK( 354 ) = '      FRAME_BETA_VIEW_XZ           =  170'
     .//           '0011'
      PCK( 355 ) = '      FRAME_1700011_NAME           = ''BE'
     .//           'TA_VIEW_XZ'''
      PCK( 356 ) = '      FRAME_1700011_CLASS          =  5'
      PCK( 357 ) = '      FRAME_1700011_CLASS_ID       =  170'
     .//           '0011'
      PCK( 358 ) = '      FRAME_1700011_CENTER         = ''BE'
     .//           'TA'''
      PCK( 359 ) = '      FRAME_1700011_RELATIVE       = ''J2'
     .//           '000'''
      PCK( 360 ) = '      FRAME_1700011_DEF_STYLE      = ''PA'
     .//           'RAMETERIZED'''
      PCK( 361 ) = '      FRAME_1700011_FAMILY         = ''TW'
     .//           'O-VECTOR'''
      PCK( 362 ) = '      FRAME_1700011_PRI_AXIS       = ''X'''
      PCK( 363 ) = '      FRAME_1700011_PRI_VECTOR_DEF = ''OB'
     .//           'SERVER_TARGET_POSITION'''
      PCK( 364 ) = '      FRAME_1700011_PRI_OBSERVER   = ''SU'
     .//           'N'''
      PCK( 365 ) = '      FRAME_1700011_PRI_TARGET     = ''BE'
     .//           'TA'''
      PCK( 366 ) = '      FRAME_1700011_PRI_ABCORR     = ''NO'
     .//           'NE'''
      PCK( 367 ) = '      FRAME_1700011_SEC_AXIS       = ''Z'''
      PCK( 368 ) = '      FRAME_1700011_SEC_VECTOR_DEF = ''OB'
     .//           'SERVER_TARGET_VELOCITY'''
      PCK( 369 ) = '      FRAME_1700011_SEC_OBSERVER   = ''SU'
     .//           'N'''
      PCK( 370 ) = '      FRAME_1700011_SEC_TARGET     = ''BE'
     .//           'TA'''
      PCK( 371 ) = '      FRAME_1700011_SEC_ABCORR     = ''NO'
     .//           'NE'''
      PCK( 372 ) = '      FRAME_1700011_SEC_FRAME      = ''J2'
     .//           '000'''
      PCK( 373 ) = ' '
      PCK( 374 ) = BEGTXT()
      PCK( 375 ) = ' '
      PCK( 376 ) = '   Radii and GM for the Sun.'
      PCK( 377 ) = ' '
      PCK( 378 ) = BEGDAT()
      PCK( 379 ) = ' '
      PCK( 380 ) = '      BODY10_RADII            = ( 1000, 1'
     .//           '000, 1000 )'
      PCK( 381 ) = ' '
      PCK( 382 ) = '      BODY10_GM               =  0.997452'
     .//           '90739151156E+09'
      PCK( 383 ) = ' '
      PCK( 384 ) = BEGTXT()
      PCK( 385 ) = ' '
      PCK( 386 ) = '   GM for ALPHA. This GM, together with a'
     .//           ' circular'
      PCK( 387 ) = '   orbit of radius 1.E5 for body GAMMA, y'
     .//           'ields a 24 hour period'
      PCK( 388 ) = '   for GAMMA.'
      PCK( 389 ) = ' '
      PCK( 390 ) = BEGDAT()
      PCK( 391 ) = ' '
      PCK( 392 ) = '      BODY1000_GM             =  0.528849'
     .//           '68712970233E+07'
      PCK( 393 ) = ' '


C
C     Create the PCK.
C
      CALL TXTOPN ( NAMEPC, UNIT )
 
      DO I = 1, NLINES
         CALL WRITLN ( PCK(I), UNIT )
      END DO
 
      CLOSE ( UNIT )

C
C     If this file needs to be loaded, do it. 
C
      IF ( LOADPC ) THEN
         CALL LDPOOL ( NAMEPC )
      END IF
 
      IF ( KEEPPC ) THEN
C
C        If we are keeping this file, we need to register it
C        with FILREG.
C
         CALL TFILES ( NAMEPC )

      ELSE
C
C        Lose the file.
C
         CALL KILFIL ( NAMEPC )

      END IF

      CALL CHKOUT ( 'NATPCK' )
      RETURN
      END
