C$Procedure      F_CHGIRF (Family of tests for CHGIRF )
 
      SUBROUTINE F_CHGIRF ( OK )
      IMPLICIT NONE
 
      LOGICAL               OK
 
C$ Abstract
C
C     This test performs regressions on the SPICE routine CHGIRF.
C     To update it you should retrieve the last version of CHGIRF
C     and append an underscore O "Oh" to each entry point.  This
C     is the OLD version of the routine.
C
C     You should also update the number of previously known frames
C     to the values in the old version of CHGIRF.
C
C     Add whatever tests are appropriate for the new reference
C     frames or entry points.
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
C-&
C
C     The parameter ONAMES is the number of recognized reference
C     frames that were present in the previous version of CHGIRF
C
      INTEGER               ONAMES
      PARAMETER           ( ONAMES = 20 )
C
C     The parameter NNAMES is the number of recognized reference
C     frames that are present in the updated version of CHGIRF
C     that you are attempting to test.
C
      INTEGER               NNAMES
      PARAMETER           ( NNAMES = 21 )
C
C     The array NEWNAM contains the names of the reference frames
C     that are in the updated version of CHGIRF that were not
C     in the previous version of CHGIRF.
C
C     Set the values of the entries of this array in the first block
C     of executable code below.
C
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )
 
      CHARACTER*(WDSIZE)    NEWNAM ( 20 )
C
C     Bad is an ID code for a frame that should not be recognized
C     by CHGIRF.
C
      INTEGER               BAD
      PARAMETER           ( BAD = -1 )
 
 
 
C
C     Test Utility Functions
C
 
 
C
C     SPICELIB Functions
C
      INTEGER               CARDC
 
C
C     Local Variables
C
 
      INTEGER               MAXNAM
      PARAMETER           ( MAXNAM = 100 )
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      CHARACTER*(WDSIZE)    NAME
      CHARACTER*(WDSIZE)    EXPCT
      CHARACTER*(WDSIZE)    NEW    (LBCELL : MAXNAM )
 
      DOUBLE PRECISION      MATRIX ( 3, 3 )
      DOUBLE PRECISION      ROTATE ( 3, 3 )
      DOUBLE PRECISION      QUOTNT ( 3, 3 )
      DOUBLE PRECISION      XFORM  ( 3, 3 )
      DOUBLE PRECISION      ANGLE
      DOUBLE PRECISION      A      ( 3 )
      DOUBLE PRECISION      AXIS   ( 3 )
      DOUBLE PRECISION      ROTT   ( 3, 3 )
      DOUBLE PRECISION      ROT    ( 3, 3 )
 
      INTEGER               COUNT
      INTEGER               I
      INTEGER               ID
      INTEGER               J
      INTEGER               K
 
C
C     The names that are new to CHGIRF should be placed here.
C
      NEWNAM(1) = 'DE-143'
 
 
C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_CHGIRF' )
 
 
      CALL TCASE ( 'Comparison of old and new transformations.' )
 
         DO I = 1, ONAMES
            DO J = 1,ONAMES
 
            CALL TSTMSG ( '#', 'Subcase I = #, J = #' )
            CALL TSTMSI ( I )
            CALL TSTMSI ( J )
 
            CALL IRFROT   ( I,   J, ROTATE )
            CALL IRFROT_O ( I,   J, MATRIX )
            CALL MTXM     ( MATRIX, ROTATE, QUOTNT )
            CALL RAXISA   ( QUOTNT, AXIS,   ANGLE  )
 
            CALL CHCKXC ( .FALSE., ' ',  OK )
            CALL CHCKAD ( 'ROTATE', ROTATE, '~', MATRIX, 9, 1.0D-15, OK)
            CALL CHCKSD ( 'ANGLE',  ANGLE,  '~', 0.0D0,     1.0D-15, OK)
 
            END DO
         END DO
 
 
      CALL TCASE ( 'Converting from id to name and back.' )
 
         CALL SSIZEC ( MAXNAM, NEW )
 
         DO I = 1, NNAMES
 
            J = I
            CALL IRFNAM ( J,    NAME  )
            CALL IRFNUM ( NAME, K     )
            CALL INSRTC ( NAME, NEW   )
 
            CALL CHCKXC ( .FALSE., ' ',      OK )
            CALL CHCKSI ( 'J', J, '=', K, 0, OK )
 
         END DO
 
         COUNT = CARDC ( NEW )
 
         CALL CHCKSI ( 'NameCount', COUNT, '=', NNAMES, 0, OK )
 
      CALL TCASE ( 'Converting old names to ID''s.' )
 
 
         DO I = 1, ONAMES
 
            J = I
            CALL IRFNAM_O ( J,    NAME )
            CALL IRFNUM   ( NAME, K    )
 
            CALL CHCKXC ( .FALSE.,      ' ',       OK )
            CALL CHCKSI ( 'ID Code', K, '=', J, 0, OK )
 
         END DO
 
      CALL TCASE ( 'Comparing new and old names.' )
 
         DO I = 1, ONAMES
 
            J = I
            CALL IRFNAM   ( J, NAME  )
            CALL IRFNAM_O ( J, EXPCT )
 
            CALL CHCKXC ( .FALSE., ' ',  OK )
            CALL CHCKSC ( 'NAME', NAME, '=', EXPCT, OK )
         END DO
 
      CALL TCASE ( 'Checking new names. ' )
C
C        To update this case simply put the new names in the array below
C        in the order of their id-codes.
C
         K         =  0
 
         DO I = ONAMES+1, NNAMES
 
            J = I
            K = K + 1
            CALL TSTMSG ( '#', 'Subcase ID = #, NEWNAM = #' )
            CALL TSTMSI ( J )
            CALL TSTMSC ( NEWNAM(K) )
 
            CALL IRFNAM ( J, NAME )
            CALL CHCKXC ( .FALSE., ' ',  OK )
            CALL CHCKSC ( 'NAME', NAME, '=', NEWNAM(K), OK )
 
         END DO
 
 
      CALL TCASE ( 'Unknown ID-code' )
 
         CALL IRFNAM ( BAD, NAME )
         CALL TSTMSG ( '#', ' '  )
         CALL CHCKSC ( 'NAME', NAME, '=', ' ', OK )
 
 
      CALL TCASE ( 'Unknown Name' )
 
         CALL IRFNUM ( 'BOGUS', ID )
         CALL CHCKSI ( 'ID', ID, '=', 0, 0, OK )
 
 
      CALL TCASE ( 'DE-140 check' )
 
         XFORM (1,1) =  0.9999256765384668D0
         XFORM (2,1) = -0.0111817701797229D0
         XFORM (3,1) = -0.0048589520204830D0
 
         XFORM (1,2) =  0.0111817701197967D0
         XFORM (2,2) =  0.9999374816848701D0
         XFORM (3,2) = -0.0000271791849815D0
 
         XFORM (1,3) =  0.0048589521583895D0
         XFORM (2,3) = -0.0000271545195858D0
         XFORM (3,3) =  0.9999881948535965D0
 
         CALL IRFNUM ( 'J2000',  J   )
         CALL IRFNUM ( 'DE-140', I   )
         CALL IRFROT (  J, I,   ROTT )
 
         CALL MXMT  ( XFORM, ROTT, ROT   )
         CALL RAXISA ( ROT,  A,    ANGLE )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKAD ( 'ROTATE', ROTT, '~', XFORM, 9, 1.0D-14, OK )
         CALL CHCKSD ( 'ANGLE', ANGLE, '~', 0.0D0,    1.0D-13, OK )
 
      CALL TCASE ( 'DE-142 check' )
 
         XFORM (1,1) =  0.9999256765402605D0
         XFORM (2,1) = -0.0111817697907755D0
         XFORM (3,1) = -0.0048589525464121D0
 
         XFORM (1,2) =  0.0111817697320531D0
         XFORM (2,2) =  0.9999374816892126D0
         XFORM (3,2) = -0.0000271789392288D0
 
         XFORM (1,3) =  0.0048589526815484D0
         XFORM (2,3) = -0.0000271547693170D0
         XFORM (3,3) =  0.9999881948510477D0
 
         CALL IRFNUM ( 'J2000',  J   )
         CALL IRFNUM ( 'DE-142', I   )
         CALL IRFROT (  J, I,   ROTT )
 
         CALL MXMT  ( XFORM, ROTT, ROT   )
         CALL RAXISA ( ROT,  A,    ANGLE )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKAD ( 'ROTATE', ROTT, '~', XFORM, 9, 2.0D-14, OK )
         CALL CHCKSD ( 'ANGLE', ANGLE, '~', 0.0D0,    1.0D-13, OK )
 
       CALL TCASE ( 'DE-143 check' )
 
         XFORM(1,1) =  0.9999256765435852D0
         XFORM(2,1) = -0.0111817743300355D0
         XFORM(3,1) = -0.0048589414161348D0
 
         XFORM(1,2) =  0.0111817743077255D0
         XFORM(2,2) =  0.9999374816382505D0
         XFORM(3,2) = -0.0000271713942366D0
 
         XFORM (1,3) =  0.0048589414674762D0
         XFORM (2,3) = -0.0000271622115251D0
         XFORM (3,3) =  0.9999881949053349D0
 
         CALL IRFNUM ( 'J2000',  J   )
         CALL IRFNUM ( 'DE-143', I   )
         CALL IRFROT (  J, I,   ROTT )
 
         CALL MXMT  ( XFORM, ROTT, ROT   )
         CALL RAXISA ( ROT,  A,    ANGLE )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKAD ( 'ROTATE', ROTT, '~', XFORM, 9, 2.0D-14, OK )
         CALL CHCKSD ( 'ANGLE', ANGLE, '~', 0.0D0,    1.0D-13, OK )
 
 
 
 
      CALL T_SUCCESS ( OK )
      RETURN
      END
