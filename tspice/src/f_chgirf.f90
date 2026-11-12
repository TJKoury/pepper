!$Procedure      F_CHGIRF (Family of tests for CHGIRF )

      SUBROUTINE F_CHGIRF ( OK )
      use pepper_legacy, pepper_legacy_rotate => rotate
      IMPLICIT NONE

      LOGICAL               OK

! $ Abstract
!
!     This test performs regressions on the SPICE routine CHGIRF.
!     To update it you should retrieve the last version of CHGIRF
!     and append an underscore O "Oh" to each entry point.  This
!     is the OLD version of the routine.
!
!     You should also update the number of previously known frames
!     to the values in the old version of CHGIRF.
!
!     Add whatever tests are appropriate for the new reference
!     frames or entry points.
!
!-&
!
!     The parameter ONAMES is the number of recognized reference
!     frames that were present in the previous version of CHGIRF
!
      INTEGER               ONAMES
      PARAMETER           ( ONAMES = 20 )
!
!     The parameter NNAMES is the number of recognized reference
!     frames that are present in the updated version of CHGIRF
!     that you are attempting to test.
!
      INTEGER               NNAMES
      PARAMETER           ( NNAMES = 21 )
!
!     The array NEWNAM contains the names of the reference frames
!     that are in the updated version of CHGIRF that were not
!     in the previous version of CHGIRF.
!
!     Set the values of the entries of this array in the first block
!     of executable code below.
!
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )

      CHARACTER*(WDSIZE)    NEWNAM ( 20 )
!
!     Bad is an ID code for a frame that should not be recognized
!     by CHGIRF.
!
      INTEGER               BAD
      PARAMETER           ( BAD = -1 )



!
!     Test Utility Functions
!


!
!     SPICELIB Functions
!

!
!     Local Variables
!

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

!
!     The names that are new to CHGIRF should be placed here.
!
      NEWNAM(1) = 'DE-143'


!
!     Begin every test family with an open call.
!
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
!
!        To update this case simply put the new names in the array below
!        in the order of their id-codes.
!
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
