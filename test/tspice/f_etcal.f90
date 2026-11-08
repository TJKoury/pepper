!$Procedure      F_ETCAL ( Family of tests for ETCAL )

      SUBROUTINE F_ETCAL  ( OK )

      IMPLICIT NONE
      LOGICAL               OK


! $ Abstract
!
!     This subroutine checks a number simple inputs to ETCAL
!     to make sure that values returned are correct.
!
!     Note that since there are no calls to SIGERR in ETCAL
!     there will never be any exceptions. We don't check for any.
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
!-&

      DOUBLE PRECISION      J2000
      DOUBLE PRECISION      SPD

      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ET2

      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 48 )

      CHARACTER*(WDSIZE)    STRING
      CHARACTER*(WDSIZE)    ERROR
      CHARACTER*(WDSIZE)    TITLE

      INTEGER               I
      INTEGER               J



      CALL TOPEN ( 'F_ETCAL' )


      CALL TCASE ( 'Times out of range --- too big ' )

         ET = 1.0D17

         CALL ETCAL ( ET, STRING )
         CALL CHCKSC ( 'STRING', STRING(1:12), '=', 'Epoch after ', OK )

      CALL TCASE ( 'Times out of range --- too small ' )

         ET = -1.0D17

         CALL ETCAL ( ET, STRING )
         CALL CHCKSC ( 'STRING', STRING(1:12), '=', 'Epoch before', OK )


      CALL TCASE ( 'Zero point of the Julian Date Scale' )

         ET = - J2000()*SPD()
         CALL ETCAL ( ET, STRING )
         CALL CHCKSC( 'STRING', STRING, '=', &
                      '4714 B.C. NOV 24 12:00:00.000', OK )

      CALL TCASE ( 'From a string to ET and back ' )

         CALL TPARSE ( '1993 JAN 12, 13:12:28.999', ET, ERROR )
         CALL ETCAL  ( ET,  STRING )
         CALL CHCKSC ( 'STRING', STRING, '=', &
                       '1993 JAN 12 13:12:28.999', OK )


      CALL TCASE ( 'From a string to ET and back ' )

         CALL TPARSE ( '893 JAN 12, 13:12:28.999', ET, ERROR )
         CALL ETCAL  ( ET,  STRING )
         CALL CHCKSC ( 'STRING', STRING, '=', &
                       '893 A.D. JAN 12 13:12:28.999', OK )

!
!     In this case we loop over a large collection of ET values and
!     make sure that TPARSE reproduces the ET output from ETCAL.
!
      ET = -100000000.0D0
      DO I = 1, 100


         TITLE = 'TPARSE - ETCAL Compatibility #'
         J     =  I

         CALL REPMI ( TITLE, '#', J, TITLE )
         CALL TCASE ( TITLE )


            ET    = ET + 2000000.D0
            ERROR = ' '

            CALL ETCAL  ( ET, STRING )
            CALL TPARSE ( STRING, ET2, ERROR  )

            CALL  CHCKSC ( 'ERROR', ERROR, '=', ' ',        OK )
            CALL  CHCKSD ( 'ET',    ET,    '=', ET2, 0.0D0, OK )

      END DO


      CALL T_SUCCESS( OK )
      RETURN

      END
