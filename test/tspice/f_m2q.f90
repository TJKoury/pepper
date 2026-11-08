!$Procedure      F_M2Q ( Family of tests for the SPICE routine M2Q)

      SUBROUTINE F_M2Q ( OK )
      IMPLICIT NONE

      LOGICAL               OK

! $ Abstract
!
!     This routine performs a series of test on matrices generated
!     from quaternions to ensure that the original quaternion can
!     be recovered and that each branch in the quaternion construction
!     code is exercised.
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


!
!     Local Variables
!
      DOUBLE PRECISION      Q1(0:3)
      DOUBLE PRECISION      Q2(0:3)
      DOUBLE PRECISION      Q3(0:3)
      DOUBLE PRECISION      Q4(0:3)

      DOUBLE PRECISION      Q (0:3)


      DOUBLE PRECISION      R1(3,3)
      DOUBLE PRECISION      R2(3,3)
      DOUBLE PRECISION      R3(3,3)
      DOUBLE PRECISION      R4(3,3)

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_M2Q' )


      Q1(0) = 0.9D0
      Q1(1) = 0.3D0
      Q1(2) = 0.3D0
      Q1(3) = 0.1D0

      Q2(0) = 0.1D0
      Q2(1) = 0.9D0
      Q2(2) = 0.3D0
      Q2(3) = 0.3D0

      Q3(0) = 0.3D0
      Q3(1) = 0.1D0
      Q3(2) = 0.9D0
      Q3(3) = 0.3D0

      Q4(0) = 0.3D0
      Q4(1) = 0.3D0
      Q4(2) = 0.1D0
      Q4(3) = 0.9D0

      CALL Q2M ( Q1, R1 )
      CALL Q2M ( Q2, R2 )
      CALL Q2M ( Q3, R3 )
      CALL Q2M ( Q4, R4 )


      CALL TCASE ( 'Expecting real component to be .9' )

         CALL M2Q    ( R1, Q )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKAD ( 'Q1', Q, '~', Q1, 4, 1.0D-14, OK )


      CALL TCASE ( 'Expecting I component to be 0.9' )

         CALL M2Q    ( R2, Q )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKAD ( 'Q2', Q, '~', Q2, 4, 1.0D-14, OK )

      CALL TCASE ( 'Expecting J component to be 0.9' )

         CALL M2Q    ( R3, Q )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKAD ( 'Q3', Q, '~', Q3, 4, 1.0D-14, OK )

      CALL TCASE ( 'Expecting K component to be 0.9' )

         CALL M2Q    ( R4, Q )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKAD ( 'Q4', Q, '~', Q4, 4, 1.0D-14, OK )


      CALL T_SUCCESS ( OK )
      RETURN
      END
