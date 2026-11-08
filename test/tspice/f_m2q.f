C$Procedure      F_M2Q ( Family of tests for the SPICE routine M2Q)
 
      SUBROUTINE F_M2Q ( OK )
      IMPLICIT NONE
 
      LOGICAL               OK
 
C$ Abstract
C
C     This routine performs a series of test on matrices generated
C     from quaternions to ensure that the original quaternion can
C     be recovered and that each branch in the quaternion construction
C     code is exercised.
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
C     Local Variables
C
      DOUBLE PRECISION      Q1(0:3)
      DOUBLE PRECISION      Q2(0:3)
      DOUBLE PRECISION      Q3(0:3)
      DOUBLE PRECISION      Q4(0:3)
 
      DOUBLE PRECISION      Q (0:3)
 
 
      DOUBLE PRECISION      R1(3,3)
      DOUBLE PRECISION      R2(3,3)
      DOUBLE PRECISION      R3(3,3)
      DOUBLE PRECISION      R4(3,3)
 
C
C     Begin every test family with an open call.
C
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
