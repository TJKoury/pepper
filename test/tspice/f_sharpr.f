C$Procedure F_SHARPR ( Test the SPICELIB routine SHARPR )
 
      SUBROUTINE F_SHARPR ( OK )
 
C$ Abstract
C
C     Exercise the SPICELIB routine SHARPR.
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
C     TEST FAMILY
C
C$ Declarations
 
      IMPLICIT NONE

      LOGICAL               OK
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     OK         O   logical indicating test status.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     OK         is a logical that indicates the test status to the
C                caller.
C
C$ Parameters
C
C     None.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     This routine does not generate any errors. Routines in its
C     call tree may generate errors that are either intentional and
C     trapped or unintentional and need reporting.  The test family
C     utilities manage this.
C
C$ Particulars
C
C     This routine tests the SPICELIB routine SHARPR.  SHARPR
C     "sharpens" a 3x3 matrix that is "nearly" a rotation matrix
C     by unitizing the columns and making the columns mutually
C     orthogonal.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman     (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    TSPICE Version 1.0.0, 14-OCT-2005 (NJB)
C
C
C-&
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      PI

      LOGICAL               ISROT

C
C     Other functions
C
      DOUBLE PRECISION      T_RANDD

C
C     Local Parameters
C
      DOUBLE PRECISION      AMP
      PARAMETER           ( AMP    = 1.D-2 )

      DOUBLE PRECISION      DTOL
      PARAMETER           ( DTOL   = 1.D-14 )

      DOUBLE PRECISION      NTOL
      PARAMETER           ( NTOL   = 1.D-14 )


      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 240 )

      INTEGER               NUMCAS
      PARAMETER           ( NUMCAS = 1000 )


C
C     Local Variables
C
      CHARACTER*(MSGLEN)    TITLE

      DOUBLE PRECISION      ANGLE  ( 3 )
      DOUBLE PRECISION      AXIS   ( 3 )
      DOUBLE PRECISION      NEARLY ( 3, 3 )
      DOUBLE PRECISION      NOISE  ( 3, 3 )
      DOUBLE PRECISION      Q      ( 3, 3 )
      DOUBLE PRECISION      QANGLE
      DOUBLE PRECISION      R      ( 3, 3 )

      INTEGER               CASE
      INTEGER               I
      INTEGER               J
      INTEGER               SEED

      LOGICAL               IS

C
C     Saved values
C

C
C     Initial values
C



C
C     Open the test family.
C
      CALL TOPEN ( 'F_SHARPR' )

      SEED = -1

      DO CASE = 1, NUMCAS
C
C --- Case: ------------------------------------------------------
C
         TITLE = 'Perturb a rotation matrix; then sharpen; case #.'

         CALL REPMI ( TITLE, '#', CASE, TITLE )      
         CALL TCASE ( TITLE )

         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Construct a rotation matrix from three Euler angles.
C
         ANGLE(1) =  T_RANDD (  -PI(),       PI(),       SEED )
         ANGLE(2) =  T_RANDD (  -PI()/2.D0,  PI()/2.D0,  SEED )
         ANGLE(3) =  T_RANDD (  -PI(),       PI(),       SEED )
      
      
         CALL EUL2M ( ANGLE(3), ANGLE(2), ANGLE(1), 3, 2, 3, R )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Construct a "noise" matrix with which to perturb the 
C        rotation.
C
         DO I = 1, 3

            DO J = 1, 3
               NOISE(I,J) =  T_RANDD ( -AMP, AMP, SEED )
            END DO

         END DO

C
C        NEARLY is "nearly" a rotation.
C
         CALL VADDG ( R, NOISE, 9, NEARLY )
         CALL CHCKXC ( .FALSE., ' ', OK )
         
C
C        Now sharpen the rotation.
C
         CALL SHARPR ( NEARLY )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Make sure the result is a rotation.
C
         IS = ISROT ( NEARLY, NTOL, DTOL )

         CALL CHCKSL ( 'Is result a rotation?', IS, .TRUE., OK )

C
C        Make sure the result is not too different from R.
C        Express the quotient of R and NEARLY as a rotation;
C        measure the rotation angle.
C 
         CALL MTXM ( NEARLY, R, Q )

         CALL RAXISA ( Q, AXIS, QANGLE )

         CALL CHCKSD ( 'QANGLE', QANGLE, '~', 0.D0, 4*AMP, OK )
         
      END DO

C
C     Now for some error handling tests.  
C      
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'SHARPR:  pass in a singular matrix.  All we '//
     .              'expect is that the routine doesn''t crash.'  )
 
      CALL CLEARD ( 9, R )

      CALL SHARPR ( R )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END


