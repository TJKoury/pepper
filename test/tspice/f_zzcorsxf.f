C$Procedure      F_ZZCORSXF ( Test ZZCORSXF )
  
      SUBROUTINE F_ZZCORSXF ( OK )

C$ Abstract
C
C     Test the SPICELIB private state transformation correction
C     routine ZZCORSXF.
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
C     GF
C
C$ Keywords
C
C     GF
C     TEST
C     SEARCH
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE               'gf.inc'
      INCLUDE               'zzabcorr.inc'

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
C$ Exceptions
C
C     This routine does not generate any errors. Routines in its
C     call tree may generate errors that are either intentional and
C     trapped or unintentional and need reporting.  The test family
C     utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine tests the private SPICELIB state
C     transformation correction routine ZZCORSXF.
C
C$ Examples
C
C     None.
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
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 22-MAY-2008 (NJB)
C
C-&
 
  
C
C     SPICELIB functions
C

C
C     Local parameters
C
C     
C     Local parameters
C

      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-12 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )


C
C     Local variables
C
      CHARACTER*(LNSIZE)    TITLE

      DOUBLE PRECISION      CORXFM ( 6, 6 )
      DOUBLE PRECISION      DLT
      DOUBLE PRECISION      LTSIGN
      DOUBLE PRECISION      XFORM  ( 6, 6 )
      DOUBLE PRECISION      XCORXF ( 6, 6 )

      INTEGER               I
      INTEGER               J
      INTEGER               K

      LOGICAL               XMIT

C
C     Save everything.
C
      SAVE

C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_ZZCORSXF' )


C
C     ZZCORSXF is error free,so there are no error cases to
C     check.
C

C
C     Loop over the radiation direction cases.
C
      DLT = 5.D-2

      DO I = 1, 2
C
C---- Case -------------------------------------------------------------
C
         IF ( I .EQ. 1 ) THEN

            XMIT   = .TRUE.
            LTSIGN = 1.D0
            TITLE  = 'Initial matrix filled with index '
     .      //       'values. XMIT = .TRUE.'
         ELSE
            XMIT   = .FALSE.
            LTSIGN = -1.D0
            TITLE  = 'Initial matrix filled with index '
     .      //       'values. XMIT = .FALSE.'
         END IF

         CALL TCASE ( TITLE )

C
C        Since ZZCORSXF doesn't check the input matrix, we don't
C        have to use a real state transformation. Instead, simply
C        fill the original matrix with identifiable, distinct
C        values.
C
         DO J = 1, 6

            DO K = 1, 6
               XFORM( K, J ) = (J-1)*6 + K
            END DO

         END DO

C
C        Form the expected, transformed matrix.
C
         CALL MOVED ( XFORM, 36, XCORXF )

         DO J = 1, 3

            DO K = 4, 6
               XCORXF( K, J ) = XFORM(K,J) * ( 1.D0 + LTSIGN*DLT )
            END DO

         END DO

         CALL ZZCORSXF ( XMIT, DLT, XFORM, CORXFM )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( 'CORXFM', CORXFM, '~', XCORXF, 36, VTIGHT, OK )

      END DO

      CALL T_SUCCESS ( OK )

      RETURN
      END




