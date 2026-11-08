C$Procedure F_ZZDM ( Test ZZDIV, ZZMULT )

      SUBROUTINE F_ZZDM ( OK )

C$ Abstract
C
C     Test family to verify that ZZDIV behaves as expected.
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
C     None.
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
C     E.D. Wright     (JPL)
C
C$ Version
C
C-    TSPICE Version 1.0.0, 17-APR-2014 (EDW)
C
C-&

C
C     SPICELIB Functions
C

      DOUBLE PRECISION      DPMAX
      DOUBLE PRECISION      ZZDIV
      DOUBLE PRECISION      ZZMULT
      DOUBLE PRECISION      T_RANDD


C
C     Local Variables
C

      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      DENOM
      DOUBLE PRECISION      DIV
      DOUBLE PRECISION      LOGVAL
      DOUBLE PRECISION      MULT
      DOUBLE PRECISION      MULTX
      DOUBLE PRECISION      NUMR
      DOUBLE PRECISION      EXPNT

      INTEGER               I
      INTEGER               SEED

      LOGICAL               CONT

C
C     Start the test family with an open call.
C
      CALL TOPEN ( 'F_ZZDM' )

C
C     Initialize the random number seed.
C
      SEED = -6273618

C
C     Initialize the exponent value.
C
      EXPNT = DBLE( INT( LOG10( DPMAX() ) ) )  - 1.D0


C
C     Case 1
C
      CALL TCASE ( 'Safe division check.' )

C
C     Standard, safe evaluation.
C
      NUMR  = 1.D0
      DENOM = 10.D0

      DIV = ZZDIV( NUMR, DENOM )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'ZZDIV 1/10', DIV, '~', 0.1D0, 1.D-12, OK )



C
C     Case 2
C
      CALL TCASE ( 'Underflow division check.' )

C
C     A numeric underflow event as defined in ZZDIV. No error,
C     return zero.
C
      NUMR  = 1.D-200
      DENOM = 1.D200

      DIV = ZZDIV( NUMR, DENOM )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'ZZDIV 1D-400', DIV, '=', 0.D0, 0.D0, OK )



C
C     Case 3
C
      CALL TCASE ( 'Overflow division check.' )

C
C     A numeric overflow event as defined in ZZDIV.
C
      NUMR  = DPMAX()
      DENOM = 1.D0

      DIV = ZZDIV( NUMR, DENOM )
      CALL CHCKXC ( .TRUE., 'SPICE(NUMERICOVERFLOW)', OK )

      CALL CHCKSD ( 'ZZDIV DPMAX/1', DIV, '=', 0.D0, 0.D0, OK )


C
C     Case 4
C
      CALL TCASE ( '1/0 division check.' )

C
C     A divide by zero event.
C
      NUMR  = 1.D0
      DENOM = 0.D0

      DIV = ZZDIV( NUMR, DENOM )
      CALL CHCKXC ( .TRUE., 'SPICE(DIVIDEBYZERO)', OK )

      CALL CHCKSD ( 'ZZDIV 1/0', DIV, '=', 0.D0, 0.D0, OK )


C
C     Case 5
C
      CALL TCASE ( '0/0 division check.' )

C
C     A 0/0 event. ZZDIV treats this as a divide by zero
C     event.
C
      NUMR  = 0.D0
      DENOM = 0.D0

      DIV = ZZDIV( NUMR, DENOM )
      CALL CHCKXC ( .TRUE., 'SPICE(DIVIDEBYZERO)', OK )

      CALL CHCKSD ( 'ZZDIV 0/0', DIV, '=', 0.D0, 0.D0, OK )



C
C     Case 6
C
      CALL TCASE ( 'Safe multiplication check.' )

C
C     Standard, safe evaluation.
C
      A = 2.D0
      B = 10.D0

      MULT = ZZMULT( A, B )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'ZZMULT 2*10', MULT, '~', 20.D0, 1.D-12, OK )



C
C     Case 7
C
      CALL TCASE ( 'A numeric overflow event (1).' )

      A = 1.D0
      B = DPMAX()

      MULT = ZZMULT( A, B )
      CALL CHCKXC ( .TRUE., 'SPICE(NUMERICOVERFLOW)', OK )

      CALL CHCKSD ( 'ZZMULT overflow (1)', MULT, '=', 0.D0, 1.D-12, OK )



C
C     Case 8
C
      CALL TCASE ( 'A numeric overflow event (2).' )

      A = 1.D208
      B = 1.D307

      MULT = ZZMULT( A, B )
      CALL CHCKXC ( .TRUE., 'SPICE(NUMERICOVERFLOW)', OK )

      CALL CHCKSD ( 'ZZMULT overflow (2)', MULT, '=', 0.D0, 1.D-12, OK )



C
C     Case 9
C
      CALL TCASE ( 'Zero multiplication check (1).' )

      A = 0.D0
      B = 10.D0

      MULT = ZZMULT( A, B )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'ZZMULT zero (1)', MULT, '=', 0.D0, 1.D-12, OK )



C
C     Case 10
C
      CALL TCASE ( 'Zero multiplication check (2).' )

      A = 10.D0
      B = 0.D0

      MULT = ZZMULT( A, B )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'ZZMULT zero (2)', MULT, '=', 0.D0, 1.D-12, OK )



C
C     Case 11
C
      CALL TCASE ( 'Zero multiplication check (3).' )

      A = 0.D0
      B = 0.D0

      MULT = ZZMULT( A, B )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'ZZMULT zero (3)', MULT, '=', 0.D0, 1.D-12, OK )

C
C     Note, the use of 5000 iterations is an arbitrary choice.
C

C
C     Case 12
C
      CALL TCASE ( 'Random multiplication check.' )

      DO I=1, 5000

         A  =  10.D0 ** T_RANDD( -EXPNT, EXPNT, SEED )
         B  =  10.D0 ** T_RANDD( -EXPNT, EXPNT, SEED )

         CONT   = .FALSE.
         LOGVAL = LOG10( DABS(A) ) + LOG10( DABS(B) )

C
C        Pass over the calculation if the log10 of the calculated is not
C        within (-EXPNT, EXPNT).
C
         CONT   = (LOGVAL .LT. EXPNT) .AND. ( LOGVAL .GT. -EXPNT )

         IF ( CONT ) THEN

            MULT  = ZZMULT( A, B )
            CALL CHCKXC ( .FALSE., ' ', OK )

            MULTX = A*B

            CALL CHCKSD ( 'Random multiplication',
     .                     MULT, '=', MULTX, 1.D-12, OK )

         END IF

      END DO


C
C     Case 13
C
      CALL TCASE ( 'Random division check.' )

      DO I=1, 5000

         NUMR  =  10.D0 ** T_RANDD( -EXPNT, EXPNT, SEED )
         DENOM =  10.D0 ** T_RANDD( -EXPNT, EXPNT, SEED )

         CONT   = .FALSE.
         LOGVAL = LOG10( DABS(NUMR) ) - LOG10( DABS(DENOM) )

C
C        Pass over the calculation if the log10 of the calculated is not
C        within (-EXPNT, EXPNT) or if the denominator is zero.
C
         CONT   = (LOGVAL .LT. EXPNT)
     .            .AND.
     .            ( LOGVAL .GT. -EXPNT )
     .            .AND.
     .            ( DENOM .NE. 0.D0 )

         IF ( CONT ) THEN

            DIV  = ZZDIV( NUMR, DENOM )
            CALL CHCKXC ( .FALSE., ' ', OK )

            MULTX = NUMR/DENOM

            CALL CHCKSD ( 'Random division',
     .                    DIV, '=', MULTX, 1.D-12, OK )

         END IF

      END DO


C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN

      END
